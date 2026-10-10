(require :asdf)
(require :sb-cover)

(defpackage #:http-coverage-child-merge
  (:use #:cl)
  (:export #:*collector* #:make-collector #:supported-runtime-p #:capture-load #:record-load #:prepare-child
           #:accept-child #:merge-pending #:start-child #:finish-child))
(in-package #:http-coverage-child-merge)

(defvar *collector* nil)
(defvar *bridge* (truename *load-truename*))
(defvar *child-request* nil)
(defvar *child-methods* nil)
(defvar *child-input-method* nil)
(defvar *child-load-input* nil)
(defvar *child-roots* nil)
(defvar *child-loads* nil)

(defstruct collector
  roots loads tickets manifest merged failure directory (sequence 0)
  (table (car sb-c::*code-coverage-info*)))

(defun require-proof (value message)
  (unless value (error "Child coverage: ~A" message)))

(defun supported-runtime-p ()
  (and (member (lisp-implementation-version) '("2.6.6" "2.6.8") :test #'string=)
       (member :arm64 *features*) (member :darwin *features*) t))

(defun runtime-identity ()
  (require-proof (supported-runtime-p)
                 "unsupported native adapter")
  (list :compiler (lisp-implementation-version) :machine (machine-type)
        :runtime (namestring (truename sb-ext:*runtime-pathname*))
        :core (namestring (truename sb-ext:*core-pathname*))
        :features (sort (copy-list *features*) #'string< :key #'symbol-name)
        :policy (sb-c::policy sb-c::*policy* sb-cover:store-coverage-data)))

(defun load-identity (component)
  (list (namestring (truename (asdf:component-pathname component)))
        (mapcar (lambda (path) (namestring (truename path)))
                (asdf:input-files 'asdf:load-op component))))

(defun payload-bytes (pathname)
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (require-proof (= (length bytes) (read-sequence bytes stream)) "short payload read")
      (require-proof (eq :end (read-byte stream nil :end)) "payload grew during read")
      bytes)))

(defun capture-load (component)
  (when *collector*
    (unless (collector-directory *collector*)
      (setf (collector-directory *collector*)
            (uiop:ensure-directory-pathname
             (string-trim '(#\Newline #\Return)
                          (uiop:run-program (list "mktemp" "-d"
                                                  (namestring
                                                   (merge-pathnames "http-coverage-fasls.XXXXXXXX"
                                                                    (uiop:temporary-directory))))
                                            :output :string)))))
    (let* ((identity (load-identity component))
           (inputs (second identity)))
      (require-proof (= 1 (length inputs)) "unsupported FASL load inputs")
      (let* ((bytes (payload-bytes (first inputs)))
             (archive (merge-pathnames
                       (format nil "~D.fasl" (incf (collector-sequence *collector*)))
                       (collector-directory *collector*))))
        (with-open-file (stream archive :direction :output :if-exists :error
                                       :element-type '(unsigned-byte 8))
          (write-sequence bytes stream))
        (require-proof (equalp bytes (payload-bytes archive)) "FASL archive copy changed")
        (append identity (list (list (namestring (truename archive)))))))))

(defun record-load (record roots)
  (when *collector*
    ;; Keep the exact generation used by this load, not the subsequently overwritten cache.
    (require-proof (equalp (payload-bytes (first (second record)))
                           (payload-bytes (first (third record))))
                   "parent FASL changed during load")
    (push record (collector-loads *collector*))
    (setf (collector-roots *collector*) roots)))

(defun hashes (paths)
  (let ((paths (sort (remove-duplicates
                      (mapcar (lambda (path) (namestring (truename path))) paths)
                      :test #'equal) #'string<)))
    (dolist (path paths)
      (require-proof (not (or (find #\Newline path) (find #\Return path)))
                     "unrepresentable manifest path"))
    (with-output-to-string (stream)
      (loop for tail on paths by (lambda (tail) (nthcdr (min 100 (length tail)) tail))
            do (write-string
                (uiop:run-program (append '("shasum" "-a" "256")
                                          (subseq tail 0 (min 100 (length tail))))
                                  :output :string) stream)))))

(defun input-paths ()
  (let ((paths (list *bridge* sb-ext:*runtime-pathname* sb-ext:*core-pathname*)))
    (labels ((visit (component)
               (when (typep component 'asdf:cl-source-file)
                 (push (asdf:component-pathname component) paths)
                 (dolist (file (asdf:input-files 'asdf:load-op component))
                   (when (probe-file file) (push file paths))))
               (when (typep component 'asdf:module)
                 (mapc #'visit (asdf:component-children component)))))
      (dolist (name (asdf:already-loaded-systems))
        (let ((system (asdf:find-system name)))
          (when (asdf:system-source-file system)
            (push (asdf:system-source-file system) paths))
          (visit system))))
    paths))

(defun write-form (pathname value)
  (with-open-file (stream pathname :direction :output :if-exists :error)
    (with-standard-io-syntax
      (write value :stream stream :readably t))))

(defun read-form (pathname)
  (with-open-file (stream pathname)
    (with-standard-io-syntax
      (let ((*read-eval* nil))
        (prog1 (read stream)
          (require-proof (eq :end (read stream nil :end)) "trailing payload"))))))

(defun accepted-payload (ticket)
  (let ((bytes (payload-bytes (getf ticket :result))))
    (require-proof (and (getf ticket :accepted-bytes)
                        (equalp bytes (getf ticket :accepted-bytes)))
                   "accepted payload bytes changed")
    ;; Parse this verified snapshot, never reopen the pathname after verification.
    (with-input-from-string (stream (sb-ext:octets-to-string bytes :external-format :utf-8))
      (with-standard-io-syntax
        (let ((*read-eval* nil))
          (prog1 (read stream)
            (require-proof (eq :end (read stream nil :end)) "trailing payload")))))))

(defun code-name (code)
  (sb-c::debug-source-namestring
   (sb-c::debug-info-source (sb-kernel:%code-debug-info code))))

(defun code-index (roots)
  (let ((index (make-hash-table :test #'equal)))
    (dolist (code (remove-duplicates roots :test #'eq))
      (when (sb-c::code-coverage-map code)
        (push code (gethash (code-name code) index))))
    (maphash (lambda (file codes) (setf (gethash file index) (nreverse codes))) index)
    index))

(defun code-records (file index)
  (sort (loop for code in (gethash file index)
              for map = (sb-c::code-coverage-map code)
                collect (list (copy-tree (coerce map 'list))
                              (copy-seq (sb-cover::code-coverage-marks code))))
        #'string< :key (lambda (entry) (prin1-to-string (first entry)))))

(defun metadata (file codes)
  (let ((native (gethash file (car sb-c::*code-coverage-info*))))
    (require-proof native "unknown native file")
    (list :paths (sb-c::covered-file-paths native)
          :locations (sb-c::covered-file-locations native)
          :lines (sb-c::covered-file-lines native)
          :maps (mapcar #'first codes))))

(defun prepare-child ()
  (require-proof (and *collector* (not (collector-merged *collector*)))
                 "child outside collecting phase")
  (require-proof (eq (collector-table *collector*) (car sb-c::*code-coverage-info*))
                 "native table changed")
  (require-proof (= 3 (getf (runtime-identity) :policy)) "uninstrumented compiler policy")
  (let* ((directory (uiop:ensure-directory-pathname
                     (string-trim '(#\Newline #\Return)
                                  (uiop:run-program (list "mktemp" "-d"
                                                          (namestring
                                                           (merge-pathnames "http-coverage-child.XXXXXXXX"
                                                                            (uiop:temporary-directory))))
                                                    :output :string))))
         (request-file (merge-pathnames "request.sexp" directory))
         (result-file (merge-pathnames "result.sexp" directory))
         (paths (append (input-paths)
                        (loop for record in (collector-loads *collector*)
                              append (third record))))
         (manifest (hashes paths))
         (request (list :identity (runtime-identity) :paths paths :manifest manifest
                        :loads (collector-loads *collector*) :result result-file))
         (ticket (list :request request :result result-file :exit :pending
                       :accepted-bytes nil)))
    (if (collector-manifest *collector*)
        (require-proof (equal manifest (collector-manifest *collector*)) "input drift")
        (setf (collector-manifest *collector*) manifest))
    (write-form request-file request)
    (push ticket (collector-tickets *collector*))
    (values ticket
            (list "--eval" (format nil "(load ~S)" (namestring *bridge*))
                  "--eval" (format nil "(http-coverage-child-merge:start-child ~S)"
                                        (namestring request-file)))
            (list "--eval" "(http-coverage-child-merge:finish-child)"))))

(defun accept-child (ticket exit)
  (require-proof (member ticket (collector-tickets *collector*) :test #'eq)
                 "unregistered child")
  (require-proof (eq :pending (getf ticket :exit)) "duplicate child completion")
  (setf (getf ticket :exit) exit)
  (require-proof (eql 0 exit) "child failed")
  (require-proof (probe-file (getf ticket :result)) "missing child payload")
  (setf (getf ticket :accepted-bytes) (payload-bytes (getf ticket :result))))

(defun start-child (pathname)
  (setf *child-request* (read-form pathname))
  (proclaim '(optimize (sb-cover:store-coverage-data 3)))
  (require-proof (equal (runtime-identity) (getf *child-request* :identity)) "child identity")
  (require-proof (equal (hashes (getf *child-request* :paths))
                       (getf *child-request* :manifest)) "child input drift")
  (dolist (operation '(asdf:compile-op asdf:load-op))
    (require-proof (null (find-method #'asdf:perform '(:around)
                                     (list (find-class operation)
                                           (find-class 'asdf:cl-source-file)) nil))
                   "existing child ASDF method"))
  (require-proof (null (find-method #'asdf:input-files '(:around)
                                   (list (find-class 'asdf:load-op)
                                         (find-class 'asdf:cl-source-file)) nil))
                 "existing child ASDF input method")
  (setf *child-input-method*
        (eval '(defmethod asdf:input-files :around
                   ((op asdf:load-op) (component asdf:cl-source-file))
                 (if (and *child-load-input*
                          (eq op (first *child-load-input*))
                          (eq component (second *child-load-input*)))
                     (third *child-load-input*)
                     (call-next-method)))))
  (push (eval '(defmethod asdf:perform :around
                   ((op asdf:compile-op) (component asdf:cl-source-file))
                 (error "Child coverage forbids recompilation: ~A" component)))
        *child-methods*)
  (push (eval '(defmethod asdf:perform :around
                   ((op asdf:load-op) (component asdf:cl-source-file))
                 (let* ((identity (load-identity component))
                        (records (remove-if-not
                                  (lambda (record) (equal identity (subseq record 0 2)))
                                  (reverse (getf *child-request* :loads))))
                        (values nil))
                   (require-proof records "different child FASL")
                   (require-proof (not (find identity *child-loads*
                                            :key (lambda (record) (subseq record 0 2))
                                            :test #'equal))
                                  "unexpected repeated child ASDF load")
                   ;; ASDF's actual primary and completion methods must run for every
                   ;; archived generation, with only this action's FASL input redirected.
                   (dolist (record records)
                     (let ((old (cdr sb-c::*code-coverage-info*))
                           (*child-load-input*
                             (list op component (mapcar #'pathname (third record)))))
                       (sb-sys:without-gcing
                         (setf values (multiple-value-list (call-next-method)))
                           (push record *child-loads*)
                           (loop for tail on (cdr sb-c::*code-coverage-info*)
                                 until (eq tail old)
                                 do (dotimes (index (sb-int:weak-vector-len (car tail)))
                                      (let ((code (sb-int:weak-vector-ref (car tail) index)))
                                        (when code (push code *child-roots*))))))))
                   ;; Archive timestamps must not change ASDF's completion stamp for
                   ;; the original inputs used by its next load-system plan.
                   (asdf/action:mark-operation-done op component)
                   (values-list values))))
        *child-methods*))

(defun validate-marks (record codes)
  (let ((observed (make-hash-table :test #'equal)))
    (dolist (code codes)
      (destructuring-bind (map marks) code
        (require-proof (= (length map) (length marks)) "native mark length")
        (loop for paths in map for mark across marks
              do (require-proof (typep mark '(unsigned-byte 8)) "invalid native mark")
              unless (= mark 255)
                do (dolist (path paths) (setf (gethash path observed) t)))))
    (dotimes (index (length (second record)))
      (require-proof (= (sbit (cddr record) index)
                        (if (gethash (aref (second record) index) observed) 1 0))
                     "saved bit disagrees with native marks"))))

(defun finish-child ()
  (require-proof *child-request* "missing child request")
  (require-proof (equal (runtime-identity) (getf *child-request* :identity))
                 "post-child identity drift")
  (unwind-protect
       (let* ((state (sb-cover:save-coverage))
              (index (code-index *child-roots*))
              (entries (loop for record in state
                             for file = (first record)
                             for codes = (code-records file index)
                             do (validate-marks record codes)
                             collect (list :state record :metadata (metadata file codes)
                                           :codes codes))))
         (require-proof (and state *child-loads*) "empty child coverage")
         (require-proof (equal (hashes (getf *child-request* :paths))
                              (getf *child-request* :manifest)) "post-child input drift")
         (dolist (method *child-methods*) (remove-method #'asdf:perform method))
         (setf *child-methods* nil)
         (remove-method #'asdf:input-files *child-input-method*)
         (setf *child-input-method* nil)
         (dolist (operation '(asdf:compile-op asdf:load-op))
           (require-proof (null (find-method #'asdf:perform '(:around)
                                            (list (find-class operation)
                                                  (find-class 'asdf:cl-source-file)) nil))
                          "child ASDF restoration"))
         (require-proof (null (find-method #'asdf:input-files '(:around)
                                          (list (find-class 'asdf:load-op)
                                                (find-class 'asdf:cl-source-file)) nil))
                        "child ASDF input restoration")
         (write-form (getf *child-request* :result)
                     (list :identity (runtime-identity) :manifest (getf *child-request* :manifest)
                           :loads *child-loads* :entries entries :restored t)))
    (dolist (method *child-methods*) (remove-method #'asdf:perform method))
    (setf *child-methods* nil)
    (when *child-input-method*
      (remove-method #'asdf:input-files *child-input-method*)
      (setf *child-input-method* nil))))

(defun native-equal (expected actual field file)
  (unless (equalp expected actual)
    (let ((*print-length* 8) (*print-level* 5))
      (error "Child coverage: ~A field ~S expected ~A actual ~A"
             (file-namestring file) field (prin1-to-string expected) (prin1-to-string actual)))))

(defun canonical-input (payload request roots)
  (require-proof (and (equal (runtime-identity) (getf payload :identity))
                      (equal (getf request :identity) (getf payload :identity))
                      (equal (getf request :manifest) (getf payload :manifest))
                      (getf payload :restored)
                      (getf payload :loads) (getf payload :entries)) "payload identity")
  (dolist (load (getf payload :loads))
    (require-proof (member load (getf request :loads) :test #'equal) "payload FASL")
    (require-proof (= (count load (getf payload :loads) :test #'equal)
                      (count load (getf request :loads) :test #'equal))
                   "payload FASL load multiplicity"))
  (require-proof
   (equal (sort (remove-duplicates
                 (loop for load in (getf payload :loads)
                       when (gethash (first load) (car sb-c::*code-coverage-info*))
                         collect (first load)) :test #'equal) #'string<)
          (sort (mapcar (lambda (entry) (first (getf entry :state)))
                        (getf payload :entries)) #'string<))
   "missing or duplicate native file")
  (let ((seen (make-hash-table :test #'equal))
        (index (code-index roots)))
    (loop for entry in (getf payload :entries)
          for record = (getf entry :state)
          for file = (first record)
          for parent = (gethash file (car sb-c::*code-coverage-info*))
          do (require-proof
              (and parent (not (gethash file seen))
                   (member file (getf payload :loads) :key #'first :test #'equal)
                   (typep (second record) 'simple-vector)
                   (typep (cddr record) 'simple-bit-vector)
                   (= (length (second record)) (length (cddr record))))
              "native source/path/location/CODE-MAP mismatch")
             (native-equal (sb-c::covered-file-paths parent) (second record) :state-paths file)
             (let ((expected (metadata file (code-records file index)))
                   (actual (getf entry :metadata)))
               (dolist (field '(:paths :locations :lines :maps))
                 (native-equal (getf expected field) (getf actual field) field file))
               (native-equal expected actual :metadata file))
             (native-equal (getf (getf entry :metadata) :maps)
                           (mapcar #'first (getf entry :codes)) :code-maps file)
             (validate-marks record (getf entry :codes))
             (setf (gethash file seen) t)
          ;; SBCL compares path vectors by EQUAL, not EQUALP. Only the verified
          ;; parent vector reference changes; the child's measured bits do not.
          collect (list* file (sb-c::covered-file-paths parent) (cddr record)))))

(defun merge-pending ()
  (when (and *collector* (not (collector-merged *collector*)))
    (when (collector-failure *collector*) (error (collector-failure *collector*)))
    (handler-case
        (let* ((tickets (reverse (collector-tickets *collector*)))
               (inputs
                 (loop for ticket in tickets
                       for request = (getf ticket :request)
                       do (require-proof (eql 0 (getf ticket :exit)) "missing successful child exit")
                          (require-proof (equal (hashes (getf request :paths))
                                                (getf request :manifest)) "pre-merge input drift")
                       collect (canonical-input (accepted-payload ticket) request
                                                (collector-roots *collector*)))))
          (require-proof (eq (collector-table *collector*) (car sb-c::*code-coverage-info*))
                         "pre-merge native table changed")
          ;; Validate every child before any official native merge is attempted.
          (dolist (input inputs) (sb-cover:merge-coverage input))
          (setf (collector-merged *collector*) t)
          (format *error-output* "Child coverage merge: ~D verified children.~%" (length inputs)))
      (error (condition)
        (setf (collector-failure *collector*) condition)
        (error condition)))))
