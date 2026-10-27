(require :asdf)
(asdf:load-system "cl-weave")
(load (merge-pathnames "../scripts/coverage-child-merge.lisp" *load-truename*))

(defpackage #:http-coverage-child-merge-test
  (:use #:cl))
(in-package #:http-coverage-child-merge-test)

(defvar *checks* 0)
(defvar *roots* nil)
(defvar *output* (uiop:ensure-directory-pathname (uiop:getenv "COVERAGE_CHILD_TEST_OUTPUT")))

(defun check (value name)
  (incf *checks*)
  (unless value (error "Child merge assertion: ~S" name)))

(defun raw-equal (one two)
  (and (= (length one) (length two))
       (every (lambda (record)
                (let ((other (assoc (first record) two :test #'equal)))
                  (and other (equalp (cdr record) (cdr other))))) one)))

(defun copy-payload (payload &key copy-marks)
  (let* ((copy (copy-list payload))
         (identity (copy-list (getf payload :identity)))
         (loads (copy-list (getf payload :loads)))
         (entries (mapcar #'copy-list (getf payload :entries))))
    (when loads (setf (first loads) (copy-list (first loads))))
    (setf (getf copy :identity) identity
          (getf copy :loads) loads
          (getf copy :entries) entries)
    (dolist (entry entries)
      (let ((metadata (getf entry :metadata))
            (state (getf entry :state)))
        (when metadata
          (setf (getf entry :metadata) (copy-list metadata)))
        (when state
          (setf (getf entry :state)
                (list* (first state) (copy-seq (second state))
                       (if copy-marks (copy-seq (cddr state)) (cddr state)))))
        (when (and copy-marks (getf entry :codes))
          (setf (getf entry :codes)
                (mapcar (lambda (code) (list (first code) (copy-seq (second code))))
                        (getf entry :codes))))))
    copy))

(check (http-coverage-child-merge:supported-runtime-p) :verified-native-platform)
(let ((*features* (remove :linux (remove :darwin *features*)))
      (http-coverage-child-merge:*collector* nil))
  (check (not (http-coverage-child-merge:supported-runtime-p)) :missing-platform-rejected)
  (check (handler-case (progn (http-coverage-child-merge::runtime-identity) nil)
           (simple-error (condition)
             (search "Child coverage: unsupported native adapter"
                     (princ-to-string condition)))) :unsupported-adapter-rejected)
  (check (null (http-coverage-child-merge:merge-pending)) :unsupported-no-merge))

(let ((identity (http-coverage-child-merge::runtime-identity)))
  (let ((*features* (cons :coverage-child-feature-drift *features*)))
    (check (equal identity (http-coverage-child-merge::runtime-identity))
           :dynamic-library-feature-ignored))
  (let ((*features* (remove :sbcl *features*)))
    (check (not (equal identity (http-coverage-child-merge::runtime-identity)))
           :actual-runtime-feature-drift-observed))
  (let ((*features* (cons (make-symbol "PACKAGE-LOCAL-FEATURE") *features*)))
    (check (equal identity (http-coverage-child-merge::runtime-identity))
           :non-keyword-feature-ignored))
  (check (equal identity (http-coverage-child-merge::runtime-identity))
         :actual-runtime-features-restored))

(defun reject-control (ticket kind)
  (let* ((request (getf ticket :request))
         (bad (copy-payload (http-coverage-child-merge::accepted-payload ticket)))
         (entry (find-if (lambda (entry) (getf (getf entry :metadata) :maps))
                         (getf bad :entries)))
         (before (sb-cover:save-coverage)))
    (ecase kind
      (:source (setf (getf bad :manifest) "different source bytes"))
      (:fasl (setf (second (first (getf bad :loads))) '("different FASL")))
      (:archive (setf (third (first (getf bad :loads))) '("different archived FASL")))
      (:load-count (push (first (getf bad :loads)) (getf bad :loads)))
      (:path (setf (aref (second (getf entry :state)) 0) '(:different 999)))
      (:marker (setf (getf (getf entry :metadata) :maps) '(((:different 999)))))
      (:location (setf (getf (getf entry :metadata) :locations) #(999)))
      (:compiler (setf (getf (getf bad :identity) :compiler) "unsupported"))
      (:runtime (setf (getf (getf bad :identity) :runtime) "different runtime"))
      (:core (setf (getf (getf bad :identity) :core) "different core"))
      (:features (setf (getf (getf bad :identity) :features) '(:different)))
      (:policy (setf (getf (getf bad :identity) :policy) 0))
      (:missing-file (setf (getf bad :entries) (rest (getf bad :entries))))
      (:missing-marks (setf (getf entry :codes) nil)))
    (check (handler-case
               (progn (http-coverage-child-merge::canonical-input bad request *roots*) nil)
             (simple-error (condition)
               (let ((message (princ-to-string condition)))
                 (and (search "Child coverage:" message)
                      (search (ecase kind
                                ((:source :compiler :runtime :core :features :policy)
                                 "payload identity")
                                ((:fasl :archive) "payload FASL")
                                (:load-count "payload FASL load multiplicity")
                                (:path "field :STATE-PATHS expected")
                                (:marker "field :MAPS expected")
                                (:location "field :LOCATIONS expected")
                                (:missing-file "missing or duplicate native file")
                                (:missing-marks "field :CODE-MAPS expected"))
                              message))))) kind)
    (check (raw-equal before (sb-cover:save-coverage)) :rejection-does-not-write-native)
    (format t "REJECT ~S native-unchanged~%" kind)))

(defun reject-coherent-tampering (ticket collector)
  (let* ((original (getf ticket :stage))
         (sealed (getf ticket :digest))
         (tampered (make-pathname :name "coherent-tampering"
                                  :type "sexp"
                                  :defaults *output*))
         (bad (copy-payload (http-coverage-child-merge::accepted-payload ticket)
                            :copy-marks t))
         (before (sb-cover:save-coverage))
         (changed 0))
    ;; Only the untrusted payload copy changes, never the parent's native store.
    (dolist (entry (getf bad :entries))
      (let ((observed (make-hash-table :test #'equal))
            (record (getf entry :state)))
        (dolist (code (getf entry :codes))
          (destructuring-bind (map marks) code
            (loop for paths in map for index from 0
                  when (and paths (= http-coverage-child-merge::+unmarked-byte+
                                     (aref marks index)))
                  do (setf (aref marks index)
                           (logxor http-coverage-child-merge::+unmarked-byte+ 1)))
            (loop for paths in map for mark across marks
                  unless (= mark http-coverage-child-merge::+unmarked-byte+)
                  do (dolist (path paths) (setf (gethash path observed) t)))))
        (dotimes (index (length (second record)))
          (let ((bit (if (gethash (aref (second record) index) observed) 1 0)))
            (when (/= bit (sbit (cddr record) index)) (incf changed))
            (setf (sbit (cddr record) index) bit)))))
    (check (> changed 0) :coherent-payload-has-unexecuted-counter-forgeries)
    (check (http-coverage-child-merge::canonical-input bad (getf ticket :request) *roots*)
           :coherent-payload-passes-native-consistency-alone)
    (http-coverage-child-merge::write-form tampered bad)
    (unwind-protect
        (progn
          (setf (getf ticket :stage) tampered)
          (check (handler-case
                     (progn (http-coverage-child-merge:merge-pending) nil)
                   (simple-error (condition)
                          (not (null (search "accepted payload digest changed"
                                             (princ-to-string condition))))))
                 :coherent-tampering-rejected-by-byte-seal)
          (check (not (http-coverage-child-merge::collector-merged collector))
                 :coherent-tampering-not-merged)
          (check (raw-equal before (sb-cover:save-coverage)) :coherent-no-native-write)
          (check (equal sealed (getf ticket :digest)) :accepted-seal-unchanged))
      (setf (getf ticket :stage) original
            (http-coverage-child-merge::collector-failure collector) nil)
      (when (probe-file tampered) (delete-file tampered)))
    (check (equal sealed
                  (subseq (http-coverage-child-merge::hashes (list original)) 0 64))
           :original-accepted-file-unchanged)
    (format t "REJECT :COHERENT-TAMPERING forged-counters=~D native-unchanged seal=EQ~%"
            changed)))

(let* ((http-coverage-child-merge:*collector* (http-coverage-child-merge:make-collector))
       (merge-function (fdefinition 'sb-cover:merge-coverage))
       (report-function (fdefinition 'sb-cover:report))
       (load-method (find-method #'asdf:perform '(:around)
                                 (list (find-class 'asdf:load-op)
                                       (find-class 'asdf:cl-source-file)) nil))
       (compile-method (find-method #'asdf:perform '(:around)
                                    (list (find-class 'asdf:compile-op)
                                          (find-class 'asdf:cl-source-file)) nil))
       (input-method (find-method #'asdf:input-files '(:around)
                                  (list (find-class 'asdf:load-op)
                                        (find-class 'asdf:cl-source-file)) nil))
       (method nil))
  (check (and (null load-method) (null compile-method) (null input-method))
         :available-asdf-seams)
  (proclaim '(optimize (sb-cover:store-coverage-data 3)))
  (unwind-protect
       (progn
         (setf method
               (eval '(defmethod asdf:perform :around
                          ((op asdf:load-op) (component asdf:cl-source-file))
                        (let ((old (cdr sb-c::*code-coverage-info*))
                              (record (http-coverage-child-merge:capture-load component)))
                          (sb-sys:without-gcing
                            (multiple-value-prog1 (call-next-method)
                              (loop for tail on (cdr sb-c::*code-coverage-info*)
                                    until (eq tail old)
                                    do (dotimes (index (sb-int:weak-vector-len (car tail)))
                                         (let ((code (sb-int:weak-vector-ref (car tail) index)))
                                           (when code (push code *roots*)))))
                              (http-coverage-child-merge:record-load record *roots*)))))))
         ;; Match the canonical CLI's forced, overlapping coverage system loads.
         (dolist (name '("cl-http-kit" "cl-http-kit/http2" "cl-http-kit/http3"
                         "cl-http-kit/observability" "cl-http-kit/client"
                         "cl-http-kit/network" "cl-http-kit/tls"))
           (asdf:load-system name :force t))
         (asdf:load-system "cl-http-kit/test" :force t))
    (when method (remove-method #'asdf:perform method)))
  (let* ((roots (append *roots* *roots*))
         (index (http-coverage-child-merge::code-index roots))
         (expected-files
           (remove-duplicates
            (loop for code in roots
                  when (sb-c::code-coverage-map code)
                    collect (http-coverage-child-merge::code-name code))
            :test #'equal))
         (files 0))
    (check (= (length expected-files) (hash-table-count index))
           :indexed-file-count-equal-original-roots)
    (dolist (file expected-files)
       (incf files)
       (check
        (equalp
         (sort (loop for code in (remove-duplicates roots :test #'eq)
                     for map = (sb-c::code-coverage-map code)
                     when (and map (equal file (http-coverage-child-merge::code-name code)))
                       collect (list (copy-tree (coerce map 'list))
                                     (http-coverage-child-merge::native-marks
                                      code (length map))))
               #'string< :key (lambda (entry) (prin1-to-string (first entry))))
         (http-coverage-child-merge::code-records file index))
        :indexed-records-equal-original-snapshot))
    (check (plusp files) :nonempty-index-comparison)
    (check (null (http-coverage-child-merge::code-records "unknown native file" index))
           :missing-index-file-has-no-records))
  (let* ((selected 0)
         (assertions 0))
    (check
     (cl-weave:run-all
      :name-filter "client-contract-partial-missing-"
      :timeout-ms 30000 :max-workers 1 :retry 0 :bail t
      :pass-with-no-tests nil :coverage-reset nil
      :on-test-attempt-complete
      (lambda (path assertion-count)
        (declare (ignore path))
        (incf selected)
        (incf assertions assertion-count)))
     :existing-public-contracts)
    (check (= 3 selected) :exact-existing-selection)
    (check (= 9 assertions) :unchanged-existing-assertions)
    (format t "PUBLIC selected=~D assertions=~D timeout-ms=30000 worker=1 retry=0~%"
            selected assertions))
  (let* ((collector http-coverage-child-merge:*collector*)
         (tickets (reverse (http-coverage-child-merge::collector-tickets collector)))
         (payload-seal (mapcar (lambda (ticket) (getf ticket :digest)) tickets))
         (before (sb-cover:save-coverage))
         (expected (mapcar (lambda (record)
                             (list* (first record) (second record) (copy-seq (cddr record))))
                           before)))
    (check (= 3 (length tickets)) :three-genuine-children)
    (check (every (lambda (ticket) (eql 0 (getf ticket :exit))) tickets) :child-exits)
    (check (plusp (length (http-coverage-child-merge::collector-manifest collector)))
           :nonempty-byte-manifest)
    (http-coverage-child-merge::write-form (merge-pathnames "tickets.sexp" *output*) tickets)
    (dolist (kind '(:source :fasl :archive :load-count :path :marker :location :compiler
                    :runtime :core :features :policy :missing-file :missing-marks))
      (reject-control (first tickets) kind))
    (progn
      (let ((*features* (cons :coverage-child-feature-drift *features*))
            (unchanged (sb-cover:save-coverage)))
        (let* ((ticket (first tickets))
               (accepted (http-coverage-child-merge::canonical-input
                          (http-coverage-child-merge::accepted-payload ticket)
                          (getf ticket :request) *roots*)))
          (check accepted :volatile-feature-drift-ignored))
        (check (raw-equal unchanged (sb-cover:save-coverage))
               :dynamic-feature-no-native-write))
      (let ((*features* (remove :sbcl *features*))
            (unchanged (sb-cover:save-coverage)))
        (let ((ticket (first tickets)))
          (check (handler-case
                     (progn (http-coverage-child-merge::canonical-input
                             (http-coverage-child-merge::accepted-payload ticket)
                             (getf ticket :request) *roots*) nil)
                   (simple-error (condition)
                     (search "Child coverage: payload identity"
                             (princ-to-string condition))))
                 :actual-feature-drift-rejected))
        (check (raw-equal unchanged (sb-cover:save-coverage))
               :actual-feature-no-native-write)))
    (progn
      (reject-coherent-tampering (first tickets) collector)
      (let* ((ticket (second tickets))
             (original-stage (getf ticket :stage))
             (invalid-stage (make-pathname :name "invalid-resealed" :type "sexp" :defaults *output*))
             (original-digest (getf ticket :digest)))
        (let* ((payload (http-coverage-child-merge::accepted-payload ticket))
               (entry (first (getf payload :entries))))
          (setf (second (getf entry :state)) nil)
          (http-coverage-child-merge::write-form invalid-stage payload))
        (unwind-protect
             (progn
               (setf (getf ticket :stage) invalid-stage
                     (getf ticket :digest)
                       (subseq (http-coverage-child-merge::hashes (list invalid-stage)) 0 64))
               (check (equal (getf ticket :digest)
                             (subseq (http-coverage-child-merge::hashes
                                      (list (getf ticket :stage))) 0 64))
                      :structural-invalid-payload-resealed)
               (check (handler-case
                          (progn (http-coverage-child-merge:merge-pending) nil)
                        (simple-error (condition)
                          (not (null (search "native source/path/location/CODE-MAP mismatch"
                                             (princ-to-string condition))))))
                      :resealed-structural-invalid-payload-rejected)
               (check (not (http-coverage-child-merge::collector-merged collector))
                      :resealed-structural-invalid-not-merged)
               (check (raw-equal before (sb-cover:save-coverage))
                      :resealed-structural-invalid-no-native-write))
          (setf (getf ticket :stage) original-stage
                (getf ticket :digest) original-digest
                (http-coverage-child-merge::collector-failure collector) nil)
          (when (probe-file invalid-stage) (delete-file invalid-stage)))))
    ;; Validate a missing completion before the official merge can change any bit.
    (let ((ticket (first tickets)))
      (setf (getf ticket :exit) :pending)
      (check (handler-case (progn (http-coverage-child-merge:merge-pending) nil)
               (simple-error (condition)
                 (search "Child coverage: missing successful child exit"
                         (princ-to-string condition)))) :missing-child-fails)
      (check (raw-equal before (sb-cover:save-coverage)) :missing-child-no-native-write)
      (setf (getf ticket :exit) 0
            (http-coverage-child-merge::collector-failure collector) nil))
    (dolist (ticket tickets)
      (let ((input (http-coverage-child-merge::canonical-input
                    (http-coverage-child-merge::accepted-payload ticket)
                    (getf ticket :request) *roots*)))
        (dolist (record expected)
          (let ((child (assoc (first record) input :test #'equal)))
            (when child (bit-ior (cddr record) (cddr child) (cddr record)))))))
    (http-coverage-child-merge::write-form (merge-pathnames "before.native" *output*) before)
    (sb-cover:report (merge-pathnames "before-report/" *output*))
    (http-coverage-child-merge:merge-pending)
    (check (and (equal payload-seal
                       (mapcar (lambda (ticket) (getf ticket :digest)) tickets))
                (every (lambda (ticket)
                         (http-coverage-child-merge::accepted-payload ticket))
                       tickets))
           :original-child-payloads-unchanged)
    (let ((after (sb-cover:save-coverage)) (delta 0) (client-delta 0)
          (branches 0) (negative 0))
      (dolist (record before)
        (let* ((file (first record)) (old (cddr record))
               (new-record (assoc file after :test #'equal))
               (new (cddr new-record))
               (expected-bits (cddr (assoc file expected :test #'equal))))
          (check (equalp (second record) (second new-record)) :native-denominator-unchanged)
          (check (equalp expected-bits new) :exact-official-native-union)
          (dotimes (index (length old))
            (when (and (= 0 (sbit old index)) (= 1 (sbit new index)))
              (incf delta)
              (when (search "/client/" file) (incf client-delta))
              (when (member (car (aref (second record) index)) '(:then :else))
                (incf branches))
              (when (search "/client/client.lisp" file)
                (format t "DELTA ~A path=~S 0->1~%" file (aref (second record) index))))
            (when (= 0 (sbit new index)) (incf negative)))))
      (check (and (> delta 0) (> client-delta 0) (> branches 0)) :real-child-only-coverage)
      (check (> negative 0) :unexecuted-native-negatives-preserved)
      (http-coverage-child-merge::write-form (merge-pathnames "after.native" *output*) after)
      (sb-cover:report (merge-pathnames "after-report/" *output*))
      (check (raw-equal after (sb-cover:save-coverage)) :html-report-does-not-change-native)
      (check (equal (http-coverage-child-merge::collector-manifest collector)
                    (http-coverage-child-merge::hashes
                     (getf (getf (first tickets) :request) :paths))) :post-byte-manifest)
      (check (and (eq load-method (find-method #'asdf:perform '(:around)
                                               (list (find-class 'asdf:load-op)
                                                     (find-class 'asdf:cl-source-file)) nil))
                  (eq compile-method (find-method #'asdf:perform '(:around)
                                                  (list (find-class 'asdf:compile-op)
                                                        (find-class 'asdf:cl-source-file)) nil))
                  (eq input-method (find-method #'asdf:input-files '(:around)
                                                (list (find-class 'asdf:load-op)
                                                      (find-class 'asdf:cl-source-file)) nil))
                  (eq merge-function (fdefinition 'sb-cover:merge-coverage))
                  (eq report-function (fdefinition 'sb-cover:report))) :spies-restored-eq)
      (format t "RESULT version=~A selected=3 public-assertions=9 controls=~D delta=~D client=~D branches=~D raw-negative=~D manifests=EQ spies=EQ~%"
              (lisp-implementation-version) *checks* delta client-delta branches negative))))
