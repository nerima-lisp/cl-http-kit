(eval-when (:compile-toplevel :load-toplevel :execute) (require :asdf) (require :sb-cover))

(defpackage #:http-coverage-normalization
  (:use #:cl)
  (:export #:capture-source #:seal-source #:project-table #:call-with-projection
           #:call-with-deferred-projection
           #:source-proof #:source-proof-bytes #:source-proof-paths
           #:source-proof-candidates #:*proofs*))
(in-package #:http-coverage-normalization)

(defvar *proofs* (make-hash-table :test 'equal))

(defstruct source-proof bytes features candidates reader-constants macro-dependencies paths)

(define-condition unsupported-source (error) ())

(defun normalization-supported-p (&optional
                                    (version (lisp-implementation-version))
                                    (machine (machine-type))
                                    (software (software-type)))
  (and (string= version "2.6.6")
       (or (and (string= machine "ARM64") (string= software "Darwin"))
           (and (string= machine "X86-64") (string= software "Linux")))))

(defun source-bytes (pathname)
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream)
                             :element-type '(unsigned-byte 8))))
      (unless (= (read-sequence bytes stream) (length bytes))
        (error "Incomplete coverage source read: ~A" pathname))
      bytes)))

(defun proven-atomic-value (initializer)
  ;; Never ask CONSTANTP to evaluate compound forms or foldable calls.
  (when (and (atom initializer) (constantp initializer)
             (or (not (symbolp initializer)) (boundp initializer)))
    (let ((value (if (symbolp initializer)
                     (symbol-value initializer) initializer)))
      (when (or (integerp value) (null value) (eq value t) (keywordp value)
                (stringp initializer) (floatp initializer))
        (values value t)))))

(defparameter *callback-macro-source*
  (or (when (asdf:find-system "cl-http-kit" nil)
        (asdf:system-relative-pathname "cl-http-kit" "src/control-macros.lisp"))
      (let ((source (or *compile-file-truename* *load-truename* *load-pathname*)))
        (unless source
          (error "Coverage normalization: source pathname unavailable"))
        (merge-pathnames "../src/control-macros.lisp"
                         (uiop:pathname-directory-pathname source)))))
(defparameter *callback-macro-bytes* (source-bytes *callback-macro-source*))

(defun callback-macro-dependencies ()
  (unless (equalp *callback-macro-bytes* (source-bytes *callback-macro-source*))
    (error 'unsupported-source))
  (let ((functions
          (loop for name in '("WITH-HTTP-DEADLINE" "%WITH-HTTP-ERROR-TRANSLATION")
                for symbol = (find-symbol name "HTTP-KIT")
                for function = (and symbol (macro-function symbol))
                do (unless function (error 'unsupported-source))
                collect (list symbol function))))
    (list (copy-seq *callback-macro-bytes*) functions)))

(defun callback-key-candidates (form path)
  ;; Follow only the inspected callback ancestry, never arbitrary macro bodies/data.
  (let ((deadline (and (find-package "HTTP-KIT")
                       (find-symbol "WITH-HTTP-DEADLINE" "HTTP-KIT")))
        (translation (and (find-package "HTTP-KIT")
                          (find-symbol "%WITH-HTTP-ERROR-TRANSLATION" "HTTP-KIT"))))
    (when (and deadline translation)
      (labels ((descend (node path stages)
                 (if stages
                     (destructuring-bind (operator start end) (first stages)
                       (when (and (consp node) (eq (first node) operator))
                         (loop for slot from start below (or end (length node))
                               nconc (descend (nth slot node) (cons slot path)
                                              (rest stages)))))
                     (when (and (consp node) (eq (first node) 'when)
                                (symbolp (second node)))
                       (loop for call in (cddr node) for slot from 2
                             when (and (consp call) (eq (first call) 'funcall)
                                       (= (length call) 4)
                                       (eq (second call) (second node))
                                       (symbolp (third call)))
                               nconc
                               (let ((lambda (fourth call)))
                                 (when (and (consp lambda) (eq (first lambda) 'lambda)
                                            (listp (second lambda))
                                            (= (length (second lambda)) 2)
                                            (eq (first (second lambda)) '&key)
                                            (consp (second (second lambda)))
                                            (= (length (second (second lambda))) 2)
                                            (symbolp (first (second (second lambda))))
                                            (eql (second (second (second lambda))) 8))
                                   (key-candidates (second lambda)
                                                   (list* 1 3 slot path)))))))))
        (descend form path
                 (list (list 'defun 3 nil) (list 'let* 2 nil)
                       (list deadline 2 nil) (list translation 2 nil)
                       (list 'handler-case 1 2) (list 'let* 2 nil)
                       (list 'multiple-value-bind 3 nil)))))))

(defun h3-callback-key-candidates (form path)
  ;; Only the inspected H3 callback argument, not arbitrary nested lambdas.
  (labels ((descend (node path stages)
             (if stages
                 (destructuring-bind (operator start end) (first stages)
                   (when (and (consp node) (eq (first node) operator))
                     (loop for slot from start below (or end (length node))
                           nconc (descend (nth slot node) (cons slot path)
                                          (rest stages)))))
                 (when (and (consp node) (eq (first node) 'when)
                            (symbolp (second node)))
                   (loop for call in (cddr node) for slot from 2
                         when (and (consp call) (eq (first call) 'funcall)
                                   (= (length call) 5)
                                   (eq (second call) (second node))
                                   (symbolp (third call)) (symbolp (fourth call)))
                           nconc
                           (let ((lambda (fifth call)))
                             (when (and (consp lambda) (eq (first lambda) 'lambda)
                                        (listp (second lambda))
                                        (= (length (second lambda)) 2)
                                        (eq (first (second lambda)) '&key)
                                        (consp (second (second lambda)))
                                        (= (length (second (second lambda))) 2)
                                        (symbolp (first (second (second lambda))))
                                        (eql (second (second (second lambda))) 268))
                               (key-candidates (second lambda) (list* 1 4 slot path)))))))))
    (descend form path
             '((defun 3 nil) (let* 2 nil) (unwind-protect 1 2)
               (handler-case 1 2) (progn 1 nil)))))

(defun proven-key-value (initializer)
  ;; This exact RFC ALPN literal has a declaration path but no native marker.
  (if (equal initializer '(quote ("h3")))
      (values (list "h3") t)
      (proven-atomic-value initializer)))

(defun key-candidates (lambda-list path &optional allow-alpn)
  ;; Mixed argument lists can execute the native path for an atomic key default.
  (when (some (lambda (binding)
                (member binding '(&optional &rest &body &aux &whole &environment)))
              lambda-list)
    (return-from key-candidates nil))
  (let ((keys nil) (candidates nil))
    (loop for binding in lambda-list
          for slot from 0
          do (cond
               ((eq binding '&key) (setf keys t))
               ((member binding lambda-list-keywords) (setf keys nil))
               ((and keys (consp binding) (consp (cdr binding))
                     (or (null (cddr binding))
                         (and (consp (cddr binding)) (null (cdddr binding))
                              (symbolp (third binding))
                              (not (constantp (third binding)))
                              (not (member (third binding) lambda-list-keywords))))
                     (symbolp (first binding))
                     (not (member (first binding) lambda-list-keywords)))
                (multiple-value-bind (value proven)
                    ;; Exact quoted NIL has no CODE-MAP marker in pure &KEY lists.
                    (if (equal (second binding) '(quote nil))
                        (values nil t)
                        (if (and allow-alpn (null (cddr binding))
                                 (string= (symbol-name (first binding)) "ALPN")
                                 (equal (second binding) '(quote ("h3"))))
                            (proven-key-value (second binding))
                            (proven-atomic-value (second binding))))
                  (when proven
                    (push (list (if (or (equal (second binding) '(quote nil))
                                        (equal (second binding) '(quote ("h3"))))
                                    (cons 1 (cons slot path)) (cons slot path))
                                (if (equal (second binding) '(quote nil))
                                    nil (second binding))
                                (if (consp value) (copy-tree value)
                                    (if (stringp value) (copy-seq value) value)))
                          candidates))))))
    (nreverse candidates)))

(defun defun-key-candidates (form path)
  (when (and (consp form) (eq (first form) 'defun) (listp (third form)))
    (let* ((callback (callback-key-candidates form path))
           (macros (when callback (callback-macro-dependencies)))
           (body-slot (1- (length form))) (body (car (last form))))
      (values
       (nconc callback (h3-callback-key-candidates form path)
              (key-candidates (third form) (cons 2 path) t)
              (when (and (>= body-slot 3) (consp body)
                         (eq (first body) 'lambda) (listp (second body)))
                (key-candidates (second body) (list* 1 body-slot path))))
       macros))))

(defun keyword-nil-slot-candidates (slots top)
  ;; SBCL registers quoted NIL here without emitting a CODE-MAP marker.
  ;; Exact quoted NIL is not permission to evaluate other compound defaults.
  (loop for slot in slots for index from 2
        when (and (consp slot) (consp (cdr slot)) (symbolp (first slot))
                  (or (null (second slot)) (equal (second slot) '(quote nil))))
          collect (list (list 1 index top) nil nil)))

(defun source-candidates (bytes)
  (let ((*package* (find-package '#:cl-user))
        (*readtable* (copy-readtable nil))
        (*read-eval* nil)
        (eof (gensym "EOF"))
        (candidates nil)
        (reader-constants nil)
        (macro-dependencies nil))
    ;; Resolve only atomic constants, without enabling arbitrary read-time evaluation.
    (set-dispatch-macro-character
     #\# #\.
     (lambda (stream character argument)
       (declare (ignore character))
       (let ((initializer (read stream t nil t)))
         (unless *read-suppress*
           (when argument (error 'unsupported-source))
           (multiple-value-bind (value proven) (proven-atomic-value initializer)
             (unless proven (error 'unsupported-source))
             (push (list (if (stringp initializer) (copy-seq initializer) initializer)
                         (if (stringp value) (copy-seq value) value))
                   reader-constants)
             value)))))
    (with-input-from-string
        (stream (sb-ext:octets-to-string bytes :external-format :utf-8))
      (loop for top from 0
            for form = (read stream nil eof)
            until (eq form eof)
            do (cond
                 ((and (consp form) (eq (first form) 'in-package))
                  (setf *package* (or (find-package (second form))
                                      (error 'unsupported-source))))
                 ((and (consp form) (eq (first form) 'defstruct)
                       (consp (second form))
                       (= (length (second form)) 2))
                  ;; Native paths are verified only for a single explicit constructor.
                  (let ((option (second (second form))))
                    (when (and (consp option) (eq (first option) :constructor)
                               (second option) (symbolp (second option)))
                      (setf candidates
                            (nconc candidates
                                   (cond
                                     ((= (length option) 2)
                                      (keyword-nil-slot-candidates (cddr form) top))
                                     ((and (= (length option) 3) (listp (third option)))
                                      (key-candidates (third option) (list 2 1 1 top)))))))))
                 ((and (consp form) (member (first form) '(defun progn)))
                  (flet ((collect-defun (definition path)
                           (multiple-value-bind (found macros)
                               (defun-key-candidates definition path)
                             (setf candidates (nconc candidates found))
                             (when macros (setf macro-dependencies macros)))))
                    (if (eq (first form) 'defun)
                        (collect-defun form (list top))
                        ;; One top-level PROGN layer only, with immediate DEFUN children.
                        (loop for definition in (cdr form) for slot from 1
                              do (collect-defun definition (list slot top)))))))))
    (values candidates (nreverse reader-constants) macro-dependencies)))

(defun capture-source (pathname)
  (unless (normalization-supported-p)
    (return-from capture-source nil))
  (let ((bytes (source-bytes pathname)))
    ;; Unsupported reader forms remain completely unnormalized.
    (handler-case
        (multiple-value-bind (candidates reader-constants macro-dependencies) (source-candidates bytes)
          (make-source-proof :bytes bytes :features (copy-list *features*)
                             :candidates candidates :reader-constants reader-constants
                             :macro-dependencies macro-dependencies))
      (reader-error () nil)
      (unsupported-source () nil))))

(defun validate-source (pathname proof)
  (unless (and (equalp (source-proof-bytes proof) (source-bytes pathname))
               (equal (source-proof-features proof) *features*))
    (error "Coverage source or reader features changed: ~A" pathname))
  (dolist (candidate (source-proof-candidates proof))
    (multiple-value-bind (value proven) (proven-key-value (second candidate))
      (unless (and proven (equal value (third candidate)))
        (error "Coverage initializer changed: ~A" pathname))))
  ;; Re-derive the semantic proof, rather than trusting a supplied path list.
  (multiple-value-bind (derived reader-constants macro-dependencies)
      (source-candidates (source-proof-bytes proof))
    (let ((candidates (source-proof-candidates proof)))
      ;; Constants introduced during compilation have no precompile proof.
      (unless (and (equal (source-proof-reader-constants proof) reader-constants)
                   (equalp (source-proof-macro-dependencies proof) macro-dependencies)
                   (= (length candidates)
                      (length (remove-duplicates candidates :test #'equal)))
                   (every (lambda (candidate) (member candidate derived :test #'equal))
                          candidates))
        (error "Coverage semantic proof mismatch: ~A" pathname)))))

(defun seal-source (pathname proof)
  (validate-source pathname proof)
  (let ((file (gethash (namestring (truename pathname))
                       (car sb-c::*code-coverage-info*))))
    (when file
      (setf (source-proof-candidates proof)
            (remove-if-not
             (lambda (candidate)
               (let ((count (count (first candidate) (sb-c::covered-file-paths file)
                                   :test #'equal)))
                 (when (> count 1)
                   (error "Duplicate declaration coverage path: ~A" pathname))
                 (= count 1)))
             (source-proof-candidates proof)))
      (setf (source-proof-paths proof)
            (map 'vector #'copy-tree (sb-c::covered-file-paths file)))
      (setf (gethash (namestring (truename pathname)) *proofs*) proof)))
  proof)

(defun project-file (pathname file proof)
  (when proof
    (validate-source pathname proof)
    (unless (equalp (source-proof-paths proof) (sb-c::covered-file-paths file))
      (error "Coverage native paths changed: ~A" pathname)))
  (let* ((paths (sb-c::covered-file-paths file))
         (bits (sb-c::covered-file-executed file))
         (locations (sb-c::covered-file-locations file))
         (removed nil)
         (retained nil))
    (unless (and (= (length paths) (length bits))
                 (or (null locations) (>= (length locations) (length paths))))
      (error "Unsupported coverage record layout: ~A" pathname))
    (when proof
      (dolist (candidate (source-proof-candidates proof))
        (let ((indices (loop for path across paths for index from 0
                             when (equal path (first candidate)) collect index)))
          (unless (= (length indices) 1)
            (error "Unmatched declaration coverage path: ~A" pathname))
          (when (zerop (sbit bits (first indices)))
            (push (first indices) removed)))))
    (dotimes (index (length paths))
      (unless (member index removed) (push index retained)))
    (setf retained (nreverse retained))
    (let* ((new-paths (map 'vector (lambda (index) (copy-tree (aref paths index)))
                          retained))
           ;; Preserve the location suffix used by SB-COVER's source suppressions.
           (new-locations
             (when locations
               (concatenate 'vector
                            (map 'vector (lambda (index) (aref locations index)) retained)
                            (subseq locations (length paths)))))
           (new-file (sb-c::make-coverage-instrumented-file
                      new-paths new-locations
                      (when (sb-c::covered-file-lines file)
                        (copy-seq (sb-c::covered-file-lines file))))))
      (loop for old in retained for new from 0
            do (setf (sbit (sb-c::covered-file-executed new-file) new)
                     (sbit bits old)))
      (values new-file (length removed)))))

(defun project-table (table proofs)
  ;; No compiler invariant is assumed elsewhere; the raw gate remains intact.
  (unless (normalization-supported-p)
    (return-from project-table (values table 0)))
  (let ((projection (make-hash-table :test (hash-table-test table)))
        (removed 0))
    (maphash (lambda (pathname proof)
               (declare (ignore proof))
               (unless (gethash pathname table)
                 (error "Missing proven coverage source: ~A" pathname)))
             proofs)
    (maphash (lambda (pathname file)
               (multiple-value-bind (projected count)
                   (project-file pathname file (gethash pathname proofs))
                 (setf (gethash pathname projection) projected)
                 (incf removed count)))
             table)
    (values projection removed)))

(defun call-with-projection (proofs function)
  ;; The table is global, not special. Restore its identity even on a report error.
  (let ((original (car sb-c::*code-coverage-info*)))
    (sb-cover::refresh-coverage-bits)
    (multiple-value-bind (projection removed) (project-table original proofs)
      (unwind-protect
           (progn
             (setf (car sb-c::*code-coverage-info*) projection)
             (funcall function removed))
        (setf (car sb-c::*code-coverage-info*) original)))))

(defun call-with-deferred-projection (proofs function)
  ;; Install after tests, but retain one table through HTML, totals, and SAVE.
  (let ((original (car sb-c::*code-coverage-info*))
        (projected nil)
        (installed nil))
    (unwind-protect
         (funcall function
                  (lambda ()
                    (unless (eq (if installed projected original)
                                (car sb-c::*code-coverage-info*))
                      (error "Coverage table changed during deferred projection."))
                    (unless installed
                      (sb-cover::refresh-coverage-bits)
                      (multiple-value-bind (projection removed)
                          (project-table original proofs)
                        (setf projected projection
                              (car sb-c::*code-coverage-info*) projection
                              installed removed)))
                    installed))
      (setf (car sb-c::*code-coverage-info*) original))))
