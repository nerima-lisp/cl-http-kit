(require :asdf)
(asdf:load-system "cl-weave")
(load (merge-pathnames "coverage-normalization.lisp" *load-truename*))
(load (merge-pathnames "coverage-child-merge.lisp" *load-truename*))

(defvar *coverage-code-roots* nil)
(defvar *coverage-pending-proofs* nil)
(defvar *coverage-source-paths* nil)

(progn
  (defparameter *coverage-cli-value-options*
    '("--system" "--load" "--reporter" "--filter" "--output"
      "--watch-interval" "--retry" "--test-timeout-ms" "--max-workers"
      "--shard" "--sequence" "--seed" "--coverage-output"
      "--coverage-report-directory" "--coverage-include" "--coverage-exclude"
      "--coverage-system" "--coverage-min-expression" "--coverage-min-branch"
      "--snapshot-dir" "--snapshot-file" "--random-seed"))
  (defun coverage-option-name (token)
    (let ((equals (position #\= token)))
      (if equals (subseq token 0 equals) token)))
  (defun coverage-option-token-p (token)
    (and (plusp (length token)) (char= #\- (char token 0))))
  (defun coverage-next-cli-arguments (remaining)
    (let* ((token (first remaining))
           (equals (position #\= token))
           (option (coverage-option-name token)))
      (cond
        ((and (null equals)
              (member option *coverage-cli-value-options* :test #'string=))
         (if (rest remaining) (cddr remaining) nil))
        ((and (null equals) (string= option "--bail")
              (rest remaining)
              (not (coverage-option-token-p (second remaining))))
         (cddr remaining))
        (t (rest remaining)))))
  (defun coverage-cli-option-values (arguments flag)
    (labels ((scan (remaining values)
               (if (null remaining)
                   (nreverse values)
                   (let* ((token (first remaining))
                          (equals (position #\= token))
                          (option (coverage-option-name token)))
                     (if (string= option flag)
                         (if equals
                             (scan (rest remaining)
                                   (cons (subseq token (1+ equals)) values))
                             (if (rest remaining)
                                 (scan (cddr remaining)
                                       (cons (second remaining) values))
                                 (nreverse values)))
                         (scan (coverage-next-cli-arguments remaining) values))))))
      (scan arguments nil)))
  (defun coverage-cli-option-present-p (arguments flag)
    (labels ((scan (remaining)
               (cond
                 ((null remaining) nil)
                 ((string= (coverage-option-name (first remaining)) flag) t)
                 (t (scan (coverage-next-cli-arguments remaining))))))
      (scan arguments)))
  (defun coverage-command-token (arguments)
    (let ((arguments
            (if (and arguments (string= (first arguments) "--"))
                (rest arguments)
                arguments)))
      (labels ((scan (remaining)
                 (cond
                   ((null remaining) nil)
                   ((coverage-option-token-p (first remaining))
                    (scan (coverage-next-cli-arguments remaining)))
                   ((member (first remaining)
                            '("run" "list" "watch" "doctor" "metadata"
                              "version" "help")
                            :test #'string=)
                    (first remaining))
                   (t (scan (rest remaining))))))
        (scan arguments))))
  (defun coverage-truthy-p (value)
    (and value
         (plusp (length value))
         (not (member (string-downcase value)
                      '("0" "false" "no" "off" "nil")
                      :test #'string=))))
  (defun coverage-requested-p (arguments)
    (or (coverage-cli-option-present-p arguments "--coverage")
        (coverage-truthy-p (uiop:getenv "CL_WEAVE_COVERAGE"))))
  (defun coverage-run-command-p (arguments)
    (and (coverage-requested-p arguments)
         (not (member (coverage-command-token arguments)
                      '("list" "watch" "doctor" "metadata" "version" "help")
                      :test #'string=))
         (not (coverage-cli-option-present-p arguments "--watch"))
         (not (coverage-cli-option-present-p arguments "--list"))
         (not (coverage-truthy-p (uiop:getenv "CL_WEAVE_WATCH")))
         (not (coverage-truthy-p (uiop:getenv "CL_WEAVE_LIST")))))
  (defun coverage-pathname-asd-file-p (pathname)
    (let ((type (pathname-type pathname)))
      (and type (string-equal type "asd"))))
  (defun coverage-load-file-asd-directory (pathname)
    (let ((resolved (uiop:ensure-pathname pathname)))
      (uiop:pathname-directory-pathname
       (if (coverage-pathname-asd-file-p resolved)
           resolved
           (or (probe-file resolved) resolved)))))
  (defun coverage-system-bootstrap-directories (arguments)
    (remove-duplicates
     (cons (uiop:getcwd)
           (loop for file in (coverage-cli-option-values arguments "--load")
                 collect (coverage-load-file-asd-directory file)))
     :test #'equal))
  (defun coverage-system-bootstrap-asd-files (system arguments)
    (let ((asd-name (format nil "~A.asd" (asdf:primary-system-name system))))
      (remove-duplicates
       (loop for directory in (coverage-system-bootstrap-directories arguments)
             for pathname = (merge-pathnames asd-name directory)
             when (uiop:file-exists-p pathname)
               collect pathname)
       :test #'equal)))
  (defun coverage-ensure-system-visible (system arguments)
    (unless (asdf:find-system system nil)
      (loop for pathname in (coverage-system-bootstrap-asd-files system arguments)
            until (asdf:find-system system nil)
            do (asdf:load-asd pathname)))
    (unless (asdf:find-system system nil)
      (error "Unable to locate ASDF system ~S. Run cl-weave from the project root, pass --load path/to/system.asd, or configure CL_SOURCE_REGISTRY."
             system)))
  (defun coverage-source-paths (arguments)
    (let ((paths (make-hash-table :test (function equal))))
      (dolist (system (coverage-cli-option-values arguments "--coverage-system"))
        (coverage-ensure-system-visible system arguments)
        (dolist (pathname (cl-weave:asdf-system-files system))
          (setf (gethash (namestring (truename pathname)) paths) t)))
      paths))
  (defun coverage-test-location-paths ()
    (loop for system in '("cl-http-kit/test-core" "cl-http-kit/test")
          append
          (mapcar #'asdf:component-pathname
                  (asdf:component-children (asdf:find-system system))))))

(defun coverage-source-p (pathname)
  (gethash (namestring (truename pathname)) *coverage-source-paths*))

(let* ((arguments (uiop:command-line-arguments))
       (*coverage-code-roots* nil)
       (*coverage-pending-proofs* (make-hash-table :test 'equal))
       (*coverage-source-paths* nil)
       (http-coverage-child-merge:*collector*
         (when (http-coverage-child-merge:supported-runtime-p)
           (http-coverage-child-merge:make-collector)))
       (http-coverage-normalization:*proofs* (make-hash-table :test 'equal)))
  (unless (coverage-run-command-p arguments)
    (error "Coverage requires a non-watch test run with --coverage."))
  (asdf:load-asd (merge-pathnames "../cl-http-kit.asd" *load-truename*))
  (unless http-coverage-child-merge:*collector*
    (format *error-output*
            "Child coverage merge: unsupported runtime; native raw coverage retained.~%"))
  (let ((status
          (http-coverage-normalization:call-with-deferred-projection
           http-coverage-normalization:*proofs*
           (lambda (install)
             (cl-weave/cli:run-coverage
              arguments
              :run-options
              (list :coverage-reset nil
                    :location-filter (coverage-test-location-paths))
              :around-input-loading
              (lambda (load-inputs)
                (setf *coverage-source-paths*
                      (coverage-source-paths arguments))
                ;; Reset before loading: load-time forms must remain part of the measurement.
                (sb-cover:reset-coverage)
                (dolist (operation '(asdf:compile-op asdf:load-op))
                  (when (find-method #'asdf:perform '(:around)
                                     (list (find-class operation)
                                           (find-class 'asdf:cl-source-file))
                                     nil)
                    (error "Coverage loading would replace an existing ASDF method.")))
                (let ((methods nil))
                  (unwind-protect
                       (progn
                         (push
                          (eval
                           '(defmethod asdf:perform :around
                                ((operation asdf:compile-op)
                                 (component asdf:cl-source-file))
                              (let* ((pathname (asdf:component-pathname component))
                                     (proof
                                       (when (coverage-source-p pathname)
                                         (http-coverage-normalization:capture-source
                                          pathname))))
                                (multiple-value-prog1 (call-next-method)
                                  (when proof
                                    (setf
                                     (gethash (namestring (truename pathname))
                                              *coverage-pending-proofs*)
                                     proof)))))
                          methods)
                         ;; SBCL stores coverage code weakly; retain records through reporting.
                         (push
                          (eval
                           '(defmethod asdf:perform :around
                                ((operation asdf:load-op)
                                 (component asdf:cl-source-file))
                              (let* ((previous (cdr sb-c::*code-coverage-info*))
                                     (pathname (asdf:component-pathname component))
                                     (load-record
                                       (http-coverage-child-merge:capture-load
                                        component)))
                                (sb-sys:without-gcing
                                  (multiple-value-prog1 (call-next-method)
                                    (loop for tail on (cdr sb-c::*code-coverage-info*)
                                          until (eq tail previous)
                                          do (let ((vector (car tail)))
                                               (dotimes
                                                   (index
                                                    (sb-int:weak-vector-len vector))
                                                 (let ((code
                                                         (sb-int:weak-vector-ref
                                                          vector index)))
                                                   (when code
                                                     (push code
                                                           *coverage-code-roots*))))))
                                    (http-coverage-child-merge:record-load
                                     load-record *coverage-code-roots*)
                                    (let ((proof
                                            (gethash
                                             (namestring (truename pathname))
                                             *coverage-pending-proofs*)))
                                      (when (and proof
                                                 (coverage-source-p pathname))
                                        (http-coverage-normalization:seal-source
                                         pathname proof))))))))
                          methods)
                         (funcall load-inputs))
                    (dolist (method methods)
                      (remove-method #'asdf:perform method))))
              :around-coverage-consumer
              (lambda (kind consume)
                (declare (ignore kind))
                (http-coverage-child-merge:merge-pending)
                (format *error-output*
                        "Coverage declaration projection: ~D expressions.~%"
                        (funcall install))
                (funcall consume)))))))
    ;; A failed coverage consumer must not be hidden by reporter cleanup.
    (http-coverage-child-merge:merge-pending)
    (uiop:quit status)))))
