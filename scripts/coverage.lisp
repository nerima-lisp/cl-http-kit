(require :asdf)
(asdf:load-system "cl-weave")
(load (merge-pathnames "coverage-normalization.lisp" *load-truename*))
(load (merge-pathnames "coverage-child-merge.lisp" *load-truename*))

(defvar *coverage-code-roots* nil)
(defvar *coverage-pending-proofs* nil)

(let ((options (cl-weave/cli::parse-cli-arguments
                (uiop:command-line-arguments)))
      (*coverage-code-roots* nil)
      (*coverage-pending-proofs* (make-hash-table :test 'equal))
      (http-coverage-child-merge:*collector*
        (when (http-coverage-child-merge:supported-runtime-p)
          (http-coverage-child-merge:make-collector)))
      (http-coverage-normalization:*proofs* (make-hash-table :test 'equal)))
  (unless (and (cl-weave/cli::cli-options-coverage options)
               (eq (cl-weave/cli::command-dispatch-kind options) :run))
    (error "Coverage requires a non-watch test run with --coverage."))
  (cl-weave/cli::ensure-valid-reporter-for-command options)
  (cl-weave/cli::prepare-coverage-compilation options)
  (unless http-coverage-child-merge:*collector*
    (format *error-output* "Child coverage merge: unsupported runtime; native raw coverage retained.~%"))
  ;; Reset before loading: load-time forms must remain part of the measurement.
  (sb-cover:reset-coverage)
  (dolist (operation '(asdf:compile-op asdf:load-op))
    (when (find-method #'asdf:perform '(:around)
                       (list (find-class operation)
                             (find-class 'asdf:cl-source-file)) nil)
      (error "Coverage loading would replace an existing ASDF method.")))
  (let ((methods nil))
    (unwind-protect
         (progn
           (push
            (eval
             '(defmethod asdf:perform :around
                  ((operation asdf:compile-op) (component asdf:cl-source-file))
                (let* ((pathname (asdf:component-pathname component))
                       (proof (http-coverage-normalization:capture-source pathname)))
                  (multiple-value-prog1 (call-next-method)
                    (when proof
                      (setf (gethash (namestring (truename pathname))
                                     *coverage-pending-proofs*) proof))))))
            methods)
           ;; SBCL stores coverage code weakly; retain records through reporting.
           (push
            (eval
             '(defmethod asdf:perform :around
                  ((operation asdf:load-op) (component asdf:cl-source-file))
                (let* ((previous (cdr sb-c::*code-coverage-info*))
                       (pathname (asdf:component-pathname component))
                       (load-record (http-coverage-child-merge:capture-load component)))
                  (sb-sys:without-gcing
                    (multiple-value-prog1 (call-next-method)
                      (loop for tail on (cdr sb-c::*code-coverage-info*)
                            until (eq tail previous)
                            do (let ((vector (car tail)))
                                 (dotimes (index (sb-int:weak-vector-len vector))
                                   (let ((code (sb-int:weak-vector-ref vector index)))
                                     (when code (push code *coverage-code-roots*))))))
                      (http-coverage-child-merge:record-load load-record *coverage-code-roots*)
                      (let ((proof (gethash (namestring (truename pathname))
                                            *coverage-pending-proofs*)))
                        (when proof
                          (http-coverage-normalization:seal-source pathname proof))))))))
            methods)
           (cl-weave/cli::load-requested-inputs options))
      (dolist (method methods) (remove-method #'asdf:perform method))))
  (cl-weave/cli::with-cli-snapshot-settings (options)
    (cl-weave/cli::with-cli-journal-settings (options)
      ;; QUIT is outside the unwind-protect that restores the native table.
      (let ((success
              (http-coverage-normalization:call-with-deferred-projection
               http-coverage-normalization:*proofs*
               (lambda (install)
                 (let ((consumers
                         (loop for symbol in '(cl-weave::save-coverage-report
                                               cl-weave::coverage-statistics
                                               cl-weave::save-coverage)
                               collect (cons symbol (fdefinition symbol)))))
                   (unwind-protect
                        (progn
                          ;; Check each consumer, including totals-only and data-only runs.
                          (dolist (consumer consumers)
                            (let ((function (cdr consumer)))
                              (setf (fdefinition (car consumer))
                                    (lambda (&rest arguments)
                                      (http-coverage-child-merge:merge-pending)
                                      (format *error-output*
                                              "Coverage declaration projection: ~D expressions.~%"
                                              (funcall install))
                                      (apply function arguments)))))
                          (cl-weave/cli::call-with-output-stream
                           options
                           (lambda (stream)
                             (apply #'cl-weave:run-all
                                    (append
                                     (cl-weave/cli::run-execution-argument-pairs options)
                                     (list :stream stream
                                           :coverage-reset nil
                                           :location-filter
                                           (loop for system in '("cl-http-kit/test-core"
                                                                 "cl-http-kit/test")
                                                 append
                                                 (mapcar #'asdf:component-pathname
                                                         (asdf:component-children
                                                          (asdf:find-system system))))))))))
                     (dolist (consumer consumers)
                       (setf (fdefinition (car consumer)) (cdr consumer)))))))))
        ;; A failed coverage consumer must not be hidden by reporter cleanup.
        (http-coverage-child-merge:merge-pending)
        (uiop:quit (if success 0 1))))))
