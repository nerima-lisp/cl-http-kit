(in-package #:http-kit/test)
#+sbcl
(progn
  (defun %https-listener-test-call-with-function-stubs (bindings thunk)
    (let ((originals
            (mapcar (lambda (binding)
                      (cons (car binding) (fdefinition (car binding))))
                    bindings)))
      (unwind-protect
           (progn
             (sb-ext:without-package-locks
               (dolist (binding bindings)
                 (setf (fdefinition (car binding)) (cdr binding))))
             (funcall thunk))
        (sb-ext:without-package-locks
          (dolist (binding originals)
            (setf (fdefinition (car binding)) (cdr binding)))))))

  (deftest https-listener-covers-validation-and-accounting-paths
    (%with-https-listener-test-stubs (context)
      (ensure-true context)
      (dolist (overrides '((:host nil) (:port "443") (:handler nil)))
        (ensure-true
         (handler-case
             (progn (apply #'%https-listener-make-server overrides) nil)
           (http-server-error () t))))
      (ensure-true (http-kit::%http-server-positive-real-p 1.5))
      (ensure-false
       (http-kit::%http-server-positive-real-p
        (sb-kernel:make-single-float #x7fc00000)))
      (ensure-false
       (http-kit::%http-server-positive-real-p
        (sb-kernel:make-single-float #x7f800000)))
      (ensure-false (http-kit::%http-server-positive-real-p 0))
      (let ((store (cl-tls-kit:make-tls13-server-ticket-key-store)))
        (ensure-true
         (http-kit::http-server-p
          (%https-listener-make-server :ticket-key-store store))))
      (let* ((server (%https-listener-make-server))
             (accounting-error nil))
        (setf (%https-listener-test-context-session-action context)
              (lambda (protocol stream handler options)
                (declare (ignore protocol stream handler))
                (setf accounting-error
                      (handler-case
                          (progn
                            (funcall (getf options :release-body-bytes)
                                     "not an integer")
                            nil)
                        (http-server-error (condition) condition)))))
        (http-kit::%http-server-serve-session
         server
         (http-kit::%make-http-server-connection)
         (make-string-input-stream "session")
         :http1 (list nil))
        (ensure-equal
         :body-buffer-accounting
         (http-server-error-reason-code accounting-error)))))

  (deftest https-listener-covers-wakeup-and-worker-failure-paths
    (%with-https-listener-test-stubs (context)
      (ensure-true context)
      (let* ((server (%https-listener-make-server))
             (listener (gensym "LISTENER-"))
             (replacement (gensym "REPLACEMENT-"))
             (wake-mode :error-same))
        (setf (http-kit::http-server-state server) :running
              (http-kit::http-server-listener server) listener
              (http-kit::http-server-supervisor server)
              sb-thread:*current-thread*)
        (%https-listener-test-call-with-function-stubs
         (list
          (cons
           'http-kit/network:wake-http-tcp-listener
           (lambda (target)
             (declare (ignore target))
             (case wake-mode
               (:error-same
                (error "listener wake failure"))
               (:error-changed
                (setf (http-kit::http-server-listener server) replacement)
                (error "listener wake failure"))
               (:success-changed
                (setf (http-kit::http-server-listener server) replacement))
               (:success-same nil)))))
         (lambda ()
           (ensure-true
            (handler-case
                (progn (http-kit::%http-server-wake-listener server) nil)
              (error () t)))
           (ensure-false (http-kit::http-server-listener-wakeup-p server))
           (ensure-false
            (http-kit::http-server-listener-wake-in-progress-p server))
           (setf wake-mode :error-changed
                 (http-kit::http-server-listener server) listener)
           (ensure-true
            (handler-case
                (progn (http-kit::%http-server-wake-listener server) nil)
              (error () t)))
           (ensure-true (http-kit::http-server-listener-wakeup-p server))
           (ensure-true
            (http-kit::http-server-listener-wake-in-progress-p server))
           (setf wake-mode :success-changed
                 (http-kit::http-server-listener server) listener
                 (http-kit::http-server-listener-wakeup-p server) nil
                 (http-kit::http-server-listener-wake-in-progress-p server) nil)
           (http-kit::%http-server-wake-listener server)
           (ensure-equal replacement (http-kit::http-server-listener server))
           (ensure-true
            (http-kit::http-server-listener-wake-in-progress-p server))
           (setf wake-mode :success-same
                 (http-kit::http-server-listener-wakeup-p server) nil
                 (http-kit::http-server-listener-wake-in-progress-p server) nil)
           (http-kit::%http-server-wake-listener server)
           (ensure-true (http-kit::http-server-listener-wakeup-p server))
           (ensure-false
            (http-kit::http-server-listener-wake-in-progress-p server)))))
      (let* ((server
               (%https-listener-make-server
                :on-error
                (lambda (condition details)
                  (declare (ignore condition details))
                  (error "worker error callback failure"))))
             (stream (make-string-input-stream "worker"))
             (connection
               (http-kit::%make-http-server-connection :raw-stream stream)))
        (%https-listener-test-call-with-function-stubs
         (list
          (cons 'sb-thread:make-thread
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (error "worker thread creation failure"))))
         (lambda ()
           (http-kit::%http-server-start-connection-worker server connection)))
        (ensure-equal
         :error-callback-failed
         (http-server-error-reason-code (http-server-failure server)))
        (ensure-false (open-stream-p stream)))))

  (deftest https-listener-covers-supervisor-exit-and-accept-paths
    (%with-https-listener-test-stubs (context)
      (ensure-true context)
      (let ((stopped (%https-listener-make-server))
            (unexpected (%https-listener-make-server))
            (preserved (%https-listener-make-server))
            (existing-failure
              (make-condition 'http-server-error
                              :reason-code :existing
                              :phase :test)))
        (setf (http-kit::http-server-state stopped) :stopped
              (http-kit::http-server-state unexpected) :running
              (http-kit::http-server-state preserved) :running
              (http-kit::http-server-failure-value preserved) existing-failure
              (http-kit::http-server-stop-deadline preserved) 123.0d0)
        (%https-listener-test-call-with-function-stubs
         (list
          (cons 'sb-thread:make-thread
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  sb-thread:*current-thread*)))
         (lambda ()
           (ensure-false
            (http-kit::%http-server-ensure-shutdown-coordinator stopped))
           (http-kit::%http-server-supervisor-exited stopped)
           (http-kit::%http-server-supervisor-exited unexpected)
           (http-kit::%http-server-supervisor-exited preserved)
           (ensure-equal sb-thread:*current-thread*
                         (http-kit::http-server-shutdown-coordinator unexpected))
           (ensure-equal sb-thread:*current-thread*
                         (http-kit::http-server-shutdown-coordinator preserved))))
        (ensure-equal :stopped (http-kit::http-server-state stopped))
        (ensure-equal :failed (http-kit::http-server-state unexpected))
        (ensure-equal :server-failed
                      (http-server-error-reason-code
                       (http-server-failure unexpected)))
        (ensure-true
         (plusp (http-kit::http-server-stop-deadline unexpected)))
        (ensure-equal existing-failure (http-server-failure preserved))
        (ensure-equal 123.0d0
                      (http-kit::http-server-stop-deadline preserved)))
      (let* ((server (%https-listener-make-server))
             (stream (make-string-input-stream "force close"))
             (connection
               (http-kit::%make-http-server-connection :raw-stream stream)))
        (setf (gethash connection (http-kit::http-server-connections server)) t
              (http-kit::http-server-connection-count server) 1)
        (http-kit::%http-server-force-close-connections server)
        (ensure-false (open-stream-p stream)))
      (let ((server (%https-listener-make-server)))
        (setf (http-kit::http-server-state server) :stopping)
        (http-kit::%http-server-supervisor server nil)
        (ensure-equal :stopping (http-kit::http-server-state server)))
      (let ((server (%https-listener-make-server)))
        (setf (http-kit::http-server-state server) :running)
        (%https-listener-test-call-with-function-stubs
         (list
          (cons 'http-kit/network:accept-http-tcp-stream
                (lambda (listener)
                  (declare (ignore listener))
                  (error "accept failure"))))
         (lambda ()
           (http-kit::%http-server-supervisor server :listener)))
        (ensure-equal :failed (http-kit::http-server-state server))
        (ensure-equal :connection-error
                      (http-server-error-reason-code
                       (http-server-failure server))))
      (let ((server
              (%https-listener-make-server
               :on-error
               (lambda (condition details)
                 (declare (ignore condition details))
                 (error "supervisor error callback failure")))))
        (setf (http-kit::http-server-state server) :running)
        (%https-listener-test-call-with-function-stubs
         (list
          (cons 'http-kit::%http-server-running-p
                (lambda (target)
                  (declare (ignore target))
                  (error "supervisor running check failure"))))
         (lambda ()
           (http-kit::%http-server-supervisor server :listener)))
        (ensure-equal :failed (http-kit::http-server-state server))
        (ensure-equal
         :error-callback-failed
         (http-server-error-reason-code (http-server-failure server))))))

  (deftest https-listener-covers-shutdown-coordinator-paths
    (%with-https-listener-test-stubs (context)
      (ensure-true context)
      (let ((server (%https-listener-make-server))
            (waited nil))
        (setf (http-kit::http-server-state server) :stopping
              (http-kit::http-server-listener-wake-in-progress-p server) t)
        (%https-listener-test-call-with-function-stubs
         (list
          (cons 'sb-thread:condition-wait
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (setf waited t
                        (http-kit::http-server-listener-wake-in-progress-p server)
                        nil))))
         (lambda ()
           (http-kit::%http-server-shutdown-coordinator server)))
        (ensure-true waited)
        (ensure-equal :stopped (http-kit::http-server-state server))
        (ensure-false (http-kit::http-server-shutdown-coordinator server)))
      (let* ((server (%https-listener-make-server))
             (supervisor (sb-thread:make-thread (lambda () nil)))
             (listener (gensym "LISTENER-"))
             (replacement (gensym "REPLACEMENT-"))
             (stream (make-string-input-stream "active"))
             (connection
               (http-kit::%make-http-server-connection :raw-stream stream)))
        (setf (http-kit::http-server-state server) :failed
              (http-kit::http-server-supervisor server) supervisor
              (http-kit::http-server-listener server) listener
              (http-kit::http-server-shutdown-coordinator server) :other
              (http-kit::http-server-stop-deadline server) 0.0d0
              (http-kit::http-server-connection-count server) 1
              (gethash connection (http-kit::http-server-connections server)) t)
        (%https-listener-test-call-with-function-stubs
         (list
          (cons 'http-kit/network:close-http-tcp-listener
                (lambda (target)
                  (declare (ignore target))
                  (setf (http-kit::http-server-listener server) replacement
                        (http-kit::http-server-supervisor server) :replaced)
                  nil)))
         (lambda ()
           (http-kit::%http-server-shutdown-coordinator server)))
        (sb-thread:join-thread supervisor)
        (ensure-false (open-stream-p stream))
        (ensure-equal :failed (http-kit::http-server-state server))
        (ensure-equal replacement (http-kit::http-server-listener server))
        (ensure-equal :replaced (http-kit::http-server-supervisor server))
        (ensure-equal :other
                      (http-kit::http-server-shutdown-coordinator server)))
      (let ((server (%https-listener-make-server)))
        (setf (http-kit::http-server-state server) :stopping
              (http-kit::http-server-shutdown-coordinator server)
              sb-thread:*current-thread*)
        (%https-listener-test-call-with-function-stubs
         (list
          (cons 'http-kit::%http-server-emit-event-once
                (lambda (target event-name event)
                  (declare (ignore target event-name event))
                  (error "shutdown event failure"))))
         (lambda ()
           (ensure-true
            (handler-case
                (progn
                  (http-kit::%http-server-shutdown-coordinator server)
                  nil)
              (error () t)))))
        (ensure-false
         (http-kit::http-server-shutdown-coordinator server))))

  (deftest https-listener-covers-start-and-stop-state-paths
    (%with-https-listener-test-stubs (context)
      (ensure-true context)
      (ensure-true
       (handler-case
           (progn (start-http-server nil) nil)
         (http-server-error () t)))
      (ensure-true
       (handler-case
           (progn (stop-http-server nil) nil)
         (http-server-error () t)))
      (let ((running (%https-listener-make-server))
            (failed (%https-listener-make-server)))
        (setf (http-kit::http-server-state running) :running
              (http-kit::http-server-state failed) :failed)
        (ensure-equal running (start-http-server running))
        (ensure-equal
         :server-failed
         (http-server-error-reason-code
          (handler-case
              (progn (start-http-server failed) nil)
            (http-server-error (condition) condition)))))
      (let ((server (%https-listener-make-server)))
        (%https-listener-test-call-with-function-stubs
         (list
          (cons 'http-kit/network:open-http-tcp-listener
                (lambda (&key host port)
                  (declare (ignore host port))
                  (setf (http-kit::http-server-state server) :stopping)
                  (error "listener failed after shutdown began"))))
         (lambda ()
           (ensure-equal server (start-http-server server))))
        (ensure-equal :stopping (http-kit::http-server-state server)))
      (let ((server (%https-listener-make-server)))
        (%https-listener-test-call-with-function-stubs
         (list
          (cons 'http-kit/network:open-http-tcp-listener
                (lambda (&key host port)
                  (declare (ignore host port))
                  (setf (http-kit::http-server-start-token server) :stale)
                  (error "listener failed after start was superseded"))))
         (lambda ()
           (ensure-equal server (start-http-server server))))
        (ensure-equal :starting (http-kit::http-server-state server))
        (ensure-equal :stale (http-kit::http-server-start-token server)))
      (let ((server (%https-listener-make-server))
            (observed nil))
        (%https-listener-test-call-with-function-stubs
         (list
          (cons 'http-kit/network:open-http-tcp-listener
                (lambda (&key host port)
                  (declare (ignore host port))
                  (setf (http-kit::http-server-stop-deadline server) 19.0d0)
                  (error "listener open failure"))))
         (lambda ()
           (setf observed
                 (handler-case
                     (progn (start-http-server server) nil)
                   (http-server-error (condition) condition)))))
        (ensure-true observed)
        (ensure-equal :failed (http-kit::http-server-state server))
        (ensure-equal 19.0d0
                      (http-kit::http-server-stop-deadline server)))
      (let ((server (%https-listener-make-server))
            (listener (gensym "STALE-LISTENER-"))
            (closed nil))
        (%https-listener-test-call-with-function-stubs
         (list
          (cons 'http-kit/network:open-http-tcp-listener
                (lambda (&key host port)
                  (declare (ignore host port))
                  (setf (http-kit::http-server-state server) :stopping)
                  listener))
          (cons 'http-kit/network:close-http-tcp-listener
                (lambda (target)
                  (declare (ignore target))
                  (setf closed t))))
         (lambda ()
           (ensure-equal server (start-http-server server))))
        (ensure-true closed)
        (ensure-equal :stopping (http-kit::http-server-state server)))
      (let ((server (%https-listener-make-server))
            (listener (gensym "STALE-TOKEN-LISTENER-"))
            (closed nil))
        (%https-listener-test-call-with-function-stubs
         (list
          (cons 'http-kit/network:open-http-tcp-listener
                (lambda (&key host port)
                  (declare (ignore host port))
                  (setf (http-kit::http-server-start-token server) :stale)
                  listener))
          (cons 'http-kit/network:close-http-tcp-listener
                (lambda (target)
                  (declare (ignore target))
                  (setf closed t))))
         (lambda ()
           (ensure-equal server (start-http-server server))))
        (ensure-true closed)
        (ensure-equal :starting (http-kit::http-server-state server)))
      (let ((server
              (%https-listener-make-server
               :on-error
               (lambda (condition details)
                 (declare (ignore condition details))
                 (error "startup error callback failure"))))
            (listener (gensym "STARTUP-LISTENER-"))
            (closed nil))
        (%https-listener-test-call-with-function-stubs
         (list
          (cons 'http-kit/network:open-http-tcp-listener
                (lambda (&key host port)
                  (declare (ignore host port))
                  listener))
          (cons 'http-kit/network:http-network-listener-port
                (lambda (target)
                  (declare (ignore target))
                  443))
          (cons 'http-kit/network:close-http-tcp-listener
                (lambda (target)
                  (declare (ignore target))
                  (setf closed t)))
          (cons 'sb-thread:make-thread
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (error "supervisor thread creation failure"))))
         (lambda ()
           (start-http-server server)))
        (ensure-true closed)
        (ensure-equal :failed (http-kit::http-server-state server))
        (ensure-equal
         :error-callback-failed
         (http-server-error-reason-code (http-server-failure server))))
      (let ((server (%https-listener-make-server)))
        (setf (http-kit::http-server-state server) :stopped)
        (ensure-equal server (stop-http-server server)))
      (let ((server (%https-listener-make-server)))
        (setf (http-kit::http-server-state server) :failed)
        (%https-listener-test-call-with-function-stubs
         (list
          (cons 'http-kit::%http-server-ensure-shutdown-coordinator
                (lambda (target)
                  (declare (ignore target))
                  nil)))
         (lambda ()
           (ensure-equal server (stop-http-server server))))
        (ensure-equal :failed (http-kit::http-server-state server))
        (ensure-true (http-kit::http-server-shutdown-p server))
        (ensure-true (http-kit::http-server-stop-deadline server)))
      (let ((supervisor-self (%https-listener-make-server))
            (coordinator-self (%https-listener-make-server)))
        (setf (http-kit::http-server-state supervisor-self) :stopping
              (http-kit::http-server-shutdown-p supervisor-self) t
              (http-kit::http-server-stop-deadline supervisor-self) 1.0d0
              (http-kit::http-server-supervisor supervisor-self)
              sb-thread:*current-thread*
              (http-kit::http-server-state coordinator-self) :stopping
              (http-kit::http-server-shutdown-p coordinator-self) t
              (http-kit::http-server-stop-deadline coordinator-self) 1.0d0
              (http-kit::http-server-supervisor coordinator-self) :other)
        (%https-listener-test-call-with-function-stubs
         (list
          (cons 'http-kit::%http-server-ensure-shutdown-coordinator
                (lambda (target)
                  (declare (ignore target))
                  sb-thread:*current-thread*)))
         (lambda ()
           (ensure-equal supervisor-self
                         (stop-http-server supervisor-self))
           (ensure-equal coordinator-self
                         (stop-http-server coordinator-self)))))
      (let ((server
              (%https-listener-make-server
               :on-event
               (lambda (event)
                 (when (eq :listener-stopping (getf event :type))
                   (error "stopping event callback failure")))))
            (observed nil))
        (setf observed
              (handler-case
                  (progn (stop-http-server server) nil)
                (http-server-callback-error (condition) condition)))
        (ensure-true observed)
        (ensure-equal
         :event-callback-failed
         (http-server-error-reason-code (http-server-failure server)))))))
)
