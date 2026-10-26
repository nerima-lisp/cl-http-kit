(in-package #:http-kit/test)

(progn
  #+sbcl
  (deftest https-listener-covers-shutdown-deadline-waits
    (%with-https-listener-test-stubs (context)
      (ensure-true context)
      (let* ((server (%https-listener-make-server))
             (http-kit::*http-server-clock-function* (lambda () 10.0d0))
             (releaser
               (sb-thread:make-thread
                (lambda ()
                  (sleep 0.02d0)
                  (http-kit::%with-http-server-lock (server)
                    (http-kit::%http-server-broadcast server))))))
        (http-kit::%http-server-wait-for-listener-wakeup server)
        (sb-thread:join-thread releaser)
        (http-kit::%with-http-server-lock (server)
          (http-kit::%http-server-wait-for-listener-wakeup server 10.01d0))
        (http-kit::%http-server-wait-for-listener-wakeup server 10.0d0))
      (let* ((server (%https-listener-make-server))
             (clock-values '(10.0d0 10.02d0))
             (http-kit::*http-server-clock-function*
               (lambda () (pop clock-values))))
        (setf (http-kit::http-server-connection-count server) 1)
        (ensure-false
         (http-kit::%http-server-wait-for-connections server 10.01d0))
        (setf (http-kit::http-server-connection-count server) 0)
        (ensure-true
         (http-kit::%http-server-wait-for-connections server 10.01d0)))
      (let* ((server (%https-listener-make-server))
             (http-kit::*http-server-clock-function* (lambda () 10.0d0))
             (releaser
               (sb-thread:make-thread
                (lambda ()
                  (sleep 0.02d0)
                  (http-kit::%with-http-server-lock (server)
                    (setf (http-kit::http-server-connection-count server) 0)
                    (http-kit::%http-server-broadcast server))))))
        (setf (http-kit::http-server-connection-count server) 1)
        (ensure-true
         (http-kit::%http-server-wait-for-connections server nil))
        (sb-thread:join-thread releaser))))

  #+sbcl
  (deftest https-listener-covers-worker-reaping-and-coordinator-request
    (%with-https-listener-test-stubs (context)
      (ensure-true context)
      (let* ((server (%https-listener-make-server))
             (connection (http-kit::%make-http-server-connection))
             (finished-worker (sb-thread:make-thread (lambda () nil)))
             (coordinator-requests 0))
        (sb-thread:join-thread finished-worker)
        (setf (http-kit::http-server-state server) :stopping
              (http-kit::http-server-shutdown-p server) t
              (gethash finished-worker (http-kit::http-server-workers server)) t)
        (%https-listener-test-call-with-function-stubs
         (list
          (cons 'http-kit::%http-server-run-connection
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  nil))
          (cons 'http-kit::%http-server-ensure-shutdown-coordinator
                (lambda (target)
                  (declare (ignore target))
                  (incf coordinator-requests))))
         (lambda ()
           (let ((worker
                   (http-kit::%http-server-start-connection-worker
                    server connection)))
             (sb-thread:join-thread worker)
             (ensure-false
              (gethash finished-worker (http-kit::http-server-workers server)))
             (ensure-false
              (gethash worker (http-kit::http-server-workers server))))))
        (ensure-equal 1 coordinator-requests))
      (let* ((server (%https-listener-make-server))
             (connection (http-kit::%make-http-server-connection))
             (supervisor (sb-thread:make-thread (lambda () nil)))
             (coordinator-requests 0))
        (sb-thread:join-thread supervisor)
        (setf (http-kit::http-server-state server) :stopping
              (http-kit::http-server-shutdown-p server) t
              (http-kit::http-server-supervisor server) supervisor)
        (%https-listener-test-call-with-function-stubs
         (list
          (cons 'http-kit::%http-server-run-connection
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  nil))
          (cons 'http-kit::%http-server-ensure-shutdown-coordinator
                (lambda (target)
                  (declare (ignore target))
                  (incf coordinator-requests))))
         (lambda ()
           (sb-thread:join-thread
            (http-kit::%http-server-start-connection-worker
             server connection))))
        (ensure-equal 1 coordinator-requests))
      (let* ((server (%https-listener-make-server))
             (supervisor-gate (sb-thread:make-semaphore :count 0))
             (supervisor
               (sb-thread:make-thread
                (lambda ()
                  (sb-thread:wait-on-semaphore supervisor-gate))))
             (connection (http-kit::%make-http-server-connection)))
        (setf (http-kit::http-server-state server) :stopping
              (http-kit::http-server-shutdown-p server) t
              (http-kit::http-server-supervisor server) supervisor)
        (unwind-protect
             (%https-listener-test-call-with-function-stubs
              (list
               (cons 'http-kit::%http-server-run-connection
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       nil))
               (cons 'http-kit::%http-server-ensure-shutdown-coordinator
                     (lambda (target)
                       (declare (ignore target))
                       (error "Unexpected coordinator request."))))
              (lambda ()
                (sb-thread:join-thread
                 (http-kit::%http-server-start-connection-worker
                  server connection))))
          (sb-thread:signal-semaphore supervisor-gate)
          (sb-thread:join-thread supervisor)))))

  #+sbcl
  (deftest https-listener-covers-shutdown-coordinator-retry-and-worker-branches
    (%with-https-listener-test-stubs (context)
      (ensure-true context)
      (let* ((http-kit::*http-server-clock-function* (lambda () 10.0d0))
             (release-mode :released)
             (supervisor-to-stop nil)
             (supervisor-gate nil)
             (coordinator-requests 0))
        (%https-listener-test-call-with-function-stubs
         (list
          (cons 'http-kit::%http-server-release-owned-ticket-key-store
                (lambda (server)
                  (case release-mode
                    (:released t)
                    (:not-released nil)
                    (:stop-supervisor
                     (let ((late-worker
                             (sb-thread:make-thread (lambda () nil))))
                       (sb-thread:join-thread late-worker)
                       (setf (gethash
                              late-worker
                              (http-kit::http-server-workers server))
                             t))
                     (sb-thread:signal-semaphore supervisor-gate)
                     (sb-thread:join-thread supervisor-to-stop)
                     t))))
          (cons 'http-kit::%http-server-emit-event-once
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  nil))
          (cons 'http-kit::%http-server-ensure-shutdown-coordinator
                (lambda (server)
                  (declare (ignore server))
                  (incf coordinator-requests))))
         (lambda ()
           (let* ((server (%https-listener-make-server))
                  (supervisor (sb-thread:make-thread (lambda () nil)))
                  (worker (sb-thread:make-thread (lambda () nil))))
             (sb-thread:join-thread supervisor)
             (sb-thread:join-thread worker)
             (setf release-mode :released
                   (http-kit::http-server-state server) :running
                   (http-kit::http-server-stop-deadline server) 20.0d0
                   (http-kit::http-server-supervisor server) supervisor
                   (http-kit::http-server-shutdown-coordinator server)
                   sb-thread:*current-thread*
                   (gethash worker (http-kit::http-server-workers server)) t)
             (http-kit::%http-server-shutdown-coordinator server)
             (ensure-equal :stopped (http-kit::http-server-state server))
             (ensure-false (gethash worker (http-kit::http-server-workers server)))
             (ensure-false (http-kit::http-server-supervisor server)))
           (let* ((server (%https-listener-make-server))
                  (supervisor-gate (sb-thread:make-semaphore :count 0))
                  (worker-gate (sb-thread:make-semaphore :count 0))
                  (supervisor
                    (sb-thread:make-thread
                     (lambda ()
                       (sb-thread:wait-on-semaphore supervisor-gate))))
                  (worker
                    (sb-thread:make-thread
                     (lambda ()
                       (sb-thread:wait-on-semaphore worker-gate))))
                  (old-coordinator :other))
             (unwind-protect
                  (progn
                    (setf release-mode :released
                          (http-kit::http-server-state server) :stopping
                          (http-kit::http-server-shutdown-p server) t
                          (http-kit::http-server-stop-deadline server) 9.0d0
                          (http-kit::http-server-supervisor server) supervisor
                          (http-kit::http-server-shutdown-coordinator server)
                          old-coordinator
                          (gethash worker
                                   (http-kit::http-server-workers server)) t)
                    (http-kit::%http-server-shutdown-coordinator server)
                    (ensure-equal :stopping (http-kit::http-server-state server))
                    (ensure-equal supervisor
                                  (http-kit::http-server-supervisor server))
                    (ensure-true
                     (gethash worker
                              (http-kit::http-server-workers server))))
               (sb-thread:signal-semaphore worker-gate)
               (sb-thread:signal-semaphore supervisor-gate)
               (sb-thread:join-thread worker)
               (sb-thread:join-thread supervisor)))
           (let* ((server (%https-listener-make-server))
                  (worker sb-thread:*current-thread*))
             (setf release-mode :released
                   (http-kit::http-server-state server) :stopping
                   (http-kit::http-server-shutdown-p server) t
                   (http-kit::http-server-stop-deadline server) 9.0d0
                   (http-kit::http-server-supervisor server) worker
                   (http-kit::http-server-shutdown-coordinator server) worker
                   (gethash worker (http-kit::http-server-workers server)) t)
             (http-kit::%http-server-shutdown-coordinator server)
             (ensure-equal :stopping (http-kit::http-server-state server))
             (ensure-false (http-kit::http-server-supervisor server))
             (ensure-true
              (gethash worker (http-kit::http-server-workers server))))
           (let* ((server (%https-listener-make-server))
                  (worker sb-thread:*current-thread*))
             (setf release-mode :released
                   (http-kit::http-server-state server) :failed
                   (http-kit::http-server-shutdown-p server) t
                   (http-kit::http-server-stop-deadline server) 9.0d0
                   (http-kit::http-server-supervisor server) worker
                   (http-kit::http-server-shutdown-coordinator server) worker
                   (gethash worker (http-kit::http-server-workers server)) t)
             (http-kit::%http-server-shutdown-coordinator server)
             (ensure-equal :failed (http-kit::http-server-state server)))
           (let* ((server (%https-listener-make-server))
                  (supervisor sb-thread:*current-thread*)
                  (cleanup-error
                    (make-condition 'simple-error
                                    :format-control "store cleanup failed"
                                    :format-arguments nil))
                  (worker sb-thread:*current-thread*))
             (setf release-mode :released
                   (http-kit::http-server-state server) :stopping
                   (http-kit::http-server-shutdown-p server) t
                   (http-kit::http-server-stop-deadline server) 9.0d0
                   (http-kit::http-server-supervisor server) supervisor
                   (http-kit::http-server-shutdown-coordinator server) worker
                   (http-kit::http-server-ticket-key-store-cleanup-error
                    server) cleanup-error
                   (gethash worker (http-kit::http-server-workers server)) t)
             (http-kit::%http-server-shutdown-coordinator server)
             (ensure-equal cleanup-error
                           (http-kit::http-server-ticket-key-store-cleanup-error
                            server))
             (ensure-equal 0 coordinator-requests))
           (let* ((server (%https-listener-make-server))
                  (local-gate (sb-thread:make-semaphore :count 0))
                  (supervisor
                    (sb-thread:make-thread
                     (lambda ()
                       (sb-thread:wait-on-semaphore local-gate)))))
             (setf supervisor-to-stop supervisor
                   supervisor-gate local-gate
                   release-mode :stop-supervisor
                   (http-kit::http-server-state server) :stopping
                   (http-kit::http-server-shutdown-p server) t
                   (http-kit::http-server-stop-deadline server) 9.0d0
                   (http-kit::http-server-supervisor server) supervisor
                   (http-kit::http-server-shutdown-coordinator server)
                   sb-thread:*current-thread*)
             (http-kit::%http-server-shutdown-coordinator server)
             (ensure-equal 1 coordinator-requests)
             (ensure-equal :stopping (http-kit::http-server-state server))
             (ensure-false (sb-thread:thread-alive-p supervisor)))
           (let ((server (%https-listener-make-server)))
             (setf release-mode :not-released
                   (http-kit::http-server-state server) :stopping
                   (http-kit::http-server-shutdown-p server) t
                   (http-kit::http-server-stop-deadline server) 9.0d0
                   (http-kit::http-server-supervisor server)
                   sb-thread:*current-thread*
                   (http-kit::http-server-shutdown-coordinator server)
                   sb-thread:*current-thread*)
             (http-kit::%http-server-shutdown-coordinator server)
             (ensure-false (http-kit::http-server-shutdown-p server))
             (ensure-false (http-kit::http-server-stop-deadline server))))))))
)

#+sbcl
(progn
  (deftest https-listener-covers-cleanup-error-callback-failure
    (%with-https-listener-test-stubs (context)
      (ensure-true context)
      (let* ((store nil)
             (callback-condition nil)
             (server
               (%https-listener-make-server
                :on-error
                (lambda (condition details)
                  (declare (ignore details))
                  (setf callback-condition condition)
                  (error "injected ticket-store error callback failure"))))
             (original-destroyer
               (fdefinition
                (find-symbol
                 "DESTROY-TLS13-SERVER-TICKET-KEY-STORE"
                 "CL-TLS-KIT")))
             (destroy-attempts 0)
             (stop-error nil))
        (setf (http-kit::http-server-ticket-key-store-owned-p server) t)
        (unwind-protect
            (progn
              (setf store (cl-tls-kit:make-tls13-server-ticket-key-store))
              (setf (http-kit::http-server-ticket-key-store server) store)
              (%https-listener-test-call-with-function-stubs
               (list
                (cons
                 (find-symbol
                  "DESTROY-TLS13-SERVER-TICKET-KEY-STORE"
                  "CL-TLS-KIT")
                 (lambda (candidate)
                   (declare (ignore candidate))
                   (incf destroy-attempts)
                   (error "injected ticket-store destruction failure"))))
               (lambda ()
                 (setf stop-error
                       (handler-case
                           (progn
                             (stop-http-server server :graceful-timeout 1)
                             nil)
                         (error (condition) condition)))))
              (ensure-true callback-condition)
              (ensure-equal
               :ticket-key-store-destroy-failed
               (http-server-error-reason-code callback-condition))
              (ensure-true stop-error)
              (ensure-equal
               :ticket-key-store-destroy-failed
               (http-server-error-reason-code stop-error))
              (ensure-equal
               :error-callback-failed
               (http-server-error-reason-code (http-server-failure server)))
              (ensure-equal
               :ticket-key-store-destroy-failed
               (http-server-error-reason-code
                (http-kit::http-server-ticket-key-store-cleanup-error server)))
              (ensure-true
               (eq store (http-kit::http-server-ticket-key-store server)))
              (ensure-equal 1 destroy-attempts)
              (ensure-equal :failed (http-kit::http-server-state server)))
          (when store (http-kit::%with-http-cleanup
              (funcall original-destroyer store)))))))

  (defclass %https-listener-close-hook-stream (binary-session-stream)
    ((close-hook :initarg :close-hook :reader %https-listener-close-hook-stream-hook)))

  (defmethod close ((stream %https-listener-close-hook-stream) &key abort)
    (declare (ignore abort))
    (funcall (%https-listener-close-hook-stream-hook stream))
    (call-next-method)
    (error "injected close failure after concurrent abort"))

  (deftest https-listener-covers-close-error-after-concurrent-abort
    (%with-https-listener-test-stubs (context)
      (ensure-true context)
      (let* ((server (%https-listener-make-server))
             (connection nil)
             (close-count 0)
             (stream
               (make-instance
                '%https-listener-close-hook-stream
                :input (octets)
                :close-hook
                (lambda ()
                  (incf close-count)
                  (http-kit::%with-http-server-lock (server)
                    (setf
                     (http-kit::%http-server-connection-closed-p connection)
                     t))))))
        (setf connection
              (http-kit::%make-http-server-connection
               :tls-stream stream))
        (http-kit::%http-server-close-connection-stream
         server connection :abort nil)
        (ensure-equal 1 close-count)
        (ensure-true (http-kit::%http-server-connection-closed-p connection))
        (ensure-false (http-server-failure server))
        (ensure-equal :created (http-kit::http-server-state server))
        (ensure-false
         (http-kit::%http-server-connection-tls-stream connection)))))

  (deftest https-listener-covers-stop-cleanup-error-without-event-error
    (%with-https-listener-test-stubs (context)
      (ensure-true context)
      (let* ((original-destroyer
               (fdefinition
                'cl-tls-kit:destroy-tls13-server-ticket-key-store))
             (store nil)
             (server (%https-listener-make-server))
             (destroy-attempts 0)
             (stop-error nil))
        (setf (http-kit::http-server-ticket-key-store-owned-p server) t)
        (unwind-protect
            (progn
              (setf store (cl-tls-kit:make-tls13-server-ticket-key-store))
              (setf (http-kit::http-server-ticket-key-store server) store)
              (%https-listener-test-call-with-function-stubs
               (list
                (cons
                 'cl-tls-kit:destroy-tls13-server-ticket-key-store
                 (lambda (candidate)
                   (incf destroy-attempts)
                   (if (= destroy-attempts 1)
                       (error "injected ticket-store destruction failure")
                       (funcall original-destroyer candidate)))))
               (lambda ()
                 (setf stop-error
                       (handler-case
                           (progn
                             (stop-http-server
                              server :graceful-timeout 1)
                             nil)
                         (http-server-error (condition) condition)))))
              (ensure-true stop-error)
              (ensure-equal
               :ticket-key-store-destroy-failed
               (http-server-error-reason-code stop-error))
              (ensure-equal :stopping (http-kit::http-server-state server))
              (ensure-false (http-server-failure server))
              (ensure-equal 1 destroy-attempts)
              (ensure-equal
               :ticket-key-store-destroy-failed
               (http-server-error-reason-code
                (http-kit::http-server-ticket-key-store-cleanup-error server)))
              (ensure-equal
               server
               (stop-http-server server :graceful-timeout 1))
              (ensure-equal :stopped (http-kit::http-server-state server))
              (ensure-false
               (http-kit::http-server-ticket-key-store-cleanup-error server))
              (ensure-false (http-kit::http-server-ticket-key-store server))
              (ensure-equal
               :ticket-key-store-destroyed
               (handler-case
                   (progn
                     (cl-tls-kit:rotate-tls13-server-ticket-key-store store)
                     nil)
                 (cl-tls-kit:tls13-server-driver-error (condition)
                   (cl-tls-kit:tls13-server-driver-error-reason condition)))))
          (unwind-protect
               (unless (eq :stopped (http-kit::http-server-state server))
                 (stop-http-server server :graceful-timeout 1))
            (when store (http-kit::%with-http-cleanup
                (cl-tls-kit:destroy-tls13-server-ticket-key-store store))))))))
)
