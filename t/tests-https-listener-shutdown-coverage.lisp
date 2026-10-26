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


