(in-package #:http-kit/test)

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

  (defclass %p9b-close-hook-stream (binary-session-stream)
    ((close-hook :initarg :close-hook :reader %p9b-close-hook-stream-hook)))

  (defmethod close ((stream %p9b-close-hook-stream) &key abort)
    (declare (ignore abort))
    (funcall (%p9b-close-hook-stream-hook stream))
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
                '%p9b-close-hook-stream
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

  (deftest http2-coverage-scheduler-condition-wait-reacquires-lock
    (let* ((connection
             (http-kit/http2:make-http2-connection
              :stream
              (make-instance 'binary-session-stream :input (octets))))
           (lock
             (http-kit/http2::%http2-connection-scheduler-lock connection))
           (wait-symbol
             'http-kit/http2::%h2-scheduler-condition-wait)
           (original-wait (symbol-function wait-symbol))
           (clock-active-p nil)
           (clock-values nil)
           (clock-function
             (lambda ()
               (if clock-active-p
                   (or (pop clock-values) 2)
                   0)))
           (wait-count 0)
           (lock-reacquired-p nil)
           (result nil))
      (unwind-protect
           (progn
             (http-kit/http2::%h2-with-scheduler-lock (connection)
               (setf
                (http-kit/http2::%http2-connection-scheduler-reader-active-p
                 connection)
                t))
             (%https-listener-test-call-with-function-stubs
              (list
               (cons
                wait-symbol
                (lambda (wait-connection deadline wait-clock-function)
                  (incf wait-count)
                  (ensure-true (sb-thread:holding-mutex-p lock))
                  (setf clock-active-p t
                        clock-values (list 0 1 2))
                  (handler-case
                      (funcall original-wait
                               wait-connection deadline wait-clock-function)
                    (http-kit:http-timeout (condition)
                      (setf lock-reacquired-p
                            (sb-thread:holding-mutex-p lock))
                      (error condition))))))
              (lambda ()
                (setf result
                      (handler-case
                          (progn
                            (http-kit/http2:send-http2-request-over-connection
                             connection
                             (make-http-request
                              :method "GET"
                              :uri "https://example.com/scheduler-wait")
                             :deadline 1
                             :clock-function clock-function)
                            :returned)
                        (http-kit:http-timeout () :timed-out)))))
             (ensure-equal :timed-out result)
             (ensure-equal 1 wait-count)
             (ensure-true lock-reacquired-p))
        (http-kit/http2::%h2-with-scheduler-lock (connection)
          (setf (http-kit/http2::%http2-connection-scheduler-reader-active-p
                 connection)
                nil)
          (http-kit/http2::%h2-scheduler-broadcast connection))
        (http-kit/http2:close-http2-connection connection))))
)
