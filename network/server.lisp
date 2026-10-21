(in-package #:http-kit)

(define-condition http-server-error (error)
  ((reason-code :initarg :reason-code :reader http-server-error-reason-code)
   (phase :initarg :phase :reader http-server-error-phase))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "The HTTPS server operation failed." stream))))

(define-condition http-server-callback-error (http-server-error)
  ((callback :initarg :callback
             :reader http-server-callback-error-callback))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "An HTTPS server callback failed." stream))))

(defstruct (http-server (:constructor %make-http-server))
  host
  port
  handler
  certificate-chain
  private-key
  ticket-key-store
  tls-wrapper
  max-connections
  max-handshakes
  max-concurrent-streams
  handshake-timeout
  request-timeout
  idle-timeout
  graceful-timeout
  max-header-bytes
  max-fields
  max-body-bytes
  on-event
  on-error
  (state :created)
  shutdown-p
  stop-deadline
  listener
  supervisor
  (connection-count 0)
  (handshake-count 0)
  (connections (make-hash-table :test #'eq))
  (failure-value nil)
  started-event-emitted-p
  stopping-event-emitted-p
  stopped-event-emitted-p
  #+sbcl (lock (sb-thread:make-mutex :name "http server") :read-only t)
  #+sbcl (waitqueue (sb-thread:make-waitqueue :name "http server") :read-only t))

(defstruct (%http-server-connection
             (:constructor %make-http-server-connection))
  raw-stream
  tls-stream
  (handshake-p t)
  (closed-p nil)
  peer-address
  peer-port
  handshake-deadline
  protocol)

(defmethod print-object ((server http-server) stream)
  (print-unreadable-object (server stream :type t :identity t)))

#+sbcl
(defmacro %with-http-server-lock ((server) &body body)
  `(sb-thread:with-mutex ((http-server-lock ,server))
     ,@body))

#-sbcl
(defmacro %with-http-server-lock ((server) &body body)
  (declare (ignore server))
  `(progn ,@body))

(defun http-server-failure (server)
  (unless (http-server-p server)
    (error 'http-server-error :reason-code :invalid-server :phase :inspection))
  (%with-http-server-lock (server)
    (http-server-failure-value server)))

(defun %http-server-configuration-error (reason-code)
  (error 'http-server-error :reason-code reason-code :phase :configuration))

(defun %http-server-positive-real-p (value)
  (handler-case
      (and (realp value)
           (plusp value)
           (or (not (floatp value))
               #+sbcl
               (and (not (sb-ext:float-nan-p value))
                    (not (sb-ext:float-infinity-p value)))
               #-sbcl
               (= value value)))
    (error () nil)))

(defun %http-server-positive-integer-p (value)
  (and (integerp value) (plusp value)))

(defun %http-server-new-ticket-key-store ()
  (handler-case
      (cl-tls-kit:make-tls13-server-ticket-key-store)
    (error ()
      (%http-server-configuration-error :ticket-key-store-unavailable))))

(defun %http-server-make-tls-wrapper
    (certificate-chain private-key ticket-key-store)
  (handler-case
      (let ((wrapper
              (http-kit/tls:make-http-tls-server-wrapper
               :certificate-chain certificate-chain
               :private-key private-key
               :ticket-key-store ticket-key-store
               :alpn-protocols '("h2" "http/1.1")
               :clock-function #'http-kit::%monotonic-time)))
        (unless (functionp wrapper)
          (%http-server-configuration-error :invalid-tls-wrapper))
        wrapper)
    (http-server-error (condition)
      (error condition))
    (error ()
      (%http-server-configuration-error :invalid-tls-credentials))))

(defun make-http-server
    (&key (transport :https)
          host port handler certificate-chain private-key ticket-key-store
          (max-connections 128)
          (max-handshakes 32)
          (max-concurrent-streams 100)
          (handshake-timeout 10)
          (request-timeout 30)
          (idle-timeout 60)
          (graceful-timeout 30)
          (max-header-bytes 65536)
          (max-fields 100)
          (max-body-bytes 16777216)
          on-event on-error)
  (unless (eq transport :https)
    (%http-server-configuration-error :unsupported-transport))
  (unless (and (stringp host) (plusp (length host)))
    (%http-server-configuration-error :invalid-host))
  (unless (and (integerp port) (<= 0 port 65535))
    (%http-server-configuration-error :invalid-port))
  (unless (functionp handler)
    (%http-server-configuration-error :invalid-handler))
  (unless (and (consp certificate-chain) private-key)
    (%http-server-configuration-error :missing-tls-credentials))
  (unless (or (null on-event) (functionp on-event))
    (%http-server-configuration-error :invalid-event-callback))
  (unless (or (null on-error) (functionp on-error))
    (%http-server-configuration-error :invalid-error-callback))
  (unless (and (%http-server-positive-integer-p max-connections)
               (%http-server-positive-integer-p max-handshakes)
               (%http-server-positive-integer-p max-concurrent-streams)
               (%http-server-positive-integer-p max-header-bytes)
               (%http-server-positive-integer-p max-fields)
               (%http-server-positive-integer-p max-body-bytes))
    (%http-server-configuration-error :invalid-limit))
  (unless (<= max-handshakes max-connections)
    (%http-server-configuration-error :invalid-handshake-limit))
  (unless (and (%http-server-positive-real-p handshake-timeout)
               (%http-server-positive-real-p request-timeout)
               (%http-server-positive-real-p idle-timeout)
               (%http-server-positive-real-p graceful-timeout))
    (%http-server-configuration-error :invalid-timeout))
  (when (and ticket-key-store
             (not (cl-tls-kit:tls13-server-ticket-key-store-p
                   ticket-key-store)))
    (%http-server-configuration-error :invalid-ticket-key-store))
  (let* ((store (or ticket-key-store (%http-server-new-ticket-key-store)))
         (wrapper (%http-server-make-tls-wrapper
                   certificate-chain private-key store)))
    (unless (cl-tls-kit:tls13-server-ticket-key-store-p store)
      (%http-server-configuration-error :invalid-ticket-key-store))
    (%make-http-server
     :host host
     :port port
     :handler handler
     :certificate-chain certificate-chain
     :private-key private-key
     :ticket-key-store store
     :tls-wrapper wrapper
     :max-connections max-connections
     :max-handshakes max-handshakes
     :max-concurrent-streams max-concurrent-streams
     :handshake-timeout handshake-timeout
     :request-timeout request-timeout
     :idle-timeout idle-timeout
     :graceful-timeout graceful-timeout
     :max-header-bytes max-header-bytes
     :max-fields max-fields
     :max-body-bytes max-body-bytes
     :on-event on-event
     :on-error on-error)))

(defun %http-server-callback-condition (callback phase)
  (make-condition 'http-server-callback-error
                  :reason-code (if (eq callback :on-event)
                                   :event-callback-failed
                                   :error-callback-failed)
                  :phase phase
                  :callback callback))

(defun %http-server-now ()
  (http-kit::%monotonic-time))

(defun %http-server-broadcast (server)
  #+sbcl
  (sb-thread:condition-broadcast (http-server-waitqueue server))
  #-sbcl
  (declare (ignore server)))

(defun %http-server-close-listener-object (listener)
  (when listener
    (handler-case
        (http-kit/network:close-http-tcp-listener listener)
      (error () nil))))

(defun %http-server-mark-failed (server condition)
  (let ((listener nil))
    (%with-http-server-lock (server)
      (unless (http-server-failure-value server)
        (setf (http-server-failure-value server) condition))
      (setf (http-server-state server) :failed
            (http-server-shutdown-p server) t)
      (unless (http-server-stop-deadline server)
        (setf (http-server-stop-deadline server)
              (+ (%http-server-now) (http-server-graceful-timeout server))))
      (setf listener (http-server-listener server)
            (http-server-listener server) nil)
      (%http-server-broadcast server))
    (%http-server-close-listener-object listener)
    condition))

(defun %http-server-safe-error (condition phase)
  (make-condition
   'http-server-error
   :reason-code
   (let ((reason-code
           (and (typep condition 'http-server-error)
                (http-server-error-reason-code condition))))
     (cond
       ((member reason-code
                '(:invalid-server :unsupported-transport :invalid-host
                  :invalid-port :invalid-handler :missing-tls-credentials
                  :invalid-event-callback :invalid-error-callback :invalid-limit
                  :invalid-handshake-limit :invalid-timeout
                  :ticket-key-store-unavailable :invalid-tls-wrapper
                  :invalid-tls-credentials :invalid-ticket-key-store
                  :connection-close-failed :invalid-tls-stream :unsupported-alpn
                  :worker-start-failed :listener-open-failed :server-failed
                  :supervisor-start-failed :timeout :protocol-error
                  :connection-error)
                :test #'eq)
        reason-code)
       ((typep condition 'http-timeout) :timeout)
       ((typep condition 'http-protocol-error) :protocol-error)
       (t :connection-error)))
   :phase phase))

(defun %http-server-error-context
    (phase protocol peer-address peer-port)
  (list :phase phase
        :protocol protocol
        :peer-address peer-address
        :peer-port peer-port))

(defun %http-server-report-error
    (server condition phase &key protocol peer-address peer-port fatal-p)
  (let* ((safe-condition (%http-server-safe-error condition phase))
         (callback (http-server-on-error server)))
    (when callback
      (handler-case
          (funcall callback
                   safe-condition
                   (%http-server-error-context
                    phase protocol peer-address peer-port))
        (error ()
          (let ((callback-condition
                  (%http-server-callback-condition :on-error phase)))
            (%http-server-mark-failed server callback-condition)
            (error callback-condition)))))
    (when fatal-p
      (%http-server-mark-failed server safe-condition))
    safe-condition))

(defun %http-server-report-callback-error (server condition)
  (let ((callback (http-server-on-error server)))
    (when callback
      (handler-case
          (funcall callback
                   condition
                   (%http-server-error-context
                    (http-server-error-phase condition) nil nil nil))
        (error ()
          (let ((callback-condition
                  (%http-server-callback-condition
                   :on-error
                   (http-server-error-phase condition))))
            (%http-server-mark-failed server callback-condition)
            (error callback-condition)))))
    (%http-server-mark-failed server condition)
    condition))

(defun %http-server-emit-event (server event)
  (let ((callback (http-server-on-event server)))
    (when callback
      (handler-case
          (funcall callback event)
        (error ()
          (let ((condition
                  (%http-server-callback-condition
                   :on-event (getf event :phase))))
            (%http-server-report-callback-error server condition)
            (error condition))))))
  event)

(defun %http-server-emit-event-once (server event-name event)
  (let ((emit-p nil))
    (%with-http-server-lock (server)
      (case event-name
        (:started
         (unless (http-server-started-event-emitted-p server)
           (setf (http-server-started-event-emitted-p server) t
                 emit-p t)))
        (:stopping
         (unless (http-server-stopping-event-emitted-p server)
           (setf (http-server-stopping-event-emitted-p server) t
                 emit-p t)))
        (:stopped
         (unless (http-server-stopped-event-emitted-p server)
           (setf (http-server-stopped-event-emitted-p server) t
                 emit-p t)))))
    (when emit-p
      (%http-server-emit-event server event))))

(defun %http-server-shutdown-p (server)
  (%with-http-server-lock (server)
    (not (eq (http-server-state server) :running))))

(defun %http-server-running-p (server)
  (%with-http-server-lock (server)
    (eq (http-server-state server) :running)))

(defun %http-server-release-handshake (server connection)
  (%with-http-server-lock (server)
    (when (%http-server-connection-handshake-p connection)
      (setf (%http-server-connection-handshake-p connection) nil)
      (decf (http-server-handshake-count server))
      (%http-server-broadcast server)))
  nil)

(defun %http-server-release-connection (server connection)
  (%with-http-server-lock (server)
    (when (gethash connection (http-server-connections server))
      (remhash connection (http-server-connections server))
      (when (%http-server-connection-handshake-p connection)
        (setf (%http-server-connection-handshake-p connection) nil)
        (decf (http-server-handshake-count server)))
      (decf (http-server-connection-count server))
      (%http-server-broadcast server)))
  nil)

(defun %http-server-reserve-connection
    (server raw-stream peer-address peer-port)
  (let* ((connection
           (%make-http-server-connection
            :raw-stream raw-stream
            :peer-address peer-address
            :peer-port peer-port
            :handshake-deadline
            (+ (%http-server-now) (http-server-handshake-timeout server))))
         (rejection nil))
    (%with-http-server-lock (server)
      (cond
        ((not (eq (http-server-state server) :running))
         (setf rejection :server-stopping))
        ((>= (http-server-connection-count server)
             (http-server-max-connections server))
         (setf rejection :max-connections))
        ((>= (http-server-handshake-count server)
             (http-server-max-handshakes server))
         (setf rejection :max-handshakes))
        (t
         (incf (http-server-connection-count server))
         (incf (http-server-handshake-count server))
         (setf (gethash connection (http-server-connections server)) t))))
    (values connection rejection)))

(defun %http-server-close-connection-stream (server connection)
  (let ((stream nil))
    (%with-http-server-lock (server)
      (unless (%http-server-connection-closed-p connection)
        (setf (%http-server-connection-closed-p connection) t
              stream (or (%http-server-connection-tls-stream connection)
                         (%http-server-connection-raw-stream connection))
              (%http-server-connection-tls-stream connection) nil
              (%http-server-connection-raw-stream connection) nil)))
    (when stream
      (handler-case
          (close stream :abort t)
        (error ()
          (%http-server-mark-failed
           server
           (make-condition 'http-server-error
                           :reason-code :connection-close-failed
                           :phase :close)))))
    nil))

(defun %http-server-set-tls-stream (server connection stream)
  (%with-http-server-lock (server)
    (setf (%http-server-connection-tls-stream connection) stream))
  stream)

(defun %http-server-session-error-callback
    (server connection protocol reported-p)
  (lambda (condition request)
    (declare (ignore request))
    (setf (car reported-p) t)
    (%http-server-report-error
     server condition :session
     :protocol protocol
     :peer-address (%http-server-connection-peer-address connection)
     :peer-port (%http-server-connection-peer-port connection))))

(defun %http-server-serve-session
    (server connection stream protocol reported-p)
  (let ((on-error (%http-server-session-error-callback
                   server connection protocol reported-p))
        (shutdown-p (lambda () (%http-server-shutdown-p server))))
    (if (eq protocol :http2)
        (http-kit/http2:serve-http2-session
         stream
         (http-server-handler server)
         :max-concurrent-streams (http-server-max-concurrent-streams server)
         :max-header-bytes (http-server-max-header-bytes server)
         :max-fields (http-server-max-fields server)
         :max-body-bytes (http-server-max-body-bytes server)
         :request-timeout (http-server-request-timeout server)
         :idle-timeout (http-server-idle-timeout server)
         :shutdown-p shutdown-p
         :on-error on-error
         :close-stream nil)
        (http-kit:serve-http1-session
         stream
         (http-server-handler server)
         :max-header-bytes (http-server-max-header-bytes server)
         :max-fields (http-server-max-fields server)
         :max-body-bytes (http-server-max-body-bytes server)
         :request-timeout (http-server-request-timeout server)
         :idle-timeout (http-server-idle-timeout server)
         :shutdown-p shutdown-p
         :on-error on-error
         :close-stream nil))))

(defun %http-server-run-connection (server connection)
  (let ((phase :handshake)
        (protocol nil)
        (reported-p (list nil))
        (raw-stream (%http-server-connection-raw-stream connection))
        (tls-stream nil))
    (unwind-protect
         (handler-case
             (progn
               (setf tls-stream
                     (funcall (http-server-tls-wrapper server)
                              raw-stream
                              :timeout (http-server-handshake-timeout server)
                              :deadline
                              (%http-server-connection-handshake-deadline
                               connection)))
               (unless (streamp tls-stream)
                 (error 'http-server-error
                        :reason-code :invalid-tls-stream
                        :phase :handshake))
               (%http-server-set-tls-stream server connection tls-stream)
               (%http-server-release-handshake server connection)
               (setf phase :alpn)
               (let ((alpn
                       (http-kit/tls:http-tls-selected-alpn-protocol tls-stream)))
                 (setf protocol
                       (cond ((and (stringp alpn) (string= alpn "h2")) :http2)
                             ((and (stringp alpn)
                                   (string= alpn "http/1.1"))
                              :http1)))
                 (unless protocol
                   (error 'http-server-error
                          :reason-code :unsupported-alpn
                          :phase :alpn)))
               (%http-server-emit-event
                server
                (list :type :alpn-selected
                      :phase :alpn
                      :protocol protocol
                      :peer-address
                      (%http-server-connection-peer-address connection)
                      :peer-port
                      (%http-server-connection-peer-port connection)))
               (setf phase :session)
               (%http-server-serve-session
                server connection tls-stream protocol reported-p)
               (%http-server-emit-event
                server
                (list :type :connection-closed
                      :phase :session
                      :protocol protocol
                      :peer-address
                      (%http-server-connection-peer-address connection)
                      :peer-port
                      (%http-server-connection-peer-port connection))))
           (http-server-callback-error (condition)
             (%http-server-mark-failed server condition))
           (error (condition)
             (unless (car reported-p)
               (handler-case
                   (%http-server-report-error
                    server condition phase
                    :protocol protocol
                    :peer-address
                    (%http-server-connection-peer-address connection)
                    :peer-port
                    (%http-server-connection-peer-port connection))
                 (http-server-callback-error (callback-condition)
                   (%http-server-mark-failed server callback-condition))))))
      (%http-server-close-connection-stream server connection)
      (%http-server-release-connection server connection))))

#+sbcl
(defun %http-server-start-connection-worker (server connection)
  (handler-case
      (sb-thread:make-thread
       (lambda () (%http-server-run-connection server connection))
       :name "http server connection")
    (error ()
      (%http-server-close-connection-stream server connection)
      (%http-server-release-connection server connection)
      (let ((condition
              (make-condition 'http-server-error
                              :reason-code :worker-start-failed
                              :phase :supervisor)))
        (handler-case
            (%http-server-report-error server condition :supervisor :fatal-p t)
          (http-server-callback-error (callback-condition)
            (%http-server-mark-failed server callback-condition)))))))

(defun %http-server-reject-connection
    (server raw-stream reason peer-address peer-port)
  (handler-case
      (close raw-stream :abort t)
    (error () nil))
  (%http-server-emit-event
   server
   (list :type :connection-rejected
         :phase :admission
         :reason-code reason
         :peer-address peer-address
         :peer-port peer-port)))

(defun %http-server-accept-one (server listener)
  (multiple-value-bind (raw-stream peer-address peer-port)
      (http-kit/network:accept-http-tcp-stream listener)
    (multiple-value-bind (connection rejection)
        (%http-server-reserve-connection
         server raw-stream peer-address peer-port)
      (if rejection
          (%http-server-reject-connection
           server raw-stream rejection peer-address peer-port)
          (let ((worker-started-p nil))
            (unwind-protect
                 (progn
                   (%http-server-emit-event
                    server
                    (list :type :connection-accepted
                          :phase :admission
                          :peer-address peer-address
                          :peer-port peer-port))
                   (%http-server-start-connection-worker server connection)
                   (setf worker-started-p t))
              (unless worker-started-p
                (%http-server-close-connection-stream server connection)
                (%http-server-release-connection server connection))))))))

(defun %http-server-take-listener (server)
  (let ((listener nil))
    (%with-http-server-lock (server)
      (setf listener (http-server-listener server)
            (http-server-listener server) nil))
    listener))

(defun %http-server-close-listener (server)
  (%http-server-close-listener-object (%http-server-take-listener server)))

(defun %http-server-ensure-stopping (server)
  (%with-http-server-lock (server)
    (unless (eq (http-server-state server) :failed)
      (setf (http-server-state server) :stopping))
    (setf (http-server-shutdown-p server) t)
    (unless (http-server-stop-deadline server)
      (setf (http-server-stop-deadline server)
            (+ (%http-server-now) (http-server-graceful-timeout server))))
    (%http-server-broadcast server)))

(defun %http-server-stop-deadline (server)
  (%with-http-server-lock (server)
    (http-server-stop-deadline server)))

(defun %http-server-wait-for-connections (server deadline)
  #+sbcl
  (loop
    (%with-http-server-lock (server)
      (when (zerop (http-server-connection-count server))
        (return-from %http-server-wait-for-connections t))
      (let ((remaining (- deadline (%http-server-now))))
        (unless (plusp remaining)
          (return-from %http-server-wait-for-connections nil))
        (sb-thread:condition-wait
         (http-server-waitqueue server)
         (http-server-lock server)
         :timeout remaining))))
  #-sbcl
  (declare (ignore server deadline))
  #-sbcl
  nil)

(defun %http-server-connection-snapshot (server)
  (%with-http-server-lock (server)
    (loop for connection being the hash-keys of (http-server-connections server)
          collect connection)))

(defun %http-server-force-close-connections (server)
  (dolist (connection (%http-server-connection-snapshot server))
    (%http-server-close-connection-stream server connection))
  nil)

(defun %http-server-finish-stop (server)
  (let ((deadline (or (%http-server-stop-deadline server)
                      (+ (%http-server-now)
                         (http-server-graceful-timeout server)))))
    (unless (%http-server-wait-for-connections server deadline)
      (%http-server-force-close-connections server))
    (%http-server-emit-event-once
     server :stopped (list :type :listener-stopped :phase :shutdown))
    (%with-http-server-lock (server)
      (unless (eq (http-server-state server) :failed)
        (setf (http-server-state server) :stopped))
      (%http-server-broadcast server)))
  server)

(defun %http-server-supervisor (server listener)
  (unwind-protect
       (handler-case
           (progn
             (when (%http-server-running-p server)
               (%http-server-emit-event-once
                server :started
                (list :type :listener-started
                      :phase :startup
                      :host (http-server-host server)
                      :port (http-server-port server))))
             (loop while (%http-server-running-p server)
                   do (handler-case
                          (%http-server-accept-one server listener)
                        (http-server-callback-error (condition)
                          (error condition))
                        (error (condition)
                          (when (%http-server-running-p server)
                            (%http-server-report-error
                             server condition :accept :fatal-p t))
                          (return)))))
         (http-server-callback-error (condition)
           (%http-server-mark-failed server condition))
         (error (condition)
           (handler-case
               (%http-server-report-error
                server condition :supervisor :fatal-p t)
             (http-server-callback-error (callback-condition)
               (%http-server-mark-failed server callback-condition)))))
    (%http-server-close-listener server)
    (%http-server-ensure-stopping server)
    (%http-server-finish-stop server)))

(defun %http-server-start-listener (server)
  (handler-case
      (http-kit/network:open-http-tcp-listener
       :host (http-server-host server)
       :port (http-server-port server))
    (error ()
      (%http-server-configuration-error :listener-open-failed))))

#+sbcl
(defun start-http-server (server)
  (unless (http-server-p server)
    (%http-server-configuration-error :invalid-server))
  (%with-http-server-lock (server)
    (case (http-server-state server)
      (:running (return-from start-http-server server))
      (:starting (return-from start-http-server server))
      (:failed (error (or (http-server-failure-value server)
                          (make-condition 'http-server-error
                                          :reason-code :server-failed
                                          :phase :startup))))
      (otherwise
       (setf (http-server-state server) :starting
             (http-server-shutdown-p server) nil
             (http-server-stop-deadline server) nil
             (http-server-started-event-emitted-p server) nil
             (http-server-stopping-event-emitted-p server) nil
             (http-server-stopped-event-emitted-p server) nil))))
  (let ((listener
          (handler-case
              (%http-server-start-listener server)
            (http-server-error (condition)
              (%http-server-mark-failed server condition)
              (error condition)))))
    (let ((start-p nil))
      (%with-http-server-lock (server)
        (when (eq (http-server-state server) :starting)
          (setf (http-server-listener server) listener
                (http-server-port server)
                (http-kit/network:http-network-listener-port listener)
                (http-server-state server) :running
                start-p t)))
      (unless start-p
        (%http-server-close-listener-object listener)
        (return-from start-http-server server)))
    (handler-case
        (let ((thread
                (sb-thread:make-thread
                 (lambda () (%http-server-supervisor server listener))
                 :name "http server supervisor")))
          (%with-http-server-lock (server)
            (setf (http-server-supervisor server) thread)))
      (error ()
        (%http-server-close-listener server)
        (let ((condition
                (make-condition 'http-server-error
                                :reason-code :supervisor-start-failed
                                :phase :startup)))
          (handler-case
              (%http-server-report-error
               server condition :startup :fatal-p t)
            (http-server-callback-error (callback-condition)
              (%http-server-mark-failed server callback-condition))))))
    server))

(defun %http-server-positive-stop-timeout (server supplied-p value)
  (if supplied-p
      (if (%http-server-positive-real-p value)
          value
          (%http-server-configuration-error :invalid-graceful-timeout))
      (http-server-graceful-timeout server)))

#+sbcl
(defun stop-http-server (server &key (graceful-timeout nil supplied-p))
  (unless (http-server-p server)
    (%http-server-configuration-error :invalid-server))
  (let* ((timeout (%http-server-positive-stop-timeout
                   server supplied-p graceful-timeout))
         (listener nil)
         (started-p nil)
         (event-error nil))
    (%with-http-server-lock (server)
      (unless (http-server-shutdown-p server)
        (setf (http-server-shutdown-p server) t
              (http-server-stop-deadline server) (+ (%http-server-now) timeout)
              started-p t)
        (unless (eq (http-server-state server) :failed)
          (setf (http-server-state server) :stopping)))
      (unless (http-server-stop-deadline server)
        (setf (http-server-stop-deadline server) (+ (%http-server-now) timeout)))
      (setf listener (http-server-listener server)
            (http-server-listener server) nil)
      (%http-server-broadcast server))
    (%http-server-close-listener-object listener)
    (when started-p
      (handler-case
          (%http-server-emit-event-once
           server :stopping
           (list :type :listener-stopping :phase :shutdown))
        (http-server-callback-error (condition)
          (setf event-error condition))))
    (if (http-server-supervisor server)
        (%http-server-finish-stop server)
        (progn
          (handler-case
              (%http-server-emit-event-once
               server :stopped
               (list :type :listener-stopped :phase :shutdown))
            (http-server-callback-error (condition)
              (unless event-error
                (setf event-error condition))))
          (%with-http-server-lock (server)
            (unless (eq (http-server-state server) :failed)
              (setf (http-server-state server) :stopped)))))
    (when event-error
      (error event-error))
    server))

#-sbcl
(progn
  (defun start-http-server (server)
    (unless (http-server-p server)
      (%http-server-configuration-error :invalid-server))
    (%http-server-configuration-error :unsupported-platform))
  (defun stop-http-server (server &key graceful-timeout)
    (declare (ignore graceful-timeout))
    (unless (http-server-p server)
      (%http-server-configuration-error :invalid-server))
    (%http-server-configuration-error :unsupported-platform)))
