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

(defvar *http-server-clock-function* #'http-kit::%monotonic-time)

(defstruct (http-server (:constructor %make-http-server))
  host
  port
  handler
  certificate-chain
  private-key
  ticket-key-store
  ticket-key-store-owned-p
  ticket-key-rotation-deadline
  (retired-ticket-key-stores nil)
  (ticket-key-store-users (make-hash-table :test (function eq)))
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
  max-buffered-body-bytes
  on-event
  on-error
  (state :created)
  shutdown-p
  stop-deadline
  start-token
  listener
  supervisor
  shutdown-coordinator
  (workers (make-hash-table :test (function eq)))
  listener-wakeup-p
  listener-wake-in-progress-p
  (connection-count 0)
  (handshake-count 0)
  (buffered-body-bytes 0)
  (connections (make-hash-table :test (function eq)))
  (failure-value nil)
  (ticket-key-store-cleanup-error nil)
  started-event-emitted-p
  stopping-event-emitted-p
  stopped-event-emitted-p
  shutdown-deferred-event-emitted-p
  #+sbcl (lock (sb-thread:make-mutex :name "http server") :read-only t)
  #+sbcl (waitqueue (sb-thread:make-waitqueue :name "http server") :read-only t))

(defstruct (%http-server-connection
             (:constructor %make-http-server-connection))
  raw-stream
  raw-socket
  tls-stream
  ticket-key-store
  (handshake-p t)
  (closing-p nil)
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
  #+sbcl
  (and (realp value)
       (or (not (floatp value))
           (and (not (sb-ext:float-nan-p value))
                (not (sb-ext:float-infinity-p value))))
       (plusp value))
  #-sbcl
  (handler-case
      (and (realp value)
           (or (not (floatp value))
               (= value value))
           (plusp value))
    (arithmetic-error () nil)))

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
               :clock-function #'%http-server-now)))
        (unless (functionp wrapper)
          (%http-server-configuration-error :invalid-tls-wrapper))
        wrapper)
    (http-server-error (condition)
      (error condition))
    (error ()
      (%http-server-configuration-error :invalid-tls-credentials))))
(progn
  (defconstant +http-server-ticket-key-rotation-interval+ (* 12 60 60))

  (defun %http-server-report-ticket-key-store-cleanup-failure (server)
    (let ((condition
            (make-condition 'http-server-error
                            :reason-code :ticket-key-store-destroy-failed
                            :phase :shutdown)))
      (%with-http-server-lock (server)
        (unless (http-server-ticket-key-store-cleanup-error server)
          (setf (http-server-ticket-key-store-cleanup-error server)
                condition)))
      (handler-case
          (%http-server-report-error server condition :shutdown)
        (error (callback-condition)
          (%http-server-mark-failed server callback-condition)))
      condition))

  (defun %http-server-destroy-ticket-key-stores
      (server stores &key clear-error-p)
    (let ((failed-p nil))
      (dolist (store (remove-duplicates stores :test #'eq))
        (handler-case
            (progn
              (cl-tls-kit:destroy-tls13-server-ticket-key-store store)
              (%with-http-server-lock (server)
                (when (eq store (http-server-ticket-key-store server))
                  (setf (http-server-ticket-key-store server) nil
                        (http-server-tls-wrapper server) nil
                        (http-server-ticket-key-rotation-deadline server) nil))
                (setf (http-server-retired-ticket-key-stores server)
                      (delete store
                              (http-server-retired-ticket-key-stores server)
                              :test #'eq))
                (remhash store (http-server-ticket-key-store-users server))))
          (error ()
            (setf failed-p t)
            (%http-server-report-ticket-key-store-cleanup-failure server))))
      (when (and clear-error-p (not failed-p))
        (%with-http-server-lock (server)
          (setf (http-server-ticket-key-store-cleanup-error server) nil)))
      (not failed-p)))

  (defun %http-server-take-retired-ticket-key-store (server store)
    (when (and (zerop
                (gethash store (http-server-ticket-key-store-users server) 0))
               (member store (http-server-retired-ticket-key-stores server)
                       :test #'eq))
      store))

  (defun %http-server-rotate-ticket-key-store-if-due (server connection)
  (let ((stores-to-destroy nil)
        (uninstalled-store nil)
        (rotation-error nil)
        (wrapper nil))
    (%with-http-server-lock (server)
      (let* ((store (http-server-ticket-key-store server))
             (deadline (http-server-ticket-key-rotation-deadline server))
             (now (%http-server-now)))
        (when (and (http-server-ticket-key-store-owned-p server)
                   store
                   (not (http-server-shutdown-p server))
                   (or (null deadline) (>= now deadline)))
          (let* ((replacement-store (%http-server-new-ticket-key-store))
                 (replacement-wrapper
                   (handler-case
                       (%http-server-make-tls-wrapper
                        (http-server-certificate-chain server)
                        (http-server-private-key server)
                        replacement-store)
                     (error (condition)
                       (setf uninstalled-store replacement-store
                             rotation-error condition)
                       nil))))
            (if replacement-wrapper
                (progn
                  (push store (http-server-retired-ticket-key-stores server))
                  (setf (http-server-ticket-key-store server) replacement-store
                        (http-server-tls-wrapper server) replacement-wrapper
                        (http-server-ticket-key-rotation-deadline server)
                        (+ now +http-server-ticket-key-rotation-interval+))
                  (when (zerop
                         (gethash store
                                  (http-server-ticket-key-store-users server)
                                  0))
                    (setf stores-to-destroy
                          (list (%http-server-take-retired-ticket-key-store
                                 server store)))))
                (push replacement-store
                      (http-server-retired-ticket-key-stores server)))))
        (setf wrapper (http-server-tls-wrapper server))
        (when (http-server-ticket-key-store-owned-p server)
          (setf (%http-server-connection-ticket-key-store connection)
                (http-server-ticket-key-store server))
          (incf (gethash (http-server-ticket-key-store server) (http-server-ticket-key-store-users server) 0)))))
    (when uninstalled-store
      (%http-server-destroy-ticket-key-stores server (list uninstalled-store)))
    (when rotation-error
      (error rotation-error))
    (%http-server-destroy-ticket-key-stores server stores-to-destroy)
    wrapper))

  (defun %http-server-ensure-owned-ticket-key-store (server start-token)
  (let ((valid-start-p nil)
        (uninstalled-store nil)
        (startup-error nil))
    (%with-http-server-lock (server)
      (when (and (eq (http-server-state server) :starting)
                 (eq (http-server-start-token server) start-token))
        (setf valid-start-p t)
        (when (and (http-server-ticket-key-store-owned-p server)
                   (null (http-server-ticket-key-store server)))
          (let* ((store (%http-server-new-ticket-key-store))
                 (wrapper
                   (handler-case
                       (%http-server-make-tls-wrapper
                        (http-server-certificate-chain server)
                        (http-server-private-key server)
                        store)
                     (error (condition)
                       (setf uninstalled-store store
                             startup-error condition)
                       nil))))
            (if wrapper
                (setf (http-server-ticket-key-store server) store
                      (http-server-tls-wrapper server) wrapper
                      (http-server-ticket-key-rotation-deadline server)
                      (+ (%http-server-now)
                         +http-server-ticket-key-rotation-interval+))
                (push store (http-server-retired-ticket-key-stores server)))))))
    (when uninstalled-store
      (%http-server-destroy-ticket-key-stores server (list uninstalled-store)))
    (when startup-error
      (error startup-error))
    valid-start-p))

  (defun %http-server-release-owned-ticket-key-store (server)
  (let ((stores nil)
        (stores-to-destroy nil)
        (owned-p nil)
        (release-succeeded-p nil)
        (deferred-p nil))
    (%with-http-server-lock (server)
      (setf owned-p (http-server-ticket-key-store-owned-p server))
      (when owned-p
        (setf stores
              (remove-duplicates
               (remove nil
                       (cons (http-server-ticket-key-store server)
                             (http-server-retired-ticket-key-stores server)))
               :test (function eq))
              (http-server-tls-wrapper server) nil
              (http-server-ticket-key-rotation-deadline server) nil)
        (dolist (store stores)
          (pushnew store (http-server-retired-ticket-key-stores server)
                   :test (function eq))
          (let ((ready-store
                  (%http-server-take-retired-ticket-key-store server store)))
            (when ready-store
              (push ready-store stores-to-destroy))))))
    (setf release-succeeded-p
          (if owned-p
              (%http-server-destroy-ticket-key-stores
               server stores-to-destroy :clear-error-p t)
              t)
          deferred-p
          (and owned-p
               (%with-http-server-lock (server)
                 (loop for store in stores
                       thereis
                       (and (member store
                                    (http-server-retired-ticket-key-stores server)
                                    :test (function eq))
                            (plusp
                             (gethash
                              store
                              (http-server-ticket-key-store-users server)
                              0)))))))
    (values release-succeeded-p deferred-p))))

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
      (max-buffered-body-bytes
       (max (* 64 1024 1024)
            (* 2 (if (integerp max-body-bytes) max-body-bytes 0))))
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
               (%http-server-positive-integer-p max-body-bytes)
               (%http-server-positive-integer-p max-buffered-body-bytes))
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
  (let* ((owned-p (null ticket-key-store))
         (store (or ticket-key-store (%http-server-new-ticket-key-store)))
         (wrapper nil)
         (server nil))
    (unwind-protect
         (progn
           (setf wrapper
                 (%http-server-make-tls-wrapper
                  certificate-chain private-key store))
           (setf server
                 (%make-http-server
                  :host host
                  :port port
                  :handler handler
                  :certificate-chain certificate-chain
                  :private-key private-key
                  :ticket-key-store store
                  :ticket-key-store-owned-p owned-p
                  :ticket-key-rotation-deadline
                  (+ (%http-server-now)
                     +http-server-ticket-key-rotation-interval+)
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
                  :max-buffered-body-bytes max-buffered-body-bytes
                  :on-event on-event
                  :on-error on-error))
           server)
      (when (and owned-p (null server))
        (%with-http-cleanup
          (cl-tls-kit:destroy-tls13-server-ticket-key-store store))))))

(defun %http-server-callback-condition (callback phase)
  (make-condition 'http-server-callback-error
                  :reason-code (if (eq callback :on-event)
                                   :event-callback-failed
                                   :error-callback-failed)
                  :phase phase
                  :callback callback))

(defun %http-server-now () (funcall *http-server-clock-function*))

(defun %http-server-broadcast (server)
  #+sbcl
  (sb-thread:condition-broadcast (http-server-waitqueue server))
  #-sbcl
  (declare (ignore server)))

(defun %http-server-wait-for-listener-wakeup (server &optional deadline)
  #+sbcl
  (flet ((wait ()
           (if deadline
               (let ((remaining (- deadline (%http-server-now))))
                 (when (plusp remaining)
                   (sb-thread:condition-wait
                    (http-server-waitqueue server)
                    (http-server-lock server)
                    :timeout remaining)))
             (sb-thread:condition-wait
              (http-server-waitqueue server)
              (http-server-lock server)))))
    (if (sb-thread:holding-mutex-p (http-server-lock server))
        (wait)
      (%with-http-server-lock (server)
        (wait))))
  #-sbcl
  (declare (ignore server deadline))
  #-sbcl
  nil)

(defun %http-server-close-listener-object (listener)
  (when listener
    (%with-http-cleanup
      (http-kit/network:close-http-tcp-listener listener))))

(declaim (ftype function %http-server-ensure-shutdown-coordinator
                          %http-server-shutdown-coordinator
                          %http-server-wake-listener))

(defun %http-server-mark-failed (server condition)
  (let ((listener nil)
        (supervisor nil))
    (%with-http-server-lock (server)
      (unless (http-server-failure-value server)
        (setf (http-server-failure-value server) condition))
      (setf (http-server-state server) :failed
            (http-server-shutdown-p server) t)
      (unless (http-server-stop-deadline server)
        (setf (http-server-stop-deadline server)
              (+ (%http-server-now) (http-server-graceful-timeout server))))
      (setf listener (http-server-listener server)
            supervisor (http-server-supervisor server))
      (%http-server-broadcast server))
    (when (and listener supervisor)
      (%http-server-ensure-shutdown-coordinator server)
      (%http-server-wake-listener server))
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
                  :ticket-key-store-unavailable
                  :ticket-key-store-destroy-failed
                  :invalid-tls-wrapper :invalid-tls-credentials
                  :invalid-ticket-key-store :connection-close-failed
                  :unsupported-alpn :worker-start-failed
                  :listener-open-failed :server-failed
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
                 emit-p t)))
        (:shutdown-deferred
         (unless (http-server-shutdown-deferred-event-emitted-p server)
           (setf (http-server-shutdown-deferred-event-emitted-p server) t
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
  (let ((store-to-destroy nil))
    (%with-http-server-lock (server)
      (when (gethash connection (http-server-connections server))
        (remhash connection (http-server-connections server))
        (when (%http-server-connection-handshake-p connection)
          (setf (%http-server-connection-handshake-p connection) nil)
          (decf (http-server-handshake-count server)))
        (let ((store (%http-server-connection-ticket-key-store connection)))
          (when store
            (setf (%http-server-connection-ticket-key-store connection) nil)
            (multiple-value-bind (users tracked-p)
                (gethash store (http-server-ticket-key-store-users server))
              (when tracked-p
                (decf users)
                (setf (gethash store
                               (http-server-ticket-key-store-users server))
                      users)
                (setf store-to-destroy
                      (%http-server-take-retired-ticket-key-store
                       server store))))))
        (decf (http-server-connection-count server))
        (%http-server-broadcast server)))
    (%http-server-destroy-ticket-key-stores
     server (and store-to-destroy (list store-to-destroy))))
  nil)

(defun %http-server-reserve-connection
    (server listener raw-stream peer-address peer-port
      &optional (accepted-at (%http-server-now)))
  (let* ((connection
           (%make-http-server-connection
            :raw-stream raw-stream
            :raw-socket
            (http-kit/network::%network-stream-socket raw-stream)
            :peer-address peer-address
            :peer-port peer-port
            :handshake-deadline
            (+ accepted-at (http-server-handshake-timeout server))))
         (rejection nil))
    (%with-http-server-lock (server)
      (cond
        ((not (and (eq (http-server-state server) :running)
                   (eq listener (http-server-listener server))))
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

(defun %http-server-close-connection-stream (server connection &key (abort t))
  (let ((stream nil)
        (socket nil))
    (%with-http-server-lock (server)
      (cond
        ((%http-server-connection-closing-p connection)
         (when abort
           (setf stream (%http-server-connection-raw-stream connection)
                 socket (%http-server-connection-raw-socket connection)
                 (%http-server-connection-closed-p connection) t)))
        ((%http-server-connection-closed-p connection)
         nil)
        (t
         (setf (%http-server-connection-closing-p connection) t
               stream (or (%http-server-connection-tls-stream connection)
                          (%http-server-connection-raw-stream connection)))
         (when abort
           (setf socket (%http-server-connection-raw-socket connection)
                 (%http-server-connection-closed-p connection) t)))))
    (when (and abort socket)
      (http-kit/network::%network-shutdown-socket socket))
    (when stream
      (unless (and abort (not (open-stream-p stream)))
        (handler-case
            #+sbcl
            (http-kit/network::%network-close-accepted-stream
             stream :abort abort)
            #-sbcl
            (close stream :abort abort)
          (error ()
            (unless (or (and abort (not (open-stream-p stream)))
                        (and (not abort)
                             (%with-http-server-lock (server)
                               (%http-server-connection-closed-p connection))))
              (%http-server-mark-failed
               server
               (make-condition 'http-server-error
                               :reason-code :connection-close-failed
                               :phase :close))))))
      (%with-http-server-lock (server)
        (setf (%http-server-connection-closing-p connection) nil
              (%http-server-connection-closed-p connection) t
              (%http-server-connection-raw-stream connection) nil
              (%http-server-connection-raw-socket connection) nil
              (%http-server-connection-tls-stream connection) nil)))
    nil))

(defun %http-server-set-tls-stream (server connection stream)
  (%with-http-server-lock (server)
    (setf (%http-server-connection-tls-stream connection) stream))
  stream)
(defun %http-server-open-tls-stream (server connection raw-stream)
  (let ((wrapper
          (%http-server-rotate-ticket-key-store-if-due server connection)))
    (funcall wrapper
             raw-stream
             :timeout (http-server-handshake-timeout server)
             :deadline
             (%http-server-connection-handshake-deadline connection))))

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
        (shutdown-p (lambda () (%http-server-shutdown-p server)))
        (reserve-body-bytes
          (lambda (amount)
            (%with-http-server-lock (server)
              (let ((observed (+ (http-server-buffered-body-bytes server)
                                 amount)))
                (http-kit::%check-limit
                 :body observed (http-server-max-buffered-body-bytes server)
                 :operation :https-listener)
                (incf (http-server-buffered-body-bytes server) amount)))))
        (release-body-bytes
          (lambda (amount)
            (%with-http-server-lock (server)
              (unless (and (integerp amount)
                           (<= 0 amount
                               (http-server-buffered-body-bytes server)))
                (error 'http-server-error
                       :reason-code :body-buffer-accounting
                       :phase :session))
              (decf (http-server-buffered-body-bytes server) amount)))))
    (if (eq protocol :http2)
        (http-kit/http2:serve-http2-session
         stream
         (http-server-handler server)
         :max-concurrent-streams (http-server-max-concurrent-streams server)
         :max-header-bytes (http-server-max-header-bytes server)
         :max-fields (http-server-max-fields server)
         :max-body-bytes (http-server-max-body-bytes server)
         :max-aggregate-body-bytes (* 2 (http-server-max-body-bytes server))
         :reserve-body-bytes reserve-body-bytes
         :release-body-bytes release-body-bytes
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
         :reserve-body-bytes reserve-body-bytes
         :release-body-bytes release-body-bytes
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
        (tls-stream nil)
        (session-completed-p nil))
    (unwind-protect
         (handler-case
             (progn
               (setf tls-stream
                     (%http-server-open-tls-stream server connection raw-stream))
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
               (multiple-value-bind (request-count termination)
                   (%http-server-serve-session
                    server connection tls-stream protocol reported-p)
                 (declare (ignore request-count))
                 (setf session-completed-p
                       (not (typep termination 'http-size-limit-exceeded))))
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
      (%http-server-close-connection-stream
       server connection :abort (not session-completed-p))
      (%http-server-release-connection server connection))))

#+sbcl
(defun %http-server-start-connection-worker (server connection)
  (let ((worker nil))
    (let ((finished-workers
            (%with-http-server-lock
                (server)
              (loop for thread being the hash-keys of
                    (http-server-workers server)
                    unless (sb-thread:thread-alive-p thread)
                      collect thread))))
      (dolist (thread finished-workers)
        (sb-thread:join-thread thread)
        (%with-http-server-lock (server)
          (remhash thread (http-server-workers server)))))
    (handler-case
        (progn
          (%with-http-server-lock (server)
            (setf worker
                  (sb-thread:make-thread
                   (lambda ()
                     (%with-http-server-lock (server)
                       nil)
                     (unwind-protect
                          (%http-server-run-connection server connection)
                       (%with-http-server-lock (server)
                         (remhash sb-thread:*current-thread*
                                  (http-server-workers server))
                         (%http-server-broadcast server))
                       (when (%with-http-server-lock (server)
                               (and (http-server-shutdown-p server)
                                    (member (http-server-state server) (quote (:stopping :failed)))
                                    (let ((supervisor
                                            (http-server-supervisor server)))
                                      (or (null supervisor)
                                          (not (sb-thread:thread-alive-p
                                                supervisor))))))
                         (%http-server-ensure-shutdown-coordinator server))))
                   :name "http server connection"))
            (setf (gethash worker (http-server-workers server)) t)
            (%http-server-broadcast server))
          worker)
      (error ()
        (%http-server-close-connection-stream server connection)
        (%http-server-release-connection server connection)
        (let ((condition
                (make-condition 'http-server-error
                                :reason-code :worker-start-failed
                                :phase :supervisor)))
          (handler-case
              (%http-server-report-error
               server condition :supervisor :fatal-p t)
            (http-server-callback-error (callback-condition)
              (%http-server-mark-failed server callback-condition))))))))

(defun %http-server-reject-connection
    (server raw-stream reason peer-address peer-port)
  (%with-http-cleanup
    (close raw-stream :abort t))
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
    (let ((accepted-at (%http-server-now)))
      (multiple-value-bind (connection rejection)
          (%http-server-reserve-connection
           server listener raw-stream peer-address peer-port accepted-at)
        (cond
          ((eq rejection :server-stopping)
           (%with-http-cleanup
             (close raw-stream :abort t)))
          (rejection
           (%http-server-reject-connection
            server raw-stream rejection peer-address peer-port))
          (t
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
                 (%http-server-release-connection server connection))))))))))

(progn
  #+sbcl
  (defun %http-server-wake-listener (server)
    (let ((listener nil))
      (%with-http-server-lock (server)
        (let ((supervisor (http-server-supervisor server)))
          (when (and (http-server-listener server)
                     supervisor
                     (sb-thread:thread-alive-p supervisor)
                     (not (http-server-listener-wakeup-p server)))
            (setf listener (http-server-listener server)
                  (http-server-listener-wakeup-p server) t
                  (http-server-listener-wake-in-progress-p server) t))))
      (when listener
        (handler-case
            (http-kit/network:wake-http-tcp-listener listener)
          (error (condition)
            (%with-http-server-lock (server)
              (when (eq listener (http-server-listener server))
                (setf (http-server-listener-wakeup-p server) nil
                      (http-server-listener-wake-in-progress-p server) nil))
              (%http-server-broadcast server))
            (error condition)))
        (%with-http-server-lock (server)
          (when (eq listener (http-server-listener server))
            (setf (http-server-listener-wake-in-progress-p server) nil))
          (%http-server-broadcast server)))
    nil))
  #-sbcl
  (defun %http-server-wake-listener (server)
    (declare (ignore server))
    nil))

(progn
  #+sbcl
  (defun %http-server-ensure-shutdown-coordinator (server)
    (%with-http-server-lock (server)
      (unless (eq (http-server-state server) :stopped)
        (or (http-server-shutdown-coordinator server)
            (setf (http-server-shutdown-coordinator server)
                  (sb-thread:make-thread
                   (lambda () (%http-server-shutdown-coordinator server))
                   :name "http server shutdown coordinator"))))))
  #-sbcl
  (defun %http-server-ensure-shutdown-coordinator (server)
    (declare (ignore server))
    nil))

(defun %http-server-supervisor-exited (server)
  (let ((unexpected-p nil)
        (shutdown-p nil))
    (%with-http-server-lock (server)
      (when (eq (http-server-supervisor server)
                sb-thread:*current-thread*)
        (setf (http-server-supervisor server) nil))
      (when (eq (http-server-state server) :running)
        (setf (http-server-state server) :failed
              (http-server-shutdown-p server) t
              unexpected-p t)
        (unless (http-server-failure-value server)
          (setf (http-server-failure-value server)
                (make-condition 'http-server-error
                                :reason-code :server-failed
                                :phase :supervisor)))
        (unless (http-server-stop-deadline server)
          (setf (http-server-stop-deadline server)
                (+ (%http-server-now)
                   (http-server-graceful-timeout server)))))
      (setf shutdown-p
            (and (http-server-shutdown-p server)
                 (member (http-server-state server) (quote (:stopping :failed)))))
      (%http-server-broadcast server))
    (when (or unexpected-p shutdown-p)
      (%http-server-ensure-shutdown-coordinator server))
    nil))

(defun %http-server-stop-deadline (server)
  (%with-http-server-lock (server)
    (http-server-stop-deadline server)))

(defun %http-server-wait-for-connections (server deadline)
  #+sbcl
  (loop
    (%with-http-server-lock (server)
      (when (zerop (http-server-connection-count server))
        (return-from %http-server-wait-for-connections t))
      (if deadline
          (let ((remaining (- deadline (%http-server-now))))
            (unless (plusp remaining)
              (return-from %http-server-wait-for-connections nil))
            (sb-thread:condition-wait
             (http-server-waitqueue server)
             (http-server-lock server)
             :timeout remaining))
          (sb-thread:condition-wait
           (http-server-waitqueue server)
           (http-server-lock server)))))
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

(progn
  #+sbcl
  (defun %http-server-shutdown-coordinator (server)
    (let ((coordinator sb-thread:*current-thread*)
          (supervisor nil)
          (supervisor-alive-p nil)
          (listener nil))
      (unwind-protect
           (let* ((deadline
                    (or (%http-server-stop-deadline server)
                        (+ (%http-server-now)
                           (http-server-graceful-timeout server))))
                  (cleanup-deadline (+ deadline 1.0d0)))
             (%with-http-server-lock (server)
               (setf supervisor (http-server-supervisor server)))
             (when (and supervisor (not (eq supervisor coordinator)))
               (let ((remaining (- cleanup-deadline (%http-server-now))))
                 (when (plusp remaining)
                   (sb-thread:join-thread supervisor
                                          :timeout remaining
                                          :default nil))))
             (setf supervisor-alive-p
                   (and supervisor
                        (not (eq supervisor coordinator))
                        (sb-thread:thread-alive-p supervisor)))
             (loop while (and (http-server-listener-wake-in-progress-p server)
                              (< (%http-server-now) cleanup-deadline))
                   do (%http-server-wait-for-listener-wakeup
                       server cleanup-deadline))
             (%with-http-server-lock (server)
               (setf listener (http-server-listener server)))
             (%http-server-close-listener-object listener)
             (%with-http-server-lock (server)
               (when (eq listener (http-server-listener server))
                 (setf (http-server-listener server) nil
                       (http-server-listener-wakeup-p server) nil)))
             (unless (%http-server-wait-for-connections server deadline)
               (%http-server-force-close-connections server)
               (%http-server-wait-for-connections server cleanup-deadline))
             (let ((workers
                     (%with-http-server-lock
                         (server)
                       (loop for worker being the hash-keys of
                             (http-server-workers server)
                             collect worker))))
               (dolist (worker workers)
                 (unless (eq worker coordinator)
                   (let ((remaining (- cleanup-deadline (%http-server-now))))
                     (when (plusp remaining)
                       (sb-thread:join-thread worker
                                              :timeout remaining
                                              :default nil))))
                 (unless (sb-thread:thread-alive-p worker)
                   (%with-http-server-lock (server)
                     (remhash worker (http-server-workers server))))))
             (let ((store-release-succeeded-p nil)
                   (ticket-key-store-destruction-deferred-p nil)
                   (workers-alive-count 0)
                   (workers-alive-p nil))
               (multiple-value-setq
                   (store-release-succeeded-p
                    ticket-key-store-destruction-deferred-p)
                 (%http-server-release-owned-ticket-key-store server))
               (setf workers-alive-count
                     (%with-http-server-lock (server)
                       (loop for worker being the hash-keys of
                             (http-server-workers server)
                             count (sb-thread:thread-alive-p worker)))
                     workers-alive-p (plusp workers-alive-count))
               (when (or workers-alive-p ticket-key-store-destruction-deferred-p)
                 (handler-case
                     (%http-server-emit-event-once
                      server :shutdown-deferred
                      (list :type :listener-shutdown-deferred
                            :phase :shutdown
                            :worker-count workers-alive-count
                            :ticket-key-store-destruction-deferred-p
                            ticket-key-store-destruction-deferred-p))
                   (http-server-callback-error ()
                     nil)))
               (cond
                 ((and store-release-succeeded-p
                       (not workers-alive-p)
                       (not ticket-key-store-destruction-deferred-p)
                       (not supervisor-alive-p))
                  (%http-server-emit-event-once
                   server :stopped
                   (list :type :listener-stopped :phase :shutdown))
                  (%with-http-server-lock (server)
                    (setf (http-server-supervisor server) nil
                          (http-server-start-token server) nil
                          (http-server-listener-wake-in-progress-p server) nil)
                    (unless (eq (http-server-state server) :failed)
                      (setf (http-server-state server) :stopped))
                    (when (eq coordinator
                              (http-server-shutdown-coordinator server))
                      (setf (http-server-shutdown-coordinator server) nil))
                    (%http-server-broadcast server)))
                 (store-release-succeeded-p
                  (%with-http-server-lock (server)
                    (unless supervisor-alive-p
                      (setf (http-server-supervisor server) nil))
                    (setf (http-server-start-token server) nil
                          (http-server-listener-wake-in-progress-p server) nil)
                    (unless (eq (http-server-state server) :failed)
                      (setf (http-server-state server) :stopping))
                    (%http-server-broadcast server)))
                 (t
                  (%with-http-server-lock (server)
                    (unless supervisor-alive-p
                      (setf (http-server-supervisor server) nil
                            (http-server-shutdown-p server) nil
                            (http-server-stop-deadline server) nil))
                    (setf (http-server-start-token server) nil
                          (http-server-listener-wake-in-progress-p server) nil)
                    (unless (eq (http-server-state server) :failed)
                      (setf (http-server-state server) :stopping))
                    (%http-server-broadcast server))))))
        (%with-http-server-lock (server)
          (when (eq coordinator (http-server-shutdown-coordinator server))
            (setf (http-server-shutdown-coordinator server) nil))
          (%http-server-broadcast server))
        (when (%with-http-server-lock (server)
                (and (http-server-shutdown-p server)
                     (or (eq (http-server-state server) :stopping)
                         (and (eq (http-server-state server) :failed)
                              (http-server-shutdown-deferred-event-emitted-p server)
                              (not (http-server-stopped-event-emitted-p server))))
                     (null (http-server-ticket-key-store-cleanup-error server))
                     (zerop (http-server-connection-count server))
                     (let ((supervisor
                             (http-server-supervisor server)))
                       (or (null supervisor)
                           (not (sb-thread:thread-alive-p supervisor))))
                     (loop for worker being the hash-keys of
                           (http-server-workers server)
                           never (sb-thread:thread-alive-p worker))))
          (%http-server-ensure-shutdown-coordinator server)))
      server))
  #-sbcl
  (defun %http-server-shutdown-coordinator (server)
    (declare (ignore server))
    nil))

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
    (%http-server-supervisor-exited server)))

(defun %http-server-start-listener (server start-token)
  (unless (%http-server-ensure-owned-ticket-key-store server start-token)
    (return-from %http-server-start-listener))
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
  (let ((start-token (gensym "HTTP-SERVER-START-")))
    (%with-http-server-lock (server)
      (case (http-server-state server)
        ((:running :starting :stopping)
         (return-from start-http-server server))
        (:failed
         (error (or (http-server-failure-value server)
                    (make-condition 'http-server-error
                                    :reason-code :server-failed
                                    :phase :startup))))
        (otherwise
         (setf (http-server-state server) :starting
               (http-server-shutdown-p server) nil
               (http-server-stop-deadline server) nil
               (http-server-start-token server) start-token
               (http-server-listener-wakeup-p server) nil
               (http-server-listener-wake-in-progress-p server) nil
               (http-server-started-event-emitted-p server) nil
               (http-server-stopping-event-emitted-p server) nil
               (http-server-stopped-event-emitted-p server) nil
               (http-server-shutdown-deferred-event-emitted-p server) nil))))
    (let ((listener
            (handler-case
                (%http-server-start-listener server start-token)
              (http-server-error (condition)
                (let ((claimed-p nil))
                  (%with-http-server-lock (server)
                    (when (and (eq (http-server-state server) :starting)
                               (eq (http-server-start-token server) start-token))
                      (setf (http-server-state server) :failed
                            (http-server-shutdown-p server) t
                            (http-server-failure-value server) condition
                            claimed-p t)
                      (unless (http-server-stop-deadline server)
                        (setf (http-server-stop-deadline server)
                              (+ (%http-server-now)
                                 (http-server-graceful-timeout server))))
                      (%http-server-broadcast server)))
                  (when claimed-p
                    (error condition))
                  (return-from start-http-server server))))))
      (let ((started-p nil)
            (startup-condition nil))
        (%with-http-server-lock (server)
          (when (and (eq (http-server-state server) :starting)
                     (eq (http-server-start-token server) start-token))
            (setf (http-server-listener server) listener
                  (http-server-port server)
                  (http-kit/network:http-network-listener-port listener)
                  (http-server-state server) :running)
            (handler-case
                (setf (http-server-supervisor server)
                      (sb-thread:make-thread
                       (lambda () (%http-server-supervisor server listener))
                       :name "http server supervisor")
                      started-p t)
              (error ()
                (setf startup-condition
                      (make-condition 'http-server-error
                                      :reason-code :supervisor-start-failed
                                      :phase :startup)
                      (http-server-listener server) nil
                      (http-server-state server) :failed
                      (http-server-shutdown-p server) t
                      (http-server-failure-value server) startup-condition)
                (%http-server-broadcast server)))))
        (unless started-p
          (%http-server-close-listener-object listener)
          (when startup-condition
            (handler-case
                (%http-server-report-error
                 server startup-condition :startup :fatal-p t)
              (http-server-callback-error (callback-condition)
                (%http-server-mark-failed server callback-condition))))))
      server)))

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
         (supervisor nil)
         (coordinator nil)
         (started-p nil)
         (event-error nil)
         (caller sb-thread:*current-thread*))
    (%with-http-server-lock (server)
      (when (eq (http-server-state server) :stopped)
        (return-from stop-http-server server))
      (unless (http-server-shutdown-p server)
        (setf (http-server-shutdown-p server) t
              (http-server-stop-deadline server) (+ (%http-server-now) timeout)
              started-p t)
        (unless (eq (http-server-state server) :failed)
          (setf (http-server-state server) :stopping)))
      (unless (http-server-stop-deadline server)
        (setf (http-server-stop-deadline server) (+ (%http-server-now) timeout)))
      (setf supervisor (http-server-supervisor server))
      (%http-server-broadcast server))
    (when started-p
      (handler-case
          (%http-server-emit-event-once
           server :stopping
           (list :type :listener-stopping :phase :shutdown))
        (http-server-callback-error (condition)
          (setf event-error condition))))
    (setf coordinator (%http-server-ensure-shutdown-coordinator server))
    (when coordinator
      (%http-server-wake-listener server))
    (unless (or (null coordinator)
                (eq caller supervisor)
                (eq caller coordinator))
      (let ((deadline (%http-server-stop-deadline server)))
        (when deadline
          (let ((remaining (- deadline (%http-server-now))))
            (if (plusp remaining)
                (multiple-value-bind (result status)
                    (sb-thread:join-thread coordinator
                                           :timeout remaining
                                           :default nil)
                  (declare (ignore result))
                  (case status
                    (:abort
                     (%http-server-force-close-connections server)
                     (sb-thread:join-thread coordinator))
                    (:timeout
                     (sleep (max 0.0d0 (- deadline (%http-server-now))))
                     (%http-server-force-close-connections server))))
                (%http-server-force-close-connections server))))))
    (when event-error
      (error event-error))
    (let ((cleanup-error
            (%with-http-server-lock (server)
              (http-server-ticket-key-store-cleanup-error server))))
      (when cleanup-error
        (error cleanup-error)))
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
