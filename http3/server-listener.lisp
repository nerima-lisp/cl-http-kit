(in-package #:http-kit/http3)
(progn
  (define-condition http3-server-connection-closed
    (http-kit:http-error http-kit:http-server-error)
  ((close-kind :initarg :close-kind :initform nil
               :reader http3-server-connection-close-kind)
   (close-error-code :initarg :close-error-code :initform nil
                     :reader http3-server-connection-close-error-code)
   (close-reason :initarg :close-reason :initform nil
                 :reader http3-server-connection-close-reason)))
  (define-condition http3-server-stopped (http-kit:http-error) ()))

#+sbcl
(progn
  (defstruct (%http3-server-listener
               (:constructor %make-http3-server-listener))
    server
    quic-server
    socket
    (connections (make-hash-table :test #'eq))
    (draining-p nil)
    (closed-p nil))

  (defstruct (%http3-server-connection
               (:constructor %make-http3-server-connection))
    quic-connection
    server-connection
    http3-client
    (active-request-streams 0)
    highest-request-stream-id
    peer-goaway-id
    (peer-settings-received-p nil)
    (last-activity 0)
    (released-p nil)
    (qpack-lock (sb-thread:make-mutex :name "HTTP/3 QPACK")))

  (defun %http3-server-now ()
    (http-kit::%http-server-now))

  (defun %http3-server-timeout (kind)
    (error 'http-kit:http-timeout
           :message "The HTTP/3 server operation exceeded its deadline."
           :operation :http3-server
           :kind kind))

  (defun %http3-server-sanitize-close-reason (reason)
    (when (stringp reason)
      (with-output-to-string (output)
        (loop for index below (min 256 (length reason))
              for character = (char reason index)
              for code = (char-code character)
              do (write-char
                  (if (or (< code #x20)
                          (<= #x7f code #x9f)
                          (member code '(#x2028 #x2029)))
                      #\Space
                      character)
                  output)))))

  (defun %http3-server-transport-parameters (server)
    (list :initial-max-data 1048576
          :initial-max-stream-data-bidi-local 65536
          :initial-max-stream-data-bidi-remote 65536
          :initial-max-stream-data-uni 65536
          :initial-max-streams-bidi
          (min (1- (ash 1 60))
               (max 1024
                    (* 64 (http-kit::http-server-max-concurrent-streams server))))
          :initial-max-streams-uni 16))

  (defun %http3-server-start (server start-token)
    (declare (ignore start-token))
    (let* ((socket nil)
           (quic-server nil)
           (tls-options
             (list :certificate-chain
                   (http-kit::http-server-certificate-chain server)
                   :private-key (http-kit::http-server-private-key server)
                   :alpn-protocols '("h3")))
           (arguments
             (list :max-connections
                   (http-kit::http-server-max-connections server)
                   :max-handshakes
                   (http-kit::http-server-max-handshakes server)
                   :handshake-timeout
                   (http-kit::http-server-handshake-timeout server)
                   :retry-policy :always
                   :transport-parameters
                   (%http3-server-transport-parameters server)
                   :tls-server-options tls-options)))
      (labels ((cleanup ()
                 (when quic-server
                   (http-kit::%with-http-cleanup
                     (cl-quic-kit:quic-server-close quic-server)))
                 (when (and socket (null quic-server))
                   (http-kit::%with-http-cleanup
                     (cl-quic-kit:udp-close socket)))))
        (handler-case
            (progn
              (%http3-quic-adapter-configure-protection)
              (setf socket
                    (cl-quic-kit:make-udp-socket
                     :local-host (http-kit::http-server-host server)
                     :local-port (http-kit::http-server-port server)))
              (setf quic-server
                    (apply #'cl-quic-kit:make-quic-server
                           :udp-socket socket arguments))
              (%make-http3-server-listener
               :server server
               :quic-server quic-server
               :socket socket))
          (http-kit::http-server-error (condition)
            (cleanup)
            (error condition))
          (error ()
            (cleanup)
            (http-kit::%http-server-configuration-error :listener-open-failed))))))

  (defun %http3-server-port (listener)
    (cl-quic-kit:udp-socket-local-port
     (%http3-server-listener-socket listener)))

  (defun %http3-server-close-listener (listener)
    (let ((server (%http3-server-listener-server listener)))
      (http-kit::%with-http-server-lock (server)
        (unless (%http3-server-listener-closed-p listener)
          (setf (%http3-server-listener-closed-p listener) t)
          (cl-quic-kit:quic-server-close
           (%http3-server-listener-quic-server listener)))))
    nil)

  (defun %http3-server-wake-listener (listener)
    (declare (ignore listener))
    nil)

  (defun %http3-server-connection-snapshot (server)
    (http-kit::%http-server-connection-snapshot server))

  (defun %http3-server-force-close-connections (server)
    (dolist (connection (%http3-server-connection-snapshot server))
      (let ((quic-connection
              (http-kit::%http-server-connection-raw-stream connection)))
        (when quic-connection
          (handler-case
              (http-kit::%with-http-server-lock (server)
                (cl-quic-kit:quic-connection-close
                 quic-connection
                 :error-code :application-error
                 :reason "HTTP/3 server shutdown"))
            (error (condition)
              (http-kit::%http-server-report-error
               server condition :close :protocol :http3)))
          (http-kit::%http-server-release-connection server connection))))
    (http-kit::%http-server-broadcast server)
    nil)

  (defun %http3-server-connection-closed-p (server connection)
    (if (sb-thread:holding-mutex-p (http-kit::http-server-lock server))
        (member (cl-quic-kit:connection-state
                 (%http3-server-connection-quic-connection connection))
                '(:closing :draining :closed))
        (http-kit::%with-http-server-lock (server)
          (member (cl-quic-kit:connection-state
                   (%http3-server-connection-quic-connection connection))
                  '(:closing :draining :closed)))))

  (defun %http3-server-close-connection
      (server connection &key (error-code :no-error) (reason ""))
    (let ((quic-connection
            (%http3-server-connection-quic-connection connection)))
      (http-kit::%with-http-server-lock (server)
        (unless (%http3-server-connection-closed-p server connection)
          (cl-quic-kit:quic-connection-close
           quic-connection :error-code error-code :reason reason)
          (cl-quic-kit:quic-connection-flush quic-connection)))
      (http-kit::%http-server-broadcast server))
    nil)

  (defun %http3-server-close-application-error
      (server connection error-code reason)
    (let ((quic-connection
            (%http3-server-connection-quic-connection connection)))
      (http-kit::%with-http-server-lock (server)
        (unless (%http3-server-connection-closed-p server connection)
          ;; The public close API maps integer codes to QUIC transport errors.
          (cl-quic-kit::connection-close
           quic-connection error-code reason :kind :application)
          (cl-quic-kit:quic-connection-flush quic-connection)))
      (http-kit::%http-server-broadcast server))
    nil)

  (defun %http3-server-register-connection (server listener quic-connection)
    (let ((connection nil)
          (rejection nil))
      (http-kit::%with-http-server-lock (server)
        (cond
          ((not (eq (http-kit::http-server-state server) :running))
           (setf rejection :server-stopping))
          ((>= (http-kit::http-server-connection-count server)
               (http-kit::http-server-max-connections server))
           (setf rejection :max-connections))
          (t
           (setf connection
                 (%make-http3-server-connection
                  :quic-connection quic-connection
                  :server-connection
                  (http-kit::%make-http-server-connection
                   :raw-stream quic-connection
                   :handshake-p nil
                   :peer-address nil
                   :peer-port nil
                   :protocol :http3)
                  :last-activity (%http3-server-now)))
           (incf (http-kit::http-server-connection-count server))
           (setf (gethash
                  (%http3-server-connection-server-connection connection)
                  (http-kit::http-server-connections server))
                 t)
           (setf (gethash quic-connection
                          (%http3-server-listener-connections listener))
                 connection))))
      (values connection rejection)))

  (defun %http3-server-stream-open
      (server connection stream-type)
    (declare (ignore stream-type))
    (http-kit::%with-http-server-lock (server)
      (cl-quic-kit:quic-connection-open-stream
       (%http3-server-connection-quic-connection connection)
       :type :unidirectional)))

  (defun %http3-server-stream-read
      (server connection stream &key timeout deadline)
    (declare (ignore timeout))
    (loop
      (multiple-value-bind (chunk fin-p ready-p)
          (http-kit::%with-http-server-lock (server)
            (let ((quic-connection
                    (%http3-server-connection-quic-connection connection)))
              (cond
                ((%http3-server-connection-closed-p server connection)
                 (values nil t t))
                ((or (plusp (cl-quic-kit:stream-readable-bytes stream))
                     (cl-quic-kit:stream-finished-p stream)
                     (cl-quic-kit:stream-reset-p stream))
                 (multiple-value-bind (input ended-p)
                     (cl-quic-kit:quic-connection-read-stream
                      quic-connection stream)
                   (cl-quic-kit:quic-connection-flush quic-connection)
                   (setf (%http3-server-connection-last-activity connection)
                         (%http3-server-now))
                   (values input (or ended-p
                                     (cl-quic-kit:stream-reset-p stream)) t)))
                (t
                 (values nil nil nil)))))
        (when (and ready-p
                   (or fin-p (and chunk (plusp (length chunk)))))
          (return (values chunk fin-p))))
      (when (and deadline (>= (%http3-server-now) deadline))
        (%http3-server-timeout :read))
      (http-kit::%http-server-wait-for-listener-wakeup
       server
       (let ((poll-deadline (+ (%http3-server-now) 0.01d0)))
         (if deadline (min deadline poll-deadline) poll-deadline)))))

  (defun %http3-server-stream-write
      (server connection stream octets &key fin-p timeout deadline)
    (declare (ignore timeout))
    (let* ((quic-connection
             (%http3-server-connection-quic-connection connection))
           (expected-offset
             (http-kit::%with-http-server-lock (server)
               (let ((expected (+ (cl-quic-kit:stream-send-offset stream)
                                  (length octets))))
                 (cl-quic-kit:quic-connection-write-stream
                  quic-connection stream octets :fin-p fin-p)
                 (cl-quic-kit:quic-connection-flush quic-connection)
                 (setf (%http3-server-connection-last-activity connection)
                       (%http3-server-now))
                 expected))))
      (loop
        (when (or (zerop (length octets))
                  (http-kit::%with-http-server-lock (server)
                    (and (>= (cl-quic-kit:stream-send-offset stream)
                             expected-offset)
                         (not
                          (cl-quic-kit:client-stream-write-pending-p
                           (cl-quic-kit::%quic-connection-stream-engine
                            quic-connection)
                           stream)))))
          (return t))
        (when (and deadline (>= (%http3-server-now) deadline))
          (%http3-server-timeout :write))
        (http-kit::%http-server-wait-for-listener-wakeup
         server
         (let ((poll-deadline (+ (%http3-server-now) 0.01d0)))
           (if deadline (min deadline poll-deadline) poll-deadline))))))

  (defun %http3-server-queue-stream-event
      (quic-connection stream event)
    (let ((engine
            (cl-quic-kit::%quic-connection-stream-engine quic-connection))
          (stream-id (cl-quic-kit:stream-id stream)))
      (case (getf event :type)
        (:reset-stream
         (cl-quic-kit::%client-queue-frame
          engine
          (cl-quic-kit:make-frame
           :reset-stream
           :stream-id stream-id
           :application-protocol-error-code (getf event :error-code)
           :final-size (getf event :final-size))
          :1-rtt))
        (:stop-sending
         (cl-quic-kit::%client-queue-frame
          engine
          (cl-quic-kit:make-frame
           :stop-sending
           :stream-id stream-id
           :application-protocol-error-code (getf event :error-code))
          :1-rtt))))
    nil)

  (defun %http3-server-stream-cancel-locked
      (connection stream error-code)
    (let* ((quic-connection
             (%http3-server-connection-quic-connection connection))
           (reset-event
             (when (eq (cl-quic-kit:stream-direction stream) :bidirectional)
               (cl-quic-kit:stream-reset-send stream error-code)))
           (stop-event (cl-quic-kit:stream-stop-sending stream error-code)))
      ;; Peer-created unidirectional streams have no local send side to reset.
      (when (and (listp reset-event) (getf reset-event :type))
        (%http3-server-queue-stream-event quic-connection stream reset-event))
      (when (and (listp stop-event) (getf stop-event :type))
        (%http3-server-queue-stream-event quic-connection stream stop-event))
      (cl-quic-kit:quic-connection-flush quic-connection)
      (cl-quic-kit:quic-connection-close-stream quic-connection stream))
    t)

  (defun %http3-server-stream-cancel
      (server connection stream error-code)
    (http-kit::%with-http-server-lock (server)
      (%http3-server-stream-cancel-locked connection stream error-code)))

  (defun %http3-server-make-client (server connection)
    (let ((client
            (make-http3-client
             :open-stream
             (lambda (request &key stream-type timeout deadline)
               (declare (ignore request timeout deadline))
               (%http3-server-stream-open server connection stream-type))
             :write-stream
             (lambda (stream octets &key fin-p timeout deadline)
               (%http3-server-stream-write
                server connection stream octets
                :fin-p fin-p :timeout timeout :deadline deadline))
             :read-stream
             (lambda (stream &key timeout deadline)
               (%http3-server-stream-read
                server connection stream :timeout timeout :deadline deadline))
             :cancel-stream
             (lambda (stream &key (error-code #x10c) timeout deadline)
               (declare (ignore timeout deadline))
               (%http3-server-stream-cancel
                server connection stream error-code))
             :max-header-bytes
             (http-kit::http-server-max-header-bytes server)
             :max-fields (http-kit::http-server-max-fields server)
             :qpack-settings '()
             ;; Nested transport operations can serialize on this thread again.
             :serialize
             (lambda (thunk)
               (let ((lock (%http3-server-connection-qpack-lock connection)))
                 (if (eq (sb-thread:mutex-owner lock)
                         sb-thread:*current-thread*)
                     (funcall thunk)
                   (sb-thread:with-mutex (lock)
                     (funcall thunk))))))))
      (setf (http3-client-peer-control-state client)
            (make-http3-control-state :peer-role :client)
            (%http3-server-connection-http3-client connection) client)
      client))

  (defun %http3-server-report-error
      (server connection condition phase)
    (http-kit::%http-server-report-error
     server condition phase :protocol :http3
     :peer-address nil :peer-port nil
     :callback-condition
     (and (eq phase :session)
          (typep condition 'http3-server-connection-closed)
          condition))
    (when connection
      (http-kit::%http-server-emit-event
       server
       (list :type :connection-error
             :phase phase
             :protocol :http3)))
    nil)

  (defun %http3-server-release-connection (server listener connection)
    (let ((common-connection
            (%http3-server-connection-server-connection connection))
          (client (%http3-server-connection-http3-client connection))
          (release-p nil))
      (http-kit::%with-http-server-lock (server)
        (unless (%http3-server-connection-released-p connection)
          (setf (%http3-server-connection-released-p connection) t
                release-p t)))
      (when release-p
        (when client
          (http-kit::%with-http-cleanup
            (close-http3-client client)))
        (remhash (%http3-server-connection-quic-connection connection)
                 (%http3-server-listener-connections listener))
        (http-kit::%http-server-release-connection server common-connection)
        (http-kit::%http-server-emit-event
         server
         (list :type :connection-closed
               :phase :shutdown
               :protocol :http3)))
      nil))

  (defun %http3-server-control-worker
      (server listener connection stream)
    (declare (ignore listener))
    (let ((client (%http3-server-connection-http3-client connection)))
      (handler-case
          (http-kit/http3:serve-http3-control-stream
           stream
           :read-stream
           (lambda (input &key timeout deadline)
             (%h3-read-client-stream
              client input timeout deadline))
           :peer-role :client
           :on-settings
           (lambda (settings state)
             (declare (ignore settings state))
             (http-kit::%with-http-server-lock (server)
               (setf (%http3-server-connection-peer-settings-received-p connection)
                     t)
               (http-kit::%http-server-broadcast server)))
           :on-goaway
           (lambda (identifier state)
             (declare (ignore state))
             (http-kit::%with-http-server-lock (server)
               (setf (%http3-server-connection-peer-goaway-id connection)
                     identifier))))
        (http3-connection-protocol-error (condition)
          (error condition))
        (http-protocol-error (condition)
          (%h3-promote-http3-control-error condition)))))

  ;; Blocking QUIC reads must not hold the shared client-state serializer.
  (defun %http3-server-qpack-encoder-worker (connection stream)
    (declare (ignore stream))
    (let ((client (%http3-server-connection-http3-client connection)))
      (loop
        (multiple-value-bind (events ended-p)
            (%read-http3-qpack-encoder-stream-serialized client :deadline nil)
          (declare (ignore events))
          (when ended-p (return))))))

  (defun %http3-server-qpack-decoder-worker (connection stream)
    (declare (ignore stream))
    (let ((client (%http3-server-connection-http3-client connection)))
      (loop
        (multiple-value-bind (events ended-p)
            (%read-http3-qpack-decoder-stream-serialized client :deadline nil)
          (declare (ignore events))
          (when ended-p (return))))))

  (defun %http3-server-discard-stream (server connection stream)
    (let ((client (%http3-server-connection-http3-client connection)))
      (labels ((finished-p ()
                 (%h3-with-scheduler-lock (client)
                   (let ((entry
                           (assoc
                            stream
                            (http3-client-discard-peer-unidirectional-streams client)
                            :test #'eq)))
                     (and entry
                          (member (cdr entry) '(:finished :cancelled) :test #'eq)))))
               (forget-stream ()
                 (%h3-with-scheduler-lock (client)
                   (setf (http3-client-discard-peer-unidirectional-streams client)
                         (remove stream
                                 (http3-client-discard-peer-unidirectional-streams
                                  client)
                                 :key #'car :test #'eq)))))
        (unwind-protect
             (unless (finished-p)
               (loop
                 (multiple-value-bind (chunk fin-p)
                     (%http3-server-stream-read server connection stream)
                   (declare (ignore chunk))
                   (when fin-p (return)))))
          (forget-stream)))))

  (defun %http3-server-unidirectional-worker
      (server listener connection stream)
    (handler-case
        (let* ((client (%http3-server-connection-http3-client connection))
               (deadline (+ (%http3-server-now)
                            (http-kit::http-server-request-timeout server)))
               (kind
                 (handler-case
                     (accept-http3-peer-unidirectional-stream
                      client stream :deadline deadline)
                   (http-kit:http-timeout (condition)
                     (%http3-server-stream-cancel
                      server connection stream #x103)
                     (error condition)))))
          (case kind
            (:control
             (%http3-server-control-worker server listener connection stream))
            (:qpack-encoder
             (%http3-server-qpack-encoder-worker connection stream))
            (:qpack-decoder
             (%http3-server-qpack-decoder-worker connection stream))
            (:unknown
             (%http3-server-discard-stream server connection stream))
            (:push
             (%http3-server-close-connection
              server connection
              :error-code :application-error
              :reason "HTTP/3 client push stream is invalid"))))
      (http-kit::http-server-callback-error (condition)
        (error condition))
      (http3-connection-protocol-error (condition)
        (unless (or (%http3-server-connection-closed-p server connection)
                    (not (http-kit::%http-server-running-p server)))
          (%http3-server-report-error
           server connection condition :control-stream)
          (%http3-server-close-application-error
           server connection
           (http3-connection-protocol-error-quic-error-code condition)
           (http-kit:http-error-message condition))))
      (error (condition)
        (unless (or (%http3-server-connection-closed-p server connection)
                    (not (http-kit::%http-server-running-p server)))
          (%http3-server-report-error
           server connection condition :control-stream)
          (%http3-server-close-connection
           server connection
           :error-code :application-error
           :reason "HTTP/3 critical stream failed")))))

  (defun %http3-server-await-peer-settings
      (server connection deadline)
    (loop
      (when (%http3-server-connection-peer-settings-received-p connection)
        (return t))
      (when (%http3-server-connection-closed-p server connection)
        (let* ((quic-connection
                 (%http3-server-connection-quic-connection connection))
               (close-frame
                 (cl-quic-kit:connection-close-frame quic-connection))
               (close-kind
                 (cl-quic-kit:connection-close-kind quic-connection))
               (close-error-code
                 (and close-frame
                      (cl-quic-kit:frame-field close-frame :error-code)))
               (close-reason
                 (and close-kind
                      (cl-quic-kit:quic-connection-closed-reason
                       quic-connection))))
          (error (quote http3-server-connection-closed)
                 :message "The HTTP/3 connection closed before peer SETTINGS arrived."
                 :operation :http3-server
                 :reason-code :connection-error
                 :phase :session
                 :close-kind close-kind
                 :close-error-code close-error-code
                 :close-reason
                 (%http3-server-sanitize-close-reason close-reason))))
      (unless (http-kit::%http-server-running-p server)
        (error (quote http3-server-stopped)
               :message "The HTTP/3 server stopped before peer SETTINGS arrived."
               :operation :http3-server))
      (when (and deadline (>= (%http3-server-now) deadline))
        (%http3-server-timeout :settings))
      (http-kit::%http-server-wait-for-listener-wakeup
       server
       (let ((poll-deadline (+ (%http3-server-now) 0.01d0)))
         (if deadline (min deadline poll-deadline) poll-deadline)))))

  (defun %http3-server-reserve-body-bytes (server amount)
    (http-kit::%with-http-server-lock (server)
      (let ((observed (+ (http-kit::http-server-buffered-body-bytes server)
                         amount)))
        (http-kit::%check-limit
         :body observed
         (http-kit::http-server-max-buffered-body-bytes server)
         :operation :http3-listener)
        (incf (http-kit::http-server-buffered-body-bytes server) amount)))
    amount)

  (defun %http3-server-release-body-bytes (server amount)
    (http-kit::%with-http-server-lock (server)
      (unless (and (integerp amount)
                   (<= 0 amount
                       (http-kit::http-server-buffered-body-bytes server)))
        (error 'http-kit::http-server-error
               :reason-code :body-buffer-accounting
               :phase :session))
      (decf (http-kit::http-server-buffered-body-bytes server) amount))
    amount)

  (defun %http3-server-request-worker
      (server connection stream stream-id deadline)
    (let ((reported-p nil)
          (serving-p nil)
          (client (%http3-server-connection-http3-client connection))
          (quic-connection
            (%http3-server-connection-quic-connection connection)))
      (unwind-protect
           (handler-case
               (progn
                 (%http3-server-await-peer-settings server connection deadline)
                 (setf serving-p t)
                 (serve-http3-request-stream
                  stream
                  (http-kit::http-server-handler server)
                  :read-stream
                  (lambda (input &key timeout deadline)
                    (%h3-read-client-stream client input timeout deadline))
                  :write-stream
                  (lambda (output octets &key fin-p timeout deadline)
                    (%http3-server-stream-write
                     server connection output octets
                     :fin-p fin-p :timeout timeout :deadline deadline))
                  :close-stream
                  (lambda (output &key condition)
                    (when condition
                      (if (typep condition 'http3-connection-protocol-error)
                          (%http3-server-close-application-error
                           server connection
                           (http3-connection-protocol-error-quic-error-code
                            condition)
                           (http-kit:http-error-message condition))
                        (%http3-server-stream-cancel
                         server connection output
                         (if (typep condition 'http-kit:http-size-limit-exceeded)
                             #x107
                             #x10c)))))
                  :qpack-decoder-context
                  (http3-client-qpack-decoder-context client)
                  :qpack-http3-decoder-context
                  (http3-client-qpack-http3-decoder-context client)
                  :qpack-serialize (http3-client-serialize client)
                  :await-qpack #'http-message-kit:qpack-resume-blocked-stream
                  :on-qpack-section-acknowledgment
                  (lambda (acknowledged-stream-id)
                    (%h3-write-client-qpack-section-ack-serialized
                     client acknowledged-stream-id nil deadline))
                  :max-header-bytes
                  (http-kit::http-server-max-header-bytes server)
                  :max-fields (http-kit::http-server-max-fields server)
                  :max-body-bytes (http-kit::http-server-max-body-bytes server)
                  :stream-id stream-id
                  :deadline deadline
                  :reserve-body-bytes
                  (lambda (amount)
                    (%http3-server-reserve-body-bytes server amount))
                  :release-body-bytes
                  (lambda (amount)
                    (%http3-server-release-body-bytes server amount))
                  :on-error
                  (lambda (condition request)
                    (declare (ignore request))
                    (setf reported-p t)
                    (%http3-server-report-error
                     server connection condition :session))))
             (http-kit::http-server-callback-error (condition)
               (http-kit::%http-server-mark-failed server condition))
             (error (condition)
               (unless serving-p
                 (http-kit::%with-http-server-lock (server)
                   (%http3-server-stream-cancel-locked
                    connection stream #x10c)))
               (unless (or reported-p
                           (not (http-kit::%http-server-running-p server)))
                 (%http3-server-report-error
                  server connection condition :session))))
        (http-kit::%with-http-server-lock (server)
          (cl-quic-kit:quic-connection-close-stream quic-connection stream)
          (decf (%http3-server-connection-active-request-streams connection))
          (when (and (member (http-kit::http-server-state server)
                             '(:stopping :failed))
                     (zerop
                      (%http3-server-connection-active-request-streams connection))
                     (not (%http3-server-connection-closed-p server connection)))
            (cl-quic-kit:quic-connection-close
             quic-connection :error-code :no-error
             :reason "HTTP/3 request drain complete"))
          (http-kit::%http-server-broadcast server)))))

  (defun %http3-server-start-worker (server function name &optional connection)
    (let ((worker nil))
      (handler-case
          (http-kit::%with-http-server-lock (server)
            (setf worker
                  (sb-thread:make-thread
                   (lambda ()
                     (http-kit::%with-http-server-lock (server) nil)
                     (unwind-protect
                          (handler-case
                              (funcall function)
                            (http-kit::http-server-callback-error (condition)
                              (http-kit::%http-server-mark-failed
                               server condition))
                            (error (condition)
                              (when (and (http-kit::%http-server-running-p server)
                                         (or (null connection)
                                             (not (%http3-server-connection-closed-p
                                                   server connection))))
                                (http-kit::%http-server-report-error
                                 server condition :stream :protocol :http3))))
                       (progn
                         (http-kit::%with-http-server-lock (server)
                           (remhash sb-thread:*current-thread*
                                    (http-kit::http-server-workers server))
                           (http-kit::%http-server-broadcast server))
                         (when (http-kit::%with-http-server-lock (server)
                                 (and (http-kit::http-server-shutdown-p server)
                                      (member (http-kit::http-server-state server)
                                              '(:stopping :failed))
                                      (let ((supervisor
                                              (http-kit::http-server-supervisor server)))
                                        (or (null supervisor)
                                            (not (sb-thread:thread-alive-p
                                                  supervisor))))))
                           (http-kit::%http-server-ensure-shutdown-coordinator
                            server)))))
                   :name name))
            (setf (gethash worker (http-kit::http-server-workers server)) t))
        (error ()
          (http-kit::%http-server-configuration-error :worker-start-failed)))
      worker))

  (defun %http3-server-start-request-worker
      (server connection stream stream-id deadline)
    (handler-case
        (%http3-server-start-worker
         server
         (lambda ()
           (%http3-server-request-worker
            server connection stream stream-id deadline))
         "HTTP/3 request stream"
         connection)
      (http-kit::http-server-error (condition)
        (http-kit::%with-http-server-lock (server)
          (decf (%http3-server-connection-active-request-streams connection))
          (http-kit::%http-server-broadcast server))
        (error condition))))

  (defun %http3-server-start-unidirectional-worker
      (server listener connection stream)
    (%http3-server-start-worker
     server
     (lambda ()
       (%http3-server-unidirectional-worker
        server listener connection stream))
     "HTTP/3 unidirectional stream"
     connection))

  (defun %http3-server-accept-quic-connection
      (server listener quic-connection)
    (let ((alpn (cl-quic-kit:quic-connection-alpn quic-connection)))
      (unless (and (stringp alpn) (string= alpn "h3"))
        (http-kit::%with-http-server-lock (server)
          (cl-quic-kit:quic-connection-close
           quic-connection :error-code :application-error
           :reason "HTTP/3 requires ALPN h3"))
        (return-from %http3-server-accept-quic-connection nil))
      (multiple-value-bind (connection rejection)
          (%http3-server-register-connection server listener quic-connection)
        (when rejection
          (http-kit::%with-http-server-lock (server)
            (cl-quic-kit:quic-connection-close
             quic-connection :error-code :application-error
             :reason "HTTP/3 listener is at capacity"))
          (return-from %http3-server-accept-quic-connection nil))
        (handler-case
            (progn
              (%http3-server-make-client server connection)
              (http-kit::%http-server-emit-event
               server
               (list :type :connection-opened
                     :phase :handshake
                     :protocol :http3))
              connection)
          (error (condition)
            (%http3-server-report-error
             server connection condition :handshake)
            (%http3-server-close-connection
             server connection
             :error-code :application-error
             :reason "HTTP/3 connection setup failed")
            (error condition))))))

  (defun %http3-server-drain-connection (server connection)
    (let* ((highest
             (%http3-server-connection-highest-request-stream-id connection))
           (goaway-id (if highest (+ highest 4) 0))
           (control-stream
             (http3-client-control-stream
              (%http3-server-connection-http3-client connection)))
           (frame
             (encode-http3-frame
              (make-http3-frame
               :type +http3-goaway-type+
               :payload (http3-varint-encode goaway-id)))))
      (http-kit::%with-http-server-lock (server)
        (cl-quic-kit:quic-connection-write-stream
         (%http3-server-connection-quic-connection connection)
         control-stream frame)
        (cl-quic-kit:quic-connection-flush
         (%http3-server-connection-quic-connection connection)))
      (http-kit::%http-server-emit-event
       server
       (list :type :http3-goaway-sent
             :phase :shutdown
             :protocol :http3
             :goaway-id goaway-id)))
    nil)

  (defun %http3-server-begin-drain (server listener)
    (unless (%http3-server-listener-draining-p listener)
      (setf (%http3-server-listener-draining-p listener) t)
      (let ((connections
              (http-kit::%with-http-server-lock (server)
                (loop for connection being the hash-values of
                      (%http3-server-listener-connections listener)
                      collect connection)))
            (deadline (http-kit::%http-server-stop-deadline server)))
        (dolist (connection connections)
          (%http3-server-drain-connection server connection))
        (http-kit::%with-http-server-lock (server)
          (cl-quic-kit:quic-server-shutdown
           (%http3-server-listener-quic-server listener)
           :grace-period (max 0 (- deadline (%http3-server-now)))
           :error-code :no-error
           :reason "HTTP/3 server shutdown")))
      (http-kit::%http-server-broadcast server))
    nil)

  (defun %http3-server-stream-stop-snapshot (listener)
    (let ((snapshot (make-hash-table :test #'eq)))
      (maphash
       (lambda (quic-connection connection)
         (declare (ignore connection))
         (let* ((engine
                  (cl-quic-kit::%quic-connection-stream-engine quic-connection))
                (stopped-streams (make-hash-table :test #'eql))
                (streams (cl-quic-kit::quic-client-streams engine)))
           (when (hash-table-p streams)
             (maphash
              (lambda (stream-id stream)
                (when (cl-quic-kit:stream-stopped-p stream)
                  (setf (gethash stream-id stopped-streams) t)))
              streams))
           (setf (gethash quic-connection snapshot) stopped-streams)))
       (%http3-server-listener-connections listener))
      snapshot))

  (defun %http3-server-newly-stopped-connections (listener snapshot)
    (let ((connections nil))
      (maphash
       (lambda (quic-connection connection)
         (let* ((engine
                  (cl-quic-kit::%quic-connection-stream-engine quic-connection))
                (previous (gethash quic-connection snapshot))
                (streams (cl-quic-kit::quic-client-streams engine)))
           (when (hash-table-p streams)
             (maphash
              (lambda (stream-id stream)
                (when (and (cl-quic-kit:stream-stopped-p stream)
                           (not (and previous
                                     (gethash stream-id previous))))
                  (pushnew connection connections :test #'eq)))
              streams))))
       (%http3-server-listener-connections listener))
      connections))

  (defun %http3-server-isolate-peer-error
      (server listener connection condition)
    (let ((quic-connection
            (%http3-server-connection-quic-connection connection)))
      (http-kit::%with-http-server-lock (server)
        (unless (%http3-server-connection-closed-p server connection)
          (handler-case
              (cl-quic-kit:quic-connection-close
               quic-connection
               :error-code :application-error
               :reason "HTTP/3 connection failed")
            (cl-quic-kit:flow-control-error () nil))))
      (%http3-server-report-error server connection condition :connection)
      (%http3-server-release-connection server listener connection)))

  (defun %http3-server-poll-listener (server listener)
    (let ((quic-server (%http3-server-listener-quic-server listener))
          (accepted-connections nil)
          (unidirectional-streams nil)
          (request-streams nil)
          (rejected-streams nil)
          (released-connections nil)
          (failed-connections nil)
          (now (%http3-server-now))
          (closed-p nil))
      (http-kit::%with-http-server-lock (server)
        (let ((snapshot (%http3-server-stream-stop-snapshot listener)))
          (handler-case
              (setf closed-p (cl-quic-kit:quic-server-poll quic-server now))
            (cl-quic-kit:flow-control-error (condition)
              (let ((affected
                      (%http3-server-newly-stopped-connections
                       listener snapshot)))
                (unless affected
                  (error condition))
                (dolist (connection affected)
                  (push (cons connection condition) failed-connections))))))
        (loop for connection = (cl-quic-kit:quic-server-accept-connection
                                quic-server)
              while connection
              do (push connection accepted-connections))
        (maphash
         (lambda (quic-connection connection)
           (declare (ignore quic-connection))
           (unless (assoc connection failed-connections :test #'eq)
             (if (%http3-server-connection-closed-p server connection)
                 (push connection released-connections)
               (handler-case
                   (progn
                 (loop for stream =
                       (cl-quic-kit:quic-connection-accept-stream
                        (%http3-server-connection-quic-connection connection)
                        :type :unidirectional)
                       while stream
                       do (setf (%http3-server-connection-last-activity connection)
                                now)
                          (push (cons connection stream)
                                unidirectional-streams))
                 (loop for stream =
                       (cl-quic-kit:quic-connection-accept-stream
                        (%http3-server-connection-quic-connection connection)
                        :type :bidirectional)
                       while stream
                       for stream-id = (cl-quic-kit:stream-id stream)
                       for rejection-reason =
                         (cond
                           ((%http3-server-listener-draining-p listener)
                            :draining)
                           ((>= (%http3-server-connection-active-request-streams
                                 connection)
                                (http-kit::http-server-max-concurrent-streams
                                 server))
                            :max-concurrent-streams))
                       do (setf (%http3-server-connection-last-activity connection)
                                now)
                          (if rejection-reason
                              (progn
                                (%http3-server-stream-cancel-locked
                                 connection stream #x10b)
                                (push (list stream-id rejection-reason)
                                      rejected-streams))
                            (progn
                              (incf
                               (%http3-server-connection-active-request-streams
                                connection))
                              (setf
                               (%http3-server-connection-highest-request-stream-id
                                connection)
                               (max stream-id
                                    (or
                                     (%http3-server-connection-highest-request-stream-id
                                      connection)
                                     0)))
                              (push (list connection stream stream-id
                                          (+ now
                                             (http-kit::http-server-request-timeout
                                              server)))
                                    request-streams))))
                 (when (and (zerop
                             (%http3-server-connection-active-request-streams
                              connection))
                            (http-kit::http-server-idle-timeout server)
                            (>= (- now
                                   (%http3-server-connection-last-activity connection))
                                (http-kit::http-server-idle-timeout server)))
                   (cl-quic-kit:quic-connection-close
                    (%http3-server-connection-quic-connection connection)
                    :error-code :no-error
                    :reason "HTTP/3 idle timeout"))
                 (when (and (%http3-server-listener-draining-p listener)
                            (zerop
                             (%http3-server-connection-active-request-streams
                              connection)))
                   (cl-quic-kit:quic-connection-close
                    (%http3-server-connection-quic-connection connection)
                    :error-code :no-error
                    :reason "HTTP/3 request drain complete"))
                 (cl-quic-kit:quic-connection-flush
                  (%http3-server-connection-quic-connection connection)))
                 (cl-quic-kit:flow-control-error (condition)
                   (push (cons connection condition) failed-connections))))))
         (%http3-server-listener-connections listener))
        (http-kit::%http-server-broadcast server))
      (dolist (entry (nreverse failed-connections))
        (%http3-server-isolate-peer-error
         server listener (car entry) (cdr entry)))
      (dolist (entry (nreverse rejected-streams))
        (http-kit::%http-server-emit-event
         server
         (list :type :http3-request-stream-rejected
               :phase :request
               :protocol :http3
               :stream-id (first entry)
               :reason (second entry))))
      (dolist (connection released-connections)
        (%http3-server-release-connection server listener connection))
      (dolist (quic-connection (nreverse accepted-connections))
        (%http3-server-accept-quic-connection server listener quic-connection))
      (dolist (entry (nreverse unidirectional-streams))
        (%http3-server-start-unidirectional-worker
         server listener (car entry) (cdr entry)))
      (dolist (entry (nreverse request-streams))
        (%http3-server-start-request-worker
         server (first entry) (second entry) (third entry) (fourth entry)))
      (when closed-p
        (setf (%http3-server-listener-closed-p listener) t)
        (dolist (connection
                 (http-kit::%with-http-server-lock (server)
                   (loop for connection being the hash-values of
                         (%http3-server-listener-connections listener)
                         collect connection)))
          (%http3-server-release-connection server listener connection)))
      closed-p))

  (defun %http3-server-supervisor (server listener)
    (loop
      do (unless (http-kit::%http-server-running-p server)
           (%http3-server-begin-drain server listener))
         (when (%http3-server-poll-listener server listener)
           (return))
         (http-kit::%http-server-wait-for-listener-wakeup
          server (+ (%http3-server-now) 0.005d0)))
    nil)

  (setf http-kit::*http-server-http3-hooks*
        (list :listener-p #'%http3-server-listener-p
              :start #'%http3-server-start
              :port #'%http3-server-port
              :supervisor #'%http3-server-supervisor
              :close-listener #'%http3-server-close-listener
              :wake #'%http3-server-wake-listener
              :force-close #'%http3-server-force-close-connections)))
