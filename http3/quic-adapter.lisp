(in-package #:http-kit/http3)

(defstruct (http3-quic-adapter
            (:constructor %make-http3-quic-adapter))
  "A synchronous HTTP/3 client backed by cl-quic-kit."
  quic-client
  http3-client
  #+sbcl (owner-lock (sb-thread:make-mutex :name "HTTP/3 QUIC adapter"))
  #-sbcl (owner-lock nil)
  owner-thread
  pending-peer-unidirectional-streams
  stream-prefixes
  stream-prefix-sent
  (max-peer-unidirectional-streams +http3-default-max-peer-unidirectional-streams+)
  (processing-peer-critical-streams-p nil :type boolean)
  (started-p nil :type boolean)
  (closed-p nil :type boolean)
  (poll-interval 0 :type real))

(defun %http3-quic-adapter-error (message &optional detail)
  (error 'http-protocol-error
         :message message
         :operation :http3-quic-adapter
         :detail detail))

(progn
  (defun %http3-quic-adapter-timeout (message)
    (error 'http-kit:http-timeout
           :message message
           :operation :http3-quic-adapter
           :kind :deadline))

  (defun %http3-quic-adapter-call-with-owner (adapter thunk)
    #+sbcl
    (let ((thread sb-thread:*current-thread*)
          (bootstrap-claimed-p nil)
          (client nil))
      (sb-thread:with-mutex ((http3-quic-adapter-owner-lock adapter))
        (let ((owner (http3-quic-adapter-owner-thread adapter)))
          (when (and owner (not (eq owner thread)))
            (error 'http-kit:http-connection-error
                   :message "The HTTP/3 QUIC client is owned by another thread."
                   :operation :http3-quic-client
                   :request-sent-p nil))
          (when (and (null owner)
                     (null (http3-quic-adapter-http3-client adapter)))
            (setf (http3-quic-adapter-owner-thread adapter) thread
                  bootstrap-claimed-p t)))
        (setf client (http3-quic-adapter-http3-client adapter)))
      (unwind-protect
           (if client
               (%h3-call-with-scheduler-owner client thunk)
               (funcall thunk))
        (when bootstrap-claimed-p
          (sb-thread:with-mutex ((http3-quic-adapter-owner-lock adapter))
            (setf (http3-quic-adapter-owner-thread adapter) nil)))))
    #-sbcl
    (funcall thunk))

  (defun %http3-quic-adapter-guard-callback (adapter callback)
    (when callback
      (lambda (&rest arguments)
        (%http3-quic-adapter-call-with-owner
         adapter
         (lambda ()
           (apply callback arguments))))))

  (defun %http3-quic-adapter-guard-client-callbacks (adapter client)
    (setf (http3-client-serialize client)
          (lambda (thunk)
            (%http3-quic-adapter-call-with-owner adapter thunk))
          (http3-client-open-stream client)
          (%http3-quic-adapter-guard-callback
           adapter (http3-client-open-stream client))
          (http3-client-write-stream client)
          (%http3-quic-adapter-guard-callback
           adapter (http3-client-write-stream client))
          (http3-client-try-write-stream client)
          (%http3-quic-adapter-guard-callback
           adapter (http3-client-try-write-stream client))
          (http3-client-read-stream client)
          (%http3-quic-adapter-guard-callback
           adapter (http3-client-read-stream client))
          (http3-client-poll-function client)
          (%http3-quic-adapter-guard-callback
           adapter (http3-client-poll-function client))
          (http3-client-try-read-stream client)
          (%http3-quic-adapter-guard-callback
           adapter (http3-client-try-read-stream client))
          (http3-client-stream-write-pending-p client)
          (%http3-quic-adapter-guard-callback
           adapter (http3-client-stream-write-pending-p client))
          (http3-client-accept-peer-unidirectional-stream client)
          (%http3-quic-adapter-guard-callback
           adapter (http3-client-accept-peer-unidirectional-stream client))
          (http3-client-close-stream client)
          (%http3-quic-adapter-guard-callback
           adapter (http3-client-close-stream client))
          (http3-client-close-connection client)
          (%http3-quic-adapter-guard-callback
           adapter (http3-client-close-connection client)))
    client))

(defun %http3-quic-adapter-property-list-p (value)
  (and (listp value)
       (evenp (length value))
       (loop for tail on value by #'cddr
             always (keywordp (car tail)))))

(defun %http3-quic-adapter-now ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun %http3-quic-adapter-configure-protection ()
  (cl-quic-kit.protection:configure-crypto-backend
   :hkdf-extract #'crypto-kit:hkdf-extract
   :hkdf-expand #'crypto-kit:hkdf-expand
   :aead-seal #'crypto-kit:aead-seal
   :aead-open #'crypto-kit:aead-open
   :aes-ecb #'crypto-kit:aes-encrypt-block
   :chacha20 #'crypto-kit:chacha20-keystream
   :constant-time-equal #'crypto-kit:constant-time-equal))

(defun %http3-quic-adapter-deadline (timeout deadline)
  (let ((timeout-deadline
          (and (numberp timeout)
               (+ (%http3-quic-adapter-now) timeout))))
    (cond
      ((and timeout-deadline (numberp deadline))
       (min timeout-deadline deadline))
      (timeout-deadline timeout-deadline)
      ((numberp deadline) deadline)
      (t nil))))

(defun %http3-quic-adapter-close-detail (quic-client)
  (let* ((connection (cl-quic-kit:quic-client-connection quic-client))
         (frame (cl-quic-kit:connection-close-frame connection))
         (kind (cl-quic-kit:connection-close-kind connection))
         (reason (and kind (cl-quic-kit:quic-connection-closed-reason connection)))
         (detail (list :state (cl-quic-kit:connection-state connection)
                       :client-closed-p
                       (cl-quic-kit:quic-client-closed-p quic-client))))
    (when kind
      (setf detail (append detail (list :closed-kind kind))))
    (when frame
      (setf detail
            (append detail
                    (list :closed-error
                          (cl-quic-kit:frame-field frame :error-code)))))
    (when reason
      (setf detail (append detail (list :closed-reason reason))))
    detail))

(progn
  (defun %http3-quic-adapter-await-stream-write
    (quic-client stream &key timeout deadline adapter)
  (let* ((end (%http3-quic-adapter-deadline timeout deadline))
         (http3-client
           (and adapter (http3-quic-adapter-http3-client adapter))))
    (%h3-owner-wait
     http3-client
     (lambda ()
       (when (and end (>= (%http3-quic-adapter-now) end))
         (%http3-quic-adapter-timeout
          "Timed out waiting for HTTP/3 request body flow control.")))
     (lambda ()
       (cl-quic-kit:client-poll quic-client)
       (when adapter
         (%http3-quic-adapter-process-peer-control-stream
          adapter :timeout timeout :deadline deadline))
       (sleep 0.005)
       t)
     :ready-step
     (lambda ()
       (cl-quic-kit:client-flush quic-client)
       (unless (cl-quic-kit:client-stream-write-pending-p quic-client stream)
         (values t quic-client)))))
  quic-client)

  (defun %http3-quic-adapter-process-peer-control-stream
    (adapter &key timeout deadline)
  (%http3-quic-adapter-call-with-owner
   adapter
   (lambda ()
     (let* ((http3-client (http3-quic-adapter-http3-client adapter))
            (control-stream
              (and http3-client
                   (http3-client-peer-control-stream http3-client)))
            (control-state
              (and http3-client
                   (http3-client-peer-control-state http3-client))))
       (when (and control-stream
                  control-state
                  (http3-control-state-settings-received-p control-state)
                  (or (plusp (cl-quic-kit:stream-readable-bytes control-stream))
                      (cl-quic-kit:stream-finished-p control-stream)))
         (setf (http3-quic-adapter-processing-peer-critical-streams-p adapter) t)
         (unwind-protect
              (progn
                (loop for readable =
                        (cl-quic-kit:stream-readable-bytes control-stream)
                      while (plusp readable)
                      do (read-http3-control-stream
                          http3-client :timeout timeout :deadline deadline))
                (when (cl-quic-kit:stream-finished-p control-stream)
                  (read-http3-control-stream
                   http3-client :timeout timeout :deadline deadline)))
           (setf (http3-quic-adapter-processing-peer-critical-streams-p adapter)
                 nil)))
       adapter)))))
(defun %http3-quic-adapter-stream-write-pending-p (quic-client stream)
  (or (cl-quic-kit:client-stream-write-pending-p quic-client stream)
      ;; A peer may stop request writes while its complete response remains readable.
      (cl-quic-kit:stream-stopped-p stream)))

(defun %http3-quic-adapter-read
    (quic-client http3-client stream &key timeout deadline poll-interval adapter)
  (let* ((end (%http3-quic-adapter-deadline timeout deadline))
         (check-deadline
           (lambda ()
             (when (and end (>= (%http3-quic-adapter-now) end))
               (%http3-quic-adapter-timeout
                "Timed out waiting for HTTP/3 QUIC stream data.")))))
    (%h3-owner-wait
     http3-client
     check-deadline
     (lambda ()
       (unless (and adapter
                    (http3-quic-adapter-processing-peer-critical-streams-p
                     adapter))
         (cl-quic-kit:client-poll quic-client)
         (funcall check-deadline)
         (when adapter
           (%http3-quic-adapter-process-peer-control-stream
            adapter :timeout timeout :deadline deadline))
         (funcall check-deadline))
       (multiple-value-bind (chunk fin-p)
           (cl-quic-kit:client-read-stream
            quic-client stream :timeout timeout :deadline deadline)
         (if (or (plusp (length chunk)) fin-p)
             (values nil chunk fin-p)
             (progn
               (when (plusp poll-interval)
                 (sleep poll-interval))
               t)))))))

(defvar *http3-quic-adapter-handshake-sleep-function* #'sleep)

(defvar *http3-quic-adapter-peer-control-sleep-function* #'sleep)

(defun %http3-quic-adapter-await-established
    (quic-client &key timeout deadline)
  (let ((end (%http3-quic-adapter-deadline timeout deadline)))
    (let ((iteration-count 0)
          (state nil))
      (%h3-owner-wait
       nil
       (lambda ()
         (when (and end (>= (%http3-quic-adapter-now) end))
           (%http3-quic-adapter-timeout
            "Timed out waiting for the QUIC TLS handshake.")))
       (lambda ()
         (setf state
               (cl-quic-kit:connection-state
                (cl-quic-kit:quic-client-connection quic-client)))
         (cond
           ((>= iteration-count 3000)
            (%http3-quic-adapter-error
             "The QUIC TLS handshake did not complete."
             state))
           ((cl-quic-kit:quic-client-closed-p quic-client)
            (let ((detail (%http3-quic-adapter-close-detail quic-client)))
              (%http3-quic-adapter-error
               (format nil
                       "The QUIC client closed before the TLS handshake completed: ~S."
                       detail)
               detail)))
           (t
            (incf iteration-count)
            (handler-case
                (cl-quic-kit:client-poll quic-client)
              (cl-tls-kit:tls13-client-driver-error (condition)
                (let ((reason
                        (cl-tls-kit:tls13-client-driver-error-reason condition)))
                  (%http3-quic-adapter-error
                   (format nil
                           "The TLS 1.3 client driver rejected the QUIC handshake: ~S."
                           reason)
                   reason))))
            (funcall *http3-quic-adapter-handshake-sleep-function* 0.005)
            t)))
       :ready-step
       (lambda ()
         (when (eq (cl-quic-kit:connection-state
                    (cl-quic-kit:quic-client-connection quic-client))
                   :established)
           (values t quic-client)))))))

(defun %http3-quic-adapter-await-peer-control-data
    (quic-client &key timeout deadline adapter)
  "Flush local control data and wait for readable peer HTTP/3 streams."
  (let ((end (%http3-quic-adapter-deadline timeout deadline))
        (pending-streams
          (and adapter
               (http3-quic-adapter-pending-peer-unidirectional-streams
                adapter)))
        (iteration-count 0)
        (state nil))
    (cl-quic-kit:client-flush quic-client)
    (%h3-owner-wait
     nil
     (lambda ()
       (when (and end (>= (%http3-quic-adapter-now) end))
         (%http3-quic-adapter-timeout
          "Timed out waiting for peer HTTP/3 control data.")))
     (lambda ()
       (when (cl-quic-kit:quic-client-closed-p quic-client)
         (%http3-quic-adapter-error
          "The QUIC client closed before peer HTTP/3 control data arrived."
          (%http3-quic-adapter-close-detail quic-client)))
       (incf iteration-count)
       (cl-quic-kit:client-poll quic-client)
       (funcall *http3-quic-adapter-peer-control-sleep-function* 0.005)
       t)
     :ready-step
     (lambda ()
       (setf state
             (cl-quic-kit:connection-state
              (cl-quic-kit:quic-client-connection quic-client)))
       (when (>= iteration-count 3000)
         (%http3-quic-adapter-error
          "The peer HTTP/3 control stream did not become readable."
          state))
       (loop for stream =
               (cl-quic-kit:client-accept-peer-stream
                quic-client :type :unidirectional)
             while stream
             do (unless (member stream pending-streams :test #'eq)
                  (when (>= (length pending-streams)
                            (if adapter
                                (http3-quic-adapter-max-peer-unidirectional-streams
                                 adapter)
                                +http3-default-max-peer-unidirectional-streams+))
                    (%http3-quic-adapter-error
                     "The HTTP/3 peer unidirectional-stream limit was exceeded."
                     :h3-excessive-load))
                  (setf pending-streams
                        (append pending-streams (list stream)))))
       (when adapter
         (setf (http3-quic-adapter-pending-peer-unidirectional-streams
                adapter)
               pending-streams))
       (when (some (lambda (stream)
                     (or (plusp (cl-quic-kit:stream-readable-bytes stream))
                         (cl-quic-kit:stream-finished-p stream)))
                   pending-streams)
         (values t quic-client))))))

(defun %http3-quic-adapter-await-peer-settings
    (quic-client http3-client &key timeout deadline
                                  (poll #'cl-quic-kit:client-poll) adapter)
  "Flush local HTTP/3 streams and consume the peer control stream SETTINGS."
  (let ((end (%http3-quic-adapter-deadline timeout deadline))
        (pending-streams
          (and adapter
               (http3-quic-adapter-pending-peer-unidirectional-streams
                adapter)))
        (prefix-buffers (make-hash-table :test #'eq)))
    (labels ((accept-peer-streams ()
               (loop for stream =
                       (cl-quic-kit:client-accept-peer-stream
                        quic-client :type :unidirectional)
                     while stream
                     do (unless (member stream pending-streams
                             :test (function eq))
                          (progn
                            (when (>= (+ (http3-client-peer-unidirectional-stream-count http3-client)
                                         (length pending-streams))
                                      (http3-client-max-peer-unidirectional-streams http3-client))
                              (%http3-quic-adapter-error
                               "The HTTP/3 peer unidirectional-stream limit was exceeded."
                               :h3-excessive-load))
                            (setf pending-streams
                                  (append pending-streams (list stream))))))
               (when adapter
                 (setf (http3-quic-adapter-pending-peer-unidirectional-streams
                        adapter)
                       pending-streams)))
             (preserve-pending-prefixes ()
               (%h3-check-state-bytes
                http3-client
                (loop for input being the hash-values of prefix-buffers
                      sum (length (or (first input) #()))))
               (maphash
                (lambda (stream input)
                  (%h3-prefetch-stream-input
                   http3-client stream (first input) (second input)))
                prefix-buffers)
               (%h3-check-state-bytes http3-client))
             (classify-peer-stream (stream input fin-p)
               (let* ((original-reader (http3-client-read-stream http3-client))
                      (replayed-p nil)
                      (kind
                        (unwind-protect
                             (progn
                               (setf (http3-client-read-stream http3-client)
                                     (lambda (read-stream &key timeout deadline)
                                       (if (and (eq read-stream stream)
                                                (not replayed-p))
                                           (progn
                                             (setf replayed-p t)
                                             (values input fin-p))
                                           (funcall original-reader read-stream
                                                    :timeout timeout
                                                    :deadline deadline))))
                               (accept-http3-peer-unidirectional-stream
                                http3-client stream :deadline end))
                          (setf (http3-client-read-stream http3-client)
                                original-reader))))
                 (when (eq kind :push)
                   (%h3-with-scheduler-lock (http3-client)
                     (let ((push-entry
                             (find stream
                                  (http3-client-seen-peer-push-ids http3-client)
                                  :key (function cdr) :test (function eq))))
                       (unless push-entry
                         (%h3-transport-error
                          "An HTTP/3 push stream was not registered during initialization."
                          stream))
                       (unless (member stream
                                       (http3-client-pending-push-streams
                                        http3-client)
                                       :key #'cdr :test #'eq)
                         (setf (http3-client-pending-push-streams http3-client)
                               (append
                                (http3-client-pending-push-streams http3-client)
                                (list (cons (car push-entry) stream))))))))
                 kind)))
      (cl-quic-kit:client-flush quic-client)
      (let ((iteration-count 0)
            (state nil))
        (%h3-owner-wait
         http3-client
         (lambda ()
           (when (and end (>= (%http3-quic-adapter-now) end))
             (%http3-quic-adapter-timeout
              "Timed out waiting for peer HTTP/3 SETTINGS.")))
         (lambda ()
           (setf state
                 (cl-quic-kit:connection-state
                  (cl-quic-kit:quic-client-connection quic-client)))
           (cond
             ((cl-quic-kit:quic-client-closed-p quic-client)
              (%http3-quic-adapter-error
               "The QUIC client closed before peer HTTP/3 SETTINGS arrived."
               (%http3-quic-adapter-close-detail quic-client)))
             (t
              (incf iteration-count)
              (funcall poll quic-client)
              (accept-peer-streams)
              (unless (http3-client-peer-control-stream http3-client)
                (dolist (stream pending-streams)
                  (when (or (plusp (cl-quic-kit:stream-readable-bytes stream))
                            (cl-quic-kit:stream-finished-p stream))
                    (multiple-value-bind (chunk fin-p)
                        (cl-quic-kit:client-read-stream quic-client stream)
                      (let* ((previous (gethash stream prefix-buffers))
                             (empty (make-array 0 :element-type '(unsigned-byte 8)))
                             (buffer
                               (progn
                                 (%h3-check-state-bytes
                                  http3-client
                                  (+ (loop for input being the hash-values of prefix-buffers
                                           sum (length (or (first input) #())))
                                     (length (or chunk empty))))
                                 (%http3-concatenate-octets
                                  (if previous (first previous) empty)
                                  (or chunk empty)))))
                        (multiple-value-bind (stream-type position)
                            (http3-varint-decode buffer :allow-incomplete-p t)
                          (declare (ignore position))
                          (if (or stream-type fin-p)
                              (progn
                                (remhash stream prefix-buffers)
                                (classify-peer-stream stream buffer fin-p)
                                (setf pending-streams
                                      (remove stream pending-streams :test #'eq)))
                              (setf (gethash stream prefix-buffers)
                                    (list buffer fin-p)))))))))
              (when adapter
                (setf (http3-quic-adapter-pending-peer-unidirectional-streams
                       adapter)
                      pending-streams))
              (let ((control (http3-client-peer-control-stream http3-client)))
                (when (and control
                           (or (assoc control
                                      (http3-client-prefetched-stream-inputs
                                       http3-client))
                               (plusp (cl-quic-kit:stream-readable-bytes control))
                               (cl-quic-kit:stream-finished-p control)))
                  (read-http3-control-stream http3-client :deadline end)))
              (funcall *http3-quic-adapter-peer-control-sleep-function* 0.005)
              t)))
         :ready-step
         (lambda ()
           (setf state
                 (cl-quic-kit:connection-state
                  (cl-quic-kit:quic-client-connection quic-client)))
           (cond
             ((>= iteration-count 3000)
              (%http3-quic-adapter-error
               "The peer HTTP/3 SETTINGS did not arrive."
               state))
             ((http3-control-state-settings-received-p
               (http3-client-peer-control-state http3-client))
              (preserve-pending-prefixes)
              (values t http3-client)))))))))

(defun %http3-quic-adapter-initialize (adapter initialize)
  (let ((initialized-p nil)
        (initialization-condition nil))
    (unwind-protect
         (handler-bind
             ((error (lambda (condition)
                       (setf initialization-condition condition))))
           (multiple-value-prog1 (%http3-quic-adapter-call-with-owner adapter initialize)
             (setf initialized-p t)))
      (unless initialized-p
        (handler-case
            (if (typep initialization-condition
                       'http3-connection-protocol-error)
                (close-http3-quic-adapter
                 adapter
                 :error-code
                 (http3-connection-protocol-error-quic-error-code
                  initialization-condition)
                 :reason
                 (http-kit:http-error-message initialization-condition))
                (close-http3-quic-adapter adapter))
          (error (close-condition)
            (warn "Closing the HTTP/3 QUIC adapter after initialization failed also failed: ~A"
                  close-condition)))))))

(define-condition %http3-connect-timeout
    (http-kit:http-timeout http-kit:http-connection-error)
  ())

(defun %http3-quic-adapter-connect (connect)
  (handler-case (funcall connect)
    (http-kit:http-timeout (condition)
      (error '%http3-connect-timeout
             :message (http-kit:http-error-message condition)
             :operation (http-kit:http-error-operation condition)
             :kind (http-kit:http-timeout-kind condition)
             :cause condition
             :request-sent-p nil))
    (error (condition)
      (error 'http-kit:http-connection-error
             :message "The HTTP/3 QUIC connection could not be established."
             :operation :http3-quic-adapter
             :cause condition
             :request-sent-p nil))))

(defun make-http3-quic-adapter
    (&key connection udp-socket tls-boundary tls-driver
          local-connection-id destination-connection-id
          server-host server-port hostname (alpn '("h3"))
          transport-parameters tls-key-exchange tls-provider
          tls-trust-anchors tls-verify-signature now-fn
          tls-signature-algorithms idle-timeout io-write on-close
          (poll-interval 0)
          (http3-options nil) timeout deadline)
  "Create an HTTP/3 client using cl-quic-kit as its QUIC stream boundary.

TLS trust anchors and all other QUIC/TLS options remain caller-owned and are
passed to CL-QUIC-KIT:MAKE-QUIC-CLIENT. HTTP3-OPTIONS is a property list of
keyword arguments appended to MAKE-HTTP3-CLIENT, allowing HTTP/3 limits and
settings to be selected without exposing QUIC implementation details.

POLL-INTERVAL defaults to zero and must be a real value from zero through
1/10 seconds, inclusive. Values outside that range are rejected. The 100 ms
cap limits the no-data sleep before the next close check. See the HTTP/3
callback completion contract in the API reference; it applies to callbacks
supplied through this constructor and HTTP3-OPTIONS."
  (unless (and (realp poll-interval)
               (not (minusp poll-interval))
               (<= poll-interval 1/10))
    (%http3-quic-adapter-error
     "HTTP/3 QUIC adapter poll-interval must be a real from 0 through 1/10 seconds."
     poll-interval))
  (unless (and (stringp hostname) (plusp (length hostname)))
    (%http3-quic-adapter-error
     "HTTP/3 QUIC adapter hostname must be a non-empty string."
     hostname))
  (unless (%http3-quic-adapter-property-list-p http3-options)
    (%http3-quic-adapter-error
     "HTTP/3 QUIC adapter http3-options must be a property list."
     http3-options))
  (%http3-quic-adapter-configure-protection)
  (let* ((stream-prefixes (make-hash-table :test #'eq))
         (stream-prefix-sent (make-hash-table :test #'eq))
         (adapter
          (%make-http3-quic-adapter
           :stream-prefixes stream-prefixes
           :stream-prefix-sent stream-prefix-sent
           :max-peer-unidirectional-streams
           (getf http3-options :max-peer-unidirectional-streams
                 +http3-default-max-peer-unidirectional-streams+)
           :poll-interval poll-interval))
         (quic-client
          (%http3-quic-adapter-call-with-owner
           adapter
           (lambda ()
             (%http3-quic-adapter-connect
              (lambda ()
                (cl-quic-kit:make-quic-client
                 :connection connection
                 :udp-socket udp-socket
                 :tls-boundary tls-boundary
                 :tls-driver tls-driver
                 :local-connection-id local-connection-id
                 :destination-connection-id destination-connection-id
                 :server-host server-host
                 :server-port server-port
                 :hostname hostname
                 :alpn alpn
                 :transport-parameters transport-parameters
                 :tls-key-exchange tls-key-exchange
                 :tls-provider tls-provider
                 :tls-trust-anchors tls-trust-anchors
                 :tls-verify-signature tls-verify-signature
                 :now-fn (or now-fn #'%http3-quic-adapter-now)
                 :tls-signature-algorithms tls-signature-algorithms
                 :idle-timeout idle-timeout
                 :io-write io-write
                 :on-close on-close)))))))
    (setf (http3-quic-adapter-quic-client adapter) quic-client)
    (%http3-quic-adapter-initialize
     adapter
     (lambda ()
       (%http3-quic-adapter-connect
        (lambda ()
          (cl-quic-kit:client-start quic-client)
          (setf (http3-quic-adapter-started-p adapter) t)
          (%http3-quic-adapter-await-established
           quic-client :timeout timeout :deadline deadline)))
       (setf (http3-quic-adapter-http3-client adapter)
             (apply
              #'make-http3-client
              :open-stream
              (lambda (request &key stream-type timeout deadline)
                (let ((stream
                       (cl-quic-kit:client-open-stream
                        quic-client request
                        :stream-type stream-type
                        :timeout timeout
                        :deadline deadline)))
                  (setf (gethash stream stream-prefixes)
                        (case stream-type
                          (:control (http3-control-stream-prefix))
                          (:qpack-encoder
                           (http3-qpack-encoder-stream-prefix))
                          (:qpack-decoder
                           (http3-qpack-decoder-stream-prefix))))
                  (values stream (cl-quic-kit:stream-id stream))))
              :write-stream
              (lambda (stream octets &key fin-p timeout deadline)
                (let* ((prefix (gethash stream stream-prefixes))
                       (payload
                        (if (and prefix
                                 (not (gethash stream stream-prefix-sent))
                                 (>= (length octets) (length prefix))
                                 (equalp prefix
                                         (subseq octets 0 (length prefix))))
                            (progn
                              (setf (gethash stream stream-prefix-sent) t)
                              (subseq octets (length prefix)))
                          octets)))
                  (prog1
                      (cl-quic-kit:client-write-stream
                       quic-client stream payload
                       :fin-p fin-p :timeout timeout :deadline deadline)
                    (%http3-quic-adapter-await-stream-write
                     quic-client stream
                     :timeout timeout :deadline deadline
                     :adapter adapter))))
              :read-stream
              (lambda (stream &key timeout deadline)
                (%http3-quic-adapter-read
                 quic-client (http3-quic-adapter-http3-client adapter)
                 stream
                 :timeout timeout :deadline deadline
                 :poll-interval poll-interval :adapter adapter))
              :close-stream
              (lambda (stream &key condition)
                (unwind-protect
                    (cl-quic-kit:client-close-stream
                     quic-client stream :condition condition)
                  (remhash stream stream-prefixes)
                  (remhash stream stream-prefix-sent)))
              :on-control-stream-ready
              (lambda (&key timeout deadline)
                (%http3-quic-adapter-await-peer-control-data
                 quic-client :timeout timeout :deadline deadline
                 :adapter adapter))
              :poll-function
              (lambda (&optional at)
                (cl-quic-kit:client-poll quic-client at))
              :try-read-stream
              (lambda (stream &key timeout deadline)
                (declare (ignore timeout deadline))
                (cond
                 ((plusp (cl-quic-kit:stream-readable-bytes stream))
                  (multiple-value-bind (chunk fin-p)
                      (cl-quic-kit:client-read-stream quic-client stream)
                    (cond
                     ((and chunk (plusp (length chunk)))
                      (values chunk fin-p :data))
                     (fin-p
                      (values nil t :fin))
                     (t
                      (values nil nil :would-block)))))
                 ((cl-quic-kit:stream-finished-p stream)
                  (values nil t :fin))
                 (t
                  (values nil nil :would-block))))
              :timeout timeout
              :deadline deadline
              (append
               (list
                :stream-write-pending-p
                (lambda (stream)
                  (%http3-quic-adapter-stream-write-pending-p
                   quic-client stream))
                :try-write-stream
                (lambda (stream octets &key fin-p timeout deadline)
                  (cl-quic-kit:client-write-stream
                   quic-client stream octets
                   :fin-p fin-p :timeout timeout :deadline deadline)
                  (cl-quic-kit:client-flush quic-client))
                :accept-peer-unidirectional-stream
                (lambda ()
                  (let ((pending
                         (http3-quic-adapter-pending-peer-unidirectional-streams
                          adapter)))
                    (if pending
                        (prog1 (car pending)
                          (setf
                           (http3-quic-adapter-pending-peer-unidirectional-streams
                            adapter)
                           (cdr pending)))
                      (cl-quic-kit:client-accept-peer-stream
                       quic-client :type :unidirectional)))))
               http3-options
               (list
                :close-connection
                (lambda (&key error-code reason)
                  (prog1
                      (cl-quic-kit:client-close
                       quic-client
                       :error-code error-code :reason reason)
                    (setf (http3-quic-adapter-closed-p adapter) t)))))))
       (%http3-quic-adapter-guard-client-callbacks
        adapter (http3-quic-adapter-http3-client adapter))
       (%http3-quic-adapter-call-with-owner
        adapter
        (lambda ()
          (%http3-quic-adapter-await-peer-settings
           quic-client (http3-quic-adapter-http3-client adapter)
           :timeout timeout :deadline deadline :adapter adapter)))
       adapter))))

(defun http3-quic-adapter-start (adapter)
  "Start ADAPTER if it has not already been started. Any callback reached through
the QUIC client follows the callback completion contract in the HTTP/3 API
reference."
  (unless (http3-quic-adapter-p adapter)
    (%http3-quic-adapter-error "START requires an HTTP/3 QUIC adapter."
                               (type-of adapter)))
  (when (http3-quic-adapter-closed-p adapter)
    (%http3-quic-adapter-error "The HTTP/3 QUIC adapter is closed."))
  (unless (http3-quic-adapter-started-p adapter)
    (%http3-quic-adapter-call-with-owner
     adapter
     (lambda ()
       (when (http3-quic-adapter-closed-p adapter)
         (%http3-quic-adapter-error "The HTTP/3 QUIC adapter is closed."))
       (unless (http3-quic-adapter-started-p adapter)
         (cl-quic-kit:client-start (http3-quic-adapter-quic-client adapter))
         (setf (http3-quic-adapter-started-p adapter) t)))))
  adapter)

(defun http3-quic-adapter-poll (adapter &optional at)
  "Drive the QUIC client used by ADAPTER once and return the adapter. Any
callback reached while polling follows the callback completion contract in the
HTTP/3 API reference."
  (unless (http3-quic-adapter-p adapter)
    (%http3-quic-adapter-error "POLL requires an HTTP/3 QUIC adapter."
                               (type-of adapter)))
  (unless (http3-quic-adapter-closed-p adapter)
    (http3-quic-adapter-start adapter)
    (handler-case
        (let ((client (http3-quic-adapter-http3-client adapter)))
          (if (and client (%h3-native-scheduler-p client))
              (%h3-scheduler-poll-once client at)
              (%http3-quic-adapter-call-with-owner
               adapter
               (lambda ()
                 (cl-quic-kit:client-poll
                  (http3-quic-adapter-quic-client adapter) at)
                 (%http3-quic-adapter-process-peer-control-stream adapter)))))
      (http3-connection-protocol-error (condition)
        (http-kit::%with-http-cleanup
         (close-http3-client
          (http3-quic-adapter-http3-client adapter)
          :condition condition))
        (http-kit::%with-http-cleanup
         (close-http3-quic-adapter
          adapter
          :error-code
          (http3-connection-protocol-error-quic-error-code condition)
          :reason (http-kit:http-error-message condition)))
        (error condition))))
  adapter)

(defun close-http3-quic-adapter (adapter &key (error-code :no-error) reason)
  "Close ADAPTER HTTP/3 streams and its underlying QUIC client. Its cleanup
callbacks follow the callback completion contract in the HTTP/3 API reference."
  (unless (http3-quic-adapter-p adapter)
    (%http3-quic-adapter-error "CLOSE requires an HTTP/3 QUIC adapter."
                               (type-of adapter)))
  (when (http3-quic-adapter-closed-p adapter)
    (return-from close-http3-quic-adapter t))
  (let* ((http3-client (http3-quic-adapter-http3-client adapter))
         (close-status
           (if http3-client
               (%h3-scheduler-begin-connection-close http3-client)
               :proceed)))
    (cond
      ((eq close-status :closed)
       (setf (http3-quic-adapter-closed-p adapter) t))
      ((null close-status)
       (return-from close-http3-quic-adapter nil))
      (t
       (let ((cleanup-failure nil)
             (quic-failure nil)
             (quic-close-attempted-p nil)
             (quic-close-succeeded-p nil))
         (unwind-protect
              (progn
                (when http3-client
                  (handler-case
                      (%call-http3-serialized
                       http3-client
                       (lambda ()
                         (%close-http3-client-serialized http3-client nil)))
                    (error (condition)
                      (setf cleanup-failure condition))))
                (setf quic-close-attempted-p t)
                (handler-case
                    (%http3-quic-adapter-call-with-owner
                     adapter
                     (lambda ()
                       (cl-quic-kit:client-close
                        (http3-quic-adapter-quic-client adapter)
                        :error-code error-code :reason reason)
                       (setf quic-close-succeeded-p t
                             (http3-quic-adapter-closed-p adapter) t)))
                  (error (condition)
                    (setf quic-failure condition)))
                (cond
                  ((and cleanup-failure quic-failure)
                   (warn "The QUIC close also failed after HTTP/3 cleanup failed (~A): ~A"
                         cleanup-failure quic-failure)
                   (error 'http-kit:http-connection-error
                          :message
                          (format nil
                                  "HTTP/3 cleanup failed before QUIC close failed: ~A"
                                  cleanup-failure)
                          :operation :http3-close
                          :cause
                          (list :http3-adapter-close-failures
                                :http3-cleanup cleanup-failure
                                :quic-close quic-failure)))
                  (cleanup-failure
                   (error cleanup-failure))
                  (quic-failure
                   (error quic-failure)))
                t)
           (unwind-protect
                (unwind-protect
                     (unless quic-close-attempted-p
                       (setf quic-close-attempted-p t)
                       (handler-case
                           (%http3-quic-adapter-call-with-owner
                            adapter
                            (lambda ()
                              (cl-quic-kit:client-close
                               (http3-quic-adapter-quic-client adapter)
                               :error-code error-code :reason reason)
                              (setf quic-close-succeeded-p t
                                    (http3-quic-adapter-closed-p adapter) t)))
                         (error (condition)
                           (setf quic-failure condition)
                           (warn "Closing the HTTP/3 QUIC adapter during nonlocal cleanup failed: ~A"
                                 condition))))
                  (let ((stream-prefixes
                          (http3-quic-adapter-stream-prefixes adapter))
                        (stream-prefix-sent
                          (http3-quic-adapter-stream-prefix-sent adapter)))
                    (when stream-prefixes (clrhash stream-prefixes))
                    (when stream-prefix-sent (clrhash stream-prefix-sent))))
             (when http3-client
               (%h3-scheduler-finish-connection-close
                http3-client
                (or cleanup-failure
                    quic-failure
                    (unless quic-close-succeeded-p
                      (make-condition
                       'http-kit:http-connection-error
                       :message "Closing the HTTP/3 QUIC connection did not complete."
                       :operation :http3-close)))))))))))
  t)
