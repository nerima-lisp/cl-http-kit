(in-package #:http-kit/http3)

(defstruct (http3-quic-adapter
            (:constructor %make-http3-quic-adapter))
  "A synchronous HTTP/3 client backed by cl-quic-kit."
  quic-client
  http3-client
  (started-p nil :type boolean)
  (closed-p nil :type boolean)
  (poll-interval 0 :type real))

(defun %http3-quic-adapter-error (message &optional detail)
  (error 'http-protocol-error
         :message message
         :operation :http3-quic-adapter
         :detail detail))

(defun %http3-quic-adapter-timeout (message)
  (error 'http-kit:http-timeout
         :message message
         :operation :http3-quic-adapter
         :kind :deadline))

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
  (let ((connection (cl-quic-kit:quic-client-connection quic-client)))
    (list :state (cl-quic-kit:connection-state connection)
          :client-closed-p (cl-quic-kit::quic-client-closed-p quic-client)
          :closed-error (cl-quic-kit::quic-connection-closed-error connection)
          :closed-reason (cl-quic-kit::quic-connection-closed-reason connection))))

(defun %http3-quic-adapter-await-stream-write
    (quic-client stream &key timeout deadline)
  (let ((end (%http3-quic-adapter-deadline timeout deadline)))
    (loop
      (cl-quic-kit:client-flush quic-client)
      (unless (cl-quic-kit:client-stream-write-pending-p quic-client stream)
        (return quic-client))
      (when (and end (>= (%http3-quic-adapter-now) end))
        (%http3-quic-adapter-timeout
         "Timed out waiting for HTTP/3 request body flow control."))
      (cl-quic-kit:client-poll quic-client)
      (sleep 0.005))))

(defun %http3-quic-adapter-read
    (quic-client http3-client stream &key timeout deadline poll-interval)
  (let ((end (%http3-quic-adapter-deadline timeout deadline)))
    (loop
      (when (and end (>= (%http3-quic-adapter-now) end))
        (%http3-quic-adapter-timeout
         "Timed out waiting for HTTP/3 QUIC stream data."))
      (cl-quic-kit:client-poll quic-client)
      (when (and end (>= (%http3-quic-adapter-now) end))
        (%http3-quic-adapter-timeout
         "Timed out waiting for HTTP/3 QUIC stream data."))
      (multiple-value-bind (chunk fin-p)
          (cl-quic-kit:client-read-stream quic-client stream
                                           :timeout timeout
                                           :deadline deadline)
        (when (or (plusp (length chunk)) fin-p)
          (return (values chunk fin-p))))
      (when (plusp poll-interval)
        (sleep poll-interval)))))

(defun %http3-quic-adapter-await-established
    (quic-client &key timeout deadline)
  (let ((end (%http3-quic-adapter-deadline timeout deadline)))
    (loop repeat 3000
        for state = (cl-quic-kit:connection-state
                     (cl-quic-kit:quic-client-connection quic-client))
        do (when (eq state :established)
             (return quic-client))
           (when (and end (>= (%http3-quic-adapter-now) end))
             (%http3-quic-adapter-timeout
              "Timed out waiting for the QUIC TLS handshake."))
           (when (cl-quic-kit::quic-client-closed-p quic-client)
             (let ((detail (%http3-quic-adapter-close-detail quic-client)))
               (%http3-quic-adapter-error
                (format nil
                        "The QUIC client closed before the TLS handshake completed: ~S."
                        detail)
                detail)))
           (handler-case
               (cl-quic-kit:client-poll quic-client)
             (cl-tls-kit:tls13-client-driver-error (condition)
               (let ((reason (cl-tls-kit:tls13-client-driver-error-reason
                              condition)))
                 (%http3-quic-adapter-error
                  (format nil
                          "The TLS 1.3 client driver rejected the QUIC handshake: ~S."
                          reason)
                  reason))))
           (sleep 0.005)
        finally
           (%http3-quic-adapter-error
            "The QUIC TLS handshake did not complete."
            (cl-quic-kit:connection-state
             (cl-quic-kit:quic-client-connection quic-client))))))

(defun %http3-quic-adapter-await-peer-control-data
    (quic-client &key timeout deadline)
  "Flush local control data and wait for readable peer unidirectional data."
  (let ((end (%http3-quic-adapter-deadline timeout deadline)))
    (cl-quic-kit:client-flush quic-client)
    (loop repeat 3000
        for state = (cl-quic-kit:connection-state
                     (cl-quic-kit:quic-client-connection quic-client))
        do (when (loop for id being the hash-keys of
                         (cl-quic-kit::quic-client-streams quic-client)
                       using (hash-value stream)
                       thereis (and (integerp id)
                                    (= (logand id 3) 3)
                                    (plusp (cl-quic-kit:stream-readable-bytes stream))))
             (return quic-client))
           (when (and end (>= (%http3-quic-adapter-now) end))
             (%http3-quic-adapter-timeout
              "Timed out waiting for peer HTTP/3 control data."))
           (when (cl-quic-kit::quic-client-closed-p quic-client)
             (%http3-quic-adapter-error
              "The QUIC client closed before peer HTTP/3 control data arrived."
              (%http3-quic-adapter-close-detail quic-client)))
           (cl-quic-kit:client-poll quic-client)
           (sleep 0.005)
        finally
           (%http3-quic-adapter-error
            "The peer HTTP/3 control stream did not become readable."
            state))))

(defun %http3-quic-adapter-await-peer-settings
    (quic-client http3-client &key timeout deadline
                                  (poll #'cl-quic-kit:client-poll))
  "Flush local HTTP/3 streams and consume the peer control stream SETTINGS."
  (let ((end (%http3-quic-adapter-deadline timeout deadline))
        (classified-streams (make-hash-table :test #'eq)))
    (cl-quic-kit:client-flush quic-client)
    (loop repeat 3000
        for state = (cl-quic-kit:connection-state
                     (cl-quic-kit:quic-client-connection quic-client))
        do (when (http3-control-state-settings-received-p
                  (http3-client-peer-control-state http3-client))
             (return http3-client))
           (when (and end (>= (%http3-quic-adapter-now) end))
             (%http3-quic-adapter-timeout
              "Timed out waiting for peer HTTP/3 SETTINGS."))
           (when (cl-quic-kit::quic-client-closed-p quic-client)
             (%http3-quic-adapter-error
              "The QUIC client closed before peer HTTP/3 SETTINGS arrived."
              (%http3-quic-adapter-close-detail quic-client)))
           (funcall poll quic-client)
           (unless (http3-client-peer-control-stream http3-client)
             (maphash
              (lambda (id stream)
                (when (and (integerp id)
                           (= (logand id 3) 3)
                           (not (gethash stream classified-streams))
                           (null (http3-client-peer-control-stream http3-client))
                           ;; An incomplete type prefix must not block another stream.
                           (or (cl-quic-kit:stream-finished-p stream)
                               (http3-varint-decode
                                (cl-quic-kit::stream-read-buffer stream)
                                :allow-incomplete-p t)))
                  (accept-http3-peer-unidirectional-stream
                   http3-client stream :deadline end)
                  (setf (gethash stream classified-streams) t)))
              (cl-quic-kit::quic-client-streams quic-client)))
           (let ((control (http3-client-peer-control-stream http3-client)))
             (when (and control
                        (or (assoc control
                                   (http3-client-prefetched-stream-inputs http3-client))
                            (plusp (cl-quic-kit:stream-readable-bytes control))
                            (cl-quic-kit:stream-finished-p control)))
               (read-http3-control-stream http3-client :deadline end)))
           (sleep 0.005)
        finally
           (%http3-quic-adapter-error
            "The peer HTTP/3 SETTINGS did not arrive."
            state))))

(defun %http3-quic-adapter-initialize (adapter initialize)
  (let ((initialized-p nil))
    (unwind-protect
         (multiple-value-prog1 (funcall initialize)
           (setf initialized-p t))
      (unless initialized-p
        (ignore-errors (close-http3-quic-adapter adapter)))))) ; paredit:ignore handler-case-swallows-error -- cleanup must preserve the original initialization failure.

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
     passed to CL-QUIC-KIT:MAKE-QUIC-CLIENT.  HTTP3-OPTIONS is a property list of keyword
arguments appended to MAKE-HTTP3-CLIENT, allowing HTTP/3 limits and settings
to be selected without exposing QUIC implementation details."
  (unless (and (realp poll-interval) (not (minusp poll-interval)))
    (%http3-quic-adapter-error
     "HTTP/3 QUIC adapter poll-interval must be a non-negative real."
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
  (let* ((quic-client
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
               :on-close on-close))))
         (stream-prefixes (make-hash-table :test #'eq))
         (stream-prefix-sent (make-hash-table :test #'eq))
         (adapter (%make-http3-quic-adapter
                   :quic-client quic-client
                   :poll-interval poll-interval)))
    (%http3-quic-adapter-initialize adapter
     (lambda ()
           (%http3-quic-adapter-connect
            (lambda ()
              (cl-quic-kit:client-start quic-client)
              (setf (http3-quic-adapter-started-p adapter) t)
              (%http3-quic-adapter-await-established
               quic-client :timeout timeout :deadline deadline)))
           (setf (http3-quic-adapter-http3-client adapter)
                 (apply #'make-http3-client
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
                                            (equalp prefix (subseq octets 0
                                                                    (length prefix))))
                                       (progn
                                         (setf (gethash stream stream-prefix-sent) t)
                                       (subseq octets (length prefix)))
                                       octets)))
                            (if (null prefix)
                                (progn
                                  (cl-quic-kit:client-write-stream
                                   quic-client stream payload
                                   :fin-p fin-p :timeout timeout :deadline deadline)
                                  (%http3-quic-adapter-await-stream-write
                                   quic-client stream
                                   :timeout timeout :deadline deadline))
                                (prog1
                                    (cl-quic-kit:client-write-stream
                                     quic-client stream
                                     payload
                                     :fin-p fin-p :timeout timeout :deadline deadline)
                                  (when fin-p
                                    (%http3-quic-adapter-await-stream-write
                                     quic-client stream
                                     :timeout timeout :deadline deadline))))))
                        :read-stream
                        (lambda (stream &key timeout deadline)
                          (%http3-quic-adapter-read
                           quic-client
                           (http3-quic-adapter-http3-client adapter)
                           stream
                           :timeout timeout :deadline deadline
                           :poll-interval poll-interval))
                        :close-stream
                        (lambda (stream &key condition)
                          (cl-quic-kit:client-close-stream
                           quic-client stream :condition condition))
                        :on-control-stream-ready
                        (lambda (&key timeout deadline)
                          (%http3-quic-adapter-await-peer-control-data
                           quic-client :timeout timeout :deadline deadline))
                        :timeout timeout
                        :deadline deadline
                        http3-options))
           (%http3-quic-adapter-await-peer-settings
            quic-client (http3-quic-adapter-http3-client adapter)
            :timeout timeout :deadline deadline)
           adapter))))

(defun http3-quic-adapter-start (adapter)
  "Start ADAPTER's QUIC client when it has not already been started."
  (unless (http3-quic-adapter-p adapter)
    (%http3-quic-adapter-error "START requires an HTTP/3 QUIC adapter."
                               (type-of adapter)))
  (when (http3-quic-adapter-closed-p adapter)
    (%http3-quic-adapter-error "The HTTP/3 QUIC adapter is closed."))
  (unless (http3-quic-adapter-started-p adapter)
    (cl-quic-kit:client-start (http3-quic-adapter-quic-client adapter))
    (setf (http3-quic-adapter-started-p adapter) t))
  adapter)

(defun http3-quic-adapter-poll (adapter &optional at)
  "Drive ADAPTER's QUIC client once and return the adapter."
  (unless (http3-quic-adapter-p adapter)
    (%http3-quic-adapter-error "POLL requires an HTTP/3 QUIC adapter."
                               (type-of adapter)))
  (unless (http3-quic-adapter-closed-p adapter)
    (http3-quic-adapter-start adapter)
    (cl-quic-kit:client-poll (http3-quic-adapter-quic-client adapter) at))
  adapter)

(defun close-http3-quic-adapter (adapter &key (error-code :no-error) reason)
  "Close ADAPTER's HTTP/3 streams and its underlying QUIC client."
  (unless (http3-quic-adapter-p adapter)
    (%http3-quic-adapter-error "CLOSE requires an HTTP/3 QUIC adapter."
                               (type-of adapter)))
  (unless (http3-quic-adapter-closed-p adapter)
    (when (http3-quic-adapter-http3-client adapter)
      (ignore-errors ; paredit:ignore handler-case-swallows-error -- stream cleanup failure must not prevent closing the underlying QUIC client.
        (close-http3-client (http3-quic-adapter-http3-client adapter))))
    (cl-quic-kit:client-close
     (http3-quic-adapter-quic-client adapter)
     :error-code error-code :reason reason)
    (setf (http3-quic-adapter-closed-p adapter) t))
  t)
