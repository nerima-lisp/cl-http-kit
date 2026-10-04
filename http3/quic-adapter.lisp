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

(defun %http3-quic-adapter-now ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun %http3-quic-adapter-diagnostic (stage quic-client &optional stream)
  (when (string= "1" (sb-ext:posix-getenv "HTTP3_LOOPBACK_DIAGNOSTICS"))
    (let ((connection (cl-quic-kit:quic-client-connection quic-client)))
      (format *error-output*
              "HTTP3-DIAG ~A state=~S sent=~D received=~D udp=~S port=~S readable=~S~%"
              stage
              (cl-quic-kit:connection-state connection)
              (length (cl-quic-kit::quic-client-sent-packets quic-client))
              (length (cl-quic-kit::quic-client-received-packets quic-client))
              (not (null (cl-quic-kit::quic-client-udp-socket quic-client)))
              (cl-quic-kit::quic-client-server-port quic-client)
              (and stream (cl-quic-kit:stream-readable-bytes stream)))
      (finish-output *error-output*))))

(defun %http3-quic-adapter-configure-protection ()
  (let* ((protection (find-package "CL-QUIC-KIT.PROTECTION"))
         (crypto (find-package "CRYPTO-KIT"))
         (configure (and protection
                         (find-symbol "CONFIGURE-CRYPTO-BACKEND" protection))))
    (when (and configure crypto)
      (flet ((crypto-function (name)
               (symbol-function (find-symbol name crypto))))
        (funcall configure
                 :hkdf-extract (crypto-function "HKDF-EXTRACT")
                 :hkdf-expand (crypto-function "HKDF-EXPAND")
                 :aead-seal (crypto-function "AEAD-SEAL")
                 :aead-open (crypto-function "AEAD-OPEN")
                 :aes-ecb (crypto-function "AES-ENCRYPT-BLOCK")
                 :chacha20 (crypto-function "CHACHA20-KEYSTREAM")
                 :constant-time-equal (crypto-function "CONSTANT-TIME-EQUAL"))))))

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

(defun %http3-quic-adapter-read
    (quic-client http3-client stream &key timeout deadline poll-interval)
  (let ((end (%http3-quic-adapter-deadline timeout deadline)))
    (loop
      (cl-quic-kit:client-poll quic-client)
      (multiple-value-bind (chunk fin-p)
          (cl-quic-kit:client-read-stream quic-client stream
                                           :timeout timeout
                                           :deadline deadline)
        (when (or (plusp (length chunk)) fin-p)
          (return (values chunk fin-p))))
      (when (and end (>= (%http3-quic-adapter-now) end))
        (%http3-quic-adapter-error
         "Timed out waiting for HTTP/3 QUIC stream data."
         (list :timeout timeout :deadline deadline)))
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
             (%http3-quic-adapter-error
              "Timed out waiting for the QUIC TLS handshake."
              (list :timeout timeout :deadline deadline)))
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
  "Flush the local control stream and wait for readable peer control data."
  (let ((end (%http3-quic-adapter-deadline timeout deadline)))
    (cl-quic-kit:client-flush quic-client)
    (loop repeat 3000
        for state = (cl-quic-kit:connection-state
                     (cl-quic-kit:quic-client-connection quic-client))
        do (let ((peer-control
                   (gethash 3 (cl-quic-kit::quic-client-streams quic-client))))
             (when (and peer-control
                        (plusp (cl-quic-kit:stream-readable-bytes peer-control)))
               (return quic-client)))
           (when (and end (>= (%http3-quic-adapter-now) end))
             (%http3-quic-adapter-error
              "Timed out waiting for peer HTTP/3 control data."
              (list :timeout timeout :deadline deadline)))
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
    (quic-client http3-client &key timeout deadline)
  "Flush local HTTP/3 streams and consume the peer control stream SETTINGS."
  (let ((end (%http3-quic-adapter-deadline timeout deadline)))
    (cl-quic-kit:client-flush quic-client)
    (loop repeat 3000
        for state = (cl-quic-kit:connection-state
                     (cl-quic-kit:quic-client-connection quic-client))
        do (when (http3-control-state-settings-received-p
                  (http3-client-peer-control-state http3-client))
             (return http3-client))
           (when (and end (>= (%http3-quic-adapter-now) end))
             (%http3-quic-adapter-error
              "Timed out waiting for peer HTTP/3 SETTINGS."
              (list :timeout timeout :deadline deadline)))
           (when (cl-quic-kit::quic-client-closed-p quic-client)
             (%http3-quic-adapter-error
              "The QUIC client closed before peer HTTP/3 SETTINGS arrived."
              (%http3-quic-adapter-close-detail quic-client)))
           (cl-quic-kit:client-poll quic-client)
           (unless (http3-client-peer-control-stream http3-client)
             (maphash
              (lambda (id stream)
                (when (and (integerp id)
                           (= (logand id 3) 3)
                           (not (cl-quic-kit:stream-local-p stream))
                           (null (http3-client-peer-control-stream http3-client)))
                  (accept-http3-peer-unidirectional-stream
                   http3-client stream)))
              (cl-quic-kit::quic-client-streams quic-client)))
           (when (http3-client-peer-control-stream http3-client)
             (read-http3-control-stream http3-client))
           (sleep 0.005)
        finally
           (%http3-quic-adapter-error
            "The peer HTTP/3 SETTINGS did not arrive."
            state))))

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
passed to CL-QUIC-KIT:MAKE-QUIC-CLIENT.  HTTP3-OPTIONS is an alist of keyword
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
  (unless (listp http3-options)
    (%http3-quic-adapter-error
     "HTTP/3 QUIC adapter http3-options must be a property list."
     http3-options))
  (%http3-quic-adapter-configure-protection)
  (let* ((quic-client
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
            :on-close on-close))
         (adapter (%make-http3-quic-adapter
                   :quic-client quic-client
                   :poll-interval poll-interval)))
    (unwind-protect
         (progn
           (%http3-quic-adapter-diagnostic "client-start" quic-client)
           (cl-quic-kit:client-start quic-client)
           (setf (http3-quic-adapter-started-p adapter) t)
           (%http3-quic-adapter-await-established
            quic-client :timeout timeout :deadline deadline)
           (%http3-quic-adapter-diagnostic "handshake-complete" quic-client)
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
                            (%http3-quic-adapter-diagnostic
                             "open-stream-complete" quic-client stream)
                            (values stream (cl-quic-kit:stream-id stream))))
                        :write-stream
                        (lambda (stream octets &key fin-p timeout deadline)
                          (%http3-quic-adapter-diagnostic
                           (if fin-p "write-fin-start" "write-start")
                           quic-client stream)
                          (prog1
                              (cl-quic-kit:client-write-stream
                               quic-client stream octets
                               :fin-p fin-p :timeout timeout :deadline deadline)
                            (cl-quic-kit:client-flush quic-client))
                          (%http3-quic-adapter-diagnostic
                           (if fin-p "write-fin-complete" "write-complete")
                           quic-client stream))
                        :read-stream
                        (lambda (stream &key timeout deadline)
                          (%http3-quic-adapter-diagnostic
                           "read-start" quic-client stream)
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
           adapter)
      (unless (http3-quic-adapter-http3-client adapter)
        (ignore-errors (cl-quic-kit:client-close quic-client))))))

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
      (ignore-errors
        (close-http3-client (http3-quic-adapter-http3-client adapter))))
    (cl-quic-kit:client-close
     (http3-quic-adapter-quic-client adapter)
     :error-code error-code :reason reason)
    (setf (http3-quic-adapter-closed-p adapter) t))
  t)
