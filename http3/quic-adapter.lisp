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
  (/ (get-internal-real-time) (float internal-time-units-per-second)))

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

(defun %http3-quic-adapter-read
    (quic-client stream &key timeout deadline poll-interval)
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

(defun %http3-quic-adapter-await-established (quic-client)
  (loop repeat 3000
        for state = (cl-quic-kit:connection-state
                     (cl-quic-kit:quic-client-connection quic-client))
        do (when (eq state :established)
             (return quic-client))
           (when (cl-quic-kit::quic-client-closed-p quic-client)
             (%http3-quic-adapter-error
              "The QUIC client closed before the TLS handshake completed."
              state))
           (handler-case
               (cl-quic-kit:client-poll quic-client)
             (cl-tls-kit:tls13-client-driver-error (condition)
               (%http3-quic-adapter-error
                "The TLS 1.3 client driver rejected the QUIC handshake."
                (cl-tls-kit:tls13-client-driver-error-reason condition))))
           (sleep 0.005)
        finally
           (%http3-quic-adapter-error
            "The QUIC TLS handshake did not complete."
            (cl-quic-kit:connection-state
             (cl-quic-kit:quic-client-connection quic-client)))))

(defun make-http3-quic-adapter
    (&key connection udp-socket tls-boundary tls-driver
          local-connection-id destination-connection-id
          server-host server-port hostname (alpn '("h3"))
          transport-parameters tls-key-exchange tls-provider
          tls-trust-anchors tls-verify-signature now-fn
          tls-signature-algorithms idle-timeout io-write on-close
          (poll-interval 0)
          (http3-options nil))
  "Create an HTTP/3 client using cl-quic-kit as its QUIC stream boundary.

TLS trust anchors and all other QUIC/TLS options remain caller-owned and are
passed to CL-QUIC-KIT:MAKE-QUIC-CLIENT.  HTTP3-OPTIONS is an alist of keyword
arguments appended to MAKE-HTTP3-CLIENT, allowing HTTP/3 limits and settings
to be selected without exposing QUIC implementation details."
  (unless (and (realp poll-interval) (not (minusp poll-interval)))
    (%http3-quic-adapter-error
     "HTTP/3 QUIC adapter poll-interval must be a non-negative real."
     poll-interval))
  (unless (listp http3-options)
    (%http3-quic-adapter-error
     "HTTP/3 QUIC adapter http3-options must be a property list."
     http3-options))
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
           (cl-quic-kit:client-start quic-client)
           (setf (http3-quic-adapter-started-p adapter) t)
           (%http3-quic-adapter-await-established quic-client)
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
                            (values stream (cl-quic-kit:stream-id stream))))
                        :write-stream
                        (lambda (stream octets &key fin-p timeout deadline)
                          (cl-quic-kit:client-write-stream
                           quic-client stream octets
                           :fin-p fin-p :timeout timeout :deadline deadline))
                        :read-stream
                        (lambda (stream &key timeout deadline)
                          (%http3-quic-adapter-read
                           quic-client stream
                           :timeout timeout :deadline deadline
                           :poll-interval poll-interval))
                        :close-stream
                        (lambda (stream &key condition)
                          (cl-quic-kit:client-close-stream
                           quic-client stream :condition condition))
                        http3-options))
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
