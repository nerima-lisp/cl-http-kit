(require :asdf)
(push (truename "./") asdf:*central-registry*)
(asdf:load-system "cl-http-kit/test")

(let ((protection (find-package "CL-QUIC-KIT.PROTECTION"))
      (crypto (find-package "CRYPTO-KIT")))
  (funcall (find-symbol "CONFIGURE-CRYPTO-BACKEND" protection)
           :hkdf-extract (symbol-function (find-symbol "HKDF-EXTRACT" crypto))
           :hkdf-expand (symbol-function (find-symbol "HKDF-EXPAND" crypto))
           :aead-seal (symbol-function (find-symbol "AEAD-SEAL" crypto))
           :aead-open (symbol-function (find-symbol "AEAD-OPEN" crypto))
           :aes-ecb (symbol-function (find-symbol "AES-ENCRYPT-BLOCK" crypto))
           :chacha20 (symbol-function (find-symbol "CHACHA20-KEYSTREAM" crypto))
           :constant-time-equal
           (symbol-function (find-symbol "CONSTANT-TIME-EQUAL" crypto))))

(defun %env (name &optional default)
  (or (sb-ext:posix-getenv name) default))

(defun %env-integer (name default)
  (parse-integer (%env name (princ-to-string default))))

(defun %octets-as-string (octets)
  (map 'string #'code-char octets))

(defun %read-caddy-root ()
  (let* ((blocks (cl-tls-kit:pem-decode
                  (uiop:read-file-string (%env "CADDY_ROOT"))))
         (certificate (find-if (lambda (block)
                                (string= "CERTIFICATE"
                                         (cl-tls-kit:pem-block-label block)))
                              blocks)))
    (unless certificate
      (error "CADDY_ROOT contains no certificate"))
    (cl-tls-kit.x509:parse-certificate-der
     (cl-tls-kit:pem-block-der certificate))))

(defun %http3-loopback-transport (port trust-anchor mode)
  (lambda (request &key alternative-service timeout deadline &allow-other-keys)
    (declare (ignore alternative-service))
    (let ((adapter
            (http-kit/http3:make-http3-quic-adapter
             :server-host "127.0.0.1"
             :server-port (if (string= mode "fallback") (1+ port) port)
             :hostname "localhost"
             :alpn '("h3")
             :tls-trust-anchors (list trust-anchor)
             :tls-verify-signature #'crypto-kit:verify-signature
             :tls-signature-algorithms #(1027)
             :idle-timeout 10)))
      (unwind-protect
           (http-kit/http3:send-http3-request
            (http-kit/http3:http3-quic-adapter-http3-client adapter)
            request :timeout timeout :deadline deadline)
        (ignore-errors
          (http-kit/http3:close-http3-quic-adapter adapter))))))

(defun %make-loopback-client (port trust-anchor mode)
  (http-kit/client:make-http-client
   :open-stream (http-kit/network:make-http-network-stream-opener)
   :close-stream #'http-kit/network:close-http-tcp-stream
   :tls-upgrade
   (http-kit/tls:make-http-tls-upgrader
    :verify :required
    :trust-anchors (list trust-anchor)
    :alpn-protocols '("http/1.1"))
   :strict-transport-store nil
   :http3-transport-function (%http3-loopback-transport port trust-anchor mode)))

(let* ((port (%env-integer "CADDY_PORT" 0))
       (mode (%env "HTTP3_LOOPBACK_MODE" "alt-svc"))
       (trust-anchor (%read-caddy-root))
       (client (%make-loopback-client port trust-anchor mode))
       (uri (format nil "https://localhost:~D/" port)))
  (labels ((send (request)
             (multiple-value-bind (response ignored)
                 (http-kit/client:http-client-send client request :timeout 10)
               (declare (ignore ignored))
               response))
           (request (&optional protocol-version)
             (http-kit/client:http-client-request
              client "GET" uri :protocol-version (or protocol-version "HTTP/1.1"))))
    (cond
      ((string= mode "explicit")
       (let ((response (send (request "HTTP/3"))))
         (unless (and (= 200 (http-kit:http-response-status response))
                      (string= "HTTP/3"
                               (http-kit:http-response-protocol-version response))
                      (string= "ok"
                               (%octets-as-string
                                (http-kit:http-response-body response))))
           (error "Explicit HTTP/3 loopback did not return an HTTP/3 ok response."))))
      (t
       (let ((tcp-response (send (request))))
         (unless (and (= 200 (http-kit:http-response-status tcp-response))
                      (string= "HTTP/1.1"
                               (http-kit:http-response-protocol-version tcp-response)))
           (error "Caddy TCP bootstrap request did not return HTTP/1.1.")))
       (let ((response (send (request))))
         (unless (and (= 200 (http-kit:http-response-status response))
                      (if (string= mode "fallback")
                          (string= "HTTP/1.1"
                                   (http-kit:http-response-protocol-version response))
                          (string= "HTTP/3"
                                   (http-kit:http-response-protocol-version response)))
                      (string= "ok"
                               (%octets-as-string
                                (http-kit:http-response-body response))))
           (error "HTTP/3 loopback mode ~A returned an unexpected response." mode))))))
  (format t "Caddy HTTP/3 loopback passed: ~A~%" mode))
