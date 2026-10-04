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

(defun %read-http1-headers (stream)
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil nil)
          do (unless byte (error "Receiver reached EOF in request headers."))
             (vector-push-extend byte bytes)
             (let ((length (length bytes)))
               (when (and (>= length 4)
                          (= (aref bytes (- length 4)) 13)
                          (= (aref bytes (- length 3)) 10)
                          (= (aref bytes (- length 2)) 13)
                          (= (aref bytes (- length 1)) 10))
                 (return (map 'string #'code-char bytes)))))))

(defun %start-post-receiver (port expected-length)
  #+sbcl
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-bind listener #(127 0 0 1) port)
    (sb-bsd-sockets:socket-listen listener 1)
    (values
     (sb-thread:make-thread
      (lambda ()
        (unwind-protect
             (let* ((socket (sb-bsd-sockets:socket-accept listener))
                    (stream (sb-bsd-sockets:socket-make-stream
                             socket :input t :output t :element-type '(unsigned-byte 8)
                             :buffering :full)))
               (unwind-protect
                    (let* ((headers (%read-http1-headers stream))
                           (marker (search "content-length:" headers
                                           :test #'char-equal))
                           (line-end (and marker (position #\Newline headers
                                                            :start marker)))
                           (length (and marker line-end
                                         (parse-integer headers
                                                        :start (+ marker 15)
                                                        :end line-end
                                                        :junk-allowed t)))
                           (body (make-array (or length 0)
                                             :element-type '(unsigned-byte 8))))
                      (unless (= (or length -1) expected-length)
                        (error "Receiver got Content-Length ~S, expected ~D."
                               length expected-length))
                      (unless (= (read-sequence body stream) expected-length)
                        (error "Receiver got a truncated POST body."))
                      (unless (every (lambda (octet) (= octet #x5a)) body)
                        (error "Receiver got unexpected POST body bytes."))
                      (write-sequence
                       (map '(vector (unsigned-byte 8)) #'char-code
                            (format nil "HTTP/1.1 200 OK~C~CContent-Length: 8~C~CConnection: close~C~C~C~Cverified"
                                    #\Return #\Linefeed
                                    #\Return #\Linefeed
                                    #\Return #\Linefeed
                                    #\Return #\Linefeed))
                       stream)
                      (finish-output stream)
                      (format t "HTTP/3 POST receiver diagnostic response-written~%")
                      (finish-output))
                 (close stream)
                 (sb-bsd-sockets:socket-close socket)))
          (sb-bsd-sockets:socket-close listener)))
      :name "cl-http-kit-post-receiver")
     listener)))

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
          :timeout timeout
          :deadline deadline
          :now-fn (lambda ()
                    (/ (get-internal-real-time)
                       internal-time-units-per-second))
          :idle-timeout 30)))
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
  (let* ((post-length (* 1024 1024))
         (post-receiver (and (string= mode "explicit")
                             (%start-post-receiver
                              (%env-integer "RECEIVER_PORT" 18080)
                              post-length)))
         (post-body (make-array post-length :element-type '(unsigned-byte 8)
                                :initial-element #x5a)))
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
           (error "HTTP/3 loopback mode ~A returned an unexpected response." mode)))))
    (when (string= mode "explicit")
      (let ((response
              (send (http-kit/client:http-client-request
                     client "POST" (format nil "https://localhost:~D/upload" port)
                     :protocol-version "HTTP/3" :body post-body))))
        (unless (and (= 200 (http-kit:http-response-status response))
                     (string= "verified"
                              (%octets-as-string
                               (http-kit:http-response-body response))))
          (format t "HTTP/3 POST diagnostic status=~S protocol=~S body=~S~%"
                  (http-kit:http-response-status response)
                  (http-kit:http-response-protocol-version response)
                  (%octets-as-string (http-kit:http-response-body response)))
          (error "HTTP/3 known-length POST was not verified by the receiver."))))
    (when post-receiver
      (sb-thread:join-thread post-receiver))
    (format t "Caddy HTTP/3 loopback passed: ~A~%" mode))))
