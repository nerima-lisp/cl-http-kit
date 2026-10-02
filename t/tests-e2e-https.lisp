(in-package #:http-kit/test)

#+sbcl
(progn
  (defun %https-e2e-read-headers (stream)
    (let ((bytes (make-array 0
                             :element-type '(unsigned-byte 8)
                             :adjustable t
                             :fill-pointer 0)))
      (loop for byte = (read-byte stream nil nil)
            do (unless byte
                 (error "The HTTPS E2E server reached EOF before headers."))
               (vector-push-extend byte bytes)
               (when (and (>= (length bytes) 4)
                          (= (aref bytes (- (length bytes) 4)) 13)
                          (= (aref bytes (- (length bytes) 3)) 10)
                          (= (aref bytes (- (length bytes) 2)) 13)
                          (= (aref bytes (- (length bytes) 1)) 10))
                 (return bytes)))))

  (defun %https-e2e-response (status headers body)
    (concatenate-octets
     (ascii (format nil "HTTP/1.1 ~D~A|CRLF|"
                    status
                    (case status
                      (200 " OK")
                      (302 " Found")
                      (403 " Forbidden")
                      (otherwise ""))))
     (apply #'concatenate-octets
            (mapcar (lambda (header)
                      (ascii (format nil "~A: ~A|CRLF|"
                                     (first header) (second header))))
                    headers))
     (ascii (format nil "Content-Length: ~D|CRLF|Connection: close|CRLF||CRLF|"
                    (length body)))
     body))

  (defun %https-e2e-start-server (openssl port certificate key body)
    (let* ((process
             (uiop:launch-program
              (list openssl "s_server" "-quiet"
                    "-accept" (princ-to-string port)
                    "-cert" certificate "-key" key "-tls1_3"
                    "-alpn" "http/1.1")
              :input :stream
              :output :stream
              :error-output :stream
              :element-type '(unsigned-byte 8)
              :wait nil))
           (input (uiop:process-info-input process))
           (output (uiop:process-info-output process))
           (server-error nil)
           (server-thread
             (sb-thread:make-thread
              (lambda ()
                (handler-case
                    (loop repeat 10
                          for request = (%https-e2e-read-headers output)
                          for request-text = (octets-as-string request)
                          for path = (second (uiop:split-string
                                              (first (uiop:split-string
                                                      request-text
                                                      :separator '(#\Return #\Linefeed)))
                                              :separator '(#\Space)))
                          for response =
                            (cond
                              ((string= path "/redirect")
                               (%https-e2e-response
                                302 '(("Location" "/ok")) (octets)))
                              ((string= path "/gzip")
                               (%https-e2e-response
                                200 '(("Content-Encoding" "gzip")) body))
                              ((string= path "/cookie")
                               (%https-e2e-response
                                (if (search "Cookie: sid=e2e" request-text)
                                    200 403)
                                nil
                                (ascii (if (search "Cookie: sid=e2e" request-text)
                                            "cookie-ok"
                                            "cookie-missing"))))
                              (t
                               (%https-e2e-response
                                200 '(("Set-Cookie" "sid=e2e; Path=/"))
                                (ascii "https-ok"))))
                          do (write-sequence response input)
                             (finish-output input))
                  (error (condition)
                    (setf server-error condition)))))))
      (sleep 1)
      (values process server-thread input output
              (lambda ()
                (when server-thread
                  (sb-thread:join-thread server-thread))
                (when server-error
                  (error server-error))))))

  (deftest e2e-https-openssl-client
    (let* ((directory (merge-pathnames "cl-http-kit-https-e2e/"
                                      (uiop:temporary-directory)))
           (certificate (namestring (merge-pathnames "server.crt" directory)))
           (key (namestring (merge-pathnames "server.key" directory)))
           (openssl (or (uiop:getenv "OPENSSL") "openssl"))
           (port (multiple-value-bind (listener selected-port)
                     (%network-test-listener)
                   (%network-test-close-socket listener)
                   selected-port))
           (payload (deflate-kit:gzip-compress (ascii "gzip-ok")))
           (process nil)
           (server-thread nil)
           (input nil)
           (output nil)
           (join-server nil))
      (ensure-directories-exist directory)
      (uiop:run-program
       (list openssl "req" "-x509" "-newkey" "rsa:2048" "-nodes"
             "-days" "1" "-subj" "/CN=127.0.0.1"
             "-addext" "subjectAltName=IP:127.0.0.1"
             "-keyout" key "-out" certificate)
       :output :string :error-output :string)
      (let ((trust-anchor
              (cl-tls-kit.x509:parse-certificate-der
               (cl-tls-kit:pem-block-der
                (first (cl-tls-kit:pem-decode
                        (uiop:read-file-string certificate)))))))
        (unwind-protect
             (progn
               (multiple-value-setq (process server-thread input output join-server)
                 (%https-e2e-start-server openssl port certificate key payload))
               (let ((client
                       (make-http-client
                        :open-stream (make-http-network-stream-opener)
                        :close-stream #'close-http-tcp-stream
                        :tls-upgrade
                        (make-http-tls-upgrader
                         :verify :required
                         :trust-anchors (list trust-anchor)
                         :alpn-protocols '("http/1.1"))
                        :strict-transport-store nil
                        :alternative-service-store nil)))
                 (multiple-value-bind (response ignored)
                     (http-client-send
                      client "GET"
                      (format nil "https://127.0.0.1:~D/ok" port)
                      :timeout 5)
                   (declare (ignore ignored))
                   (ensure-equal 200 (http-response-status response))
                   (ensure-equal "https-ok"
                                 (octets-as-string (http-response-body response))))
                 (multiple-value-bind (response ignored)
                     (http-client-send
                      client "GET"
                      (format nil "https://127.0.0.1:~D/redirect" port)
                      :timeout 5)
                   (declare (ignore ignored))
                   (ensure-equal 200 (http-response-status response))
                   (ensure-equal "https-ok"
                                 (octets-as-string (http-response-body response))))
                 (multiple-value-bind (response ignored)
                     (http-client-send
                      client "GET"
                      (format nil "https://127.0.0.1:~D/gzip" port)
                      :timeout 5)
                   (declare (ignore ignored))
                   (ensure-equal "gzip-ok"
                                 (octets-as-string (http-response-body response))))
                 (multiple-value-bind (response ignored)
                     (http-client-send
                      client "GET"
                      (format nil "https://127.0.0.1:~D/cookie" port)
                      :timeout 5)
                   (declare (ignore ignored))
                   (ensure-equal 200 (http-response-status response))
                   (ensure-equal "cookie-ok"
                                 (octets-as-string (http-response-body response))))
                 (let ((previous-certificate-file (uiop:getenv "SSL_CERT_FILE")))
                   (sb-posix:setenv "SSL_CERT_FILE" certificate 1)
                   (unwind-protect
                        (let ((default-client (make-http-client)))
                          (multiple-value-bind (response ignored)
                              (http-client-send
                               default-client "GET"
                               (format nil "https://127.0.0.1:~D/ok" port))
                            (declare (ignore ignored))
                            (ensure-equal 200 (http-response-status response))
                            (ensure-equal "https-ok"
                                          (octets-as-string
                                           (http-response-body response))))
                          (multiple-value-bind (response ignored)
                              (http-client-send
                               default-client "GET"
                               (format nil "https://127.0.0.1:~D/redirect" port))
                            (declare (ignore ignored))
                            (ensure-equal 200 (http-response-status response))
                            (ensure-equal "https-ok"
                                          (octets-as-string
                                           (http-response-body response))))
                          (multiple-value-bind (response ignored)
                              (http-client-send
                               default-client "GET"
                               (format nil "https://127.0.0.1:~D/gzip" port))
                            (declare (ignore ignored))
                            (ensure-equal "gzip-ok"
                                          (octets-as-string
                                           (http-response-body response))))
                          (multiple-value-bind (response ignored)
                              (http-client-send
                               default-client "GET"
                               (format nil "https://127.0.0.1:~D/cookie" port))
                            (declare (ignore ignored))
                            (ensure-equal 200 (http-response-status response))
                            (ensure-equal "cookie-ok"
                                          (octets-as-string
                                           (http-response-body response)))))
                     (if previous-certificate-file
                         (sb-posix:setenv "SSL_CERT_FILE"
                                         previous-certificate-file 1)
                         (sb-posix:unsetenv "SSL_CERT_FILE"))))))
          (when input
            (ignore-errors (close input :abort t)))
          (when output
            (ignore-errors (close output :abort t)))
          (when process
            (ignore-errors (uiop:terminate-process process))
            (ignore-errors (uiop:wait-process process)))
          (when server-thread
            (ignore-errors (sb-thread:join-thread server-thread))))))))
