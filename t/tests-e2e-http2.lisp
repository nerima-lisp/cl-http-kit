(in-package #:http-kit/test)

#+sbcl
(progn
  (defun %http2-e2e-write-file (pathname content)
    (with-open-file (stream pathname
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (write-sequence content stream)))

  (deftest e2e-http2-nghttpd-alpn-and-multiplex
    (let* ((directory (merge-pathnames "cl-http-kit-http2-e2e/"
                                      (uiop:temporary-directory)))
           (certificate (namestring (merge-pathnames "server.crt" directory)))
           (key (namestring (merge-pathnames "server.key" directory)))
           (one (ascii "nghttpd-one"))
           (two (ascii "nghttpd-two"))
           (nghttpd (or (uiop:getenv "NGHTTPD") "nghttpd"))
           (port (multiple-value-bind (listener selected-port)
                     (%network-test-listener)
                   (%network-test-close-socket listener)
                   selected-port))
           (process nil)
           (tcp-stream nil)
           (tls-stream nil)
           (connection nil))
      (ensure-directories-exist directory)
      (%http2-e2e-write-file (merge-pathnames "one.txt" directory) one)
      (%http2-e2e-write-file (merge-pathnames "two.txt" directory) two)
      (uiop:run-program
       (list "openssl" "req" "-x509" "-newkey" "rsa:2048" "-nodes"
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
               (setf process
                     (uiop:launch-program
                      (list nghttpd "-a" "127.0.0.1"
                            "-d" (namestring directory)
                            "-m" "100"
                            (princ-to-string port) key certificate)
                      :output :stream
                      :error-output :stream
                      :wait nil))
               (%e2e-wait-for-tcp "127.0.0.1" port)
               (let* ((request
                        (make-http-request
                         :method "GET"
                         :uri (format nil "https://127.0.0.1:~D/one.txt" port)))
                      (tls-upgrade
                        (make-http-tls-upgrader
                         :verify :required
                         :trust-anchors (list trust-anchor)
                         :alpn-protocols '("h2" "http/1.1"))))
                 (setf tcp-stream (open-http-tcp-stream request :timeout 5)
                       tls-stream (funcall tls-upgrade
                                           tcp-stream
                                           (http-request-uri request)
                                           :timeout 5)
                       connection
                         (make-http2-connection
                          :stream tls-stream
                          :close-stream #'close))
                 (ensure-equal "h2"
                               (http-tls-selected-alpn-protocol tls-stream))
                 (let ((chunks '())
                       (responses nil))
                   (setf responses
                         (send-http2-requests-over-connection
                          connection
                          (list request
                                (make-http-request
                                 :method "GET"
                                 :uri (format nil
                                              "https://127.0.0.1:~D/two.txt"
                                              port)))
                          :timeout 5
                          :on-body-chunk
                          (lambda (payload callback-request)
                            (declare (ignore payload))
                            (push (http-uri-path
                                   (http-request-uri callback-request))
                                  chunks))))
                   (ensure-equal 2 (length responses))
                   (ensure-equal 2 (length chunks))
                   (ensure-equal '("/one.txt" "/two.txt")
                                 (sort (copy-seq chunks) #'string<))
                   (dolist (response (coerce responses 'list))
                     (ensure-equal 200 (http-response-status response))
                     (ensure-equal "HTTP/2"
                                   (http-response-protocol-version response)))
                   (ensure-octets-equal one
                                         (http-response-body (first responses)))
                   (ensure-octets-equal two
                                         (http-response-body (second responses))))))
          (when connection
            (ignore-errors (close-http2-connection connection)))
          (when tls-stream
            (ignore-errors (close tls-stream :abort t)))
          (when tcp-stream
            (ignore-errors (close tcp-stream :abort t)))
          (when process
            (ignore-errors (uiop:terminate-process process))
            (ignore-errors (uiop:wait-process process))))))))

#+sbcl
(deftest e2e-http2-nghttpd-native-client-alpn-dispatch
  (let* ((directory (merge-pathnames "cl-http-kit-http2-native-client-e2e/"
                                     (uiop:temporary-directory)))
         (certificate (namestring (merge-pathnames "server.crt" directory)))
         (key (namestring (merge-pathnames "server.key" directory)))
         (body (ascii "native-default-client"))
         (nghttpd (or (uiop:getenv "NGHTTPD") "nghttpd"))
         (port (multiple-value-bind (listener selected-port)
                   (%network-test-listener)
                 (%network-test-close-socket listener)
                 selected-port))
         (process nil))
    (ensure-directories-exist directory)
    (%http2-e2e-write-file (merge-pathnames "body.txt" directory) body)
    (uiop:run-program
     (list "openssl" "req" "-x509" "-newkey" "rsa:2048" "-nodes"
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
             (setf process
                   (uiop:launch-program
                    (list nghttpd "-a" "127.0.0.1"
                          "-d" (namestring directory)
                          "-m" "100"
                          (princ-to-string port) key certificate)
                    :output :stream
                    :error-output :stream
                    :wait nil))
             (%e2e-wait-for-tcp "127.0.0.1" port)
             (let* ((selected-alpn nil)
                    (tls-upgrader
                      (make-http-tls-upgrader
                       :verify :required
                       :trust-anchors (list trust-anchor)
                       :alpn-protocols (list "h2" "http/1.1")))
                    (tls-upgrade
                      (lambda (stream uri &key timeout deadline &allow-other-keys)
                        (let ((tls-stream
                                (funcall tls-upgrader stream uri
                                         :timeout timeout
                                         :deadline deadline)))
                          (setf selected-alpn
                                (http-tls-selected-alpn-protocol tls-stream))
                          (values tls-stream selected-alpn))))
                    (client
                      (make-http-client :tls-upgrade tls-upgrade
                                        :cache nil
                                        :default-timeout 5))
                    (response
                      (http-client-send
                       client "GET"
                       (format nil "https://127.0.0.1:~D/body.txt" port))))
               (ensure-true (http-client-connection-pool client))
               (ensure-equal "h2" selected-alpn)
               (ensure-equal 200 (http-response-status response))
               (ensure-equal "HTTP/2"
                             (http-response-protocol-version response))
               (ensure-octets-equal body (http-response-body response))))
        (when process
          (ignore-errors (uiop:terminate-process process))
          (ignore-errors (uiop:wait-process process)))))))

