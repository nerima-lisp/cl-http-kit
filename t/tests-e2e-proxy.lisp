(in-package #:http-kit/test)

#+sbcl
(progn
  (defun %proxy-e2e-read-headers (stream)
    (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0)))
      (loop for byte = (read-byte stream nil nil)
            do (unless byte (error "Proxy E2E peer reached EOF before headers."))
               (vector-push-extend byte bytes)
               (when (and (>= (length bytes) 4)
                          (= (aref bytes (- (length bytes) 4)) 13)
                          (= (aref bytes (- (length bytes) 3)) 10)
                          (= (aref bytes (- (length bytes) 2)) 13)
                          (= (aref bytes (- (length bytes) 1)) 10))
                 (return (copy-seq bytes))))))

  (defun %proxy-e2e-set-environment (bindings thunk)
    (let ((previous
            (mapcar (lambda (binding)
                      (cons (car binding) (uiop:getenv (car binding))))
                    bindings)))
      (unwind-protect
           (progn
             (dolist (binding bindings)
               (if (cdr binding)
                   (sb-posix:setenv (car binding) (cdr binding) 1)
                   (sb-posix:unsetenv (car binding))))
             (funcall thunk))
        (dolist (binding previous)
          (if (cdr binding)
              (sb-posix:setenv (car binding) (cdr binding) 1)
              (sb-posix:unsetenv (car binding)))))))

  (defun %proxy-e2e-origin (body &key (max-connections 1))
    (let ((listener (open-http-tcp-listener :host "127.0.0.1" :port 0))
          (error-value nil) (thread nil))
      (setf thread
            (sb-thread:make-thread
             (lambda ()
               (serve-http1-listener
                listener
                (lambda (request)
                  (declare (ignore request))
                  (make-http-response
                   :status 200
                   :headers (list (make-http-header "Connection" "close"))
                   :body body))
                :max-connections max-connections
                :on-error (lambda (condition address port)
                            (declare (ignore address port))
                            (setf error-value condition))))))
      (values listener thread
              (lambda ()
                (sb-thread:join-thread thread)
                (when error-value (error error-value))))))

  (defun %proxy-e2e-forward-proxy (origin-port)
    (let ((listener (open-http-tcp-listener :host "127.0.0.1" :port 0))
          (requests nil) (authorization nil) (error-value nil) (thread nil))
      (setf thread
            (sb-thread:make-thread
             (lambda ()
               (serve-http1-listener
                listener
                (lambda (request)
                  (push request requests)
                  (let ((received (http-header-value
                                   (http-request-headers request)
                                   "Proxy-Authorization")))
                    (when received (setf authorization received))
                    (if (= (length requests) 1)
                        (progn
                          (when received
                            (error "The initial proxy request unexpectedly carried authorization."))
                          (make-http-response
                           :status 407
                           :headers (list
                                     (make-http-header "Proxy-Authenticate"
                                                       "Basic realm=proxy-e2e")
                                     (make-http-header "Connection" "close"))
                           :body (ascii "proxy-auth-required")))
                        (let* ((upstream-request
                                 (make-http-request
                                  :method (http-request-method request)
                                  :uri (http-request-uri request)
                                  :headers (http-request-headers request)
                                  :body (http-request-body request)))
                               (upstream-stream
                                 (open-http-tcp-stream
                                  (make-http-request
                                   :method "GET"
                                   :uri (format nil "http://127.0.0.1:~D/"
                                                origin-port))
                                  :timeout 5)))
                          (unwind-protect
                               (multiple-value-bind (response reusable-p)
                                   (send-http-request-over-open-stream
                                    upstream-request upstream-stream :timeout 5
                                    :request-target
                                    (let ((path (http-request-path request))
                                          (query (http-request-query request)))
                                      (if query (format nil "~A?~A" path query)
                                          path)))
                                 (declare (ignore reusable-p))
                                 response)
                            (close-http-tcp-stream upstream-stream))))))
                :max-connections 2
                :on-error (lambda (condition address port)
                            (declare (ignore address port))
                            (setf error-value condition))))))
      (values listener thread
              (lambda ()
                (sb-thread:join-thread thread)
                (when error-value (error error-value)))
              (lambda () (nreverse requests))
              (lambda () authorization))))

  (defun %proxy-e2e-relay (left right)
    (let ((errors nil))
      (labels ((copy (source destination)
                 (handler-case
                     (loop for byte = (read-byte source nil nil)
                           while byte
                           do (write-byte byte destination)
                              (finish-output destination))
                   (stream-error () nil)
                   (error (condition) (push condition errors)))))
        (let ((thread (sb-thread:make-thread
                       (lambda () (copy right left)))))
          (copy left right)
          (sb-thread:join-thread thread)))
      (values errors)))

  (defun %proxy-e2e-connect-proxy (target-port)
    (let ((listener (open-http-tcp-listener :host "127.0.0.1" :port 0))
          (seen-requests nil) (authorized-p nil) (error-value nil) (thread nil))
      (setf thread
            (sb-thread:make-thread
             (lambda ()
               (handler-case
                   (loop repeat 1
                         for client-stream =
                           (accept-http-tcp-stream listener :timeout 5)
                         for wire = (%proxy-e2e-read-headers client-stream)
                         for text = (octets-as-string wire)
                         do (push text seen-requests)
                            (if (search "Proxy-Authorization: Basic dXNlcjpwYXNz"
                                        text :test #'char-equal)
                                (progn
                                  (unless (and (search
                                                (format nil "CONNECT 127.0.0.1:~D HTTP/1.1"
                                                        target-port)
                                                text :test #'char-equal)
                                               (search
                                                (format nil "Host: 127.0.0.1:~D"
                                                        target-port)
                                                text :test #'char-equal))
                                    (error "The proxy CONNECT request had an unexpected target."))
                                  (setf authorized-p t)
                                  (let* ((target-request
                                           (make-http-request
                                            :method "GET"
                                            :uri (format nil "http://127.0.0.1:~D/"
                                                         target-port)))
                                         (target-stream
                                           (open-http-tcp-stream target-request
                                                                 :timeout 5)))
                                    (unwind-protect
                                         (progn
                                           (write-sequence
                                            (ascii "HTTP/1.1 200 Connection Established|CRLF|Proxy-Agent: e2e|CRLF||CRLF|")
                                            client-stream)
                                           (finish-output client-stream)
                                           (multiple-value-bind (relay-errors)
                                               (%proxy-e2e-relay client-stream
                                                                 target-stream)
                                             (when relay-errors
                                               (error "Proxy relay failed: ~S"
                                                      relay-errors))))
                                      (close-http-tcp-stream target-stream))))
                                (progn
                                  (write-sequence
                                   (ascii "HTTP/1.1 407 Proxy Authentication Required|CRLF|Proxy-Authenticate: Basic realm=proxy-e2e|CRLF|Connection: close|CRLF||CRLF|proxy-auth-required")
                                   client-stream)
                                  (finish-output client-stream)))
                            (close-http-tcp-stream client-stream))
                 (error (condition) (setf error-value condition))))))
      (values listener thread
              (lambda ()
                (sb-thread:join-thread thread)
                (when error-value (error error-value)))
              (lambda () (nreverse seen-requests))
              (lambda () authorized-p)))))

  (defun %proxy-e2e-certificate (directory)
    (let ((certificate (namestring (merge-pathnames "server.crt" directory)))
          (key (namestring (merge-pathnames "server.key" directory)))
          (openssl (or (uiop:getenv "OPENSSL") "openssl")))
      (uiop:run-program
       (list openssl "req" "-x509" "-newkey" "rsa:2048" "-nodes"
             "-days" "1" "-subj" "/CN=127.0.0.1"
             "-addext" "subjectAltName=IP:127.0.0.1"
             "-keyout" key "-out" certificate)
       :output :string :error-output :string)
      (values openssl certificate key)))

  (defun %proxy-e2e-openssl-server (openssl port certificate key)
    (let* ((process (uiop:launch-program
                     (list openssl "s_server" "-quiet"
                           "-accept" (princ-to-string port)
                           "-cert" certificate "-key" key "-tls1_3"
                           "-alpn" "http/1.1")
                     :input :stream :output :stream :error-output :stream
                     :element-type '(unsigned-byte 8) :wait nil))
           (input (uiop:process-info-input process))
           (output (uiop:process-info-output process))
           (error-value nil)
           (thread (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (let ((wire (%proxy-e2e-read-headers output)))
                            (unless (search "GET /proxy-tls HTTP/1.1"
                                            (octets-as-string wire))
                              (error "OpenSSL origin received unexpected request."))
                            (write-sequence
                             (ascii "HTTP/1.1 200 OK|CRLF|Connection: close|CRLF|Content-Length: 13|CRLF||CRLF|tls-origin-ok")
                             input)
                            (finish-output input))
                        (error (condition) (setf error-value condition)))))))
      (sleep 1)
      (values process input output thread
              (lambda ()
                (sb-thread:join-thread thread)
                (when error-value (error error-value))))))

  (deftest e2e-http-proxy-environment-basic-retry
    (multiple-value-bind (origin-listener origin-thread join-origin)
        (%proxy-e2e-origin (ascii "origin-through-http-proxy"))
      (multiple-value-bind (proxy-listener proxy-thread join-proxy requests auth)
          (%proxy-e2e-forward-proxy
           (http-network-listener-port origin-listener))
        (unwind-protect
             (progn
               (%proxy-e2e-set-environment
                `(("http_proxy" . ,(format nil "http://127.0.0.1:~D"
                                            (http-network-listener-port
                                             proxy-listener)))
                  ("HTTP_PROXY") ("https_proxy") ("HTTPS_PROXY")
                  ("no_proxy") ("NO_PROXY"))
                (lambda ()
                  (let ((client (make-http-client
                                 :proxy nil
                                 :proxy-challenge-auth-provider
                                 (lambda (&rest ignored)
                                   (declare (ignore ignored))
                                   (http-basic-authorization "user" "pass"))
                                 :strict-transport-store nil
                                 :alternative-service-store nil)))
                    (multiple-value-bind (response ignored)
                        (http-client-send
                         client "GET"
                         (format nil "http://127.0.0.1:~D/http?via=proxy"
                                 (http-network-listener-port origin-listener))
                         :timeout 5)
                      (declare (ignore ignored))
                      (ensure-equal 200 (http-response-status response))
                      (ensure-equal "origin-through-http-proxy"
                                    (octets-as-string
                                     (http-response-body response)))))))
               (funcall join-proxy) (funcall join-origin)
               (let ((seen (funcall requests)))
                 (ensure-equal 2 (length seen))
                 (ensure-true
                  (search "http://127.0.0.1:"
                          (http-request-target (first seen))))
                 (ensure-equal "Basic dXNlcjpwYXNz" (funcall auth))))
          (ignore-errors (sb-thread:join-thread proxy-thread))
          (ignore-errors (sb-thread:join-thread origin-thread))
          (close-http-tcp-listener proxy-listener)
          (close-http-tcp-listener origin-listener)))))

  (deftest e2e-https-proxy-connect-tls-and-lowercase-environment
    (let* ((directory (merge-pathnames "cl-http-kit-proxy-e2e/"
                                      (uiop:temporary-directory)))
           (origin-listener (open-http-tcp-listener :host "127.0.0.1" :port 0))
           (origin-port (http-network-listener-port origin-listener))
           (proxy-listener nil) (proxy-thread nil) (join-proxy nil)
           (proxy-requests nil) (proxy-authorized-p nil)
           (process nil) (tls-input nil) (tls-output nil)
           (tls-thread nil) (join-tls nil))
      (close-http-tcp-listener origin-listener)
      (ensure-directories-exist directory)
      (multiple-value-bind (openssl certificate key)
          (%proxy-e2e-certificate directory)
        (let ((trust-anchor
                (cl-tls-kit.x509:parse-certificate-der
                 (cl-tls-kit:pem-block-der
                  (first (cl-tls-kit:pem-decode
                          (uiop:read-file-string certificate)))))))
          (multiple-value-setq (process tls-input tls-output tls-thread join-tls)
            (%proxy-e2e-openssl-server openssl origin-port certificate key))
          (multiple-value-setq (proxy-listener proxy-thread join-proxy
                                               proxy-requests proxy-authorized-p)
            (%proxy-e2e-connect-proxy origin-port))
          (let ((failure nil))
            (unwind-protect
                 (handler-case
                     (%proxy-e2e-set-environment
                      `(("https_proxy" . ,(format nil "http://user:pass@127.0.0.1:~D"
                                                   (http-network-listener-port
                                                    proxy-listener)))
                        ("HTTPS_PROXY") ("http_proxy") ("HTTP_PROXY")
                        ("no_proxy") ("NO_PROXY"))
                      (lambda ()
                        (let ((client (make-http-client
                                       :proxy nil
                                       :tls-upgrade
                                       (make-http-tls-upgrader
                                        :verify :required
                                        :trust-anchors (list trust-anchor)
                                        :alpn-protocols '("http/1.1"))
                                       :proxy-challenge-auth-provider
                                       (lambda (&rest ignored)
                                         (declare (ignore ignored))
                                         (http-basic-authorization "user" "pass"))
                                       :strict-transport-store nil
                                       :alternative-service-store nil)))
                          (multiple-value-bind (response ignored)
                              (http-client-send
                               client "GET"
                               (format nil "https://127.0.0.1:~D/proxy-tls"
                                       origin-port)
                               :timeout 5)
                            (declare (ignore ignored))
                            (ensure-equal 200 (http-response-status response))
                            (ensure-equal "tls-origin-ok"
                                          (octets-as-string
                                           (http-response-body response)))))))
                   (error (condition)
                     (setf failure condition)))
            (when process
              (ignore-errors (uiop:terminate-process process))
              (ignore-errors (uiop:wait-process process)))
            (handler-case
                (funcall join-proxy)
              (error (condition)
                (unless failure (setf failure condition))))
            (when (and (null failure) proxy-listener)
              (handler-case
                  (progn
                    (ensure-equal 1 (length (funcall proxy-requests)))
                    (ensure-true (funcall proxy-authorized-p)))
                (error (condition)
                  (setf failure condition))))
            (handler-case
                (funcall join-tls)
              (error (condition)
                (unless failure (setf failure condition))))
            (when proxy-listener (close-http-tcp-listener proxy-listener))
            (when failure (error failure))))))))

  (deftest e2e-no-proxy-lowercase-direct-no-proxy-connection
    (multiple-value-bind (origin-listener origin-thread join-origin)
        (%proxy-e2e-origin (ascii "direct-origin"))
      (let ((proxy-listener (open-http-tcp-listener :host "127.0.0.1" :port 0))
            (proxy-connections 0)
            (proxy-error nil)
            (proxy-thread nil))
        (setf proxy-thread
              (sb-thread:make-thread
               (lambda ()
                 (handler-case
                     (multiple-value-bind (stream ignored-address ignored-port)
                         (accept-http-tcp-stream proxy-listener :timeout 1)
                       (declare (ignore ignored-address ignored-port))
                       (incf proxy-connections)
                       (close-http-tcp-stream stream))
                   (http-timeout () nil)
                   (error (condition)
                     (setf proxy-error condition))))))
        (unwind-protect
             (%proxy-e2e-set-environment
              `(("http_proxy" . ,(format nil "http://127.0.0.1:~D"
                                          (http-network-listener-port
                                           proxy-listener)))
                ("HTTP_PROXY") ("no_proxy" . "127.0.0.1") ("NO_PROXY"))
              (lambda ()
                (let ((client (make-http-client
                               :proxy nil :strict-transport-store nil
                               :alternative-service-store nil)))
                  (multiple-value-bind (response ignored)
                      (http-client-send
                       client "GET"
                       (format nil "http://127.0.0.1:~D/direct"
                               (http-network-listener-port origin-listener))
                       :timeout 5)
                    (declare (ignore ignored))
                    (ensure-equal 200 (http-response-status response))
                    (ensure-equal "direct-origin"
                                  (octets-as-string
                                   (http-response-body response)))))))
          (sb-thread:join-thread proxy-thread)
          (when proxy-error (error proxy-error))
          (ensure-equal 0 proxy-connections)
          (close-http-tcp-listener proxy-listener)
          (funcall join-origin)
          (close-http-tcp-listener origin-listener)))))
