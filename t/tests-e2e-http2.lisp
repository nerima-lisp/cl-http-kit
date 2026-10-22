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
(deftest e2e-http2-nghttpd-native-default-client-reuses-clears-and-overlaps
  (let* ((directory (merge-pathnames
                     (format nil "cl-http-kit-http2-native-client-e2e-~36R/"
                             (random (expt 2 64)))
                     (uiop:temporary-directory)))
         (certificate (namestring (merge-pathnames "server.crt" directory)))
         (key (namestring (merge-pathnames "server.key" directory)))
         (body (ascii "native-default-client"))
         (second-body (ascii "native-second-request"))
         (client-clear-body (ascii "native-after-client-clear"))
         (pool-clear-body (ascii "native-after-pool-clear"))
         (parallel-first-body (ascii "native-parallel-first"))
         (parallel-second-body (ascii "native-parallel-second"))
         (nghttpd (or (uiop:getenv "NGHTTPD") "nghttpd"))
         (port (multiple-value-bind (listener selected-port)
                   (%network-test-listener)
                 (%network-test-close-socket listener)
                 selected-port))
         (process nil)
         (client nil)
         (upgraded 0)
         (upgrade-lock (sb-thread:make-mutex :name "HTTP/2 E2E TLS counter"))
         (runtime-symbol 'http-kit/client::%client-runtime-function)
         (original-runtime (symbol-function runtime-symbol))
         (first-callback-arrived (sb-thread:make-semaphore :count 0))
         (release-first-callback (sb-thread:make-semaphore :count 0))
         (second-completed (sb-thread:make-semaphore :count 0))
         (first-callback-entered-p nil)
         (first-parallel-response nil)
         (second-parallel-response nil)
         (first-parallel-error nil)
         (second-parallel-error nil)
         (first-thread nil)
         (second-thread nil))
    (let ((trust-anchor nil))
      (handler-case
          (progn
            (ensure-directories-exist directory)
            (%http2-e2e-write-file (merge-pathnames "body.txt" directory) body)
            (%http2-e2e-write-file (merge-pathnames "second.txt" directory) second-body)
            (%http2-e2e-write-file (merge-pathnames "client-clear.txt" directory)
                                   client-clear-body)
            (%http2-e2e-write-file (merge-pathnames "pool-clear.txt" directory)
                                   pool-clear-body)
            (%http2-e2e-write-file (merge-pathnames "parallel-first.txt" directory)
                                   parallel-first-body)
            (%http2-e2e-write-file (merge-pathnames "parallel-second.txt" directory)
                                   parallel-second-body)
            (uiop:run-program
             (list "openssl" "req" "-x509" "-newkey" "rsa:2048" "-nodes"
                   "-days" "1" "-subj" "/CN=127.0.0.1"
                   "-addext" "subjectAltName=IP:127.0.0.1"
                   "-keyout" key "-out" certificate)
             :output :string :error-output :string)
            (setf trust-anchor
                  (cl-tls-kit.x509:parse-certificate-der
                   (cl-tls-kit:pem-block-der
                    (first (cl-tls-kit:pem-decode
                            (uiop:read-file-string certificate)))))))
        (error (condition)
          (ignore-errors (uiop:delete-directory-tree directory :validate t))
          (error condition)))
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
             (setf (symbol-function runtime-symbol)
                   (lambda (package-name function-name)
                     (if (and (string= package-name "HTTP-KIT/TLS")
                              (string= function-name "MAKE-HTTP-TLS-UPGRADER"))
                         (lambda (&key alpn-protocols)
                           (ensure-equal '("h2" "http/1.1") alpn-protocols)
                           (let ((tls-upgrader
                                   (make-http-tls-upgrader
                                    :verify :required
                                    :trust-anchors (list trust-anchor)
                                    :alpn-protocols alpn-protocols)))
                             (lambda (stream uri &key timeout deadline)
                               (sb-thread:with-mutex (upgrade-lock)
                                 (incf upgraded))
                               (funcall tls-upgrader stream uri
                                        :timeout timeout
                                        :deadline deadline))))
                         (funcall original-runtime package-name function-name))))
             (setf client
                   (make-http-client :cache nil :default-timeout 5))
             (ensure-true (http-client-connection-pool client))
             (let* ((url-base (format nil "https://127.0.0.1:~D/" port))
                    (response
                      (http-client-send client "GET"
                                        (concatenate 'string url-base "body.txt")))
                    (second-response
                      (http-client-send client "GET"
                                        (concatenate 'string url-base "second.txt"))))
               (ensure-equal 200 (http-response-status response))
               (ensure-equal 200 (http-response-status second-response))
               (ensure-equal "HTTP/2"
                             (http-response-protocol-version response))
               (ensure-equal "HTTP/2"
                             (http-response-protocol-version second-response))
               (ensure-octets-equal body (http-response-body response))
               (ensure-octets-equal second-body
                                    (http-response-body second-response))
               (sb-thread:with-mutex (upgrade-lock)
                 (ensure-equal 1 upgraded)))
             (ensure-equal 0 (http-client-clear-connections client))
             (let ((response
                     (http-client-send
                      client "GET"
                      (format nil "https://127.0.0.1:~D/client-clear.txt" port))))
               (sb-thread:with-mutex (upgrade-lock)
                 (ensure-equal 2 upgraded))
               (ensure-equal 200 (http-response-status response))
               (ensure-equal "HTTP/2"
                             (http-response-protocol-version response))
               (ensure-octets-equal client-clear-body
                                    (http-response-body response)))
             (ensure-equal
              0
              (http-connection-pool-clear
               (http-client-connection-pool client)))
             (let ((response
                     (http-client-send
                      client "GET"
                      (format nil "https://127.0.0.1:~D/pool-clear.txt" port))))
               (sb-thread:with-mutex (upgrade-lock)
                 (ensure-equal 3 upgraded))
               (ensure-equal 200 (http-response-status response))
               (ensure-equal "HTTP/2"
                             (http-response-protocol-version response))
               (ensure-octets-equal pool-clear-body
                                    (http-response-body response)))
             (setf first-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (setf first-parallel-response
                                (http-client-send
                                 client "GET"
                                 (format nil
                                         "https://127.0.0.1:~D/parallel-first.txt"
                                         port)
                                 :on-body-chunk
                                 (lambda (chunk)
                                   (declare (ignore chunk))
                                   (unless first-callback-entered-p
                                     (setf first-callback-entered-p t)
                                     (sb-thread:signal-semaphore
                                      first-callback-arrived)
                                     (sb-thread:wait-on-semaphore
                                      release-first-callback)))))
                        (error (condition)
                          (setf first-parallel-error condition))))))
             (ensure-true
              (sb-thread:wait-on-semaphore first-callback-arrived :timeout 5))
             (setf second-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (setf second-parallel-response
                                (http-client-send
                                 client "GET"
                                 (format nil
                                         "https://127.0.0.1:~D/parallel-second.txt"
                                         port)))
                        (error (condition)
                          (setf second-parallel-error condition)))
                      (sb-thread:signal-semaphore second-completed))))
             (ensure-true
              (sb-thread:wait-on-semaphore second-completed :timeout 5))
             (ensure-true (null second-parallel-error))
             (ensure-equal 200 (http-response-status second-parallel-response))
             (ensure-equal "HTTP/2"
                           (http-response-protocol-version second-parallel-response))
             (ensure-octets-equal parallel-second-body
                                  (http-response-body second-parallel-response))
             (sb-thread:with-mutex (upgrade-lock)
               (ensure-equal 4 upgraded))
             (sb-thread:signal-semaphore release-first-callback)
             (sb-thread:join-thread first-thread)
             (setf first-thread nil)
             (sb-thread:join-thread second-thread)
             (setf second-thread nil)
             (ensure-true (null first-parallel-error))
             (ensure-equal 200 (http-response-status first-parallel-response))
             (ensure-equal "HTTP/2"
                           (http-response-protocol-version first-parallel-response))
             (ensure-octets-equal parallel-first-body
                                  (http-response-body first-parallel-response)))
        (sb-thread:signal-semaphore release-first-callback)
        (when first-thread
          (ignore-errors (sb-thread:join-thread first-thread)))
        (when second-thread
          (ignore-errors (sb-thread:join-thread second-thread)))
        (when client
          (ignore-errors (http-client-clear-connections client)))
        (setf (symbol-function runtime-symbol) original-runtime)
        (when process
          (ignore-errors (uiop:terminate-process process))
          (ignore-errors (uiop:wait-process process)))
        (ignore-errors (uiop:delete-directory-tree directory :validate t))))))

