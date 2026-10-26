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
(deftest e2e-http2-connection-manager-cold-open-client-tcp-tls-session-and-stream-ids
  (let* ((directory
           (merge-pathnames
            (format nil "cl-http-kit-http2-cold-open-e2e-~36R/"
                    (random (expt 2 64)))
            (uiop:temporary-directory)))
         (certificate (namestring (merge-pathnames "server.crt" directory)))
         (key (namestring (merge-pathnames "server.key" directory)))
         (one (ascii "cold-open-first"))
         (two (ascii "cold-open-second"))
         (nghttpd (or (uiop:getenv "NGHTTPD") "nghttpd"))
         (port (multiple-value-bind (listener selected-port)
                   (%network-test-listener)
                 (%network-test-close-socket listener)
                 selected-port))
         (process nil)
         (client nil)
         (counter-lock (sb-thread:make-mutex :name "HTTP/2 cold-open counters"))
         (client-tcp-open-calls 0)
         (tls-upgrades 0)
         (manager-open-callbacks 0)
         (constructed-entries 0)
         (sessions nil)
         (stream-ids nil)
         (first-manager-open-entered (sb-thread:make-semaphore :count 0))
         (release-first-manager-open (sb-thread:make-semaphore :count 0))
         (second-manager-waiting (sb-thread:make-semaphore :count 0))
         (first-response nil)
         (second-response nil)
         (first-error nil)
         (second-error nil)
         (first-thread nil)
         (second-thread nil)
         (runtime-symbol 'http-kit/client::%client-runtime-function)
         (original-runtime (symbol-function runtime-symbol))
         (open-symbol 'http-kit/http2::%h2-manager-open-connection)
         (original-open (symbol-function open-symbol))
         (wait-symbol 'http-kit/http2::%h2-manager-wait-for-opening)
         (original-wait (symbol-function wait-symbol))
         (new-entry-symbol 'http-kit/http2::%h2-manager-new-entry)
         (original-new-entry (symbol-function new-entry-symbol))
         (connection-symbol 'http-kit/http2:make-http2-connection)
         (original-make-connection (symbol-function connection-symbol))
         (scheduler-open-symbol 'http-kit/http2::%h2-scheduler-open-entry)
         (original-scheduler-open (symbol-function scheduler-open-symbol)))
    (unwind-protect
         (progn
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
             (setf process
                   (uiop:launch-program
                    (list nghttpd "-a" "127.0.0.1"
                          "-d" (namestring directory)
                          "-m" "100"
                          (princ-to-string port) key certificate)
                    :output :stream
                    :error-output :stream
                    :wait nil))
             (ensure-true (%e2e-wait-for-tcp "127.0.0.1" port :timeout 5))
             (setf (symbol-function runtime-symbol)
                   (lambda (package-name function-name)
                     (cond
                       ((and (string= package-name "HTTP-KIT/NETWORK")
                             (string= function-name
                                      "MAKE-HTTP-NETWORK-STREAM-OPENER"))
                        (let ((factory
                                (funcall original-runtime package-name
                                         function-name)))
                          (lambda (&rest factory-arguments)
                            (let ((opener (apply factory factory-arguments)))
                              (lambda (&rest open-arguments)
                                (sb-thread:with-mutex (counter-lock)
                                  (incf client-tcp-open-calls))
                                (apply opener open-arguments))))))
                       ((and (string= package-name "HTTP-KIT/TLS")
                             (string= function-name "MAKE-HTTP-TLS-UPGRADER"))
                        (lambda (&key alpn-protocols)
                          (ensure-equal '("h2" "http/1.1") alpn-protocols)
                          (let ((tls-upgrader
                                  (make-http-tls-upgrader
                                   :verify :required
                                   :trust-anchors (list trust-anchor)
                                   :alpn-protocols alpn-protocols)))
                            (lambda (stream uri &key timeout deadline)
                              (sb-thread:with-mutex (counter-lock)
                                (incf tls-upgrades))
                              (funcall tls-upgrader stream uri
                                       :timeout timeout
                                       :deadline deadline)))))
                       (t
                        (funcall original-runtime package-name function-name)))))
             (setf (symbol-function open-symbol)
                   (lambda (manager &rest arguments)
                     (let ((first-open-p nil))
                       (sb-thread:with-mutex (counter-lock)
                         (incf manager-open-callbacks)
                         (setf first-open-p (= manager-open-callbacks 1)))
                       (when first-open-p
                         (sb-thread:signal-semaphore
                          first-manager-open-entered)
                         (unless (sb-thread:wait-on-semaphore
                                  release-first-manager-open :timeout 10)
                           (error "Timed out waiting to release the first manager open")))
                       (apply original-open manager arguments))))
             (setf (symbol-function wait-symbol)
                   (lambda (manager open-key opening deadline clock-function)
                     (sb-thread:signal-semaphore second-manager-waiting)
                     (funcall original-wait manager open-key opening
                              deadline clock-function)))
             (setf (symbol-function new-entry-symbol)
                   (lambda (&rest arguments)
                     (let ((entry (apply original-new-entry arguments)))
                       (sb-thread:with-mutex (counter-lock)
                         (incf constructed-entries))
                       entry)))
             (setf (symbol-function connection-symbol)
                   (lambda (&rest arguments)
                     (let ((connection
                             (apply original-make-connection arguments)))
                       (sb-thread:with-mutex (counter-lock)
                         (push connection sessions))
                       connection)))
             (setf (symbol-function scheduler-open-symbol)
                   (lambda (connection &rest arguments)
                     (let ((entry
                             (apply original-scheduler-open connection arguments)))
                       (sb-thread:with-mutex (counter-lock)
                         (push
                          (http-kit/http2::%h2-batch-entry-stream-id entry)
                          stream-ids))
                       entry)))
             (setf client (make-http-client :cache nil :default-timeout 15))
             (ensure-equal 0 client-tcp-open-calls)
             (ensure-equal 0 tls-upgrades)
             (ensure-equal 0 (length sessions))
             (setf first-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (setf first-response
                                (sb-ext:with-timeout 20
                                  (http-client-send
                                   client "GET"
                                   (format nil "https://127.0.0.1:~D/one.txt"
                                           port))))
                        (error (condition)
                          (setf first-error condition))))))
             (ensure-true
              (sb-thread:wait-on-semaphore first-manager-open-entered
                                           :timeout 10))
             (sb-thread:with-mutex (counter-lock)
               (ensure-equal 1 client-tcp-open-calls)
               (ensure-equal 1 tls-upgrades)
               (ensure-equal 1 manager-open-callbacks)
               (ensure-equal 0 constructed-entries)
               (ensure-equal 0 (length sessions)))
             (setf second-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (setf second-response
                                (sb-ext:with-timeout 20
                                  (http-client-send
                                   client "GET"
                                   (format nil "https://127.0.0.1:~D/two.txt"
                                           port))))
                        (error (condition)
                          (setf second-error condition))))))
             (ensure-true
              (sb-thread:wait-on-semaphore second-manager-waiting :timeout 10))
             (sb-thread:with-mutex (counter-lock)
               (ensure-equal 1 client-tcp-open-calls)
               (ensure-equal 1 tls-upgrades)
               (ensure-equal 1 manager-open-callbacks)
               (ensure-equal 0 constructed-entries)
               (ensure-equal 0 (length sessions)))
             (sb-thread:signal-semaphore release-first-manager-open)
             (sb-thread:join-thread first-thread)
             (setf first-thread nil)
             (sb-thread:join-thread second-thread)
             (setf second-thread nil)
             (ensure-true (null first-error))
             (ensure-true (null second-error))
             (ensure-equal 200 (http-response-status first-response))
             (ensure-equal 200 (http-response-status second-response))
             (ensure-equal "HTTP/2"
                           (http-response-protocol-version first-response))
             (ensure-equal "HTTP/2"
                           (http-response-protocol-version second-response))
             (ensure-octets-equal one (http-response-body first-response))
             (ensure-octets-equal two (http-response-body second-response))
             (sb-thread:with-mutex (counter-lock)
               (ensure-equal 1 client-tcp-open-calls)
               (ensure-equal 1 tls-upgrades)
               (ensure-equal 1 manager-open-callbacks)
               (ensure-equal 1 constructed-entries)
               (ensure-equal 1 (length sessions))
               (ensure-equal '(1 3) (sort (copy-list stream-ids) #'<)))
             (http-client-clear-connections client)))
      (sb-thread:signal-semaphore release-first-manager-open)
      (when first-thread
        (ignore-errors (sb-thread:join-thread first-thread)))
      (when second-thread
        (ignore-errors (sb-thread:join-thread second-thread)))
      (when client
        (ignore-errors (http-client-clear-connections client)))
      (setf (symbol-function scheduler-open-symbol) original-scheduler-open)
      (setf (symbol-function connection-symbol) original-make-connection)
      (setf (symbol-function new-entry-symbol) original-new-entry)
      (setf (symbol-function wait-symbol) original-wait)
      (setf (symbol-function open-symbol) original-open)
      (setf (symbol-function runtime-symbol) original-runtime)
      (when process
        (ignore-errors (uiop:terminate-process process))
        (ignore-errors (uiop:wait-process process)))
      (ignore-errors (uiop:delete-directory-tree directory :validate t)))))

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
               (ensure-equal 3 upgraded))
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

#+sbcl
(deftest e2e-http2-single-request-callers-share-one-multiplexed-connection-with-reentrant-callback
  (multiple-value-bind (listener port)
      (%network-test-listener)
    (let ((server-thread nil)
          (server-socket nil)
          (server-stream nil)
          (client-stream nil)
          (connection nil)
          (caller-threads (make-array 3 :initial-element nil))
          (responses (make-array 3 :initial-element nil))
          (caller-errors (make-array 3 :initial-element nil))
          (stream-ids (make-array 3 :initial-element nil))
          (ping-payload (ascii "pingtest"))
          (callback-count 0)
          (server-error nil)
          (nested-response nil)
          (nested-error nil)
          (nested-stream-id nil)
          (ping-response nil)
          (ping-ack-sent-p nil)
          (late-data-sent-p nil)
          (partial-data-flow-credit-seen-p nil)
          (late-data-flow-credit-seen-p nil)
          (timeout-entry nil)
          (timeout-body-chunk-seen-p nil)
          (timeout-reset-seen-p nil)
          (nested-request-seen-p nil)
          (both-requests-before-response-p nil)
          (lock (sb-thread:make-mutex :name "HTTP/2 scheduler E2E"))
          (waitqueue (sb-thread:make-waitqueue :name "HTTP/2 scheduler E2E")))
      (labels ((check-prewrite-deadline ()
                 (let* ((clock (lambda () 1))
                        (probe-connection
                          (make-http2-connection
                           :stream (make-string-output-stream)))
                        (request
                          (make-http-request
                           :method "GET"
                           :uri "http://example.test/prewrite"))
                        (active-entry
                          (http-kit/http2::%make-h2-batch-entry
                           :request request :stream-id 1))
                        (timeout-seen-p nil))
                   (setf (http-kit/http2::%http2-connection-session-started-p
                          probe-connection) t
                         (http-kit/http2::%http2-connection-next-stream-id
                          probe-connection) 3
                         (http-kit/http2::%http2-connection-scheduler-last-opened-stream-id
                          probe-connection) 1
                         (http-kit/http2::%http2-connection-scheduler-entries
                          probe-connection) (list active-entry))
                   (handler-case
                       (http-kit/http2::%h2-scheduler-open-entry
                        probe-connection request nil nil
                        (http-kit/http2::%http2-connection-max-header-bytes
                         probe-connection)
                        (http-kit/http2::%http2-connection-max-fields
                         probe-connection)
                        (http-kit/http2::%http2-connection-max-body-bytes
                         probe-connection)
                        0 clock nil nil nil t nil
                        (lambda (wire)
                          (declare (ignore wire))
                          (http-kit::%check-deadline 0 clock :write)))
                     (http-kit:http-timeout ()
                       (setf timeout-seen-p t)))
                   (ensure-true timeout-seen-p)
                   (ensure-equal
                    (list active-entry)
                    (http-kit/http2::%http2-connection-scheduler-entries
                     probe-connection))
                   (ensure-equal
                    5
                    (http-kit/http2::%http2-connection-next-stream-id
                     probe-connection))
                   (ensure-equal
                    1
                    (http-kit/http2::%http2-connection-scheduler-last-opened-stream-id
                     probe-connection))
                   (ensure-true
                    (http-kit/http2::http2-connection-open-p probe-connection))
                   (ensure-true
                    (http-kit/http2::%http2-connection-session-started-p
                     probe-connection))))
               (wait-for-callers (count)
                 (let ((deadline (+ (http-kit::%monotonic-time) 5))
                       (timed-out-p nil))
                   (sb-thread:grab-mutex lock)
                   (unwind-protect
                        (loop while (< callback-count count)
                              for remaining =
                                (- deadline (http-kit::%monotonic-time))
                              do (when (<= remaining 0)
                                   (setf timed-out-p t)
                                   (return))
                                 (unless
                                     (sb-thread:condition-wait
                                      waitqueue lock :timeout remaining)
                                   (setf timed-out-p t)
                                   (return)))
                     (when (sb-thread:holding-mutex-p lock)
                       (sb-thread:release-mutex lock)))
                   (when timed-out-p
                     (error "Timed out waiting for ~D HTTP/2 callbacks."
                            count))
                   (sb-thread:with-mutex (lock)
                     (ensure-equal count callback-count))))
               (start-caller
                   (index path timeout &optional information-callback body-callback)
                 (setf (aref caller-threads index)
                       (sb-thread:make-thread
                        (lambda ()
                          (handler-case
                              (setf (aref responses index)
                                    (send-http2-request-over-connection
                                     connection
                                     (make-http-request
                                      :method "GET"
                                      :uri (format nil
                                                   "http://127.0.0.1:~D/~A"
                                                   port path))
                                     :timeout timeout
                                     :on-information information-callback
                                     :on-body-chunk body-callback
                                     :on-stream-open
                                     (lambda (stream-id &rest ignored)
                                       (declare (ignore ignored))
                                       (sb-thread:with-mutex (lock)
                                         (setf (aref stream-ids index)
                                               stream-id)
                                         (when (= index 2)
                                           (sb-thread:with-mutex
                                               ((http-kit/http2::%http2-connection-scheduler-lock
                                                 connection))
                                             (setf timeout-entry
                                                   (http-kit/http2::%h2-scheduler-entry-for-stream
                                                    connection stream-id))))
                                         (incf callback-count)
                                         (sb-thread:condition-broadcast
                                          waitqueue)))))
                            (error (condition)
                              (setf (aref caller-errors index) condition))))))))
        (unwind-protect
             (progn
               (check-prewrite-deadline)
               (setf server-thread
                     (sb-thread:make-thread
                      (lambda ()
                        (unwind-protect
                             (handler-case
                                 (progn
                                   (setf server-socket
                                         (sb-bsd-sockets:socket-accept listener)
                                         server-stream
                                         (sb-bsd-sockets:socket-make-stream
                                          server-socket
                                          :input t
                                          :output t
                                          :element-type '(unsigned-byte 8)
                                          :buffering :full))
                                   (let* ((preface
                                            (make-array
                                             (length
                                              http-kit/http2::+http2-connection-preface+)
                                             :element-type '(unsigned-byte 8)))
                                          (position 0))
                                     (loop while (< position (length preface))
                                           for next =
                                             (read-sequence preface server-stream
                                                            :start position)
                                           do (when (= next position)
                                                (error "The HTTP/2 client closed before its preface."))
                                              (setf position next))
                                     (unless (equalp
                                              preface
                                              http-kit/http2::+http2-connection-preface+)
                                       (error "The HTTP/2 client sent an invalid preface.")))
                                   (let* ((reader
                                            (http-kit/http2::%h2-reader-for
                                             server-stream))
                                          (deadline
                                            (+ (http-kit::%monotonic-time) 5))
                                          (request-streams nil))
                                     (loop while (< (length request-streams) 3)
                                           for frame =
                                             (http-kit/http2::%h2-read-frame
                                              reader
                                              http-kit/http2::+http2-default-max-frame-size+
                                              deadline
                                              #'http-kit::%monotonic-time)
                                           do (when (eq frame :eof)
                                                (error "The HTTP/2 client closed before opening three streams."))
                                              (when (= (http-kit/http2::%h2-frame-type frame)
                                                       http-kit/http2::+http2-headers-type+)
                                                (push
                                                 (http-kit/http2::%h2-frame-stream-id frame)
                                                 request-streams)))
                                     (wait-for-callers 3)
                                     (setf both-requests-before-response-p t
                                           deadline
                                           (+ (http-kit::%monotonic-time) 5))
                                     (let* ((information-headers
                                              (http-kit/http2::%h2-encode-header-block
                                               '((":status" . "103"))))
                                            (headers
                                              (http-kit/http2::%h2-encode-header-block
                                               '((":status" . "200"))))
                                            (empty-payload
                                              (make-array
                                               0
                                               :element-type '(unsigned-byte 8)))
                                            (initial-stream-ids
                                              (loop for stream-id across stream-ids
                                                    collect stream-id)))
                                       (write-sequence
                                        (http-kit/http2::%h2-frame-wire
                                         http-kit/http2::+http2-settings-type+
                                         0 0 empty-payload)
                                        server-stream)
                                       (write-sequence
                                        (http-kit/http2::%h2-frame-wire
                                         http-kit/http2::+http2-headers-type+
                                         http-kit/http2::+http2-end-headers-flag+
                                         (aref stream-ids 0)
                                         information-headers)
                                        server-stream)
                                       (write-sequence
                                        (http-kit/http2::%h2-frame-wire
                                         http-kit/http2::+http2-headers-type+
                                         http-kit/http2::+http2-end-headers-flag+
                                         (aref stream-ids 2)
                                         headers)
                                        server-stream)
                                       (write-sequence
                                        (http-kit/http2::%h2-frame-wire
                                         http-kit/http2::+http2-data-type+
                                         0
                                         (aref stream-ids 2)
                                         (ascii "partial-timeout-body"))
                                        server-stream)
                                       (finish-output server-stream)
                                       (loop until (and nested-request-seen-p
                                                        timeout-reset-seen-p
                                                        partial-data-flow-credit-seen-p)
                                             for frame =
                                               (http-kit/http2::%h2-read-frame
                                                reader
                                                http-kit/http2::+http2-default-max-frame-size+
                                                deadline
                                                #'http-kit::%monotonic-time)
                                             do (when (eq frame :eof)
                                                  (error "The HTTP/2 client closed before the nested request and stream reset."))
                                                (cond
                                                  ((= (http-kit/http2::%h2-frame-type frame)
                                                      http-kit/http2::+http2-headers-type+)
                                                   (let ((stream-id
                                                           (http-kit/http2::%h2-frame-stream-id frame)))
                                                     (unless (member stream-id initial-stream-ids)
                                                       (setf nested-stream-id stream-id
                                                             nested-request-seen-p t)
                                                       (write-sequence
                                                        (http-kit/http2::%h2-frame-wire
                                                         http-kit/http2::+http2-headers-type+
                                                         http-kit/http2::+http2-end-headers-flag+
                                                         stream-id headers)
                                                        server-stream)
                                                       (write-sequence
                                                        (http-kit/http2::%h2-frame-wire
                                                         http-kit/http2::+http2-data-type+
                                                         http-kit/http2::+http2-end-stream-flag+
                                                         stream-id
                                                         (ascii "nested-response"))
                                                        server-stream)
                                                       (finish-output server-stream))))
                                                  ((and (= (http-kit/http2::%h2-frame-type frame)
                                                           http-kit/http2::+http2-rst-stream-type+)
                                                        (= (http-kit/http2::%h2-frame-stream-id frame)
                                                           (aref stream-ids 2)))
                                                   (setf timeout-reset-seen-p t))
                                                  ((= (http-kit/http2::%h2-frame-type frame)
                                                      http-kit/http2::+http2-ping-type+)
                                                   (unless (and
                                                            (zerop
                                                             (http-kit/http2::%h2-frame-stream-id
                                                              frame))
                                                            (= (length
                                                                (http-kit/http2::%h2-frame-payload
                                                                 frame))
                                                               8)
                                                            (zerop
                                                             (logand
                                                              (http-kit/http2::%h2-frame-flags
                                                               frame)
                                                              http-kit/http2::+http2-ack-flag+)))
                                                     (error "The client sent an invalid HTTP/2 PING."))
                                                   (write-sequence
                                                    (http-kit/http2::%h2-frame-wire
                                                     http-kit/http2::+http2-ping-type+
                                                     http-kit/http2::+http2-ack-flag+
                                                     0
                                                     (http-kit/http2::%h2-frame-payload
                                                      frame))
                                                    server-stream)
                                                   (finish-output server-stream)
                                                   (setf ping-ack-sent-p t))
                                                  ((and
                                                    (= (http-kit/http2::%h2-frame-type frame)
                                                       http-kit/http2::+http2-window-update-type+)
                                                    (zerop
                                                     (http-kit/http2::%h2-frame-stream-id
                                                      frame)))
                                                   (when (and
                                                          (= (length
                                                              (http-kit/http2::%h2-frame-payload
                                                               frame))
                                                             4)
                                                          (= (http-kit/http2::%h2-u32
                                                              (http-kit/http2::%h2-frame-payload
                                                               frame)
                                                              0)
                                                             (length
                                                              (ascii "partial-timeout-body"))))
                                                     (setf partial-data-flow-credit-seen-p
                                                           t)))))
                                       (write-sequence
                                        (http-kit/http2::%h2-frame-wire
                                         http-kit/http2::+http2-headers-type+
                                         http-kit/http2::+http2-end-headers-flag+
                                         (aref stream-ids 2)
                                         headers)
                                        server-stream)
                                       (write-sequence
                                        (http-kit/http2::%h2-frame-wire
                                         http-kit/http2::+http2-data-type+
                                         http-kit/http2::+http2-end-stream-flag+
                                         (aref stream-ids 2)
                                         (ascii "late-timeout-body"))
                                        server-stream)
                                       (finish-output server-stream)
                                       (setf late-data-sent-p t
                                             deadline
                                             (+ (http-kit::%monotonic-time) 5))
                                       (loop until late-data-flow-credit-seen-p
                                             for frame =
                                               (http-kit/http2::%h2-read-frame
                                                reader
                                                http-kit/http2::+http2-default-max-frame-size+
                                                deadline
                                                #'http-kit::%monotonic-time)
                                             do (when (eq frame :eof)
                                                  (error "The HTTP/2 client closed before returning late DATA flow credit."))
                                                (when (and
                                                       (= (http-kit/http2::%h2-frame-type frame)
                                                          http-kit/http2::+http2-window-update-type+)
                                                       (zerop
                                                        (http-kit/http2::%h2-frame-stream-id
                                                         frame))
                                                       (= (length
                                                           (http-kit/http2::%h2-frame-payload
                                                            frame))
                                                          4)
                                                       (= (http-kit/http2::%h2-u32
                                                           (http-kit/http2::%h2-frame-payload
                                                            frame)
                                                           0)
                                                          (length
                                                           (ascii "late-timeout-body"))))
                                                  (setf late-data-flow-credit-seen-p
                                                        t)))
                                       (dolist (stream-id
                                                (list (aref stream-ids 1)
                                                      (aref stream-ids 0)))
                                         (write-sequence
                                          (http-kit/http2::%h2-frame-wire
                                           http-kit/http2::+http2-headers-type+
                                           http-kit/http2::+http2-end-headers-flag+
                                           stream-id headers)
                                          server-stream))
                                       (write-sequence
                                        (http-kit/http2::%h2-frame-wire
                                         http-kit/http2::+http2-data-type+
                                         0
                                         (aref stream-ids 1)
                                         (ascii "response-"))
                                        server-stream)
                                       (write-sequence
                                        (http-kit/http2::%h2-frame-wire
                                         http-kit/http2::+http2-data-type+
                                         http-kit/http2::+http2-end-stream-flag+
                                         (aref stream-ids 0)
                                         (ascii "response-first"))
                                        server-stream)
                                       (write-sequence
                                        (http-kit/http2::%h2-frame-wire
                                         http-kit/http2::+http2-data-type+
                                         http-kit/http2::+http2-end-stream-flag+
                                         (aref stream-ids 1)
                                         (ascii "second"))
                                        server-stream)
                                       (finish-output server-stream))))
                               (error (condition)
                                 (setf server-error condition)))
                          (when server-error
                            (%network-test-close-stream server-stream)
                            (%network-test-close-socket server-socket)))))))
               (setf client-stream
                     (open-http-tcp-stream
                      (make-http-request
                       :method "GET"
                       :uri (format nil "http://127.0.0.1:~D/first" port))
                      :timeout 5)
                     connection
                     (make-http2-connection
                      :stream client-stream
                      :close-stream #'close))
               (start-caller
                0 "first" 6
                (lambda (information-response)
                  (when (= (http-response-status information-response) 103)
                    (handler-case
                        (progn
                          (setf ping-response
                                (ping-http2-connection
                                 connection :payload ping-payload :timeout 4))
                          (setf nested-response
                                (send-http2-request-over-connection
                                 connection
                                 (make-http-request
                                  :method "GET"
                                  :uri (format nil "http://127.0.0.1:~D/nested" port))
                                 :timeout 4)))
                      (error (condition)
                        (setf nested-error condition))))))
               (wait-for-callers 1)
               (start-caller 1 "second" 6)
               (wait-for-callers 2)
               (start-caller
                2 "timeout" 2 nil
                (lambda (payload)
                  (when (equalp payload (ascii "partial-timeout-body"))
                    (setf timeout-body-chunk-seen-p t))))
               (wait-for-callers 3)
               (loop for thread across caller-threads
                     when thread do (sb-thread:join-thread thread))
               (sb-thread:join-thread server-thread)
               (ensure-equal nil server-error)
               (ensure-equal nil (aref caller-errors 0))
               (ensure-equal nil (aref caller-errors 1))
               (ensure-true (typep (aref caller-errors 2) 'http-kit:http-timeout))
               (ensure-equal nil nested-error)
               (ensure-true both-requests-before-response-p)
               (ensure-true nested-request-seen-p)
               (ensure-true ping-ack-sent-p)
               (ensure-true late-data-sent-p)
               (ensure-true partial-data-flow-credit-seen-p)
               (ensure-true late-data-flow-credit-seen-p)
               (ensure-true timeout-body-chunk-seen-p)
               (ensure-true timeout-reset-seen-p)
               (ensure-true timeout-entry)
               (ensure-equal
                0
                (length
                 (http-kit/http2::%h2-batch-entry-body-vector timeout-entry)))
               (ensure-equal
                0
                (http-kit/http2::%h2-batch-entry-body-length timeout-entry))
               (ensure-equal nil
                             (http-kit/http2::%h2-batch-entry-response timeout-entry))
               (ensure-equal nil
                             (http-kit/http2::%h2-scheduler-entry-for-stream
                              connection (aref stream-ids 2)))
               (ensure-octets-equal ping-payload ping-response)
               (ensure-equal 200 (http-response-status (aref responses 0)))
               (ensure-equal 200 (http-response-status (aref responses 1)))
               (ensure-equal 200 (http-response-status nested-response))
               (ensure-octets-equal
                (ascii "response-first")
                (http-response-body (aref responses 0)))
               (ensure-octets-equal
                (ascii "response-second")
                (http-response-body (aref responses 1)))
               (ensure-octets-equal
                (ascii "nested-response")
                (http-response-body nested-response)))
          (when connection
            (ignore-errors (close-http2-connection connection)))
          (%network-test-close-stream client-stream)
          (%network-test-close-stream server-stream)
          (%network-test-close-socket server-socket)
          (%network-test-close-socket listener)
          (when server-thread
            (ignore-errors (sb-thread:join-thread server-thread)))
          (loop for thread across caller-threads
                when thread do (ignore-errors (sb-thread:join-thread thread)))))))

