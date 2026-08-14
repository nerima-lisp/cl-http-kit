(in-package #:http-kit/test)

(defun client-test-response (status &key headers body)
  (make-http-response :status status
                      :headers headers
                      :body (or body (octets))))

(deftest client-uri-encoding-and-authentication
  (let ((resolved (resolve-http-uri
                   "http://example.test/a/b"
                   "../c?x=1#fragment")))
    (ensure-equal "http://example.test/c?x=1"
                  (http-uri-string resolved))
    (ensure-true (http-same-origin-p resolved "http://example.test:80/")))
  (ensure-equal "q=a+b&x=1%2B2"
                (http-form-urlencode '(("q" "a b") ("x" "1+2"))))
  (ensure-equal "Basic dXNlcjpwYXNz"
                (http-basic-authorization "user" "pass"))
  (ensure-equal "Bearer token"
                (http-bearer-authorization "token")))

(deftest client-request-trailers-through-policy
  (let ((seen nil)
        (trailers (list (make-http-header "X-Checksum" "abc"))))
    (with-test-client (client
                       (lambda (request &key proxy-plan &allow-other-keys)
                         (declare (ignore proxy-plan))
                         (setf seen request)
                         (client-test-response 200))
                       :cache nil)
      (let ((request (http-client-request
                      client "POST" "http://example.test/upload"
                      :trailers trailers)))
      (ensure-equal "abc"
                    (http-header-value
                     (http-request-trailers request) "X-Checksum"))
      (multiple-value-bind (response effective)
          (http-client-send client request)
        (declare (ignore response))
        (ensure-equal "abc"
                      (http-header-value
                       (http-request-trailers effective) "X-Checksum"))
        (ensure-equal "abc"
                      (http-header-value
                       (http-request-trailers seen) "X-Checksum"))))))

(deftest client-multipart-parser-round-trip
  (multiple-value-bind (body content-type)
      (make-http-multipart-body
       (list (make-http-multipart-part :name "field" :value "value")
             (make-http-multipart-part
              :name "upload"
              :value (octets 0 1 2 255)
              :filename "data.bin"
              :content-type "application/octet-stream"))
       :boundary "boundary")
    (ensure-equal "multipart/form-data; boundary=boundary" content-type)
    (let ((parts (parse-http-multipart-body
                  body
                  :content-type "multipart/form-data; boundary=\"boundary\"")))
      (ensure-equal 2 (length parts))
      (let ((field (first parts))
            (upload (second parts)))
        (ensure-equal "field" (http-multipart-part-name field))
        (ensure-equal (octets-as-string (octets 118 97 108 117 101))
                      (octets-as-string (http-multipart-part-value field)))
        (ensure-equal "upload" (http-multipart-part-name upload))
        (ensure-equal "data.bin" (http-multipart-part-filename upload))
        (ensure-equal "application/octet-stream"
                      (http-multipart-part-content-type upload))
        (ensure-equal (octets 0 1 2 255)
                      (http-multipart-part-value upload))))))

(deftest client-multipart-parser-enforces-limits
  (multiple-value-bind (body content-type)
      (make-http-multipart-body
       (list (make-http-multipart-part :name "field" :value "value"))
       :boundary "boundary")
    (signals http-size-limit-exceeded
      (parse-http-multipart-body body :content-type content-type :max-parts 0))
    (signals http-size-limit-exceeded
      (parse-http-multipart-body body :content-type content-type :max-body-bytes 2))
    (signals http-protocol-error
      (parse-http-multipart-body body
                                 :content-type "multipart/form-data; boundary=other"))))

(deftest client-cookie-jar
  (let* ((jar (make-http-cookie-jar :clock-function (lambda () 1000)))
         (response (client-test-response
                    200
                    :headers (list (make-http-header
                                    "Set-Cookie"
                                    "sid=abc; Path=/; Max-Age=60")))))
    (http-cookie-jar-accept-response
     jar "http://example.test/login" response :now 1000)
    (ensure-equal "sid=abc"
                  (http-cookie-jar-cookie-header
                   jar "http://example.test/dashboard" :now 1001))
    (ensure-equal nil
                  (http-cookie-jar-cookie-header
                   jar "http://other.example/dashboard" :now 1001))))

(deftest client-cache-integration
  (let ((calls 0)
        (cache (make-http-cache :clock-function (lambda () 1000))))
    (with-test-client (client
                       (lambda (request &key proxy-plan &allow-other-keys)
                         (declare (ignore request proxy-plan))
                         (incf calls)
                         (client-test-response
                          200
                          :headers (list (make-http-header
                                          "Cache-Control" "max-age=60"))
                          :body (ascii "cached")))
                       :cache cache)
      (let ((request (http-client-request client "GET"
                                          "http://example.test/resource")))
      (multiple-value-bind (response effective)
          (http-client-send client request)
        (ensure-equal 200 (http-response-status response))
        (ensure-equal request effective))
      (multiple-value-bind (response effective)
          (http-client-send client request)
        (ensure-equal 200 (http-response-status response))
        (ensure-equal request effective))
      (ensure-equal 1 calls)
      (ensure-equal 1 (length (http-cache-entries cache))))))))

(deftest client-cache-keeps-get-when-head-is-stored
  (let* ((cache (make-http-cache :clock-function (lambda () 1000)))
         (get-request (make-http-request
                       :method "GET"
                       :uri "http://example.test/resource"))
         (head-request (make-http-request
                        :method "HEAD"
                        :uri "http://example.test/resource"))
         (headers (list (make-http-header "Cache-Control" "max-age=60"))))
    (http-cache-store cache get-request
                      (client-test-response 200
                                             :headers headers
                                             :body (ascii "get"))
                      :now 1000)
    (http-cache-store cache head-request
                      (client-test-response 200 :headers headers)
                      :now 1000)
    (multiple-value-bind (response state entry)
        (http-cache-lookup cache get-request :now 1001)
      (declare (ignore entry))
      (ensure-equal :fresh state)
      (ensure-equal "get"
                    (map 'string #'code-char (http-response-body response))))
    (multiple-value-bind (response state entry)
        (http-cache-lookup cache head-request :now 1001)
      (declare (ignore entry))
      (ensure-equal :fresh state)
      (ensure-equal "HEAD" (http-request-method head-request))
      (ensure-equal 200 (http-response-status response)))
    (ensure-equal 2 (length (http-cache-entries cache)))))

(deftest client-redirects-and-hooks
  (let ((calls 0)
        (methods nil)
        (uris nil)
        (request-events 0)
        (response-events 0))
    (with-test-client (client
                       (lambda (request &key proxy-plan &allow-other-keys)
                         (declare (ignore proxy-plan))
                         (incf calls)
                         (push (http-request-method request) methods)
                         (push (http-uri-string (http-request-uri request)) uris)
                         (if (= calls 1)
                             (client-test-response
                              302
                              :headers (list (make-http-header
                                              "Location" "/final")))
                             (client-test-response 200 :body (ascii "done"))))
                       :cache nil
                       :on-request (lambda (request attempt)
                                     (declare (ignore request attempt))
                                     (incf request-events))
                       :on-response (lambda (response request attempt)
                                      (declare (ignore response request attempt))
                                      (incf response-events)))
      (let ((request (http-client-request client "POST"
                                          "http://example.test/start"
                                          :body (ascii "payload"))))
      (multiple-value-bind (response effective)
          (http-client-send client request)
        (ensure-equal 200 (http-response-status response))
        (ensure-equal "http://example.test/final"
                      (http-uri-string (http-request-uri effective)))
        (ensure-equal '(("POST" . "http://example.test/start")
                        ("GET" . "http://example.test/final"))
                      (mapcar #'cons (reverse methods) (reverse uris)))
        (ensure-equal 2 calls)
        (ensure-equal 2 request-events)
        (ensure-equal 2 response-events))))))

(deftest client-redirect-drops-trailers-when-method-changes
  (let ((calls 0)
        (seen-trailers nil))
    (let* ((client
             (make-http-client
              :cache nil
              :transport-function
              (lambda (request &key proxy-plan &allow-other-keys)
                (declare (ignore proxy-plan))
                (incf calls)
                (push (http-request-trailers request) seen-trailers)
                (if (= calls 1)
                    (client-test-response
                     302
                     :headers (list (make-http-header
                                     "Location" "/final")))
                    (client-test-response 200)))))
           (request (http-client-request
                     client "POST" "http://example.test/start"
                     :trailers (list (make-http-header "X-Checksum" "abc")))))
      (multiple-value-bind (response effective)
          (http-client-send client request)
        (declare (ignore response))
        (ensure-equal "GET" (http-request-method effective))
        (ensure-equal nil (http-request-trailers effective)))
      (ensure-equal 2 calls)
      (ensure-equal nil (first seen-trailers))
      (ensure-equal "abc"
                    (http-header-value (second seen-trailers) "X-Checksum")))))

(deftest client-retries
  (let ((calls 0)
        (sleeps nil))
    (let* ((client
             (make-http-client
              :cache nil
              :sleep-function (lambda (seconds)
                                (push seconds sleeps))
              :retry-policy (make-http-retry-policy
                             :max-attempts 2
                             :base-delay 0.25
                             :max-delay 0.25)
              :transport-function
              (lambda (request &key proxy-plan &allow-other-keys)
                (declare (ignore request proxy-plan))
                (incf calls)
                (if (= calls 1)
                    (client-test-response 503)
                    (client-test-response 200 :body (ascii "ok"))))))
           (request (http-client-request client "GET"
                                         "http://example.test/retry")))
      (multiple-value-bind (response effective)
          (http-client-send client request)
        (declare (ignore effective))
        (ensure-equal 200 (http-response-status response))
        (ensure-equal "ok"
                      (octets-as-string (http-response-body response))))
      (ensure-equal 2 calls)
      (ensure-equal '(0.25) sleeps)))
  (let ((calls 0)
        (condition nil))
    (let* ((client
             (make-http-client
              :cache nil
              :retry-policy (make-http-retry-policy :max-attempts 2)
              :transport-function
              (lambda (request &key proxy-plan &allow-other-keys)
                (declare (ignore request proxy-plan))
                (incf calls)
                (client-test-response 503))))
           (request (http-client-request client "GET"
                                         "http://example.test/exhausted")))
      (handler-case
          (http-client-send client request)
        (http-retry-exhausted (caught)
          (setf condition caught)))
      (ensure-true condition)
      (ensure-equal 2 calls)
      (ensure-equal 2 (http-retry-exhausted-attempts condition))
      (ensure-equal 503
                     (http-response-status
                     (http-retry-exhausted-last-response condition))))))

(deftest client-streaming-request-body-replay-factory
  (let ((attempts 0)
        (factory-calls 0)
        (bodies nil))
    (let ((client
            (make-http-client
             :cache nil
             :retry-policy (make-http-retry-policy
                            :max-attempts 2
                            :methods '("POST")
                            :base-delay 0
                            :max-delay 0)
             :transport-function
             (lambda (request &key request-body-function &allow-other-keys)
               (declare (ignore request))
               (incf attempts)
               (let ((chunks nil))
                 (loop for chunk = (funcall request-body-function 65536)
                       while chunk
                       do (push (octets-as-string chunk) chunks))
                 (push (nreverse chunks) bodies))
               (if (= attempts 1)
                   (client-test-response 503)
                   (client-test-response 200 :body (ascii "ok")))))))
      (multiple-value-bind (response effective)
          (http-client-send
           client
           (http-client-request client "POST" "http://example.test/retry-body")
           :request-body-factory
           (lambda ()
             (incf factory-calls)
             (let ((chunks (list (ascii "abc")
                                 (ascii "de"))))
               (lambda (maximum-size)
                 (ensure-equal 65536 maximum-size)
                 (pop chunks))))
           :request-body-length 5)
        (declare (ignore effective))
        (ensure-equal 200 (http-response-status response))
        (ensure-equal "ok" (octets-as-string (http-response-body response))))
      (ensure-equal 2 attempts)
      (ensure-equal 2 factory-calls)
      (ensure-equal '(("abc" "de") ("abc" "de"))
                    (reverse bodies)))))

(deftest client-streaming-request-body-does-not-retry-without-factory
  (let ((attempts 0)
        (bodies nil)
        (chunks (list (ascii "abc")
                      (ascii "de"))))
    (let ((client
            (make-http-client
             :cache nil
             :retry-policy (make-http-retry-policy :max-attempts 2)
             :transport-function
             (lambda (request &key request-body-function &allow-other-keys)
               (declare (ignore request))
               (incf attempts)
               (let ((received nil))
                 (loop for chunk = (funcall request-body-function 65536)
                       while chunk
                       do (push (octets-as-string chunk) received))
                 (push (nreverse received) bodies))
               (client-test-response 503)))))
      (multiple-value-bind (response effective)
          (http-client-send
           client
           (http-client-request client "POST" "http://example.test/no-replay")
           :request-body-function
           (lambda (maximum-size)
             (ensure-equal 65536 maximum-size)
             (pop chunks))
           :request-body-length 5)
        (declare (ignore effective))
        (ensure-equal 503 (http-response-status response)))
      (ensure-equal 1 attempts)
      (ensure-true (null chunks))
      (ensure-equal '(("abc" "de")) bodies))))

(deftest client-streaming-request-body-does-not-follow-same-method-redirect
  (let ((calls 0)
        (chunks (list (ascii "abc")
                      (ascii "de"))))
    (let ((client
            (make-http-client
             :cache nil
             :transport-function
             (lambda (request &key request-body-function &allow-other-keys)
               (declare (ignore request))
               (incf calls)
               (when request-body-function
                 (loop for chunk = (funcall request-body-function 65536)
                       while chunk))
               (client-test-response
                307
                :headers (list (make-http-header "Location" "/next")))))))
      (multiple-value-bind (response effective)
          (http-client-send
           client
           (http-client-request client "PUT" "http://example.test/start")
           :request-body-function
           (lambda (maximum-size)
             (ensure-equal 65536 maximum-size)
             (pop chunks))
           :request-body-length 5)
        (ensure-equal 307 (http-response-status response))
        (ensure-equal "http://example.test/start"
                      (http-uri-string (http-request-uri effective))))
      (ensure-equal 1 calls)
      (ensure-true (null chunks)))))

(deftest client-proxy-plans
  (let ((proxy (make-http-proxy :scheme :http
                                :host "proxy.example"
                                :port 8080
                                :username "user"
                                :password "pass"
                                :no-proxy "bypass.example")))
    (let ((plan (http-proxy-plan proxy "http://example.test/path")))
      (ensure-equal :forward (getf plan :mode))
      (ensure-equal "http://example.test/path"
                    (getf plan :request-target))
      (ensure-equal "proxy.example" (getf plan :connect-host))
      (ensure-equal 8080 (getf plan :connect-port))
      (ensure-equal "Basic dXNlcjpwYXNz"
                    (getf plan :proxy-authorization)))
    (let ((plan (http-proxy-plan proxy "https://example.test/path")))
      (ensure-equal :connect (getf plan :mode))
      (ensure-equal "example.test" (getf plan :connect-host))
      (ensure-equal 443 (getf plan :connect-port)))
    (ensure-true (http-proxy-no-proxy-p
                 proxy "http://bypass.example/path"))))

#+sbcl
(deftest client-connection-pool-reuses-reusable-stream
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (ascii
             "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|abcHTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|def")))
         (opened 0)
         (closed 0)
         (pool
           (make-http-connection-pool
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (incf opened)
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (incf closed))))
         (client (make-http-client :cache nil :connection-pool pool)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://example.test/one"))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "abc" (octets-as-string (http-response-body response)))
      (ensure-equal "http://example.test/one"
                    (http-uri-string (http-request-uri effective))))
    (ensure-equal 1 opened)
    (ensure-equal 0 closed)
    (ensure-equal 1 (getf (http-connection-pool-stats pool) :idle-count))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://example.test/two"))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "def" (octets-as-string (http-response-body response)))
      (ensure-equal "http://example.test/two"
                    (http-uri-string (http-request-uri effective))))
    (ensure-equal 1 opened)
    (ensure-equal 0 closed)
    (ensure-equal 1 (http-connection-pool-clear pool))
    (ensure-equal 1 closed)
    (ensure-equal
     (ascii
      "GET /one HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 0|CRLF||CRLF|GET /two HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 0|CRLF||CRLF|")
     (binary-test-output stream))))

#+sbcl
(deftest client-connection-pool-closes-non-reusable-stream
  (let* ((first-stream
           (make-instance
            'binary-test-stream
            :input (ascii "HTTP/1.1 200 OK|CRLF||CRLF|abc")))
         (second-stream
           (make-instance
            'binary-test-stream
            :input
            (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|def")))
         (streams (list first-stream second-stream))
         (opened 0)
         (closed-streams nil)
         (pool
           (make-http-connection-pool
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (incf opened)
              (pop streams))
            :close-stream
            (lambda (stream)
              (push stream closed-streams))))
         (client (make-http-client :cache nil :connection-pool pool)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://example.test/first"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "abc" (octets-as-string (http-response-body response))))
    (ensure-equal 1 opened)
    (ensure-equal 1 (length closed-streams))
    (ensure-equal first-stream (first closed-streams))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://example.test/second"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "def" (octets-as-string (http-response-body response))))
    (ensure-equal 2 opened)
    (ensure-equal 1 (getf (http-connection-pool-stats pool) :idle-count))
    (ensure-equal 1 (http-connection-pool-clear pool))
    (ensure-equal 2 (length closed-streams))
    (ensure-true (member second-stream closed-streams :test #'eq))))

#+sbcl
(deftest client-connection-pool-expires-idle-streams
  (let* ((now 0)
         (streams
           (list
            (make-instance
             'binary-test-stream
             :input
             (ascii
              "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|abc"))
            (make-instance
             'binary-test-stream
             :input
             (ascii
              "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|def"))))
         (opened 0)
         (closed 0)
         (pool
           (make-http-connection-pool
            :idle-timeout 10
            :clock-function (lambda () now)
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (incf opened)
              (pop streams))
            :close-stream (lambda (stream)
                            (declare (ignore stream))
                            (incf closed))))
         (client (make-http-client :cache nil :connection-pool pool)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://example.test/"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response)))
    (ensure-equal 1 opened)
    (setf now 11)
    (ensure-equal 0 (getf (http-connection-pool-stats pool) :idle-count))
    (ensure-equal 1 closed)
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://example.test/"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "def" (octets-as-string (http-response-body response))))
    (ensure-equal 2 opened)
    (ensure-equal 1 (http-connection-pool-clear pool))
    (ensure-equal 2 closed)))

#+sbcl
(deftest client-connection-pool-proxy-tls-and-resolver-boundary
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (concatenate-octets
             (octets 5 0
                     5 0 0 1 0 0 0 0 0 0)
             (ascii
              "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok"))))
         (resolved-hosts nil)
         (upgraded-uris nil)
         (opened-plans nil)
         (closed 0)
         (proxy
           (make-http-proxy :scheme :socks5
                            :host "proxy.example"
                            :port 1080))
         (pool
           (make-http-connection-pool
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy))
              (push proxy-plan opened-plans)
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (incf closed))
            :resolve-host
            (lambda (host)
              (push host resolved-hosts)
              "192.0.2.10")
            :tls-upgrade
            (lambda (received-stream uri &key timeout deadline)
              (declare (ignore timeout deadline))
              (push uri upgraded-uris)
              received-stream)))
         (client
           (make-http-client :cache nil
                             :proxy proxy
                             :connection-pool pool)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "https://example.test/path"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (ensure-equal '("example.test") (reverse resolved-hosts))
    (ensure-equal 1 (length upgraded-uris))
    (ensure-equal "https" (http-uri-scheme (first upgraded-uris)))
    (ensure-equal :socks5 (getf (first opened-plans) :mode))
    (let ((output (binary-test-output stream)))
      (ensure-equal
       (octets 5 1 0
               5 1 0 1 192 0 2 10 1 187)
       (subseq output 0 13))
      (ensure-true
       (search "GET /path HTTP/1.1"
               (octets-as-string (subseq output 13)))))
    (ensure-equal 1 (http-connection-pool-clear pool))
    (ensure-equal 1 closed)))

#+sbcl
(deftest client-streaming-response-over-direct-stream
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|abc")))
         (chunks nil)
         (closed nil)
         (client
           (make-http-client
            :cache nil
            :open-stream
            (lambda (request &key timeout deadline &allow-other-keys)
              (declare (ignore request timeout deadline))
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (setf closed t)))))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://example.test/stream")
         :on-body-chunk
         (lambda (chunk)
           (push (octets-as-string chunk) chunks))
         :collect-body-p nil)
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "" (octets-as-string (http-response-body response)))
      (ensure-equal '("abc") (reverse chunks)))
    (ensure-true closed)))

#+sbcl
(deftest client-informational-response-over-direct-stream
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (ascii
             "HTTP/1.1 100 Continue|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok")))
         (statuses nil)
         (closed nil)
         (client
           (make-http-client
            :cache nil
            :open-stream
            (lambda (request &key timeout deadline &allow-other-keys)
              (declare (ignore request timeout deadline))
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (setf closed t)))))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request
          client
          "POST"
          "http://example.test/continue"
          :headers (list (make-http-header "Expect" "100-continue"))
          :body "abc")
         :on-information
         (lambda (information)
           (push (http-response-status information) statuses)))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (ensure-equal '(100) (reverse statuses))
    (ensure-true closed)
    (ensure-equal
     (ascii
      "POST /continue HTTP/1.1|CRLF|Expect: 100-continue|CRLF|Host: example.test|CRLF|Content-Length: 3|CRLF||CRLF|abc")
     (binary-test-output stream))))

#+sbcl
(deftest client-streaming-request-body-over-direct-stream
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok")))
         (chunks (list (octets 97 98 99)
                       (octets 100 101)
                       nil))
         (calls 0)
         (closed nil)
         (client
           (make-http-client
            :cache nil
            :open-stream
            (lambda (request &key timeout deadline &allow-other-keys)
              (declare (ignore request timeout deadline))
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (setf closed t)))))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "POST" "http://example.test/upload")
         :request-body-function
         (lambda (maximum-size)
           (ensure-equal 65536 maximum-size)
           (incf calls)
           (pop chunks))
         :request-body-length 5)
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (ensure-equal 3 calls)
    (ensure-true (null chunks))
    (ensure-true closed)
    (ensure-equal
     (ascii "POST /upload HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 5|CRLF||CRLF|abcde")
     (binary-test-output stream))))

#+sbcl
(deftest client-streaming-request-body-over-connection-pool
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok")))
         (opened 0)
         (closed 0)
         (chunks (list (octets 97 98 99)
                       (octets 100 101)
                       nil))
         (pool
           (make-http-connection-pool
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (incf opened)
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (incf closed))))
         (client (make-http-client :cache nil :connection-pool pool)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "POST" "http://example.test/upload")
         :request-body-function
         (lambda (maximum-size)
           (ensure-equal 65536 maximum-size)
           (pop chunks)))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (ensure-equal 1 opened)
    (ensure-true (null chunks))
    (ensure-equal
     (ascii "POST /upload HTTP/1.1|CRLF|Host: example.test|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|3|CRLF|abc|CRLF|2|CRLF|de|CRLF|0|CRLF||CRLF|")
     (binary-test-output stream))
    (ensure-equal 1 (http-connection-pool-clear pool))
    (ensure-equal 1 closed)))

#+sbcl
(deftest client-http-forward-proxy-wire
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok")))
         (opened 0)
         (closed 0)
         (proxy
           (make-http-proxy :scheme :http
                            :host "proxy.example"
                            :port 8080
                            :username "user"
                            :password "pass"))
         (client
           (make-http-client
            :cache nil
            :proxy proxy
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy))
              (ensure-equal :forward (getf proxy-plan :mode))
              (incf opened)
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (incf closed)))))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET"
                              "http://example.test/path?x=1"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (let ((wire (octets-as-string (binary-test-output stream))))
      (ensure-true (search "GET http://example.test/path?x=1 HTTP/1.1" wire))
      (ensure-true (search "Proxy-Authorization: Basic dXNlcjpwYXNz" wire)))
    (ensure-equal 1 opened)
    (ensure-equal 1 closed)))

#+sbcl
(deftest client-http-connect-proxy-wire
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (ascii
             "HTTP/1.1 200 Connection Established|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok")))
         (upgrades 0)
         (proxy
           (make-http-proxy :scheme :http
                            :host "proxy.example"
                            :port 8080
                            :username "user"
                            :password "pass"))
         (client
           (make-http-client
            :cache nil
            :proxy proxy
            :tls-upgrade
            (lambda (received-stream uri &key timeout deadline)
              (declare (ignore timeout deadline))
              (ensure-equal "https" (http-uri-scheme uri))
              (incf upgrades)
              received-stream)
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy))
              (ensure-equal :connect (getf proxy-plan :mode))
              stream)
            :close-stream #'close)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "https://example.test/path"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (let* ((wire (octets-as-string (binary-test-output stream)))
           (authorization "Proxy-Authorization: Basic dXNlcjpwYXNz")
           (first-authorization (search authorization wire)))
      (ensure-true (search "CONNECT example.test:443 HTTP/1.1" wire))
      (ensure-true (search "GET /path HTTP/1.1" wire))
      (ensure-true first-authorization)
      (ensure-equal nil
                    (search authorization wire
                            :start2 (1+ first-authorization))))
    (ensure-equal 1 upgrades)))

#+sbcl
(deftest client-socks5-proxy-wire
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (concatenate-octets
             (octets 5 0
                     5 0 0 1 0 0 0 0 0 0)
             (ascii
              "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok"))))
         (proxy
           (make-http-proxy :scheme :socks5
                            :host "proxy.example"
                            :port 1080))
         (client
           (make-http-client
            :cache nil
            :proxy proxy
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy))
              (ensure-equal :socks5 (getf proxy-plan :mode))
              stream)
            :close-stream #'close)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://192.0.2.1/path"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (let ((output (binary-test-output stream)))
      (ensure-equal
       (octets 5 1 0
               5 1 0 1 192 0 2 1 0 80)
       (subseq output 0 13))
      (ensure-true
       (search "GET /path HTTP/1.1"
               (octets-as-string (subseq output 13)))))))

(deftest websocket-rfc6455-frame-boundaries
  (let* ((masked-frame
           (make-websocket-frame
            :fin-p t
            :opcode 1
            :mask-p t
            :masking-key (octets #x37 #xfa #x21 #x3d)
            :payload (ascii "Hello")))
         (masked-wire (serialize-websocket-frame masked-frame)))
    (ensure-equal
     (octets #x81 #x85 #x37 #xfa #x21 #x3d
             #x7f #x9f #x4d #x51 #x58)
     masked-wire)
    (multiple-value-bind (parsed consumed)
        (parse-websocket-frame masked-wire :require-mask-p t)
      (ensure-equal 11 consumed)
      (ensure-true (websocket-frame-fin-p parsed))
      (ensure-equal 1 (websocket-frame-opcode parsed))
      (ensure-equal (ascii "Hello") (websocket-frame-payload parsed)))
    (let* ((payload (make-array 126
                                :element-type '(unsigned-byte 8)
                                :initial-element #x61))
           (frame (make-websocket-frame :opcode 2 :payload payload))
           (wire (serialize-websocket-frame frame)))
      (ensure-equal #x7e (aref wire 1))
      (ensure-equal 130 (length wire))
      (multiple-value-bind (parsed consumed)
          (parse-websocket-frame wire)
        (ensure-equal (length wire) consumed)
        (ensure-equal payload (websocket-frame-payload parsed))))
    (signals http-protocol-error
      (parse-websocket-frame
       (serialize-websocket-frame
        (make-websocket-frame :payload (ascii "unmasked")))
       :require-mask-p t))
    (signals http-protocol-error
      (parse-websocket-frame (octets #x81 #x7e 0 125)))
    (signals http-protocol-error
      (parse-websocket-frame
       (octets #x81 #x7f 0 0 0 0 0 0 0 125)))))

#+sbcl
(deftest websocket-stream-rejects-nonminimal-lengths
  (dolist (wire (list (octets #x81 #x7e 0 125)
                      (octets #x81 #x7f 0 0 0 0 0 0 0 125)))
    (signals http-protocol-error
      (read-websocket-frame
       (make-instance 'binary-test-stream :input wire)))))

(deftest websocket-http-upgrade-and-close-payload
  (let* ((request
           (make-http-request
            :method "GET"
            :uri "http://example.test/chat"
            :headers
            (list (make-http-header "Host" "example.test")
                  (make-http-header "Upgrade" "websocket")
                  (make-http-header "Connection" "keep-alive, Upgrade")
                  (make-http-header "Sec-WebSocket-Key"
                                    "dGhlIHNhbXBsZSBub25jZQ==")
                  (make-http-header "Sec-WebSocket-Version" "13")
                  (make-http-header "Sec-WebSocket-Protocol"
                                    "chat, superchat"))))
         (response
           (websocket-upgrade-response
            request
            :protocol "chat"
            :extensions "permessage-deflate")))
    (ensure-true (websocket-upgrade-request-p request))
    (ensure-equal "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
                  (websocket-accept-key
                   "dGhlIHNhbXBsZSBub25jZQ=="))
    (ensure-equal 101 (http-response-status response))
    (ensure-equal "websocket"
                  (http-header-value (http-response-headers response)
                                     "Upgrade"))
    (ensure-equal "chat"
                  (http-header-value (http-response-headers response)
                                     "Sec-WebSocket-Protocol"))
    (ensure-equal "permessage-deflate"
                  (http-header-value (http-response-headers response)
                                     "Sec-WebSocket-Extensions"))
    (signals http-protocol-error
      (websocket-upgrade-response request :protocol "not-offered"))
    (let ((close-payload
            (make-websocket-close-payload :code 1000 :reason "bye")))
      (multiple-value-bind (code reason)
          (parse-websocket-close-payload close-payload)
        (ensure-equal 1000 code)
        (ensure-equal "bye" reason)))
    (signals http-protocol-error
      (make-websocket-close-payload :code 1004))
    (signals http-protocol-error
      (parse-websocket-close-payload (octets 3)))))

#+sbcl
(deftest websocket-client-handshake-keeps-stream-open
  (let* ((key "dGhlIHNhbXBsZSBub25jZQ==")
         (request
           (make-websocket-upgrade-request
            "http://example.test/chat"
            :key key
            :protocols '("chat" "superchat")
            :headers (list (make-http-header "Origin" "http://example.test"))))
         (frame-wire
           (serialize-websocket-frame
            (make-websocket-frame :opcode 1 :payload (ascii "hello"))))
         (stream
           (make-instance
            'binary-test-stream
            :input
            (concatenate-octets
             (ascii
              (concatenate
               'string
               "HTTP/1.1 101 Switching Protocols|CRLF|"
               "Upgrade: websocket|CRLF|"
               "Connection: Upgrade|CRLF|"
               "Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=|CRLF|"
               "Sec-WebSocket-Protocol: chat|CRLF||CRLF|"))
             frame-wire))))
    (ensure-true (websocket-upgrade-request-p request))
    (multiple-value-bind (response reusable-p)
        (websocket-client-handshake stream request)
      (ensure-equal 101 (http-response-status response))
      (ensure-true (not reusable-p))
      (multiple-value-bind (frame consumed)
          (read-websocket-frame stream)
        (ensure-equal (length frame-wire) consumed)
        (ensure-equal 1 (websocket-frame-opcode frame))
        (ensure-equal (ascii "hello") (websocket-frame-payload frame))))
    (let ((wire (octets-as-string (binary-test-output stream))))
      (ensure-true (search "GET /chat HTTP/1.1" wire))
      (ensure-true (search (concatenate 'string "Sec-WebSocket-Key: " key) wire))
      (ensure-true (search "Sec-WebSocket-Protocol: chat, superchat" wire))
      (ensure-true (search "Origin: http://example.test" wire)))
    (signals http-protocol-error
      (make-websocket-upgrade-request
       "http://example.test/chat"
       :key key
       :headers (list (make-http-header "Upgrade" "other"))))
    (let ((bad-stream
            (make-instance
             'binary-test-stream
             :input
             (ascii
              (concatenate
               'string
               "HTTP/1.1 101 Switching Protocols|CRLF|"
               "Upgrade: websocket|CRLF|"
               "Connection: Upgrade|CRLF|"
               "Sec-WebSocket-Accept: invalid|CRLF||CRLF|")))))
      (signals http-protocol-error
        (websocket-client-handshake bad-stream request)))))

#+sbcl
(deftest websocket-fragmented-message-and-control-frames
  (let* ((wire
           (concatenate-octets
            (serialize-websocket-frame
             (make-websocket-frame :opcode 9 :payload (ascii "ping")))
            (serialize-websocket-frame
             (make-websocket-frame :fin-p nil
                                   :opcode 1
                                   :payload (ascii "Hel")))
            (serialize-websocket-frame
             (make-websocket-frame :fin-p t
                                   :opcode 0
                                   :payload (ascii "lo")))))
         (stream (make-instance 'binary-test-stream :input wire))
         (control-opcodes nil))
    (multiple-value-bind (message opcode)
        (read-websocket-message
         stream
         :on-control (lambda (frame)
                       (push (websocket-frame-opcode frame)
                             control-opcodes)))
    (ensure-equal (ascii "Hello") message)
    (ensure-equal 1 opcode))
    (ensure-equal '(9) control-opcodes)))

#+sbcl
(deftest websocket-message-and-control-frame-writers
  (let* ((stream
           (make-instance 'binary-test-stream :input (octets)))
         (masking-key-count 0)
         (message-result
           (multiple-value-list
            (write-websocket-message
             stream "Hello"
             :opcode 1
             :max-frame-payload-bytes 2
             :mask-p t
             :masking-key-function
             (lambda ()
               (incf masking-key-count)
               (octets 1 2 3 4)))))
         (wire (binary-test-output stream)))
    (ensure-equal '(3 5) message-result)
    (ensure-equal 3 masking-key-count)
    (multiple-value-bind (first first-consumed)
        (parse-websocket-frame wire :require-mask-p t)
      (multiple-value-bind (second second-consumed)
          (parse-websocket-frame (subseq wire first-consumed)
                                 :require-mask-p t
                                 :allow-unmasked-p nil)
        (multiple-value-bind (third third-consumed)
            (parse-websocket-frame
             (subseq wire (+ first-consumed second-consumed))
             :require-mask-p t)
          (ensure-equal 1 (websocket-frame-opcode first))
          (ensure-true (not (websocket-frame-fin-p first)))
          (ensure-equal (ascii "He") (websocket-frame-payload first))
          (ensure-equal 0 (websocket-frame-opcode second))
          (ensure-true (not (websocket-frame-fin-p second)))
          (ensure-equal (ascii "ll") (websocket-frame-payload second))
          (ensure-equal 0 (websocket-frame-opcode third))
          (ensure-true (websocket-frame-fin-p third))
          (ensure-equal (ascii "o") (websocket-frame-payload third))
          (ensure-equal (length wire)
                        (+ first-consumed second-consumed third-consumed))))))
  (let ((stream (make-instance 'binary-test-stream :input (octets))))
    (signals http-protocol-error
      (write-websocket-message
       stream (ascii "too long for one fixed key")
       :max-frame-payload-bytes 2
       :mask-p t
       :masking-key (octets 1 2 3 4)))))

#+sbcl
(deftest websocket-control-frame-writers
  (let ((stream (make-instance 'binary-test-stream :input (octets))))
    (websocket-ping stream
                    :payload "ping"
                    :mask-p t
                    :masking-key (octets 4 3 2 1))
    (websocket-pong stream :payload (octets 1 2 3))
    (websocket-close stream :code 1000 :reason "bye")
    (let ((wire (binary-test-output stream)))
      (multiple-value-bind (ping ping-consumed)
          (parse-websocket-frame wire :require-mask-p t)
        (multiple-value-bind (pong pong-consumed)
            (parse-websocket-frame (subseq wire ping-consumed))
          (multiple-value-bind (close close-consumed)
              (parse-websocket-frame
               (subseq wire (+ ping-consumed pong-consumed)))
            (ensure-equal 9 (websocket-frame-opcode ping))
            (ensure-equal (ascii "ping") (websocket-frame-payload ping))
            (ensure-equal 10 (websocket-frame-opcode pong))
            (ensure-equal (octets 1 2 3) (websocket-frame-payload pong))
            (ensure-equal 8 (websocket-frame-opcode close))
            (multiple-value-bind (code reason)
                (parse-websocket-close-payload
                 (websocket-frame-payload close))
              (ensure-equal 1000 code)
              (ensure-equal "bye" reason))
            (ensure-equal (length wire)
                          (+ ping-consumed pong-consumed close-consumed))))))
  (let ((stream (make-instance 'binary-test-stream :input (octets))))
    (signals http-protocol-error
      (websocket-close stream :payload (octets 0) :code 1000)))))

#+sbcl
(deftest websocket-server-session-ping-close-and-masking
  (let* ((close-payload
           (make-websocket-close-payload :code 1000 :reason "bye"))
         (input
           (concatenate-octets
            (serialize-websocket-frame
             (make-websocket-frame
              :opcode 9
              :mask-p t
              :masking-key (octets 1 2 3 4)
              :payload (ascii "ping")))
            (serialize-websocket-frame
             (make-websocket-frame
              :opcode 1
              :mask-p t
              :masking-key (octets 5 6 7 8)
              :payload (ascii "hello")))
            (serialize-websocket-frame
             (make-websocket-frame
              :opcode 8
              :mask-p t
              :masking-key (octets 9 10 11 12)
              :payload close-payload))))
         (stream (make-instance 'binary-test-stream :input input))
         (messages nil)
         (control-opcodes nil))
    (multiple-value-bind (count termination)
        (serve-websocket-session
         stream
         (lambda (received-stream payload opcode)
           (declare (ignore received-stream))
           (push (list payload opcode) messages))
         :close-stream nil
         :on-control
         (lambda (frame)
           (push (websocket-frame-opcode frame) control-opcodes)))
      (ensure-equal 1 count)
      (ensure-equal :peer-close termination))
    (ensure-equal (list (list (ascii "hello") 1)) (nreverse messages))
    (ensure-equal '(9 8) (nreverse control-opcodes))
    (let ((wire (binary-test-output stream)))
      (multiple-value-bind (pong pong-consumed)
          (parse-websocket-frame wire)
        (multiple-value-bind (close close-consumed)
            (parse-websocket-frame (subseq wire pong-consumed))
          (ensure-equal 10 (websocket-frame-opcode pong))
          (ensure-equal (ascii "ping") (websocket-frame-payload pong))
          (ensure-equal 8 (websocket-frame-opcode close))
          (ensure-equal close-payload (websocket-frame-payload close))
          (multiple-value-bind (code reason)
              (parse-websocket-close-payload
               (websocket-frame-payload close))
            (ensure-equal 1000 code)
            (ensure-equal "bye" reason))
          (ensure-equal (length wire) (+ pong-consumed close-consumed)))))))

#+sbcl
(deftest websocket-server-session-requires-masked-client-frames
  (let* ((input
           (serialize-websocket-frame
            (make-websocket-frame :opcode 1 :payload (ascii "bad"))))
         (stream (make-instance 'binary-test-stream :input input))
         (condition nil))
    (signals http-protocol-error
      (serve-websocket-session
       stream
       (lambda (received-stream payload opcode)
         (declare (ignore received-stream payload opcode)))
       :close-stream nil
       :on-error (lambda (seen-condition)
                   (setf condition seen-condition))))
    (ensure-true (typep condition 'http-protocol-error))
    (let ((wire (binary-test-output stream)))
      (multiple-value-bind (close consumed)
          (parse-websocket-frame wire)
        (ensure-equal 8 (websocket-frame-opcode close))
        (multiple-value-bind (code reason)
            (parse-websocket-close-payload
             (websocket-frame-payload close))
          (ensure-equal 1002 code)
          (ensure-equal "WebSocket session error" reason))
        (ensure-equal (length wire) consumed)))))

(deftest client-sse-parse-and-serialize
  (let* ((linefeed (string #\Linefeed))
         (input
           (concatenate-octets
            (octets #xef #xbb #xbf)
            (ascii
             "event: update|CRLF|data: hello|CRLF|data: world|CRLF|id: 7|CRLF|")
            (ascii "retry: 1500|CRLF||CRLF|:keepalive")
            (octets #x0a)
            (ascii "data: final")))
         (events (parse-http-sse-events input)))
    (ensure-equal 2 (length events))
    (let ((first (first events))
          (second (second events)))
      (ensure-equal "update" (http-sse-event-event first))
      (ensure-equal (concatenate 'string "hello" linefeed "world")
                    (http-sse-event-data first))
      (ensure-equal "7" (http-sse-event-id first))
      (ensure-equal 1500 (http-sse-event-retry first))
      (ensure-equal "message" (http-sse-event-event second))
      (ensure-equal "final" (http-sse-event-data second))
      (ensure-equal '("keepalive") (http-sse-event-comments second)))
    (let* ((event
             (make-http-sse-event
              :event "notice"
              :data (concatenate 'string "a" linefeed "b")
              :id "9"
              :retry 10
              :comments '("c" "d")))
           (wire (serialize-http-sse-event event)))
      (ensure-equal
       (ascii
        (concatenate
         'string
         ":c|CRLF|:d|CRLF|event:notice|CRLF|id:9|CRLF|retry:10|CRLF|"
         "data:a|CRLF|data:b|CRLF||CRLF|"))
       wire)
      (let ((round-trip (first (parse-http-sse-events wire))))
        (ensure-equal "notice" (http-sse-event-event round-trip))
        (ensure-equal (concatenate 'string "a" linefeed "b")
                      (http-sse-event-data round-trip))
        (ensure-equal "9" (http-sse-event-id round-trip))
        (ensure-equal 10 (http-sse-event-retry round-trip))
        (ensure-equal '("c" "d") (http-sse-event-comments round-trip))))))

#+sbcl
(deftest client-sse-stream-callback-and-limits
  (let* ((linefeed (string #\Linefeed))
         (wire
           (concatenate-octets
            (ascii "data:one|CRLF|data:two")
            (octets #x0a #x0a)))
         (stream (make-instance 'binary-test-stream :input wire))
         (seen nil)
         (events
           (read-http-sse-events
            stream
            :on-event (lambda (event)
                        (push (http-sse-event-data event) seen)))))
    (ensure-equal (list (concatenate 'string "one" linefeed "two"))
                  (mapcar #'http-sse-event-data events))
    (ensure-equal (list (concatenate 'string "one" linefeed "two"))
                  (nreverse seen)))
  (signals http-size-limit-exceeded
    (parse-http-sse-events (concatenate-octets (ascii "data:one")
                                               (octets #x0a #x0a))
                           :max-line-bytes 4))
  (signals http-size-limit-exceeded
    (parse-http-sse-events
     (concatenate-octets
      (ascii "data:one")
      (octets #x0a #x0a)
      (ascii "data:two")
      (octets #x0a #x0a))
     :max-events 1))
  (signals http-protocol-error
    (parse-http-sse-events
     (octets #x64 #x61 #x74 #x61 #x3a #xc3 #x28 #x0a #x0a))))
