(in-package #:http-kit/test)

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
