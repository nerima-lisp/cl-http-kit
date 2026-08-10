(in-package #:http-kit/test)

(deftest request-serialization
  (let* ((request (make-http-request
                   :method "post"
                   :uri "http://127.0.0.1/submit?x=1"
                   :body (vector 0 #xff)))
         (wire (serialize-http-request request))
         (head (ascii "POST /submit?x=1 HTTP/1.1|CRLF|Host: 127.0.0.1|CRLF|Content-Length: 2|CRLF||CRLF|")))
    (ensure-equal "POST" (http-request-method request) "request method")
    (ensure-equal "127.0.0.1" (http-request-authority request) "request authority")
    (ensure-equal "/submit" (http-request-path request) "request path")
    (ensure-equal "x=1" (http-request-query request) "request query")
    (ensure-equal head (subseq wire 0 (length head)) "serialized request head")
    (ensure-equal (octets 0 #xff) (subseq wire (length head)) "serialized binary body"))
    (signals http-invalid-header
      (serialize-http-request
       (make-http-request :method "GET"
                          :uri "http://127.0.0.1/submit?x=1"
                          :headers (list (cons "Host" "127.0.0.2"))))))

(deftest header-lookup-and-duplicates
  (let ((headers (list (make-http-header "X-Test" "one")
                       (make-http-header "x-test" "two")
                       (make-http-header "X-Empty" "")
                       (make-http-header "Other" "value"))))
    (ensure-equal '("one" "two") (http-header-values headers "X-TEST")
                  "case-insensitive duplicate lookup")
    (ensure-equal "one" (http-header-value headers "x-test") "first header value")
    (ensure-equal "" (http-header-value headers "x-empty" "fallback")
                  "empty header value is not absence")
    (ensure-true (http-header-present-p headers "X-Test") "duplicate header presence"))
  (let* ((input-header (make-http-header "X-Mutable" "original"))
         (input-uri (make-http-uri :authority "127.0.0.1"))
         (request (make-http-request :method "GET"
                                     :uri input-uri
                                     :headers (list input-header)))
         (returned-headers (http-request-headers request))
         (returned-uri (http-request-uri request)))
    (setf (http-header-content input-header) "changed-by-caller"
          (http-header-content (first returned-headers)) "changed-by-reader"
          (http-uri-path returned-uri) "/changed-by-reader")
    (ensure-equal "original"
                  (http-header-value (http-request-headers request) "x-mutable")
                  "request header defensive copies")
    (ensure-equal "/"
                  (http-request-path request)
                  "request URI defensive copy")))

(deftest response-content-length-and-binary-body
  (let* ((special-wire (concatenate-octets
                        (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF|X-Test: one|CRLF|X-Test: two|CRLF||CRLF|")
                        (octets 0 #xff 10)))
         (wire (make-array (length special-wire)
                           :initial-contents special-wire))
         (response (parse-http-response wire)))
    (ensure-equal 200 (http-response-status response) "response status")
    (ensure-equal '("one" "two")
                  (http-header-values (http-response-headers response) "x-test")
                  "response duplicate headers")
    (ensure-equal (octets 0 #xff 10) (http-response-body response)
                  "response binary body")))

(deftest response-chunked-and-trailers
  (let ((response (parse-http-response
                   (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|3;foo=bar|CRLF|abc|CRLF|0|CRLF|X-Trail: done|CRLF||CRLF|"))))
    (ensure-equal "abc" (octets-as-string (http-response-body response))
                  "chunked response body")
    (ensure-equal "done"
                  (http-header-value (http-response-trailers response) "x-trail")
                  "chunked response trailer")))

(deftest response-connection-close-framing
  (let ((response (parse-http-response
                   (concatenate-octets
                    (ascii "HTTP/1.1 200 OK|CRLF|Connection: close|CRLF||CRLF|")
                    (octets 1 0 #xff)))))
    (ensure-equal (octets 1 0 #xff) (http-response-body response)
                  "connection-close response body")))

(deftest interim-response
  (let* ((statuses '())
         (response (parse-http-response
                    (ascii "HTTP/1.1 100 Continue|CRLF||CRLF|HTTP/1.1 204 No Content|CRLF||CRLF|")
                    :on-information
                    (lambda (information)
                      (push (http-response-status information) statuses)))))
    (ensure-equal 204 (http-response-status response) "final status after interim response")
    (ensure-equal 0 (length (http-response-body response)) "interim response body")
    (ensure-equal '(100) (nreverse statuses) "interim response callback")))

(deftest head-and-no-content-framing
  (let ((response
          (parse-http-response
           (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 99|CRLF||CRLF|")
           :request-method "HEAD")))
    (ensure-equal 200 (http-response-status response) "HEAD status")
    (ensure-equal (octets) (http-response-body response)
                  "HEAD response body is empty"))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 204 No Content|CRLF|Content-Length: 1|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 205 Reset Content|CRLF|Content-Length: 1|CRLF||CRLF|"))))

(deftest unsupported-framing-and-malformed-response
  (signals http-unsupported-feature
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: gzip|CRLF||CRLF|")))
  (signals http-unsupported-feature
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked, chunked|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF|Content-Length: 0|CRLF||CRLF|")))
  (signals http-invalid-status
    (parse-http-response (ascii "HTTP/1.1 20 OK|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response (ascii "HTTP/1.1 200 OK|CRLF|Broken|CRLF||CRLF|")))
  (signals http-protocol-error
    (parse-http-response
     (concatenate-octets (ascii "HTTP/1.1 200 OK") (octets 10 10)))))

(deftest header-injection-and-uri-validation
  (signals http-invalid-header
    (make-http-header "X-Test"
                      (concatenate 'string "safe"
                                   (string #\Return)
                                   (string #\Linefeed)
                                   "Injected: yes")))
  (signals http-invalid-header
    (make-http-header
     (concatenate 'string "X-Test"
                  (string #\Return)
                  (string #\Linefeed)
                  "Injected")
     "value"))
  (let ((uri (parse-http-uri "https://127.0.0.1:8443/api/v1?q=one%20two")))
    (ensure-equal "https" (http-uri-scheme uri) "URI scheme")
    (ensure-equal "127.0.0.1:8443" (http-uri-authority uri) "URI authority")
    (ensure-equal "127.0.0.1" (http-uri-host uri) "URI host")
    (ensure-equal 8443 (http-uri-port uri) "URI port")
    (ensure-equal "/api/v1" (http-uri-path uri) "URI path")
    (ensure-equal "q=one%20two" (http-uri-query uri) "URI query"))
  (let ((uri (parse-http-uri "http://127.0.0.1/path?")))
    (ensure-equal "" (http-uri-query uri) "empty URI query")
    (ensure-equal "http://127.0.0.1/path?" (http-uri-string uri)
                  "empty URI query round trip"))
  (signals http-invalid-uri
    (parse-http-uri "http://127.0.0.1/path#fragment"))
  (signals http-invalid-uri
    (parse-http-uri "http://127.0.0.1/%zz"))
  (signals http-invalid-uri
    (make-http-uri :authority "127.0.0.1" :path "/path?query"))
  (let ((uri (parse-http-uri "http://[::1]/")))
    (ensure-equal "[::1]" (http-uri-authority uri) "IPv6 URI authority")
    (ensure-equal "::1" (http-uri-host uri) "IPv6 URI host"))
  (signals http-invalid-uri
    (parse-http-uri "http://[::1]:"))
  (signals http-invalid-uri
    (parse-http-uri "http://127.0.0.1]/"))
  (signals http-invalid-uri
    (parse-http-uri (concatenate 'string "http://127.0.0.1/" (string (code-char #x80))))))

(deftest limits-and-deadline
  (signals http-size-limit-exceeded
    (parse-http-response
     (concatenate-octets
      (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|")
      (octets 1 2 3))
     :max-body-bytes 2))
  (signals http-size-limit-exceeded
    (parse-http-response
     (concatenate-octets
      (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 1|CRLF||CRLF|")
      (octets 1))
     :max-body-bytes 0))
  (signals http-size-limit-exceeded
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|X-Test: value|CRLF||CRLF|")
     :max-header-bytes 10))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF||CRLF|")
     :max-header-bytes 0))
  (signals http-timeout
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF||CRLF|")
     :deadline 10d0
     :clock-function (lambda () 10d0))))

(deftest recording-session-and-public-quick-start
  (let* ((response (make-http-response :protocol-version "HTTP/1.0"
                                       :status 200 :reason "OK"
                                       :headers (list (make-http-header "X-Reply" "yes"))
                                       :trailers (list (make-http-header "X-Trail" "done"))
                                       :body (octets #x80 #xff)))
         (second-response (make-http-response :status 201 :reason "Created"
                                              :body (octets 7)))
         (transport (make-recording-session
                     :responses (list response second-response)))
         (request (make-http-request
                   :method "GET"
                   :uri "http://127.0.0.1/"
                   :headers (list (make-http-header "X-Request" "recorded"))))
         (received (send-recorded-http-request transport request)))
    (ensure-true (recording-session-p transport)
                 "recording transport predicate")
    (ensure-equal 200 (http-response-status received) "recording response status")
    (ensure-equal "HTTP/1.0"
                  (http-response-protocol-version received)
                  "recording response protocol version")
    (ensure-equal (octets #x80 #xff) (http-response-body received)
                  "recording response body")
    (ensure-equal "done"
                  (http-header-value (http-response-trailers received) "x-trail")
                  "recording response trailer")
    (ensure-equal "recorded"
                  (http-header-value
                   (http-request-headers
                    (first (recording-session-requests transport)))
                   "x-request")
                  "recording request header")
    (let ((second-received
            (send-recorded-http-request
             transport
             (make-http-request :method "POST" :uri "http://127.0.0.1/"))))
      (ensure-equal 201 (http-response-status second-received)
                    "recording response queue order"))
    (ensure-equal '("GET" "POST")
                  (mapcar #'http-request-method
                          (recording-session-requests transport))
                  "recording request history order")
    (ensure-equal '(200 201)
                  (mapcar #'http-response-status
                          (recording-session-responses transport))
                  "recording response history order")
    (ensure-equal 2 (length (recording-session-requests transport))
                  "recorded request count")
    (ensure-equal 2 (length (recording-session-responses transport))
                  "recorded response count")
    (signals http-size-limit-exceeded
      (send-recorded-http-request
       (make-recording-session :responses (list response))
       request
       :max-body-bytes 1))))

(deftest recording-session-failure-and-redaction
  (let ((transport (make-recording-session
                    :response-function (lambda (request &key timeout deadline)
                                         (declare (ignore request timeout deadline))
                                         (error "synthetic transport failure")))))
    (signals http-connection-error
      (send-recorded-http-request transport
                        (make-http-request :method "GET"
                                           :uri "http://127.0.0.1/"))))
  (let* ((header (make-http-header "Authorization" "secret-token"))
         (request (make-http-request
                   :method "GET"
                   :uri "http://127.0.0.1/?token=secret-token"
                   :headers (list header))))
    (ensure-true (not (search "secret-token" (princ-to-string header)))
                 "secret must not appear in header debug output")
    (ensure-true (not (search "secret-token" (princ-to-string request)))
                 "secret must not appear in request debug output")
    (ensure-true (not (search "secret-token"
                              (princ-to-string (http-request-uri request))))
                 "secret must not appear in URI debug output")))
