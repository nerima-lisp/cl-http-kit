(in-package #:http-kit/test-core)

(deftest http1-input-and-line-boundaries
  (let* ((wire (concatenate-octets
                (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|")
                (octets 1 2)))
         (response (parse-http-response (coerce wire 'list))))
    (ensure-equal 200 (http-response-status response) "list input status")
    (ensure-equal (octets 1 2)
                  (http-response-body response)
                  "list input body"))
  (signals http-protocol-error
    (parse-http-response "HTTP/1.1 200 OK"))
  (signals http-protocol-error
    (parse-http-response (make-array '(1 1)
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0)))
  (signals http-protocol-error
    (parse-http-response '(1 :bad)))
  (signals http-protocol-error
    (parse-http-response (ascii "HTTP/1.1 200 OK")))
  (signals http-protocol-error
    (parse-http-response
     (concatenate-octets (ascii "HTTP/1.1 200 OK") (octets 10))))
  (signals http-protocol-error
    (parse-http-response
     (concatenate-octets (ascii "HTTP/1.1 200 OK") (octets 13) (ascii "X"))))
  (signals http-protocol-error
    (parse-http-response
     (concatenate-octets (ascii "HTTP/1.1 200 OK") (octets 1))))
  (signals http-invalid-status
    (parse-http-response (ascii "HTTP/1.1 600 Error|CRLF||CRLF|")))
  (let ((response
          (parse-http-response (ascii "HTTP/1.0 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|"))))
    (ensure-equal "HTTP/1.0"
                  (http-response-protocol-version response)
                  "HTTP/1.0 response version")
    (ensure-equal 200 (http-response-status response)
                  "HTTP/1.0 response status"))
  (signals http-invalid-status
    (parse-http-response (ascii "HTTP/1.1X200 OK|CRLF||CRLF|")))
  (let ((response
          (parse-http-response
           (ascii "HTTP/1.1 200 |CRLF||CRLF|"))))
    (ensure-equal "" (http-response-reason response))))

(deftest http1-body-and-trailer-boundaries
  (let ((response
          (parse-http-response
           (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1 ; foo = \"bar\\\"baz\";flag|CRLF|a|CRLF|0|CRLF||CRLF|"))))
    (ensure-equal "a" (octets-as-string (http-response-body response))
                  "chunk extensions are ignored"))
  (dolist (chunk-line '("1;" "1;=value" "1;name=" "1;name=\"unterminated"
                        "1;name=bad value" "1 " "1;na(me=value"))
    (signals http-protocol-error
      (parse-http-response
       (ascii (format nil
                      "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|~A|CRLF|a|CRLF|0|CRLF||CRLF|"
                      chunk-line)))))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 1|CRLF||CRLF|")))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|Z|CRLF|0|CRLF||CRLF|")))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF||CRLF|")))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|2|CRLF|a")))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1|CRLF|a")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|0|CRLF|Broken|CRLF||CRLF|")))
  (dolist (name '("Authorization" "If-None-Match" "Content-Type"
                  "Cache-Control" "Set-Cookie"))
    (signals http-invalid-header
      (parse-http-response
       (ascii (format nil
                      "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|0|CRLF|~A: forbidden|CRLF||CRLF|"
                      name)))))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|1")))
  (let ((response
          (parse-http-response
           (ascii "HTTP/1.1 101 Switching Protocols|CRLF|Connection: Upgrade|CRLF|Upgrade: websocket|CRLF||CRLF|"))))
    (ensure-equal 101 (http-response-status response)
                  "101 status")
    (ensure-equal "HTTP/1.1"
                  (http-response-protocol-version response)
                  "101 response version")
    (ensure-equal (octets) (http-response-body response)
                  "101 response body"))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 101 Switching Protocols|CRLF|Content-Length: 1|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 101 Switching Protocols|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 101 Switching Protocols|CRLF|Content-Length: 0|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 100 Continue|CRLF|Content-Length: 0|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 103 Early Hints|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 204 No Content|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|0|CRLF||CRLF|")))
  (let ((response
          (parse-http-response
           (ascii "HTTP/1.1 205 Reset Content|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|0|CRLF||CRLF|"))))
    (ensure-equal 205 (http-response-status response))
    (ensure-equal 0 (length (http-response-body response))))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 205 Reset Content|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1|CRLF|a|CRLF|0|CRLF||CRLF|")))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 205 Reset Content|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1|CRLF|a|CRLF|0|CRLF||CRLF|")
     :collect-body-p nil))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 200 Connection Established|CRLF|Content-Length: 0|CRLF||CRLF|")
     :request-method "CONNECT"))
  (let ((response
          (parse-http-response
           (ascii "HTTP/1.1 304 Not Modified|CRLF|Content-Length: 1|CRLF||CRLF|"))))
    (ensure-equal 304 (http-response-status response)
                  "304 may describe the selected representation length")))

(deftest http1-framing-and-header-error-boundaries
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1|CRLF|aX")))
  (signals http-protocol-error
    (parse-http-response
     (concatenate-octets
      (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1|CRLF|a")
      (octets 13)
      (ascii "X"))))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF| Content: folded|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked,,gzip|CRLF||CRLF|")))
  (signals http-unsupported-feature
    (parse-http-response
     (ascii "HTTP/1.0 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: nope|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 1|CRLF|Content-Length: 2|CRLF||CRLF|")))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF||CRLF|")
     :max-body-bytes -1))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF||CRLF|")
     :max-header-bytes "invalid"))
  (let ((response
          (parse-http-response
           (ascii "HTTP/1.1 200 OK|CRLF|X-One: 1|CRLF|X-Two: 2|CRLF||CRLF|")
           :max-fields 2)))
    (ensure-equal 2 (length (http-response-headers response))))
  (let ((response
          (parse-http-response
           (ascii "HTTP/1.1 103 Early Hints|CRLF|Link: </one>|CRLF|Link: </two>|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|X-One: 1|CRLF|X-Two: 2|CRLF||CRLF|")
           :max-fields 2)))
    (ensure-equal 200 (http-response-status response))
    (ensure-equal 2 (length (http-response-headers response))))
  (signals http-size-limit-exceeded
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|X-One: 1|CRLF|X-Two: 2|CRLF||CRLF|")
     :max-fields 1))
  (signals http-size-limit-exceeded
    (parse-http-response
     (ascii "HTTP/1.1 103 Early Hints|CRLF|Link: </one>|CRLF|Link: </two>|CRLF|Link: </three>|CRLF||CRLF|HTTP/1.1 200 OK|CRLF||CRLF|")
     :max-fields 2))
  (signals http-size-limit-exceeded
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|0|CRLF|X-One: 1|CRLF|X-Two: 2|CRLF||CRLF|")
     :max-fields 1))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF||CRLF|")
     :max-fields 0))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF||CRLF|")
     :max-body-bytes "invalid"))
  (signals http-invalid-header
    (http-kit::%parse-response-header-line "")))

(deftest http1-persistence-boundaries
  (let ((request (make-http-request :method "GET" :uri "http://127.0.0.1/")))
    (ensure-true
     (http-response-reusable-p
      request
      (parse-http-response
       (ascii "HTTP/1.0 200 OK|CRLF|Content-Length: 0|CRLF|Connection: keep-alive|CRLF||CRLF|")))
     "HTTP/1.0 keep-alive is reusable")
    (ensure-true
     (not
      (http-response-reusable-p
       request
       (parse-http-response
        (ascii "HTTP/1.0 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|"))))
     "HTTP/1.0 without keep-alive is not reusable")
    (ensure-true
     (http-response-reusable-p
      request
      (parse-http-response
       (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|")))
     "HTTP/1.1 self-delimited response is reusable")
    (ensure-true
     (not
      (http-response-reusable-p
       request
       (parse-http-response
        (ascii "HTTP/1.1 101 Switching Protocols|CRLF||CRLF|"))))
     "101 switches cannot be reused as HTTP responses")))
