(in-package #:http-kit/test)

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
           (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1;foo=bar|CRLF|a|CRLF|0|CRLF||CRLF|"))))
    (ensure-equal "a" (octets-as-string (http-response-body response))
                  "chunk extensions are ignored"))
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
     (ascii "HTTP/1.1 304 Not Modified|CRLF|Content-Length: 1|CRLF||CRLF|"))))

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
