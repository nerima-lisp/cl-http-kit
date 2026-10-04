(in-package #:http-kit/test-core)

(deftest http1-request-target-boundaries
  (with-http1-request
      (request
       (ascii "POST /upload?x=1 HTTP/1.1|CRLF|Host: Example.COM:80|CRLF|Content-Length: 2|CRLF||CRLF|ab"))
    (ensure-equal "POST" (http-request-method request))
    (ensure-equal "/upload?x=1" (http-request-target request))
    (ensure-equal "example.com" (http-uri-host (http-request-uri request)))
    (ensure-equal 80 (http-uri-port (http-request-uri request)))
    (ensure-equal "/upload" (http-request-path request))
    (ensure-equal "x=1" (http-request-query request))
    (ensure-equal "ab" (octets-as-string (http-request-body request))))
  (with-http1-request
      (request
       (ascii "OPTIONS * HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|"))
    (ensure-equal "*" (http-request-target request))
    (ensure-equal "/" (http-request-path request)))
  (with-http1-request
      (request
       (ascii "CONNECT example.com:443 HTTP/1.1|CRLF|Host: example.com:443|CRLF||CRLF|"))
    (ensure-equal "example.com:443" (http-request-target request))
    (ensure-equal "example.com" (http-uri-host (http-request-uri request)))
    (ensure-equal 443 (http-uri-port (http-request-uri request))))
  (with-http1-request
      (request
       (ascii "GET https://Example.COM/resource HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|"))
    (ensure-equal "https" (http-uri-scheme (http-request-uri request)))
    (ensure-equal "example.com" (http-uri-host (http-request-uri request)))
    (ensure-equal "/resource" (http-request-path request)))
  (with-http1-request
      (request
       (ascii "GET /legacy HTTP/1.0|CRLF||CRLF|")
       :default-authority "legacy.example:80")
    (ensure-equal "legacy.example" (http-uri-host (http-request-uri request)))
    (ensure-equal 80 (http-uri-port (http-request-uri request))))
  (signals http-invalid-header
    (parse-http-request
     (ascii "GET / HTTP/1.1|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-request
     (ascii "GET / HTTP/1.1|CRLF|Host: one.example|CRLF|Host: two.example|CRLF||CRLF|")))
  (signals http-protocol-error
    (parse-http-request
     (ascii "GET * HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|"))))

(deftest http1-request-body-and-trailer-boundaries
  (with-http1-request
      (request
       (ascii "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|4|CRLF|Wiki|CRLF|5;name=value|CRLF|pedia|CRLF|0|CRLF|X-Checksum: ok|CRLF||CRLF|"))
    (ensure-equal "Wikipedia" (octets-as-string (http-request-body request)))
    (ensure-equal "ok"
                  (http-header-value (http-request-trailers request) "x-checksum")))
  (let* ((chunks '())
         (request nil))
    (with-http1-request
        (parsed-request
         (ascii "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Content-Length: 3|CRLF||CRLF|abc")
         :collect-body-p nil
         :on-body-chunk (lambda (chunk)
                          (push (octets-as-string chunk) chunks)))
      (setf request parsed-request))
    (ensure-equal (octets) (http-request-body request))
    (ensure-equal '("abc") (nreverse chunks)))
  (signals http-invalid-header
    (parse-http-request
     (ascii "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1|CRLF|a|CRLF|0|CRLF|Content-Length: 1|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-request
     (ascii "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Content-Length: 1|CRLF|Content-Length: 2|CRLF||CRLF|a")))
  (signals http-unsupported-feature
    (parse-http-request
     (ascii "POST / HTTP/1.0|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|0|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-request
     (ascii "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1|CRLF|a|CRLF|0|CRLF|Content-Length: 1|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-request
     (ascii "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|0|CRLF|Host: forbidden|CRLF||CRLF|"))))

(deftest http1-request-framing-error-boundaries
  (signals http-protocol-error
    (parse-http-request
     (ascii "GET / HTTP/1.1|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|Z|CRLF|0|CRLF||CRLF|")))
  (signals http-protocol-error
    (parse-http-request
     (ascii "GET / HTTP/1.1|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1|CRLF|aX")))
  (signals http-size-limit-exceeded
    (parse-http-request
     (ascii "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Content-Length: 2|CRLF||CRLF|ab")
     :max-body-bytes 1))
  (signals http-size-limit-exceeded
    (parse-http-request
     (ascii (format nil
                    "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Content-Length: ~A|CRLF||CRLF|"
                    (make-string 10000 :initial-element #\9)))
     :max-body-bytes 0))
  (signals http-size-limit-exceeded
    (parse-http-request
     (ascii (format nil
                    "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|~A|CRLF|"
                    (make-string 10000 :initial-element #\f)))
     :max-body-bytes 0))
  (signals http-protocol-error
    (parse-http-request
     (ascii "GET / HTTP/1.1|CRLF| Host: folded|CRLF||CRLF|")))
  (signals http-protocol-error
    (parse-http-request
     (ascii "GET / HTTP/1.1 extra|CRLF|Host: example.com|CRLF||CRLF|"))))

(deftest http1-request-expect-continue-hook
  (let ((observed nil))
    (with-http1-request
        (request
         (ascii "POST /upload HTTP/1.1|CRLF|Host: example.com|CRLF|Expect: 100-continue|CRLF|Content-Length: 3|CRLF||CRLF|abc")
         :on-expect-continue
         (lambda (metadata)
           (setf observed metadata)
           (ensure-equal "POST" (http-request-method metadata))
           (ensure-equal "/upload" (http-request-target metadata))
           (ensure-equal 0 (length (http-request-body metadata)))))
      (ensure-true observed)
      (ensure-equal "abc" (octets-as-string (http-request-body request))))))

(deftest http1-request-unsupported-expectation
  (let ((caught nil))
    (handler-case
        (parse-http-request
         (ascii "POST /upload HTTP/1.1|CRLF|Host: example.com|CRLF|Expect: 102-processing|CRLF|Content-Length: 3|CRLF||CRLF|abc"))
      (http-unsupported-feature (condition)
        (setf caught condition)))
    (ensure-true caught)
    (ensure-equal :http1-expectation
                  (http-unsupported-feature-name caught))))
