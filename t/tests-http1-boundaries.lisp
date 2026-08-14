(in-package #:http-kit/test-core)

(defun http1-request-for-test (&key headers body trailers)
  (make-http-request :method "POST"
                     :uri "http://127.0.0.1/upload"
                     :headers headers
                     :trailers trailers
                     :body (or body (octets))))

(deftest http1-request-framing-boundaries
  (let* ((body (octets 1 2))
         (wire (serialize-http-request
                (http1-request-for-test
                 :headers (list (make-http-header "Host" "127.0.0.1")
                                (make-http-header "Content-Length" "2")
                                (make-http-header "content-length" "2"))
                 :body body)))
         (text (octets-as-string wire)))
    (ensure-true (search "Host: 127.0.0.1" text)
                 "matching Host is serialized")
    (ensure-true (search "Content-Length: 2" text)
                 "first Content-Length is serialized")
    (ensure-true (search "content-length: 2" text)
                 "duplicate Content-Length is serialized"))
  (ensure-serialization-cases
   serialize-http-request
   ((concatenate-octets
     (ascii "POST /upload HTTP/1.1|CRLF|Transfer-Encoding: chunked|CRLF|Host: 127.0.0.1|CRLF||CRLF|2|CRLF|")
     (octets 1 2)
     (ascii "|CRLF|0|CRLF||CRLF|"))
    (http1-request-for-test
     :headers (list (make-http-header "Transfer-Encoding" "chunked"))
     :body (octets 1 2)))
   ((ascii "POST /upload HTTP/1.1|CRLF|transfer-encoding: CHUNKED|CRLF|Host: 127.0.0.1|CRLF||CRLF|0|CRLF||CRLF|")
    (http1-request-for-test
     :headers (list (make-http-header "transfer-encoding" "CHUNKED"))))
   ((ascii "POST /upload HTTP/1.1|CRLF|Transfer-Encoding: chunked|CRLF|Host: 127.0.0.1|CRLF|Trailer: X-Checksum|CRLF||CRLF|2|CRLF|ab|CRLF|0|CRLF|X-Checksum: ok|CRLF||CRLF|")
    (http1-request-for-test
     :headers (list (make-http-header "Transfer-Encoding" "chunked"))
     :body (octets 97 98)
     :trailers (list (make-http-header "X-Checksum" "ok"))))
   ((ascii "POST /upload HTTP/1.1|CRLF|Transfer-Encoding: chunked|CRLF|Trailer: X-Checksum|CRLF|Host: 127.0.0.1|CRLF||CRLF|2|CRLF|ab|CRLF|0|CRLF|X-Checksum: ok|CRLF||CRLF|")
    (http1-request-for-test
     :headers (list (make-http-header "Transfer-Encoding" "chunked")
                    (make-http-header "Trailer" "X-Checksum"))
     :body (octets 97 98)
     :trailers (list (make-http-header "X-Checksum" "ok"))))
  ((ascii "POST /upload HTTP/1.1|CRLF|Transfer-Encoding: chunked|CRLF|Trailer: X-Checksum, x-checksum|CRLF|Host: 127.0.0.1|CRLF||CRLF|1|CRLF|a|CRLF|0|CRLF|X-Checksum: ok|CRLF||CRLF|")
    (http1-request-for-test
     :headers (list (make-http-header "Transfer-Encoding" "chunked")
                    (make-http-header "Trailer" "X-Checksum, x-checksum"))
     :body (octets 97)
     :trailers (list (make-http-header "X-Checksum" "ok")))))
  (ensure-signals-cases
   http-invalid-header
   (serialize-http-request
    (http1-request-for-test
     :headers (list (make-http-header "Transfer-Encoding" "chunked")
                    (make-http-header "Trailer" "X-Other"))
     :body (octets 97)
     :trailers (list (make-http-header "X-Checksum" "ok"))))
   (serialize-http-request
    (http1-request-for-test
     :headers (list (make-http-header "Trailer" "Content-Length"))
     :body (octets 97)))
   (serialize-http-request
    (http1-request-for-test
     :headers (list (make-http-header "Transfer-Encoding" "chunked")
                    (make-http-header "Content-Length" "2"))
     :body (octets 1 2)))
   (serialize-http-request
    (http1-request-for-test
     :headers (list (make-http-header "Transfer-Encoding" "chunked,"))))
   (serialize-http-request
    (http1-request-for-test
     :headers (list (make-http-header "Content-Length" "not-a-number"))))
   (serialize-http-request
    (http1-request-for-test
     :headers (list (make-http-header "Content-Length" "2")
                    (make-http-header "content-length" "1"))
     :body (octets 1 2)))
   (serialize-http-request
    (http1-request-for-test
     :headers (list (make-http-header "Content-Length" "1"))
     :body (octets 1 2)))
   (serialize-http-request
    (http1-request-for-test
     :headers (list (make-http-header "Host" "127.0.0.1")
                    (make-http-header "host" "127.0.0.1")))))
  (ensure-signals-cases
   http-unsupported-feature
   (serialize-http-request
    (http1-request-for-test
     :headers (list (make-http-header "Transfer-Encoding" "gzip"))))
   (serialize-http-request
    (http1-request-for-test
     :headers (list (make-http-header "Transfer-Encoding" "gzip, chunked"))))))
