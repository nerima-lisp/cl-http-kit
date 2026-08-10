(in-package #:http-kit/test)

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
  (ensure-equal
   (concatenate-octets
    (ascii "POST /upload HTTP/1.1|CRLF|Transfer-Encoding: chunked|CRLF|Host: 127.0.0.1|CRLF||CRLF|2|CRLF|")
    (octets 1 2)
    (ascii "|CRLF|0|CRLF||CRLF|"))
   (serialize-http-request
    (http1-request-for-test
     :headers (list (make-http-header "Transfer-Encoding" "chunked"))
     :body (octets 1 2))))
  (ensure-equal
   (ascii "POST /upload HTTP/1.1|CRLF|transfer-encoding: CHUNKED|CRLF|Host: 127.0.0.1|CRLF||CRLF|0|CRLF||CRLF|")
   (serialize-http-request
    (http1-request-for-test
     :headers (list (make-http-header "transfer-encoding" "CHUNKED")))))
  (ensure-equal
   (ascii "POST /upload HTTP/1.1|CRLF|Transfer-Encoding: chunked|CRLF|Host: 127.0.0.1|CRLF|Trailer: X-Checksum|CRLF||CRLF|2|CRLF|ab|CRLF|0|CRLF|X-Checksum: ok|CRLF||CRLF|")
   (serialize-http-request
    (http1-request-for-test
     :headers (list (make-http-header "Transfer-Encoding" "chunked"))
     :body (octets 97 98)
     :trailers (list (make-http-header "X-Checksum" "ok")))))
  (ensure-equal
   (ascii "POST /upload HTTP/1.1|CRLF|Transfer-Encoding: chunked|CRLF|Trailer: X-Checksum|CRLF|Host: 127.0.0.1|CRLF||CRLF|2|CRLF|ab|CRLF|0|CRLF|X-Checksum: ok|CRLF||CRLF|")
   (serialize-http-request
    (http1-request-for-test
     :headers (list (make-http-header "Transfer-Encoding" "chunked")
                    (make-http-header "Trailer" "X-Checksum"))
     :body (octets 97 98)
     :trailers (list (make-http-header "X-Checksum" "ok")))))
  (signals http-invalid-header
    (serialize-http-request
     (http1-request-for-test
      :headers (list (make-http-header "Transfer-Encoding" "chunked")
                     (make-http-header "Trailer" "X-Other"))
      :body (octets 97)
      :trailers (list (make-http-header "X-Checksum" "ok")))))
  (signals http-invalid-header
    (serialize-http-request
     (http1-request-for-test
      :headers (list (make-http-header "Trailer" "Content-Length"))
      :body (octets 97))))
  (signals http-unsupported-feature
    (serialize-http-request
     (http1-request-for-test
      :headers (list (make-http-header "Transfer-Encoding" "gzip")))))
  (signals http-unsupported-feature
    (serialize-http-request
     (http1-request-for-test
      :headers (list (make-http-header "Transfer-Encoding" "gzip, chunked")))))
  (signals http-invalid-header
    (serialize-http-request
     (http1-request-for-test
      :headers (list (make-http-header "Transfer-Encoding" "chunked")
                     (make-http-header "Content-Length" "2"))
      :body (octets 1 2))))
  (signals http-invalid-header
    (serialize-http-request
     (http1-request-for-test
      :headers (list (make-http-header "Transfer-Encoding" "chunked,")))))
  (signals http-invalid-header
    (serialize-http-request
     (http1-request-for-test
      :headers (list (make-http-header "Content-Length" "not-a-number")))))
  (signals http-invalid-header
    (serialize-http-request
     (http1-request-for-test
      :headers (list (make-http-header "Content-Length" "2")
                     (make-http-header "content-length" "1"))
      :body (octets 1 2))))
  (signals http-invalid-header
    (serialize-http-request
      (http1-request-for-test
       :headers (list (make-http-header "Content-Length" "1"))
      :body (octets 1 2))))
  (signals http-invalid-header
    (serialize-http-request
     (http1-request-for-test
      :headers (list (make-http-header "Host" "127.0.0.1")
                     (make-http-header "host" "127.0.0.1"))))))
