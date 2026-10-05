(in-package #:http-kit/test-core)

(defun http1-request-for-test (&key headers body trailers)
  (make-http-request :method "POST"
                     :uri "http://127.0.0.1/upload"
                     :headers headers
                     :trailers trailers
                     :body (or body (octets))))

(deftest http1-absolute-request-target-authority-boundaries
  (labels ((request (&key (uri "http://example.com/resource")
                         request-target headers)
             (make-http-request :method "GET"
                                :uri uri
                                :request-target request-target
                                :headers headers))
           (ensure-authority-mismatch (thunk)
             (handler-case
                 (progn
                   (funcall thunk)
                   (ensure-true nil "authority mismatch was accepted"))
               (http-invalid-header (condition)
                 (ensure-equal "host" (http-invalid-header-name condition))
                 (ensure-equal :host-authority-mismatch
                               (http-invalid-header-reason condition))))))
    (ensure-authority-mismatch
     (lambda ()
       (serialize-http-request
        (request :request-target "http://other.example/resource"))))
    (ensure-authority-mismatch
     (lambda ()
       (serialize-http-request
        (request :request-target "http://example.com:81/resource"))))
    (ensure-authority-mismatch
     (lambda ()
       (serialize-http-request
        (request)
        :request-target "http://other.example/resource")))
    (ensure-true
     (search "GET http://example.com/resource HTTP/1.1"
             (octets-as-string
              (serialize-http-request
               (request :request-target "http://example.com/resource"))))
     "matching request-carried absolute target is serialized")
    (ensure-true
     (search "GET https://example.com:443/resource HTTP/1.1"
             (octets-as-string
              (serialize-http-request
               (request :uri "https://example.com/resource"
                        :headers (list (make-http-header "Host"
                                                        "EXAMPLE.COM:443")))
               :request-target "https://example.com:443/resource")))
     "default HTTPS port and normalized host are equivalent")
    (signals http-invalid-uri
      (serialize-http-request
       (request)
       :request-target "http://example.com:bad/resource"))))

(deftest http1-asterisk-request-target-boundaries
  (ensure-true
   (search "OPTIONS * HTTP/1.1"
           (octets-as-string
            (serialize-http-request
             (make-http-request :method "OPTIONS"
                                :uri "http://example.test/"
                                :request-target "*")))))
  (signals http-protocol-error
    (serialize-http-request
     (make-http-request :method "GET"
                        :uri "http://example.test/"
                        :request-target "*"))))

(deftest http1-request-target-form-boundaries
  (ensure-true
   (search "CONNECT example.test:443 HTTP/1.1"
           (octets-as-string
            (serialize-http-request
             (make-http-request :method "CONNECT"
                                :uri "https://example.test:443/"
                                :request-target "example.test:443")))))
  (dolist (target '("/tunnel" "*" "https://example.test:443/"
                    "example.test"))
    (signals http-protocol-error
      (serialize-http-request
       (make-http-request :method "CONNECT"
                          :uri "https://example.test:443/"
                          :request-target target))))
  (signals http-invalid-header
    (serialize-http-request
     (make-http-request :method "CONNECT"
                        :uri "https://example.test:443/"
                        :request-target "other.example:443")))
  (signals http-protocol-error
    (serialize-http-request
     (make-http-request :method "GET"
                        :uri "http://example.test/"
                        :request-target "example.test:80")))
  (signals http-protocol-error
    (serialize-http-request
     (make-http-request :method "GET"
                        :uri "http://example.test/"
                        :request-target "ftp://example.test/resource"))))

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
  (dolist (name '("Authorization" "If-None-Match" "Content-Type"
                  "Cache-Control" "Set-Cookie" "Via"))
    (signals http-invalid-header
      (serialize-http-request
       (http1-request-for-test
        :headers (list (make-http-header "Transfer-Encoding" "chunked"))
        :trailers (list (make-http-header name "forbidden"))))))
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
