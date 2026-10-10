(in-package #:http-kit/test-core)

(deftest core-coverage-http1-response-framing-error-metadata
  (dolist (case '((200 (("Content-Length" "bad")) "a" "content-length" :value)
                  (200 (("Content-Length" "1") ("Content-Length" "2"))
                   "a" "content-length" :duplicate)
                  (200 (("Content-Length" "2")) "a" "content-length" :mismatch)
                  (205 (("Content-Length" "1")) "" "content-length" :forbidden)
                  (200 (("Transfer-Encoding" "")) "a" "transfer-encoding" :empty-item)
                  (200 (("Transfer-Encoding" "chunked,"))
                   "a" "transfer-encoding" :empty-item)))
    (destructuring-bind (status headers body name reason) case
      (let ((condition
              (handler-case
                  (serialize-http-response
                   (make-http-response
                    :status status :body (ascii body)
                    :headers (mapcar (lambda (pair) (apply #'make-http-header pair)) headers)))
                (http-invalid-header (condition) condition))))
        (ensure-true (typep condition 'http-invalid-header))
        (ensure-equal :serialization (http-error-operation condition))
        (ensure-equal name (http-invalid-header-name condition))
        (ensure-equal reason (http-invalid-header-reason condition))
        (ensure-equal nil (http-protocol-error-detail condition))))))

(deftest core-coverage-http1-response-unsupported-metadata
  (dolist (case '(("HTTP/2" nil :http1-response-version "HTTP/2")
                  ("HTTP/1.1" "gzip, chunked" :http1-response-transfer-encoding
                   ("gzip" "chunked"))
                  ("HTTP/1.1" "chunked, chunked" :http1-response-transfer-encoding
                   ("chunked" "chunked"))))
    (destructuring-bind (version encoding feature detail) case
      (let ((condition
              (handler-case
                  (serialize-http-response
                   (make-http-response
                    :status 200 :protocol-version version
                    :headers (when encoding (list (make-http-header "Transfer-Encoding" encoding)))))
                (http-unsupported-feature (condition) condition))))
        (ensure-true (typep condition 'http-unsupported-feature))
        (ensure-equal :serialization (http-error-operation condition))
        (ensure-equal feature (http-unsupported-feature-name condition))
        (ensure-equal detail (http-protocol-error-detail condition))))))

(deftest core-coverage-http1-response-option-and-trailer-errors
  (dolist (option '(:head-p :request-method))
    (let ((condition
            (handler-case
                (apply #'serialize-http-response (make-http-response :status 200)
                       (list option 7))
              (http-protocol-error (condition) condition))))
      (ensure-true (typep condition 'http-protocol-error))
      (ensure-equal :serialization (http-error-operation condition))
      (ensure-equal 7 (http-protocol-error-detail condition))))
  (dolist (case '((200 "HEAD") (204 nil) (200 "CONNECT")))
    (destructuring-bind (status method) case
      (let ((condition
              (handler-case
                  (serialize-http-response
                   (make-http-response :status status
                                       :headers (list (make-http-header "Trailer" "X-Checksum")))
                   :request-method method)
                (http-protocol-error (condition) condition))))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :serialization (http-error-operation condition))
        (ensure-equal status (http-protocol-error-detail condition))))))

(deftest http1-response-serialization-boundaries
  (ensure-serialization-cases
   serialize-http-response
   ((ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|hi")
    (make-http-response :status 200 :body (ascii "hi")))
   ((ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|")
    (make-http-response :status 200 :body (ascii "hi"))
    :request-method "HEAD")
   ((ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF|Trailer: X-Checksum|CRLF||CRLF|3|CRLF|abc|CRLF|0|CRLF|X-Checksum: ok|CRLF||CRLF|")
    (make-http-response
     :status 200
     :body (ascii "abc")
     :trailers (list (make-http-header "X-Checksum" "ok"))))
   ((ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF|Trailer: X-Checksum|CRLF||CRLF|3|CRLF|abc|CRLF|0|CRLF|X-Checksum: ok|CRLF||CRLF|")
    (make-http-response
     :status 200
     :headers (list (make-http-header "Transfer-Encoding" "chunked")
                    (make-http-header "Trailer" "X-Checksum"))
     :body (ascii "abc")
     :trailers (list (make-http-header "X-Checksum" "ok"))))
  ((ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF|Trailer: X-Checksum, x-checksum|CRLF||CRLF|1|CRLF|a|CRLF|0|CRLF|X-Checksum: ok|CRLF||CRLF|")
    (make-http-response
     :status 200
     :headers (list (make-http-header "Transfer-Encoding" "chunked")
                    (make-http-header "Trailer" "X-Checksum, x-checksum"))
     :body (ascii "a")
     :trailers (list (make-http-header "X-Checksum" "ok"))))
   ((ascii "HTTP/1.1 204 No Content|CRLF||CRLF|")
    (make-http-response :status 204))
   ((ascii "HTTP/1.1 205 Reset Content|CRLF|Content-Length: 0|CRLF||CRLF|")
    (make-http-response :status 205 :reason "Reset Content"))
   ((ascii "HTTP/1.1 304 Not Modified|CRLF|Content-Length: 4|CRLF||CRLF|")
    (make-http-response
     :status 304
     :headers (list (make-http-header "Content-Length" "4")))))
  (let ((response
          (make-http-response
           :status 200
           :body (ascii "abc")
           :trailers (list (make-http-header "X-Checksum" "ok")))))
    (with-http1-response-roundtrip (response parsed)
      (ensure-equal "abc" (octets-as-string (http-response-body parsed)))
      (ensure-equal "ok"
                    (http-header-value (http-response-trailers parsed) "x-checksum")))))

(deftest http1-response-serialization-error-boundaries
  (ensure-signals-cases
   http-invalid-header
   (serialize-http-response
    (make-http-response
     :status 200
     :headers (list (make-http-header "Transfer-Encoding" "chunked")
                    (make-http-header "Content-Length" "1"))
     :body (ascii "a")))
   (serialize-http-response
    (make-http-response
     :status 200
     :trailers (list (make-http-header "Content-Length" "1"))))
   (serialize-http-response
    (make-http-response
     :status 200
     :trailers (list (make-http-header "Host" "forbidden"))))
   (serialize-http-response
    (make-http-response
     :status 200
     :headers (list (make-http-header "Transfer-Encoding" "chunked")
                    (make-http-header "Trailer" "X-Other"))
     :body (ascii "a")
     :trailers (list (make-http-header "X-Checksum" "ok"))))
   (serialize-http-response
    (make-http-response
     :status 200
     :headers (list (make-http-header "Trailer" "Content-Length"))
     :body (ascii "a")))
   (serialize-http-response
    (make-http-response
     :status 200
     :headers (list (make-http-header "Content-Length" "1")
                    (make-http-header "Trailer" "X-Checksum"))
     :body (ascii "a"))))
  (ensure-signals-cases
   http-protocol-error
   (serialize-http-response
    (make-http-response :status 204 :body (ascii "a")))
   (serialize-http-response
    (make-http-response :status 200 :body (ascii "a"))
    :request-method "CONNECT"))
  (ensure-signals-cases
   http-unsupported-feature
   (serialize-http-response
    (make-http-response
     :protocol-version "HTTP/1.0"
     :status 200
     :trailers (list (make-http-header "X-Trailer" "value"))))))
