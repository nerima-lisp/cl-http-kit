(in-package #:http-kit/test-core)

(deftest http1-session-response-stream-known-length
  (let ((chunks (list (ascii "ab") (ascii "cd") nil)))
    (let ((wire
            (ensure-http1-session-run
             (ascii "GET /stream HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
             (lambda (request)
               (declare (ignore request))
               (make-http-response-stream
                :status 200
                :body-length 4
                :body-function (lambda () (pop chunks))))
             1
             :eof
             :close-stream nil)))
      (ensure-equal
       (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 4|CRLF||CRLF|abcd")
       wire))))

(deftest http1-session-response-stream-chunked-trailers
  (let ((chunks (list (ascii "a") (ascii "bc") nil)))
    (let ((wire
            (ensure-http1-session-run
             (ascii "GET /stream HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
             (lambda (request)
               (declare (ignore request))
               (make-http-response-stream
                :status 200
                :trailers (list (make-http-header "X-Checksum" "ok"))
                :body-function (lambda () (pop chunks))))
             1
             :eof
             :close-stream nil)))
      (ensure-equal
       (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF|Trailer: X-Checksum|CRLF||CRLF|1|CRLF|a|CRLF|2|CRLF|bc|CRLF|0|CRLF|X-Checksum: ok|CRLF||CRLF|")
       wire)
      (let ((parsed (parse-http-response wire)))
        (ensure-equal "abc" (octets-as-string (http-response-body parsed)))
        (ensure-equal "ok"
                      (http-header-value (http-response-trailers parsed)
                                         "x-checksum"))))))

(deftest http1-session-response-stream-head
  (let ((body-called nil))
    (let ((wire
            (ensure-http1-session-run
             (ascii "HEAD /stream HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
             (lambda (request)
               (declare (ignore request))
               (make-http-response-stream
                :status 200
                :body-length 7
                :body-function (lambda ()
                                 (setf body-called t)
                                 (ascii "payload"))))
             1
             :eof
             :close-stream nil)))
      (let ((parsed (parse-http-response wire :request-method "HEAD")))
        (ensure-equal "7"
                      (http-header-value (http-response-headers parsed)
                                         "content-length"))
        (ensure-equal 0 (length (http-response-body parsed))))
      (ensure-true (not body-called))
      (ensure-true (null (search "payload" (octets-as-string wire)))))))

(deftest http1-session-response-stream-head-rejects-trailers
  (signals http-protocol-error
    (%run-http1-session-from-file
     (ascii "HEAD /stream HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
     (lambda (request)
       (declare (ignore request))
       (make-http-response-stream
        :status 200
        :trailers (list (make-http-header "X-Checksum" "ok"))
        :body-function (lambda () nil)))
     :close-stream nil)))

(deftest http1-session-response-stream-http10-close-delimited
  (let ((chunks (list (ascii "abc") nil)))
    (let ((wire
            (ensure-http1-session-run
             (ascii "GET /stream HTTP/1.0|CRLF|Host: example.com|CRLF||CRLF|")
             (lambda (request)
               (declare (ignore request))
               (make-http-response-stream
                :status 200
                :body-function (lambda () (pop chunks))))
             1
             :close
             :close-stream nil)))
      (ensure-equal
       (ascii "HTTP/1.0 200 OK|CRLF||CRLF|abc")
       wire))))
