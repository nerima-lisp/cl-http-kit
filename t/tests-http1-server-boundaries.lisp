(in-package #:http-kit/test)

(defun http1-request-wire-for-test (request-line &rest fields)
  (apply #'concatenate-octets
         (ascii request-line)
         (mapcar #'ascii fields)))

(deftest http1-request-target-boundaries
  (let ((request
          (parse-http-request
           (ascii "POST /upload?x=1 HTTP/1.1|CRLF|Host: Example.COM:80|CRLF|Content-Length: 2|CRLF||CRLF|ab"))))
    (ensure-equal "POST" (http-request-method request))
    (ensure-equal "/upload?x=1" (http-request-target request))
    (ensure-equal "example.com" (http-uri-host (http-request-uri request)))
    (ensure-equal 80 (http-uri-port (http-request-uri request)))
    (ensure-equal "/upload" (http-request-path request))
    (ensure-equal "x=1" (http-request-query request))
    (ensure-equal "ab" (octets-as-string (http-request-body request))))
  (let ((request
          (parse-http-request
           (ascii "OPTIONS * HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|"))))
    (ensure-equal "*" (http-request-target request))
    (ensure-equal "/" (http-request-path request)))
  (let ((request
          (parse-http-request
           (ascii "CONNECT example.com:443 HTTP/1.1|CRLF|Host: example.com:443|CRLF||CRLF|"))))
    (ensure-equal "example.com:443" (http-request-target request))
    (ensure-equal "example.com" (http-uri-host (http-request-uri request)))
    (ensure-equal 443 (http-uri-port (http-request-uri request))))
  (let ((request
          (parse-http-request
           (ascii "GET https://Example.COM/resource HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|"))))
    (ensure-equal "https" (http-uri-scheme (http-request-uri request)))
    (ensure-equal "example.com" (http-uri-host (http-request-uri request)))
    (ensure-equal "/resource" (http-request-path request)))
  (let ((request
          (parse-http-request
           (ascii "GET /legacy HTTP/1.0|CRLF||CRLF|")
           :default-authority "legacy.example:80")))
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
  (let ((request
          (parse-http-request
           (ascii "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|4|CRLF|Wiki|CRLF|5;name=value|CRLF|pedia|CRLF|0|CRLF|X-Checksum: ok|CRLF||CRLF|"))))
    (ensure-equal "Wikipedia" (octets-as-string (http-request-body request)))
    (ensure-equal "ok"
                  (http-header-value (http-request-trailers request) "x-checksum")))
  (let* ((chunks '())
        (request
          (parse-http-request
           (ascii "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Content-Length: 3|CRLF||CRLF|abc")
           :collect-body-p nil
           :on-body-chunk (lambda (chunk)
                            (push (octets-as-string chunk) chunks)))))
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
  (signals http-protocol-error
    (parse-http-request
     (ascii "GET / HTTP/1.1|CRLF| Host: folded|CRLF||CRLF|")))
  (signals http-protocol-error
    (parse-http-request
     (ascii "GET / HTTP/1.1 extra|CRLF|Host: example.com|CRLF||CRLF|"))))

(deftest http1-response-serialization-boundaries
  (let ((response
          (make-http-response :status 200 :body (ascii "hi"))))
    (ensure-equal
     (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|hi")
     (serialize-http-response response)))
  (let ((response
          (make-http-response :status 200 :body (ascii "hi"))))
    (ensure-equal
     (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|")
     (serialize-http-response response :request-method "HEAD")))
  (let ((response
          (make-http-response
           :status 200
           :body (ascii "abc")
           :trailers (list (make-http-header "X-Checksum" "ok")))))
    (ensure-equal
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF|Trailer: X-Checksum|CRLF||CRLF|3|CRLF|abc|CRLF|0|CRLF|X-Checksum: ok|CRLF||CRLF|")
     (serialize-http-response response)))
  (let ((response
          (make-http-response
           :status 200
           :headers (list (make-http-header "Transfer-Encoding" "chunked")
                          (make-http-header "Trailer" "X-Checksum"))
           :body (ascii "abc")
           :trailers (list (make-http-header "X-Checksum" "ok")))))
    (ensure-equal
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF|Trailer: X-Checksum|CRLF||CRLF|3|CRLF|abc|CRLF|0|CRLF|X-Checksum: ok|CRLF||CRLF|")
     (serialize-http-response response)))
  (let* ((response
           (make-http-response
            :status 200
            :body (ascii "abc")
            :trailers (list (make-http-header "X-Checksum" "ok"))))
         (parsed (parse-http-response (serialize-http-response response))))
    (ensure-equal "abc" (octets-as-string (http-response-body parsed)))
    (ensure-equal "ok"
                  (http-header-value (http-response-trailers parsed) "x-checksum")))
  (ensure-equal
   (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|")
   (serialize-http-response (make-http-response :status 204)))
  (ensure-equal
   (ascii "HTTP/1.1 205 Reset Content|CRLF|Content-Length: 0|CRLF||CRLF|")
   (serialize-http-response
    (make-http-response :status 205 :reason "Reset Content")))
  (ensure-equal
   (ascii "HTTP/1.1 304 Not Modified|CRLF|Content-Length: 4|CRLF||CRLF|")
   (serialize-http-response
    (make-http-response
     :status 304
     :headers (list (make-http-header "Content-Length" "4"))))))

(deftest http1-response-serialization-error-boundaries
  (signals http-invalid-header
    (serialize-http-response
     (make-http-response
      :status 200
      :headers (list (make-http-header "Transfer-Encoding" "chunked")
                     (make-http-header "Content-Length" "1"))
      :body (ascii "a"))))
  (signals http-protocol-error
    (serialize-http-response
     (make-http-response :status 204 :body (ascii "a"))))
  (signals http-invalid-header
    (serialize-http-response
     (make-http-response
      :status 200
      :trailers (list (make-http-header "Content-Length" "1")))))
  (signals http-invalid-header
    (serialize-http-response
     (make-http-response
      :status 200
      :trailers (list (make-http-header "Host" "forbidden")))))
  (signals http-invalid-header
    (serialize-http-response
     (make-http-response
      :status 200
      :headers (list (make-http-header "Transfer-Encoding" "chunked")
                     (make-http-header "Trailer" "X-Other"))
      :body (ascii "a")
      :trailers (list (make-http-header "X-Checksum" "ok")))))
  (signals http-invalid-header
    (serialize-http-response
     (make-http-response
      :status 200
      :headers (list (make-http-header "Trailer" "Content-Length"))
      :body (ascii "a"))))
  (signals http-invalid-header
    (serialize-http-response
     (make-http-response
      :status 200
      :headers (list (make-http-header "Content-Length" "1")
                     (make-http-header "Trailer" "X-Checksum"))
      :body (ascii "a"))))
  (signals http-unsupported-feature
    (serialize-http-response
     (make-http-response
      :protocol-version "HTTP/1.0"
      :status 200
      :trailers (list (make-http-header "X-Trailer" "value")))))
  (signals http-protocol-error
    (serialize-http-response
     (make-http-response :status 200 :body (ascii "a"))
     :request-method "CONNECT")))

(defun %http1-session-output-octets (path)
  (with-open-file (stream path
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((result (make-array 0
                              :element-type '(unsigned-byte 8)
                              :adjustable t
                              :fill-pointer 0)))
      (loop for byte = (read-byte stream nil :eof)
            until (eq byte :eof)
            do (vector-push-extend byte result))
      result)))

(defun %run-http1-session-from-file (request-bytes handler &rest arguments)
  (let* ((base-name (format nil "cl-http-kit-http1-session-~A" (gensym)))
         (input-path (merge-pathnames
                      (make-pathname :name base-name :type "input")
                      (uiop:temporary-directory)))
         (output-path (merge-pathnames
                       (make-pathname :name base-name :type "output")
                       (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (stream input-path
                                   :direction :output
                                   :element-type '(unsigned-byte 8)
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-sequence request-bytes stream))
           (let ((result
                   (with-open-file (input input-path
                                          :direction :input
                                          :element-type '(unsigned-byte 8))
                     (with-open-file (output output-path
                                            :direction :output
                                            :element-type '(unsigned-byte 8)
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
                       (let ((session-stream (make-two-way-stream input output)))
                         (multiple-value-list
                          (apply #'serve-http1-session
                                 session-stream
                                 handler
                                 arguments)))))))
             (values (first result)
                     (second result)
                     (%http1-session-output-octets output-path))))
      (http-kit::%with-http-cleanup (delete-file input-path))
      (http-kit::%with-http-cleanup (delete-file output-path)))))

(deftest http1-session-keep-alive-and-eof
  (let ((seen '()))
    (multiple-value-bind (count reason wire)
        (%run-http1-session-from-file
         (ascii "GET /one HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|GET /two HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
         (lambda (request)
           (push (http-request-target request) seen)
           (make-http-response
            :status 200
            :body (ascii (http-request-target request))))
         :close-stream nil)
      (ensure-equal 2 count)
      (ensure-equal :eof reason)
      (ensure-equal 2 (length seen))
      (let ((wire-string (octets-as-string wire)))
        (ensure-true (search "HTTP/1.1 200 OK" wire-string))
        (ensure-true (search "/one" wire-string))
        (ensure-true (search "/two" wire-string))))))

(deftest http1-session-connection-close
  (let ((seen '()))
    (multiple-value-bind (count reason wire)
        (%run-http1-session-from-file
         (ascii "GET /one HTTP/1.1|CRLF|Host: example.com|CRLF|Connection: close|CRLF||CRLF|GET /two HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
         (lambda (request)
           (push (http-request-target request) seen)
           (make-http-response
            :status 200
            :body (ascii (http-request-target request))))
         :close-stream nil)
      (ensure-equal 1 count)
      (ensure-equal :close reason)
      (ensure-equal 1 (length seen))
      (let ((wire-string (octets-as-string wire)))
        (ensure-true (search "/one" wire-string))
        (ensure-true (null (search "/two" wire-string)))))))

(deftest http1-session-http10-keep-alive
  (let ((seen '()))
    (multiple-value-bind (count reason wire)
        (%run-http1-session-from-file
         (ascii "GET /one HTTP/1.0|CRLF|Host: example.com|CRLF|Connection: keep-alive|CRLF||CRLF|GET /two HTTP/1.0|CRLF|Host: example.com|CRLF|Connection: keep-alive|CRLF||CRLF|")
         (lambda (request)
           (push (http-request-target request) seen)
           (make-http-response
            :status 200
            :headers (list (make-http-header "Connection" "keep-alive"))
            :body (ascii (http-request-target request))))
         :close-stream nil)
      (ensure-equal 2 count)
      (ensure-equal :eof reason)
      (ensure-equal 2 (length seen))
      (ensure-true
       (search "HTTP/1.0 200 OK" (octets-as-string wire))))))

(deftest http1-session-max-requests-and-head
  (multiple-value-bind (count reason wire)
      (%run-http1-session-from-file
       (ascii "GET /one HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|GET /two HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
       (lambda (request)
         (make-http-response
          :status 200
          :body (ascii (http-request-target request))))
       :max-requests 1
       :close-stream nil)
    (ensure-equal 1 count)
    (ensure-equal :max-requests reason)
    (ensure-true (null (search "/two" (octets-as-string wire)))))
  (multiple-value-bind (count reason wire)
      (%run-http1-session-from-file
       (ascii "HEAD /resource HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
       (lambda (request)
         (declare (ignore request))
         (make-http-response :status 200 :body (ascii "payload")))
       :close-stream nil)
    (ensure-equal 1 count)
    (ensure-equal :eof reason)
    (let ((parsed (parse-http-response wire :request-method "HEAD")))
      (ensure-equal "7"
                    (http-header-value (http-response-headers parsed)
                                       "content-length"))
      (ensure-equal 0 (length (http-response-body parsed))))
    (ensure-true (null (search "payload" (octets-as-string wire))))))

(deftest http1-session-handler-error-and-close-hook
  (let ((observed-condition nil)
        (observed-request nil)
        (closed nil))
    (signals http-protocol-error
      (%run-http1-session-from-file
       (ascii "GET /failure HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
       (lambda (request)
         (declare (ignore request))
         (error 'http-protocol-error
                :message "handler failure"
                :operation :test
                :detail :session))
       :on-error (lambda (condition request)
                   (setf observed-condition condition
                         observed-request request))
       :close-stream (lambda (stream)
                       (declare (ignore stream))
                       (setf closed t))))
    (ensure-true (typep observed-condition 'http-protocol-error))
    (ensure-true observed-request)
    (ensure-true closed)))

(deftest http1-session-upgrade-handoff
  (let ((observed-stream nil)
        (observed-request nil)
        (observed-response nil)
        (closed nil))
    (multiple-value-bind (count reason wire)
        (%run-http1-session-from-file
         (ascii "GET /chat HTTP/1.1|CRLF|Host: example.com|CRLF|Connection: Upgrade|CRLF|Upgrade: websocket|CRLF||CRLF|")
         (lambda (request)
           (declare (ignore request))
           (make-http-response
            :status 101
            :headers (list (make-http-header "Connection" "Upgrade")
                           (make-http-header "Upgrade" "websocket"))))
         :on-upgrade (lambda (stream request response)
                       (setf observed-stream stream
                             observed-request request
                             observed-response response)
                       t)
         :close-stream (lambda (stream)
                         (declare (ignore stream))
                         (setf closed t)))
      (ensure-equal 1 count)
      (ensure-equal :upgrade reason)
      (ensure-true observed-stream)
      (ensure-equal "/chat" (http-request-target observed-request))
      (ensure-equal 101 (http-response-status observed-response))
      (ensure-true (not closed))
      (ensure-true (search "HTTP/1.1 101 Switching Protocols"
                           (octets-as-string wire))))))

(deftest http1-session-response-stream-known-length
  (let ((chunks (list (ascii "ab") (ascii "cd") nil)))
    (multiple-value-bind (count reason wire)
        (%run-http1-session-from-file
         (ascii "GET /stream HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
         (lambda (request)
           (declare (ignore request))
           (make-http-response-stream
            :status 200
            :body-length 4
            :body-function (lambda () (pop chunks))))
         :close-stream nil)
      (ensure-equal 1 count)
      (ensure-equal :eof reason)
      (ensure-equal
       (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 4|CRLF||CRLF|abcd")
       wire))))

(deftest http1-session-response-stream-chunked-trailers
  (let ((chunks (list (ascii "a") (ascii "bc") nil)))
    (multiple-value-bind (count reason wire)
        (%run-http1-session-from-file
         (ascii "GET /stream HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
         (lambda (request)
           (declare (ignore request))
           (make-http-response-stream
            :status 200
            :trailers (list (make-http-header "X-Checksum" "ok"))
            :body-function (lambda () (pop chunks))))
         :close-stream nil)
      (ensure-equal 1 count)
      (ensure-equal :eof reason)
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
    (multiple-value-bind (count reason wire)
        (%run-http1-session-from-file
         (ascii "HEAD /stream HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
         (lambda (request)
           (declare (ignore request))
           (make-http-response-stream
            :status 200
            :body-length 7
            :body-function (lambda ()
                             (setf body-called t)
                             (ascii "payload"))))
         :close-stream nil)
      (ensure-equal 1 count)
      (ensure-equal :eof reason)
      (let ((parsed (parse-http-response wire :request-method "HEAD")))
        (ensure-equal "7"
                      (http-header-value (http-response-headers parsed)
                                         "content-length"))
        (ensure-equal 0 (length (http-response-body parsed))))
      (ensure-true (not body-called))
      (ensure-true (null (search "payload" (octets-as-string wire)))))))

(deftest http1-session-response-stream-http10-close-delimited
  (let ((chunks (list (ascii "abc") nil)))
    (multiple-value-bind (count reason wire)
        (%run-http1-session-from-file
         (ascii "GET /stream HTTP/1.0|CRLF|Host: example.com|CRLF||CRLF|")
         (lambda (request)
           (declare (ignore request))
           (make-http-response-stream
            :status 200
            :body-function (lambda () (pop chunks))))
         :close-stream nil)
      (ensure-equal 1 count)
      (ensure-equal :close reason)
      (ensure-equal
       (ascii "HTTP/1.0 200 OK|CRLF||CRLF|abc")
       wire))))

(deftest http1-request-expect-continue-hook
  (let ((observed nil))
    (let ((request
            (parse-http-request
             (ascii "POST /upload HTTP/1.1|CRLF|Host: example.com|CRLF|Expect: 100-continue|CRLF|Content-Length: 3|CRLF||CRLF|abc")
             :on-expect-continue
             (lambda (metadata)
               (setf observed metadata)
               (ensure-equal "POST" (http-request-method metadata))
               (ensure-equal "/upload" (http-request-target metadata))
               (ensure-equal 0 (length (http-request-body metadata)))))))
      (ensure-true observed)
      (ensure-equal "abc" (octets-as-string (http-request-body request))))))

(deftest http1-session-expect-continue-automatic
  (let ((observed-body nil))
    (multiple-value-bind (count reason wire)
        (%run-http1-session-from-file
         (ascii "POST /upload HTTP/1.1|CRLF|Host: example.com|CRLF|Expect: 100-continue|CRLF|Content-Length: 3|CRLF||CRLF|abc")
         (lambda (request)
           (setf observed-body (octets-as-string (http-request-body request)))
           (make-http-response :status 204))
         :close-stream nil)
      (ensure-equal 1 count)
      (ensure-equal :eof reason)
      (ensure-equal "abc" observed-body)
      (ensure-equal
       (ascii "HTTP/1.1 100 Continue|CRLF||CRLF|HTTP/1.1 204 No Content|CRLF||CRLF|")
       wire)))
  (multiple-value-bind (count reason wire)
      (%run-http1-session-from-file
       (ascii "POST /upload HTTP/1.1|CRLF|Host: example.com|CRLF|Expect: 100-continue|CRLF|Content-Length: 3|CRLF||CRLF|abc")
       (lambda (request)
         (ensure-equal "abc" (octets-as-string (http-request-body request)))
         (make-http-response :status 204))
       :on-expect-continue nil
       :close-stream nil)
    (ensure-equal 1 count)
    (ensure-equal :eof reason)
    (ensure-equal
     (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|")
     wire)))

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
