(in-package #:http-kit/test-core)

(deftest http1-session-keep-alive-and-eof
  (let ((seen '()))
    (let ((wire
            (ensure-http1-session-run
             (ascii "GET /one HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|GET /two HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
             (lambda (request)
               (push (http-request-target request) seen)
               (make-http-response
                :status 200
                :body (ascii (http-request-target request))))
             2
             :eof
             :close-stream nil)))
      (ensure-equal 2 (length seen))
      (let ((wire-string (octets-as-string wire)))
        (ensure-true (search "HTTP/1.1 200 OK" wire-string))
        (ensure-true (search "/one" wire-string))
        (ensure-true (search "/two" wire-string))))))

(deftest http1-session-connection-close
  (let ((seen '()))
    (let ((wire
            (ensure-http1-session-run
             (ascii "GET /one HTTP/1.1|CRLF|Host: example.com|CRLF|Connection: close|CRLF||CRLF|GET /two HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
             (lambda (request)
               (push (http-request-target request) seen)
               (make-http-response
                :status 200
                :body (ascii (http-request-target request))))
             1
             :close
             :close-stream nil)))
      (ensure-equal 1 (length seen))
      (let ((wire-string (octets-as-string wire)))
        (ensure-true (search "/one" wire-string))
        (ensure-true (null (search "/two" wire-string)))))))

(deftest http1-session-http10-keep-alive
  (let ((seen '()))
    (let ((wire
            (ensure-http1-session-run
             (ascii "GET /one HTTP/1.0|CRLF|Host: example.com|CRLF|Connection: keep-alive|CRLF||CRLF|GET /two HTTP/1.0|CRLF|Host: example.com|CRLF|Connection: keep-alive|CRLF||CRLF|")
             (lambda (request)
               (push (http-request-target request) seen)
               (make-http-response
                :status 200
                :headers (list (make-http-header "Connection" "keep-alive"))
                :body (ascii (http-request-target request))))
             2
             :eof
             :close-stream nil)))
      (ensure-equal 2 (length seen))
      (ensure-true
       (search "HTTP/1.0 200 OK" (octets-as-string wire))))))

(deftest http1-session-max-requests-and-head
  (let ((wire
          (ensure-http1-session-run
           (ascii "GET /one HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|GET /two HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
           (lambda (request)
             (make-http-response
              :status 200
              :body (ascii (http-request-target request))))
           1
           :max-requests
           :max-requests 1
           :close-stream nil)))
    (ensure-true (null (search "/two" (octets-as-string wire)))))
  (let ((wire
          (ensure-http1-session-run
           (ascii "HEAD /resource HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
           (lambda (request)
             (declare (ignore request))
             (make-http-response :status 200 :body (ascii "payload")))
           1
           :eof
           :close-stream nil)))
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
    (let ((wire
            (ensure-http1-session-run
             (ascii "GET /chat HTTP/1.1|CRLF|Host: example.com|CRLF|Connection: Upgrade|CRLF|Upgrade: websocket|CRLF||CRLF|")
             (lambda (request)
               (declare (ignore request))
               (make-http-response
                :status 101
                :headers (list (make-http-header "Connection" "Upgrade")
                               (make-http-header "Upgrade" "websocket"))))
             1
             :upgrade
             :on-upgrade (lambda (stream request response)
                           (setf observed-stream stream
                                 observed-request request
                                 observed-response response)
                           t)
             :close-stream (lambda (stream)
                             (declare (ignore stream))
                             (setf closed t)))))
      (ensure-true observed-stream)
      (ensure-equal "/chat" (http-request-target observed-request))
      (ensure-equal 101 (http-response-status observed-response))
      (ensure-true (not closed))
      (ensure-true (search "HTTP/1.1 101 Switching Protocols"
                           (octets-as-string wire))))))

(deftest http1-session-expect-continue-automatic
  (let ((observed-body nil))
    (let ((wire
            (ensure-http1-session-run
             (ascii "POST /upload HTTP/1.1|CRLF|Host: example.com|CRLF|Expect: 100-continue|CRLF|Content-Length: 3|CRLF||CRLF|abc")
             (lambda (request)
               (setf observed-body (octets-as-string (http-request-body request)))
               (make-http-response :status 204))
             1
             :eof
             :close-stream nil)))
      (ensure-equal "abc" observed-body)
      (ensure-equal
       (ascii "HTTP/1.1 100 Continue|CRLF||CRLF|HTTP/1.1 204 No Content|CRLF||CRLF|")
       wire)))
  (let ((wire
          (ensure-http1-session-run
           (ascii "POST /upload HTTP/1.1|CRLF|Host: example.com|CRLF|Expect: 100-continue|CRLF|Content-Length: 3|CRLF||CRLF|abc")
           (lambda (request)
             (ensure-equal "abc" (octets-as-string (http-request-body request)))
             (make-http-response :status 204))
           1
           :eof
           :on-expect-continue nil
           :close-stream nil)))
    (ensure-equal
     (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|")
     wire)))
