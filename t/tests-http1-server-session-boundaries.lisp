(in-package #:http-kit/test-core)

(deftest core-coverage-http1-session-argument-errors
  (dolist (case '((:stream 7) (:handler 7) (:max-requests -1)
                  (:max-requests "1") (:on-error 7) (:on-upgrade 7)
                  (:on-expect-continue 7) (:close-stream 7)))
    (destructuring-bind (option value) case
      (let* ((output (make-string-output-stream))
             (stream (make-two-way-stream (make-string-input-stream "") output))
             (handler-calls 0)
             (handler (lambda (request)
                        (declare (ignore request))
                        (incf handler-calls)
                        (make-http-response :status 204))))
        (unwind-protect
             (let ((condition
                     (handler-case
                         (apply #'serve-http1-session
                                (if (eq option :stream) value stream)
                                (if (eq option :handler) value handler)
                                (unless (member option '(:stream :handler))
                                  (list option value)))
                       (http-protocol-error (condition) condition))))
               (ensure-true (typep condition 'http-protocol-error))
               (ensure-equal :session (http-error-operation condition))
               (ensure-equal (if (eq option :max-requests) value (type-of value))
                             (http-protocol-error-detail condition))
               (ensure-equal 0 handler-calls)
               (ensure-true (open-stream-p stream))
               (ensure-equal "" (get-output-stream-string output)))
          (close stream))))))

(deftest core-coverage-http1-session-connect-outcomes
  (dolist (stream-response-p '(nil t))
    (dolist (status '(200 299 400))
      (let ((upgrade-calls 0)
            (body-calls 0)
            (handler-calls 0))
        (let ((wire
                (ensure-http1-session-run
                 (ascii "CONNECT example.test:443 HTTP/1.1|CRLF|Host: example.test:443|CRLF||CRLF|")
                 (lambda (request)
                   (incf handler-calls)
                   (ensure-equal "CONNECT" (http-request-method request))
                   (if stream-response-p
                       (make-http-response-stream
                        :status status :reason "Tunnel" :body-length 0
                        :body-function (lambda () (incf body-calls) nil))
                       (make-http-response :status status :reason "Tunnel")))
                 1 (if (< status 300) :upgrade :eof)
                 :on-upgrade (lambda (stream request response)
                               (incf upgrade-calls)
                               (ensure-true (open-stream-p stream))
                               (ensure-equal "example.test:443"
                                             (http-request-target request))
                               (ensure-equal status (http-response-status response))
                               (ensure-equal "HTTP/1.1"
                                             (http-response-protocol-version response))
                               t)
                 :close-stream nil)))
          (ensure-equal 1 handler-calls)
          (ensure-equal (if (< status 300) 1 0) upgrade-calls)
          (ensure-equal (if (and stream-response-p (= status 400)) 1 0) body-calls)
          (ensure-equal
           (ascii (format nil "HTTP/1.1 ~D Tunnel|CRLF|~A|CRLF|"
                          status (if (= status 400) "Content-Length: 0|CRLF|" "")))
           wire))))))

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


(defun %http1-session-boundary-failure (request-bytes handler condition-type
                                       &rest arguments)
  (let ((hook-condition nil)
        (hook-request nil)
        (error-calls 0)
        (close-calls 0)
        (output-length nil))
    (let ((caught
            (handler-case
                (apply #'%run-http1-session-from-file request-bytes handler
                       :on-error (lambda (condition request)
                                   (incf error-calls)
                                   (setf hook-condition condition
                                         hook-request request))
                       :close-stream
                       (lambda (stream)
                         (incf close-calls)
                         (setf output-length
                               (file-position (two-way-stream-output-stream stream))))
                       arguments)
              (http-error (condition) condition))))
      (ensure-true (typep caught condition-type))
      (ensure-true (eq caught hook-condition))
      (ensure-equal 1 error-calls)
      (ensure-equal 1 close-calls)
      (values caught hook-request output-length))))

(deftest http1-session-boundary-upgrade-versioned-protocols
  (let ((close-calls 0)
        (upgrade-calls 0))
    (let ((wire
            (ensure-http1-session-run
             (ascii "GET /chat HTTP/1.1|CRLF|Host: example.com|CRLF|Connection: keep-alive, Upgrade|CRLF|Upgrade: other/2, chat/1|CRLF||CRLF|")
             (lambda (request)
               (declare (ignore request))
               (make-http-response
                :status 101
                :headers (list (make-http-header "Connection" "keep-alive, Upgrade")
                               (make-http-header "Upgrade" "CHAT/1"))))
             1 :upgrade
             :on-upgrade (lambda (stream request response)
                           (declare (ignore stream))
                           (incf upgrade-calls)
                           (ensure-equal "/chat" (http-request-target request))
                           (ensure-equal 101 (http-response-status response))
                           nil)
             :close-stream (lambda (stream)
                             (declare (ignore stream))
                             (incf close-calls)))))
      (ensure-true (search "Upgrade: CHAT/1" (octets-as-string wire)))
      (ensure-equal 1 upgrade-calls)
      (ensure-equal 1 close-calls)))
  (dolist (invalid '("/1" "chat/" "chat/1/2" "chat /1" "chat/1 2"))
    (dolist (side '(:request :response))
      (let ((handler-calls 0)
            (upgrade-calls 0))
        (multiple-value-bind (condition request output-length)
            (%http1-session-boundary-failure
             (ascii (format nil "GET /chat HTTP/1.1|CRLF|Host: example.com|CRLF|Connection: Upgrade|CRLF|Upgrade: ~A|CRLF||CRLF|"
                            (if (eq side :request) invalid "chat/1")))
             (lambda (request)
               (declare (ignore request))
               (incf handler-calls)
               (make-http-response
                :status 101
                :headers (list (make-http-header "Connection" "Upgrade")
                               (make-http-header "Upgrade"
                                                 (if (eq side :response)
                                                     invalid "chat/1")))))
             'http-protocol-error
             :on-upgrade (lambda (&rest arguments)
                           (declare (ignore arguments))
                           (incf upgrade-calls)))
          (ensure-equal :session (http-error-operation condition))
          (ensure-equal invalid (http-protocol-error-detail condition))
          (ensure-equal "/chat" (http-request-target request))
          (ensure-equal 0 output-length)
          (ensure-equal 1 handler-calls)
          (ensure-equal 0 upgrade-calls))))))

(deftest http1-session-boundary-upgrade-missing-request-offer
  (multiple-value-bind (condition request output-length)
      (%http1-session-boundary-failure
       (ascii "GET /chat HTTP/1.1|CRLF|Host: example.com|CRLF|Connection: Upgrade|CRLF||CRLF|")
       (lambda (request)
         (declare (ignore request))
         (make-http-response
          :status 101
          :headers (list (make-http-header "Connection" "Upgrade")
                         (make-http-header "Upgrade" "chat/1"))))
       'http-protocol-error)
    (ensure-equal :session (http-error-operation condition))
    (ensure-equal :request-upgrade (http-protocol-error-detail condition))
    (ensure-equal "/chat" (http-request-target request))
    (ensure-equal 0 output-length)))

(deftest http1-session-boundary-upgrade-missing-response-selection
  (multiple-value-bind (condition request output-length)
      (%http1-session-boundary-failure
       (ascii "GET /chat HTTP/1.1|CRLF|Host: example.com|CRLF|Connection: Upgrade|CRLF|Upgrade: chat/1|CRLF||CRLF|")
       (lambda (request)
         (declare (ignore request))
         (make-http-response
          :status 101
          :headers (list (make-http-header "Connection" "Upgrade"))))
       'http-protocol-error)
    (ensure-equal :session (http-error-operation condition))
    (ensure-equal :response-upgrade (http-protocol-error-detail condition))
    (ensure-equal "/chat" (http-request-target request))
    (ensure-equal 0 output-length)))

(deftest http1-session-boundary-invalid-informational-element
  (let ((input (ascii "GET /hints HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")))
    (ensure-equal
     (ascii "HTTP/1.1 103 Early Hints|CRLF||CRLF|HTTP/1.1 204 No Content|CRLF||CRLF|")
     (ensure-http1-session-run
      input
      (lambda (request)
        (declare (ignore request))
        (values (make-http-response :status 204)
                (list (make-http-response :status 103 :reason "Early Hints"))))
      1 :eof :close-stream nil))
    (multiple-value-bind (condition request output-length)
        (%http1-session-boundary-failure
         input
         (lambda (request)
           (declare (ignore request))
           (values (make-http-response :status 204)
                   (list (make-http-response :status 103 :reason "Early Hints")
                         :invalid)))
         'http-protocol-error)
      (ensure-equal :session (http-error-operation condition))
      (ensure-equal (type-of :invalid) (http-protocol-error-detail condition))
      (ensure-equal "/hints" (http-request-target request))
      (ensure-equal 0 output-length))))

(deftest http1-session-boundary-invalid-final-response
  (let ((input (ascii "GET /final HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")))
    (ensure-equal
     (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|")
     (ensure-http1-session-run
      input (lambda (request)
              (declare (ignore request))
              (make-http-response :status 204))
      1 :eof :close-stream nil))
    (dolist (invalid (list :invalid (make-http-response :status 103)))
      (multiple-value-bind (condition request output-length)
          (%http1-session-boundary-failure
           input (lambda (request) (declare (ignore request)) invalid)
           'http-protocol-error)
        (ensure-equal :session (http-error-operation condition))
        (ensure-equal (if (http-response-p invalid) 103 (type-of invalid))
                      (http-protocol-error-detail condition))
        (ensure-equal "/final" (http-request-target request))
        (ensure-equal 0 output-length)))
    (dolist (status '(100 103 199))
      (let ((body-calls 0))
        (multiple-value-bind (condition request output-length)
            (%http1-session-boundary-failure
             input
             (lambda (request)
               (declare (ignore request))
               (make-http-response-stream
                :status status
                :body-function (lambda () (incf body-calls) nil)))
             'http-protocol-error)
          (ensure-equal :session (http-error-operation condition))
          (ensure-equal status (http-protocol-error-detail condition))
          (ensure-equal "/final" (http-request-target request))
          (ensure-equal 0 output-length)
          (ensure-equal 0 body-calls))))))

(deftest http1-session-boundary-stream-length-overflow
  (let ((input (ascii "GET /stream HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|GET /next HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|"))
        (handler-calls 0)
        (body-calls 0)
        (chunks (list (ascii "ab") (ascii "cde") nil)))
    (multiple-value-bind (condition request output-length)
        (%http1-session-boundary-failure
         input
         (lambda (request)
           (declare (ignore request))
           (incf handler-calls)
           (make-http-response-stream
            :status 200 :body-length 4
            :body-function (lambda () (incf body-calls) (pop chunks))))
         'http-protocol-error)
      (ensure-equal :serialization (http-error-operation condition))
      (ensure-equal 4 (http-protocol-error-detail condition))
      (ensure-equal "/stream" (http-request-target request))
      (ensure-equal (length (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 4|CRLF||CRLF|ab"))
                    output-length)
      (ensure-equal 1 handler-calls)
      (ensure-equal 2 body-calls)))
  (let ((chunks (list (ascii "ab") (ascii "cd") nil)))
    (ensure-equal
     (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 4|CRLF||CRLF|abcd")
     (ensure-http1-session-run
      (ascii "GET /stream HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
      (lambda (request)
        (declare (ignore request))
        (make-http-response-stream :status 200 :body-length 4
                                   :body-function (lambda () (pop chunks))))
      1 :eof :close-stream nil))))

(deftest http1-session-boundary-stream-length-underflow
  (let ((input (ascii "GET /stream HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|GET /next HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|"))
        (handler-calls 0)
        (body-calls 0)
        (chunks (list (ascii "ab") nil)))
    (multiple-value-bind (condition request output-length)
        (%http1-session-boundary-failure
         input
         (lambda (request)
           (declare (ignore request))
           (incf handler-calls)
           (make-http-response-stream
            :status 200 :body-length 4
            :body-function (lambda () (incf body-calls) (pop chunks))))
         'http-protocol-error)
      (ensure-equal :serialization (http-error-operation condition))
      (ensure-equal '(:expected 4 :observed 2) (http-protocol-error-detail condition))
      (ensure-equal "/stream" (http-request-target request))
      (ensure-equal (length (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 4|CRLF||CRLF|ab"))
                    output-length)
      (ensure-equal 1 handler-calls)
      (ensure-equal 2 body-calls)))
  (let ((body-calls 0))
    (ensure-equal
     (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|")
     (ensure-http1-session-run
      (ascii "GET /stream HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
      (lambda (request)
        (declare (ignore request))
        (make-http-response-stream :status 200 :body-length 0
                                   :body-function (lambda () (incf body-calls) nil)))
      1 :eof :close-stream nil))
    (ensure-equal 1 body-calls)))

(deftest http1-session-boundary-stream-framing-validation
  (dolist (case '((200 1 (("Content-Length" "1") ("Transfer-Encoding" "chunked"))
                  nil "HTTP/1.1" http-invalid-header :ambiguous-framing)
                 (204 nil (("Transfer-Encoding" "chunked"))
                  nil "HTTP/1.1" http-invalid-header :forbidden)
                 (204 1 nil nil "HTTP/1.1" http-protocol-error 204)
                 (205 nil (("Content-Length" "1"))
                  nil "HTTP/1.1" http-invalid-header :forbidden)
                 (200 2 (("Content-Length" "1"))
                  nil "HTTP/1.1" http-invalid-header :mismatch)
                 (200 1 (("Content-Length" "1"))
                  (("X-Checksum" "ok")) "HTTP/1.1" http-invalid-header :framing)
                 (200 nil (("Transfer-Encoding" "chunked"))
                  nil "HTTP/1.0" http-unsupported-feature :chunked)))
    (destructuring-bind (status body-length headers trailers version class detail) case
      (let ((body-calls 0))
        (multiple-value-bind (condition request output-length)
            (%http1-session-boundary-failure
             (ascii (format nil "GET /framing ~A|CRLF|Host: example.com|CRLF||CRLF|" version))
             (lambda (request)
               (declare (ignore request))
               (make-http-response-stream
                :status status :body-length body-length
                :headers (mapcar (lambda (pair) (apply #'make-http-header pair)) headers)
                :trailers (mapcar (lambda (pair) (apply #'make-http-header pair)) trailers)
                :body-function (lambda () (incf body-calls) nil)))
             class)
          (ensure-equal :serialization (http-error-operation condition))
          (ensure-equal detail
                        (if (typep condition 'http-invalid-header)
                            (http-invalid-header-reason condition)
                            (http-protocol-error-detail condition)))
          (when (typep condition 'http-invalid-header)
            (ensure-equal (if (and (= status 204) (null body-length))
                              "transfer-encoding" "content-length")
                          (http-invalid-header-name condition)))
          (when (typep condition 'http-unsupported-feature)
            (ensure-equal :http1-response-transfer-encoding
                          (http-unsupported-feature-name condition)))
          (ensure-equal "/framing" (http-request-target request))
          (ensure-equal 0 output-length)
          (ensure-equal 0 body-calls)))))
  (let ((body-calls 0))
    (ensure-equal
     (ascii "HTTP/1.1 205 Reset Content|CRLF|Content-Length: 0|CRLF||CRLF|")
     (ensure-http1-session-run
      (ascii "GET /framing HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")
      (lambda (request)
        (declare (ignore request))
        (make-http-response-stream
         :status 205 :body-function (lambda () (incf body-calls) nil)))
      1 :eof :close-stream nil))
    (ensure-equal 0 body-calls)))

(deftest http1-session-boundary-custom-expect-continue
  (let ((input (ascii "POST /upload HTTP/1.1|CRLF|Host: example.com|CRLF|Expect: 100-continue|CRLF|Content-Length: 3|CRLF||CRLF|abc"))
        (events nil))
    (ensure-equal
     (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|")
     (ensure-http1-session-run
      input
      (lambda (request)
        (push :handler events)
        (ensure-equal "abc" (octets-as-string (http-request-body request)))
        (make-http-response :status 204))
      1 :eof
      :on-expect-continue (lambda (request)
                            (push :expect events)
                            (ensure-equal "/upload" (http-request-target request))
                            (ensure-equal 0 (length (http-request-body request))))
      :close-stream nil))
    (ensure-equal '(:expect :handler) (reverse events))
    (let ((primary (make-condition 'http-protocol-error
                                   :message "Upload rejected by continue callback."
                                   :operation :test :detail :upload-rejected))
          (expect-calls 0)
          (handler-calls 0))
      (multiple-value-bind (condition request output-length)
          (%http1-session-boundary-failure
           input
           (lambda (request)
             (declare (ignore request))
             (incf handler-calls)
             (make-http-response :status 204))
           'http-protocol-error
           :on-expect-continue (lambda (request)
                                 (ensure-equal "/upload" (http-request-target request))
                                 (incf expect-calls)
                                 (error primary)))
        (ensure-true (eq primary condition))
        (ensure-equal :test (http-error-operation condition))
        (ensure-equal :upload-rejected (http-protocol-error-detail condition))
        (ensure-false request)
        (ensure-equal 0 output-length)
        (ensure-equal 1 expect-calls)
        (ensure-equal 0 handler-calls)))))

(deftest http1-public-boundary-shared-stream-version-invalid
  (let ((body-calls 0)
        (handler-calls 0)
        (input (ascii "GET / HTTP/1.1|CRLF|Host: example.com|CRLF|Connection: close|CRLF||CRLF|")))
    (multiple-value-bind (condition request output-length)
        (%http1-session-boundary-failure
         input
         (lambda (request)
           (incf handler-calls)
           (let* ((version (http-request-protocol-version request))
                  (response
                    (make-http-response-stream
                     :protocol-version version :status 200 :body-length 0
                     :body-function (lambda () (incf body-calls) nil))))
             (ensure-true (eq version (http-response-stream-protocol-version response)))
             (setf (char version 7) #\9)
             response))
         'http-unsupported-feature)
      (ensure-equal :serialization (http-error-operation condition))
      (ensure-equal :http1-response-version (http-unsupported-feature-name condition))
      (ensure-equal "HTTP/1.9" (http-protocol-error-detail condition))
      (ensure-equal "HTTP/1.9" (http-request-protocol-version request))
      (ensure-equal 0 output-length)
      (ensure-equal 0 body-calls)
      (ensure-equal 1 handler-calls))))

(deftest http1-public-boundary-shared-stream-version-valid
  (let ((body-calls 0)
        (handler-calls 0))
    (ensure-equal
     (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok")
     (ensure-http1-session-run
      (ascii "GET / HTTP/1.1|CRLF|Host: example.com|CRLF|Connection: close|CRLF||CRLF|")
      (lambda (request)
        (incf handler-calls)
        (let* ((version (http-request-protocol-version request))
               (response
                 (make-http-response-stream
                  :protocol-version version :status 200 :body-length 2
                  :body-function
                  (lambda ()
                    (incf body-calls)
                    (when (= body-calls 1) (ascii "ok"))))))
          (ensure-true (eq version (http-response-stream-protocol-version response)))
          (ensure-equal "HTTP/1.1" version)
          response))
      1 :close))
    (ensure-equal 2 body-calls)
    (ensure-equal 1 handler-calls)))

(deftest http1-second-wave-explicit-content-length-body-boundaries
  (let ((request-wire
          (ascii "GET /stream HTTP/1.1|CRLF|Host: example.com|CRLF|Connection: close|CRLF||CRLF|"))
        (response-head (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 4|CRLF||CRLF|")))
    (dolist (case '("abcde" "ab"))
      (let ((body-calls 0)
            (handler-calls 0)
            (chunks (list (ascii case) nil)))
        (multiple-value-bind (condition request output-length)
            (%http1-session-boundary-failure
             request-wire
             (lambda (request)
               (declare (ignore request))
               (incf handler-calls)
               (make-http-response-stream
                :status 200
                :headers (list (make-http-header "Content-Length" "4"))
                :body-length 4
                :body-function (lambda ()
                                 (incf body-calls)
                                 (pop chunks))))
             'http-protocol-error)
          (ensure-true (not (typep condition 'http-invalid-header)))
          (ensure-equal :serialization (http-error-operation condition))
          (ensure-equal (if (= (length case) 5)
                            4
                            '(:expected 4 :observed 2))
                        (http-protocol-error-detail condition))
          (ensure-equal "/stream" (http-request-target request))
          (ensure-equal 1 handler-calls)
          (ensure-equal (if (= (length case) 5) 1 2) body-calls)
          (ensure-equal (+ (length response-head)
                           (if (= (length case) 5) 0 2))
                        output-length))))
    (let ((handler-calls 0)
          (body-calls 0)
          (chunks (list (ascii "abcd") nil)))
      (ensure-octets-equal
       (concatenate-octets response-head (ascii "abcd"))
       (ensure-http1-session-run
        request-wire
        (lambda (request)
          (declare (ignore request))
          (incf handler-calls)
          (make-http-response-stream
           :status 200
           :headers (list (make-http-header "Content-Length" "4"))
           :body-length 4
           :body-function (lambda ()
                            (incf body-calls)
                            (pop chunks))))
        1 :close))
      (ensure-equal 1 handler-calls)
      (ensure-equal 2 body-calls)
      (ensure-true (null chunks)))))

(deftest http1-native-boundaries-http10-response-persistence
  (let ((request-wire
          (ascii "GET /one HTTP/1.0|CRLF|Host: example.test|CRLF|Connection: keep-alive|CRLF||CRLF|GET /two HTTP/1.0|CRLF|Host: example.test|CRLF|Connection: keep-alive|CRLF||CRLF|")))
    (dolist (keep-alive-p '(nil t))
      (let ((seen nil))
        (let ((wire
                (ensure-http1-session-run
                 request-wire
                 (lambda (request)
                   (push (http-request-target request) seen)
                   (make-http-response
                    :status 200
                    :headers (when keep-alive-p '(("Connection" . "keep-alive")))
                    :body (ascii (http-request-target request))))
                 (if keep-alive-p 2 1) (if keep-alive-p :eof :close)
                 :close-stream nil)))
          (ensure-equal (if keep-alive-p '("/two" "/one") '("/one")) seen)
          (ensure-octets-equal
           (if keep-alive-p
               (ascii "HTTP/1.0 200 OK|CRLF|Connection: keep-alive|CRLF|Content-Length: 4|CRLF||CRLF|/oneHTTP/1.0 200 OK|CRLF|Connection: keep-alive|CRLF|Content-Length: 4|CRLF||CRLF|/two")
               (ascii "HTTP/1.0 200 OK|CRLF|Content-Length: 4|CRLF||CRLF|/one"))
           wire))))))
#+sbcl
(progn
  (defclass http1-deadline-session-stream
      (sb-gray:fundamental-binary-input-stream
       sb-gray:fundamental-binary-output-stream)
    ((input :initarg :input :reader http1-deadline-session-input)
     (position :initform 0 :accessor http1-deadline-session-position)
     (block-at :initarg :block-at :initform nil
               :reader http1-deadline-session-block-at)
     (block-writes-p :initarg :block-writes-p :initform nil
                     :reader http1-deadline-session-block-writes-p)
     (output :initform (make-array 0
                                  :element-type '(unsigned-byte 8)
                                  :adjustable t
                                  :fill-pointer 0)
             :reader http1-deadline-session-output)))

  (defmethod sb-gray:stream-read-byte
      ((stream http1-deadline-session-stream))
    (let ((position (http1-deadline-session-position stream)))
      (when (and (http1-deadline-session-block-at stream)
                 (>= position (http1-deadline-session-block-at stream)))
        (sleep 10))
      (let ((input (http1-deadline-session-input stream)))
        (if (< position (length input))
            (prog1 (aref input position)
              (incf (http1-deadline-session-position stream)))
            :eof))))

  (defmethod sb-gray:stream-write-sequence
      ((stream http1-deadline-session-stream) sequence
       &optional (start 0) end)
    (when (http1-deadline-session-block-writes-p stream)
      (sleep 10))
    (let ((end (or end (length sequence)))
          (output (http1-deadline-session-output stream)))
      (loop for index from start below end
            do (vector-push-extend (aref sequence index) output)))
    sequence)

  (defmethod sb-gray:stream-finish-output
      ((stream http1-deadline-session-stream))
    (declare (ignore stream))
    nil)

  (deftest http1-session-request-timeout-blocks-stream-read
    (let ((stream (make-instance 'http1-deadline-session-stream
                                 :input (ascii "G")
                                 :block-at 1))
          (handler-calls 0)
          (observed nil))
      (handler-case
          (sb-ext:with-timeout 1
            (serve-http1-session
             stream
             (lambda (request)
               (declare (ignore request))
               (incf handler-calls)
               (make-http-response :status 204))
             :request-timeout 0.05d0
             :clock-function (lambda () 0)
             :close-stream nil))
        (http-timeout (condition)
          (setf observed condition)))
      (ensure-true (typep observed 'http-timeout))
      (ensure-equal 0 handler-calls)
      (ensure-equal 0 (length (http1-deadline-session-output stream)))))

  (deftest http1-session-request-timeout-late-handler-writes-no-response
    (let* ((now 0)
           (handler-calls 0)
           (observed nil)
           (stream
             (make-instance
              'http1-deadline-session-stream
              :input (ascii "GET /late HTTP/1.1|CRLF|Host: example.test|CRLF||CRLF|"))))
      (handler-case
          (serve-http1-session
           stream
           (lambda (request)
             (declare (ignore request))
             (incf handler-calls)
             (setf now 2)
             (values (make-http-response :status 200)
                     (list (make-http-response :status 103))))
           :request-timeout 1
           :clock-function (lambda () now)
           :close-stream nil)
        (http-timeout (condition)
          (setf observed condition)))
      (ensure-true (typep observed 'http-timeout))
      (ensure-equal 1 handler-calls)
      (ensure-equal 0 (length (http1-deadline-session-output stream)))))

  (deftest http1-session-request-timeout-interrupts-response-write
    (let ((stream
            (make-instance
             'http1-deadline-session-stream
             :input (ascii "GET /write HTTP/1.1|CRLF|Host: example.test|CRLF||CRLF|")
             :block-writes-p t))
          (handler-calls 0)
          (observed nil))
      (handler-case
          (sb-ext:with-timeout 1
            (serve-http1-session
             stream
             (lambda (request)
               (declare (ignore request))
               (incf handler-calls)
               (make-http-response :status 200))
             :request-timeout 0.05d0
             :clock-function (lambda () 0)
             :close-stream nil))
        (http-timeout (condition)
          (setf observed condition)))
      (ensure-true (typep observed 'http-timeout))
      (ensure-equal 1 handler-calls)
      (ensure-equal 0 (length (http1-deadline-session-output stream)))))

  (deftest http1-session-idle-timeout-blocks-next-request-read
    (let* ((request-wire
             (ascii "GET /idle HTTP/1.1|CRLF|Host: example.test|CRLF||CRLF|"))
           (now 0)
           (handler-calls 0)
           (observed nil)
           (stream
             (make-instance
              'http1-deadline-session-stream
              :input request-wire
              :block-at (length request-wire))))
      (handler-case
          (sb-ext:with-timeout 1
            (serve-http1-session
             stream
             (lambda (request)
               (declare (ignore request))
               (incf handler-calls)
               (make-http-response :status 204))
             :idle-timeout 0.05d0
             :clock-function (lambda () now)
             :close-stream nil))
        (http-timeout (condition)
          (setf observed condition)))
      (ensure-true (typep observed 'http-timeout))
      (ensure-equal 1 handler-calls)
      (ensure-true
       (search "HTTP/1.1 204 No Content"
               (octets-as-string
                (http1-deadline-session-output stream))))))

  (deftest http1-session-idle-timeout-bounds-initial-request-read
    (let ((stream (make-instance 'http1-deadline-session-stream
                                 :input (ascii "")
                                 :block-at 0))
          (handler-calls 0)
          (observed nil))
      (handler-case
          (sb-ext:with-timeout 1
            (serve-http1-session
             stream
             (lambda (request)
               (declare (ignore request))
               (incf handler-calls)
               (make-http-response :status 204))
             :idle-timeout 0.05d0
             :clock-function (lambda () 0)
             :close-stream nil))
        (http-timeout (condition)
          (setf observed condition)))
      (ensure-true (typep observed 'http-timeout))
      (ensure-equal :read (http-timeout-kind observed))
      (ensure-equal 0 handler-calls)
      (ensure-equal 0 (length (http1-deadline-session-output stream)))))

  (deftest http1-session-shutdown-closes-after-current-response
    (let ((handler-calls 0)
          (request-wire
            (ascii "GET /one HTTP/1.1|CRLF|Host: example.test|CRLF||CRLF|GET /two HTTP/1.1|CRLF|Host: example.test|CRLF||CRLF|")))
      (let ((wire
              (ensure-http1-session-run
               request-wire
               (lambda (request)
                 (declare (ignore request))
                 (incf handler-calls)
                 (make-http-response :status 204))
               1 :close
               :shutdown-p (lambda () (= handler-calls 1))
               :close-stream nil)))
        (ensure-equal 1 handler-calls)
        (ensure-octets-equal
         (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|")
         wire)))))

