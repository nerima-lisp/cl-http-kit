(in-package #:http-kit/test-core)

(deftest core-runtime-request-target-visible-ascii-contract
  (dolist (code '(0 9 10 13 31 32 127 128 256))
    (let* ((target (concatenate 'string "/" (string (code-char code))))
           (condition
             (handler-case
                 (make-http-request :method "GET" :uri "http://example.test/"
                                    :request-target target)
               (http-protocol-error (condition) condition))))
      (ensure-true (typep condition 'http-protocol-error))
      (ensure-equal :request (http-error-operation condition))
      (ensure-equal target (http-protocol-error-detail condition))))
  (dolist (target '("/!" "/~"))
    (ensure-equal target
                  (http-request-target
                   (make-http-request :method "GET" :uri "http://example.test/"
                                      :request-target target)))))

(deftest core-runtime-request-head-known-length-contract
  (let ((request (make-http-request :method "POST" :uri "http://example.test/")))
    (dolist (length '(nil -1 1/2 1.0 "1" :unknown))
      (let ((condition
              (handler-case (http-kit::%request-head-wire request :body-length length)
                (http-protocol-error (condition) condition))))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :serialization (http-error-operation condition))
        (ensure-equal length (http-protocol-error-detail condition))))
    (multiple-value-bind (head transfer-mode expected-length)
        (http-kit::%request-head-wire request :body-length 0)
      (ensure-equal (ascii "POST / HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 0|CRLF||CRLF|") head)
      (ensure-equal nil transfer-mode)
      (ensure-equal 0 expected-length))
    (multiple-value-bind (head transfer-mode expected-length)
        (http-kit::%request-head-wire request :body-length-known-p nil)
      (ensure-equal (ascii "POST / HTTP/1.1|CRLF|Host: example.test|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|") head)
      (ensure-equal :chunked transfer-mode)
      (ensure-equal nil expected-length))))

(deftest core-runtime-octet-body-length-contract
  (let ((request (make-http-request :method "POST" :uri "http://example.test/"
                                    :body (ascii "abc"))))
    (dolist (expected-length '(0 2 4))
      (let ((stream (make-instance 'binary-test-stream :input (ascii "!"))))
        (unwind-protect
             (let ((condition
                     (handler-case
                         (http-kit::%write-request-body-octets
                          stream request nil expected-length nil nil)
                       (http-protocol-error (condition) condition))))
               (ensure-true (typep condition 'http-protocol-error))
               (ensure-equal :body (http-error-operation condition))
               (ensure-equal (list :expected expected-length :observed 3)
                             (http-protocol-error-detail condition))
               (ensure-equal (ascii "") (binary-test-output stream))
               (ensure-equal 0 (binary-test-input-position stream)))
          (close stream))))
    (dolist (expected-length '(nil 3))
      (let ((stream (make-instance 'binary-test-stream :input (ascii "!"))))
        (unwind-protect
             (progn
               (ensure-equal 3 (http-kit::%write-request-body-octets
                                stream request nil expected-length nil nil))
               (ensure-equal (ascii "abc") (binary-test-output stream))
               (ensure-equal 0 (binary-test-input-position stream)))
          (close stream))))))

(deftest core-runtime-expect-request-version-contract
  (dolist (version '("HTTP/1.0" "HTTP/1.1"))
    (let* ((head (ascii (format nil "POST / HTTP/1.~A|CRLF|Host: example.test|CRLF|Expect: 100-Continue|CRLF|Content-Length: 3|CRLF||CRLF|"
                               (if (string= version "HTTP/1.0") "0" "1"))))
           (stream (make-instance 'binary-test-stream
                                  :input (concatenate-octets head (ascii "abc!"))))
           (continue-calls 0)
           (chunks nil))
      (unwind-protect
           (let ((result
                   (handler-case
                       (parse-http-request
                        stream
                        :on-expect-continue
                        (lambda (request)
                          (incf continue-calls)
                          (ensure-equal version (http-request-protocol-version request))
                          (ensure-equal (ascii "") (http-request-body request))
                          (ensure-equal (length head) (binary-test-input-position stream)))
                        :on-body-chunk (lambda (chunk) (push (copy-seq chunk) chunks)))
                     (http-unsupported-feature (condition) condition))))
             (if (string= version "HTTP/1.0")
                 (progn
                   (ensure-true (typep result 'http-unsupported-feature))
                   (ensure-equal :request-parse (http-error-operation result))
                   (ensure-equal :http1-expectation (http-unsupported-feature-name result))
                   (ensure-equal '("100-continue") (http-protocol-error-detail result))
                   (ensure-equal 0 continue-calls)
                   (ensure-equal nil chunks)
                   (ensure-equal (length head) (binary-test-input-position stream))
                   (ensure-equal (char-code #\a) (read-byte stream)))
                 (progn
                   (ensure-true (http-request-p result))
                   (ensure-equal (ascii "abc") (http-request-body result))
                   (ensure-equal (list (ascii "abc")) chunks)
                   (ensure-equal 1 continue-calls)
                   (ensure-equal (+ (length head) 3) (binary-test-input-position stream))
                   (ensure-equal (char-code #\!) (read-byte stream)))))
        (close stream)))))

(deftest core-runtime-client-expect-empty-body-and-trailers
  (dolist (mode '(:trailers :empty :http10 :no-expect))
    (let* ((trailers-p (eq mode :trailers))
           (empty-body-p (member mode '(:trailers :empty)))
           (version (if (eq mode :http10) "HTTP/1.0" "HTTP/1.1"))
           (request
             (make-http-request
              :method "POST" :uri "http://example.test/" :protocol-version version
              :headers (unless (eq mode :no-expect)
                         (list (make-http-header "Expect" "100-continue")))
              :trailers (when trailers-p (list (make-http-header "X-End" "done")))
              :body (ascii (if empty-body-p "" "abc"))))
           (head
             (ascii (format nil "POST / ~A|CRLF|~AHost: example.test|CRLF|~A|CRLF|"
                            version
                            (if (eq mode :no-expect) "" "Expect: 100-continue|CRLF|")
                            (cond (trailers-p "Transfer-Encoding: chunked|CRLF|Trailer: X-End|CRLF|")
                                  (empty-body-p "Content-Length: 0|CRLF|")
                                  (t "Content-Length: 3|CRLF|")))))
           (body (ascii (cond (trailers-p "0|CRLF|X-End: done|CRLF||CRLF|")
                              (empty-body-p "")
                              (t "abc"))))
           (reply (ascii (if (eq mode :http10)
                             "HTTP/1.0 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|"
                             "HTTP/1.1 103 Early Hints|CRLF||CRLF|HTTP/1.1 100 Continue|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|")))
           (stream (make-instance 'binary-test-stream
                                  :input (concatenate-octets reply (ascii "!"))))
           (statuses nil))
      (unwind-protect
           (progn
             (multiple-value-bind (response reusable)
                 (send-http-request-over-open-stream
                  request stream
                  :on-information
                  (lambda (information)
                    (push (http-response-status information) statuses)
                    (ensure-equal (if trailers-p head (concatenate-octets head body))
                                  (binary-test-output stream))))
               (ensure-equal 200 (http-response-status response))
               (ensure-equal (ascii "") (http-response-body response))
               (ensure-equal (not (eq mode :http10)) reusable))
             (ensure-equal (if (eq mode :http10) nil '(103 100)) (nreverse statuses))
             (ensure-equal (concatenate-octets head body) (binary-test-output stream))
             (ensure-equal (length reply) (binary-test-input-position stream))
             (ensure-equal (char-code #\!) (read-byte stream))
             (if (eq mode :http10)
                 (let ((condition
                         (handler-case (parse-http-request (binary-test-output stream))
                           (http-unsupported-feature (condition) condition))))
                   (ensure-true (typep condition 'http-unsupported-feature))
                   (ensure-equal :request-parse (http-error-operation condition))
                   (ensure-equal :http1-expectation (http-unsupported-feature-name condition)))
                 (let ((parse-stream
                         (make-instance 'binary-test-stream
                                        :input (concatenate-octets
                                                (binary-test-output stream) (ascii "!")))))
                   (unwind-protect
                        (let ((parsed (parse-http-request parse-stream)))
                          (ensure-equal (length (binary-test-output stream))
                                        (binary-test-input-position parse-stream))
                          (ensure-equal (char-code #\!) (read-byte parse-stream))
                          (ensure-equal (ascii (if empty-body-p "" "abc"))
                                        (http-request-body parsed))
                          (ensure-equal (if trailers-p '("done") nil)
                                        (http-header-values (http-request-trailers parsed) "x-end")))
                     (close parse-stream)))))
        (close stream)))))

(deftest core-runtime-empty-upgrade-protocol-contract
  (dolist (side '(:request :response :valid))
    (let* ((head (ascii (format nil "GET /chat HTTP/1.1|CRLF|Host: example.test|CRLF|Connection: Upgrade|CRLF|Upgrade: ~A|CRLF||CRLF|"
                               (if (eq side :request) "" "chat"))))
           (stream (make-instance 'binary-test-stream
                                  :input (concatenate-octets head (ascii "!"))))
           (handler-calls 0) (upgrade-calls 0) (error-calls 0) (close-calls 0)
           (hook-condition nil))
      (unwind-protect
           (let ((result
                   (handler-case
                       (multiple-value-list
                        (serve-http1-session
                         stream
                         (lambda (request)
                           (incf handler-calls)
                           (ensure-equal "/chat" (http-request-target request))
                           (make-http-response
                            :status 101
                            :headers (list (make-http-header "Connection" "Upgrade")
                                           (make-http-header "Upgrade"
                                                             (if (eq side :response) "" "CHAT")))))
                         :on-error (lambda (condition request)
                                     (incf error-calls)
                                     (setf hook-condition condition)
                                     (ensure-equal "/chat" (http-request-target request)))
                         :on-upgrade (lambda (received-stream request response)
                                       (incf upgrade-calls)
                                       (ensure-true (eq stream received-stream))
                                       (ensure-equal "/chat" (http-request-target request))
                                       (ensure-equal 101 (http-response-status response))
                                       nil)
                         :close-stream (lambda (received-stream)
                                         (incf close-calls)
                                         (ensure-true (eq stream received-stream)))))
                     (http-protocol-error (condition) condition))))
             (ensure-equal 1 handler-calls)
             (ensure-equal 1 close-calls)
             (ensure-equal (length head) (binary-test-input-position stream))
             (ensure-equal (char-code #\!) (read-byte stream))
             (if (eq side :valid)
                 (progn
                   (ensure-equal '(1 :upgrade) result)
                   (ensure-equal 1 upgrade-calls)
                   (ensure-equal 0 error-calls)
                   (ensure-equal
                    (ascii "HTTP/1.1 101 Switching Protocols|CRLF|Connection: Upgrade|CRLF|Upgrade: CHAT|CRLF||CRLF|")
                    (binary-test-output stream)))
                 (progn
                   (ensure-true (typep result 'http-protocol-error))
                   (ensure-true (eq result hook-condition))
                   (ensure-equal :session (http-error-operation result))
                   (ensure-equal "" (http-protocol-error-detail result))
                   (ensure-equal 1 error-calls)
                   (ensure-equal 0 upgrade-calls)
                   (ensure-equal (ascii "") (binary-test-output stream)))))
        (close stream)))))

(deftest core-runtime-request-print-query-redaction
  (dolist (suffix '("" "?" "?visible-fixture-query"))
    (let* ((request (make-http-request
                     :method "POST" :uri (concatenate 'string "http://example.test/path" suffix)
                     :headers (list (make-http-header "X-Fixture" "header-not-for-display"))
                     :body (ascii "body-not-for-display")))
           (printed (princ-to-string request)))
      (ensure-true
       (search (concatenate 'string "POST http://example.test/path"
                            (if (string= suffix "") "" "?<redacted>")
                            " headers=1 body-bytes=20") printed))
      (ensure-false (search "visible-fixture-query" printed))
      (ensure-false (search "header-not-for-display" printed))
      (ensure-false (search "body-not-for-display" printed))
      (ensure-equal (not (string= suffix ""))
                    (not (null (search "?<redacted>" printed)))))))

(deftest core-runtime-informational-status-contract
  (let ((condition
          (handler-case (make-http-response :status 99)
            (http-invalid-status (condition) condition))))
    (ensure-true (typep condition 'http-invalid-status))
    (ensure-equal :response (http-error-operation condition))
    (ensure-equal 99 (http-invalid-status-code condition)))
  (dolist (status '(100 199 101 200))
    (let* ((head (ascii "GET /information HTTP/1.1|CRLF|Host: example.test|CRLF||CRLF|"))
           (stream (make-instance 'binary-test-stream :input head))
           (handler-calls 0) (error-calls 0) (close-calls 0)
           (hook-condition nil))
      (unwind-protect
           (let ((result
                   (handler-case
                       (multiple-value-list
                        (serve-http1-session
                         stream
                         (lambda (request)
                           (incf handler-calls)
                           (ensure-equal "/information" (http-request-target request))
                           (values (make-http-response :status 204)
                                   (list (make-http-response :status status :reason "Boundary"))))
                         :on-error (lambda (condition request)
                                     (incf error-calls)
                                     (setf hook-condition condition)
                                     (ensure-equal "/information" (http-request-target request)))
                         :close-stream (lambda (received-stream)
                                         (incf close-calls)
                                         (ensure-true (eq stream received-stream)))))
                     (http-protocol-error (condition) condition))))
             (ensure-equal 1 handler-calls)
             (ensure-equal 1 close-calls)
             (ensure-equal (length head) (binary-test-input-position stream))
             (if (member status '(100 199))
                 (progn
                   (ensure-equal '(1 :eof) result)
                   (ensure-equal 0 error-calls)
                   (ensure-equal
                    (ascii (format nil "HTTP/1.1 ~D Boundary|CRLF||CRLF|HTTP/1.1 204 No Content|CRLF||CRLF|"
                                   status))
                    (binary-test-output stream)))
                 (progn
                   (ensure-true (typep result 'http-protocol-error))
                   (ensure-true (eq result hook-condition))
                   (ensure-equal :session (http-error-operation result))
                   (ensure-equal status (http-protocol-error-detail result))
                   (ensure-equal 1 error-calls)
                   (ensure-equal (ascii "") (binary-test-output stream)))))
        (close stream)))))
