(in-package #:http-kit/test-core)

(deftest recording-request-cps-success
  (let* ((request (make-http-request :method "GET"
                                     :uri "http://127.0.0.1/"))
         (transport
           (make-recording-session
            :responses (list (make-http-response :status 204))))
         (status nil))
    (ensure-equal :success
                  (send-recorded-http-request/cps
                   transport request
                   (lambda (response)
                     (setf status (http-response-status response))
                     :success)))
    (ensure-equal 204 status)))
(deftest recording-request-copies-trailer-headers
  (let* ((request
           (make-http-request
            :method "POST"
            :uri "http://example.test/upload"
            :headers (list (make-http-header "Transfer-Encoding" "chunked")
                           (make-http-header "Trailer" "X-Checksum"))
            :body (ascii "payload")
            :trailers (list (make-http-header "X-Checksum" "done"))))
         (source-trailer (first (http-request-trailers request)))
         (transport
           (make-recording-session
            :responses (list (make-http-response :status 204))))
         (result
           (send-recorded-http-request/cps
            transport request
            (lambda (response)
              (ensure-equal 204 (http-response-status response))
              :success)))
         (recorded-trailer
           (first (http-request-trailers
                   (first (recording-session-requests transport))))))
    (ensure-equal :success result)
    (ensure-equal "X-Checksum" (http-header-name recorded-trailer))
    (ensure-equal "done" (http-header-content recorded-trailer))
    (ensure-false
     (eq (http-header-name source-trailer)
         (http-header-name recorded-trailer)))
    (ensure-false
     (eq (http-header-content source-trailer)
         (http-header-content recorded-trailer)))))

(deftest recording-request-cps-error
  (let* ((request (make-http-request :method "GET"
                                     :uri "http://127.0.0.1/"))
         (transport
           (make-recording-session
            :response-function (lambda (received-request &key timeout deadline)
                                 (declare (ignore received-request timeout deadline))
                                 (error "synthetic transport failure"))))
         (condition-type nil))
    (ensure-equal :failure
                  (send-recorded-http-request/cps
                   transport request
                   (lambda (response)
                     (declare (ignore response))
                     :unexpected)
                   :on-error (lambda (condition)
                               (setf condition-type (type-of condition))
                               :failure)))
    (ensure-equal 'http-connection-error condition-type)))

(deftest recording-request-cps-boundaries
  (let* ((request (make-http-request :method "GET"
                                     :uri "http://127.0.0.1/"))
         (transport (make-recording-session)))
    (signals http-connection-error
      (send-recorded-http-request/cps
       transport request
       (lambda (response)
         (declare (ignore response))
         :unexpected)))
    (signals http-protocol-error
      (send-recorded-http-request/cps
       transport request
       nil))
    (signals http-protocol-error
      (send-recorded-http-request/cps
       transport request
       (lambda (response)
         (declare (ignore response))
         :unexpected)
       :on-error 7))
    (let ((transport-with-response
            (make-recording-session
             :responses (list (make-http-response :status 200)))))
      (signals error
        (send-recorded-http-request/cps
         transport-with-response request
         (lambda (response)
           (declare (ignore response))
           (error "success continuation failure")))))))

(deftest public-recording-transport-boundaries-require-state
  (let ((request (make-http-request :method "GET"
                                    :uri "http://127.0.0.1/")))
    (signals http-protocol-error
      (send-recorded-http-request nil request))))

(deftest core-coverage-transport-invalid-object-metadata
  (let ((request (make-http-request :method "GET" :uri "http://example.test/"))
        (response (make-http-response :status 204)))
    (dolist (arguments (list (list 7 response) (list request 7)))
      (let ((condition
              (handler-case (apply #'http-response-reusable-p arguments)
                (http-protocol-error (condition) condition))))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :transport (http-error-operation condition))
        (ensure-equal 7 (http-protocol-error-detail condition))))
    (let ((condition
            (handler-case (send-http-request-over-open-stream request 7)
              (http-connection-error (condition) condition))))
      (ensure-true (typep condition 'http-connection-error))
      (ensure-equal :connect (http-error-operation condition))
      (ensure-equal (type-of 7) (http-connection-error-cause condition)))))

(deftest core-coverage-transport-connection-token-lists
  (dolist (case '(("HTTP/1.1" nil "upgrade, close" nil)
                  ("HTTP/1.1" "upgrade, close" nil nil)
                  ("HTTP/1.0" nil "other, keep-alive" t)
                  ("HTTP/1.0" nil "keep-alive, close" nil)
                  ("HTTP/1.0" nil nil nil)
                  ("HTTP/2" nil "keep-alive" nil)))
    (destructuring-bind (version request-connection response-connection expected) case
      (let ((request
              (make-http-request
               :method "GET" :uri "http://example.test/"
               :headers (when request-connection
                          (list (make-http-header "Connection" request-connection)))))
            (response
              (make-http-response
               :status 200 :protocol-version version
               :headers (append (list (make-http-header "Content-Length" "0"))
                                (when response-connection
                                  (list (make-http-header
                                         "Connection" response-connection)))))))
        (ensure-equal expected (http-response-reusable-p request response))))))

#+sbcl
(progn
  (deftest core-coverage-transport-continue-chunked-trailers
    (dolist (body '("ab" ""))
      (let* ((request
               (make-http-request
                :method "POST" :uri "http://example.test/upload"
                :headers (list (make-http-header "Expect" "100-continue"))
                :body (ascii body)
                :trailers (list (make-http-header "X-Checksum" "done"))))
             (stream
               (make-instance 'binary-test-stream
                              :input (ascii "HTTP/1.1 100 Continue|CRLF||CRLF|HTTP/1.1 204 No Content|CRLF||CRLF|")))
             (head (ascii "POST /upload HTTP/1.1|CRLF|Expect: 100-continue|CRLF|Host: example.test|CRLF|Transfer-Encoding: chunked|CRLF|Trailer: X-Checksum|CRLF||CRLF|"))
             (information-statuses nil))
        (unwind-protect
             (progn
               (multiple-value-bind (response reusable-p)
                   (send-http-request-over-open-stream
                    request stream
                    :on-information
                    (lambda (information)
                      (push (http-response-status information) information-statuses)
                      (ensure-equal head (binary-test-output stream))))
                 (ensure-equal 204 (http-response-status response))
                 (ensure-equal t reusable-p))
               (ensure-equal '(100) information-statuses)
               (ensure-equal
                (concatenate-octets
                 head
                 (ascii (if (string= body "")
                            "0|CRLF|X-Checksum: done|CRLF||CRLF|"
                            "2|CRLF|ab|CRLF|0|CRLF|X-Checksum: done|CRLF||CRLF|")))
                (binary-test-output stream))
               (ensure-true (open-stream-p stream)))
          (close stream)))))

  (deftest core-coverage-transport-stream-producer-boundaries
    (let* ((request (make-http-request :method "POST"
                                       :uri "http://example.test/upload"))
           (stream (make-instance
                    'binary-test-stream
                    :input (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|")))
           (chunk (vector :not-an-octet))
           (calls 0)
           (condition
             (handler-case
                 (send-http-request-over-open-stream
                  request stream
                  :request-body-function
                  (lambda (maximum-size)
                    (ensure-equal 65536 maximum-size)
                    (incf calls)
                    chunk))
               (http-protocol-error (condition) condition))))
      (ensure-true (typep condition 'http-protocol-error))
      (ensure-equal :body (http-error-operation condition))
      (ensure-equal (type-of chunk) (http-protocol-error-detail condition))
      (ensure-equal 1 calls)
      (ensure-equal 0 (binary-test-input-position stream))
      (ensure-equal
       (ascii "POST /upload HTTP/1.1|CRLF|Host: example.test|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|")
       (binary-test-output stream))
      (close stream))

    (dolist (body-length '(3 nil))
      (let* ((request (make-http-request
                       :method "POST"
                       :uri "http://example.test/upload"
                       :headers (list (make-http-header "Expect" "100-continue"))))
             (payload (ascii "abc"))
             (chunks (list payload nil))
             (calls 0)
             (producer (lambda (maximum-size)
                         (ensure-equal 65536 maximum-size)
                         (incf calls)
                         (pop chunks)))
             (head (ascii
                    (if body-length
                        "POST /upload HTTP/1.1|CRLF|Expect: 100-continue|CRLF|Host: example.test|CRLF|Content-Length: 3|CRLF||CRLF|"
                        "POST /upload HTTP/1.1|CRLF|Expect: 100-continue|CRLF|Host: example.test|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|")))
             (stream
               (make-instance
                'binary-test-stream
                :input (ascii "HTTP/1.1 100 Continue|CRLF||CRLF|HTTP/1.1 204 No Content|CRLF||CRLF|")))
             (statuses nil)
             (response nil)
             (reusable nil))
        (unwind-protect
             (progn
               (multiple-value-setq (response reusable)
                 (send-http-request-over-open-stream
                  request stream
                  :request-body-function producer
                  :request-body-length body-length
                  :on-information
                  (lambda (information)
                    (push (http-response-status information) statuses)
                    (ensure-equal head (binary-test-output stream)))))
               (ensure-equal 204 (http-response-status response))
               (ensure-true reusable)
               (ensure-equal '(100) (nreverse statuses))
               (ensure-equal 2 calls)
               (ensure-true (null chunks))
               (ensure-equal
                (concatenate-octets
                 head
                 (if body-length
                     payload
                     (ascii "3|CRLF|abc|CRLF|0|CRLF||CRLF|")))
                (binary-test-output stream)))
          (close stream))))))
