(in-package #:http-kit/test-core)

(deftest runtime-boundaries-recording-request-snapshot
  (dolist (explicit-p '(t nil))
    (let* ((target (copy-seq "/recorded"))
           (body (ascii "abc"))
           (header-name (copy-seq "X-Original"))
           (header-content (copy-seq "kept"))
           (trailer-name (copy-seq "X-Checksum"))
           (trailer (copy-seq "old"))
           (request (make-http-request
                     :method "POST" :uri "http://example.test/derived?x=1"
                     :protocol-version (copy-seq "HTTP/1.1")
                     :request-target (when explicit-p target)
                     :headers (list (make-http-header header-name header-content))
                     :body body :trailers (list (make-http-header trailer-name trailer))))
           (calls 0)
           (session
             (make-recording-session
              :response-function
              (lambda (received &key timeout deadline)
                (declare (ignore timeout deadline))
                (incf calls)
                (ensure-true (eq request received))
                (setf (char target 1) #\X
                      (aref body 0) 0
                      (char header-name 2) #\Z
                      (char header-content 0) #\X
                      (char trailer-name 2) #\Z
                      (char trailer 0) #\X
                      (char (http-request-protocol-version received) 7) #\0)
                (let ((copied-target (http-request-target received))
                      (copied-body (http-request-body received))
                      (copied-trailer (http-header-content
                                       (first (http-request-trailers received)))))
                  (setf (char copied-target 1) #\X
                        (aref copied-body 0) 0
                        (char copied-trailer 0) #\X))
                (make-http-response :status 204)))))
      (ensure-equal 204 (http-response-status (send-recorded-http-request session request)))
      (ensure-equal 1 calls)
      (ensure-equal 1 (length (recording-session-requests session)))
      (ensure-equal 1 (length (recording-session-responses session)))
      (let ((snapshot (first (recording-session-requests session))))
        (ensure-false (eq snapshot request))
        (ensure-equal (if explicit-p "/recorded" "/derived?x=1")
                      (http-request-target snapshot))
        (ensure-equal (ascii "abc") (http-request-body snapshot))
        (ensure-equal "kept" (http-header-value (http-request-headers snapshot) "x-original"))
        (ensure-equal "old" (http-header-value (http-request-trailers snapshot) "x-checksum"))
        (ensure-equal "HTTP/1.1" (http-request-protocol-version snapshot))
        (ensure-equal (http-request-target snapshot) (http-request-target request))
        (ensure-equal (http-request-body snapshot) (http-request-body request))))))

(deftest runtime-boundaries-recording-response-snapshot
  (dolist (mutate-returned-p '(nil t))
    (let* ((response (make-http-response
                      :status 200 :reason (copy-seq "Fine")
                      :protocol-version (copy-seq "HTTP/1.1")
                      :headers (list (make-http-header (copy-seq "X-Reply") (copy-seq "kept")))
                      :trailers (list (make-http-header (copy-seq "X-Checksum") (copy-seq "old")))
                      :body (ascii "abc")))
           (session (make-recording-session :responses (list response)))
           (returned (send-recorded-http-request
                      session (make-http-request :method "GET" :uri "http://example.test/")))
           (changed (if mutate-returned-p returned response)))
      (ensure-false (eq response returned))
      (ensure-equal 200 (http-response-status returned))
      (ensure-equal (ascii "abc") (http-response-body returned))
      (let ((header (first (http-response-headers changed)))
            (trailer (first (http-response-trailers changed)))
            (body (http-response-body changed)))
        (setf (char (http-header-name header) 2) #\Z
              (char (http-header-content header) 0) #\X
              (char (http-header-name trailer) 2) #\Z
              (char (http-header-content trailer) 0) #\X
              (char (http-response-protocol-version changed) 7) #\0
              (char (http-response-reason changed) 0) #\X
              (aref body 0) 0))
      (ensure-equal 1 (length (recording-session-requests session)))
      (ensure-equal 1 (length (recording-session-responses session)))
      (let ((snapshot (first (recording-session-responses session)))
            (unchanged (if mutate-returned-p response returned)))
        (ensure-false (eq snapshot returned))
        (dolist (value (list snapshot unchanged))
          (ensure-equal "kept" (http-header-value (http-response-headers value) "x-reply"))
          (ensure-equal "old" (http-header-value (http-response-trailers value) "x-checksum"))
          (ensure-equal "HTTP/1.1" (http-response-protocol-version value))
          (ensure-equal "Fine" (http-response-reason value))
          (ensure-equal (ascii "abc") (http-response-body value)))))))

#+sbcl
(deftest runtime-boundaries-expect-producer-framing
  (dolist (known-length '(3 nil))
    (let* ((input (ascii "HTTP/1.1 103 Early Hints|CRLF||CRLF|HTTP/1.1 100 Continue|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok!"))
           (stream (make-instance 'binary-test-stream :input input))
           (head (ascii (if known-length
                            "POST / HTTP/1.1|CRLF|Expect: 100-continue|CRLF|Host: example.test|CRLF|Content-Length: 3|CRLF||CRLF|"
                            "POST / HTTP/1.1|CRLF|Expect: 100-continue|CRLF|Host: example.test|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|")))
           (calls 0)
           (statuses nil))
      (unwind-protect
           (multiple-value-bind (response reusable-p)
               (send-http-request-over-open-stream
                (make-http-request :method "POST" :uri "http://example.test/"
                                   :headers (list (make-http-header "Expect" "100-continue"))) stream
                :request-body-length known-length
                :request-body-function (lambda (maximum)
                                         (ensure-equal 65536 maximum)
                                         (incf calls)
                                         (when (= calls 1) (ascii "abc")))
                :on-information (lambda (information)
                                  (ensure-equal 0 calls)
                                  (ensure-equal head (binary-test-output stream))
                                  (push (http-response-status information) statuses)))
             (ensure-equal '(103 100) (reverse statuses))
             (ensure-equal 2 calls)
             (ensure-equal 200 (http-response-status response))
             (ensure-equal (ascii "ok") (http-response-body response))
             (ensure-true reusable-p)
             (ensure-equal (concatenate-octets
                            head (ascii (if known-length "abc" "3|CRLF|abc|CRLF|0|CRLF||CRLF|")))
                           (binary-test-output stream))
             (ensure-equal (1- (length input)) (binary-test-input-position stream))
             (ensure-equal 33 (read-byte stream))
             (ensure-true (open-stream-p stream)))
        (close stream))
      (ensure-false (open-stream-p stream)))))

#+sbcl
(deftest runtime-boundaries-parser-generic-octet-vectors
  (dolist (request-p '(t nil))
    (let ((callbacks 0)
          (parser (if request-p #'parse-http-request #'parse-http-response)))
      (dolist (invalid '(#(0 -1) #(0 256) #(0 1/2)))
        (let ((caught (handler-case
                          (funcall parser invalid :on-body-chunk
                                   (lambda (chunk) (declare (ignore chunk)) (incf callbacks)))
                        (http-protocol-error (condition) condition))))
          (ensure-true (typep caught 'http-protocol-error))
          (ensure-equal (if request-p :request-parse :response-parse)
                        (http-error-operation caught))
          (ensure-equal (type-of invalid) (http-protocol-error-detail caught))
          (ensure-equal 0 callbacks)))
      (let* ((wire (ascii (if request-p
                             "POST / HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 3|CRLF||CRLF|abc"
                             "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|abc")))
             (chunks nil))
        (let ((message (funcall parser (map 'vector #'identity wire)
                                :on-body-chunk (lambda (chunk) (push (copy-seq chunk) chunks)))))
          (ensure-equal (ascii "abc") (if request-p (http-request-body message)
                                         (http-response-body message)))
          (ensure-equal (list (ascii "abc")) (reverse chunks)))
        (let ((stream (make-instance 'binary-test-stream
                                    :input (concatenate-octets wire #(33)))))
          (unwind-protect
               (let ((message (funcall parser stream)))
                 (ensure-equal (ascii "abc") (if request-p (http-request-body message)
                                                (http-response-body message)))
                 (ensure-equal (length wire) (binary-test-input-position stream))
                 (ensure-equal 33 (read-byte stream)))
            (close stream))
          (ensure-false (open-stream-p stream)))))))

#+sbcl
(deftest runtime-boundaries-producer-generic-octet-vectors
  (dolist (payload '(#(0 -1) #(0 256) #(0 1/2) #(97 98 99)))
    (let* ((input (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|"))
           (stream (make-instance 'binary-test-stream :input input))
           (calls 0)
           (head (ascii "POST / HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 3|CRLF||CRLF|"))
           (valid-p (equalp payload #(97 98 99))))
      (unwind-protect
           (let ((result
                   (handler-case
                       (send-http-request-over-open-stream
                        (make-http-request :method "POST" :uri "http://example.test/") stream
                        :request-body-length 3
                        :request-body-function (lambda (maximum)
                                                 (ensure-equal 65536 maximum)
                                                 (incf calls)
                                                 (when (= calls 1) payload)))
                     (http-protocol-error (condition) condition))))
             (if valid-p
                 (progn
                   (ensure-equal 200 (http-response-status result))
                   (ensure-equal (ascii "") (http-response-body result))
                   (ensure-equal 2 calls)
                   (ensure-equal (length input) (binary-test-input-position stream))
                   (ensure-equal (concatenate-octets head (ascii "abc")) (binary-test-output stream)))
                 (progn
                   (ensure-true (typep result 'http-protocol-error))
                   (ensure-equal :body (http-error-operation result))
                   (ensure-equal (type-of payload) (http-protocol-error-detail result))
                   (ensure-equal 1 calls)
                   (ensure-equal 0 (binary-test-input-position stream))
                   (ensure-equal head (binary-test-output stream))))
             (ensure-true (open-stream-p stream)))
        (close stream))
      (ensure-false (open-stream-p stream)))))

(deftest runtime-boundaries-ipvfuture-empty-address
  (let* ((input "[v12.]")
         (caught (handler-case (make-http-uri :authority input)
                   (http-invalid-uri (condition) condition))))
    (ensure-true (typep caught 'http-invalid-uri))
    (ensure-equal :uri (http-error-operation caught))
    (ensure-equal input (http-invalid-uri-input caught)))
  (let ((request (make-http-request :method "GET" :uri "http://[v12.x]/")))
    (ensure-equal "[v12.x]" (http-uri-authority (http-request-uri request)))
    (ensure-equal (ascii "GET / HTTP/1.1|CRLF|Host: [v12.x]|CRLF|Content-Length: 0|CRLF||CRLF|")
                  (serialize-http-request request))))

(deftest runtime-helper-client-parse-integer-bounds-preserve-input
  (let* ((input (copy-seq "123"))
         (before (copy-seq input)))
    (ensure-equal 123 (http-kit/client::%client-parse-integer input))
    (ensure-equal 23
                  (http-kit/client::%client-parse-integer input :start 1))
    (ensure-equal before input)
    (dolist (bounds (list (list -1 2) (list 2 1) (list 0 4) (list 1 1)))
      (destructuring-bind (start end) bounds
        (ensure-equal
         nil
         (http-kit/client::%client-parse-integer
          input :start start :end end))
        (ensure-equal before input))))
  (let* ((input (copy-seq "x12y"))
         (before (copy-seq input)))
    (ensure-equal 12 (http-kit/client::%client-parse-integer input :start 1 :end 3))
    (ensure-equal before input)))
