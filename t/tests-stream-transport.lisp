(in-package #:http-kit/test-core)

#+sbcl
(progn
  (defclass binary-test-stream
      (sb-gray:fundamental-binary-input-stream
       sb-gray:fundamental-binary-output-stream)
    ((input
       :initarg :input
       :reader binary-test-input)
     (input-position
       :initform 0
       :accessor binary-test-input-position)
     (output
       :initform (make-array 0
                             :element-type '(unsigned-byte 8)
                             :adjustable t
                             :fill-pointer 0)
       :reader binary-test-output)))

  (defmethod sb-gray:stream-read-byte ((stream binary-test-stream))
    (let ((position (binary-test-input-position stream))
          (input (binary-test-input stream)))
      (if (< position (length input))
          (prog1 (aref input position)
            (incf (binary-test-input-position stream)))
          :eof)))

  (defmethod sb-gray:stream-write-byte
      ((stream binary-test-stream) byte)
    (vector-push-extend byte (binary-test-output stream))
    byte)

  (defmethod sb-gray:stream-read-sequence
      ((stream binary-test-stream) sequence &optional (start 0) end)
    (let* ((end (or end (length sequence)))
           (input (binary-test-input stream))
           (position (binary-test-input-position stream))
           (count (min (- end start) (- (length input) position))))
      (when (plusp count)
        (replace sequence input
                 :start1 start
                 :end1 (+ start count)
                 :start2 position
                 :end2 (+ position count))
        (incf (binary-test-input-position stream) count))
      (+ start count)))

  (defmethod sb-gray:stream-write-sequence
      ((stream binary-test-stream) sequence &optional (start 0) end)
    (let ((end (or end (length sequence)))
          (output (binary-test-output stream)))
      (loop for index from start below end
            do (vector-push-extend (aref sequence index) output)))
    sequence)

  (defmethod sb-gray:stream-finish-output ((stream binary-test-stream))
    (declare (ignore stream))
    nil)

  (deftest public-stream-request
    (let* ((request (make-http-request :method "GET"
                                       :uri "http://127.0.0.1/"))
           (stream (make-instance
                    'binary-test-stream
                    :input (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|abc")))
           (closed nil)
           (response
             (send-http-request-over-stream
              request
              :open-stream (lambda (received-request &key timeout deadline)
                             (declare (ignore timeout deadline))
                             (ensure-equal request received-request)
                             stream)
              :close-stream (lambda (closed-stream)
                              (ensure-equal stream closed-stream)
                              (setf closed t)))))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "abc" (octets-as-string (http-response-body response)))
      (ensure-true closed)
      (ensure-equal (ascii "GET / HTTP/1.1|CRLF|Host: 127.0.0.1|CRLF|Content-Length: 0|CRLF||CRLF|")
                    (binary-test-output stream))))

  (deftest public-stream-request-cps
    (let* ((request (make-http-request :method "GET"
                                       :uri "http://127.0.0.1/"))
           (stream (make-instance
                    'binary-test-stream
                    :input (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|")))
           (status nil)
           (closed nil))
      (ensure-equal :success
                    (send-http-request-over-stream/cps
                     request
                     (lambda (response)
                       (setf status (http-response-status response))
                       :success)
                     :on-error (lambda (condition)
                                 (declare (ignore condition))
                                 :unexpected)
                     :open-stream (lambda (received-request &key timeout deadline)
                                    (declare (ignore timeout deadline))
                                    (ensure-equal request received-request)
                                    stream)
                     :close-stream (lambda (closed-stream)
                                     (ensure-equal stream closed-stream)
                                     (setf closed t))))
      (ensure-equal 204 status)
      (ensure-true closed)))

  (deftest public-stream-switching-protocol
    (let* ((request (make-http-request :method "GET"
                                       :uri "http://127.0.0.1/"))
           (prefix (ascii "HTTP/1.1 101 Switching Protocols|CRLF|Connection: Upgrade|CRLF|Upgrade: websocket|CRLF||CRLF|"))
           (tail (ascii "UPGRADED-BYTES"))
           (stream (make-instance
                    'binary-test-stream
                    :input (concatenate-octets prefix tail)))
           (response nil)
           (reusable nil))
      (multiple-value-setq (response reusable)
        (send-http-request-over-open-stream request stream))
      (ensure-equal 101 (http-response-status response))
      (ensure-equal "HTTP/1.1"
                    (http-response-protocol-version response))
      (ensure-equal (octets) (http-response-body response))
      (ensure-true (not reusable))
      (ensure-equal (length prefix)
                    (binary-test-input-position stream)
                    "101 leaves upgraded bytes unread")
      (ensure-equal (ascii "GET / HTTP/1.1|CRLF|Host: 127.0.0.1|CRLF|Content-Length: 0|CRLF||CRLF|")
                    (binary-test-output stream))))

  (deftest public-stream-request-body-producer-fixed-length
    (let* ((request (make-http-request :method "POST"
                                       :uri "http://127.0.0.1/"))
           (stream (make-instance
                    'binary-test-stream
                    :input (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|")))
           (chunks (list (octets 97 98 99)
                         (octets 100 101)
                         nil))
           (calls 0)
           (response
             (send-http-request-over-open-stream
              request stream
              :request-body-function
              (lambda (maximum-size)
                (ensure-equal 65536 maximum-size)
                (incf calls)
                (pop chunks))
              :request-body-length 5)))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal 3 calls)
      (ensure-true (null chunks))
      (ensure-equal
       (ascii "POST / HTTP/1.1|CRLF|Host: 127.0.0.1|CRLF|Content-Length: 5|CRLF||CRLF|abcde")
       (binary-test-output stream))))

  (deftest public-stream-request-body-producer-chunked
    (let* ((request (make-http-request :method "POST"
                                       :uri "http://127.0.0.1/"))
           (stream (make-instance
                    'binary-test-stream
                    :input (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|")))
           (chunks (list (octets 97 98 99)
                         (octets 100 101)
                         nil))
           (response
             (send-http-request-over-open-stream
              request stream
              :request-body-function
              (lambda (maximum-size)
                (ensure-equal 65536 maximum-size)
                (pop chunks)))))
      (ensure-equal 200 (http-response-status response))
      (ensure-true (null chunks))
      (ensure-equal
       (ascii "POST / HTTP/1.1|CRLF|Host: 127.0.0.1|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|3|CRLF|abc|CRLF|2|CRLF|de|CRLF|0|CRLF||CRLF|")
       (binary-test-output stream))))

  (deftest public-stream-expect-continue-in-memory-body
    (let* ((request (make-http-request :method "POST"
                                       :uri "http://127.0.0.1/"
                                       :headers
                                       (list (make-http-header
                                              "Expect" "100-continue"))
                                       :body (octets 97 98 99)))
           (stream (make-instance
                    'binary-test-stream
                    :input
                    (ascii "HTTP/1.1 100 Continue|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|")))
           (statuses '())
           (response nil)
           (reusable nil))
      (multiple-value-setq (response reusable)
        (send-http-request-over-open-stream
         request stream
         :on-information
         (lambda (information)
           (push (http-response-status information) statuses))))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal '(100) (nreverse statuses))
      (ensure-true reusable)
      (ensure-equal
       (ascii "POST / HTTP/1.1|CRLF|Expect: 100-continue|CRLF|Host: 127.0.0.1|CRLF|Content-Length: 3|CRLF||CRLF|abc")
       (binary-test-output stream))))

  (deftest public-stream-expect-continue-final-response
    (let* ((request (make-http-request :method "POST"
                                       :uri "http://127.0.0.1/"
                                       :headers
                                       (list (make-http-header
                                              "Expect" "100-continue"))
                                       :body (octets 97 98 99)))
           (stream (make-instance
                    'binary-test-stream
                    :input
                    (ascii "HTTP/1.1 417 Expectation Failed|CRLF|Content-Length: 0|CRLF||CRLF|")))
           (response nil)
           (reusable nil))
      (multiple-value-setq (response reusable)
        (send-http-request-over-open-stream request stream))
      (ensure-equal 417 (http-response-status response))
      (ensure-true (not reusable))
      (ensure-equal
       (ascii "POST / HTTP/1.1|CRLF|Expect: 100-continue|CRLF|Host: 127.0.0.1|CRLF|Content-Length: 3|CRLF||CRLF|")
       (binary-test-output stream))))

  (deftest public-stream-request-body-producer-length-mismatch
    (let* ((request (make-http-request :method "POST"
                                       :uri "http://127.0.0.1/"))
           (stream (make-instance
                    'binary-test-stream
                    :input (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|"))))
      (signals http-protocol-error
        (send-http-request-over-open-stream
         request stream
         :request-body-function
         (lambda (maximum-size)
           (declare (ignore maximum-size))
           (octets 97 98 99))
         :request-body-length 5))))

  (deftest public-stream-request-cps-error
    (let ((condition-type nil))
      (ensure-equal :failure
                    (send-http-request-over-stream/cps
                     (make-http-request :method "GET"
                                        :uri "http://127.0.0.1/")
                     (lambda (response)
                       (declare (ignore response))
                       :unexpected)
                     :on-error (lambda (condition)
                                 (setf condition-type (type-of condition))
                                 :failure)
                     :open-stream (lambda (request &key timeout deadline)
                                    (declare (ignore request timeout deadline))
                                    (error "synthetic open failure"))))
      (ensure-equal 'http-connection-error condition-type)))

  (deftest stream-request-errors
    (let ((closed nil))
      (signals http-connection-error
        (send-http-request-over-stream
         (make-http-request :method "GET" :uri "http://127.0.0.1/")
         :open-stream (lambda (request &key timeout deadline)
                        (declare (ignore request timeout deadline))
                        7)
         :close-stream (lambda (stream)
                         (ensure-equal 7 stream)
                         (setf closed t))))
      (ensure-true closed))
    (signals http-connection-error
      (send-http-request-over-stream
       (make-http-request :method "GET" :uri "http://127.0.0.1/")
       :open-stream (lambda (request &key timeout deadline)
                      (declare (ignore request timeout deadline))
                      (error "synthetic open failure"))))))
