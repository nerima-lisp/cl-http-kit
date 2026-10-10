(in-package #:http-kit/test)

(deftest public-http2-transport-boundaries-require-state
  (let ((request (make-http-request :method "GET"
                                    :uri "http://127.0.0.1/")))
    (signals http-protocol-error
      (http-kit/http2:send-http2-request nil request))))

(deftest http2-request-cps-success
  (let* ((request (make-http-request :method "GET"
                                     :uri "https://127.0.0.1/"))
         (client
           (http-kit/http2:make-http2-client
            :exchange (lambda (received-request wire &key timeout deadline)
                        (declare (ignore timeout deadline))
                        (ensure-equal request received-request)
                        (ensure-true (plusp (array-total-size wire)))
                        (h2-response-wire (octets 0 #xff)))))
         (status nil))
    (ensure-equal :success
                  (http-kit/http2:send-http2-request/cps
                   client request
                   (lambda (response)
                     (setf status (http-response-status response))
                     :success)))
    (ensure-equal 200 status)))

(deftest http2-request-cps-error
  (let* ((request (make-http-request :method "GET"
                                     :uri "https://127.0.0.1/"))
         (client
           (http-kit/http2:make-http2-client
            :exchange (lambda (received-request wire &key timeout deadline)
                        (declare (ignore received-request wire timeout deadline))
                        (error "synthetic HTTP/2 exchange failure"))))
         (condition-type nil))
    (ensure-equal :failure
                  (http-kit/http2:send-http2-request/cps
                   client request
                   (lambda (response)
                     (declare (ignore response))
                     :unexpected)
                   :on-error (lambda (condition)
                               (setf condition-type (type-of condition))
                               :failure)))
    (ensure-equal 'http-connection-error condition-type)))

#+sbcl
(deftest stream-cps-boundary-invalid-continuations-have-no-io
  (let ((request (make-http-request :method "POST"
                                    :uri "http://127.0.0.1/"))
        (opens 0)
        (closes 0)
        (produces 0))
    (dolist (invalid '(nil 7))
      (let ((condition
              (handler-case
                  (send-http-request-over-stream/cps
                   request
                   (if invalid #'identity nil)
                   :on-error invalid
                   :open-stream
                   (lambda (received &key timeout deadline)
                     (declare (ignore received timeout deadline))
                     (incf opens))
                   :close-stream (lambda (stream)
                                   (declare (ignore stream))
                                   (incf closes))
                   :request-body-function
                   (lambda (maximum)
                     (declare (ignore maximum))
                     (incf produces)))
                (http-protocol-error (condition) condition))))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :transport (http-error-operation condition))
        (ensure-equal invalid (http-protocol-error-detail condition))))
    (ensure-equal 0 opens)
    (ensure-equal 0 closes)
    (ensure-equal 0 produces)))

#+sbcl
(deftest stream-cps-boundary-success-callback-after-close
  (let ((request (make-http-request :method "GET"
                                    :uri "http://127.0.0.1/")))
    (dolist (fail-p '(nil t))
      (let ((stream (make-instance 'binary-test-stream
                                  :input (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|abc")))
            (marker (make-condition 'http-protocol-error
                                    :message "Synthetic continuation failure."
                                    :operation :transport
                                    :detail :continuation))
            (events '())
            (errors 0))
        (let ((result
                (handler-case
                    (multiple-value-list
                     (send-http-request-over-stream/cps
                      request
                      (lambda (response)
                        (push :success events)
                        (ensure-equal '(:success :close :open) events)
                        (ensure-equal 200 (http-response-status response))
                        (ensure-octets-equal (ascii "abc")
                                             (http-response-body response))
                        (if fail-p
                            (error marker)
                            (values :done :secondary)))
                      :on-error (lambda (condition)
                                  (declare (ignore condition))
                                  (incf errors))
                      :open-stream
                      (lambda (received &key timeout deadline)
                        (declare (ignore timeout deadline))
                        (ensure-true (eq request received))
                        (push :open events)
                        stream)
                      :close-stream (lambda (received)
                                      (ensure-true (eq stream received))
                                      (push :close events))))
                  (http-protocol-error (condition) condition))))
          (if fail-p
              (ensure-true (eq marker result))
              (ensure-equal '(:done :secondary) result)))
        (ensure-equal '(:success :close :open) events)
        (ensure-equal 0 errors)
        (ensure-octets-equal
         (ascii "GET / HTTP/1.1|CRLF|Host: 127.0.0.1|CRLF|Content-Length: 0|CRLF||CRLF|")
         (binary-test-output stream))))))

#+sbcl
(deftest stream-cps-boundary-producer-error-after-close
  (let ((request (make-http-request :method "POST"
                                    :uri "http://127.0.0.1/")))
    (dolist (length '(1 2))
      (let ((stream (make-instance 'binary-test-stream
                                  :input (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|")))
            (chunks (list (octets 97) nil))
            (events '())
            (produces 0))
        (ensure-equal
         (if (= length 1) '(:success 204) '(:failure :body))
         (multiple-value-list
          (send-http-request-over-stream/cps
           request
           (lambda (response)
             (push :success events)
             (ensure-equal '(:success :close :open) events)
             (values :success (http-response-status response)))
           :on-error
           (lambda (condition)
             (push :error events)
             (ensure-equal '(:error :close :open) events)
             (ensure-true (typep condition 'http-protocol-error))
             (ensure-equal :body (http-error-operation condition))
             (ensure-equal '(:expected 2 :observed 1)
                           (http-protocol-error-detail condition))
             (ensure-equal 0
                           (http-kit/test-core::binary-test-input-position stream))
             (values :failure (http-error-operation condition)))
           :open-stream
           (lambda (received &key timeout deadline)
             (declare (ignore timeout deadline))
             (ensure-true (eq request received))
             (push :open events)
             stream)
           :close-stream (lambda (received)
                           (ensure-true (eq stream received))
                           (push :close events))
           :request-body-function
           (lambda (maximum)
             (ensure-equal 65536 maximum)
             (incf produces)
             (pop chunks))
           :request-body-length length)))
        (ensure-equal 2 produces)
        (ensure-true (null chunks))
        (ensure-equal (if (= length 1) '(:success :close :open)
                         '(:error :close :open))
                      events)
        (ensure-octets-equal
         (ascii (format nil "POST / HTTP/1.1|CRLF|Host: 127.0.0.1|CRLF|Content-Length: ~D|CRLF||CRLF|a" length))
         (binary-test-output stream))))))
