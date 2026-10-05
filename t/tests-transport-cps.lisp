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
