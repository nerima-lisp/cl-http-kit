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
