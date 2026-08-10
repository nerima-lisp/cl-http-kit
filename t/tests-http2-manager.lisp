(in-package #:http-kit/test)

#+sbcl
(progn
  (deftest http2-connection-manager-reuses-a-connection
    (let* ((stream (make-instance 'binary-session-stream
                                  :input
                                  (concatenate-octets
                                   (h2-response-wire (octets))
                                   (h2-frame 1 5 3 (octets #x88)))))
           (opened 0)
           (closed 0)
           (connection nil)
           (manager
             (http-kit/http2:make-http2-connection-manager
              :open-connection
              (lambda (request &key timeout deadline)
                (declare (ignore timeout deadline))
                (ensure-equal "GET" (http-request-method request))
                (incf opened)
                (setf connection
                      (http-kit/http2:make-http2-connection
                       :stream stream
                       :close-stream
                       (lambda (closed-stream)
                         (ensure-equal stream closed-stream)
                         (incf closed))))
                connection)))
           (first-response
             (http-kit/http2:send-http2-request-over-connection-manager
              manager
              (make-http-request :method "GET"
                                 :uri "https://127.0.0.1/first")))
           (second-response
             (http-kit/http2:send-http2-request-over-connection-manager
              manager
              (make-http-request :method "GET"
                                 :uri "https://127.0.0.1/second"))))
      (ensure-equal 200 (http-response-status first-response))
      (ensure-equal 200 (http-response-status second-response))
      (ensure-equal 1 opened)
      (ensure-equal 1 (http-kit/http2:http2-connection-manager-connection-count
                       manager))
      (ensure-true
       (http-kit/http2:http2-connection-manager-open-p manager))
      (http-kit/http2:close-http2-connection-manager manager)
      (ensure-equal 1 closed)
      (ensure-equal 0
                    (http-kit/http2:http2-connection-manager-connection-count
                     manager))
      (signals http-connection-error
        (http-kit/http2:send-http2-request-over-connection-manager
         manager
         (make-http-request :method "GET"
                            :uri "https://127.0.0.1/closed")))))

  (deftest http2-connection-manager-evicts-the-oldest-key
    (let* ((streams
             (list
              (make-instance 'binary-session-stream
                             :input (h2-response-wire (octets 1)))
              (make-instance 'binary-session-stream
                             :input (h2-response-wire (octets 2)))))
           (opened 0)
           (closed 0)
           (manager
             (http-kit/http2:make-http2-connection-manager
              :max-connections 1
              :open-connection
              (lambda (request &key timeout deadline)
                (declare (ignore timeout deadline))
                (ensure-equal "GET" (http-request-method request))
                (incf opened)
                (let ((stream (pop streams)))
                  (http-kit/http2:make-http2-connection
                   :stream stream
                   :close-stream (lambda (closed-stream)
                                   (declare (ignore closed-stream))
                                    (incf closed)))))))
           (first-response
             (http-kit/http2:send-http2-request-over-connection-manager
              manager
              (make-http-request :method "GET"
                                 :uri "https://one.test/")))
           (second-response
             (http-kit/http2:send-http2-request-over-connection-manager
              manager
              (make-http-request :method "GET"
                                 :uri "https://two.test/"))))
      (ensure-equal 200 (http-response-status first-response))
      (ensure-equal 200 (http-response-status second-response))
      (ensure-equal (octets 1) (http-response-body first-response))
      (ensure-equal (octets 2) (http-response-body second-response))
      (ensure-equal 2 opened)
      (ensure-equal 1 closed)
      (ensure-equal 1
                    (http-kit/http2:http2-connection-manager-connection-count
                     manager))
      (http-kit/http2:close-http2-connection-manager manager)
       (ensure-equal 2 closed)))

  (deftest http2-connection-manager-transport-keeps-default-keys
    (let* ((streams
             (list
              (make-instance 'binary-session-stream
                             :input (h2-response-wire (octets 3)))
              (make-instance 'binary-session-stream
                             :input (h2-response-wire (octets 4)))))
           (opened 0)
           (closed 0)
           (manager
             (http-kit/http2:make-http2-connection-manager
              :open-connection
              (lambda (request &key timeout deadline)
                (declare (ignore request timeout deadline))
                (incf opened)
                (let ((stream (pop streams)))
                  (http-kit/http2:make-http2-connection
                   :stream stream
                   :close-stream (lambda (closed-stream)
                                   (declare (ignore closed-stream))
                                   (incf closed)))))))
           (transport
             (http-kit/http2:make-http2-connection-manager-transport manager))
           (first-response
             (funcall transport
                      (make-http-request :method "GET"
                                         :uri "https://three.test/")))
           (second-response
             (funcall transport
                      (make-http-request :method "GET"
                                         :uri "https://four.test/"))))
      (ensure-equal 200 (http-response-status first-response))
      (ensure-equal 200 (http-response-status second-response))
      (ensure-equal (octets 3) (http-response-body first-response))
      (ensure-equal (octets 4) (http-response-body second-response))
      (ensure-equal 2 opened)
      (ensure-equal 2
                    (http-kit/http2:http2-connection-manager-connection-count
                     manager))
      (http-kit/http2:close-http2-connection-manager manager)
      (ensure-equal 2 closed))))
