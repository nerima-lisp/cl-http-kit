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

  (deftest http2-connection-manager-expires-idle-connections
    (let* ((now 0)
           (clock (lambda () now))
           (streams
             (list
              (make-instance 'binary-session-stream
                             :input
                             (concatenate-octets
                              (h2-response-wire (octets 1))
                              (h2-frame 1 5 3 (octets #x88))))
              (make-instance 'binary-session-stream
                             :input (h2-response-wire (octets 2)))))
           (opened 0)
           (closed 0)
           (manager
             (http-kit/http2:make-http2-connection-manager
              :idle-timeout 5
              :clock-function clock
              :open-connection
              (lambda (request &key timeout deadline)
                (declare (ignore request timeout deadline))
                (incf opened)
                (http-kit/http2:make-http2-connection
                 :stream (pop streams)
                 :close-stream
                 (lambda (stream)
                   (declare (ignore stream))
                   (incf closed)))))))
      (ensure-equal 5
                    (http-kit/http2:http2-connection-manager-idle-timeout
                     manager))
      (ensure-true
       (eq (http-kit/http2:http2-connection-manager-clock-function manager)
           clock))
      (http-kit/http2:send-http2-request-over-connection-manager
       manager
       (make-http-request :method "GET" :uri "https://idle.test/first"))
      (setf now 4)
      (http-kit/http2:send-http2-request-over-connection-manager
       manager
       (make-http-request :method "GET" :uri "https://idle.test/second"))
      (ensure-equal 1 opened)
      (ensure-equal 0 closed)
      (setf now 9)
      (let ((response
              (http-kit/http2:send-http2-request-over-connection-manager
               manager
               (make-http-request :method "GET"
                                  :uri "https://idle.test/third"))))
        (ensure-equal (octets 2) (http-response-body response)))
      (ensure-equal 2 opened)
      (ensure-equal 1 closed)
      (http-kit/http2:close-http2-connection-manager manager)
      (ensure-equal 2 closed)))

  (deftest http2-connection-manager-validates-idle-policy
    (let ((opener
            (lambda (request &key timeout deadline)
              (declare (ignore request timeout deadline))
              nil)))
      (signals http-protocol-error
        (http-kit/http2:make-http2-connection-manager
         :open-connection opener
         :idle-timeout -1))
      (signals http-protocol-error
        (http-kit/http2:make-http2-connection-manager
         :open-connection opener
         :idle-timeout "later"))
      (signals http-protocol-error
        (http-kit/http2:make-http2-connection-manager
         :open-connection opener
         :max-connection-age -1))
      (signals http-protocol-error
        (http-kit/http2:make-http2-connection-manager
         :open-connection opener
         :max-connection-age "later"))
      (signals http-protocol-error
        (http-kit/http2:make-http2-connection-manager
         :open-connection opener
         :clock-function 7))
      (let ((manager
              (http-kit/http2:make-http2-connection-manager
               :open-connection opener
               :idle-timeout 1
               :clock-function (lambda () "now"))))
        (signals http-protocol-error
          (http-kit/http2:send-http2-request-over-connection-manager
           manager
           (make-http-request :method "GET"
                              :uri "https://idle.test/invalid-clock"))))))

  (deftest http2-connection-manager-expires-old-active-connections
    (let* ((now 0)
           (streams
             (list
              (make-instance 'binary-session-stream
                             :input
                             (concatenate-octets
                              (h2-response-wire (octets 1))
                              (h2-frame 1 5 3 (octets #x88))))
              (make-instance 'binary-session-stream
                             :input (h2-response-wire (octets 2)))))
           (opened 0)
           (closed 0)
           (manager
             (http-kit/http2:make-http2-connection-manager
              :max-connection-age 5
              :clock-function (lambda () now)
              :open-connection
              (lambda (request &key timeout deadline)
                (declare (ignore request timeout deadline))
                (incf opened)
                (http-kit/http2:make-http2-connection
                 :stream (pop streams)
                 :close-stream
                 (lambda (stream)
                   (declare (ignore stream))
                   (incf closed)))))))
      (dolist (time '(0 4 5))
        (setf now time)
        (ensure-equal
         200
         (http-response-status
          (http-kit/http2:send-http2-request-over-connection-manager
           manager
           (make-http-request :method "GET"
                              :uri "https://age.test/")))))
      (ensure-equal
       5
       (http-kit/http2:http2-connection-manager-max-connection-age manager))
      (ensure-equal 2 opened)
      (ensure-equal 1 closed)
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
      (ensure-equal 2 closed)))

  (deftest http2-connection-manager-retries-safe-request-after-goaway
    (let* ((streams
             (list
              (make-instance
               'binary-session-stream
               :input
               (concatenate-octets
                (h2-frame 4 0 0 (octets))
                (h2-frame 7 0 0 (octets 0 0 0 0 0 0 0 0))))
              (make-instance 'binary-session-stream
                             :input (h2-response-wire (octets 9)))))
           (opened 0)
           (manager
             (http-kit/http2:make-http2-connection-manager
              :open-connection
              (lambda (request &key timeout deadline)
                (declare (ignore request timeout deadline))
                (incf opened)
                (http-kit/http2:make-http2-connection
                 :stream (pop streams)
                 :close-stream (lambda (stream) (declare (ignore stream)))))))
           (response
             (http-kit/http2:send-http2-request-over-connection-manager
              manager
              (make-http-request :method "GET"
                                 :uri "https://retry.test/"))))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal (octets 9) (http-response-body response))
      (ensure-equal 2 opened)
      (http-kit/http2:close-http2-connection-manager manager)))

  (deftest http2-connection-manager-retries-unprocessed-post-after-goaway
    (let* ((streams
             (list
              (make-instance
               'binary-session-stream
               :input
               (concatenate-octets
                (h2-frame 4 0 0 (octets))
                (h2-frame 7 0 0 (octets 0 0 0 0 0 0 0 0))))
              (make-instance 'binary-session-stream
                             :input (h2-response-wire (octets 8)))))
           (opened 0)
           (manager
             (http-kit/http2:make-http2-connection-manager
              :open-connection
              (lambda (request &key timeout deadline)
                (declare (ignore request timeout deadline))
                (incf opened)
                (http-kit/http2:make-http2-connection
                 :stream (pop streams)
                 :close-stream (lambda (value) (declare (ignore value))))))))
      (let ((response
              (http-kit/http2:send-http2-request-over-connection-manager
               manager
               (make-http-request :method "POST"
                                  :uri "https://retry.test/"
                                  :body (octets 1 2 3)))))
        (ensure-equal 200 (http-response-status response))
        (ensure-equal (octets 8) (http-response-body response)))
      (ensure-equal 2 opened)
      (http-kit/http2:close-http2-connection-manager manager)))

  (deftest http2-connection-manager-retries-refused-stream
    (let* ((stream
             (make-instance
              'binary-session-stream
              :input
              (concatenate-octets
               (h2-frame 4 0 0 (octets))
               (h2-frame 3 0 1 (octets 0 0 0 7))
               (h2-frame 1 5 3 (octets #x88)))))
           (opened 0)
           (manager
             (http-kit/http2:make-http2-connection-manager
              :open-connection
              (lambda (request &key timeout deadline)
                (declare (ignore request timeout deadline))
                (incf opened)
                (http-kit/http2:make-http2-connection
                 :stream stream
                 :close-stream (lambda (value) (declare (ignore value))))))))
      (let ((response
              (http-kit/http2:send-http2-request-over-connection-manager
               manager
               (make-http-request :method "POST"
                                  :uri "https://retry.test/"))))
        (ensure-equal 200 (http-response-status response)))
      (ensure-equal 1 opened)
      (http-kit/http2:close-http2-connection-manager manager)))

  (deftest http2-connection-manager-does-not-retry-body-producer
    (let* ((stream
             (make-instance
              'binary-session-stream
              :input
              (concatenate-octets
               (h2-frame 4 0 0 (octets))
               (h2-frame 3 0 1 (octets 0 0 0 7)))))
           (opened 0)
           (producer-calls 0)
           (manager
             (http-kit/http2:make-http2-connection-manager
              :open-connection
              (lambda (request &key timeout deadline)
                (declare (ignore request timeout deadline))
                (incf opened)
                (http-kit/http2:make-http2-connection
                 :stream stream
                 :close-stream (lambda (value) (declare (ignore value))))))))
      (signals http-connection-error
        (http-kit/http2:send-http2-request-over-connection-manager
         manager
         (make-http-request :method "POST"
                            :uri "https://retry.test/")
         :request-body-function
         (lambda (maximum-size)
           (declare (ignore maximum-size))
           (incf producer-calls)
           nil)))
      (ensure-equal 1 opened)
      (ensure-equal 1 producer-calls)
      (http-kit/http2:close-http2-connection-manager manager))))
