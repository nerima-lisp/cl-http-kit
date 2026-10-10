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

#+sbcl
(progn
  (deftest http2-connection-manager-boundary-batch-reuse-and-goaway
    (dolist (goaway-p '(nil t))
      (let* ((first-request (make-http-request :method "GET" :uri "https://batch.test/first"))
             (requests (list first-request
                             (make-http-request :method "GET" :uri "https://batch.test/second")))
             (first-stream
               (make-instance
                'binary-session-stream
                :input (concatenate-octets
                        (h2-frame 4 0 0 (octets))
                        (if goaway-p
                            (h2-frame 7 0 0 (octets 0 0 0 3 0 0 0 0))
                            (octets))
                        (h2-frame 1 5 3 (h2-header-block (cons ":status" "201")))
                        (h2-frame 1 5 1 (octets #x88))
                        (if goaway-p (octets) (h2-frame 1 5 5 (octets #x88))))))
             (replacement (make-instance 'binary-session-stream
                                         :input (h2-response-wire (octets 9))))
             (opened 0) (closed 0) (connections nil)
             (manager
               (http-kit/http2:make-http2-connection-manager
                :open-connection
                (lambda (request &key timeout deadline)
                  (when (zerop opened) (ensure-true (eq request first-request)))
                  (ensure-equal 20 timeout)
                  (ensure-equal 10 deadline)
                  (let* ((stream (if (zerop opened) first-stream replacement))
                         (connection
                           (http-kit/http2:make-http2-connection
                            :stream stream
                            :close-stream (lambda (value)
                                            (ensure-true (eq stream value))
                                            (incf closed)))))
                    (incf opened)
                    (push connection connections)
                    connection)))))
        (unwind-protect
             (progn
               (let ((responses
                       (http-kit/http2:send-http2-requests-over-connection-manager
                        manager requests :connection-key nil
                        :timeout 20 :deadline 10 :clock-function (lambda () 0))))
                 (ensure-equal '(200 201) (mapcar #'http-response-status responses))
                 (dolist (response responses)
                   (ensure-equal (octets) (http-response-body response))))
               (ensure-equal (if goaway-p 0 1)
                             (http-kit/http2:http2-connection-manager-connection-count manager))
               (ensure-equal (if goaway-p 1 0) closed)
               (ensure-equal (not goaway-p)
                             (http-kit/http2:http2-connection-open-p (first connections)))
               (let ((response
                       (funcall (http-kit/http2:make-http2-connection-manager-transport manager)
                                (make-http-request :method "GET" :uri "https://other.test/next")
                                :connection-key nil :timeout 20 :deadline 10
                                :clock-function (lambda () 0))))
                 (ensure-equal 200 (http-response-status response))
                 (ensure-equal (if goaway-p (octets 9) (octets))
                               (http-response-body response)))
               (ensure-equal (if goaway-p 2 1) opened)
               (ensure-equal 1 (http-kit/http2:http2-connection-manager-connection-count manager))
               (ensure-equal (if goaway-p '(1 3) '(1 3 5))
                             (loop for frame in (h2-output-frames first-stream)
                                   when (= 1 (http-kit/http2::%h2-frame-type frame))
                                     collect (http-kit/http2::%h2-frame-stream-id frame)))
               (ensure-true (plusp (binary-session-input-position first-stream)))
               (when goaway-p
                 (ensure-equal '(1)
                               (loop for frame in (h2-output-frames replacement)
                                     when (= 1 (http-kit/http2::%h2-frame-type frame))
                                       collect (http-kit/http2::%h2-frame-stream-id frame)))))
          (http-kit/http2:close-http2-connection-manager manager))
        (ensure-equal opened closed)
        (http-kit/http2:close-http2-connection-manager manager)
        (ensure-equal opened closed)
        (ensure-equal 0 (http-kit/http2:http2-connection-manager-connection-count manager)))))

  (deftest http2-connection-manager-boundary-batch-eof-cleanup
    (let* ((failed-stream (make-instance 'binary-session-stream
                                         :input (h2-frame 4 0 0 (octets))))
           (control-stream (make-instance 'binary-session-stream
                                          :input (h2-response-wire (octets 8))))
           (opened 0) (closed 0) (failed-connection nil)
           (manager
             (http-kit/http2:make-http2-connection-manager
              :open-connection
              (lambda (request &key timeout deadline)
                (declare (ignore request timeout deadline))
                (let* ((stream (if (zerop opened) failed-stream control-stream))
                       (connection
                         (http-kit/http2:make-http2-connection
                          :stream stream :close-stream
                          (lambda (value)
                            (ensure-true (eq stream value)) (incf closed)))))
                  (when (zerop opened) (setf failed-connection connection))
                  (incf opened)
                  connection)))))
      (unwind-protect
           (progn
             (let ((failure
                     (handler-case
                         (http-kit/http2:send-http2-requests-over-connection-manager
                          manager
                          (list (make-http-request :method "GET" :uri "https://eof.test/1")
                                (make-http-request :method "GET" :uri "https://eof.test/2")))
                       (http-connection-error (condition) condition))))
               (ensure-true (typep failure 'http-connection-error))
               (ensure-equal :http2-read (http-error-operation failure))
               (ensure-equal :eof (http-connection-error-cause failure)))
             (ensure-equal 1 opened)
             (ensure-equal 1 closed)
             (ensure-false (http-kit/http2:http2-connection-open-p failed-connection))
             (ensure-equal 0 (http-kit/http2:http2-connection-manager-connection-count manager))
             (ensure-equal 9 (binary-session-input-position failed-stream))
             (ensure-equal '(1 3)
                           (loop for frame in (h2-output-frames failed-stream)
                                 when (= 1 (http-kit/http2::%h2-frame-type frame))
                                   collect (http-kit/http2::%h2-frame-stream-id frame)))
             (let ((response (http-kit/http2:send-http2-request-over-connection-manager
                              manager (make-http-request :method "GET" :uri "https://eof.test/control"))))
               (ensure-equal 200 (http-response-status response))
               (ensure-equal (octets 8) (http-response-body response)))
             (ensure-equal 2 opened)
             (ensure-equal 1 (http-kit/http2:http2-connection-manager-connection-count manager))
             (ensure-true (plusp (binary-session-input-position control-stream))))
        (http-kit/http2:close-http2-connection-manager manager))
      (ensure-equal 2 closed)
      (http-kit/http2:close-http2-connection-manager manager)
      (ensure-equal 2 closed)))

  (deftest http2-connection-manager-boundary-batch-write-deadline
    (dolist (expired-p '(nil t))
      (let* ((stream (make-instance
                      'binary-session-stream
                      :input (concatenate-octets
                              (h2-frame 4 0 0 (octets))
                              (h2-frame 1 5 1 (octets #x88))
                              (h2-frame 1 5 3 (octets #x88))
                              (h2-frame 1 5 5 (octets #x88)))))
             (ticks 0) (opened 0) (closed 0) (connection nil)
             (clock (lambda () (incf ticks) (if (and expired-p (> ticks 1)) 1 0)))
             (manager
               (http-kit/http2:make-http2-connection-manager
                :open-connection
                (lambda (request &key timeout deadline)
                  (declare (ignore request))
                  (ensure-equal nil timeout)
                  (ensure-equal 1 deadline)
                  (incf opened)
                  (setf connection
                        (http-kit/http2:make-http2-connection
                         :stream stream :close-stream
                         (lambda (value)
                           (ensure-true (eq stream value)) (incf closed))))))))
        (unwind-protect
             (let ((result
                     (handler-case
                         (http-kit/http2:send-http2-requests-over-connection-manager
                          manager
                          (list (make-http-request :method "GET" :uri "https://deadline.test/1")
                                (make-http-request :method "GET" :uri "https://deadline.test/2"))
                          :deadline 1 :clock-function clock)
                       (http-timeout (condition) condition))))
               (ensure-equal 1 opened)
               (ensure-true (>= ticks 2))
               (if expired-p
                   (progn
                     (ensure-true (typep result 'http-timeout))
                     (ensure-equal :write (http-timeout-kind result))
                     (ensure-equal :write (http-error-operation result))
                     (ensure-equal 0 (length (binary-session-output stream)))
                     (ensure-equal 0 (binary-session-input-position stream))
                     (ensure-false (http-kit/http2:http2-connection-open-p connection))
                     (ensure-equal 1 closed)
                     (ensure-equal 0 (http-kit/http2:http2-connection-manager-connection-count manager)))
                   (progn
                     (ensure-equal '(200 200) (mapcar #'http-response-status result))
                     (ensure-equal 0 closed)
                     (ensure-true (http-kit/http2:http2-connection-open-p connection))
                     (ensure-equal 1 (http-kit/http2:http2-connection-manager-connection-count manager))
                     (ensure-equal '(1 3)
                                   (loop for frame in (h2-output-frames stream)
                                         when (= 1 (http-kit/http2::%h2-frame-type frame))
                                           collect (http-kit/http2::%h2-frame-stream-id frame)))
                     (ensure-equal 200
                                   (http-response-status
                                    (http-kit/http2:send-http2-request-over-connection-manager
                                     manager (make-http-request :method "GET" :uri "https://deadline.test/control")
                                     :deadline 1 :clock-function clock)))
                     (ensure-equal 1 opened)
                     (ensure-true (plusp (binary-session-input-position stream))))))
          (http-kit/http2:close-http2-connection-manager manager))
        (ensure-equal 1 closed)
        (http-kit/http2:close-http2-connection-manager manager)
        (ensure-equal 1 closed))))

  (deftest http2-connection-manager-boundary-refused-stream-retry-limit
    (let* ((stream (make-instance
                    'binary-session-stream
                    :input (concatenate-octets
                            (h2-frame 4 0 0 (octets))
                            (h2-frame 3 0 1 (octets 0 0 0 7))
                            (h2-frame 3 0 3 (octets 0 0 0 7))
                            (h2-frame 1 5 5 (octets #x88)))))
           (opened 0) (closed 0) (connection nil)
           (request (make-http-request :method "GET" :uri "https://refused.test/"))
           (manager
             (http-kit/http2:make-http2-connection-manager
              :open-connection
              (lambda (value &key timeout deadline)
                (declare (ignore timeout deadline))
                (ensure-true (eq request value))
                (incf opened)
                (setf connection
                      (http-kit/http2:make-http2-connection
                       :stream stream :close-stream
                       (lambda (value)
                         (ensure-true (eq stream value)) (incf closed))))))))
      (unwind-protect
           (progn
             (let ((failure
                     (handler-case
                         (http-kit/http2:send-http2-request-over-connection-manager manager request)
                       (http-connection-error (condition) condition))))
               (ensure-true (typep failure 'http-connection-error))
               (ensure-equal '(:rst-stream 3 7) (http-connection-error-cause failure)))
             (ensure-equal 1 opened)
             (ensure-equal 0 closed)
             (ensure-true (http-kit/http2:http2-connection-open-p connection))
             (ensure-equal 1 (http-kit/http2:http2-connection-manager-connection-count manager))
             (ensure-equal '(1 3)
                           (loop for frame in (h2-output-frames stream)
                                 when (= 1 (http-kit/http2::%h2-frame-type frame))
                                   collect (http-kit/http2::%h2-frame-stream-id frame)))
             (ensure-equal 35 (binary-session-input-position stream))
             (let ((response (http-kit/http2:send-http2-request-over-connection-manager manager request)))
               (ensure-equal 200 (http-response-status response))
               (ensure-equal (octets) (http-response-body response)))
             (ensure-equal 1 opened)
             (ensure-equal '(1 3 5)
                           (loop for frame in (h2-output-frames stream)
                                 when (= 1 (http-kit/http2::%h2-frame-type frame))
                                   collect (http-kit/http2::%h2-frame-stream-id frame))))
        (http-kit/http2:close-http2-connection-manager manager))
      (ensure-equal 1 closed)
      (http-kit/http2:close-http2-connection-manager manager)
      (ensure-equal 1 closed))))

#+sbcl
(progn
  (deftest http2-public-gap-manager-contracts
    (let ((opened 0)
          (request (make-http-request :method "GET" :uri "https://manager.test/")))
      (flet ((open-invalid (value &key timeout deadline)
               (declare (ignore timeout deadline))
               (ensure-true (eq request value))
               (incf opened)
               nil))
        (dolist (options '((:open-connection :bad) (:max-connections 0)
                          (:max-connections -1) (:max-connections 1/2)))
          (let ((failure (handler-case
                             (apply #'http-kit/http2:make-http2-connection-manager
                                    (append options (unless (eq :open-connection (first options))
                                                      (list :open-connection #'open-invalid))))
                           (http-protocol-error (condition) condition))))
            (ensure-true (typep failure 'http-protocol-error))
            (ensure-equal :http2-manager (http-error-operation failure))
            (ensure-equal (second options) (http-protocol-error-detail failure))))
        (ensure-equal 0 opened)
        (ensure-false (http-kit/http2:http2-connection-manager-max-connections nil))
        (dolist (function (list #'http-kit/http2:http2-connection-manager-connections
                               #'http-kit/http2:make-http2-connection-manager-transport
                               #'http-kit/http2:close-http2-connection-manager
                               (lambda (value)
                                 (http-kit/http2:send-http2-request-over-connection-manager value request))))
          (let ((failure (handler-case (funcall function nil)
                           (http-protocol-error (condition) condition))))
            (ensure-true (typep failure 'http-protocol-error))
            (ensure-equal :http2-manager (http-error-operation failure))
            (ensure-equal 'null (http-protocol-error-detail failure))))
        (let* ((manager (http-kit/http2:make-http2-connection-manager
                         :open-connection #'open-invalid :max-connections 2))
               (failure (handler-case
                            (http-kit/http2:send-http2-request-over-connection-manager manager request)
                          (http-protocol-error (condition) condition))))
          (ensure-true (typep failure 'http-protocol-error))
          (ensure-equal :http2-manager (http-error-operation failure))
          (ensure-equal 'null (http-protocol-error-detail failure))
          (ensure-equal 1 opened)
          (ensure-equal 2 (http-kit/http2:http2-connection-manager-max-connections manager))
          (ensure-equal 0 (http-kit/http2:http2-connection-manager-connection-count manager))
          (ensure-false (http-kit/http2:http2-connection-manager-connections manager))
          (ensure-true (http-kit/http2:http2-connection-manager-open-p manager))
          (ensure-true (eq manager (http-kit/http2:close-http2-connection-manager manager)))
          (ensure-false (http-kit/http2:http2-connection-manager-open-p manager))))))

  (deftest http2-public-gap-manager-three-key-lru
    (let* ((streams (list (make-instance 'binary-session-stream
                                        :input (concatenate-octets (h2-response-wire (octets 1))
                                                                   (h2-frame 1 4 3 (octets #x88))
                                                                   (h2-frame 0 1 3 (octets 4))))
                          (make-instance 'binary-session-stream :input (h2-response-wire (octets 2)))
                          (make-instance 'binary-session-stream :input (h2-response-wire (octets 3)))))
           (opened 0) (closed nil) (connections nil)
           (manager (http-kit/http2:make-http2-connection-manager
                     :max-connections 2
                     :open-connection
                     (lambda (request &key timeout deadline)
                       (declare (ignore timeout deadline))
                       (ensure-equal "GET" (http-request-method request))
                       (let* ((index (incf opened))
                              (stream (nth (1- index) streams))
                              (connection
                                (http-kit/http2:make-http2-connection
                                 :stream stream :close-stream (lambda (value)
                                                                (ensure-true (eq stream value))
                                                                (push index closed)))))
                         (push connection connections)
                         connection)))))
      (unwind-protect
           (progn
             (loop for host in '("one.test" "two.test" "one.test" "three.test")
                   for expected in '(1 2 4 3)
                   do (let ((response (http-kit/http2:send-http2-request-over-connection-manager
                                       manager (make-http-request :method "GET"
                                                                  :uri (format nil "https://~A/" host)))))
                        (ensure-equal 200 (http-response-status response))
                        (ensure-equal (octets expected) (http-response-body response))))
             (ensure-equal 3 opened)
             (ensure-equal '(2) closed)
             (ensure-equal 2 (http-kit/http2:http2-connection-manager-connection-count manager))
             (let ((retained (http-kit/http2:http2-connection-manager-connections manager)))
               (ensure-equal 2 (length retained))
               (ensure-true (member (first connections) retained :test #'eq))
               (ensure-true (member (third connections) retained :test #'eq))
               (ensure-false (member (second connections) retained :test #'eq)))
             (ensure-true (http-kit/http2:http2-connection-open-p (first connections)))
             (ensure-false (http-kit/http2:http2-connection-open-p (second connections)))
             (ensure-true (http-kit/http2:http2-connection-open-p (third connections)))
             (loop for stream in streams for ids in '((1 3) (1) (1))
                   do (ensure-equal ids
                                    (loop for frame in (h2-output-frames stream)
                                          when (= 1 (http-kit/http2::%h2-frame-type frame))
                                            collect (http-kit/http2::%h2-frame-stream-id frame)))
                      (ensure-equal (length (binary-session-input stream))
                                    (binary-session-input-position stream))))
        (http-kit/http2:close-http2-connection-manager manager))
      (ensure-equal '(1 2 3) (sort (copy-list closed) #'<))
      (ensure-false (http-kit/http2:http2-connection-manager-connections manager))
      (http-kit/http2:close-http2-connection-manager manager)
      (ensure-equal 3 (length closed)))))

#+sbcl
(progn
  (defun %check-http2-manager-mutable-key-recency (first-most-recent-p)
    (let* ((first-key (copy-seq "a"))
           (second-key (copy-seq "b"))
           (recent-index (if first-most-recent-p 0 1))
           (streams
             (loop for index below 2
                   for marker = (1+ index)
                   collect
                   (make-instance
                    'binary-session-stream
                    :input
                    (if (= index recent-index)
                        (concatenate-octets
                         (h2-response-wire (octets marker))
                         (h2-frame 1 4 3 (octets #x88))
                         (h2-frame 0 1 3 (octets marker))
                         (h2-frame 1 4 5 (octets #x88))
                         (h2-frame 0 1 5 (octets marker)))
                        (h2-response-wire (octets marker))))))
           (requests (loop for index below 4
                           collect (make-http-request
                                    :method "GET"
                                    :uri (format nil "https://example.test/key/~D" index))))
           (connections (make-array 2 :initial-element nil))
           (closed (make-array 2 :initial-element 0))
           (opened 0)
           (callbacks nil)
           (events nil)
           (current-request nil)
           (manager
             (http-kit/http2:make-http2-connection-manager
              :max-connections 2
              :open-connection
              (lambda (request &key timeout deadline)
                (ensure-true (eq (nth opened requests) request))
                (ensure-false timeout)
                (ensure-false deadline)
                (let* ((index opened)
                       (stream (nth index streams))
                       (connection
                         (http-kit/http2:make-http2-connection
                          :stream stream :close-stream
                          (lambda (actual)
                            (ensure-true (eq stream actual))
                            (incf (aref closed index))
                            (push (list :close index) events)))))
                  (incf opened)
                  (setf (aref connections index) connection)
                  (push (list :open index) events)
                  connection)))))
      (unwind-protect
           (flet ((send (index key marker)
                    (setf current-request (nth index requests))
                    (let ((response
                            (http-kit/http2:send-http2-request-over-connection-manager
                             manager current-request :connection-key key
                             :on-body-chunk
                             (lambda (chunk)
                               (push (list current-request (copy-seq chunk)) callbacks)
                               (push (list :body index) events)))))
                      (ensure-equal 200 (http-response-status response))
                      (ensure-equal (octets marker) (http-response-body response)))))
             (send 0 first-key 1)
             (send 1 second-key 2)
             (send 2 (if first-most-recent-p first-key second-key) (1+ recent-index))
             ;; The manager retains caller-owned keys. A legal mutation makes two
             ;; entries equal; selection must still use the most recently used one.
             (setf (char second-key 0) #\a)
             (ensure-equal first-key second-key)
             (ensure-false (eq first-key second-key))
             (send 3 first-key (1+ recent-index))
             (ensure-equal 2 opened)
             (ensure-equal 2 (http-kit/http2:http2-connection-manager-connection-count manager))
             (ensure-equal 4 (length callbacks))
             (loop for callback in (reverse callbacks)
                   for request in requests
                   for marker in (list 1 2 (1+ recent-index) (1+ recent-index))
                   do (ensure-true (eq request (first callback)))
                      (ensure-equal (octets marker) (second callback)))
             (ensure-equal '((:open 0) (:body 0) (:open 1) (:body 1) (:body 2) (:body 3))
                           (reverse events))
             (ensure-equal #(0 0) closed)
             (loop for stream in streams for index below 2
                   for connection = (aref connections index)
                   do (ensure-true (http-kit/http2:http2-connection-open-p connection))
                      (ensure-true
                       (member connection
                               (http-kit/http2:http2-connection-manager-connections manager)
                               :test #'eq))
                      (ensure-equal (if (= index recent-index) '(1 3 5) '(1))
                                    (loop for frame in (h2-output-frames stream)
                                          when (= 1 (http-kit/http2::%h2-frame-type frame))
                                            collect (http-kit/http2::%h2-frame-stream-id frame)))
                      (ensure-equal (length (binary-session-input stream))
                                    (binary-session-input-position stream))))
        (http-kit/http2:close-http2-connection-manager manager))
      (ensure-equal #(1 1) closed)
      (ensure-equal '((:open 0) (:body 0) (:open 1) (:body 1) (:body 2) (:body 3)
                      (:close 1) (:close 0))
                    (reverse events))
      (ensure-false (http-kit/http2:http2-connection-manager-connections manager))
      (ensure-false (http-kit/http2:http2-connection-manager-open-p manager))
      (ensure-true (every (lambda (connection)
                            (not (http-kit/http2:http2-connection-open-p connection)))
                          connections))
      (http-kit/http2:close-http2-connection-manager manager)
      (ensure-equal #(1 1) closed)))

  (deftest http2-public-contract-mutable-key-prefers-first-opened
    (%check-http2-manager-mutable-key-recency t))

  (deftest http2-public-contract-mutable-key-prefers-last-opened
    (%check-http2-manager-mutable-key-recency nil)))
