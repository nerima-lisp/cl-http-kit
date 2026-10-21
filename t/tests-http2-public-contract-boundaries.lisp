(in-package #:http-kit/test)

#+sbcl
(progn
  (deftest http2-public-contract-boundary-manager-batch-preflight
    (let* ((stream
             (make-instance
              'binary-session-stream
              :input (concatenate-octets
                      (h2-frame 4 0 0 (octets))
                      (h2-frame 1 5 1 (h2-header-block '(":status" . "200")))
                      (h2-frame 1 5 3 (h2-header-block '(":status" . "201"))))))
           (opened 0)
           (closed 0)
           (manager
             (http-kit/http2:make-http2-connection-manager
              :open-connection
              (lambda (request &key timeout deadline)
                (declare (ignore timeout deadline))
                (ensure-equal "GET" (http-request-method request))
                (incf opened)
                (http-kit/http2:make-http2-connection
                 :stream stream
                 :close-stream (lambda (closed-stream)
                                 (ensure-true (eq stream closed-stream))
                                 (incf closed)))))))
      (unwind-protect
           (progn
             (dolist (invalid (list nil (vector :not-a-list)))
               (let ((condition
                       (handler-case
                           (http-kit/http2:send-http2-requests-over-connection-manager
                            manager invalid)
                         (http-protocol-error (condition) condition))))
                 (ensure-true (typep condition 'http-protocol-error))
                 (ensure-equal :http2-manager (http-error-operation condition))
                 (ensure-true (eq invalid (http-protocol-error-detail condition))))
               (ensure-equal 0 opened)
               (ensure-equal 0 closed)
               (ensure-equal 0 (binary-session-input-position stream))
               (ensure-equal 0 (length (binary-session-output stream)))
               (ensure-equal 0
                             (http-kit/http2:http2-connection-manager-connection-count manager))
               (ensure-true (http-kit/http2:http2-connection-manager-open-p manager)))
             (let ((responses
                     (http-kit/http2:send-http2-requests-over-connection-manager
                      manager
                      (list (make-http-request :method "GET" :uri "https://127.0.0.1/one")
                            (make-http-request :method "GET" :uri "https://127.0.0.1/two")))))
               (ensure-equal 2 (length responses))
               (ensure-equal '(200 201) (mapcar #'http-response-status responses))
               (dolist (response responses)
                 (ensure-equal (octets) (http-response-body response))))
             (ensure-equal 1 opened)
             (ensure-equal 0 closed)
             (ensure-equal 1
                           (http-kit/http2:http2-connection-manager-connection-count manager))
             (ensure-equal (length (binary-session-input stream))
                           (binary-session-input-position stream))
             (let ((frames (h2-output-frames stream)))
               (ensure-equal '(1 3)
                             (mapcar #'http-kit/http2::%h2-frame-stream-id
                                     (remove-if-not
                                      (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                                      frames)))
               (ensure-equal 1
                             (count-if
                              (lambda (frame)
                                (and (= 4 (http-kit/http2::%h2-frame-type frame))
                                     (= 1 (http-kit/http2::%h2-frame-flags frame))))
                              frames))))
        (http-kit/http2:close-http2-connection-manager manager))
      (ensure-equal 1 closed)
      (ensure-equal 0 (http-kit/http2:http2-connection-manager-connection-count manager))
      (ensure-true (not (http-kit/http2:http2-connection-manager-open-p manager)))
      (http-kit/http2:close-http2-connection-manager manager)
      (ensure-equal 1 closed)))

  (deftest http2-public-contract-boundary-local-goaway-replacement
    (let* ((streams (list (make-instance 'binary-session-stream
                                         :input (h2-response-wire (octets 1)))
                         (make-instance 'binary-session-stream
                                         :input (h2-response-wire (octets 2)))))
           (opened 0)
           (closed (vector 0 0))
           (connections nil)
           (manager
             (http-kit/http2:make-http2-connection-manager
              :open-connection
              (lambda (request &key timeout deadline)
                (declare (ignore timeout deadline))
                (ensure-equal "GET" (http-request-method request))
                (let* ((index opened)
                       (stream (nth index streams))
                       (connection
                         (http-kit/http2:make-http2-connection
                          :stream stream
                          :close-stream (lambda (closed-stream)
                                          (ensure-true (eq stream closed-stream))
                                          (incf (aref closed index))))))
                  (incf opened)
                  (push connection connections)
                  connection)))))
      (unwind-protect
           (let* ((request (make-http-request :method "GET" :uri "https://127.0.0.1/same"))
                  (first-response
                    (http-kit/http2:send-http2-request-over-connection-manager manager request))
                  (old (first connections)))
             (ensure-equal 200 (http-response-status first-response))
             (ensure-equal (octets 1) (http-response-body first-response))
             (ensure-equal 1 opened)
             (ensure-true (http-kit/http2:http2-connection-open-p old))
             (http-kit/http2:send-http2-goaway
              old :last-stream-id 0 :error-code 0 :debug-data (octets 9))
             (ensure-true (http-kit/http2:http2-connection-open-p old))
             (ensure-true (http-kit/http2:http2-connection-draining-p old))
             (ensure-equal 0 (http-kit/http2:http2-connection-local-goaway-last-stream-id old))
             (ensure-equal #(0 0) closed)
             (ensure-equal 1 (http-kit/http2:http2-connection-manager-connection-count manager))
             (let* ((second-response
                     (http-kit/http2:send-http2-request-over-connection-manager manager request))
                   (new (first connections)))
               (ensure-equal 200 (http-response-status second-response))
               (ensure-equal (octets 2) (http-response-body second-response))
               (ensure-equal 2 opened)
               (ensure-true (not (eq old new)))
               (ensure-true (not (http-kit/http2:http2-connection-open-p old)))
               (ensure-true (http-kit/http2:http2-connection-open-p new))
               (ensure-equal #(1 0) closed)
               (ensure-equal 1 (http-kit/http2:http2-connection-manager-connection-count manager)))
             (loop for stream in streams do
               (ensure-equal (length (binary-session-input stream))
                             (binary-session-input-position stream))
               (let ((frames (h2-output-frames stream)))
                 (ensure-equal '(1)
                               (mapcar #'http-kit/http2::%h2-frame-stream-id
                                       (remove-if-not
                                        (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                                        frames)))))
             (let ((goaways
                     (remove-if-not
                      (lambda (frame) (= 7 (http-kit/http2::%h2-frame-type frame)))
                      (h2-output-frames (first streams)))))
               (ensure-equal 1 (length goaways))
               (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id (first goaways)))
               (ensure-equal 0 (http-kit/http2::%h2-frame-flags (first goaways)))
               (ensure-equal (octets 0 0 0 0 0 0 0 0 9)
                             (http-kit/http2::%h2-frame-payload (first goaways)))))
        (http-kit/http2:close-http2-connection-manager manager))
      (ensure-equal #(1 1) closed)
      (dolist (connection connections)
        (ensure-true (not (http-kit/http2:http2-connection-open-p connection))))
      (http-kit/http2:close-http2-connection-manager manager)
      (ensure-equal #(1 1) closed)))

  (deftest http2-public-contract-boundary-batch-window-update
    (let* ((stream
             (make-instance
              'binary-session-stream
              :input (concatenate-octets
                      (h2-frame 4 0 0 (octets))
                      (h2-frame 8 0 0 (octets 0 0 0 7))
                      (h2-frame 8 0 1 (octets 0 0 0 5))
                      (h2-frame 1 5 1 (h2-header-block '(":status" . "200")))
                      (h2-frame 1 5 3 (h2-header-block '(":status" . "201"))))))
           (closed 0)
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream
              :close-stream (lambda (closed-stream)
                              (ensure-true (eq stream closed-stream))
                              (incf closed)))))
      (unwind-protect
           (progn
             (ensure-equal 65535
                           (http-kit/http2:http2-connection-peer-connection-window-size connection))
             (let ((responses
                     (http-kit/http2:send-http2-requests-over-connection
                      connection
                      (list (make-http-request :method "GET" :uri "https://127.0.0.1/one")
                            (make-http-request :method "GET" :uri "https://127.0.0.1/two")))))
               (ensure-equal 2 (length responses))
               (ensure-equal '(200 201) (mapcar #'http-response-status responses))
               (dolist (response responses)
                 (ensure-equal (octets) (http-response-body response))))
             (ensure-equal 65542
                           (http-kit/http2:http2-connection-peer-connection-window-size connection))
             (ensure-true (http-kit/http2:http2-connection-open-p connection))
             (ensure-equal 0 closed)
             (ensure-equal (length (binary-session-input stream))
                           (binary-session-input-position stream))
             (let ((frames (h2-output-frames stream)))
               (ensure-equal '(1 3)
                             (mapcar #'http-kit/http2::%h2-frame-stream-id
                                     (remove-if-not
                                      (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                                      frames)))
               (ensure-equal 1
                             (count-if
                              (lambda (frame)
                                (and (= 4 (http-kit/http2::%h2-frame-type frame))
                                     (= 1 (http-kit/http2::%h2-frame-flags frame))))
                              frames))))
        (http-kit/http2:close-http2-connection connection))
      (ensure-equal 1 closed)
      (ensure-true (not (http-kit/http2:http2-connection-open-p connection)))
      (http-kit/http2:close-http2-connection connection)
      (ensure-equal 1 closed))
    (check-public-batch-control-frame
     (h2-frame 8 0 0 (octets 0 0 0 0))
     :operation :http2-control :detail (list 0 (octets 0 0 0 0)))))

#+sbcl
(deftest http2-public-contract-wrong-type-getters
  (dolist (value '(nil :invalid 7))
    (dolist (getter (list #'http-kit/http2:http2-client-connection
                         #'http-kit/http2:http2-connection-open-p
                         #'http-kit/http2:http2-connection-session-started-p
                         #'http-kit/http2:http2-connection-peer-max-frame-size
                         #'http-kit/http2:http2-connection-peer-max-table-size
                         #'http-kit/http2:http2-connection-peer-max-concurrent-streams
                         #'http-kit/http2:http2-connection-peer-max-header-list-size
                         #'http-kit/http2:http2-connection-peer-initial-window-size
                         #'http-kit/http2:http2-connection-peer-connection-window-size
                         #'http-kit/http2:http2-connection-goaway-last-stream-id
                         #'http-kit/http2:http2-connection-local-goaway-last-stream-id
                         #'http-kit/http2:http2-connection-draining-p
                         #'http-kit/http2:http2-connection-manager-open-p
                         #'http-kit/http2:http2-connection-manager-max-connections
                         #'http-kit/http2:http2-connection-manager-idle-timeout
                         #'http-kit/http2:http2-connection-manager-max-connection-age
                         #'http-kit/http2:http2-connection-manager-clock-function
                         #'http-kit/http2:http2-connection-manager-connection-count))
      (ensure-false (funcall getter value)))
    (let ((condition
            (handler-case
                (http-kit/http2:http2-connection-manager-connections value)
              (http-protocol-error (error) error))))
      (ensure-true (typep condition 'http-protocol-error))
      (ensure-equal :http2-manager (http-error-operation condition))
      (ensure-equal (type-of value) (http-protocol-error-detail condition)))))

#+sbcl
(deftest http2-public-contract-client-connection-defaults
  (let ((condition
          (handler-case
              (http-kit/http2:make-http2-client :connection :invalid)
            (http-protocol-error (error) error))))
    (ensure-true (typep condition 'http-protocol-error))
    (ensure-equal :http2-client (http-error-operation condition))
    (ensure-equal '(nil nil :invalid) (http-protocol-error-detail condition)))
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream)))
    (unwind-protect
         (let ((client (http-kit/http2:make-http2-client :connection connection)))
           (ensure-true (eq connection (http-kit/http2:http2-client-connection client)))
           (ensure-true (http-kit/http2:http2-connection-open-p connection))
           (ensure-false (http-kit/http2:http2-connection-session-started-p connection))
           (ensure-equal 16384 (http-kit/http2:http2-connection-peer-max-frame-size connection))
           (ensure-equal 4096 (http-kit/http2:http2-connection-peer-max-table-size connection))
           (ensure-equal 65535 (http-kit/http2:http2-connection-peer-initial-window-size connection))
           (ensure-equal 65535 (http-kit/http2:http2-connection-peer-connection-window-size connection))
           (ensure-false (http-kit/http2:http2-connection-draining-p connection))
           (ensure-equal 0 (binary-session-input-position stream))
           (ensure-equal (octets) (binary-session-output stream)))
      (http-kit/http2:close-http2-connection connection))
    (ensure-false (open-stream-p stream))
    (ensure-false (http-kit/http2:http2-connection-open-p connection))
    (ensure-true (eq connection (http-kit/http2:close-http2-connection connection)))))

#+sbcl
(deftest http2-public-contract-manager-open-result-validation
  (let* ((opened 0)
         (request (make-http-request :method "GET" :uri "https://127.0.0.1/"))
         (manager (http-kit/http2:make-http2-connection-manager
                   :open-connection
                   (lambda (received &key timeout deadline)
                     (incf opened)
                     (ensure-true (eq request received))
                     (ensure-false timeout)
                     (ensure-false deadline)
                     :invalid))))
    (unwind-protect
         (progn
           (ensure-equal 8 (http-kit/http2:http2-connection-manager-max-connections manager))
           (ensure-false (http-kit/http2:http2-connection-manager-idle-timeout manager))
           (ensure-false (http-kit/http2:http2-connection-manager-max-connection-age manager))
           (ensure-true (functionp (http-kit/http2:http2-connection-manager-clock-function manager)))
           (ensure-equal 0 opened)
           (let ((condition
                   (handler-case
                       (http-kit/http2:send-http2-request-over-connection-manager manager request)
                     (http-protocol-error (error) error))))
             (ensure-true (typep condition 'http-protocol-error))
             (ensure-equal :http2-manager (http-error-operation condition))
             (ensure-equal (type-of :invalid) (http-protocol-error-detail condition)))
           (ensure-equal 1 opened)
           (ensure-equal 0 (http-kit/http2:http2-connection-manager-connection-count manager))
           (ensure-false (http-kit/http2:http2-connection-manager-connections manager))
           (ensure-true (http-kit/http2:http2-connection-manager-open-p manager)))
      (http-kit/http2:close-http2-connection-manager manager))
    (ensure-false (http-kit/http2:http2-connection-manager-open-p manager))
    (ensure-equal 1 opened)))

#+sbcl
(deftest http2-public-contract-manager-batch-error-discard
  (let* ((stream (make-instance 'binary-session-stream
                                :input (concatenate-octets
                                        (h2-frame 4 0 0 (octets))
                                        (h2-frame 1 5 0 (h2-header-block (cons ":status" "200"))))))
         (closed 0) (opened 0)
         (request (make-http-request :method "GET" :uri "https://example.com/"))
         (connection
           (http-kit/http2:make-http2-connection
            :stream stream
            :close-stream (lambda (received)
                            (ensure-true (eq stream received))
                            (incf closed) (close received))))
         (manager
           (http-kit/http2:make-http2-connection-manager
            :open-connection
            (lambda (received &key timeout deadline)
              (incf opened)
              (ensure-true (eq request received))
              (ensure-false timeout)
              (ensure-false deadline)
              connection))))
    (unwind-protect
         (let ((condition
                 (handler-case
                     (http-kit/http2:send-http2-requests-over-connection-manager manager (list request))
                   (http-protocol-error (error) error))))
           (ensure-true (typep condition 'http-protocol-error))
           (ensure-equal :http2-read (http-error-operation condition))
           (ensure-equal 0 (http-protocol-error-detail condition))
           (ensure-equal 1 opened)
           (ensure-equal 1 closed)
           (ensure-false (open-stream-p stream))
           (ensure-false (http-kit/http2:http2-connection-open-p connection))
           (ensure-true (http-kit/http2:http2-connection-manager-open-p manager))
           (ensure-equal 0 (http-kit/http2:http2-connection-manager-connection-count manager))
           (ensure-false (http-kit/http2:http2-connection-manager-connections manager))
           (ensure-equal (length (binary-session-input stream))
                         (binary-session-input-position stream)))
      (http-kit/http2:close-http2-connection-manager manager)
      (http-kit/http2:close-http2-connection connection))
    (ensure-equal 1 closed)
    (ensure-equal 1 opened)
    (ensure-false (http-kit/http2:http2-connection-manager-open-p manager))))

#+sbcl
(deftest http2-public-contract-producer-noninteger-octets
  (dolist (value '(not-an-octet 1/2 1.0 -1))
    (let* ((stream (make-instance 'binary-session-stream :input (h2-response-wire (octets 7))))
           (closed 0) (calls 0) (order nil)
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream
              :close-stream (lambda (received)
                              (ensure-true (eq stream received))
                              (push :close order)
                              (incf closed)
                              (close received)))))
      (unwind-protect
           (let ((condition
                   (handler-case
                       (http-kit/http2:send-http2-request-over-connection
                        connection (make-http-request :method "POST" :uri "https://example.com/")
                        :request-body-function
                        (lambda (maximum)
                          (incf calls)
                          (push :producer order)
                          (ensure-equal 16384 maximum)
                          (ensure-equal 0 (binary-session-input-position stream))
                          (ensure-equal '(4 1) (mapcar #'http-kit/http2::%h2-frame-type
                                                     (h2-output-frames stream)))
                          (vector value)))
                     (http-protocol-error (error) error))))
             (ensure-true (typep condition 'http-protocol-error))
             (ensure-equal :http2-write (http-error-operation condition))
             (ensure-equal value (http-protocol-error-detail condition))
             (ensure-equal 1 calls)
             (ensure-equal '(:producer :close) (reverse order))
             (ensure-equal 1 closed)
             (ensure-false (open-stream-p stream))
             (ensure-false (http-kit/http2:http2-connection-open-p connection))
             (ensure-equal 0 (binary-session-input-position stream))
             (ensure-equal '(4 1) (mapcar #'http-kit/http2::%h2-frame-type
                                        (h2-output-frames stream)))
             (ensure-equal '(0 1) (mapcar #'http-kit/http2::%h2-frame-stream-id
                                        (h2-output-frames stream)))
             (multiple-value-bind (body frames acks) (h2-output-data-summary stream)
               (ensure-equal (octets) body)
               (ensure-equal 0 frames)
               (ensure-equal 0 acks)))
        (http-kit/http2:close-http2-connection connection))
      (ensure-equal 1 closed))))
#+sbcl
(deftest http2-public-contract-boundary-information-validation-before-io
  (let* ((stream (make-instance 'binary-session-stream
                                :input (h2-response-wire (octets 7))))
         (closed 0)
         (connection
           (http-kit/http2:make-http2-connection
            :stream stream
            :close-stream (lambda (received)
                            (ensure-true (eq stream received))
                            (incf closed)
                            (close received))))
         (initial-input-position (binary-session-input-position stream))
         (initial-output (copy-seq (binary-session-output stream))))
    (unwind-protect
         (let ((condition
                 (handler-case
                     (progn
                       (http-kit/http2:send-http2-request-over-connection
                        connection
                        (make-http-request :method "GET" :uri "https://example.com/")
                        :on-information 17)
                       nil)
                   (http-protocol-error (error) error))))
           (ensure-true (typep condition 'http-protocol-error))
           (ensure-equal :http2-client (http-error-operation condition))
           (ensure-equal 17 (http-protocol-error-detail condition))
           (ensure-equal initial-input-position (binary-session-input-position stream))
           (ensure-equal initial-output (binary-session-output stream))
           (ensure-true (http-kit/http2:http2-connection-open-p connection))
           (ensure-equal 0 closed))
      (http-kit/http2:close-http2-connection connection))
    (ensure-equal 1 closed)))

#+sbcl
(deftest http2-public-contract-completed-stream-window-update
  (let* ((stream (make-instance
                  'binary-session-stream
                  :input (concatenate-octets
                          (h2-frame 4 0 0 (octets))
                          (h2-frame 1 5 1 (h2-header-block (cons ":status" "200")))
                          (h2-frame 8 0 1 (octets 0 0 0 1))
                          (h2-frame 1 5 3 (h2-header-block (cons ":status" "201"))))))
         (closed 0)
         (connection (http-kit/http2:make-http2-connection
                      :stream stream :close-stream (lambda (received)
                                                     (ensure-true (eq received stream))
                                                     (incf closed) (close received))))
         (request (make-http-request :method "GET" :uri "https://example.com/")))
    (unwind-protect
         (progn
           (ensure-equal 200 (http-response-status
                              (http-kit/http2:send-http2-request-over-connection connection request)))
           (ensure-equal 65535 (http-kit/http2:http2-connection-peer-connection-window-size connection))
           (ensure-equal 201 (http-response-status
                              (http-kit/http2:send-http2-request-over-connection connection request)))
           (ensure-equal 65535 (http-kit/http2:http2-connection-peer-connection-window-size connection))
           (ensure-equal 0 closed)
           (ensure-true (http-kit/http2:http2-connection-open-p connection))
           (ensure-equal (length (binary-session-input stream)) (binary-session-input-position stream))
           (ensure-equal '(1 3)
                         (mapcar #'http-kit/http2::%h2-frame-stream-id
                                 (remove-if-not (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                                                (h2-output-frames stream)))))
      (http-kit/http2:close-http2-connection connection))
    (ensure-equal 1 closed)))

#+sbcl
(deftest http2-public-contract-paused-admission-goaway
  (let* ((stream (make-instance
                  'binary-session-stream
                  :input (concatenate-octets
                          (h2-frame 4 0 0 (octets 0 3 0 0 0 0))
                          (h2-frame 1 5 1 (h2-header-block (cons ":status" "200")))
                          (h2-frame 7 0 0 (octets 0 0 0 3 0 0 0 0)))))
         (closed 0)
         (connection (http-kit/http2:make-http2-connection
                      :stream stream :close-stream (lambda (received)
                                                     (ensure-true (eq received stream))
                                                     (incf closed) (close received))))
         (request (make-http-request :method "GET" :uri "https://example.com/")))
    (unwind-protect
         (progn
           (ensure-equal 200 (http-response-status
                              (http-kit/http2:send-http2-request-over-connection connection request)))
           (ensure-equal 0 closed)
           (let* ((before (copy-seq (binary-session-output stream)))
                  (condition (handler-case
                                 (http-kit/http2:send-http2-request-over-connection connection request)
                               (http-connection-error (error) error))))
             (ensure-true (typep condition 'http-connection-error))
             (ensure-equal :http2-write (http-error-operation condition))
             (ensure-equal '(:goaway 3 3) (http-connection-error-cause condition))
             (ensure-equal before (binary-session-output stream)))
           (ensure-equal 1 closed)
           (ensure-false (http-kit/http2:http2-connection-open-p connection))
           (ensure-false (open-stream-p stream))
           (ensure-equal (length (binary-session-input stream)) (binary-session-input-position stream))
           (ensure-equal '(1)
                         (mapcar #'http-kit/http2::%h2-frame-stream-id
                                 (remove-if-not (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                                                (h2-output-frames stream)))))
      (http-kit/http2:close-http2-connection connection))
    (ensure-equal 1 closed)))

#+sbcl
(deftest http2-public-contract-manager-producer-error-no-retry
  (dolist (failure (list (make-condition 'http-protocol-error :message "Producer protocol failure" :operation :http2-write :detail :producer)
                         (make-condition 'http-connection-error :message "Producer EOF failure" :operation :http2-write :cause :eof)
                         (make-condition 'http-connection-error :message "Producer custom failure" :operation :http2-write :cause '(:custom 8))))
    (let* ((stream (make-instance 'binary-session-stream :input (h2-response-wire (octets))))
           (opened 0) (closed 0) (produced 0) (order nil)
           (request (make-http-request :method "POST" :uri "https://example.com/"))
           (connection (http-kit/http2:make-http2-connection
                        :stream stream :close-stream (lambda (received)
                                                       (ensure-true (eq stream received))
                                                       (push :close order)
                                                       (incf closed) (close received))))
           (manager (http-kit/http2:make-http2-connection-manager
                     :open-connection (lambda (received &key timeout deadline)
                                        (ensure-true (eq request received))
                                        (ensure-false timeout)
                                        (ensure-false deadline)
                                        (push :open order)
                                        (incf opened)
                                        connection))))
      (unwind-protect
           (let ((condition
                   (handler-case
                       (http-kit/http2:send-http2-request-over-connection-manager
                        manager request :request-body-function
                        (lambda (maximum)
                          (ensure-equal 16384 maximum)
                          (push :producer order)
                          (incf produced)
                          (error failure)))
                     (http-protocol-error (error) error)
                     (http-connection-error (error) error))))
             (ensure-true (eq failure condition))
             (ensure-equal :http2-write (http-error-operation condition))
             (if (typep failure 'http-connection-error)
                 (ensure-equal (http-connection-error-cause failure) (http-connection-error-cause condition))
                 (ensure-equal :producer (http-protocol-error-detail condition)))
             (ensure-equal '(:open :producer :close) (reverse order))
             (ensure-equal 1 opened)
             (ensure-equal 1 produced)
             (ensure-equal 1 closed)
             (ensure-equal 0 (http-kit/http2:http2-connection-manager-connection-count manager))
             (ensure-false (http-kit/http2:http2-connection-manager-connections manager))
             (ensure-true (http-kit/http2:http2-connection-manager-open-p manager))
             (ensure-false (http-kit/http2:http2-connection-open-p connection))
             (ensure-false (open-stream-p stream))
             (ensure-equal 0 (binary-session-input-position stream))
             (ensure-equal '(4 1) (mapcar #'http-kit/http2::%h2-frame-type (h2-output-frames stream))))
        (http-kit/http2:close-http2-connection-manager manager)
        (http-kit/http2:close-http2-connection connection))
      (ensure-equal 1 closed)
      (ensure-equal 1 opened))))

#+sbcl
(deftest http2-public-contract-manager-batch-reset-retains-connection
  (dolist (reset-stream '(3 1))
    (let* ((stream (make-instance
                    'binary-session-stream
                    :input (concatenate-octets
                            (h2-frame 4 0 0 (octets))
                            (h2-frame 1 5 1 (h2-header-block (cons ":status" "200")))
                            (h2-frame 3 0 reset-stream (octets 0 0 0 8))
                            (h2-frame 1 5 5 (h2-header-block (cons ":status" "201"))))))
           (opened 0) (closed 0)
           (request (make-http-request :method "GET" :uri "https://example.com/"))
           (connection (http-kit/http2:make-http2-connection
                        :stream stream :close-stream (lambda (received)
                                                       (ensure-true (eq stream received))
                                                       (incf closed) (close received))))
           (manager (http-kit/http2:make-http2-connection-manager
                     :open-connection (lambda (received &key timeout deadline)
                                        (ensure-true (eq request received))
                                        (ensure-false timeout)
                                        (ensure-false deadline)
                                        (incf opened)
                                        connection))))
      (unwind-protect
           (progn
             (ensure-equal 200 (http-response-status
                                (http-kit/http2:send-http2-request-over-connection-manager manager request)))
             (let ((condition (handler-case
                                  (http-kit/http2:send-http2-requests-over-connection-manager manager (list request))
                                (http-connection-error (error) error))))
               (ensure-true (typep condition 'http-connection-error))
               (ensure-equal :http2-control (http-error-operation condition))
               (ensure-equal (list :rst-stream reset-stream 8) (http-connection-error-cause condition)))
             (ensure-equal 1 opened)
             (ensure-equal 0 closed)
             (ensure-true (open-stream-p stream))
             (ensure-true (http-kit/http2:http2-connection-open-p connection))
             (ensure-equal 1 (http-kit/http2:http2-connection-manager-connection-count manager))
             (ensure-equal (list connection) (http-kit/http2:http2-connection-manager-connections manager))
             (ensure-equal 201 (http-response-status
                                (http-kit/http2:send-http2-request-over-connection-manager manager request)))
             (ensure-equal 1 opened)
             (ensure-equal 0 closed)
             (ensure-equal (length (binary-session-input stream)) (binary-session-input-position stream))
             (ensure-equal '(1 3 5)
                           (mapcar #'http-kit/http2::%h2-frame-stream-id
                                   (remove-if-not (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                                                  (h2-output-frames stream)))))
        (http-kit/http2:close-http2-connection-manager manager)
        (http-kit/http2:close-http2-connection connection))
      (ensure-equal 1 closed)
      (ensure-equal 1 opened))))

#+sbcl
(deftest http2-public-contract-client-close-stream-validation
  (let ((calls 0))
    (flet ((exchange (request wire &key timeout deadline)
             (declare (ignore request wire timeout deadline))
             (incf calls)
             (h2-response-wire (octets))))
      (dolist (value '(7 "close"))
        (let ((condition (handler-case
                             (http-kit/http2:make-http2-client :exchange #'exchange :close-stream value)
                           (http-protocol-error (error) error))))
          (ensure-true (typep condition 'http-protocol-error))
          (ensure-equal :http2-client (http-error-operation condition))
          (ensure-equal value (http-protocol-error-detail condition))
          (ensure-equal 0 calls)))
      (dolist (client (list (http-kit/http2:make-http2-client :exchange #'exchange)
                            (http-kit/http2:make-http2-client :exchange #'exchange :close-stream #'close)))
        (ensure-true (http-kit/http2:http2-client-p client))
        (ensure-equal 0 calls)))))

#+sbcl
(deftest http2-public-contract-request-trailer-connection-specific
  (dolist (entry '(("HTTP2-Settings" :connection-specific)
                   ("Connection" :forbidden)
                   ("X-End" nil)))
    (destructuring-bind (name reason) entry
      (let* ((stream (make-instance 'binary-session-stream
                                    :input (h2-response-wire (octets))))
             (order nil)
             (produced 0)
             (opened 0)
             (body-calls 0)
             (closed 0)
             (request (make-http-request :method "POST" :uri "https://example.com/"
                                         :trailers (list (cons name "done"))))
             (connection
               (http-kit/http2:make-http2-connection
                :stream stream
                :close-stream (lambda (received)
                                (ensure-true (eq stream received))
                                (push :close order)
                                (incf closed)
                                (close received)))))
        (unwind-protect
             (let ((result
                     (handler-case
                         (http-kit/http2:send-http2-request-over-connection
                          connection request
                          :request-body-length 0
                          :request-body-function
                          (lambda (maximum)
                            (ensure-equal 16384 maximum)
                            (incf produced)
                            (push :producer order)
                            nil)
                          :on-stream-open
                          (lambda (stream-id cancel)
                            (ensure-equal 1 stream-id)
                            (ensure-true (functionp cancel))
                            (incf opened)
                            (push :stream-open order))
                          :on-body-chunk (lambda (chunk)
                                           (declare (ignore chunk))
                                           (incf body-calls)))
                       (http-invalid-header (condition) condition))))
               (ensure-equal 0 body-calls)
               (if reason
                   (progn
                     (ensure-true (typep result 'http-invalid-header))
                     (ensure-equal :http2-trailers (http-error-operation result))
                     (ensure-equal (string-downcase name) (http-invalid-header-name result))
                     (ensure-equal reason (http-invalid-header-reason result))
                     (ensure-equal '(:close) (reverse order))
                     (ensure-equal 0 produced)
                     (ensure-equal 0 opened)
                     (ensure-equal 1 closed)
                     (ensure-equal 0 (binary-session-input-position stream))
                     (ensure-equal 0 (length (binary-session-output stream)))
                     (ensure-false (http-kit/http2:http2-connection-open-p connection))
                     (ensure-false (open-stream-p stream)))
                   (progn
                     (ensure-true (http-response-p result))
                     (ensure-equal 200 (http-response-status result))
                     (ensure-equal (octets) (http-response-body result))
                     (ensure-equal '(:stream-open :producer) (reverse order))
                     (ensure-equal 1 produced)
                     (ensure-equal 1 opened)
                     (ensure-equal 0 closed)
                     (ensure-equal (length (binary-session-input stream))
                                   (binary-session-input-position stream))
                     (ensure-true (http-kit/http2:http2-connection-open-p connection))
                     (ensure-true (open-stream-p stream))
                     (let* ((frames (h2-output-frames stream))
                            (trailer (third frames)))
                       (ensure-equal '(4 1 1 4) (mapcar #'http-kit/http2::%h2-frame-type frames))
                       (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id trailer))
                       (ensure-equal 5 (http-kit/http2::%h2-frame-flags trailer))
                       (ensure-equal '(("x-end" . "done"))
                                     (h2-decode-header-block
                                      (http-kit/http2::%h2-frame-payload trailer)
                                      (h2-hpack-context)))))))
          (http-kit/http2:close-http2-connection connection))
        (ensure-equal 1 closed)
        (ensure-false (open-stream-p stream))
        (ensure-equal (if reason '(:close) '(:stream-open :producer :close))
                      (reverse order))))))

#+sbcl
(progn
  (defun %check-public-h2-server-response-validation
      (response &key operation name reason trailer-p)
    (let* ((stream
             (make-instance
              'binary-session-stream
              :input (concatenate-octets
                      (h2-preface)
                      (h2-frame 4 0 0 (octets))
                      (h2-frame 1 5 1
                                (h2-header-block '(":method" . "GET")
                                                 '(":scheme" . "https")
                                                 '(":authority" . "example.com")
                                                 '(":path" . "/"))))))
           (order nil)
           (handled 0)
           (notified 0)
           (closed 0)
           (handler-request nil)
           (callback-condition nil))
      (labels ((check-protocol-output ()
                 (let* ((frames (server-output-frames
                                 (binary-session-output stream)))
                        (goaway (car (last frames)))
                        (payload (octets 0 0 0 1 0 0 0 1))
                        (wire (h2-frame 7 0 0 payload)))
                   (ensure-equal '(4 4 7)
                                 (mapcar #'http-kit/http2::%h2-frame-type frames))
                   (ensure-equal 0 (http-kit/http2::%h2-frame-flags goaway))
                   (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id goaway))
                   (ensure-equal payload (http-kit/http2::%h2-frame-payload goaway))
                   (ensure-equal wire
                                 (subseq (binary-session-output stream)
                                         (- (length (binary-session-output stream))
                                            (length wire)))))))
        (unwind-protect
             (let ((result
                     (handler-case
                         (multiple-value-list
                          (http-kit/http2:serve-http2-session
                           stream
                           (lambda (request)
                             (ensure-equal 0 handled)
                             (ensure-equal 0 notified)
                             (ensure-equal 0 closed)
                             (ensure-equal nil order)
                             (ensure-true (open-stream-p stream))
                             (ensure-true (http-request-p request))
                             (ensure-equal "GET" (http-request-method request))
                             (ensure-equal "/" (http-request-target request))
                             (ensure-equal '(4 4)
                                           (mapcar #'http-kit/http2::%h2-frame-type
                                                   (server-output-frames
                                                    (binary-session-output stream))))
                             (setf handler-request request)
                             (incf handled)
                             (push :handler order)
                             response)
                           :on-error
                           (lambda (condition request)
                             (ensure-true reason)
                             (ensure-equal 1 handled)
                             (ensure-equal 0 notified)
                             (ensure-equal 0 closed)
                             (ensure-equal '(:handler) (reverse order))
                             (ensure-true (open-stream-p stream))
                             (ensure-true (eq handler-request request))
                             (ensure-true (typep condition 'http-invalid-header))
                             (ensure-equal operation (http-error-operation condition))
                             (ensure-equal name (http-invalid-header-name condition))
                             (ensure-equal reason (http-invalid-header-reason condition))
                             (check-protocol-output)
                             (setf callback-condition condition)
                             (incf notified)
                             (push :on-error order))
                           :close-stream
                           (lambda (received)
                             (ensure-true (eq stream received))
                             (ensure-true (open-stream-p received))
                             (ensure-equal 1 handled)
                             (ensure-equal (if reason 1 0) notified)
                             (ensure-equal 0 closed)
                             (ensure-equal (if reason '(:handler :on-error) '(:handler))
                                           (reverse order))
                             (incf closed)
                             (push :close order)
                             (close received))))
                       (http-invalid-header (condition)
                         (push :caught order)
                         condition))))
               (ensure-equal 1 handled)
               (ensure-equal 1 closed)
               (ensure-false (open-stream-p stream))
               (ensure-equal (length (binary-session-input stream))
                             (binary-session-input-position stream))
               (if reason
                   (progn
                     (ensure-true (eq result callback-condition))
                     (ensure-equal 1 notified)
                     (ensure-equal '(:handler :on-error :close :caught) (reverse order))
                     (check-protocol-output))
                   (let* ((frames (server-output-frames
                                   (binary-session-output stream)))
                          (headers (third frames))
                          (data (fourth frames)))
                     (ensure-equal '(1 :eof) result)
                     (ensure-equal 0 notified)
                     (ensure-equal nil callback-condition)
                     (ensure-equal '(:handler :close) (reverse order))
                     (ensure-equal (if trailer-p '(4 4 1 0 1) '(4 4 1 0))
                                   (mapcar #'http-kit/http2::%h2-frame-type frames))
                     (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id headers))
                     (ensure-equal 4 (http-kit/http2::%h2-frame-flags headers))
                     (ensure-equal (if trailer-p '((":status" . "200"))
                                       '((":status" . "200") ("x-end" . "done")))
                                   (server-response-fields headers))
                     (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id data))
                     (ensure-equal (if trailer-p 0 1) (http-kit/http2::%h2-frame-flags data))
                     (ensure-equal (octets 90) (http-kit/http2::%h2-frame-payload data))
                     (when trailer-p
                       (let ((trailer (fifth frames)))
                         (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id trailer))
                         (ensure-equal 5 (http-kit/http2::%h2-frame-flags trailer))
                         (ensure-equal '(("x-end" . "done"))
                                       (server-response-fields trailer)))))))
          (when (open-stream-p stream)
            (close stream))))))

  (deftest http2-public-contract-server-response-uppercase-header
    (%check-public-h2-server-response-validation
     (make-http-response :status 200 :headers '(("X-End" . "done")) :body (octets 90))
     :operation :http2-headers :name "X-End" :reason :name))

  (deftest http2-public-contract-server-response-connection-header
    (%check-public-h2-server-response-validation
     (make-http-response :status 200 :headers '(("connection" . "close")) :body (octets 90))
     :operation :http2-headers :name "connection" :reason :connection-specific))

  (deftest http2-public-contract-server-response-te-header
    (%check-public-h2-server-response-validation
     (make-http-response :status 200 :headers '(("te" . "trailers")) :body (octets 90))
     :operation :http2-headers :name "te" :reason :forbidden))

  (deftest http2-public-contract-server-response-content-length-trailer
    (%check-public-h2-server-response-validation
     (make-http-response :status 200 :body (octets 90)
                         :trailers '(("content-length" . "1")))
     :operation :http2-trailers :name "content-length" :reason :forbidden))

  (deftest http2-public-contract-server-response-header-control
    (%check-public-h2-server-response-validation
     (make-http-response :status 200 :headers '(("x-end" . "done")) :body (octets 90))))

  (deftest http2-public-contract-server-response-trailer-control
    (%check-public-h2-server-response-validation
     (make-http-response :status 200 :body (octets 90) :trailers '(("x-end" . "done")))
     :trailer-p t)))

#+sbcl
(progn
  (deftest http2-public-contract-open-stream-constructor-invalid-function
    (let* ((callback (copy-seq "not a callback"))
           (failure
             (handler-case
                 (http-kit/http2:make-http2-client :open-stream callback)
               (http-protocol-error (condition) condition))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :http2-client (http-error-operation failure))
      (ensure-equal
       "An HTTP/2 client requires exactly one of :EXCHANGE, :OPEN-STREAM, or :CONNECTION."
       (http-error-message failure))
      (ensure-equal (list nil callback nil) (http-protocol-error-detail failure))
      (ensure-true (eq callback (second (http-protocol-error-detail failure))))))

  (deftest http2-public-contract-open-stream-constructor-control
    (let* ((request (make-http-request :method "GET" :uri "https://example.test/open"))
           (stream (make-instance 'binary-session-stream
                                  :input (h2-response-wire (octets 0 255 7))))
           (opened 0)
           (closed 0)
           (events nil)
           (client
             (http-kit/http2:make-http2-client
              :open-stream
              (lambda (actual &key timeout deadline)
                (ensure-true (eq request actual))
                (ensure-false timeout)
                (ensure-false deadline)
                (incf opened)
                (push :open events)
                stream)
              :close-stream
              (lambda (actual)
                (ensure-true (eq stream actual))
                (incf closed)
                (push :close events)))))
      (ensure-equal 0 opened)
      (let ((response (http-kit/http2:send-http2-request client request)))
        (ensure-equal 200 (http-response-status response))
        (ensure-equal (octets 0 255 7) (http-response-body response)))
      (ensure-equal 1 opened)
      (ensure-equal 1 closed)
      (ensure-equal '(:open :close) (reverse events))
      (ensure-equal (length (binary-session-input stream))
                    (binary-session-input-position stream))
      (ensure-equal '(1)
                    (loop for frame in (h2-output-frames stream)
                          when (= 1 (http-kit/http2::%h2-frame-type frame))
                            collect (http-kit/http2::%h2-frame-stream-id frame)))))

  (deftest http2-public-contract-trailer-name-prevalidation
    (dolist (name '("" ":status"))
      (let ((failure
              (handler-case
                  (make-http-request :method "POST" :uri "https://example.test/trailer"
                                     :trailers (list (cons name "value")))
                (http-invalid-header (condition) condition))))
        (ensure-true (typep failure 'http-invalid-header))
        (ensure-equal :header (http-error-operation failure))
        (ensure-equal :name (http-invalid-header-reason failure))
        (ensure-true (eq name (http-invalid-header-name failure)))
        (ensure-equal "Header names must be non-empty ASCII tokens."
                      (http-error-message failure)))))

  (deftest http2-public-contract-request-trailer-wire-control
    (let* ((request (make-http-request :method "POST" :uri "https://example.test/trailer"
                                     :body (octets 7) :trailers '(("X-End" . "done"))))
           (stream (make-instance 'binary-session-stream :input (h2-response-wire (octets))))
           (closed 0)
           (opened 0)
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream :close-stream
              (lambda (actual)
                (ensure-true (eq stream actual))
                (incf closed)))))
      (unwind-protect
           (progn
             (let ((response
                     (http-kit/http2:send-http2-request-over-connection
                      connection request :on-stream-open
                      (lambda (id cancel)
                        (ensure-equal 1 id)
                        (ensure-true (functionp cancel))
                        (incf opened)))))
               (ensure-equal 200 (http-response-status response))
               (ensure-equal (octets) (http-response-body response)))
             (let* ((frames (h2-output-frames stream))
                    (headers (remove-if-not
                              (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                              frames))
                    (data (remove-if-not
                           (lambda (frame) (= 0 (http-kit/http2::%h2-frame-type frame)))
                           frames)))
               (ensure-equal 2 (length headers))
               (ensure-equal '(1 1) (mapcar #'http-kit/http2::%h2-frame-stream-id headers))
               (ensure-equal 0 (logand 1 (http-kit/http2::%h2-frame-flags (first headers))))
               (ensure-equal 1 (logand 1 (http-kit/http2::%h2-frame-flags (second headers))))
               (ensure-equal (h2-header-block '("x-end" . "done"))
                             (http-kit/http2::%h2-frame-payload (second headers)))
               (ensure-equal 1 (length data))
               (ensure-equal (octets 7) (http-kit/http2::%h2-frame-payload (first data)))
               (ensure-equal 0 (logand 1 (http-kit/http2::%h2-frame-flags (first data)))))
             (ensure-equal "X-End" (http-header-name (first (http-request-trailers request))))
             (ensure-equal "done" (http-header-content (first (http-request-trailers request))))
             (ensure-equal 1 opened)
             (ensure-equal 0 closed)
             (ensure-equal (length (binary-session-input stream))
                           (binary-session-input-position stream)))
        (http-kit/http2:close-http2-connection connection))
      (http-kit/http2:close-http2-connection connection)
      (ensure-equal 1 closed)))

  (deftest http2-public-contract-stream-id-exhaustion-lifecycle
    (let* ((request (make-http-request :method "GET" :uri "https://example.test/last"))
           (stream (make-instance 'binary-session-stream
                                  :input (concatenate-octets
                                          (h2-response-wire (octets))
                                          (h2-frame 1 5 #x7fffffff (octets #x88)))))
           (closed 0)
           (opened 0)
           (chunks 0)
           (events nil)
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream :close-stream
              (lambda (actual)
                (ensure-true (eq stream actual))
                (incf closed)
                (push :close events)))))
      (unwind-protect
           (progn
             (ensure-equal 200
                           (http-response-status
                            (http-kit/http2:send-http2-request-over-connection connection request)))
             (ensure-equal 3 (http-kit/http2::%http2-connection-next-stream-id connection))
             (ensure-true (http-kit/http2::%http2-connection-session-started-p connection))
             (ensure-false (http-kit/http2::%http2-connection-peer-stream-windows connection))
             (ensure-equal 65535
                           (http-kit/http2:http2-connection-peer-connection-window-size connection))
             (ensure-true (http-kit/http2:http2-connection-open-p connection))
             (ensure-false (http-kit/http2:http2-connection-draining-p connection))
             (ensure-false (http-kit/http2:http2-connection-goaway-last-stream-id connection))
             (ensure-false (http-kit/http2:http2-connection-local-goaway-last-stream-id connection))
             (let ((context (http-kit/http2::%http2-connection-hpack-context connection)))
               (ensure-equal 0 (http-message-kit:hpack-context-dynamic-entry-count context))
               (ensure-equal 0 (http-message-kit:hpack-context-table-size context)))
             ;; Completed empty-body streams leave no flow-control entries or HPACK
             ;; dynamic entries. Skip their equivalent ID increments, not lifecycle state.
             (setf (http-kit/http2::%http2-connection-next-stream-id connection) #x7fffffff)
             (let ((response
                     (http-kit/http2:send-http2-request-over-connection
                      connection request :on-stream-open
                      (lambda (id cancel)
                        (ensure-equal #x7fffffff id)
                        (ensure-true (functionp cancel))
                        (incf opened)
                        (push :last-open events)))))
               (ensure-equal 200 (http-response-status response))
               (ensure-equal (octets) (http-response-body response)))
             (ensure-equal #x80000001
                           (http-kit/http2::%http2-connection-next-stream-id connection))
             (ensure-true (http-kit/http2:http2-connection-open-p connection))
             (ensure-equal 0 closed)
             (ensure-equal (length (binary-session-input stream))
                           (binary-session-input-position stream))
             (let* ((frames (h2-output-frames stream))
                    (headers (remove-if-not
                              (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame))) frames)))
               (ensure-equal '(1 #x7fffffff)
                             (mapcar #'http-kit/http2::%h2-frame-stream-id headers))
               (ensure-true (every (lambda (frame)
                                     (= 1 (logand 1 (http-kit/http2::%h2-frame-flags frame))))
                                   headers))
               (let ((settings (remove-if-not
                                (lambda (frame) (= 4 (http-kit/http2::%h2-frame-type frame)))
                                frames)))
                 (ensure-equal '(0 1) (mapcar #'http-kit/http2::%h2-frame-flags settings))
                 (ensure-equal '(0 0) (mapcar #'http-kit/http2::%h2-frame-stream-id settings))
                 (ensure-equal (octets) (http-kit/http2::%h2-frame-payload (second settings))))
               (ensure-equal 0 (count 0 frames :key #'http-kit/http2::%h2-frame-type)))
             (let* ((wire (copy-seq (binary-session-output stream)))
                    (failure
                      (handler-case
                          (http-kit/http2:send-http2-request-over-connection
                           connection request
                           :on-stream-open (lambda (id cancel)
                                             (declare (ignore id cancel)) (incf opened))
                           :on-body-chunk (lambda (chunk) (declare (ignore chunk)) (incf chunks)))
                        (http-connection-error (condition) condition))))
               (ensure-true (typep failure 'http-connection-error))
               (ensure-equal :http2-write (http-error-operation failure))
               (ensure-equal :stream-id-exhausted (http-connection-error-cause failure))
               (ensure-equal "The HTTP/2 client exhausted its stream identifiers."
                             (http-error-message failure))
               (ensure-equal wire (binary-session-output stream)))
             (ensure-equal 1 opened)
             (ensure-equal 0 chunks)
             (ensure-equal 1 closed)
             (ensure-false (http-kit/http2:http2-connection-open-p connection))
             (ensure-equal '(:last-open :close) (reverse events)))
        (http-kit/http2:close-http2-connection connection))
      (http-kit/http2:close-http2-connection connection)
      (ensure-equal 1 closed)))

  (deftest http2-public-contract-batch-zero-stream-prevalidation
    (let* ((stream (make-instance 'binary-session-stream
                                  :input (concatenate-octets
                                          (h2-frame 4 0 0 (octets))
                                          (h2-frame 1 5 0 (octets #x88)))))
           (closed 0)
           (callbacks 0)
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream :close-stream
              (lambda (actual) (ensure-true (eq stream actual)) (incf closed))))
           (requests (list (make-http-request :method "GET" :uri "https://example.test/one")
                           (make-http-request :method "GET" :uri "https://example.test/two")))
           (failure
             (handler-case
                 (http-kit/http2:send-http2-requests-over-connection
                  connection requests :on-body-chunk
                  (lambda (request chunk) (declare (ignore request chunk)) (incf callbacks)))
               (http-protocol-error (condition) condition))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :http2-read (http-error-operation failure))
      (ensure-equal 0 (http-protocol-error-detail failure))
      (ensure-equal "The HTTP/2 batch received HEADERS for unknown stream 0."
                    (http-error-message failure))
      (ensure-equal 0 callbacks)
      (ensure-equal 1 closed)
      (ensure-false (http-kit/http2:http2-connection-open-p connection))
      (ensure-equal '(1 3)
                    (loop for frame in (h2-output-frames stream)
                          when (= 1 (http-kit/http2::%h2-frame-type frame))
                            collect (http-kit/http2::%h2-frame-stream-id frame)))
      (ensure-equal (length (binary-session-input stream))
                    (binary-session-input-position stream))
      (http-kit/http2:close-http2-connection connection)
      (ensure-equal 1 closed))))

#+sbcl
(progn
  (defun %h2-public-huffman-response-contract
      (encoded-string expected &optional error-detail error-message)
    (let* ((block (concatenate-octets
                   (octets #x88 #x00 #x01 #x78)
                   encoded-string))
           (stream (make-instance 'binary-session-stream
                                  :input (concatenate-octets
                                          (h2-frame 4 0 0 (octets))
                                          (h2-frame 1 5 1 block))))
           (closed 0)
           (opened 0)
           (chunks 0)
           (events nil)
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream :close-stream
              (lambda (actual)
                (ensure-true (eq stream actual))
                (incf closed)
                (push :close events)))))
      (unwind-protect
           (let ((result
                   (handler-case
                       (http-kit/http2:send-http2-request-over-connection
                        connection
                        (make-http-request :method "GET" :uri "https://example.test/huffman")
                        :on-stream-open
                        (lambda (id cancel)
                          (ensure-equal 1 id)
                          (ensure-true (functionp cancel))
                          (incf opened)
                          (push :open events))
                        :on-body-chunk
                        (lambda (chunk) (declare (ignore chunk)) (incf chunks)))
                     (http-protocol-error (condition) condition))))
             (if error-message
                 (progn
                   (ensure-true (typep result 'http-protocol-error))
                   (ensure-equal :hpack (http-error-operation result))
                   (let ((detail (http-protocol-error-detail result)))
                     (ensure-equal 4 (length detail))
                     (ensure-equal error-detail (list (first detail) (second detail) (third detail)))
                     (ensure-true (typep (fourth detail) 'condition)))
                   (ensure-equal error-message (http-error-message result))
                   (ensure-equal 1 closed)
                   (ensure-false (http-kit/http2:http2-connection-open-p connection)))
                 (progn
                   (ensure-true (typep result 'http-response))
                   (ensure-equal 200 (http-response-status result))
                   (ensure-equal expected (http-header-value (http-response-headers result) "x"))
                   (ensure-equal (octets) (http-response-body result))
                   (ensure-equal 0 closed)
                   (ensure-true (http-kit/http2:http2-connection-open-p connection))))
             (ensure-equal 1 opened)
             (ensure-equal 0 chunks)
             (ensure-equal (length (binary-session-input stream))
                           (binary-session-input-position stream))
             (ensure-equal '(1)
                           (loop for frame in (h2-output-frames stream)
                                 when (= 1 (http-kit/http2::%h2-frame-type frame))
                                   collect (http-kit/http2::%h2-frame-stream-id frame)))
             (ensure-equal 0 (count 0 (h2-output-frames stream)
                                    :key #'http-kit/http2::%h2-frame-type)))
        (http-kit/http2:close-http2-connection connection))
      (http-kit/http2:close-http2-connection connection)
      (ensure-equal 1 closed)
      (ensure-equal '(:open :close) (reverse events))))

  (deftest http2-public-contract-huffman-normal-response
    (%h2-public-huffman-response-contract
     (concatenate-octets
      (http-message-kit:hpack-encode-integer 12 7 #x80)
      (octets #xf1 #xe3 #xc2 #xe5 #xf2 #x3a #x6b #xa0 #xab #x90 #xf4 #xff))
     "www.example.com")
    (%h2-public-huffman-response-contract
     (http-message-kit:hpack-encode-string "" :huffman t) "")
    (let ((text (concatenate 'string "abcdefghijklmnopqrstuvwxyz"
                             "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")))
      (%h2-public-huffman-response-contract
       (http-message-kit:hpack-encode-string text :huffman t) text)))

  (deftest http2-public-contract-huffman-eos-response-rejected
    (%h2-public-huffman-response-contract
     (concatenate-octets
      (http-message-kit:hpack-encode-integer 4 7 #x80)
      (octets #xff #xff #xff #xfc))
     nil '(:codec-operation :hpack-string :codec-detail)
     "An HPACK Huffman string is invalid."))

  (deftest http2-public-contract-huffman-excessive-padding-response-rejected
    (%h2-public-huffman-response-contract
     (concatenate-octets
      (http-message-kit:hpack-encode-integer 1 7 #x80)
      (octets #xff))
     nil '(:codec-operation :hpack-string :codec-detail)
     "An HPACK Huffman string is invalid."))

  (deftest http2-public-contract-huffman-invalid-padding-response-rejected
    (%h2-public-huffman-response-contract
     (concatenate-octets
      (http-message-kit:hpack-encode-integer 1 7 #x80)
      (octets #x1e))
     nil '(:codec-operation :hpack-string :codec-detail)
     "An HPACK Huffman string is invalid.")))
