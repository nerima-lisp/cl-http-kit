(in-package #:http-kit/test)

(deftest http2-public-transport-and-binary-body
  (let* ((expected-preface (h2-preface))
         (transport
           (http-kit/http2:make-http2-client
            :exchange (lambda (request wire &key timeout deadline)
                        (declare (ignore request timeout deadline))
                        (ensure-equal expected-preface
                                      (subseq wire 0 (length expected-preface))
                                      "HTTP/2 client preface")
                        (h2-response-wire (octets 0 #xff 7)))))
         (response (http-kit/http2:send-http2-request
                    transport
                    (make-http-request :method "GET"
                                       :uri "https://127.0.0.1/data"))))
    (ensure-equal 200 (http-response-status response) "HTTP/2 status")
    (ensure-equal "HTTP/2"
                  (http-response-protocol-version response)
                  "HTTP/2 response version")
    (ensure-equal (octets 0 #xff 7) (http-response-body response)
                  "HTTP/2 binary body")))

(deftest http2-streams-response-body-without-collecting
  (let* ((chunks '())
        (response
          (http-kit/http2:send-http2-request
           (http-kit/http2:make-http2-client
            :exchange (lambda (request wire &key timeout deadline)
                        (declare (ignore request wire timeout deadline))
                        (concatenate-octets
                         (h2-frame 4 0 0 (octets))
                         (h2-frame 1 4 1
                                   (h2-header-block
                                    (cons ":status" "200")
                                    (cons "content-length" "4")))
                         (h2-frame 0 0 1 (octets 1 2))
                         (h2-frame 0 1 1 (octets 3 4)))))
           (make-http-request :method "GET" :uri "https://127.0.0.1/")
           :on-body-chunk (lambda (chunk)
                            (push chunk chunks))
           :collect-body-p nil)))
    (ensure-equal (octets) (http-response-body response)
                  "HTTP/2 non-collecting response body")
    (ensure-equal (list (octets 1 2) (octets 3 4))
                  (nreverse chunks)
                  "HTTP/2 DATA callbacks preserve frame boundaries")))

(progn
  (deftest http2-response-headers-callback-precedes-body-and-skips-information
    (let* ((events nil)
           (response
             (http-kit/http2:send-http2-request
              (http-kit/http2:make-http2-client
               :exchange
               (lambda (request wire &key timeout deadline)
                 (declare (ignore request wire timeout deadline))
                 (concatenate-octets
                  (h2-frame 4 0 0 (octets))
                  (h2-frame 1 4 1 (h2-header-block (cons ":status" "103")))
                  (h2-frame 1 4 1
                            (h2-header-block (cons ":status" "200")
                                             (cons "content-type"
                                                   "text/event-stream")
                                             (cons "content-length" "2")))
                  (h2-frame 0 1 1 (octets 65 66)))))
              (make-http-request :method "GET" :uri "https://example.test/")
              :on-response-headers
              (lambda (status headers)
                (push (list :headers status
                            (http-header-value headers "content-type"))
                      events))
              :on-body-chunk
              (lambda (chunk)
                (push (list :body (copy-seq chunk)) events)))))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal
       (list (list :headers 200 "text/event-stream")
             (list :body (octets 65 66)))
       (nreverse events))))
  (deftest http2-response-header-http-error-is-preserved
    (let* ((input
             (concatenate-octets
              (h2-frame 4 0 0 (octets))
              (h2-frame 1 5 1
                        (h2-header-block (cons ":status" "200")))))
           (stream (make-instance 'binary-session-stream :input input))
           (connection (http-kit/http2:make-http2-connection :stream stream))
           (request (make-http-request :method "GET"
                                       :uri "https://example.test/reject"))
           (callback-error
             (make-condition 'http-protocol-error
                             :message "response rejected"
                             :operation :test-callback
                             :detail :rejected))
           (observed-error nil))
      (unwind-protect
           (progn
             (handler-case
                 (http-kit/http2::%h2-send-http2-request-over-connection-serial
                  connection request
                  :on-response-headers
                  (lambda (status headers)
                    (declare (ignore status headers))
                    (error callback-error)))
               (error (condition)
                 (setf observed-error condition)))
             (ensure-true (eq callback-error observed-error))
             (ensure-true (http-kit/http2:http2-connection-open-p connection)))
        (http-kit/http2:close-http2-connection connection))))
  (deftest http2-response-cancel-callbacks-use-default-code
    (dolist (sender
             (list #'http-kit/http2::%h2-send-http2-request-over-connection-serial
                   #'http-kit/http2:send-http2-request-over-connection))
      (let* ((stream
               (make-instance 'binary-session-stream
                              :input (h2-frame 4 0 0 (octets))))
             (connection (http-kit/http2:make-http2-connection :stream stream))
             (request (make-http-request :method "GET"
                                         :uri "https://example.test/cancel"))
             (failure nil))
        (unwind-protect
             (progn
               (handler-case
                   (funcall sender
                            connection request
                            :on-cancel
                            (lambda (cancel-function)
                              (funcall cancel-function)
                              nil))
                 (error (condition)
                   (setf failure condition)))
               (ensure-true (typep failure 'http-connection-error))
               (ensure-equal '(:local-cancel 1 8)
                             (http-connection-error-cause failure)))
          (http-kit/http2:close-http2-connection connection)))))

  (deftest http2-response-callbacks-cover-serial-and-scheduled-transport
    (let* ((input
             (concatenate-octets
              (h2-frame 4 0 0 (octets))
              (h2-frame 1 4 1
                        (h2-header-block (cons ":status" "200")
                                         (cons "content-length" "2")))
              (h2-frame 0 1 1 (octets 65 66))))
           (stream (make-instance 'binary-session-stream :input input))
           (connection (http-kit/http2:make-http2-connection :stream stream))
           (request (make-http-request :method "GET"
                                      :uri "https://example.test/serial"))
           (events nil)
           (response nil))
      (unwind-protect
           (progn
             (setf response
                   (http-kit/http2::%h2-send-http2-request-over-connection-serial
                    connection request
                    :on-response-headers
                    (lambda (status headers)
                      (ensure-equal 200 status)
                      (ensure-equal "2" (http-header-value headers "content-length"))
                      (push :headers events))
                    :on-body-chunk
                    (lambda (chunk)
                      (declare (ignore chunk))
                      (push :body events))
                    :on-cancel
                    (lambda (cancel-function)
                      (ensure-true (functionp cancel-function))
                      (push :registered events)
                      (lambda () (push :cleanup events)))))
             (ensure-equal 200 (http-response-status response))
             (ensure-equal (octets 65 66) (http-response-body response))
             (ensure-equal '(:registered :headers :body :cleanup)
                           (nreverse events)))
        (http-kit/http2:close-http2-connection connection)))
    (let* ((input
             (concatenate-octets
              (h2-frame 4 0 0 (octets))
              (h2-frame 1 5 1
                        (h2-header-block (cons ":status" "200")))))
           (stream (make-instance 'binary-session-stream :input input))
           (connection (http-kit/http2:make-http2-connection :stream stream))
           (request (make-http-request :method "GET"
                                      :uri "https://example.test/reject"))
           (callback-error
             (make-condition 'simple-error
                             :format-control "response rejected"
                             :format-arguments nil))
           (observed-error nil))
      (unwind-protect
           (progn
             (handler-case
                 (http-kit/http2::%h2-send-http2-request-over-connection-serial
                  connection request
                  :on-response-headers
                  (lambda (status headers)
                    (declare (ignore status headers))
                    (error callback-error)))
               (error (condition)
                 (setf observed-error condition)))
             (ensure-true (eq callback-error observed-error))
             (ensure-true (http-kit/http2:http2-connection-open-p connection))
             (let ((reset-frame
                     (find-if
                      (lambda (frame)
                        (and (= 3 (http-kit/http2::%h2-frame-type frame))
                             (= 1 (http-kit/http2::%h2-frame-stream-id frame))))
                      (h2-output-frames stream))))
               (ensure-true reset-frame)
               (ensure-equal (octets 0 0 0 8)
                             (http-kit/http2::%h2-frame-payload reset-frame))))
        (http-kit/http2:close-http2-connection connection)))
    (let* ((stream (make-instance 'binary-session-stream :input (octets)))
           (connection (http-kit/http2:make-http2-connection :stream stream))
           (request (make-http-request :method "GET"
                                      :uri "https://example.test/invalid")))
      (unwind-protect
           (progn
             (signals http-protocol-error
               (http-kit/http2::%h2-send-http2-request-over-connection-serial
                connection request :on-response-headers :invalid))
             (signals http-protocol-error
               (http-kit/http2::%h2-send-http2-request-over-connection-serial
                connection request :on-cancel :invalid))
             (signals http-protocol-error
               (http-kit/http2:send-http2-request-over-connection
                connection request :on-response-headers :invalid))
             (signals http-protocol-error
               (http-kit/http2:send-http2-request-over-connection
                connection request :on-cancel :invalid))
             (ensure-equal 0 (length (binary-session-output stream))))
        (http-kit/http2:close-http2-connection connection)))
    (let* ((stream
             (make-instance 'binary-session-stream
                            :input (h2-frame 4 0 0 (octets))))
           (connection (http-kit/http2:make-http2-connection :stream stream))
           (request (make-http-request :method "GET"
                                      :uri "https://example.test/invalid-cleanup"))
           (failure nil))
      (unwind-protect
           (progn
             (handler-case
                 (http-kit/http2::%h2-send-http2-request-over-connection-serial
                  connection request
                  :on-cancel
                  (lambda (cancel-function)
                    (declare (ignore cancel-function))
                    :invalid))
               (http-protocol-error (condition)
                 (setf failure condition)))
             (ensure-true (typep failure 'http-protocol-error))
             (ensure-equal :invalid (http-protocol-error-detail failure)))
        (http-kit/http2:close-http2-connection connection)))
    (let* ((stream
             (make-instance 'binary-session-stream
                            :input (h2-frame 4 0 0 (octets))))
           (connection (http-kit/http2:make-http2-connection :stream stream))
           (request (make-http-request :method "GET"
                                      :uri "https://example.test/cancel"))
           (failure nil))
      (unwind-protect
           (progn
             (handler-case
                 (http-kit/http2:send-http2-request-over-connection
                  connection request
                  :on-cancel
                  (lambda (cancel-function)
                    (funcall cancel-function)
                    nil))
               (error (condition)
                 (setf failure condition)))
             (ensure-true (typep failure 'http-connection-error))
             (ensure-equal '(:local-cancel 1 8)
                           (http-connection-error-cause failure))
             (ensure-true (http-kit/http2:http2-connection-open-p connection)))
        (http-kit/http2:close-http2-connection connection)))
    (let* ((stream
             (make-instance 'binary-session-stream
                            :input (h2-frame 4 0 0 (octets))))
           (connection (http-kit/http2:make-http2-connection :stream stream))
           (request (make-http-request :method "GET"
                                      :uri "https://example.test/invalid-cleanup"))
           (failure nil))
      (unwind-protect
           (progn
             (handler-case
                 (http-kit/http2:send-http2-request-over-connection
                  connection request
                  :on-cancel
                  (lambda (cancel-function)
                    (declare (ignore cancel-function))
                    :invalid))
               (http-protocol-error (condition)
                 (setf failure condition)))
             (ensure-true (typep failure 'http-protocol-error))
             (ensure-equal :invalid (http-protocol-error-detail failure)))
        (http-kit/http2:close-http2-connection connection))))
)

(deftest http2-accepts-padded-response-frames
  (let* ((header-block (h2-header-block
                        (cons ":status" "200")
                        (cons "content-length" "3")))
         (response
           (http-kit/http2:send-http2-request
            (http-kit/http2:make-http2-client
             :exchange (lambda (request wire &key timeout deadline)
                         (declare (ignore request wire timeout deadline))
                         (concatenate-octets
                          (h2-frame 4 0 0 (octets))
                          (h2-frame 1 12 1
                                    (concatenate-octets
                                     (octets 2)
                                     header-block
                                     (octets 0 0)))
                          (h2-frame 0 9 1
                                    (octets 2 1 2 3 0 0)))))
            (make-http-request :method "GET"
                               :uri "https://127.0.0.1/"))))
    (ensure-equal 200 (http-response-status response)
                  "Padded HTTP/2 response status")
    (ensure-equal (octets 1 2 3) (http-response-body response)
                  "Padded HTTP/2 response body")))

(deftest http2-accepts-priority-response-headers
  (let ((response
          (http-kit/http2:send-http2-request
           (http-kit/http2:make-http2-client
            :exchange (lambda (request wire &key timeout deadline)
                        (declare (ignore request wire timeout deadline))
                        (concatenate-octets
                         (h2-frame 4 0 0 (octets))
                         (h2-frame 1 37 1
                                   (concatenate-octets
                                    (octets 0 0 0 0 0)
                                    (h2-header-block (cons ":status" "204")))))))
           (make-http-request :method "GET"
                              :uri "https://127.0.0.1/"))))
    (ensure-equal 204 (http-response-status response)
                  "Priority HTTP/2 response status")
    (ensure-equal (octets) (http-response-body response)
                  "Priority HTTP/2 response body")))

(deftest http2-sends-window-updates-after-data
  (let* ((sent '())
         (response
           (http-kit/http2::%h2-read-response
            (http-kit/http2::%h2-reader-for
             (let ((header-block (h2-header-block
                                  (cons ":status" "200")
                                  (cons "content-length" "4"))))
               (concatenate-octets
                (h2-frame 4 0 0 (octets))
                (h2-frame 1 4 1 header-block)
                (h2-frame 0 8 1 (octets 2 1 2 3 0 0))
                (h2-frame 0 1 1 (octets 4)))))
            (lambda (wire) (push wire sent))
            16384 nil nil 65536 65536 "GET" nil t)))
    (ensure-equal 200 (http-response-status response)
                  "WINDOW_UPDATE response status")
    (ensure-equal (octets 1 2 3 4) (http-response-body response)
                  "WINDOW_UPDATE response body")
    (ensure-equal
     (list (h2-frame 8 0 0 (octets 0 0 0 1))
           (h2-frame 8 0 0 (octets 0 0 0 6))
           (h2-frame 8 0 1 (octets 0 0 0 6))
           (h2-frame 4 1 0 (octets)))
     sent
     "HTTP/2 stream and connection receive windows are restored")))

(deftest http2-rejects-duplicate-host
  (signals http-invalid-header
    (http-kit/http2:send-http2-request
     (http-kit/http2:make-http2-client
      :exchange (lambda (request wire &key timeout deadline)
                  (declare (ignore request wire timeout deadline))
                  (error "The exchange must not run for duplicate Host.")))
     (make-http-request :method "GET"
                        :uri "https://127.0.0.1/"
                        :headers (list (make-http-header "Host" "127.0.0.1")
                                       (make-http-header "host" "127.0.0.1"))))))

(deftest http2-rejects-uppercase-response-header
  (let ((uppercase-headers
          (apply #'h2-header-block (list (cons ":status" "200")
                 (cons "X-Test" "yes")))))
    (signals http-invalid-header
      (http-kit/http2:send-http2-request
       (http-kit/http2:make-http2-client
        :exchange (lambda (request wire &key timeout deadline)
                    (declare (ignore request wire timeout deadline))
                    (concatenate-octets
                     (h2-frame 4 0 0 (octets))
                     (h2-frame 1 4 1 uppercase-headers))))
       (make-http-request :method "GET" :uri "https://127.0.0.1/")))))

(deftest http2-304-response-has-no-body
  (let ((not-modified-headers
          (apply #'h2-header-block (list (cons ":status" "304")
                 (cons "content-length" "3")))))
    (let ((response
            (http-kit/http2:send-http2-request
             (http-kit/http2:make-http2-client
              :exchange (lambda (request wire &key timeout deadline)
                          (declare (ignore request wire timeout deadline))
                          (concatenate-octets
                           (h2-frame 4 0 0 (octets))
                           (h2-frame 1 5 1 not-modified-headers))))
             (make-http-request :method "GET" :uri "https://127.0.0.1/"))))
      (ensure-equal 304 (http-response-status response)
                    "HTTP/2 304 status")
      (ensure-equal (octets) (http-response-body response)
                    "HTTP/2 304 has no message body"))))

(deftest http2-204-response-rejects-content-length
  (let ((no-content-headers
          (apply #'h2-header-block (list (cons ":status" "204")
                 (cons "content-length" "1")))))
    (signals http-invalid-header
      (http-kit/http2:send-http2-request
       (http-kit/http2:make-http2-client
        :exchange (lambda (request wire &key timeout deadline)
                    (declare (ignore request wire timeout deadline))
                    (concatenate-octets
                     (h2-frame 4 0 0 (octets))
                     (h2-frame 1 5 1 no-content-headers))))
       (make-http-request :method "GET" :uri "https://127.0.0.1/")))))

(deftest http2-enforces-header-size-limit
  (let ((large-header-block (make-array 5
                                        :element-type '(unsigned-byte 8)
                                        :initial-element #x88)))
    (signals http-size-limit-exceeded
      (http-kit/http2:send-http2-request
       (http-kit/http2:make-http2-client
        :exchange (lambda (request wire &key timeout deadline)
                    (declare (ignore request wire timeout deadline))
                    (concatenate-octets
                     (h2-frame 4 0 0 (octets))
                     (h2-frame 1 5 1 large-header-block))))
       (make-http-request :method "GET" :uri "https://127.0.0.1/")
       :max-header-bytes 4))))

(deftest http2-enforces-response-field-count-limit
  (let ((response-headers
          (h2-header-block
           (cons ":status" "200")
           (cons "content-type" "text/plain"))))
    (signals http-size-limit-exceeded
      (http-kit/http2:send-http2-request
       (http-kit/http2:make-http2-client
        :exchange (lambda (request wire &key timeout deadline)
                    (declare (ignore request wire timeout deadline))
                    (concatenate-octets
                     (h2-frame 4 0 0 (octets))
                     (h2-frame 1 5 1 response-headers))))
       (make-http-request :method "GET" :uri "https://127.0.0.1/")
       :max-fields 1))))

(deftest http2-request-cannot-weaken-client-field-count-limit
  (let ((response-headers
          (h2-header-block
           (cons ":status" "200")
           (cons "content-type" "text/plain"))))
    (signals http-size-limit-exceeded
      (http-kit/http2:send-http2-request
       (http-kit/http2:make-http2-client
        :exchange (lambda (request wire &key timeout deadline)
                    (declare (ignore request wire timeout deadline))
                    (concatenate-octets
                     (h2-frame 4 0 0 (octets))
                     (h2-frame 1 5 1 response-headers)))
        :max-fields 1)
       (make-http-request :method "GET" :uri "https://127.0.0.1/")
       :max-fields 2))))

(deftest http2-rejects-oversized-table-update
  (let ((oversized-table-update
          (http-message-kit:hpack-encode-integer 4097 5 0)))
    (signals http-protocol-error
      (http-kit/http2:send-http2-request
       (http-kit/http2:make-http2-client
        :exchange (lambda (request wire &key timeout deadline)
                    (declare (ignore request wire timeout deadline))
                    (concatenate-octets
                     (h2-frame 4 0 0 (octets))
                     (h2-frame 1 5 1
                               (concatenate-octets oversized-table-update
                                                    (octets #x88))))))
       (make-http-request :method "GET" :uri "https://127.0.0.1/")))))

(deftest http2-keeps-dynamic-table-local-limit
  (let ((dynamic-indexed-block
          (concatenate-octets
           (octets #x88 #x40 6)
           (ascii "x-test")
           (octets 1)
           (ascii "a")
           (octets #xbe))))
    (let ((response
            (http-kit/http2:send-http2-request
             (http-kit/http2:make-http2-client
              :exchange (lambda (request wire &key timeout deadline)
                          (declare (ignore request wire timeout deadline))
                          (concatenate-octets
                           (h2-frame 4 0 0 (octets 0 1 0 0 0 0))
                           (h2-frame 1 5 1 dynamic-indexed-block))))
             (make-http-request :method "GET" :uri "https://127.0.0.1/"))))
      (ensure-equal 200 (http-response-status response)
                    "HPACK decoder keeps its local table limit")
      (ensure-equal '("a" "a")
                    (http-header-values (http-response-headers response)
                                        "x-test")
                     "HPACK dynamic table response fields"))))

#+sbcl
(deftest http2-public-single-preflight-preserves-connection
  (let* ((request (make-http-request :method "GET" :uri "https://example.test/"))
         (stream (make-instance 'binary-session-stream
                                :input (h2-response-wire (octets 65 66))))
         (closes 0)
         (connection
           (http-kit/http2:make-http2-connection
            :stream stream
            :close-stream (lambda (actual)
                            (ensure-true (eq actual stream))
                            (incf closes)
                            (close actual)))))
    (unwind-protect
         (progn
           (dolist (operation
                    (list (lambda ()
                            (http-kit/http2:send-http2-request-over-connection
                             :invalid request))
                          (lambda ()
                            (http-kit/http2:make-http2-connection-transport :invalid))))
             (let ((failure (handler-case (funcall operation)
                              (http-protocol-error (condition) condition))))
               (ensure-true (typep failure 'http-protocol-error))
               (ensure-equal :http2-client (http-error-operation failure))
               (ensure-equal (type-of :invalid)
                             (http-protocol-error-detail failure))))
           (dolist (options '((:on-body-chunk :invalid)
                              (:on-stream-open :invalid)
                              (:collect-body-p :invalid)))
             (let ((failure
                     (handler-case
                         (apply #'http-kit/http2:send-http2-request-over-connection
                                connection request options)
                       (http-protocol-error (condition) condition))))
               (ensure-true (typep failure 'http-protocol-error))
               (ensure-equal :http2-client (http-error-operation failure))
               (ensure-equal :invalid (http-protocol-error-detail failure))
               (ensure-equal 0 (length (binary-session-output stream)))
               (ensure-equal 0 (binary-session-input-position stream))
               (ensure-equal 0 closes)
               (ensure-true (http-kit/http2:http2-connection-open-p connection))))
           (let* ((chunks nil)
                  (opened nil)
                  (response
                    (http-kit/http2:send-http2-request-over-connection
                     connection request
                     :on-stream-open (lambda (id cancel)
                                       (ensure-true (functionp cancel))
                                       (push id opened))
                     :on-body-chunk (lambda (chunk) (push chunk chunks))
                     :collect-body-p nil)))
             (ensure-equal 200 (http-response-status response))
             (ensure-equal (octets) (http-response-body response))
             (ensure-equal (list (octets 65 66)) (nreverse chunks))
             (ensure-equal '(1) opened)
             (ensure-equal (length (binary-session-input stream))
                           (binary-session-input-position stream))
             (ensure-equal 0 closes)
             (let ((frames (h2-output-frames stream)))
               (ensure-equal '(4 1 4 8)
                             (mapcar #'http-kit/http2::%h2-frame-type frames))
               (ensure-equal '(0 5 1 0)
                             (mapcar #'http-kit/http2::%h2-frame-flags frames))
               (ensure-equal '(0 1 0 0)
                             (mapcar #'http-kit/http2::%h2-frame-stream-id frames))
               (dolist (frame (last frames 1))
                 (ensure-equal (octets 0 0 0 2)
                               (http-kit/http2::%h2-frame-payload frame)))))
           (http-kit/http2:close-http2-connection connection)
           (let ((sent (copy-seq (binary-session-output stream)))
                 (failure
                   (handler-case
                       (http-kit/http2:send-http2-request-over-connection
                        connection request)
                     (http-connection-error (condition) condition))))
             (ensure-true (typep failure 'http-connection-error))
             (ensure-equal :http2-client (http-error-operation failure))
             (ensure-equal :closed (http-connection-error-cause failure))
             (ensure-equal sent (binary-session-output stream))
             (ensure-false (open-stream-p stream))))
      (http-kit/http2:close-http2-connection connection))
    (ensure-equal 1 closes)))

#+sbcl
(deftest http2-public-exchange-preflight-and-factory-cleanup
  (let* ((request (make-http-request :method "GET" :uri "https://example.test/"))
         (exchanges 0)
         (produced 0)
         (client
           (http-kit/http2:make-http2-client
            :exchange
            (lambda (actual wire &key timeout deadline)
              (declare (ignore timeout deadline))
              (ensure-true (eq request actual))
              (ensure-equal (h2-preface) (subseq wire 0 (length (h2-preface))))
              (incf exchanges)
              (h2-response-wire (octets 0 255))))))
    (dolist (options '((:on-body-chunk :invalid) (:collect-body-p :invalid)))
      (let ((failure
              (handler-case
                  (apply #'http-kit/http2:send-http2-request client request options)
                (http-protocol-error (condition) condition))))
        (ensure-true (typep failure 'http-protocol-error))
        (ensure-equal :http2-client (http-error-operation failure))
        (ensure-equal :invalid (http-protocol-error-detail failure))
        (ensure-equal 0 exchanges)))
    (let ((failure
            (handler-case
                (http-kit/http2:send-http2-request
                 client request :request-body-length 1
                 :request-body-function (lambda (maximum-size)
                                          (declare (ignore maximum-size))
                                          (incf produced)
                                          (octets 65)))
              (http-unsupported-feature (condition) condition))))
      (ensure-true (typep failure 'http-unsupported-feature))
      (ensure-equal :http2-write (http-error-operation failure))
      (ensure-equal :http2-streaming-request-body
                    (http-kit:http-unsupported-feature-name failure))
      (ensure-equal 0 exchanges)
      (ensure-equal 0 produced))
    (let* ((chunks nil)
           (response (http-kit/http2:send-http2-request
                      client request
                      :on-body-chunk (lambda (chunk) (push chunk chunks)))))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal (octets 0 255) (http-response-body response))
      (ensure-equal (list (octets 0 255)) (nreverse chunks))
      (ensure-equal 1 exchanges)))
  (let* ((invalid-stream (list :not-a-stream))
         (opened 0)
         (closes 0)
         (client
           (http-kit/http2:make-http2-client
            :open-stream (lambda (request &key timeout deadline)
                           (declare (ignore request timeout deadline))
                           (incf opened)
                           invalid-stream)
            :close-stream (lambda (actual)
                            (ensure-true (eq actual invalid-stream))
                            (incf closes))))
         (failure
           (handler-case
               (http-kit/http2:send-http2-request
                client (make-http-request :method "GET" :uri "https://example.test/"))
             (http-connection-error (condition) condition))))
    (ensure-true (typep failure 'http-connection-error))
    (ensure-equal :connect (http-error-operation failure))
    (ensure-equal (type-of invalid-stream) (http-connection-error-cause failure))
    (ensure-equal 1 opened)
    (ensure-equal 1 closes)))

#+sbcl
(deftest http2-public-single-blocked-upload-keeps-response
  (dolist (producer-p '(nil t))
    (let* ((body (make-array 65536 :element-type '(unsigned-byte 8)))
           (early-response
             (concatenate-octets
              (h2-frame 1 4 1 (h2-header-block (cons ":status" "200")
                                              (cons "content-length" "2")))
              (h2-frame 0 0 1 (octets 0))
              (h2-frame 0 1 1 (octets 255))))
           (input (concatenate-octets
                   (h2-frame 4 0 0 (octets)) early-response
                   (h2-frame 8 0 0 (octets 0 0 0 1))
                   (h2-frame 8 0 1 (octets 0 0 0 1))))
           (stream (make-instance 'binary-session-stream :input input))
           (closes 0)
           (chunks nil)
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream
              :close-stream (lambda (actual)
                              (ensure-true (eq stream actual))
                              (incf closes)
                              (close actual)))))
      (dotimes (index (length body))
        (setf (aref body index) (mod index 256)))
      (unwind-protect
           (multiple-value-bind (producer position calls)
               (make-octet-body-producer body)
             (let ((response
                     (http-kit/http2:send-http2-request-over-connection
                      connection
                      (make-http-request :method "POST" :uri "https://example.test/upload"
                                         :body (unless producer-p body))
                      :request-body-function (when producer-p producer)
                      :request-body-length (when producer-p (length body))
                      :on-body-chunk
                      (lambda (chunk)
                        (ensure-true (< (binary-session-input-position stream)
                                        (length input)))
                        (multiple-value-bind (sent count ack) (h2-output-data-summary stream)
                          (declare (ignore count ack))
                          (ensure-true (< (length sent) (length body)))
                          (ensure-true (equalp sent (subseq body 0 (length sent)))))
                        (push chunk chunks)))))
               (ensure-equal 200 (http-response-status response))
               (ensure-equal (octets 0 255) (http-response-body response))
               (ensure-equal (list (octets 0) (octets 255)) (nreverse chunks))
               (ensure-equal (if producer-p 65536 0) (funcall position))
               (ensure-equal (if producer-p 4 0) (funcall calls))
               (ensure-equal (length input) (binary-session-input-position stream))
               (multiple-value-bind (sent count ack) (h2-output-data-summary stream)
                 (ensure-equal 65536 (length sent))
                 (ensure-true (equalp body sent))
                 (ensure-equal 5 count)
                 (ensure-equal 1 ack))
               (let ((data (remove-if-not
                            (lambda (frame) (= 0 (http-kit/http2::%h2-frame-type frame)))
                            (h2-output-frames stream))))
                 (ensure-equal '(16384 16384 16384 16383 1)
                               (mapcar (lambda (frame)
                                         (length (http-kit/http2::%h2-frame-payload frame))) data))
                 (ensure-equal '(0 0 0 0 1) (mapcar #'http-kit/http2::%h2-frame-flags data))
                 (ensure-true (every (lambda (frame)
                                      (= 1 (http-kit/http2::%h2-frame-stream-id frame))) data)))
               (ensure-equal 0 closes)
               (ensure-true (http-kit/http2:http2-connection-open-p connection))))
        (http-kit/http2:close-http2-connection connection))
      (ensure-equal 1 closes)
      (ensure-false (open-stream-p stream)))))

#+sbcl
(deftest http2-public-single-blocked-upload-no-error-reset-keeps-response
  (dolist (producer-p '(nil t))
    (let* ((body (make-array 65536 :element-type '(unsigned-byte 8)))
           (early-response
             (concatenate-octets
              (h2-frame 1 4 1 (h2-header-block (cons ":status" "200")
                                              (cons "content-length" "2")))
              (h2-frame 0 0 1 (octets 0))
              (h2-frame 0 1 1 (octets 255))))
           (reset-input (concatenate-octets
                         (h2-frame 4 0 0 (octets)) early-response
                         (h2-frame 3 0 1 (octets 0 0 0 0))))
           (input (concatenate-octets
                   reset-input (h2-frame 8 0 0 (octets 0 0 0 1))))
           (stream (make-instance 'binary-session-stream :input input))
           (closes 0)
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream
              :close-stream (lambda (actual)
                              (ensure-true (eq stream actual))
                              (incf closes)
                              (close actual)))))
      (dotimes (index (length body))
        (setf (aref body index) (mod index 256)))
      (unwind-protect
           (multiple-value-bind (producer position calls)
               (make-octet-body-producer body)
             (declare (ignore position calls))
             (let ((response
                     (http-kit/http2:send-http2-request-over-connection
                      connection
                      (make-http-request :method "POST" :uri "https://example.test/upload"
                                         :body (unless producer-p body))
                      :request-body-function (when producer-p producer)
                      :request-body-length (when producer-p (length body)))))
               (ensure-equal 200 (http-response-status response))
               (ensure-equal (octets 0 255) (http-response-body response))
               (ensure-true (>= (binary-session-input-position stream)
                                (length reset-input)))
               (multiple-value-bind (sent count ack) (h2-output-data-summary stream)
                 (ensure-equal 65535 (length sent))
                 (ensure-true (equalp sent (subseq body 0 65535)))
                 (ensure-equal 4 count)
                 (ensure-equal 1 ack))
               (let ((data (remove-if-not
                            (lambda (frame) (= 0 (http-kit/http2::%h2-frame-type frame)))
                            (h2-output-frames stream))))
                 (ensure-equal '(16384 16384 16384 16383)
                               (mapcar (lambda (frame)
                                         (length (http-kit/http2::%h2-frame-payload frame))) data))
                 (ensure-equal '(0 0 0 0)
                               (mapcar #'http-kit/http2::%h2-frame-flags data))
                 (ensure-true (every (lambda (frame)
                                      (= 1 (http-kit/http2::%h2-frame-stream-id frame))) data)))
               (ensure-equal 0 closes)
               (ensure-true (http-kit/http2:http2-connection-open-p connection))))
        (http-kit/http2:close-http2-connection connection))
      (ensure-equal 1 closes)
      (ensure-false (open-stream-p stream)))))

#+sbcl
(deftest http2-public-connection-peer-hpack-setting-boundaries
  (dolist (case (list (list 0 (octets #x20 #x88) nil)
                     (list 0 (octets #x21 #x88) '(1 0))
                     (list 8192 (octets #x3f #xe1 #x1f #x88) nil)
                     (list 8192 (octets #x3f #xe2 #x1f #x88) '(4097 4096))))
    (destructuring-bind (setting block detail) case
      (let* ((input (concatenate-octets
                     (h2-frame 4 0 0 (if (zerop setting) (octets 0 1 0 0 0 0)
                                         (octets 0 1 0 0 32 0)))
                     (h2-frame 1 5 1 block)))
             (stream (make-instance 'binary-session-stream :input input))
             (closes 0)
             (seen nil)
             (connection
               (http-kit/http2:make-http2-connection
                :stream stream
                :close-stream (lambda (actual)
                                (ensure-true (eq stream actual))
                                (incf closes)
                                (close actual)))))
        (unwind-protect
             (let ((result
                     (handler-case
                         (handler-bind ((http-error (lambda (condition) (setf seen condition))))
                           (http-kit/http2:send-http2-request-over-connection
                            connection (make-http-request :method "GET"
                                                          :uri "https://example.test/")))
                       (http-error (condition) condition))))
               (if detail
                   (progn
                     (ensure-true (typep result 'http-protocol-error))
                     (ensure-true (eq result seen))
                     (ensure-equal :hpack (http-error-operation result))
                     (ensure-equal detail (http-protocol-error-detail result))
                     (ensure-equal 1 closes)
                     (ensure-false (http-kit/http2:http2-connection-open-p connection)))
                   (progn
                     (ensure-equal 200 (http-response-status result))
                     (ensure-equal (octets) (http-response-body result))
                     (ensure-false seen)
                     (ensure-equal 0 closes)
                     (ensure-true (http-kit/http2:http2-connection-open-p connection))))
               (ensure-equal setting (http-kit/http2:http2-connection-peer-max-table-size connection))
               (ensure-equal (length input) (binary-session-input-position stream))
               (let ((frames (h2-output-frames stream)))
                 (ensure-equal '(4 1 4) (mapcar #'http-kit/http2::%h2-frame-type frames))
                 (ensure-equal '(0 5 1) (mapcar #'http-kit/http2::%h2-frame-flags frames))
                 (ensure-equal '(0 1 0) (mapcar #'http-kit/http2::%h2-frame-stream-id frames))
                 (ensure-equal (octets) (http-kit/http2::%h2-frame-payload (third frames)))))
          (http-kit/http2:close-http2-connection connection))
        (ensure-equal 1 closes)
        (ensure-false (open-stream-p stream))))))

(deftest http2-legacy-headers-adapter-streaming-trailers
  ;; Only legacy arities are called directly; the current reader supplies four arguments.
  (dolist (arity '(1 2 3))
    (dolist (mismatch-p '(nil t))
      (let* ((stream-id (if (= arity 3) 3 1))
             (body (if (= arity 1) (octets 0 255) (octets)))
             (headers (list (make-http-header "content-length" (if mismatch-p "3" "2"))))
             (block (h2-header-block (cons "x-first" "one") (cons "x-last" "two")))
             (reader (h2-reader-from-frames
                      (h2-frame 9 4 stream-id (subseq block 1))))
             (frame (h2-frame-object 1 1 stream-id (subseq block 0 1)))
             (arguments (case arity (1 (list "GET"))
                              (2 (list 2 "GET")) (3 (list 2 "GET" 3))))
             (seen nil)
             (result
               (handler-case
                   (handler-bind ((http-error (lambda (condition) (setf seen condition))))
                     (multiple-value-bind (status actual-headers response)
                         (apply #'http-kit/http2::%h2-process-headers-frame
                                frame reader 16384 nil nil 65536
                                (h2-hpack-context) 200 headers body arguments)
                       (ensure-equal 200 status)
                       (ensure-true (eq headers actual-headers))
                       response))
                 (http-invalid-header (condition) condition))))
        (if mismatch-p
            (progn
              (ensure-true (typep result 'http-invalid-header))
              (ensure-true (eq seen result))
              (ensure-equal :http2-response (http-error-operation result))
              (ensure-equal "content-length" (http-invalid-header-name result))
              (ensure-equal :mismatch (http-invalid-header-reason result)))
            (progn
              (ensure-false seen)
              (ensure-equal 200 (http-response-status result))
              (ensure-equal body (http-response-body result))
              (ensure-equal '(("x-first" . "one") ("x-last" . "two"))
                            (mapcar (lambda (header)
                                      (cons (http-header-name header) (http-header-content header)))
                                    (http-response-trailers result)))))
        (ensure-equal body (if (= arity 1) (octets 0 255) (octets)))
        (ensure-equal :eof (http-kit/http2::%h2-read-frame reader 16384 nil nil))))))

(deftest http2-legacy-data-adapter-padding-and-limits
  (dolist (streaming-p '(nil t))
    (dolist (limit '(3 2))
      (let* ((body (make-array 1 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer (if streaming-p 0 1)
                               :initial-element 7))
             (chunks nil)
             (seen nil)
             (frame (h2-frame-object 0 9 1 (octets 2 0 255 0 0)))
             (arguments (if streaming-p
                            (list 1 "GET" limit (lambda (chunk) (push chunk chunks)) nil)
                            (list "GET" limit)))
             (result
               (handler-case
                   (handler-bind ((http-error (lambda (condition) (setf seen condition))))
                     (multiple-value-list
                      (apply #'http-kit/http2::%h2-append-data-frame
                             frame 200 body arguments)))
                 (http-size-limit-exceeded (condition) condition))))
        (if (= limit 3)
            (progn
              (ensure-false seen)
              (ensure-equal '(t 3 5) result)
              (ensure-equal (if streaming-p (octets) (octets 7 0 255)) body)
              (ensure-equal (if streaming-p (list (octets 0 255)) nil) chunks)
              (when streaming-p
                (ensure-equal '(t 3 0)
                              (multiple-value-list
                               (http-kit/http2::%h2-append-data-frame
                                (h2-frame-object 0 1 1 (octets)) 200 body 3 "GET" 3
                                (lambda (chunk) (push chunk chunks)) nil)))
                (ensure-equal 1 (length chunks))))
            (progn
              (ensure-true (typep result 'http-size-limit-exceeded))
              (ensure-true (eq seen result))
              (ensure-equal :response-parse (http-error-operation result))
              (ensure-equal :body (http-size-limit-exceeded-kind result))
              (ensure-equal 2 (http-size-limit-exceeded-limit result))
              (ensure-equal 3 (http-size-limit-exceeded-observed result))
              (ensure-equal (if streaming-p (octets) (octets 7)) body)
              (ensure-false chunks)))))))
(deftest http2-connection-request-limits-cannot-weaken-connection-caps
  (dolist (batch-p (list nil t))
    (dolist (kind (list :headers :body))
      (let* ((header-limit 256)
             (body-limit 2)
             (header-block
               (h2-header-block
                (cons ":status" "200")
                (cons "x-padding" (make-string 512 :initial-element #\a))))
             (response-headers
               (if (eq kind :headers)
                   header-block
                 (h2-header-block (cons ":status" "200"))))
             (input
               (concatenate-octets
                (h2-frame 4 0 0 (octets))
                (h2-frame 1 4 1 response-headers)
                (if (eq kind :body)
                    (h2-frame 0 1 1 (octets 1 2 3))
                  (octets))))
             (stream (make-instance 'binary-session-stream :input input))
             (closed 0)
             (connection
               (http-kit/http2:make-http2-connection
                :stream stream
                :max-header-bytes header-limit
                :max-body-bytes body-limit
                :close-stream (lambda (actual)
                                (ensure-true (eq stream actual))
                                (incf closed)
                                (close actual))))
             (request
               (make-http-request :method "GET" :uri "https://limits.test/"))
             (result
               (handler-case
                   (if batch-p
                       (first
                        (http-kit/http2:send-http2-requests-over-connection
                         connection (list request)
                         :max-header-bytes 4096
                         :max-body-bytes 4096))
                     (http-kit/http2:send-http2-request-over-connection
                      connection request
                      :max-header-bytes 4096
                      :max-body-bytes 4096))
                 (http-size-limit-exceeded (condition) condition))))
        (ensure-true (typep result 'http-size-limit-exceeded))
        (ensure-equal :response-parse (http-error-operation result))
        (ensure-equal (if (eq kind :headers) :headers :body)
                      (http-size-limit-exceeded-kind result))
        (ensure-equal (if (eq kind :headers) header-limit body-limit)
                      (http-size-limit-exceeded-limit result))
        (ensure-equal 1 closed)
        (ensure-false
         (http-kit/http2:http2-connection-open-p connection))
        (http-kit/http2:close-http2-connection connection)
        (ensure-equal 1 closed)))))
#+sbcl
(deftest http2-scheduler-frame-dispatch-error-fails-all-active-requests
  (let* ((input
           (concatenate-octets
            (h2-frame 4 0 0 (octets))
            (h2-frame 1 4 1 (h2-header-block (cons ":status" "200")))
            (h2-frame 1 4 3 (h2-header-block (cons ":status" "200")))
            (h2-frame 0 1 1 (octets 1 2))))
         (stream (make-instance 'binary-session-stream :input input))
         (closed 0)
         (connection
           (http-kit/http2:make-http2-connection
            :stream stream
            :max-body-bytes 1
            :close-stream (lambda (actual)
                            (ensure-true (eq stream actual))
                            (incf closed)
                            (close actual))))
         (first-opened (sb-thread:make-semaphore :count 0))
         (second-opened (sb-thread:make-semaphore :count 0))
         (release-first (sb-thread:make-semaphore :count 0))
         (first-error nil)
         (second-error nil)
         (first-thread nil)
         (second-thread nil))
    (labels ((send-first ()
               (handler-case
                   (http-kit/http2:send-http2-request-over-connection
                    connection
                    (make-http-request :method "GET" :uri "https://errors.test/first")
                    :on-stream-open
                    (lambda (stream-id cancel)
                      (declare (ignore cancel))
                      (ensure-equal 1 stream-id)
                      (sb-thread:signal-semaphore first-opened)
                      (sb-thread:wait-on-semaphore release-first :timeout 5)))
                 (error (condition)
                   (setf first-error condition))))
             (send-second ()
               (handler-case
                   (http-kit/http2:send-http2-request-over-connection
                    connection
                    (make-http-request :method "GET" :uri "https://errors.test/second")
                    :on-stream-open
                    (lambda (stream-id cancel)
                      (declare (ignore cancel))
                      (ensure-equal 3 stream-id)
                      (sb-thread:signal-semaphore second-opened)))
                 (error (condition)
                   (setf second-error condition)))))
      (unwind-protect
           (progn
             (setf first-thread (sb-thread:make-thread #'send-first))
             (ensure-true
              (sb-thread:wait-on-semaphore first-opened :timeout 5))
             (setf second-thread (sb-thread:make-thread #'send-second))
             (ensure-true
              (sb-thread:wait-on-semaphore second-opened :timeout 5))
             (sb-thread:signal-semaphore release-first)
             (sb-thread:join-thread first-thread)
             (setf first-thread nil)
             (sb-thread:join-thread second-thread)
             (setf second-thread nil)
             (ensure-true (typep first-error 'http-size-limit-exceeded))
             (ensure-true (typep second-error 'http-size-limit-exceeded))
             (ensure-false
              (http-kit/http2:http2-connection-open-p connection))
             (ensure-equal 1 closed))
        (sb-thread:signal-semaphore second-opened)
        (sb-thread:signal-semaphore release-first)
        (when first-thread
          (ignore-errors (sb-thread:join-thread first-thread)))
        (when second-thread
          (ignore-errors (sb-thread:join-thread second-thread)))
        (http-kit/http2:close-http2-connection connection)
        (ensure-equal 1 closed)))))

#+sbcl
(deftest http2-public-padded-streaming-trailers-cleanup
  (dolist (mismatch-p '(nil t))
    (let* ((input (concatenate-octets
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 4 1 (h2-header-block
                                    (cons ":status" "200")
                                    (cons "content-length" (if mismatch-p "3" "2"))))
                   (h2-frame 0 8 1 (octets 2 0 255 0 0))
                   (h2-frame 1 5 1 (h2-header-block (cons "x-end" "done")))))
           (stream (make-instance 'binary-session-stream :input input))
           (closes 0)
           (chunks nil)
           (seen nil)
           (transport
             (http-kit/http2:make-http2-client
              :open-stream (lambda (request &key timeout deadline)
                             (declare (ignore timeout deadline))
                             (ensure-equal "GET" (http-request-method request)) stream)
              :close-stream (lambda (actual)
                              (ensure-true (eq stream actual))
                              (incf closes) (close actual))))
           (result
             (handler-case
                 (handler-bind ((http-error (lambda (condition) (setf seen condition))))
                   (http-kit/http2:send-http2-request
                    transport (make-http-request :method "GET" :uri "https://example.test/")
                    :collect-body-p nil :on-body-chunk (lambda (chunk) (push chunk chunks))))
               (http-invalid-header (condition) condition))))
      (if mismatch-p
          (progn
            (ensure-true (typep result 'http-invalid-header))
            (ensure-true (eq seen result))
            (ensure-equal :http2-response (http-error-operation result))
            (ensure-equal "content-length" (http-invalid-header-name result))
            (ensure-equal :mismatch (http-invalid-header-reason result)))
          (progn
            (ensure-false seen)
            (ensure-equal 200 (http-response-status result))
            (ensure-equal (octets) (http-response-body result))
            (ensure-equal "done" (http-header-value (http-response-trailers result) "x-end"))))
      (ensure-equal (list (octets 0 255)) chunks)
      (ensure-equal (length input) (binary-session-input-position stream))
      (ensure-equal 1 closes)
      (ensure-false (open-stream-p stream))
      (let ((frames (h2-output-frames stream)))
        (ensure-equal '(4 1 4 8 8) (mapcar #'http-kit/http2::%h2-frame-type frames))
        (ensure-equal '(0 5 1 0 0) (mapcar #'http-kit/http2::%h2-frame-flags frames))
        (ensure-equal '(0 1 0 1 0) (mapcar #'http-kit/http2::%h2-frame-stream-id frames))
        (ensure-equal (octets) (http-kit/http2::%h2-frame-payload (third frames)))
        (dolist (frame (last frames 2))
          (ensure-equal (octets 0 0 0 5) (http-kit/http2::%h2-frame-payload frame)))))))
