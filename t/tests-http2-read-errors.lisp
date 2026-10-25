(in-package #:http-kit/test)

(deftest http2-read-response-errors
  (signals http-protocol-error
    (http-kit/http2::%h2-read-response
     (h2-reader-from-frames) nil 16384 nil (lambda () 0d0) 1024 1024 "GET"))
  (signals http-protocol-error
    (http-kit/http2::%h2-read-response
     (h2-reader-from-frames
      (h2-frame http-kit/http2::+http2-data-type+ 0 1 (octets 65)))
     nil 16384 nil (lambda () 0d0) 1024 1024 "GET"))
  (signals http-protocol-error
    (http-kit/http2::%h2-read-response
     (h2-reader-from-frames
     (h2-frame http-kit/http2::+http2-settings-type+
                http-kit/http2::+http2-ack-flag+ 0 (octets)))
     nil 16384 nil (lambda () 0d0) 1024 1024 "GET"))
  (signals http-protocol-error
    (http-kit/http2::%h2-read-response
     (h2-reader-from-frames
      (h2-frame http-kit/http2::+http2-settings-type+ 0 0 (octets)))
     nil 16384 nil (lambda () 0d0) 1024 1024 "GET"))
  (signals http-protocol-error
    (http-kit/http2::%h2-read-response
     (h2-reader-from-frames
      (h2-frame http-kit/http2::+http2-settings-type+ 0 0 (octets))
      (h2-frame http-kit/http2::+http2-push-promise-type+ 0 1 (octets)))
     nil 16384 nil (lambda () 0d0) 1024 1024 "GET"))
  (signals http-protocol-error
    (http-kit/http2::%h2-read-response
     (h2-reader-from-frames
      (h2-frame http-kit/http2::+http2-settings-type+ 0 0 (octets))
      (h2-frame http-kit/http2::+http2-continuation-type+ 0 1 (octets)))
     nil 16384 nil (lambda () 0d0) 1024 1024 "GET"))
  (let ((unknown-response
          (concatenate-octets
           (h2-frame http-kit/http2::+http2-settings-type+ 0 0 (octets))
           (h2-frame #xfe 0 0 (octets))
           (h2-frame http-kit/http2::+http2-headers-type+
                     (logior http-kit/http2::+http2-end-headers-flag+
                            http-kit/http2::+http2-end-stream-flag+)
                     1
                     (h2-header-block (cons ":status" "200"))))))
    (ensure-equal 200
                  (http-response-status
                   (http-kit/http2::%h2-read-response
                    (h2-reader-from-frames unknown-response)
                    nil 16384 nil (lambda () 0d0) 1024 1024 "GET")))))
  (let ((writes '())
        (response-wire
          (concatenate-octets
           (h2-frame http-kit/http2::+http2-settings-type+ 0 0 (octets))
           (h2-frame http-kit/http2::+http2-headers-type+
                     (logior http-kit/http2::+http2-end-headers-flag+
                            http-kit/http2::+http2-end-stream-flag+)
                     1
                     (h2-header-block (cons ":status" "200"))))))
    (let ((response
            (http-kit/http2::%h2-read-response
             (h2-reader-from-frames response-wire)
             (lambda (wire) (push wire writes))
             16384 nil (lambda () 0d0) 1024 1024 "GET")))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal 1 (length writes))
      (ensure-equal (h2-frame http-kit/http2::+http2-settings-type+
                              http-kit/http2::+http2-ack-flag+
                              0
                              (octets))
                    (first writes))))

(deftest http2-read-response-data-stream
  (let ((response-wire
          (concatenate-octets
           (h2-frame http-kit/http2::+http2-settings-type+ 0 0 (octets))
           (h2-frame http-kit/http2::+http2-headers-type+
                     http-kit/http2::+http2-end-headers-flag+
                     1
                     (h2-header-block (cons ":status" "200")))
           (h2-frame http-kit/http2::+http2-data-type+ 0 1 (octets 65))
           (h2-frame http-kit/http2::+http2-data-type+
                     http-kit/http2::+http2-end-stream-flag+
                     1
                     (octets 66)))))
    (let ((response
            (http-kit/http2::%h2-read-response
             (h2-reader-from-frames response-wire)
             nil 16384 nil (lambda () 0d0) 1024 1024 "GET")))
      (ensure-equal 200 (http-response-status response))
        (ensure-equal (octets 65 66) (http-response-body response)))))

(deftest http2-response-stream-id-errors
  (signals http-protocol-error
    (http-kit/http2::%h2-validate-response-stream-id 0 1))
  (signals http-unsupported-feature
    (http-kit/http2::%h2-validate-response-stream-id 3 1))
  (let ((frame (h2-frame-object http-kit/http2::+http2-headers-type+
                               http-kit/http2::+http2-end-headers-flag+
                               0
                               (h2-header-block (cons ":status" "200")))))
    (signals http-protocol-error
      (http-kit/http2::%h2-process-headers-frame
       frame (h2-reader-from-frames) 16384 nil (lambda () 0d0) 1024
       (h2-hpack-context) nil nil
       (make-array 0 :element-type '(unsigned-byte 8)
                   :adjustable t :fill-pointer 0)
       "GET"))))

(deftest shared-error-translation-boundaries
  (signals http-connection-error
    (http-kit::%with-http-error-translation
        ("synthetic transport failure" :transport)
      (error "plain transport failure")))
  (signals http-protocol-error
    (http-kit::%with-http-error-translation
        ("synthetic transport failure" :transport)
      (error 'http-protocol-error
             :message "already an HTTP error"
             :operation :test))))

#+sbcl
(deftest http2-public-connect-initial-settings-boundaries
  (dolist (case (list (list (octets) :eof)
                     (list (h2-frame 0 0 1 (octets 65)) '(0 1 0))
                     (list (h2-frame 4 0 1 (octets)) '(4 1 0))
                     (list (h2-frame 4 1 0 (octets)) '(4 0 1))
                     (list (concatenate-octets
                            (h2-frame 4 0 0 (octets 0 8 0 0 0 1))
                            (h2-frame 1 5 1 (octets #x88))) nil)))
    (destructuring-bind (input detail) case
      (let* ((stream (make-instance 'binary-session-stream :input input))
             (closes 0)
             (seen nil)
             (connection
               (http-kit/http2:make-http2-connection
                :stream stream
                :close-stream (lambda (actual)
                                (ensure-true (eq actual stream))
                                (incf closes)
                                (close actual)))))
        (unwind-protect
             (let ((result
                     (handler-case
                         (handler-bind ((http-error (lambda (condition)
                                                      (setf seen condition))))
                           (http-kit/http2:send-http2-request-over-connection
                            connection
                            (make-http-request :method "CONNECT" :protocol "websocket"
                                               :uri "https://example.test/chat")))
                       (http-error (condition) condition))))
               (if detail
                   (progn
                     (ensure-true (typep result 'http-protocol-error))
                     (ensure-true (eq seen result))
                     (ensure-equal :http2-read (http-error-operation result))
                     (ensure-equal detail (http-protocol-error-detail result))
                     (ensure-equal 1 closes)
                     (ensure-false (http-kit/http2:http2-connection-open-p connection)))
                   (progn
                     (ensure-equal 200 (http-response-status result))
                     (ensure-equal (octets) (http-response-body result))
                     (ensure-false seen)
                     (ensure-equal 0 closes)
                     (ensure-true (http-kit/http2:http2-connection-open-p connection))))
               (ensure-equal (length input) (binary-session-input-position stream))
               (let ((frames (h2-output-frames stream)))
                 (ensure-equal (if detail '(4) '(4 4 1))
                               (mapcar #'http-kit/http2::%h2-frame-type frames))
                 (ensure-equal (if detail '(0) '(0 1 5))
                               (mapcar #'http-kit/http2::%h2-frame-flags frames))
                 (ensure-equal (if detail '(0) '(0 0 1))
                               (mapcar #'http-kit/http2::%h2-frame-stream-id frames))
                 (unless detail
                   (let ((fields (server-response-fields (third frames))))
                     (ensure-equal "CONNECT" (cdr (assoc ":method" fields :test #'string=)))
                     (ensure-equal "websocket" (cdr (assoc ":protocol" fields :test #'string=)))
                     (ensure-equal "/chat" (cdr (assoc ":path" fields :test #'string=)))))))
          (http-kit/http2:close-http2-connection connection))
        (ensure-equal 1 closes)
        (ensure-false (open-stream-p stream))))))

#+sbcl
(deftest http2-public-single-goaway-boundaries
  (dolist (case (list (list 1 (octets 0 0 0 1 0 0 0 0) '(1 8) nil)
                     (list 0 (octets 0 0 0 1 0 0 0) '(0 7) nil)
                     (list 0 (octets 128 0 0 1 0 0 0 0) #x80000001 nil)
                     (list 0 (octets 0 0 0 0 0 0 0 2) '(:goaway 0 2) t)
                     (list 0 (octets 0 0 0 1 0 0 0 0) nil nil)))
    (destructuring-bind (stream-id payload detail connection-error-p) case
      (let* ((input (concatenate-octets
                     (h2-frame 4 0 0 (octets))
                     (h2-frame 7 0 stream-id payload)
                     (if detail (octets) (h2-frame 1 5 1 (octets #x88)))))
             (stream (make-instance 'binary-session-stream :input input))
             (closes 0)
             (seen nil)
             (connection
               (http-kit/http2:make-http2-connection
                :stream stream
                :close-stream (lambda (actual)
                                (ensure-true (eq actual stream))
                                (incf closes)
                                (close actual)))))
        (unwind-protect
             (let ((result
                     (handler-case
                         (handler-bind ((http-error (lambda (condition)
                                                      (setf seen condition))))
                           (http-kit/http2:send-http2-request-over-connection
                            connection (make-http-request :method "GET"
                                                          :uri "https://example.test/")))
                       (http-error (condition) condition))))
               (if detail
                   (progn
                     (ensure-true (typep result (if connection-error-p
                                                   'http-connection-error
                                                   'http-protocol-error)))
                     (ensure-true (eq result seen))
                     (ensure-equal :http2-control (http-error-operation result))
                     (ensure-equal detail (if connection-error-p
                                              (http-connection-error-cause result)
                                              (http-protocol-error-detail result)))
                     (ensure-equal 1 closes)
                     (ensure-false (http-kit/http2:http2-connection-open-p connection)))
                   (progn
                     (ensure-equal 200 (http-response-status result))
                     (ensure-equal 1 (http-kit/http2:http2-connection-goaway-last-stream-id connection))
                     (ensure-false seen)
                     (ensure-equal 0 closes)
                     (ensure-true (http-kit/http2:http2-connection-open-p connection))))
               (ensure-equal (length input) (binary-session-input-position stream))
               (let ((frames (h2-output-frames stream)))
                 (ensure-equal '(4 1 4) (mapcar #'http-kit/http2::%h2-frame-type frames))
                 (ensure-equal '(0 5 1) (mapcar #'http-kit/http2::%h2-frame-flags frames))
                 (ensure-equal '(0 1 0) (mapcar #'http-kit/http2::%h2-frame-stream-id frames))
                 (ensure-equal (octets) (http-kit/http2::%h2-frame-payload (third frames)))))
          (http-kit/http2:close-http2-connection connection))
        (ensure-equal 1 closes)
        (ensure-false (open-stream-p stream))))))


#+sbcl
(deftest http2-public-single-blocked-upload-errors
  (dolist (case (list (list :input (h2-frame 1 5 1 (octets #x88)) :detail '(1 1 5) :eof-p nil :expected-acks 0 :expected-callbacks 0)
                     (list :input (h2-frame 4 1 0 (octets)) :detail '(4 0 1) :eof-p nil :expected-acks 0 :expected-callbacks 0)
                     (list :input (concatenate-octets
                            (h2-frame 4 0 0 (octets))
                            (h2-frame 1 4 1 (octets #x88))
                            (h2-frame 0 1 1 (octets 9)))
                           :detail :eof :eof-p t :expected-acks 1 :expected-callbacks 1)))
    (destructuring-bind (&key input detail eof-p expected-acks expected-callbacks) case
      (let* ((body (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 7))
             (stream (make-instance 'binary-session-stream :input input))
             (closes 0)
             (callbacks 0)
             (seen nil)
             (connection
               (http-kit/http2:make-http2-connection
                :stream stream
                :close-stream (lambda (actual)
                                (ensure-true (eq stream actual))
                                (incf closes)
                                (close actual)))))
        (unwind-protect
             (let ((failure
                     (handler-case
                         (handler-bind ((http-error (lambda (condition) (setf seen condition))))
                           (http-kit/http2:send-http2-request-over-connection
                            connection (make-http-request :method "POST"
                                                          :uri "https://example.test/upload"
                                                          :body body)
                            :on-body-chunk (lambda (chunk)
                                             (declare (ignore chunk))
                                             (incf callbacks))))
                       (http-error (condition) condition))))
               (ensure-true (typep failure (if eof-p 'http-connection-error 'http-protocol-error)))
               (ensure-true (eq failure seen))
               (ensure-equal :http2-read (http-error-operation failure))
               (ensure-equal detail (if eof-p (http-connection-error-cause failure)
                                       (http-protocol-error-detail failure)))
               (ensure-equal expected-callbacks callbacks)
               (ensure-equal (length input) (binary-session-input-position stream))
               (multiple-value-bind (sent count ack) (h2-output-data-summary stream)
                 (ensure-equal 65535 (length sent))
                 (ensure-true (equalp (subseq body 0 65535) sent))
                 (ensure-equal 4 count)
                 (ensure-equal expected-acks ack))
               (let ((data (remove-if-not
                            (lambda (frame) (= 0 (http-kit/http2::%h2-frame-type frame)))
                            (h2-output-frames stream))))
                 (ensure-equal '(16384 16384 16384 16383)
                               (mapcar (lambda (frame)
                                         (length (http-kit/http2::%h2-frame-payload frame))) data))
                 (ensure-true (every (lambda (frame)
                                      (and (= 1 (http-kit/http2::%h2-frame-stream-id frame))
                                           (= 0 (http-kit/http2::%h2-frame-flags frame)))) data)))
               (ensure-equal 1 closes)
               (ensure-false (http-kit/http2:http2-connection-open-p connection)))
          (http-kit/http2:close-http2-connection connection))
        (ensure-equal 1 closes)
        (ensure-false (open-stream-p stream))))))

#+sbcl
(deftest http2-public-single-callback-condition-cleanup
  (dolist (phase '(nil :open :producer :body))
    (let* ((settings (h2-frame 4 0 0 (octets)))
           (headers (h2-frame 1 4 1 (h2-header-block (cons ":status" "200")
                                                   (cons "content-length" "2"))))
           (first-data (h2-frame 0 0 1 (octets 0)))
           (input (concatenate-octets settings headers first-data (h2-frame 0 1 1 (octets 255))))
           (stream (make-instance 'binary-session-stream :input input))
           (original (make-condition 'http-protocol-error :message "Public callback failed."
                                     :operation :test-callback :detail phase))
           (seen nil)
           (events nil)
           (chunks nil)
           (closes 0)
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
                          connection (make-http-request :method "POST"
                                                        :uri "https://example.test/upload")
                          :request-body-length 1
                          :request-body-function
                          (lambda (maximum-size)
                            (ensure-equal 16384 maximum-size)
                            (push :producer events)
                            (when (eq phase :producer) (error original))
                            (octets 7))
                          :on-stream-open
                          (lambda (stream-id cancel)
                            (ensure-equal 1 stream-id)
                            (ensure-true (functionp cancel))
                            (push :open events)
                            (when (eq phase :open) (error original)))
                          :on-body-chunk
                          (lambda (chunk)
                            (push :body events)
                            (push chunk chunks)
                            (when (eq phase :body) (error original)))))
                     (http-error (condition) condition))))
             (if phase
                 (progn
                   (ensure-true (eq original result))
                   (ensure-true (eq original seen))
                   (ensure-equal :test-callback (http-error-operation result))
                   (ensure-equal phase (http-protocol-error-detail result))
                   (ensure-equal (if (eq phase :body) 0 1) closes)
                   (ensure-equal (eq phase :body)
                                 (http-kit/http2:http2-connection-open-p connection)))
                 (progn
                   (ensure-equal 200 (http-response-status result))
                   (ensure-equal (octets 0 255) (http-response-body result))
                   (ensure-false seen)
                   (ensure-equal 0 closes)
                   (ensure-true (http-kit/http2:http2-connection-open-p connection))))
             (ensure-equal (case phase (:open '(:open)) (:producer '(:open :producer))
                                (:body '(:open :producer :body))
                                (t '(:open :producer :body :body)))
                           (nreverse events))
             (ensure-equal (case phase ((:open :producer) 0)
                                (:body (+ (length settings) (length headers) (length first-data)))
                                (t (length input)))
                           (binary-session-input-position stream))
             (ensure-equal (case phase ((:open :producer) nil)
                                (:body (list (octets 0)))
                                (t (list (octets 0) (octets 255))))
                           (nreverse chunks))
             ;; Earlier DATA gets stream and connection credit; END_STREAM gets connection credit only.
             (let ((frames (h2-output-frames stream)))
               (ensure-equal
                (case phase
                  ((:open :producer)
                   (list http-kit/http2::+http2-settings-type+
                         http-kit/http2::+http2-headers-type+))
                  (:body
                   (list http-kit/http2::+http2-settings-type+
                         http-kit/http2::+http2-headers-type+
                         http-kit/http2::+http2-data-type+
                         http-kit/http2::+http2-settings-type+
                         http-kit/http2::+http2-window-update-type+
                         http-kit/http2::+http2-rst-stream-type+))
                  ((nil)
                   (list http-kit/http2::+http2-settings-type+
                         http-kit/http2::+http2-headers-type+
                         http-kit/http2::+http2-data-type+
                         http-kit/http2::+http2-settings-type+
                         http-kit/http2::+http2-window-update-type+
                         http-kit/http2::+http2-window-update-type+
                         http-kit/http2::+http2-window-update-type+)))
                (mapcar #'http-kit/http2::%h2-frame-type frames))
               (ensure-equal '(0 4) (mapcar #'http-kit/http2::%h2-frame-flags (subseq frames 0 2)))
               (when (null phase)
                 (ensure-equal '(1 0 0)
                               (mapcar #'http-kit/http2::%h2-frame-stream-id
                                       (nthcdr 4 frames))))
               (when (eq phase :body)
                 (ensure-equal '(0 1)
                               (mapcar #'http-kit/http2::%h2-frame-stream-id
                                       (nthcdr 4 frames))))
               (multiple-value-bind (sent count ack) (h2-output-data-summary stream)
                 (ensure-equal (if (member phase '(:open :producer)) (octets) (octets 7)) sent)
                 (ensure-equal (if (member phase '(:open :producer)) 0 1) count)
                 (ensure-equal (if (member phase '(:open :producer)) 0 1) ack))
               (when (or (null phase) (eq phase :body))
                 (ensure-equal 1 (http-kit/http2::%h2-frame-flags (third frames)))
                 (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id (third frames))))))
        (http-kit/http2:close-http2-connection connection))
      (ensure-equal 1 closes)
      (ensure-false (open-stream-p stream)))))
