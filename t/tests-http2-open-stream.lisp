(in-package #:http-kit/test)

#+sbcl
(progn
  (defclass binary-session-stream
      (sb-gray:fundamental-binary-input-stream
       sb-gray:fundamental-binary-output-stream)
    ((input
       :initarg :input
       :reader binary-session-input)
     (input-position
       :initform 0
       :accessor binary-session-input-position)
     (output
       :initform (make-array 0
                             :element-type '(unsigned-byte 8)
                             :adjustable t
                             :fill-pointer 0)
       :reader binary-session-output)))

  (defmethod sb-gray:stream-read-sequence
      ((stream binary-session-stream) sequence &optional (start 0) end)
    (let* ((end (or end (length sequence)))
           (input (binary-session-input stream))
           (position (binary-session-input-position stream))
           (count (min (- end start) (- (length input) position))))
      (when (plusp count)
        (replace sequence input
                 :start1 start
                 :end1 (+ start count)
                 :start2 position
                 :end2 (+ position count))
        (incf (binary-session-input-position stream) count))
      (+ start count)))

  (defmethod sb-gray:stream-write-sequence
      ((stream binary-session-stream) sequence &optional (start 0) end)
    (let ((end (or end (length sequence)))
          (output (binary-session-output stream)))
      (loop for index from start below end
            do (vector-push-extend (aref sequence index) output)))
    sequence)

  (defclass scheduler-write-failure-stream (binary-session-stream)
    ((write-error
       :initarg :write-error
       :reader scheduler-write-failure-stream-error)))

  (defmethod sb-gray:stream-write-sequence
      ((stream scheduler-write-failure-stream) sequence &optional start end)
    (declare (ignore sequence start end))
    (error (scheduler-write-failure-stream-error stream)))

  (defmethod sb-gray:stream-finish-output ((stream binary-session-stream))
    (declare (ignore stream))
    nil)

  (defun h2-output-frames (stream)
    (let ((reader
            (http-kit/http2::%h2-reader-for
             (subseq (binary-session-output stream)
                     (length (h2-preface)))))
          (frames nil))
      (loop for frame = (http-kit/http2::%h2-read-frame
                         reader 16384 nil nil)
            until (eq frame :eof)
            do (push frame frames))
      (nreverse frames)))

  (defun h2-output-data-summary (stream)
    (let ((data-body
            (make-array 0
                        :element-type '(unsigned-byte 8)
                        :adjustable t
                        :fill-pointer 0))
          (data-frame-count 0)
          (settings-ack-count 0))
      (dolist (frame (h2-output-frames stream))
        (cond
          ((= 0 (http-kit/http2::%h2-frame-type frame))
           (incf data-frame-count)
           (loop for octet across (http-kit/http2::%h2-frame-payload frame)
                 do (vector-push-extend octet data-body)))
          ((and (= 4 (http-kit/http2::%h2-frame-type frame))
                (= 1 (http-kit/http2::%h2-frame-flags frame)))
           (incf settings-ack-count))))
      (values data-body data-frame-count settings-ack-count)))

  (defun make-octet-body-producer (body &key on-maximum-size)
    (let ((position 0)
          (calls 0))
      (values
       (lambda (maximum-size)
         (incf calls)
         (when on-maximum-size
           (funcall on-maximum-size maximum-size))
         (when (< position (length body))
           (let* ((size (min maximum-size
                             (- (length body) position)))
                  (chunk (make-array size
                                     :element-type '(unsigned-byte 8))))
             (replace chunk body
                      :start2 position
                      :end2 (+ position size))
             (incf position size)
             chunk)))
       (lambda () position)
       (lambda () calls))))

  (deftest http2-public-open-stream-transport
    (let* ((expected-preface (h2-preface))
           (stream (make-instance 'binary-session-stream
                                  :input (h2-response-wire
                                          (octets 0 #xff 7))))
           (closed nil)
           (transport
             (http-kit/http2:make-http2-client
              :open-stream (lambda (request &key timeout deadline)
                             (declare (ignore timeout deadline))
                             (ensure-equal "GET"
                                           (http-request-method request))
                             stream)
              :close-stream (lambda (closed-stream)
                              (ensure-equal stream closed-stream)
                              (setf closed t))))
           (response (http-kit/http2:send-http2-request
                      transport
                      (make-http-request :method "GET"
                                         :uri "https://127.0.0.1/data")))
           (output (binary-session-output stream)))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal (octets 0 #xff 7) (http-response-body response))
      (ensure-true closed)
      (ensure-equal expected-preface
                    (subseq output 0 (length expected-preface)))
      (ensure-equal (h2-frame 8 0 0 (octets 0 0 0 3))
                    (subseq output (- (length output) 13)))))

  (deftest http2-public-open-stream-requires-stream
    (let ((transport
            (http-kit/http2:make-http2-client
             :open-stream (lambda (request &key timeout deadline)
                            (declare (ignore request timeout deadline))
                            nil))))
      (signals http-connection-error
        (http-kit/http2:send-http2-request
         transport
         (make-http-request :method "GET"
                            :uri "https://127.0.0.1/data"))))))

  (deftest http2-public-connection-reuses-session
    (let* ((expected-preface (h2-preface))
           (stream (make-instance 'binary-session-stream
                                  :input
                                  (concatenate-octets
                                   (h2-response-wire (octets 1 2))
                                   (h2-frame 1 4 3 (octets #x88))
                                   (h2-frame 0 9 3 (octets 2 3 4 0 0)))))
           (closed nil)
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream
              :close-stream (lambda (closed-stream)
                              (ensure-equal stream closed-stream)
                              (setf closed t))))
           (transport (http-kit/http2:make-http2-client
                       :connection connection))
           (first-response
             (http-kit/http2:send-http2-request
              transport
              (make-http-request :method "GET"
                                 :uri "https://127.0.0.1/first")))
           (second-response
             (http-kit/http2:send-http2-request
              transport
              (make-http-request :method "GET"
                                 :uri "https://127.0.0.1/second")))
           (output (binary-session-output stream))
           (request-frames
             (http-kit/http2::%h2-reader-for
              (subseq output (length expected-preface))))
           (settings-frame
             (http-kit/http2::%h2-read-frame
              request-frames 16384 nil nil))
           (first-request-frame
             (http-kit/http2::%h2-read-frame
              request-frames 16384 nil nil))
           (first-settings-ack
             (http-kit/http2::%h2-read-frame
              request-frames 16384 nil nil))
           (first-connection-window-update
             (http-kit/http2::%h2-read-frame
              request-frames 16384 nil nil))
           (second-request-frame
             (http-kit/http2::%h2-read-frame
              request-frames 16384 nil nil))
           (second-connection-window-update
             (http-kit/http2::%h2-read-frame
              request-frames 16384 nil nil)))
      (ensure-equal 200 (http-response-status first-response))
      (ensure-equal (octets 1 2) (http-response-body first-response))
      (ensure-equal 200 (http-response-status second-response))
      (ensure-equal (octets 3 4) (http-response-body second-response))
      (ensure-true (http-kit/http2:http2-connection-open-p connection))
      (ensure-true
       (http-kit/http2:http2-connection-session-started-p connection))
      (ensure-equal expected-preface
                    (subseq output 0 (length expected-preface)))
      (ensure-equal 4 (http-kit/http2::%h2-frame-type settings-frame))
      (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id first-request-frame))
      (ensure-equal 4 (http-kit/http2::%h2-frame-type first-settings-ack))
      (ensure-equal 1 (http-kit/http2::%h2-frame-flags first-settings-ack))
      (ensure-equal 0
                    (http-kit/http2::%h2-frame-stream-id first-settings-ack))
      (ensure-equal 8
                    (http-kit/http2::%h2-frame-type
                     first-connection-window-update))
      (ensure-equal 0
                    (http-kit/http2::%h2-frame-stream-id
                     first-connection-window-update))
      (ensure-equal 3 (http-kit/http2::%h2-frame-stream-id second-request-frame))
      (ensure-equal 8
                    (http-kit/http2::%h2-frame-type
                     second-connection-window-update))
      (ensure-equal 0
                    (http-kit/http2::%h2-frame-stream-id
                     second-connection-window-update))
      (ensure-equal (octets 0 0 0 5)
                    (http-kit/http2::%h2-frame-payload
                     second-connection-window-update))
      (progn
        (ensure-equal :eof
                      (http-kit/http2::%h2-read-frame
                       request-frames 16384 nil nil))
        (http-kit/http2:close-http2-connection connection))
      (ensure-true closed)
      (ensure-true
       (not (http-kit/http2:http2-connection-open-p connection)))))

  (deftest http2-public-connection-survives-peer-stream-reset
    (let* ((stream
             (make-instance
              'binary-session-stream
              :input
              (concatenate-octets
               (h2-frame 4 0 0 (octets))
               (h2-frame 3 0 1 (octets 0 0 0 8))
               (h2-frame 1 5 3 (octets #x88)))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream)))
      (signals http-connection-error
        (http-kit/http2:send-http2-request-over-connection
         connection
         (make-http-request :method "GET"
                            :uri "https://127.0.0.1/reset")))
      (ensure-true (http-kit/http2:http2-connection-open-p connection))
      (let ((response
              (http-kit/http2:send-http2-request-over-connection
               connection
               (make-http-request :method "GET"
                                  :uri "https://127.0.0.1/next"))))
        (ensure-equal 200 (http-response-status response))
        (ensure-equal (octets) (http-response-body response)))
      (http-kit/http2:close-http2-connection connection)))
  (deftest http2-public-connection-does-not-send-data-after-peer-reset-during-upload
    (let* ((producer-started (sb-thread:make-semaphore :count 0))
           (release-producer (sb-thread:make-semaphore :count 0))
           (upload-done (sb-thread:make-semaphore :count 0))
           (stream
             (make-instance
              'binary-session-stream
              :input
              (concatenate-octets
               (h2-frame 4 0 0 (octets))
               (h2-frame 3 0 1 (octets 0 0 0 8))
               (h2-frame 1 5 3 (octets #x88)))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream))
           (upload-thread nil)
           (upload-error nil)
           (peer-response nil)
           (producer-started-p nil)
           (upload-done-p nil))
      (unwind-protect
           (progn
             (setf upload-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (handler-case
                               (http-kit/http2:send-http2-request-over-connection
                                connection
                                (make-http-request
                                 :method "POST"
                                 :uri "https://127.0.0.1/upload")
                                :timeout 5
                                :request-body-function
                                (lambda (maximum-size)
                                  (declare (ignore maximum-size))
                                  (sb-thread:signal-semaphore producer-started)
                                  (unless
                                      (sb-thread:wait-on-semaphore
                                       release-producer :timeout 5)
                                    (error "Timed out waiting to release the upload producer."))
                                  (octets 65))
                                :request-body-length 1)
                             (error (condition)
                               (setf upload-error condition)))
                        (sb-thread:signal-semaphore upload-done)))))
             (setf producer-started-p
                   (sb-thread:wait-on-semaphore producer-started :timeout 5))
             (ensure-true producer-started-p)
             (setf peer-response
                   (http-kit/http2:send-http2-request-over-connection
                    connection
                    (make-http-request :method "GET"
                                       :uri "https://127.0.0.1/next")
                    :timeout 5))
             (ensure-equal 200 (http-response-status peer-response))
             (ensure-equal (octets) (http-response-body peer-response))
             (sb-thread:signal-semaphore release-producer)
             (setf upload-done-p
                   (sb-thread:wait-on-semaphore upload-done :timeout 5))
             (ensure-true upload-done-p)
             (sb-thread:join-thread upload-thread)
             (setf upload-thread nil)
             (ensure-true (typep upload-error 'http-connection-error))
             (ensure-equal '(:rst-stream 1 8)
                           (http-connection-error-cause upload-error))
             (ensure-true (http-kit/http2:http2-connection-open-p connection))
             (ensure-equal
              0
              (count-if
               (lambda (frame)
                 (and (= 0 (http-kit/http2::%h2-frame-type frame))
                      (= 1 (http-kit/http2::%h2-frame-stream-id frame))))
               (h2-output-frames stream))))
        (sb-thread:signal-semaphore release-producer)
        (when upload-thread
          (sb-thread:join-thread upload-thread))
        (http-kit/http2:close-http2-connection connection))))
  (deftest http2-response-headers-cancel-completed-stream-does-not-send-rst
    (let* ((stream
             (make-instance
              'binary-session-stream
              :input
              (concatenate-octets
               (h2-frame 4 0 0 (octets))
               (h2-frame 1 5 1
                         (h2-header-block (cons ":status" "200"))))))
           (connection (http-kit/http2:make-http2-connection :stream stream))
           (client
             (make-http-client
              :cache nil
              :transport-function
              (http-kit/http2:make-http2-connection-transport connection)))
           (token (make-http-cancellation-token))
           (response
             (http-client-send
              client "GET" "https://closed.test/bodyless"
              :cancel-token token
              :on-response-headers
              (lambda (status headers)
                (declare (ignore headers))
                (ensure-equal 200 status)
                (ensure-true (http-client-cancel-request token))))))
      (unwind-protect
           (progn
             (ensure-equal 200 (http-response-status response))
             (ensure-equal (octets) (http-response-body response))
             (ensure-false
              (find-if
               (lambda (frame)
                 (and (= 3 (http-kit/http2::%h2-frame-type frame))
                      (= 1 (http-kit/http2::%h2-frame-stream-id frame))))
               (h2-output-frames stream)))
             (ensure-false (http-client-cancel-request token)))
        (http-kit/http2:close-http2-connection connection))))
  (deftest http2-client-cancel-after-response-complete-stops-upload
    (let* ((stream
             (make-instance
              'binary-session-stream
              :input
              (concatenate-octets
               (h2-frame 4 0 0 (octets))
               (h2-frame 1 4 1
                         (h2-header-block (cons ":status" "200")))
               (h2-frame 0 1 1 (octets 82)))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream))
           (client
             (make-http-client
              :cache nil
              :transport-function
              (http-kit/http2:make-http2-connection-transport connection)))
           (token (make-http-cancellation-token))
           (producer-calls 0)
           (response nil))
      (http-kit/http2::%h2-with-scheduler-lock (connection)
        (setf (http-kit/http2::%http2-connection-peer-initial-window-size
               connection)
              1))
      (unwind-protect
           (progn
             (setf response
                   (http-client-send
                    client
                    (http-client-request
                     client "POST" "https://closed.test/upload")
                    :request-body-function
                    (lambda (maximum-size)
                      (declare (ignore maximum-size))
                      (incf producer-calls)
                      (when (= producer-calls 1)
                        (octets 65 66)))
                    :request-body-length 2
                    :cancel-token token
                    :on-body-chunk
                    (lambda (chunk)
                      (ensure-equal (octets 82) chunk)
                      (ensure-true (http-client-cancel-request token)))))
             (ensure-equal 200 (http-response-status response))
             (ensure-equal (octets 82) (http-response-body response))
             (ensure-equal 1 producer-calls)
             (ensure-false (http-client-cancel-request token))
             (let* ((frames (h2-output-frames stream))
                    (upload-data
                      (remove-if-not
                       (lambda (frame)
                         (and (= 0 (http-kit/http2::%h2-frame-type frame))
                              (= 1 (http-kit/http2::%h2-frame-stream-id frame))))
                       frames))
                    (reset-frame
                      (find-if
                       (lambda (frame)
                         (and (= 3 (http-kit/http2::%h2-frame-type frame))
                              (= 1 (http-kit/http2::%h2-frame-stream-id frame))))
                       frames)))
               (ensure-equal 1 (length upload-data))
               (ensure-equal (octets 65)
                             (http-kit/http2::%h2-frame-payload
                              (first upload-data)))
               (ensure-true reset-frame)
               (ensure-equal (octets 0 0 0 8)
                             (http-kit/http2::%h2-frame-payload reset-frame))
               (ensure-true
                (< (position (first upload-data) frames :test #'eq)
                   (position reset-frame frames :test #'eq)))))
        (http-kit/http2:close-http2-connection connection))))

  (deftest http2-public-priority-update
    (let* ((stream
             (make-instance
              'binary-session-stream
              :input (h2-response-wire (octets))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream)))
      (http-kit/http2:send-http2-request-over-connection
       connection
       (make-http-request :method "GET"
                          :uri "https://127.0.0.1/priority"))
      (http-kit/http2:send-http2-priority-update
       connection 1 :urgency 0 :incremental t)
      (let* ((output (binary-session-output stream))
             (reader (http-kit/http2::%h2-reader-for
                      (subseq output (length (h2-preface)))))
             (priority nil))
        (loop for frame = (http-kit/http2::%h2-read-frame
                           reader 16384 nil nil)
              until (eq frame :eof)
              when (= http-kit/http2::+http2-priority-update-type+
                      (http-kit/http2::%h2-frame-type frame))
                do (setf priority frame))
        (ensure-true priority)
        (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id priority))
        (ensure-equal
         (concatenate '(vector (unsigned-byte 8))
                      (octets 0 0 0 1)
                      (http-kit::%string-octets "u=0, i"))
         (http-kit/http2::%h2-frame-payload priority)))
      (signals http-protocol-error
        (http-kit/http2:send-http2-priority-update connection 0))
      (signals http-protocol-error
        (http-kit/http2:send-http2-priority-update
         connection 1 :priority-field-value (string #\Newline)))
      (http-kit/http2:close-http2-connection connection)))

  (progn
  (deftest http2-public-connection-cancel-callback-default-code
    (let* ((stream (make-instance 'binary-session-stream
                                  :input (h2-response-wire (octets))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream))
           (opened-stream-id nil)
           (failure nil))
      (unwind-protect
           (progn
             (handler-case
                 (http-kit/http2:send-http2-request-over-connection
                  connection
                  (make-http-request :method "GET"
                                     :uri "https://127.0.0.1/cancel")
                  :on-stream-open
                  (lambda (stream-id cancel-function)
                    (setf opened-stream-id stream-id)
                    (funcall cancel-function)))
               (error (condition)
                 (setf failure condition)))
             (ensure-true (typep failure 'http-connection-error))
             (ensure-equal '(:local-cancel 1 8)
                           (http-connection-error-cause failure))
             (ensure-equal 1 opened-stream-id)
             (ensure-true (http-kit/http2:http2-connection-open-p connection))
             (let ((reader
                     (http-kit/http2::%h2-reader-for
                      (subseq (binary-session-output stream)
                              (length (h2-preface)))))
                   (reset-frame nil))
               (loop for frame = (http-kit/http2::%h2-read-frame
                                  reader 16384 nil nil)
                     until (eq frame :eof)
                     when (= 3 (http-kit/http2::%h2-frame-type frame))
                       do (setf reset-frame frame))
               (ensure-true reset-frame)
               (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id reset-frame))
               (ensure-equal (octets 0 0 0 8)
                             (http-kit/http2::%h2-frame-payload reset-frame))))
        (http-kit/http2:close-http2-connection connection))))
  (deftest http2-public-connection-cancel-during-flow-control-wait-reuses-slot
    (let* ((initial-input
             (concatenate-octets
              (h2-frame 4 0 0
                        (octets 0 3 0 0 0 1 0 4 0 0 0 1))
              (h2-frame 1 5 1 (octets #x88))))
           (input
             (make-array (length initial-input)
                         :element-type (quote (unsigned-byte 8))
                         :adjustable t
                         :fill-pointer t
                         :initial-contents initial-input))
           (stream
             (make-instance (quote binary-session-stream) :input input))
           (connection
             (http-kit/http2:make-http2-connection :stream stream))
           (wait-symbol
             (quote http-kit/http2::%h2-scheduler-condition-wait))
           (original-wait (symbol-function wait-symbol))
           (flow-wait-entered (sb-thread:make-semaphore :count 0))
           (upload-finished (sb-thread:make-semaphore :count 0))
           (upload-thread nil)
           (upload-stream-id nil)
           (upload-error nil)
           (producer-calls 0)
           (follow-up-stream-id nil))
      (unwind-protect
           (progn
             (ensure-equal
              200
              (http-response-status
               (http-kit/http2:send-http2-request-over-connection
                connection
                (make-http-request :method "GET"
                                   :uri "https://127.0.0.1/warmup"))))
             (ensure-equal
              1
              (http-kit/http2:http2-connection-peer-max-concurrent-streams
               connection))
             (setf (symbol-function wait-symbol)
                   (lambda (wait-connection deadline clock-function)
                     (sb-thread:signal-semaphore flow-wait-entered)
                     (funcall original-wait
                              wait-connection deadline clock-function)))
             (http-kit/http2::%h2-with-scheduler-lock (connection)
               (setf
                (http-kit/http2::%http2-connection-scheduler-reader-active-p
                 connection)
                t))
             (setf upload-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (handler-case
                               (http-kit/http2:send-http2-request-over-connection
                                connection
                                (make-http-request
                                 :method "POST"
                                 :uri "https://127.0.0.1/cancel-upload")
                                :request-body-function
                                (lambda (maximum-size)
                                  (declare (ignore maximum-size))
                                  (incf producer-calls)
                                  (octets 65 66))
                                :request-body-length 2
                                :on-stream-open
                                (lambda (stream-id cancel-function)
                                  (declare (ignore cancel-function))
                                  (setf upload-stream-id stream-id)))
                             (error (condition)
                               (setf upload-error condition)))
                        (sb-thread:signal-semaphore upload-finished))))))
             (ensure-true
              (sb-thread:wait-on-semaphore flow-wait-entered :timeout 5))
             (ensure-equal 3 upload-stream-id)
             (http-kit/http2::%h2-with-scheduler-lock (connection)
               (ensure-equal
                0
                (http-kit/http2::%h2-connection-stream-window
                 connection upload-stream-id))
               (ensure-true
                (plusp
                 (http-kit/http2::%http2-connection-peer-connection-window-size
                  connection))))
             (ensure-true
              (http-kit/http2:cancel-http2-stream
               connection upload-stream-id))
             (ensure-true
              (sb-thread:wait-on-semaphore upload-finished :timeout 5))
             (sb-thread:join-thread upload-thread)
             (setf upload-thread nil)
             (ensure-true
              (typep upload-error (quote http-connection-error)))
             (ensure-equal
              (list :local-cancel upload-stream-id 8)
              (http-connection-error-cause upload-error))
             (ensure-equal 1 producer-calls)
             (ensure-true
              (http-kit/http2:http2-connection-open-p connection))
             (let ((upload-data
                     (remove-if-not
                      (lambda (frame)
                        (and (= 0
                                (http-kit/http2::%h2-frame-type frame))
                             (= upload-stream-id
                                (http-kit/http2::%h2-frame-stream-id frame))))
                      (h2-output-frames stream))))
               (ensure-equal 1 (length upload-data))
               (ensure-equal
                (octets 65)
                (http-kit/http2::%h2-frame-payload (first upload-data))))
             (setf (symbol-function wait-symbol) original-wait)
             (loop for octet across (h2-frame 1 5 5 (octets #x88))
                   do (vector-push-extend
                       octet (binary-session-input stream)))
             (http-kit/http2::%h2-with-scheduler-lock (connection)
               (setf
                (http-kit/http2::%http2-connection-scheduler-reader-active-p
                 connection)
                nil)
               (http-kit/http2::%h2-scheduler-broadcast connection))
             (ensure-equal
              200
              (http-response-status
               (http-kit/http2:send-http2-request-over-connection
                connection
                (make-http-request :method "GET"
                                   :uri "https://127.0.0.1/reused-slot")
                :on-stream-open
                (lambda (stream-id cancel-function)
                  (declare (ignore cancel-function))
                  (setf follow-up-stream-id stream-id)))))
             (ensure-equal 5 follow-up-stream-id)
             (ensure-true
              (http-kit/http2:http2-connection-open-p connection))
             (ensure-equal
              1
              (count-if
               (lambda (frame)
                 (and (= 0 (http-kit/http2::%h2-frame-type frame))
                      (= upload-stream-id
                         (http-kit/http2::%h2-frame-stream-id frame))))
               (h2-output-frames stream)))
        (when upload-thread
          (when (and upload-stream-id
                     (http-kit/http2:http2-connection-open-p connection))
            (http-kit/http2:cancel-http2-stream
             connection upload-stream-id))
          (sb-thread:join-thread upload-thread))
        (setf (symbol-function wait-symbol) original-wait)
        (http-kit/http2:close-http2-connection connection)))))

  (deftest http2-public-connection-cancel-custom-and-direct-api
    (let* ((stream (make-instance 'binary-session-stream
                                  :input (h2-response-wire (octets))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream))
           (failure nil))
      (unwind-protect
           (progn
             (handler-case
                 (http-kit/http2:send-http2-request-over-connection
                  connection
                  (make-http-request :method "GET"
                                     :uri "https://127.0.0.1/cancel-custom")
                  :on-stream-open
                  (lambda (stream-id cancel-function)
                    (ensure-equal 1 stream-id)
                    (funcall cancel-function :error-code 7)))
               (error (condition)
                 (setf failure condition)))
             (ensure-true (typep failure 'http-connection-error))
             (ensure-equal '(:local-cancel 1 7)
                           (http-connection-error-cause failure))
             (ensure-true
              (http-kit/http2:cancel-http2-stream connection 1 :error-code 9))
             (let ((reader
                     (http-kit/http2::%h2-reader-for
                      (subseq (binary-session-output stream)
                              (length (h2-preface)))))
                   (reset-payloads nil))
               (loop for frame = (http-kit/http2::%h2-read-frame
                                  reader 16384 nil nil)
                     until (eq frame :eof)
                     when (= 3 (http-kit/http2::%h2-frame-type frame))
                       do (progn
                            (ensure-equal
                             1 (http-kit/http2::%h2-frame-stream-id frame))
                            (push (http-kit/http2::%h2-frame-payload frame)
                                  reset-payloads)))
               (ensure-equal (list (octets 0 0 0 7) (octets 0 0 0 9))
                             (nreverse reset-payloads))))
        (http-kit/http2:close-http2-connection connection))))

  (deftest http2-public-connection-cancel-rejects-invalid-callback
    (let* ((stream (make-instance 'binary-session-stream
                                  :input (h2-response-wire (octets))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream)))
      (signals http-protocol-error
        (http-kit/http2:send-http2-request-over-connection
         connection
         (make-http-request :method "GET"
                            :uri "https://127.0.0.1/cancel-invalid")
         :on-stream-open 7))
      (ensure-equal 0 (length (binary-session-output stream)))
      (http-kit/http2:close-http2-connection connection)))

  (deftest http2-public-connection-uploads-through-window-updates
    (let* ((body (make-array 65536
                             :element-type '(unsigned-byte 8)
                             :initial-element #x5a))
           (window-increment (octets 0 0 1 0))
           (stream (make-instance 'binary-session-stream
                                  :input
                                  (concatenate-octets
                                   (h2-frame 4 0 0 (octets))
                                   (h2-frame 8 0 1 window-increment)
                                   (h2-frame 8 0 0 window-increment)
                                   (h2-frame 1 4 1 (octets #x88))
                                   (h2-frame 0 1 1 (octets 9 8)))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream))
           (transport (http-kit/http2:make-http2-client
                       :connection connection))
           (response
             (http-kit/http2:send-http2-request
              transport
              (make-http-request :method "POST"
                                 :uri "https://127.0.0.1/upload"
                                 :body body)))
           (output (binary-session-output stream))
           (reader (http-kit/http2::%h2-reader-for
                    (subseq output (length (h2-preface)))))
           (data-body (make-array 0
                                  :element-type '(unsigned-byte 8)
                                  :adjustable t
                                  :fill-pointer 0))
           (data-frame-count 0)
           (settings-ack-count 0))
      (loop for frame = (http-kit/http2::%h2-read-frame
                         reader 16384 nil nil)
            until (eq frame :eof)
            do (cond
                 ((= 0 (http-kit/http2::%h2-frame-type frame))
                  (incf data-frame-count)
                  (loop for octet across (http-kit/http2::%h2-frame-payload frame)
                        do (vector-push-extend octet data-body)))
                 ((and (= 4 (http-kit/http2::%h2-frame-type frame))
                       (= 1 (http-kit/http2::%h2-frame-flags frame)))
                  (incf settings-ack-count))))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal (octets 9 8) (http-response-body response))
      (ensure-equal body data-body)
      (ensure-equal 5 data-frame-count)
      (ensure-equal 1 settings-ack-count)
      (http-kit/http2:close-http2-connection connection)))

  (deftest http2-public-open-stream-uploads-through-window-updates
    (let* ((body (make-array 65536
                             :element-type '(unsigned-byte 8)
                             :initial-element #x5a))
           (window-increment (octets 0 0 1 0))
           (stream (make-instance 'binary-session-stream
                                  :input
                                  (concatenate-octets
                                   (h2-frame 4 0 0 (octets))
                                   (h2-frame 8 0 1 window-increment)
                                   (h2-frame 8 0 0 window-increment)
                                   (h2-frame 1 4 1 (octets #x88))
                                   (h2-frame 0 1 1 (octets 9 8)))))
           (closed nil)
           (transport
             (http-kit/http2:make-http2-client
              :open-stream (lambda (request &key timeout deadline)
                             (declare (ignore timeout deadline))
                             (ensure-equal "POST"
                                           (http-request-method request))
                             stream)
              :close-stream (lambda (closed-stream)
                              (ensure-equal stream closed-stream)
                              (setf closed t))))
           (response
             (http-kit/http2:send-http2-request
              transport
              (make-http-request :method "POST"
                                 :uri "https://127.0.0.1/upload"
                                 :body body)))
           (output (binary-session-output stream))
           (reader (http-kit/http2::%h2-reader-for
                    (subseq output (length (h2-preface)))))
           (data-body (make-array 0
                                  :element-type '(unsigned-byte 8)
                                  :adjustable t
                                  :fill-pointer 0))
           (data-frame-count 0)
           (settings-ack-count 0))
      (loop for frame = (http-kit/http2::%h2-read-frame
                         reader 16384 nil nil)
            until (eq frame :eof)
            do (cond
                 ((= 0 (http-kit/http2::%h2-frame-type frame))
                  (incf data-frame-count)
                  (loop for octet across (http-kit/http2::%h2-frame-payload frame)
                        do (vector-push-extend octet data-body)))
                 ((and (= 4 (http-kit/http2::%h2-frame-type frame))
                       (= 1 (http-kit/http2::%h2-frame-flags frame)))
                  (incf settings-ack-count))))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal (octets 9 8) (http-response-body response))
      (ensure-equal body data-body)
      (ensure-true closed)
      (ensure-equal 5 data-frame-count)
      (ensure-equal 1 settings-ack-count)))

  (deftest http2-public-open-stream-request-trailers
    (let* ((stream
             (make-instance 'binary-session-stream
                            :input (h2-response-wire (octets 9 8))))
           (closed nil)
           (transport
             (http-kit/http2:make-http2-client
              :open-stream (lambda (request &key timeout deadline)
                             (declare (ignore timeout deadline))
                             (ensure-equal "POST"
                                           (http-request-method request))
                             stream)
              :close-stream (lambda (closed-stream)
                              (ensure-equal stream closed-stream)
                              (setf closed t))))
           (response
             (http-kit/http2:send-http2-request
              transport
              (make-http-request
               :method "POST"
               :uri "https://127.0.0.1/trailers"
               :body (octets 1 2 3)
               :trailers (list (make-http-header "X-Checksum" "done")))))
           (output (binary-session-output stream))
           (reader (http-kit/http2::%h2-reader-for
                    (subseq output (length (h2-preface)))))
           (frames nil))
      (loop for frame = (http-kit/http2::%h2-read-frame
                         reader 16384 nil nil)
            until (eq frame :eof)
            do (push frame frames))
      (setf frames (nreverse frames))
      (let* ((stream-frames
               (remove-if-not
                (lambda (frame)
                  (and (= 1 (http-kit/http2::%h2-frame-stream-id frame))
                       (member (http-kit/http2::%h2-frame-type frame)
                               '(0 1))))
                frames))
             (initial (first stream-frames))
             (data (second stream-frames))
             (trailer (third stream-frames))
             (context (h2-hpack-context)))
        (ensure-equal 3 (length stream-frames))
        (ensure-equal 1 (http-kit/http2::%h2-frame-type initial))
        (ensure-equal 4 (http-kit/http2::%h2-frame-flags initial))
        (ensure-equal 0 (http-kit/http2::%h2-frame-type data))
        (ensure-equal 0 (http-kit/http2::%h2-frame-flags data))
        (ensure-equal (octets 1 2 3)
                      (http-kit/http2::%h2-frame-payload data))
        (ensure-equal 1 (http-kit/http2::%h2-frame-type trailer))
        (ensure-equal 5 (http-kit/http2::%h2-frame-flags trailer))
        (h2-decode-header-block (http-kit/http2::%h2-frame-payload initial) context)
        (ensure-equal
         (list (cons "x-checksum" "done"))
         (h2-decode-header-block (http-kit/http2::%h2-frame-payload trailer) context)))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal (octets 9 8) (http-response-body response))
      (ensure-true closed)))

#+sbcl
(progn
  (deftest http2-public-connection-streams-produced-body-through-window-updates
    (let* ((body (make-array 65536
                             :element-type '(unsigned-byte 8)
                             :initial-element #x5a))
           (position 0)
           (calls 0)
           (maximum-sizes nil)
           (window-increment (octets 0 0 1 0))
           (stream (make-instance 'binary-session-stream
                                  :input
                                  (concatenate-octets
                                   (h2-frame 4 0 0 (octets))
                                   (h2-frame 8 0 1 window-increment)
                                   (h2-frame 8 0 0 window-increment)
                                   (h2-frame 1 4 1 (octets #x88))
                                   (h2-frame 0 1 1 (octets 9 8)))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream))
           (transport (http-kit/http2:make-http2-client
                       :connection connection))
           (response
             (http-kit/http2:send-http2-request
              transport
              (make-http-request :method "POST"
                                 :uri "https://127.0.0.1/upload")
              :request-body-function
              (lambda (maximum-size)
                (ensure-true (plusp maximum-size))
                (incf calls)
                (push maximum-size maximum-sizes)
                (if (< position (length body))
                    (let* ((size (min maximum-size
                                      (- (length body) position)))
                           (chunk (make-array size
                                              :element-type
                                              '(unsigned-byte 8))))
                      (replace chunk body
                               :start2 position
                               :end2 (+ position size))
                      (incf position size)
                      chunk)
                    nil))
              :request-body-length (length body)))
           (output (binary-session-output stream))
           (reader (http-kit/http2::%h2-reader-for
                    (subseq output (length (h2-preface)))))
           (data-body (make-array 0
                                  :element-type '(unsigned-byte 8)
                                  :adjustable t
                                  :fill-pointer 0))
           (data-frame-count 0)
           (settings-ack-count 0))
      (loop for frame = (http-kit/http2::%h2-read-frame
                         reader 16384 nil nil)
            until (eq frame :eof)
            do (cond
                 ((= 0 (http-kit/http2::%h2-frame-type frame))
                  (incf data-frame-count)
                  (loop for octet across (http-kit/http2::%h2-frame-payload frame)
                        do (vector-push-extend octet data-body)))
                 ((and (= 4 (http-kit/http2::%h2-frame-type frame))
                       (= 1 (http-kit/http2::%h2-frame-flags frame)))
                  (incf settings-ack-count))))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal (octets 9 8) (http-response-body response))
      (ensure-equal body data-body)
      (ensure-equal (length body) position)
      (ensure-true (plusp calls))
      (ensure-true (every #'plusp maximum-sizes))
      (ensure-equal 5 data-frame-count)
      (ensure-equal 1 settings-ack-count)
      (http-kit/http2:close-http2-connection connection)))

  (deftest http2-public-client-adapter-streams-produced-body
    (let* ((body (make-array 65536
                             :element-type '(unsigned-byte 8)
                             :initial-element #x5a))
           (position 0)
           (calls 0)
           (window-increment (octets 0 0 1 0))
           (stream (make-instance 'binary-session-stream
                                  :input
                                  (concatenate-octets
                                   (h2-frame 4 0 0 (octets))
                                   (h2-frame 8 0 1 window-increment)
                                   (h2-frame 8 0 0 window-increment)
                                   (h2-frame 1 4 1 (octets #x88))
                                   (h2-frame 0 1 1 (octets 9 8)))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream))
           (client
             (make-http-client
              :cache nil
              :transport-function
              (http-kit/http2:make-http2-connection-transport connection)))
           (response
             (http-client-send
              client
              (http-client-request client "POST"
                                   "https://127.0.0.1/upload")
              :request-body-function
              (lambda (maximum-size)
                (incf calls)
                (if (< position (length body))
                    (let* ((size (min maximum-size
                                      (- (length body) position)))
                           (chunk (make-array size
                                              :element-type
                                              '(unsigned-byte 8))))
                      (replace chunk body
                               :start2 position
                               :end2 (+ position size))
                      (incf position size)
                      chunk)
                    nil))
              :request-body-length (length body)))
           (output (binary-session-output stream))
           (reader (http-kit/http2::%h2-reader-for
                    (subseq output (length (h2-preface)))))
           (data-body (make-array 0
                                  :element-type '(unsigned-byte 8)
                                  :adjustable t
                                  :fill-pointer 0)))
      (loop for frame = (http-kit/http2::%h2-read-frame
                         reader 16384 nil nil)
            until (eq frame :eof)
            do (when (= 0 (http-kit/http2::%h2-frame-type frame))
                 (loop for octet across (http-kit/http2::%h2-frame-payload frame)
                       do (vector-push-extend octet data-body))))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal (octets 9 8) (http-response-body response))
      (ensure-equal body data-body)
      (ensure-equal (length body) position)
      (ensure-true (plusp calls))
      (http-kit/http2:close-http2-connection connection)))

#+sbcl
(progn
  (deftest http2-public-connection-multiplexes-out-of-order-responses
    (let* ((request-one
             (make-http-request :method "GET"
                                :uri "https://127.0.0.1/one"))
           (request-two
             (make-http-request :method "GET"
                                :uri "https://127.0.0.1/two"))
           (stream-two-header-block
             (h2-header-block (cons ":status" "200")
                              (cons "content-length" "2")))
           (stream-two-header-split
             (max 1 (floor (length stream-two-header-block) 2)))
           (stream
             (make-instance
              'binary-session-stream
              :input
              (concatenate-octets
               (h2-frame 4 0 0 (octets))
               ;; Stream 3 responds first, with its HEADERS block split
               ;; across HEADERS and CONTINUATION.
               (h2-frame 1 0 3
                         (subseq stream-two-header-block
                                 0 stream-two-header-split))
               (h2-frame 9 4 3
                         (subseq stream-two-header-block
                                 stream-two-header-split))
               (h2-frame 1 4 1
                         (h2-header-block (cons ":status" "103")))
               (h2-frame 0 0 3 (octets 9))
               (h2-frame 0 1 3 (octets 8))
               (h2-frame 1 4 1
                         (h2-header-block (cons ":status" "200")
                                          (cons "content-length" "3")))
               (h2-frame 0 0 1 (octets 1))
               (h2-frame 0 0 1 (octets 2 3))
               (h2-frame 1 5 1
                         (h2-header-block (cons "x-trailer" "done"))))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream))
           (events nil)
           (responses
             (http-kit/http2:send-http2-requests-over-connection
              connection
              (list request-one request-two)
              :on-body-chunk
              (lambda (chunk request)
                (push (list request (copy-seq chunk)) events))))
           (output (binary-session-output stream))
           (reader (http-kit/http2::%h2-reader-for
                    (subseq output (length (h2-preface)))))
           (frames nil))
      (setf events (nreverse events))
      (loop for frame = (http-kit/http2::%h2-read-frame
                         reader 16384 nil nil)
            until (eq frame :eof)
            do (push frame frames))
      (setf frames (nreverse frames))
      (ensure-equal 2 (length responses))
      (ensure-equal 200 (http-response-status (first responses)))
      (ensure-equal (octets 1 2 3)
                    (http-response-body (first responses)))
      (ensure-equal "done"
                    (http-header-value
                     (http-response-trailers (first responses))
                     "x-trailer"))
      (ensure-equal 200 (http-response-status (second responses)))
      (ensure-equal (octets 9 8)
                    (http-response-body (second responses)))
      (ensure-equal (list (octets 9) (octets 8)
                          (octets 1) (octets 2 3))
                    (mapcar #'second events))
      (ensure-true (eq request-two (first (first events))))
      (ensure-true (eq request-one (first (third events))))
      (let ((request-header-frames
              (remove-if-not
               (lambda (frame)
                 (= 1 (http-kit/http2::%h2-frame-type frame)))
               frames)))
        (ensure-equal '(1 3)
                      (mapcar #'http-kit/http2::%h2-frame-stream-id
                              request-header-frames))
        (ensure-equal 1
                      (count-if
                       (lambda (frame)
                         (and (= 4 (http-kit/http2::%h2-frame-type frame))
                              (= 1 (http-kit/http2::%h2-frame-flags frame))))
                       frames))
        (let ((window-update-frames
                (remove-if-not
                 (lambda (frame)
                   (= 8 (http-kit/http2::%h2-frame-type frame)))
                 frames)))
          (ensure-equal 7 (length window-update-frames))
          (ensure-equal 4
                        (count 0 window-update-frames
                               :key #'http-kit/http2::%h2-frame-stream-id))
          (ensure-equal 2
                        (count 1 window-update-frames
                               :key #'http-kit/http2::%h2-frame-stream-id))
          (ensure-equal 1
                        (count 3 window-update-frames
                               :key #'http-kit/http2::%h2-frame-stream-id))))
      (ensure-true
       (http-kit/http2:http2-connection-session-started-p connection))
      (http-kit/http2:close-http2-connection connection)))))

#+sbcl
(progn
  (deftest http2-public-connection-schedules-batch-in-peer-limited-waves
    (let* ((requests
             (list (make-http-request :method "GET"
                                      :uri "https://127.0.0.1/one")
                   (make-http-request :method "GET"
                                      :uri "https://127.0.0.1/two")
                   (make-http-request :method "GET"
                                      :uri "https://127.0.0.1/three")))
           (stream
             (make-instance
              'binary-session-stream
              :input
              (concatenate-octets
               (h2-frame 1 4 3
                         (h2-header-block (cons ":status" "200")
                                          (cons "content-length" "1")))
               (h2-frame 0 1 3 (octets 2))
               (h2-frame 1 4 1
                         (h2-header-block (cons ":status" "200")
                                          (cons "content-length" "1")))
               (h2-frame 0 1 1 (octets 1))
               (h2-frame 1 4 5
                         (h2-header-block (cons ":status" "200")
                                          (cons "content-length" "1")))
               (h2-frame 0 1 5 (octets 3)))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream)))
      (setf (http-kit/http2::%http2-connection-session-started-p connection)
            t
            (http-kit/http2::%http2-connection-peer-max-concurrent-streams
             connection)
            2)
      (let* ((responses
               (http-kit/http2:send-http2-requests-over-connection
                connection requests))
             (reader
               (http-kit/http2::%h2-reader-for
                (binary-session-output stream)))
             (request-stream-ids nil))
        (loop for frame = (http-kit/http2::%h2-read-frame
                           reader 16384 nil nil)
              until (eq frame :eof)
              do (when (= 1 (http-kit/http2::%h2-frame-type frame))
                   (push (http-kit/http2::%h2-frame-stream-id frame)
                         request-stream-ids)))
        (ensure-equal '(1 3 5) (nreverse request-stream-ids))
        (ensure-equal (list (octets 1) (octets 2) (octets 3))
                      (mapcar #'http-response-body responses)))
      (http-kit/http2:close-http2-connection connection))))

#+sbcl
(progn
  (deftest http2-public-single-request-waits-for-peer-stream-capacity
    (let* ((request
             (make-http-request :method "GET"
                                :uri "https://127.0.0.1/resumed-single"))
           (stream
             (make-instance
              'binary-session-stream
              :input
              (concatenate-octets
               (h2-frame 4 0 0 (octets 0 3 0 0 0 1))
               (h2-frame 1 4 1
                         (h2-header-block (cons ":status" "200")
                                          (cons "content-length" "1")))
               (h2-frame 0 1 1 (octets 9)))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream)))
      (setf (http-kit/http2::%http2-connection-session-started-p connection)
            t
            (http-kit/http2::%http2-connection-peer-max-concurrent-streams
             connection)
            0)
      (let ((response
              (http-kit/http2:send-http2-request-over-connection
               connection request)))
        (ensure-equal (octets 9) (http-response-body response)))
      (http-kit/http2:close-http2-connection connection)))

  (deftest http2-public-connection-waits-for-peer-stream-capacity
    (let* ((request
             (make-http-request :method "GET"
                                :uri "https://127.0.0.1/resumed"))
           (stream
             (make-instance
              'binary-session-stream
              :input
              (concatenate-octets
               (h2-frame 4 0 0 (octets 0 3 0 0 0 1))
               (h2-frame 1 4 1
                         (h2-header-block (cons ":status" "200")
                                          (cons "content-length" "1")))
               (h2-frame 0 1 1 (octets 7)))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream)))
      (setf (http-kit/http2::%http2-connection-session-started-p connection)
            t
            (http-kit/http2::%http2-connection-peer-max-concurrent-streams
             connection)
            0)
      (let* ((response
               (first
                (http-kit/http2:send-http2-requests-over-connection
                 connection (list request))))
             (reader
               (http-kit/http2::%h2-reader-for
                (binary-session-output stream)))
             (frames nil))
        (loop for frame = (http-kit/http2::%h2-read-frame
                           reader 16384 nil nil)
              until (eq frame :eof)
              do (push frame frames))
        (setf frames (nreverse frames))
        (ensure-equal 1
                      (count-if
                       (lambda (frame)
                         (and (= 4 (http-kit/http2::%h2-frame-type frame))
                              (= 1 (http-kit/http2::%h2-frame-flags frame))))
                       frames))
        (ensure-equal '(1)
                      (loop for frame in frames
                            when (= 1 (http-kit/http2::%h2-frame-type frame))
                              collect (http-kit/http2::%h2-frame-stream-id
                                       frame)))
        (ensure-equal (octets 7) (http-response-body response)))
      (http-kit/http2:close-http2-connection connection))))

#+sbcl
(progn
  (deftest http2-public-connection-multiplexes-produced-request-bodies
    (let* ((produced-body (octets 1 2 3))
           (produced-position 0)
           (producer-calls 0)
           (producer
             (lambda (maximum-size)
               (ensure-true (plusp maximum-size))
               (incf producer-calls)
               (if (< produced-position (length produced-body))
                   (let* ((size (min maximum-size
                                     (- (length produced-body)
                                        produced-position)))
                          (chunk (make-array size
                                             :element-type
                                             '(unsigned-byte 8))))
                     (replace chunk produced-body
                              :start2 produced-position
                              :end2 (+ produced-position size))
                     (incf produced-position size)
                     chunk)
                   nil)))
           (request-one
             (make-http-request :method "POST"
                                :uri "https://127.0.0.1/produced"))
           (request-two
             (make-http-request :method "POST"
                                :uri "https://127.0.0.1/in-memory"
                                :body (octets 4 5)))
           (stream
             (make-instance
              'binary-session-stream
              :input
              (concatenate-octets
               (h2-frame 4 0 0 (octets))
               ;; Stream 3 responds before stream 1.
               (h2-frame 1 4 3
                         (h2-header-block (cons ":status" "200")
                                          (cons "content-length" "1")))
               (h2-frame 0 1 3 (octets 8))
               (h2-frame 1 4 1
                         (h2-header-block (cons ":status" "200")
                                          (cons "content-length" "1")))
               (h2-frame 0 1 1 (octets 7)))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream))
           (responses
             (http-kit/http2:send-http2-requests-over-connection
              connection
              (list request-one request-two)
              :request-body-functions (list producer nil)
              :request-body-lengths (list (length produced-body) nil)))
           (output (binary-session-output stream))
           (reader (http-kit/http2::%h2-reader-for
                    (subseq output (length (h2-preface)))))
           (frames nil))
      (loop for frame = (http-kit/http2::%h2-read-frame
                         reader 16384 nil nil)
            until (eq frame :eof)
            do (push frame frames))
      (setf frames (nreverse frames))
      (let ((data-frames
              (remove-if-not
               (lambda (frame)
                 (= 0 (http-kit/http2::%h2-frame-type frame)))
               frames)))
        (ensure-equal '(1 3)
                      (mapcar #'http-kit/http2::%h2-frame-stream-id
                              data-frames))
        (ensure-equal produced-body
                      (http-kit/http2::%h2-frame-payload
                       (first data-frames)))
        (ensure-equal (octets 4 5)
                      (http-kit/http2::%h2-frame-payload
                       (second data-frames))))
      (ensure-equal 2 (length responses))
      (ensure-equal (octets 7)
                    (http-response-body (first responses)))
      (ensure-equal (octets 8)
                    (http-response-body (second responses)))
      (ensure-equal (length produced-body) produced-position)
      (ensure-true (plusp producer-calls))
      (http-kit/http2:close-http2-connection connection))))

  (deftest http2-public-connection-ping
    (let* ((payload (octets 1 2 3 4 5 6 7 8))
           (stream
             (make-instance
              'binary-session-stream
              :input
              (concatenate-octets
               (h2-response-wire (octets))
               (h2-frame 6 1 0 payload))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream))
           (response
             (http-kit/http2:send-http2-request-over-connection
              connection
              (make-http-request :method "GET"
                                 :uri "https://127.0.0.1/ping")))
           (result (http-kit/http2:ping-http2-connection
                    connection :payload payload))
           (output (binary-session-output stream))
           (reader (http-kit/http2::%h2-reader-for
                    (subseq output (length (h2-preface)))))
           (frames nil))
      (loop for frame = (http-kit/http2::%h2-read-frame
                         reader 16384 nil nil)
            until (eq frame :eof)
            do (push frame frames))
      (setf frames (nreverse frames))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal payload result)
      (ensure-true
       (find-if
        (lambda (frame)
          (and (= 6 (http-kit/http2::%h2-frame-type frame))
               (zerop (http-kit/http2::%h2-frame-flags frame))
               (ensure-equal payload
                             (http-kit/http2::%h2-frame-payload frame))))
        frames))
      (ensure-true (http-kit/http2:http2-connection-open-p connection))
      (http-kit/http2:close-http2-connection connection)))

  (deftest http2-public-connection-extended-connect-requires-peer-setting
    (let* ((stream
             (make-instance
              'binary-session-stream
              :input (h2-frame 4 0 0 (octets))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream)))
      (signals http-protocol-error
        (http-kit/http2:send-http2-request-over-connection
         connection
         (make-http-request :method "CONNECT"
                            :protocol "websocket"
                            :uri "https://127.0.0.1/")))
      (let* ((output (binary-session-output stream))
             (reader (http-kit/http2::%h2-reader-for
                      (subseq output (length (h2-preface)))))
             (settings (http-kit/http2::%h2-read-frame
                        reader 16384 nil nil))
             (ack (http-kit/http2::%h2-read-frame
                   reader 16384 nil nil)))
        (ensure-equal 4 (http-kit/http2::%h2-frame-type settings))
        (ensure-equal 4 (http-kit/http2::%h2-frame-type ack))
        (ensure-true (logbitp 0 (http-kit/http2::%h2-frame-flags ack)))
        (ensure-equal :eof
                      (http-kit/http2::%h2-read-frame
                       reader 16384 nil nil)))))

  (deftest http2-public-connection-extended-connect-after-peer-setting
    (let* ((stream
             (make-instance
              'binary-session-stream
              :input (concatenate-octets
                      (h2-frame 4 0 0 (octets 0 8 0 0 0 1))
                      (h2-frame 1 5 1 (octets #x88)))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream))
           (response
             (http-kit/http2:send-http2-request-over-connection
              connection
              (make-http-request :method "CONNECT"
                                 :protocol "websocket"
                                 :uri "https://127.0.0.1/")))
           (output (binary-session-output stream))
           (reader (http-kit/http2::%h2-reader-for
                    (subseq output (length (h2-preface)))))
           (settings (http-kit/http2::%h2-read-frame
                      reader 16384 nil nil))
           (ack (http-kit/http2::%h2-read-frame
                 reader 16384 nil nil))
           (headers (http-kit/http2::%h2-read-frame
                     reader 16384 nil nil)))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal 4 (http-kit/http2::%h2-frame-type settings))
      (ensure-equal 4 (http-kit/http2::%h2-frame-type ack))
      (ensure-true (logbitp 0 (http-kit/http2::%h2-frame-flags ack)))
      (ensure-equal 1 (http-kit/http2::%h2-frame-type headers))
      (http-kit/http2:close-http2-connection connection)))

  (deftest http2-public-connection-graceful-shutdown
    (let* ((stream
             (make-instance
              'binary-session-stream
              :input (h2-response-wire (octets))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream)))
      (http-kit/http2:send-http2-request-over-connection
       connection
       (make-http-request :method "GET"
                          :uri "https://127.0.0.1/shutdown"))
      (http-kit/http2:graceful-shutdown-http2-connection
       connection :error-code 7 :debug-data (octets 100 101))
      (let* ((output (binary-session-output stream))
             (reader (http-kit/http2::%h2-reader-for
                      (subseq output (length (h2-preface)))))
             (frames nil))
        (loop for frame = (http-kit/http2::%h2-read-frame
                           reader 16384 nil nil)
              until (eq frame :eof)
              do (push frame frames))
        (setf frames (nreverse frames))
        (let ((goaway
                (find-if
                 (lambda (frame)
                   (= 7 (http-kit/http2::%h2-frame-type frame)))
                 frames)))
          (ensure-true goaway)
          (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id goaway))
          (ensure-equal (octets 0 0 0 1 0 0 0 7 100 101)
                        (http-kit/http2::%h2-frame-payload goaway))))
      (ensure-true (http-kit/http2:http2-connection-open-p connection))
      (ensure-true (http-kit/http2:http2-connection-draining-p connection))
      (ensure-equal 1
                    (http-kit/http2:http2-connection-local-goaway-last-stream-id
                     connection))
      (signals http-protocol-error
        (http-kit/http2:send-http2-goaway connection :last-stream-id 3))
      (ensure-equal 1
                    (http-kit/http2:http2-connection-local-goaway-last-stream-id
                     connection))
      (signals http-protocol-error
        (http-kit/http2:send-http2-request-over-connection
         connection
         (make-http-request :method "GET"
                            :uri "https://127.0.0.1/rejected")))
      (http-kit/http2:close-http2-connection connection)))
(deftest http2-public-connection-state-accessors
    (let ((connection
            (http-kit/http2:make-http2-connection
             :stream
             (make-instance
              'binary-session-stream
              :input (make-array 0 :element-type '(unsigned-byte 8))))))
      (ensure-true (http-kit/http2:http2-connection-open-p connection))
      (ensure-true
       (not (http-kit/http2:http2-connection-session-started-p connection)))
      (ensure-equal 16384
                    (http-kit/http2:http2-connection-peer-max-frame-size
                     connection))
      (ensure-equal 4096
                    (http-kit/http2:http2-connection-peer-max-table-size
                     connection))
      (ensure-equal 65535
                    (http-kit/http2:http2-connection-peer-initial-window-size
                     connection))
      (ensure-equal 65535
                    (http-kit/http2:http2-connection-peer-connection-window-size
                     connection))
      (ensure-true
       (not (http-kit/http2:http2-connection-goaway-last-stream-id connection)))
      (ensure-true
       (not (http-kit/http2:http2-connection-local-goaway-last-stream-id
             connection)))
      (ensure-true
       (not (http-kit/http2:http2-connection-draining-p connection)))
      (setf (http-kit/http2::%http2-connection-session-started-p connection) t
            (http-kit/http2::%http2-connection-peer-max-frame-size connection)
            8192
            (http-kit/http2::%http2-connection-peer-max-table-size connection)
            1024
            (http-kit/http2::%http2-connection-peer-initial-window-size
             connection)
            32768
            (http-kit/http2::%http2-connection-peer-connection-window-size
             connection)
            32768
            (http-kit/http2::%http2-connection-goaway-last-stream-id connection)
            7
            (http-kit/http2::%http2-connection-local-goaway-last-stream-id
             connection)
            5
            (http-kit/http2::%http2-connection-draining-p connection) t)
      (ensure-true
       (http-kit/http2:http2-connection-session-started-p connection))
      (ensure-equal 8192
                    (http-kit/http2:http2-connection-peer-max-frame-size
                     connection))
      (ensure-equal 1024
                    (http-kit/http2:http2-connection-peer-max-table-size
                     connection))
      (ensure-equal 32768
                    (http-kit/http2:http2-connection-peer-initial-window-size
                     connection))
      (ensure-equal 32768
                    (http-kit/http2:http2-connection-peer-connection-window-size
                     connection))
      (ensure-equal 7
                    (http-kit/http2:http2-connection-goaway-last-stream-id
                     connection))
      (ensure-equal 5
                    (http-kit/http2:http2-connection-local-goaway-last-stream-id
                     connection))
      (ensure-true (http-kit/http2:http2-connection-draining-p connection))
      (ensure-true (not (http-kit/http2:http2-connection-open-p nil)))
      (ensure-true
       (not (http-kit/http2:http2-connection-session-started-p nil)))
      (ensure-true
       (not (http-kit/http2:http2-connection-peer-max-frame-size nil)))
      (ensure-true
       (not (http-kit/http2:http2-connection-draining-p nil)))))

(deftest http2-public-client-streams-produced-body
    (let* ((body (make-array 65536
                             :element-type '(unsigned-byte 8)
                             :initial-element #x5a))
           (window-increment (octets 0 0 1 0))
           (stream (make-instance 'binary-session-stream
                                  :input
                                  (concatenate-octets
                                   (h2-frame 4 0 0 (octets))
                                   (h2-frame 8 0 1 window-increment)
                                   (h2-frame 8 0 0 window-increment)
                                   (h2-frame 1 4 1 (octets #x88))
                                   (h2-frame 0 1 1 (octets 9 8)))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream))
           (client
             (make-http-client
              :cache nil
              :transport-function
              (http-kit/http2:make-http2-connection-transport connection)))
           (response nil)
           (request-body-function nil)
           (body-position nil)
           (body-calls nil))
      (multiple-value-setq (request-body-function body-position body-calls)
        (make-octet-body-producer body))
      (setf response
             (http-client-send
              client
              (http-client-request client "POST"
                                   "https://127.0.0.1/upload")
              :request-body-function request-body-function
              :request-body-length (length body)))
      (multiple-value-bind (data-body)
          (h2-output-data-summary stream)
        (ensure-equal 200 (http-response-status response))
        (ensure-equal (octets 9 8) (http-response-body response))
        (ensure-equal body data-body)
        (ensure-equal (length body) (funcall body-position))
        (ensure-true (plusp (funcall body-calls)))
        (http-kit/http2:close-http2-connection connection))))
#+sbcl
(deftest http2-public-produced-body-respects-peer-frame-size-shrink
  (let* ((initial-frame-size 32768)
         (reduced-frame-size 16384)
         (body (make-array initial-frame-size
                           :element-type '(unsigned-byte 8)
                           :initial-element #x5a))
         (stream
           (make-instance 'binary-session-stream
                          :input
                          (concatenate-octets
                           (h2-frame 4 0 0
                                     (octets 0 5 0 0 128 0))
                           (h2-frame 1 5 1
                                     (h2-header-block (cons ":status" "200")))
                           (h2-frame 1 5 3
                                     (h2-header-block (cons ":status" "200"))))))
         (connection
           (http-kit/http2:make-http2-connection
            :stream stream :max-frame-size initial-frame-size))
         (window-symbol
           (find-symbol "%H2-CONNECTION-STREAM-WINDOW" "HTTP-KIT/HTTP2"))
         (original-window (symbol-function window-symbol))
         (shrink-frame
           (h2-frame-object 4 0 0 (octets 0 5 0 0 64 0)))
         (settings-applied-p nil)
         (producer-calls 0)
         (producer-thread nil)
         (response nil))
    (unwind-protect
         (progn
           (let ((warmup-response
                   (http-kit/http2:send-http2-request-over-connection
                    connection
                    (make-http-request :method "GET"
                                       :uri "https://127.0.0.1/frame-size-warmup"))))
             (ensure-equal 200 (http-response-status warmup-response))
             (ensure-equal
              initial-frame-size
              (http-kit/http2:http2-connection-peer-max-frame-size connection)))
           (setf (symbol-function window-symbol)
                 (lambda (window-connection stream-id)
                   ;; Apply a peer SETTINGS frame after the body loop has read
                   ;; its old frame limit but before the DATA writer takes the lock.
                   (when (and (eq window-connection connection)
                              (eq sb-thread:*current-thread* producer-thread)
                              (= stream-id 3)
                              (plusp producer-calls)
                              (not settings-applied-p))
                     (setf settings-applied-p t)
                     (http-kit/http2::%h2-scheduler-control-frame
                      connection shrink-frame
                      (http-kit/http2::%h2-scheduler-control-writer
                       connection
                       (http-kit/http2::%http2-connection-clock-function
                        connection))))
                   (funcall original-window window-connection stream-id)))
           (setf response
                 (handler-case
                     (http-kit/http2:send-http2-request-over-connection
                      connection
                      (make-http-request :method "POST"
                                         :uri "https://127.0.0.1/frame-size-upload")
                      :request-body-function
                      (lambda (maximum-size)
                        (incf producer-calls)
                        (setf producer-thread sb-thread:*current-thread*)
                        (when (= producer-calls 1)
                          (ensure-equal initial-frame-size maximum-size)
                          body))
                      :request-body-length (length body))
                   (http-kit:http-connection-error (condition)
                     (error "HTTP/2 frame-size request failed with cause: ~A"
                            (http-kit:http-connection-error-cause condition)))))
           (ensure-equal 200 (http-response-status response))
           (ensure-true settings-applied-p)
           (ensure-equal
            reduced-frame-size
            (http-kit/http2:http2-connection-peer-max-frame-size connection))
           (let* ((reader
                    (http-kit/http2::%h2-reader-for
                     (subseq (binary-session-output stream)
                             (length (h2-preface)))))
                  (frames
                    (loop for frame = (http-kit/http2::%h2-read-frame
                                       reader initial-frame-size nil nil)
                          until (eq frame :eof)
                          collect frame))
                  (data-frames
                    (remove-if-not
                     (lambda (frame)
                       (and (= 0 (http-kit/http2::%h2-frame-type frame))
                            (= 3 (http-kit/http2::%h2-frame-stream-id frame))))
                     frames))
                  (payloads
                    (mapcar #'http-kit/http2::%h2-frame-payload data-frames)))
             (ensure-equal 2 (length data-frames))
             (ensure-true
              (every (lambda (frame)
                       (<= (length (http-kit/http2::%h2-frame-payload frame))
                           reduced-frame-size))
                     data-frames))
             (ensure-equal (list reduced-frame-size reduced-frame-size)
                           (mapcar #'length payloads))
             (ensure-equal body (apply #'concatenate-octets payloads))
             (ensure-equal 0
                           (logand 1
                                   (http-kit/http2::%h2-frame-flags
                                    (first data-frames))))
             (ensure-equal 1
                           (logand 1
                                   (http-kit/http2::%h2-frame-flags
                                    (second data-frames))))))
      (setf (symbol-function window-symbol) original-window)
      (http-kit/http2:close-http2-connection connection))))

  (deftest http2-client-control-budget-counts-all-but-rst-stream
    (let* ((stream (make-instance 'binary-session-stream
                                  :input
                                  (concatenate-octets
                                   (h2-frame 4 0 0 (octets))
                                   (h2-frame 6 0 0 (octets 1 2 3 4 5 6 7 8))
                                   (h2-frame 6 0 0 (octets 9 10 11 12 13 14 15 16))
                                   (h2-frame 1 5 1 (octets #x88)))))
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream
              :max-control-frames 2
              :max-control-window 10.0d0
              :close-stream nil)))
      (signals http-protocol-error
        (http-kit/http2:send-http2-request-over-connection
         connection
         (make-http-request :method "GET"
                            :uri "https://127.0.0.1/data")))
      (let* ((frames (h2-output-frames stream))
             (acks (count-if
                    (lambda (frame)
                      (and (= (http-kit/http2::%h2-frame-type frame) 6)
                           (= (http-kit/http2::%h2-frame-flags frame) 1)))
                    frames))
             (goaway (find-if
                      (lambda (frame)
                        (= (http-kit/http2::%h2-frame-type frame) 7))
                      frames)))
        (ensure-equal 1 acks)
        (ensure-true goaway)
        (ensure-equal 11
                      (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload goaway) 4)))))

#+sbcl
(deftest http2-public-control-constructor-boundaries
  (signals http-connection-error
    (http-kit/http2:make-http2-connection :stream nil))
  (let ((stream (make-instance 'binary-session-stream :input (octets))))
    (dolist (options '((:close-stream :invalid)
                       (:max-control-window -1)
                       (:max-control-window "1")
                       (:max-control-window #C(1 1))
                       (:clock-function :invalid)))
      (signals http-protocol-error
        (apply #'http-kit/http2:make-http2-connection :stream stream options))
      (ensure-equal (octets) (binary-session-output stream)))
    (let* ((close-count 0)
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream :max-body-bytes 0 :max-control-frames 0
              :max-control-window 0 :clock-function (lambda () 0)
              :close-stream (lambda (closed-stream)
                              (ensure-true (eq stream closed-stream))
                              (incf close-count)))))
      (ensure-true (http-kit/http2:http2-connection-open-p connection))
      (ensure-true (eq connection
                       (http-kit/http2:close-http2-connection connection)))
      (http-kit/http2:close-http2-connection connection)
      (ensure-equal 1 close-count)
      (ensure-true (not (http-kit/http2:http2-connection-open-p connection)))))
  (signals http-protocol-error
    (http-kit/http2:close-http2-connection nil)))

#+sbcl
(deftest http2-public-control-before-session
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream)))
    (dolist (operation
             (list (lambda () (http-kit/http2:send-http2-goaway connection))
                   (lambda ()
                     (http-kit/http2:send-http2-priority-update connection 1))
                   (lambda () (http-kit/http2:ping-http2-connection connection))))
      (signals http-protocol-error (funcall operation))
      (ensure-equal (octets) (binary-session-output stream))
      (ensure-true (http-kit/http2:http2-connection-open-p connection))
      (ensure-true (not (http-kit/http2:http2-connection-draining-p connection))))
    (http-kit/http2:close-http2-connection connection))
  (signals http-protocol-error (http-kit/http2:send-http2-goaway nil))
  (signals http-protocol-error (http-kit/http2:send-http2-priority-update nil 1))
  (signals http-protocol-error (http-kit/http2:ping-http2-connection nil)))

#+sbcl
(deftest http2-public-control-goaway-wire-boundaries
  (let* ((stream (make-instance 'binary-session-stream
                                :input (h2-response-wire (octets))))
         (connection (http-kit/http2:make-http2-connection :stream stream)))
    (http-kit/http2:send-http2-request-over-connection
     connection (make-http-request :method "GET" :uri "https://127.0.0.1/"))
    (let ((before (copy-seq (binary-session-output stream))))
      (dolist (options
               (list '(:error-code -1) '(:error-code 4294967296)
                     '(:error-code 1/2) '(:last-stream-id -1)
                     '(:last-stream-id 2147483648) '(:last-stream-id "1")
                     (list :debug-data #(1 2))
                     (list :debug-data (make-array '(1 2)
                                                   :element-type '(unsigned-byte 8)))
                     (list :debug-data (make-array 16377
                                                   :element-type '(unsigned-byte 8)))))
        (signals http-protocol-error
          (apply #'http-kit/http2:send-http2-goaway connection options))
        (ensure-equal before (binary-session-output stream))
        (ensure-true (not (http-kit/http2:http2-connection-draining-p connection)))
        (ensure-true
         (null (http-kit/http2:http2-connection-local-goaway-last-stream-id
                connection)))))
    (let ((debug (make-array 16376 :element-type '(unsigned-byte 8)
                                  :initial-element #x5a)))
      (http-kit/http2:send-http2-goaway
       connection :last-stream-id #x7fffffff :error-code #xffffffff
       :debug-data debug)
      (setf (aref debug 0) 0)
      (let* ((frame (car (last (h2-output-frames stream))))
             (payload (http-kit/http2::%h2-frame-payload frame)))
        (ensure-equal 7 (http-kit/http2::%h2-frame-type frame))
        (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id frame))
        (ensure-equal 16384 (length payload))
        (ensure-equal #x7fffffff (http-kit/http2::%h2-u32 payload 0))
        (ensure-equal #xffffffff (http-kit/http2::%h2-u32 payload 4))
        (ensure-true (every (lambda (byte) (= byte #x5a)) (subseq payload 8)))))
    (http-kit/http2:send-http2-goaway connection :last-stream-id 0)
    (ensure-equal 0
                  (http-kit/http2:http2-connection-local-goaway-last-stream-id
                   connection))
    (ensure-equal (octets 0 0 0 0 0 0 0 0)
                  (http-kit/http2::%h2-frame-payload
                   (car (last (h2-output-frames stream)))))
    (ensure-true (http-kit/http2:http2-connection-open-p connection))
    (ensure-true (http-kit/http2:http2-connection-draining-p connection))
    (http-kit/http2:close-http2-connection connection)))
(deftest http2-public-control-goaway-serializes-with-scheduled-stream-open
  (let* ((stream
           (make-instance
            'binary-session-stream
            :input (concatenate-octets
                    (h2-frame 4 0 0 (octets))
                    (h2-frame 1 5 1 (octets #x88))
                    (h2-frame 1 5 3 (octets #x88)))))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (scheduler-lock
           (http-kit/http2::%http2-connection-scheduler-lock connection))
         (goaway-started (sb-thread:make-semaphore :count 0))
         (goaway-completed (sb-thread:make-semaphore :count 0))
         (request-started (sb-thread:make-semaphore :count 0))
         (goaway-thread nil)
         (request-thread nil)
         (goaway-error nil)
         (request-error nil)
         (request-response nil)
         (goaway-started-p nil)
         (request-started-p nil)
         (goaway-waiting-for-lock-p nil)
         (goaway-completed-after-unlock-p nil))
    (unwind-protect
         (progn
           (http-kit/http2:send-http2-request-over-connection
            connection
            (make-http-request :method "GET" :uri "https://127.0.0.1/first"))
           (sb-thread:with-mutex (scheduler-lock)
             (setf goaway-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (sb-thread:signal-semaphore goaway-started)
                      (handler-case
                          (http-kit/http2:send-http2-goaway connection)
                        (error (condition)
                          (setf goaway-error condition)))
                      (sb-thread:signal-semaphore goaway-completed))))
             (setf goaway-started-p
                   (sb-thread:wait-on-semaphore goaway-started :timeout 5)
                   goaway-waiting-for-lock-p
                   (loop with deadline =
                           (+ (get-internal-real-time)
                              (* 5 internal-time-units-per-second))
                         for waiting-for =
                           (sb-thread::thread-waiting-for goaway-thread)
                         when (eq waiting-for scheduler-lock)
                           return t
                         while (< (get-internal-real-time) deadline)
                         do (sb-thread:thread-yield)
                         finally (return nil))
                   request-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (sb-thread:signal-semaphore request-started)
                      (handler-case
                          (setf request-response
                                (http-kit/http2:send-http2-request-over-connection
                                 connection
                                 (make-http-request
                                  :method "GET"
                                  :uri "https://127.0.0.1/second")))
                        (error (condition)
                          (setf request-error condition)))))
                   request-started-p
                   (sb-thread:wait-on-semaphore request-started :timeout 5)))
           (setf goaway-completed-after-unlock-p
                 (sb-thread:wait-on-semaphore goaway-completed :timeout 5))
           (sb-thread:join-thread goaway-thread)
           (setf goaway-thread nil)
           (sb-thread:join-thread request-thread)
           (setf request-thread nil)
           (ensure-true goaway-started-p)
           (ensure-true request-started-p)
           (ensure-true goaway-waiting-for-lock-p)
           (ensure-true goaway-completed-after-unlock-p)
           (ensure-true (null goaway-error))
           (ensure-true (http-kit/http2:http2-connection-draining-p connection))
           (let* ((frames (h2-output-frames stream))
                  (request-headers
                    (find-if (lambda (frame)
                               (and (= 1 (http-kit/http2::%h2-frame-type frame))
                                    (= 3 (http-kit/http2::%h2-frame-stream-id frame))))
                             frames))
                  (goaway
                    (find-if (lambda (frame)
                               (= 7 (http-kit/http2::%h2-frame-type frame)))
                             frames))
                  (last-stream-id
                    (http-kit/http2::%h2-u32
                     (http-kit/http2::%h2-frame-payload goaway) 0)))
             (ensure-true goaway)
             (if request-headers
                 (progn
                   (ensure-true (null request-error))
                   (ensure-equal 200 (http-response-status request-response))
                   (ensure-true (>= last-stream-id 3)))
                 (progn
                   (ensure-true (typep request-error 'http-protocol-error))
                   (ensure-equal 1 last-stream-id)))))
      (when goaway-thread
        (sb-thread:join-thread goaway-thread))
      (when request-thread
        (sb-thread:join-thread request-thread))
      (http-kit/http2:close-http2-connection connection))))
#+sbcl
(deftest http2-public-single-request-waits-for-peer-stream-capacity-settings-update
  (let* ((initial-input
           (concatenate-octets
            (h2-frame 4 0 0 (octets 0 3 0 0 0 1))
            (h2-frame 1 4 1
                      (h2-header-block (cons ":status" "200")
                                       (cons "content-length" "1")))
            (h2-frame 0 1 1 (octets 9))))
         (input
           (make-array (length initial-input)
                       :element-type '(unsigned-byte 8)
                       :adjustable t
                       :fill-pointer t
                       :initial-contents initial-input))
         (stream (make-instance 'binary-session-stream :input input))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (wait-symbol 'http-kit/http2::%h2-scheduler-condition-wait)
         (original-wait (symbol-function wait-symbol))
         (capacity-wait-entered (sb-thread:make-semaphore :count 0))
         (request-opened (sb-thread:make-semaphore :count 0))
         (request-finished (sb-thread:make-semaphore :count 0))
         (request-thread nil)
         (request-stream-id nil)
         (request-response nil)
         (request-error nil)
         (capacity-wait-entered-p nil)
         (request-opened-p nil)
         (request-finished-p nil))
    (unwind-protect
         (progn
           (ensure-equal
            (octets 9)
            (http-response-body
             (http-kit/http2:send-http2-request-over-connection
              connection
              (make-http-request :method "GET"
                                 :uri "https://127.0.0.1/capacity-warmup"))))
           (http-kit/http2::%h2-with-scheduler-lock (connection)
             (setf (http-kit/http2::%http2-connection-peer-max-concurrent-streams
                    connection)
                   0))
           (setf (symbol-function wait-symbol)
                 (lambda (wait-connection deadline clock-function)
                   (sb-thread:signal-semaphore capacity-wait-entered)
                   (funcall original-wait wait-connection deadline clock-function)))
           (http-kit/http2::%h2-with-scheduler-lock (connection)
             (setf (http-kit/http2::%http2-connection-scheduler-reader-active-p
                    connection)
                   t))
           (setf request-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (setf request-response
                                   (http-kit/http2:send-http2-request-over-connection
                                    connection
                                    (make-http-request
                                     :method "GET"
                                     :uri "https://127.0.0.1/settings-update")
                                    :timeout 5
                                    :on-stream-open
                                    (lambda (stream-id cancel-function)
                                      (declare (ignore cancel-function))
                                      (setf request-stream-id stream-id)
                                      (http-kit/http2::%h2-with-scheduler-lock
                                          (connection)
                                        (setf
                                         (http-kit/http2::%http2-connection-scheduler-reader-active-p
                                          connection)
                                         t))
                                      (sb-thread:signal-semaphore request-opened))))
                           (error (condition)
                             (setf request-error condition)))
                      (sb-thread:signal-semaphore request-finished)))))
           (setf capacity-wait-entered-p
                 (sb-thread:wait-on-semaphore capacity-wait-entered :timeout 5))
           (ensure-true capacity-wait-entered-p)
           (ensure-equal
            0
            (count-if
             (lambda (frame)
               (and (= 1 (http-kit/http2::%h2-frame-type frame))
                    (= 3 (http-kit/http2::%h2-frame-stream-id frame))))
             (h2-output-frames stream)))
           (loop for octet across (h2-frame 4 0 0 (octets 0 3 0 0 0 1))
                 do (vector-push-extend octet input))
           (http-kit/http2::%h2-with-scheduler-lock (connection)
             (setf (http-kit/http2::%http2-connection-scheduler-reader-active-p
                    connection)
                   nil)
             (http-kit/http2::%h2-scheduler-broadcast connection))
           (setf request-opened-p
                 (sb-thread:wait-on-semaphore request-opened :timeout 5))
           (ensure-true request-opened-p)
           (ensure-equal 3 request-stream-id)
           (http-kit/http2::%h2-with-scheduler-lock (connection)
             (ensure-equal
              1
              (http-kit/http2::%http2-connection-peer-max-concurrent-streams
               connection)))
           (ensure-equal
            1
            (count-if
             (lambda (frame)
               (and (= 1 (http-kit/http2::%h2-frame-type frame))
                    (= 3 (http-kit/http2::%h2-frame-stream-id frame))))
             (h2-output-frames stream)))
           (loop for octet across
                 (concatenate-octets
                  (h2-frame 1 4 3
                            (h2-header-block (cons ":status" "200")
                                             (cons "content-length" "1")))
                  (h2-frame 0 1 3 (octets 7)))
                 do (vector-push-extend octet input))
           (http-kit/http2::%h2-with-scheduler-lock (connection)
             (setf (http-kit/http2::%http2-connection-scheduler-reader-active-p
                    connection)
                   nil)
             (http-kit/http2::%h2-scheduler-broadcast connection))
           (setf request-finished-p
                 (sb-thread:wait-on-semaphore request-finished :timeout 5))
           (ensure-true request-finished-p)
           (sb-thread:join-thread request-thread)
           (setf request-thread nil)
           (ensure-true (null request-error))
           (ensure-equal (octets 7) (http-response-body request-response))
           (ensure-true (http-kit/http2:http2-connection-open-p connection)))
      (setf (symbol-function wait-symbol) original-wait)
      (when request-thread
        (http-kit/http2::%h2-with-scheduler-lock (connection)
          (setf (http-kit/http2::%http2-connection-scheduler-reader-active-p
                 connection)
                nil)
          (http-kit/http2::%h2-scheduler-broadcast connection))
        (http-kit/http2:close-http2-connection connection)
        (sb-thread:join-thread request-thread))
      (http-kit/http2:close-http2-connection connection))))
#+sbcl
(deftest http2-public-single-request-reassembles-multiple-continuations
  (let* ((header-block
           (h2-header-block (cons ":status" "200")
                            (cons "content-length" "3")
                            (cons "x-continuation" "reassembled")))
         (first-end (floor (length header-block) 3))
         (second-end (floor (* 2 (length header-block)) 3))
         (first-fragment (subseq header-block 0 first-end))
         (intermediate-fragment (subseq header-block first-end second-end))
         (final-fragment (subseq header-block second-end))
         (body (octets 1 2 3))
         (stream
           (make-instance
            'binary-session-stream
            :input
            (concatenate-octets
             (h2-frame 4 0 0 (octets))
             (h2-frame 1 0 1 first-fragment)
             ;; The scheduler must retain the block until the final continuation.
             (h2-frame 9 0 1 intermediate-fragment)
             (h2-frame 9 4 1 final-fragment)
             (h2-frame 0 1 1 body)
             (h2-frame 1 4 3
                       (h2-header-block (cons ":status" "201")
                                        (cons "content-length" "1")))
             (h2-frame 0 1 3 (octets 9)))))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (first-response
           (http-kit/http2:send-http2-request-over-connection
            connection
            (make-http-request :method "GET" :uri "https://127.0.0.1/first")))
         (second-response
           (http-kit/http2:send-http2-request-over-connection
            connection
            (make-http-request :method "GET" :uri "https://127.0.0.1/second"))))
    (ensure-true (plusp (length first-fragment)))
    (ensure-true (plusp (length intermediate-fragment)))
    (ensure-true (plusp (length final-fragment)))
    (ensure-equal 200 (http-response-status first-response))
    (ensure-equal "reassembled"
                  (http-header-value (http-response-headers first-response)
                                     "x-continuation"))
    (ensure-equal body (http-response-body first-response))
    (ensure-true (http-kit/http2:http2-connection-open-p connection))
    (ensure-equal 201 (http-response-status second-response))
    (ensure-equal (octets 9) (http-response-body second-response))
    (ensure-true (http-kit/http2:http2-connection-open-p connection))
    (ensure-true (http-kit/http2:http2-connection-session-started-p connection))
    (ensure-equal '(1 3)
                  (mapcar #'http-kit/http2::%h2-frame-stream-id
                          (remove-if-not
                           (lambda (frame)
                             (= 1 (http-kit/http2::%h2-frame-type frame)))
                           (h2-output-frames stream))))
    (ensure-equal (length (binary-session-input stream))
                  (binary-session-input-position stream))
    (http-kit/http2:close-http2-connection connection)))
#+sbcl
(deftest http2-public-single-request-rejects-malformed-continuation-frame
  (let* ((header-block (h2-header-block (cons ":status" "200")))
         (split (max 1 (floor (length header-block) 2)))
         (first-fragment (subseq header-block 0 split))
         (last-fragment (subseq header-block split)))
    (ensure-true (plusp (length first-fragment)))
    (ensure-true (plusp (length last-fragment)))
    (dolist (scenario '(:wrong-stream :wrong-type :invalid-flags))
      (let* ((bad-frame
               (case scenario
                 (:wrong-stream (h2-frame 9 4 3 last-fragment))
                 (:wrong-type (h2-frame 0 0 1 last-fragment))
                 (:invalid-flags (h2-frame 9 1 1 last-fragment))))
             (expected-detail
               (case scenario
                 (:wrong-stream '(9 3 1))
                 (:wrong-type '(0 1 1))
                 (:invalid-flags 1)))
             (stream
               (make-instance
                'binary-session-stream
                :input
                (concatenate-octets
                 (h2-frame 4 0 0 (octets))
                 (h2-frame 1 0 1 first-fragment)
                 bad-frame)))
             (closed 0)
             (connection
               (http-kit/http2:make-http2-connection
                :stream stream
                :close-stream (lambda (closed-stream)
                                (ensure-true (eq stream closed-stream))
                                (incf closed))))
             (result
               (handler-case
                   (http-kit/http2:send-http2-request-over-connection
                    connection
                    (make-http-request :method "GET"
                                       :uri "https://127.0.0.1/malformed"))
                 (http-protocol-error (condition) condition))))
        (unwind-protect
             (progn
               (ensure-true (typep result 'http-protocol-error))
               (ensure-equal :http2-headers (http-error-operation result))
               (ensure-equal expected-detail
                             (http-protocol-error-detail result))
               (ensure-true (not (http-kit/http2:http2-connection-open-p
                                  connection)))
               (ensure-equal 1 closed)
               (ensure-equal (length (binary-session-input stream))
                             (binary-session-input-position stream))
               (ensure-equal '(1)
                             (mapcar #'http-kit/http2::%h2-frame-stream-id
                                     (remove-if-not
                                      (lambda (frame)
                                        (= 1 (http-kit/http2::%h2-frame-type frame)))
                                      (h2-output-frames stream)))))
          (http-kit/http2:close-http2-connection connection))))))
#+sbcl
(progn
  (defclass h2-concurrent-start-stream-wrapper
      (sb-gray:fundamental-binary-input-stream
       sb-gray:fundamental-binary-output-stream)
    ((delegate :initarg :delegate :reader h2-concurrent-start-stream-delegate)
     (gate-lock
       :initform (sb-thread:make-mutex :name "HTTP/2 concurrent start gate")
       :reader h2-concurrent-start-stream-gate-lock)
     (first-write-seen-p
       :initform nil :accessor h2-concurrent-start-stream-first-write-seen-p)
     (first-write-entered
       :initform (sb-thread:make-semaphore :count 0)
       :reader h2-concurrent-start-stream-first-write-entered)
     (release-first-write
       :initform (sb-thread:make-semaphore :count 0)
       :reader h2-concurrent-start-stream-release-first-write)
     (startup-write-completed
       :initform (sb-thread:make-semaphore :count 0)
       :reader h2-concurrent-start-stream-startup-write-completed)))

  (defmethod sb-gray:stream-read-sequence
      ((stream h2-concurrent-start-stream-wrapper) sequence
       &optional (start 0) end)
    (sb-gray:stream-read-sequence
     (h2-concurrent-start-stream-delegate stream) sequence start end))

  (defmethod sb-gray:stream-write-sequence
      ((stream h2-concurrent-start-stream-wrapper) sequence
       &optional (start 0) end)
    (let ((first-write-p nil)
          (gate-lock (h2-concurrent-start-stream-gate-lock stream)))
      (sb-thread:with-mutex (gate-lock)
        (unless (h2-concurrent-start-stream-first-write-seen-p stream)
          (setf (h2-concurrent-start-stream-first-write-seen-p stream) t
                first-write-p t)))
      (when first-write-p
        (sb-thread:signal-semaphore
         (h2-concurrent-start-stream-first-write-entered stream))
        (sb-thread:wait-on-semaphore
         (h2-concurrent-start-stream-release-first-write stream)))
      (let ((result
              (sb-gray:stream-write-sequence
               (h2-concurrent-start-stream-delegate stream)
               sequence start end)))
        (when first-write-p
          (sb-thread:signal-semaphore
           (h2-concurrent-start-stream-startup-write-completed stream)))
        result)))

  (defmethod sb-gray:stream-finish-output
      ((stream h2-concurrent-start-stream-wrapper))
    (sb-gray:stream-finish-output
     (h2-concurrent-start-stream-delegate stream)))

  (deftest http2-public-concurrent-extended-connect-start-is-shared
    (let* ((peer (make-instance 'h2-manager-capacity-peer-stream
                                :input (octets)
                                :response-end-stream-p t))
           (stream
             (make-instance 'h2-concurrent-start-stream-wrapper
                            :delegate peer))
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream
              :close-stream
              (lambda (closed-stream)
                (ensure-true (eq stream closed-stream))
                (h2-manager-capacity-peer-close peer))))
           (first-entered (sb-thread:make-semaphore :count 0))
           (second-entered (sb-thread:make-semaphore :count 0))
           (first-done (sb-thread:make-semaphore :count 0))
           (second-done (sb-thread:make-semaphore :count 0))
           (first-response nil)
           (second-response nil)
           (first-failure nil)
           (second-failure nil)
           (first-thread nil)
           (second-thread nil)
           (release-signaled-p nil))
      (unwind-protect
           (progn
             (setf first-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (sb-thread:signal-semaphore first-entered)
                      (unwind-protect
                           (handler-case
                               (setf first-response
                                     (sb-ext:with-timeout 10
                                       (http-kit/http2:send-http2-request-over-connection
                                        connection
                                        (make-http-request
                                         :method "CONNECT" :protocol "websocket"
                                         :uri "https://concurrent-start.test/first")
                                        :timeout 5)))
                             (error (condition)
                               (setf first-failure condition)))
                        (sb-thread:signal-semaphore first-done)))))
             (ensure-true (sb-thread:wait-on-semaphore first-entered :timeout 5))
             (ensure-true
              (sb-thread:wait-on-semaphore
               (h2-concurrent-start-stream-first-write-entered stream)
               :timeout 5))
             (ensure-equal
              :starting
              (http-kit/http2::%http2-connection-session-start-state connection))
             (setf second-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (sb-thread:signal-semaphore second-entered)
                      (unwind-protect
                           (handler-case
                               (setf second-response
                                     (sb-ext:with-timeout 10
                                       (http-kit/http2:send-http2-request-over-connection
                                        connection
                                        (make-http-request
                                         :method "CONNECT" :protocol "websocket"
                                         :uri "https://concurrent-start.test/second")
                                        :timeout 5)))
                             (error (condition)
                               (setf second-failure condition)))
                        (sb-thread:signal-semaphore second-done)))))
             (ensure-true (sb-thread:wait-on-semaphore second-entered :timeout 5))
             (ensure-true
              (sb-thread:wait-on-semaphore
               (h2-manager-capacity-peer-input-read-waiting peer) :timeout 5))
             (ensure-equal
              :starting
              (http-kit/http2::%http2-connection-session-start-state connection))
             (let ((output-lock
                     (h2-manager-capacity-peer-output-lock peer)))
               (ensure-equal
                0
                (sb-thread:with-mutex (output-lock)
                  (length (binary-session-output peer)))))
             (ensure-equal 0 (sb-thread:semaphore-count first-done))
             (ensure-equal 0 (sb-thread:semaphore-count second-done))
             (ensure-false first-response)
             (ensure-false second-response)
             (setf release-signaled-p t)
             (sb-thread:signal-semaphore
              (h2-concurrent-start-stream-release-first-write stream))
             (ensure-true
              (sb-thread:wait-on-semaphore
               (h2-concurrent-start-stream-startup-write-completed stream)
               :timeout 5))
             (h2-manager-capacity-peer-feed
              peer (h2-frame 4 0 0 (octets 0 8 0 0 0 1)))
             (ensure-true
              (sb-thread:wait-on-semaphore
               (h2-manager-capacity-peer-header-available peer) :timeout 5))
             (ensure-true
              (sb-thread:wait-on-semaphore
               (h2-manager-capacity-peer-header-available peer) :timeout 5))
             (ensure-true (sb-thread:wait-on-semaphore first-done :timeout 5))
             (ensure-true (sb-thread:wait-on-semaphore second-done :timeout 5))
             (sb-thread:join-thread first-thread)
             (setf first-thread nil)
             (sb-thread:join-thread second-thread)
             (setf second-thread nil)
             (ensure-false first-failure)
             (ensure-false second-failure)
             (ensure-equal '(200 201)
                           (sort (list (http-response-status first-response)
                                       (http-response-status second-response))
                                 #'<))
             (ensure-equal (octets) (http-response-body first-response))
             (ensure-equal (octets) (http-response-body second-response))
             (let ((output-lock (h2-manager-capacity-peer-output-lock peer)))
               (sb-thread:with-mutex (output-lock)
                 (let* ((preface (h2-preface))
                        (output (binary-session-output peer))
                        (frames (h2-output-frames peer))
                        (headers
                          (remove-if-not
                           (lambda (frame)
                             (= 1 (http-kit/http2::%h2-frame-type frame)))
                           frames))
                        (settings
                          (remove-if-not
                           (lambda (frame)
                             (and (= 4 (http-kit/http2::%h2-frame-type frame))
                                  (zerop (http-kit/http2::%h2-frame-flags frame))
                                  (zerop (http-kit/http2::%h2-frame-stream-id frame))))
                           frames)))
                   (ensure-equal preface (subseq output 0 (length preface)))
                   (ensure-false (search preface output :start2 (length preface)))
                   (ensure-equal 1 (length settings))
                   (ensure-equal 2 (length headers))))
               (ensure-equal '(1 3)
                             (sort
                              (h2-manager-capacity-peer-output-header-ids peer)
                              #'<)))
             (ensure-true (http-kit/http2:http2-connection-open-p connection))
             (ensure-true
              (http-kit/http2:http2-connection-session-started-p connection))
             (ensure-equal
              :ready
              (http-kit/http2::%http2-connection-session-start-state connection)))
        (unless release-signaled-p
          (setf release-signaled-p t)
          (sb-thread:signal-semaphore
           (h2-concurrent-start-stream-release-first-write stream)))
        (h2-manager-capacity-peer-close peer)
        (http-kit/http2:close-http2-connection connection)
        (dolist (thread (list first-thread second-thread))
          (when thread
            (sb-thread:join-thread thread)))))))

#+sbcl
(deftest http2-public-control-priority-wire-boundaries
  (let* ((stream (make-instance 'binary-session-stream
                                :input (h2-response-wire (octets))))
         (connection (http-kit/http2:make-http2-connection :stream stream)))
    (http-kit/http2:send-http2-request-over-connection
     connection (make-http-request :method "GET" :uri "https://127.0.0.1/"))
    (let ((before (copy-seq (binary-session-output stream))))
      (dolist (value (list nil 3 (string (code-char 127))
                          (string (code-char 128))
                          (make-string 16381 :initial-element #\a)))
        (signals http-protocol-error
          (http-kit/http2:send-http2-priority-update
           connection 1 :priority-field-value value))
        (ensure-equal before (binary-session-output stream))
        (ensure-true (http-kit/http2:http2-connection-open-p connection)))
      (dolist (id (list -1 #x80000000 1/2))
        (signals http-protocol-error
          (http-kit/http2:send-http2-priority-update connection id))
        (ensure-equal before (binary-session-output stream))))
    (let ((value (make-string 16380 :initial-element #\a)))
      (setf (char value 0) #\Tab)
      (http-kit/http2:send-http2-priority-update
       connection #x7fffffff :priority-field-value value)
      (let* ((frame (car (last (h2-output-frames stream))))
             (payload (http-kit/http2::%h2-frame-payload frame)))
        (ensure-equal http-kit/http2::+http2-priority-update-type+
                      (http-kit/http2::%h2-frame-type frame))
        (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id frame))
        (ensure-equal 16384 (length payload))
        (ensure-equal #x7fffffff (http-kit/http2::%h2-u32 payload 0))
        (ensure-equal (http-kit::%string-octets value) (subseq payload 4))))
    (http-kit/http2:close-http2-connection connection)
    (signals http-protocol-error
      (http-kit/http2:send-http2-priority-update connection 1))))

#+sbcl
(deftest http2-public-control-ping-interleaved-frames
  (let* ((payload (octets 1 2 3 4 5 6 7 8))
         (peer-payload (octets 8 7 6 5 4 3 2 1))
         (stream
           (make-instance
            'binary-session-stream
            :input (concatenate-octets
                    (h2-response-wire (octets))
                    (h2-frame 6 1 0 peer-payload)
                    (h2-frame 6 0 0 peer-payload)
                    (h2-frame 4 0 0 (octets 0 3 0 0 0 7))
                    (h2-frame 6 1 0 payload))))
         (connection (http-kit/http2:make-http2-connection :stream stream)))
    (http-kit/http2:send-http2-request-over-connection
     connection (make-http-request :method "GET" :uri "https://127.0.0.1/"))
    (let ((offset (length (binary-session-output stream))))
      (ensure-equal payload
                    (http-kit/http2:ping-http2-connection
                     connection :payload payload))
      (let* ((reader (http-kit/http2::%h2-reader-for
                      (subseq (binary-session-output stream) offset)))
             (ping (http-kit/http2::%h2-read-frame reader 16384 nil nil))
             (ack (http-kit/http2::%h2-read-frame reader 16384 nil nil))
             (settings (http-kit/http2::%h2-read-frame reader 16384 nil nil)))
        (ensure-equal 6 (http-kit/http2::%h2-frame-type ping))
        (ensure-equal 0 (http-kit/http2::%h2-frame-flags ping))
        (ensure-equal payload (http-kit/http2::%h2-frame-payload ping))
        (ensure-equal 6 (http-kit/http2::%h2-frame-type ack))
        (ensure-equal 1 (http-kit/http2::%h2-frame-flags ack))
        (ensure-equal peer-payload (http-kit/http2::%h2-frame-payload ack))
        (ensure-equal 4 (http-kit/http2::%h2-frame-type settings))
        (ensure-equal 1 (http-kit/http2::%h2-frame-flags settings))
        (ensure-equal :eof (http-kit/http2::%h2-read-frame reader 16384 nil nil))))
    (ensure-equal 7
                  (http-kit/http2:http2-connection-peer-max-concurrent-streams
                   connection))
    (ensure-true (http-kit/http2:http2-connection-open-p connection))
    (http-kit/http2:close-http2-connection connection)))

#+sbcl
(deftest http2-public-control-goaway-after-close
  (let* ((close-count 0)
         (stream (make-instance 'binary-session-stream
                                :input (h2-response-wire (octets))))
         (connection
           (http-kit/http2:make-http2-connection
            :stream stream :close-stream (lambda (closed-stream)
                                          (ensure-true (eq stream closed-stream))
                                          (incf close-count)))))
    (http-kit/http2:send-http2-request-over-connection
     connection (make-http-request :method "GET" :uri "https://127.0.0.1/"))
    (http-kit/http2:close-http2-connection connection)
    (let ((before (copy-seq (binary-session-output stream))))
      (signals http-protocol-error (http-kit/http2:send-http2-goaway connection))
      (ensure-equal before (binary-session-output stream))
      (ensure-true (not (http-kit/http2:http2-connection-draining-p connection)))
      (ensure-true
       (null (http-kit/http2:http2-connection-local-goaway-last-stream-id connection))))
    (ensure-equal 1 close-count)))

#+sbcl
(deftest http2-public-control-ping-after-close
  (let* ((close-count 0)
         (payload (octets 1 2 3 4 5 6 7 8))
         (stream (make-instance
                  'binary-session-stream
                  :input (concatenate-octets (h2-response-wire (octets))
                                            (h2-frame 6 1 0 payload))))
         (connection
           (http-kit/http2:make-http2-connection
            :stream stream :close-stream (lambda (closed-stream)
                                          (ensure-true (eq stream closed-stream))
                                          (incf close-count)))))
    (http-kit/http2:send-http2-request-over-connection
     connection (make-http-request :method "GET" :uri "https://127.0.0.1/"))
    (http-kit/http2:close-http2-connection connection)
    (let ((before (copy-seq (binary-session-output stream)))
          (input-position (binary-session-input-position stream)))
      (signals http-protocol-error
        (http-kit/http2:ping-http2-connection connection :payload payload))
      (ensure-equal before (binary-session-output stream))
      (ensure-equal input-position (binary-session-input-position stream)))
    (ensure-equal 1 close-count)))

#+sbcl
(deftest http2-public-control-goaway-respects-peer-frame-limit
  (let* ((stream (make-instance 'binary-session-stream
                                :input (h2-response-wire (octets))))
         (connection (http-kit/http2:make-http2-connection
                      :stream stream :max-frame-size 32768)))
    (http-kit/http2:send-http2-request-over-connection
     connection (make-http-request :method "GET" :uri "https://127.0.0.1/"))
    (ensure-equal 16384
                  (http-kit/http2:http2-connection-peer-max-frame-size connection))
    (let ((before (copy-seq (binary-session-output stream))))
      (signals http-protocol-error
        (http-kit/http2:send-http2-goaway
         connection :debug-data (make-array 16377 :element-type '(unsigned-byte 8)
                                                  :initial-element #x5a)))
      (ensure-equal before (binary-session-output stream))
      (ensure-true (http-kit/http2:http2-connection-open-p connection))
      (ensure-true (not (http-kit/http2:http2-connection-draining-p connection)))
      (ensure-true
       (null (http-kit/http2:http2-connection-local-goaway-last-stream-id connection))))
    (http-kit/http2:close-http2-connection connection)))

#+sbcl
(deftest http2-public-control-goaway-uses-negotiated-peer-frame-limit
  (let* ((stream (make-instance
                  'binary-session-stream
                  :input (concatenate-octets
                          (h2-frame 4 0 0 (octets 0 5 0 0 128 0))
                          (h2-frame 1 5 1 (octets #x88)))))
         (connection (http-kit/http2:make-http2-connection :stream stream)))
    (http-kit/http2:send-http2-request-over-connection
     connection (make-http-request :method "GET" :uri "https://127.0.0.1/"))
    (ensure-equal 32768
                  (http-kit/http2:http2-connection-peer-max-frame-size connection))
    (http-kit/http2:send-http2-goaway
     connection :last-stream-id 0 :error-code 0
     :debug-data (make-array 32760 :element-type '(unsigned-byte 8)
                                   :initial-element #x5a))
    (let ((reader (http-kit/http2::%h2-reader-for
                   (subseq (binary-session-output stream) (length (h2-preface)))))
          (last-frame nil))
      (loop for frame = (http-kit/http2::%h2-read-frame reader 32768 nil nil)
            until (eq frame :eof)
            do (setf last-frame frame))
      (ensure-equal 7 (http-kit/http2::%h2-frame-type last-frame))
      (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id last-frame))
      (ensure-equal 32768 (http-kit/http2::%h2-frame-length last-frame))
      (ensure-equal 0 (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload last-frame) 0))
      (ensure-equal 0 (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload last-frame) 4))
      (ensure-true (every (lambda (byte) (= byte #x5a))
                          (subseq (http-kit/http2::%h2-frame-payload last-frame) 8))))
    (ensure-true (http-kit/http2:http2-connection-draining-p connection))
    (http-kit/http2:close-http2-connection connection)))

#+sbcl
(deftest http2-public-runtime-boundary-producer-declared-length
  (dolist (case '((1 2 2 (1 2)) (2 1 1 (2 1)) (1 1 1 nil)))
    (destructuring-bind (size declared calls detail) case
      (let* ((body (make-array size :element-type '(unsigned-byte 8)
                                   :initial-element #x5a))
             (stream (make-instance 'binary-session-stream
                                    :input (h2-response-wire (octets))))
             (closed 0)
             (connection (http-kit/http2:make-http2-connection
                          :stream stream :close-stream
                          (lambda (closed-stream)
                            (ensure-true (eq stream closed-stream))
                            (incf closed)))))
        (multiple-value-bind (producer cursor call-count)
            (make-octet-body-producer body)
          (let ((result
                  (handler-case
                      (http-kit/http2:send-http2-request-over-connection
                       connection
                       (make-http-request :method "POST" :uri "https://127.0.0.1/upload")
                       :request-body-function producer :request-body-length declared)
                    (http-protocol-error (condition) condition))))
            (ensure-equal size (funcall cursor))
            (ensure-equal calls (funcall call-count))
            (if detail
                (progn
                  (ensure-true (typep result 'http-protocol-error))
                  (ensure-equal :http2-write (http-error-operation result))
                  (ensure-equal detail (http-protocol-error-detail result))
                  (ensure-true (not (http-kit/http2:http2-connection-open-p connection)))
                  (ensure-equal 1 closed))
                (progn
                  (ensure-equal 200 (http-response-status result))
                  (ensure-true (http-kit/http2:http2-connection-open-p connection))
                  (ensure-equal 0 closed)))
            (multiple-value-bind (sent count) (h2-output-data-summary stream)
              (ensure-equal (if (and detail (> size declared)) (octets) body) sent)
              (ensure-equal (if (and detail (> size declared)) 0 1) count))
            (let ((data (remove-if-not
                         (lambda (frame) (= 0 (http-kit/http2::%h2-frame-type frame)))
                         (h2-output-frames stream))))
              (ensure-equal (if detail 0 1)
                            (count-if (lambda (frame)
                                        (logbitp 0 (http-kit/http2::%h2-frame-flags frame)))
                                      data)))
            (http-kit/http2:close-http2-connection connection)
            (ensure-equal 1 closed)))))))

#+sbcl
(deftest http2-public-runtime-boundary-upload-credit-eof
  (dolist (replenish '(nil t))
    (let* ((body (make-array 65536 :element-type '(unsigned-byte 8)
                                  :initial-element #x5a))
           (stream (make-instance
                    'binary-session-stream
                    :input (concatenate-octets
                            (h2-frame 4 0 0 (octets))
                            (if replenish
                                (concatenate-octets
                                 (h2-frame 8 0 1 (octets 0 0 0 1))
                                 (h2-frame 8 0 0 (octets 0 0 0 1))
                                 (h2-frame 1 5 1 (octets #x88)))
                                (octets)))))
           (closed 0)
           (connection (http-kit/http2:make-http2-connection
                        :stream stream :close-stream
                        (lambda (closed-stream)
                          (ensure-true (eq stream closed-stream))
                          (incf closed))))
           (result
             (handler-case
                 (http-kit/http2:send-http2-request-over-connection
                  connection (make-http-request :method "POST"
                                                :uri "https://127.0.0.1/upload"
                                                :body body))
               (http-connection-error (condition) condition))))
      (if replenish
          (progn
            (ensure-equal 200 (http-response-status result))
            (ensure-equal 0 closed))
          (progn
            (ensure-true (typep result 'http-connection-error))
            (ensure-equal :http2-read (http-error-operation result))
            (ensure-equal :eof (http-connection-error-cause result))
            (ensure-equal 1 closed)))
      (multiple-value-bind (sent count ack-count) (h2-output-data-summary stream)
        (ensure-equal (subseq body 0 (if replenish 65536 65535)) sent)
        (ensure-equal (if replenish 5 4) count)
        (ensure-equal 1 ack-count))
      (let ((data (remove-if-not
                   (lambda (frame) (= 0 (http-kit/http2::%h2-frame-type frame)))
                   (h2-output-frames stream))))
        (ensure-equal (if replenish '(16384 16384 16384 16383 1)
                                      '(16384 16384 16384 16383))
                      (mapcar #'http-kit/http2::%h2-frame-length data))
        (ensure-equal (if replenish 1 0)
                      (count-if (lambda (frame)
                                  (logbitp 0 (http-kit/http2::%h2-frame-flags frame))) data)))
      (ensure-equal (length (binary-session-input stream))
                    (binary-session-input-position stream))
      (ensure-equal replenish (http-kit/http2:http2-connection-open-p connection))
      (http-kit/http2:close-http2-connection connection)
      (ensure-equal 1 closed))))

#+sbcl
(deftest http2-public-runtime-boundary-zero-concurrency-read
  (dolist (mode '(:eof :unexpected-data :available))
    (let* ((stream (make-instance
                    'binary-session-stream
                    :input (concatenate-octets
                            (h2-frame 4 0 0 (octets 0 3 0 0 0 0))
                            (h2-frame 1 5 1 (octets #x88))
                            (ecase mode
                              (:eof (octets))
                              (:unexpected-data (h2-frame 0 0 3 (octets 9)))
                              (:available
                               (concatenate-octets
                                (h2-frame 4 0 0 (octets 0 3 0 0 0 1))
                                (h2-frame 1 5 3 (octets #x88))))))))
           (closed 0)
           (connection (http-kit/http2:make-http2-connection
                        :stream stream :close-stream
                        (lambda (closed-stream)
                          (ensure-true (eq stream closed-stream))
                          (incf closed))))
           (request (make-http-request :method "GET" :uri "https://127.0.0.1/")))
      (ensure-equal 200 (http-response-status
                         (http-kit/http2:send-http2-request-over-connection connection request)))
      (ensure-equal 0 (http-kit/http2:http2-connection-peer-max-concurrent-streams connection))
      (let ((result (handler-case
                        (http-kit/http2:send-http2-request-over-connection connection request)
                      (http-connection-error (condition) condition)
                      (http-protocol-error (condition) condition))))
        (ecase mode
          (:available
           (ensure-equal 200 (http-response-status result))
           (ensure-equal 1 (http-kit/http2:http2-connection-peer-max-concurrent-streams connection)))
          (:eof
           (ensure-true (typep result 'http-connection-error))
           (ensure-equal :http2-read (http-error-operation result))
           (ensure-equal :eof (http-connection-error-cause result)))
          (:unexpected-data
           (ensure-true (typep result 'http-protocol-error))
           (ensure-equal :http2-read (http-error-operation result))
           (ensure-equal '(0 3) (http-protocol-error-detail result)))))
      (ensure-equal (if (eq mode :available) '(1 3) '(1))
                    (mapcar #'http-kit/http2::%h2-frame-stream-id
                            (remove-if-not
                             (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                             (h2-output-frames stream))))
      (ensure-equal (if (eq mode :available) 0 1) closed)
      (ensure-equal (eq mode :available) (http-kit/http2:http2-connection-open-p connection))
      (ensure-equal (length (binary-session-input stream))
                    (binary-session-input-position stream))
      (http-kit/http2:close-http2-connection connection)
      (ensure-equal 1 closed))))

#+sbcl
(deftest http2-public-runtime-boundary-ping-ack-read
  (dolist (mode '(:eof :unexpected-data :ack))
    (let* ((payload (octets 1 2 3 4 5 6 7 8))
           (stream (make-instance
                    'binary-session-stream
                    :input (concatenate-octets
                            (h2-response-wire (octets))
                            (ecase mode
                              (:eof (octets))
                              (:unexpected-data (h2-frame 0 0 1 (octets 9)))
                              (:ack (h2-frame 6 1 0 payload))))))
           (closed 0)
           (connection (http-kit/http2:make-http2-connection
                          :stream stream :close-stream
                          (lambda (closed-stream)
                            (ensure-true (eq stream closed-stream))
                            (incf closed)))))
      (ensure-equal 200 (http-response-status
                         (http-kit/http2:send-http2-request-over-connection
                          connection (make-http-request :method "GET" :uri "https://127.0.0.1/"))))
      (let ((result (handler-case
                        (http-kit/http2:ping-http2-connection connection :payload payload)
                      (http-protocol-error (condition) condition))))
        (if (eq mode :ack)
            (ensure-equal payload result)
            (progn
              (ensure-true (typep result 'http-protocol-error))
              (ensure-equal :http2-client (http-error-operation result))
              (if (eq mode :eof)
                  (ensure-equal :eof (http-protocol-error-detail result))
                  (let ((frame (http-protocol-error-detail result)))
                    (ensure-equal 0 (http-kit/http2::%h2-frame-type frame))
                    (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id frame))
                    (ensure-equal (octets 9) (http-kit/http2::%h2-frame-payload frame)))))))
      (let ((pings (remove-if-not
                    (lambda (frame) (= 6 (http-kit/http2::%h2-frame-type frame)))
                    (h2-output-frames stream))))
        (ensure-equal 1 (length pings))
        (ensure-equal payload (http-kit/http2::%h2-frame-payload (first pings)))
        (ensure-equal 0 (http-kit/http2::%h2-frame-flags (first pings)))
        (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id (first pings))))
      (ensure-equal (length (binary-session-input stream))
                    (binary-session-input-position stream))
      (ensure-equal (if (eq mode :ack) 0 1) closed)
      (ensure-equal (eq mode :ack) (http-kit/http2:http2-connection-open-p connection))
      (http-kit/http2:close-http2-connection connection)
      (ensure-equal 1 closed))))

#+sbcl
(progn
  (defun scheduled-ping-wire-p (sequence start end)
    ;; A PING frame has nine bytes of header and eight bytes of payload.
    ;; Offsets are zero-based: type is at 3, flags at 4, stream ID starts at 5.
    (and (= (- end start) 17)
         (= (aref sequence (+ start 3))
            http-kit/http2::+http2-ping-type+)
         (zerop (aref sequence (+ start 4)))
         (zerop (http-kit/http2::%h2-u32 sequence (+ start 5)))))

  (defclass scheduled-ping-eof-stream (binary-session-stream)
    ((eof-started
       :initarg :eof-started
       :reader scheduled-ping-eof-started)
     (eof-release
       :initarg :eof-release
       :reader scheduled-ping-eof-release)
     (ping-write-started
       :initarg :ping-write-started
       :reader scheduled-ping-write-started)
     (ping-write-release
       :initarg :ping-write-release
       :reader scheduled-ping-write-release)
     (ping-write-failed
       :initarg :ping-write-failed
       :reader scheduled-ping-write-failed)
     (ping-written
       :initarg :ping-written
       :reader scheduled-ping-written)
     (defer-ping-write-p
       :initarg :defer-ping-write-p
       :reader scheduled-ping-defer-write-p)
     (fail-ping-write-p
       :initarg :fail-ping-write-p
       :reader scheduled-ping-fail-write-p)
     (closed-p
       :initform nil
       :accessor scheduled-ping-stream-closed-p)))

  (defmethod sb-gray:stream-read-sequence
      ((stream scheduled-ping-eof-stream) sequence &optional (start 0) end)
    (declare (ignore start end))
    (when (>= (binary-session-input-position stream)
              (length (binary-session-input stream)))
      (sb-thread:signal-semaphore (scheduled-ping-eof-started stream))
      (sb-thread:wait-on-semaphore (scheduled-ping-eof-release stream)))
    (call-next-method))

  (defmethod sb-gray:stream-write-sequence
      ((stream scheduled-ping-eof-stream) sequence &optional (start 0) end)
    (let ((end (or end (length sequence))))
      (if (scheduled-ping-wire-p sequence start end)
          (progn
            (sb-thread:signal-semaphore
             (scheduled-ping-write-started stream))
            (when (scheduled-ping-defer-write-p stream)
              (sb-thread:wait-on-semaphore
               (scheduled-ping-write-release stream))
              (when (scheduled-ping-fail-write-p stream)
                (sb-thread:signal-semaphore
                 (scheduled-ping-write-failed stream))
                (error 'http-connection-error
                       :message "The test stream failed the PING write."
                       :operation :http2-write
                       :cause :test-write-failure))
              (when (scheduled-ping-stream-closed-p stream)
                (error 'http-connection-error
                       :message "The test stream closed before the PING write."
                       :operation :http2-write
                       :cause :closed)))
            (prog1 (call-next-method)
              (sb-thread:signal-semaphore (scheduled-ping-written stream))))
          (call-next-method)))))

#+sbcl
(deftest http2-scheduler-ping-reader-eof-handoff
  (dolist (mode '(:ping-written-before-eof
                  :eof-recorded-before-ping-write-finishes
                  :ping-write-fails-before-reader-eof))
    (let* ((payload (octets 1 2 3 4 5 6 7 8))
           (eof-started (sb-thread:make-semaphore :count 0))
           (eof-release (sb-thread:make-semaphore :count 0))
           (ping-write-started (sb-thread:make-semaphore :count 0))
           (ping-write-release (sb-thread:make-semaphore :count 0))
           (ping-write-failed (sb-thread:make-semaphore :count 0))
           (ping-written (sb-thread:make-semaphore :count 0))
           (stream
             (make-instance 'scheduled-ping-eof-stream
                            :input (h2-response-wire (octets))
                            :eof-started eof-started
                            :eof-release eof-release
                            :ping-write-started ping-write-started
                            :ping-write-release ping-write-release
                            :ping-write-failed ping-write-failed
                            :ping-written ping-written
                            :defer-ping-write-p
                            (member mode
                                    '(:eof-recorded-before-ping-write-finishes
                                      :ping-write-fails-before-reader-eof))
                            :fail-ping-write-p
                            (eq mode :ping-write-fails-before-reader-eof)))
           (closed 0)
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream
              :close-stream
              (lambda (closed-stream)
                (ensure-true (eq stream closed-stream))
                (setf (scheduled-ping-stream-closed-p stream) t)
                (incf closed))))
           (request-thread nil)
           (ping-thread nil)
           (request-error nil)
           (ping-error nil)
           (eof-released-p nil)
           (ping-write-released-p nil))
      (unwind-protect
           (progn
             (ensure-equal
              200
              (http-response-status
               (http-kit/http2:send-http2-request-over-connection
                connection
                (make-http-request :method "GET"
                                   :uri "https://127.0.0.1/first"))))
             (setf request-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (http-kit/http2:send-http2-request-over-connection
                           connection
                           (make-http-request
                            :method "GET"
                            :uri "https://127.0.0.1/second"))
                        (error (condition)
                          (setf request-error condition))))))
             (ensure-true
              (sb-thread:wait-on-semaphore eof-started :timeout 5))
             (setf ping-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (http-kit/http2:ping-http2-connection
                           connection :payload payload)
                        (error (condition)
                          (setf ping-error condition))))))
             (ecase mode
               (:ping-written-before-eof
                (ensure-true
                 (sb-thread:wait-on-semaphore ping-written :timeout 5))
                (setf eof-released-p t)
                (sb-thread:signal-semaphore eof-release)
                (sb-thread:join-thread request-thread)
                (setf request-thread nil)
                (sb-thread:join-thread ping-thread)
                (setf ping-thread nil))
               (:eof-recorded-before-ping-write-finishes
                (ensure-true
                 (sb-thread:wait-on-semaphore ping-write-started :timeout 5))
                (setf eof-released-p t)
                (sb-thread:signal-semaphore eof-release)
                (sb-thread:join-thread request-thread)
                (setf request-thread nil)
                (setf ping-write-released-p t)
                (sb-thread:signal-semaphore ping-write-release)
                (sb-thread:join-thread ping-thread)
                (setf ping-thread nil))
               (:ping-write-fails-before-reader-eof
                (ensure-true
                 (sb-thread:wait-on-semaphore ping-write-started :timeout 5))
                (setf ping-write-released-p t)
                (sb-thread:signal-semaphore ping-write-release)
                (ensure-true
                 (sb-thread:wait-on-semaphore ping-write-failed :timeout 5))
                (sb-thread:join-thread ping-thread)
                (setf ping-thread nil)
                (setf eof-released-p t)
                (sb-thread:signal-semaphore eof-release)
                (sb-thread:join-thread request-thread)
                (setf request-thread nil)))
             (ensure-true (typep request-error 'http-connection-error))
             (ensure-equal
              (if (eq mode :ping-write-fails-before-reader-eof)
                  :http2-write
                  :http2-read)
              (http-error-operation request-error))
             (ensure-equal
              (if (eq mode :ping-write-fails-before-reader-eof)
                  :test-write-failure
                  :eof)
              (http-connection-error-cause request-error))
             (if (eq mode :ping-write-fails-before-reader-eof)
                 (progn
                   (ensure-true (typep ping-error 'http-connection-error))
                   (ensure-equal :http2-write (http-error-operation ping-error))
                   (ensure-equal :test-write-failure
                                 (http-connection-error-cause ping-error)))
                 (progn
                   (ensure-true (typep ping-error 'http-protocol-error))
                   (ensure-equal :http2-client (http-error-operation ping-error))
                   (ensure-equal :eof (http-protocol-error-detail ping-error))))
             (ensure-equal 1 closed)
             (ensure-true (not (http-kit/http2:http2-connection-open-p connection)))
             (ensure-equal
              (if (eq mode :ping-written-before-eof) 1 0)
              (count-if
               (lambda (frame)
                 (and (= 6 (http-kit/http2::%h2-frame-type frame))
                      (zerop (http-kit/http2::%h2-frame-flags frame))))
               (h2-output-frames stream))))
        (unless eof-released-p
          (setf eof-released-p t)
          (sb-thread:signal-semaphore eof-release))
        (unless ping-write-released-p
          (setf ping-write-released-p t)
          (sb-thread:signal-semaphore ping-write-release))
        (when request-thread
          (sb-thread:join-thread request-thread))
        (when ping-thread
          (sb-thread:join-thread ping-thread))
        (http-kit/http2:close-http2-connection connection)))))

#+sbcl
(deftest http2-public-runtime-boundary-batch-completed-stream
  (dolist (mode '(:headers :data :other-stream))
    (let* ((requests (list (make-http-request :method "GET" :uri "https://127.0.0.1/one")
                           (make-http-request :method "GET" :uri "https://127.0.0.1/two")))
           (stream (make-instance
                    'binary-session-stream
                    :input (concatenate-octets
                            (h2-frame 4 0 0 (octets))
                            (h2-frame 1 5 1 (octets #x88))
                            (ecase mode
                              (:headers (h2-frame 1 5 1 (octets #x88)))
                              (:data (h2-frame 0 1 1 (octets 9)))
                              (:other-stream
                               (concatenate-octets
                                (h2-frame 1 4 3 (h2-header-block (cons ":status" "201")))
                                (h2-frame 0 1 3 (octets 9))))))))
           (closed 0)
           (events nil)
           (connection (http-kit/http2:make-http2-connection
                        :stream stream :close-stream
                        (lambda (closed-stream)
                          (ensure-true (eq stream closed-stream))
                          (incf closed))))
           (result (handler-case
                       (http-kit/http2:send-http2-requests-over-connection
                        connection requests :on-body-chunk
                        (lambda (chunk request) (push (list (copy-seq chunk) request) events)))
                     (http-protocol-error (condition) condition))))
      (if (eq mode :other-stream)
          (progn
            (ensure-equal '(200 201) (mapcar #'http-response-status result))
            (ensure-equal (octets) (http-response-body (first result)))
            (ensure-equal (octets 9) (http-response-body (second result)))
            (ensure-equal 1 (length events))
            (ensure-equal (octets 9) (first (first events)))
            (ensure-true (eq (second requests) (second (first events))))
            (ensure-equal 0 closed))
          (progn
            (ensure-true (typep result 'http-protocol-error))
            (ensure-equal :http2-read (http-error-operation result))
            (ensure-equal 1 (http-protocol-error-detail result))
            (ensure-equal nil events)
            (ensure-equal 1 closed)))
      (ensure-equal '(1 3)
                    (mapcar #'http-kit/http2::%h2-frame-stream-id
                            (remove-if-not
                             (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                             (h2-output-frames stream))))
      (ensure-equal (length (binary-session-input stream))
                    (binary-session-input-position stream))
      (http-kit/http2:close-http2-connection connection)
      (ensure-equal 1 closed))))
#+sbcl
(deftest http2-scheduler-unknown-data-active-entry
  (let* ((input
           (concatenate-octets
            (h2-frame 4 0 0 (octets))
            (h2-frame 0 0 3 (octets 9))
            (h2-frame 1 5 1
                      (h2-header-block (cons ":status" "200")))))
         (stream (make-instance 'binary-session-stream :input input))
         (closed 0)
         (connection
           (http-kit/http2:make-http2-connection
            :stream stream
            :close-stream
            (lambda (closed-stream)
              (ensure-true (eq stream closed-stream))
              (incf closed))))
         (result
           (handler-case
               (http-kit/http2:send-http2-request-over-connection
                connection
                (make-http-request :method "GET"
                                   :uri "https://127.0.0.1/active"))
             (http-protocol-error (condition) condition))))
    (ensure-true (typep result 'http-protocol-error))
    (ensure-equal :http2-read (http-error-operation result))
    (ensure-equal 3 (http-protocol-error-detail result))
    (ensure-equal 1 closed)
    (http-kit/http2:close-http2-connection connection)
    (ensure-equal 1 closed)))

#+sbcl
(deftest http2-public-runtime-boundary-batch-goaway-last-stream
  (dolist (last-stream-id '(1 3))
    (let* ((stream (make-instance
                    'binary-session-stream
                    :input (concatenate-octets
                            (h2-frame 4 0 0 (octets))
                            (h2-frame 1 5 1 (octets #x88))
                            (h2-frame 7 0 0 (octets 0 0 0 last-stream-id 0 0 0 0))
                            (if (= last-stream-id 3)
                                (h2-frame 1 5 3 (h2-header-block (cons ":status" "201")))
                                (octets)))))
           (closed 0)
           (connection (http-kit/http2:make-http2-connection
                        :stream stream :close-stream
                        (lambda (closed-stream)
                          (ensure-true (eq stream closed-stream))
                          (incf closed))))
           (result (handler-case
                       (http-kit/http2:send-http2-requests-over-connection
                        connection
                        (list (make-http-request :method "GET" :uri "https://127.0.0.1/one")
                              (make-http-request :method "GET" :uri "https://127.0.0.1/two")))
                     (http-connection-error (condition) condition))))
      (if (= last-stream-id 3)
          (progn
            (ensure-equal '(200 201) (mapcar #'http-response-status result))
            (ensure-equal 0 closed))
          (progn
            (ensure-true (typep result 'http-connection-error))
            (ensure-equal :http2-control (http-error-operation result))
            (ensure-equal '(:goaway 1 0) (http-connection-error-cause result))
            (ensure-equal 1 closed)))
      (ensure-equal last-stream-id
                    (http-kit/http2:http2-connection-goaway-last-stream-id connection))
      (ensure-equal '(1 3)
                    (mapcar #'http-kit/http2::%h2-frame-stream-id
                            (remove-if-not
                             (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                             (h2-output-frames stream))))
      (ensure-equal (length (binary-session-input stream))
                    (binary-session-input-position stream))
      (http-kit/http2:close-http2-connection connection)
      (ensure-equal 1 closed))))

#+sbcl
(deftest http2-public-batch-continuation-wire-errors
  (let* ((header-block (h2-header-block (cons ":status" "200")
                                         (cons "content-length" "0")))
         (split (floor (length header-block) 2))
         (first-fragment (subseq header-block 0 split))
         (last-fragment (subseq header-block split)))
    (ensure-true (plusp (length first-fragment)))
    (ensure-true (plusp (length last-fragment)))
    (dolist (scenario '(:complete :eof :other-stream :invalid-flag))
      (let* ((stream
               (make-instance
                'binary-session-stream
                :input
                (concatenate-octets
                 (h2-frame 4 0 0 (octets))
                 (h2-frame 1 1 1 first-fragment)
                 (case scenario
                   (:eof (octets))
                   (:other-stream (h2-frame 9 4 3 last-fragment))
                   (:invalid-flag (h2-frame 9 1 1 last-fragment))
                   (:complete (h2-frame 9 4 1 last-fragment))))))
             (closed 0)
             (body-calls 0)
             (connection
               (http-kit/http2:make-http2-connection
                :stream stream
                :close-stream (lambda (closed-stream)
                                (ensure-true (eq stream closed-stream))
                                (incf closed))))
             (result
               (handler-case
                   (http-kit/http2:send-http2-requests-over-connection
                    connection
                    (list (make-http-request :method "GET"
                                             :uri "https://127.0.0.1/"))
                    :on-body-chunk (lambda (chunk request)
                                     (declare (ignore chunk request))
                                     (incf body-calls)))
                 (http-protocol-error (condition) condition)
                 (http-connection-error (condition) condition))))
        (case scenario
          (:complete
           (ensure-equal 1 (length result))
           (ensure-equal 200 (http-response-status (first result)))
           (ensure-equal (octets) (http-response-body (first result)))
           (ensure-equal "0" (http-header-value
                              (http-response-headers (first result))
                              "content-length"))
           (ensure-true
            (http-kit/http2:http2-connection-session-started-p connection))
           (ensure-equal 0 closed))
          (:eof
           ;; The public batch reader signals EOF before the header-block reader.
           (ensure-true (typep result 'http-connection-error))
           (ensure-equal :http2-read (http-error-operation result))
           (ensure-equal :eof (http-connection-error-cause result))
           (ensure-equal 1 closed))
          (otherwise
           (ensure-true (typep result 'http-protocol-error))
           (ensure-equal :http2-headers (http-error-operation result))
           (ensure-equal (if (eq scenario :other-stream) '(9 3) 1)
                         (http-protocol-error-detail result))
           (ensure-equal 1 closed)))
        (ensure-equal 0 body-calls)
        (ensure-equal (length (binary-session-input stream))
                      (binary-session-input-position stream))
        (let ((frames (h2-output-frames stream)))
          (ensure-equal '(1)
                        (mapcar #'http-kit/http2::%h2-frame-stream-id
                                (remove-if-not
                                 (lambda (frame)
                                   (= 1 (http-kit/http2::%h2-frame-type frame)))
                                 frames)))
          (ensure-equal 1
                        (count-if
                         (lambda (frame)
                           (and (= 4 (http-kit/http2::%h2-frame-type frame))
                                (= 1 (http-kit/http2::%h2-frame-flags frame))))
                         frames)))
        (http-kit/http2:close-http2-connection connection)
        (ensure-equal 1 closed)))))

#+sbcl
(progn
  (defun check-public-batch-control-frame (frame &key operation detail cause)
    (let* ((stream
             (make-instance
              'binary-session-stream
              :input (concatenate
                      '(vector (unsigned-byte 8))
                      (h2-frame 4 0 0 (octets))
                      frame
                      (unless operation
                        (concatenate
                         '(vector (unsigned-byte 8))
                         (h2-frame 1 5 1
                                   (apply #'h2-header-block '((":status" . "200")
                                                      ("content-length" . "0"))))
                         (h2-frame 1 5 3
                                   (apply #'h2-header-block '((":status" . "201")
                                                      ("content-length" . "0")))))))))
           (closed 0)
           (body-calls 0)
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream
              :close-stream (lambda (closed-stream)
                              (ensure-true (eq stream closed-stream))
                              (incf closed))))
           (result
             (handler-case
                 (http-kit/http2:send-http2-requests-over-connection
                  connection
                  (list (make-http-request :method "GET" :uri "https://127.0.0.1/one")
                        (make-http-request :method "GET" :uri "https://127.0.0.1/two"))
                  :on-body-chunk (lambda (chunk request)
                                   (declare (ignore chunk request))
                                   (incf body-calls)))
               (http-protocol-error (condition) condition)
               (http-connection-error (condition) condition))))
      (cond
        (cause
         (ensure-true (typep result 'http-connection-error))
         (ensure-equal operation (http-error-operation result))
         (ensure-equal cause (http-connection-error-cause result)))
        (operation
         (ensure-true (typep result 'http-protocol-error))
         (ensure-equal operation (http-error-operation result))
         (ensure-equal detail (http-protocol-error-detail result)))
        (t
         (ensure-equal 2 (length result))
         (ensure-equal '(200 201) (mapcar #'http-response-status result))
         (dolist (response result)
           (ensure-equal (octets) (http-response-body response)))))
      (ensure-equal 0 body-calls)
      (ensure-equal (if (and operation (not cause)) 1 0) closed)
      (ensure-equal (not (and operation (not cause)))
                    (http-kit/http2:http2-connection-open-p connection))
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
                       frames)))
      (http-kit/http2:close-http2-connection connection)
      (ensure-equal 1 closed)))

  (deftest http2-public-batch-goaway-validation
    (check-public-batch-control-frame (h2-frame 7 0 0 (octets 0 0 0 3 0 0 0 0)))
    (check-public-batch-control-frame
     (h2-frame 7 0 1 (octets 0 0 0 0 0 0 0 0))
     :operation :http2-control :detail '(1 8))
    (check-public-batch-control-frame
     (h2-frame 7 0 0 (octets 0 0 0 0 0 0 0))
     :operation :http2-control :detail '(0 7))
    (check-public-batch-control-frame
     (h2-frame 7 0 0 (octets 128 0 0 0 0 0 0 0))
     :operation :http2-control :detail #x80000000))

  (deftest http2-public-batch-rst-validation
    (check-public-batch-control-frame (octets))
    (check-public-batch-control-frame
     (h2-frame 3 0 0 (octets 0 0 0 8))
     :operation :http2-control :detail '(0 4))
    (check-public-batch-control-frame
     (h2-frame 3 0 1 (octets 0 0 0))
     :operation :http2-control :detail '(1 3))
    (check-public-batch-control-frame
     (h2-frame 3 0 1 (octets 0 0 0 8))
     :operation :http2-control :cause '(:rst-stream 1 8)))

  (deftest http2-public-batch-unknown-response-stream
    (check-public-batch-control-frame (octets))
    (check-public-batch-control-frame
     (h2-frame 1 5 5 (apply #'h2-header-block '((":status" . "200")
                                       ("content-length" . "0"))))
     :operation :http2-read :detail 5)
    (check-public-batch-control-frame
     (h2-frame 0 1 5 (octets 9))
     :operation :http2-read :detail 5))

  (deftest http2-public-batch-unexpected-continuation
    (check-public-batch-control-frame (octets))
    (check-public-batch-control-frame
     (h2-frame 9 4 1 (octets))
     :operation :http2-headers :detail 1)))

#+sbcl
(progn
  (defun public-batch-final-frames ()
    (concatenate-octets
     (h2-frame 1 5 1 (h2-header-block (cons ":status" "200")))
     (h2-frame 1 5 3 (h2-header-block (cons ":status" "201")))))

  (defun check-public-batch-boundary
      (input &key operation detail connect-p (request-streams '(1 3))
                  (body (octets)) trailer)
    (let* ((stream (make-instance 'binary-session-stream :input input))
           (closed 0)
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream
              :close-stream (lambda (closed-stream)
                              (ensure-true (eq stream closed-stream))
                              (incf closed))))
           (result
             (handler-case
                 (http-kit/http2:send-http2-requests-over-connection
                  connection
                  (list (make-http-request
                         :method (if connect-p "CONNECT" "GET")
                         :protocol (when connect-p "websocket")
                         :uri "https://127.0.0.1/one")
                        (make-http-request :method "GET" :uri "https://127.0.0.1/two")))
               (http-protocol-error (condition) condition))))
      (if operation
          (progn
            (ensure-true (typep result 'http-protocol-error))
            (ensure-equal operation (http-error-operation result))
            (ensure-equal detail (http-protocol-error-detail result)))
          (progn
            (ensure-equal 2 (length result))
            (ensure-equal '(200 201) (mapcar #'http-response-status result))
            (ensure-equal body (http-response-body (first result)))
            (ensure-equal (octets) (http-response-body (second result)))
            (ensure-equal trailer
                          (http-header-value (http-response-trailers (first result))
                                             "x-end"))))
      (ensure-equal (if operation 1 0) closed)
      (ensure-equal (not operation) (http-kit/http2:http2-connection-open-p connection))
      (ensure-equal (length input) (binary-session-input-position stream))
      (ensure-equal request-streams
                    (mapcar #'http-kit/http2::%h2-frame-stream-id
                            (remove-if-not
                             (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                             (h2-output-frames stream))))
      (http-kit/http2:close-http2-connection connection)
      (ensure-equal 1 closed)))

  (deftest http2-public-batch-first-settings-boundary
    (check-public-batch-boundary
     (concatenate-octets (h2-frame 4 0 0 (octets)) (public-batch-final-frames)))
    (dolist (spec '((6 0 0 (0 1 2 3 4 5 6 7)) (4 1 0 ()) (4 0 1 ())))
      (destructuring-bind (type flags stream payload) spec
        (check-public-batch-boundary
         (h2-frame type flags stream (apply #'octets payload))
         :operation :http2-read :detail (list type stream flags)))))

  (deftest http2-public-batch-response-end-boundary
    (let ((settings (h2-frame 4 0 0 (octets)))
          (information (h2-header-block (cons ":status" "103")))
          (final (h2-header-block (cons ":status" "200")))
          (trailer (h2-header-block (cons "x-end" "yes"))))
      (check-public-batch-boundary
       (concatenate-octets
        settings (h2-frame 1 4 1 information) (h2-frame 1 4 1 final)
        (h2-frame 0 0 1 (octets 90)) (h2-frame 1 5 1 trailer)
        (h2-frame 1 5 3 (h2-header-block (cons ":status" "201"))))
       :body (octets 90) :trailer "yes")
      (check-public-batch-boundary
       (concatenate-octets settings (h2-frame 1 5 1 information))
       :operation :http2-read :detail 103)
      (check-public-batch-boundary
       (concatenate-octets settings (h2-frame 1 4 1 final) (h2-frame 1 4 1 trailer))
       :operation :http2-trailers :detail :missing-end-stream)))

  (deftest http2-public-batch-connect-negotiation-boundary
    (check-public-batch-boundary
     (concatenate-octets (h2-frame 4 0 0 (octets 0 8 0 0 0 1))
                        (public-batch-final-frames))
     :connect-p t)
    (check-public-batch-boundary
     (h2-frame 4 0 0 (octets)) :connect-p t :request-streams nil
     :operation :http2-write :detail :protocol-error)
    (check-public-batch-boundary
     (octets) :connect-p t :request-streams nil
     :operation :http2-read :detail :eof)
    (check-public-batch-boundary
     (h2-frame 6 0 0 (octets 0 1 2 3 4 5 6 7))
     :connect-p t :request-streams nil
     :operation :http2-read :detail '(6 0 0))
    (check-public-batch-boundary
     (h2-frame 4 1 0 (octets))
     :connect-p t :request-streams nil
     :operation :http2-read :detail '(4 0 1))
    (check-public-batch-boundary
     (h2-frame 4 0 1 (octets))
     :connect-p t :request-streams nil
     :operation :http2-read :detail '(4 1 0)))

  (deftest http2-public-batch-disabled-push-boundary
    (check-public-batch-boundary
     (concatenate-octets (h2-frame 4 0 0 (octets)) (public-batch-final-frames)))
    (check-public-batch-boundary
     (concatenate-octets
      (h2-frame 4 0 0 (octets))
      (h2-frame 5 4 1
                (concatenate-octets
                 (octets 0 0 0 2)
                 (h2-header-block (cons ":method" "GET") (cons ":scheme" "https")
                                  (cons ":authority" "example.com") (cons ":path" "/")))))
     :operation :http2-read :detail :protocol-error)))

#+sbcl
(progn
  (deftest http2-public-batch-preflight-boundary
    (check-public-batch-boundary
     (concatenate-octets (h2-frame 4 0 0 (octets)) (public-batch-final-frames)))
    (dolist (spec '((:requests nil nil) (:requests :invalid :invalid)
                    (:connection :invalid keyword)
                    (:request-body-functions #(nil nil) (:request-body-functions #(nil nil)))
                    (:request-body-functions (nil) (:request-body-functions 1 2))
                    (:request-body-lengths #(nil nil) (:request-body-lengths #(nil nil)))
                    (:request-body-lengths (nil) (:request-body-lengths 1 2))
                    (:on-body-chunk :invalid :invalid)
                    (:collect-body-p :invalid :invalid)
                    (:closed nil nil)))
      (destructuring-bind (kind value detail) spec
        (let* ((stream (make-instance 'binary-session-stream :input (octets)))
               (closed 0)
               (connection (http-kit/http2:make-http2-connection
                            :stream stream :close-stream
                            (lambda (s) (ensure-true (eq s stream)) (incf closed))))
               (requests (list (make-http-request :method "GET" :uri "https://example.com/a")
                               (make-http-request :method "GET" :uri "https://example.com/b"))))
          (when (eq kind :closed) (http-kit/http2:close-http2-connection connection))
          (let ((condition
                  (handler-case
                      (apply #'http-kit/http2:send-http2-requests-over-connection
                             (if (eq kind :connection) value connection)
                             (if (eq kind :requests) value requests)
                             (unless (member kind '(:connection :requests :closed))
                               (list kind value)))
                    (http-error (condition) condition))))
            (ensure-true (typep condition (if (eq kind :closed)
                                            'http-connection-error 'http-protocol-error)))
            (ensure-equal :http2-client (http-error-operation condition))
            (if (eq kind :closed)
                (ensure-equal :closed (http-connection-error-cause condition))
                (ensure-equal detail (http-protocol-error-detail condition)))
            (ensure-equal 0 (binary-session-input-position stream))
            (ensure-equal 0 (length (binary-session-output stream)))
            (ensure-equal (if (eq kind :closed) 1 0) closed)
            (ensure-equal (not (eq kind :closed))
                          (http-kit/http2:http2-connection-open-p connection)))
          (http-kit/http2:close-http2-connection connection)
          (ensure-equal 1 closed))))
    (dolist (circular-p '(nil t))
      (dolist (kind '(:requests :request-body-functions :request-body-lengths))
        (let* ((stream (make-instance 'binary-session-stream :input (octets)))
               (closed 0)
               (connection (http-kit/http2:make-http2-connection
                            :stream stream :close-stream
                            (lambda (s) (ensure-true (eq s stream)) (incf closed))))
               (requests (list (make-http-request :method "GET" :uri "https://example.com/a")
                               (make-http-request :method "GET" :uri "https://example.com/b")))
               (value (cons (if (eq kind :requests) (first requests) nil) :tail)))
          (when circular-p (setf (cdr value) value))
          (unwind-protect
               (let ((condition
                       (sb-ext:with-timeout 1
                         (handler-case
                             (apply #'http-kit/http2:send-http2-requests-over-connection
                                    connection (if (eq kind :requests) value requests)
                                    (unless (eq kind :requests) (list kind value)))
                           (http-error (condition) condition)))))
                 (ensure-true (typep condition 'http-protocol-error))
                 (ensure-equal :http2-client (http-error-operation condition))
                 (let ((detail (http-protocol-error-detail condition)))
                   (if (eq kind :requests)
                       (ensure-true (eq value detail))
                       (progn
                         (ensure-equal 2 (length detail))
                         (ensure-equal kind (first detail))
                         (ensure-true (eq value (second detail))))))
                 (ensure-equal 0 (binary-session-input-position stream))
                 (ensure-equal 0 (length (binary-session-output stream)))
                 (ensure-equal 0 closed)
                 (ensure-true (http-kit/http2:http2-connection-open-p connection)))
            (http-kit/http2:close-http2-connection connection))
          (ensure-equal 1 closed)))))

  (defun check-public-connection-cps (batch-p mode)
    (let* ((bad-continuation-p (member mode '(:bad-success :bad-error)))
           (failure-p (member mode '(:error :rethrow)))
           (input (cond
                    (bad-continuation-p (octets))
                    (failure-p (h2-frame 6 0 0 (octets 0 1 2 3 4 5 6 7)))
                    (batch-p
                     (concatenate-octets
                      (h2-frame 4 0 0 (octets))
                      (h2-frame 1 4 1 (h2-header-block (cons ":status" "200")))
                      (h2-frame 0 1 1 (octets 90))
                      (h2-frame 1 5 3 (h2-header-block (cons ":status" "201")))))
                    (t (h2-response-wire (octets 90)))))
           (stream (make-instance 'binary-session-stream :input input))
           (closed 0) (successes 0) (errors 0) (chunks nil) (notified nil)
           (requests (list (make-http-request :method "POST" :uri "https://example.com/a")
                           (make-http-request :method "GET" :uri "https://example.com/b")))
           (connection (http-kit/http2:make-http2-connection
                        :stream stream :close-stream
                        (lambda (s) (ensure-true (eq s stream)) (incf closed)))))
      (multiple-value-bind (producer position calls) (make-octet-body-producer (octets 7 8))
        (declare (ignore calls))
        (let* ((success (if (eq mode :bad-success) :invalid
                           (lambda (result)
                             (incf successes)
                             (let ((responses (if batch-p result (list result))))
                               (ensure-equal (if batch-p '(200 201) '(200))
                                             (mapcar #'http-response-status responses))
                               (dolist (response responses)
                                 (ensure-equal (octets) (http-response-body response))))
                             :completed)))
               (error-function (cond
                                 ((eq mode :bad-error) :invalid)
                                 ((eq mode :rethrow) nil)
                                 (t (lambda (condition)
                                      (incf errors) (setf notified condition) condition))))
               (result
                 (handler-case
                     (if batch-p
                         (http-kit/http2:send-http2-requests-over-connection/cps
                          connection requests success :on-error error-function
                          :request-body-functions (list producer nil)
                          :request-body-lengths '(2 nil) :collect-body-p nil
                          :on-body-chunk (lambda (chunk request)
                                           (ensure-true (eq request (first requests)))
                                           (push chunk chunks)))
                         (http-kit/http2:send-http2-request-over-connection/cps
                          connection (first requests) success :on-error error-function
                          :request-body-function producer :request-body-length 2
                          :collect-body-p nil :on-body-chunk (lambda (chunk) (push chunk chunks))))
                   (http-error (condition) condition))))
          (cond
            (bad-continuation-p
             (ensure-true (typep result 'http-protocol-error))
             (ensure-equal :transport (http-error-operation result))
             (ensure-equal :invalid (http-protocol-error-detail result))
             (ensure-equal 0 (binary-session-input-position stream))
             (ensure-equal 0 (length (binary-session-output stream))))
            (failure-p
             (ensure-true (typep result 'http-protocol-error))
             (ensure-equal :http2-read (http-error-operation result))
             (ensure-equal '(6 0 0) (http-protocol-error-detail result))
             (unless (eq mode :rethrow) (ensure-true (eq result notified))))
            (t
             (ensure-equal :completed result)
             (ensure-equal 2 (funcall position))
             (ensure-equal 1 (length chunks))
             (ensure-equal (octets 90) (first chunks))
             (let* ((frames (h2-output-frames stream))
                    (data (remove-if-not (lambda (frame)
                                           (= 0 (http-kit/http2::%h2-frame-type frame))) frames)))
               (ensure-true (equalp (octets 7 8)
                                   (apply #'concatenate-octets
                                          (mapcar #'http-kit/http2::%h2-frame-payload data))))
               (ensure-equal (if batch-p '(1 3) '(1))
                             (mapcar #'http-kit/http2::%h2-frame-stream-id
                                     (remove-if-not (lambda (frame)
                                                      (= 1 (http-kit/http2::%h2-frame-type frame)))
                                                    frames))))))
          (ensure-equal (if (or failure-p bad-continuation-p) 0 1) successes)
          (ensure-equal (if (eq mode :error) 1 0) errors)
          (ensure-equal (if failure-p 1 0) closed)
          (ensure-equal (not failure-p) (http-kit/http2:http2-connection-open-p connection))
          (http-kit/http2:close-http2-connection connection)
          (ensure-equal 1 closed)))))

  (deftest http2-public-connection-cps-boundary
    (dolist (batch-p '(nil t))
      (dolist (mode '(:success :error :rethrow :bad-success :bad-error))
        (check-public-connection-cps batch-p mode))))

  (deftest http2-public-batch-blocked-upload-boundary
    (dolist (eof-p '(nil t))
      (let* ((body (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 7))
             (early-response
               (concatenate-octets
                (h2-frame 1 4 3 (h2-header-block (cons ":status" "200")
                                               (cons "content-length" "1")))
                (h2-frame 0 1 3 (octets 8))))
             (input
               (concatenate-octets
                (h2-frame 4 0 0 (octets)) early-response
                (if eof-p (octets)
                    (concatenate-octets
                     (h2-frame 8 0 0 (octets 0 0 0 1))
                     (h2-frame 8 0 1 (octets 0 0 0 1))
                     (h2-frame 1 5 1 (h2-header-block (cons ":status" "204")))))))
             (stream (make-instance 'binary-session-stream :input input))
             (closed 0) (chunks nil)
             (requests (list (make-http-request :method "POST" :uri "https://example.com/upload")
                             (make-http-request :method "GET" :uri "https://example.com/other")))
             (connection (http-kit/http2:make-http2-connection
                          :stream stream :close-stream
                          (lambda (s) (ensure-true (eq s stream)) (incf closed)))))
        (multiple-value-bind (producer position calls) (make-octet-body-producer body)
          (let ((result
                  (handler-case
                      (http-kit/http2:send-http2-requests-over-connection
                       connection requests :request-body-functions (list producer nil)
                       :request-body-lengths '(65536 nil)
                       :on-body-chunk (lambda (chunk request)
                                        (ensure-true (eq request (second requests)))
                                        (push chunk chunks)))
                    (http-connection-error (condition) condition))))
            (if eof-p
                (progn
                  (ensure-true (typep result 'http-connection-error))
                  (ensure-equal :http2-read (http-error-operation result))
                  (ensure-equal :eof (http-connection-error-cause result))
                  (ensure-false chunks))
                (progn
                  (ensure-equal '(204 200) (mapcar #'http-response-status result))
                  (ensure-equal (octets) (http-response-body (first result)))
                  (ensure-equal (octets 8) (http-response-body (second result)))
                  (ensure-equal 1 (length chunks))
                  (ensure-equal (octets 8) (first chunks))))
            (ensure-equal 65536 (funcall position))
            (ensure-equal 4 (funcall calls))
            (ensure-equal (length input) (binary-session-input-position stream))
            (let* ((frames (h2-output-frames stream))
                   (data (remove-if-not (lambda (frame)
                                          (= 0 (http-kit/http2::%h2-frame-type frame))) frames))
                   (sent (apply #'concatenate-octets
                                (mapcar #'http-kit/http2::%h2-frame-payload data))))
              (ensure-equal '(1 3)
                            (mapcar #'http-kit/http2::%h2-frame-stream-id
                                    (remove-if-not (lambda (frame)
                                                     (= 1 (http-kit/http2::%h2-frame-type frame))) frames)))
              (ensure-equal (if eof-p 65535 65536) (length sent))
              (ensure-true (equalp (subseq body 0 (length sent)) sent))
              (ensure-true (every (lambda (frame)
                                   (and (= 1 (http-kit/http2::%h2-frame-stream-id frame))
                                        (<= (length (http-kit/http2::%h2-frame-payload frame)) 16384)))
                                 data))
              (ensure-equal (if eof-p 0 1)
                            (logand 1 (http-kit/http2::%h2-frame-flags (car (last data)))))
              (ensure-equal 1 (count-if (lambda (frame)
                                        (and (= 4 (http-kit/http2::%h2-frame-type frame))
                                             (= 1 (http-kit/http2::%h2-frame-flags frame)))) frames)))
            (ensure-equal (if eof-p 1 0) closed)
            (ensure-equal (not eof-p) (http-kit/http2:http2-connection-open-p connection))
            (http-kit/http2:close-http2-connection connection)
            (ensure-equal 1 closed)))))))

#+sbcl
(progn
  (deftest http2-public-gap-producer-options
    (let* ((stream (make-instance 'binary-session-stream :input (h2-response-wire (octets 7))))
           (closed 0) (calls 0)
           (connection (http-kit/http2:make-http2-connection
                        :stream stream :close-stream (lambda (value)
                                                       (ensure-true (eq stream value))
                                                       (incf closed))))
           (request (make-http-request :method "POST" :uri "https://producer.test/"))
           (producer (lambda (maximum) (declare (ignore maximum)) (incf calls) nil))
           (trace (make-http-request :method "TRACE" :uri "https://producer.test/"))
           (memory (make-http-request :method "POST" :uri "https://producer.test/" :body (octets 1))))
      (dolist (case (list (list request :invalid nil :invalid)
                         (list request producer -1 -1)
                         (list request producer 1/2 1/2)
                         (list request nil 0 0)
                         (list trace producer nil :trace-content)
                         (list memory producer nil memory)))
        (destructuring-bind (value function length detail) case
          (let ((failure (handler-case
                             (http-kit/http2:send-http2-request-over-connection
                              connection value :request-body-function function :request-body-length length)
                           (http-protocol-error (condition) condition))))
            (ensure-true (typep failure 'http-protocol-error))
            (ensure-equal :http2-write (http-error-operation failure))
            (ensure-true (eql detail (http-protocol-error-detail failure)))))
        (ensure-equal 0 (length (binary-session-output stream)))
        (ensure-equal 0 (binary-session-input-position stream))
        (ensure-equal 0 calls)
        (ensure-equal 0 closed)
        (ensure-true (http-kit/http2:http2-connection-open-p connection)))
      (let ((response (http-kit/http2:send-http2-request-over-connection
                       connection request :request-body-function producer :request-body-length 0)))
        (ensure-equal 200 (http-response-status response))
        (ensure-equal (octets 7) (http-response-body response)))
      (ensure-equal 1 calls)
      (ensure-true (http-kit/http2:http2-connection-open-p connection))
      (let ((data (remove-if-not (lambda (frame) (= 0 (http-kit/http2::%h2-frame-type frame)))
                                 (h2-output-frames stream))))
        (ensure-equal 1 (length data))
        (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id (first data)))
        (ensure-equal 1 (http-kit/http2::%h2-frame-flags (first data)))
        (ensure-equal (octets) (http-kit/http2::%h2-frame-payload (first data))))
      (http-kit/http2:close-http2-connection connection)
      (ensure-equal 1 closed)))

  (deftest http2-public-gap-producer-chunks
    (dolist (chunk (list "x" (make-array '(1 1) :initial-element 1) (octets)
                        (make-array 16385 :element-type '(unsigned-byte 8) :initial-element 1)
                        (vector 256) (octets 9)))
      (let* ((good (equalp chunk (octets 9)))
             (stream (make-instance 'binary-session-stream :input (h2-response-wire (octets 7))))
             (closed 0) (calls 0) (maximum nil)
             (connection (http-kit/http2:make-http2-connection
                          :stream stream :close-stream (lambda (value)
                                                         (ensure-true (eq stream value))
                                                         (incf closed))))
             (result (handler-case
                         (http-kit/http2:send-http2-request-over-connection
                          connection (make-http-request :method "POST" :uri "https://producer.test/")
                          :request-body-function (lambda (limit)
                                                   (setf maximum limit)
                                                   (when (= 1 (incf calls)) chunk)))
                       (http-protocol-error (condition) condition))))
        (ensure-equal 16384 maximum)
        (if good
            (progn
              (ensure-equal 200 (http-response-status result))
              (ensure-equal (octets 7) (http-response-body result))
              (ensure-equal 2 calls))
            (progn
              (ensure-true (typep result 'http-protocol-error))
              (ensure-equal :http2-write (http-error-operation result))
              (ensure-equal (cond ((or (stringp chunk) (= 2 (array-rank chunk))) (type-of chunk))
                                  ((zerop (length chunk)) chunk)
                                  ((> (length chunk) 16384) '(16385 16384))
                                  (t 256))
                            (http-protocol-error-detail result))
              (ensure-equal 1 calls)))
        (multiple-value-bind (body frames acks) (h2-output-data-summary stream)
          (ensure-equal (if good (octets 9) (octets)) body)
          (ensure-equal (if good 2 0) frames)
          (ensure-equal (if good 1 0) acks))
        (ensure-equal (if good (length (binary-session-input stream)) 0)
                      (binary-session-input-position stream))
        (let ((frames (h2-output-frames stream)))
          (ensure-equal (if good '(4 1 0 0 4 8) '(4 1))
                        (mapcar #'http-kit/http2::%h2-frame-type frames))
          (ensure-equal (if good '(0 1 1 1 0 0) '(0 1))
                        (mapcar #'http-kit/http2::%h2-frame-stream-id frames)))
        (when good
          (let ((last (car (last (remove-if-not
                                 (lambda (frame) (= 0 (http-kit/http2::%h2-frame-type frame)))
                                 (h2-output-frames stream))))))
            (ensure-equal 0 (http-kit/http2::%h2-frame-type last))
            (ensure-equal 1 (http-kit/http2::%h2-frame-flags last))
            (ensure-equal (octets) (http-kit/http2::%h2-frame-payload last)))
          (dolist (frame (last (h2-output-frames stream) 1))
            (ensure-equal 0 (http-kit/http2::%h2-frame-flags frame))
            (ensure-equal (octets 0 0 0 1) (http-kit/http2::%h2-frame-payload frame))))
        (ensure-equal (if good 0 1) closed)
        (ensure-equal good (http-kit/http2:http2-connection-open-p connection))
        (http-kit/http2:close-http2-connection connection)
        (ensure-equal 1 closed))))

  (deftest http2-public-gap-padded-priority-responses
    (dolist (case (list (list (h2-frame 1 12 1 (octets)) :http2-headers :missing-pad-length)
                       (list (h2-frame 1 36 1 (octets 0 0 0 0)) :http2-headers 4)
                       (list (h2-frame 1 36 1 (octets 0 0 0 1 0 #x88)) :http2-headers 1)
                       (list (concatenate-octets (h2-frame 1 4 1 (octets #x88))
                                                 (h2-frame 0 9 1 (octets)))
                             :http2-read :missing-pad-length)
                       (list (concatenate-octets (h2-frame 1 44 1 (octets 1 0 0 0 0 15 #x88 0))
                                                 (h2-frame 0 9 1 (octets 1 90 0))) nil nil)))
      (destructuring-bind (wire operation detail) case
        (let* ((input (concatenate-octets (h2-frame 4 0 0 (octets)) wire))
               (stream (make-instance 'binary-session-stream :input input))
               (closed 0)
               (connection (http-kit/http2:make-http2-connection
                            :stream stream :close-stream (lambda (value)
                                                           (ensure-true (eq stream value))
                                                           (incf closed))))
               (result (handler-case
                           (http-kit/http2:send-http2-request-over-connection
                            connection (make-http-request :method "GET" :uri "https://padding.test/"))
                         (http-protocol-error (condition) condition))))
          (if operation
              (progn
                (ensure-true (typep result 'http-protocol-error))
                (ensure-equal operation (http-error-operation result))
                (ensure-equal detail (http-protocol-error-detail result)))
              (progn
                (ensure-equal 200 (http-response-status result))
                (ensure-equal (octets 90) (http-response-body result))))
          (ensure-equal (length input) (binary-session-input-position stream))
          (let ((frames (h2-output-frames stream)))
            (ensure-equal (if operation '(4 1 4) '(4 1 4 8))
                          (mapcar #'http-kit/http2::%h2-frame-type frames))
            (ensure-equal (if operation '(0 1 0) '(0 1 0 0))
                          (mapcar #'http-kit/http2::%h2-frame-stream-id frames))
            (ensure-equal 1 (http-kit/http2::%h2-frame-flags (third frames)))
            (unless operation
              (dolist (frame (last frames 1))
                (ensure-equal 0 (http-kit/http2::%h2-frame-flags frame))
                (ensure-equal (octets 0 0 0 3) (http-kit/http2::%h2-frame-payload frame)))))
          (ensure-equal (if operation 1 0) closed)
          (ensure-equal (null operation) (http-kit/http2:http2-connection-open-p connection))
          (http-kit/http2:close-http2-connection connection)
          (ensure-equal 1 closed)))))

  (deftest http2-public-gap-ping-validation
    (let ((payload (octets 1 2 3 4 5 6 7 8)))
      (dolist (peer (list (h2-frame 6 1 1 payload) (h2-frame 6 1 0 (octets 1))
                          (h2-frame 6 1 0 payload)))
        (let* ((good (equalp peer (h2-frame 6 1 0 payload)))
               (input (concatenate-octets (h2-response-wire (octets)) peer))
               (stream (make-instance 'binary-session-stream :input input))
               (closed 0)
               (connection (http-kit/http2:make-http2-connection
                            :stream stream :close-stream (lambda (value)
                                                           (ensure-true (eq stream value))
                                                           (incf closed)))))
          (ensure-equal 200 (http-response-status
                             (http-kit/http2:send-http2-request-over-connection
                              connection (make-http-request :method "GET" :uri "https://ping.test/"))))
          (let ((before (copy-seq (binary-session-output stream)))
                (position (binary-session-input-position stream)))
            (dolist (bad (list (octets) (octets 1) (octets 1 2 3 4 5 6 7 8 9)))
              (let ((failure (handler-case (http-kit/http2:ping-http2-connection connection :payload bad)
                               (http-protocol-error (condition) condition))))
                (ensure-true (typep failure 'http-protocol-error))
                (ensure-equal :http2-client (http-error-operation failure))
                (ensure-equal (length bad) (http-protocol-error-detail failure)))
              (ensure-true (equalp before (binary-session-output stream)))
              (ensure-equal position (binary-session-input-position stream))
              (ensure-equal 0 closed)))
          (let ((result (handler-case (http-kit/http2:ping-http2-connection connection :payload payload)
                          (http-protocol-error (condition) condition))))
            (if good
                (ensure-equal payload result)
                (progn
                  (ensure-true (typep result 'http-protocol-error))
                  (ensure-equal :http2-client (http-error-operation result))
                  (let ((frame (http-protocol-error-detail result)))
                    (ensure-equal 6 (http-kit/http2::%h2-frame-type frame))
                    (ensure-equal (aref peer 8) (http-kit/http2::%h2-frame-stream-id frame))
                    (ensure-equal (subseq peer 9) (http-kit/http2::%h2-frame-payload frame))))))
          (let ((ping (car (last (h2-output-frames stream)))))
            (ensure-equal 6 (http-kit/http2::%h2-frame-type ping))
            (ensure-equal 0 (http-kit/http2::%h2-frame-flags ping))
            (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id ping))
            (ensure-equal payload (http-kit/http2::%h2-frame-payload ping)))
          (ensure-equal (length input) (binary-session-input-position stream))
          (ensure-equal (if good 0 1) closed)
          (ensure-equal good (http-kit/http2:http2-connection-open-p connection))
          (http-kit/http2:close-http2-connection connection)
          (ensure-equal 1 closed)))))

  (deftest http2-public-gap-cancel-validation
    (let* ((stream (make-instance 'binary-session-stream :input (h2-response-wire (octets))))
           (closed 0)
           (connection (http-kit/http2:make-http2-connection
                        :stream stream :close-stream (lambda (value)
                                                       (ensure-true (eq stream value))
                                                       (incf closed)))))
      (ensure-equal 200 (http-response-status
                         (http-kit/http2:send-http2-request-over-connection
                          connection (make-http-request :method "GET" :uri "https://cancel.test/"))))
      (let ((before (copy-seq (binary-session-output stream)))
            (position (binary-session-input-position stream)))
        (dolist (case (list (list nil 1 nil :http2-client 'null)
                           (list connection 0 nil :http2-write 0)
                           (list connection 2 nil :http2-write 2)
                           (list connection 3 nil :http2-write 3)
                           (list connection 1/2 nil :http2-write 1/2)
                           (list connection 1 :bad-clock :http2-client :bad-clock)))
          (destructuring-bind (value id clock operation detail) case
            (let ((failure (handler-case
                               (http-kit/http2:cancel-http2-stream value id :clock-function clock)
                             (http-protocol-error (condition) condition))))
              (ensure-true (typep failure 'http-protocol-error))
              (ensure-equal operation (http-error-operation failure))
              (ensure-equal detail (http-protocol-error-detail failure))))
          (ensure-true (equalp before (binary-session-output stream)))
          (ensure-equal position (binary-session-input-position stream))
          (ensure-equal 0 closed)))
      (ensure-true (http-kit/http2:cancel-http2-stream connection 1))
      (let ((rst (car (last (h2-output-frames stream)))))
        (ensure-equal 3 (http-kit/http2::%h2-frame-type rst))
        (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id rst))
        (ensure-equal (octets 0 0 0 8) (http-kit/http2::%h2-frame-payload rst)))
      (http-kit/http2:close-http2-connection connection)
      (let ((before (copy-seq (binary-session-output stream)))
            (failure (handler-case (http-kit/http2:cancel-http2-stream connection 1)
                       (http-connection-error (condition) condition))))
        (ensure-true (typep failure 'http-connection-error))
        (ensure-equal :http2-client (http-error-operation failure))
        (ensure-equal :closed (http-connection-error-cause failure))
        (ensure-true (equalp before (binary-session-output stream))))
      (ensure-equal 1 closed))))
#+sbcl
(progn
  (defclass scheduler-close-after-ping-write-stream (binary-session-stream)
    ((ping-write-started
       :initarg :ping-write-started
       :reader scheduler-close-after-ping-write-started)
     (ping-write-release
       :initarg :ping-write-release
       :reader scheduler-close-after-ping-write-release)))

  (defmethod sb-gray:stream-write-sequence
      ((stream scheduler-close-after-ping-write-stream) sequence
       &optional (start 0) end)
    (let ((end (or end (length sequence))))
      (if (scheduled-ping-wire-p sequence start end)
          (prog1 (call-next-method)
            (sb-thread:signal-semaphore
             (scheduler-close-after-ping-write-started stream))
            (sb-thread:wait-on-semaphore
             (scheduler-close-after-ping-write-release stream)))
          (call-next-method)))))
#+sbcl
(deftest http2-scheduler-ping-close-after-write
  (let* ((payload (octets 9 8 7 6 5 4 3 2))
         (ping-write-started (sb-thread:make-semaphore :count 0))
         (ping-write-release (sb-thread:make-semaphore :count 0))
         (stream
           (make-instance 'scheduler-close-after-ping-write-stream
                          :input (octets)
                          :ping-write-started ping-write-started
                          :ping-write-release ping-write-release))
         (closed 0)
         (connection
           (http-kit/http2:make-http2-connection
            :stream stream
            :close-stream
            (lambda (closed-stream)
              (ensure-true (eq stream closed-stream))
              (incf closed))))
         (ping-error nil)
         (ping-thread nil)
         (ping-write-released-p nil))
    (setf (http-kit/http2::%http2-connection-session-started-p connection) t)
    (unwind-protect
         (progn
           (setf ping-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (setf ping-error
                          (handler-case
                              (http-kit/http2:ping-http2-connection
                               connection :payload payload)
                            (error (condition) condition))))))
           (ensure-true
            (sb-thread:wait-on-semaphore ping-write-started :timeout 5))
           (ensure-true
            (http-kit/http2:http2-connection-open-p connection))
           (http-kit/http2:close-http2-connection connection)
           (setf ping-write-released-p t)
           (sb-thread:signal-semaphore ping-write-release)
           (sb-thread:join-thread ping-thread)
           (setf ping-thread nil)
           (ensure-true (typep ping-error 'http-kit:http-connection-error))
           (ensure-equal :http2-control (http-kit:http-error-operation ping-error))
           (ensure-equal :closed
                         (http-kit:http-connection-error-cause ping-error))
           (ensure-false
            (http-kit/http2:http2-connection-open-p connection))
           (ensure-true
            (scheduled-ping-wire-p
             (binary-session-output stream) 0
             (length (binary-session-output stream))))
           (ensure-equal 1 closed))
      (http-kit/http2:close-http2-connection connection)
      (unless ping-write-released-p
        (setf ping-write-released-p t)
        (sb-thread:signal-semaphore ping-write-release))
      (when ping-thread
        (sb-thread:join-thread ping-thread)))))
#+sbcl
(deftest http2-scheduler-response-waits-for-late-callback
  (let* ((request (make-http-request :method "GET"
                                    :uri "https://scheduler.test/late-callback"))
         (stream
           (make-instance 'binary-session-stream
                          :input (h2-response-wire (octets 65 66))))
         (closed 0)
         (connection
           (http-kit/http2:make-http2-connection
            :stream stream
            :close-stream
            (lambda (closed-stream)
              (ensure-true (eq stream closed-stream))
              (incf closed))))
         (runner-symbol
           'http-kit/http2::%h2-scheduler-run-entry-callbacks)
         (original-runner (symbol-function runner-symbol))
         (runner-calls 0)
         (callback-enqueued-p nil)
         (late-callback-calls 0)
         (body-chunks nil)
         (response nil))
    (unwind-protect
         (progn
           (setf (symbol-function runner-symbol)
                 (lambda (received-connection entry clock-function)
                   (funcall original-runner
                            received-connection entry clock-function)
                   (incf runner-calls)
                   (unless callback-enqueued-p
                     (setf callback-enqueued-p t)
                     (http-kit/http2::%h2-scheduler-queue-callback
                      received-connection entry
                      (list :information
                            (lambda (information)
                              (declare (ignore information))
                              (incf late-callback-calls))
                            nil)))))
           (setf response
                 (http-kit/http2:send-http2-request-over-connection
                  connection request
                  :on-body-chunk
                  (lambda (chunk)
                    (push (copy-seq chunk) body-chunks))))
           (ensure-equal 200 (http-response-status response))
           (ensure-equal (octets 65 66) (http-response-body response))
           (ensure-equal (list (octets 65 66)) (nreverse body-chunks))
           (ensure-equal 2 runner-calls)
           (ensure-true callback-enqueued-p)
           (ensure-equal 1 late-callback-calls)
           (ensure-equal 0 closed)
           (http-kit/http2:close-http2-connection connection)
           (ensure-equal 1 closed))
      (setf (symbol-function runner-symbol) original-runner)
      (http-kit/http2:close-http2-connection connection))))
#+sbcl
(deftest http2-scheduler-extra-runtime-edge-boundaries
  (let ((request (make-http-request :method "GET"
                                    :uri "https://scheduler.test/edge")))
    (labels ((make-connection (input &optional write-error)
               (let* ((stream
                        (if write-error
                            (make-instance 'scheduler-write-failure-stream
                                           :input input
                                           :write-error write-error)
                            (make-instance 'binary-session-stream :input input)))
                      (closes (list 0))
                      (connection
                        (http-kit/http2:make-http2-connection
                         :stream stream
                         :close-stream
                         (lambda (received)
                           (ensure-true (eq received stream))
                           (incf (car closes))))))
                 (values stream connection closes)))
             (entry (request stream-id &key error)
               (let ((entry
                       (http-kit/http2::%make-h2-batch-entry
                        :request request :stream-id stream-id)))
                 (setf (http-kit/http2::%h2-batch-entry-request-state entry)
                       :written
                       (http-kit/http2::%h2-batch-entry-error entry) error)
                 entry)))
      (multiple-value-bind (stream connection closes)
          (make-connection (octets))
        (let* ((active-entry (entry request 1))
               (clock-calls 0)
               (clock (lambda () (if (= (incf clock-calls) 1) 0 2)))
               (observed nil))
          (http-kit/http2::%h2-with-scheduler-lock (connection)
            (setf (http-kit/http2::%http2-connection-scheduler-entries connection)
                  (list active-entry)))
          (setf observed
                (handler-case
                    (http-kit/http2::%h2-scheduler-wait-until
                     connection (lambda () nil)
                     (http-kit/http2::%h2-reader-for stream) nil 1 clock active-entry)
                  (http-kit:http-timeout (condition) condition)))
          (ensure-true (typep observed 'http-kit:http-timeout))
          (ensure-equal :read (http-kit:http-error-operation observed))
          (ensure-true (eq observed
                           (http-kit/http2::%h2-batch-entry-error active-entry)))
          (ensure-true
           (http-kit/http2::%h2-batch-entry-local-retired-p active-entry))
          (ensure-false
           (http-kit/http2::%http2-connection-scheduler-entries connection))
          (ensure-true (http-kit/http2:http2-connection-open-p connection))
          (ensure-equal (h2-frame 3 0 1 (octets 0 0 0 8))
                        (binary-session-output stream))
          (http-kit/http2:close-http2-connection connection)
          (ensure-equal 1 (car closes))))
      (dolist (timeout-p '(nil t))
        (let* ((failure
                   (if timeout-p
                       (make-condition 'http-kit:http-timeout
                                       :message "scheduled request write timed out"
                                       :operation :write :kind :write)
                       (make-condition 'http-kit:http-connection-error
                                       :message "scheduled request write failed"
                                       :operation :http2-write
                                       :cause :test-write))))
          (multiple-value-bind (stream connection closes)
              (make-connection (octets) failure)
            (let ((observed
                    (handler-case
                        (http-kit/http2:send-http2-request-over-connection
                         connection request)
                      (http-kit:http-error (condition) condition))))
            (ensure-true (eq failure observed))
            (ensure-false
             (http-kit/http2::%http2-connection-scheduler-entries connection))
            (ensure-equal 3
                          (http-kit/http2::%http2-connection-next-stream-id
                           connection))
            (ensure-equal 0
                          (http-kit/http2::%http2-connection-scheduler-last-opened-stream-id
                           connection))
            (ensure-false
             (http-kit/http2::%http2-connection-session-started-p connection))
            (ensure-equal 0 (length (binary-session-output stream)))
            (ensure-false (http-kit/http2:http2-connection-open-p connection))
            (ensure-equal 1 (car closes))))))
      (multiple-value-bind (stream connection closes)
          (make-connection (octets))
        (http-kit/http2:close-http2-connection connection)
        (let ((observed
                (handler-case
                    (http-kit/http2::%h2-scheduler-open-entry
                     connection request nil nil
                     (http-kit/http2::%http2-connection-max-header-bytes connection)
                     (http-kit/http2::%http2-connection-max-fields connection)
                     (http-kit/http2::%http2-connection-max-body-bytes connection)
                     nil #'http-kit::%monotonic-time nil nil nil nil t nil nil
                     (lambda () nil)
                     (lambda () (error "Unexpected request cancellation."))
                     (lambda (opened-entry)
                       (declare (ignore opened-entry))))
                  (http-kit:http-connection-error (condition) condition))))
          (ensure-true (typep observed 'http-kit:http-connection-error))
          (ensure-equal :http2-client (http-kit:http-error-operation observed))
          (ensure-equal :closed (http-kit:http-connection-error-cause observed))
          (ensure-equal 0 (length (binary-session-output stream)))
          (ensure-equal 1 (car closes))))
      (multiple-value-bind (stream connection closes)
          (make-connection (octets))
        (let* ((entry-error
                 (make-condition 'http-kit:http-connection-error
                                 :message "request flow-control entry failed"
                                 :operation :http2-write :cause :test-flow-error))
               (active-entry (entry request 1 :error entry-error))
               (observed nil))
          (http-kit/http2::%h2-with-scheduler-lock (connection)
            (setf (http-kit/http2::%http2-connection-scheduler-entries connection)
                  (list active-entry)))
          (setf observed
                (handler-case
                    (http-kit/http2::%h2-scheduler-wait-for-window
                     connection 1 nil #'http-kit::%monotonic-time)
                  (http-kit:http-connection-error (condition) condition)))
          (ensure-true (eq entry-error observed))
          (ensure-equal :test-flow-error
                        (http-kit:http-connection-error-cause observed))
          (ensure-equal 0 (length (binary-session-output stream)))
          (http-kit/http2:close-http2-connection connection)
          (ensure-equal 1 (car closes))))
      (dolist (closed-p '(nil t))
        (multiple-value-bind (stream connection closes)
            (make-connection (octets))
          (when closed-p
            (http-kit/http2:close-http2-connection connection))
          (let ((observed
                  (handler-case
                      (http-kit/http2::%h2-scheduler-wait-for-window
                       connection 1 nil #'http-kit::%monotonic-time)
                    (http-kit:http-connection-error (condition) condition))))
            (ensure-true (typep observed 'http-kit:http-connection-error))
            (ensure-equal :http2-write (http-kit:http-error-operation observed))
            (ensure-equal :closed (http-kit:http-connection-error-cause observed))
            (ensure-equal 0 (length (binary-session-output stream)))
            (http-kit/http2:close-http2-connection connection)
            (ensure-equal 1 (car closes)))))
      (multiple-value-bind (stream connection closes)
          (make-connection (octets))
        (setf (http-kit/http2::%http2-connection-peer-max-concurrent-streams
               connection)
              0)
        (http-kit/http2:close-http2-connection connection)
        (let ((observed
                (handler-case
                    (http-kit/http2::%h2-scheduler-wait-for-capacity
                     connection nil #'http-kit::%monotonic-time
                     (lambda () nil)
                     (lambda () (error "Unexpected request cancellation.")))
                  (http-kit:http-connection-error (condition) condition))))
          (ensure-true (typep observed 'http-kit:http-connection-error))
          (ensure-equal :http2-write (http-kit:http-error-operation observed))
          (ensure-equal :closed (http-kit:http-connection-error-cause observed))
          (ensure-equal 0 (length (binary-session-output stream)))
          (ensure-equal 1 (car closes))))
      (multiple-value-bind (stream connection closes)
          (make-connection (octets))
        (declare (ignore stream))
        (let* ((old-entry-error
                 (make-condition 'http-kit:http-protocol-error
                                 :message "earlier stream error"
                                 :operation :http2-read :detail :earlier))
               (failure
                 (make-condition 'http-kit:http-connection-error
                                 :message "reader failed"
                                 :operation :http2-read :cause :reader-failure))
               (old-ping-error
                 (make-condition 'http-kit:http-connection-error
                                 :message "earlier ping error"
                                 :operation :http2-control :cause :earlier))
               (old-entry (entry request 1 :error old-entry-error))
               (active-entry (entry request 3))
               (acknowledged
                 (http-kit/http2::%make-h2-scheduler-ping-waiter
                  (octets 0 0 0 0 0 0 0 1)))
               (pending
                 (http-kit/http2::%make-h2-scheduler-ping-waiter
                  (octets 0 0 0 0 0 0 0 2)))
               (failed
                 (http-kit/http2::%make-h2-scheduler-ping-waiter
                  (octets 0 0 0 0 0 0 0 3))))
          (setf (http-kit/http2::%h2-scheduler-ping-waiter-acknowledged-p
                 acknowledged)
                t
                (http-kit/http2::%h2-scheduler-ping-waiter-failure failed)
                old-ping-error)
          (http-kit/http2::%h2-with-scheduler-lock (connection)
            (setf (http-kit/http2::%http2-connection-scheduler-entries connection)
                  (list old-entry active-entry)
                  (http-kit/http2::%http2-connection-scheduler-ping-waiters
                   connection)
                  (list acknowledged pending failed)))
          (http-kit/http2::%h2-scheduler-fail-connection connection failure)
          (http-kit/http2::%h2-scheduler-fail-connection connection failure)
          (ensure-true (http-kit/http2::%http2-connection-closed-p connection))
          (ensure-false
           (http-kit/http2::%http2-connection-scheduler-entries connection))
          (ensure-true
           (eq old-entry-error (http-kit/http2::%h2-batch-entry-error old-entry)))
          (ensure-true
           (eq failure (http-kit/http2::%h2-batch-entry-error active-entry)))
          (ensure-true
           (http-kit/http2::%h2-scheduler-ping-waiter-acknowledged-p acknowledged))
          (ensure-false
           (http-kit/http2::%h2-scheduler-ping-waiter-failure acknowledged))
          (ensure-true
           (eq failure
               (http-kit/http2::%h2-scheduler-ping-waiter-failure pending)))
          (ensure-true
           (eq old-ping-error
               (http-kit/http2::%h2-scheduler-ping-waiter-failure failed)))
          (ensure-equal 1 (car closes))))
      (multiple-value-bind (stream connection closes)
          (make-connection (octets))
        (setf (http-kit/http2::%http2-connection-session-started-p connection) t)
        (http-kit/http2:close-http2-connection connection)
        (let ((observed
                (handler-case
                    (http-kit/http2::%h2-scheduler-ping
                     connection (octets 0 0 0 0 0 0 0 9) nil
                     #'http-kit::%monotonic-time)
                  (http-kit:http-connection-error (condition) condition))))
          (ensure-true (typep observed 'http-kit:http-connection-error))
          (ensure-equal :http2-control (http-kit:http-error-operation observed))
          (ensure-equal :closed (http-kit:http-connection-error-cause observed))
          (ensure-equal 0 (length (binary-session-output stream)))
          (ensure-equal 1 (car closes))))
      (multiple-value-bind (stream connection closes)
          (make-connection (h2-response-wire (octets)))
        (let ((observed
                (handler-case
                    (http-kit/http2:send-http2-request-over-connection
                     connection request
                     :on-stream-open
                     (lambda (stream-id cancel)
                       (declare (ignore cancel))
                       (ensure-equal 1 stream-id)
                       (http-kit/http2:close-http2-connection connection)))
                  (http-kit:http-connection-error (condition) condition))))
          (ensure-true (typep observed 'http-kit:http-connection-error))
          (ensure-equal :http2-read (http-kit:http-error-operation observed))
          (ensure-equal :closed (http-kit:http-connection-error-cause observed))
          (ensure-equal 0 (binary-session-input-position stream))
          (ensure-equal '(1)
                        (mapcar #'http-kit/http2::%h2-frame-stream-id
                                (remove-if-not
                                 (lambda (frame)
                                   (= 1 (http-kit/http2::%h2-frame-type frame)))
                                 (h2-output-frames stream))))
          (ensure-equal 1 (car closes))))
      (multiple-value-bind (stream connection closes)
          (make-connection (h2-response-wire (octets 65 66)))
        (let* ((chunks nil)
               (response
                (http-kit/http2:send-http2-request-over-connection
                 connection request
                 :on-body-chunk
                 (lambda (chunk) (push (copy-seq chunk) chunks)))))
          (ensure-equal 200 (http-response-status response))
          (ensure-equal (octets 65 66) (http-response-body response))
          (ensure-equal (list (octets 65 66)) (nreverse chunks))
          (ensure-true
           (member 8 (mapcar #'http-kit/http2::%h2-frame-type
                             (h2-output-frames stream))))
          (http-kit/http2:close-http2-connection connection)
          (ensure-equal 1 (car closes))))
      (multiple-value-bind (stream connection closes)
          (make-connection (octets))
        (let ((observed
                (handler-case
                    (http-kit/http2::%h2-send-http2-request-over-connection-scheduled
                     connection request :clock-function :invalid-clock)
                  (http-kit:http-protocol-error (condition) condition))))
          (ensure-true (typep observed 'http-kit:http-protocol-error))
          (ensure-equal :http2-client (http-kit:http-error-operation observed))
          (ensure-equal :invalid-clock
                        (http-kit:http-protocol-error-detail observed))
          (ensure-equal 0 (binary-session-input-position stream))
          (ensure-equal 0 (length (binary-session-output stream)))
          (ensure-true (http-kit/http2:http2-connection-open-p connection))
          (http-kit/http2:close-http2-connection connection)
          (ensure-equal 1 (car closes)))))))

#+sbcl
(progn
  (deftest http2-response-headers-callback-error-resets-only-stream
    (let* ((stream
             (make-instance
              'binary-session-stream
              :input
              (concatenate-octets
               (h2-frame 4 0 0 (octets))
               (h2-frame 1 4 1
                         (h2-header-block (cons ":status" "200")))
               (h2-frame 0 1 1 (octets 65)))))
           (connection (http-kit/http2:make-http2-connection :stream stream))
           (callback-error
             (make-condition 'simple-error
                             :format-control "response rejected"
                             :format-arguments nil))
           (body-callback-count 0)
           (failure nil))
      (unwind-protect
           (progn
             (handler-case
                 (http-kit/http2:send-http2-request-over-connection
                  connection
                  (make-http-request :method "GET"
                                     :uri "https://127.0.0.1/reject")
                  :on-response-headers
                  (lambda (status headers)
                    (declare (ignore headers))
                    (ensure-equal 200 status)
                    (error callback-error))
                  :on-body-chunk
                  (lambda (chunk)
                    (declare (ignore chunk))
                    (incf body-callback-count)))
               (error (condition)
                 (setf failure condition)))
             (ensure-true (eq callback-error failure))
             (ensure-equal 0 body-callback-count)
             (ensure-true
              (http-kit/http2:http2-connection-open-p connection))
             (let ((reset-frame
                     (find-if
                      (lambda (frame)
                        (and (= 3 (http-kit/http2::%h2-frame-type frame))
                             (= 1
                                (http-kit/http2::%h2-frame-stream-id frame))))
                      (h2-output-frames stream))))
               (ensure-true reset-frame)
               (ensure-equal (octets 0 0 0 8)
                             (http-kit/http2::%h2-frame-payload reset-frame))))
        (http-kit/http2:close-http2-connection connection))))

  (deftest http2-manager-response-headers-cancel-registration-cleanup
    (let* ((stream
             (make-instance
              'binary-session-stream
              :input
              (concatenate-octets
               (h2-frame 4 0 0 (octets))
               (h2-frame 1 5 1
                         (h2-header-block (cons ":status" "200")
                                          (cons "content-type" "text/plain"))))))
           (cancel-function nil)
           (cleanup-count 0)
           (response-headers nil)
           (manager
             (http-kit/http2:make-http2-connection-manager
              :open-connection
              (lambda (request &rest arguments)
                (declare (ignore request arguments))
                (http-kit/http2:make-http2-connection :stream stream))))
           (transport
             (http-kit/http2:make-http2-connection-manager-transport manager))
           (response nil))
      (unwind-protect
           (progn
             (setf response
                   (funcall transport
                            (make-http-request :method "GET"
                                               :uri "https://127.0.0.1/manager")
                            :on-response-headers
                            (lambda (status headers)
                              (setf response-headers
                                    (list status
                                          (http-header-value
                                           headers "content-type"))))
                            :on-cancel
                            (lambda (function)
                              (setf cancel-function function)
                              (lambda () (incf cleanup-count)))))
             (ensure-equal 200 (http-response-status response))
             (ensure-equal '(200 "text/plain") response-headers)
             (ensure-true (functionp cancel-function))
             (ensure-equal 1 cleanup-count)
             (ensure-equal 1
                           (http-kit/http2:http2-connection-manager-connection-count
                            manager)))
        (http-kit/http2:close-http2-connection-manager manager)))))
#+sbcl
(deftest http2-response-headers-callback-error-precedes-body-limit-error
    (let* ((callback-error
             (make-condition (quote simple-error)
                             :format-control "headers rejected"
                             :format-arguments nil))
           (headers-entered (sb-thread:make-semaphore :count 0))
           (release-headers (sb-thread:make-semaphore :count 0))
           (release-signaled-p nil)
           (input
             (concatenate-octets
              (h2-frame 4 0 0 (octets))
              (h2-frame 1 4 1 (h2-header-block (cons ":status" "200")))
              (h2-frame 0 0 1 (octets 65))
              (h2-frame 0 1 1 (octets 66))
              (h2-frame 1 5 3 (h2-header-block (cons ":status" "200")))))
           (stream (make-instance (quote binary-session-stream) :input input))
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream :max-body-bytes 1))
           (callback-failure nil)
           (second-failure nil)
           (second-response nil)
           (body-callback-count 0)
           (first-thread nil))
      (unwind-protect
           (progn
             (setf first-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (http-kit/http2:send-http2-request-over-connection
                           connection
                           (make-http-request :method "GET"
                                              :uri "https://race.test/first")
                           :collect-body-p nil
                           :on-response-headers
                           (lambda (status headers)
                             (declare (ignore status headers))
                             (sb-thread:signal-semaphore headers-entered)
                             (sb-thread:wait-on-semaphore
                              release-headers :timeout 10)
                             (error callback-error))
                           :on-body-chunk
                           (lambda (chunk)
                             (declare (ignore chunk))
                             (incf body-callback-count)))
                        (error (condition)
                          (setf callback-failure condition))))))
             (ensure-true
              (sb-thread:wait-on-semaphore headers-entered :timeout 5))
             (handler-case
                 (setf second-response
                       (http-kit/http2:send-http2-request-over-connection
                        connection
                        (make-http-request :method "GET"
                                           :uri "https://race.test/second")))
               (error (condition)
                 (setf second-failure condition)))
             (setf release-signaled-p t)
             (sb-thread:signal-semaphore release-headers)
             (sb-thread:join-thread first-thread)
             (setf first-thread nil)
             (ensure-equal 200 (http-response-status second-response))
             (ensure-false second-failure)
             (ensure-true (eq callback-error callback-failure))
             (ensure-equal 0 body-callback-count)
             (let ((window-updates
                     (remove-if-not
                      (lambda (frame)
                        (= 8 (http-kit/http2::%h2-frame-type frame)))
                      (h2-output-frames stream))))
               (ensure-equal '(0 0)
                             (mapcar #'http-kit/http2::%h2-frame-stream-id
                                     window-updates))
               (ensure-equal '(1 1)
                             (mapcar
                              (lambda (frame)
                                (http-kit/http2::%h2-u32
                                 (http-kit/http2::%h2-frame-payload frame) 0))
                              window-updates))))
        (when first-thread
          (unless release-signaled-p
            (sb-thread:signal-semaphore release-headers))
          (sb-thread:join-thread first-thread))
        (http-kit/http2:close-http2-connection connection))))
#+sbcl
(progn
  (deftest http2-response-headers-callback-success-delays-body-callback
    (let* ((headers-entered (sb-thread:make-semaphore :count 0))
           (release-headers (sb-thread:make-semaphore :count 0))
           (release-signaled-p nil)
           (callback-wait-result nil)
           (input
             (concatenate-octets
              (h2-frame 4 0 0 (octets))
              (h2-frame 1 4 1 (h2-header-block (cons ":status" "200")))
              (h2-frame 0 8 1 (octets 2 65 0 0))
              (h2-frame 1 5 3 (h2-header-block (cons ":status" "200")))
              (h2-frame 1 5 1
                        (h2-header-block (cons "x-trailer" "done")))))
           (stream (make-instance 'binary-session-stream :input input))
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream :max-body-bytes 1))
           (body-callback-count 0)
           (body-callback-chunks nil)
           (first-response nil)
           (first-failure nil)
           (second-response nil)
           (second-failure nil)
           (first-thread nil))
      (unwind-protect
           (progn
             (setf first-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (setf first-response
                                (http-kit/http2:send-http2-request-over-connection
                                 connection
                                 (make-http-request
                                  :method "GET"
                                  :uri "https://race.test/first")
                                 :collect-body-p nil
                                 :on-response-headers
                                 (lambda (status headers)
                                   (declare (ignore headers))
                                   (ensure-equal 200 status)
                                   (sb-thread:signal-semaphore headers-entered)
                                   (setf callback-wait-result
                                         (sb-thread:wait-on-semaphore
                                          release-headers :timeout 10)))
                                 :on-body-chunk
                                 (lambda (chunk)
                                   (incf body-callback-count)
                                   (push (copy-seq chunk)
                                         body-callback-chunks)
                                   (let ((window-updates
                                           (remove-if-not
                                            (lambda (frame)
                                              (= 8
                                                 (http-kit/http2::%h2-frame-type
                                                  frame)))
                                            (h2-output-frames stream))))
                                     (ensure-equal '(0)
                                                   (mapcar
                                                    #'http-kit/http2::%h2-frame-stream-id
                                                    window-updates))
                                     (ensure-equal '(4)
                                                   (mapcar
                                                    (lambda (frame)
                                                      (http-kit/http2::%h2-u32
                                                       (http-kit/http2::%h2-frame-payload
                                                        frame)
                                                       0))
                                                    window-updates))))))
                        (error (condition)
                          (setf first-failure condition))))))
             (ensure-true
              (sb-thread:wait-on-semaphore headers-entered :timeout 5))
             (handler-case
                 (setf second-response
                       (http-kit/http2:send-http2-request-over-connection
                        connection
                        (make-http-request
                         :method "GET"
                         :uri "https://race.test/second")))
               (error (condition)
                 (setf second-failure condition)))
             (ensure-false second-failure)
             (ensure-equal 200 (http-response-status second-response))
             (ensure-equal 0 body-callback-count)
             (let ((window-updates
                     (remove-if-not
                      (lambda (frame)
                        (= 8 (http-kit/http2::%h2-frame-type frame)))
                      (h2-output-frames stream))))
               (ensure-equal '(0)
                             (mapcar #'http-kit/http2::%h2-frame-stream-id
                                     window-updates))
               (ensure-equal '(4)
                             (mapcar
                              (lambda (frame)
                                (http-kit/http2::%h2-u32
                                 (http-kit/http2::%h2-frame-payload frame) 0))
                              window-updates)))
             (setf release-signaled-p t)
             (sb-thread:signal-semaphore release-headers)
             (sb-thread:join-thread first-thread)
             (setf first-thread nil)
             (ensure-false first-failure)
             (ensure-true callback-wait-result)
             (ensure-equal 200 (http-response-status first-response))
             (ensure-equal (octets) (http-response-body first-response))
             (ensure-equal 1 body-callback-count)
             (ensure-equal (list (octets 65)) (nreverse body-callback-chunks))
             (let* ((window-updates
                      (remove-if-not
                       (lambda (frame)
                         (= 8 (http-kit/http2::%h2-frame-type frame)))
                       (h2-output-frames stream)))
                    (connection-updates
                      (remove-if-not
                       (lambda (frame)
                         (and (zerop (http-kit/http2::%h2-frame-stream-id frame))
                              (= 4
                                 (http-kit/http2::%h2-u32
                                  (http-kit/http2::%h2-frame-payload frame) 0))))
                       window-updates)))
               (ensure-equal '(0 1)
                             (mapcar #'http-kit/http2::%h2-frame-stream-id
                                     window-updates))
               (ensure-equal '(4 4)
                             (mapcar
                              (lambda (frame)
                                (http-kit/http2::%h2-u32
                                 (http-kit/http2::%h2-frame-payload frame) 0))
                              window-updates))
               (ensure-equal 1 (length connection-updates))))
        (when first-thread
          (unless release-signaled-p
            (sb-thread:signal-semaphore release-headers))
          (sb-thread:join-thread first-thread))
        (http-kit/http2:close-http2-connection connection))))

  (defclass h2-window-update-peer-stream (binary-session-stream)
    ((extra-data
      :initarg :extra-data
      :initform (h2-frame 0 1 1 (octets 66))
      :reader h2-window-update-peer-extra-data)
     (window-update-stream-id
      :initarg :window-update-stream-id
      :initform 1
      :reader h2-window-update-peer-window-update-stream-id)
     (extra-data-sent-p
      :initform nil
      :accessor h2-window-update-peer-extra-data-sent-p)))

  (defmethod sb-gray:stream-write-sequence
      ((stream h2-window-update-peer-stream) sequence &optional start end)
    (let ((result (call-next-method stream sequence start end)))
      (unless (h2-window-update-peer-extra-data-sent-p stream)
        (when (some (lambda (frame)
                      (and (= 8 (http-kit/http2::%h2-frame-type frame))
                           (= (h2-window-update-peer-window-update-stream-id stream)
                              (http-kit/http2::%h2-frame-stream-id frame))))
                    (h2-output-frames stream))
          (setf (h2-window-update-peer-extra-data-sent-p stream) t
                (slot-value stream 'input)
                (concatenate-octets
                 (binary-session-input stream)
                 (h2-window-update-peer-extra-data stream))))
      result)))

  (deftest http2-public-connection-collects-body-over-initial-flow-window
    (let* ((full-frame-body
             (make-array 16384 :element-type '(unsigned-byte 8)
                         :initial-element 65))
           (short-frame-body
             (make-array 16383 :element-type '(unsigned-byte 8)
                         :initial-element 65))
           (input
             (apply #'concatenate-octets
                    (append
                     (list
                      (h2-frame 4 0 0 (octets))
                      (h2-frame 1 4 1
                                (h2-header-block (cons ":status" "200"))))
                     (make-list 3
                                :initial-element
                                (h2-frame 0 0 1 full-frame-body))
                     (list (h2-frame 0 0 1 short-frame-body)))))
           (stream (make-instance 'h2-window-update-peer-stream :input input))
           (connection (http-kit/http2:make-http2-connection :stream stream))
           (response
             (http-kit/http2:send-http2-request-over-connection
              connection
              (make-http-request :method "GET"
                                 :uri "https://flow.test/large")))
           (body (http-response-body response)))
      (unwind-protect
           (progn
             (ensure-equal 200 (http-response-status response))
             (ensure-equal 65536 (length body))
             (ensure-equal 65 (aref body 0))
             (ensure-equal 66 (aref body (1- (length body))))
             (ensure-true (h2-window-update-peer-extra-data-sent-p stream)))
        (http-kit/http2:close-http2-connection connection)))) (deftest http2-response-headers-end-stream-cancel-preserves-response
    (let* ((headers-entered (sb-thread:make-semaphore :count 0))
           (release-headers (sb-thread:make-semaphore :count 0))
           (release-signaled-p nil)
           (callback-wait-result nil)
           (input
             (concatenate-octets
              (h2-frame 4 0 0 (octets))
              (h2-frame 1 4 1 (h2-header-block (cons ":status" "200")))
              (h2-frame 0 1 1 (octets 65))
              (h2-frame 1 5 3 (h2-header-block (cons ":status" "200")))))
           (stream (make-instance 'binary-session-stream :input input))
           (connection (http-kit/http2:make-http2-connection :stream stream))
           (cancel-function nil)
           (first-response nil)
           (first-failure nil)
           (second-response nil)
           (second-failure nil)
           (first-thread nil))
      (unwind-protect
           (progn
             (setf first-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (setf first-response
                                (http-kit/http2:send-http2-request-over-connection
                                 connection
                                 (make-http-request
                                  :method "GET"
                                  :uri "https://race.test/cancel")
                                 :on-response-headers
                                 (lambda (status headers)
                                   (declare (ignore status headers))
                                   (sb-thread:signal-semaphore headers-entered)
                                   (setf callback-wait-result
                                         (sb-thread:wait-on-semaphore
                                          release-headers :timeout 10)))
                                 :on-cancel
                                 (lambda (function)
                                   (setf cancel-function function))))
                        (error (condition)
                          (setf first-failure condition))))))
             (ensure-true
              (sb-thread:wait-on-semaphore headers-entered :timeout 5))
             (setf second-response
                   (http-kit/http2:send-http2-request-over-connection
                    connection
                    (make-http-request :method "GET"
                                       :uri "https://race.test/second")))
             (ensure-false second-failure)
             (ensure-equal 200 (http-response-status second-response))
             (ensure-true (functionp cancel-function))
             (ensure-equal :response-complete (funcall cancel-function))
             (ensure-equal
              0
              (count-if
               (lambda (frame)
                 (and (= 3 (http-kit/http2::%h2-frame-type frame))
                      (= 1 (http-kit/http2::%h2-frame-stream-id frame))))
               (h2-output-frames stream)))
             (setf release-signaled-p t)
             (sb-thread:signal-semaphore release-headers)
             (sb-thread:join-thread first-thread)
             (setf first-thread nil)
             (ensure-false first-failure)
             (ensure-true callback-wait-result)
             (ensure-equal 200 (http-response-status first-response))
             (ensure-equal (octets 65) (http-response-body first-response)))
        (when first-thread
          (unless release-signaled-p
            (sb-thread:signal-semaphore release-headers))
          (sb-thread:join-thread first-thread))
        (http-kit/http2:close-http2-connection connection)))) (deftest http2-response-headers-callback-success-body-limit-is-stream-local
    (let* ((headers-entered (sb-thread:make-semaphore :count 0))
           (release-headers (sb-thread:make-semaphore :count 0))
           (release-signaled-p nil)
           (callback-wait-result nil)
           (input
             (concatenate-octets
              (h2-frame 4 0 0 (octets))
              (h2-frame 1 4 1 (h2-header-block (cons ":status" "200")))
              (h2-frame 0 0 1 (octets 65))
              (h2-frame 0 0 1 (octets 66))
              (h2-frame 0 1 1 (octets 67))
              (h2-frame 1 5 3 (h2-header-block (cons ":status" "200")))))
           (stream (make-instance 'binary-session-stream :input input))
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream :max-body-bytes 1))
           (first-failure nil)
           (second-response nil)
           (second-failure nil)
           (first-thread nil))
      (unwind-protect
           (progn
             (setf first-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (http-kit/http2:send-http2-request-over-connection
                           connection
                           (make-http-request :method "GET"
                                              :uri "https://race.test/limited")
                           :on-response-headers
                           (lambda (status headers)
                             (declare (ignore status headers))
                             (sb-thread:signal-semaphore headers-entered)
                             (setf callback-wait-result
                                   (sb-thread:wait-on-semaphore
                                    release-headers :timeout 10))))
                        (error (condition)
                          (setf first-failure condition))))))
             (ensure-true
              (sb-thread:wait-on-semaphore headers-entered :timeout 5))
             (handler-case
                 (setf second-response
                       (http-kit/http2:send-http2-request-over-connection
                        connection
                        (make-http-request :method "GET"
                                           :uri "https://race.test/second")))
               (error (condition)
                 (setf second-failure condition)))
             (setf release-signaled-p t)
             (sb-thread:signal-semaphore release-headers)
             (sb-thread:join-thread first-thread)
             (setf first-thread nil)
             (ensure-true callback-wait-result)
             (ensure-true (typep first-failure 'http-size-limit-exceeded))
             (ensure-equal 1
                           (http-size-limit-exceeded-limit first-failure))
             (ensure-equal 2
                           (http-size-limit-exceeded-observed first-failure))
             (ensure-equal :body
                           (http-size-limit-exceeded-kind first-failure))
             (ensure-false second-failure)
             (ensure-equal 200 (http-response-status second-response))
             (ensure-equal (octets) (http-response-body second-response))
             (ensure-false
              (http-kit/http2::%http2-connection-closed-p connection))
             (let ((reset-frame
                     (find-if
                      (lambda (frame)
                        (and (= 3 (http-kit/http2::%h2-frame-type frame))
                             (= 1
                                (http-kit/http2::%h2-frame-stream-id frame))))
                      (h2-output-frames stream))))
               (ensure-true reset-frame)
               (ensure-equal
                8
                (http-kit/http2::%h2-u32
                 (http-kit/http2::%h2-frame-payload reset-frame) 0))))
        (when first-thread
          (unless release-signaled-p
            (sb-thread:signal-semaphore release-headers))
          (sb-thread:join-thread first-thread))
        (http-kit/http2:close-http2-connection connection)))) (deftest http2-cancel-keeps-completed-response-entry
    (let* ((stream (make-instance 'binary-session-stream :input (octets)))
           (connection (http-kit/http2:make-http2-connection :stream stream))
           (entry
             (http-kit/http2::%make-h2-batch-entry :stream-id 1)))
      (unwind-protect
           (progn
             (setf (http-kit/http2::%http2-connection-next-stream-id connection) 3
                   (http-kit/http2::%http2-connection-scheduler-entries connection)
                   (list entry)
                   (http-kit/http2::%h2-batch-entry-response entry) :completed)
             (ensure-true
              (http-kit/http2::%h2-cancel-http2-stream
               connection 1 :only-if-active-p t))
             (ensure-equal
              (list entry)
              (http-kit/http2::%http2-connection-scheduler-entries connection))
             (ensure-false
              (http-kit/http2::%h2-batch-entry-local-retired-p entry)))
        (http-kit/http2:close-http2-connection connection)))) (deftest http2-cancel-after-body-sent-preserves-completed-response
    (let* ((stream (make-instance 'binary-session-stream :input (octets)))
           (connection (http-kit/http2:make-http2-connection :stream stream))
           (entry
             (http-kit/http2::%make-h2-batch-entry :stream-id 1)))
      (unwind-protect
           (progn
             (setf (http-kit/http2::%http2-connection-next-stream-id connection) 3
                   (http-kit/http2::%http2-connection-scheduler-entries connection)
                   (list entry)
                   (http-kit/http2::%h2-batch-entry-response entry) :completed
                   (http-kit/http2::%h2-batch-entry-body-sent-p entry) t)
             (ensure-true
              (http-kit/http2:cancel-http2-stream connection 1))
             (ensure-equal
              (list entry)
              (http-kit/http2::%http2-connection-scheduler-entries connection))
             (ensure-false
              (http-kit/http2::%h2-batch-entry-local-retired-p entry))
             (ensure-false
              (http-kit/http2::%h2-batch-entry-request-body-abandoned-p entry))
             (ensure-equal 0 (length (binary-session-output stream))))
        (http-kit/http2:close-http2-connection connection))))

  (deftest http2-cancel-does-not-preserve-entry-without-completed-response
    (let* ((stream (make-instance 'binary-session-stream :input (octets)))
           (connection (http-kit/http2:make-http2-connection :stream stream))
           (entry (http-kit/http2::%make-h2-batch-entry :stream-id 1)))
      (unwind-protect
           (progn
             (setf (http-kit/http2::%http2-connection-next-stream-id connection) 3
                   (http-kit/http2::%http2-connection-scheduler-entries connection)
                   (list entry)
                   (http-kit/http2::%h2-batch-entry-request-state entry)
                   :written)
             (ensure-true
              (http-kit/http2::%h2-cancel-http2-stream
               connection 1 :only-if-active-p t))
             (ensure-true (http-kit/http2::%h2-batch-entry-local-retired-p entry))
             (let ((output (binary-session-output stream)))
               (ensure-equal 13 (length output))
               (ensure-equal 4 (http-kit/http2::%h2-u24 output 0))
               (ensure-equal 3 (aref output 3))
               (ensure-equal 1 (http-kit/http2::%h2-u32 output 5))))
        (http-kit/http2:close-http2-connection connection))))
)

#+sbcl
(deftest http2-public-peer-capacity-reader-cancel-preserves-partial-frame-and-reuses-connection
  (let* ((initial-settings
           (h2-frame 4 0 0 (octets 0 3 0 0 0 1)))
         (stream
           (make-instance 'h2-manager-capacity-peer-stream
                          :input initial-settings))
         (connection nil)
         (manager
           (http-kit/http2:make-http2-connection-manager
            :open-connection
            (lambda (request &key timeout deadline proxy-plan proxy)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (setf connection
                    (http-kit/http2:make-http2-connection
                     :stream stream
                     :max-concurrent-streams 2
                     :close-stream
                     (lambda (closed-stream)
                       (ensure-true (eq stream closed-stream))
                       (h2-manager-capacity-peer-close stream)))))))
         (transport
           (http-kit/http2:make-http2-connection-manager-transport manager))
         (first-headers-entered (sb-thread:make-semaphore :count 0))
         (release-first-headers (sb-thread:make-semaphore :count 0))
         (first-release-signaled-p nil)
         (first-response nil)
         (first-failure nil)
         (first-done (sb-thread:make-semaphore :count 0))
         (second-cancel-registered (sb-thread:make-semaphore :count 0))
         (second-cancel nil)
         (second-response nil)
         (second-failure nil)
         (second-done (sb-thread:make-semaphore :count 0))
         (third-response nil)
         (third-failure nil)
         (third-done (sb-thread:make-semaphore :count 0))
         (first-thread nil)
         (second-thread nil)
         (third-thread nil))
    (unwind-protect
         (progn
           (setf first-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (setf first-response
                                   (sb-ext:with-timeout 10
                                     (funcall
                                      transport
                                      (make-http-request
                                       :method "GET"
                                       :uri "http://peer-reader.test/first")
                                      :connection-key "peer-reader-cancel.test"
                                      :on-response-headers
                                      (lambda (status headers)
                                        (declare (ignore headers))
                                        (ensure-equal 200 status)
                                        (sb-thread:signal-semaphore
                                         first-headers-entered)
                                        (ensure-true
                                         (sb-thread:wait-on-semaphore
                                          release-first-headers :timeout 10))))))
                           (error (condition)
                             (setf first-failure condition)))
                      (sb-thread:signal-semaphore first-done)))))
           (ensure-true
            (sb-thread:wait-on-semaphore first-headers-entered :timeout 5))
           (ensure-equal '(1)
                         (h2-manager-capacity-peer-output-header-ids stream))
           (ensure-true
            (sb-thread:wait-on-semaphore
             (h2-manager-capacity-peer-header-available stream) :timeout 0.001))
           (loop while
                 (sb-thread:wait-on-semaphore
                  (h2-manager-capacity-peer-input-read-waiting stream)
                  :timeout 0.000001))
           (setf second-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (setf second-response
                                   (sb-ext:with-timeout 10
                                     (funcall
                                      transport
                                      (make-http-request
                                       :method "GET"
                                       :uri "http://peer-reader.test/cancelled")
                                      :connection-key "peer-reader-cancel.test"
                                      :on-cancel
                                      (lambda (cancel-function)
                                        (setf second-cancel cancel-function)
                                        (sb-thread:signal-semaphore
                                         second-cancel-registered)
                                        nil))))
                           (error (condition)
                             (setf second-failure condition)))
                      (sb-thread:signal-semaphore second-done)))))
           (ensure-true
            (sb-thread:wait-on-semaphore second-cancel-registered :timeout 5))
           (ensure-true
            (sb-thread:wait-on-semaphore
             (h2-manager-capacity-peer-input-read-waiting stream) :timeout 5))
           (let ((ping-frame (h2-frame 6 0 0 (octets 1 2 3 4 5 6 7 8))))
             (h2-manager-capacity-peer-feed stream (subseq ping-frame 0 4))
             (ensure-true
              (sb-thread:wait-on-semaphore
               (h2-manager-capacity-peer-input-read-waiting stream) :timeout 5))
             (ensure-true (functionp second-cancel))
             (funcall second-cancel))
           (ensure-true (sb-thread:wait-on-semaphore second-done :timeout 1))
           (ensure-false second-response)
           (ensure-true (typep second-failure 'http-kit:http-connection-error))
           (let ((cause
                   (http-kit:http-connection-error-cause second-failure)))
             (ensure-true (consp cause))
             (ensure-equal :local-cancel (first cause)))
           (ensure-equal '(1)
                         (h2-manager-capacity-peer-output-header-ids stream))
           (ensure-false
            (some (lambda (frame)
                    (and (= 3 (http-kit/http2::%h2-frame-type frame))
                         (= 3 (http-kit/http2::%h2-frame-stream-id frame))))
                  (h2-output-frames stream)))
           (ensure-true (http-kit/http2:http2-connection-open-p connection))
           (setf first-release-signaled-p t)
           (sb-thread:signal-semaphore release-first-headers)
           (let ((ping-frame (h2-frame 6 0 0 (octets 1 2 3 4 5 6 7 8))))
             (h2-manager-capacity-peer-feed
              stream
              (concatenate-octets
               (subseq ping-frame 4)
               (h2-frame 0 1 1 (octets 65)))))
           (ensure-true (sb-thread:wait-on-semaphore first-done :timeout 5))
           (sb-thread:join-thread first-thread)
           (setf first-thread nil)
           (ensure-false first-failure)
           (ensure-equal 200 (http-response-status first-response))
           (ensure-equal (octets 65) (http-response-body first-response))
           (setf third-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (setf third-response
                                   (sb-ext:with-timeout 10
                                     (funcall
                                      transport
                                      (make-http-request
                                       :method "GET"
                                       :uri "http://peer-reader.test/after-cancel")
                                      :connection-key "peer-reader-cancel.test")))
                           (error (condition)
                             (setf third-failure condition)))
                      (sb-thread:signal-semaphore third-done)))))
           (ensure-true
            (sb-thread:wait-on-semaphore
             (h2-manager-capacity-peer-header-available stream) :timeout 5))
           (ensure-equal '(1 3)
                         (h2-manager-capacity-peer-output-header-ids stream))
           (h2-manager-capacity-peer-feed
            stream (h2-frame 0 1 3 (octets)))
           (ensure-true (sb-thread:wait-on-semaphore third-done :timeout 5))
           (sb-thread:join-thread third-thread)
           (setf third-thread nil)
           (ensure-false third-failure)
           (ensure-equal 201 (http-response-status third-response))
           (ensure-true (http-kit/http2:http2-connection-open-p connection)))
      (unless first-release-signaled-p
        (setf first-release-signaled-p t)
        (sb-thread:signal-semaphore release-first-headers))
      (h2-manager-capacity-peer-close stream)
      (http-kit/http2:close-http2-connection-manager manager)
      (dolist (thread (list first-thread second-thread third-thread))
        (when thread
          (sb-thread:join-thread thread))))))

#+sbcl
(deftest http2-public-peer-capacity-wait-times-out-on-an-active-request
  (let* ((stream
           (make-instance 'h2-manager-capacity-peer-stream
                          :input (h2-frame 4 0 0 (octets 0 3 0 0 0 1))))
         (connection nil)
         (manager
           (http-kit/http2:make-http2-connection-manager
            :open-connection
            (lambda (request &key timeout deadline proxy-plan proxy)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (setf connection
                    (http-kit/http2:make-http2-connection
                     :stream stream
                     :max-concurrent-streams 2
                     :close-stream
                     (lambda (closed-stream)
                       (ensure-true (eq stream closed-stream))
                       (h2-manager-capacity-peer-close stream)))))))
         (transport
           (http-kit/http2:make-http2-connection-manager-transport manager))
         (first-headers-entered (sb-thread:make-semaphore :count 0))
         (release-first-headers (sb-thread:make-semaphore :count 0))
         (first-release-signaled-p nil)
         (first-response nil)
         (first-failure nil)
         (first-done (sb-thread:make-semaphore :count 0))
         (second-failure nil)
         (second-response nil)
         (second-done (sb-thread:make-semaphore :count 0))
         (first-thread nil)
         (second-thread nil))
    (unwind-protect
         (progn
           (setf first-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (setf first-response
                                   (sb-ext:with-timeout 10
                                     (funcall
                                      transport
                                      (make-http-request
                                       :method "GET"
                                       :uri "http://peer-deadline.test/first")
                                      :connection-key "peer-deadline.test"
                                      :on-response-headers
                                      (lambda (status headers)
                                        (declare (ignore headers))
                                        (ensure-equal 200 status)
                                        (sb-thread:signal-semaphore
                                         first-headers-entered)
                                        (ensure-true
                                         (sb-thread:wait-on-semaphore
                                          release-first-headers :timeout 10))))))
                           (error (condition)
                             (setf first-failure condition)))
                      (sb-thread:signal-semaphore first-done)))))
           (ensure-true
            (sb-thread:wait-on-semaphore first-headers-entered :timeout 5))
           (loop while
                 (sb-thread:wait-on-semaphore
                  (h2-manager-capacity-peer-input-read-waiting stream)
                  :timeout 0.000001))
           (setf second-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (setf second-response
                                   (sb-ext:with-timeout 5
                                     (funcall
                                      transport
                                      (make-http-request
                                       :method "GET"
                                       :uri "http://peer-deadline.test/timeout")
                                      :connection-key "peer-deadline.test"
                                      :timeout 0.2)))
                           (error (condition)
                             (setf second-failure condition)))
                      (sb-thread:signal-semaphore second-done)))))
           (ensure-true
            (sb-thread:wait-on-semaphore
             (h2-manager-capacity-peer-input-read-waiting stream) :timeout 5))
           (ensure-true (sb-thread:wait-on-semaphore second-done :timeout 2))
           (ensure-false second-response)
           (ensure-true (typep second-failure 'http-kit:http-timeout))
           (ensure-true (http-kit/http2:http2-connection-open-p connection))
           (setf first-release-signaled-p t)
           (sb-thread:signal-semaphore release-first-headers)
           (h2-manager-capacity-peer-feed
            stream (h2-frame 0 1 1 (octets 65)))
           (ensure-true (sb-thread:wait-on-semaphore first-done :timeout 5))
           (sb-thread:join-thread first-thread)
           (setf first-thread nil)
           (ensure-false first-failure)
           (ensure-equal 200 (http-response-status first-response))
           (ensure-equal (octets 65) (http-response-body first-response)))
      (unless first-release-signaled-p
        (setf first-release-signaled-p t)
        (sb-thread:signal-semaphore release-first-headers))
      (h2-manager-capacity-peer-close stream)
      (http-kit/http2:close-http2-connection-manager manager)
      (dolist (thread (list first-thread second-thread))
        (when thread
          (sb-thread:join-thread thread))))))

#+sbcl
(deftest http2-public-peer-capacity-wait-exits-on-connection-close
  (let* ((stream
           (make-instance 'h2-manager-capacity-peer-stream
                          :input (h2-frame 4 0 0 (octets 0 3 0 0 0 1))))
         (connection nil)
         (manager
           (http-kit/http2:make-http2-connection-manager
            :open-connection
            (lambda (request &key timeout deadline proxy-plan proxy)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (setf connection
                    (http-kit/http2:make-http2-connection
                     :stream stream
                     :max-concurrent-streams 2
                     :close-stream
                     (lambda (closed-stream)
                       (ensure-true (eq stream closed-stream))
                       (h2-manager-capacity-peer-close stream)))))))
         (transport
           (http-kit/http2:make-http2-connection-manager-transport manager))
         (first-headers-entered (sb-thread:make-semaphore :count 0))
         (release-first-headers (sb-thread:make-semaphore :count 0))
         (first-release-signaled-p nil)
         (first-response nil)
         (first-failure nil)
         (first-done (sb-thread:make-semaphore :count 0))
         (second-response nil)
         (second-failure nil)
         (second-done (sb-thread:make-semaphore :count 0))
         (close-done (sb-thread:make-semaphore :count 0))
         (first-thread nil)
         (second-thread nil)
         (close-thread nil))
    (unwind-protect
         (progn
           (setf first-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (setf first-response
                                   (sb-ext:with-timeout 10
                                     (funcall
                                      transport
                                      (make-http-request
                                       :method "GET"
                                       :uri "http://peer-close.test/first")
                                      :connection-key "peer-close.test"
                                      :on-response-headers
                                      (lambda (status headers)
                                        (declare (ignore headers))
                                        (ensure-equal 200 status)
                                        (sb-thread:signal-semaphore
                                         first-headers-entered)
                                        (ensure-true
                                         (sb-thread:wait-on-semaphore
                                          release-first-headers :timeout 10))))))
                           (error (condition)
                             (setf first-failure condition)))
                      (sb-thread:signal-semaphore first-done)))))
           (ensure-true
            (sb-thread:wait-on-semaphore first-headers-entered :timeout 5))
           (loop while
                 (sb-thread:wait-on-semaphore
                  (h2-manager-capacity-peer-input-read-waiting stream)
                  :timeout 0.000001))
           (setf second-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (setf second-response
                                   (sb-ext:with-timeout 10
                                     (funcall
                                      transport
                                      (make-http-request
                                       :method "GET"
                                       :uri "http://peer-close.test/waiter")
                                      :connection-key "peer-close.test")))
                           (error (condition)
                             (setf second-failure condition)))
                      (sb-thread:signal-semaphore second-done)))))
           (ensure-true
            (sb-thread:wait-on-semaphore
             (h2-manager-capacity-peer-input-read-waiting stream) :timeout 5))
           (setf close-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                         (http-kit/http2:close-http2-connection connection)
                      (sb-thread:signal-semaphore close-done)))))
           (ensure-true (sb-thread:wait-on-semaphore close-done :timeout 1))
           (ensure-true (sb-thread:wait-on-semaphore second-done :timeout 1))
           (ensure-false second-response)
           (ensure-true (typep second-failure 'http-kit:http-connection-error))
           (ensure-false (http-kit/http2:http2-connection-open-p connection))
           (setf first-release-signaled-p t)
           (sb-thread:signal-semaphore release-first-headers)
           (ensure-true (sb-thread:wait-on-semaphore first-done :timeout 5))
           (sb-thread:join-thread first-thread)
           (setf first-thread nil)
           (ensure-true first-failure))
      (unless first-release-signaled-p
        (setf first-release-signaled-p t)
        (sb-thread:signal-semaphore release-first-headers))
      (h2-manager-capacity-peer-close stream)
      (http-kit/http2:close-http2-connection-manager manager)
      (dolist (thread (list first-thread second-thread close-thread))
        (when thread
          (sb-thread:join-thread thread))))))

#+sbcl
(deftest http2-public-connection-manager-reserves-racing-peer-capacity-slots
  (let* ((stream
           (make-instance 'h2-manager-capacity-peer-stream
                          :input (h2-frame 4 0 0 (octets 0 3 0 0 0 1))
                          :response-end-stream-p t))
         (connection nil)
         (manager
           (http-kit/http2:make-http2-connection-manager
            :open-connection
            (lambda (request &key timeout deadline proxy-plan proxy)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (setf connection
                    (http-kit/http2:make-http2-connection
                     :stream stream
                     :max-concurrent-streams 8
                     :close-stream
                     (lambda (closed-stream)
                       (ensure-true (eq stream closed-stream))
                       (h2-manager-capacity-peer-close stream)))))))
         (transport
           (http-kit/http2:make-http2-connection-manager-transport manager))
         (first-headers-entered (sb-thread:make-semaphore :count 0))
         (release-first-headers (sb-thread:make-semaphore :count 0))
         (first-release-signaled-p nil)
         (first-response nil)
         (first-failure nil)
         (first-done (sb-thread:make-semaphore :count 0))
         (waiters-registered (sb-thread:make-semaphore :count 0))
         (responses (make-array 4 :initial-element nil))
         (failures (make-array 4 :initial-element nil))
         (waiter-done
           (make-array 4 :initial-contents
                       (loop repeat 4 collect
                         (sb-thread:make-semaphore :count 0))))
         (waiter-threads (make-array 4 :initial-element nil))
         (first-thread nil))
    (unwind-protect
         (progn
           (setf first-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (setf first-response
                                   (sb-ext:with-timeout 10
                                     (funcall
                                      transport
                                      (make-http-request
                                       :method "GET"
                                       :uri "http://peer-race.test/first")
                                      :connection-key "peer-race.test"
                                      :on-response-headers
                                      (lambda (status headers)
                                        (declare (ignore headers))
                                        (ensure-equal 200 status)
                                        (sb-thread:signal-semaphore
                                         first-headers-entered)
                                        (ensure-true
                                         (sb-thread:wait-on-semaphore
                                          release-first-headers :timeout 10))))))
                           (error (condition)
                             (setf first-failure condition)))
                      (sb-thread:signal-semaphore first-done)))))
           (ensure-true
            (sb-thread:wait-on-semaphore first-headers-entered :timeout 5))
           (ensure-true
            (sb-thread:wait-on-semaphore
             (h2-manager-capacity-peer-header-available stream) :timeout 5))
           (loop for index below 4
                 do (setf (aref waiter-threads index)
                          (sb-thread:make-thread
                           (let ((request-index index))
                             (lambda ()
                               (unwind-protect
                                    (handler-case
                                        (setf (aref responses request-index)
                                              (sb-ext:with-timeout 10
                                                (funcall
                                                 transport
                                                 (make-http-request
                                                  :method "GET"
                                                  :uri
                                                  (format nil
                                                          "http://peer-race.test/w~D"
                                                          request-index))
                                                 :connection-key "peer-race.test"
                                                 :on-cancel
                                                 (lambda (cancel-function)
                                                   (declare (ignore cancel-function))
                                                   (sb-thread:signal-semaphore
                                                    waiters-registered)
                                                   nil))))
                                      (error (condition)
                                        (setf (aref failures request-index)
                                              condition)))
                                 (sb-thread:signal-semaphore
                                  (aref waiter-done request-index))))))))
           (loop repeat 4
                 do (ensure-true
                     (sb-thread:wait-on-semaphore waiters-registered :timeout 5)))
           (loop while
                 (sb-thread:wait-on-semaphore
                  (h2-manager-capacity-peer-input-read-waiting stream)
                  :timeout 0.000001))
           (ensure-true
            (sb-thread:wait-on-semaphore
             (h2-manager-capacity-peer-input-read-waiting stream) :timeout 5))
           (h2-manager-capacity-peer-feed
            stream (h2-frame 4 0 0 (octets 0 3 0 0 0 5)))
           (loop repeat 4
                 do (ensure-true
                     (sb-thread:wait-on-semaphore
                      (h2-manager-capacity-peer-header-available stream)
                      :timeout 5)))
           (loop for index below 4
                 do (ensure-true
                     (sb-thread:wait-on-semaphore
                      (aref waiter-done index) :timeout 5)))
           (loop for index below 4
                 do (sb-thread:join-thread (aref waiter-threads index))
                    (setf (aref waiter-threads index) nil)
                    (ensure-false (aref failures index))
                    (ensure-true
                     (member (http-response-status (aref responses index))
                             '(200 201 202))))
           (setf first-release-signaled-p t)
           (sb-thread:signal-semaphore release-first-headers)
           (ensure-true (sb-thread:wait-on-semaphore first-done :timeout 5))
           (sb-thread:join-thread first-thread)
           (setf first-thread nil)
           (ensure-false first-failure)
           (ensure-equal 200 (http-response-status first-response))
           (ensure-equal '(1 3 5 7 9)
                         (h2-manager-capacity-peer-output-header-ids stream))
           (ensure-true (http-kit/http2:http2-connection-open-p connection))
           (ensure-equal
            1
            (http-kit/http2:http2-connection-manager-connection-count manager)))
      (unless first-release-signaled-p
        (setf first-release-signaled-p t)
        (sb-thread:signal-semaphore release-first-headers))
      (h2-manager-capacity-peer-close stream)
      (http-kit/http2:close-http2-connection-manager manager)
      (when first-thread
        (sb-thread:join-thread first-thread))
      (loop for thread across waiter-threads
            when thread do (sb-thread:join-thread thread)))))

#+sbcl
(deftest http2-public-connection-manager-keeps-peer-streams-after-request-header-limit
  (let* ((stream
           (make-instance 'h2-manager-capacity-peer-stream
                          :input (h2-frame 4 0 0 (octets))
                          :response-end-stream-p nil))
         (connection nil)
         (manager
           (http-kit/http2:make-http2-connection-manager
            :open-connection
            (lambda (request &key timeout deadline proxy-plan proxy)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (setf connection
                    (http-kit/http2:make-http2-connection
                     :stream stream
                     :max-concurrent-streams 2
                     :max-header-bytes 512
                     :close-stream
                     (lambda (closed-stream)
                       (ensure-true (eq stream closed-stream))
                       (h2-manager-capacity-peer-close stream)))))))
         (transport
           (http-kit/http2:make-http2-connection-manager-transport manager))
         (first-headers-entered (sb-thread:make-semaphore :count 0))
         (release-first-headers (sb-thread:make-semaphore :count 0))
         (first-release-signaled-p nil)
         (first-response nil)
         (first-failure nil)
         (first-done (sb-thread:make-semaphore :count 0))
         (third-response nil)
         (third-failure nil)
         (third-done (sb-thread:make-semaphore :count 0))
         (first-thread nil)
         (third-thread nil))
    (unwind-protect
         (progn
           (setf first-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (setf first-response
                                   (sb-ext:with-timeout 10
                                     (funcall
                                      transport
                                      (make-http-request
                                       :method "GET"
                                       :uri "http://header-limit.test/active")
                                      :connection-key "header-limit.test"
                                      :on-response-headers
                                      (lambda (status headers)
                                        (declare (ignore headers))
                                        (ensure-equal 200 status)
                                        (sb-thread:signal-semaphore
                                         first-headers-entered)
                                        (ensure-true
                                         (sb-thread:wait-on-semaphore
                                          release-first-headers :timeout 10))))))
                           (error (condition)
                             (setf first-failure condition)))
                      (sb-thread:signal-semaphore first-done)))))
           (ensure-true
            (sb-thread:wait-on-semaphore first-headers-entered :timeout 5))
           (ensure-true
            (sb-thread:wait-on-semaphore
             (h2-manager-capacity-peer-header-available stream) :timeout 5))
           (let ((failure
                   (handler-case
                       (progn
                         (funcall
                          transport
                          (make-http-request
                           :method "GET"
                           :uri "http://header-limit.test/oversized"
                           :headers
                           (list
                            (make-http-header
                             "x-too-large"
                             (make-string 1024 :initial-element #\a))))
                          :connection-key "header-limit.test")
                         nil)
                     (error (condition) condition))))
             (ensure-true (typep failure 'http-kit:http-size-limit-exceeded))
             (ensure-equal :headers
                           (http-kit:http-size-limit-exceeded-kind failure)))
           (ensure-equal '(1)
                         (h2-manager-capacity-peer-output-header-ids stream))
           (ensure-true (http-kit/http2:http2-connection-open-p connection))
           (setf first-release-signaled-p t)
           (sb-thread:signal-semaphore release-first-headers)
           (h2-manager-capacity-peer-feed
            stream (h2-frame 0 1 1 (octets 65)))
           (ensure-true (sb-thread:wait-on-semaphore first-done :timeout 5))
           (sb-thread:join-thread first-thread)
           (setf first-thread nil)
           (ensure-false first-failure)
           (ensure-equal (octets 65) (http-response-body first-response))
           (setf third-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (setf third-response
                                   (sb-ext:with-timeout 10
                                     (funcall
                                      transport
                                      (make-http-request
                                       :method "GET"
                                       :uri "http://header-limit.test/after")
                                      :connection-key "header-limit.test")))
                           (error (condition)
                             (setf third-failure condition)))
                      (sb-thread:signal-semaphore third-done)))))
           (ensure-true
            (sb-thread:wait-on-semaphore
             (h2-manager-capacity-peer-header-available stream) :timeout 5))
           (let ((header-ids
                   (h2-manager-capacity-peer-output-header-ids stream)))
             (ensure-equal 2 (length header-ids))
             (ensure-equal 1 (first header-ids))
             (ensure-true (> (second header-ids) 1)))
           (h2-manager-capacity-peer-feed
            stream (h2-frame 0 1 3 (octets)))
           (ensure-true (sb-thread:wait-on-semaphore third-done :timeout 5))
           (sb-thread:join-thread third-thread)
           (setf third-thread nil)
           (ensure-false third-failure)
           (ensure-equal 201 (http-response-status third-response))
           (ensure-true (http-kit/http2:http2-connection-open-p connection))
           (ensure-equal
            1
            (http-kit/http2:http2-connection-manager-connection-count manager)))
      (unless first-release-signaled-p
        (setf first-release-signaled-p t)
        (sb-thread:signal-semaphore release-first-headers))
      (h2-manager-capacity-peer-close stream)
      (http-kit/http2:close-http2-connection-manager manager)
      (dolist (thread (list first-thread third-thread))
        (when thread
          (sb-thread:join-thread thread))))))

#+sbcl
(deftest http2-public-extended-connect-initial-settings-wait-can-be-cancelled
  (let* ((stream (make-instance 'h2-manager-capacity-peer-stream
                                :input (octets)))
         (connection nil)
         (manager
           (http-kit/http2:make-http2-connection-manager
            :open-connection
            (lambda (request &key timeout deadline proxy-plan proxy)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (setf connection
                    (http-kit/http2:make-http2-connection
                     :stream stream
                     :close-stream
                     (lambda (closed-stream)
                       (ensure-true (eq stream closed-stream))
                       (h2-manager-capacity-peer-close stream)))))))
         (transport
           (http-kit/http2:make-http2-connection-manager-transport manager))
         (cancel-registered (sb-thread:make-semaphore :count 0))
         (cancel-function nil)
         (response nil)
         (failure nil)
         (done (sb-thread:make-semaphore :count 0))
         (thread nil))
    (unwind-protect
         (progn
           (setf thread
                 (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (setf response
                                   (sb-ext:with-timeout 10
                                     (funcall
                                      transport
                                      (make-http-request
                                       :method "CONNECT"
                                       :protocol "websocket"
                                       :uri "https://extended-settings.test/socket")
                                      :connection-key "extended-settings-cancel.test"
                                      :on-cancel
                                      (lambda (function)
                                        (setf cancel-function function)
                                        (sb-thread:signal-semaphore
                                         cancel-registered)
                                        nil))))
                           (error (condition)
                             (setf failure condition)))
                      (sb-thread:signal-semaphore done)))))
           (ensure-true
            (sb-thread:wait-on-semaphore cancel-registered :timeout 5))
           (ensure-true
            (sb-thread:wait-on-semaphore
             (h2-manager-capacity-peer-input-read-waiting stream) :timeout 5))
           (ensure-true (functionp cancel-function))
           (funcall cancel-function)
           (ensure-true (sb-thread:wait-on-semaphore done :timeout 1))
           (ensure-false response)
           (ensure-true (typep failure 'http-kit:http-connection-error))
           (ensure-equal :local-cancel
                         (first
                          (http-kit:http-connection-error-cause failure)))
           (ensure-true (http-kit/http2:http2-connection-open-p connection))
           (ensure-equal nil
                         (h2-manager-capacity-peer-output-header-ids stream)))
      (h2-manager-capacity-peer-close stream)
      (http-kit/http2:close-http2-connection-manager manager)
      (when thread
        (sb-thread:join-thread thread)))))

#+sbcl
(deftest http2-public-extended-connect-initial-settings-wait-can-be-closed
  (let* ((stream (make-instance 'h2-manager-capacity-peer-stream
                                :input (octets)))
         (connection nil)
         (manager
           (http-kit/http2:make-http2-connection-manager
            :open-connection
            (lambda (request &key timeout deadline proxy-plan proxy)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (setf connection
                    (http-kit/http2:make-http2-connection
                     :stream stream
                     :close-stream
                     (lambda (closed-stream)
                       (ensure-true (eq stream closed-stream))
                       (h2-manager-capacity-peer-close stream)))))))
         (transport
           (http-kit/http2:make-http2-connection-manager-transport manager))
         (response nil)
         (failure nil)
         (request-done (sb-thread:make-semaphore :count 0))
         (close-done (sb-thread:make-semaphore :count 0))
         (request-thread nil)
         (close-thread nil))
    (unwind-protect
         (progn
           (setf request-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (setf response
                                   (sb-ext:with-timeout 10
                                     (funcall
                                      transport
                                      (make-http-request
                                       :method "CONNECT"
                                       :protocol "websocket"
                                       :uri "https://extended-close.test/socket")
                                      :connection-key "extended-settings-close.test")))
                           (error (condition)
                             (setf failure condition)))
                      (sb-thread:signal-semaphore request-done)))))
           (ensure-true
            (sb-thread:wait-on-semaphore
             (h2-manager-capacity-peer-input-read-waiting stream) :timeout 5))
           (setf close-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                         (http-kit/http2:close-http2-connection connection)
                      (sb-thread:signal-semaphore close-done)))))
           (ensure-true (sb-thread:wait-on-semaphore close-done :timeout 1))
           (ensure-true (sb-thread:wait-on-semaphore request-done :timeout 1))
           (ensure-false response)
           (ensure-true failure)
           (ensure-false (http-kit/http2:http2-connection-open-p connection))
           (ensure-equal nil
                         (h2-manager-capacity-peer-output-header-ids stream)))
      (h2-manager-capacity-peer-close stream)
      (http-kit/http2:close-http2-connection-manager manager)
      (dolist (thread (list request-thread close-thread))
        (when thread
          (sb-thread:join-thread thread))))))

#+sbcl
(deftest http2-public-connection-coalesces-delayed-body-callbacks
  (let* ((headers-entered (sb-thread:make-semaphore :count 0))
         (release-headers (sb-thread:make-semaphore :count 0))
         (release-signaled-p nil)
         (header-callback-wait-result nil)
         (one-byte-frame (h2-frame 0 0 1 (octets 65)))
         (input
           (apply #'concatenate-octets
                  (append
                   (list (h2-frame 4 0 0 (octets))
                         (h2-frame 1 4 1
                                   (h2-header-block (cons ":status" "200"))))
                   (make-list 31 :initial-element one-byte-frame)
                   (list (h2-frame 0 1 1 (octets 65))
                         (h2-frame 1 5 3
                                   (h2-header-block (cons ":status" "200")))))))
         (stream (make-instance 'binary-session-stream :input input))
         (connection
           (http-kit/http2:make-http2-connection :stream stream))
         (body-callback-count 0)
         (body-callback-chunks nil)
         (first-response nil)
         (first-failure nil)
         (second-response nil)
         (first-thread nil))
    (unwind-protect
         (progn
           (setf first-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (setf first-response
                              (http-kit/http2:send-http2-request-over-connection
                               connection
                               (make-http-request :method "GET"
                                                  :uri "https://queue.test/first")
                               :collect-body-p nil
                               :on-response-headers
                               (lambda (status headers)
                                 (declare (ignore headers))
                                 (ensure-equal 200 status)
                                 (sb-thread:signal-semaphore headers-entered)
                                 (setf header-callback-wait-result
                                       (sb-thread:wait-on-semaphore
                                        release-headers :timeout 10)))
                               :on-body-chunk
                               (lambda (chunk)
                                 (incf body-callback-count)
                                 (push (copy-seq chunk) body-callback-chunks))))
                      (error (condition)
                        (setf first-failure condition))))))
           (ensure-true
            (sb-thread:wait-on-semaphore headers-entered :timeout 5))
           (setf second-response
                 (http-kit/http2:send-http2-request-over-connection
                  connection
                  (make-http-request :method "GET"
                                     :uri "https://queue.test/second")))
           (ensure-equal 200 (http-response-status second-response))
           (ensure-equal (octets) (http-response-body second-response))
           (ensure-equal 0 body-callback-count)
           (setf release-signaled-p t)
           (sb-thread:signal-semaphore release-headers)
           (sb-thread:join-thread first-thread)
           (setf first-thread nil)
           (ensure-false first-failure)
           (ensure-true header-callback-wait-result)
           (ensure-equal 200 (http-response-status first-response))
           (ensure-equal (octets) (http-response-body first-response))
           (ensure-equal 1 body-callback-count)
           (ensure-equal
            (list (make-array 32 :element-type '(unsigned-byte 8)
                              :initial-element 65))
            (nreverse body-callback-chunks)))
      (when first-thread
        (unless release-signaled-p
          (sb-thread:signal-semaphore release-headers))
        (sb-thread:join-thread first-thread))
      (http-kit/http2:close-http2-connection connection))))


#+sbcl
(progn
  (deftest http2-public-connection-coalesces-collected-body-callbacks-over-window
    (let* ((full-frame-body
             (make-array 16384 :element-type '(unsigned-byte 8)
                         :initial-element 65))
           (short-frame-body
             (make-array 16383 :element-type '(unsigned-byte 8)
                         :initial-element 65))
           (extra-body
             (make-array 465 :element-type '(unsigned-byte 8)
                         :initial-element 65))
           (ping-payload (octets 1 2 3 4 5 6 7 8))
           (extra-data
             (concatenate-octets
              (h2-frame 0 1 1 extra-body)
              (h2-frame 6 1 0 ping-payload)))
           (input
             (apply #'concatenate-octets
                    (append
                     (list
                      (h2-frame 4 0 0 (octets))
                      (h2-frame 1 4 1
                                (h2-header-block (cons ":status" "200"))))
                     (make-list 3
                                :initial-element
                                (h2-frame 0 0 1 full-frame-body))
                     (list (h2-frame 0 0 1 short-frame-body)))))
           (stream
             (make-instance 'h2-window-update-peer-stream
                            :input input
                            :extra-data extra-data))
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream :max-body-bytes 70000))
           (headers-entered (sb-thread:make-semaphore :count 0))
           (release-headers (sb-thread:make-semaphore :count 0))
           (release-signaled-p nil)
           (header-wait-result nil)
           (body-callback-count 0)
           (body-callback-chunks nil)
           (first-response nil)
           (first-failure nil)
           (first-thread nil))
      (unwind-protect
           (progn
             (setf first-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (setf first-response
                                (http-kit/http2:send-http2-request-over-connection
                                 connection
                                 (make-http-request
                                  :method "GET"
                                  :uri "https://collect-queue.test/large")
                                 :collect-body-p t
                                 :max-body-bytes 70000
                                 :on-response-headers
                                 (lambda (status headers)
                                   (declare (ignore headers))
                                   (ensure-equal 200 status)
                                   (sb-thread:signal-semaphore headers-entered)
                                   (setf header-wait-result
                                         (sb-thread:wait-on-semaphore
                                          release-headers :timeout 10)))
                                 :on-body-chunk
                                 (lambda (chunk)
                                   (incf body-callback-count)
                                   (push (copy-seq chunk)
                                         body-callback-chunks))))
                        (error (condition)
                          (setf first-failure condition))))))
             (ensure-true
              (sb-thread:wait-on-semaphore headers-entered :timeout 5))
             (ensure-equal
              ping-payload
              (http-kit/http2:ping-http2-connection
               connection :payload ping-payload))
             (ensure-true
              (h2-window-update-peer-extra-data-sent-p stream))
             (ensure-true
              (some (lambda (frame)
                      (and (= 8 (http-kit/http2::%h2-frame-type frame))
                           (zerop (http-kit/http2::%h2-frame-stream-id frame))))
                    (h2-output-frames stream)))
             (setf release-signaled-p t)
             (sb-thread:signal-semaphore release-headers)
             (sb-thread:join-thread first-thread)
             (setf first-thread nil)
             (ensure-true header-wait-result)
             (ensure-false first-failure)
             (ensure-equal 200 (http-response-status first-response))
             (ensure-equal 66000 (length (http-response-body first-response)))
             (ensure-true (equalp (http-response-body first-response) (first body-callback-chunks)))
             (ensure-equal 1 body-callback-count)
             (ensure-equal '(66000)
                           (mapcar #'length
                                   (nreverse body-callback-chunks)))
             (ensure-true
              (every (lambda (octet) (= octet 65))
                     (first body-callback-chunks)))
             (http-kit/http2:close-http2-connection connection))
        (when first-thread
          (unless release-signaled-p
            (sb-thread:signal-semaphore release-headers))
          (sb-thread:join-thread first-thread))
        (http-kit/http2:close-http2-connection connection))))

  (deftest http2-public-informational-callback-keeps-response-pending
    (let* ((information-payload (h2-header-block (cons ":status" "103")))
           (input
             (concatenate-octets
              (h2-frame 4 0 0 (octets))
              (h2-frame 1 4 3 information-payload)
              (h2-frame 1 5 3
                        (h2-header-block (cons ":status" "200")))
              (h2-frame 1 5 1
                        (h2-header-block (cons ":status" "200")))))
           (stream (make-instance 'binary-session-stream :input input))
           (connection (http-kit/http2:make-http2-connection :stream stream))
           (first-opened (sb-thread:make-semaphore :count 0))
           (second-opened (sb-thread:make-semaphore :count 0))
           (release-first-open (sb-thread:make-semaphore :count 0))
           (release-second-open (sb-thread:make-semaphore :count 0))
           (first-release-signaled-p nil)
           (second-release-signaled-p nil)
           (first-open-wait-result nil)
           (second-open-wait-result nil)
           (information-callback-count 0)
           (first-response nil)
           (first-failure nil)
           (second-response nil)
           (second-failure nil)
           (first-thread nil)
           (second-thread nil))
      (unwind-protect
           (progn
             (setf first-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (setf first-response
                                (http-kit/http2:send-http2-request-over-connection
                                 connection
                                 (make-http-request :method "GET"
                                                    :uri "https://information-race.test/first")
                                 :on-stream-open
                                 (lambda (stream-id cancel)
                                   (declare (ignore cancel))
                                   (ensure-equal 1 stream-id)
                                   (sb-thread:signal-semaphore first-opened)
                                   (setf first-open-wait-result
                                         (sb-thread:wait-on-semaphore
                                          release-first-open :timeout 10)))))
                        (error (condition)
                          (setf first-failure condition))))))
             (ensure-true
              (sb-thread:wait-on-semaphore first-opened :timeout 5))
             (setf second-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (progn
                            (setf second-response
                                  (http-kit/http2:send-http2-request-over-connection
                                   connection
                                   (make-http-request :method "GET"
                                                      :uri "https://information-race.test/second")
                                   :on-information
                                   (lambda (response)
                                     (ensure-equal 103
                                                   (http-response-status response))
                                     (incf information-callback-count))
                                   :on-stream-open
                                   (lambda (stream-id cancel)
                                     (declare (ignore cancel))
                                     (ensure-equal 3 stream-id)
                                     (sb-thread:signal-semaphore second-opened)
                                     (setf second-open-wait-result
                                           (sb-thread:wait-on-semaphore
                                            release-second-open :timeout 10)))))
                            (ensure-equal 1 information-callback-count))
                        (error (condition)
                          (setf second-failure condition))))))
             (ensure-true
              (sb-thread:wait-on-semaphore second-opened :timeout 5))
             (setf first-release-signaled-p t)
             (sb-thread:signal-semaphore release-first-open)
             (sb-thread:join-thread first-thread)
             (setf first-thread nil)
             (ensure-false first-failure)
             (ensure-true first-open-wait-result)
             (ensure-equal 200 (http-response-status first-response))
             (ensure-equal 0 information-callback-count)
             (ensure-false second-response)
             (setf second-release-signaled-p t)
             (sb-thread:signal-semaphore release-second-open)
             (sb-thread:join-thread second-thread)
             (setf second-thread nil)
             (ensure-true second-open-wait-result)
             (ensure-false second-failure)
             (ensure-equal 200 (http-response-status second-response))
             (ensure-equal 1 information-callback-count)
             (http-kit/http2:close-http2-connection connection))
        (when first-thread
          (unless first-release-signaled-p
            (sb-thread:signal-semaphore release-first-open))
          (sb-thread:join-thread first-thread))
        (when second-thread
          (unless second-release-signaled-p
            (sb-thread:signal-semaphore release-second-open))
          (sb-thread:join-thread second-thread))
        (http-kit/http2:close-http2-connection connection))))

  (deftest http2-public-information-callback-limit-allows-two-pending-callbacks
    (let* ((input
             (concatenate-octets
              (h2-frame 4 0 0 (octets))
              (h2-frame 1 4 1
                        (h2-header-block (cons ":status" "103")))
              (h2-frame 1 4 1
                        (h2-header-block (cons ":status" "102")))
              (h2-frame 1 5 1
                        (h2-header-block (cons ":status" "200")))
              (h2-frame 1 5 3
                        (h2-header-block (cons ":status" "200")))))
           (stream (make-instance 'binary-session-stream :input input))
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream :max-pending-information-callbacks 2))
           (first-opened (sb-thread:make-semaphore :count 0))
           (release-first-open (sb-thread:make-semaphore :count 0))
           (first-release-signaled-p nil)
           (first-open-wait-result nil)
           (information-statuses nil)
           (first-response nil)
           (first-failure nil)
           (second-response nil)
           (first-thread nil))
      (unwind-protect
           (progn
             (setf first-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (setf first-response
                                (http-kit/http2:send-http2-request-over-connection
                                 connection
                                 (make-http-request
                                  :method "GET"
                                  :uri "https://information-limit.test/within")
                                 :on-information
                                 (lambda (response)
                                   (push (http-response-status response)
                                         information-statuses))
                                 :on-stream-open
                                 (lambda (stream-id cancel)
                                   (declare (ignore cancel))
                                   (ensure-equal 1 stream-id)
                                   (sb-thread:signal-semaphore first-opened)
                                   (setf first-open-wait-result
                                         (sb-thread:wait-on-semaphore
                                          release-first-open :timeout 10)))))
                        (error (condition)
                          (setf first-failure condition))))))
             (ensure-true
              (sb-thread:wait-on-semaphore first-opened :timeout 5))
             (setf second-response
                   (http-kit/http2:send-http2-request-over-connection
                    connection
                    (make-http-request
                     :method "GET"
                     :uri "https://information-limit.test/neighbor")))
             (ensure-equal 200 (http-response-status second-response))
             (ensure-equal nil information-statuses)
             (setf first-release-signaled-p t)
             (sb-thread:signal-semaphore release-first-open)
             (sb-thread:join-thread first-thread)
             (setf first-thread nil)
             (ensure-true first-open-wait-result)
             (ensure-false first-failure)
             (ensure-equal 200 (http-response-status first-response))
             (ensure-equal '(103 102) (nreverse information-statuses))
             (ensure-true (http-kit/http2:http2-connection-open-p connection)))
        (when first-thread
          (unless first-release-signaled-p
            (sb-thread:signal-semaphore release-first-open))
          (sb-thread:join-thread first-thread))
        (http-kit/http2:close-http2-connection connection))))

  (deftest http2-public-information-callback-limit-retires-only-over-limit-stream
    (let* ((input
             (concatenate-octets
              (h2-frame 4 0 0 (octets))
              (h2-frame 1 4 1
                        (h2-header-block (cons ":status" "103")))
              (h2-frame 1 4 1
                        (h2-header-block (cons ":status" "102")))
              (h2-frame 1 5 3
                        (h2-header-block (cons ":status" "200")))))
           (stream (make-instance 'binary-session-stream :input input))
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream :max-pending-information-callbacks 1))
           (first-opened (sb-thread:make-semaphore :count 0))
           (release-first-open (sb-thread:make-semaphore :count 0))
           (first-release-signaled-p nil)
           (first-open-wait-result nil)
           (information-callback-count 0)
           (first-response nil)
           (first-failure nil)
           (second-response nil)
           (first-thread nil))
      (unwind-protect
           (progn
             (setf first-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (setf first-response
                                (http-kit/http2:send-http2-request-over-connection
                                 connection
                                 (make-http-request
                                  :method "GET"
                                  :uri "https://information-limit.test/over")
                                 :on-information
                                 (lambda (response)
                                   (declare (ignore response))
                                   (incf information-callback-count))
                                 :on-stream-open
                                 (lambda (stream-id cancel)
                                   (declare (ignore cancel))
                                   (ensure-equal 1 stream-id)
                                   (sb-thread:signal-semaphore first-opened)
                                   (setf first-open-wait-result
                                         (sb-thread:wait-on-semaphore
                                          release-first-open :timeout 10)))))
                        (error (condition)
                          (setf first-failure condition))))))
             (ensure-true
              (sb-thread:wait-on-semaphore first-opened :timeout 5))
             (setf second-response
                   (http-kit/http2:send-http2-request-over-connection
                    connection
                    (make-http-request
                     :method "GET"
                     :uri "https://information-limit.test/neighbor")))
             (ensure-equal 200 (http-response-status second-response))
             (ensure-equal 0 information-callback-count)
             (ensure-true
              (some (lambda (frame)
                      (and (= 3 (http-kit/http2::%h2-frame-type frame))
                           (= 1 (http-kit/http2::%h2-frame-stream-id frame))
                           (= 8 (http-kit/http2::%h2-u32
                                 (http-kit/http2::%h2-frame-payload frame) 0))))
                    (h2-output-frames stream)))
             (ensure-false
              (some (lambda (frame)
                      (and (= 3 (http-kit/http2::%h2-frame-type frame))
                           (= 3 (http-kit/http2::%h2-frame-stream-id frame))))
                    (h2-output-frames stream)))
             (ensure-true (http-kit/http2:http2-connection-open-p connection))
             (setf first-release-signaled-p t)
             (sb-thread:signal-semaphore release-first-open)
             (sb-thread:join-thread first-thread)
             (setf first-thread nil)
             (ensure-true first-open-wait-result)
             (ensure-true
              (typep first-failure 'http-kit:http-protocol-error))
             (ensure-false first-response)
             (ensure-equal 0 information-callback-count)
             (ensure-true (http-kit/http2:http2-connection-open-p connection)))
        (when first-thread
          (unless first-release-signaled-p
            (sb-thread:signal-semaphore release-first-open))
          (sb-thread:join-thread first-thread))
        (http-kit/http2:close-http2-connection connection))))

(deftest http2-public-connection-manager-completes-concurrent-streams-over-aggregate-window
  (let* ((full-frame-body
           (make-array 16384 :element-type '(unsigned-byte 8)
                       :initial-element 65))
         (short-frame-body
           (make-array 16383 :element-type '(unsigned-byte 8)
                       :initial-element 65))
         (first-expected
           (concatenate-octets full-frame-body full-frame-body (octets 66)))
         (second-expected
           (concatenate-octets full-frame-body short-frame-body (octets 67)))
         (extra-data
           (concatenate-octets
            (h2-frame 0 1 1 (octets 66))
            (h2-frame 0 1 3 (octets 67))
            (h2-frame 1 5 5
                      (h2-header-block (cons ":status" "202")))))
         (input-wire
           (apply #'concatenate-octets
                  (list
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 4 1
                             (h2-header-block (cons ":status" "200")))
                   (h2-frame 0 0 1 full-frame-body)
                   (h2-frame 0 0 1 full-frame-body)
                   (h2-frame 1 4 3
                             (h2-header-block (cons ":status" "201")))
                   (h2-frame 0 0 3 full-frame-body)
                   (h2-frame 0 0 3 short-frame-body))))
         (stream
           (make-instance 'h2-window-update-peer-stream
                          :input input-wire
                          :window-update-stream-id 0
                          :extra-data extra-data))
         (connection nil)
         (opened 0)
         (closed 0)
         (manager
           (http-kit/http2:make-http2-connection-manager
            :open-connection
            (lambda (request &key timeout deadline proxy-plan proxy)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (incf opened)
              (setf connection
                    (http-kit/http2:make-http2-connection
                     :stream stream
                     :max-concurrent-streams 3
                     :close-stream
                     (lambda (closed-stream)
                       (ensure-true (eq stream closed-stream))
                       (incf closed)))))))
         (manager-transport
           (http-kit/http2:make-http2-connection-manager-transport manager))
         (client
           (http-kit/client:make-http-client
            :transport-function
            (lambda (request &rest arguments)
              (apply manager-transport request
                     :connection-key "aggregate-window.test" arguments))
            :cache nil
            :default-timeout 15))
         (first-headers-entered (sb-thread:make-semaphore :count 0))
         (second-headers-entered (sb-thread:make-semaphore :count 0))
         (release-first-headers (sb-thread:make-semaphore :count 0))
         (release-second-headers (sb-thread:make-semaphore :count 0))
         (first-release-signaled-p nil)
         (second-release-signaled-p nil)
         (first-header-wait-result nil)
         (second-header-wait-result nil)
         (first-body-chunks nil)
         (second-body-chunks nil)
         (first-response nil)
         (first-error nil)
         (second-response nil)
         (second-error nil)
         (driver-response nil)
         (first-thread nil)
         (second-thread nil))
    (unwind-protect
         (progn
           (setf first-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (setf first-response
                              (sb-ext:with-timeout 15
                                (http-kit/client:http-client-send
                                 client "GET"
                                 "http://aggregate-window.test/first"
                                 :collect-body-p nil
                                 :on-response-headers
                                 (lambda (status headers)
                                   (declare (ignore headers))
                                   (ensure-equal 200 status)
                                   (sb-thread:signal-semaphore
                                    first-headers-entered)
                                   (setf first-header-wait-result
                                         (sb-thread:wait-on-semaphore
                                          release-first-headers :timeout 10)))
                                 :on-body-chunk
                                 (lambda (chunk)
                                   (push (copy-seq chunk) first-body-chunks)))))
                      (error (condition)
                        (setf first-error condition))))))
           (ensure-true
            (sb-thread:wait-on-semaphore first-headers-entered :timeout 5))
           (setf second-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (setf second-response
                              (sb-ext:with-timeout 15
                                (http-kit/client:http-client-send
                                 client "GET"
                                 "http://aggregate-window.test/second"
                                 :collect-body-p nil
                                 :on-response-headers
                                 (lambda (status headers)
                                   (declare (ignore headers))
                                   (ensure-equal 201 status)
                                   (sb-thread:signal-semaphore
                                    second-headers-entered)
                                   (setf second-header-wait-result
                                         (sb-thread:wait-on-semaphore
                                          release-second-headers :timeout 10)))
                                 :on-body-chunk
                                 (lambda (chunk)
                                   (push (copy-seq chunk) second-body-chunks)))))
                      (error (condition)
                        (setf second-error condition))))))
           (ensure-true
            (sb-thread:wait-on-semaphore second-headers-entered :timeout 5))
           (setf driver-response
                 (sb-ext:with-timeout 15
                   (http-kit/client:http-client-send
                    client "GET" "http://aggregate-window.test/driver")))
           (ensure-equal 202 (http-response-status driver-response))
           (ensure-true (h2-window-update-peer-extra-data-sent-p stream))
           (ensure-equal nil first-body-chunks)
           (ensure-equal nil second-body-chunks)
           (ensure-true (http-kit/http2:http2-connection-open-p connection))
           (setf first-release-signaled-p t
                 second-release-signaled-p t)
           (sb-thread:signal-semaphore release-first-headers)
           (sb-thread:signal-semaphore release-second-headers)
           (sb-thread:join-thread first-thread)
           (setf first-thread nil)
           (sb-thread:join-thread second-thread)
           (setf second-thread nil)
           (ensure-false first-error)
           (ensure-false second-error)
           (ensure-true first-header-wait-result)
           (ensure-true second-header-wait-result)
           (ensure-equal 200 (http-response-status first-response))
           (ensure-equal 201 (http-response-status second-response))
           (ensure-equal (octets) (http-response-body first-response))
           (ensure-equal (octets) (http-response-body second-response))
           (ensure-true
            (equalp first-expected
                    (apply #'concatenate-octets
                           (nreverse first-body-chunks))))
           (ensure-true
            (equalp second-expected
                    (apply #'concatenate-octets
                           (nreverse second-body-chunks))))
           (ensure-equal
            65537
            (+ (length first-expected) (length second-expected)))
           (ensure-equal
            1
            (http-kit/http2:http2-connection-manager-connection-count manager)))
      (unless first-release-signaled-p
        (setf first-release-signaled-p t)
        (sb-thread:signal-semaphore release-first-headers))
      (unless second-release-signaled-p
        (setf second-release-signaled-p t)
        (sb-thread:signal-semaphore release-second-headers))
      (when first-thread
        (sb-thread:join-thread first-thread))
      (when second-thread
        (sb-thread:join-thread second-thread))
      (http-kit/http2:close-http2-connection-manager manager))
    (ensure-equal 1 opened)
    (ensure-equal 1 closed)))

)


#+sbcl
(deftest http2-public-connection-manager-keeps-stalled-stream-within-its-window
  (let* ((full-frame-body
           (make-array 16384 :element-type '(unsigned-byte 8)
                       :initial-element 65))
         (short-frame-body
           (make-array 16383 :element-type '(unsigned-byte 8)
                       :initial-element 65))
         (input-wire
           (apply #'concatenate-octets
                  (list
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 4 1
                             (h2-header-block (cons ":status" "200")))
                   (h2-frame 0 0 1 full-frame-body)
                   (h2-frame 0 0 1 full-frame-body)
                   (h2-frame 0 0 1 full-frame-body)
                   (h2-frame 0 0 1 short-frame-body)
                   (h2-frame 1 5 3
                             (h2-header-block (cons ":status" "201"))))))
         (stream
           (make-instance 'h2-window-update-peer-stream :input input-wire))
         (connection nil)
         (opened 0)
         (closed 0)
         (manager
           (http-kit/http2:make-http2-connection-manager
            :open-connection
            (lambda (request &key timeout deadline proxy-plan proxy)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (incf opened)
              (setf connection
                    (http-kit/http2:make-http2-connection
                     :stream stream
                     :max-concurrent-streams 2
                     :close-stream
                     (lambda (closed-stream)
                       (ensure-true (eq stream closed-stream))
                       (incf closed)))))))
         (manager-transport
           (http-kit/http2:make-http2-connection-manager-transport manager))
         (client
           (http-kit/client:make-http-client
            :transport-function
            (lambda (request &rest arguments)
              (apply manager-transport request
                     :connection-key "stalled-window.test" arguments))
            :cache nil
            :default-timeout 15))
         (headers-entered (sb-thread:make-semaphore :count 0))
         (release-headers (sb-thread:make-semaphore :count 0))
         (release-signaled-p nil)
         (header-wait-result nil)
         (first-body-chunks nil)
         (first-response nil)
         (first-error nil)
         (second-response nil)
         (first-done (sb-thread:make-semaphore :count 0))
         (first-thread nil))
    (unwind-protect
         (progn
           (setf first-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (setf first-response
                                   (sb-ext:with-timeout 15
                                     (http-kit/client:http-client-send
                                      client "GET"
                                      "http://stalled-window.test/first"
                                      :collect-body-p nil
                                      :on-response-headers
                                      (lambda (status headers)
                                        (declare (ignore headers))
                                        (ensure-equal 200 status)
                                        (sb-thread:signal-semaphore
                                         headers-entered)
                                        (setf header-wait-result
                                              (sb-thread:wait-on-semaphore
                                               release-headers :timeout 10)))
                                      :on-body-chunk
                                      (lambda (chunk)
                                        (push (copy-seq chunk)
                                              first-body-chunks)))))
                           (error (condition)
                             (setf first-error condition)))
                      (sb-thread:signal-semaphore first-done)))))
           (ensure-true
            (sb-thread:wait-on-semaphore headers-entered :timeout 5))
           (setf second-response
                 (sb-ext:with-timeout 15
                   (http-kit/client:http-client-send
                    client "GET" "http://stalled-window.test/second")))
           (ensure-equal 201 (http-response-status second-response))
           (ensure-equal (octets) (http-response-body second-response))
           (ensure-false
            (sb-thread:wait-on-semaphore first-done :timeout 0.05))
           (ensure-equal nil first-body-chunks)
           (ensure-false (h2-window-update-peer-extra-data-sent-p stream))
           (ensure-false
            (some (lambda (frame)
                    (and (= 8 (http-kit/http2::%h2-frame-type frame))
                         (= 1 (http-kit/http2::%h2-frame-stream-id frame))))
                  (h2-output-frames stream)))
           (ensure-true (http-kit/http2:http2-connection-open-p connection))
           (setf release-signaled-p t)
           (sb-thread:signal-semaphore release-headers)
           (sb-thread:join-thread first-thread)
           (setf first-thread nil)
           (ensure-false first-error)
           (ensure-true header-wait-result)
           (ensure-equal 200 (http-response-status first-response))
           (ensure-equal (octets) (http-response-body first-response))
           (ensure-true (h2-window-update-peer-extra-data-sent-p stream))
           (ensure-equal
            65536
            (length (apply #'concatenate-octets
                           (reverse first-body-chunks))))
           (ensure-true
            (every (lambda (octet) (= octet 65))
                   (subseq (apply #'concatenate-octets
                                  (reverse first-body-chunks))
                           0 65535)))
           (ensure-equal
            66
            (aref (apply #'concatenate-octets
                         (reverse first-body-chunks))
                  65535))
           (ensure-equal
            1
            (http-kit/http2:http2-connection-manager-connection-count manager)))
      (unless release-signaled-p
        (setf release-signaled-p t)
        (sb-thread:signal-semaphore release-headers))
      (when first-thread
        (sb-thread:join-thread first-thread))
      (http-kit/http2:close-http2-connection-manager manager))
    (ensure-equal 1 opened)
    (ensure-equal 1 closed)))
#+sbcl
(progn
  (defclass h2-manager-capacity-peer-stream (binary-session-stream)
    ((input-lock
       :initform (sb-thread:make-mutex :name "HTTP/2 capacity peer input")
       :reader h2-manager-capacity-peer-input-lock)
     (input-ready
       :initform (sb-thread:make-waitqueue :name "HTTP/2 capacity peer input")
       :reader h2-manager-capacity-peer-input-ready)
     (input-read-waiting
       :initform (sb-thread:make-semaphore :count 0)
       :reader h2-manager-capacity-peer-input-read-waiting)
     (output-lock
       :initform (sb-thread:make-mutex :name "HTTP/2 capacity peer output")
       :reader h2-manager-capacity-peer-output-lock)
     (header-available
       :initform (sb-thread:make-semaphore :count 0)
       :reader h2-manager-capacity-peer-header-available)
     (header-stream-ids
       :initform nil
       :reader h2-manager-capacity-peer-header-stream-ids)
     (seen-header-stream-ids
       :initform (make-hash-table :test #'eql)
       :reader h2-manager-capacity-peer-seen-header-stream-ids)
     (response-end-stream-p
       :initarg :response-end-stream-p
       :initform nil
       :reader h2-manager-capacity-peer-response-end-stream-p)
     (closed-p
       :initform nil
       :accessor h2-manager-capacity-peer-closed-p)))

  (defmethod sb-gray:stream-read-sequence
      ((stream h2-manager-capacity-peer-stream) sequence
       &optional (start 0) end)
    (let ((end (or end (length sequence)))
          (lock (h2-manager-capacity-peer-input-lock stream)))
      (sb-thread:with-mutex (lock)
        (loop while (and (>= (binary-session-input-position stream)
                             (length (binary-session-input stream)))
                         (not (h2-manager-capacity-peer-closed-p stream)))
              do (progn
                   (sb-thread:signal-semaphore
                    (h2-manager-capacity-peer-input-read-waiting stream))
                   (sb-thread:condition-wait
                    (h2-manager-capacity-peer-input-ready stream) lock)))
        (let* ((input (binary-session-input stream))
               (position (binary-session-input-position stream))
               (count (min (- end start) (- (length input) position))))
          (when (plusp count)
            (replace sequence input
                     :start1 start
                     :end1 (+ start count)
                     :start2 position
                     :end2 (+ position count))
            (incf (binary-session-input-position stream) count))
          (+ start count)))))

  (defun h2-manager-capacity-peer-feed (stream bytes)
    (let ((lock (h2-manager-capacity-peer-input-lock stream)))
      (sb-thread:with-mutex (lock)
        (setf (slot-value stream 'input)
              (concatenate-octets (binary-session-input stream) bytes))
        (sb-thread:condition-broadcast
         (h2-manager-capacity-peer-input-ready stream))))
    stream)

  (defun h2-manager-capacity-peer-close (stream)
    (let ((lock (h2-manager-capacity-peer-input-lock stream)))
      (sb-thread:with-mutex (lock)
        (setf (h2-manager-capacity-peer-closed-p stream) t)
        (sb-thread:condition-broadcast
         (h2-manager-capacity-peer-input-ready stream))))
    stream)

  (defun h2-manager-capacity-peer-output-header-ids (stream)
    (let ((lock (h2-manager-capacity-peer-output-lock stream)))
      (sb-thread:with-mutex (lock)
        (reverse
         (copy-list
          (h2-manager-capacity-peer-header-stream-ids stream))))))

  (defmethod sb-gray:stream-write-sequence
      ((stream h2-manager-capacity-peer-stream) sequence
       &optional (start 0) end)
    (let ((lock (h2-manager-capacity-peer-output-lock stream)))
      (sb-thread:with-mutex (lock)
        (let ((result (call-next-method stream sequence start end)))
          (when (>= (length (binary-session-output stream))
                    (length (h2-preface)))
            (dolist (frame (h2-output-frames stream))
              (when (= 1 (http-kit/http2::%h2-frame-type frame))
                (let* ((stream-id
                         (http-kit/http2::%h2-frame-stream-id frame))
                       (seen
                         (h2-manager-capacity-peer-seen-header-stream-ids
                          stream)))
                  (unless (gethash stream-id seen)
                    (setf (gethash stream-id seen) t
                          (slot-value stream 'header-stream-ids)
                          (cons stream-id
                                (h2-manager-capacity-peer-header-stream-ids
                                 stream)))
                    (h2-manager-capacity-peer-feed
                     stream
                     (h2-frame
                      1 (if (h2-manager-capacity-peer-response-end-stream-p
                             stream)
                            5
                            4)
                      stream-id
                      (h2-header-block
                       (cons ":status"
                             (case stream-id
                               (1 "200")
                               (3 "201")
                               (5 "202")
                               (otherwise "200"))))))
                    (sb-thread:signal-semaphore
                     (h2-manager-capacity-peer-header-available stream)))))))
          result))))

  (deftest http2-public-connection-manager-serializes-streams-at-local-capacity
    (let* ((stream
             (make-instance 'h2-manager-capacity-peer-stream
                            :input (h2-frame 4 0 0 (octets))))
           (connection nil)
           (opened 0)
           (closed 0)
           (manager
             (http-kit/http2:make-http2-connection-manager
              :open-connection
              (lambda (request &key timeout deadline proxy-plan proxy)
                (declare (ignore request timeout deadline proxy-plan proxy))
                (incf opened)
                (setf connection
                      (http-kit/http2:make-http2-connection
                       :stream stream
                       :max-concurrent-streams 1
                       :close-stream
                       (lambda (closed-stream)
                         (ensure-true (eq stream closed-stream))
                         (h2-manager-capacity-peer-close stream)
                         (incf closed)))))))
           (manager-transport
             (http-kit/http2:make-http2-connection-manager-transport manager))
           (second-transport-entered (sb-thread:make-semaphore :count 0))
           (third-transport-entered (sb-thread:make-semaphore :count 0))
           (client
             (http-kit/client:make-http-client
              :transport-function
              (lambda (request &rest arguments)
                (let ((uri (http-uri-string (http-request-uri request))))
                  (cond
                    ((search "/second" uri)
                     (sb-thread:signal-semaphore second-transport-entered))
                    ((search "/third" uri)
                     (sb-thread:signal-semaphore third-transport-entered))))
                (apply manager-transport request
                       :connection-key "capacity-contention.test" arguments))
              :cache nil
              :default-timeout 15))
           (first-headers-entered (sb-thread:make-semaphore :count 0))
           (second-headers-entered (sb-thread:make-semaphore :count 0))
           (third-headers-entered (sb-thread:make-semaphore :count 0))
           (later-headers-entered (sb-thread:make-semaphore :count 0))
           (release-first-headers (sb-thread:make-semaphore :count 0))
           (release-second-headers (sb-thread:make-semaphore :count 0))
           (release-third-headers (sb-thread:make-semaphore :count 0))
           (first-release-signaled-p nil)
           (second-release-signaled-p nil)
           (third-release-signaled-p nil)
           (first-response nil)
           (first-error nil)
           (second-response nil)
           (second-error nil)
           (third-response nil)
           (third-error nil)
           (first-done (sb-thread:make-semaphore :count 0))
           (second-done (sb-thread:make-semaphore :count 0))
           (third-done (sb-thread:make-semaphore :count 0))
           (first-thread nil)
           (second-thread nil)
           (third-thread nil))
      (labels
          ((start-request
               (path expected-status headers-entered release-headers
                response-setter error-setter done)
             (sb-thread:make-thread
              (lambda ()
                (unwind-protect
                     (handler-case
                         (funcall
                          response-setter
                          (sb-ext:with-timeout 15
                            (http-kit/client:http-client-send
                             client "GET"
                             (concatenate 'string
                                          "http://capacity-one.test" path)
                             :collect-body-p t
                             :on-response-headers
                             (lambda (status headers)
                               (declare (ignore headers))
                               (if (listp expected-status)
                                   (ensure-true (member status expected-status))
                                   (ensure-equal expected-status status))
                               (sb-thread:signal-semaphore headers-entered)
                               (unless (string= path "/first")
                                 (sb-thread:signal-semaphore
                                  later-headers-entered))
                               (ensure-true
                                (sb-thread:wait-on-semaphore
                                 release-headers :timeout 10))))))
                       (error (condition)
                         (funcall error-setter condition)))
                  (sb-thread:signal-semaphore done))))))
        (unwind-protect
             (progn
               (setf first-thread
                     (start-request
                      "/first" 200 first-headers-entered release-first-headers
                      (lambda (response) (setf first-response response))
                      (lambda (condition) (setf first-error condition))
                      first-done))
               (ensure-true
                (sb-thread:wait-on-semaphore first-headers-entered :timeout 5))
               (ensure-true
                (sb-thread:wait-on-semaphore
                 (h2-manager-capacity-peer-header-available stream)
                 :timeout 5))
               (ensure-equal '(1)
                             (h2-manager-capacity-peer-output-header-ids stream))
               (setf second-thread
                     (start-request
                      "/second" '(201 202) second-headers-entered
                      release-second-headers
                      (lambda (response) (setf second-response response))
                      (lambda (condition) (setf second-error condition))
                      second-done))
               (ensure-true
                (sb-thread:wait-on-semaphore second-transport-entered :timeout 5))
               (setf third-thread
                     (start-request
                      "/third" '(201 202) third-headers-entered
                      release-third-headers
                      (lambda (response) (setf third-response response))
                      (lambda (condition) (setf third-error condition))
                      third-done))
               (ensure-true
                (sb-thread:wait-on-semaphore third-transport-entered :timeout 5))
               (ensure-false
                (sb-thread:wait-on-semaphore
                 (h2-manager-capacity-peer-header-available stream)
                 :timeout 0.05))
               (ensure-equal '(1)
                             (h2-manager-capacity-peer-output-header-ids stream))
               (ensure-false
                (sb-thread:wait-on-semaphore second-done :timeout 0.001))
               (ensure-false
                (sb-thread:wait-on-semaphore third-done :timeout 0.001))
               (ensure-true (http-kit/http2:http2-connection-open-p connection))
               (ensure-equal
                1
                (http-kit/http2:http2-connection-manager-connection-count
                 manager))
               (setf first-release-signaled-p t)
               (sb-thread:signal-semaphore release-first-headers)
               (h2-manager-capacity-peer-feed
                stream (h2-frame 0 1 1 (octets 65)))
               (let* ((first-slot-request
                        (progn
                          (ensure-true
                           (sb-thread:wait-on-semaphore
                            later-headers-entered :timeout 5))
                          (cond
                            ((sb-thread:wait-on-semaphore
                              second-headers-entered :timeout 0.000001)
                             :second)
                            ((sb-thread:wait-on-semaphore
                              third-headers-entered :timeout 0.000001)
                             :third)
                            (t (error "A capacity waiter opened an unknown request.")))))
                      (second-slot-request
                        (if (eq first-slot-request :second) :third :second))
                      (first-slot-done
                        (if (eq first-slot-request :second)
                            second-done
                            third-done))
                      (second-slot-done
                        (if (eq second-slot-request :second)
                            second-done
                            third-done)))
                 (ensure-true
                  (sb-thread:wait-on-semaphore
                   (h2-manager-capacity-peer-header-available stream)
                   :timeout 5))
                 (ensure-equal '(1 3)
                               (h2-manager-capacity-peer-output-header-ids stream))
                 (ensure-false
                  (sb-thread:wait-on-semaphore
                   (h2-manager-capacity-peer-header-available stream)
                   :timeout 0.05))
                 (ensure-false
                  (sb-thread:wait-on-semaphore second-done :timeout 0.001))
                 (ensure-false
                  (sb-thread:wait-on-semaphore third-done :timeout 0.001))
                 (ensure-true (http-kit/http2:http2-connection-open-p connection))
                 (if (eq first-slot-request :second)
                     (progn
                       (setf second-release-signaled-p t)
                       (sb-thread:signal-semaphore release-second-headers))
                     (progn
                       (setf third-release-signaled-p t)
                       (sb-thread:signal-semaphore release-third-headers)))
                 (h2-manager-capacity-peer-feed
                  stream (h2-frame 0 1 3 (octets 66)))
                 (ensure-true
                  (sb-thread:wait-on-semaphore first-slot-done :timeout 5))
                 (ensure-true
                  (sb-thread:wait-on-semaphore
                   later-headers-entered :timeout 5))
                 (ensure-true
                  (if (eq second-slot-request :second)
                      (sb-thread:wait-on-semaphore
                       second-headers-entered :timeout 0.000001)
                      (sb-thread:wait-on-semaphore
                       third-headers-entered :timeout 0.000001)))
                 (ensure-true
                  (sb-thread:wait-on-semaphore
                   (h2-manager-capacity-peer-header-available stream)
                   :timeout 5))
                 (ensure-equal '(1 3 5)
                               (h2-manager-capacity-peer-output-header-ids stream))
                 (ensure-false
                  (sb-thread:wait-on-semaphore
                   (h2-manager-capacity-peer-header-available stream)
                   :timeout 0.05))
                 (ensure-false
                  (sb-thread:wait-on-semaphore second-slot-done :timeout 0.001))
                 (ensure-true (http-kit/http2:http2-connection-open-p connection))
                 (if (eq second-slot-request :second)
                     (progn
                       (setf second-release-signaled-p t)
                       (sb-thread:signal-semaphore release-second-headers))
                     (progn
                       (setf third-release-signaled-p t)
                       (sb-thread:signal-semaphore release-third-headers)))
                 (h2-manager-capacity-peer-feed
                  stream (h2-frame 0 1 5 (octets 67)))
                 (ensure-true
                  (sb-thread:wait-on-semaphore second-slot-done :timeout 5)))
               (ensure-true
                (sb-thread:wait-on-semaphore first-done :timeout 5))
               (sb-thread:join-thread first-thread)
               (setf first-thread nil)
               (sb-thread:join-thread second-thread)
               (setf second-thread nil)
               (sb-thread:join-thread third-thread)
               (setf third-thread nil)
               (ensure-false first-error)
               (ensure-false second-error)
               (ensure-false third-error)
               (ensure-equal 200 (http-response-status first-response))
               (ensure-true
                (member (http-response-status second-response) '(201 202)))
               (ensure-true
                (member (http-response-status third-response) '(201 202)))
               (ensure-false
                (= (http-response-status second-response)
                   (http-response-status third-response)))
               (ensure-equal (octets 65) (http-response-body first-response))
               (ensure-true
                (member (http-response-body second-response)
                        (list (octets 66) (octets 67)) :test #'equalp))
               (ensure-true
                (member (http-response-body third-response)
                        (list (octets 66) (octets 67)) :test #'equalp))
               (ensure-false
                (equalp (http-response-body second-response)
                        (http-response-body third-response)))
               (ensure-equal '(1 3 5)
                             (h2-manager-capacity-peer-output-header-ids stream))
               (ensure-true (http-kit/http2:http2-connection-open-p connection))
               (ensure-equal
                1
                (http-kit/http2:http2-connection-manager-connection-count
                 manager)))
          (unless first-release-signaled-p
            (setf first-release-signaled-p t)
            (sb-thread:signal-semaphore release-first-headers))
          (unless second-release-signaled-p
            (setf second-release-signaled-p t)
            (sb-thread:signal-semaphore release-second-headers))
          (unless third-release-signaled-p
            (setf third-release-signaled-p t)
            (sb-thread:signal-semaphore release-third-headers))
          (h2-manager-capacity-peer-close stream)
          (http-kit/http2:close-http2-connection-manager manager)
          (when first-thread
            (sb-thread:join-thread first-thread))
          (when second-thread
            (sb-thread:join-thread second-thread))
          (when third-thread
            (sb-thread:join-thread third-thread)))
      (ensure-equal 1 opened)
      (ensure-equal 1 closed)))
)

  (deftest http2-public-connection-manager-cancels-waiting-request-before-stream-open
    (let* ((stream
             (make-instance 'h2-manager-capacity-peer-stream
                            :input (h2-frame 4 0 0 (octets))))
           (connection nil)
           (manager
             (http-kit/http2:make-http2-connection-manager
              :open-connection
              (lambda (request &key timeout deadline proxy-plan proxy)
                (declare (ignore request timeout deadline proxy-plan proxy))
                (setf connection
                      (http-kit/http2:make-http2-connection
                       :stream stream
                       :max-concurrent-streams 1
                       :close-stream
                       (lambda (closed-stream)
                         (ensure-true (eq stream closed-stream))
                         (h2-manager-capacity-peer-close stream)))))))
           (manager-transport
             (http-kit/http2:make-http2-connection-manager-transport manager))
           (second-transport-entered (sb-thread:make-semaphore :count 0))
           (second-cancel-registered (sb-thread:make-semaphore :count 0))
           (client
             (http-kit/client:make-http-client
              :transport-function
              (lambda (request &rest arguments)
                (when (search "/second"
                              (http-uri-string (http-request-uri request)))
                  (let ((register-cancel (getf arguments :on-cancel)))
                    (ensure-true (functionp register-cancel))
                    (setf (getf arguments :on-cancel)
                          (lambda (cancel-function)
                            (let ((cleanup (funcall register-cancel cancel-function)))
                              (sb-thread:signal-semaphore
                               second-cancel-registered)
                              cleanup)))
                    (sb-thread:signal-semaphore second-transport-entered)))
                (apply manager-transport request
                       :connection-key "capacity-cancel.test" arguments))
              :cache nil
              :default-timeout 15))
           (cancel-token (http-kit/client:make-http-cancellation-token))
           (first-headers-entered (sb-thread:make-semaphore :count 0))
           (release-first-headers (sb-thread:make-semaphore :count 0))
           (first-release-signaled-p nil)
           (first-response nil)
           (first-error nil)
           (first-done (sb-thread:make-semaphore :count 0))
           (second-response nil)
           (second-error nil)
           (second-done (sb-thread:make-semaphore :count 0))
           (first-thread nil)
           (second-thread nil))
      (unwind-protect
           (progn
             (setf first-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (handler-case
                               (setf first-response
                                     (sb-ext:with-timeout 15
                                       (http-kit/client:http-client-send
                                        client "GET"
                                        "http://capacity-cancel.test/first"
                                        :collect-body-p t
                                        :on-response-headers
                                        (lambda (status headers)
                                          (declare (ignore headers))
                                          (ensure-equal 200 status)
                                          (sb-thread:signal-semaphore
                                           first-headers-entered)
                                          (ensure-true
                                           (sb-thread:wait-on-semaphore
                                            release-first-headers
                                            :timeout 10))))))
                             (error (condition)
                               (setf first-error condition)))
                        (sb-thread:signal-semaphore first-done)))))
             (ensure-true
              (sb-thread:wait-on-semaphore first-headers-entered :timeout 5))
             (ensure-true
              (sb-thread:wait-on-semaphore
               (h2-manager-capacity-peer-header-available stream) :timeout 5))
             (ensure-equal '(1)
                           (h2-manager-capacity-peer-output-header-ids stream))
             (setf second-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (handler-case
                               (setf second-response
                                     (sb-ext:with-timeout 15
                                       (http-kit/client:http-client-send
                                        client "GET"
                                        "http://capacity-cancel.test/second"
                                        :collect-body-p t
                                        :cancel-token cancel-token)))
                             (error (condition)
                               (setf second-error condition)))
                        (sb-thread:signal-semaphore second-done)))))
             (ensure-true
              (sb-thread:wait-on-semaphore second-transport-entered :timeout 5))
             (ensure-true
              (sb-thread:wait-on-semaphore second-cancel-registered :timeout 5))
             (ensure-equal '(1)
                           (h2-manager-capacity-peer-output-header-ids stream))
             (ensure-true
              (http-kit/client:http-client-cancel-request cancel-token))
             (ensure-true
              (sb-thread:wait-on-semaphore second-done :timeout 1))
             (ensure-false second-response)
             (ensure-true
              (typep second-error 'http-kit/client:http-client-request-cancelled))
             (let* ((detail
                      (http-kit/client:http-client-error-detail second-error))
                    (cause
                      (and (typep detail 'http-kit:http-connection-error)
                           (http-kit:http-connection-error-cause detail))))
               (ensure-true (typep detail 'http-kit:http-connection-error))
               (ensure-true (consp cause))
               (ensure-equal :local-cancel (first cause)))
             (ensure-equal '(1)
                           (h2-manager-capacity-peer-output-header-ids stream))
             (ensure-true (http-kit/http2:http2-connection-open-p connection))
             (setf first-release-signaled-p t)
             (sb-thread:signal-semaphore release-first-headers)
             (h2-manager-capacity-peer-feed
              stream (h2-frame 0 1 1 (octets 65)))
             (ensure-true (sb-thread:wait-on-semaphore first-done :timeout 5))
             (sb-thread:join-thread first-thread)
             (setf first-thread nil)
             (ensure-false first-error)
             (ensure-equal 200 (http-response-status first-response))
             (ensure-equal (octets 65) (http-response-body first-response))
             (ensure-true (http-kit/http2:http2-connection-open-p connection)))
        (unless first-release-signaled-p
          (setf first-release-signaled-p t)
          (sb-thread:signal-semaphore release-first-headers))
        (h2-manager-capacity-peer-close stream)
        (http-kit/http2:close-http2-connection-manager manager)
        (when first-thread
          (sb-thread:join-thread first-thread))
        (when second-thread
          (sb-thread:join-thread second-thread)))))
)
#+sbcl
(progn
  (defmethod sb-gray:stream-read-byte
      ((stream binary-session-stream))
    (let ((input (binary-session-input stream))
          (position (binary-session-input-position stream)))
      (if (< position (length input))
          (prog1 (aref input position)
            (incf (binary-session-input-position stream)))
          :eof)))

  (defmethod sb-gray:stream-listen
      ((stream binary-session-stream))
    (declare (ignore stream))
    t)

  (defmethod sb-gray:stream-read-byte
      ((stream h2-manager-capacity-peer-stream))
    (let ((lock (h2-manager-capacity-peer-input-lock stream)))
      (sb-thread:with-mutex (lock)
        (loop while (and (>= (binary-session-input-position stream)
                             (length (binary-session-input stream)))
                         (not (h2-manager-capacity-peer-closed-p stream)))
              do (progn
                   (sb-thread:signal-semaphore
                    (h2-manager-capacity-peer-input-read-waiting stream))
                   (sb-thread:condition-wait
                    (h2-manager-capacity-peer-input-ready stream) lock)))
        (let* ((input (binary-session-input stream))
               (position (binary-session-input-position stream)))
          (if (< position (length input))
              (prog1 (aref input position)
                (incf (binary-session-input-position stream)))
              :eof)))))

  (defmethod sb-gray:stream-listen
      ((stream h2-manager-capacity-peer-stream))
    (let ((lock (h2-manager-capacity-peer-input-lock stream)))
      (sb-thread:with-mutex (lock)
        (let ((ready-p
                (or (h2-manager-capacity-peer-closed-p stream)
                    (< (binary-session-input-position stream)
                       (length (binary-session-input stream))))))
          (unless ready-p
            (sb-thread:signal-semaphore
             (h2-manager-capacity-peer-input-read-waiting stream)))
          ready-p))))

  (defclass h2-manager-partial-read-timeout-stream
      (h2-manager-capacity-peer-stream)
    ((stall-after-position
       :initarg :stall-after-position
       :reader h2-manager-partial-read-stall-after-position)
     (read-consumed
       :initarg :read-consumed
       :reader h2-manager-partial-read-consumed)))

  (defmethod sb-gray:stream-read-sequence
      ((stream h2-manager-partial-read-timeout-stream) sequence
       &optional (start 0) end)
    (let ((end (or end (length sequence)))
          (lock (h2-manager-capacity-peer-input-lock stream)))
      (sb-thread:with-mutex (lock)
        (loop while (and (>= (binary-session-input-position stream)
                             (length (binary-session-input stream)))
                         (not (h2-manager-capacity-peer-closed-p stream)))
              do (progn
                   (sb-thread:signal-semaphore
                    (h2-manager-capacity-peer-input-read-waiting stream))
                   (sb-thread:condition-wait
                    (h2-manager-capacity-peer-input-ready stream) lock)))
        (let* ((input (binary-session-input stream))
               (position (binary-session-input-position stream))
               (count (min (- end start) (- (length input) position))))
          (when (plusp count)
            (replace sequence input
                     :start1 start
                     :end1 (+ start count)
                     :start2 position
                     :end2 (+ position count))
            (incf (binary-session-input-position stream) count))
          (when (= (binary-session-input-position stream)
                   (h2-manager-partial-read-stall-after-position stream))
            (sb-thread:signal-semaphore
             (h2-manager-partial-read-consumed stream))
            (loop while (not (h2-manager-capacity-peer-closed-p stream))
                  do (sb-thread:condition-wait
                      (h2-manager-capacity-peer-input-ready stream) lock)))
          (+ start count)))))

  (defmethod sb-gray:stream-listen
      ((stream scheduled-ping-eof-stream))
    (let ((input (binary-session-input stream))
          (position (binary-session-input-position stream)))
      (if (< position (length input))
          t
          (progn
            (sb-thread:signal-semaphore (scheduled-ping-eof-started stream))
            (plusp
             (sb-thread:semaphore-count
              (scheduled-ping-eof-release stream)))))))

  (defclass h2-generic-stream-wrapper
      (sb-gray:fundamental-binary-input-stream
       sb-gray:fundamental-binary-output-stream)
    ((delegate
       :initarg :delegate
       :reader h2-generic-stream-wrapper-delegate)))

  (defmethod sb-gray:stream-read-sequence
      ((stream h2-generic-stream-wrapper) sequence &optional (start 0) end)
    (sb-gray:stream-read-sequence
     (h2-generic-stream-wrapper-delegate stream) sequence start end))

  (defmethod sb-gray:stream-write-sequence
      ((stream h2-generic-stream-wrapper) sequence &optional (start 0) end)
    (sb-gray:stream-write-sequence
     (h2-generic-stream-wrapper-delegate stream) sequence start end))

  (defmethod sb-gray:stream-finish-output ((stream h2-generic-stream-wrapper))
    (sb-gray:stream-finish-output
     (h2-generic-stream-wrapper-delegate stream)))

  (deftest http2-public-partial-frame-generic-read-timeout-closes-connection
    (let* ((initial-settings
             (h2-frame 4 0 0 (octets 0 3 0 0 0 1)))
           (read-consumed (sb-thread:make-semaphore :count 0))
           (delegate
             (make-instance (quote h2-manager-partial-read-timeout-stream)
                            :input initial-settings
                            :stall-after-position (1+ (length initial-settings))
                            :read-consumed read-consumed))
           (stream
             (make-instance 'h2-generic-stream-wrapper :delegate delegate))
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream
              :close-stream
              (lambda (closed-stream)
                (ensure-true (eq stream closed-stream))
                (h2-manager-capacity-peer-close delegate))))
           (request-done (sb-thread:make-semaphore :count 0))
           (response nil)
           (failure nil)
           (request-thread nil))
      (unwind-protect
           (progn
             (ensure-false
              (sb-mop:compute-applicable-methods
               #'sb-gray:stream-listen (list stream)))
             (setf request-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (handler-case
                               (setf response
                                     (http-kit/http2:send-http2-request-over-connection
                                      connection
                                      (make-http-request
                                       :method "GET"
                                       :uri "https://partial-timeout.test/")
                                      :timeout 2))
                             (error (condition)
                               (setf failure condition)))
                        (sb-thread:signal-semaphore request-done)))))
             (ensure-true
              (sb-thread:wait-on-semaphore read-consumed :timeout 5))
             (ensure-true (sb-thread:wait-on-semaphore request-done :timeout 2))
             (sb-thread:join-thread request-thread)
             (setf request-thread nil)
             (ensure-false response)
             (ensure-true
              (typep failure (quote http-kit:http-connection-error)))
             (ensure-equal
              :ambiguous-read-timeout
              (http-kit:http-connection-error-cause failure))
             (ensure-false (http-kit/http2:http2-connection-open-p connection))
             (ensure-equal
              (1+ (length initial-settings))
              (binary-session-input-position delegate)))
        (h2-manager-capacity-peer-close delegate)
        (http-kit/http2:close-http2-connection connection)
        (when request-thread
          (sb-thread:join-thread request-thread)))))
)

#+sbcl
(progn
  (defclass h2-generic-listen-stream-wrapper
      (h2-generic-stream-wrapper)
    ())
  (defmethod sb-gray:stream-listen
      ((stream h2-generic-listen-stream-wrapper))
    (sb-gray:stream-listen
     (h2-generic-stream-wrapper-delegate stream)))
  (deftest http2-reader-generic-listen-preserves-partial-frame
    (let* ((wire (h2-frame 4 0 0 (octets 0 4 0 0 0 2)))
           (peer
             (make-instance (quote h2-manager-capacity-peer-stream)
                            :input (subseq wire 0 10)))
           (stream
             (make-instance (quote h2-generic-listen-stream-wrapper)
                            :delegate peer))
           (reader (http-kit/http2::%h2-reader-for stream)))
      (ensure-true
       (not
        (null
         (sb-mop:compute-applicable-methods
          (function sb-gray:stream-listen)
          (list stream)))))
      (ensure-equal
       :would-block
       (http-kit/http2::%h2-read-frame
        reader 16384 nil (function http-kit::%monotonic-time) :slice-p t))
      (h2-manager-capacity-peer-feed peer (subseq wire 10))
      (h2-manager-capacity-peer-close peer)
      (let ((frame
              (http-kit/http2::%h2-read-frame
               reader 16384 nil (function http-kit::%monotonic-time) :slice-p t)))
        (ensure-equal 4 (http-kit/http2::%h2-frame-type frame))
        (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id frame))
        (ensure-equal
         (octets 0 4 0 0 0 2)
         (http-kit/http2::%h2-frame-payload frame)))
      (ensure-equal
       :eof
       (http-kit/http2::%h2-read-frame
        reader 16384 nil (function http-kit::%monotonic-time) :slice-p t))))
  (deftest http2-reader-generic-listen-eof-rejects-partial-frame
    (let ((wire (h2-frame 4 0 0 (octets 0 4 0 0 0 2))))
      (dolist (partial-length (list 5 12))
        (let* ((peer
                 (make-instance (quote h2-manager-capacity-peer-stream)
                                :input (subseq wire 0 partial-length)))
               (stream
                 (make-instance (quote h2-generic-listen-stream-wrapper)
                                :delegate peer))
               (reader (http-kit/http2::%h2-reader-for stream))
               (failure nil))
          (h2-manager-capacity-peer-close peer)
          (handler-case
              (http-kit/http2::%h2-read-frame
               reader 16384 nil (function http-kit::%monotonic-time) :slice-p t)
            (http-kit:http-protocol-error (condition)
              (setf failure condition)))
          (ensure-true
           (typep failure (quote http-kit:http-protocol-error))))))))
