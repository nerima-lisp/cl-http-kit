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

  (deftest http2-public-connection-cancel-callback-default-code
    (let* ((stream (make-instance 'binary-session-stream
                                  :input (h2-response-wire (octets))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream))
           (opened-stream-id nil)
           (response
             (http-kit/http2:send-http2-request-over-connection
              connection
              (make-http-request :method "GET"
                                 :uri "https://127.0.0.1/cancel")
              :on-stream-open
              (lambda (stream-id cancel-function)
                (setf opened-stream-id stream-id)
                (funcall cancel-function))))
           (reader
             (http-kit/http2::%h2-reader-for
              (subseq (binary-session-output stream)
                      (length (h2-preface)))))
           (reset-frame nil))
      (loop for frame = (http-kit/http2::%h2-read-frame
                         reader 16384 nil nil)
            until (eq frame :eof)
            when (= 3 (http-kit/http2::%h2-frame-type frame))
              do (setf reset-frame frame))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal 1 opened-stream-id)
      (ensure-true reset-frame)
      (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id reset-frame))
      (ensure-equal (octets 0 0 0 8)
                    (http-kit/http2::%h2-frame-payload reset-frame))
      (http-kit/http2:close-http2-connection connection)))

  (deftest http2-public-connection-cancel-custom-and-direct-api
    (let* ((stream (make-instance 'binary-session-stream
                                  :input (h2-response-wire (octets))))
           (connection
             (http-kit/http2:make-http2-connection :stream stream)))
      (http-kit/http2:send-http2-request-over-connection
       connection
       (make-http-request :method "GET"
                          :uri "https://127.0.0.1/cancel-custom")
       :on-stream-open
       (lambda (stream-id cancel-function)
         (ensure-equal 1 stream-id)
         (funcall cancel-function :error-code 7)))
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
                      (nreverse reset-payloads)))
      (http-kit/http2:close-http2-connection connection)))

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
         (goaway-completed-while-locked-p nil))
    (unwind-protect
         (progn
           (http-kit/http2:send-http2-request-over-connection
            connection
            (make-http-request :method "GET" :uri "https://127.0.0.1/first"))
           (sb-thread:with-mutex
               ((http-kit/http2::%http2-connection-scheduler-lock connection))
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
                   goaway-completed-while-locked-p
                   (sb-thread:wait-on-semaphore goaway-completed :timeout 0.05)
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
           (sb-thread:join-thread goaway-thread)
           (setf goaway-thread nil)
           (sb-thread:join-thread request-thread)
           (setf request-thread nil)
           (ensure-true goaway-started-p)
           (ensure-true request-started-p)
           (ensure-true (not goaway-completed-while-locked-p))
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
             (ensure-equal :http2-read (http-error-operation request-error))
             (ensure-equal :eof (http-connection-error-cause request-error))
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
     :operation :http2-write :detail :protocol-error))

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
