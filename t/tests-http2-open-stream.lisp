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
                                   (h2-frame 0 1 3 (octets 3 4)))))
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
           (first-stream-window-update
             (http-kit/http2::%h2-read-frame
              request-frames 16384 nil nil))
           (first-connection-window-update
             (http-kit/http2::%h2-read-frame
              request-frames 16384 nil nil))
           (second-request-frame
             (http-kit/http2::%h2-read-frame
              request-frames 16384 nil nil))
           (second-stream-window-update
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
                     first-stream-window-update))
      (ensure-equal 1
                    (http-kit/http2::%h2-frame-stream-id
                     first-stream-window-update))
      (ensure-equal 8
                    (http-kit/http2::%h2-frame-type
                     first-connection-window-update))
      (ensure-equal 0
                    (http-kit/http2::%h2-frame-stream-id
                     first-connection-window-update))
      (ensure-equal 3 (http-kit/http2::%h2-frame-stream-id second-request-frame))
      (ensure-equal 8
                    (http-kit/http2::%h2-frame-type
                     second-stream-window-update))
      (ensure-equal 3
                    (http-kit/http2::%h2-frame-stream-id
                     second-stream-window-update))
      (ensure-equal 8
                    (http-kit/http2::%h2-frame-type
                     second-connection-window-update))
      (ensure-equal 0
                    (http-kit/http2::%h2-frame-stream-id
                     second-connection-window-update))
      (http-kit/http2:close-http2-connection connection)
      (ensure-true closed)
      (ensure-true
       (not (http-kit/http2:http2-connection-open-p connection)))))

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
             (context (http-kit/http2::%make-hpack-context)))
        (ensure-equal 3 (length stream-frames))
        (ensure-equal 1 (http-kit/http2::%h2-frame-type initial))
        (ensure-equal 4 (http-kit/http2::%h2-frame-flags initial))
        (ensure-equal 0 (http-kit/http2::%h2-frame-type data))
        (ensure-equal 0 (http-kit/http2::%h2-frame-flags data))
        (ensure-equal (octets 1 2 3)
                      (http-kit/http2::%h2-frame-payload data))
        (ensure-equal 1 (http-kit/http2::%h2-frame-type trailer))
        (ensure-equal 5 (http-kit/http2::%h2-frame-flags trailer))
        (http-kit/http2::%hpack-decode-block
         (http-kit/http2::%h2-frame-payload initial) context)
        (ensure-equal
         (list (cons "x-checksum" "done"))
         (http-kit/http2::%hpack-decode-block
          (http-kit/http2::%h2-frame-payload trailer) context)))
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

  (deftest http2-public-client-streams-produced-body
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
        (ensure-equal 8
                      (count 8 frames
                             :key #'http-kit/http2::%h2-frame-type)))
      (ensure-true
       (http-kit/http2:http2-connection-session-started-p connection))
      (http-kit/http2:close-http2-connection connection)))))

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
        (http-kit/http2:send-http2-request-over-connection
         connection
         (make-http-request :method "GET"
                            :uri "https://127.0.0.1/rejected")))
      (http-kit/http2:close-http2-connection connection)))
