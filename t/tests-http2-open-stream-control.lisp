(in-package #:http-kit/test)

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
      (http-kit/http2:close-http2-connection connection)))

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
      (http-kit/http2:close-http2-connection connection)))

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
      (http-kit/http2:close-http2-connection connection))))
