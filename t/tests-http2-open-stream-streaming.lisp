(in-package #:http-kit/test)

#+sbcl
(progn
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
                                 :body body))))
      (multiple-value-bind (data-body data-frame-count settings-ack-count)
          (h2-output-data-summary stream)
        (ensure-equal 200 (http-response-status response))
        (ensure-equal (octets 9 8) (http-response-body response))
        (ensure-equal body data-body)
        (ensure-equal 5 data-frame-count)
        (ensure-equal 1 settings-ack-count)
        (http-kit/http2:close-http2-connection connection))))

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
                                 :body body))))
      (multiple-value-bind (data-body data-frame-count settings-ack-count)
          (h2-output-data-summary stream)
        (ensure-equal 200 (http-response-status response))
        (ensure-equal (octets 9 8) (http-response-body response))
        (ensure-equal body data-body)
        (ensure-true closed)
        (ensure-equal 5 data-frame-count)
        (ensure-equal 1 settings-ack-count))))

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
               :trailers (list (make-http-header "X-Checksum" "done"))))))
      (let* ((stream-frames
               (remove-if-not
                (lambda (frame)
                  (and (= 1 (http-kit/http2::%h2-frame-stream-id frame))
                       (member (http-kit/http2::%h2-frame-type frame)
                               '(0 1))))
                (h2-output-frames stream)))
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

  (deftest http2-public-connection-streams-produced-body-through-window-updates
    (let* ((body (make-array 65536
                             :element-type '(unsigned-byte 8)
                             :initial-element #x5a))
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
           (response nil)
           (request-body-function nil)
           (body-position nil)
           (body-calls nil))
      (multiple-value-setq (request-body-function body-position body-calls)
        (make-octet-body-producer
         body
         :on-maximum-size
         (lambda (maximum-size)
           (ensure-true (plusp maximum-size))
           (push maximum-size maximum-sizes))))
      (setf response
             (http-kit/http2:send-http2-request
              transport
              (make-http-request :method "POST"
                                 :uri "https://127.0.0.1/upload")
              :request-body-function request-body-function
              :request-body-length (length body))))
      (multiple-value-bind (data-body data-frame-count settings-ack-count)
          (h2-output-data-summary stream)
        (ensure-equal 200 (http-response-status response))
        (ensure-equal (octets 9 8) (http-response-body response))
        (ensure-equal body data-body)
        (ensure-equal (length body) (funcall body-position))
        (ensure-true (plusp (funcall body-calls)))
        (ensure-true (every #'plusp maximum-sizes))
        (ensure-equal 5 data-frame-count)
        (ensure-equal 1 settings-ack-count)
        (http-kit/http2:close-http2-connection connection)))

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
              :request-body-length (length body))))
      (multiple-value-bind (data-body)
          (h2-output-data-summary stream)
        (ensure-equal 200 (http-response-status response))
        (ensure-equal (octets 9 8) (http-response-body response))
        (ensure-equal body data-body)
        (ensure-equal (length body) (funcall body-position))
        (ensure-true (plusp (funcall body-calls)))
        (http-kit/http2:close-http2-connection connection))))
