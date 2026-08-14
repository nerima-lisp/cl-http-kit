(in-package #:http-kit/test)

#+sbcl
(progn
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
