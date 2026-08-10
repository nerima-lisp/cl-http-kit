(in-package #:http-kit/test)

(deftest http2-accepts-settings-initial-window
  (let ((response
          (http-kit/http2:send-http2-request
           (http-kit/http2:make-http2-client
            :exchange (lambda (request wire &key timeout deadline)
                        (declare (ignore request wire timeout deadline))
                        (concatenate-octets
                         (h2-frame 4 0 0 (octets 0 4 0 0 #xff #xfe))
                         (h2-frame 1 5 1 (octets #x88)))))
           (make-http-request :method "GET" :uri "https://127.0.0.1/"))))
    (ensure-equal 200 (http-response-status response)
                  "HTTP/2 accepts a non-default initial window")))

(deftest http2-rejects-invalid-settings-values
  (signals http-protocol-error
    (http-kit/http2:send-http2-request
     (http-kit/http2:make-http2-client
      :exchange (lambda (request wire &key timeout deadline)
                  (declare (ignore request wire timeout deadline))
                  (h2-frame 4 0 0 (octets 0 2 0 0 0 1))))
     (make-http-request :method "GET" :uri "https://127.0.0.1/"))))

(deftest http2-rejects-settings-on-a-stream
  (let ((bad-settings (h2-frame 4 0 0 (octets))))
    (setf (aref bad-settings 5) #x80)
    (signals http-protocol-error
      (http-kit/http2:send-http2-request
       (http-kit/http2:make-http2-client
        :exchange (lambda (request wire &key timeout deadline)
                    (declare (ignore request wire timeout deadline))
                    bad-settings))
       (make-http-request :method "GET" :uri "https://127.0.0.1/")))))

(deftest http2-rejects-priority-on-connection-stream
  (signals http-protocol-error
    (http-kit/http2:send-http2-request
     (http-kit/http2:make-http2-client
      :exchange (lambda (request wire &key timeout deadline)
                  (declare (ignore request wire timeout deadline))
                  (concatenate-octets
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 2 0 0 (octets #x80 0 0 1)))))
     (make-http-request :method "GET" :uri "https://127.0.0.1/"))))

(deftest http2-ignores-priority-frame-on-stream
  (let ((response
          (http-kit/http2:send-http2-request
           (http-kit/http2:make-http2-client
            :exchange (lambda (request wire &key timeout deadline)
                        (declare (ignore request wire timeout deadline))
                        (concatenate-octets
                         (h2-frame 4 0 0 (octets))
                         (h2-frame 2 0 1 (octets 0 0 0 0 0))
                         (h2-frame 1 5 1 (octets #x88)))))
           (make-http-request :method "GET" :uri "https://127.0.0.1/"))))
    (ensure-equal 200 (http-response-status response)
                  "HTTP/2 ignores a valid stream PRIORITY frame")))

(deftest http2-rejects-headers-on-connection-stream
  (signals http-protocol-error
    (http-kit/http2:send-http2-request
     (http-kit/http2:make-http2-client
      :exchange (lambda (request wire &key timeout deadline)
                  (declare (ignore request wire timeout deadline))
                  (concatenate-octets
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 5 0 (octets #x88)))))
     (make-http-request :method "GET" :uri "https://127.0.0.1/"))))

(deftest http2-rejects-unexpected-data-frame
  (signals http-protocol-error
    (http-kit/http2:send-http2-request
     (http-kit/http2:make-http2-client
      :exchange (lambda (request wire &key timeout deadline)
                  (declare (ignore request wire timeout deadline))
                  (concatenate-octets
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 4 1 (octets #x88))
                   (h2-frame 0 1 0 (octets 1)))))
     (make-http-request :method "GET" :uri "https://127.0.0.1/"))))

(deftest http2-rejects-invalid-data-end-stream
  (signals http-protocol-error
    (http-kit/http2:send-http2-request
     (http-kit/http2:make-http2-client
      :exchange (lambda (request wire &key timeout deadline)
                  (declare (ignore request wire timeout deadline))
                  (concatenate-octets
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 4 1 (octets #x89))
                   (h2-frame 0 1 1 (octets 1)))))
     (make-http-request :method "GET" :uri "https://127.0.0.1/"))))

(deftest http2-rejects-ping-on-a-stream
  (signals http-protocol-error
    (http-kit/http2:send-http2-request
     (http-kit/http2:make-http2-client
      :exchange (lambda (request wire &key timeout deadline)
                  (declare (ignore request wire timeout deadline))
                  (concatenate-octets
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 6 0 1 (octets 0 0 0 0 0 0 0 0))
                   (h2-frame 1 5 1 (octets #x88)))))
     (make-http-request :method "GET" :uri "https://127.0.0.1/"))))

(deftest http2-ignores-unknown-settings
  (let ((response
          (http-kit/http2:send-http2-request
           (http-kit/http2:make-http2-client
            :exchange (lambda (request wire &key timeout deadline)
                        (declare (ignore request wire timeout deadline))
                        (concatenate-octets
                         (h2-frame 4 0 0 (octets #x12 #x34 0 0 0 1))
                         (h2-frame 1 5 1 (octets #x88)))))
           (make-http-request :method "GET" :uri "https://127.0.0.1/"))))
    (ensure-equal 200 (http-response-status response)
                  "unknown HTTP/2 SETTINGS is ignored")))

(deftest http2-enforces-body-size-limit
  (signals http-size-limit-exceeded
    (http-kit/http2:send-http2-request
     (http-kit/http2:make-http2-client
      :exchange (lambda (request wire &key timeout deadline)
                  (declare (ignore request wire timeout deadline))
                  (h2-response-wire (octets 1 2 3))))
     (make-http-request :method "GET" :uri "https://127.0.0.1/")
     :max-body-bytes 0)))
