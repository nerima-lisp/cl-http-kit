(in-package #:http-kit/test)

(deftest http2-accepts-connect-method
  (let ((response
          (http-kit/http2:send-http2-request
           (http-kit/http2:make-http2-client
            :exchange (lambda (request wire &key timeout deadline)
                        (declare (ignore request wire timeout deadline))
                        (h2-response-wire (octets))))
           (make-http-request :method "CONNECT"
                              :uri "https://127.0.0.1/tunnel"))))
    (ensure-equal 200 (http-response-status response)
                  "HTTP/2 CONNECT request is sent")))

(deftest http2-rejects-large-post-body
  (let ((large-body (make-array 65536 :element-type '(unsigned-byte 8)
                                :initial-element 1)))
    (signals http-unsupported-feature
      (http-kit/http2:send-http2-request
       (http-kit/http2:make-http2-client
        :exchange (lambda (request wire &key timeout deadline)
                    (declare (ignore request wire timeout deadline))
                    (error "must not be called")))
       (make-http-request :method "POST"
                          :uri "https://127.0.0.1/upload"
                          :body large-body)))))

(deftest http2-translates-connection-failure
  (signals http-connection-error
    (http-kit/http2:send-http2-request
     (http-kit/http2:make-http2-client
      :exchange (lambda (request wire &key timeout deadline)
                  (declare (ignore request wire timeout deadline))
                  (error "synthetic HTTP/2 connection failure")))
     (make-http-request :method "GET" :uri "https://127.0.0.1/"))))
