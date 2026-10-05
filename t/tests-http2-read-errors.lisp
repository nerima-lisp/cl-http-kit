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
       (http-kit/http2::%make-hpack-context) nil nil
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
