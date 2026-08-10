(in-package #:http-kit/test)

(deftest http2-client-and-settings-boundaries
  (signals http-protocol-error
    (http-kit/http2:make-http2-client))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :open-stream #'identity))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :close-stream 7))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :max-frame-size 16383))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :max-frame-size #x1000000))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :max-frame-size "bad"))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :max-header-bytes 0))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :max-header-bytes "bad"))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :max-body-bytes -1))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :max-body-bytes "bad"))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :clock-function 7))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :clock-function nil))
  (ensure-true
   (http-kit/http2:http2-client-p
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :max-body-bytes 0)))
  (multiple-value-bind (max-frame max-table initial-window)
      (http-kit/http2::%h2-settings
       (octets 0 1 0 0 0 42
               0 5 0 0 64 0
               0 4 0 0 255 255))
    (ensure-equal 16384 max-frame)
    (ensure-equal 42 max-table)
    (ensure-equal 65535 initial-window))
  (signals http-protocol-error
    (http-kit/http2::%h2-settings (octets 0 1 0)))
  (signals http-protocol-error
    (http-kit/http2::%h2-settings (octets 0 0 0 0 0 0)))
  (signals http-protocol-error
    (http-kit/http2::%h2-settings (octets 0 2 0 0 0 1)))
  (signals http-protocol-error
    (http-kit/http2::%h2-settings (octets 0 4 128 0 0 0)))
  (signals http-protocol-error
    (http-kit/http2::%h2-settings (octets 0 5 0 0 63 255)))
  (multiple-value-bind (max-frame max-table initial-window)
      (http-kit/http2::%h2-settings (octets 0 4 0 0 255 254))
    (ensure-equal nil max-frame)
    (ensure-equal nil max-table)
    (ensure-equal 65534 initial-window)))
