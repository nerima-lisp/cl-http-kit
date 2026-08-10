(in-package #:http-kit/test)

#+sbcl
(deftest http2-frame-reader-and-wire-boundaries
  (signals http-protocol-error
    (http-kit/http2::%h2-reader-for
     (make-array '(1 1)
                 :element-type '(unsigned-byte 8)
                 :initial-element 0)))
  (signals http-protocol-error
    (http-kit/http2::%h2-reader-for '(1 :bad)))
  (let ((reader
          (http-kit/http2::%h2-reader-for
           (make-instance 'binary-test-stream :input (octets)))))
    (ensure-equal :eof
                  (http-kit/http2::%h2-reader-read
                   reader 1 nil (lambda () 0d0) :allow-eof t)))
  (let ((reader
          (http-kit/http2::%h2-reader-for
           (make-instance 'binary-test-stream :input (octets 1)))))
    (signals http-protocol-error
      (http-kit/http2::%h2-reader-read
       reader 2 nil (lambda () 0d0) :allow-eof t)))
  (let ((reader
          (http-kit/http2::%h2-reader-for (octets 1))))
    (signals http-protocol-error
      (http-kit/http2::%h2-reader-read
       reader :invalid nil (lambda () 0d0))))
  (signals http-protocol-error
    (http-kit/http2::%h2-frame-wire -1 0 0 (octets)))
  (signals http-protocol-error
    (http-kit/http2::%h2-frame-wire "type" 0 0 (octets)))
  (signals http-protocol-error
    (http-kit/http2::%h2-frame-wire 0 :invalid 0 (octets)))
  (signals http-protocol-error
    (http-kit/http2::%h2-frame-wire 0 0 "stream" (octets)))
  (signals http-protocol-error
    (http-kit/http2::%h2-frame-wire 0 0 #x80000000 (octets)))
  (signals http-protocol-error
    (http-kit/http2::%h2-read-frame
     (http-kit/http2::%h2-reader-for
      (h2-frame 1 0 0 (octets 1)))
     0 nil (lambda () 0d0)))
  (signals http-protocol-error
    (http-kit/http2::%h2-read-frame
     (http-kit/http2::%h2-reader-for
      (h2-frame 0 0 #x80000000 (octets)))
     16384 nil (lambda () 0d0))))
