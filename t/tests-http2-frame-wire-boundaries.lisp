(in-package #:http-kit/test)

(progn
  #+sbcl
  (deftest http2-frame-reader-and-wire-boundaries
    (signals http-protocol-error
      (http-kit/http2::%h2-reader-for
       (make-array (quote (1 1))
                   :element-type (quote (unsigned-byte 8))
                   :initial-element 0)))
    (signals http-protocol-error
      (http-kit/http2::%h2-reader-for (quote (:bad))))
    (let ((reader
            (http-kit/http2::%h2-reader-for
             (make-instance (quote binary-test-stream) :input (octets)))))
      (ensure-equal :eof
                    (http-kit/http2::%h2-reader-read
                     reader 1 nil (lambda () 0d0) :allow-eof t)))
    (let ((reader
            (http-kit/http2::%h2-reader-for
             (make-instance (quote binary-test-stream) :input (octets 1)))))
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
       16384 nil (lambda () 0d0)))))
(progn
  #+sbcl
  (deftest http2-fd-frame-reader-preserves-partial-header-after-timeout
    (let* ((listener
             (make-instance (quote sb-bsd-sockets:inet-socket)
                            :type :stream
                            :protocol :tcp))
           (port nil)
           (client-socket nil)
           (client-stream nil)
           (accepted-socket nil)
           (server-stream nil)
           (server-thread nil)
           (server-error nil)
           (frame-wire (h2-frame 6 0 0 (octets 1 2 3 4 5 6 7 8)))
           (clock-function
             (lambda ()
               (/ (get-internal-real-time)
                  internal-time-units-per-second)))
           (first-write-ready (sb-thread:make-semaphore :count 0))
           (send-rest (sb-thread:make-semaphore :count 0))
           (rest-write-ready (sb-thread:make-semaphore :count 0))
           (server-done (sb-thread:make-semaphore :count 0)))
      (unwind-protect
           (progn
             (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
             (sb-bsd-sockets:socket-listen listener 1)
             (setf port
                   (nth-value 1 (sb-bsd-sockets:socket-name listener))
                   client-socket
                   (make-instance (quote sb-bsd-sockets:inet-socket)
                                  :type :stream
                                  :protocol :tcp))
             (sb-bsd-sockets:socket-connect
              client-socket #(127 0 0 1) port)
             (setf client-stream
                   (sb-bsd-sockets:socket-make-stream
                    client-socket
                    :input t
                    :output t
                    :element-type (quote (unsigned-byte 8))
                    :buffering :none))
             (setf server-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (handler-case
                               (progn
                                 (setf accepted-socket
                                       (sb-bsd-sockets:socket-accept listener)
                                       server-stream
                                       (sb-bsd-sockets:socket-make-stream
                                        accepted-socket
                                        :input t
                                        :output t
                                        :element-type
                                        (quote (unsigned-byte 8))
                                        :buffering :none))
                                 (write-sequence
                                  (subseq frame-wire 0 4) server-stream)
                                 (finish-output server-stream)
                                 (sb-thread:signal-semaphore first-write-ready)
                                 (unless
                                     (sb-thread:wait-on-semaphore
                                      send-rest :timeout 5)
                                   (error
                                    "The frame test timed out before sending the remainder."))
                                 (write-sequence
                                  (subseq frame-wire 4) server-stream)
                                 (finish-output server-stream)
                                 (sb-thread:signal-semaphore rest-write-ready))
                             (error (condition)
                               (setf server-error condition)))
                        (sb-thread:signal-semaphore server-done)))))
             (ensure-true
              (sb-thread:wait-on-semaphore first-write-ready :timeout 5))
             (let* ((reader (http-kit/http2::%h2-reader-for client-stream))
                    (timed-out-p
                      (handler-case
                          (progn
                            (http-kit/http2::%h2-read-frame
                             reader 16384
                             (+ (funcall clock-function) 0.05d0)
                             clock-function)
                            nil)
                        (http-kit:http-timeout () t))))
               (ensure-true timed-out-p)
               (ensure-equal
                4
                (http-kit/http2::%h2-reader-frame-header-position reader))
               (sb-thread:signal-semaphore send-rest)
               (ensure-true
                (sb-thread:wait-on-semaphore rest-write-ready :timeout 5))
               (let ((frame
                       (http-kit/http2::%h2-read-frame
                        reader 16384
                        (+ (funcall clock-function) 5.0d0)
                        clock-function)))
                 (ensure-equal 6
                               (http-kit/http2::%h2-frame-type frame))
                 (ensure-equal 0
                               (http-kit/http2::%h2-frame-flags frame))
                 (ensure-equal 0
                               (http-kit/http2::%h2-frame-stream-id frame))
                 (ensure-equal (octets 1 2 3 4 5 6 7 8)
                               (http-kit/http2::%h2-frame-payload frame))))
             (ensure-true
              (sb-thread:wait-on-semaphore server-done :timeout 5))
             (sb-thread:join-thread server-thread)
             (setf server-thread nil)
             (ensure-false server-error))
        (when server-thread
          (sb-thread:signal-semaphore send-rest)
          (when client-stream
            (ignore-errors (close client-stream :abort t)))
          (when server-stream
            (ignore-errors (close server-stream :abort t)))
          (sb-thread:join-thread server-thread))
        (when client-stream
          (ignore-errors (close client-stream :abort t)))
        (when server-stream
          (ignore-errors (close server-stream :abort t)))
        (when client-socket
          (ignore-errors (sb-bsd-sockets:socket-close client-socket)))
        (when accepted-socket
          (ignore-errors (sb-bsd-sockets:socket-close accepted-socket)))
        (ignore-errors (sb-bsd-sockets:socket-close listener))))))
