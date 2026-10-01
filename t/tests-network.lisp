(in-package #:http-kit/test)

#+sbcl
(progn
  (defun %network-test-close-stream (stream)
    (when stream
      (http-kit::%with-http-cleanup (close stream :abort t)))
    nil)

  (defun %network-test-close-socket (socket)
    (when socket
      (http-kit::%with-http-cleanup (sb-bsd-sockets:socket-close socket)))
    nil)

  (defun %network-test-listener ()
    (let ((socket
            (make-instance 'sb-bsd-sockets:inet-socket
                           :type :stream
                           :protocol :tcp)))
      (handler-case
          (progn
            (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
            (sb-bsd-sockets:socket-listen socket 1)
            (multiple-value-bind (address port)
                (sb-bsd-sockets:socket-name socket)
              (declare (ignore address))
              (values socket port)))
        (error (condition)
          (%network-test-close-socket socket)
          (error condition)))))

  (defun %network-test-read-headers (stream)
    (let ((bytes (make-array 0
                             :element-type '(unsigned-byte 8)
                             :adjustable t
                             :fill-pointer 0)))
      (loop
        for byte = (read-byte stream nil nil)
        do (unless byte
             (error "The test server reached EOF before the request headers."))
           (vector-push-extend byte bytes)
           (let ((length (length bytes)))
             (when (and (>= length 4)
                        (= (aref bytes (- length 4)) 13)
                        (= (aref bytes (- length 3)) 10)
                        (= (aref bytes (- length 2)) 13)
                        (= (aref bytes (- length 1)) 10))
               (return (let ((copy (make-array length
                                               :element-type '(unsigned-byte 8))))
                         (replace copy bytes)
                         copy)))
             (when (> length 65536)
               (error "The test server received oversized request headers.")))))))

  (deftest native-network-http1-client
    (multiple-value-bind (listener port)
        (%network-test-listener)
      (let ((server-thread nil)
            (accepted-socket nil)
            (server-stream nil)
            (request-wire nil)
            (server-error nil))
        (unwind-protect
             (progn
               #+sbcl
               (setf server-thread
                     (sb-thread:make-thread
                      (lambda ()
                        (handler-case
                            (progn
                              #+sbcl
                              (setf accepted-socket
                                    (sb-bsd-sockets:socket-accept listener))
                              #+sbcl
                              (setf server-stream
                                    (sb-bsd-sockets:socket-make-stream
                                     accepted-socket
                                     :input t
                                     :output t
                                     :element-type '(unsigned-byte 8)
                                     :buffering :full))
                              (setf request-wire
                                    (%network-test-read-headers server-stream))
                              (write-sequence
                               (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF|Connection: close|CRLF||CRLF|ok")
                               server-stream)
                              (finish-output server-stream))
                          (error (condition)
                            (setf server-error condition)))
                        (%network-test-close-stream server-stream)
                        (%network-test-close-socket accepted-socket))))
               (let* ((client
                       (make-http-client
                         :automatic-decompression-p nil
                         :open-stream (make-http-network-stream-opener)
                         :close-stream #'close-http-tcp-stream))
                      (request
                        (http-client-request
                         client "GET"
                         (format nil "http://127.0.0.1:~D/" port))))
                 (multiple-value-bind (response effective-request)
                     (http-client-send client request :timeout 5)
                   (ensure-equal 200 (http-response-status response))
                   (ensure-equal "ok"
                                 (octets-as-string (http-response-body response)))
                   (ensure-equal request effective-request)))
               #+sbcl
               (sb-thread:join-thread server-thread)
               (ensure-true (null server-error))
               (ensure-true request-wire)
               (ensure-true
                (search "GET / HTTP/1.1"
                        (octets-as-string request-wire))))
          (when server-thread
            (%network-test-close-stream server-stream)
            (%network-test-close-socket accepted-socket)
            (%network-test-close-socket listener)
              (http-kit::%with-http-cleanup
                #+sbcl
                (sb-thread:join-thread server-thread)))
          (unless server-thread
            (%network-test-close-socket listener))))))

  (deftest native-network-dns-and-deadline
    (ensure-equal "127.0.0.1"
                  (http-network-resolve-host "127.0.0.1"))
    (ensure-equal
     "0000:0000:0000:0000:0000:0000:0000:0001"
     (http-network-resolve-host "::1"))
    (let ((request (make-http-request
                    :method "GET"
                    :uri "http://127.0.0.1/")))
      (signals http-timeout
        (open-http-tcp-stream
         request
         :deadline 0
         :clock-function (lambda () 1)))))

  (deftest native-network-read-deadline
    (multiple-value-bind (listener port)
        (%network-test-listener)
      (let ((server-thread nil)
            (accepted-socket nil)
            (server-stream nil))
        (unwind-protect
             (progn
               (setf server-thread
                     (sb-thread:make-thread
                      (lambda ()
                        (setf accepted-socket
                              (sb-bsd-sockets:socket-accept listener))
                        (setf server-stream
                              (sb-bsd-sockets:socket-make-stream
                               accepted-socket
                               :input t
                               :output t
                               :element-type '(unsigned-byte 8)
                               :buffering :full))
                        (sleep 1))))
               (let ((stream
                       (open-http-tcp-stream
                        (make-http-request
                         :method "GET"
                         :uri (format nil "http://127.0.0.1:~D/" port)))))
                 (unwind-protect
                      (signals http-timeout
                        (parse-http-response stream :timeout 0.01))
                   (close-http-tcp-stream stream))))
          (when server-thread
            (%network-test-close-stream server-stream)
            (%network-test-close-socket accepted-socket)
            (http-kit::%with-http-cleanup
              (sb-thread:join-thread server-thread)))
          (%network-test-close-socket listener)))))

  (deftest native-network-listener-accept
    (let ((listener (open-http-tcp-listener
                     :host "127.0.0.1"
                     :port 0))
          (server-thread nil)
          (server-error nil)
          (peer-address nil)
          (peer-port nil))
      (unwind-protect
           (progn
             (ensure-true (http-network-listener-p listener))
             (ensure-equal :ipv4
                           (http-network-listener-address-family listener))
             (ensure-equal "127.0.0.1"
                           (http-network-listener-address listener))
             (ensure-true (plusp (http-network-listener-port listener)))
              #+sbcl
              (setf server-thread
                    (sb-thread:make-thread
                     (lambda ()
                       (handler-case
                           (multiple-value-bind (stream address port)
                               (accept-http-tcp-stream listener :timeout 5)
                             (setf peer-address address
                                   peer-port port)
                             (let ((request-wire
                                     (%network-test-read-headers stream)))
                               (ensure-true
                                (search "GET / HTTP/1.1"
                                        (octets-as-string request-wire)))
                               (write-sequence
                                (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF|Connection: close|CRLF||CRLF|ok")
                                stream)
                               (finish-output stream)
                               (%network-test-close-stream stream)))
                         (error (condition)
                           (setf server-error condition))))))
             (let* ((client
                      (make-http-client
                       :open-stream (make-http-network-stream-opener)
                       :close-stream #'close-http-tcp-stream))
                    (request
                      (http-client-request
                       client "GET"
                       (format nil "http://127.0.0.1:~D/"
                               (http-network-listener-port listener)))))
               (multiple-value-bind (response effective-request)
                   (http-client-send client request :timeout 5)
                 (ensure-equal 200 (http-response-status response))
                 (ensure-equal "ok"
                               (octets-as-string (http-response-body response)))
                 (ensure-equal request effective-request)))
             #+sbcl
             (sb-thread:join-thread server-thread)
             (ensure-true (null server-error))
             (ensure-equal "127.0.0.1" peer-address)
             (ensure-true (plusp peer-port)))
        (when server-thread
          (http-kit::%with-http-cleanup
            #+sbcl
            (sb-thread:join-thread server-thread)))
        (close-http-tcp-listener listener))))

#+sbcl
(deftest native-network-http1-listener-service
  (let ((listener (open-http-tcp-listener
                   :host "127.0.0.1"
                   :port 0))
        (server-thread nil)
        (server-error nil)
        (served-count nil)
        (termination nil)
        (accepted-address nil)
        (accepted-port nil))
    (unwind-protect
         (progn
           (setf server-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (multiple-value-bind (count reason)
                            (serve-http1-listener
                             listener
                             (lambda (request)
                               (declare (ignore request))
                               (make-http-response
                                :status 200
                                :headers
                                (list (make-http-header
                                       "connection"
                                       "close"))
                                :body (ascii "ok")))
                             :max-connections 1
                             :on-accept
                             (lambda (stream address port)
                               (declare (ignore stream))
                               (setf accepted-address address
                                     accepted-port port))
                             :session-options (list :max-requests 1))
                          (setf served-count count
                                termination reason))
                      (error (condition)
                        (setf server-error condition))))))
           (let* ((client
                    (make-http-client
                     :open-stream (make-http-network-stream-opener)
                     :close-stream #'close-http-tcp-stream))
                  (request
                    (http-client-request
                     client
                     "GET"
                     (format nil "http://127.0.0.1:~D/"
                             (http-network-listener-port listener)))))
             (multiple-value-bind (response effective-request)
                 (http-client-send client request :timeout 5)
               (ensure-equal 200 (http-response-status response))
               (ensure-equal "ok"
                             (octets-as-string (http-response-body response)))
               (ensure-equal request effective-request)))
           (sb-thread:join-thread server-thread)
           (ensure-true (null server-error))
           (ensure-equal 1 served-count)
           (ensure-equal :max-connections termination)
           (ensure-equal "127.0.0.1" accepted-address)
           (ensure-true (plusp accepted-port)))
      (when server-thread
        (http-kit::%with-http-cleanup
          (sb-thread:join-thread server-thread)))
      (close-http-tcp-listener listener))))

#-sbcl
(deftest native-network-unavailable
  (signals http-unsupported-feature
    (http-network-resolve-host "127.0.0.1")))
