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
                       :automatic-decompression-p nil
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
                     :automatic-decompression-p nil
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

#+sbcl
(progn
  (defclass tls-lifecycle-test-stream (binary-test-stream)
    ((closed-p :initform nil :accessor tls-lifecycle-test-stream-closed-p)))

  (defmethod open-stream-p ((stream tls-lifecycle-test-stream))
    (not (tls-lifecycle-test-stream-closed-p stream)))

  (defmethod close ((stream tls-lifecycle-test-stream) &key abort)
    (declare (ignore abort))
    (setf (tls-lifecycle-test-stream-closed-p stream) t))

  (defun tls-record-bound-test-wire (type length)
    (let ((wire (make-array (+ 5 length) :element-type '(unsigned-byte 8)
                                          :initial-element 0)))
      (replace wire (octets type 3 3 (ldb (byte 8 8) length)
                           (ldb (byte 8 0) length)))
      wire))

  (defun tls-record-bound-test-driver (raw state handshake-key-p application-key-p)
    (let* ((provider (cl-tls-kit:make-cl-crypto-kit-provider))
           (driver (cl-tls-kit:make-tls13-client-driver
                    :provider provider
                    :transport (http-kit/tls::%tls-transport raw)))
           (read-state
             (when (or handshake-key-p application-key-p)
               (cl-tls-kit:make-tls13-traffic-state
                provider :sha256 :aes-128-gcm
                (make-array 32 :element-type '(unsigned-byte 8)
                               :initial-element 1)
                16 12))))
      (setf (cl-tls-kit::tls13-client-driver-state driver) state
            (cl-tls-kit::tls13-client-driver-handshake-read-state driver)
            (and handshake-key-p read-state)
            (cl-tls-kit::tls13-client-driver-application-read-state driver)
            (and application-key-p read-state))
      driver))

  (defun tls-record-read-bound-cases ()
    ;; Fixed RFC 8446 limits, independent of the implementation's calculation.
    '((:awaiting-server-hello nil nil 22 16384)
      (:awaiting-server-hello nil nil 23 16384)
      (:awaiting-finished t nil 23 16640)
      (:connected t t 23 16640)
      (:connected t nil 23 16384)
      (:awaiting-server-hello nil t 23 16384)
      (:connected t t 20 16384)
      (:connected t t 21 16384)
      (:connected t t 22 16384)))

  (deftest native-tls-record-read-bound-maximum
    (dolist (case (tls-record-read-bound-cases))
      (destructuring-bind (state handshake-key-p application-key-p type limit) case
        (let* ((wire (tls-record-bound-test-wire type limit))
               (raw (make-instance 'binary-test-stream :input wire))
               (driver (tls-record-bound-test-driver
                        raw state handshake-key-p application-key-p)))
          (ensure-octets-equal
           wire (funcall (getf (http-kit/tls::%tls-transport raw) :read) driver))
          (ensure-equal (+ 5 limit)
                        (http-kit/test-core::binary-test-input-position raw))))))

  (deftest native-tls-record-read-bound-overflow
    (dolist (case (tls-record-read-bound-cases))
      (destructuring-bind (state handshake-key-p application-key-p type limit) case
        (dolist (length (list (1+ limit) 65535))
          (let* ((raw (make-instance 'binary-test-stream
                                    :input (tls-record-bound-test-wire type length)))
                 (driver (tls-record-bound-test-driver
                          raw state handshake-key-p application-key-p))
                 (condition
                   (handler-case
                       (progn
                         (funcall (getf (http-kit/tls::%tls-transport raw) :read)
                                  driver)
                         nil)
                     (http-protocol-error (condition) condition))))
            (ensure-true condition)
            (when condition
              (ensure-equal :record-overflow
                            (http-kit:http-protocol-error-detail condition)))
            (ensure-equal 5
                          (http-kit/test-core::binary-test-input-position raw)))))))

  (deftest native-tls-record-truncation
    (dolist (wire (list (octets 23)
                       (octets 23 3)
                       (octets 23 3 3)
                       (octets 23 3 3 0)
                       (octets 23 3 3 0 2)
                       (octets 23 3 3 0 2 1)))
      (signals http-protocol-error
        (http-kit/tls::%tls-read-record
         (make-instance 'binary-test-stream :input wire))))
    (ensure-equal nil
                  (http-kit/tls::%tls-read-record
                   (make-instance 'binary-test-stream :input (octets))))
    (ensure-octets-equal
     (octets 23 3 3 0 2 1 2)
     (http-kit/tls::%tls-read-record
      (make-instance 'binary-test-stream :input (octets 23 3 3 0 2 1 2))))
    (ensure-octets-equal
     (octets 23 3 3 0 0)
     (http-kit/tls::%tls-read-record
      (make-instance 'binary-test-stream :input (octets 23 3 3 0 0)))))

  (deftest native-tls-close-notify-eof
    (let* ((provider (cl-tls-kit:make-cl-crypto-kit-provider))
           (secret (make-array 32 :element-type '(unsigned-byte 8)
                                 :initial-element 1))
           (write-state (cl-tls-kit:make-tls13-traffic-state
                         provider :sha256 :aes-128-gcm secret 16 12))
           (read-state (cl-tls-kit:make-tls13-traffic-state
                        provider :sha256 :aes-128-gcm secret 16 12))
           (wire (cl-tls-kit:encrypt-tls13-traffic-record
                  (cl-tls-kit:make-tls-plaintext
                   cl-tls-kit:+tls-content-type-alert+ (octets 1 0))
                  write-state))
           (reads 0)
           (raw (make-instance 'tls-lifecycle-test-stream :input (octets)))
           (driver
             (cl-tls-kit:make-tls13-client-driver
              :provider provider
              :transport-read
              (lambda (driver)
                (declare (ignore driver))
                (incf reads)
                (if (= reads 1) wire (error "Read after close_notify.")))))
           (stream (http-kit/tls::%make-http-tls-stream driver raw)))
      (setf (cl-tls-kit::tls13-client-driver-state driver) :connected
            (cl-tls-kit::tls13-client-driver-application-read-state driver)
            read-state)
      (unwind-protect
           (progn
             (ensure-equal :eof (read-byte stream nil :eof))
             (ensure-equal :eof (read-byte stream nil :eof))
             (ensure-equal 1 reads))
        (close stream :abort t))))

  (deftest native-tls-unauthenticated-eof-rejected
    (let* ((raw (make-instance 'tls-lifecycle-test-stream :input (octets)))
           (driver (cl-tls-kit:make-tls13-client-driver
                    :provider (cl-tls-kit:make-cl-crypto-kit-provider)
                    :transport (http-kit/tls::%tls-transport raw)))
           (stream (http-kit/tls::%make-http-tls-stream driver raw)))
      (setf (cl-tls-kit::tls13-client-driver-state driver) :connected)
      (unwind-protect
           (signals http-protocol-error
             (read-byte stream nil :eof))
        (close stream :abort t))))

  (deftest native-tls-close-releases-peer-closed-transport
    (let* ((raw (make-instance 'tls-lifecycle-test-stream :input (octets)))
           (driver (cl-tls-kit:make-tls13-client-driver
                    :provider (cl-tls-kit:make-cl-crypto-kit-provider)
                    :transport (http-kit/tls::%tls-transport raw)))
           (stream (http-kit/tls::%make-http-tls-stream driver raw)))
      (setf (cl-tls-kit::tls13-client-driver-state driver) :closed
            (cl-tls-kit::tls13-client-driver-close-notify-received driver) t)
      (close stream)
      (ensure-false (open-stream-p raw))
      (ensure-false (open-stream-p stream))))

  (deftest native-tls-setup-failure-releases-transport
    (let ((raw (make-instance 'tls-lifecycle-test-stream :input (octets)))
          (upgrade (make-http-tls-upgrader :trust-anchors (list t))))
      (signals type-error
        (funcall upgrade raw nil))
      (ensure-false (open-stream-p raw))))

  (deftest native-tls-close-write-failure-releases-transport
    (let* ((raw (make-instance 'tls-lifecycle-test-stream :input (octets)))
           (failure (make-condition 'simple-error
                                    :format-control "close_notify write failed"))
           (driver (cl-tls-kit:make-tls13-client-driver
                    :provider (cl-tls-kit:make-cl-crypto-kit-provider)
                    :transport-read (lambda (driver)
                                      (declare (ignore driver))
                                      nil)
                    :transport-write (lambda (driver wire)
                                       (declare (ignore driver wire))
                                       (error failure))))
           (stream (http-kit/tls::%make-http-tls-stream driver raw)))
      (setf (cl-tls-kit::tls13-client-driver-state driver) :connected)
      (ensure-true
       (eq failure (handler-case (close stream)
                     (error (condition) condition))))
      (ensure-false (open-stream-p raw))
      (ensure-false (open-stream-p stream)))))

#+sbcl
(deftest network-public-listener-defaults-contract
  (let* ((seams (list '(setf sb-bsd-sockets:sockopt-reuse-address)
                      'sb-bsd-sockets:socket-bind 'sb-bsd-sockets:socket-listen))
         (originals (mapcar #'fdefinition seams))
         (events nil)
         (listener nil))
    (unwind-protect
         (progn
           (setf (fdefinition (first seams))
                 (lambda (value socket)
                   (push (list :reuse value) events)
                   (funcall (first originals) value socket))
                 (fdefinition (second seams))
                 (lambda (socket address port)
                   (push (list :bind port) events)
                   (funcall (second originals) socket address port))
                 (fdefinition (third seams))
                 (lambda (socket backlog)
                   (push (list :listen backlog) events)
                   (funcall (third originals) socket backlog)))
           (setf listener (open-http-tcp-listener :host "127.0.0.1"))
           (ensure-equal '((:reuse t) (:bind 0) (:listen 128)) (reverse events))
           (ensure-equal "127.0.0.1" (http-network-listener-address listener))
           (ensure-equal :ipv4 (http-network-listener-address-family listener))
           (ensure-true (< 0 (http-network-listener-port listener) 65536))
           (ensure-equal nil (close-http-tcp-listener listener))
           (ensure-equal nil (close-http-tcp-listener listener))
           (let ((caught (handler-case (accept-http-tcp-stream listener :timeout 0)
                           (http-protocol-error (condition) condition))))
             (ensure-true (typep caught 'http-protocol-error))
             (ensure-equal :accept (http-error-operation caught))
             (ensure-equal :closed-listener (http-protocol-error-detail caught))))
      (loop for seam in seams for original in originals
            do (setf (fdefinition seam) original))
      (close-http-tcp-listener listener))
    (loop for seam in seams for original in originals
          do (ensure-true (eq original (fdefinition seam))))))
