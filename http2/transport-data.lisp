(in-package #:http-kit/http2)

(defparameter *h2-connection-specific-header-names*
  '("connection" "keep-alive" "proxy-connection"
    "transfer-encoding" "upgrade" "http2-settings"))

(defun %h2-control-octets (value name)
  (unless (and value
               (vectorp value)
               (= (array-rank value) 1)
               (subtypep (array-element-type value) '(unsigned-byte 8)))
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 control payload must be an octet vector."
           :operation :http2-client
           :detail (list name value)))
  (let ((copy (make-array (length value)
                          :element-type '(unsigned-byte 8))))
    (replace copy value)
    copy))

(defun %h2-control-error-code (value)
  (unless (and (integerp value) (<= 0 value #xffffffff))
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 error code must be an unsigned 32-bit integer."
           :operation :http2-client
           :detail value))
  value)

(defun %h2-control-stream-id (connection value)
  (if (null value)
      (max 0 (- (%http2-connection-next-stream-id connection) 2))
      (progn
        (unless (and (integerp value) (<= 0 value #x7fffffff))
          (error 'http-kit:http-protocol-error
                 :message "An HTTP/2 GOAWAY stream ID is invalid."
                 :operation :http2-client
                 :detail value))
        value)))

(defun send-http2-goaway
    (connection &key last-stream-id (error-code 0) debug-data timeout deadline
                  clock-function)
  "Send GOAWAY and mark CONNECTION as draining.

CONNECTION remains open so already-created streams can finish.  New streams
are rejected after this function marks the connection draining."
  (let* ((connection (if (http2-connection-p connection)
                         connection
                         (error 'http-kit:http-protocol-error
                                :message "SEND-HTTP2-GOAWAY requires an HTTP2-CONNECTION."
                                :operation :http2-client
                                :detail (type-of connection))))
         (stream (%http2-connection-stream connection))
         (clock (or clock-function
                    (%http2-connection-clock-function connection)))
         (error-code (%h2-control-error-code error-code))
         (last-stream-id (%h2-control-stream-id connection last-stream-id))
         (debug-data (%h2-control-octets
                      (or debug-data
                          (make-array 0 :element-type '(unsigned-byte 8)))
                      :debug-data)))
    (unless (%http2-connection-session-started-p connection)
      (error 'http-kit:http-protocol-error
             :message "The HTTP/2 session has not started."
             :operation :http2-client
             :detail :goaway-before-session))
    (when (> (+ 8 (length debug-data))
             (%http2-connection-max-frame-size connection))
      (error 'http-kit:http-protocol-error
             :message "The HTTP/2 GOAWAY debug payload is too large."
             :operation :http2-client
             :detail (list :length (+ 8 (length debug-data))
                           :max-frame-size
                           (%http2-connection-max-frame-size connection))))
    (let ((payload (make-array (+ 8 (length debug-data))
                               :element-type '(unsigned-byte 8)))
          (writer nil))
      (%h2-put-u32 payload 0 last-stream-id)
      (%h2-put-u32 payload 4 error-code)
      (replace payload debug-data :start1 8)
      (setf (%http2-connection-local-goaway-last-stream-id connection)
            last-stream-id
            (%http2-connection-draining-p connection) t
            writer (%h2-writer stream nil nil))
      (handler-case
          (http-kit:with-http-deadline (absolute-deadline timeout
                                         :inherited deadline
                                         :clock-function clock)
            (setf writer (%h2-writer stream absolute-deadline clock))
            (%h2-send-control writer +http2-goaway-type+ 0 0 payload))
        (error (condition)
          (close-http2-connection connection)
          (error condition)))
      connection)))

(defun graceful-shutdown-http2-connection
    (connection &key last-stream-id (error-code 0) debug-data timeout deadline
                  clock-function)
  "Mark CONNECTION draining and send a graceful HTTP/2 GOAWAY."
  (send-http2-goaway connection
                      :last-stream-id last-stream-id
                      :error-code error-code
                      :debug-data debug-data
                      :timeout timeout
                      :deadline deadline
                      :clock-function clock-function))

(defun ping-http2-connection
    (connection &key payload timeout deadline clock-function)
  "Send an HTTP/2 PING and wait for its matching ACK."
  (let* ((connection (if (http2-connection-p connection)
                         connection
                         (error 'http-kit:http-protocol-error
                                :message "PING-HTTP2-CONNECTION requires an HTTP2-CONNECTION."
                                :operation :http2-client
                                :detail (type-of connection))))
         (stream (%http2-connection-stream connection))
         (clock (or clock-function
                    (%http2-connection-clock-function connection)))
         (payload (%h2-control-octets
                   (or payload
                       (make-array 8 :element-type '(unsigned-byte 8)
                                   :initial-element 0))
                   :payload)))
    (unless (= (length payload) 8)
      (error 'http-kit:http-protocol-error
             :message "An HTTP/2 PING payload must contain eight octets."
             :operation :http2-client
             :detail (length payload)))
    (unless (%http2-connection-session-started-p connection)
      (error 'http-kit:http-protocol-error
             :message "The HTTP/2 session has not started."
             :operation :http2-client
             :detail :ping-before-session))
    (handler-case
        (http-kit:with-http-deadline (absolute-deadline timeout
                                         :inherited deadline
                                         :clock-function clock)
          (let* ((reader (%h2-reader-for stream))
                 (writer (%h2-writer stream absolute-deadline clock)))
            (%h2-send-control writer +http2-ping-type+ 0 0 payload)
            (loop
              (let ((frame (%h2-read-frame reader
                                           (%http2-connection-max-frame-size connection)
                                           absolute-deadline clock)))
                (when (eq frame :eof)
                  (error 'http-kit:http-protocol-error
                         :message "The HTTP/2 peer closed while awaiting PING ACK."
                         :operation :http2-client
                         :detail :eof))
                (let ((type (%h2-frame-type frame))
                      (flags (%h2-frame-flags frame))
                      (stream-id (%h2-frame-stream-id frame))
                      (frame-payload (%h2-frame-payload frame)))
                  (cond
                    ((= type +http2-ping-type+)
                     (unless (and (= stream-id 0)
                                  (= (length frame-payload) 8))
                       (error 'http-kit:http-protocol-error
                              :message "The HTTP/2 PING frame is invalid."
                              :operation :http2-client
                              :detail frame))
                     (if (logbitp 0 flags)
                         (when (equalp payload frame-payload)
                           (return payload))
                         (%h2-send-control writer +http2-ping-type+
                                           +http2-ack-flag+ 0 frame-payload)))
                    ((and (%h2-control-frame-p type)
                          (/= type +http2-ping-type+))
                     (%h2-connection-control-handler connection frame writer 0))
                    (t
                     (error 'http-kit:http-protocol-error
                            :message "The HTTP/2 PING received an unexpected frame."
                            :operation :http2-client
                            :detail frame))))))))
      (error (condition)
        (close-http2-connection connection)
        (error condition)))))
