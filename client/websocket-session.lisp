(in-package #:http-kit/websocket)

(defun %websocket-session-close-code (condition)
  (cond ((typep condition 'http-size-limit-exceeded) 1009)
        ((typep condition 'http-protocol-error) 1002)
        (t 1011)))

(defun serve-websocket-session
    (stream handler &key
                     (max-message-bytes +websocket-default-max-payload-bytes+)
                     (max-payload-bytes +websocket-default-max-payload-bytes+)
                     (max-messages nil)
                     (require-mask-p t)
                     (allow-unmasked-p nil)
                     on-control
                     on-error
                     (close-on-error-p t)
                     (close-stream #'close))
  "Serve messages on an already-upgraded WebSocket STREAM.

HANDLER is called as (STREAM PAYLOAD OPCODE) for every complete text or
binary message.  It may return :CLOSE to start a normal close handshake.
The server automatically replies to Ping frames and echoes a valid peer
Close frame.  Client frames are required to be masked by default.

The function returns two values: the number of messages delivered and a
termination keyword (:PEER-CLOSE, :HANDLER-CLOSE, or :MAX-MESSAGES).  On a
protocol, size, or handler error it sends an appropriate Close frame when
CLOSE-ON-ERROR-P is true, invokes ON-ERROR with the condition, and re-signals
the condition.  CLOSE-STREAM is called at the end unless it is NIL, which is
useful when the caller owns the upgraded stream lifecycle."
  (unless (streamp stream)
    (%websocket-protocol-error
     "A WebSocket session requires a stream." stream))
  (unless (functionp handler)
    (%websocket-protocol-error
     "A WebSocket session handler must be callable." handler))
  (%websocket-validate-limit max-message-bytes "MAX-MESSAGE-BYTES")
  (%websocket-validate-limit max-payload-bytes "MAX-PAYLOAD-BYTES")
  (when (and max-messages
             (or (not (integerp max-messages)) (minusp max-messages)))
    (%websocket-protocol-error
     "MAX-MESSAGES must be NIL or a non-negative integer."
     max-messages))
  (when (and on-control (not (functionp on-control)))
    (%websocket-protocol-error
     "ON-CONTROL must be a function or NIL." on-control))
  (when (and on-error (not (functionp on-error)))
    (%websocket-protocol-error
     "ON-ERROR must be a function or NIL." on-error))
  (when (and close-stream (not (functionp close-stream)))
    (%websocket-protocol-error
     "CLOSE-STREAM must be a function or NIL." close-stream))
  (let ((message-count 0)
        (close-sent-p nil)
        (close-tag (gensym "WEBSOCKET-CLOSE-")))
    (labels ((send-close (&key payload code reason)
               (unless close-sent-p
                 (setf close-sent-p t)
                 (if payload
                     (websocket-close stream :payload payload)
                     (websocket-close stream
                                      :code (or code 1000)
                                      :reason (or reason "")))))
             (handle-control (frame)
               (let ((opcode (websocket-frame-opcode frame))
                     (payload (websocket-frame-payload frame)))
                 (when on-control
                   (funcall on-control frame))
                 (case opcode
                   (9
                    (websocket-pong stream :payload payload))
                   (8
                    (parse-websocket-close-payload payload)
                    (send-close :payload payload)
                    (throw close-tag :peer-close))
                   (10 nil)))))
      (unwind-protect
           (handler-case
               (let ((termination
                       (if (and max-messages (zerop max-messages))
                           (progn
                             (send-close :code 1000)
                             :max-messages)
                           (catch close-tag
                             (loop
                               (multiple-value-bind (payload opcode)
                                   (read-websocket-message
                                    stream
                                    :max-message-bytes max-message-bytes
                                    :max-payload-bytes max-payload-bytes
                                    :require-mask-p require-mask-p
                                    :allow-unmasked-p allow-unmasked-p
                                    :on-control #'handle-control)
                                 (incf message-count)
                                 (when (eq :close
                                           (funcall handler
                                                    stream payload opcode))
                                   (send-close :code 1000)
                                   (return :handler-close))
                                 (when (and max-messages
                                            (>= message-count max-messages))
                                   (send-close :code 1000)
                                   (return :max-messages))))))))
                 (values message-count termination))
             (error (condition)
               (when close-on-error-p
                 (http-kit::%with-http-cleanup
                   (send-close
                    :code (%websocket-session-close-code condition)
                    :reason "WebSocket session error")))
               (when on-error
                 (funcall on-error condition))
               (error condition)))
        (when close-stream
          (funcall close-stream stream))))))
