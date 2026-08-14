(in-package #:http-kit/websocket)

(defun %websocket-append-octets (target source)
  (let* ((old-length (fill-pointer target))
         (new-length (+ old-length (length source))))
    (setf target (adjust-array target new-length :fill-pointer new-length))
    (replace target source :start1 old-length)
    target))

(defun read-websocket-message
    (stream &key (max-message-bytes +websocket-default-max-payload-bytes+)
                  (max-payload-bytes +websocket-default-max-payload-bytes+)
                  (require-mask-p nil) (allow-unmasked-p t) on-control)
  "Read one fragmented WebSocket data message.

Returns the message payload octets and its data opcode (1 for text or 2 for
binary).  Control frames are delivered to ON-CONTROL, when supplied, and are
otherwise consumed while the data message is assembled."
  (%websocket-validate-limit max-message-bytes "MAX-MESSAGE-BYTES")
  (%websocket-validate-limit max-payload-bytes "MAX-PAYLOAD-BYTES")
  (when (and on-control (not (functionp on-control)))
    (%websocket-protocol-error "ON-CONTROL must be a function or NIL." on-control))
  (let ((message-opcode nil)
        (message (make-array 0
                             :element-type '(unsigned-byte 8)
                             :adjustable t
                             :fill-pointer 0)))
    (loop
      (let ((frame (read-websocket-frame
                    stream
                    :max-payload-bytes max-payload-bytes
                    :require-mask-p require-mask-p
                    :allow-unmasked-p allow-unmasked-p)))
        (let ((opcode (websocket-frame-opcode frame))
              (payload (websocket-frame-payload frame)))
          (cond ((%websocket-control-opcode-p opcode)
                 (when on-control
                   (funcall on-control frame)))
                ((zerop opcode)
                 (unless message-opcode
                   (%websocket-protocol-error
                    "A WebSocket continuation frame has no initial data frame."))
                 (when (> (+ (fill-pointer message) (length payload))
                          max-message-bytes)
                   (%websocket-size-error
                    "A WebSocket message exceeded its size limit."
                    max-message-bytes
                    (+ (fill-pointer message) (length payload))))
                 (setf message (%websocket-append-octets message payload))
                 (when (websocket-frame-fin-p frame)
                   (return
                     (values (subseq message 0 (fill-pointer message))
                             message-opcode))))
                ((member opcode '(1 2) :test #'=)
                 (when message-opcode
                   (%websocket-protocol-error
                    "A WebSocket data frame arrived before the prior message ended."
                    opcode))
                 (setf message-opcode opcode)
                 (when (> (length payload) max-message-bytes)
                   (%websocket-size-error
                    "A WebSocket message exceeded its size limit."
                    max-message-bytes (length payload)))
                 (setf message (%websocket-append-octets message payload))
                 (when (websocket-frame-fin-p frame)
                   (return
                     (values (subseq message 0 (fill-pointer message))
                             message-opcode))))
                (t
                 (%websocket-protocol-error
                 "A WebSocket message encountered an invalid data opcode."
                  opcode))))))))

(defun %websocket-message-octets (payload opcode)
  (cond ((%websocket-octet-vector-p payload)
         (%websocket-copy-octets payload))
        ((and (= opcode 1) (stringp payload))
         (cl-codec-kit:string-to-octets payload :encoding :utf-8))
        (t
         (%websocket-protocol-error
          "A WebSocket data message must be octets, or a text string for opcode 1."
          payload))))

(defun %websocket-positive-limit (limit name)
  (unless (and (integerp limit) (plusp limit))
    (%websocket-protocol-error
     (format nil "~A must be a positive integer." name)
     limit))
  limit)

(defun %websocket-masking-options
    (mask-p masking-key masking-key-function)
  (when (and masking-key masking-key-function)
    (%websocket-protocol-error
     "A WebSocket masking key and masking-key function are mutually exclusive."))
  (when (and (not mask-p) (or masking-key masking-key-function))
    (%websocket-protocol-error
     "An unmasked WebSocket frame cannot specify a masking key."))
  (when (and mask-p masking-key-function (not (functionp masking-key-function)))
    (%websocket-protocol-error
     "A WebSocket masking-key function must be callable."
     masking-key-function))
  (when (and mask-p (not (or masking-key masking-key-function)))
    (%websocket-protocol-error
     "Masked WebSocket output requires an explicit masking key or key function."))
  t)

(defun %websocket-next-masking-key
    (mask-p masking-key masking-key-function)
  (when mask-p
    (%websocket-copy-octets
     (if masking-key-function
         (funcall masking-key-function)
         masking-key))))

(defun write-websocket-message
    (stream payload &key (opcode 2) (max-frame-payload-bytes 65535)
                   (mask-p nil) masking-key masking-key-function
                   (finish-output-p t))
  "Write one text or binary WebSocket message.

PAYLOAD may be an octet vector, or a string when OPCODE is 1.  Large payloads
are fragmented into frames no larger than MAX-FRAME-PAYLOAD-BYTES.  When
MASK-P is true, MASKING-KEY-FUNCTION is called once per frame and must return
four octets; a single MASKING-KEY is accepted only when one frame is emitted.
Returns the number of frames and the payload length."
  (unless (member opcode '(1 2) :test #'=)
    (%websocket-protocol-error
     "A WebSocket message opcode must be 1 (text) or 2 (binary)."
     opcode))
  (%websocket-positive-limit max-frame-payload-bytes
                              "MAX-FRAME-PAYLOAD-BYTES")
  (let* ((octets (%websocket-message-octets payload opcode))
         (payload-length (length octets))
         (frame-count (max 1 (ceiling payload-length
                                      max-frame-payload-bytes))))
    (%websocket-masking-options mask-p masking-key masking-key-function)
    (when (and mask-p (> frame-count 1) masking-key)
      (%websocket-protocol-error
       "Fragmented masked output requires a masking-key function so each frame has a fresh key."))
    (unless (streamp stream)
      (%websocket-protocol-error "WebSocket message output must be a stream." stream))
    (let ((position 0)
          (frame-index 0)
          (first-p t))
      (loop while (or first-p (< position payload-length))
            do (let* ((remaining (- payload-length position))
                      (chunk-length (min max-frame-payload-bytes remaining))
                      (last-p (= (+ position chunk-length) payload-length))
                      (frame (make-websocket-frame
                              :fin-p last-p
                              :opcode (if (zerop frame-index) opcode 0)
                              :mask-p mask-p
                              :masking-key
                              (%websocket-next-masking-key
                               mask-p masking-key masking-key-function)
                              :payload (subseq octets position
                                               (+ position chunk-length)))))
                 (write-websocket-frame stream frame :finish-output-p nil)
                 (incf frame-index)
                 (setf position (+ position chunk-length)
                       first-p nil)))
      (when finish-output-p
        (finish-output stream))
      (values frame-count payload-length))))

(defun %write-websocket-control-frame
    (stream opcode payload &key (mask-p nil) masking-key masking-key-function
                         (finish-output-p t))
  (%websocket-masking-options mask-p masking-key masking-key-function)
  (let ((frame
          (make-websocket-frame
           :fin-p t
           :opcode opcode
           :mask-p mask-p
           :masking-key
           (%websocket-next-masking-key
            mask-p masking-key masking-key-function)
           :payload
           (cond ((%websocket-octet-vector-p payload)
                  payload)
                 ((stringp payload)
                  (cl-codec-kit:string-to-octets payload :encoding :utf-8))
                 (t
                  (%websocket-protocol-error
                   "A WebSocket control payload must be octets or a string."
                   payload))))))
    (write-websocket-frame stream frame :finish-output-p finish-output-p)))

(defun websocket-ping
    (stream &key (payload (make-array 0 :element-type '(unsigned-byte 8)))
                   (mask-p nil) masking-key masking-key-function
                   (finish-output-p t))
  "Write a final WebSocket Ping control frame."
  (%write-websocket-control-frame
   stream 9 payload
   :mask-p mask-p
   :masking-key masking-key
   :masking-key-function masking-key-function
   :finish-output-p finish-output-p))

(defun websocket-pong
    (stream &key (payload (make-array 0 :element-type '(unsigned-byte 8)))
                   (mask-p nil) masking-key masking-key-function
                   (finish-output-p t))
  "Write a final WebSocket Pong control frame."
  (%write-websocket-control-frame
   stream 10 payload
   :mask-p mask-p
   :masking-key masking-key
   :masking-key-function masking-key-function
   :finish-output-p finish-output-p))

(defun websocket-close
    (stream &key payload code reason
                   (mask-p nil) masking-key masking-key-function
                   (finish-output-p t))
  "Write a WebSocket Close control frame.

When PAYLOAD is supplied it is used as the already encoded close payload and
CODE and REASON must be NIL.  Otherwise CODE defaults to 1000 and REASON to
the empty string."
  (when (and payload (or code reason))
    (%websocket-protocol-error
     "A raw WebSocket close payload cannot be combined with CODE or REASON."))
  (%write-websocket-control-frame
   stream 8
   (or payload (make-websocket-close-payload :code (or code 1000)
                                     :reason (or reason "")))
   :mask-p mask-p
   :masking-key masking-key
   :masking-key-function masking-key-function
   :finish-output-p finish-output-p))
