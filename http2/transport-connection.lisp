(in-package #:http-kit/http2)

(defmacro %h2-with-scheduler-lock ((connection) &body body)
  #+sbcl
  `(sb-thread:with-mutex ((%http2-connection-scheduler-lock ,connection))
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun http2-client-connection (client)
  (and (http2-client-p client)
       (%http2-connection client)))

(defun %h2-peer-stream-reset-condition-p (condition)
  (and (typep condition 'http-kit:http-connection-error)
       (let ((cause (http-kit:http-connection-error-cause condition)))
         (and (consp cause)
              (member (first cause) '(:rst-stream :goaway))))))

(defun make-http2-connection
    (&key stream close-stream
          (max-frame-size +http2-default-max-frame-size+)
          (max-header-bytes http-kit::*default-max-header-bytes*)
          (max-fields 256)
          (max-body-bytes http-kit::*default-max-body-bytes*)
          (max-control-frames 100)
          (max-control-window 1.0d0)
          (clock-function #'http-kit::%monotonic-time))
  "Create a reusable HTTP/2 connection over an already negotiated stream.

STREAM must be a binary bidirectional stream.  The caller owns socket, TLS,
ALPN, proxy, and DNS negotiation; this object owns the HTTP/2 preface,
SETTINGS, stream identifiers, HPACK decoder state, and control frames."
  (unless (streamp stream)
    (error 'http-kit:http-connection-error
           :message "MAKE-HTTP2-CONNECTION requires a stream."
           :operation :connect
           :cause (type-of stream)))
  (unless (or (null close-stream) (functionp close-stream))
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 connection close function must be callable."
           :operation :http2-client
           :detail close-stream))
  (%h2-validate-frame-size max-frame-size)
  (%h2-validate-limit :max-header-bytes max-header-bytes)
  (%h2-validate-limit :max-fields max-fields)
  (%h2-validate-limit :max-body-bytes max-body-bytes :allow-zero t)
  (%h2-validate-limit :max-control-frames max-control-frames :allow-zero t)
  (unless (and (realp max-control-window)
               (not (minusp max-control-window)))
    (error 'http-kit:http-protocol-error
           :message "The HTTP/2 max-control-window must be a non-negative real."
           :operation :http2-client
           :detail max-control-window))
  (unless (functionp clock-function)
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 connection clock function must be callable."
           :operation :http2-client
           :detail clock-function))
  (%make-http2-connection
   :stream stream
   :close-stream (or close-stream #'close)
   :max-frame-size max-frame-size
   :max-header-bytes max-header-bytes
   :max-fields max-fields
   :max-body-bytes max-body-bytes
   :max-control-frames max-control-frames
   :max-control-window max-control-window
   :clock-function clock-function
   :hpack-context
   (%h2-make-hpack-context :max-table-size +hpack-default-table-size+)))

(defun http2-connection-open-p (connection)
  (and (http2-connection-p connection)
       (not (%http2-connection-closed-p connection))))

(defun http2-connection-session-started-p (connection)
  (and (http2-connection-p connection)
       (%http2-connection-session-started-p connection)))

(defun http2-connection-peer-max-frame-size (connection)
  (and (http2-connection-p connection)
       (%http2-connection-peer-max-frame-size connection)))

(defun http2-connection-peer-max-table-size (connection)
  (and (http2-connection-p connection)
       (%http2-connection-peer-max-table-size connection)))

(defun http2-connection-peer-max-concurrent-streams (connection)
  (and (http2-connection-p connection)
       (%http2-connection-peer-max-concurrent-streams connection)))

(defun http2-connection-peer-max-header-list-size (connection)
  (and (http2-connection-p connection)
       (%http2-connection-peer-max-header-list-size connection)))

(defun http2-connection-peer-initial-window-size (connection)
  (and (http2-connection-p connection)
       (%http2-connection-peer-initial-window-size connection)))

(defun http2-connection-peer-connection-window-size (connection)
  (and (http2-connection-p connection)
       (%http2-connection-peer-connection-window-size connection)))

(defun http2-connection-goaway-last-stream-id (connection)
  (and (http2-connection-p connection)
       (%http2-connection-goaway-last-stream-id connection)))

(defun http2-connection-local-goaway-last-stream-id (connection)
  (and (http2-connection-p connection)
       (%http2-connection-local-goaway-last-stream-id connection)))

(defun http2-connection-draining-p (connection)
  (and (http2-connection-p connection)
       (%http2-connection-draining-p connection)))

(defun close-http2-connection (connection)
  "Close CONNECTION idempotently and mark it unusable for future requests."
  (unless (http2-connection-p connection)
    (error 'http-kit:http-protocol-error
           :message "CLOSE-HTTP2-CONNECTION requires an HTTP2-CONNECTION."
           :operation :http2-client
           :detail (type-of connection)))
  (let ((close-p nil))
    #+sbcl
    (%h2-with-scheduler-lock (connection)
      (unless (%http2-connection-closed-p connection)
        (setf (%http2-connection-closed-p connection) t
              close-p t))
      (%h2-scheduler-broadcast connection))
    #-sbcl
    (unless (%http2-connection-closed-p connection)
      (setf (%http2-connection-closed-p connection) t
            close-p t))
    (when close-p
      (http-kit::%with-http-cleanup
        (funcall (%http2-connection-close-stream connection)
                 (%http2-connection-stream connection))))
    connection))

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
         (last-stream-id (when last-stream-id
                           (%h2-control-stream-id connection last-stream-id)))
         (debug-data (%h2-control-octets
                      (or debug-data
                          (make-array 0
                                      :element-type '(unsigned-byte 8)))
                      :debug-data)))
    (unless (%http2-connection-session-started-p connection)
      (error 'http-kit:http-protocol-error
             :message "The HTTP/2 session has not started."
             :operation :http2-client
             :detail :goaway-before-session))
    (when (%http2-connection-closed-p connection)
      (error 'http-kit:http-protocol-error
             :message "The HTTP/2 connection is already closed."
             :operation :http2-client
             :detail :goaway-after-close))
    (when (> (+ 8 (length debug-data))
             (%http2-connection-peer-max-frame-size connection))
      (error 'http-kit:http-protocol-error
             :message "The HTTP/2 GOAWAY debug payload is too large."
             :operation :http2-client
             :detail (list :length (+ 8 (length debug-data))
                           :max-frame-size
                           (%http2-connection-peer-max-frame-size connection))))
    (let ((payload (make-array (+ 8 (length debug-data))
                               :element-type '(unsigned-byte 8)))
          (writer nil))
      (replace payload debug-data :start1 8)
      (%h2-with-scheduler-lock (connection)
        (unless last-stream-id
          (setf last-stream-id (%h2-control-stream-id connection nil)))
        (let ((previous-last-stream-id
                (%http2-connection-local-goaway-last-stream-id connection)))
          (when (and previous-last-stream-id
                     (> last-stream-id previous-last-stream-id))
            (error 'http-kit:http-protocol-error
                   :message "A subsequent HTTP/2 GOAWAY last-stream identifier must not increase."
                   :operation :http2-client
                   :detail (list :previous previous-last-stream-id
                                 :new last-stream-id))))
        (%h2-put-u32 payload 0 last-stream-id)
        (%h2-put-u32 payload 4 error-code)
        ;; Record the state before writing so a re-entrant caller cannot open a
        ;; stream while the GOAWAY bytes are still in the writer.
        (setf (%http2-connection-local-goaway-last-stream-id connection)
              last-stream-id
              (%http2-connection-draining-p connection) t
              writer (%h2-writer stream nil nil
                                 #+sbcl (%http2-connection-writer-lock connection)
                                 #-sbcl nil)))
      (handler-case
          (http-kit:with-http-deadline (absolute-deadline timeout
                                         :inherited deadline
                                         :clock-function clock)
            (setf writer (%h2-writer stream absolute-deadline clock
                                     #+sbcl (%http2-connection-writer-lock connection)
                                     #-sbcl nil))
            (%h2-send-control writer +http2-goaway-type+ 0 0 payload))
        (error (condition)
          (close-http2-connection connection)
          (error condition)))
      connection)))

(defun %h2-priority-field-value-octets (value)
  (unless (stringp value)
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 Priority field value must be a string."
           :operation :http2-client
           :detail value))
  (let ((octets (make-array (length value) :element-type '(unsigned-byte 8))))
    (loop for character across value
          for code = (char-code character)
          for position from 0
          unless (or (= code 9) (<= 32 code 126))
            do (error 'http-kit:http-protocol-error
                      :message "An HTTP/2 Priority field value must contain ASCII field-value bytes."
                      :operation :http2-client
                      :detail (list position code))
          do (setf (aref octets position) code))
    octets))

(defun send-http2-priority-update
    (connection stream-id
     &key (priority-field-value nil priority-field-value-p)
       (urgency 3) incremental timeout deadline clock-function)
  "Send an RFC 9218 PRIORITY_UPDATE for STREAM-ID."
  (unless (http2-connection-p connection)
    (error 'http-kit:http-protocol-error
           :message "SEND-HTTP2-PRIORITY-UPDATE requires an HTTP2-CONNECTION."
           :operation :http2-client
           :detail (type-of connection)))
  (unless (%http2-connection-session-started-p connection)
    (error 'http-kit:http-protocol-error
           :message "The HTTP/2 session has not started."
           :operation :http2-client
           :detail :priority-update-before-session))
  (when (%http2-connection-closed-p connection)
    (error 'http-kit:http-protocol-error
           :message "The HTTP/2 connection is already closed."
           :operation :http2-client
           :detail :priority-update-after-close))
  (unless (and (integerp stream-id) (<= 1 stream-id #x7fffffff))
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 PRIORITY_UPDATE target must be a non-zero 31-bit stream ID."
           :operation :http2-client
           :detail stream-id))
  (let* ((value (if priority-field-value-p
                    priority-field-value
                    (http-kit:format-http-priority-field-value
                     :urgency urgency :incremental incremental)))
         (value-octets (%h2-priority-field-value-octets value))
         (payload (make-array (+ 4 (length value-octets))
                              :element-type '(unsigned-byte 8)))
         (clock (or clock-function
                    (%http2-connection-clock-function connection))))
    (when (> (length payload) (%http2-connection-peer-max-frame-size connection))
      (error 'http-kit:http-protocol-error
             :message "The HTTP/2 PRIORITY_UPDATE payload is too large for the peer."
             :operation :http2-client
             :detail (length payload)))
    (%h2-put-u32 payload 0 stream-id)
    (replace payload value-octets :start1 4)
    (handler-case
        (http-kit:with-http-deadline (absolute-deadline timeout
                                       :inherited deadline
                                       :clock-function clock)
          (%h2-send-control (%h2-writer (%http2-connection-stream connection) absolute-deadline clock #+sbcl (%http2-connection-writer-lock connection) #-sbcl nil)
                            +http2-priority-update-type+ 0 0 payload))
      (error (condition)
        (close-http2-connection connection)
        (error condition)))
    connection))

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
  "Send an HTTP/2 PING and return its payload after a matching ACK."
  (let* ((connection (if (http2-connection-p connection)
                         connection
                         (error 'http-kit:http-protocol-error
                                :message "PING-HTTP2-CONNECTION requires an HTTP2-CONNECTION."
                                :operation :http2-client
                                :detail (type-of connection))))
         (clock (or clock-function
                    (%http2-connection-clock-function connection)))
         (payload (%h2-control-octets
                   (or payload
                       (make-array 8
                                   :element-type '(unsigned-byte 8)
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
    (when (%http2-connection-closed-p connection)
      (error 'http-kit:http-protocol-error
             :message "The HTTP/2 connection is already closed."
             :operation :http2-client
             :detail :ping-after-close))
    (handler-case
        (http-kit:with-http-deadline (absolute-deadline timeout
                                         :inherited deadline
                                         :clock-function clock)
          #+sbcl
          (%h2-scheduler-ping
           connection payload absolute-deadline clock)
          #-sbcl
          (let* ((stream (%http2-connection-stream connection))
                 (reader (%h2-reader-for stream))
                 (writer (%h2-writer stream absolute-deadline clock)))
            (%h2-send-control writer +http2-ping-type+ 0 0 payload)
            (loop
              (let ((frame (%h2-read-frame
                            reader
                            (%http2-connection-max-frame-size connection)
                            absolute-deadline
                            clock)))
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
                         (%h2-send-control writer
                                           +http2-ping-type+
                                           +http2-ack-flag+
                                           0
                                           frame-payload)))
                    ((and (%h2-control-frame-p type)
                          (/= type +http2-ping-type+))
                     (%h2-connection-control-handler connection
                                                      frame
                                                      writer
                                                      0))
                    (t
                     (error 'http-kit:http-protocol-error
                            :message "The HTTP/2 PING received an unexpected frame."
                            :operation :http2-client
                            :detail frame))))))))
      (error (condition)
        (close-http2-connection connection)
        (error condition)))))

(defun %h2-connection-note-peer-settings
    (connection max-frame-size max-table-size initial-window-size
                enable-connect-protocol &optional max-concurrent-streams
                  max-header-list-size)
  (when max-frame-size
    (setf (%http2-connection-peer-max-frame-size connection)
          max-frame-size))
  (unless (null max-table-size)
    (setf (%http2-connection-peer-max-table-size connection)
          max-table-size)
    ;; The peer's SETTINGS value bounds the table size it may use in
    ;; outbound header blocks, so the decoder must enforce it as well.
    (http-message-kit:hpack-context-set-decoder-max-table-size (%http2-connection-hpack-context connection) (min max-table-size *h2-max-peer-hpack-table-size*)))
  (when max-concurrent-streams
    (setf (%http2-connection-peer-max-concurrent-streams connection)
          max-concurrent-streams))
  (when max-header-list-size
    (setf (%http2-connection-peer-max-header-list-size connection)
          max-header-list-size))
  (when initial-window-size
    (let ((delta (- initial-window-size
                    (%http2-connection-peer-initial-window-size connection))))
      (when (some (lambda (entry)
                    (> (+ (cdr entry) delta) #x7fffffff))
                  (%http2-connection-peer-stream-windows connection))
        (error 'http-kit:http-protocol-error
               :message "SETTINGS_INITIAL_WINDOW_SIZE overflowed an HTTP/2 stream flow-control window."
               :operation :http2-settings
               :detail :flow-control-error))
      (dolist (entry (%http2-connection-peer-stream-windows connection))
        (incf (cdr entry) delta))
      (setf (%http2-connection-peer-initial-window-size connection)
            initial-window-size)))
  (when enable-connect-protocol
    (setf (%http2-connection-peer-enable-connect-protocol-p connection)
          (= enable-connect-protocol 1))))

(defun %h2-connection-stream-window (connection stream-id)
  (let ((entry (assoc stream-id
                      (%http2-connection-peer-stream-windows connection))))
    (or (and entry (cdr entry))
        (let ((new-entry
                (cons stream-id
                      (%http2-connection-peer-initial-window-size
                       connection))))
          (push new-entry (%http2-connection-peer-stream-windows connection))
          (cdr new-entry)))))

(defun %h2-connection-send-request-body
    (connection reader writer stream-id body request-body-function
                expected-body-length max-body-bytes local-max-frame-size
                trailer-fields deadline clock-function read-initial-settings-p
                huffman-p &key scheduler-p flow-wait-function)
  (let ((position 0)
        (pending-frames '())
        (first-settings-p read-initial-settings-p)
        (body-length (length body)))
    (labels ((read-while-blocked ()
               (if flow-wait-function
                   (funcall flow-wait-function connection stream-id
                            deadline clock-function)
                   (let ((frame (%h2-read-frame
                                 reader
                                 (%http2-connection-max-frame-size connection)
                                 deadline clock-function)))
                     (when (eq frame :eof)
                       (error 'http-kit:http-connection-error
                              :message "The HTTP/2 peer closed while upload flow control was blocked."
                              :operation :http2-read
                              :cause :eof))
                     (when first-settings-p
                       (unless (and (= (%h2-frame-type frame)
                                       +http2-settings-type+)
                                    (zerop (%h2-frame-stream-id frame))
                                    (zerop (logand (%h2-frame-flags frame)
                                                   +http2-ack-flag+)))
                         (error 'http-kit:http-protocol-error
                                :message "The first HTTP/2 peer frame must be a non-ACK SETTINGS frame."
                                :operation :http2-read
                                :detail (list (%h2-frame-type frame)
                                              (%h2-frame-stream-id frame)
                                              (%h2-frame-flags frame))))
                       (setf first-settings-p nil))
                     (if (%h2-control-frame-p (%h2-frame-type frame))
                         (%h2-connection-control-handler
                          connection frame writer stream-id)
                         (push frame pending-frames)))))
             (write-data (payload end-stream-p)
               (let ((size (length payload)))
                 (let ((wire (%h2-frame-wire
                              +http2-data-type+
                              (if end-stream-p +http2-end-stream-flag+ 0)
                              stream-id payload)))
                   (if scheduler-p
                       (%h2-with-scheduler-lock (connection)
                         (let* ((stream-window
                                  (%h2-connection-stream-window
                                   connection stream-id))
                                (connection-window
                                  (%http2-connection-peer-connection-window-size
                                   connection)))
                           (when (or (zerop size)
                                     (>= (min stream-window connection-window)
                                         size))
                             (funcall writer wire)
                             (when (plusp size)
                               (decf
                                (%http2-connection-peer-connection-window-size
                                 connection)
                                size)
                               (decf (cdr (assoc
                                           stream-id
                                           (%http2-connection-peer-stream-windows
                                            connection)))
                                     size)
                               (incf position size))
                             t)))
                       (progn
                         (funcall writer wire)
                         (when (plusp size)
                           (decf
                            (%http2-connection-peer-connection-window-size
                             connection)
                            size)
                           (let ((entry
                                   (assoc
                                    stream-id
                                    (%http2-connection-peer-stream-windows
                                     connection))))
                             (decf (cdr entry) size))
                           (incf position size))
                         t)))))
             (effective-max-frame-size ()
               (min local-max-frame-size
                    (%http2-connection-peer-max-frame-size connection)))
             (write-trailers ()
               (when trailer-fields
                 (funcall
                  writer
                  (%h2-concat
                   (%h2-header-frames
                    (%h2-encode-header-block trailer-fields
                                             :huffman-p huffman-p)
                    t (effective-max-frame-size) stream-id)))))
             (window-available-size (frame-size)
               (if scheduler-p
                   (%h2-with-scheduler-lock (connection)
                     (min frame-size
                          (%h2-connection-stream-window
                           connection stream-id)
                          (%http2-connection-peer-connection-window-size
                           connection)))
                   (min frame-size
                        (%h2-connection-stream-window connection stream-id)
                        (%http2-connection-peer-connection-window-size
                         connection)))))
      (if request-body-function
          (let ((pending-chunk nil)
                (pending-offset 0)
                (producer-done-p nil))
            (loop
              (when (and (null pending-chunk)
                         (not producer-done-p))
                (let* ((producer-max-frame-size
                         (effective-max-frame-size))
                       (chunk (funcall request-body-function
                                       producer-max-frame-size)))
                  (if chunk
                      (setf pending-chunk
                            (%h2-validate-request-body-chunk
                             chunk producer-max-frame-size)
                            pending-offset 0)
                      (setf producer-done-p t))))
              (cond
                ((and producer-done-p (null pending-chunk))
                 (unless (or (null expected-body-length)
                             (= position expected-body-length))
                   (error 'http-kit:http-protocol-error
                          :message "The HTTP/2 request body producer ended before its declared length."
                          :operation :http2-write
                          :detail (list position expected-body-length)))
                 (if trailer-fields
                     nil
                     (write-data (http-kit::%empty-octets) t))
                 (return))
                (t
                 (let* ((max-frame-size (effective-max-frame-size))
                        (available
                          (window-available-size max-frame-size))
                        (remaining
                          (- (length pending-chunk) pending-offset)))
                   (when (and expected-body-length
                              (> (+ position remaining) expected-body-length))
                     (error 'http-kit:http-protocol-error
                            :message "The HTTP/2 request body producer exceeded its declared length."
                            :operation :http2-write
                            :detail (list (+ position remaining)
                                          expected-body-length)))
                   (if (plusp available)
                       (let* ((size (min remaining available))
                              (end-stream-p
                                (and (null trailer-fields)
                                     expected-body-length
                                     (= (+ position size)
                                        expected-body-length))))
                         (http-kit::%check-limit
                          :body (+ position size) max-body-bytes)
                         (when
                             (write-data
                              (subseq pending-chunk
                                      pending-offset (+ pending-offset size))
                              end-stream-p)
                           (incf pending-offset size)
                           (when (= pending-offset (length pending-chunk))
                             (setf pending-chunk nil
                                   pending-offset 0))
                           (when end-stream-p
                             (return))))
                       (read-while-blocked)))))))
          (loop while (< position body-length)
                do (let* ((max-frame-size (effective-max-frame-size))
                          (size (min max-frame-size
                                     (- body-length position)
                                     (window-available-size
                                      max-frame-size))))
                     (if (plusp size)
                         (write-data
                          (subseq body position (+ position size))
                          (and (= (+ position size) body-length)
                               (null trailer-fields)))
                         (read-while-blocked)))))
      (when trailer-fields
        (write-trailers))
      (values (nreverse pending-frames) first-settings-p))))

(defun %h2-connection-note-window-update (connection frame expected-stream-id)
  (let* ((stream-id (%h2-frame-stream-id frame))
         (payload (%h2-frame-payload frame))
         (increment (and (= (length payload) 4)
                         (%h2-u32 payload 0))))
    (when (and increment
               (zerop (logand increment #x80000000))
               (plusp increment))
      (if (zerop stream-id)
          (let ((new-window
                  (+ (%http2-connection-peer-connection-window-size connection)
                     increment)))
            (when (> new-window #x7fffffff)
              (error 'http-kit:http-protocol-error
                     :message "The HTTP/2 connection flow-control window overflowed."
                     :operation :http2-control
                     :detail new-window))
            (setf (%http2-connection-peer-connection-window-size connection)
                  new-window))
          (progn
            (when (> stream-id expected-stream-id)
              (error 'http-kit:http-protocol-error
                     :message "An HTTP/2 WINDOW_UPDATE targeted an idle stream."
                     :operation :http2-control
                     :detail stream-id))
            (when (= stream-id expected-stream-id)
            (let* ((windows (%http2-connection-peer-stream-windows connection))
                   (entry (assoc stream-id windows)))
              (if entry
                  (let ((new-window (+ (cdr entry) increment)))
                    (when (> new-window #x7fffffff)
                      (error 'http-kit:http-protocol-error
                             :message "The HTTP/2 stream flow-control window overflowed."
                             :operation :http2-control
                             :detail new-window))
                    (setf (cdr entry) new-window))
                  (let ((new-window
                          (+ (%http2-connection-peer-initial-window-size
                              connection)
                             increment)))
                    (when (> new-window #x7fffffff)
                      (error 'http-kit:http-protocol-error
                             :message "The HTTP/2 stream flow-control window overflowed."
                             :operation :http2-control
                             :detail new-window))
                    (push (cons stream-id new-window)
                          (%http2-connection-peer-stream-windows
                           connection)))))))))))

(defun %h2-connection-note-settings (connection frame writer)
  (multiple-value-bind (max-frame-size max-table-size initial-window-size
                        enable-connect-protocol max-concurrent-streams
                        max-header-list-size)
      (%h2-settings (%h2-frame-payload frame))
    (%h2-connection-note-peer-settings
     connection max-frame-size max-table-size initial-window-size
     enable-connect-protocol max-concurrent-streams max-header-list-size))
  (%h2-validate-settings-frame frame writer))

(defun %h2-connection-start-for-extended-connect
    (connection reader writer deadline clock-function)
  (%h2-write-wire
   (%http2-connection-stream connection)
   (%h2-concat
    (list +http2-connection-preface+
          (%h2-settings-wire (%http2-connection-max-frame-size connection))))
   deadline clock-function)
  (let ((frame (%h2-read-frame reader
                               (%http2-connection-max-frame-size connection)
                               deadline clock-function)))
    (when (eq frame :eof)
      (error 'http-kit:http-protocol-error
             :message "The HTTP/2 peer sent no initial SETTINGS frame."
             :operation :http2-read
             :detail :eof))
    (unless (and (= (%h2-frame-type frame) +http2-settings-type+)
                 (zerop (%h2-frame-stream-id frame))
                 (zerop (logand (%h2-frame-flags frame) +http2-ack-flag+)))
      (error 'http-kit:http-protocol-error
             :message "The first HTTP/2 peer frame must be a non-ACK SETTINGS frame."
             :operation :http2-read
             :detail (list (%h2-frame-type frame)
                           (%h2-frame-stream-id frame)
                           (%h2-frame-flags frame))))
    (%h2-client-control-budget-check connection frame writer)
    (%h2-connection-note-settings connection frame writer)
    (setf (%http2-connection-session-started-p connection) t)))

(defun %h2-connection-control-handler (connection frame writer expected-stream-id)
  (%h2-client-control-budget-check connection frame writer)
  (let ((type (%h2-frame-type frame))
        (stream-id (%h2-frame-stream-id frame))
        (payload (%h2-frame-payload frame)))
    (cond
      ((= type +http2-settings-type+)
       (%h2-connection-note-settings connection frame writer))
      ((= type +http2-window-update-type+)
       (%h2-connection-note-window-update connection frame expected-stream-id)
       (%h2-handle-control-frame frame writer expected-stream-id))
      ((= type +http2-goaway-type+)
       (unless (and (zerop stream-id) (>= (length payload) 8))
         (error 'http-kit:http-protocol-error
                :message "An HTTP/2 GOAWAY frame is invalid."
                :operation :http2-control
                :detail (list stream-id (length payload))))
       (let ((last-stream-id (%h2-u32 payload 0)))
         (when (/= 0 (logand last-stream-id #x80000000))
           (error 'http-kit:http-protocol-error
                  :message "An HTTP/2 GOAWAY last-stream identifier is invalid."
                  :operation :http2-control
                  :detail last-stream-id))
         (setf (%http2-connection-goaway-last-stream-id connection)
               last-stream-id)
         (when (> expected-stream-id last-stream-id)
           (error 'http-kit:http-connection-error
                  :message "The HTTP/2 peer rejected the active response stream with GOAWAY."
                  :operation :http2-control
                  :cause (list :goaway last-stream-id
                               (%h2-u32 payload 4))))))
      (t
       (%h2-handle-control-frame frame writer expected-stream-id)))))

(defun %h2-connection-wait-for-stream-capacity
    (connection reader writer deadline clock-function)
  (loop while (eql 0
                   (%http2-connection-peer-max-concurrent-streams connection))
        do (let ((frame (%h2-read-frame
                         reader
                         (%http2-connection-max-frame-size connection)
                         deadline clock-function)))
             (when (eq frame :eof)
               (error 'http-kit:http-connection-error
                      :message "The HTTP/2 peer closed while new streams were paused."
                      :operation :http2-read
                      :cause :eof))
             (unless (%h2-control-frame-p (%h2-frame-type frame))
               (error 'http-kit:http-protocol-error
                      :message "The HTTP/2 peer sent a stream frame while no request stream was active."
                      :operation :http2-read
                      :detail (list (%h2-frame-type frame)
                                    (%h2-frame-stream-id frame))))
             (%h2-connection-control-handler
              connection frame writer
              (%http2-connection-next-stream-id connection))
             (when (%http2-connection-goaway-last-stream-id connection)
               (%h2-connection-next-stream-id connection))))
  (%http2-connection-peer-max-concurrent-streams connection))

(defun %h2-connection-next-stream-id (connection)
  (when (%http2-connection-draining-p connection)
    (error 'http-kit:http-protocol-error
           :message "The HTTP/2 connection is draining."
           :operation :http2-client
           :detail :goaway-sent))
  (let ((stream-id (%http2-connection-next-stream-id connection)))
    (when (> stream-id #x7fffffff)
      (error 'http-kit:http-connection-error
             :message "The HTTP/2 client exhausted its stream identifiers."
             :operation :http2-write
             :cause :stream-id-exhausted))
    (let ((last-stream-id (%http2-connection-goaway-last-stream-id connection)))
      (when last-stream-id
        (error 'http-kit:http-connection-error
               :message "The HTTP/2 peer has closed this connection with GOAWAY."
               :operation :http2-write
               :cause (list :goaway last-stream-id stream-id))))
    stream-id))

(defun cancel-http2-stream
    (connection stream-id
     &key (error-code 8) timeout deadline clock-function)
  "Cancel an active client-initiated STREAM-ID with an HTTP/2 RST_STREAM."
  (unless (http2-connection-p connection)
    (error 'http-kit:http-protocol-error
           :message "CANCEL-HTTP2-STREAM requires an HTTP2-CONNECTION."
           :operation :http2-client
           :detail (type-of connection)))
  (let ((clock (or clock-function
                   (%http2-connection-clock-function connection)))
        (error-code (%h2-control-error-code error-code)))
    (unless (functionp clock)
      (error 'http-kit:http-protocol-error
             :message "An HTTP/2 cancellation clock must be callable."
             :operation :http2-client
             :detail clock))
    (http-kit:with-http-deadline (absolute-deadline timeout
                                   :inherited deadline
                                   :clock-function clock)
      (%h2-with-scheduler-lock (connection)
        (unless (http2-connection-open-p connection)
          (error 'http-kit:http-connection-error
                 :message "The HTTP/2 connection is closed."
                 :operation :http2-client
                 :cause :closed))
        (unless (and (integerp stream-id)
                     (plusp stream-id)
                     (oddp stream-id)
                     (< stream-id (%http2-connection-next-stream-id connection)))
          (error 'http-kit:http-protocol-error
                 :message "STREAM-ID must identify an opened client HTTP/2 stream."
                 :operation :http2-write
                 :detail stream-id))
        (%h2-send-rst-stream
         (%h2-writer (%http2-connection-stream connection)
                     absolute-deadline clock
                     #+sbcl (%http2-connection-writer-lock connection)
                     #-sbcl nil)
         stream-id error-code)))
  t))

(defun %h2-send-http2-request-over-connection-serial
    (connection request
     &key timeout deadline max-header-bytes max-fields max-body-bytes clock-function
       request-body-function request-body-length
       on-body-chunk on-information on-stream-open (collect-body-p t) (huffman-p nil))
  "Send REQUEST on CONNECTION and retain the connection for later streams.

Requests on one connection are serialized by this API.  A caller that needs
concurrent streams should use SEND-HTTP2-REQUESTS-OVER-CONNECTION."
  (unless (http2-connection-p connection)
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 request requires an HTTP2-CONNECTION."
           :operation :http2-client
           :detail (type-of connection)))
  (unless (http2-connection-open-p connection)
    (error 'http-kit:http-connection-error
           :message "The HTTP/2 connection is closed."
           :operation :http2-client
           :cause :closed))
  (http-kit::%check-http-request request)
  (%h2-validate-request-body-options
   request request-body-function request-body-length)
  (when (and on-body-chunk (not (functionp on-body-chunk)))
    (error 'http-kit:http-protocol-error
           :message "ON-BODY-CHUNK must be a function or NIL."
           :operation :http2-client
           :detail on-body-chunk))
  (when (and on-information (not (functionp on-information)))
    (error 'http-kit:http-protocol-error
           :message "ON-INFORMATION must be a function or NIL."
           :operation :http2-client
           :detail on-information))
  (when (and on-stream-open (not (functionp on-stream-open)))
    (error 'http-kit:http-protocol-error
           :message "ON-STREAM-OPEN must be a function or NIL."
           :operation :http2-client
           :detail on-stream-open))
  (unless (member collect-body-p '(nil t))
    (error 'http-kit:http-protocol-error
           :message "COLLECT-BODY-P must be NIL or T."
           :operation :http2-client
           :detail collect-body-p))
  (let* ((clock (or clock-function
                    (%http2-connection-clock-function connection)))
         (header-limit (or max-header-bytes
                           (%http2-connection-max-header-bytes connection)))
         (field-limit (min (or max-fields most-positive-fixnum)
                           (%http2-connection-max-fields connection)))
         (body-limit (or max-body-bytes
                         (%http2-connection-max-body-bytes connection)))
         (stream-id nil))
    (http-kit:with-http-deadline (absolute-deadline timeout
                                   :inherited deadline
                                   :clock-function clock)
      (%h2-validate-limit :max-header-bytes header-limit)
      (%h2-validate-limit :max-fields field-limit)
      (%h2-validate-limit :max-body-bytes body-limit :allow-zero t)
      (http-kit::%with-http-error-translation
          ("The HTTP/2 connection request failed." :http2-connection)
        (handler-case
            (let* ((session-started-p
                     (%http2-connection-session-started-p connection))
                   (reader (%h2-reader-for
                            (%http2-connection-stream connection)))
                   (writer (%h2-writer (%http2-connection-stream connection)
                                       absolute-deadline clock)))
              (when (and (http-kit:http-request-protocol request)
                         (not session-started-p))
                (%h2-connection-start-for-extended-connect
                 connection reader writer absolute-deadline clock)
                (setf session-started-p t))
              (when (and (http-kit:http-request-protocol request)
                         (not (%http2-connection-peer-enable-connect-protocol-p
                               connection)))
                (error 'http-kit:http-protocol-error
                       :message "The HTTP/2 peer did not enable extended CONNECT."
                       :operation :http2-write
                       :detail :protocol-error))
              (when (and session-started-p
                         (eql 0
                              (%http2-connection-peer-max-concurrent-streams
                               connection)))
                (%h2-connection-wait-for-stream-capacity
                 connection reader writer absolute-deadline clock))
              (setf stream-id (%h2-connection-next-stream-id connection))
              (multiple-value-bind (header-wire body outgoing-frame-size
                                     expected-body-length trailer-fields)
                  (%h2-request-header-wire
                   request
                   (%http2-connection-max-frame-size connection)
                   header-limit body-limit
                   :stream-id stream-id
                   :include-session-p (not session-started-p)
                   :peer-max-frame-size
                   (%http2-connection-peer-max-frame-size connection)
                   :peer-max-header-list-size
                   (%http2-connection-peer-max-header-list-size connection)
                   :request-body-function request-body-function
                   :request-body-length request-body-length
                   :huffman-p huffman-p)
                (%h2-write-wire
                 (%http2-connection-stream connection)
                 header-wire absolute-deadline clock)
                (incf (%http2-connection-next-stream-id connection) 2)
                (when on-stream-open
                  (funcall on-stream-open
                           stream-id
                           (lambda (&key (error-code 8))
                             (cancel-http2-stream
                              connection stream-id
                              :error-code error-code
                              :deadline absolute-deadline
                              :clock-function clock))))
                (multiple-value-bind (pending-frames read-initial-settings-p)
                    (%h2-connection-send-request-body
                     connection reader writer stream-id body
                     request-body-function expected-body-length
                     body-limit outgoing-frame-size
                     trailer-fields
                     absolute-deadline clock (not session-started-p)
                     huffman-p)
                  (when read-initial-settings-p
                    (setf (%http2-connection-session-started-p connection) t))
                  (let ((response
                          (%h2-read-response
                           reader writer
                           (%http2-connection-max-frame-size connection)
                           absolute-deadline clock header-limit body-limit
                           (http-kit:http-request-method request)
                           on-body-chunk collect-body-p
                           :expected-stream-id stream-id
                           :hpack-context
                           (%http2-connection-hpack-context connection)
                           :max-fields field-limit :on-information on-information
                           :budget-owner connection
                           :read-initial-settings-p read-initial-settings-p
                           :peer-max-frame-size
                           (%http2-connection-peer-max-frame-size connection)
                           :initial-frames pending-frames
                           :on-peer-settings
                           (lambda (peer-frame-size peer-table-size
                                    peer-window-size peer-enable-connect
                                    peer-max-concurrent-streams
                                    peer-max-header-list-size)
                             (%h2-connection-note-peer-settings
                              connection peer-frame-size peer-table-size
                              peer-window-size peer-enable-connect
                              peer-max-concurrent-streams
                              peer-max-header-list-size))
                           :control-handler
                           (lambda (frame response-writer expected-id)
                             (%h2-connection-control-handler
                              connection frame response-writer expected-id)))))
                    (setf (%http2-connection-session-started-p connection) t)
                    response))))
          (error (condition)
            (unless (%h2-peer-stream-reset-condition-p condition)
              (close-http2-connection connection))
            (error condition)))))))

(defun send-http2-request-over-connection/cps
    (connection request on-success
     &key on-error timeout deadline max-header-bytes max-fields max-body-bytes
       clock-function request-body-function request-body-length
       on-body-chunk on-stream-open (collect-body-p t) (huffman-p nil))
  "Send a request on a reusable HTTP/2 connection using CPS continuations."
  (http-kit::%call-http-operation/cps
   (lambda ()
     (send-http2-request-over-connection
      connection request
      :timeout timeout
      :deadline deadline
      :max-header-bytes max-header-bytes
      :max-fields max-fields
      :max-body-bytes max-body-bytes
      :clock-function clock-function
      :request-body-function request-body-function
      :request-body-length request-body-length
      :huffman-p huffman-p
      :on-body-chunk on-body-chunk
      :on-stream-open on-stream-open
      :collect-body-p collect-body-p))
   on-success
   :on-error on-error))

(defun make-http2-connection-transport (connection)
  "Return a high-level client transport function backed by CONNECTION."
  (unless (http2-connection-p connection)
    (error 'http-kit:http-protocol-error
           :message "MAKE-HTTP2-CONNECTION-TRANSPORT requires an HTTP2-CONNECTION."
           :operation :http2-client
           :detail (type-of connection)))
  (lambda (request
           &key timeout deadline max-header-bytes max-fields max-body-bytes clock-function
             request-body-function request-body-length
             on-body-chunk on-information on-stream-open
             (collect-body-p t) (huffman-p nil)
             &allow-other-keys)
    (send-http2-request-over-connection
     connection request
     :timeout timeout
     :deadline deadline
     :max-header-bytes max-header-bytes
     :max-fields max-fields
     :max-body-bytes max-body-bytes
     :clock-function clock-function
     :request-body-function request-body-function
     :request-body-length request-body-length
     :huffman-p huffman-p
     :on-body-chunk on-body-chunk
     :on-information on-information
     :on-stream-open on-stream-open
     :collect-body-p collect-body-p)))

(defstruct (%h2-batch-entry
             (:constructor %make-h2-batch-entry
                 (&key request stream-id body request-body-function
                       expected-body-length outgoing-frame-size trailer-fields
                       max-header-bytes max-fields max-body-bytes
                       on-body-chunk on-information collect-body-p
                       deadline clock-function)))
  request
  stream-id
  body
  request-body-function
  expected-body-length
  outgoing-frame-size
  trailer-fields
  max-header-bytes
  max-fields
  max-body-bytes
  on-body-chunk
  on-information
  (collect-body-p t)
  deadline
  clock-function
  body-sent-p
  error
  local-retired-p
  callback-events
  callback-events-tail
  (body-position 0)
  status
  headers
  (body-vector (make-array 0
                           :element-type '(unsigned-byte 8)
                           :adjustable t
                           :fill-pointer 0))
  (body-length 0)
  response)

(defstruct (%h2-scheduler-ping-waiter
             (:constructor %make-h2-scheduler-ping-waiter (payload)))
  payload
  acknowledged-p
  failure)

(defun %h2-batch-entry-for-stream (entries stream-id)
  (find stream-id entries
        :key #'%h2-batch-entry-stream-id
        :test #'=))

(defun %h2-batch-read-header-block
    (first-frame next-frame max-frame-size max-header-bytes)
  "Read one complete response header block through NEXT-FRAME.

NEXT-FRAME is used instead of the ordinary frame reader because a flow-control
stall can make the batch uploader read response frames into a pending queue.
CONTINUATION frames must still be consumed contiguously from that queue."
  (declare (ignore max-frame-size))
  (let ((stream-id (%h2-frame-stream-id first-frame)))
    (let ((first-fragment (%h2-header-fragment first-frame :first-p t))
          (parts nil)
          (header-block-bytes 0)
          (frame-count 1)
          (continuation-count 0)
          (frame first-frame))
      (push first-fragment parts)
      (%h2-check-header-block-limit :frames frame-count
                                    *h2-max-header-block-frames*)
      (%h2-check-header-block-limit :parts (length parts)
                                    *h2-max-header-block-parts*)
      (incf header-block-bytes (length first-fragment))
      (http-kit::%check-limit :headers header-block-bytes max-header-bytes)
      (loop until (/= 0 (logand (%h2-frame-flags frame)
                                +http2-end-headers-flag+))
            do (setf frame (funcall next-frame))
               (when (eq frame :eof)
                 (error 'http-kit:http-protocol-error
                        :message "HTTP/2 HEADERS ended before CONTINUATION END_HEADERS."
                        :operation :http2-headers
                        :detail :eof))
               (unless (and (= (%h2-frame-type frame)
                               +http2-continuation-type+)
                            (= (%h2-frame-stream-id frame) stream-id))
                 (error 'http-kit:http-protocol-error
                        :message "HTTP/2 HEADERS must be followed by same-stream CONTINUATION frames."
                        :operation :http2-headers
                        :detail (list (%h2-frame-type frame)
                                      (%h2-frame-stream-id frame))))
               (when (/= 0 (logand (%h2-frame-flags frame)
                                   (lognot +http2-end-headers-flag+)))
                 (error 'http-kit:http-protocol-error
                        :message "An HTTP/2 CONTINUATION has an invalid flag."
                        :operation :http2-headers
                        :detail (%h2-frame-flags frame)))
               (incf frame-count)
               (incf continuation-count)
               (%h2-check-header-block-limit :frames frame-count
                                             *h2-max-header-block-frames*)
               (%h2-check-header-block-limit :continuations
                                             continuation-count
                                             *h2-max-continuation-frames*)
               (push (%h2-frame-payload frame) parts)
               (%h2-check-header-block-limit :parts (length parts)
                                             *h2-max-header-block-parts*)
               (incf header-block-bytes
                     (length (%h2-frame-payload frame)))
               (http-kit::%check-limit :headers header-block-bytes
                                       max-header-bytes))
      (values (%h2-concat (nreverse parts))
              (/= 0 (logand (%h2-frame-flags first-frame)
                            +http2-end-stream-flag+))))))

(defun %h2-batch-process-headers-frame
    (entry frame next-frame max-frame-size max-header-bytes max-fields context
           request-method)
  (multiple-value-bind (block end-stream)
      (%h2-batch-read-header-block frame next-frame max-frame-size
                                   max-header-bytes)
    (let* ((fields (%h2-decode-header-block block context
                                        :max-header-bytes max-header-bytes
                                        :max-fields max-fields))
           (status (%h2-batch-entry-status entry))
           (headers (%h2-batch-entry-headers entry))
           (body (%h2-batch-entry-body-vector entry))
           (body-length (%h2-batch-entry-body-length entry)))
      (if status
          (progn
            (unless end-stream
              (error 'http-kit:http-protocol-error
                     :message "HTTP/2 trailing HEADERS must end the stream."
                     :operation :http2-trailers
                     :detail :missing-end-stream))
            (values status headers
                    (%h2-finish-request-response
                     status headers (%h2-trailers fields) body request-method
                     :body-length body-length)))
          (multiple-value-bind (candidate-status candidate-headers)
              (%h2-status-and-headers fields)
            (if (< candidate-status 200)
                (progn
                  (when (http-kit:http-header-values
                         candidate-headers "content-length")
                    (error 'http-kit:http-invalid-header
                           :message "An informational HTTP/2 response cannot contain Content-Length."
                           :operation :http2-response
                           :name "content-length"
                           :reason :forbidden))
                  (when end-stream
                    (error 'http-kit:http-protocol-error
                           :message "An informational HTTP/2 response cannot end the stream."
                           :operation :http2-read
                           :detail candidate-status))
                  (when (%h2-batch-entry-on-information entry)
                    (funcall (%h2-batch-entry-on-information entry)
                             (http-kit:make-http-response
                              :status candidate-status
                              :headers candidate-headers
                              :protocol-version "HTTP/2")))
                  (values nil nil nil))
                (values candidate-status candidate-headers
                        (when end-stream
                          (%h2-finish-request-response
                           candidate-status candidate-headers nil body
                           request-method :body-length body-length)))))))))

#+sbcl
(progn
  (defun %h2-scheduler-writer (connection deadline clock-function)
    (%h2-writer (%http2-connection-stream connection)
                deadline clock-function
                (%http2-connection-writer-lock connection)))

  (defun %h2-scheduler-condition-wait (connection deadline clock-function)
    (http-kit::%check-deadline deadline clock-function :read)
    (let* ((lock (%http2-connection-scheduler-lock connection))
           (timeout (and deadline
                         (max 0 (- deadline (funcall clock-function))))))
      (sb-thread:condition-wait
       (%http2-connection-scheduler-condition connection)
       lock
       :timeout timeout)
      (unless (sb-thread:holding-mutex-p lock)
        (sb-sys:without-interrupts
          (sb-sys:allow-with-interrupts
            (sb-thread:grab-mutex lock))))
      (http-kit::%check-deadline deadline clock-function :read)))

  (defun %h2-scheduler-broadcast (connection)
    (sb-thread:condition-broadcast
     (%http2-connection-scheduler-condition connection)))

  (defun %h2-scheduler-entry-ready-p (entry)
    (or (%h2-batch-entry-error entry)
        (%h2-batch-entry-callback-events entry)
        (and (%h2-batch-entry-response entry)
             (%h2-batch-entry-body-sent-p entry))))

  (defun %h2-scheduler-control-writer (connection clock-function)
    (%h2-writer (%http2-connection-stream connection)
                nil clock-function
                (%http2-connection-writer-lock connection)))

  (defun %h2-scheduler-return-flow-credit
      (connection stream-id increment clock-function &optional connection-only-p)
    (let ((writer (%h2-scheduler-control-writer connection clock-function)))
      (unless connection-only-p
        (%h2-send-window-update writer stream-id increment))
      (%h2-send-window-update writer 0 increment)))

  (defun %h2-scheduler-queue-callback (connection entry event)
    (let ((queued-p nil))
      (%h2-with-scheduler-lock (connection)
        (unless (or (%h2-batch-entry-local-retired-p entry)
                    (%h2-batch-entry-error entry))
          (let ((cell (list event))
                (tail (%h2-batch-entry-callback-events-tail entry)))
            (if tail
                (setf (cdr tail) cell)
                (setf (%h2-batch-entry-callback-events entry) cell))
            (setf (%h2-batch-entry-callback-events-tail entry) cell
                  queued-p t))
          (%h2-scheduler-broadcast connection)))
      queued-p))

  (defun %h2-scheduler-pop-callback (connection entry)
    (%h2-with-scheduler-lock (connection)
      (let ((events (%h2-batch-entry-callback-events entry)))
        (when events
          (setf (%h2-batch-entry-callback-events entry) (cdr events))
          (unless (%h2-batch-entry-callback-events entry)
            (setf (%h2-batch-entry-callback-events-tail entry) nil))
          (car events)))))

  (defun %h2-scheduler-retire-entry
      (connection entry condition clock-function &optional (flow-credit 0))
    (let ((reset-p nil)
          (connection-credit flow-credit))
      (%h2-with-scheduler-lock (connection)
        (unless (or (%h2-batch-entry-local-retired-p entry)
                    (%h2-peer-stream-reset-condition-p
                     (%h2-batch-entry-error entry)))
          (setf (%h2-batch-entry-local-retired-p entry) t)
          (unless (%h2-batch-entry-error entry)
            (setf (%h2-batch-entry-error entry) condition))
          (setf reset-p
                (member entry (%http2-connection-scheduler-entries connection)
                        :test #'eq))
          (%h2-scheduler-remove-entry connection entry)
          (loop for event in (%h2-batch-entry-callback-events entry)
                when (eq (first event) :body)
                  do (incf connection-credit (fourth event)))
          (setf (%h2-batch-entry-callback-events entry) nil
                (%h2-batch-entry-callback-events-tail entry) nil
                (%h2-batch-entry-body-vector entry)
                (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0)
                (%h2-batch-entry-body-length entry) 0
                (%h2-batch-entry-response entry) nil)
          (%h2-scheduler-broadcast connection)))
      (handler-case
          (progn
            (when (plusp connection-credit)
              (%h2-scheduler-return-flow-credit
               connection 0 connection-credit clock-function t))
            (when reset-p
              (%h2-send-rst-stream
               (%h2-scheduler-control-writer connection clock-function)
               (%h2-batch-entry-stream-id entry) 8)))
        (error (reset-condition)
          (%h2-scheduler-fail-connection connection reset-condition)
          (error reset-condition)))))

  (defun %h2-scheduler-run-entry-callbacks
      (connection entry clock-function)
    (loop for event = (%h2-scheduler-pop-callback connection entry)
          while event
          do (let ((entry-error
                     (%h2-with-scheduler-lock (connection)
                       (%h2-batch-entry-error entry))))
               (ecase (first event)
                 (:information
                  (unless entry-error
                    (handler-case
                        (funcall (second event) (third event))
                      (error (condition)
                        (%h2-scheduler-retire-entry
                         connection entry condition clock-function)
                        (error condition)))))
                 (:body
                  (if entry-error
                      (%h2-scheduler-return-flow-credit
                       connection 0 (fourth event) clock-function t)
                      (progn
                        (handler-case
                            (funcall (second event) (third event))
                          (error (condition)
                            (%h2-scheduler-retire-entry
                             connection entry condition clock-function
                             (fourth event))
                            (error condition)))
                        (%h2-scheduler-return-flow-credit
                         connection
                         (%h2-batch-entry-stream-id entry)
                         (fourth event)
                         clock-function
                         (%h2-with-scheduler-lock (connection)
                           (or (%h2-batch-entry-local-retired-p entry)
                               (%h2-batch-entry-error entry)
                               (%h2-batch-entry-response entry)
                               (fifth event)))))))))))

  (defun %h2-scheduler-entry-for-stream (connection stream-id)
    (%h2-batch-entry-for-stream
     (%http2-connection-scheduler-entries connection) stream-id))

  (defun %h2-scheduler-has-non-error-entry-p (connection)
    (some (lambda (entry)
            (not (%h2-batch-entry-error entry)))
          (%http2-connection-scheduler-entries connection)))

  (defun %h2-scheduler-remove-entry (connection entry)
    (setf (%http2-connection-scheduler-entries connection)
          (delete entry (%http2-connection-scheduler-entries connection)
                  :test #'eq)))

  (defun %h2-scheduler-close-goaway-retired-if-idle (connection)
    (let ((close-p nil))
      (%h2-with-scheduler-lock (connection)
        (let ((last-stream-id
                (%http2-connection-goaway-last-stream-id connection))
              (highest-opened-stream-id
                (max 0 (- (%http2-connection-next-stream-id connection) 2))))
          (when (and (not (%http2-connection-closed-p connection))
                     last-stream-id
                     (> highest-opened-stream-id last-stream-id)
                     (null (%http2-connection-scheduler-entries connection)))
            (setf (%http2-connection-closed-p connection) t
                  close-p t)
            (%h2-scheduler-broadcast connection))))
      (when close-p
        (http-kit::%with-http-cleanup
          (funcall (%http2-connection-close-stream connection)
                   (%http2-connection-stream connection))))))

  (defun %h2-scheduler-mark-response (connection entry response)
    (%h2-with-scheduler-lock (connection)
      (if (%h2-batch-entry-local-retired-p entry)
          (setf (%h2-batch-entry-response entry) nil)
          (setf (%h2-batch-entry-response entry) response))
      (when (or (%h2-batch-entry-body-sent-p entry)
                (%h2-batch-entry-local-retired-p entry))
        (%h2-scheduler-remove-entry connection entry))
      (%h2-scheduler-broadcast connection)))

  (defun %h2-scheduler-mark-body-sent (connection entry)
    (%h2-with-scheduler-lock (connection)
      (setf (%h2-batch-entry-body-sent-p entry) t)
      (when (%h2-batch-entry-response entry)
        (%h2-scheduler-remove-entry connection entry))
      (%h2-scheduler-broadcast connection)))

  (defun %h2-scheduler-window-available-p (connection stream-id)
    (and (plusp (%http2-connection-peer-connection-window-size connection))
         (plusp (%h2-connection-stream-window connection stream-id))))

  (defun %h2-scheduler-capacity-available-p (connection)
    (let ((limit (%http2-connection-peer-max-concurrent-streams connection)))
      (and (not (eql limit 0))
           (or (null limit)
               (< (count-if-not #'%h2-batch-entry-error
                                (%http2-connection-scheduler-entries
                                 connection))
                  limit)))))

  (defun %h2-scheduler-control-frame (connection frame writer)
    (let* ((type (%h2-frame-type frame))
           (stream-id (%h2-frame-stream-id frame))
           (payload (%h2-frame-payload frame)))
      (%h2-client-control-budget-check connection frame writer)
      (cond
        ((= type +http2-settings-type+)
         (%h2-connection-note-settings connection frame writer))
        ((= type +http2-ping-type+)
         (unless (and (zerop stream-id) (= (length payload) 8))
           (if (some (lambda (waiter)
                       (and (not (%h2-scheduler-ping-waiter-acknowledged-p
                                  waiter))
                            (not (%h2-scheduler-ping-waiter-failure waiter))))
                     (%http2-connection-scheduler-ping-waiters connection))
               (error 'http-kit:http-protocol-error
                      :message "An HTTP/2 PING frame is invalid."
                      :operation :http2-client
                      :detail frame)
               (error 'http-kit:http-protocol-error
                      :message "An HTTP/2 PING must be an eight-byte stream-zero frame."
                      :operation :http2-control
                      :detail (list stream-id (length payload)))))
         (if (/= 0 (logand (%h2-frame-flags frame) +http2-ack-flag+))
             (let ((waiter
                     (find-if
                      (lambda (candidate)
                        (and (not (%h2-scheduler-ping-waiter-acknowledged-p
                                   candidate))
                             (equalp payload
                                     (%h2-scheduler-ping-waiter-payload
                                      candidate))))
                      (%http2-connection-scheduler-ping-waiters connection))))
               (when waiter
                 (setf (%h2-scheduler-ping-waiter-acknowledged-p waiter) t)))
             (%h2-send-control writer +http2-ping-type+ +http2-ack-flag+
                               0 payload)))
        ((= type +http2-window-update-type+)
         (when (and (plusp stream-id)
                    (>= stream-id (%http2-connection-next-stream-id connection)))
           (error 'http-kit:http-protocol-error
                  :message "An HTTP/2 WINDOW_UPDATE targeted an idle stream."
                  :operation :http2-control
                  :detail stream-id))
         (%h2-connection-note-window-update
          connection frame (if (zerop stream-id) 1 stream-id))
         (%h2-handle-control-frame
          frame writer (if (zerop stream-id) 1 stream-id)))
        ((= type +http2-goaway-type+)
         (unless (and (zerop stream-id) (>= (length payload) 8))
           (error 'http-kit:http-protocol-error
                  :message "An HTTP/2 GOAWAY frame is invalid."
                  :operation :http2-control
                  :detail (list stream-id (length payload))))
         (let ((last-stream-id (%h2-u32 payload 0)))
           (when (/= 0 (logand last-stream-id #x80000000))
             (error 'http-kit:http-protocol-error
                    :message "An HTTP/2 GOAWAY last-stream identifier is invalid."
                    :operation :http2-control
                    :detail last-stream-id))
           (setf (%http2-connection-goaway-last-stream-id connection)
                 last-stream-id)
           (dolist (entry
                    (copy-list
                     (%http2-connection-scheduler-entries connection)))
             (when (> (%h2-batch-entry-stream-id entry) last-stream-id)
               (unless (%h2-batch-entry-error entry)
                 (setf (%h2-batch-entry-error entry)
                       (make-condition
                        'http-kit:http-connection-error
                        :message "The HTTP/2 peer rejected the active stream with GOAWAY."
                        :operation :http2-control
                        :cause (list :goaway last-stream-id
                                     (%h2-u32 payload 4)))))
               (when (%h2-batch-entry-local-retired-p entry)
                 (%h2-scheduler-remove-entry connection entry))))))
        ((= type +http2-rst-stream-type+)
         (unless (and (plusp stream-id) (= (length payload) 4))
           (error 'http-kit:http-protocol-error
                  :message "An HTTP/2 RST_STREAM frame is invalid."
                  :operation :http2-control
                  :detail (list stream-id (length payload))))
         (when (>= stream-id (%http2-connection-next-stream-id connection))
           (error 'http-kit:http-protocol-error
                  :message "An HTTP/2 RST_STREAM targeted an idle stream."
                  :operation :http2-control
                  :detail stream-id))
         (let ((entry (%h2-scheduler-entry-for-stream connection stream-id)))
           (when entry
             (unless (%h2-batch-entry-error entry)
               (setf (%h2-batch-entry-error entry)
                     (make-condition
                      'http-kit:http-connection-error
                      :message "The HTTP/2 peer reset the response stream."
                      :operation :http2-control
                      :cause (list :rst-stream stream-id
                                   (%h2-u32 payload 0)))))
             (when (%h2-batch-entry-local-retired-p entry)
               (%h2-scheduler-remove-entry connection entry)))))
        (t
         (%h2-handle-control-frame frame writer
                                  (max 1 (%http2-connection-next-stream-id
                                          connection)))))
      (%h2-scheduler-broadcast connection))))

  (defun %h2-scheduler-dispatch-retired-data
      (connection entry frame clock-function)
    (%h2-data-payload frame)
    (%h2-scheduler-return-flow-credit
     connection 0 (length (%h2-frame-payload frame)) clock-function t)
    (when (and entry
               (/= 0 (logand (%h2-frame-flags frame)
                             +http2-end-stream-flag+)))
      (%h2-with-scheduler-lock (connection)
        (%h2-scheduler-remove-entry connection entry)
        (%h2-scheduler-broadcast connection))
      (%h2-scheduler-close-goaway-retired-if-idle connection)))

  (defun %h2-scheduler-previously-opened-client-stream-p
      (connection stream-id)
    (%h2-with-scheduler-lock (connection)
      (and (plusp stream-id)
           (oddp stream-id)
           (<= stream-id
               (%http2-connection-scheduler-last-opened-stream-id connection)))))

  (defun %h2-scheduler-discard-closed-headers
      (connection frame reader deadline clock-function)
    (multiple-value-bind (block end-stream-p)
        (%h2-batch-read-header-block
         frame
         (lambda ()
           (%h2-read-frame reader
                           (%http2-connection-max-frame-size connection)
                           deadline clock-function))
         (%http2-connection-max-frame-size connection)
         (%http2-connection-max-header-bytes connection))
      (declare (ignore end-stream-p))
      (%h2-decode-header-block
       block
       (%http2-connection-hpack-context connection)
       :max-header-bytes (%http2-connection-max-header-bytes connection)
       :max-fields (%http2-connection-max-fields connection))))

  (defun %h2-scheduler-dispatch-frame
      (connection frame reader writer deadline clock-function)
    (declare (ignore writer))
    (when (%http2-connection-scheduler-first-peer-frame-p connection)
      (unless (and (= (%h2-frame-type frame) +http2-settings-type+)
                   (zerop (%h2-frame-stream-id frame))
                   (zerop (logand (%h2-frame-flags frame) +http2-ack-flag+)))
        (error 'http-kit:http-protocol-error
               :message "The first HTTP/2 peer frame must be a non-ACK SETTINGS frame."
               :operation :http2-read
               :detail (list (%h2-frame-type frame)
                             (%h2-frame-stream-id frame)
                             (%h2-frame-flags frame))))
      (setf (%http2-connection-scheduler-first-peer-frame-p connection) nil))
    (let ((type (%h2-frame-type frame))
          (stream-id (%h2-frame-stream-id frame))
          (control-writer (%h2-scheduler-control-writer connection
                                                        clock-function)))
      (cond
        ((%h2-control-frame-p type)
         (%h2-with-scheduler-lock (connection)
           (%h2-scheduler-control-frame connection frame control-writer))
         (when (= type +http2-goaway-type+)
           (%h2-scheduler-close-goaway-retired-if-idle connection)))
        ((= type +http2-headers-type+)
         (let ((entry
                 (%h2-with-scheduler-lock (connection)
                   (%h2-scheduler-entry-for-stream connection stream-id))))
           (unless entry
             (if (%h2-scheduler-previously-opened-client-stream-p
                  connection stream-id)
                 (%h2-scheduler-discard-closed-headers
                  connection frame reader deadline clock-function)
                 (error 'http-kit:http-protocol-error
                        :message "The HTTP/2 scheduler received HEADERS for an unknown stream."
                        :operation :http2-read
                        :detail stream-id)))
           (when entry
             (when (%h2-batch-entry-response entry)
               (error 'http-kit:http-protocol-error
                      :message "The HTTP/2 scheduler received HEADERS after stream completion."
                      :operation :http2-read
                      :detail stream-id))
             (let* ((retired-p (%h2-batch-entry-local-retired-p entry))
                    (processing-entry (copy-%h2-batch-entry entry))
                    (on-information
                      (and (not retired-p)
                           (%h2-batch-entry-on-information entry))))
               (setf (%h2-batch-entry-on-information processing-entry)
                     (when on-information
                       (lambda (response)
                         (%h2-scheduler-queue-callback
                          connection entry
                          (list :information on-information response)))))
               (multiple-value-bind (status headers response)
                   (%h2-batch-process-headers-frame
                    processing-entry frame
                    (lambda ()
                      (%h2-read-frame reader
                                      (%http2-connection-max-frame-size connection)
                                      deadline clock-function))
                    (%http2-connection-max-frame-size connection)
                    (%h2-batch-entry-max-header-bytes entry)
                    (%h2-batch-entry-max-fields entry)
                    (%http2-connection-hpack-context connection)
                    (http-kit:http-request-method
                     (%h2-batch-entry-request entry)))
                 (when status
                   (setf (%h2-batch-entry-status entry) status
                         (%h2-batch-entry-headers entry) headers))
                 (when response
                   (if retired-p
                       (%h2-with-scheduler-lock (connection)
                         (%h2-scheduler-remove-entry connection entry)
                         (%h2-scheduler-broadcast connection))
                       (%h2-scheduler-mark-response
                        connection entry response))))))))
        ((= type +http2-data-type+)
         (let ((entry nil)
               (non-error-entry-p nil)
               (pending-ping-p nil))
           (%h2-with-scheduler-lock (connection)
             (setf entry (%h2-scheduler-entry-for-stream connection stream-id)
                   non-error-entry-p (%h2-scheduler-has-non-error-entry-p connection)
                   pending-ping-p
                   (some (lambda (waiter)
                           (and (not (%h2-scheduler-ping-waiter-acknowledged-p waiter))
                                (not (%h2-scheduler-ping-waiter-failure waiter))))
                         (%http2-connection-scheduler-ping-waiters connection))))
           (unless entry
             (if (%h2-scheduler-previously-opened-client-stream-p
                  connection stream-id)
                 (if (and pending-ping-p (not non-error-entry-p))
                     (error 'http-kit:http-protocol-error
                            :message "The HTTP/2 PING received an unexpected frame."
                            :operation :http2-client
                            :detail frame)
                     (%h2-scheduler-dispatch-retired-data
                      connection nil frame clock-function))
                 (error 'http-kit:http-protocol-error
                        :message "The HTTP/2 scheduler received DATA for an unknown stream."
                        :operation :http2-read
                        :detail (if non-error-entry-p
                                    stream-id
                                    (list type stream-id)))))
           (when entry
  (if (%h2-batch-entry-local-retired-p entry)
      (%h2-scheduler-dispatch-retired-data
       connection entry frame clock-function)
      (progn
        (when (%h2-batch-entry-response entry)
          (error 'http-kit:http-protocol-error
                 :message "The HTTP/2 scheduler received DATA after stream completion."
                 :operation :http2-read
                 :detail stream-id))
        (let* ((callback (%h2-batch-entry-on-body-chunk entry))
               (callback-queued-p nil)
               (flow-controlled-length
                 (length (%h2-frame-payload frame)))
               (body-vector (%h2-batch-entry-body-vector entry))
               (collect-body-p (%h2-batch-entry-collect-body-p entry))
               (queue-callback
                 (when callback
                   (lambda (payload)
                     (setf callback-queued-p
                           (%h2-scheduler-queue-callback
                            connection entry
                            (list :body callback payload flow-controlled-length
                                  (/= 0 (logand (%h2-frame-flags frame)
                                                +http2-end-stream-flag+)))))))))
          (multiple-value-bind (end-stream-p body-length payload-length)
              (%h2-append-data-frame*
               frame
               (%h2-batch-entry-status entry)
               body-vector
               (%h2-batch-entry-body-length entry)
               (http-kit:http-request-method
                (%h2-batch-entry-request entry))
               (%h2-batch-entry-max-body-bytes entry)
               queue-callback
               collect-body-p
               stream-id)
            (let ((retired-p
                    (%h2-with-scheduler-lock (connection)
                      (or (%h2-batch-entry-local-retired-p entry)
                          (%h2-batch-entry-error entry)))))
              (if retired-p
                  (when collect-body-p
                    (setf (fill-pointer body-vector) 0))
                  (setf (%h2-batch-entry-body-length entry) body-length))
              (unless callback-queued-p
                (%h2-with-scheduler-lock (connection)
                  (unless (%http2-connection-closed-p connection)
                    (%h2-scheduler-return-flow-credit
                     connection stream-id payload-length clock-function
                     (or retired-p end-stream-p))))))
            (when end-stream-p
              (%h2-scheduler-mark-response
               connection entry
               (%h2-finish-request-response
                (%h2-batch-entry-status entry)
                (%h2-batch-entry-headers entry)
                nil
                body-vector
                (http-kit:http-request-method
                 (%h2-batch-entry-request entry))
                :body-length body-length))))))))))
        ((= type +http2-continuation-type+)
         (error 'http-kit:http-protocol-error
                :message "An HTTP/2 CONTINUATION arrived without HEADERS."
                :operation :http2-headers
                :detail stream-id))
        ((= type +http2-push-promise-type+)
         (error 'http-kit:http-protocol-error
                :message "The peer sent HTTP/2 PUSH_PROMISE after server push was disabled."
                :operation :http2-read
                :detail :protocol-error))
        (t nil))))

  (defun %h2-scheduler-pump-until
      (connection predicate reader writer deadline clock-function)
    (loop
      (when (%h2-with-scheduler-lock (connection)
              (or (%http2-connection-closed-p connection)
                  (funcall predicate)))
        (return))
      (let ((frame (%h2-read-frame
                    reader (%http2-connection-max-frame-size connection)
                    deadline clock-function)))
        (when (eq frame :eof)
          (error 'http-kit:http-connection-error
                 :message "The HTTP/2 peer closed during a scheduled request."
                 :operation :http2-read
                 :cause :eof))
        (%h2-scheduler-dispatch-frame
         connection frame reader writer deadline clock-function))))

  (defun %h2-scheduler-wait-until
      (connection predicate reader writer deadline clock-function
       &optional entry)
    (handler-case
        (loop
          (http-kit::%check-deadline deadline clock-function :read)
          (let ((run-pump-p nil)
                (finished-p nil))
            (%h2-with-scheduler-lock (connection)
              (setf finished-p
                    (or (%http2-connection-closed-p connection)
                        (funcall predicate)))
              (unless finished-p
                (if (%http2-connection-scheduler-reader-active-p connection)
                    (%h2-scheduler-condition-wait
                     connection deadline clock-function)
                    (setf (%http2-connection-scheduler-reader-active-p connection)
                          t
                          run-pump-p t))))
            (when finished-p
              (return))
            (when run-pump-p
              (unwind-protect
                   (handler-case
                       (%h2-scheduler-pump-until
                        connection predicate reader writer deadline clock-function)
                     (http-kit:http-timeout (condition)
                       (when entry
                         (%h2-scheduler-retire-entry
                          connection entry condition clock-function))
                       (error condition))
                     (error (condition)
                       (%h2-scheduler-fail-connection connection condition)
                       (error condition)))
                (%h2-with-scheduler-lock (connection)
                  (setf (%http2-connection-scheduler-reader-active-p connection)
                        nil)
                  (%h2-scheduler-broadcast connection))))))
      (http-kit:http-timeout (condition)
        (when entry
          (%h2-scheduler-retire-entry
           connection entry condition clock-function))
        (error condition))))

  (defun %h2-scheduler-ping
      (connection payload deadline clock-function)
    (let* ((reader (%h2-reader-for (%http2-connection-stream connection)))
           (writer (%h2-scheduler-writer connection deadline clock-function))
           (waiter (%make-h2-scheduler-ping-waiter payload)))
      (%h2-with-scheduler-lock (connection)
        (unless (http2-connection-open-p connection)
          (error 'http-kit:http-connection-error
                 :message "The HTTP/2 connection closed before PING."
                 :operation :http2-control
                 :cause :closed))
        (push waiter (%http2-connection-scheduler-ping-waiters connection)))
      (unwind-protect
           (progn
             (handler-case
                 (progn
                   (handler-case
                       (%h2-send-control writer +http2-ping-type+ 0 0 payload)
                     (error (condition)
                       (let ((failure
                               (%h2-with-scheduler-lock (connection)
                                 (%h2-scheduler-ping-waiter-failure waiter))))
                         (error (or failure condition)))))
                   (%h2-scheduler-wait-until
                    connection
                    (lambda ()
                      (%h2-scheduler-ping-waiter-acknowledged-p waiter))
                    reader writer deadline clock-function)
                   (let ((failure
                           (%h2-with-scheduler-lock (connection)
                             (%h2-scheduler-ping-waiter-failure waiter))))
                     (when failure
                       (error failure))))
               (http-kit:http-connection-error (condition)
                 (if (and (eq :http2-read (http-kit:http-error-operation condition))
                          (eq :eof (http-kit:http-connection-error-cause condition)))
                     (error 'http-kit:http-protocol-error
                            :message "The HTTP/2 peer closed while awaiting PING ACK."
                            :operation :http2-client
                            :detail :eof)
                     (error condition))))
             (%h2-with-scheduler-lock (connection)
               (unless (http2-connection-open-p connection)
                 (error 'http-kit:http-connection-error
                        :message "The HTTP/2 connection closed while awaiting PING ACK."
                        :operation :http2-control
                        :cause :closed))
               payload))
        (%h2-with-scheduler-lock (connection)
          (setf (%http2-connection-scheduler-ping-waiters connection)
                (delete waiter
                        (%http2-connection-scheduler-ping-waiters connection)
                        :test #'eq))
          (%h2-scheduler-broadcast connection)))))

  (defun %h2-scheduler-wait-for-window
      (connection stream-id deadline clock-function)
    (let ((reader (%h2-reader-for (%http2-connection-stream connection)))
          (writer (%h2-scheduler-writer connection deadline clock-function)))
      (loop
        (let ((entry (%h2-with-scheduler-lock (connection)
                       (%h2-scheduler-entry-for-stream connection stream-id))))
          (%h2-scheduler-wait-until
           connection
           (lambda ()
             (let ((active-entry
                     (%h2-scheduler-entry-for-stream connection stream-id)))
               (or (null active-entry)
                   (%h2-batch-entry-error active-entry)
                   (%h2-batch-entry-callback-events active-entry)
                   (%h2-scheduler-window-available-p connection stream-id))))
           reader writer deadline clock-function entry)
          (when entry
            (%h2-scheduler-run-entry-callbacks
             connection entry clock-function))
          (%h2-with-scheduler-lock (connection)
            (let ((active-entry
                    (%h2-scheduler-entry-for-stream connection stream-id)))
              (when (and active-entry (%h2-batch-entry-error active-entry))
                (error (%h2-batch-entry-error active-entry)))
              (when (or (%http2-connection-closed-p connection)
                        (null active-entry))
                (error 'http-kit:http-connection-error
                       :message "The HTTP/2 connection closed while request flow control was blocked."
                       :operation :http2-write
                       :cause :closed))
              (when (%h2-scheduler-window-available-p connection stream-id)
                (return))))))))

  (defun %h2-scheduler-wait-for-capacity
      (connection reader writer deadline clock-function)
    (%h2-scheduler-wait-until
     connection
     (lambda () (%h2-scheduler-capacity-available-p connection))
     reader writer deadline clock-function)
    (%h2-with-scheduler-lock (connection)
      (unless (%h2-scheduler-capacity-available-p connection)
        (error 'http-kit:http-connection-error
               :message "The HTTP/2 connection closed while streams were paused."
               :operation :http2-write
               :cause :closed))))

  (defun %h2-scheduler-fail-connection (connection condition)
    (let ((close-p nil))
      (%h2-with-scheduler-lock (connection)
        (unless (%http2-connection-closed-p connection)
          (setf (%http2-connection-closed-p connection) t
                close-p t))
        (dolist (entry (%http2-connection-scheduler-entries connection))
          (unless (%h2-batch-entry-error entry)
            (setf (%h2-batch-entry-error entry) condition)))
        (dolist (waiter (%http2-connection-scheduler-ping-waiters connection))
          (unless (or (%h2-scheduler-ping-waiter-acknowledged-p waiter)
                      (%h2-scheduler-ping-waiter-failure waiter))
            (setf (%h2-scheduler-ping-waiter-failure waiter) condition)))
        (setf (%http2-connection-scheduler-entries connection) nil)
        (%h2-scheduler-broadcast connection))
      (when close-p
        (http-kit::%with-http-cleanup
          (funcall (%http2-connection-close-stream connection)
                   (%http2-connection-stream connection))))))

  (defun %h2-scheduler-open-entry
      (connection request body-function body-length header-limit field-limit
       body-limit deadline clock-function huffman-p on-body-chunk
       on-information collect-body-p reader writer)
    (loop
      (unless (%h2-with-scheduler-lock (connection)
                (%h2-scheduler-capacity-available-p connection))
        (%h2-scheduler-wait-for-capacity
         connection reader writer deadline clock-function))
      (let ((entry nil))
        (%h2-with-scheduler-lock (connection)
          (unless (http2-connection-open-p connection)
            (error 'http-kit:http-connection-error
                   :message "The HTTP/2 connection is closed."
                   :operation :http2-client
                   :cause :closed))
          (when (%h2-scheduler-capacity-available-p connection)
            (let ((session-started-p
                    (%http2-connection-session-started-p connection)))
              (when (and (http-kit:http-request-protocol request)
                         (not session-started-p))
                (%h2-connection-start-for-extended-connect
                 connection reader writer deadline clock-function)
                (setf session-started-p t
                      (%http2-connection-scheduler-first-peer-frame-p
                       connection) nil))
              (when (and (http-kit:http-request-protocol request)
                         (not (%http2-connection-peer-enable-connect-protocol-p
                               connection)))
                (error 'http-kit:http-protocol-error
                       :message "The HTTP/2 peer did not enable extended CONNECT."
                       :operation :http2-write
                       :detail :protocol-error))
              (let* ((stream-id (%h2-connection-next-stream-id connection))
                     (header-wire nil)
                     (body nil)
                     (outgoing-frame-size nil)
                     (expected-body-length nil)
                     (trailer-fields nil))
                (multiple-value-setq
                    (header-wire body outgoing-frame-size
                     expected-body-length trailer-fields)
                  (%h2-request-header-wire
                   request
                   (%http2-connection-max-frame-size connection)
                   header-limit body-limit
                   :stream-id stream-id
                   :include-session-p (not session-started-p)
                   :peer-max-frame-size
                   (%http2-connection-peer-max-frame-size connection)
                   :peer-max-header-list-size
                   (%http2-connection-peer-max-header-list-size connection)
                   :request-body-function body-function
                   :request-body-length body-length
                   :huffman-p huffman-p))
                (setf entry
                      (%make-h2-batch-entry
                       :request request
                       :stream-id stream-id
                       :body body
                       :request-body-function body-function
                       :expected-body-length expected-body-length
                       :outgoing-frame-size outgoing-frame-size
                       :trailer-fields trailer-fields
                       :max-header-bytes header-limit
                       :max-fields field-limit
                       :max-body-bytes body-limit
                       :on-body-chunk on-body-chunk
                       :on-information on-information
                       :collect-body-p collect-body-p
                       :deadline deadline
                       :clock-function clock-function))
                (push entry (%http2-connection-scheduler-entries connection))
                (incf (%http2-connection-next-stream-id connection) 2)
                (handler-case
                    (funcall writer header-wire)
                  (http-kit:http-timeout (condition)
                    (%h2-scheduler-remove-entry connection entry)
                    (%h2-scheduler-broadcast connection)
                    (error condition))
                  (error (condition)
                    (%h2-scheduler-remove-entry connection entry)
                    (%h2-scheduler-broadcast connection)
                    (error condition)))
                (setf (%http2-connection-scheduler-last-opened-stream-id
                       connection)
                      stream-id
                      (%http2-connection-session-started-p connection) t)))))
        (when entry
          (return entry)))))

  (defun %h2-send-http2-request-over-connection-scheduled
      (connection request
       &key timeout deadline max-header-bytes max-fields max-body-bytes
         clock-function request-body-function request-body-length
         on-body-chunk on-information on-stream-open
         (collect-body-p t) (huffman-p nil))
    (unless (http2-connection-p connection)
      (error 'http-kit:http-protocol-error
             :message "An HTTP/2 request requires an HTTP2-CONNECTION."
             :operation :http2-client
             :detail (type-of connection)))
    (unless (http2-connection-open-p connection)
      (error 'http-kit:http-connection-error
             :message "The HTTP/2 connection is closed."
             :operation :http2-client
             :cause :closed))
    (http-kit::%check-http-request request)
    (%h2-validate-request-body-options
     request request-body-function request-body-length)
    (when (and on-body-chunk (not (functionp on-body-chunk)))
      (error 'http-kit:http-protocol-error
             :message "ON-BODY-CHUNK must be a function or NIL."
             :operation :http2-client
             :detail on-body-chunk))
    (when (and on-information (not (functionp on-information)))
      (error 'http-kit:http-protocol-error
             :message "ON-INFORMATION must be a function or NIL."
             :operation :http2-client
             :detail on-information))
    (when (and on-stream-open (not (functionp on-stream-open)))
      (error 'http-kit:http-protocol-error
             :message "ON-STREAM-OPEN must be a function or NIL."
             :operation :http2-client
             :detail on-stream-open))
    (unless (member collect-body-p '(nil t))
      (error 'http-kit:http-protocol-error
             :message "COLLECT-BODY-P must be NIL or T."
             :operation :http2-client
             :detail collect-body-p))
    (let* ((clock (or clock-function
                      (%http2-connection-clock-function connection)))
           (header-limit (or max-header-bytes
                             (%http2-connection-max-header-bytes connection)))
           (field-limit (min (or max-fields most-positive-fixnum)
                             (%http2-connection-max-fields connection)))
           (body-limit (or max-body-bytes
                           (%http2-connection-max-body-bytes connection)))
           (reader (%h2-reader-for (%http2-connection-stream connection)))
           (entry nil))
      (unless (functionp clock)
        (error 'http-kit:http-protocol-error
               :message "An HTTP/2 request clock must be callable."
               :operation :http2-client
               :detail clock))
      (http-kit:with-http-deadline (absolute-deadline timeout
                                     :inherited deadline
                                     :clock-function clock)
        (%h2-validate-limit :max-header-bytes header-limit)
        (%h2-validate-limit :max-fields field-limit)
        (%h2-validate-limit :max-body-bytes body-limit :allow-zero t)
        (http-kit::%with-http-error-translation
            ("The HTTP/2 connection request failed." :http2-connection)
          (handler-case
              (let ((writer (%h2-scheduler-writer
                             connection absolute-deadline clock)))
                (setf entry
                      (%h2-scheduler-open-entry
                       connection request request-body-function
                       request-body-length header-limit field-limit body-limit
                       absolute-deadline clock huffman-p on-body-chunk
                       on-information collect-body-p reader writer))
                (when on-stream-open
                  (funcall on-stream-open
                           (%h2-batch-entry-stream-id entry)
                           (lambda (&key (error-code 8))
                             (cancel-http2-stream
                              connection (%h2-batch-entry-stream-id entry)
                              :error-code error-code
                              :deadline absolute-deadline
                              :clock-function clock))))
                (%h2-connection-send-request-body
                 connection reader writer
                 (%h2-batch-entry-stream-id entry)
                 (%h2-batch-entry-body entry)
                 request-body-function
                 (%h2-batch-entry-expected-body-length entry)
                 body-limit
                 (%h2-batch-entry-outgoing-frame-size entry)
                 (%h2-batch-entry-trailer-fields entry)
                 absolute-deadline clock nil huffman-p
                 :scheduler-p t
                 :flow-wait-function #'%h2-scheduler-wait-for-window)
                (%h2-scheduler-mark-body-sent connection entry)
                (loop
                  (%h2-scheduler-wait-until
                   connection
                   (lambda () (%h2-scheduler-entry-ready-p entry))
                   reader writer absolute-deadline clock entry)
                  (%h2-scheduler-run-entry-callbacks
                   connection entry clock)
                  (let ((response
                          (%h2-with-scheduler-lock (connection)
                            (when (%h2-batch-entry-error entry)
                              (error (%h2-batch-entry-error entry)))
                            (when (%http2-connection-closed-p connection)
                              (error 'http-kit:http-connection-error
                                     :message "The HTTP/2 connection closed before the response completed."
                                     :operation :http2-read
                                     :cause :closed))
                            (when (and (%h2-batch-entry-response entry)
                                       (%h2-batch-entry-body-sent-p entry)
                                       (null (%h2-batch-entry-callback-events entry)))
                              (%h2-batch-entry-response entry)))))
                    (when response
                      (%h2-scheduler-close-goaway-retired-if-idle connection)
                      (return response)))))
            (error (condition)
              (let ((timeout-p (typep condition 'http-kit:http-timeout))
                    (peer-reset-p
                      (%h2-peer-stream-reset-condition-p condition)))
                (when entry
                  (cond
                    (timeout-p
                     (%h2-scheduler-retire-entry
                      connection entry condition clock))
                    (peer-reset-p
                     (%h2-with-scheduler-lock (connection)
                       (%h2-scheduler-remove-entry connection entry)))
                    ((%h2-batch-entry-local-retired-p entry)
                     nil)
                    (t
                     (%h2-with-scheduler-lock (connection)
                       (%h2-scheduler-remove-entry connection entry)))))
                (%h2-scheduler-close-goaway-retired-if-idle connection)
                (unless (or timeout-p peer-reset-p
                            (and entry (%h2-batch-entry-local-retired-p entry)))
                  (%h2-scheduler-fail-connection connection condition))
                (error condition)))))))
)

(defun send-http2-request-over-connection
    (connection request
     &key timeout deadline max-header-bytes max-fields max-body-bytes
       clock-function request-body-function request-body-length
       on-body-chunk on-information on-stream-open
       (collect-body-p t) (huffman-p nil))
  #+sbcl
  (%h2-send-http2-request-over-connection-scheduled
   connection request
   :timeout timeout
   :deadline deadline
   :max-header-bytes max-header-bytes
   :max-fields max-fields
   :max-body-bytes max-body-bytes
   :clock-function clock-function
   :request-body-function request-body-function
   :request-body-length request-body-length
   :on-body-chunk on-body-chunk
   :on-information on-information
   :on-stream-open on-stream-open
   :collect-body-p collect-body-p
   :huffman-p huffman-p)
  #-sbcl
  (%h2-send-http2-request-over-connection-serial
   connection request
   :timeout timeout
   :deadline deadline
   :max-header-bytes max-header-bytes
   :max-fields max-fields
   :max-body-bytes max-body-bytes
   :clock-function clock-function
   :request-body-function request-body-function
   :request-body-length request-body-length
   :on-body-chunk on-body-chunk
   :on-information on-information
   :on-stream-open on-stream-open
   :collect-body-p collect-body-p
   :huffman-p huffman-p))

(defun %h2-batch-list-length (value)
  (handler-case (list-length value)
    (type-error () nil)))

(defun %h2-batch-option-list (name value count)
  (let ((length (%h2-batch-list-length value)))
    (cond
      ((null value)
       (make-list count :initial-element nil))
      ((null length)
       (error 'http-kit:http-protocol-error
              :message "An HTTP/2 batch option must be a proper list."
              :operation :http2-client
              :detail (list name value)))
      ((/= length count)
       (error 'http-kit:http-protocol-error
              :message "An HTTP/2 batch option must be parallel to REQUESTS."
              :operation :http2-client
              :detail (list name length count)))
      (t value))))

(defun send-http2-requests-over-connection
    (connection requests
     &key timeout deadline max-header-bytes max-fields max-body-bytes clock-function
       request-body-functions request-body-lengths
       on-body-chunk (collect-body-p t) (huffman-p nil))
  "Send REQUESTS as multiplexed HTTP/2 streams.

The request headers are emitted for every stream before request bodies are
uploaded.  REQUEST-BODY-FUNCTIONS and REQUEST-BODY-LENGTHS, when supplied,
are proper lists parallel to REQUESTS; a non-NIL function produces octet
chunks and its matching length may be NIL for an unknown length.  NIL entries
use each request's in-memory body.  Responses may arrive in any order and are
returned in the same order as REQUESTS.  ON-BODY-CHUNK, when supplied,
receives a payload and the corresponding request.  This API is cooperative
rather than thread-safe: one caller owns a connection while this operation is
active.  A batch larger than the peer's advertised concurrent-stream limit is
sent in successive multiplexed waves under the same deadline."
  (unless (http2-connection-p connection)
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 batch request requires an HTTP2-CONNECTION."
           :operation :http2-client
           :detail (type-of connection)))
  (unless (http2-connection-open-p connection)
    (error 'http-kit:http-connection-error
           :message "The HTTP/2 connection is closed."
           :operation :http2-client
           :cause :closed))
  (let* ((request-count
           (or (and requests (%h2-batch-list-length requests))
               (error 'http-kit:http-protocol-error
                      :message "HTTP/2 batch requests must be a non-empty proper list."
                      :operation :http2-client
                      :detail requests)))
         (body-functions
           (%h2-batch-option-list :request-body-functions
                                  request-body-functions
                                  request-count))
         (body-lengths
           (%h2-batch-option-list :request-body-lengths
                                  request-body-lengths
                                  request-count)))
    (loop for request in requests
          for body-function in body-functions
          for body-length in body-lengths
          do (http-kit::%check-http-request request)
             (%h2-validate-request-body-options request
                                                body-function body-length))
    (when (and on-body-chunk (not (functionp on-body-chunk)))
      (error 'http-kit:http-protocol-error
             :message "ON-BODY-CHUNK must be a function or NIL."
             :operation :http2-client
             :detail on-body-chunk))
    (unless (member collect-body-p '(nil t))
      (error 'http-kit:http-protocol-error
             :message "COLLECT-BODY-P must be NIL or T."
             :operation :http2-client
             :detail collect-body-p))
    (let* ((clock (or clock-function
                      (%http2-connection-clock-function connection)))
           (header-limit (or max-header-bytes
                             (%http2-connection-max-header-bytes connection)))
           (field-limit (min (or max-fields most-positive-fixnum)
                             (%http2-connection-max-fields connection)))
           (body-limit (or max-body-bytes
                           (%http2-connection-max-body-bytes connection))))
      (http-kit:with-http-deadline (absolute-deadline timeout
                                     :inherited deadline
                                     :clock-function clock)
        (%h2-validate-limit :max-header-bytes header-limit)
        (%h2-validate-limit :max-fields field-limit)
        (%h2-validate-limit :max-body-bytes body-limit :allow-zero t)
        (http-kit::%with-http-error-translation
            ("The HTTP/2 batch request failed." :http2-connection)
          (handler-case
            (let* ((session-started-p
                     (%http2-connection-session-started-p connection))
                   (reader (%h2-reader-for
                            (%http2-connection-stream connection)))
                   (writer (%h2-writer (%http2-connection-stream connection)
                                       absolute-deadline clock))
                   (entries nil)
                   (header-writes nil))
              (when (and (find-if #'http-kit:http-request-protocol requests)
                         (not session-started-p))
                (%h2-connection-start-for-extended-connect
                 connection reader writer absolute-deadline clock)
                (setf session-started-p t))
              (when (and (find-if #'http-kit:http-request-protocol requests)
                         (not (%http2-connection-peer-enable-connect-protocol-p
                               connection)))
                (error 'http-kit:http-protocol-error
                       :message "The HTTP/2 peer did not enable extended CONNECT."
                       :operation :http2-write
                       :detail :protocol-error))
              (let ((concurrent-limit
                      (and session-started-p
                           (%http2-connection-peer-max-concurrent-streams
                            connection))))
                (when (eql concurrent-limit 0)
                  (setf concurrent-limit
                        (%h2-connection-wait-for-stream-capacity
                         connection reader writer absolute-deadline clock)))
                (when (and concurrent-limit
                           (> (length requests) concurrent-limit))
                  (return-from send-http2-requests-over-connection
                    (loop with request-count = (length requests)
                          for start from 0 below request-count
                            by concurrent-limit
                          for end = (min request-count
                                         (+ start concurrent-limit))
                          append
                          (send-http2-requests-over-connection
                           connection (subseq requests start end)
                           :deadline absolute-deadline
                           :max-header-bytes header-limit
                           :max-fields field-limit
                           :max-body-bytes body-limit
                           :clock-function clock
                           :request-body-functions
                           (subseq body-functions start end)
                           :request-body-lengths
                           (subseq body-lengths start end)
                           :on-body-chunk on-body-chunk
                           :collect-body-p collect-body-p
                           :huffman-p huffman-p)))))
              ;; Build every header block before writing any bytes.  A bad
              ;; request therefore cannot leave a half-emitted batch on the
              ;; connection.
              (loop for request in requests
                    for body-function in body-functions
                    for body-length in body-lengths
                    do (let ((stream-id
                               (%h2-connection-next-stream-id connection)))
                         (multiple-value-bind
                               (header-wire body outgoing-frame-size
                                expected-body-length trailer-fields)
                             (%h2-request-header-wire
                              request
                              (%http2-connection-max-frame-size connection)
                              header-limit body-limit
                              :stream-id stream-id
                              :include-session-p
                              (and (not session-started-p)
                                   (null entries))
                              :peer-max-frame-size
                              (%http2-connection-peer-max-frame-size
                               connection)
                              :peer-max-header-list-size
                              (%http2-connection-peer-max-header-list-size
                               connection)
                              :request-body-function body-function
                              :request-body-length body-length
                              :huffman-p huffman-p)
                           (push (%make-h2-batch-entry
                                  :request request
                                  :stream-id stream-id
                                  :body body
                                  :request-body-function body-function
                                  :expected-body-length expected-body-length
                                  :outgoing-frame-size outgoing-frame-size
                                  :trailer-fields trailer-fields)
                                 entries)
                          (push (list header-wire outgoing-frame-size stream-id)
                                 header-writes)
                           (incf (%http2-connection-next-stream-id connection)
                                 2))))
              (setf entries (nreverse entries)
                    header-writes (nreverse header-writes))
              (dolist (write header-writes)
                (%h2-write-wire (%http2-connection-stream connection)
                                (first write) absolute-deadline clock))
              (let ((pending-head nil)
                    (pending-tail nil)
                    (first-settings-p (not session-started-p))
                    (remaining (length entries)))
                (labels
                    ((enqueue (frame)
                       (let ((cell (list frame)))
                         (if pending-tail
                             (setf (cdr pending-tail) cell
                                   pending-tail cell)
                             (setf pending-head cell
                                   pending-tail cell))))
                     (dequeue ()
                       (when pending-head
                         (let ((frame (car pending-head)))
                           (setf pending-head (cdr pending-head))
                           (unless pending-head
                             (setf pending-tail nil))
                           frame)))
                     (read-network-frame ()
                       (let ((frame (%h2-read-frame
                                     reader
                                     (%http2-connection-max-frame-size connection)
                                     absolute-deadline clock)))
                         (when (eq frame :eof)
                           (error 'http-kit:http-connection-error
                                  :message "The HTTP/2 peer closed during a batch request."
                                  :operation :http2-read
                                  :cause :eof))
                         (when first-settings-p
                           (unless (and (= (%h2-frame-type frame)
                                           +http2-settings-type+)
                                        (zerop (%h2-frame-stream-id frame))
                                        (zerop (logand (%h2-frame-flags frame)
                                                       +http2-ack-flag+)))
                             (error 'http-kit:http-protocol-error
                                    :message "The first HTTP/2 peer frame must be a non-ACK SETTINGS frame."
                                    :operation :http2-read
                                    :detail (list (%h2-frame-type frame)
                                                  (%h2-frame-stream-id frame)
                                                  (%h2-frame-flags frame))))
                           (setf first-settings-p nil))
                         frame))
                     (next-frame ()
                       (or (dequeue) (read-network-frame)))
                     (control-expected-stream-id (stream-id)
                       (if (plusp stream-id)
                           stream-id
                           (or (and entries
                                    (%h2-batch-entry-stream-id
                                     (first entries)))
                               1)))
                     (handle-control (frame)
                       (let ((type (%h2-frame-type frame))
                             (stream-id (%h2-frame-stream-id frame))
                             (payload (%h2-frame-payload frame)))
                         (cond
                           ((= type +http2-settings-type+)
                            (%h2-connection-note-settings connection frame writer))
                           ((= type +http2-window-update-type+)
                            (%h2-connection-note-window-update
                             connection frame
                             (control-expected-stream-id stream-id))
                            (%h2-handle-control-frame
                             frame writer
                             (control-expected-stream-id stream-id)))
                           ((= type +http2-goaway-type+)
                            (unless (and (zerop stream-id)
                                         (>= (length payload) 8))
                              (error 'http-kit:http-protocol-error
                                     :message "An HTTP/2 GOAWAY frame is invalid."
                                     :operation :http2-control
                                     :detail (list stream-id (length payload))))
                            (let ((last-stream-id (%h2-u32 payload 0)))
                              (when (/= 0 (logand last-stream-id #x80000000))
                                (error 'http-kit:http-protocol-error
                                       :message "An HTTP/2 GOAWAY last-stream identifier is invalid."
                                       :operation :http2-control
                                       :detail last-stream-id))
                              (setf (%http2-connection-goaway-last-stream-id
                                     connection)
                                    last-stream-id)
                              (when (some
                                     (lambda (entry)
                                       (> (%h2-batch-entry-stream-id entry)
                                          last-stream-id))
                                     entries)
                                (error 'http-kit:http-connection-error
                                       :message "The HTTP/2 peer rejected a batch stream with GOAWAY."
                                       :operation :http2-control
                                       :cause (list :goaway last-stream-id
                                                    (%h2-u32 payload 4))))))
                           ((= type +http2-rst-stream-type+)
                            (unless (and (plusp stream-id)
                                         (= (length payload) 4))
                              (error 'http-kit:http-protocol-error
                                     :message "An HTTP/2 RST_STREAM frame is invalid."
                                     :operation :http2-control
                                     :detail (list stream-id (length payload))))
                            (if (%h2-batch-entry-for-stream entries stream-id)
                                (error 'http-kit:http-connection-error
                                       :message "The HTTP/2 peer reset a batch response stream."
                                       :operation :http2-control
                                       :cause (list :rst-stream stream-id
                                                    (%h2-u32 payload 0)))
                                (%h2-handle-control-frame
                                 frame writer
                                 (control-expected-stream-id stream-id))))
                           (t
                            (%h2-handle-control-frame
                             frame writer
                             (control-expected-stream-id stream-id))))))
                     (send-entry-body-with-producer (entry)
                       (multiple-value-bind (pending-frames new-first-settings-p)
                           (%h2-connection-send-request-body
                            connection reader writer
                            (%h2-batch-entry-stream-id entry)
                            (%h2-batch-entry-body entry)
                            (%h2-batch-entry-request-body-function entry)
                            (%h2-batch-entry-expected-body-length entry)
                            body-limit
                            (%h2-batch-entry-outgoing-frame-size entry)
                            (%h2-batch-entry-trailer-fields entry)
                            absolute-deadline clock first-settings-p
                            huffman-p)
                         (dolist (frame pending-frames)
                           (enqueue frame))
                         (setf first-settings-p new-first-settings-p)))
                     (mark-complete (entry response)
                       (setf (%h2-batch-entry-response entry) response)
                       (decf remaining))
                  )
                  (dolist (entry entries)
                    (send-entry-body-with-producer entry))
                  (loop while (plusp remaining)
                        do (let ((frame (next-frame)))
                             (cond
                               ((%h2-control-frame-p (%h2-frame-type frame))
                                (handle-control frame))
                               ((= (%h2-frame-type frame)
                                   +http2-headers-type+)
                                (let ((entry
                                        (%h2-batch-entry-for-stream
                                         entries (%h2-frame-stream-id frame))))
                                  (unless entry
                                    (error 'http-kit:http-protocol-error
                                           :message (format nil
                                                            "The HTTP/2 batch received HEADERS for unknown stream ~D."
                                                            (%h2-frame-stream-id frame))
                                           :operation :http2-read
                                           :detail (%h2-frame-stream-id frame)))
                                  (when (%h2-batch-entry-response entry)
                                    (error 'http-kit:http-protocol-error
                                           :message "The HTTP/2 batch received HEADERS after stream completion."
                                           :operation :http2-read
                                           :detail (%h2-frame-stream-id frame)))
                                  (multiple-value-bind
                                        (status headers response)
                                      (%h2-batch-process-headers-frame
                                       entry frame
                                       (lambda ()
                                         (next-frame))
                                       (%http2-connection-max-frame-size
                                        connection)
                                       header-limit
                                       field-limit
                                       (%http2-connection-hpack-context
                                        connection)
                                       (http-kit:http-request-method
                                        (%h2-batch-entry-request entry)))
                                    (when status
                                      (setf (%h2-batch-entry-status entry)
                                            status
                                            (%h2-batch-entry-headers entry)
                                            headers))
                                    (when response
                                      (mark-complete entry response)))))
                               ((= (%h2-frame-type frame)
                                   +http2-data-type+)
                                (let* ((stream-id (%h2-frame-stream-id frame))
                                       (entry (%h2-batch-entry-for-stream
                                               entries stream-id)))
                                  (unless entry
                                    (error 'http-kit:http-protocol-error
                                           :message (format nil
                                                            "The HTTP/2 batch received DATA for unknown stream ~D."
                                                            stream-id)
                                           :operation :http2-read
                                           :detail stream-id))
                                  (when (%h2-batch-entry-response entry)
                                    (error 'http-kit:http-protocol-error
                                           :message "The HTTP/2 batch received DATA after stream completion."
                                           :operation :http2-read
                                           :detail stream-id))
                                  (multiple-value-bind (end-stream-p new-body-length
                                                        payload-length)
                                      (%h2-append-data-frame*
                                       frame
                                       (%h2-batch-entry-status entry)
                                       (%h2-batch-entry-body-vector entry)
                                       (%h2-batch-entry-body-length entry)
                                       (http-kit:http-request-method
                                        (%h2-batch-entry-request entry))
                                       body-limit
                                       (and on-body-chunk
                                            (lambda (payload)
                                              (funcall
                                               on-body-chunk payload
                                               (%h2-batch-entry-request entry))))
                                       collect-body-p stream-id)
                                    (setf (%h2-batch-entry-body-length entry)
                                          new-body-length)
                                    (unless end-stream-p
                                      (%h2-send-window-update writer stream-id
                                                              payload-length))
                                    (%h2-send-window-update writer 0
                                                            payload-length)
                                    (when end-stream-p
                                      (mark-complete
                                       entry
                                       (%h2-finish-request-response
                                        (%h2-batch-entry-status entry)
                                        (%h2-batch-entry-headers entry)
                                        nil
                                        (%h2-batch-entry-body-vector entry)
                                        (http-kit:http-request-method
                                         (%h2-batch-entry-request entry))
                                        :body-length new-body-length))))))
                               ((= (%h2-frame-type frame)
                                   +http2-continuation-type+)
                                (error 'http-kit:http-protocol-error
                                       :message "An HTTP/2 CONTINUATION arrived without HEADERS."
                                       :operation :http2-headers
                                       :detail (%h2-frame-stream-id frame)))
                               ((= (%h2-frame-type frame)
                                   +http2-push-promise-type+)
                                (error 'http-kit:http-protocol-error
                                       :message "The peer sent HTTP/2 PUSH_PROMISE after server push was disabled."
                                       :operation :http2-read
                                       :detail :protocol-error))
                               (t nil)))))
                  (setf (%http2-connection-session-started-p connection) t)
                  (mapcar #'%h2-batch-entry-response entries)))
            (error (condition)
              (let ((stream-reset-p
                      (and (%h2-peer-stream-reset-condition-p condition)
                           (eq (first
                                (http-kit:http-connection-error-cause
                                 condition))
                               :rst-stream))))
                (unless stream-reset-p
                  (close-http2-connection connection)))
              (error condition))))))))

(defun send-http2-requests-over-connection/cps
    (connection requests on-success
     &key on-error timeout deadline max-header-bytes max-fields max-body-bytes
       clock-function request-body-functions request-body-lengths
       on-body-chunk (collect-body-p t) (huffman-p nil))
  "Send a concurrent HTTP/2 batch using CPS continuations."
  (http-kit::%call-http-operation/cps
   (lambda ()
     (send-http2-requests-over-connection
      connection requests
      :timeout timeout
      :deadline deadline
      :max-header-bytes max-header-bytes
      :max-fields max-fields
      :max-body-bytes max-body-bytes
      :clock-function clock-function
      :request-body-functions request-body-functions
      :request-body-lengths request-body-lengths
      :huffman-p huffman-p
      :on-body-chunk on-body-chunk
      :collect-body-p collect-body-p))
   on-success
   :on-error on-error))
