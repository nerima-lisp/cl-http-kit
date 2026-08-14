(in-package #:http-kit/http2)

(defun http2-client-connection (client)
  (and (http2-client-p client)
       (%http2-connection client)))

(defun make-http2-connection
    (&key stream close-stream
          (max-frame-size +http2-default-max-frame-size+)
          (max-header-bytes http-kit::*default-max-header-bytes*)
          (max-body-bytes http-kit::*default-max-body-bytes*)
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
  (%h2-validate-limit :max-body-bytes max-body-bytes :allow-zero t)
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
   :max-body-bytes max-body-bytes
   :clock-function clock-function
   :hpack-context
   (%make-hpack-context
    :max-size +hpack-default-table-size+
    :maximum-size +hpack-default-table-size+)))

(defun close-http2-connection (connection)
  "Close CONNECTION idempotently and mark it unusable for future requests."
  (unless (http2-connection-p connection)
    (error 'http-kit:http-protocol-error
           :message "CLOSE-HTTP2-CONNECTION requires an HTTP2-CONNECTION."
           :operation :http2-client
           :detail (type-of connection)))
  (unless (%http2-connection-closed-p connection)
    (setf (%http2-connection-closed-p connection) t)
    (http-kit::%with-http-cleanup
      (funcall (%http2-connection-close-stream connection)
               (%http2-connection-stream connection))))
  connection)

(defun %h2-connection-note-peer-settings
    (connection max-frame-size max-table-size initial-window-size)
  (when max-frame-size
    (setf (%http2-connection-peer-max-frame-size connection)
          max-frame-size))
  (when max-table-size
    (setf (%http2-connection-peer-max-table-size connection)
          max-table-size))
  (when initial-window-size
    (let ((delta (- initial-window-size
                    (%http2-connection-peer-initial-window-size connection))))
      (dolist (entry (%http2-connection-peer-stream-windows connection))
        (incf (cdr entry) delta))
      (setf (%http2-connection-peer-initial-window-size connection)
            initial-window-size))))

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
                huffman-p)
  (let ((position 0)
        (pending-frames '())
        (first-settings-p read-initial-settings-p)
        (body-length (length body)))
    (labels ((read-while-blocked ()
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
                     (push frame pending-frames))))
             (write-data (payload end-stream-p)
               (let ((size (length payload)))
                 (%h2-write-wire
                  (%http2-connection-stream connection)
                  (%h2-frame-wire
                   +http2-data-type+
                   (if end-stream-p +http2-end-stream-flag+ 0)
                   stream-id payload)
                  deadline clock-function)
                 (when (plusp size)
                   (decf (%http2-connection-peer-connection-window-size
                          connection)
                         size)
                   (let ((entry (assoc stream-id
                                       (%http2-connection-peer-stream-windows
                                        connection))))
                     (decf (cdr entry) size))
                   (incf position size))))
             (effective-max-frame-size ()
               (min local-max-frame-size
                    (%http2-connection-peer-max-frame-size connection)))
             (write-trailers ()
               (when trailer-fields
                 (%h2-write-wire
                  (%http2-connection-stream connection)
                  (%h2-concat
                   (%h2-header-frames
                    (%hpack-encode-block trailer-fields
                                          :huffman-p huffman-p)
                    t (effective-max-frame-size) stream-id))
                  deadline clock-function))))
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
                ((null pending-chunk)
                 (error 'http-kit:http-protocol-error
                        :message "The HTTP/2 request body producer did not provide a chunk or NIL."
                        :operation :http2-write
                        :detail pending-chunk))
                (t
                 (let* ((max-frame-size (effective-max-frame-size))
                        (stream-window
                          (%h2-connection-stream-window
                           connection stream-id))
                        (connection-window
                          (%http2-connection-peer-connection-window-size
                           connection))
                        (available
                          (min max-frame-size stream-window connection-window))
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
                         (write-data
                          (subseq pending-chunk
                                  pending-offset (+ pending-offset size))
                          end-stream-p)
                         (incf pending-offset size)
                         (when (= pending-offset (length pending-chunk))
                           (setf pending-chunk nil
                                 pending-offset 0))
                         (when end-stream-p
                           (return)))
                       (read-while-blocked)))))))
          (loop while (< position body-length)
                do (let* ((max-frame-size (effective-max-frame-size))
                          (stream-window
                            (%h2-connection-stream-window
                             connection stream-id))
                          (connection-window
                            (%http2-connection-peer-connection-window-size
                             connection))
                          (size (min max-frame-size
                                     (- body-length position)
                                     stream-window
                                     connection-window)))
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
          (when (or (= stream-id expected-stream-id)
                    (> expected-stream-id 1))
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
                  (push (cons stream-id
                              (+ (%http2-connection-peer-initial-window-size
                                  connection)
                                 increment))
                        (%http2-connection-peer-stream-windows connection)))))))))

(defun %h2-connection-note-settings (connection frame writer)
  (multiple-value-bind (max-frame-size max-table-size initial-window-size)
      (%h2-settings (%h2-frame-payload frame))
    (%h2-connection-note-peer-settings
     connection max-frame-size max-table-size initial-window-size))
  (%h2-validate-settings-frame frame writer))

(defun %h2-connection-control-handler (connection frame writer expected-stream-id)
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
      (when (and last-stream-id (> stream-id last-stream-id))
        (error 'http-kit:http-connection-error
               :message "The HTTP/2 peer has closed this connection with GOAWAY."
               :operation :http2-write
               :cause (list :goaway last-stream-id stream-id))))
    stream-id))

(defun send-http2-request-over-connection
    (connection request
     &key timeout deadline max-header-bytes max-body-bytes clock-function
       request-body-function request-body-length
       on-body-chunk (collect-body-p t) (huffman-p nil))
  "Send REQUEST on CONNECTION and retain the connection for later streams.

Requests on one connection are serialized by this API.  A caller that needs
concurrent streams should provide its own scheduler and use separate
HTTP2-CONNECTION objects until a stream multiplexer is installed."
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
  (unless (member collect-body-p '(nil t))
    (error 'http-kit:http-protocol-error
           :message "COLLECT-BODY-P must be NIL or T."
           :operation :http2-client
           :detail collect-body-p))
  (let* ((clock (or clock-function
                    (%http2-connection-clock-function connection)))
         (header-limit (or max-header-bytes
                           (%http2-connection-max-header-bytes connection)))
         (body-limit (or max-body-bytes
                         (%http2-connection-max-body-bytes connection)))
         (stream-id nil))
    (http-kit:with-http-deadline (absolute-deadline timeout
                                   :inherited deadline
                                   :clock-function clock)
      (%h2-validate-limit :max-header-bytes header-limit)
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
                   :request-body-function request-body-function
                   :request-body-length request-body-length
                   :huffman-p huffman-p)
                (%h2-write-wire
                 (%http2-connection-stream connection)
                 header-wire absolute-deadline clock)
                (incf (%http2-connection-next-stream-id connection) 2)
                (multiple-value-bind (pending-frames read-initial-settings-p)
                    (%h2-connection-send-request-body
                     connection reader writer stream-id body
                     request-body-function expected-body-length
                     body-limit outgoing-frame-size
                     trailer-fields
                     absolute-deadline clock (not session-started-p)
                     huffman-p)
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
                           :read-initial-settings-p read-initial-settings-p
                           :peer-max-frame-size
                           (%http2-connection-peer-max-frame-size connection)
                           :initial-frames pending-frames
                           :on-peer-settings
                           (lambda (peer-frame-size peer-table-size
                                    peer-window-size)
                             (%h2-connection-note-peer-settings
                              connection peer-frame-size peer-table-size
                              peer-window-size))
                           :control-handler
                           (lambda (frame response-writer expected-id)
                             (%h2-connection-control-handler
                              connection frame response-writer expected-id)))))
                    (setf (%http2-connection-session-started-p connection) t)
                    response))))
          (error (condition)
            (close-http2-connection connection)
            (error condition)))))))

(defun send-http2-request-over-connection/cps
    (connection request on-success
     &key on-error timeout deadline max-header-bytes max-body-bytes
       clock-function request-body-function request-body-length
       on-body-chunk (collect-body-p t) (huffman-p nil))
  "Send a request on a reusable HTTP/2 connection using CPS continuations."
  (http-kit::%call-http-operation/cps
   (lambda ()
     (send-http2-request-over-connection
      connection request
      :timeout timeout
      :deadline deadline
      :max-header-bytes max-header-bytes
      :max-body-bytes max-body-bytes
      :clock-function clock-function
      :request-body-function request-body-function
      :request-body-length request-body-length
      :huffman-p huffman-p
      :on-body-chunk on-body-chunk
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
           &key timeout deadline max-header-bytes max-body-bytes clock-function
             request-body-function request-body-length
             on-body-chunk (collect-body-p t) (huffman-p nil)
             &allow-other-keys)
    (send-http2-request-over-connection
     connection request
     :timeout timeout
     :deadline deadline
     :max-header-bytes max-header-bytes
     :max-body-bytes max-body-bytes
     :clock-function clock-function
     :request-body-function request-body-function
     :request-body-length request-body-length
     :huffman-p huffman-p
     :on-body-chunk on-body-chunk
     :collect-body-p collect-body-p)))

(defstruct (%h2-batch-entry
             (:constructor %make-h2-batch-entry
                 (&key request stream-id body request-body-function
                       expected-body-length outgoing-frame-size trailer-fields)))
  request
  stream-id
  body
  request-body-function
  expected-body-length
  outgoing-frame-size
  trailer-fields
  (body-position 0)
  status
  headers
  (body-vector (make-array 0
                           :element-type '(unsigned-byte 8)
                           :adjustable t
                           :fill-pointer 0))
  (body-length 0)
  response)
