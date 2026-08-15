(in-package #:http-kit/http3)

(defun %h3-transport-error (message &optional detail)
  (error 'http-protocol-error
         :message message
         :operation :http3-transport
         :detail detail))

(defun %h3-invalid-header (name reason)
  (error 'http-invalid-header
         :message (format nil "Invalid HTTP/3 header ~S: ~A." name reason)
         :operation :http3-header
         :name name
         :reason reason))

(defun %h3-size-error (kind limit observed)
  (error 'http-size-limit-exceeded
         :message (format nil "HTTP/3 ~A exceeds the configured limit." kind)
         :operation :http3-transport
         :limit limit
         :observed observed
         :kind kind))

(defun %h3-positive-limit-p (value)
  (and (integerp value) (plusp value) (<= value +http3-max-varint+)))

(defun %h3-non-negative-limit-p (value)
  (and (integerp value) (>= value 0)))

(defun %h3-copy-settings (settings)
  (unless (listp settings)
    (%h3-transport-error "HTTP/3 additional settings must be an alist." settings))
  (mapcar
   (lambda (setting)
     (unless (and (consp setting)
                  (integerp (car setting))
                  (<= 0 (car setting) +http3-max-varint+)
                  (integerp (cdr setting))
                  (<= 0 (cdr setting) +http3-max-varint+))
       (%h3-transport-error
        "HTTP/3 additional settings must be (integer . non-negative-integer) pairs."
        setting))
     (cons (car setting) (cdr setting)))
   settings))

(defstruct (http3-client (:constructor %make-http3-client))
  open-stream
  write-stream
  read-stream
  (prefetched-stream-inputs '())
  close-stream
  cancel-stream
  control-stream
  qpack-encoder-stream
  qpack-decoder-stream
  peer-control-stream
  peer-control-state
  (peer-control-buffer #() :type vector)
  (peer-control-prefix-seen-p nil :type boolean)
  (peer-control-fin-p nil :type boolean)
  peer-qpack-encoder-stream
  (peer-qpack-encoder-buffer #() :type vector)
  (peer-qpack-encoder-prefix-seen-p nil :type boolean)
  (peer-qpack-encoder-fin-p nil :type boolean)
  peer-qpack-decoder-stream
  (peer-qpack-decoder-buffer #() :type vector)
  (peer-qpack-decoder-prefix-seen-p nil :type boolean)
  (peer-qpack-decoder-fin-p nil :type boolean)
  qpack-encoder-table
  qpack-decoder-table
  qpack-decoder-context
  qpack-decoder-state
  (serialize (lambda (thunk) (funcall thunk)) :type function)
  (max-frame-size #x4000 :type integer)
  (max-header-bytes 65536 :type integer)
  (max-fields 256 :type integer)
  max-push-id
  (promised-push-ids '())
  (push-promises '())
  (cancelled-push-ids '())
  (consumed-push-ids '())
  (qpack-settings nil)
  (open-p t))

(defun make-http3-client
    (&key open-stream write-stream read-stream close-stream cancel-stream
          (serialize (lambda (thunk) (funcall thunk)))
          (max-frame-size #x4000) (max-header-bytes 65536) (max-fields 256)
          (qpack-settings '()) max-push-id peer-control-stream timeout deadline)
  "Open an HTTP/3 client over caller-supplied QUIC stream callbacks.

OPEN-STREAM is called as (REQUEST &KEY STREAM-TYPE TIMEOUT DEADLINE) and
returns an opaque stream object plus, for request streams, an optional numeric
QUIC stream ID.  WRITE-STREAM is called as
(STREAM OCTETS &KEY FIN-P TIMEOUT DEADLINE), while READ-STREAM is called as
(STREAM &KEY TIMEOUT DEADLINE) and returns an octet vector, or NIL at QUIC
stream FIN.  CLOSE-STREAM is called as (STREAM &KEY CONDITION), and optional
CANCEL-STREAM as (STREAM &KEY ERROR-CODE TIMEOUT DEADLINE).  The
callbacks own QUIC, TLS, ALPN, socket, and event-loop behavior; this system
only supplies the HTTP/3 stream and codec layer."
  (dolist (callback (list open-stream write-stream read-stream))
    (unless (functionp callback)
      (%h3-transport-error "HTTP/3 open, write, and read callbacks are required."
                           (type-of callback))))
  (unless (or (null close-stream) (functionp close-stream))
    (%h3-transport-error "HTTP/3 close-stream must be a function or NIL."
                         (type-of close-stream)))
  (unless (or (null cancel-stream) (functionp cancel-stream))
    (%h3-transport-error "HTTP/3 cancel-stream must be a function or NIL."
                         (type-of cancel-stream)))
  (unless (functionp serialize)
    (%h3-transport-error "HTTP/3 serialize must be a function."
                         (type-of serialize)))
  (unless (%h3-positive-limit-p max-frame-size)
    (%h3-transport-error "HTTP/3 max-frame-size must be a positive QUIC varint."
                         max-frame-size))
  (unless (%h3-positive-limit-p max-header-bytes)
    (%h3-transport-error "HTTP/3 max-header-bytes must be a positive integer."
                         max-header-bytes))
  (unless (%h3-positive-limit-p max-fields)
    (%h3-transport-error "HTTP/3 max-fields must be a positive integer."
                         max-fields))
  (unless (or (null max-push-id)
              (and (%h3-non-negative-limit-p max-push-id)
                   (<= max-push-id +http3-max-varint+)))
    (%h3-transport-error
     "HTTP/3 max-push-id must be NIL or a non-negative QUIC varint."
     max-push-id))
  (let* ((settings (%h3-copy-settings qpack-settings))
         (closer (or close-stream
                     (lambda (stream &key condition)
                       (declare (ignore stream condition))
                       nil)))
         (control-stream nil)
         (qpack-encoder-stream nil)
         (qpack-decoder-stream nil))
    (handler-case
        (progn
          (setf control-stream
                (funcall open-stream nil
                         :stream-type :control
                         :timeout timeout
                         :deadline deadline))
          (unless control-stream
            (%h3-transport-error
             "The HTTP/3 open-stream callback returned NIL for the control stream."))
          (setf qpack-encoder-stream
                (funcall open-stream nil
                         :stream-type :qpack-encoder
                         :timeout timeout
                         :deadline deadline))
          (unless qpack-encoder-stream
            (%h3-transport-error
             "The HTTP/3 open-stream callback returned NIL for the QPACK encoder stream."))
          (setf qpack-decoder-stream
                (funcall open-stream nil
                         :stream-type :qpack-decoder
                         :timeout timeout
                         :deadline deadline))
          (unless qpack-decoder-stream
            (%h3-transport-error
             "The HTTP/3 open-stream callback returned NIL for the QPACK decoder stream."))
          (let ((settings-frame
                  (make-http3-settings-frame
                   :qpack-max-table-capacity
                   (or (cdr (assoc +http3-setting-qpack-max-table-capacity+
                                   settings))
                       0)
                   :qpack-blocked-streams
                   (or (cdr (assoc +http3-setting-qpack-blocked-streams+
                                   settings))
                       0)
                   :extra-settings
                   (remove-if
                    (lambda (setting)
                      (member (car setting)
                              (list +http3-setting-qpack-max-table-capacity+
                                    +http3-setting-qpack-blocked-streams+)))
                    settings))))
            (funcall write-stream control-stream
                     (apply #'%http3-concatenate-octets
                            (append
                             (list (http3-control-stream-prefix)
                                   (encode-http3-frame settings-frame))
                             (when max-push-id
                               (list
                                (encode-http3-frame
                                 (make-http3-frame
                                  :type +http3-max-push-id-type+
                                  :payload (http3-varint-encode max-push-id)))))))
                     :fin-p nil
                     :timeout timeout
                     :deadline deadline))
          (funcall write-stream qpack-encoder-stream
                   (http3-qpack-encoder-stream-prefix)
                   :fin-p nil :timeout timeout :deadline deadline)
          (funcall write-stream qpack-decoder-stream
                   (http3-qpack-decoder-stream-prefix)
                   :fin-p nil :timeout timeout :deadline deadline)
          (let* ((decoder-capacity
                   (or (cdr (assoc +http3-setting-qpack-max-table-capacity+
                                   settings))
                       0))
                 (blocked-stream-limit
                   (or (cdr (assoc +http3-setting-qpack-blocked-streams+
                                   settings))
                       0))
                 (encoder-table
                   (make-qpack-dynamic-table :max-capacity 0 :capacity 0))
                 (decoder-table
                   (make-qpack-dynamic-table
                    :max-capacity decoder-capacity :capacity 0)))
          (%make-http3-client
           :open-stream open-stream
           :write-stream write-stream
           :read-stream read-stream
           :close-stream closer
           :cancel-stream cancel-stream
           :control-stream control-stream
           :qpack-encoder-stream qpack-encoder-stream
           :qpack-decoder-stream qpack-decoder-stream
           :peer-control-stream peer-control-stream
           :peer-control-state (make-http3-control-state :peer-role :server)
           :peer-control-buffer (make-array 0 :element-type '(unsigned-byte 8))
           :peer-control-prefix-seen-p nil
           :peer-control-fin-p nil
           :peer-qpack-encoder-buffer (make-array 0 :element-type '(unsigned-byte 8))
           :peer-qpack-decoder-buffer (make-array 0 :element-type '(unsigned-byte 8))
           :qpack-encoder-table encoder-table
           :qpack-decoder-table decoder-table
           :qpack-decoder-context
           (make-http3-qpack-decoder-context
            decoder-table :blocked-stream-limit blocked-stream-limit)
           :qpack-decoder-state
           (make-qpack-decoder-stream-state encoder-table)
           :serialize serialize
           :max-frame-size max-frame-size
           :max-header-bytes max-header-bytes
           :max-fields max-fields
           :max-push-id max-push-id
           :promised-push-ids '()
           :push-promises '()
           :cancelled-push-ids '()
           :consumed-push-ids '()
           :qpack-settings settings
           :open-p t)))
      (error (condition)
        (dolist (stream (remove nil (list control-stream
                                          qpack-encoder-stream
                                          qpack-decoder-stream)))
          (http-kit::%with-http-cleanup
            (funcall closer stream :condition condition)))
        (error condition)))))

(defun %call-http3-serialized (client thunk)
  (funcall (http3-client-serialize client) thunk))

(defun %h3-read-client-stream (client stream timeout deadline)
  (let ((prefetched
          (%call-http3-serialized
           client
           (lambda ()
             (let ((entry (assoc stream
                                 (http3-client-prefetched-stream-inputs client))))
               (when entry
                 (setf (http3-client-prefetched-stream-inputs client)
                       (remove stream
                               (http3-client-prefetched-stream-inputs client)
                               :key #'car :test #'eql :count 1))
                 (cdr entry)))))))
    (if prefetched
        (values (first prefetched) (second prefetched))
        (funcall (http3-client-read-stream client)
                 stream :timeout timeout :deadline deadline))))

(defun %h3-prefetch-stream-input (client stream chunk fin-p)
  (%call-http3-serialized
   client
   (lambda ()
     (push (cons stream (list chunk fin-p))
           (http3-client-prefetched-stream-inputs client)))))

(defun accept-http3-peer-unidirectional-stream
    (client stream &key timeout deadline)
  "Classify and attach one peer-created HTTP/3 unidirectional STREAM.

The stream-type prefix may be fragmented across reads.  The consumed prefix
is replayed to the existing control, QPACK, or push reader, so callers can
continue with READ-HTTP3-CONTROL-STREAM, the corresponding QPACK reader, or
RECEIVE-HTTP3-PUSH without losing bytes.  Returns :CONTROL, :QPACK-ENCODER,
:QPACK-DECODER, :PUSH, or :UNKNOWN.  Unknown stream types are not errors; the
surrounding QUIC transport remains responsible for discarding their contents."
  (unless (and (http3-client-p client) (http3-client-open-p client))
    (%h3-transport-error "A live HTTP/3 client is required."))
  (unless stream
    (%h3-transport-error
     "ACCEPT-HTTP3-PEER-UNIDIRECTIONAL-STREAM requires a stream."))
  (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)))
        (prefetched-fin-p nil))
    (loop
      (multiple-value-bind (stream-type position)
          (http3-varint-decode buffer :allow-incomplete-p t)
        (declare (ignore position))
        (when stream-type
          (let ((kind
                  (cond
                    ((= stream-type +http3-control-stream-type+)
                     (attach-http3-peer-control-stream client stream)
                     :control)
                    ((= stream-type +http3-push-stream-type+) :push)
                    ((= stream-type +http3-qpack-encoder-stream-type+)
                     (attach-http3-peer-qpack-encoder-stream client stream)
                     :qpack-encoder)
                    ((= stream-type +http3-qpack-decoder-stream-type+)
                     (attach-http3-peer-qpack-decoder-stream client stream)
                     :qpack-decoder)
                    (t :unknown))))
            (unless (eq kind :unknown)
              (%h3-prefetch-stream-input client stream buffer prefetched-fin-p))
            (return kind))))
      (multiple-value-bind (chunk fin-p)
          (funcall (http3-client-read-stream client)
                   stream :timeout timeout :deadline deadline)
        (when chunk
          (unless (%http3-octet-vector-p chunk)
            (%h3-transport-error
             "HTTP/3 read-stream must return an octet vector or NIL."
             (type-of chunk)))
          (setf buffer (%http3-concatenate-octets buffer chunk)))
        (setf prefetched-fin-p fin-p)
        (when (or fin-p (null chunk))
          (multiple-value-bind (stream-type position)
              (http3-varint-decode buffer :allow-incomplete-p t)
            (declare (ignore position))
            (unless stream-type
              (%h3-transport-error
               "A peer HTTP/3 unidirectional stream ended before its type prefix."
               :h3-stream-creation-error))))))))

(defun process-http3-peer-unidirectional-stream
    (client stream &key timeout deadline)
  "Classify STREAM and apply its first control or QPACK input to CLIENT.

Returns the stream kind, a list of effects, and whether the processed input
ended.  :PUSH and :UNKNOWN streams are classified but not consumed further;
the caller must pass push streams to RECEIVE-HTTP3-PUSH and discard unknown
extension streams.  Call the corresponding READ-HTTP3-* function when a
critical stream becomes readable again."
  (let ((kind
          (accept-http3-peer-unidirectional-stream
           client stream :timeout timeout :deadline deadline)))
    (case kind
      (:control
       (multiple-value-bind (events ended-p)
           (read-http3-control-stream
            client :timeout timeout :deadline deadline)
         (values kind events ended-p)))
      (:qpack-encoder
       (multiple-value-bind (events ended-p)
           (read-http3-qpack-encoder-stream
            client :timeout timeout :deadline deadline)
         (values kind events ended-p)))
      (:qpack-decoder
       (multiple-value-bind (events ended-p)
           (read-http3-qpack-decoder-stream
            client :timeout timeout :deadline deadline)
         (values kind events ended-p)))
      (otherwise
       (values kind '() nil)))))

(defun cancel-http3-stream
    (client stream &key (error-code #x10c) timeout deadline)
  "Cancel STREAM through the QUIC backend using H3_REQUEST_CANCELLED by default."
  (unless (http3-client-p client)
    (%h3-transport-error "CANCEL-HTTP3-STREAM requires an HTTP/3 client."
                         (type-of client)))
  (unless (http3-client-open-p client)
    (%h3-transport-error "The HTTP/3 client is already closed."))
  (unless stream
    (%h3-transport-error "CANCEL-HTTP3-STREAM requires a stream." stream))
  (unless (and (integerp error-code) (<= 0 error-code +http3-max-varint+))
    (%h3-transport-error "HTTP/3 cancellation error-code must be a QUIC varint."
                         error-code))
  (let ((cancel (http3-client-cancel-stream client)))
    (unless cancel
      (%h3-transport-error
       "The HTTP/3 QUIC backend does not provide stream cancellation."
       :unsupported))
    (funcall cancel stream :error-code error-code
             :timeout timeout :deadline deadline))
  t)

(defun send-http3-priority-update
    (client element-id
     &key (kind :request)
       (priority-field-value nil priority-field-value-p)
       (urgency 3) incremental timeout deadline)
  "Send an RFC 9218 PRIORITY_UPDATE on CLIENT's control stream."
  (unless (http3-client-p client)
    (%h3-transport-error "SEND-HTTP3-PRIORITY-UPDATE requires an HTTP/3 client."
                         (type-of client)))
  (unless (http3-client-open-p client)
    (%h3-transport-error "The HTTP/3 client is already closed."))
  (unless (and (integerp element-id) (<= 0 element-id +http3-max-varint+))
    (%h3-transport-error "An HTTP/3 priority element ID must be a QUIC varint."
                         element-id))
  (unless (eq kind :request)
    (%h3-transport-error
     "Push PRIORITY_UPDATE requires HTTP/3 server-push support."
     :unsupported))
  (unless (zerop (mod element-id 4))
    (%h3-transport-error
     "An HTTP/3 request priority target must be a client-initiated bidirectional stream."
     element-id))
  (let* ((value (if priority-field-value-p
                    priority-field-value
                    (http-kit:format-http-priority-field-value
                     :urgency urgency :incremental incremental)))
         (frame (make-http3-priority-update-frame
                 :element-id element-id
                 :priority-field-value value
                 :kind kind)))
    (when (> (length (http3-frame-payload frame))
             (http3-client-max-frame-size client))
      (%h3-transport-error
       "The HTTP/3 PRIORITY_UPDATE payload exceeds max-frame-size."
       (length (http3-frame-payload frame))))
    (%call-http3-serialized
     client
     (lambda ()
       (funcall (http3-client-write-stream client)
                (http3-client-control-stream client)
                (encode-http3-frame frame)
                :fin-p nil :timeout timeout :deadline deadline)))
    client))

(defun advertise-http3-max-push-id (client max-push-id &key timeout deadline)
  "Increase CLIENT's advertised HTTP/3 server-push limit."
  (unless (http3-client-p client)
    (%h3-transport-error
     "ADVERTISE-HTTP3-MAX-PUSH-ID requires an HTTP/3 client."
     (type-of client)))
  (unless (http3-client-open-p client)
    (%h3-transport-error "The HTTP/3 client is already closed."))
  (unless (and (integerp max-push-id)
               (<= 0 max-push-id +http3-max-varint+))
    (%h3-transport-error
     "An HTTP/3 maximum push ID must be a QUIC varint."
     max-push-id))
  (%call-http3-serialized
   client
   (lambda ()
     (let ((previous (http3-client-max-push-id client)))
       (when (and previous (<= max-push-id previous))
         (%h3-transport-error
          "MAX_PUSH_ID must increase the previously advertised push ID."
          :h3-id-error))
       (funcall (http3-client-write-stream client)
                (http3-client-control-stream client)
                (encode-http3-frame
                 (make-http3-frame
                  :type +http3-max-push-id-type+
                  :payload (http3-varint-encode max-push-id)))
                :fin-p nil :timeout timeout :deadline deadline)
       (setf (http3-client-max-push-id client) max-push-id))))
  client)

(defun cancel-http3-push (client push-id &key timeout deadline)
  "Cancel a promised, not-yet-received HTTP/3 server push."
  (unless (http3-client-p client)
    (%h3-transport-error "CANCEL-HTTP3-PUSH requires an HTTP/3 client."
                         (type-of client)))
  (unless (http3-client-open-p client)
    (%h3-transport-error "The HTTP/3 client is already closed."))
  (unless (and (integerp push-id) (<= 0 push-id +http3-max-varint+))
    (%h3-transport-error "An HTTP/3 push ID must be a QUIC varint." push-id))
  (%call-http3-serialized
   client
   (lambda ()
     (unless (assoc push-id (http3-client-push-promises client) :test #'=)
       (%h3-transport-error
        "CANCEL_PUSH must identify a promised HTTP/3 server push."
        :h3-id-error))
     (when (member push-id (http3-client-consumed-push-ids client) :test #'=)
       (%h3-transport-error
        "CANCEL_PUSH cannot cancel an already received HTTP/3 server push."
        :h3-id-error))
     (unless (member push-id (http3-client-cancelled-push-ids client) :test #'=)
       (funcall (http3-client-write-stream client)
                (http3-client-control-stream client)
                (encode-http3-frame
                 (make-http3-frame
                  :type +http3-cancel-push-type+
                  :payload (http3-varint-encode push-id)))
                :fin-p nil :timeout timeout :deadline deadline)
       (push push-id (http3-client-cancelled-push-ids client)))))
  client)

(defun close-http3-client (client &key condition)
  (unless (http3-client-p client)
    (%h3-transport-error "CLOSE-HTTP3-CLIENT requires an HTTP/3 client."
                         (type-of client)))
  (when (http3-client-open-p client)
    (setf (http3-client-open-p client) nil)
    (dolist (stream
              (remove-duplicates
               (remove nil
                       (list (http3-client-control-stream client)
                             (http3-client-qpack-encoder-stream client)
                             (http3-client-qpack-decoder-stream client)
                             (http3-client-peer-control-stream client)
                             (http3-client-peer-qpack-encoder-stream client)
                             (http3-client-peer-qpack-decoder-stream client)))
               :test #'eq))
      (funcall (http3-client-close-stream client)
               stream
               :condition condition)))
  t)

(defun %h3-consume-control-prefix (buffer prefix-seen-p)
  (if prefix-seen-p
      (values t buffer)
      (multiple-value-bind (stream-type position)
          (http3-varint-decode buffer :allow-incomplete-p t)
        (if (null stream-type)
            (values nil buffer)
            (progn
              (unless (= stream-type +http3-control-stream-type+)
                (%h3-transport-error
                 "The peer HTTP/3 unidirectional stream is not a control stream."
                 stream-type))
              (values t (subseq buffer position)))))))

(defun %h3-consume-stream-prefix (buffer prefix-seen-p expected-type error-detail)
  (if prefix-seen-p
      (values t buffer)
      (multiple-value-bind (stream-type position)
          (http3-varint-decode buffer :allow-incomplete-p t)
        (if (null stream-type)
            (values nil buffer)
            (progn
              (unless (= stream-type expected-type)
                (%h3-transport-error
                 "The peer HTTP/3 unidirectional stream has the wrong type."
                 error-detail))
              (values t (subseq buffer position)))))))

(defun %h3-qpack-stream-error (condition detail)
  (error 'http-protocol-error
         :message (http-kit:http-error-message condition)
         :operation :qpack
         :detail detail))

(defun attach-http3-peer-control-stream (client stream)
  "Attach the peer's incoming HTTP/3 control stream to CLIENT.

The stream is read with READ-HTTP3-CONTROL-STREAM.  A client may attach at
most one peer control stream, and attaching after reading has started is an
error."
  (unless (http3-client-p client)
    (%h3-transport-error
     "ATTACH-HTTP3-PEER-CONTROL-STREAM requires an HTTP/3 client."
     (type-of client)))
  (unless stream
    (%h3-transport-error
     "ATTACH-HTTP3-PEER-CONTROL-STREAM requires a stream."))
  (when (http3-client-peer-control-stream client)
    (%h3-transport-error
     "An HTTP/3 client can have only one peer control stream."))
  (when (http3-client-peer-control-prefix-seen-p client)
    (%h3-transport-error
     "The HTTP/3 peer control stream cannot be attached after reading starts."))
  (setf (http3-client-peer-control-stream client) stream)
  client)

(defun read-http3-control-stream (client &key timeout deadline)
  "Read one chunk from CLIENT's peer control stream and apply its frames.

Returns a list of effect keywords and a boolean indicating peer FIN.  The
peer control stream must begin with its HTTP/3 stream-type prefix, then a
SETTINGS frame; its state is available through
HTTP3-CLIENT-PEER-CONTROL-STATE."
  (unless (http3-client-p client)
    (%h3-transport-error
     "READ-HTTP3-CONTROL-STREAM requires an HTTP/3 client."
     (type-of client)))
  (unless (http3-client-open-p client)
    (%h3-transport-error "Cannot read an HTTP/3 control stream after client close."))
  (let ((stream (http3-client-peer-control-stream client)))
    (unless stream
      (%h3-transport-error
       "READ-HTTP3-CONTROL-STREAM requires an attached peer control stream."))
    (when (http3-client-peer-control-fin-p client)
      (return-from read-http3-control-stream (values '() t)))
    (multiple-value-bind (chunk fin-p)
        (%h3-read-client-stream client stream timeout deadline)
      (let ((buffer (http3-client-peer-control-buffer client))
            (events '())
            (ended-p (or fin-p (null chunk))))
        (when chunk
          (unless (%http3-octet-vector-p chunk)
            (%h3-transport-error
             "HTTP/3 read-stream must return an octet vector or NIL."
             (type-of chunk)))
          (setf buffer (%http3-concatenate-octets buffer chunk)))
        (multiple-value-bind (prefix-seen-p remainder)
            (%h3-consume-control-prefix
             buffer (http3-client-peer-control-prefix-seen-p client))
          (setf (http3-client-peer-control-prefix-seen-p client) prefix-seen-p
                buffer remainder)
          (when prefix-seen-p
            (multiple-value-bind (new-events frame-remainder)
                (process-http3-control-bytes
                 (http3-client-peer-control-state client)
                 buffer :allow-incomplete-p t
                 :max-frame-size (http3-client-max-frame-size client))
              (setf events new-events
                    buffer frame-remainder))))
        (when (http3-control-state-settings-received-p
               (http3-client-peer-control-state client))
          (%call-http3-serialized
           client
           (lambda ()
             (setf (qpack-dynamic-table-max-capacity
                    (http3-client-qpack-encoder-table client))
                   (or (cdr (assoc +http3-setting-qpack-max-table-capacity+
                                   (http3-control-state-settings
                                    (http3-client-peer-control-state client))))
                       0)
                   (qpack-decoder-stream-state-blocked-stream-limit
                    (http3-client-qpack-decoder-state client))
                   (or (cdr (assoc +http3-setting-qpack-blocked-streams+
                                   (http3-control-state-settings
                                    (http3-client-peer-control-state client))))
                       0)))))
        (when ended-p
          (unless (http3-client-peer-control-prefix-seen-p client)
            (%http3-control-error
             "The peer HTTP/3 control stream ended before its stream-type prefix."))
          (when (plusp (array-total-size buffer))
            (%http3-control-error
             "The peer HTTP/3 control stream ended with a truncated frame."))
          (unless (http3-control-state-settings-received-p
                   (http3-client-peer-control-state client))
            (%http3-control-error
             "The peer HTTP/3 control stream ended before SETTINGS."))
          (setf (http3-client-peer-control-fin-p client) t)
          (%http3-control-error
           "The peer closed the HTTP/3 control stream."
           :h3-closed-critical-stream))
        (setf (http3-client-peer-control-buffer client) buffer)
        (values events ended-p)))))

(defun http3-client-set-qpack-capacity
    (client capacity &key timeout deadline)
  "Set CLIENT's encoder capacity and notify the peer over its QPACK stream."
  (unless (and (http3-client-p client) (http3-client-open-p client))
    (%h3-transport-error "A live HTTP/3 client is required."))
  (%call-http3-serialized
   client
   (lambda ()
     (let ((stream (http3-client-qpack-encoder-stream client))
           (table (http3-client-qpack-encoder-table client)))
       (unless stream
         (%h3-transport-error "The local QPACK encoder stream is not open."))
       (qpack-dynamic-table-set-capacity
        (copy-qpack-dynamic-table table) capacity)
       (let ((instruction (qpack-encode-set-dynamic-table-capacity capacity)))
         (funcall (http3-client-write-stream client)
                  stream instruction :fin-p nil
                  :timeout timeout :deadline deadline))
       (qpack-dynamic-table-set-capacity table capacity)
       client))))

(defun http3-client-insert-qpack-field
    (client name value &key timeout deadline)
  "Insert NAME and VALUE into CLIENT's encoder table and notify the peer."
  (unless (and (http3-client-p client) (http3-client-open-p client))
    (%h3-transport-error "A live HTTP/3 client is required."))
  (%call-http3-serialized
   client
   (lambda ()
     (let ((stream (http3-client-qpack-encoder-stream client))
           (table (http3-client-qpack-encoder-table client)))
       (unless stream
         (%h3-transport-error "The local QPACK encoder stream is not open."))
       (qpack-dynamic-table-insert
        (copy-qpack-dynamic-table table) name value)
       (let ((instruction (qpack-encode-insert-with-literal-name name value)))
         (funcall (http3-client-write-stream client)
                  stream instruction :fin-p nil
                  :timeout timeout :deadline deadline))
       (let ((entry (qpack-dynamic-table-insert table name value)))
         (qpack-decoder-stream-state-note-insertions-sent
          (http3-client-qpack-decoder-state client)
          (qpack-dynamic-table-insert-count table))
         entry)))))

(defun attach-http3-peer-qpack-encoder-stream (client stream)
  "Attach the peer's single QPACK encoder stream to CLIENT."
  (unless (and (http3-client-p client) stream)
    (%h3-transport-error
     "ATTACH-HTTP3-PEER-QPACK-ENCODER-STREAM requires a client and stream."))
  (when (or (http3-client-peer-qpack-encoder-stream client)
            (http3-client-peer-qpack-encoder-prefix-seen-p client))
    (%h3-transport-error
     "An HTTP/3 connection can have only one peer QPACK encoder stream."
     :h3-stream-creation-error))
  (setf (http3-client-peer-qpack-encoder-stream client) stream)
  client)

(defun attach-http3-peer-qpack-decoder-stream (client stream)
  "Attach the peer's single QPACK decoder stream to CLIENT."
  (unless (and (http3-client-p client) stream)
    (%h3-transport-error
     "ATTACH-HTTP3-PEER-QPACK-DECODER-STREAM requires a client and stream."))
  (when (or (http3-client-peer-qpack-decoder-stream client)
            (http3-client-peer-qpack-decoder-prefix-seen-p client))
    (%h3-transport-error
     "An HTTP/3 connection can have only one peer QPACK decoder stream."
     :h3-stream-creation-error))
  (setf (http3-client-peer-qpack-decoder-stream client) stream)
  client)

(defun read-http3-qpack-encoder-stream (client &key timeout deadline)
  "Read and apply one chunk from the peer's QPACK encoder stream."
  (unless (and (http3-client-p client) (http3-client-open-p client))
    (%h3-transport-error "A live HTTP/3 client is required."))
  (let ((stream (http3-client-peer-qpack-encoder-stream client)))
    (unless stream
      (%h3-transport-error "The peer QPACK encoder stream is not attached."))
    (multiple-value-bind (chunk fin-p)
        (%h3-read-client-stream client stream timeout deadline)
      (multiple-value-bind (events ended-p ready)
          (%call-http3-serialized
           client
           (lambda ()
         (let ((buffer (http3-client-peer-qpack-encoder-buffer client))
               (events '())
               (ended-p (or fin-p (null chunk))))
           (when chunk
             (unless (%http3-octet-vector-p chunk)
               (%h3-transport-error "QPACK stream reads must return octets."))
             (setf buffer (%http3-concatenate-octets buffer chunk)))
           (multiple-value-bind (seen-p remainder)
               (%h3-consume-stream-prefix
                buffer
                (http3-client-peer-qpack-encoder-prefix-seen-p client)
                +http3-qpack-encoder-stream-type+
                :qpack-encoder-stream-error)
             (setf (http3-client-peer-qpack-encoder-prefix-seen-p client) seen-p
                   buffer remainder)
             (when seen-p
               (handler-case
                   (multiple-value-bind (new-events consumed)
                       (qpack-process-encoder-stream
                        (http3-client-qpack-decoder-table client)
                        buffer :allow-incomplete-p t)
                     (setf events new-events
                           buffer (subseq buffer consumed)))
                 (http-protocol-error (condition)
                   (%h3-qpack-stream-error condition
                                           :qpack-encoder-stream-error)))))
           (when ended-p
             (setf (http3-client-peer-qpack-encoder-fin-p client) t)
             (when (or (not (http3-client-peer-qpack-encoder-prefix-seen-p client))
                       (plusp (length buffer)))
               (%h3-transport-error
                "The peer QPACK encoder stream ended with truncated input."
                :qpack-encoder-stream-error))
             (%h3-transport-error
              "The peer closed the QPACK encoder stream."
              :h3-closed-critical-stream))
           (setf (http3-client-peer-qpack-encoder-buffer client) buffer)
             (values events ended-p
                     (%http3-qpack-detach-ready-streams
                      (http3-client-qpack-decoder-context client))))))
        (values (nconc events
                       (mapcar (lambda (blocked)
                                 (list :qpack-stream-unblocked blocked))
                               ready))
                ended-p)))))

(defun read-http3-qpack-decoder-stream (client &key timeout deadline)
  "Read and apply one chunk from the peer's QPACK decoder stream."
  (unless (and (http3-client-p client) (http3-client-open-p client))
    (%h3-transport-error "A live HTTP/3 client is required."))
  (let ((stream (http3-client-peer-qpack-decoder-stream client)))
    (unless stream
      (%h3-transport-error "The peer QPACK decoder stream is not attached."))
    (multiple-value-bind (chunk fin-p)
        (%h3-read-client-stream client stream timeout deadline)
      (%call-http3-serialized
       client
       (lambda ()
         (let ((buffer (http3-client-peer-qpack-decoder-buffer client))
               (events '())
               (ended-p (or fin-p (null chunk))))
           (when chunk
             (unless (%http3-octet-vector-p chunk)
               (%h3-transport-error "QPACK stream reads must return octets."))
             (setf buffer (%http3-concatenate-octets buffer chunk)))
           (multiple-value-bind (seen-p remainder)
               (%h3-consume-stream-prefix
                buffer
                (http3-client-peer-qpack-decoder-prefix-seen-p client)
                +http3-qpack-decoder-stream-type+
                :qpack-decoder-stream-error)
             (setf (http3-client-peer-qpack-decoder-prefix-seen-p client) seen-p
                   buffer remainder)
             (when seen-p
               (handler-case
                   (multiple-value-bind (new-events consumed)
                       (qpack-process-decoder-stream
                        buffer :allow-incomplete-p t
                        :state (http3-client-qpack-decoder-state client))
                     (setf events new-events
                           buffer (subseq buffer consumed)))
                 (http-protocol-error (condition)
                   (%h3-qpack-stream-error condition
                                           :qpack-decoder-stream-error)))))
           (when ended-p
             (setf (http3-client-peer-qpack-decoder-fin-p client) t)
             (when (or (not (http3-client-peer-qpack-decoder-prefix-seen-p client))
                       (plusp (length buffer)))
               (%h3-transport-error
                "The peer QPACK decoder stream ended with truncated input."
                :qpack-decoder-stream-error))
             (%h3-transport-error
              "The peer closed the QPACK decoder stream."
              :h3-closed-critical-stream))
           (setf (http3-client-peer-qpack-decoder-buffer client) buffer)
           (values events ended-p)))))))

(defun serve-http3-control-stream
    (stream &key read-stream close-stream
            (max-frame-size +http3-default-max-frame-size+)
            on-settings on-goaway on-max-push-id on-cancel-push
            timeout deadline on-error peer-role (promised-push-ids '()))
  "Serve one incoming HTTP/3 control stream over QUIC callbacks.

READ-STREAM is called as (STREAM &KEY TIMEOUT DEADLINE) and returns an octet
vector and a FIN boolean.  CLOSE-STREAM, when supplied, is called as
(STREAM &KEY CONDITION) exactly once.  The stream must begin with the
HTTP/3 control-stream type prefix and a SETTINGS frame.  ON-SETTINGS receives
(SETTINGS STATE), while ON-GOAWAY, ON-MAX-PUSH-ID, and ON-CANCEL-PUSH receive
(IDENTIFIER STATE).  ON-ERROR receives (CONDITION STATE) before the condition
is re-signaled.  PROMISED-PUSH-IDS identifies pushes already sent to a client."
  (unless (functionp read-stream)
    (%h3-transport-error
     "HTTP/3 control read-stream callback is required."
     (type-of read-stream)))
  (unless (or (null close-stream) (functionp close-stream))
    (%h3-transport-error
     "HTTP/3 control close-stream must be a function or NIL."
     (type-of close-stream)))
  (dolist (callback (list on-settings on-goaway on-max-push-id on-cancel-push
                          on-error))
    (unless (or (null callback) (functionp callback))
      (%h3-transport-error
       "HTTP/3 control callbacks must be functions or NIL."
       (type-of callback))))
  (unless (%h3-positive-limit-p max-frame-size)
    (%h3-transport-error
     "HTTP/3 control max-frame-size must be a positive QUIC varint."
     max-frame-size))
  (unless (member peer-role '(:client :server))
    (%h3-transport-error
     "HTTP/3 control peer-role must be :CLIENT or :SERVER."
     peer-role))
  (let ((closer (or close-stream
                    (lambda (ignored-stream &key condition)
                      (declare (ignore ignored-stream condition))
                      nil)))
        (state (make-http3-control-state
                :peer-role peer-role
                :promised-push-ids (copy-list promised-push-ids)))
        (buffer (make-array 0 :element-type '(unsigned-byte 8)))
        (prefix-seen-p nil)
        (failure nil))
    (labels ((notify (effect frame)
               (case effect
                 (:settings
                  (when on-settings
                    (funcall on-settings
                             (http3-control-state-settings state)
                             state)))
                 (:goaway
                  (when on-goaway
                    (funcall on-goaway
                             (%http3-control-payload-varint frame "GOAWAY")
                             state)))
                 (:max-push-id
                  (when on-max-push-id
                    (funcall on-max-push-id
                             (%http3-control-payload-varint frame "MAX_PUSH_ID")
                             state)))
                 (:cancel-push
                  (when on-cancel-push
                    (funcall on-cancel-push
                             (%http3-control-payload-varint frame "CANCEL_PUSH")
                             state)))
                 (otherwise nil)))
             (process-buffer ()
               (multiple-value-bind (frames remainder)
                   (decode-http3-frames
                    buffer :allow-incomplete-p t
                    :max-frame-size max-frame-size)
                 (setf buffer remainder)
                 (dolist (frame frames)
                   (notify (process-http3-control-frame state frame) frame)))))
      (unwind-protect
           (handler-case
               (loop
                 (multiple-value-bind (chunk fin-p)
                     (funcall read-stream stream
                              :timeout timeout :deadline deadline)
                   (let ((ended-p (or fin-p (null chunk))))
                     (when chunk
                       (unless (%http3-octet-vector-p chunk)
                         (%h3-transport-error
                          "HTTP/3 control read-stream must return an octet vector or NIL."
                          (type-of chunk)))
                       (setf buffer (%http3-concatenate-octets buffer chunk)))
                     (multiple-value-bind (seen-p remainder)
                         (%h3-consume-control-prefix buffer prefix-seen-p)
                       (setf prefix-seen-p seen-p
                             buffer remainder)
                       (when prefix-seen-p
                         (process-buffer)))
                     (when ended-p
                       (unless prefix-seen-p
                         (%http3-control-error
                          "The HTTP/3 control stream ended before its stream-type prefix."))
                       (when (plusp (array-total-size buffer))
                         (%http3-control-error
                          "The HTTP/3 control stream ended with a truncated frame."))
                       (unless (http3-control-state-settings-received-p state)
                         (%http3-control-error
                          "The HTTP/3 control stream ended before SETTINGS."))
                       (%http3-control-error
                        "The peer closed the HTTP/3 control stream."
                        :h3-closed-critical-stream)))))
             (error (condition)
               (setf failure condition)
               (when on-error
                 (http-kit::%with-http-cleanup
                   (funcall on-error condition state)))
               (error condition)))
        (funcall closer stream :condition failure)))))

(defun %h3-write-frame
    (client stream frame &key fin-p timeout deadline)
  (let ((payload (http3-frame-payload frame)))
    (when (> (length payload) (http3-client-max-frame-size client))
      (%h3-size-error :frame (http3-client-max-frame-size client)
                      (length payload)))
    (funcall (http3-client-write-stream client)
             stream
             (encode-http3-frame frame)
             :fin-p fin-p
             :timeout timeout
             :deadline deadline)))

(defun %h3-header-name (header)
  (let ((name (http-kit:http-header-name header)))
    (unless (string= name (string-downcase name))
      (%h3-invalid-header name "HTTP/3 field names must be lowercase."))
    (unless (http-kit::%header-name-p name)
      (%h3-invalid-header name "the field name is not a token."))
    name))

(defun %h3-connection-specific-name-p (name)
  (member name '("connection" "keep-alive" "proxy-connection"
                 "transfer-encoding" "upgrade")
          :test #'string=))

(defun %h3-te-value-p (value)
  (every (lambda (token)
           (string= token "trailers"))
         (http-kit::%split-comma-values (list value))))

(defun %h3-header-values (header)
  (list (http-kit:http-header-content header)))

(defun %h3-field-sections-equal-p (left right)
  (and (= (length left) (length right))
       (every (lambda (left-field right-field)
                (and (string= (car left-field) (car right-field))
                     (string= (cdr left-field) (cdr right-field))))
              left right)))

(defun %h3-content-length (headers body-length body-length-known-p)
  (let ((content-lengths
          (loop for header in headers
                when (string= (http-kit:http-header-name header)
                              "content-length")
                  collect (http-kit:http-header-content header))))
    (when content-lengths
      (unless (every #'http-kit::%decimal-string-p content-lengths)
        (%h3-invalid-header "content-length"
                            "content-length must be an ASCII decimal integer."))
      (let ((length (http-kit::%parse-decimal (first content-lengths))))
        (unless (every (lambda (value)
                        (= length (http-kit::%parse-decimal value)))
                      content-lengths)
          (%h3-invalid-header "content-length"
                              "duplicate content-length values must agree."))
        (when (and body-length-known-p (/= length body-length))
          (%h3-invalid-header "content-length"
                              "content-length does not match the request body."))
        length))))

(defun %h3-request-fields
    (request &key
               (body-length (length (http-kit:http-request-body request)))
               (body-length-known-p t)
               (force-content-length-p nil))
  (let* ((method (http-kit:http-request-method request))
         (uri (http-kit:http-request-uri request))
         (protocol (http-kit:http-request-protocol request))
         (authority (http-kit:http-uri-authority uri))
         (path-and-query (http-kit:http-request-target request))
         (headers (http-kit:http-request-headers request))
         (regular '())
         (host-values '()))
    (when (if (and (string= method "CONNECT") protocol)
              (or (zerop (length path-and-query))
                  (not (char= (char path-and-query 0) #\/)))
              (and (not (string= method "CONNECT"))
                   (or (and (string= path-and-query "*")
                            (not (string= method "OPTIONS")))
                       (and (not (string= path-and-query "*"))
                            (not (char= (char path-and-query 0) #\/))))))
      (%h3-invalid-header
       ":path" "must be origin-form, or asterisk-form for OPTIONS."))
    (dolist (header headers)
      (let ((name (%h3-header-name header)))
        (cond
          ((string= name "host")
           (setf host-values (append host-values (%h3-header-values header))))
          ((%h3-connection-specific-name-p name)
           (%h3-invalid-header name "connection-specific fields are forbidden."))
          ((and (string= name "te")
                (not (every #'%h3-te-value-p (%h3-header-values header))))
           (%h3-invalid-header name "HTTP/3 permits only the trailers value."))
          (t
           (dolist (value (%h3-header-values header))
             (push (cons name value) regular))))))
    (when (and host-values
               (or (not (= (length host-values) 1))
                   (not (string-equal (first host-values) authority))))
      (%h3-invalid-header "host" "host must match the request URI authority."))
    (let ((content-length
            (%h3-content-length headers body-length body-length-known-p)))
    (let ((fields
            (if (and (string= method "CONNECT") protocol)
                (list (cons ":method" method)
                      (cons ":protocol" protocol)
                      (cons ":scheme" (http-kit:http-uri-scheme uri))
                      (cons ":authority" authority)
                      (cons ":path" path-and-query))
                (if (string= method "CONNECT")
                    (list (cons ":method" method)
                          (cons ":authority" authority))
                    (list (cons ":method" method)
                          (cons ":scheme" (http-kit:http-uri-scheme uri))
                          (cons ":authority" authority)
                          (cons ":path" path-and-query))))))
      (when (and body-length-known-p
                 (or force-content-length-p (plusp body-length) content-length)
                 (not (find "content-length" regular :key #'car :test #'string=)))
        (setf regular
              (append regular (list (cons "content-length"
                                          (princ-to-string body-length))))))
      (values (append fields (nreverse regular)) content-length)))))

(defun %h3-validate-request-body-options
    (request request-body-function request-body-length)
  (when request-body-function
    (unless (functionp request-body-function)
      (%h3-transport-error
       "The HTTP/3 request body producer must be a function."
       request-body-function)))
  (unless (or (null request-body-length)
              (and (integerp request-body-length)
                   (>= request-body-length 0)))
    (%h3-transport-error
     "The HTTP/3 request body length must be a non-negative integer or NIL."
     request-body-length))
  (when (and request-body-length (null request-body-function))
    (%h3-transport-error
     "A request body length requires a request body producer."
     request-body-length))
  (when (and request-body-function
             (string= (http-kit:http-request-method request) "TRACE"))
    (%h3-transport-error
     "TRACE requests must not contain content."
     :trace-content))
  (when (and request-body-function
             (plusp (length (http-kit:http-request-body request))))
    (%h3-transport-error
     "A request body producer cannot be combined with an in-memory request body."
     request))
  (values request-body-function request-body-length))

(defun %h3-validate-request-body-chunk (chunk max-frame-size)
  (unless (and (arrayp chunk)
               (= (array-rank chunk) 1)
               (not (stringp chunk)))
    (%h3-transport-error
     "An HTTP/3 request body producer must return a one-dimensional octet array or NIL."
     (type-of chunk)))
  (when (zerop (length chunk))
    (%h3-transport-error
     "An HTTP/3 request body producer returned an empty chunk." chunk))
  (when (> (length chunk) max-frame-size)
    (%h3-transport-error
     "An HTTP/3 request body producer returned a chunk larger than the advertised maximum."
     (list (length chunk) max-frame-size)))
  (loop for octet across chunk
        unless (and (integerp octet) (<= 0 octet 255))
          do (%h3-transport-error
              "An HTTP/3 request body chunk contains a non-octet value."
              octet))
  chunk)

(defun %h3-trailer-fields (request)
  (let ((result '()))
    (dolist (header (http-kit:http-request-trailers request)
             (nreverse result))
      (let ((name (%h3-header-name header)))
        (when (http-kit::%forbidden-trailer-field-name-p name)
          (%h3-invalid-header name "the field is forbidden in HTTP/3 trailers."))
        (dolist (value (%h3-header-values header))
          (push (cons name value) result))))))

(defun %h3-response-status (value)
  (unless (and (= (length value) 3) (http-kit::%decimal-string-p value))
    (%h3-transport-error "HTTP/3 :status must be a three-digit ASCII status code."
                         value))
  (let ((status (http-kit::%parse-decimal value)))
    (unless (<= 100 status 599)
      (%h3-transport-error "HTTP/3 :status is outside the HTTP status range."
                           status))
    status))

(defun %h3-response-fields (fields &key trailers-p)
  (let ((status nil)
        (headers '())
        (regular-seen-p nil))
    (dolist (field fields)
      (let ((name (car field))
            (value (cdr field)))
        (if (and (string/= name "") (char= (char name 0) #\:))
            (progn
              (when trailers-p
                (%h3-transport-error
                 "HTTP/3 trailers cannot contain pseudo-fields." name))
              (when regular-seen-p
                (%h3-transport-error
                 "HTTP/3 pseudo-fields must precede regular fields." name))
              (unless (string= name ":status")
                (%h3-transport-error "Unknown HTTP/3 response pseudo-field." name))
              (when status
                (%h3-transport-error "An HTTP/3 response has duplicate :status fields."))
              (setf status (%h3-response-status value)))
            (progn
              (setf regular-seen-p t)
              (when (if trailers-p
                        (http-kit::%forbidden-trailer-field-name-p name)
                        (%h3-connection-specific-name-p name))
                (%h3-invalid-header name "the field is forbidden in this field section."))
              (when (and (not trailers-p) (string= name "te"))
                (%h3-invalid-header
                 name "HTTP/3 TE is permitted only in request header sections."))
              (push (http-kit:make-http-header name value) headers)))))
    (if trailers-p
        (progn
          (when status
            (%h3-transport-error "HTTP/3 trailers cannot contain :status."))
          (values nil (nreverse headers)))
        (progn
          (unless status
            (%h3-transport-error "An HTTP/3 response HEADERS block lacks :status."))
          (values status (nreverse headers))))))

(defun %h3-append-body (body octets)
  (let ((result (make-array (+ (length body) (length octets))
                            :element-type '(unsigned-byte 8))))
    (replace result body)
    (replace result octets :start1 (length body))
    result))

(defun %h3-no-body-response-p (request-method status)
  (or (and (stringp request-method)
           (string= request-method "HEAD"))
      (member status '(204 205 304) :test #'=)))

(defun %h3-response-content-length
    (headers body-length &key status request-method no-body)
  (let ((values
          (loop for header in headers
                when (string= (http-kit:http-header-name header)
                              "content-length")
                  collect (http-kit:http-header-content header))))
    (when values
      (unless (and (= (length values) 1)
                   (http-kit::%decimal-string-p (first values)))
        (%h3-invalid-header "content-length"
                            "content-length is invalid or duplicated."))
      (let ((expected (http-kit::%parse-decimal (first values))))
        (when (or (and status (< status 200))
                  (and status (= status 204))
                  (and status (= status 205) (plusp expected))
                  (and status
                       (stringp request-method)
                       (string= request-method "CONNECT")
                       (<= 200 status 299)))
          (%h3-invalid-header
           "content-length" "content-length is forbidden for this response."))
        (unless (or no-body (= expected body-length))
          (%h3-invalid-header
           "content-length" "content-length does not match the response body."))))))

(defun %h3-decode-dynamic-field-section
    (payload context stream-id await-qpack serialize
     max-header-bytes max-fields)
  (unless (and (integerp stream-id) (>= stream-id 0))
    (%h3-transport-error
     "Dynamic QPACK decoding requires a numeric stream ID." stream-id))
  (unless (functionp await-qpack)
    (%h3-transport-error
     "Dynamic QPACK decoding requires an await-qpack callback." await-qpack))
  (let ((decoded nil)
        (decoded-count 0))
    (flet ((section-decoded (fields)
             (incf decoded-count)
             (unless (= decoded-count 1)
               (%h3-transport-error
                "A QPACK field section was decoded more than once." stream-id))
             (setf decoded fields)))
      (let ((result
              (decode-http3-qpack-field-section
               payload context stream-id
               :serialize serialize
               :on-decoded #'section-decoded
               :max-header-bytes max-header-bytes
               :max-fields max-fields)))
        (when (http3-qpack-blocked-stream-p result)
          (funcall await-qpack result))
        (unless (= decoded-count 1)
          (%h3-transport-error
           "The QPACK await callback returned before decoding completed."
           stream-id))
        decoded))))

(defun %h3-decode-response-field-section
    (client payload qpack-decoder-table
     &key qpack-decoder-context stream-id await-qpack max-header-bytes max-fields)
  (let ((header-limit
          (min (or max-header-bytes most-positive-fixnum)
               (http3-client-max-header-bytes client)))
        (field-limit
          (min (or max-fields most-positive-fixnum)
               (http3-client-max-fields client))))
    (if qpack-decoder-context
        (%h3-decode-dynamic-field-section
         payload qpack-decoder-context stream-id await-qpack
         (http3-client-serialize client)
         header-limit field-limit)
        (flet ((decode ()
                 (qpack-decode-field-section
                  payload
                  :max-header-bytes header-limit
                  :max-fields field-limit
                  :dynamic-table qpack-decoder-table)))
          (if (eq qpack-decoder-table
                  (http3-client-qpack-decoder-table client))
              (%call-http3-serialized client #'decode)
              (decode))))))

(defun %h3-read-response
    (client stream &key on-body-chunk on-information collect-body-p max-body-bytes
            max-header-bytes max-fields
            qpack-decoder-table qpack-decoder-context stream-id await-qpack
            request-method on-push-promise timeout deadline)
  (when qpack-decoder-context
    (unless (and (integerp stream-id) (>= stream-id 0))
      (%h3-transport-error
       "Dynamic QPACK response decoding requires a numeric stream ID."
       stream-id))
    (unless (functionp await-qpack)
      (%h3-transport-error
       "Dynamic QPACK response decoding requires an await-qpack callback."
       await-qpack)))
  (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)))
        (status nil)
        (headers nil)
        (trailers nil)
        (body (make-array 0 :element-type '(unsigned-byte 8)))
        (body-length 0)
        (final-response-p nil)
        (trailers-seen-p nil))
    (labels
        ((process-frame (frame)
           (let ((type (http3-frame-type frame))
                 (payload (http3-frame-payload frame)))
             (cond
               ((= type +http3-headers-type+)
                (let ((fields
                        (%h3-decode-response-field-section
                         client payload qpack-decoder-table
                         :qpack-decoder-context qpack-decoder-context
                         :stream-id stream-id
                         :await-qpack await-qpack
                         :max-header-bytes max-header-bytes
                         :max-fields max-fields)))
                  (multiple-value-bind (new-status new-headers)
                      (%h3-response-fields fields :trailers-p final-response-p)
                    (if final-response-p
                        (progn
                          (when trailers-seen-p
                            (%h3-transport-error
                             "An HTTP/3 response cannot contain multiple trailer blocks."))
                          (setf trailers new-headers
                                trailers-seen-p t))
                        (progn
                          (when (= new-status 101)
                            (%h3-transport-error
                             "HTTP/3 does not permit a 101 Switching Protocols response."))
                          (if (>= new-status 200)
                              (progn
                                (setf status new-status
                                      headers new-headers
                                      final-response-p t))
                              (progn
                                (when (http-kit:http-header-values
                                       new-headers "content-length")
                                  (%h3-invalid-header
                                   "content-length"
                                   "informational responses cannot contain content-length."))
                                (when on-information
                                  (funcall
                                   on-information
                                   (http-kit:make-http-response
                                    :protocol-version "HTTP/3"
                                    :status new-status
                                    :headers new-headers
                                    :trailers nil
                                    :body (http-kit::%empty-octets)))))))))))
               ((= type +http3-data-type+)
                (unless final-response-p
                  (%h3-transport-error
                   "An HTTP/3 DATA frame arrived before final response HEADERS."))
                (when trailers-seen-p
                  (%h3-transport-error
                   "An HTTP/3 DATA frame arrived after response trailers."))
                (when (%h3-no-body-response-p request-method status)
                  (%h3-transport-error
                   "This HTTP/3 response cannot carry DATA."
                   (list request-method status)))
                (let ((new-length (+ body-length (length payload))))
                  (when (and max-body-bytes (> new-length max-body-bytes))
                    (%h3-size-error :body max-body-bytes new-length))
                  (setf body-length new-length)
                  (when collect-body-p
                    (setf body (%h3-append-body body payload)))
                  (when on-body-chunk
                    (funcall on-body-chunk (%http3-copy-octets payload)))))
               ((= type +http3-push-promise-type+)
                (multiple-value-bind (push-id field-position)
                    (http3-varint-decode payload)
                  (let ((max-push-id (http3-client-max-push-id client)))
                    (unless (and max-push-id (<= push-id max-push-id))
                      (%h3-transport-error
                       "HTTP/3 PUSH_PROMISE exceeds the advertised MAX_PUSH_ID."
                       :h3-id-error))
                    (let ((fields
                            (%h3-decode-response-field-section
                             client (subseq payload field-position)
                             qpack-decoder-table
                             :qpack-decoder-context qpack-decoder-context
                             :stream-id stream-id
                             :await-qpack await-qpack
                             :max-header-bytes max-header-bytes
                             :max-fields max-fields)))
                      (multiple-value-call #'list
                        (%h3-server-parse-request-fields fields))
                      (%call-http3-serialized
                       client
                       (lambda ()
                         (let ((existing
                                 (assoc push-id
                                        (http3-client-push-promises client)
                                        :test #'=)))
                           (cond
                             ((and existing
                                   (not (%h3-field-sections-equal-p
                                         (cdr existing) fields)))
                              (%h3-transport-error
                               "Repeated HTTP/3 PUSH_PROMISE fields differ."
                               :h3-id-error))
                             ((null existing)
                              (pushnew
                               push-id
                               (http3-client-promised-push-ids client)
                               :test #'=)
                              (push (cons push-id (copy-tree fields))
                                    (http3-client-push-promises client)))))))
                      (when on-push-promise
                        (funcall on-push-promise push-id (copy-tree fields)))))))
               ((member type (list +http3-settings-type+
                                   +http3-cancel-push-type+
                                   +http3-goaway-type+
                                   +http3-max-push-id-type+
                                   +http3-priority-update-request-type+
                                   +http3-priority-update-push-type+)
                        :test #'=)
                (%h3-transport-error
                 "HTTP/3 control frames are not valid on a request stream." type))
               (t
                ;; Extension frame types are ignored by HTTP/3 endpoints.
                nil))))
         (finish-response ()
           (unless final-response-p
             (%h3-transport-error "The HTTP/3 stream ended before final response HEADERS."))
           (%h3-response-content-length
            headers body-length :status status :request-method request-method
            :no-body (%h3-no-body-response-p request-method status))
           (http-kit:make-http-response
            :protocol-version "HTTP/3"
            :status status
            :headers headers
            :trailers trailers
            :body (if collect-body-p body nil))))
      (loop
        (multiple-value-bind (chunk fin-p)
            (%h3-read-client-stream client stream timeout deadline)
          (when chunk
            (unless (%http3-octet-vector-p chunk)
              (%h3-transport-error
               "HTTP/3 read-stream must return an octet vector or NIL."
               (type-of chunk)))
            (when (plusp (length chunk))
              (setf buffer (%http3-concatenate-octets buffer chunk))
              (multiple-value-bind (frames remainder)
                  (decode-http3-frames
                   buffer :allow-incomplete-p t
                   :max-frame-size (http3-client-max-frame-size client))
                (setf buffer remainder)
                (dolist (frame frames)
                  (process-frame frame)))))
          (when (or fin-p (null chunk))
            (when (plusp (length buffer))
              (%h3-transport-error
               "The HTTP/3 stream ended with a truncated frame."))
            (return (finish-response))))))))

(defun %h3-write-request-field-section
    (client stream fields encoder-table encoder-state stream-id huffman-p
     &key fin-p timeout deadline)
  (if (eq encoder-table (http3-client-qpack-encoder-table client))
      (%call-http3-serialized
       client
       (lambda ()
         (multiple-value-bind (section registration)
             (%qpack-prepare-field-section
              fields
              :dynamic-table encoder-table
              :decoder-stream-state encoder-state
              :stream-id stream-id
              :huffman-p huffman-p)
           (%h3-write-frame
            client stream
            (make-http3-frame :type +http3-headers-type+ :payload section)
            :fin-p fin-p :timeout timeout :deadline deadline)
           (%qpack-commit-field-section registration))))
      (%h3-write-frame
       client stream
       (make-http3-frame
        :type +http3-headers-type+
        :payload
        (qpack-encode-field-section
         fields :dynamic-table encoder-table :huffman-p huffman-p))
       :fin-p fin-p :timeout timeout :deadline deadline)))

(defun send-http3-request
    (client request &key on-body-chunk on-information (collect-body-p t)
            max-header-bytes max-fields max-body-bytes
            request-body-function request-body-length
            qpack-encoder-table qpack-decoder-table qpack-decoder-context
            await-qpack (huffman-p nil) on-stream-open on-push-promise
            timeout deadline)
  "Send REQUEST on a new HTTP/3 bidirectional stream and read its response.

The in-memory request and response bodies are represented as octet vectors.
REQUEST-BODY-FUNCTION, when supplied, receives the maximum chunk size and
returns non-empty octet arrays until it returns NIL.  REQUEST-BODY-LENGTH may
declare the producer's exact length.  When ON-BODY-CHUNK is supplied it
receives each response DATA payload; the payload is still collected when
COLLECT-BODY-P is true.  QPACK-ENCODER-TABLE and
QPACK-DECODER-TABLE, when supplied, are caller-owned dynamic tables used for
request and response field sections.  HUFFMAN-P enables Huffman encoding for
newly emitted field values and names.  QPACK-DECODER-CONTEXT enables blocked
field sections; AWAIT-QPACK must wait for and resume the token before returning.
ON-PUSH-PROMISE receives the push ID and decoded request field pairs after
the promise has passed HTTP/3 request-field validation.  ON-INFORMATION
receives each validated informational response in wire order."
  (unless (http3-client-p client)
    (%h3-transport-error "SEND-HTTP3-REQUEST requires an HTTP/3 client."
                         (type-of client)))
  (unless (http3-client-open-p client)
    (%h3-transport-error "The HTTP/3 client is already closed."))
  (unless (http-kit:http-request-p request)
    (%h3-transport-error "SEND-HTTP3-REQUEST requires an HTTP request."
                         (type-of request)))
  (unless (or (null on-stream-open) (functionp on-stream-open))
    (%h3-transport-error "ON-STREAM-OPEN must be a function or NIL."
                         on-stream-open))
  (unless (or (null on-push-promise) (functionp on-push-promise))
    (%h3-transport-error "ON-PUSH-PROMISE must be a function or NIL."
                         on-push-promise))
  (unless (or (null on-information) (functionp on-information))
    (%h3-transport-error "ON-INFORMATION must be a function or NIL."
                         on-information))
  (unless (or (null max-header-bytes)
              (%h3-positive-limit-p max-header-bytes))
    (%h3-transport-error
     "MAX-HEADER-BYTES must be NIL or a positive integer." max-header-bytes))
  (unless (or (null max-fields) (%h3-positive-limit-p max-fields))
    (%h3-transport-error
     "MAX-FIELDS must be NIL or a positive integer." max-fields))
  (when (http-kit:http-request-protocol request)
    (let ((state (http3-client-peer-control-state client)))
      (unless (and (http3-control-state-settings-received-p state)
                   (= 1 (or (cdr (assoc +http3-setting-enable-connect+
                                        (http3-control-state-settings state)))
                            0)))
        (%h3-transport-error
         "Extended CONNECT requires peer SETTINGS_ENABLE_CONNECT_PROTOCOL=1."
         :h3-settings-error))))
  (unless (or (null max-body-bytes) (%h3-non-negative-limit-p max-body-bytes))
    (%h3-transport-error "max-body-bytes must be NIL or a non-negative integer."
                         max-body-bytes))
  (%h3-validate-request-body-options
   request request-body-function request-body-length)
  (let* ((body (http-kit:http-request-body request))
         (known-body-length
           (if request-body-function request-body-length (length body)))
         (request-fields nil)
         (declared-body-length nil)
         (trailer-fields (%h3-trailer-fields request))
         (encoder-table
           (or qpack-encoder-table (http3-client-qpack-encoder-table client)))
         (decoder-table
           (or qpack-decoder-table (http3-client-qpack-decoder-table client)))
         (decoder-context
           (or qpack-decoder-context
               (and await-qpack
                    (eq decoder-table
                        (http3-client-qpack-decoder-table client))
                    (http3-client-qpack-decoder-context client))))
         (stream nil)
         (stream-id nil)
         (failure nil))
    (multiple-value-setq (request-fields declared-body-length)
      (if request-body-function
          (if request-body-length
              (%h3-request-fields request :body-length request-body-length
                                  :force-content-length-p t)
              (%h3-request-fields request :body-length-known-p nil))
          (%h3-request-fields request :body-length (length body))))
    (when (and request-body-function (null known-body-length))
      (setf known-body-length declared-body-length))
    (multiple-value-setq (stream stream-id)
      (funcall (http3-client-open-stream client)
               request :stream-type :request :timeout timeout :deadline deadline))
    (unless stream
      (%h3-transport-error "The HTTP/3 open-stream callback returned NIL."))
    (unwind-protect
         (handler-case
             (progn
               (let ((goaway-id
                       (http3-control-state-goaway-id
                        (http3-client-peer-control-state client))))
                 (when goaway-id
                   (unless (and (integerp stream-id)
                                (>= stream-id 0)
                                (< stream-id goaway-id))
                     (%h3-transport-error
                      "The peer GOAWAY excludes this HTTP/3 request stream."
                      :h3-request-rejected))))
               (when decoder-context
                 (unless (and (integerp stream-id) (>= stream-id 0))
                   (%h3-transport-error
                    "Dynamic QPACK response decoding requires a numeric stream ID."
                    stream-id))
                 (unless (functionp await-qpack)
                   (%h3-transport-error
                    "Dynamic QPACK response decoding requires an await-qpack callback."
                    await-qpack)))
               (when on-stream-open
                 (funcall on-stream-open stream stream-id
                          (lambda (&key (error-code #x10c))
                            (cancel-http3-stream
                             client stream :error-code error-code
                             :timeout timeout :deadline deadline))))
               (when (and (eq encoder-table
                              (http3-client-qpack-encoder-table client))
                          (plusp (qpack-dynamic-table-insert-count encoder-table))
                          (not (and (integerp stream-id) (>= stream-id 0))))
                 (%h3-transport-error
                  "OPEN-STREAM must return a numeric stream ID when dynamic QPACK entries exist."))
               (let ((encoder-state
                     (and (integerp stream-id)
                          (eq encoder-table
                              (http3-client-qpack-encoder-table client))
                          (http3-client-qpack-decoder-state client))))
                (%h3-write-request-field-section
                 client stream request-fields encoder-table encoder-state
                 stream-id huffman-p
                 :fin-p (and (null request-body-function)
                             (zerop (length body))
                             (null trailer-fields))
                 :timeout timeout :deadline deadline)
               (if request-body-function
                   (loop with sent = 0
                         for chunk = (funcall request-body-function
                                              (http3-client-max-frame-size client))
                         do (if chunk
                                (progn
                                  (%h3-validate-request-body-chunk
                                   chunk (http3-client-max-frame-size client))
                                  (when (and known-body-length
                                             (> (+ sent (length chunk))
                                                known-body-length))
                                    (%h3-transport-error
                                     "The HTTP/3 request body producer exceeded its declared length."
                                     (list (+ sent (length chunk))
                                           known-body-length)))
                                  (%h3-write-frame
                                   client stream
                                   (make-http3-frame
                                    :type +http3-data-type+ :payload chunk)
                                   :fin-p nil :timeout timeout :deadline deadline)
                                  (incf sent (length chunk)))
                                (progn
                                  (when (and known-body-length
                                             (/= sent known-body-length))
                                    (%h3-transport-error
                                     "The HTTP/3 request body producer ended before its declared length."
                                     (list sent known-body-length)))
                                  (unless trailer-fields
                                    (%h3-write-frame
                                     client stream
                                     (make-http3-frame
                                      :type +http3-data-type+
                                      :payload (http-kit::%empty-octets))
                                     :fin-p t :timeout timeout :deadline deadline))
                                  (return))))
                   (unless (zerop (length body))
                     (loop with position = 0
                           while (< position (length body))
                           for end = (min (length body)
                                          (+ position
                                             (http3-client-max-frame-size client)))
                           for last-p = (= end (length body))
                           do (%h3-write-frame
                               client stream
                               (make-http3-frame
                                :type +http3-data-type+
                                :payload (subseq body position end))
                               :fin-p (and last-p (null trailer-fields))
                               :timeout timeout :deadline deadline)
                              (setf position end))))
               (when trailer-fields
                 (%h3-write-request-field-section
                  client stream trailer-fields encoder-table encoder-state
                  stream-id huffman-p
                  :fin-p t :timeout timeout :deadline deadline))
               (%h3-read-response
               client stream
                :on-body-chunk on-body-chunk
                :on-information on-information
                :collect-body-p collect-body-p
                :max-header-bytes max-header-bytes
                :max-fields max-fields
                :max-body-bytes max-body-bytes
                :qpack-decoder-table decoder-table
                :qpack-decoder-context decoder-context
                :stream-id stream-id
                :await-qpack await-qpack
                :request-method (http-kit:http-request-method request)
                :on-push-promise on-push-promise
                :timeout timeout
                :deadline deadline)))
           (error (condition)
             (setf failure condition)
             (error condition)))
      (funcall (http3-client-close-stream client)
               stream :condition failure))))

(defun %h3-server-pseudo-field-p (name)
  (and (plusp (length name))
       (char= (char name 0) #\:)))

(defun %h3-server-parse-request-fields (fields &key enable-connect-p)
  (let ((pseudo '())
        (headers '())
        (host-values '())
        (regular-seen-p nil))
    (dolist (field fields)
      (unless (and (consp field)
                   (stringp (car field))
                   (stringp (cdr field)))
        (%h3-transport-error
         "HTTP/3 request fields must be name/value pairs."
         field))
      (let ((name (car field))
            (value (cdr field)))
        (unless (http-kit::%header-value-p value)
          (%h3-invalid-header name "field values cannot contain controls."))
        (if (%h3-server-pseudo-field-p name)
            (progn
              (when regular-seen-p
                (%h3-transport-error
                 "HTTP/3 pseudo-fields must precede regular fields."
                 name))
              (unless (member name '(":method" ":protocol" ":scheme" ":authority" ":path")
                       :test #'string=)
                (%h3-transport-error
                 "An HTTP/3 request contains an unknown pseudo-field."
                 :h3-message-error))
              (when (assoc name pseudo :test #'string=)
                (%h3-transport-error
                 "HTTP/3 request pseudo-fields must be unique."
                 name))
              (push (cons name value) pseudo))
            (progn
              (setf regular-seen-p t)
              (unless (and (string= name (string-downcase name))
                           (http-kit::%header-name-p name))
                (%h3-invalid-header
                 name "HTTP/3 field names must be lowercase ASCII tokens."))
              (when (%h3-connection-specific-name-p name)
                (%h3-invalid-header
                 name "connection-specific fields are forbidden."))
              (when (and (string= name "te")
                         (not (%h3-te-value-p value)))
                (%h3-invalid-header
                 name "HTTP/3 permits only the trailers value for te."))
              (let ((header (http-kit:make-http-header name value)))
                (push header headers)
                (when (string= name "host")
                  (push (http-kit:http-header-content header) host-values)))))))
    (setf pseudo (nreverse pseudo)
          headers (nreverse headers))
    (let* ((method (cdr (assoc ":method" pseudo :test #'string=)))
           (protocol (cdr (assoc ":protocol" pseudo :test #'string=)))
           (scheme (cdr (assoc ":scheme" pseudo :test #'string=)))
           (authority-field (assoc ":authority" pseudo :test #'string=))
           (authority (cdr authority-field))
           (path (cdr (assoc ":path" pseudo :test #'string=)))
           (target nil)
           (uri-scheme scheme)
           (uri-path nil)
           (uri-query nil))
      (unless (and method (http-kit::%token-p method))
        (%h3-transport-error
         "An HTTP/3 request must contain a valid :method pseudo-field."))
      (when (and host-values
                 (or (/= (length host-values) 1)
                     (zerop (length (first host-values)))))
        (%h3-invalid-header
         "host" "host must contain exactly one non-empty value."))
      (cond
        (authority-field
         (when (zerop (length authority))
           (%h3-transport-error
            "An HTTP/3 :authority pseudo-field must not be empty."))
         (when (and host-values
                    (not (string-equal (first host-values) authority)))
           (%h3-invalid-header "host" "host must match :authority.")))
        (host-values
         (setf authority (first host-values)))
        (t
         (%h3-transport-error
          "An HTTP/3 request must contain a non-empty :authority or host field.")))
      (when protocol
        (unless (and (string= method "CONNECT")
                     (http-kit::%token-p protocol))
          (%h3-transport-error
           "The :protocol pseudo-field is valid only for CONNECT and must be a token."))
        (unless enable-connect-p
          (%h3-transport-error
           "Extended CONNECT was not enabled by local SETTINGS."
           :h3-message-error)))
      (if (and (string= method "CONNECT") protocol)
          (progn
            (unless (and scheme
                         (member scheme '("http" "https") :test #'string=))
              (%h3-transport-error
               "An extended CONNECT request requires an http or https :scheme."))
            (unless path
              (%h3-transport-error
               "An extended CONNECT request requires a :path pseudo-field."))
            (unless (and (plusp (length path))
                         (char= (char path 0) #\/))
              (%h3-transport-error
               "An extended CONNECT :path must be an origin-form path."))
            (let ((query-position (position #\? path)))
              (setf uri-scheme scheme
                    uri-path (if query-position
                                 (subseq path 0 query-position)
                                 path)
                    uri-query (and query-position
                                   (subseq path (1+ query-position)))
                    target path)))
          (if (string= method "CONNECT")
          (progn
            (unless authority-field
              (%h3-transport-error
               "A CONNECT request requires a non-empty :authority pseudo-field."))
            (when (or scheme path)
              (%h3-transport-error
               "A CONNECT request must not contain :scheme or :path."))
            (setf uri-scheme "http"
                  uri-path "/"
                  target authority))
          (progn
            (unless (and scheme
                         (member scheme '("http" "https") :test #'string=))
              (%h3-transport-error
               "An HTTP/3 request requires an http or https :scheme."))
            (unless path
              (%h3-transport-error
               "An HTTP/3 request requires a :path pseudo-field."))
            (if (string= path "*")
                (progn
                  (unless (string= method "OPTIONS")
                    (%h3-transport-error
                     "The asterisk-form HTTP/3 target is valid only for OPTIONS."))
                  (setf uri-path "/"
                        uri-query nil
                        target path))
                (progn
                  (unless (and (plusp (length path))
                               (char= (char path 0) #\/))
                    (%h3-transport-error
                     "An HTTP/3 :path must be an origin-form path or *."))
                  (let ((query-position (position #\? path)))
                    (setf uri-path (if query-position
                                       (subseq path 0 query-position)
                                       path)
                          uri-query (and query-position
                                         (subseq path (1+ query-position)))
                          target path)))))))
      (values method
              protocol
              uri-scheme
              authority
              target
              uri-path
              uri-query
              headers))))

(defun %h3-read-push-stream-prefix (client stream timeout deadline)
  (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)))
        (position 0)
        (values '()))
    (loop
      (loop while (< (length values) 2)
            do (multiple-value-bind (value end)
                   (http3-varint-decode
                    buffer :position position :allow-incomplete-p t)
                 (unless value
                   (return))
                 (push value values)
                 (setf position end)))
      (when (= (length values) 2)
        (return (values (second values) (first values)
                        (subseq buffer position) nil)))
      (multiple-value-bind (chunk fin-p)
          (%h3-read-client-stream client stream timeout deadline)
        (when chunk
          (unless (%http3-octet-vector-p chunk)
            (%h3-transport-error
             "HTTP/3 read-stream must return an octet vector or NIL."
             (type-of chunk)))
          (setf buffer (%http3-concatenate-octets buffer chunk)))
        (when (or fin-p (null chunk))
          (loop while (< (length values) 2)
                do (multiple-value-bind (value end)
                       (http3-varint-decode
                        buffer :position position :allow-incomplete-p t)
                     (unless value
                       (%h3-transport-error
                        "An HTTP/3 push stream ended with a truncated prefix."
                        :h3-stream-creation-error))
                     (push value values)
                     (setf position end)))
          (return (values (second values) (first values)
                          (subseq buffer position) t)))))))

(defun receive-http3-push
    (client stream &key stream-id on-body-chunk (collect-body-p t)
            max-header-bytes max-fields max-body-bytes
            qpack-decoder-table qpack-decoder-context
            await-qpack await-push-promise timeout deadline)
  "Read one server-initiated HTTP/3 push stream and return its push ID and response.

STREAM must begin with the unidirectional push-stream type.  If it arrives
before the corresponding PUSH_PROMISE, AWAIT-PUSH-PROMISE is called with the
push ID and must wait until that promise has been processed.  Each promised ID
can be consumed only once."
  (unless (http3-client-p client)
    (%h3-transport-error "RECEIVE-HTTP3-PUSH requires an HTTP/3 client."
                         (type-of client)))
  (unless (http3-client-open-p client)
    (%h3-transport-error "The HTTP/3 client is already closed."))
  (unless stream
    (%h3-transport-error "RECEIVE-HTTP3-PUSH requires a QUIC stream."))
  (unless (or (null on-body-chunk) (functionp on-body-chunk))
    (%h3-transport-error "ON-BODY-CHUNK must be a function or NIL."
                         on-body-chunk))
  (unless (or (null await-push-promise) (functionp await-push-promise))
    (%h3-transport-error "AWAIT-PUSH-PROMISE must be a function or NIL."
                         await-push-promise))
  (unless (or (null max-header-bytes)
              (%h3-positive-limit-p max-header-bytes))
    (%h3-transport-error
     "max-header-bytes must be NIL or a positive integer."
     max-header-bytes))
  (unless (or (null max-fields) (%h3-positive-limit-p max-fields))
    (%h3-transport-error "max-fields must be NIL or a positive integer."
                         max-fields))
  (unless (or (null max-body-bytes) (%h3-non-negative-limit-p max-body-bytes))
    (%h3-transport-error "max-body-bytes must be NIL or a non-negative integer."
                         max-body-bytes))
  (let ((failure nil))
    (unwind-protect
         (handler-case
             (multiple-value-bind (stream-type push-id remainder fin-p)
                 (%h3-read-push-stream-prefix client stream timeout deadline)
               (unless (= stream-type +http3-push-stream-type+)
                 (%h3-transport-error
                  "A server push stream must use the HTTP/3 push stream type."
                  :h3-stream-creation-error))
               (labels ((claim-promise ()
                          (%call-http3-serialized
                           client
                           (lambda ()
                             (let ((entry
                                     (assoc
                                      push-id
                                      (http3-client-push-promises client)
                                      :test #'=)))
                               (when entry
                                 (when (member
                                        push-id
                                        (http3-client-cancelled-push-ids client)
                                        :test #'=)
                                   (let ((cancel
                                           (http3-client-cancel-stream client)))
                                     (when cancel
                                       (funcall cancel stream
                                                :error-code #x10c
                                                :timeout timeout
                                                :deadline deadline)))
                                   (%h3-transport-error
                                    "An HTTP/3 push stream arrived after cancellation."
                                    :h3-request-cancelled))
                                 (when (member
                                        push-id
                                        (http3-client-consumed-push-ids client)
                                        :test #'=)
                                   (%h3-transport-error
                                    "An HTTP/3 push ID cannot be consumed more than once."
                                    :h3-id-error))
                                 (push push-id
                                       (http3-client-consumed-push-ids client))
                                 (copy-tree (cdr entry)))))))))
                 (let ((promise (claim-promise)))
                   (unless promise
                     (when await-push-promise
                       (funcall await-push-promise push-id)
                       (setf promise (claim-promise)))
                     (unless promise
                       (%h3-transport-error
                        "An HTTP/3 push stream used an unpromised push ID."
                        :h3-id-error)))
                 (let* ((original-reader (http3-client-read-stream client))
                        (first-read-p t)
                        (push-client (copy-http3-client client))
                        (decoder-table
                          (or qpack-decoder-table
                              (http3-client-qpack-decoder-table client)))
                        (decoder-context
                          (or qpack-decoder-context
                              (and await-qpack
                                   (eq decoder-table
                                       (http3-client-qpack-decoder-table client))
                                   (http3-client-qpack-decoder-context client)))))
                   (setf (http3-client-read-stream push-client)
                         (lambda (read-stream &key timeout deadline)
                           (if first-read-p
                               (progn
                                 (setf first-read-p nil)
                                 (values remainder fin-p))
                               (funcall original-reader read-stream
                                        :timeout timeout :deadline deadline))))
                   (values
                    push-id
                    (%h3-read-response
                     push-client stream
                     :on-body-chunk on-body-chunk
                     :collect-body-p collect-body-p
                     :max-header-bytes max-header-bytes
                     :max-fields max-fields
                     :max-body-bytes max-body-bytes
                     :qpack-decoder-table decoder-table
                     :qpack-decoder-context decoder-context
                     :stream-id stream-id
                     :await-qpack await-qpack
                     :request-method (cdr (assoc ":method" promise :test #'string=))
                     :timeout timeout
                     :deadline deadline)))))
           (error (condition)
             (setf failure condition)
             (error condition)))
      (funcall (http3-client-close-stream client)
               stream :condition failure))))

(defun %h3-server-parse-trailer-fields (fields)
  (let ((headers '()))
    (dolist (field fields (nreverse headers))
      (unless (and (consp field)
                   (stringp (car field))
                   (stringp (cdr field)))
        (%h3-transport-error
         "HTTP/3 trailer fields must be name/value pairs."
         field))
      (let ((name (car field))
            (value (cdr field)))
        (when (%h3-server-pseudo-field-p name)
          (%h3-transport-error
           "HTTP/3 request trailers cannot contain pseudo-fields."
           name))
        (unless (and (string= name (string-downcase name))
                     (http-kit::%header-name-p name))
          (%h3-invalid-header
           name "HTTP/3 field names must be lowercase ASCII tokens."))
        (when (http-kit::%forbidden-trailer-field-name-p name)
          (%h3-invalid-header
           name "the field is forbidden in HTTP/3 request trailers."))
        (push (http-kit:make-http-header name value) headers)))))

(defun %h3-server-make-request
    (request-info body body-length trailers collect-body-p)
  (destructuring-bind
      (method protocol scheme authority target path query headers)
      request-info
    (%h3-content-length headers body-length t)
    (when (and (string= method "TRACE") (plusp body-length))
      (%h3-transport-error "TRACE requests must not contain content."
                           :trace-content))
    (http-kit:make-http-request
     :method method
     :protocol protocol
     :uri (http-kit:make-http-uri
           :scheme scheme
           :authority authority
           :path path
           :query query)
     :request-target target
     :headers headers
     :trailers trailers
     :body (if collect-body-p
               (%http3-copy-octets body)
               (make-array 0 :element-type '(unsigned-byte 8)))
     :protocol-version "HTTP/3")))

(defun %h3-server-response-fields (status headers)
  (let ((fields (list (cons ":status" (princ-to-string status))))
        (regular '()))
    (dolist (header headers)
      (unless (http-kit:http-header-p header)
        (%h3-transport-error
         "HTTP/3 response headers must be HTTP-HEADER values."
         (type-of header)))
      (let ((name (%h3-header-name header))
            (value (http-kit:http-header-content header)))
        (when (%h3-connection-specific-name-p name)
          (%h3-invalid-header
           name "connection-specific fields are forbidden."))
        (when (string= name "te")
          (%h3-invalid-header
           name "HTTP/3 TE is permitted only in request header sections."))
        (push (cons name value) regular)))
    (append fields (nreverse regular))))

(defun %h3-server-response-trailer-fields (headers)
  (let ((fields '()))
    (dolist (header headers (nreverse fields))
      (unless (http-kit:http-header-p header)
        (%h3-transport-error
         "HTTP/3 response trailers must be HTTP-HEADER values."
         (type-of header)))
      (let ((name (%h3-header-name header))
            (value (http-kit:http-header-content header)))
        (when (http-kit::%forbidden-trailer-field-name-p name)
          (%h3-invalid-header
           name "the field is forbidden in HTTP/3 response trailers."))
        (push (cons name value) fields)))))

(defun %h3-server-write-frame
    (stream write-stream frame max-frame-size &key fin-p timeout deadline)
  (let ((payload (http3-frame-payload frame)))
    (when (> (length payload) max-frame-size)
      (%h3-size-error :frame max-frame-size (length payload)))
    (funcall write-stream stream (encode-http3-frame frame)
             :fin-p fin-p :timeout timeout :deadline deadline)))

(defun send-http3-push
    (request-stream promised-request response control-state
     &key push-id open-stream write-stream close-stream cancel-stream
       (serialize (lambda (thunk) (funcall thunk))) qpack-encoder-table
       (max-frame-size +http3-default-max-frame-size+)
       (max-header-bytes 65536) (huffman-p nil) timeout deadline)
  "Promise and send one HTTP/3 server push over caller-supplied QUIC callbacks.

REQUEST-STREAM is the client-initiated stream that caused the push.  The
PUSH_PROMISE is written there before OPEN-STREAM is called for a new
unidirectional push stream.  CONTROL-STATE must be the state populated by
SERVE-HTTP3-CONTROL-STREAM for a client peer.  PUSH-ID defaults to the lowest
unused ID.  PROMISED-REQUEST must be GET or HEAD without content or trailers.

SERIALIZE protects push-ID allocation, the PUSH_PROMISE write, and its state
commit.  On failure after opening the push stream, CANCEL-STREAM is called with
H3_REQUEST_CANCELLED when supplied.  CLOSE-STREAM is called exactly once for
an opened push stream.  Returns the push ID and RESPONSE."
  (unless (http-kit:http-request-p promised-request)
    (%h3-transport-error "SEND-HTTP3-PUSH requires an HTTP request."
                         (type-of promised-request)))
  (unless (http3-control-state-p control-state)
    (%h3-transport-error "SEND-HTTP3-PUSH requires HTTP/3 control state."
                         (type-of control-state)))
  (unless (eq (http3-control-state-peer-role control-state) :client)
    (%h3-transport-error
     "HTTP/3 server push requires control state for a client peer."))
  (dolist (callback (list open-stream write-stream serialize))
    (unless (functionp callback)
      (%h3-transport-error
       "HTTP/3 server push requires open-stream, write-stream, and serialize callbacks."
       (type-of callback))))
  (dolist (callback (list close-stream cancel-stream))
    (unless (or (null callback) (functionp callback))
      (%h3-transport-error
       "HTTP/3 server push close-stream and cancel-stream must be functions or NIL."
       (type-of callback))))
  (unless (%h3-positive-limit-p max-frame-size)
    (%h3-transport-error "max-frame-size must be a positive integer."
                         max-frame-size))
  (unless (%h3-non-negative-limit-p max-header-bytes)
    (%h3-transport-error "max-header-bytes must be a non-negative integer."
                         max-header-bytes))
  (unless (member (http-kit:http-request-method promised-request)
                  '("GET" "HEAD") :test #'string=)
    (%h3-transport-error "An HTTP/3 pushed request must be safe and cacheable."
                         (http-kit:http-request-method promised-request)))
  (when (or (plusp (length (http-kit:http-request-body promised-request)))
            (http-kit:http-request-trailers promised-request))
    (%h3-transport-error
     "An HTTP/3 pushed request cannot contain content or trailers."))
  (let ((selected-push-id nil)
        (push-stream nil)
        (failure nil))
    (funcall
     serialize
     (lambda ()
       (let* ((maximum (http3-control-state-max-push-id control-state))
              (promised (http3-control-state-promised-push-ids control-state))
              (candidate
                (or push-id
                    (loop for id from 0
                          unless (member id promised :test #'=)
                            return id))))
         (unless (and (integerp candidate)
                      (<= 0 candidate +http3-max-varint+))
           (%h3-transport-error "The HTTP/3 push ID is invalid."
                                candidate))
         (unless (and maximum (<= candidate maximum))
           (%h3-transport-error
            "The HTTP/3 push ID exceeds the client's MAX_PUSH_ID."
            :h3-id-error))
         (when (member candidate promised :test #'=)
           (%h3-transport-error "The HTTP/3 push ID was already promised."
                                :h3-id-error))
         (when (member candidate
                       (http3-control-state-cancelled-push-ids control-state)
                       :test #'=)
           (%h3-transport-error "The HTTP/3 push was cancelled."
                                :h3-request-cancelled))
         (let* ((field-section
                  (qpack-encode-field-section
                   (%h3-request-fields promised-request)
                   :dynamic-table qpack-encoder-table
                   :huffman-p huffman-p))
                (payload
                  (%http3-concatenate-octets
                   (http3-varint-encode candidate) field-section)))
           (when (> (length field-section) max-header-bytes)
             (%h3-size-error :headers max-header-bytes
                             (length field-section)))
           (%h3-server-write-frame
            request-stream write-stream
            (make-http3-frame :type +http3-push-promise-type+
                              :payload payload)
            max-frame-size :timeout timeout :deadline deadline)
           (push candidate
                 (http3-control-state-promised-push-ids control-state))
           (setf selected-push-id candidate)))))
    (when (member selected-push-id
                  (http3-control-state-cancelled-push-ids control-state)
                  :test #'=)
      (%h3-transport-error "The HTTP/3 push was cancelled."
                           :h3-request-cancelled))
    (setf push-stream
          (funcall open-stream promised-request :stream-type :push
                   :timeout timeout :deadline deadline))
    (unless push-stream
      (%h3-transport-error
       "The HTTP/3 open-stream callback returned NIL for a push stream."))
    (unwind-protect
         (handler-case
             (progn
               (funcall write-stream push-stream
                        (%http3-concatenate-octets
                         (http3-varint-encode +http3-push-stream-type+)
                         (http3-varint-encode selected-push-id))
                        :fin-p nil :timeout timeout :deadline deadline)
               (%h3-server-send-response
                push-stream write-stream response promised-request
                max-frame-size max-header-bytes qpack-encoder-table huffman-p
                timeout deadline)
               (values selected-push-id response))
           (error (condition)
             (setf failure condition)
             (when cancel-stream
               (funcall cancel-stream push-stream :error-code #x10c
                        :timeout timeout :deadline deadline))
             (error condition)))
      (when close-stream
        (funcall close-stream push-stream :condition failure)))))

(defun %h3-server-send-information
    (stream write-stream response max-frame-size max-header-bytes
            qpack-encoder-table huffman-p timeout deadline)
  (let* ((stream-response-p (http-kit:http-response-stream-p response))
         (ordinary-response-p (http-kit:http-response-p response))
         (status (if stream-response-p
                     (http-kit:http-response-stream-status response)
                     (and ordinary-response-p
                          (http-kit:http-response-status response))))
         (headers (if stream-response-p
                      (http-kit:http-response-stream-headers response)
                      (and ordinary-response-p
                           (http-kit:http-response-headers response))))
         (trailers (if stream-response-p
                       (http-kit:http-response-stream-trailers response)
                       (and ordinary-response-p
                            (http-kit:http-response-trailers response))))
         (body (and ordinary-response-p
                    (http-kit:http-response-body response)))
         (body-function
           (and stream-response-p
                (http-kit:http-response-stream-body-function response)))
         (declared-body-length
           (and stream-response-p
                (http-kit:http-response-stream-body-length response))))
    (unless (or ordinary-response-p stream-response-p)
      (%h3-transport-error
       "HTTP/3 informational responses must be HTTP responses or response streams."
       (type-of response)))
    (unless (and (integerp status) (<= 100 status 199) (/= status 101))
      (%h3-transport-error
       "An HTTP/3 informational response must have status 100-199 other than 101."
       status))
    (when trailers
      (%h3-transport-error
       "An HTTP/3 informational response cannot contain trailers."))
    (when (or (and body (plusp (length body)))
              (and declared-body-length (plusp declared-body-length)))
      (%h3-transport-error
       "An HTTP/3 informational response cannot contain a body."))
    (when body-function
      (loop for chunk = (funcall body-function)
            while chunk
            do (unless (%http3-octet-vector-p chunk)
                 (%h3-transport-error
                  "HTTP/3 response body functions must return octet vectors or NIL."
                  (type-of chunk)))
               (when (plusp (length chunk))
                 (%h3-transport-error
                  "An HTTP/3 informational response cannot contain a body."))))
    (%h3-response-content-length headers 0 :status status :no-body t)
    (let* ((fields (%h3-server-response-fields status headers))
           (header-block
             (qpack-encode-field-section
              fields :dynamic-table qpack-encoder-table :huffman-p huffman-p)))
      (when (> (length header-block) max-header-bytes)
        (%h3-size-error :headers max-header-bytes (length header-block)))
      (%h3-server-write-frame
       stream write-stream
       (make-http3-frame :type +http3-headers-type+ :payload header-block)
       max-frame-size :fin-p nil :timeout timeout :deadline deadline))
    response))

(defun %h3-server-send-response
    (stream write-stream response request max-frame-size max-header-bytes
            qpack-encoder-table huffman-p timeout deadline)
  (let* ((stream-response-p (http-kit:http-response-stream-p response))
         (ordinary-response-p (http-kit:http-response-p response))
         (status (if stream-response-p
                     (http-kit:http-response-stream-status response)
                     (and ordinary-response-p
                          (http-kit:http-response-status response))))
         (headers (if stream-response-p
                      (http-kit:http-response-stream-headers response)
                      (and ordinary-response-p
                           (http-kit:http-response-headers response))))
         (trailers (if stream-response-p
                       (http-kit:http-response-stream-trailers response)
                       (and ordinary-response-p
                            (http-kit:http-response-trailers response))))
         (body (and ordinary-response-p
                     (http-kit:http-response-body response)))
         (body-function (and stream-response-p
                             (http-kit:http-response-stream-body-function
                              response)))
         (declared-body-length
           (and stream-response-p
                (http-kit:http-response-stream-body-length response)))
         (method (http-kit:http-request-method request))
         (body-suppressed-p
           (and (integerp status)
                (%h3-no-body-response-p method status)))
         (header-fields nil)
         (trailer-fields nil)
         (header-block nil)
         (trailer-block nil)
         (body-done-p nil)
         (actual-body-length 0))
    (unless (or ordinary-response-p stream-response-p)
      (%h3-transport-error
       "An HTTP/3 handler must return an HTTP response or response stream."
       (type-of response)))
    (unless (and (integerp status) (<= 200 status 599))
      (%h3-transport-error
       "HTTP/3 handlers must return a final status from 200 through 599."
       status))
    (when (and body-suppressed-p
               (or (and body (plusp (length body)))
                   (and declared-body-length (plusp declared-body-length))))
      (unless (string= method "HEAD")
        (%h3-transport-error
         "This HTTP/3 response cannot contain a body.")))
    (cond
      (ordinary-response-p
       (%h3-response-content-length
        headers (length body) :status status :request-method method
        :no-body body-suppressed-p))
      (body-suppressed-p
       (%h3-response-content-length
        headers 0 :status status :request-method method :no-body t))
      (declared-body-length
       (%h3-response-content-length
        headers declared-body-length :status status :request-method method))
      ((http-kit:http-header-values headers "content-length")
       (%h3-transport-error
        "An HTTP/3 response stream with Content-Length requires body-length.")))
    (setf header-fields (%h3-server-response-fields status headers)
          trailer-fields (%h3-server-response-trailer-fields trailers)
          header-block
            (qpack-encode-field-section
             header-fields
             :dynamic-table qpack-encoder-table
             :huffman-p huffman-p))
    (when (> (length header-block) max-header-bytes)
      (%h3-size-error :headers max-header-bytes (length header-block)))
    (when trailer-fields
      (setf trailer-block
              (qpack-encode-field-section
               trailer-fields
               :dynamic-table qpack-encoder-table
               :huffman-p huffman-p))
      (when (> (length trailer-block) max-header-bytes)
        (%h3-size-error :headers max-header-bytes (length trailer-block))))
    (labels
        ((next-body-chunk ()
           (if body-suppressed-p
               nil
               (if stream-response-p
                   (loop
                     (when body-done-p
                       (return nil))
                     (let ((chunk (funcall body-function)))
                       (cond
                         ((null chunk)
                          (setf body-done-p t)
                          (return nil))
                         ((not (%http3-octet-vector-p chunk))
                          (%h3-transport-error
                           "HTTP/3 response body functions must return octet vectors or NIL."
                           (type-of chunk)))
                         ((zerop (length chunk)) nil)
                         (t (return (%http3-copy-octets chunk))))))
                   (progn
                     (if body-done-p
                         nil
                         (progn
                           (setf body-done-p t)
                           (if (plusp (length body))
                               (%http3-copy-octets body)
                                nil))))))))
      (let ((pending (next-body-chunk)))
        (%h3-server-write-frame
         stream write-stream
         (make-http3-frame :type +http3-headers-type+ :payload header-block)
         max-frame-size
         :fin-p (and (null pending) (null trailer-fields))
         :timeout timeout :deadline deadline)
        (loop while pending
              do (let ((position 0)
                       (length (length pending)))
                   (loop while (< position length)
                         do (let* ((end (min length
                                             (+ position max-frame-size)))
                                   (last-piece-p (= end length))
                                   (piece (subseq pending position end)))
                              (if last-piece-p
                                  (let ((next (next-body-chunk)))
                                    (incf actual-body-length (length piece))
                                    (%h3-server-write-frame
                                     stream write-stream
                                     (make-http3-frame
                                      :type +http3-data-type+
                                      :payload piece)
                                     max-frame-size
                                     :fin-p (and (null next)
                                                 (null trailer-fields))
                                     :timeout timeout :deadline deadline)
                                    (setf position end
                                          pending next))
                                  (progn
                                    (incf actual-body-length (length piece))
                                    (%h3-server-write-frame
                                     stream write-stream
                                     (make-http3-frame
                                      :type +http3-data-type+
                                      :payload piece)
                                     max-frame-size
                                     :fin-p nil
                                     :timeout timeout :deadline deadline)
                                    (setf position end))))
                         (when (null pending)
                           (return)))))
        (when trailer-fields
          (%h3-server-write-frame
           stream write-stream
           (make-http3-frame :type +http3-headers-type+ :payload trailer-block)
           max-frame-size :fin-p t :timeout timeout :deadline deadline))
        (when (and (not body-suppressed-p)
                   declared-body-length
                   (/= declared-body-length actual-body-length))
          (%h3-transport-error
           "HTTP/3 response stream body-length does not match emitted bytes."
           (list declared-body-length actual-body-length)))
        (%h3-response-content-length
         headers actual-body-length :status status :request-method method
         :no-body body-suppressed-p)
        response))))

(defun serve-http3-request-stream
    (stream handler &key read-stream write-stream close-stream
            (max-frame-size +http3-default-max-frame-size+)
            (max-header-bytes 65536) (max-fields 256) max-body-bytes
            (collect-body-p t) on-body-chunk qpack-decoder-table
            qpack-decoder-context stream-id await-qpack
            (qpack-serialize (lambda (thunk) (funcall thunk)))
            qpack-encoder-table (huffman-p nil) (enable-connect-p nil)
            timeout deadline on-error)
  "Serve one HTTP/3 request stream over caller-supplied QUIC callbacks.

READ-STREAM is called as (STREAM &KEY TIMEOUT DEADLINE) and returns an octet
vector and a FIN boolean.  WRITE-STREAM is called as
(STREAM OCTETS &KEY FIN-P TIMEOUT DEADLINE).  CLOSE-STREAM, when supplied, is
called as (STREAM &KEY CONDITION) exactly once.  The callbacks own QUIC,
TLS 1.3, ALPN, packet loss recovery, flow control, and socket behavior; this
function owns the HTTP/3 request stream framing, QPACK field validation,
request-body limits, and response framing.  QPACK-DECODER-TABLE and
QPACK-ENCODER-TABLE are caller-owned dynamic tables for request and response
field sections.  QPACK-DECODER-CONTEXT enables blocked request field sections;
STREAM-ID identifies this request stream, AWAIT-QPACK waits for and resumes a
blocked token, and QPACK-SERIALIZE protects the connection-wide context.
HUFFMAN-P enables Huffman encoding for response fields.

HANDLER receives one HTTP request and must return an HTTP response or response
stream as its first value.  Its optional second value is a list of informational
HTTP responses sent before the final response.  ON-BODY-CHUNK, when supplied,
receives each request DATA payload.  If
COLLECT-BODY-P is false, handlers receive an empty request body while the
body-length and Content-Length checks still use all received bytes.  ON-ERROR
receives (CONDITION REQUEST), where REQUEST is NIL if parsing failed before the
request model was constructed."
  (dolist (callback (list handler read-stream write-stream))
    (unless (functionp callback)
      (%h3-transport-error
       "HTTP/3 server handler, read-stream, and write-stream callbacks are required."
       (type-of callback))))
  (unless (or (null close-stream) (functionp close-stream))
    (%h3-transport-error
     "HTTP/3 server close-stream must be a function or NIL."
     (type-of close-stream)))
  (unless (%h3-positive-limit-p max-frame-size)
    (%h3-transport-error
     "HTTP/3 server max-frame-size must be a positive QUIC varint."
     max-frame-size))
  (unless (%h3-positive-limit-p max-header-bytes)
    (%h3-transport-error
     "HTTP/3 server max-header-bytes must be a positive integer."
     max-header-bytes))
  (unless (and (integerp max-fields) (plusp max-fields))
    (%h3-transport-error
     "HTTP/3 server max-fields must be a positive integer."
     max-fields))
  (unless (or (null max-body-bytes) (%h3-non-negative-limit-p max-body-bytes))
    (%h3-transport-error
     "HTTP/3 server max-body-bytes must be NIL or a non-negative integer."
     max-body-bytes))
  (unless (member collect-body-p '(t nil))
    (%h3-transport-error
     "HTTP/3 server collect-body-p must be boolean."
     collect-body-p))
  (unless (or (null on-body-chunk) (functionp on-body-chunk))
    (%h3-transport-error
     "HTTP/3 server on-body-chunk must be a function or NIL."
     (type-of on-body-chunk)))
  (unless (or (null on-error) (functionp on-error))
    (%h3-transport-error
     "HTTP/3 server on-error must be a function or NIL."
     (type-of on-error)))
  (when qpack-decoder-context
    (unless (and (integerp stream-id) (>= stream-id 0))
      (%h3-transport-error
       "Dynamic QPACK request decoding requires a numeric stream ID."
       stream-id))
    (unless (functionp await-qpack)
      (%h3-transport-error
       "Dynamic QPACK request decoding requires an await-qpack callback."
       await-qpack))
    (unless (functionp qpack-serialize)
      (%h3-transport-error
       "Dynamic QPACK request decoding requires a serializer."
       qpack-serialize)))
  (let ((closer (or close-stream
                    (lambda (ignored-stream &key condition)
                      (declare (ignore ignored-stream condition))
                      nil)))
        (buffer (make-array 0 :element-type '(unsigned-byte 8)))
        (body (make-array 0 :element-type '(unsigned-byte 8)
                          :adjustable t :fill-pointer 0))
        (body-length 0)
        (request-info nil)
        (request-headers-seen-p nil)
        (request-trailers nil)
        (request-trailers-seen-p nil)
        (request nil)
        (failure nil))
    (unwind-protect
         (handler-case
             (loop
               (multiple-value-bind (chunk fin-p)
                   (funcall read-stream stream
                            :timeout timeout :deadline deadline)
                 (when chunk
                   (unless (%http3-octet-vector-p chunk)
                     (%h3-transport-error
                      "HTTP/3 read-stream must return an octet vector or NIL."
                      (type-of chunk)))
                   (when (plusp (length chunk))
                     (setf buffer (%http3-concatenate-octets buffer chunk))
                     (multiple-value-bind (frames remainder)
                         (decode-http3-frames
                          buffer :allow-incomplete-p t
                          :max-frame-size max-frame-size)
                       (setf buffer remainder)
                       (dolist (frame frames)
                         (let ((type (http3-frame-type frame))
                               (payload (http3-frame-payload frame)))
                           (cond
                             ((= type +http3-headers-type+)
                              (let ((fields
                                      (if qpack-decoder-context
                                          (%h3-decode-dynamic-field-section
                                           payload qpack-decoder-context
                                           stream-id await-qpack qpack-serialize
                                           max-header-bytes max-fields)
                                          (qpack-decode-field-section
                                           payload
                                           :max-header-bytes max-header-bytes
                                           :max-fields max-fields
                                           :dynamic-table qpack-decoder-table))))
                                (if (not request-headers-seen-p)
                                    (progn
                                      (multiple-value-bind
                                            (method protocol scheme authority target path query headers)
                                          (%h3-server-parse-request-fields
                                           fields :enable-connect-p enable-connect-p)
                                        (setf request-info
                                              (list method protocol scheme authority
                                                    target path query headers)))
                                      (setf request-headers-seen-p t))
                                    (progn
                                      (when request-trailers-seen-p
                                        (%h3-transport-error
                                         "An HTTP/3 request cannot contain multiple trailer blocks."))
                                      (setf request-trailers
                                            (%h3-server-parse-trailer-fields fields)
                                            request-trailers-seen-p t)))))
                             ((= type +http3-data-type+)
                              (unless request-headers-seen-p
                                (%h3-transport-error
                                 "An HTTP/3 DATA frame arrived before request HEADERS."))
                              (when request-trailers-seen-p
                                (%h3-transport-error
                                 "An HTTP/3 DATA frame arrived after request trailers."))
                              (let ((new-length (+ body-length (length payload))))
                                (when (and max-body-bytes
                                           (> new-length max-body-bytes))
                                  (%h3-size-error :body max-body-bytes new-length))
                                (setf body-length new-length)
                                (when collect-body-p
                                  (loop for octet across payload
                                        do (vector-push-extend octet body)))
                                (when on-body-chunk
                                  (funcall on-body-chunk
                                           (%http3-copy-octets payload)))))
                             ((member type (list +http3-settings-type+
                                                 +http3-cancel-push-type+
                                                 +http3-goaway-type+
                                                 +http3-max-push-id-type+
                                                 +http3-push-promise-type+
                                                 +http3-priority-update-request-type+
                                                 +http3-priority-update-push-type+)
                                      :test #'=)
                              (%h3-transport-error
                               "HTTP/3 control or push frames are invalid on a request stream."
                               type))
                             (t
                              ;; Extension frames are ignored, as required by
                              ;; the HTTP/3 frame extensibility rules.
                              nil)))))))
                 (when (or fin-p (null chunk))
                   (when (plusp (length buffer))
                     (%h3-transport-error
                      "The HTTP/3 request stream ended with a truncated frame."))
                   (unless request-headers-seen-p
                     (%h3-transport-error
                      "The HTTP/3 request stream ended before request HEADERS."))
                   (setf request
                         (%h3-server-make-request
                          request-info body body-length request-trailers
                          collect-body-p))
                   (return
                     (multiple-value-bind (response information)
                         (funcall handler request)
                       (unless (listp information)
                         (%h3-transport-error
                          "HTTP/3 informational responses must be supplied as a list."
                          (type-of information)))
                       (dolist (informational-response information)
                         (%h3-server-send-information
                          stream write-stream informational-response
                          max-frame-size max-header-bytes qpack-encoder-table
                          huffman-p timeout deadline))
                       (%h3-server-send-response
                        stream write-stream response request
                        max-frame-size max-header-bytes
                        qpack-encoder-table huffman-p
                        timeout deadline))))))
           (error (condition)
             (setf failure condition)
             (when on-error
               (http-kit::%with-http-cleanup
                 (funcall on-error condition request)))
             (error condition)))
      (funcall closer stream :condition failure))))

(defstruct (%http3-managed-client
             (:constructor %make-http3-managed-client
                 (&key key client opened-at last-used last-used-at)))
  key client opened-at last-used last-used-at)

(defstruct (http3-connection-manager
             (:conc-name %http3-connection-manager-)
             (:constructor %make-http3-connection-manager
                 (&key open-client max-connections idle-timeout
                       max-connection-age clock-function)))
  open-client max-connections idle-timeout max-connection-age clock-function
  (entries nil)
  (sequence 0)
  (closed-p nil))

(defun make-http3-connection-manager
    (&key open-client (max-connections 8) idle-timeout max-connection-age
          (clock-function #'http-kit::%monotonic-time))
  "Create a cooperative pool of reusable HTTP/3 clients.

OPEN-CLIENT receives the request and TIMEOUT and DEADLINE keyword arguments,
and must return an open HTTP3-CLIENT. Connections are keyed by request scheme,
host, and port unless CONNECTION-KEY is supplied when sending. The manager is
owner-thread and non-reentrant."
  (unless (functionp open-client)
    (%h3-transport-error
     "MAKE-HTTP3-CONNECTION-MANAGER requires an open-client callback."
     open-client))
  (unless (and (integerp max-connections) (plusp max-connections))
    (%h3-transport-error "MAX-CONNECTIONS must be a positive integer."
                         max-connections))
  (when (and idle-timeout
             (or (not (realp idle-timeout)) (< idle-timeout 0)))
    (%h3-transport-error "IDLE-TIMEOUT must be a non-negative real or NIL."
                         idle-timeout))
  (when (and max-connection-age
             (or (not (realp max-connection-age)) (< max-connection-age 0)))
    (%h3-transport-error
     "MAX-CONNECTION-AGE must be a non-negative real or NIL."
     max-connection-age))
  (unless (functionp clock-function)
    (%h3-transport-error "CLOCK-FUNCTION must be a function." clock-function))
  (%make-http3-connection-manager
   :open-client open-client :max-connections max-connections
   :idle-timeout idle-timeout :max-connection-age max-connection-age
   :clock-function clock-function))

(defun http3-connection-manager-open-p (manager)
  (and (http3-connection-manager-p manager)
       (not (%http3-connection-manager-closed-p manager))))

(defun http3-connection-manager-max-connections (manager)
  (and (http3-connection-manager-p manager)
       (%http3-connection-manager-max-connections manager)))

(defun http3-connection-manager-idle-timeout (manager)
  (and (http3-connection-manager-p manager)
       (%http3-connection-manager-idle-timeout manager)))

(defun http3-connection-manager-max-connection-age (manager)
  (and (http3-connection-manager-p manager)
       (%http3-connection-manager-max-connection-age manager)))

(defun http3-connection-manager-clock-function (manager)
  (and (http3-connection-manager-p manager)
       (%http3-connection-manager-clock-function manager)))

(defun http3-connection-manager-connection-count (manager)
  (and (http3-connection-manager-p manager)
       (length (%http3-connection-manager-entries manager))))

(defun http3-connection-manager-clients (manager)
  "Return the HTTP3-CLIENT objects currently retained by MANAGER."
  (unless (http3-connection-manager-p manager)
    (%h3-transport-error
     "HTTP3-CONNECTION-MANAGER-CLIENTS requires a manager."
     (type-of manager)))
  (mapcar #'%http3-managed-client-client
          (%http3-connection-manager-entries manager)))

(defun %h3-manager-request-key (request)
  (let ((uri (http-kit:http-request-uri request)))
    (list (http-kit:http-uri-scheme uri)
          (http-kit:http-uri-host uri)
          (http-kit:http-uri-port uri))))

(defun %h3-manager-now (manager)
  (let ((now (funcall (%http3-connection-manager-clock-function manager))))
    (unless (realp now)
      (%h3-transport-error
       "The HTTP/3 manager clock must return a real number." now))
    now))

(defun %h3-manager-entry-expired-p (manager entry now)
  (let ((idle-timeout (%http3-connection-manager-idle-timeout manager))
        (max-age (%http3-connection-manager-max-connection-age manager)))
    (or (and idle-timeout
             (>= (- now (%http3-managed-client-last-used-at entry))
                 idle-timeout))
        (and max-age
             (>= (- now (%http3-managed-client-opened-at entry)) max-age)))))

(defun %h3-manager-discard-entry (manager entry)
  (setf (%http3-connection-manager-entries manager)
        (delete entry (%http3-connection-manager-entries manager) :test #'eq))
  (let ((client (%http3-managed-client-client entry)))
    (when (http3-client-open-p client)
      (close-http3-client client)))
  entry)

(defun %h3-manager-prune (manager)
  (let ((now (and (or (%http3-connection-manager-idle-timeout manager)
                      (%http3-connection-manager-max-connection-age manager))
                  (%h3-manager-now manager))))
    (dolist (entry (copy-list (%http3-connection-manager-entries manager)))
      (unless (and (http3-client-open-p (%http3-managed-client-client entry))
                   (not (%h3-manager-entry-expired-p manager entry now)))
        (%h3-manager-discard-entry manager entry)))))

(defun %h3-manager-oldest-entry (manager)
  (reduce (lambda (oldest entry)
            (if (or (null oldest)
                    (< (%http3-managed-client-last-used entry)
                       (%http3-managed-client-last-used oldest)))
                entry
                oldest))
          (%http3-connection-manager-entries manager) :initial-value nil))

(defun %h3-manager-touch (manager entry)
  (setf (%http3-managed-client-last-used entry)
        (incf (%http3-connection-manager-sequence manager)))
  (when (or (%http3-connection-manager-idle-timeout manager)
            (%http3-connection-manager-max-connection-age manager))
    (setf (%http3-managed-client-last-used-at entry)
          (%h3-manager-now manager)))
  entry)

(defun %h3-manager-client-for
    (manager request connection-key connection-key-supplied-p timeout deadline)
  (unless (http3-connection-manager-p manager)
    (%h3-transport-error "An HTTP/3 request requires a connection manager."
                         (type-of manager)))
  (unless (http3-connection-manager-open-p manager)
    (%h3-transport-error "The HTTP/3 connection manager is closed."))
  (unless (http-kit:http-request-p request)
    (%h3-transport-error "The HTTP/3 manager requires an HTTP request."
                         (type-of request)))
  (%h3-manager-prune manager)
  (let* ((key (if connection-key-supplied-p
                  connection-key
                  (%h3-manager-request-key request)))
         (entry (find key (%http3-connection-manager-entries manager)
                      :key #'%http3-managed-client-key :test #'equal)))
    (unless entry
      (when (>= (length (%http3-connection-manager-entries manager))
                (%http3-connection-manager-max-connections manager))
        (let ((oldest (%h3-manager-oldest-entry manager)))
          (when oldest
            (%h3-manager-discard-entry manager oldest))))
      (let ((client
              (funcall (%http3-connection-manager-open-client manager)
                       request :timeout timeout :deadline deadline)))
        (unless (and (http3-client-p client) (http3-client-open-p client))
          (%h3-transport-error
           "The HTTP/3 manager open-client callback returned no open client."
           (type-of client)))
        (let ((now (and (or (%http3-connection-manager-idle-timeout manager)
                            (%http3-connection-manager-max-connection-age manager))
                        (%h3-manager-now manager))))
          (setf entry (%make-http3-managed-client
                       :key key :client client :opened-at now
                       :last-used 0 :last-used-at now))
          (push entry (%http3-connection-manager-entries manager)))))
    (%h3-manager-touch manager entry)
    (values (%http3-managed-client-client entry) entry)))

(defun send-http3-request-over-connection-manager
    (manager request
     &key (connection-key nil connection-key-supplied-p)
       on-body-chunk on-information (collect-body-p t)
       max-header-bytes max-fields max-body-bytes
       request-body-function request-body-length proxy proxy-plan
       qpack-encoder-table qpack-decoder-table qpack-decoder-context
       await-qpack (huffman-p nil) on-stream-open on-push-promise
       timeout deadline)
  "Send REQUEST through a reusable HTTP/3 connection manager."
  (declare (ignore proxy-plan))
  (when proxy
    (%h3-transport-error
     "HTTP/3 proxy routing is not supported by this transport." proxy))
  (when (and max-header-bytes
             (not (%h3-positive-limit-p max-header-bytes)))
    (%h3-transport-error
     "MAX-HEADER-BYTES must be NIL or a positive integer." max-header-bytes))
  (when (and max-fields (not (%h3-positive-limit-p max-fields)))
    (%h3-transport-error
     "MAX-FIELDS must be NIL or a positive integer." max-fields))
  (multiple-value-bind (client entry)
      (%h3-manager-client-for
       manager request connection-key connection-key-supplied-p timeout deadline)
    (handler-case
        (let ((response
                (send-http3-request
                 client request :on-body-chunk on-body-chunk
                 :on-information on-information
                 :collect-body-p collect-body-p
                 :max-header-bytes max-header-bytes
                 :max-fields max-fields
                 :max-body-bytes max-body-bytes
                 :request-body-function request-body-function
                 :request-body-length request-body-length
                 :qpack-encoder-table qpack-encoder-table
                 :qpack-decoder-table qpack-decoder-table
                 :qpack-decoder-context qpack-decoder-context
                 :await-qpack await-qpack :huffman-p huffman-p
                 :on-stream-open on-stream-open
                 :on-push-promise on-push-promise
                 :timeout timeout :deadline deadline)))
          (%h3-manager-touch manager entry)
          response)
      (error (condition)
        (unless (http3-client-open-p client)
          (%h3-manager-discard-entry manager entry))
        (error condition)))))

(defun make-http3-connection-manager-transport (manager)
  "Return a high-level client transport backed by MANAGER."
  (unless (http3-connection-manager-p manager)
    (%h3-transport-error
     "MAKE-HTTP3-CONNECTION-MANAGER-TRANSPORT requires a manager."
     (type-of manager)))
  (lambda (request
           &key timeout deadline max-header-bytes max-fields max-body-bytes
             request-body-function request-body-length
             on-body-chunk on-information (collect-body-p t)
             proxy proxy-plan
             (connection-key nil connection-key-supplied-p)
             &allow-other-keys)
    (apply #'send-http3-request-over-connection-manager
           manager request
           (append
            (when connection-key-supplied-p
              (list :connection-key connection-key))
            (list :timeout timeout :deadline deadline
                  :max-header-bytes max-header-bytes
                  :max-fields max-fields
                  :max-body-bytes max-body-bytes
                  :request-body-function request-body-function
                  :request-body-length request-body-length
                  :on-body-chunk on-body-chunk
                  :on-information on-information
                  :collect-body-p collect-body-p
                  :proxy proxy :proxy-plan proxy-plan)))))

(defun close-http3-connection-manager (manager)
  "Close every retained HTTP/3 client and make MANAGER unusable."
  (unless (http3-connection-manager-p manager)
    (%h3-transport-error
     "CLOSE-HTTP3-CONNECTION-MANAGER requires a manager." (type-of manager)))
  (unless (%http3-connection-manager-closed-p manager)
    (setf (%http3-connection-manager-closed-p manager) t)
    (dolist (entry (copy-list (%http3-connection-manager-entries manager)))
      (%h3-manager-discard-entry manager entry)))
  manager)
