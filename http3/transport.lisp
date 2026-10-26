(in-package #:http-kit/http3)

(defparameter +http3-default-qpack-max-instruction-bytes+ 65536)
(defparameter +http3-default-qpack-max-buffer-bytes+ 262144)
(defparameter +http3-default-max-request-streams+ 100)
(defparameter +http3-default-max-peer-unidirectional-streams+ 16)
(defparameter +http3-default-max-state-bytes+ (* 4 1024 1024))

(progn
  (define-condition http3-connection-protocol-error (http-protocol-error)
    ((quic-error-code :initarg :quic-error-code
                     :reader http3-connection-protocol-error-quic-error-code)))

  (defun %h3-connection-transport-error (message detail quic-error-code)
    (error 'http3-connection-protocol-error
           :message message
           :operation :http3-transport
           :detail detail
           :quic-error-code quic-error-code))

  (defun %h3-transport-error (message &optional detail)
    (error 'http-protocol-error
           :message message
           :operation :http3-transport
           :detail detail)))

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

(defun %h3-check-state-bytes (client &optional (additional-bytes 0))
  (let ((observed (+ additional-bytes
                     (length (http3-client-peer-control-buffer client))
                     (length (http3-client-peer-qpack-encoder-buffer client))
                     (length (http3-client-peer-qpack-decoder-buffer client))
                     (reduce #'+ (http3-client-prefetched-stream-inputs client)
                             :key (lambda (entry)
                                    (length (or (first (cdr entry)) #())))
                             :initial-value 0)
                     (reduce #'+
                             (http3-client-peer-unidirectional-prefix-buffers client)
                             :key (lambda (entry)
                                    (length (or (first (cdr entry)) #())))
                             :initial-value 0))))
    (when (> observed (http3-client-max-state-bytes client))
      (%h3-transport-error "HTTP/3 aggregate connection state exceeds its limit."
                           (list :h3-excessive-load
                                 (http3-client-max-state-bytes client)
                                 observed)))))

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

(progn
  (defstruct (http3-client (:constructor %make-http3-client))
    open-stream
    write-stream
    try-write-stream
    read-stream
    poll-function
    try-read-stream
    stream-write-pending-p
    accept-peer-unidirectional-stream
    prefetched-stream-inputs
    pending-peer-unidirectional-streams
    peer-unidirectional-prefix-buffers
    pending-push-streams
    seen-peer-push-ids
    discard-peer-unidirectional-streams
    close-stream
    close-connection
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
    (request-stream-count 0 :type integer)
    (request-streams (make-hash-table :test (function eql)) :type hash-table)
    pending-request-streams
    runnable-stream-ids
    scheduler-owner-thread
    (scheduler-poll-generation 0 :type integer)
    scheduler-close-state
    scheduler-close-owner-active-p
    scheduler-close-owner-thread
    #+sbcl (scheduler-lock (sb-thread:make-mutex :name "HTTP/3 scheduler"))
    #-sbcl (scheduler-lock nil)
    #+sbcl (scheduler-condition (sb-thread:make-waitqueue :name "HTTP/3 scheduler"))
    #-sbcl (scheduler-condition nil)
    (peer-unidirectional-stream-count 0 :type integer)
    (max-request-streams +http3-default-max-request-streams+ :type integer)
    (max-peer-unidirectional-streams
     +http3-default-max-peer-unidirectional-streams+ :type integer)
    (max-state-bytes +http3-default-max-state-bytes+ :type integer)
    (qpack-max-instruction-bytes
     +http3-default-qpack-max-instruction-bytes+ :type integer)
    (qpack-max-buffer-bytes +http3-default-qpack-max-buffer-bytes+ :type integer)
    qpack-encoder-context
    qpack-decoder-context
    qpack-http3-decoder-context
    (serialize (lambda (thunk) (funcall thunk)) :type function)
    (max-frame-size #x4000 :type integer)
    (max-header-bytes 65536 :type integer)
    (max-fields 256 :type integer)
    max-push-id
    promised-push-ids
    push-promises
    cancelled-push-ids
    consumed-push-ids
    (qpack-settings nil)
    (peer-qpack-settings-applied-p nil :type boolean)
    close-pending-streams
    (open-p t))
  (defstruct (%http3-request-stream
               (:constructor %make-http3-request-stream))
    request
    stream
    stream-id
    request-method
    request-fields
    request-body
    request-body-function
    request-body-length
    trailer-fields
    encoder-context
    huffman-p
    on-stream-open
    on-body-chunk
    on-information
    on-push-promise
    collect-body-p
    max-body-bytes
    max-header-bytes
    max-fields
    timeout
    deadline
    request-write-state
    (response-buffer #() :type vector)
    response-frames
    response-status
    response-headers
    response-trailers
    (response-body #() :type vector)
    (response-body-length 0 :type integer)
    (final-response-p nil :type boolean)
    (trailers-seen-p nil :type boolean)
    (response-fin-p nil :type boolean)
    qpack-token
    qpack-resume-fields
    (qpack-resume-fields-p nil :type boolean)
    continuation
    response
    failure
    (cancel-requested-p nil :type boolean)
    (cancel-error-code #x10c :type integer)
    (done-p nil :type boolean)
    (started-p nil :type boolean)))

(defun make-http3-client
    (&key open-stream write-stream try-write-stream read-stream poll-function try-read-stream stream-write-pending-p close-stream close-connection cancel-stream
          (serialize (lambda (thunk) (funcall thunk)))
          (max-frame-size #x4000) (max-header-bytes 65536) (max-fields 256)
          (max-request-streams +http3-default-max-request-streams+)
          (max-peer-unidirectional-streams
           +http3-default-max-peer-unidirectional-streams+)
          (max-state-bytes +http3-default-max-state-bytes+)
          (qpack-max-instruction-bytes
           +http3-default-qpack-max-instruction-bytes+)
          (qpack-max-buffer-bytes +http3-default-qpack-max-buffer-bytes+)
          (qpack-settings (quote ())) max-push-id peer-control-stream timeout deadline
          on-control-stream-ready accept-peer-unidirectional-stream)
  "Open an HTTP/3 client over caller-supplied QUIC stream callbacks.

OPEN-STREAM is called as (REQUEST &KEY STREAM-TYPE TIMEOUT DEADLINE) and
returns a stream object plus, for request streams, an optional numeric QUIC
stream ID. WRITE-STREAM is called as (STREAM OCTETS &KEY FIN-P TIMEOUT
DEADLINE). READ-STREAM is called as (STREAM &KEY TIMEOUT DEADLINE) and returns
an octet vector, or NIL at QUIC stream FIN. CLOSE-STREAM is called as (STREAM
&KEY CONDITION), optional CANCEL-STREAM as (STREAM &KEY ERROR-CODE TIMEOUT
DEADLINE), and optional CLOSE-CONNECTION as (CLOSE-CONNECTION &KEY ERROR-CODE
REASON). CLOSE-CONNECTION receives the QUIC application error code and reason
when a connection-scoped HTTP/3 protocol error is detected.

For native scheduler operation, supply POLL-FUNCTION, TRY-READ-STREAM, and
STREAM-WRITE-PENDING-P together. POLL-FUNCTION takes no arguments and performs
one QUIC poll. TRY-READ-STREAM is called with the READ-STREAM arguments and
returns (VALUES OCTETS FIN-P :DATA), (VALUES NIL T :FIN), or
(VALUES NIL NIL :WOULD-BLOCK). STREAM-WRITE-PENDING-P receives a stream and
returns true while a previously submitted write remains queued. The scheduler
does not resubmit those octets and polls until the write completes before its
next write on that stream. When these callbacks are absent, READ-STREAM keeps
its blocking behavior.

ACCEPT-PEER-UNIDIRECTIONAL-STREAM, when supplied, takes no arguments and
returns one newly accepted peer-created unidirectional stream, or NIL when
none is available. It must return without blocking. The native scheduler drains
this callback after each poll and buffers an incomplete stream-type prefix
until it can classify the stream. When omitted, the native scheduler does not
discover peer-created unidirectional streams.

TRY-WRITE-STREAM, when supplied, is called like WRITE-STREAM and returns after
submitting its octets. The native scheduler uses it for request frames and
falls back to WRITE-STREAM when it is absent. WRITE-STREAM remains the callback
for synchronous operations.

These callbacks own QUIC, TLS, ALPN, socket, and event-loop behavior; this
system supplies the HTTP/3 stream and codec layer."
  (progn
    (unless (or (and (null poll-function)
                     (null try-read-stream)
                     (null stream-write-pending-p))
                (and (functionp poll-function)
                     (functionp try-read-stream)
                     (functionp stream-write-pending-p)))
      (%h3-transport-error
       "Native HTTP/3 polling, nonblocking reads, and write-pending callbacks must be supplied together."
       (list poll-function try-read-stream stream-write-pending-p)))
    (dolist (callback (list open-stream write-stream read-stream))
      (unless (functionp callback)
        (%h3-transport-error "HTTP/3 open, write, and read callbacks are required."
                             (type-of callback))))
    (unless (or (null try-write-stream) (functionp try-write-stream))
      (%h3-transport-error "HTTP/3 try-write-stream must be a function or NIL."
                           (type-of try-write-stream)))
    (unless (or (null accept-peer-unidirectional-stream)
                (functionp accept-peer-unidirectional-stream))
      (%h3-transport-error
       "HTTP/3 accept-peer-unidirectional-stream must be a function or NIL."
       (type-of accept-peer-unidirectional-stream))))
  (unless (or (null close-stream) (functionp close-stream))
    (%h3-transport-error "HTTP/3 close-stream must be a function or NIL."
                         (type-of close-stream)))
  (unless (or (null close-connection) (functionp close-connection))
    (%h3-transport-error "HTTP/3 close-connection must be a function or NIL."
                         (type-of close-connection)))
  (unless (or (null cancel-stream) (functionp cancel-stream))
    (%h3-transport-error "HTTP/3 cancel-stream must be a function or NIL."
                         (type-of cancel-stream)))
  (unless (or (null on-control-stream-ready)
              (functionp on-control-stream-ready))
    (%h3-transport-error
     "HTTP/3 on-control-stream-ready must be a function or NIL."
     (type-of on-control-stream-ready)))
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
  (dolist (entry (list (cons :max-request-streams max-request-streams)
                       (cons :max-peer-unidirectional-streams
                             max-peer-unidirectional-streams)
                       (cons :max-state-bytes max-state-bytes)
                       (cons :qpack-max-instruction-bytes
                             qpack-max-instruction-bytes)
                       (cons :qpack-max-buffer-bytes qpack-max-buffer-bytes)))
    (unless (%h3-positive-limit-p (cdr entry))
      (%h3-transport-error "HTTP/3 connection limits must be positive integers."
                           entry)))
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
            (%h3-write-stream-callback
             write-stream
             poll-function
             stream-write-pending-p
             control-stream
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
          (when on-control-stream-ready
            (funcall on-control-stream-ready :timeout timeout :deadline deadline))
          (%h3-write-stream-callback
           write-stream
           poll-function
           stream-write-pending-p
           qpack-encoder-stream
           (http3-qpack-encoder-stream-prefix)
           :fin-p nil
           :timeout timeout
           :deadline deadline)
          (%h3-write-stream-callback
           write-stream
           poll-function
           stream-write-pending-p
           qpack-decoder-stream
           (http3-qpack-decoder-stream-prefix)
           :fin-p nil
           :timeout timeout
           :deadline deadline)
          (let* ((decoder-capacity
                  (or (cdr (assoc +http3-setting-qpack-max-table-capacity+
                                  settings))
                      0))
                 (blocked-stream-limit
                  (or (cdr (assoc +http3-setting-qpack-blocked-streams+
                                  settings))
                      0))
                 (encoder-context
                  (http-message-kit:make-qpack-context :max-table-capacity 0 :table-capacity 0 :max-blocked-streams 0 :huffman nil))
                 (decoder-context
                  (http-message-kit:make-qpack-context :max-table-capacity decoder-capacity :table-capacity 0 :max-blocked-streams blocked-stream-limit :max-header-list-size max-header-bytes :huffman nil)))
            (%make-http3-client
             :poll-function poll-function
             :try-read-stream try-read-stream
             :stream-write-pending-p stream-write-pending-p
             :accept-peer-unidirectional-stream
             accept-peer-unidirectional-stream
             :open-stream open-stream
             :write-stream write-stream
             :try-write-stream try-write-stream
             :read-stream read-stream
             :close-stream closer
             :close-connection close-connection
             :cancel-stream cancel-stream
             :control-stream control-stream
             :qpack-encoder-stream qpack-encoder-stream
             :qpack-decoder-stream qpack-decoder-stream
             :peer-control-stream peer-control-stream
             :peer-control-state (make-http3-control-state :peer-role :server)
             :peer-control-buffer (make-array 0 :element-type (quote (unsigned-byte 8)))
             :peer-control-prefix-seen-p nil
             :peer-control-fin-p nil
             :peer-qpack-encoder-buffer (make-array 0 :element-type (quote (unsigned-byte 8)))
             :peer-qpack-decoder-buffer (make-array 0 :element-type (quote (unsigned-byte 8)))
             :qpack-encoder-context encoder-context
             :qpack-decoder-context decoder-context
             :qpack-http3-decoder-context
             (http-message-kit:qpack-make-http3-decoder-context decoder-context :max-blocked-streams blocked-stream-limit)
             :serialize serialize
             :max-frame-size max-frame-size
             :max-header-bytes max-header-bytes
             :max-fields max-fields
             :max-request-streams max-request-streams
             :max-peer-unidirectional-streams max-peer-unidirectional-streams
             :max-state-bytes max-state-bytes
             :qpack-max-instruction-bytes qpack-max-instruction-bytes
             :qpack-max-buffer-bytes qpack-max-buffer-bytes
             :max-push-id max-push-id
             :promised-push-ids (quote ())
             :push-promises (quote ())
             :cancelled-push-ids (quote ())
             :consumed-push-ids (quote ())
             :qpack-settings settings
             :peer-qpack-settings-applied-p nil
             :open-p t)))
      (error (condition)
             (dolist (stream (remove nil (list control-stream
                                               qpack-encoder-stream
                                               qpack-decoder-stream)))
               (http-kit::%with-http-cleanup
                (funcall closer stream :condition condition)))
             (error condition)))))

(progn
  (defun %h3-write-stream-callback
      (write-stream poll-function stream-write-pending-p stream octets
       &key fin-p timeout deadline)
    (funcall write-stream stream octets
             :fin-p fin-p :timeout timeout :deadline deadline)
    (when stream-write-pending-p
      (let ((effective-deadline
              (http-kit:http-deadline timeout :deadline deadline)))
        (loop while (funcall stream-write-pending-p stream)
              do (when effective-deadline
                   (http-kit::%check-deadline
                    effective-deadline #'http-kit::%monotonic-time :write))
                 (funcall poll-function)
                 (sleep 0.005))))
    stream)
  (defun %h3-write-stream
      (client stream octets &key fin-p timeout deadline)
    (%h3-write-stream-callback
     (http3-client-write-stream client)
     (http3-client-poll-function client)
     (http3-client-stream-write-pending-p client)
     stream octets :fin-p fin-p :timeout timeout :deadline deadline))
  (defun %call-http3-serialized (client thunk)
    (funcall (http3-client-serialize client) thunk)))

(progn
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

(defmacro %h3-with-scheduler-lock ((client) &body body)
  #+sbcl
  `(sb-thread:with-mutex ((http3-client-scheduler-lock ,client))
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun %h3-scheduler-broadcast (client)
  #+sbcl
  (sb-thread:condition-broadcast
   (http3-client-scheduler-condition client))
  #-sbcl
  (values client))

(defun %h3-scheduler-condition-wait (client deadline clock-function)
  #+sbcl
  (let* ((lock (http3-client-scheduler-lock client))
         (timeout (and deadline
                       (max 0 (- deadline (funcall clock-function))))))
    (sb-thread:condition-wait
     (http3-client-scheduler-condition client) lock :timeout timeout)
    (http-kit::%check-deadline deadline clock-function :read))
  #-sbcl
  (progn
    (sleep 0.005)
    (values client deadline clock-function)))

(defun %h3-native-scheduler-p (client)
  (and (functionp (http3-client-poll-function client))
       (functionp (http3-client-try-read-stream client))
       (functionp (http3-client-stream-write-pending-p client))))

(defun %h3-try-read-client-stream (client stream timeout deadline)
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
        (let ((chunk (first prefetched))
              (fin-p (second prefetched)))
          (values chunk fin-p (if chunk :data :fin)))
        (multiple-value-bind (chunk fin-p status)
            (funcall (http3-client-try-read-stream client)
                     stream :timeout timeout :deadline deadline)
          (unless (member status '(:data :fin :would-block))
            (%h3-transport-error
             "The HTTP/3 try-read-stream callback returned an invalid status."
             status))
          (case status
            (:data
             (unless (%http3-octet-vector-p chunk)
               (%h3-transport-error
                "HTTP/3 try-read-stream :DATA must include an octet vector."
                (type-of chunk)))
             (values chunk fin-p :data))
            (:fin
             (unless (and (null chunk) fin-p)
               (%h3-transport-error
                "HTTP/3 try-read-stream :FIN must return NIL and true."
                (list chunk fin-p)))
             (values nil t :fin))
            (:would-block
             (unless (and (null chunk) (not fin-p))
               (%h3-transport-error
                "HTTP/3 try-read-stream :WOULD-BLOCK must return NIL and false."
                (list chunk fin-p)))
             (values nil nil :would-block)))))))

)

(defun %h3-prefetch-stream-input (client stream chunk fin-p)
  (%call-http3-serialized
   client
   (lambda ()
     (push (cons stream (list chunk fin-p))
           (http3-client-prefetched-stream-inputs client)))))

(defun %h3-register-peer-push-stream (client stream push-id)
  (let ((failure nil))
    (%h3-with-scheduler-lock (client)
      (cond
        ((not (http3-client-open-p client))
         (setf failure :closed))
        ((not (and (http3-client-max-push-id client)
                   (<= push-id (http3-client-max-push-id client))))
         (setf failure :max-push-id))
        (t
         (let ((entry (assoc push-id (http3-client-seen-peer-push-ids client)
                             :test (function =))))
           (cond
             ((null entry)
              (push (cons push-id stream)
                    (http3-client-seen-peer-push-ids client)))
             ((not (eq (cdr entry) stream))
              (setf failure :duplicate)))))))
    (case failure
      (:closed
       (%h3-transport-error "The HTTP/3 client is already closed."))
      (:max-push-id
       (%h3-transport-error
        "The peer HTTP/3 push ID exceeds MAX_PUSH_ID."
        :h3-id-error))
      (:duplicate
       (%h3-transport-error
        "The peer used an HTTP/3 push ID more than once."
        :h3-id-error)))
    push-id))

(defun %h3-forget-peer-push-stream (client stream)
  (%h3-with-scheduler-lock (client)
    (dolist (entry (http3-client-seen-peer-push-ids client))
      (when (eq (cdr entry) stream)
        (setf (cdr entry) nil))))
  stream)

(defun dequeue-http3-pending-push (client)
  "Return the next peer push stream and its push ID. During polling, the native
scheduler enqueues discovered push streams. Return two NIL values when empty.
This function does not poll QUIC. Pass only the stream to
RECEIVE-HTTP3-PUSH."
  (unless (http3-client-p client)
    (%h3-transport-error "DEQUEUE-HTTP3-PENDING-PUSH requires an HTTP/3 client."
                         (type-of client)))
  (let ((entry nil)
        (open-p nil))
    (%h3-with-scheduler-lock (client)
      (setf open-p (http3-client-open-p client))
      (when open-p
        (setf entry (pop (http3-client-pending-push-streams client)))))
    (unless open-p
      (%h3-transport-error "The HTTP/3 client is already closed."))
    (if entry
        (values (cdr entry) (car entry))
        (values nil nil))))
(defun %h3-retain-unknown-peer-stream (client stream fin-p)
  (let ((entry nil)
        (cancel (http3-client-cancel-stream client))
        (cancel-p nil)
        (drain-capable-p (%h3-native-scheduler-p client)))
    (%h3-with-scheduler-lock (client)
      (let ((entries (http3-client-discard-peer-unidirectional-streams client)))
        (setf entry (assoc stream entries :test #'eq))
        (unless entry
          (setf (http3-client-discard-peer-unidirectional-streams client)
                (acons stream
                       (cond
                         (cancel :cancel-pending)
                         (fin-p :finished)
                         (t :drain))
                       entries))
          (setf entry
                (assoc stream
                       (http3-client-discard-peer-unidirectional-streams client)
                       :test #'eq)))
        (setf cancel-p (and cancel (eq (cdr entry) :cancel-pending)))))
    (when cancel-p
      (let ((cancel-returned-p nil)
            (cancel-condition nil))
        (unwind-protect
             (handler-case
                 (progn
                   (funcall cancel stream :error-code #x103 :timeout nil :deadline nil)
                   (setf cancel-returned-p t))
               (error (condition)
                 (setf cancel-condition condition)))
          (unless cancel-returned-p
            (%h3-with-scheduler-lock (client)
              (setf (cdr entry)
                    (if drain-capable-p :drain :cancel-pending)))
            (unless drain-capable-p
              (handler-case
                  (close-http3-client client :condition cancel-condition)
                (error (close-condition)
                  (declare (ignore close-condition)))))))
        (cond
          (cancel-returned-p
           (%h3-with-scheduler-lock (client)
             (setf (cdr entry) :cancelled)))
          (drain-capable-p nil)
          (t
           (error cancel-condition)))))
    stream))
(defun %accept-http3-peer-unidirectional-stream-serialized
    (client stream &key timeout deadline)
  "Classify and attach one peer-created HTTP/3 unidirectional STREAM.

The stream-type prefix may be fragmented across reads. Push streams are
classified only after their complete push ID is available. Consumed prefix
bytes are replayed to the corresponding reader. Returns :CONTROL,
:QPACK-ENCODER, :QPACK-DECODER, :PUSH, or :UNKNOWN."
  (unless stream
    (%h3-transport-error
     "ACCEPT-HTTP3-PEER-UNIDIRECTIONAL-STREAM requires a stream."))
  (when (>= (http3-client-peer-unidirectional-stream-count client)
            (http3-client-max-peer-unidirectional-streams client))
    (%h3-transport-error
     "The HTTP/3 peer unidirectional-stream limit was exceeded."
     :h3-excessive-load))
  (incf (http3-client-peer-unidirectional-stream-count client))
  (let* ((prefetched
          (%call-http3-serialized
           client
           (lambda ()
             (let ((entry
                    (assoc stream
                           (http3-client-prefetched-stream-inputs client))))
               (when entry
                 (setf (http3-client-prefetched-stream-inputs client)
                       (remove stream
                               (http3-client-prefetched-stream-inputs client)
                               :key #'car :test #'eq :count 1))
                 (cdr entry))))))
         (buffer (if prefetched
                     (first prefetched)
                   (make-array 0 :element-type '(unsigned-byte 8))))
         (prefetched-fin-p (and prefetched (second prefetched))))
    (loop
     (multiple-value-bind (stream-type type-end)
         (http3-varint-decode buffer :allow-incomplete-p t)
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
           (if (eq kind :push)
               (multiple-value-bind (push-id push-end)
                   (http3-varint-decode
                    buffer :position type-end :allow-incomplete-p t)
                 (declare (ignore push-end))
                 (when push-id
                   (%h3-register-peer-push-stream client stream push-id)
                   (%h3-prefetch-stream-input
                    client stream buffer prefetched-fin-p)
                   (return :push))
                 (when prefetched-fin-p
                   (%h3-transport-error
                    "An HTTP/3 push stream ended with a truncated push ID."
                    :h3-stream-creation-error)))
             (progn
               (if (eq kind :unknown)
                   (%h3-retain-unknown-peer-stream client stream prefetched-fin-p)
                 (%h3-prefetch-stream-input client stream buffer prefetched-fin-p))
               (return kind)))))
       (multiple-value-bind (chunk fin-p)
           (funcall (http3-client-read-stream client)
                    stream :timeout timeout :deadline deadline)
         (when chunk
           (unless (%http3-octet-vector-p chunk)
             (%h3-transport-error
              "HTTP/3 read-stream must return an octet vector or NIL."
              (type-of chunk)))
           (progn
             (%h3-check-state-bytes client (+ (length buffer) (length chunk)))
             (setf buffer (%http3-concatenate-octets buffer chunk))))
         (setf prefetched-fin-p (or fin-p (null chunk)))
         (when prefetched-fin-p
           (multiple-value-bind (stream-type position)
               (http3-varint-decode buffer :allow-incomplete-p t)
             (declare (ignore position))
             (unless stream-type
               (%h3-transport-error
                "A peer HTTP/3 unidirectional stream ended before its type prefix."
                :h3-stream-creation-error)))))))))
(defun accept-http3-peer-unidirectional-stream
    (client stream &key timeout deadline)
  "Classify and attach one peer-created HTTP/3 unidirectional STREAM."
  (unless (and (http3-client-p client) (http3-client-open-p client))
    (%h3-transport-error "A live HTTP/3 client is required."))
  (%call-http3-serialized
   client
   (lambda ()
     (%accept-http3-peer-unidirectional-stream-serialized
      client stream :timeout timeout :deadline deadline))))

(defun %process-http3-peer-unidirectional-stream-serialized
    (client stream &key timeout deadline)
  "Classify STREAM and apply its first control or QPACK input to CLIENT.

Returns the stream kind, a list of effects, and whether the processed input
ended. :PUSH streams must be passed to RECEIVE-HTTP3-PUSH. The native
scheduler cancels unknown streams with H3_STREAM_CREATION_ERROR when a
cancellation callback is available; otherwise, it discards their data through
FIN. Call the corresponding READ-HTTP3-* function when a critical stream
becomes readable again."
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
(defun process-http3-peer-unidirectional-stream
    (client stream &key timeout deadline)
  "Classify and apply input from one peer-created unidirectional stream."
  (unless (and (http3-client-p client) (http3-client-open-p client))
    (%h3-transport-error "A live HTTP/3 client is required."))
  (%call-http3-serialized
   client
   (lambda ()
     (%process-http3-peer-unidirectional-stream-serialized
      client stream :timeout timeout :deadline deadline))))

(progn
(defun %h3-scheduler-cancel-qpack (client entry timeout deadline)
  (let ((token (%http3-request-stream-qpack-token entry)))
    (when token
      (let ((decoder (http3-client-qpack-http3-decoder-context client))
            (stream-id (%http3-request-stream-stream-id entry))
            (instruction nil))
        (unless (and decoder stream-id)
          (%h3-transport-error
           "A blocked HTTP/3 request must retain its QPACK context and stream ID."
           stream-id))
        (multiple-value-bind (released-p cancellation)
            (%call-http3-serialized
             client
             (lambda ()
               (http-message-kit:qpack-cancel-blocked-stream
                decoder stream-id)))
          (declare (ignore released-p))
          (setf instruction cancellation
                (%http3-request-stream-qpack-token entry) nil
                (%http3-request-stream-qpack-resume-fields entry) nil
                (%http3-request-stream-qpack-resume-fields-p entry) nil))
        (when (and instruction
                   (plusp (length instruction))
                   (http3-client-open-p client)
                   (http3-client-qpack-decoder-stream client))
          (%h3-write-stream
           client (http3-client-qpack-decoder-stream client) instruction
           :fin-p nil :timeout timeout :deadline deadline)))))
  entry)

(defun %h3-scheduler-cancel-entry (client entry condition)
  (unless (%http3-request-stream-done-p entry)
    (let ((stream (%http3-request-stream-stream entry))
          (cancel (http3-client-cancel-stream client)))
      (when (and stream (null cancel))
        (%h3-transport-error
         "The HTTP/3 QUIC backend does not provide stream cancellation."
         :unsupported))
      (when stream
        (funcall cancel stream
                 :error-code (%http3-request-stream-cancel-error-code entry)
                 :timeout (%http3-request-stream-timeout entry)
                 :deadline (%http3-request-stream-deadline entry)))
      (%h3-scheduler-cancel-qpack
       client entry
       (%http3-request-stream-timeout entry)
       (%http3-request-stream-deadline entry))
      (%h3-scheduler-complete-entry client entry :failure condition)))
  entry)

(defun %h3-scheduler-request-cancel (client entry error-code)
  (%h3-with-scheduler-lock (client)
    (unless (%http3-request-stream-done-p entry)
      (setf (%http3-request-stream-cancel-requested-p entry) t
            (%http3-request-stream-cancel-error-code entry) error-code)
      (%h3-scheduler-broadcast client)))
  (when (and (%h3-scheduler-owner-p client)
             (%http3-request-stream-stream entry))
    (%h3-scheduler-cancel-entry client entry
                                (make-condition
                                 'http-kit:http-connection-error
                                 :message "The HTTP/3 request stream was cancelled."
                                 :operation :http3-request
                                 :request-sent-p t
                                 :cause (list :local-cancel
                                              (%http3-request-stream-stream-id entry)
                                              error-code))))
  t)

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
  (let ((entry (and (%h3-native-scheduler-p client)
                    (%h3-scheduler-entry-for-stream client stream)))
        (cancel (http3-client-cancel-stream client)))
    (unless cancel
      (%h3-transport-error
       "The HTTP/3 QUIC backend does not provide stream cancellation."
       :unsupported))
    (if entry
        (%h3-scheduler-request-cancel client entry error-code)
        (funcall cancel stream :error-code error-code
                 :timeout timeout :deadline deadline)))
  t))


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
       (%h3-write-stream client (http3-client-control-stream client) (encode-http3-frame frame)
                         :fin-p nil
                         :timeout timeout
                         :deadline deadline)))
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
     (let ((previous (let ((max-push-id nil))
                       (%h3-with-scheduler-lock (client)
                         (setf max-push-id (http3-client-max-push-id client)))
                       max-push-id)))
       (when (and previous (<= max-push-id previous))
         (%h3-transport-error
          "MAX_PUSH_ID must increase the previously advertised push ID."
          :h3-id-error))
       (%h3-write-stream client (http3-client-control-stream client) (encode-http3-frame
                                                                      (make-http3-frame
                                                                       :type +http3-max-push-id-type+
                                                                       :payload (http3-varint-encode max-push-id)))
                         :fin-p nil
                         :timeout timeout
                         :deadline deadline)
       (%h3-with-scheduler-lock (client)
         (setf (http3-client-max-push-id client) max-push-id)))))
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
       (%h3-write-stream client (http3-client-control-stream client) (encode-http3-frame
                                                                      (make-http3-frame
                                                                       :type +http3-cancel-push-type+
                                                                       :payload (http3-varint-encode push-id)))
                         :fin-p nil
                         :timeout timeout
                         :deadline deadline)
       (push push-id (http3-client-cancelled-push-ids client)))))
  client)

(progn
  (defun %close-http3-client-serialized (client condition)
    (let ((snapshot nil)
          (first-failure nil))
      (%h3-with-scheduler-lock (client)
        (let* ((open-p (http3-client-open-p client))
               (pending-pushes
                 (copy-list (http3-client-pending-push-streams client)))
               (seen-push-streams
                 (remove nil
                         (mapcar #'cdr
                                 (http3-client-seen-peer-push-ids client))))
               (pending-peer-streams
                 (remove-duplicates
                  (append
                   (copy-list
                    (http3-client-pending-peer-unidirectional-streams client))
                   (mapcar #'car
                           (http3-client-peer-unidirectional-prefix-buffers
                            client)))
                  :test #'eq))
               (discard-entries
                 (copy-list
                  (http3-client-discard-peer-unidirectional-streams client)))
               (discard-streams (mapcar #'car discard-entries))
               (push-streams
                 (remove-duplicates
                  (append (mapcar #'cdr pending-pushes) seen-push-streams)
                  :test #'eq))
               (cancel-pairs
                 (when open-p
                   (remove-duplicates
                    (append
                     (mapcar (lambda (stream) (cons stream #x10c)) push-streams)
                     (mapcar (lambda (stream) (cons stream #x103))
                             pending-peer-streams)
                     (loop for entry in discard-entries
                           when (or (eq (cdr entry) :drain)
                                    (eq (cdr entry) :cancel-pending))
                             collect (cons (car entry) #x103)))
                    :key #'car :test #'eq)))
               (streams
                 (if open-p
                     (remove-duplicates
                      (append
                       (http3-client-close-pending-streams client)
                       (remove nil
                               (list
                                (http3-client-control-stream client)
                                (http3-client-qpack-encoder-stream client)
                                (http3-client-qpack-decoder-stream client)
                                (http3-client-peer-control-stream client)
                                (http3-client-peer-qpack-encoder-stream client)
                                (http3-client-peer-qpack-decoder-stream client)))
                       pending-peer-streams discard-streams push-streams
                       (remove nil
                               (append
                                (loop for entry being the hash-values
                                      of (http3-client-request-streams client)
                                      collect (%http3-request-stream-stream entry))
                                (mapcar #'%http3-request-stream-stream
                                        (http3-client-pending-request-streams client)))))
                      :test #'eq)
                   (copy-list (http3-client-close-pending-streams client))))
               (decoder
                 (and open-p
                      (http3-client-qpack-http3-decoder-context client))))
          (setf snapshot (list cancel-pairs streams decoder))
          (when open-p
            (setf (http3-client-open-p client) nil
                  (http3-client-qpack-encoder-context client) nil
                  (http3-client-qpack-decoder-context client) nil
                  (http3-client-qpack-http3-decoder-context client) nil
                  (http3-client-close-pending-streams client) streams
                  (http3-client-pending-push-streams client) nil
                  (http3-client-seen-peer-push-ids client) nil
                  (http3-client-pending-peer-unidirectional-streams client) nil
                  (http3-client-peer-unidirectional-prefix-buffers client) nil
                  (http3-client-discard-peer-unidirectional-streams client) nil
                  (http3-client-prefetched-stream-inputs client) nil))))
      (destructuring-bind (cancel-pairs streams decoder) snapshot
        (labels ((remember-failure (caught-condition)
                   (unless first-failure
                     (setf first-failure caught-condition)))
                 (try-cancel (entry)
                   (let ((cancel (http3-client-cancel-stream client)))
                     (when cancel
                       (handler-case
                           (funcall cancel (car entry)
                                    :error-code (cdr entry)
                                    :timeout nil :deadline nil)
                         (error (caught-condition)
                           (remember-failure caught-condition))))))
                 (try-close (stream failed-streams)
                   (handler-case
                       (progn
                         (funcall (http3-client-close-stream client)
                                  stream :condition condition)
                         failed-streams)
                     (error (caught-condition)
                       (remember-failure caught-condition)
                       (cons stream failed-streams)))))
          (when decoder
            (handler-case
                (http-message-kit:qpack-close-http3-decoder-context decoder)
              (error (caught-condition)
                (remember-failure caught-condition))))
          (dolist (entry cancel-pairs)
            (try-cancel entry))
          (let ((failed-streams nil)
                (remaining streams))
            (unwind-protect
                 (loop while remaining
                       for stream = (car remaining)
                       do (setf failed-streams (try-close stream failed-streams)
                                remaining (cdr remaining)))
              ;; Keep unattempted streams available if a callback exits nonlocally.
              (%h3-with-scheduler-lock (client)
                (setf (http3-client-close-pending-streams client)
                      (append (nreverse failed-streams) remaining)))))
          (when first-failure
            (error first-failure))
          t))))
  (defun close-http3-client (client &key condition)
    (unless (http3-client-p client)
      (%h3-transport-error "CLOSE-HTTP3-CLIENT requires an HTTP/3 client."
                           (type-of client)))
    (let ((close-status (%h3-scheduler-begin-connection-close client)))
      (when (eq close-status :proceed)
        (let ((close-succeeded-p nil)
              (close-failure nil))
          (unwind-protect
              (handler-bind
                  ((error
                    (lambda (condition)
                      (unless close-failure
                        (setf close-failure condition)))))
                (progn
                  (%call-http3-serialized
                   client
                   (lambda ()
                     (%close-http3-client-serialized client condition)))
                  (setf close-succeeded-p t)))
            (%h3-scheduler-finish-connection-close
             client
             (unless close-succeeded-p
               (or close-failure
                   (make-condition
                    (quote http-kit:http-connection-error)
                    :message "Closing the HTTP/3 client did not complete."
                    :operation :http3-close)))
             :http3-only-p t
             :scheduler-condition condition))))
      (not (null close-status)))))

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

(defun %h3-consume-stream-prefix (buffer prefix-seen-p expected-type)
  (if prefix-seen-p
      (values t buffer)
      (multiple-value-bind (stream-type position)
          (http3-varint-decode buffer :allow-incomplete-p t)
        (if (null stream-type)
            (values nil buffer)
            (progn
              (unless (= stream-type expected-type)
                (%h3-connection-transport-error
                 "The peer HTTP/3 unidirectional stream has the wrong type."
                 :h3-stream-creation-error #x103))
              (values t (subseq buffer position)))))))

(progn
  (defun %h3-qpack-stream-error (condition detail error-code)
    (%h3-connection-transport-error
     (http-kit:http-error-message condition)
     detail
     error-code))

  (progn
  (defun %h3-promote-http3-control-error (condition)
    (let ((detail (http-kit:http-protocol-error-detail condition)))
      (cond
        ((eq detail :h3-settings-error)
         (%h3-connection-transport-error
          (http-kit:http-error-message condition) detail #x109))
        ((eq detail :h3-frame-unexpected)
         (%h3-connection-transport-error
          (http-kit:http-error-message condition) detail #x105))
        ((eq detail :h3-missing-settings)
         (%h3-connection-transport-error
          (http-kit:http-error-message condition) detail #x10a))
        (t
         (error condition)))))

  (defun %h3-promote-qpack-decompression-error (condition)
    (let* ((cause (if (typep condition
                             'http-message-kit:http-connection-error)
                      (http-message-kit:http-connection-error-cause condition)
                      condition))
           (detail (and (typep cause 'http-protocol-error)
                        (http-kit:http-protocol-error-detail cause))))
      (when (and (consp detail)
                 (eq (first detail) :qpack-decompression-failed)
                 (not (typep (second detail) 'http-size-limit-exceeded)))
        (%h3-connection-transport-error
         (http-kit:http-error-message condition)
         detail
         #x200)))
    (error condition))))

(defun attach-http3-peer-control-stream (client stream)
  "Attach the incoming HTTP/3 control stream to CLIENT."
  (unless (http3-client-p client)
    (%h3-transport-error
     "ATTACH-HTTP3-PEER-CONTROL-STREAM requires an HTTP/3 client."
     (type-of client)))
  (unless stream
    (%h3-transport-error
     "ATTACH-HTTP3-PEER-CONTROL-STREAM requires a stream."))
  (%call-http3-serialized
   client
   (lambda ()
     (when (http3-client-peer-control-stream client)
       (%h3-connection-transport-error
        "An HTTP/3 client can have only one peer control stream."
        :h3-stream-creation-error #x103))
     (unless (http3-client-open-p client)
       (%h3-transport-error
        "Cannot attach an HTTP/3 control stream after client close."))
     ;; Reading requires this attachment, and closing retains it.
     (setf (http3-client-peer-control-stream client) stream)
     client)))

(defun %read-http3-control-stream-serialized (client &key timeout deadline)
  "Read one chunk from CLIENT's peer control stream and apply its frames.

Returns a list of effect keywords and a boolean indicating peer FIN.  The
peer control stream must begin with its HTTP/3 stream-type prefix, then a
SETTINGS frame; its state is available through
HTTP3-CLIENT-PEER-CONTROL-STATE."
  (unless (http3-client-open-p client)
    (%h3-transport-error "Cannot read an HTTP/3 control stream after client close."))
  (let ((stream (http3-client-peer-control-stream client)))
    (unless stream
      (%h3-transport-error
       "READ-HTTP3-CONTROL-STREAM requires an attached peer control stream."))
    (when (http3-client-peer-control-fin-p client)
      (return-from %read-http3-control-stream-serialized (values '() t)))
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
                (handler-case
                    (process-http3-control-bytes
                     (http3-client-peer-control-state client)
                     buffer :allow-incomplete-p t
                     :max-frame-size (http3-client-max-frame-size client))
                  (http-protocol-error (condition)
                    (%h3-promote-http3-control-error condition)))
              (setf events new-events
                    buffer frame-remainder))))
        (when (and (http3-control-state-settings-received-p
                    (http3-client-peer-control-state client))
                   (not (http3-client-peer-qpack-settings-applied-p client)))
          (%call-http3-serialized
           client
           (lambda ()
             (let* ((settings
                      (http3-control-state-settings
                       (http3-client-peer-control-state client)))
                    (blocked-stream-limit
                      (or (cdr (assoc +http3-setting-qpack-blocked-streams+
                                      settings))
                          0))
                    (encoder-context
                      (http-message-kit:make-qpack-context
                       :max-table-capacity 0
                       :table-capacity 0
                       :max-blocked-streams blocked-stream-limit
                       :huffman nil)))
               (http-message-kit:qpack-context-set-max-table-capacity
                encoder-context
                (or (cdr (assoc +http3-setting-qpack-max-table-capacity+
                                settings))
                    0))
               (setf (http3-client-qpack-encoder-context client)
                     encoder-context
                     (http3-client-peer-qpack-settings-applied-p client)
                     t)))))
        (when ended-p
          (unless (http3-client-peer-control-prefix-seen-p client)
            (%h3-connection-transport-error
             "The peer HTTP/3 control stream ended before its stream-type prefix."
             :h3-closed-critical-stream #x104))
          (when (plusp (array-total-size buffer))
            (%h3-connection-transport-error
             "The peer HTTP/3 control stream ended with a truncated frame."
             :h3-closed-critical-stream #x104))
          (unless (http3-control-state-settings-received-p
                   (http3-client-peer-control-state client))
            (%h3-connection-transport-error
             "The peer HTTP/3 control stream ended before SETTINGS."
             :h3-closed-critical-stream #x104))
          (setf (http3-client-peer-control-fin-p client) t)
          (%h3-connection-transport-error
           "The peer closed the HTTP/3 control stream."
           :h3-closed-critical-stream #x104))
        (setf (http3-client-peer-control-buffer client) buffer)
        (values events ended-p)))))
(defun read-http3-control-stream (client &key timeout deadline)
  "Read and apply one chunk from CLIENT’s peer control stream."
  (unless (http3-client-p client)
    (%h3-transport-error
     "READ-HTTP3-CONTROL-STREAM requires an HTTP/3 client."
     (type-of client)))
  (%call-http3-serialized
   client
   (lambda ()
     (%read-http3-control-stream-serialized
      client :timeout timeout :deadline deadline))))

(defun %h3-write-client-qpack-transaction
    (client prepare write result-function)
  (let ((failure nil)
        (result nil))
    (%call-http3-serialized
     client
     (lambda ()
       (multiple-value-bind (octets transaction) (funcall prepare)
         (handler-case
             (funcall write octets)
           (error (condition)
             (setf failure condition)
             (http-kit::%with-http-cleanup
               (http-message-kit:qpack-abort-encoding transaction))
             (http-kit::%with-http-cleanup
               (%close-http3-client-serialized client condition))
             (http-kit::%with-http-cleanup
               (%h3-scheduler-finish-connection-close client condition))))
         (unless failure
           (handler-case
               (progn
                 (http-message-kit:qpack-commit-encoding transaction)
                 (setf result (funcall result-function)))
             (error (condition)
               (setf failure condition)
               (http-kit::%with-http-cleanup
                 (%close-http3-client-serialized client condition))
               (http-kit::%with-http-cleanup
                 (%h3-scheduler-finish-connection-close client condition))))))))
    (when failure
      (error failure))
    result))

(defun http3-client-set-qpack-capacity
    (client capacity &key timeout deadline)
  "Set the encoder capacity for the client and notify the peer over its QPACK stream."
  (unless (and (http3-client-p client) (http3-client-open-p client))
    (%h3-transport-error "A live HTTP/3 client is required."))
  (%h3-write-client-qpack-transaction
   client
   (lambda ()
     (http-message-kit:qpack-prepare-set-capacity
      (http3-client-qpack-encoder-context client)
      capacity))
   (lambda (octets)
     (%h3-write-stream client (http3-client-qpack-encoder-stream client) octets
                       :fin-p nil
                       :timeout timeout
                       :deadline deadline))
   (lambda () client)))

(defun http3-client-insert-qpack-field
    (client name value &key timeout deadline)
  "Insert a field for the peer and return the committed insert count."
  (unless (and (http3-client-p client) (http3-client-open-p client))
    (%h3-transport-error "A live HTTP/3 client is required."))
  (%h3-write-client-qpack-transaction
   client
   (lambda ()
     (http-message-kit:qpack-prepare-insert-with-literal-name
      (http3-client-qpack-encoder-context client)
      name value))
   (lambda (octets)
     (%h3-write-stream client (http3-client-qpack-encoder-stream client) octets
                       :fin-p nil
                       :timeout timeout
                       :deadline deadline))
   (lambda ()
     (http-message-kit:qpack-context-insert-count
      (http3-client-qpack-encoder-context client)))))

(defun attach-http3-peer-qpack-encoder-stream (client stream)
  "Attach one peer QPACK encoder stream to CLIENT."
  (unless (and (http3-client-p client) stream)
    (%h3-transport-error
     "ATTACH-HTTP3-PEER-QPACK-ENCODER-STREAM requires a client and stream."))
  (%call-http3-serialized
   client
   (lambda ()
     (when (or (http3-client-peer-qpack-encoder-stream client)
               (http3-client-peer-qpack-encoder-prefix-seen-p client))
       (%h3-connection-transport-error
        "An HTTP/3 connection can have only one peer QPACK encoder stream."
        :h3-stream-creation-error #x103))
     (setf (http3-client-peer-qpack-encoder-stream client) stream)
     client)))

(defun attach-http3-peer-qpack-decoder-stream (client stream)
  "Attach one peer QPACK decoder stream to CLIENT."
  (unless (and (http3-client-p client) stream)
    (%h3-transport-error
     "ATTACH-HTTP3-PEER-QPACK-DECODER-STREAM requires a client and stream."))
  (%call-http3-serialized
   client
   (lambda ()
     (when (or (http3-client-peer-qpack-decoder-stream client)
               (http3-client-peer-qpack-decoder-prefix-seen-p client))
       (%h3-connection-transport-error
        "An HTTP/3 connection can have only one peer QPACK decoder stream."
        :h3-stream-creation-error #x103))
     (setf (http3-client-peer-qpack-decoder-stream client) stream)
     client)))

(defun %read-http3-qpack-encoder-stream-serialized (client &key timeout deadline)
  "Read and apply one chunk from the peer's QPACK encoder stream."
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
                 (setf buffer (%http3-concatenate-octets buffer chunk))
                 (%h3-check-state-bytes client))
               (multiple-value-bind (seen-p remainder)
                   (%h3-consume-stream-prefix
                    buffer
                    (http3-client-peer-qpack-encoder-prefix-seen-p client)
                    +http3-qpack-encoder-stream-type+)
                 (setf (http3-client-peer-qpack-encoder-prefix-seen-p client)
                       seen-p
                       buffer remainder)
                 (when seen-p
                   (handler-case
                       (multiple-value-bind (new-events consumed)
                           (http-message-kit:qpack-decode-encoder-stream
                            (http3-client-qpack-decoder-context client)
                            buffer
                            :allow-incomplete-p t
                            :max-instruction-bytes
                            (http3-client-qpack-max-instruction-bytes client)
                            :max-buffer-bytes
                            (http3-client-qpack-max-buffer-bytes client))
                         (setf events new-events
                               buffer (subseq buffer consumed)))
                     (http-protocol-error (condition)
                       (%h3-qpack-stream-error condition :qpack-encoder-stream-error #x201)))))
               (when ended-p
                 (setf (http3-client-peer-qpack-encoder-fin-p client) t)
                 (when (or (not (http3-client-peer-qpack-encoder-prefix-seen-p
                                 client))
                           (plusp (length buffer)))
                   (%h3-connection-transport-error
                    "The peer QPACK encoder stream ended with truncated input."
                    :qpack-encoder-stream-error #x201))
                 (%h3-connection-transport-error
                  "The peer closed the QPACK encoder stream."
                  :h3-closed-critical-stream #x104))
               (setf (http3-client-peer-qpack-encoder-buffer client) buffer)
               (%h3-check-state-bytes client)
               (values events ended-p
                       (http-message-kit:qpack-take-ready-streams
                        (http3-client-qpack-http3-decoder-context client))))))
        (values (nconc events
                       (mapcar (lambda (blocked)
                                 (list :qpack-stream-unblocked blocked))
                               ready))
                ended-p)))))
(defun read-http3-qpack-encoder-stream (client &key timeout deadline)
  "Read and apply one chunk from the peer QPACK encoder stream."
  (unless (and (http3-client-p client) (http3-client-open-p client))
    (%h3-transport-error "A live HTTP/3 client is required."))
  (%call-http3-serialized
   client
   (lambda ()
     (%read-http3-qpack-encoder-stream-serialized
      client :timeout timeout :deadline deadline))))

(defun %read-http3-qpack-decoder-stream-serialized (client &key timeout deadline)
  "Read and apply one chunk from the peer's QPACK decoder stream."
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
                +http3-qpack-decoder-stream-type+)
             (setf (http3-client-peer-qpack-decoder-prefix-seen-p client)
                   seen-p
                   buffer remainder)
             (when seen-p
               (handler-case
                   (multiple-value-bind (new-events consumed)
                       (http-message-kit:qpack-decode-decoder-stream
                        (http3-client-qpack-encoder-context client)
                        buffer
                        :allow-incomplete-p t
                        :max-instruction-bytes
                        (http3-client-qpack-max-instruction-bytes client)
                        :max-buffer-bytes
                        (http3-client-qpack-max-buffer-bytes client))
                     (setf events new-events
                           buffer (subseq buffer consumed)))
                 (http-protocol-error (condition)
                   (%h3-qpack-stream-error condition :qpack-decoder-stream-error #x202)))))
           (when ended-p
             (setf (http3-client-peer-qpack-decoder-fin-p client) t)
             (when (or (not (http3-client-peer-qpack-decoder-prefix-seen-p client))
                       (plusp (length buffer)))
               (%h3-connection-transport-error
                "The peer QPACK decoder stream ended with truncated input."
                :qpack-decoder-stream-error #x202))
             (%h3-connection-transport-error
              "The peer closed the QPACK decoder stream."
              :h3-closed-critical-stream #x104))
           (setf (http3-client-peer-qpack-decoder-buffer client) buffer)
           (%h3-check-state-bytes client)
           (values events ended-p)))))))
(defun read-http3-qpack-decoder-stream (client &key timeout deadline)
  "Read and apply one chunk from the peer QPACK decoder stream."
  (unless (and (http3-client-p client) (http3-client-open-p client))
    (%h3-transport-error "A live HTTP/3 client is required."))
  (%call-http3-serialized
   client
   (lambda ()
     (%read-http3-qpack-decoder-stream-serialized
      client :timeout timeout :deadline deadline))))

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
    (%h3-write-stream client stream (encode-http3-frame frame)
                      :fin-p fin-p
                      :timeout timeout
                      :deadline deadline)))

(defun %h3-header-name (header)
  (let ((name (string-downcase (http-kit:http-header-name header))))
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

(defun %h3-copy-field-section (fields)
  (mapcar (lambda (field)
            (cons (copy-seq (car field)) (copy-seq (cdr field))))
          fields))

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
        (values nil (nreverse headers))
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

(defun %h3-write-client-qpack-section-ack-serialized
    (client stream-id timeout deadline)
  (let ((stream (http3-client-qpack-decoder-stream client)))
    (unless stream
      (%h3-transport-error "The client QPACK decoder stream is unavailable."))
    (handler-case
        (%h3-write-stream client stream (http-message-kit:qpack-encode-section-ack stream-id)
                          :fin-p nil
                          :timeout timeout
                          :deadline deadline)
      (error (condition)
        (http-kit::%with-http-cleanup
          (%close-http3-client-serialized client condition))
        (http-kit::%with-http-cleanup
          (%h3-scheduler-finish-connection-close client condition))
        (error condition)))))

(defun %h3-decode-dynamic-field-section
    (payload context stream-id await-qpack serialize role
             on-section-acknowledgment max-header-bytes max-fields
             &key serialize-whole-operation-p serialize-guard return-blocked-p)
  (let* ((inside-operation-p (list nil))
         (serialized-decode
          (if serialize-whole-operation-p
              (lambda (thunk)
                (if (car inside-operation-p)
                    (progn
                      (when serialize-guard
                        (funcall serialize-guard))
                      (funcall thunk))
                  (funcall
                   serialize
                   (lambda ()
                     (when serialize-guard
                       (funcall serialize-guard))
                     (funcall thunk)))))
            serialize))
         (serialized-acknowledgment
          (if (and serialize-whole-operation-p
                   on-section-acknowledgment)
              (lambda (acknowledged-stream-id)
                (if (car inside-operation-p)
                    (progn
                      (when serialize-guard
                        (funcall serialize-guard))
                      (funcall on-section-acknowledgment
                               acknowledged-stream-id))
                  (funcall
                   serialize
                   (lambda ()
                     (when serialize-guard
                       (funcall serialize-guard))
                     (funcall on-section-acknowledgment
                              acknowledged-stream-id)))))
            on-section-acknowledgment))
         (decode
          (lambda ()
            (if serialize-whole-operation-p
                (unwind-protect
                    (progn
                      (setf (car inside-operation-p) t)
                      (when serialize-guard
                        (funcall serialize-guard))
                      (http-message-kit:qpack-decode-http3-field-section
                       payload context stream-id
                       :role role
                       :serialize serialized-decode
                       :on-section-acknowledgment
                       serialized-acknowledgment
                       :max-header-bytes max-header-bytes
                       :max-fields max-fields))
                  (setf (car inside-operation-p) nil))
              (http-message-kit:qpack-decode-http3-field-section
               payload context stream-id
               :role role
               :serialize serialized-decode
               :on-section-acknowledgment
               serialized-acknowledgment
               :max-header-bytes max-header-bytes
               :max-fields max-fields))))
         (result
          (if serialize-whole-operation-p
              (funcall serialize decode)
            (funcall decode))))
    (when (http-message-kit:qpack-blocked-stream-p result)
      (if return-blocked-p
          (return-from %h3-decode-dynamic-field-section (values nil result))
        (progn (unless (functionp await-qpack)
                 (%h3-transport-error
                  "A blocked QPACK field section requires an await-qpack callback."
                  stream-id))
          (setf result (funcall await-qpack result))
          (unless (listp result)
            (%h3-transport-error
             "The await-qpack callback must return decoded headers."
             stream-id)))))
    result))

(defun %h3-check-qpack-section-acknowledgment-stream-id
    (expected-stream-id acknowledged-stream-id)
  (unless (= acknowledged-stream-id expected-stream-id)
    (%h3-transport-error
     "QPACK Section Acknowledgment used the wrong stream ID."
     acknowledged-stream-id)))

(defun %h3-decode-response-field-section
    (client payload qpack-http3-decoder-context
            &key stream-id await-qpack role max-header-bytes max-fields
            timeout deadline return-blocked-p)
  (let ((header-limit
         (min (or max-header-bytes most-positive-fixnum)
              (http3-client-max-header-bytes client)))
        (field-limit
         (min (or max-fields most-positive-fixnum)
              (http3-client-max-fields client))))
    (handler-case
        (%h3-decode-dynamic-field-section
         payload qpack-http3-decoder-context stream-id await-qpack
         (http3-client-serialize client) role
         (lambda (acknowledged-stream-id)
           (%h3-check-qpack-section-acknowledgment-stream-id
            stream-id acknowledged-stream-id)
           (%h3-write-client-qpack-section-ack-serialized
            client acknowledged-stream-id timeout deadline))
         header-limit field-limit
         :serialize-whole-operation-p t
         :serialize-guard
         (lambda ()
           (unless (and (http3-client-open-p client)
                        (eq qpack-http3-decoder-context
                            (http3-client-qpack-http3-decoder-context client)))
             (%h3-transport-error
              "A live HTTP/3 client with its own QPACK decoder context is required.")))
         :return-blocked-p return-blocked-p)
      (http-message-kit:http-connection-error (condition)
        (%h3-promote-qpack-decompression-error condition))
      (http-protocol-error (condition)
        (%h3-promote-qpack-decompression-error condition)))))

(defun %h3-read-response
    (client stream &key on-body-chunk on-information collect-body-p
            (max-body-bytes http-kit::*default-max-body-bytes*)
            max-header-bytes max-fields
            qpack-http3-decoder-context stream-id await-qpack
            request-method on-push-promise timeout deadline)
  (when qpack-http3-decoder-context
    (unless (and (integerp stream-id) (>= stream-id 0))
      (%h3-transport-error
       "Dynamic QPACK response decoding requires a numeric stream ID."
       stream-id))
    (when (and
           (plusp
            (or (cdr (assoc +http3-setting-qpack-blocked-streams+
                            (http3-client-qpack-settings client)))
                0))
           (not (functionp await-qpack)))
      (%h3-transport-error
       "A blocked QPACK field section requires an await-qpack callback."
       stream-id)))
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
                         client payload qpack-http3-decoder-context
                         :role (if final-response-p :trailers :response)
                         :stream-id stream-id
                         :await-qpack await-qpack
                         :timeout timeout :deadline deadline
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
                  (let ((max-push-id (let ((max-push-id nil))
                             (%h3-with-scheduler-lock (client)
                               (setf max-push-id (http3-client-max-push-id client)))
                             max-push-id)))
                    (unless (and max-push-id (<= push-id max-push-id))
                      (%h3-transport-error
                       "HTTP/3 PUSH_PROMISE exceeds the advertised MAX_PUSH_ID."
                       :h3-id-error))
                    (let ((fields
                            (%h3-decode-response-field-section
                             client (subseq payload field-position)
                             qpack-http3-decoder-context
                             :role :request
                             :stream-id stream-id
                             :await-qpack await-qpack
                             :timeout timeout :deadline deadline
                             :max-header-bytes max-header-bytes
                             :max-fields max-fields)))
                      (multiple-value-call #'list
                        (%h3-server-parse-request-fields fields))
                      (setf fields (%h3-copy-field-section fields))
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
                              (push (cons push-id fields)
                                    (http3-client-push-promises client)))))))
                      (when on-push-promise
                        (funcall on-push-promise push-id
                                 (%h3-copy-field-section fields)))))))
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

(progn
(defun %h3-write-request-field-section
    (client stream fields encoder-context stream-id huffman-p
     &key fin-p timeout deadline)
  (let ((use-dynamic-p
          (plusp
           (http-message-kit:qpack-context-dynamic-entry-count
            encoder-context))))
    (when (and use-dynamic-p
               (not (and (integerp stream-id) (>= stream-id 0))))
      (%h3-transport-error
       "Dynamic QPACK field encoding requires a numeric stream ID."
       stream-id))
    (%h3-write-client-qpack-transaction
     client
     (lambda ()
       (http-message-kit:qpack-prepare-header-block
        fields :context encoder-context
        :use-dynamic-p use-dynamic-p
        :huffman huffman-p
        :stream-id stream-id))
     (lambda (octets)
       (%h3-write-frame
        client stream
        (make-http3-frame :type +http3-headers-type+ :payload octets)
        :fin-p fin-p :timeout timeout :deadline deadline))
     (lambda () stream))))

(defun %h3-scheduler-process-data-frame (entry payload)
  (unless (%http3-request-stream-final-response-p entry)
    (%h3-transport-error
     "An HTTP/3 DATA frame arrived before final response HEADERS."))
  (when (%http3-request-stream-trailers-seen-p entry)
    (%h3-transport-error
     "An HTTP/3 DATA frame arrived after response trailers."))
  (when (%h3-no-body-response-p
         (%http3-request-stream-request-method entry)
         (%http3-request-stream-response-status entry))
    (%h3-transport-error
     "This HTTP/3 response cannot carry DATA."
     (list (%http3-request-stream-request-method entry)
           (%http3-request-stream-response-status entry))))
  (let ((new-length
          (+ (%http3-request-stream-response-body-length entry)
             (length payload))))
    (when (and (%http3-request-stream-max-body-bytes entry)
               (> new-length (%http3-request-stream-max-body-bytes entry)))
      (%h3-size-error
       :body (%http3-request-stream-max-body-bytes entry) new-length))
    (setf (%http3-request-stream-response-body-length entry) new-length)
    (when (%http3-request-stream-collect-body-p entry)
      (setf (%http3-request-stream-response-body entry)
            (%h3-append-body (%http3-request-stream-response-body entry)
                             payload)))
    (when (%http3-request-stream-on-body-chunk entry)
      (funcall (%http3-request-stream-on-body-chunk entry)
               (%http3-copy-octets payload))))
  t)

)

(defun %h3-scheduler-process-headers-frame (client entry payload)
  (multiple-value-bind (fields ready-p)
      (%h3-scheduler-fields
       client entry payload
       (if (%http3-request-stream-final-response-p entry)
           :trailers :response))
    (unless ready-p
      (return-from %h3-scheduler-process-headers-frame nil))
    (multiple-value-bind (status headers)
        (%h3-response-fields
         fields :trailers-p (%http3-request-stream-final-response-p entry))
      (if (%http3-request-stream-final-response-p entry)
          (progn
            (when (%http3-request-stream-trailers-seen-p entry)
              (%h3-transport-error
               "An HTTP/3 response cannot contain multiple trailer blocks."))
            (setf (%http3-request-stream-response-trailers entry) headers
                  (%http3-request-stream-trailers-seen-p entry) t))
          (progn
            (when (= status 101)
              (%h3-transport-error
               "HTTP/3 does not permit a 101 Switching Protocols response."))
            (if (>= status 200)
                (setf (%http3-request-stream-response-status entry) status
                      (%http3-request-stream-response-headers entry) headers
                      (%http3-request-stream-final-response-p entry) t)
                (progn
                  (when (http-kit:http-header-values headers "content-length")
                    (%h3-invalid-header
                     "content-length"
                     "informational responses cannot contain content-length."))
                  (when (%http3-request-stream-on-information entry)
                    (funcall
                     (%http3-request-stream-on-information entry)
                     (http-kit:make-http-response
                      :protocol-version "HTTP/3"
                      :status status :headers headers :trailers nil
                      :body (http-kit::%empty-octets))))))))))
  t)

(defun %h3-scheduler-fields (client entry payload role)
  (if (%http3-request-stream-qpack-resume-fields-p entry)
      (multiple-value-prog1
          (values (%http3-request-stream-qpack-resume-fields entry) t)
        (setf (%http3-request-stream-qpack-resume-fields entry) nil
              (%http3-request-stream-qpack-resume-fields-p entry) nil))
      (multiple-value-bind (fields token)
          (%h3-decode-response-field-section
           client payload
           (http3-client-qpack-http3-decoder-context client)
           :role role
           :stream-id (%http3-request-stream-stream-id entry)
           :timeout (%http3-request-stream-timeout entry)
           :deadline (%http3-request-stream-deadline entry)
           :max-header-bytes (%http3-request-stream-max-header-bytes entry)
           :max-fields (%http3-request-stream-max-fields entry)
           :return-blocked-p t)
        (if (and token (http-message-kit:qpack-blocked-stream-p token))
            (progn
              (setf (%http3-request-stream-qpack-token entry) token)
              (values nil nil))
            (values fields t)))))

(defun %h3-scheduler-finish-response (entry)
  (unless (%http3-request-stream-final-response-p entry)
    (%h3-transport-error
     "The HTTP/3 stream ended before final response HEADERS."))
  (%h3-response-content-length
   (%http3-request-stream-response-headers entry)
   (%http3-request-stream-response-body-length entry)
   :status (%http3-request-stream-response-status entry)
   :request-method (%http3-request-stream-request-method entry)
   :no-body
   (%h3-no-body-response-p
    (%http3-request-stream-request-method entry)
    (%http3-request-stream-response-status entry)))
  (http-kit:make-http-response
   :protocol-version "HTTP/3"
   :status (%http3-request-stream-response-status entry)
   :headers (%http3-request-stream-response-headers entry)
   :trailers (%http3-request-stream-response-trailers entry)
   :body (if (%http3-request-stream-collect-body-p entry)
             (%http3-request-stream-response-body entry)
             nil)))

(defun %h3-scheduler-current-thread ()
  #+sbcl sb-thread:*current-thread*
  #-sbcl nil)

(progn
  (defun %h3-scheduler-owner-p (client)
    #+sbcl (eq (http3-client-scheduler-owner-thread client)
               (%h3-scheduler-current-thread))
    #-sbcl (declare (ignore client))
    #-sbcl nil)

  (defun %h3-call-with-scheduler-owner (client thunk)
  #+sbcl
  (let ((thread (%h3-scheduler-current-thread))
        (claimed-p nil))
    (%h3-with-scheduler-lock (client)
      (cond
        ((eq (http3-client-scheduler-owner-thread client) thread))
        ((and (http3-client-scheduler-close-owner-active-p client)
              (not (eq (http3-client-scheduler-close-owner-thread client)
                       thread)))
         (error 'http-kit:http-connection-error
                :message "The HTTP/3 QUIC client is owned by another thread."
                :operation :http3-quic-client
                :request-sent-p nil))
        ((and (http3-client-scheduler-close-state client)
              (not (and (http3-client-scheduler-close-owner-active-p client)
                        (eq (http3-client-scheduler-close-owner-thread client)
                            thread))))
         (error 'http-kit:http-connection-error
                :message "The HTTP/3 QUIC connection is closing."
                :operation :http3-quic-client
                :request-sent-p nil))
        ((null (http3-client-scheduler-owner-thread client))
         (setf (http3-client-scheduler-owner-thread client) thread
               claimed-p t)
         (%h3-scheduler-broadcast client))
        (t
         (error 'http-kit:http-connection-error
                :message "The HTTP/3 QUIC client is owned by another thread."
                :operation :http3-quic-client
                :request-sent-p nil))))
    (unwind-protect
         (funcall thunk)
      (when claimed-p
        (%h3-with-scheduler-lock (client)
          (setf (http3-client-scheduler-owner-thread client) nil)
          (%h3-scheduler-broadcast client)))))
  #-sbcl
  (funcall thunk)))


(defun %h3-scheduler-entry-for-stream (client stream)
  (%h3-with-scheduler-lock (client)
                           (loop for entry
                                 in (append
                                     (loop for entry being the hash-values
                                           of (http3-client-request-streams client)
                                           collect entry)
                                     (http3-client-pending-request-streams client))
                                 when (eq stream (%http3-request-stream-stream entry))
                                 return entry)))

(defun %h3-scheduler-entry-for-token (client token)
  (%h3-with-scheduler-lock (client)
                           (loop for entry
                                 in (append
                                     (loop for entry being the hash-values
                                           of (http3-client-request-streams client)
                                           collect entry)
                                     (http3-client-pending-request-streams client))
                                 when (eq token (%http3-request-stream-qpack-token entry))
                                 return entry)))

(defun %h3-scheduler-add-runnable (client stream-id)
  (unless (member stream-id (http3-client-runnable-stream-ids client) :test #'=)
    (setf (http3-client-runnable-stream-ids client)
          (append (http3-client-runnable-stream-ids client)
                  (list stream-id))))
  stream-id)

(defun %h3-scheduler-remove-runnable (client stream-id)
  (setf (http3-client-runnable-stream-ids client)
        (delete stream-id (http3-client-runnable-stream-ids client) :test #'=)))

(defun %h3-scheduler-next-runnable-id (client)
  (let ((ids (http3-client-runnable-stream-ids client)))
    (when ids
      (setf (http3-client-runnable-stream-ids client)
            (append (cdr ids) (list (car ids))))
      (car ids))))

(defun %h3-scheduler-complete-entry (client entry &key response failure)
  (unless (%http3-request-stream-done-p entry)
    (%h3-with-scheduler-lock (client)
      (let ((stream-id (%http3-request-stream-stream-id entry)))
        (when (and stream-id
                   (eq (gethash stream-id (http3-client-request-streams client))
                       entry))
          (remhash stream-id (http3-client-request-streams client))
          (%h3-scheduler-remove-runnable client stream-id)))
      (setf (http3-client-pending-request-streams client)
            (delete entry (http3-client-pending-request-streams client)
                    :test #'eq)
            (%http3-request-stream-response entry) response
            (%http3-request-stream-failure entry) failure
            (%http3-request-stream-done-p entry) t)
      (when (plusp (http3-client-request-stream-count client))
        (decf (http3-client-request-stream-count client)))
      (%h3-scheduler-broadcast client)))
  entry)
(progn
  (defun %h3-scheduler-clear-closed-state-locked (client)
    (let ((entries
           (remove-duplicates
            (append (copy-list (http3-client-pending-request-streams client))
                    (loop for entry being the hash-values
                          of (http3-client-request-streams client)
                          collect entry))
            :test (function eq))))
      (dolist (entry entries)
        (unless (%http3-request-stream-done-p entry)
          (setf (%http3-request-stream-failure entry)
                (or (%http3-request-stream-failure entry)
                    (make-condition
                     'http-kit:http-connection-error
                     :message "The HTTP/3 QUIC connection was closed."
                     :operation :http3-request))
                (%http3-request-stream-done-p entry) t)))
      (clrhash (http3-client-request-streams client))
      (setf (http3-client-pending-request-streams client) nil
            (http3-client-runnable-stream-ids client) nil
            (http3-client-request-stream-count client) 0
            (http3-client-close-pending-streams client) nil)
      (%h3-scheduler-broadcast client))
    client)

  (defun %h3-scheduler-fail-closed-state-locked (client condition)
    (let ((entries
            (remove-duplicates
             (append (copy-list (http3-client-pending-request-streams client))
                     (loop for entry being the hash-values
                             of (http3-client-request-streams client)
                           collect entry))
             :test (function eq))))
      (dolist (entry entries)
        (unless (%http3-request-stream-done-p entry)
          (setf (%http3-request-stream-failure entry)
                (or (%http3-request-stream-failure entry) condition)
                (%http3-request-stream-done-p entry) t)))
      (clrhash (http3-client-request-streams client))
      (setf (http3-client-pending-request-streams client) nil
            (http3-client-runnable-stream-ids client) nil
            (http3-client-request-stream-count client) 0)
      (%h3-scheduler-broadcast client))
    client)

  (defun %h3-scheduler-close-started-p (client)
    (%h3-with-scheduler-lock (client)
      (not (null (http3-client-scheduler-close-state client)))))

  (defun %h3-scheduler-begin-connection-close (client)
    (let ((thread (%h3-scheduler-current-thread))
          (result nil))
      (%h3-with-scheduler-lock (client)
                               (loop until result
                                     do (cond
                                         ((eq (http3-client-scheduler-close-state client) :closed)
                                          (setf result :closed))
                                         ((and (http3-client-scheduler-close-owner-active-p client)
                                               (eq (http3-client-scheduler-close-owner-thread client)
                                                   thread))
                                          (return-from %h3-scheduler-begin-connection-close nil))
                                         ((not (http3-client-scheduler-close-owner-active-p client))
                                          (setf (http3-client-scheduler-close-owner-active-p client) t
                                                (http3-client-scheduler-close-owner-thread client) thread
                                                (http3-client-scheduler-close-state client) :closing)
                                          (%h3-scheduler-broadcast client)
                                          (unless (eq (http3-client-scheduler-owner-thread client)
                                                      thread)
                                            (loop while (http3-client-scheduler-owner-thread client)
                                                  do (%h3-scheduler-condition-wait
                                                      client nil #'http-kit::%monotonic-time)))
                                          (setf result :proceed))
                                         ((eq (http3-client-scheduler-owner-thread client) thread)
                                          (error
                                           (make-condition
                                            'http-kit:http-connection-error
                                            :message "A scheduler owner cannot join a concurrent close."
                                            :operation :http3-close
                                            :request-sent-p t)))
                                         (t
                                          (%h3-scheduler-condition-wait
                                           client nil #'http-kit::%monotonic-time)))))
      result))

  (defun %h3-scheduler-finish-connection-close
      (client &optional condition &key http3-only-p scheduler-condition)
    (%h3-with-scheduler-lock (client)
                             (cond
                              (condition
                               (setf (http3-client-scheduler-close-state client) condition)
                               (unless (eq (http3-client-scheduler-owner-thread client)
                                           (%h3-scheduler-current-thread))
                                 (%h3-scheduler-fail-closed-state-locked client condition)))
                              (http3-only-p
                               (unless (eq (http3-client-scheduler-close-state client) :closed)
                                 (setf (http3-client-scheduler-close-state client) :http3-closed)
                                 (%h3-scheduler-fail-closed-state-locked
                                  client
                                  (or scheduler-condition
                                      (make-condition
                                       (quote http-kit:http-connection-error)
                                       :message "The HTTP/3 client was closed."
                                       :operation :http3-request)))))
                              (t
                               (setf (http3-client-scheduler-close-state client) :closed)
                               (%h3-scheduler-clear-closed-state-locked client)))
                             (when (and (http3-client-scheduler-close-owner-active-p client)
                                        (eq (http3-client-scheduler-close-owner-thread client)
                                            (%h3-scheduler-current-thread)))
                               (setf (http3-client-scheduler-close-owner-active-p client) nil
                                     (http3-client-scheduler-close-owner-thread client) nil))
                             (%h3-scheduler-broadcast client))
    client))

(defun %h3-scheduler-register-entry (client entry stream stream-id)
  (unless (and stream (integerp stream-id) (>= stream-id 0)
               (zerop (mod stream-id 4)))
    (%h3-transport-error
     "Native HTTP/3 request streams require a numeric client bidirectional QUIC stream ID."
     (list stream stream-id)))
  (%h3-with-scheduler-lock (client)
    (when (gethash stream-id (http3-client-request-streams client))
      (%h3-transport-error
       "The HTTP/3 transport reused an active request stream ID."
       stream-id))
    (setf (%http3-request-stream-stream entry) stream
          (%http3-request-stream-stream-id entry) stream-id
          (gethash stream-id (http3-client-request-streams client)) entry)
    (%h3-scheduler-add-runnable client stream-id))
  entry)

(defun %h3-scheduler-start-entry (client entry)
  (let ((stream nil))
    (handler-case
        (multiple-value-bind (opened-stream stream-id)
            (funcall (http3-client-open-stream client)
                     (%http3-request-stream-request entry)
                     :stream-type :request
                     :timeout (%http3-request-stream-timeout entry)
                     :deadline (%http3-request-stream-deadline entry))
          (unless opened-stream
            (%h3-transport-error
             "The HTTP/3 open-stream callback returned NIL."))
          (setf stream opened-stream
                (%http3-request-stream-stream entry) opened-stream)
          (%h3-scheduler-register-entry client entry opened-stream stream-id)
          (let ((goaway-id
                 (http3-control-state-goaway-id
                  (http3-client-peer-control-state client))))
            (when goaway-id
              (unless (< stream-id goaway-id)
                (%h3-transport-error
                 "The peer GOAWAY excludes this HTTP/3 request stream."
                 :h3-request-rejected))))
          (setf (%http3-request-stream-started-p entry) t)
          (when (%http3-request-stream-on-stream-open entry)
            (funcall (%http3-request-stream-on-stream-open entry)
                     stream stream-id
                     (lambda (&rest arguments)
                       (let ((error-code-supplied-p
                              (loop for tail on arguments by #'cddr
                                    thereis (eq (first tail) :error-code))))
                         (apply
                          #'(lambda (&key error-code)
                            (cancel-http3-stream
                             client stream
                             :error-code (if error-code-supplied-p error-code #x10c)
                             :timeout (%http3-request-stream-timeout entry)
                             :deadline (%http3-request-stream-deadline entry)))
                          arguments)))))
          entry)
      (error (condition)
             (if stream
                 (%h3-scheduler-fail-entry client entry condition)
               (%h3-scheduler-complete-entry client entry :failure condition))
             entry))))

(defun %h3-scheduler-start-pending (client)
  (let ((entries
          (%h3-with-scheduler-lock (client)
            (unless (http3-client-scheduler-close-state client)
              (prog1 (nreverse (http3-client-pending-request-streams client))
                (setf (http3-client-pending-request-streams client) nil)))))
        (remaining nil)
        (current nil)
        (retained nil))
    (unwind-protect
         (loop for tail on entries
               for entry = (car tail)
               do (setf current entry
                        remaining (cdr tail))
                  (cond
                    ((%http3-request-stream-done-p entry))
                    ((%http3-request-stream-failure entry)
                     (push entry retained))
                    ((%h3-scheduler-close-started-p client)
                     (push entry retained))
                    (t
                     (%h3-scheduler-start-entry client entry)))
                  (setf current nil))
      (%h3-with-scheduler-lock (client)
        (let ((to-restore (append (and current (list current))
                                  remaining
                                  (nreverse retained))))
          (if (eq (http3-client-scheduler-close-state client) :closed)
              (dolist (entry to-restore)
                (unless (%http3-request-stream-done-p entry)
                  (setf (%http3-request-stream-failure entry)
                        (or (%http3-request-stream-failure entry)
                            (make-condition
                             'http-kit:http-connection-error
                             :message "The HTTP/3 QUIC connection was closed."
                             :operation :http3-request))
                        (%http3-request-stream-done-p entry) t)))
              (dolist (entry to-restore)
                (unless (or (%http3-request-stream-done-p entry)
                            (loop for active-entry being the hash-values
                                    of (http3-client-request-streams client)
                                  thereis (eq entry active-entry))
                            (member entry
                                    (http3-client-pending-request-streams client)
                                    :test (function eq)))
                  (push entry (http3-client-pending-request-streams client))))))))
    entries))
(progn
(defun %h3-scheduler-write-frame (client entry frame fin-p)
  (funcall
   (or (http3-client-try-write-stream client)
       (http3-client-write-stream client))
   (%http3-request-stream-stream entry)
   (encode-http3-frame frame)
   :fin-p fin-p
   :timeout (%http3-request-stream-timeout entry)
   :deadline (%http3-request-stream-deadline entry))
  t)

(defun %h3-scheduler-write-fields (client entry fields fin-p)
  (let ((encoder-context (%http3-request-stream-encoder-context entry))
        (stream-id (%http3-request-stream-stream-id entry)))
    (let ((use-dynamic-p
            (plusp
             (http-message-kit:qpack-context-dynamic-entry-count
              encoder-context))))
      (%h3-write-client-qpack-transaction
       client
       (lambda ()
         (http-message-kit:qpack-prepare-header-block
          fields :context encoder-context
          :use-dynamic-p use-dynamic-p
          :huffman (%http3-request-stream-huffman-p entry)
          :stream-id stream-id))
       (lambda (octets)
         (%h3-scheduler-write-frame
          client entry
          (make-http3-frame :type +http3-headers-type+ :payload octets)
          fin-p))
       (lambda () (%http3-request-stream-stream entry))))))

(defun %h3-scheduler-write-data (client entry chunk fin-p)
  (%h3-scheduler-write-frame
   client entry
   (make-http3-frame :type +http3-data-type+ :payload chunk)
   fin-p))

(defun %h3-scheduler-write-entry-step (client entry)
  (when (or (not (%http3-request-stream-started-p entry))
            (%http3-request-stream-done-p entry)
            (%h3-scheduler-close-started-p client)
            (getf (%http3-request-stream-request-write-state entry) :finished-p))
    (return-from %h3-scheduler-write-entry-step nil))
  (let* ((stream (%http3-request-stream-stream entry))
         (state (%http3-request-stream-request-write-state entry))
         (trailers (%http3-request-stream-trailer-fields entry)))
    (when (funcall (http3-client-stream-write-pending-p client) stream)
      (return-from %h3-scheduler-write-entry-step nil))
    (cond
     ((not (getf state :headers-sent-p))
       (let ((finish-p (and (null (%http3-request-stream-request-body-function entry))
                            (zerop (length (%http3-request-stream-request-body entry)))
                            (null (%http3-request-stream-trailer-fields entry)))))
         (%h3-scheduler-write-fields
          client entry (%http3-request-stream-request-fields entry) finish-p)
         (setf (getf state :headers-sent-p) t)
         (when finish-p (setf (getf state :finished-p) t))
         t))
     ((%http3-request-stream-request-body-function entry)
      (let* ((chunk
              (funcall (%http3-request-stream-request-body-function entry)
                       (http3-client-max-frame-size client)))
             (sent (getf state :sent-length 0))
             (expected (%http3-request-stream-request-body-length entry)))
        (when (or (%http3-request-stream-done-p entry)
                  (%http3-request-stream-failure entry)
                  (%h3-scheduler-close-started-p client))
          (return-from %h3-scheduler-write-entry-step nil))
        (if chunk
            (progn
              (%h3-validate-request-body-chunk
               chunk (http3-client-max-frame-size client))
              (when (and expected (> (+ sent (length chunk)) expected))
                (%h3-transport-error
                 "The HTTP/3 request body producer exceeded its declared length."
                 (list (+ sent (length chunk)) expected)))
              (%h3-scheduler-write-data client entry chunk nil)
              (setf (getf state :sent-length) (+ sent (length chunk))))
          (progn
            (when (and expected (/= sent expected))
              (%h3-transport-error
               "The HTTP/3 request body producer ended before its declared length."
               (list sent expected)))
            (if trailers
                (%h3-scheduler-write-fields client entry trailers t)
              (%h3-scheduler-write-data
               client entry (http-kit::%empty-octets) t))
            (setf (getf state :finished-p) t)))
        t))
     ((< (getf state :body-offset 0)
         (length (%http3-request-stream-request-body entry)))
      (let* ((body (%http3-request-stream-request-body entry))
             (position (getf state :body-offset 0))
             (end (min (length body)
                       (+ position (http3-client-max-frame-size client))))
             (chunk (subseq body position end))
             (fin-p (and (= end (length body)) (null trailers))))
        (%h3-scheduler-write-data client entry chunk fin-p)
        (setf (getf state :body-offset) end)
        (when fin-p (setf (getf state :finished-p) t))
        t))
     (trailers
      (%h3-scheduler-write-fields client entry trailers t)
      (setf (getf state :finished-p) t)
      t))))
(progn
(defun %h3-scheduler-process-push-promise (client entry payload)
  (multiple-value-bind (push-id field-position)
      (http3-varint-decode payload)
    (let ((max-push-id (let ((max-push-id nil))
                         (%h3-with-scheduler-lock (client)
                                                  (setf max-push-id (http3-client-max-push-id client)))
                         max-push-id)))
      (unless (and max-push-id (<= push-id max-push-id))
        (%h3-transport-error
         "HTTP/3 PUSH_PROMISE exceeds the advertised MAX_PUSH_ID."
         :h3-id-error))
      (multiple-value-bind (fields ready-p)
          (%h3-scheduler-fields client entry (subseq payload field-position) :request)
        (unless ready-p
          (return-from %h3-scheduler-process-push-promise nil))
        (%h3-server-parse-request-fields fields)
        (setf fields (%h3-copy-field-section fields))
        (%call-http3-serialized
         client
         (lambda ()
           (let ((existing (assoc push-id (http3-client-push-promises client)
                                  :test #'=)))
             (cond
              ((and existing
                    (not (%h3-field-sections-equal-p (cdr existing) fields)))
               (%h3-transport-error
                "Repeated HTTP/3 PUSH_PROMISE fields differ." :h3-id-error))
              ((null existing)
               (pushnew push-id (http3-client-promised-push-ids client) :test #'=)
               (push (cons push-id fields)
                     (http3-client-push-promises client)))))))
        (when (%http3-request-stream-on-push-promise entry)
          (funcall (%http3-request-stream-on-push-promise entry)
                   push-id (%h3-copy-field-section fields)))
        t))))

(defun %h3-scheduler-process-frame (client entry frame)
  (let ((type (http3-frame-type frame))
        (payload (http3-frame-payload frame)))
    (cond
      ((= type +http3-headers-type+)
       (%h3-scheduler-process-headers-frame client entry payload))
      ((= type +http3-data-type+)
       (%h3-scheduler-process-data-frame entry payload))
      ((= type +http3-push-promise-type+)
       (%h3-scheduler-process-push-promise client entry payload))
      ((member type (list +http3-settings-type+
                          +http3-cancel-push-type+
                          +http3-goaway-type+
                          +http3-max-push-id-type+
                          +http3-priority-update-request-type+
                          +http3-priority-update-push-type+)
               :test #'=)
       (%h3-transport-error
        "HTTP/3 control frames are not valid on a request stream." type))
      (t t))))

(defun %h3-scheduler-drain-response-frames (client entry)
  (loop while (and (not (%h3-scheduler-close-started-p client))
                   (%http3-request-stream-response-frames entry)
                   (null (%http3-request-stream-qpack-token entry)))
        for frame = (pop (%http3-request-stream-response-frames entry))
        do (%h3-scheduler-process-frame client entry frame)
        (when (and (not (%h3-scheduler-close-started-p client))
                   (%http3-request-stream-qpack-token entry))
          (setf (%http3-request-stream-continuation entry) frame)))
  (and (not (%h3-scheduler-close-started-p client))
       (null (%http3-request-stream-qpack-token entry))))

(progn
(defun %h3-scheduler-complete-response-if-finished (client entry)
  (when (and (%http3-request-stream-response-fin-p entry)
             (null (%http3-request-stream-qpack-token entry))
             (null (%http3-request-stream-response-frames entry)))
    (unless (%h3-scheduler-close-started-p client)
      (%h3-scheduler-poll-critical-streams client))
    (when (and (not (%h3-scheduler-close-started-p client))
               (%http3-request-stream-response-fin-p entry)
               (null (%http3-request-stream-qpack-token entry))
               (null (%http3-request-stream-response-frames entry)))
      (when (plusp (length (%http3-request-stream-response-buffer entry)))
        (%h3-transport-error
         "The HTTP/3 stream ended with a truncated frame."))
      (let ((response (%h3-scheduler-finish-response entry)))
        (funcall (http3-client-close-stream client)
                 (%http3-request-stream-stream entry) :condition nil)
        (%h3-scheduler-complete-entry client entry :response response)
        t))))

(defun %h3-scheduler-read-entry-step (client entry)
  (when (%http3-request-stream-qpack-token entry)
    (return-from %h3-scheduler-read-entry-step nil))
  (multiple-value-bind (chunk fin-p status)
      (%h3-try-read-client-stream
       client (%http3-request-stream-stream entry)
       (%http3-request-stream-timeout entry)
       (%http3-request-stream-deadline entry))
    (let ((read-progress-p (not (eq status :would-block))))
      (unless (eq status :would-block)
        (when chunk
          (when (plusp (length chunk))
            (setf (%http3-request-stream-response-buffer entry)
                  (%http3-concatenate-octets
                   (%http3-request-stream-response-buffer entry) chunk))
            (multiple-value-bind (frames remainder)
                (decode-http3-frames
                 (%http3-request-stream-response-buffer entry)
                 :allow-incomplete-p t
                 :max-frame-size (http3-client-max-frame-size client))
              (setf (%http3-request-stream-response-buffer entry) remainder
                    (%http3-request-stream-response-frames entry)
                    (nconc (%http3-request-stream-response-frames entry)
                           frames)))))
        (when fin-p
          (setf (%http3-request-stream-response-fin-p entry) t)))
      (let ((frames-p
             (not (null (%http3-request-stream-response-frames entry)))))
        (unless (%h3-scheduler-close-started-p client)
          (%h3-scheduler-drain-response-frames client entry))
        (or (and (not (%h3-scheduler-close-started-p client))
                 (%h3-scheduler-complete-response-if-finished client entry))
            read-progress-p
            frames-p)))))
)
)
)

(defun send-http3-request
    (client request &key on-body-chunk on-information (collect-body-p t)
            max-header-bytes max-fields
            (max-body-bytes http-kit::*default-max-body-bytes*)
            request-body-function request-body-length
            qpack-encoder-context qpack-decoder-context qpack-http3-decoder-context
            await-qpack (huffman-p nil) on-stream-open on-push-promise
            timeout deadline)
  "Send REQUEST on a new HTTP/3 bidirectional stream and read its response.

The in-memory request and response bodies are represented as octet vectors.
REQUEST-BODY-FUNCTION, when supplied, receives the maximum chunk size and
returns non-empty octet arrays until it returns NIL. REQUEST-BODY-LENGTH may
declare the exact producer length. ON-BODY-CHUNK receives each response DATA
payload when supplied; the payload is still collected when COLLECT-BODY-P is
true.

The client uses its connection-owned QPACK contexts and HTTP/3 decoder adapter
by default. QPACK-ENCODER-CONTEXT selects the message-kit context for outgoing
request field sections. For custom response decoding, supply
QPACK-DECODER-CONTEXT and its QPACK-HTTP3-DECODER-CONTEXT adapter. The client
writes Section Ack instructions on its decoder stream after dynamic response
sections decode and pass role validation. AWAIT-QPACK receives an opaque
blocked token, waits for inserts, resumes it with QPACK-RESUME-BLOCKED-STREAM,
and returns the decoded headers. HUFFMAN-P enables Huffman encoding for newly
emitted field values and names.

ON-PUSH-PROMISE receives the push ID and decoded request fields after the
promise passes HTTP/3 request-field validation. ON-INFORMATION receives each
validated informational response in wire order."
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
         (encoder-context
           (or qpack-encoder-context (http3-client-qpack-encoder-context client)))
         (decoder-context
           (or qpack-decoder-context (http3-client-qpack-decoder-context client)))
         (http3-decoder-context
           (progn
             (when (and qpack-decoder-context
                        (not (eq qpack-decoder-context
                                 (http3-client-qpack-decoder-context client))))
               (%h3-transport-error
                "A custom QPACK decoder context must be the client-owned context."
                qpack-decoder-context))
             (when (and qpack-http3-decoder-context
                        (not (eq qpack-http3-decoder-context
                                 (http3-client-qpack-http3-decoder-context client))))
               (%h3-transport-error
                "A custom HTTP/3 decoder adapter must be the client-owned adapter."
                qpack-http3-decoder-context))
             (or qpack-http3-decoder-context
                 (http3-client-qpack-http3-decoder-context client))))
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
    (progn
      (when (and request-body-function (null known-body-length))
        (setf known-body-length declared-body-length))
      (when (%h3-native-scheduler-p client)
        (return-from send-http3-request
          (%h3-scheduler-submit-entry
           client
           (%make-http3-request-stream
            :request request
            :request-method (http-kit:http-request-method request)
            :request-fields request-fields
            :request-body body
            :request-body-function request-body-function
            :request-body-length known-body-length
            :trailer-fields trailer-fields
            :encoder-context encoder-context
            :huffman-p huffman-p
            :on-stream-open on-stream-open
            :on-body-chunk on-body-chunk
            :on-information on-information
            :on-push-promise on-push-promise
            :collect-body-p collect-body-p
            :max-body-bytes max-body-bytes
            :max-header-bytes max-header-bytes
            :max-fields max-fields
            :timeout timeout
            :deadline (http-kit:http-deadline timeout :deadline deadline)
            :request-write-state
            (list :headers-sent-p nil :body-offset 0
                  :sent-length 0 :finished-p nil))))))

    (when (>= (http3-client-request-stream-count client)
              (http3-client-max-request-streams client))
      (%h3-transport-error
       "The HTTP/3 request-stream limit was exceeded."
       :h3-excessive-load))
    (multiple-value-setq (stream stream-id)
      (funcall (http3-client-open-stream client)
               request :stream-type :request :timeout timeout :deadline deadline))
    (unless stream
      (%h3-transport-error "The HTTP/3 open-stream callback returned NIL."))
    (incf (http3-client-request-stream-count client))
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
                      :h3-request-rejected)))
                 (when (and http3-decoder-context
                            (not (and (integerp stream-id)
                                      (>= stream-id 0))))
                   (%h3-transport-error
                    "Dynamic QPACK response decoding requires a numeric stream ID."
                    stream-id)))
               (when (and decoder-context (null http3-decoder-context))
                 (%h3-transport-error
                  "A QPACK HTTP/3 decoder context is required with the decoder context."
                  decoder-context))
               (when on-stream-open
                 (funcall on-stream-open stream stream-id
                          (lambda (&key (error-code #x10c))
                            (cancel-http3-stream
                             client stream :error-code error-code
                             :timeout timeout :deadline deadline))))
               (progn (%h3-write-request-field-section
                 client stream request-fields encoder-context
                 stream-id huffman-p
                 :fin-p nil
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
                   (progn
                     (loop with position = 0
                           with max-frame-size = (http3-client-max-frame-size client)
                           while (< position (length body))
                           for end = (min (length body)
                                         (+ position max-frame-size))
                           for chunk = (subseq body position end)
                           do (%h3-write-frame
                               client stream
                               (make-http3-frame
                                :type +http3-data-type+ :payload chunk)
                               :fin-p (and (= end (length body))
                                           (null trailer-fields))
                               :timeout timeout :deadline deadline)
                              (setf position end))
                     (when (and (zerop (length body))
                                (null trailer-fields))
                       (%h3-write-frame
                        client stream
                        (make-http3-frame
                         :type +http3-data-type+
                         :payload (http-kit::%empty-octets))
                        :fin-p t :timeout timeout :deadline deadline))))
               (when trailer-fields
                 (%h3-write-request-field-section
                  client stream trailer-fields encoder-context
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
                :qpack-http3-decoder-context http3-decoder-context
                :stream-id stream-id
                :await-qpack await-qpack
                :request-method (http-kit:http-request-method request)
                :on-push-promise on-push-promise
                :timeout timeout
                :deadline deadline)))
           (error (condition)
             (setf failure condition)
             (error condition)))
      (unwind-protect
           (funcall (http3-client-close-stream client)
                    stream :condition failure)
        (when (plusp (http3-client-request-stream-count client))
          (decf (http3-client-request-stream-count client)))))))
(progn
(defun %h3-scheduler-resume-ready-token (client token)
  (let ((entry (%h3-scheduler-entry-for-token client token)))
    (when entry
      (let* ((stream-id (%http3-request-stream-stream-id entry))
             (fields
               (%call-http3-serialized
                client
                (lambda ()
                  (prog1
                      (http-message-kit:qpack-resume-blocked-stream token)
                    (%h3-write-client-qpack-section-ack-serialized
                     client stream-id
                     (%http3-request-stream-timeout entry)
                     (%http3-request-stream-deadline entry))))))
             (continuation (%http3-request-stream-continuation entry)))
        (setf (%http3-request-stream-qpack-token entry) nil
              (%http3-request-stream-qpack-resume-fields entry) fields
              (%http3-request-stream-qpack-resume-fields-p entry) t
              (%http3-request-stream-continuation entry) nil)
        (when continuation
          (push continuation
                (%http3-request-stream-response-frames entry)))
        (%h3-scheduler-add-runnable client stream-id)
        t))))

(defun %h3-scheduler-resume-ready-tokens (client events)
  (loop for event in events
        when (and (consp event)
                  (eq (first event) :qpack-stream-unblocked))
          do (%h3-scheduler-resume-ready-token client (second event))))

(defun %h3-scheduler-read-critical-stream (client stream reader)
  (when stream
    (multiple-value-bind (chunk fin-p status)
        (%h3-try-read-client-stream client stream nil nil)
      (unless (eq status :would-block)
        (%h3-prefetch-stream-input client stream chunk fin-p)
        (multiple-value-bind (events ended-p)
            (funcall reader client :timeout nil :deadline nil)
          (declare (ignore ended-p))
          (values t events))))))

(progn
  (defun %h3-scheduler-drain-discard-streams (client)
    (let ((progress-p nil))
      (dolist (entry (http3-client-discard-peer-unidirectional-streams client))
        (when (eq (cdr entry) :drain)
          (loop
           (multiple-value-bind (chunk fin-p status)
               (%h3-try-read-client-stream client (car entry) nil nil)
             (declare (ignore chunk))
             (when (eq status :would-block)
               (return))
             (setf progress-p t)
             (when (or fin-p (eq status :fin))
               (setf (cdr entry) :finished)
               (return))))))
      progress-p))
  (defun %h3-scheduler-discover-peer-unidirectional-streams (client)
    (let ((accept (http3-client-accept-peer-unidirectional-stream client))
          (pending (http3-client-pending-peer-unidirectional-streams client))
          (progress-p nil)
          (empty (make-array 0 :element-type '(unsigned-byte 8))))
      (when accept
        (loop for stream = (funcall accept)
              while stream
              do (unless (member stream pending :test #'eq)
                   (when (>= (http3-client-peer-unidirectional-stream-count client)
                             (http3-client-max-peer-unidirectional-streams client))
                     (%h3-transport-error
                      "The HTTP/3 peer unidirectional-stream limit was exceeded."
                      :h3-excessive-load))
                   (incf (http3-client-peer-unidirectional-stream-count client))
                   (setf pending (append pending (list stream))
                         progress-p t))))
      (setf (http3-client-pending-peer-unidirectional-streams client) pending)
      (dolist (stream (copy-list pending))
        (multiple-value-bind (chunk fin-p status)
            (%h3-try-read-client-stream client stream nil nil)
          (unless (eq status :would-block) (progn (%h3-check-state-bytes client (length (or chunk empty))) (let* ((entry (assoc stream
                                 (http3-client-peer-unidirectional-prefix-buffers
                                  client)
                                 :test #'eq))
                   (buffer (%http3-concatenate-octets
                            (if entry (first (cdr entry)) empty)
                            (or chunk empty))))
              (multiple-value-bind (stream-type position)
                  (http3-varint-decode buffer :allow-incomplete-p t)
                (cond
                 ((and stream-type
                       (= stream-type +http3-push-stream-type+))
                  (multiple-value-bind (push-id push-position)
                      (http3-varint-decode
                       buffer :position position :allow-incomplete-p t)
                    (declare (ignore push-position))
                    (if push-id
                        (progn
                          (%h3-register-peer-push-stream client stream push-id)
                          (setf pending
                                (remove stream pending :test #'eq))
                          (setf (http3-client-pending-peer-unidirectional-streams
                                 client)
                                (remove stream
                                        (http3-client-pending-peer-unidirectional-streams
                                         client)
                                        :test #'eq)
                                (http3-client-peer-unidirectional-prefix-buffers client)
                                (remove stream
                                        (http3-client-peer-unidirectional-prefix-buffers
                                         client)
                                        :key #'car :test #'eq))
                          (%h3-prefetch-stream-input client stream buffer fin-p)
                          (%h3-with-scheduler-lock (client)
                            (when (http3-client-open-p client)
                              (setf (http3-client-pending-push-streams client)
                                    (append
                                     (http3-client-pending-push-streams client)
                                     (list (cons push-id stream))))))
                          (%h3-check-state-bytes client)
                          (setf progress-p t))
                      (if fin-p
                          (%h3-transport-error
                           "An HTTP/3 push stream ended with a truncated push ID."
                           :h3-stream-creation-error)
                        (progn
                          (setf (http3-client-peer-unidirectional-prefix-buffers
                                 client)
                                (acons
                                 stream (list buffer nil)
                                 (remove
                                  stream
                                  (http3-client-peer-unidirectional-prefix-buffers
                                   client)
                                  :key #'car :test #'eq)))
                          (%h3-check-state-bytes client))))))
                 (stream-type
                  (let ((kind
                         (cond
                          ((= stream-type +http3-control-stream-type+)
                           (attach-http3-peer-control-stream client stream)
                           :control)
                          ((= stream-type +http3-qpack-encoder-stream-type+)
                           (attach-http3-peer-qpack-encoder-stream client stream)
                           :qpack-encoder)
                          ((= stream-type +http3-qpack-decoder-stream-type+)
                           (attach-http3-peer-qpack-decoder-stream client stream)
                           :qpack-decoder)
                          (t :unknown))))
                    (if (eq kind :unknown)
                        (%h3-retain-unknown-peer-stream client stream fin-p)
                      (progn
                        (%h3-prefetch-stream-input client stream buffer fin-p)
                        (%h3-check-state-bytes client)))
                    (setf pending (remove stream pending :test #'eq))
                    (setf (http3-client-pending-peer-unidirectional-streams
                           client)
                          (remove stream
                                  (http3-client-pending-peer-unidirectional-streams
                                   client)
                                  :test #'eq)
                          (http3-client-peer-unidirectional-prefix-buffers client)
                          (remove stream
                                  (http3-client-peer-unidirectional-prefix-buffers
                                   client)
                                  :key #'car :test #'eq))
                    (setf progress-p t)))
                 (fin-p
                  (%h3-transport-error
                   "A peer HTTP/3 unidirectional stream ended before its type prefix."
                   :h3-stream-creation-error))
                 (t
                  (setf (http3-client-peer-unidirectional-prefix-buffers client)
                        (acons
                         stream (list buffer nil)
                         (remove
                          stream
                          (http3-client-peer-unidirectional-prefix-buffers client)
                          :key #'car :test #'eq)))
                  (%h3-check-state-bytes client))))))))
        (when (%h3-scheduler-drain-discard-streams client)
          (setf progress-p t))
        progress-p)))

  (defun %h3-scheduler-poll-critical-streams (client)
    (let ((progress-p
            (%h3-scheduler-discover-peer-unidirectional-streams client)))
      (multiple-value-bind (read-p events)
          (%h3-scheduler-read-critical-stream
           client (http3-client-peer-control-stream client)
           #'read-http3-control-stream)
        (declare (ignore events))
        (when read-p (setf progress-p t)))
      (multiple-value-bind (read-p events)
          (%h3-scheduler-read-critical-stream
           client (http3-client-peer-qpack-encoder-stream client)
           #'read-http3-qpack-encoder-stream)
        (when read-p
          (setf progress-p t)
          (%h3-scheduler-resume-ready-tokens client events)))
      (multiple-value-bind (read-p events)
          (%h3-scheduler-read-critical-stream
           client (http3-client-peer-qpack-decoder-stream client)
           #'read-http3-qpack-decoder-stream)
        (declare (ignore events))
        (when read-p (setf progress-p t)))
      progress-p)))

(defun %h3-scheduler-fail-entry (client entry condition)
  (unless (or (%http3-request-stream-done-p entry)
              (%http3-request-stream-failure entry))
    (let ((cleanup-condition nil)
          (cleanup-returned-p nil))
      (unwind-protect
           (handler-case
               (progn
                 (%h3-scheduler-cancel-entry client entry condition)
                 (setf cleanup-returned-p t))
             (error (caught-condition)
               (setf cleanup-condition caught-condition
                     cleanup-returned-p t)))
        (unless cleanup-returned-p
          (setf cleanup-condition
                (make-condition
                 'http-kit:http-connection-error
                 :message "The HTTP/3 cancellation cleanup exited nonlocally."
                 :operation :http3-request
                 :request-sent-p t)))
        (when cleanup-condition
          (%h3-with-scheduler-lock (client)
            (unless (%http3-request-stream-done-p entry)
              (setf (%http3-request-stream-failure entry) cleanup-condition)
              (%h3-scheduler-broadcast client)))
          (close-http3-client client :condition cleanup-condition))
        (when cleanup-condition
          (error cleanup-condition)))))
  entry)

(defun %h3-scheduler-fail-active (client condition)
  (let ((entries
          (%h3-with-scheduler-lock (client)
            (append (copy-list (http3-client-pending-request-streams client))
                    (loop for entry being the hash-values
                            of (http3-client-request-streams client)
                          collect entry)))))
    (dolist (entry entries)
      (%h3-scheduler-fail-entry client entry condition)))
  client)
(defun %h3-scheduler-fail-connection (client condition)
  (let ((close-failure
          (handler-case
              (progn
                ;; A truthy close result means success, not a failure.
                (close-http3-client client :condition condition)
                nil)
            (error (caught-condition)
              caught-condition))))
    (let ((close-connection (http3-client-close-connection client)))
      (when close-connection
        (handler-case
            (funcall
             close-connection
             :error-code
             (http3-connection-protocol-error-quic-error-code condition)
             :reason (http-kit:http-error-message condition))
          (error (caught-condition)
            (unless close-failure
              (setf close-failure caught-condition))))))
    (when close-failure
      (warn "Closing the HTTP/3 QUIC connection also failed after HTTP/3 connection error (~A): ~A"
            condition close-failure))
    client))

(defun %h3-scheduler-progress-entry (client entry)
  (unless (or (%http3-request-stream-done-p entry)
              (%http3-request-stream-failure entry)
              (%h3-scheduler-close-started-p client))
    (handler-case
        (progn
          (when (%http3-request-stream-cancel-requested-p entry)
            (error
             (make-condition
              (quote http-kit:http-connection-error)
              :message "The HTTP/3 request stream was cancelled."
              :operation :http3-request
              :request-sent-p t
              :cause (list :local-cancel
                           (%http3-request-stream-stream-id entry)
                           (%http3-request-stream-cancel-error-code entry)))))
          (http-kit::%check-deadline
           (%http3-request-stream-deadline entry)
           (function http-kit::%monotonic-time) :read)
          (let ((progress-p (%h3-scheduler-write-entry-step client entry)))
            (unless (or (%http3-request-stream-done-p entry)
                        (%http3-request-stream-failure entry)
                        (%h3-scheduler-close-started-p client))
              (setf progress-p
                    (or (%h3-scheduler-read-entry-step client entry)
                        progress-p)))
            progress-p))
      (error (condition)
        (unless (%h3-scheduler-close-started-p client)
          (if (typep condition 'http3-connection-protocol-error)
              (%h3-scheduler-fail-connection client condition)
              (%h3-scheduler-fail-entry client entry condition)))
        t))))

(defun %h3-scheduler-owner-loop (client owner-entry)
  (let ((normal-exit-p nil))
    (unwind-protect
         (progn
           (loop
             (unless
                 (%h3-with-scheduler-lock (client)
                   (and (null (http3-client-scheduler-close-state client))
                        (not (or (%http3-request-stream-done-p owner-entry)
                                 (%http3-request-stream-failure owner-entry)))))
               (return))
             (let ((progress-p nil))
               (handler-case
                   (progn
                     (funcall (http3-client-poll-function client))
                     (when (%h3-scheduler-poll-critical-streams client)
                       (setf progress-p t))
                     (%h3-with-scheduler-lock (client)
                       (incf (http3-client-scheduler-poll-generation client))
                       (%h3-scheduler-broadcast client))
                     (unless (%h3-scheduler-close-started-p client)
                       (%h3-scheduler-start-pending client)
                       (unless (%h3-scheduler-close-started-p client)
                         (let* ((stream-id (%h3-scheduler-next-runnable-id client))
                                (entry (and stream-id
                                            (gethash stream-id
                                                     (http3-client-request-streams
                                                      client)))))
                           (when (and entry
                                      (%h3-scheduler-progress-entry client entry))
                             (setf progress-p t))))))
                 (error (condition)
                   (unless (%h3-scheduler-close-started-p client)
                     (if (typep condition 'http3-connection-protocol-error)
                         (%h3-scheduler-fail-connection client condition)
                         (%h3-scheduler-fail-active client condition)))))
               (unless progress-p
                 (sleep 0.001))))
           (setf normal-exit-p t))
      (unwind-protect
           (unwind-protect
                (unless (or normal-exit-p
                            (%h3-with-scheduler-lock (client)
                              (or (http3-client-scheduler-close-state client)
                                  (%http3-request-stream-done-p owner-entry)
                                  (%http3-request-stream-failure owner-entry))))
                  (%h3-scheduler-fail-entry
                   client owner-entry
                   (make-condition
                    'http-kit:http-connection-error
                    :message "The HTTP/3 scheduler owner exited before its request completed."
                    :operation :http3-request
                    :request-sent-p (%http3-request-stream-started-p owner-entry))))
             (%h3-with-scheduler-lock (client)
               (let ((close-state (http3-client-scheduler-close-state client)))
                 (cond
                   ((eq close-state :closed)
                    (%h3-scheduler-clear-closed-state-locked client))
                   ((eq close-state :http3-closed)
                    (%h3-scheduler-fail-closed-state-locked
                     client
                     (make-condition
                      'http-kit:http-connection-error
                      :message "The HTTP/3 client was closed."
                      :operation :http3-request)))
                   ((and close-state (not (eq close-state :closing)))
                    (%h3-scheduler-fail-closed-state-locked
                     client close-state)))))
        (%h3-with-scheduler-lock (client)
          (when (eq (http3-client-scheduler-owner-thread client)
                    (%h3-scheduler-current-thread))
            (setf (http3-client-scheduler-owner-thread client) nil))
          (%h3-scheduler-broadcast client)))))
  client))
(defun %h3-scheduler-poll-once (client &optional at)
  #+sbcl
  (let ((thread (%h3-scheduler-current-thread))
        (generation
          (%h3-with-scheduler-lock (client)
            (http3-client-scheduler-poll-generation client))))
    (loop
      for status =
        (%h3-with-scheduler-lock (client)
          (cond
            ((/= generation
                 (http3-client-scheduler-poll-generation client))
             :completed)
            ((http3-client-scheduler-close-state client)
             :closing)
            ((eq thread (http3-client-scheduler-owner-thread client))
             :reentrant)
            ((null (http3-client-scheduler-owner-thread client))
             (setf (http3-client-scheduler-owner-thread client) thread)
             :owned)
            (t
             (%h3-scheduler-condition-wait
              client nil (function http-kit::%monotonic-time))
             :wait)))
      do (case status
           (:owned (return))
           ((:completed :closing :reentrant)
            (return-from %h3-scheduler-poll-once client))))
    (unwind-protect
         (handler-case
             (progn
               (if at
                   (funcall (http3-client-poll-function client) at)
                   (funcall (http3-client-poll-function client)))
               (%h3-scheduler-poll-critical-streams client)
               (%h3-with-scheduler-lock (client)
                 (incf (http3-client-scheduler-poll-generation client))
                 (%h3-scheduler-broadcast client))
               client)
           (http3-connection-protocol-error (condition)
             (%h3-with-scheduler-lock (client)
               (unless (http3-client-scheduler-close-state client)
                 (setf (http3-client-scheduler-close-state client) condition))
               (%h3-scheduler-broadcast client))
             (error condition)))
      (%h3-with-scheduler-lock (client)
        (setf (http3-client-scheduler-owner-thread client) nil)
        (%h3-scheduler-broadcast client))))
  #-sbcl
  (progn
    (if at
        (funcall (http3-client-poll-function client) at)
        (funcall (http3-client-poll-function client)))
    (%h3-scheduler-poll-critical-streams client)
    client))

(defun %h3-scheduler-wait-entry (client entry)
  (handler-case
      (progn
        (loop until (or (%http3-request-stream-done-p entry)
                        (%http3-request-stream-failure entry))
              for owner-p = nil
              do (progn
                   (%h3-with-scheduler-lock (client)
                     (unless (or (%http3-request-stream-done-p entry)
                                 (%http3-request-stream-failure entry))
                       (if (or (http3-client-scheduler-close-state client)
                               (http3-client-scheduler-owner-thread client))
                           (%h3-scheduler-condition-wait
                            client (%http3-request-stream-deadline entry)
                            (function http-kit::%monotonic-time))
                           (setf (http3-client-scheduler-owner-thread client)
                                 (%h3-scheduler-current-thread)
                                 owner-p t))))
                   (when owner-p
                     (%h3-scheduler-owner-loop client entry))))
        (when (%http3-request-stream-failure entry)
          (error (%http3-request-stream-failure entry)))
        (%http3-request-stream-response entry))
    (error (condition)
      (unless (or (eq condition (%http3-request-stream-failure entry))
                  (%h3-scheduler-close-started-p client))
        (%h3-scheduler-request-cancel client entry #x10c))
      (error condition))))

(defun %h3-scheduler-submit-entry (client entry)
  (let ((owner-p nil))
    (%h3-with-scheduler-lock (client)
      (when (http3-client-scheduler-close-state client)
        (error
         (make-condition
          'http-kit:http-connection-error
          :message "The HTTP/3 QUIC connection is closing."
          :operation :http3-request
          :request-sent-p nil)))
      (when (>= (http3-client-request-stream-count client)
                (http3-client-max-request-streams client))
        (%h3-transport-error
         "The HTTP/3 request-stream limit was exceeded."
         :h3-excessive-load))
      (when (eq (http3-client-scheduler-owner-thread client)
                (%h3-scheduler-current-thread))
        (%h3-transport-error
         "A native HTTP/3 request cannot be started reentrantly by its scheduler owner."
         :h3-excessive-load))
      (incf (http3-client-request-stream-count client))
      (push entry (http3-client-pending-request-streams client))
      (unless (http3-client-scheduler-owner-thread client)
        (setf (http3-client-scheduler-owner-thread client)
              (%h3-scheduler-current-thread)
              owner-p t))
      (%h3-scheduler-broadcast client))
    (when owner-p
      (%h3-scheduler-owner-loop client entry))
    (%h3-scheduler-wait-entry client entry)
    (%http3-request-stream-response entry)))

)

(defun send-http3-request/cps
    (client request on-success
     &key on-error on-body-chunk on-information (collect-body-p t)
       max-header-bytes max-fields
       (max-body-bytes http-kit::*default-max-body-bytes*) request-body-function
       request-body-length qpack-encoder-context qpack-decoder-context
       qpack-http3-decoder-context await-qpack (huffman-p nil) on-stream-open
       on-push-promise timeout deadline)
  "Send REQUEST and deliver its response to ON-SUCCESS."
  (http-kit::%call-http-operation/cps
   (lambda ()
     (send-http3-request
      client request
      :on-body-chunk on-body-chunk
      :on-information on-information
      :collect-body-p collect-body-p
      :max-header-bytes max-header-bytes
      :max-fields max-fields
      :max-body-bytes max-body-bytes
      :request-body-function request-body-function
      :request-body-length request-body-length
      :qpack-encoder-context qpack-encoder-context
      :qpack-decoder-context qpack-decoder-context
      :qpack-http3-decoder-context qpack-http3-decoder-context
      :await-qpack await-qpack
      :huffman-p huffman-p
      :on-stream-open on-stream-open
      :on-push-promise on-push-promise
      :timeout timeout
      :deadline deadline))
   on-success
   :on-error on-error))

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
            max-header-bytes max-fields
            (max-body-bytes http-kit::*default-max-body-bytes*)
            qpack-decoder-context qpack-http3-decoder-context
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
  (progn
    (unless stream
      (%h3-transport-error "RECEIVE-HTTP3-PUSH requires a QUIC stream."))
    (when (and qpack-decoder-context
               (not (eq qpack-decoder-context
                        (http3-client-qpack-decoder-context client))))
      (%h3-transport-error
       "A custom QPACK decoder context must be the client-owned context."
       qpack-decoder-context))
    (when (and qpack-http3-decoder-context
               (not (eq qpack-http3-decoder-context
                        (http3-client-qpack-http3-decoder-context client))))
      (%h3-transport-error
       "A custom HTTP/3 decoder adapter must be the client-owned adapter."
       qpack-http3-decoder-context)))
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
               (%h3-register-peer-push-stream client stream push-id)
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
                                 (copy-tree (cdr entry))))))))
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
                        (decoder-context
                          (or qpack-decoder-context
                              (http3-client-qpack-decoder-context client)))
                        (http3-decoder-context
                          (or qpack-http3-decoder-context
                              (http3-client-qpack-http3-decoder-context client))))
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
                     :qpack-http3-decoder-context http3-decoder-context
                     :stream-id stream-id
                     :await-qpack await-qpack
                     :request-method (cdr (assoc ":method" promise :test (function string=)))
                     :timeout timeout
                     :deadline deadline))))))
           (error (condition)
             (setf failure condition)
             (error condition)))
      (unwind-protect
          (funcall (http3-client-close-stream client)
                   stream :condition failure)
        (%h3-forget-peer-push-stream client stream)))))

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
       (serialize (lambda (thunk) (funcall thunk))) qpack-encoder-context
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
                  (http-message-kit:qpack-encode-header-block
                   (%h3-request-fields promised-request)
                   :context qpack-encoder-context
                   :use-dynamic-p nil
                   :huffman huffman-p))
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
                max-frame-size max-header-bytes qpack-encoder-context huffman-p
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
            qpack-encoder-context huffman-p timeout deadline)
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
             (http-message-kit:qpack-encode-header-block
              fields :context qpack-encoder-context
              :use-dynamic-p nil :huffman huffman-p)))
      (when (> (length header-block) max-header-bytes)
        (%h3-size-error :headers max-header-bytes (length header-block)))
      (%h3-server-write-frame
       stream write-stream
       (make-http3-frame :type +http3-headers-type+ :payload header-block)
       max-frame-size :fin-p nil :timeout timeout :deadline deadline))
    response))

(defun %h3-server-send-response
    (stream write-stream response request max-frame-size max-header-bytes
            qpack-encoder-context huffman-p timeout deadline)
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
    (unless (<= 200 status 599)
      (%h3-transport-error
       "HTTP/3 handlers must return a final status from 200 through 599."
       status))
    (progn
      (when (and (or (= status 204) (= status 304)) trailers)
        (%h3-transport-error
         "This HTTP/3 response cannot contain trailers."))
      (when (and body-suppressed-p
                 (or (and body (plusp (length body)))
                     (and declared-body-length (plusp declared-body-length))))
        (unless (and (string= method "HEAD")
                     (/= status 204)
                     (/= status 304))
          (%h3-transport-error
           "This HTTP/3 response cannot contain a body."))))
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
            (http-message-kit:qpack-encode-header-block
             header-fields
             :context qpack-encoder-context
             :use-dynamic-p nil
             :huffman huffman-p))
    (when (> (length header-block) max-header-bytes)
      (%h3-size-error :headers max-header-bytes (length header-block)))
    (when trailer-fields
      (setf trailer-block
              (http-message-kit:qpack-encode-header-block
               trailer-fields
               :context qpack-encoder-context
               :use-dynamic-p nil
               :huffman huffman-p))
      (when (> (length trailer-block) max-header-bytes)
        (%h3-size-error :headers max-header-bytes (length trailer-block))))
    (labels
        ((next-body-chunk ()
           (if body-suppressed-p
               nil
               (if stream-response-p
                   (loop
                     (let ((chunk (funcall body-function)))
                       (cond
                         ((null chunk) (return nil))
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
            (max-header-bytes 65536) (max-fields 256)
            (max-body-bytes http-kit::*default-max-body-bytes*)
            (collect-body-p t) on-body-chunk qpack-decoder-context
            qpack-http3-decoder-context on-qpack-section-acknowledgment
            stream-id await-qpack
            (qpack-serialize (lambda (thunk) (funcall thunk)))
            qpack-encoder-context (huffman-p nil) (enable-connect-p nil)
            timeout deadline on-error)
  "Serve one HTTP/3 request stream over caller-supplied QUIC callbacks.

READ-STREAM is called as (STREAM &KEY TIMEOUT DEADLINE) and returns an octet
vector and a FIN boolean. WRITE-STREAM is called as
(STREAM OCTETS &KEY FIN-P TIMEOUT DEADLINE). CLOSE-STREAM, when supplied, is
called as (STREAM &KEY CONDITION) exactly once. The callbacks own QUIC,
TLS 1.3, ALPN, packet loss recovery, flow control, and socket behavior; this
function owns HTTP/3 request framing, QPACK field validation, request-body
limits, and response framing.

QPACK-DECODER-CONTEXT is the message-kit context for incoming field sections;
QPACK-HTTP3-DECODER-CONTEXT is its HTTP/3 adapter. When the adapter is supplied,
ON-QPACK-SECTION-ACKNOWLEDGMENT is required. It receives STREAM-ID after a
dynamic section has decoded and passed role validation. Static sections do not
invoke it. Use QPACK-ENCODE-SECTION-ACK and the decoder-stream writer in this
callback to send the acknowledgment. If decoding returns a blocked token, no
Ack is sent yet. AWAIT-QPACK receives the opaque token, waits for inserts,
resumes it with QPACK-RESUME-BLOCKED-STREAM, and returns the decoded header
list. For a blocked section, the acknowledgment callback runs once after
successful resume and validation. QPACK-SERIALIZE must call the message-kit
serialization thunk exactly once while holding the connection-wide QPACK lock.

If supplied, QPACK-ENCODER-CONTEXT is the message-kit outbound context. This
server uses static-table references and literal field representations for
responses. HUFFMAN-P enables Huffman encoding for response fields.

HANDLER receives one HTTP request and must return an HTTP response or response
stream as its first value. Its optional second value is a list of informational
HTTP responses sent before the final response. ON-BODY-CHUNK, when supplied,
receives each request DATA payload. If COLLECT-BODY-P is false, handlers receive
an empty request body while the body-length and Content-Length checks still
use all received bytes. ON-ERROR receives (CONDITION REQUEST), where REQUEST
is NIL if parsing failed before the request model was constructed."
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
  (progn
  (when qpack-http3-decoder-context
    (unless (and (integerp stream-id) (>= stream-id 0))
      (%h3-transport-error
       "Dynamic QPACK request decoding requires a numeric stream ID."
       stream-id))
    (unless (functionp on-qpack-section-acknowledgment)
      (%h3-transport-error
       "QPACK HTTP/3 request decoding requires an acknowledgment callback."
       on-qpack-section-acknowledgment))
    (unless (functionp qpack-serialize)
      (%h3-transport-error
       "Dynamic QPACK request decoding requires a serializer."
       qpack-serialize)))
  (progn
    (when (not (eq (null qpack-decoder-context)
                   (null qpack-http3-decoder-context)))
      (%h3-transport-error
       "A QPACK decoder context and its HTTP/3 adapter must be supplied together."
       (list qpack-decoder-context qpack-http3-decoder-context)))
    (when qpack-http3-decoder-context
      (unless (functionp await-qpack)
        (%h3-transport-error
         "A blocked QPACK field section requires an await-qpack callback."
         stream-id)))))
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
                                      (if qpack-http3-decoder-context
                                          (%h3-decode-dynamic-field-section
                                           payload qpack-http3-decoder-context
                                           stream-id await-qpack qpack-serialize
                                           (if request-headers-seen-p
                                               :trailers :request)
                                           (lambda (acknowledged-stream-id)
                                             (%h3-check-qpack-section-acknowledgment-stream-id
                                              stream-id acknowledged-stream-id)
                                             (funcall
                                              on-qpack-section-acknowledgment
                                              acknowledged-stream-id))
                                           max-header-bytes max-fields)
                                          (mapcar
                                           (lambda (header)
                                             (cons (http-message-kit:http-header-name header)
                                                   (http-message-kit:http-header-content header)))
                                           (http-message-kit:qpack-decode-header-block
                                            payload
                                            :context qpack-decoder-context
                                            :stream-id stream-id
                                            :max-header-bytes max-header-bytes
                                            :max-fields max-fields)))))
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
                          max-frame-size max-header-bytes qpack-encoder-context
                          huffman-p timeout deadline))
                       (%h3-server-send-response
                        stream write-stream response request
                        max-frame-size max-header-bytes
                        qpack-encoder-context huffman-p
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
       max-header-bytes max-fields
       (max-body-bytes http-kit::*default-max-body-bytes*)
       request-body-function request-body-length proxy proxy-plan
       qpack-encoder-context qpack-decoder-context qpack-http3-decoder-context
       await-qpack (huffman-p nil) on-stream-open on-push-promise
       timeout deadline)
  "Send REQUEST through a reusable HTTP/3 connection manager."
  (when (or proxy
            (and proxy-plan
                 (not
                  (or (eq proxy-plan :direct)
                      (let ((length
                              (handler-case (list-length proxy-plan)
                                (type-error () nil))))
                        (and length
                             (evenp length)
                             (loop for tail on proxy-plan by #'cddr
                                   always (keywordp (car tail)))
                             (eq (getf proxy-plan :mode) :direct)
                             (null (getf proxy-plan :proxy))))))))
    (%h3-transport-error
     "HTTP/3 proxy routing is not supported by this transport."
     (or proxy proxy-plan)))
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
                 :qpack-encoder-context qpack-encoder-context
                 :qpack-decoder-context qpack-decoder-context
                 :qpack-http3-decoder-context qpack-http3-decoder-context
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
           &key timeout deadline max-header-bytes max-fields
             (max-body-bytes http-kit::*default-max-body-bytes*)
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
