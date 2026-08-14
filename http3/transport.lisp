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
  close-stream
  control-stream
  peer-control-stream
  peer-control-state
  (peer-control-buffer #() :type vector)
  (peer-control-prefix-seen-p nil :type boolean)
  (peer-control-fin-p nil :type boolean)
  (max-frame-size #x4000 :type integer)
  (max-header-bytes 65536 :type integer)
  (qpack-settings nil)
  (open-p t))

(defun make-http3-client
    (&key open-stream write-stream read-stream close-stream
          (max-frame-size #x4000) (max-header-bytes 65536)
          (qpack-settings '()) peer-control-stream timeout deadline)
  "Open an HTTP/3 client over caller-supplied QUIC stream callbacks.

OPEN-STREAM is called as (REQUEST &KEY STREAM-TYPE TIMEOUT DEADLINE) and
returns an opaque stream object.  WRITE-STREAM is called as
(STREAM OCTETS &KEY FIN-P TIMEOUT DEADLINE), while READ-STREAM is called as
(STREAM &KEY TIMEOUT DEADLINE) and returns an octet vector, or NIL at QUIC
stream FIN.  CLOSE-STREAM is called as (STREAM &KEY CONDITION).  The
callbacks own QUIC, TLS, ALPN, socket, and event-loop behavior; this system
only supplies the HTTP/3 stream and codec layer."
  (dolist (callback (list open-stream write-stream read-stream))
    (unless (functionp callback)
      (%h3-transport-error "HTTP/3 open, write, and read callbacks are required."
                           (type-of callback))))
  (unless (or (null close-stream) (functionp close-stream))
    (%h3-transport-error "HTTP/3 close-stream must be a function or NIL."
                         (type-of close-stream)))
  (unless (%h3-positive-limit-p max-frame-size)
    (%h3-transport-error "HTTP/3 max-frame-size must be a positive QUIC varint."
                         max-frame-size))
  (unless (%h3-positive-limit-p max-header-bytes)
    (%h3-transport-error "HTTP/3 max-header-bytes must be a positive integer."
                         max-header-bytes))
  (let* ((settings (%h3-copy-settings qpack-settings))
         (closer (or close-stream
                     (lambda (stream &key condition)
                       (declare (ignore stream condition))
                       nil)))
         (control-stream nil))
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
          (let ((settings-frame
                  (make-http3-settings-frame :extra-settings settings)))
            (funcall write-stream control-stream
                     (%http3-concatenate-octets
                      (http3-control-stream-prefix)
                      (encode-http3-frame settings-frame))
                     :fin-p nil
                     :timeout timeout
                     :deadline deadline))
          (%make-http3-client
           :open-stream open-stream
           :write-stream write-stream
           :read-stream read-stream
           :close-stream closer
           :control-stream control-stream
           :peer-control-stream peer-control-stream
           :peer-control-state (make-http3-control-state)
           :peer-control-buffer (make-array 0 :element-type '(unsigned-byte 8))
           :peer-control-prefix-seen-p nil
           :peer-control-fin-p nil
           :max-frame-size max-frame-size
           :max-header-bytes max-header-bytes
           :qpack-settings settings
           :open-p t))
      (error (condition)
        (when control-stream
          (http-kit::%with-http-cleanup
            (funcall closer control-stream :condition condition)))
        (error condition)))))

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
                             (http3-client-peer-control-stream client)))
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
        (funcall (http3-client-read-stream client)
                 stream :timeout timeout :deadline deadline)
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
          (setf (http3-client-peer-control-fin-p client) t))
        (setf (http3-client-peer-control-buffer client) buffer)
        (values events ended-p)))))

(defun serve-http3-control-stream
    (stream &key read-stream close-stream
            (max-frame-size +http3-default-max-frame-size+)
            on-settings on-goaway on-max-push-id on-cancel-push
            timeout deadline on-error)
  "Serve one incoming HTTP/3 control stream over QUIC callbacks.

READ-STREAM is called as (STREAM &KEY TIMEOUT DEADLINE) and returns an octet
vector and a FIN boolean.  CLOSE-STREAM, when supplied, is called as
(STREAM &KEY CONDITION) exactly once.  The stream must begin with the
HTTP/3 control-stream type prefix and a SETTINGS frame.  ON-SETTINGS receives
(SETTINGS STATE), while ON-GOAWAY, ON-MAX-PUSH-ID, and ON-CANCEL-PUSH receive
(IDENTIFIER STATE).  ON-ERROR receives (CONDITION STATE) before the condition
is re-signaled."
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
  (let ((closer (or close-stream
                    (lambda (ignored-stream &key condition)
                      (declare (ignore ignored-stream condition))
                      nil)))
        (state (make-http3-control-state))
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
                       (return (values state t))))))
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
                 "transfer-encoding" "upgrade" "trailer")
          :test #'string=))

(defun %h3-te-value-p (value)
  (every (lambda (token)
           (string= token "trailers"))
         (http-kit::%split-comma-values (list value))))

(defun %h3-header-values (header)
  (list (http-kit:http-header-content header)))

(defun %h3-content-length (headers body-length)
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
        (unless (= length body-length)
          (%h3-invalid-header "content-length"
                              "content-length does not match the request body."))
        length))))

(defun %h3-request-metadata (request)
  (let* ((uri (http-kit:http-request-uri request))
         (authority (http-kit:http-uri-authority uri)))
    (values (http-kit:http-request-method request)
            uri
            (http-kit:http-request-body request)
            authority
            (http-kit:http-uri-path uri)
            (http-kit:http-uri-query uri)
            (http-kit:http-request-headers request))))

(defun %h3-request-fields (request)
  (multiple-value-bind (method uri body authority path query headers)
      (%h3-request-metadata request)
    (let ((regular '())
          (host-values '()))
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
    (%h3-content-length headers (length body))
      (let ((fields
            (if (string= method "CONNECT")
                (list (cons ":method" method)
                      (cons ":authority" authority))
                (list (cons ":method" method)
                      (cons ":scheme" (http-kit:http-uri-scheme uri))
                      (cons ":authority" authority)
                      (cons ":path"
                            (if query
                                (concatenate 'string path "?" query)
                                path))))))
      (when (and (plusp (array-total-size body))
                 (not (find "content-length" regular :key #'car :test #'string=)))
        (setf regular
              (append regular (list (cons "content-length"
                                          (princ-to-string (length body)))))))
        (append fields (nreverse regular))))))

(defun %h3-trailer-fields (request)
  (let ((result '()))
    (dolist (header (http-kit:http-request-trailers request)
             (nreverse result))
      (let ((name (%h3-header-name header)))
        (when (or (%h3-connection-specific-name-p name)
                  (member name '("content-length" "host") :test #'string=))
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
              (when (%h3-connection-specific-name-p name)
                (%h3-invalid-header name "connection-specific fields are forbidden."))
              (when (and (string= name "te")
                         (not (%h3-te-value-p value)))
                (%h3-invalid-header name
                                    "HTTP/3 permits only the trailers value."))
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

(defun %h3-response-content-length (headers body-length)
  (let ((values
          (loop for header in headers
                when (string= (http-kit:http-header-name header)
                              "content-length")
                  collect (http-kit:http-header-content header))))
    (when values
      (unless (and (= (length values) 1)
                   (http-kit::%decimal-string-p (first values))
                   (= (http-kit::%parse-decimal (first values)) body-length))
        (%h3-invalid-header "content-length"
                            "content-length does not match the response body.")))))
