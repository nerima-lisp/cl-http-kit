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
          (ignore-errors
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
          (when (plusp (length buffer))
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
                       (when (plusp (length buffer))
                         (%http3-control-error
                          "The HTTP/3 control stream ended with a truncated frame."))
                       (unless (http3-control-state-settings-received-p state)
                         (%http3-control-error
                          "The HTTP/3 control stream ended before SETTINGS."))
                       (return (values state t))))))
             (error (condition)
               (setf failure condition)
               (when on-error
                 (ignore-errors (funcall on-error condition state)))
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

(defun %h3-request-fields (request)
  (let* ((method (http-kit:http-request-method request))
         (uri (http-kit:http-request-uri request))
         (body (http-kit:http-request-body request))
         (authority (http-kit:http-uri-authority uri))
         (path (http-kit:http-uri-path uri))
         (query (http-kit:http-uri-query uri))
         (headers (http-kit:http-request-headers request))
         (regular '())
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
      (when (and (plusp (length body))
                 (not (find "content-length" regular :key #'car :test #'string=)))
        (setf regular
              (append regular (list (cons "content-length"
                                          (princ-to-string (length body)))))))
      (append fields (nreverse regular)))))

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
        (if (and (plusp (length name)) (char= (char name 0) #\:))
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

(defun %h3-read-response
    (client stream &key on-body-chunk collect-body-p max-body-bytes
            qpack-decoder-table timeout deadline)
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
                        (qpack-decode-field-section
                         payload
                         :max-header-bytes
                         (http3-client-max-header-bytes client)
                         :dynamic-table qpack-decoder-table)))
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
                              ;; Informational response fields are valid but are
                              ;; intentionally not retained by this one-shot API.
                              nil))))))
               ((= type +http3-data-type+)
                (unless final-response-p
                  (%h3-transport-error
                   "An HTTP/3 DATA frame arrived before final response HEADERS."))
                (when trailers-seen-p
                  (%h3-transport-error
                   "An HTTP/3 DATA frame arrived after response trailers."))
                (let ((new-length (+ body-length (length payload))))
                  (when (and max-body-bytes (> new-length max-body-bytes))
                    (%h3-size-error :body max-body-bytes new-length))
                  (setf body-length new-length)
                  (when collect-body-p
                    (setf body (%h3-append-body body payload)))
                  (when on-body-chunk
                    (funcall on-body-chunk (%http3-copy-octets payload)))))
               ((= type +http3-push-promise-type+)
                (error 'http-unsupported-feature
                       :message "HTTP/3 server push is not implemented by this client."
                       :operation :http3-response
                       :feature :http3-server-push))
               ((member type (list +http3-settings-type+
                                   +http3-cancel-push-type+
                                   +http3-goaway-type+
                                   +http3-max-push-id-type+)
                        :test #'=)
                (%h3-transport-error
                 "HTTP/3 control frames are not valid on a request stream." type))
               (t
                ;; Extension frame types are ignored by HTTP/3 endpoints.
                nil))))
         (finish-response ()
           (unless final-response-p
             (%h3-transport-error "The HTTP/3 stream ended before final response HEADERS."))
           (%h3-response-content-length headers body-length)
           (http-kit:make-http-response
            :protocol-version "HTTP/3"
            :status status
            :headers headers
            :trailers trailers
            :body (if collect-body-p body nil))))
      (loop
        (multiple-value-bind (chunk fin-p)
            (funcall (http3-client-read-stream client)
                     stream :timeout timeout :deadline deadline)
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

(defun send-http3-request
    (client request &key on-body-chunk (collect-body-p t) max-body-bytes
            qpack-encoder-table qpack-decoder-table (huffman-p nil)
            timeout deadline)
  "Send REQUEST on a new HTTP/3 bidirectional stream and read its response.

The request and response bodies are represented as octet vectors.  When
ON-BODY-CHUNK is supplied it receives each response DATA payload; the payload
is still collected when COLLECT-BODY-P is true.  QPACK-ENCODER-TABLE and
QPACK-DECODER-TABLE, when supplied, are caller-owned dynamic tables used for
request and response field sections.  HUFFMAN-P enables Huffman encoding for
newly emitted field values and names."
  (unless (http3-client-p client)
    (%h3-transport-error "SEND-HTTP3-REQUEST requires an HTTP/3 client."
                         (type-of client)))
  (unless (http3-client-open-p client)
    (%h3-transport-error "The HTTP/3 client is already closed."))
  (unless (http-kit:http-request-p request)
    (%h3-transport-error "SEND-HTTP3-REQUEST requires an HTTP request."
                         (type-of request)))
  (unless (or (null max-body-bytes) (%h3-non-negative-limit-p max-body-bytes))
    (%h3-transport-error "max-body-bytes must be NIL or a non-negative integer."
                         max-body-bytes))
  (let* ((body (http-kit:http-request-body request))
         (request-fields (%h3-request-fields request))
         (trailer-fields (%h3-trailer-fields request))
         (header-block
           (qpack-encode-field-section
            request-fields
            :dynamic-table qpack-encoder-table
            :huffman-p huffman-p))
         (stream nil)
         (failure nil))
    (setf stream
          (funcall (http3-client-open-stream client)
                   request :stream-type :request :timeout timeout :deadline deadline))
    (unless stream
      (%h3-transport-error "The HTTP/3 open-stream callback returned NIL."))
    (unwind-protect
         (handler-case
             (progn
                (%h3-write-frame
                 client stream
                 (make-http3-frame :type +http3-headers-type+
                                   :payload header-block)
                :fin-p (and (zerop (length body)) (null trailer-fields))
                :timeout timeout :deadline deadline)
               (unless (zerop (length body))
                 (loop with position = 0
                       while (< position (length body))
                       for end = (min (length body)
                                      (+ position (http3-client-max-frame-size client)))
                       for last-p = (= end (length body))
                       do (%h3-write-frame
                           client stream
                            (make-http3-frame
                             :type +http3-data-type+
                             :payload (subseq body position end))
                           :fin-p (and last-p (null trailer-fields))
                           :timeout timeout :deadline deadline)
                          (setf position end)))
               (when trailer-fields
                  (%h3-write-frame
                   client stream
                   (make-http3-frame
                    :type +http3-headers-type+
                    :payload
                    (qpack-encode-field-section
                     trailer-fields
                     :dynamic-table qpack-encoder-table
                     :huffman-p huffman-p))
                  :fin-p t :timeout timeout :deadline deadline))
               (%h3-read-response
                client stream
                :on-body-chunk on-body-chunk
                :collect-body-p collect-body-p
                :max-body-bytes max-body-bytes
                :qpack-decoder-table qpack-decoder-table
                :timeout timeout
                :deadline deadline))
           (error (condition)
             (setf failure condition)
             (error condition)))
      (funcall (http3-client-close-stream client)
               stream :condition failure))))

(defun %h3-server-pseudo-field-p (name)
  (and (plusp (length name))
       (char= (char name 0) #\:)))

(defun %h3-server-parse-request-fields (fields)
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
              (unless (member name '(":method" ":scheme" ":authority" ":path")
                       :test #'string=)
                (error 'http-unsupported-feature
                       :message "This HTTP/3 server does not implement the request pseudo-field."
                       :operation :http3-request
                       :feature name))
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
           (scheme (cdr (assoc ":scheme" pseudo :test #'string=)))
           (authority (cdr (assoc ":authority" pseudo :test #'string=)))
           (path (cdr (assoc ":path" pseudo :test #'string=)))
           (target nil)
           (uri-scheme scheme)
           (uri-path nil)
           (uri-query nil))
      (unless (and method (http-kit::%token-p method))
        (%h3-transport-error
         "An HTTP/3 request must contain a valid :method pseudo-field."))
      (unless (and authority (plusp (length authority)))
        (%h3-transport-error
         "An HTTP/3 request must contain a non-empty :authority pseudo-field."))
      (when (and host-values
                 (or (/= (length host-values) 1)
                     (not (string-equal (first host-values) authority))))
        (%h3-invalid-header "host" "host must match :authority."))
      (if (string= method "CONNECT")
          (progn
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
                          target path))))))
      (values method
              uri-scheme
              authority
              target
              uri-path
              uri-query
              headers))))

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
        (when (or (%h3-connection-specific-name-p name)
                  (member name '("content-length" "host") :test #'string=))
          (%h3-invalid-header
           name "the field is forbidden in HTTP/3 request trailers."))
        (when (and (string= name "te")
                   (not (%h3-te-value-p value)))
          (%h3-invalid-header
           name "HTTP/3 permits only the trailers value for te."))
        (push (http-kit:make-http-header name value) headers)))))

(defun %h3-server-make-request
    (request-info body body-length trailers collect-body-p)
  (destructuring-bind
      (method scheme authority target path query headers)
      request-info
    (%h3-content-length headers body-length)
    (http-kit:make-http-request
     :method method
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
        (when (and (string= name "te")
                   (not (%h3-te-value-p value)))
          (%h3-invalid-header
           name "HTTP/3 permits only the trailers value for te."))
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
        (when (or (%h3-connection-specific-name-p name)
                  (member name '("content-length" "host") :test #'string=))
          (%h3-invalid-header
           name "the field is forbidden in HTTP/3 response trailers."))
        (when (and (string= name "te")
                   (not (%h3-te-value-p value)))
          (%h3-invalid-header
           name "HTTP/3 permits only the trailers value for te."))
        (push (cons name value) fields)))))

(defun %h3-server-write-frame
    (stream write-stream frame max-frame-size &key fin-p timeout deadline)
  (let ((payload (http3-frame-payload frame)))
    (when (> (length payload) max-frame-size)
      (%h3-size-error :frame max-frame-size (length payload)))
    (funcall write-stream stream (encode-http3-frame frame)
             :fin-p fin-p :timeout timeout :deadline deadline)))

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
           (or (string= method "HEAD")
               (and (integerp status) (= status 204))
               (and (integerp status) (= status 304))))
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
         "HTTP/3 status 204 and 304 responses cannot contain a body.")))
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
        (unless body-suppressed-p
          (%h3-response-content-length headers actual-body-length))
        response))))

(defun serve-http3-request-stream
    (stream handler &key read-stream write-stream close-stream
            (max-frame-size +http3-default-max-frame-size+)
            (max-header-bytes 65536) (max-fields 256) max-body-bytes
            (collect-body-p t) on-body-chunk qpack-decoder-table
            qpack-encoder-table (huffman-p nil) timeout deadline on-error)
  "Serve one HTTP/3 request stream over caller-supplied QUIC callbacks.

READ-STREAM is called as (STREAM &KEY TIMEOUT DEADLINE) and returns an octet
vector and a FIN boolean.  WRITE-STREAM is called as
(STREAM OCTETS &KEY FIN-P TIMEOUT DEADLINE).  CLOSE-STREAM, when supplied, is
called as (STREAM &KEY CONDITION) exactly once.  The callbacks own QUIC,
TLS 1.3, ALPN, packet loss recovery, flow control, and socket behavior; this
function owns the HTTP/3 request stream framing, QPACK field validation,
request-body limits, and response framing.  QPACK-DECODER-TABLE and
QPACK-ENCODER-TABLE are caller-owned dynamic tables for request and response
field sections.  HUFFMAN-P enables Huffman encoding for response fields.

HANDLER receives one HTTP request and must return an HTTP response or response
stream.  ON-BODY-CHUNK, when supplied, receives each request DATA payload.  If
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
                                      (qpack-decode-field-section
                                       payload
                                       :max-header-bytes max-header-bytes
                                       :max-fields max-fields
                                       :dynamic-table qpack-decoder-table)))
                                (if (not request-headers-seen-p)
                                    (progn
                                      (multiple-value-bind
                                            (method scheme authority target path query headers)
                                          (%h3-server-parse-request-fields fields)
                                        (setf request-info
                                              (list method scheme authority target
                                                    path query headers)))
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
                                                 +http3-push-promise-type+)
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
                     (%h3-server-send-response
                      stream write-stream
                      (funcall handler request)
                      request max-frame-size max-header-bytes
                      qpack-encoder-table huffman-p
                      timeout deadline)))))
           (error (condition)
             (setf failure condition)
             (when on-error
               (ignore-errors (funcall on-error condition request)))
             (error condition)))
      (funcall closer stream :condition failure))))
