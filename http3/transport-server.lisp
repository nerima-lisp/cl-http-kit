(in-package #:http-kit/http3)

(defun %h3-server-pseudo-field-p (name)
  (and (string/= name "")
       (char= (char name 0) #\:)))

(defun %h3-server-pseudo-values (pseudo)
  (values (cdr (assoc ":method" pseudo :test #'string=))
          (cdr (assoc ":scheme" pseudo :test #'string=))
          (cdr (assoc ":authority" pseudo :test #'string=))
          (cdr (assoc ":path" pseudo :test #'string=))))

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
    (multiple-value-bind (method scheme authority path)
        (%h3-server-pseudo-values pseudo)
      (let* ((target nil)
             (uri-scheme scheme)
             (uri-path nil)
             (uri-query nil))
      (unless (and method (http-kit::%token-p method))
        (%h3-transport-error
         "An HTTP/3 request must contain a valid :method pseudo-field."))
      (unless (and authority (string/= authority ""))
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
                  (unless (and (string/= path "")
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
              headers)))))

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
               (or (and body (plusp (array-total-size body)))
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
                         ((zerop (array-total-size chunk)) nil)
                         (t (return (%http3-copy-octets chunk))))))
                   (progn
                     (if body-done-p
                         nil
                         (progn
                           (setf body-done-p t)
                           (if (plusp (array-total-size body))
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
            (max-header-bytes 65536) (max-fields 256)
            (max-body-bytes http-kit::*default-max-body-bytes*)
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
                   (when (plusp (array-total-size chunk))
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
                   (when (plusp (array-total-size buffer))
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
               (http-kit::%with-http-cleanup
                 (funcall on-error condition request)))
             (error condition)))
      (funcall closer stream :condition failure))))
