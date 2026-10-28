(in-package #:http-kit/http2)

(defun %h2-concat (vectors)
  (if vectors
      (apply #'http-kit::%join-octets vectors)
      (http-kit::%empty-octets)))

(defun %h2-validate-request-body-options
    (request request-body-function request-body-length)
  (when request-body-function
    (unless (functionp request-body-function)
      (error 'http-kit:http-protocol-error
             :message "The HTTP/2 request body producer must be a function."
             :operation :http2-write
             :detail request-body-function)))
  (unless (or (null request-body-length)
              (and (integerp request-body-length)
                   (>= request-body-length 0)))
    (error 'http-kit:http-protocol-error
           :message "The HTTP/2 request body length must be a non-negative integer or NIL."
           :operation :http2-write
           :detail request-body-length))
  (when (and request-body-length (null request-body-function))
    (error 'http-kit:http-protocol-error
           :message "A request body length requires a request body producer."
           :operation :http2-write
           :detail request-body-length))
  (when (and request-body-function
             (string= (http-kit:http-request-method request) "TRACE"))
    (error 'http-kit:http-protocol-error
           :message "TRACE requests must not contain content."
           :operation :http2-write
           :detail :trace-content))
  (when (and request-body-function
             (plusp (length (http-kit:http-request-body request))))
    (error 'http-kit:http-protocol-error
           :message "A request body producer cannot be combined with an in-memory request body."
           :operation :http2-write
           :detail request))
  (values request-body-function request-body-length))

(defun %h2-validate-request-body-chunk (chunk max-frame-size)
  (unless (and (arrayp chunk)
               (= (array-rank chunk) 1)
               (not (stringp chunk)))
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 request body producer must return a one-dimensional octet array or NIL."
           :operation :http2-write
           :detail (type-of chunk)))
  (when (zerop (array-total-size chunk))
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 request body producer returned an empty chunk."
           :operation :http2-write
           :detail chunk))
  (when (> (length chunk) max-frame-size)
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 request body producer returned a chunk larger than the advertised maximum."
           :operation :http2-write
           :detail (list (length chunk) max-frame-size)))
  (loop for octet across chunk
        unless (and (integerp octet) (<= 0 octet 255))
          do (error 'http-kit:http-protocol-error
                    :message "An HTTP/2 request body chunk contains a non-octet value."
                    :operation :http2-write
                    :detail octet))
  chunk)

(defun %h2-data-frames (body max-frame-size &optional (stream-id 1)
                                      (end-stream-p t))
  (when (> (length body) +http2-default-window-size+)
    (error 'http-kit:http-unsupported-feature
           :message "One-shot HTTP/2 wire serialization cannot consume peer WINDOW_UPDATE frames; use a connection or open-stream transport for request bodies larger than the initial flow-control window."
           :operation :http2-write
           :feature :http2-flow-control))
  (let ((frames '())
        (position 0)
        (length (length body)))
    (loop while (< position length)
          do (let* ((size (min max-frame-size (- length position)))
                    (last (= (+ position size) length))
                    (payload (subseq body position (+ position size))))
               (push (%h2-frame-wire +http2-data-type+
                                     (if (and last end-stream-p)
                                         +http2-end-stream-flag+
                                         0)
                                     stream-id payload)
                     frames)
               (incf position size)))
    (nreverse frames)))

(defun %h2-request-trailer-fields (request)
  (mapcar
   (lambda (header)
     (let* ((name (string-downcase (http-kit:http-header-name header)))
            (value (http-kit:http-header-content header))
            (field (cons name value)))
       (when (http-kit::%forbidden-trailer-field-name-p name)
         (error 'http-kit:http-invalid-header
                :message "The field definition does not permit this HTTP/2 request trailer."
                :operation :http2-trailers
                :name name
                :reason :forbidden))
       (when (%h2-connection-specific-header-p name)
         (error 'http-kit:http-invalid-header
                :message "Connection-specific headers are forbidden in HTTP/2 request trailers."
                :operation :http2-trailers
                :name name
                :reason :connection-specific))
       (%h2-hpack-header-object field)
       field))
   (http-kit:http-request-trailers request)))
(progn
  (defun %h2-make-hpack-context
      (&key (max-table-size +hpack-default-table-size+))
    (http-message-kit:make-hpack-context
     :max-table-size max-table-size
     :max-header-list-size +http2-default-hpack-max-header-bytes+
     :max-field-count +http2-default-hpack-max-field-count+
     :validate-header-names t
     :huffman nil))

  (defun %h2-hpack-header-object (field)
    (let ((name (car field))
          (value (cdr field)))
      (if (and (stringp name)
               (plusp (length name))
               (char= (char name 0) #\:))
          (http-message-kit:make-http-pseudo-header name value)
          (http-message-kit:make-http-header name value))))

  (defun %h2-encode-header-block (fields &key (huffman-p nil))
    (http-message-kit:hpack-encode-header-block
     (mapcar #'%h2-hpack-header-object fields)
     :static-only-p t
     :huffman huffman-p))

  (defun %h2-decode-header-block
      (octets context &key max-header-bytes max-fields)
    (handler-case
        (multiple-value-bind (headers end-position)
            (http-message-kit:hpack-decode-header-block
             octets
             :context context
             :max-header-bytes
             (min (or max-header-bytes
                      +http2-default-hpack-max-header-bytes+)
                  +http2-default-hpack-max-header-bytes+)
             :max-fields
             (min (or max-fields +http2-default-hpack-max-field-count+)
                  +http2-default-hpack-max-field-count+)
             :validate-header-names t)
          (declare (ignore end-position))
          (mapcar (lambda (header)
                    (cons (http-message-kit:http-header-name header)
                          (http-message-kit:http-header-content header)))
                  headers))
      (http-kit:http-protocol-error (condition)
        (let* ((detail (http-kit:http-protocol-error-detail condition))
               (codec-detail
                 (if (and (listp detail)
                          (eq (getf detail :codec-operation) :hpack-decode))
                     (getf detail :codec-detail)
                     detail))
               (name
                (when (and (vectorp codec-detail)
                           (every (lambda (octet)
                                    (typep octet '(unsigned-byte 8)))
                                  codec-detail))
                  (map 'string
                       (lambda (octet) (or (code-char octet) #\?))
                       codec-detail))))
          (cond
            ((and (eq :hpack (http-kit:http-error-operation condition))
                  (and (listp detail)
                      (eq (getf detail :codec-operation) :hpack-decode))
                  (string= (http-kit:http-error-message condition)
                           "An HPACK table size update exceeds the decoder limit.")
                  (integerp codec-detail)
                  (http-message-kit:hpack-context-p context))
             (error 'http-kit:http-protocol-error
                    :message (http-kit:http-error-message condition)
                    :operation :hpack
                    :detail
                    (list codec-detail
                          (http-message-kit:hpack-context-max-table-size context))))
            ((and (listp detail)
                  (eq (getf detail :codec-operation) :hpack-header-block)
                  (string= (http-kit:http-error-message condition)
                           "The HPACK header field count exceeds its limit.")
                  (integerp (getf detail :codec-detail)))
             (error 'http-kit:http-size-limit-exceeded
                    :message (http-kit:http-error-message condition)
                    :operation :hpack
                    :limit (getf detail :codec-detail)
                    :observed (1+ (getf detail :codec-detail))
                    :kind :headers))
            ((and (eq :hpack (http-kit:http-error-operation condition))
                  (and (listp detail)
                      (eq (getf detail :codec-operation) :hpack-decode))
                  (string= (http-kit:http-error-message condition)
                           "An HPACK header name is invalid for HTTP/2.")
                  (stringp name))
             (error 'http-kit:http-invalid-header
                    :message "HTTP/2 header names must be lowercase ASCII tokens."
                    :operation :hpack
                    :name name
                    :reason :name))
            (t
             (error condition))))))))

(define-condition %h2-local-request-header-size-error
    (http-kit:http-size-limit-exceeded)
  ())

(defun %h2-check-request-header-limit (observed limit)
  (handler-case
      (http-kit::%check-limit :headers observed limit)
    (http-kit:http-size-limit-exceeded (condition)
      (error '%h2-local-request-header-size-error
             :message (http-kit:http-error-message condition)
             :operation (http-kit:http-error-operation condition)
             :detail (http-kit:http-protocol-error-detail condition)
             :limit (http-kit:http-size-limit-exceeded-limit condition)
             :observed (http-kit:http-size-limit-exceeded-observed condition)
             :kind (http-kit:http-size-limit-exceeded-kind condition)))))

(defun %h2-request-header-wire (request max-frame-size max-header-bytes
                                max-body-bytes
                                &key (stream-id 1) (include-session-p t)
                                     peer-max-frame-size
                                     peer-max-header-list-size
                                     request-body-function request-body-length
                                     (huffman-p nil))
  (let* ((body (http-kit:http-request-body request))
         (headers (http-kit:http-request-headers request))
         (body-length-known-p
           (or (null request-body-function)
               (not (null request-body-length))))
         (known-body-length
           (and body-length-known-p
                (if request-body-function
                    request-body-length
                    (length body))))
         (fields (%h2-request-fields
                  request
                  :body-length known-body-length
                  :body-length-known-p body-length-known-p))
         (declared-body-length
           (%h2-content-length headers known-body-length
                               :body-length-known-p body-length-known-p))
         (expected-body-length
           (if request-body-function
               (or request-body-length declared-body-length)
               (length body)))
         (trailer-fields (%h2-request-trailer-fields request))
         (header-size (%h2-header-list-size fields))
         (trailer-size (%h2-header-list-size trailer-fields)))
    (%h2-check-request-header-limit header-size max-header-bytes)
    (%h2-check-request-header-limit trailer-size max-header-bytes)
    (when peer-max-header-list-size
      (%h2-check-request-header-limit header-size peer-max-header-list-size)
      (%h2-check-request-header-limit trailer-size peer-max-header-list-size))
    (http-kit::%check-limit :body (or expected-body-length (length body))
                            max-body-bytes)
    (let* ((block (%h2-encode-header-block fields :huffman-p huffman-p))
           ;; A peer has not received our SETTINGS yet, so the initial
           ;; request must obey HTTP/2's default peer receive limit.
           (outgoing-frame-size
             (min max-frame-size
                  (or peer-max-frame-size +http2-default-max-frame-size+)))
           (frames (%h2-header-frames
                   block
                   (and (null request-body-function)
                        (zerop (array-total-size body))
                        (null trailer-fields))
                                      outgoing-frame-size stream-id)))
      (values
       (%h2-concat
        (append (if include-session-p
                    (list +http2-connection-preface+
                          (%h2-settings-wire max-frame-size))
                    '())
                frames))
       body
       outgoing-frame-size
       expected-body-length
       trailer-fields))))

(defun %h2-settings-wire (max-frame-size &key enable-connect-p)
  (let ((payload (make-array (if enable-connect-p 18 12)
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    ;; Disable server push and advertise the local maximum frame size.
    (%h2-put-u16 payload 0 2)
    (%h2-put-u32 payload 2 0)
    (%h2-put-u16 payload 6 5)
    (%h2-put-u32 payload 8 max-frame-size)
    (when enable-connect-p
      (%h2-put-u16 payload 12 8)
      (%h2-put-u32 payload 14 1))
    (%h2-frame-wire +http2-settings-type+ 0 0 payload)))

(defun %h2-request-wire (request max-frame-size max-header-bytes max-body-bytes
                         &key (stream-id 1) (include-session-p t)
                              peer-max-frame-size (huffman-p nil))
    (multiple-value-bind (header-wire body outgoing-frame-size
                       expected-body-length trailer-fields)
      (%h2-request-header-wire request max-frame-size max-header-bytes
                               max-body-bytes
                               :stream-id stream-id
                               :include-session-p include-session-p
                               :peer-max-frame-size peer-max-frame-size
                               :huffman-p huffman-p)
    (declare (ignore expected-body-length))
    (%h2-concat
     (cons header-wire
           (append (%h2-data-frames body outgoing-frame-size stream-id
                                    (null trailer-fields))
                   (if trailer-fields
                       (%h2-header-frames
                        (%h2-encode-header-block trailer-fields
                                              :huffman-p huffman-p)
                        t outgoing-frame-size stream-id)
                       '()))))))

(defun %h2-settings (payload &key (peer-role :server))
  (unless (member peer-role '(:client :server))
    (error 'http-kit:http-protocol-error
           :message "HTTP/2 SETTINGS peer role must be :client or :server."
           :operation :http2-settings
           :detail peer-role))
  (unless (zerop (mod (length payload) 6))
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 SETTINGS payload must contain six-byte entries."
           :operation :http2-settings
           :detail (length payload)))
  (let ((max-frame-size nil)
        (max-table-size nil)
        (max-concurrent-streams nil)
        (max-header-list-size nil)
        (initial-window-size nil)
        (enable-connect-protocol nil))
    (loop for position from 0 below (length payload) by 6
          for identifier = (%h2-u16 payload position)
          for value = (%h2-u32 payload (+ position 2))
          do (case identifier
               (0
                (error 'http-kit:http-protocol-error
                       :message "HTTP/2 setting identifier zero is invalid."
                       :operation :http2-settings
                       :detail identifier))
               (1
                (setf max-table-size value))
               (2
                (unless (<= value 1)
                  (error 'http-kit:http-protocol-error
                         :message "SETTINGS_ENABLE_PUSH must be zero or one."
                         :operation :http2-settings
                         :detail value))
                (when (and (eq peer-role :server) (plusp value))
                  (error 'http-kit:http-protocol-error
                         :message "An HTTP/2 server must not enable server push."
                         :operation :http2-settings
                         :detail value)))
               (3
                (setf max-concurrent-streams value))
               (4
                (when (> value #x7fffffff)
                  (error 'http-kit:http-protocol-error
                         :message "SETTINGS_INITIAL_WINDOW_SIZE is too large."
                         :operation :http2-settings
                         :detail value))
                (setf initial-window-size value))
               (5
                (unless (<= 16384 value #xffffff)
                  (error 'http-kit:http-protocol-error
                         :message "SETTINGS_MAX_FRAME_SIZE is outside the HTTP/2 range."
                         :operation :http2-settings
                         :detail value))
                (setf max-frame-size value))
               (6
                (setf max-header-list-size value))
               (8
                (unless (<= value 1)
                  (error 'http-kit:http-protocol-error
                         :message "SETTINGS_ENABLE_CONNECT_PROTOCOL must be zero or one."
                         :operation :http2-settings
                         :detail value))
                (setf enable-connect-protocol value))))
    (values max-frame-size max-table-size initial-window-size
            enable-connect-protocol max-concurrent-streams
            max-header-list-size)))
