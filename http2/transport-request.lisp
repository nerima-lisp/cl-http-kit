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
            (value (http-kit:http-header-content header)))
       (when (or (string= name "")
                 (char= (char name 0) #\:))
         (error 'http-kit:http-invalid-header
                :message "HTTP/2 request trailers cannot contain pseudo-header fields."
                :operation :http2-trailers
                :name name
                :reason :pseudo-field))
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
       (%hpack-validate-field name value)))
   (http-kit:http-request-trailers request)))

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
    (http-kit::%check-limit :headers header-size max-header-bytes)
    (http-kit::%check-limit :headers trailer-size max-header-bytes)
    (when peer-max-header-list-size
      (http-kit::%check-limit :headers header-size peer-max-header-list-size)
      (http-kit::%check-limit :headers trailer-size peer-max-header-list-size))
    (http-kit::%check-limit :body (or expected-body-length (length body))
                            max-body-bytes)
    (let* ((block (%hpack-encode-block fields :huffman-p huffman-p))
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
                        (%hpack-encode-block trailer-fields
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
