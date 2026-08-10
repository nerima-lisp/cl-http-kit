(in-package #:http-kit/client)

(defun %client-octet-vector-p (value)
  (and (arrayp value)
       (= (array-rank value) 1)
       (not (stringp value))
       (loop for octet across value
             always (and (integerp octet) (<= 0 octet #xff)))))

(defun %copy-client-octets (value)
  (unless (%client-octet-vector-p value)
    (%client-protocol-error
     "Expected a one-dimensional vector containing octets."
     value))
  (let ((copy (make-array (length value)
                          :element-type '(unsigned-byte 8))))
    (replace copy value)
    copy))

(defun %push-utf8-code-point (code result)
  (cond ((<= code #x7f)
         (vector-push-extend code result))
        ((<= code #x7ff)
         (vector-push-extend (+ #xc0 (ldb (byte 5 6) code)) result)
         (vector-push-extend (+ #x80 (ldb (byte 6 0) code)) result))
        ((<= code #xffff)
         (when (<= #xd800 code #xdfff)
           (%client-protocol-error
            "UTF-8 cannot encode a surrogate code point."
            code))
         (vector-push-extend (+ #xe0 (ldb (byte 4 12) code)) result)
         (vector-push-extend (+ #x80 (ldb (byte 6 6) code)) result)
         (vector-push-extend (+ #x80 (ldb (byte 6 0) code)) result))
        ((<= code #x10ffff)
         (vector-push-extend (+ #xf0 (ldb (byte 3 18) code)) result)
         (vector-push-extend (+ #x80 (ldb (byte 6 12) code)) result)
         (vector-push-extend (+ #x80 (ldb (byte 6 6) code)) result)
         (vector-push-extend (+ #x80 (ldb (byte 6 0) code)) result))
        (t
         (%client-protocol-error
          "A character is outside the Unicode scalar value range."
          code))))

(defun http-utf8-octets (string)
  "Encode STRING as UTF-8 octets without depending on implementation codecs."
  (unless (stringp string)
    (%client-protocol-error "UTF-8 encoding requires a string." string))
  (let ((result (make-array 0
                            :element-type '(unsigned-byte 8)
                            :adjustable t
                            :fill-pointer 0)))
    (loop for character across string
          do (%push-utf8-code-point (char-code character) result))
    (let ((copy (make-array (length result)
                            :element-type '(unsigned-byte 8))))
      (replace copy result)
      copy)))

(defun %hex-digit (value)
  (char "0123456789ABCDEF" value))

(defun %percent-safe-byte-p (byte safe)
  (or (and (<= (char-code #\A) byte) (<= byte (char-code #\Z)))
      (and (<= (char-code #\a) byte) (<= byte (char-code #\z)))
      (and (<= (char-code #\0) byte) (<= byte (char-code #\9)))
      (and safe (find (code-char byte) safe :test #'char=))))

(defun http-percent-encode (string &key (safe "-._~") (space-as-plus-p nil))
  "Percent-encode STRING after UTF-8 conversion.

SAFE contains ASCII characters that remain literal.  SPACE-AS-PLUS-P is for
application/x-www-form-urlencoded rather than generic URI components."
  (unless (stringp string)
    (%client-protocol-error "Percent encoding requires a string." string))
  (unless (or (null safe) (stringp safe))
    (%client-protocol-error "The percent-encoding safe set must be a string or NIL."
                            safe))
  (let ((octets (http-utf8-octets string)))
    (with-output-to-string (stream)
      (loop for byte across octets
            do (cond ((and space-as-plus-p (= byte #x20))
                      (write-char #\+ stream))
                     ((%percent-safe-byte-p byte safe)
                      (write-char (code-char byte) stream))
                     (t
                      (write-char #\% stream)
                      (write-char (%hex-digit (ldb (byte 4 4) byte)) stream)
                      (write-char (%hex-digit (ldb (byte 4 0) byte)) stream)))))))

(defun %form-field (field)
  (cond ((and (consp field) (stringp (car field)))
         (values (car field)
                 (let ((tail (cdr field)))
                   (cond ((stringp tail) tail)
                         ((and (consp tail) (null (cdr tail))) (car tail))
                         (t
                          (%client-protocol-error
                           "Form fields must be name/value pairs."
                           field))))))
        ((and (listp field) (= (length field) 2) (stringp (first field)))
         (values (first field) (second field)))
        (t
         (%client-protocol-error "Form fields must be name/value pairs." field))))

(defun http-form-urlencode (fields)
  "Return FIELDS in application/x-www-form-urlencoded form.

FIELDS is a list of cons pairs or two-element lists.  NIL values encode as an
empty value; non-string values are rejected so callers cannot accidentally
serialize implementation-specific objects into a request body."
  (unless (listp fields)
    (%client-protocol-error "Form fields must be a list." fields))
  (with-output-to-string (stream)
    (loop for field in fields
          for firstp = t then nil
          do (multiple-value-bind (name value) (%form-field field)
               (unless (or (null value) (stringp value))
                 (%client-protocol-error
                  "Form field values must be strings or NIL."
                  value))
               (unless firstp (write-char #\& stream))
               (write-string (http-percent-encode name
                                                  :safe "-._*"
                                                  :space-as-plus-p t)
                             stream)
               (write-char #\= stream)
               (write-string (http-percent-encode (or value "")
                                                  :safe "-._*"
                                                  :space-as-plus-p t)
                             stream)))))

(defun http-form-urlencoded-octets (fields)
  (http-utf8-octets (http-form-urlencode fields)))

(defun %multipart-header-safe-p (string)
  (and (stringp string)
       (not (find-if (lambda (character)
                       (or (char= character #\Return)
                           (char= character #\Linefeed)
                           (char= character #\")))
                     string))))

(defun %multipart-quoted-value (string)
  (with-output-to-string (stream)
    (loop for character across string
          do (when (member character '(#\" #\\) :test #'char=)
               (write-char #\\ stream))
             (write-char character stream))))

(defun %generated-boundary ()
  (format nil "------------------------cl-http-kit-~36R-~36R"
          (get-universal-time)
          (random (expt 36 8))))

(defun %multipart-append-string (result string)
  (loop for byte across (http-utf8-octets string)
        do (vector-push-extend byte result))
  result)

(defun %client-crlf ()
  (coerce (list #\Return #\Linefeed) 'string))

(defun make-http-multipart-body (parts &key boundary)
  "Return two values: multipart octets and its Content-Type value.

Each item in PARTS should be an HTTP-MULTIPART-PART.  The boundary can be
provided for deterministic tests; an implementation-generated boundary is
otherwise used."
  (unless (listp parts)
    (%client-protocol-error "Multipart parts must be a list." parts))
  (let ((boundary (or boundary (%generated-boundary))))
    (unless (and (stringp boundary)
                 (plusp (length boundary))
                 (not (find-if (lambda (character)
                                 (or (char= character #\Return)
                                     (char= character #\Linefeed)
                                     (char= character #\Space)))
                               boundary)))
      (%client-protocol-error "A multipart boundary contains invalid characters."
                              boundary))
    (let ((crlf (%client-crlf))
          (result (make-array 0
                              :element-type '(unsigned-byte 8)
                              :adjustable t
                              :fill-pointer 0)))
      (dolist (part parts)
        (unless (http-multipart-part-p part)
          (%client-protocol-error "Multipart parts must be HTTP-MULTIPART-PART values."
                                  part))
        (let ((name (http-multipart-part-name part))
              (filename (http-multipart-part-filename part))
              (content-type (http-multipart-part-content-type part))
              (value (http-multipart-part-value part)))
          (unless (%multipart-header-safe-p name)
            (%client-protocol-error "A multipart name contains invalid characters."
                                    name))
          (when (and filename (not (%multipart-header-safe-p filename)))
            (%client-protocol-error
             "A multipart filename contains invalid characters."
             filename))
          (when (and content-type (not (%multipart-header-safe-p content-type)))
            (%client-protocol-error
             "A multipart content type contains invalid characters."
             content-type))
          (%multipart-append-string result (format nil "--~A~A" boundary crlf))
          (%multipart-append-string
           result
           (format nil "Content-Disposition: form-data; name=\"~A\"~A~A"
                   (%multipart-quoted-value name)
                   (if filename
                       (format nil "; filename=\"~A\""
                               (%multipart-quoted-value filename))
                       "")
                   crlf))
          (when content-type
            (%multipart-append-string result
                                      (format nil "Content-Type: ~A~A"
                                              content-type crlf)))
          (%multipart-append-string result crlf)
          (if (stringp value)
              (%multipart-append-string result value)
              (let ((octets (%copy-client-octets value)))
                (loop for byte across octets
                      do (vector-push-extend byte result))))
          (%multipart-append-string result crlf)))
      (%multipart-append-string result (format nil "--~A--~A" boundary crlf))
      (let ((copy (make-array (length result)
                              :element-type '(unsigned-byte 8))))
        (replace copy result)
        (values copy (format nil "multipart/form-data; boundary=~A" boundary))))))

(defun %multipart-octets-string (octets start end)
  (map 'string #'code-char (subseq octets start end)))

(defun %multipart-find-sequence (octets needle start &key end)
  (let* ((end (or end (length octets)))
         (needle-length (length needle))
         (last-start (- end needle-length)))
    (when (and (<= 0 start) (<= start last-start))
      (loop for position from start to last-start
            when (loop for offset below needle-length
                       always (= (aref octets (+ position offset))
                                 (aref needle offset)))
              return position))))

(defun %multipart-parameter-segments (text)
  (let ((segments '())
        (start 0)
        (quoted-p nil)
        (escaped-p nil))
    (loop for index from 0 below (length text)
          for character = (char text index)
          do (cond
               (escaped-p (setf escaped-p nil))
               ((and quoted-p (char= character #\\))
                (setf escaped-p t))
               ((char= character #\")
                (setf quoted-p (not quoted-p)))
               ((and (not quoted-p) (char= character #\;))
                (push (string-trim '(#\Space #\Tab)
                                   (subseq text start index))
                      segments)
                (setf start (1+ index)))))
    (when (or quoted-p escaped-p)
      (%client-protocol-error
       "A multipart parameter has an unterminated quoted value."
       text))
    (push (string-trim '(#\Space #\Tab) (subseq text start)) segments)
    (nreverse segments)))

(defun %multipart-parameter-value (value detail)
  (let ((value (string-trim '(#\Space #\Tab) value)))
    (cond
      ((and (>= (length value) 2)
            (char= (char value 0) #\")
            (char= (char value (1- (length value))) #\"))
       (with-output-to-string (stream)
         (loop for index from 1 below (1- (length value))
               for character = (char value index)
               do (if (char= character #\\)
                      (progn
                        (incf index)
                        (when (>= index (1- (length value)))
                          (%client-protocol-error
                           "A multipart quoted parameter has a dangling escape."
                           detail))
                        (write-char (char value index) stream))
                      (write-char character stream)))))
      ((find #\" value)
       (%client-protocol-error
        "A multipart parameter has invalid quote placement."
        detail))
      (t value))))

(defun %multipart-parse-parameters (text operation)
  (let* ((segments (%multipart-parameter-segments text))
         (main (string-downcase (string-trim '(#\Space #\Tab)
                                             (or (first segments) ""))))
         (parameters '()))
    (when (zerop (length main))
      (%client-protocol-error operation text))
    (dolist (segment (rest segments))
      (let ((equals (position #\= segment)))
        (unless (and equals (plusp equals) (< equals (1- (length segment))))
          (%client-protocol-error operation segment))
        (let* ((name (string-downcase
                      (string-trim '(#\Space #\Tab)
                                   (subseq segment 0 equals))))
               (value (%multipart-parameter-value
                       (subseq segment (1+ equals))
                       segment)))
          (unless (plusp (length name))
            (%client-protocol-error operation segment))
          (when (assoc name parameters :test #'string=)
            (%client-protocol-error
             "A multipart header contains a duplicate parameter."
             name))
          (push (cons name value) parameters))))
    (values main (nreverse parameters))))

(defun %multipart-content-type-boundary (content-type)
  (when content-type
    (unless (stringp content-type)
      (%client-protocol-error "Multipart Content-Type must be a string."
                              content-type))
    (multiple-value-bind (media-type parameters)
        (%multipart-parse-parameters
         content-type
         "A multipart Content-Type is malformed.")
      (unless (string= media-type "multipart/form-data")
        (%client-protocol-error
         "Only multipart/form-data bodies are supported."
         media-type))
      (let ((boundary (cdr (assoc "boundary" parameters :test #'string=))))
        (unless boundary
          (%client-protocol-error
           "A multipart Content-Type must contain a boundary parameter."
           content-type))
        boundary))))

(defun %multipart-valid-boundary-p (boundary)
  (and (stringp boundary)
       (<= 1 (length boundary) 70)
       (every (lambda (character)
                (let ((code (char-code character)))
                  (<= #x21 code #x7e)))
              boundary)))

(defun %multipart-delimiter-kind (octets position marker)
  (let ((suffix (+ position (length marker))))
    (cond
      ((and (<= (+ suffix 2) (length octets))
            (= (aref octets suffix) #x2d)
            (= (aref octets (1+ suffix)) #x2d))
       :final)
      ((and (<= (+ suffix 2) (length octets))
            (= (aref octets suffix) #x0d)
            (= (aref octets (1+ suffix)) #x0a))
       :next)
      (t nil))))

(defun %multipart-first-delimiter (octets marker)
  (loop for position = (%multipart-find-sequence octets marker 0)
          then (%multipart-find-sequence octets marker (1+ position))
        while position
        when (and (or (zerop position)
                      (and (>= position 2)
                           (= (aref octets (- position 2)) #x0d)
                           (= (aref octets (1- position)) #x0a)))
                  (%multipart-delimiter-kind octets position marker))
          return position))

(defun %multipart-header-block (octets start end max-header-bytes)
  (let ((position start)
        (headers '()))
    (loop
      (let ((line-end (%multipart-find-sequence
                       octets #(13 10) position :end end)))
        (unless line-end
          (%client-protocol-error
           "A multipart part has no complete header block."
           (list :start start :end end)))
        (let ((header-bytes (+ (- (+ line-end 2) start) 2)))
          (when (and max-header-bytes (> header-bytes max-header-bytes))
            (error 'http-size-limit-exceeded
                   :message "A multipart header block exceeded its size limit."
                   :operation :multipart
                   :detail (list :limit max-header-bytes
                                 :observed header-bytes)))
        (if (= line-end position)
            (progn
              (setf headers (nreverse headers))
              (return (values headers (+ line-end 2))))
            (let ((colon (%multipart-find-sequence
                          octets #(58) position :end line-end)))
              (unless (and colon (> colon position))
                (%client-protocol-error
                 "A multipart header line is missing its name or colon."
                 (%multipart-octets-string octets position line-end)))
              (let ((name (%multipart-octets-string octets position colon))
                    (value (%multipart-octets-string octets (1+ colon) line-end)))
                (push (make-http-header
                       name
                       (string-trim '(#\Space #\Tab) value))
                      headers))
              (setf position (+ line-end 2)))))))))

(defun %multipart-part-metadata (headers)
  (let ((disposition (http-header-value headers "Content-Disposition")))
    (unless disposition
      (%client-protocol-error
       "A multipart part must have Content-Disposition."
       headers))
    (multiple-value-bind (kind parameters)
        (%multipart-parse-parameters
         disposition
         "A multipart Content-Disposition is malformed.")
      (unless (string= kind "form-data")
        (%client-protocol-error
         "A multipart part must use form-data Content-Disposition."
         disposition))
      (let ((name (cdr (assoc "name" parameters :test #'string=)))
            (filename (cdr (assoc "filename" parameters :test #'string=)))
            (content-type (http-header-value headers "Content-Type")))
        (unless (and name (plusp (length name)))
          (%client-protocol-error
           "A multipart part must have a non-empty name parameter."
           disposition))
        (values name filename content-type)))))

(defun parse-http-multipart-body
    (body &optional content-type
     &key boundary (max-parts 1000) (max-header-bytes 65536)
       max-body-bytes)
  "Parse a multipart/form-data BODY into HTTP-MULTIPART-PART values.

CONTENT-TYPE may be the complete Content-Type header value.  BOUNDARY is an
explicit alternative for callers that already parsed that header.  Part
values remain octets when they are not valid text; callers may interpret them
according to each part's Content-Type.  MAX-BODY-BYTES limits the sum of part
bodies, while MAX-HEADER-BYTES applies to each part's header block."
  (let* ((octets (%copy-client-octets body))
         (header-boundary (%multipart-content-type-boundary content-type))
         (boundary (or boundary header-boundary)))
    (when (and boundary header-boundary
               (not (string= boundary header-boundary)))
      (%client-protocol-error
       "The explicit multipart boundary disagrees with Content-Type."
       (list boundary header-boundary)))
    (unless (%multipart-valid-boundary-p boundary)
      (%client-protocol-error
       "A multipart boundary must contain one to seventy visible ASCII characters."
       boundary))
    (unless (or (null max-parts)
                (and (integerp max-parts) (>= max-parts 0)))
      (%client-protocol-error "MAX-PARTS must be NIL or a non-negative integer."
                              max-parts))
    (unless (or (null max-header-bytes)
                (and (integerp max-header-bytes) (>= max-header-bytes 0)))
      (%client-protocol-error
       "MAX-HEADER-BYTES must be NIL or a non-negative integer."
       max-header-bytes))
    (unless (or (null max-body-bytes)
                (and (integerp max-body-bytes) (>= max-body-bytes 0)))
      (%client-protocol-error
       "MAX-BODY-BYTES must be NIL or a non-negative integer."
       max-body-bytes))
    (let* ((marker (%multipart-append-string
                    (make-array 0 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0)
                    (format nil "--~A" boundary)))
           (separator (make-array (+ 2 (length marker))
                                  :element-type '(unsigned-byte 8)))
           (delimiter (%multipart-first-delimiter octets marker)))
      (replace separator #(13 10))
      (replace separator marker :start1 2)
      (unless delimiter
        (%client-protocol-error
         "A multipart body does not contain a valid opening boundary."
         boundary))
      (let ((kind (%multipart-delimiter-kind octets delimiter marker))
            (position (+ delimiter (length marker)))
            (parts '())
            (body-bytes 0))
        (when (eq kind :final)
          (return-from parse-http-multipart-body nil))
        (setf position (+ position 2))
        (loop
          (let ((next (%multipart-find-sequence octets separator position)))
            (unless next
              (%client-protocol-error
               "A multipart body has no terminating boundary."
               boundary))
            (let ((body-end next))
              (multiple-value-bind (headers body-start)
                  (%multipart-header-block octets position body-end
                                            max-header-bytes)
                (multiple-value-bind (name filename content-type)
                    (%multipart-part-metadata headers)
                  (let ((part-length (- body-end body-start)))
                    (incf body-bytes part-length)
                    (when (and max-body-bytes (> body-bytes max-body-bytes))
                      (error 'http-size-limit-exceeded
                             :message "Multipart part bodies exceeded their size limit."
                             :operation :multipart
                             :detail (list :limit max-body-bytes
                                           :observed body-bytes)))
                    (when (and max-parts (>= (length parts) max-parts))
                      (error 'http-size-limit-exceeded
                             :message "A multipart body exceeded its part-count limit."
                             :operation :multipart
                             :detail (list :limit max-parts
                                           :observed (1+ (length parts)))))
                    (push (make-http-multipart-part
                           :name name
                           :value (subseq octets body-start body-end)
                           :filename filename
                           :content-type content-type)
                          parts)))))
            (let* ((marker-position (+ next 2))
                   (delimiter-kind
                     (%multipart-delimiter-kind octets marker-position marker)))
              (unless delimiter-kind
                (%client-protocol-error
                 "A multipart boundary has an invalid delimiter suffix."
                 boundary))
              (if (eq delimiter-kind :final)
                  (return (nreverse parts))
                  (setf position (+ marker-position (length marker) 2))))))))))

(in-package #:http-kit/websocket)

(defconstant +websocket-default-max-payload-bytes+ (* 16 1024 1024))
(defparameter +websocket-close-guid+
  "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
(defparameter +websocket-base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun %websocket-protocol-error (message &optional detail)
  (error 'http-protocol-error
         :message message
         :operation :websocket
         :detail detail))

(defun %websocket-size-error (message limit observed)
  (error 'http-size-limit-exceeded
         :message message
         :operation :websocket
         :kind :websocket
         :limit limit
         :observed observed))

(defun %websocket-octet-vector-p (value)
  (and (arrayp value)
       (= (array-rank value) 1)
       (not (stringp value))
       (loop for octet across value
             always (and (integerp octet)
                         (<= 0 octet #xff)))))

(defun %websocket-copy-octets (value)
  (unless (%websocket-octet-vector-p value)
    (%websocket-protocol-error
     "WebSocket data must be a one-dimensional vector of octets."
     value))
  (let ((copy (make-array (length value)
                          :element-type '(unsigned-byte 8))))
    (replace copy value)
    copy))

(defun %websocket-empty-octets ()
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun %websocket-control-opcode-p (opcode)
  (member opcode '(8 9 10) :test #'=))

(defun %websocket-valid-opcode-p (opcode)
  (member opcode '(0 1 2 8 9 10) :test #'=))

(defun %websocket-validate-limit (limit name)
  (unless (and (integerp limit) (<= 0 limit))
    (%websocket-protocol-error
     (format nil "~A must be a non-negative integer." name)
     limit))
  limit)

(defun %websocket-validate-frame-components
    (fin-p opcode mask-p masking-key payload)
  (unless (and (integerp opcode)
               (%websocket-valid-opcode-p opcode))
    (%websocket-protocol-error
     "A WebSocket frame has an unsupported opcode."
     opcode))
  (unless (%websocket-octet-vector-p payload)
    (%websocket-protocol-error
     "A WebSocket frame payload must be a vector of octets."
     payload))
  (when (and mask-p (not (%websocket-octet-vector-p masking-key)))
    (%websocket-protocol-error
     "A masked WebSocket frame must have a four-octet masking key."
     masking-key))
  (when (and mask-p (/= (length masking-key) 4))
    (%websocket-protocol-error
     "A WebSocket masking key must contain exactly four octets."
     (length masking-key)))
  (when (and (not mask-p) masking-key)
    (%websocket-protocol-error
     "An unmasked WebSocket frame cannot carry a masking key."))
  (when (and (%websocket-control-opcode-p opcode)
             (or (not fin-p) (> (length payload) 125)))
    (%websocket-protocol-error
     "WebSocket control frames must be final and no larger than 125 octets."
     (list :fin fin-p :length (length payload))))
  t)

(defstruct (websocket-frame
            (:constructor %make-websocket-frame
                (&key fin-p opcode mask-p masking-key payload)))
  fin-p
  opcode
  mask-p
  masking-key
  payload)

(defun make-websocket-frame
    (&key (fin-p t) (opcode 1) (mask-p nil) masking-key payload)
  "Construct a validated WebSocket frame.

MASKING-KEY is required for masked frames.  The library deliberately does not
generate masking keys implicitly, so callers must make the randomness policy
explicit at the client boundary."
  (let ((final (not (null fin-p)))
        (masked (not (null mask-p)))
        (frame-payload (if payload
                           (%websocket-copy-octets payload)
                           (%websocket-empty-octets)))
        (frame-key (and masking-key (%websocket-copy-octets masking-key))))
    (%websocket-validate-frame-components
     final opcode masked frame-key frame-payload)
    (%make-websocket-frame :fin-p final
                           :opcode opcode
                           :mask-p masked
                           :masking-key frame-key
                           :payload frame-payload)))

(defun %websocket-store-integer (vector start width value)
  (loop for index below width
        for shift from (* 8 (1- width)) downto 0 by 8
        do (setf (aref vector (+ start index))
                 (ldb (byte 8 shift) value)))
  vector)

(defun %websocket-read-integer (vector start width)
  (loop with result = 0
        for index below width
        do (setf result
                 (+ (ash result 8)
                    (aref vector (+ start index))))
        finally (return result)))

(defun %websocket-validate-length-encoding (length-code payload-length)
  (when (and (= length-code 126) (< payload-length 126))
    (%websocket-protocol-error
     "A 16-bit WebSocket payload length must be at least 126."
     payload-length))
  (when (and (= length-code 127) (< payload-length #x10000))
    (%websocket-protocol-error
     "A 64-bit WebSocket payload length must be at least 65536."
     payload-length))
  payload-length)

(defun %websocket-mask-octets (payload masking-key)
  (let ((result (make-array (length payload)
                            :element-type '(unsigned-byte 8))))
    (loop for index below (length payload)
          do (setf (aref result index)
                   (logxor (aref payload index)
                           (aref masking-key (mod index 4)))))
    result))

(defun serialize-websocket-frame (frame)
  "Serialize FRAME to its wire representation as an octet vector."
  (unless (websocket-frame-p frame)
    (%websocket-protocol-error "Expected a WebSocket frame." frame))
  (let* ((fin-p (websocket-frame-fin-p frame))
         (opcode (websocket-frame-opcode frame))
         (mask-p (websocket-frame-mask-p frame))
         (masking-key (websocket-frame-masking-key frame))
         (payload (websocket-frame-payload frame)))
    (%websocket-validate-frame-components
     fin-p opcode mask-p masking-key payload)
    (let* ((payload-length (length payload))
           (extended-width (cond ((<= payload-length 125) 0)
                                 ((<= payload-length #xffff) 2)
                                 ((< payload-length (ash 1 63)) 8)
                                 (t
                                  (%websocket-protocol-error
                                   "A WebSocket payload length exceeds the 63-bit wire limit."
                                   payload-length))))
           (header-length (+ 2 extended-width (if mask-p 4 0)))
           (wire (make-array (+ header-length payload-length)
                             :element-type '(unsigned-byte 8)))
           (length-code (cond ((= extended-width 0) payload-length)
                              ((= extended-width 2) 126)
                              (t 127))))
      (setf (aref wire 0)
            (logior (if fin-p #x80 0) opcode)
            (aref wire 1)
            (logior (if mask-p #x80 0) length-code))
      (when (plusp extended-width)
        (%websocket-store-integer wire 2 extended-width payload-length))
      (let ((offset (+ 2 extended-width)))
        (when mask-p
          (replace wire masking-key :start1 offset)
          (incf offset 4))
        (replace wire (if mask-p
                         (%websocket-mask-octets payload masking-key)
                         payload)
                 :start1 offset))
      wire)))

(defun %websocket-parse-length (octets length-code offset)
  (cond ((< length-code 126)
         (values length-code offset))
        ((= length-code 126)
        (when (< (length octets) (+ offset 2))
           (%websocket-protocol-error
            "A WebSocket frame ended before its extended payload length."))
         (let ((payload-length (%websocket-read-integer octets offset 2)))
           (%websocket-validate-length-encoding length-code payload-length)
           (values payload-length
                 (+ offset 2))))
        (t
         (when (< (length octets) (+ offset 8))
           (%websocket-protocol-error
            "A WebSocket frame ended before its extended payload length."))
         (when (logbitp 63 (%websocket-read-integer octets offset 8))
           (%websocket-protocol-error
            "A WebSocket payload length must have its high bit clear."))
         (let ((payload-length (%websocket-read-integer octets offset 8)))
           (%websocket-validate-length-encoding length-code payload-length)
           (values payload-length
                 (+ offset 8))))))

(defun parse-websocket-frame
    (octets &key (max-payload-bytes +websocket-default-max-payload-bytes+)
                  (require-mask-p nil) (allow-unmasked-p t))
  "Parse one WebSocket frame from OCTETS.

Returns the frame and the number of consumed octets.  Additional octets are
left for the caller, which makes this function suitable for buffered input."
  (%websocket-validate-limit max-payload-bytes "MAX-PAYLOAD-BYTES")
  (unless (%websocket-octet-vector-p octets)
    (%websocket-protocol-error
     "WebSocket wire data must be a one-dimensional vector of octets."
     octets))
  (when (< (length octets) 2)
    (%websocket-protocol-error
     "A WebSocket frame requires at least two header octets."))
  (let* ((first (aref octets 0))
         (second (aref octets 1))
         (fin-p (not (zerop (logand first #x80))))
         (reserved (logand first #x70))
         (opcode (logand first #x0f))
         (mask-p (not (zerop (logand second #x80))))
         (length-code (logand second #x7f)))
    (when (plusp reserved)
      (%websocket-protocol-error
       "WebSocket RSV bits are reserved and must be zero."
       reserved))
    (unless (%websocket-valid-opcode-p opcode)
      (%websocket-protocol-error
       "A WebSocket frame has an unsupported opcode."
       opcode))
    (when (and require-mask-p (not mask-p))
      (%websocket-protocol-error
       "A WebSocket frame was required to be masked."))
    (when (and (not allow-unmasked-p) (not mask-p))
      (%websocket-protocol-error
       "An unmasked WebSocket frame is not allowed here."))
    (multiple-value-bind (payload-length header-end)
        (%websocket-parse-length octets length-code 2)
      (when (> payload-length max-payload-bytes)
        (%websocket-size-error
         "A WebSocket frame exceeded its payload-size limit."
         max-payload-bytes payload-length))
      (let* ((mask-end (+ header-end (if mask-p 4 0)))
             (frame-end (+ mask-end payload-length)))
        (when (> mask-end (length octets))
          (%websocket-protocol-error
           "A WebSocket frame ended before its masking key."))
        (when (> frame-end (length octets))
          (%websocket-protocol-error
           "A WebSocket frame ended before its payload."))
        (let* ((masking-key (and mask-p
                                 (subseq octets header-end mask-end)))
               (wire-payload (subseq octets mask-end frame-end))
               (payload (if mask-p
                            (%websocket-mask-octets wire-payload masking-key)
                            wire-payload)))
          (%websocket-validate-frame-components
           fin-p opcode mask-p masking-key payload)
          (values (%make-websocket-frame :fin-p fin-p
                                         :opcode opcode
                                         :mask-p mask-p
                                         :masking-key masking-key
                                         :payload payload)
                  frame-end))))))

(defun %websocket-read-exact (stream count)
  (unless (and (integerp count) (<= 0 count))
    (%websocket-protocol-error
     "A WebSocket read requested an invalid octet count."
     count))
  (let ((result (make-array count :element-type '(unsigned-byte 8)))
        (position 0))
    (loop while (< position count)
          do (let ((new-position
                     (handler-case
                         (read-sequence result stream :start position :end count)
                       (end-of-file () position))))
               (if (<= new-position position)
                   (%websocket-protocol-error
                    "The stream ended in the middle of a WebSocket frame.")
                   (setf position new-position))))
    result))

(defun read-websocket-frame
    (stream &key (max-payload-bytes +websocket-default-max-payload-bytes+)
                  (require-mask-p nil) (allow-unmasked-p t))
  "Read and parse one WebSocket frame from STREAM."
  (%websocket-validate-limit max-payload-bytes "MAX-PAYLOAD-BYTES")
  (unless (streamp stream)
    (%websocket-protocol-error "WebSocket frame input must be a stream." stream))
  (let* ((first-two (%websocket-read-exact stream 2))
         (mask-p (not (zerop (logand (aref first-two 1) #x80))))
         (length-code (logand (aref first-two 1) #x7f))
         (extended-width (cond ((< length-code 126) 0)
                               ((= length-code 126) 2)
                               (t 8))))
    (when (and require-mask-p (not mask-p))
      (%websocket-protocol-error
       "A WebSocket frame was required to be masked."))
    (when (and (not allow-unmasked-p) (not mask-p))
      (%websocket-protocol-error
       "An unmasked WebSocket frame is not allowed here."))
    (let* ((extension (%websocket-read-exact stream extended-width))
           (payload-length (if (zerop extended-width)
                               length-code
                               (%websocket-read-integer
                                extension 0 extended-width))))
      (when (and (= extended-width 8)
                 (logbitp 63 payload-length))
        (%websocket-protocol-error
         "A WebSocket payload length must have its high bit clear."))
      (%websocket-validate-length-encoding length-code payload-length)
      (when (> payload-length max-payload-bytes)
        (%websocket-size-error
         "A WebSocket frame exceeded its payload-size limit."
         max-payload-bytes payload-length))
      (let* ((masking-key (if mask-p (%websocket-read-exact stream 4) nil))
             (payload (%websocket-read-exact stream payload-length))
             (wire (make-array (+ 2 extended-width (if mask-p 4 0)
                                  payload-length)
                               :element-type '(unsigned-byte 8))))
        (replace wire first-two)
        (replace wire extension :start1 2)
        (let ((offset (+ 2 extended-width)))
          (when mask-p
            (replace wire masking-key :start1 offset)
            (incf offset 4))
          (replace wire payload :start1 offset))
        (parse-websocket-frame wire
                               :max-payload-bytes max-payload-bytes
                               :require-mask-p require-mask-p
                               :allow-unmasked-p allow-unmasked-p)))))

(defun write-websocket-frame (stream frame &key (finish-output-p t))
  "Write FRAME to STREAM and optionally flush the stream."
  (unless (streamp stream)
    (%websocket-protocol-error "WebSocket frame output must be a stream." stream))
  (write-sequence (serialize-websocket-frame frame) stream)
  (when finish-output-p
    (finish-output stream))
  frame)

(defun %websocket-base64-encode (octets)
  (unless (%websocket-octet-vector-p octets)
    (%websocket-protocol-error "Base64 input must be a vector of octets." octets))
  (let* ((length (length octets))
         (result (make-string (* 4 (ceiling length 3)))))
    (loop for input from 0 below length by 3
          for output from 0 by 4
          for first = (aref octets input)
          for second-present = (< (1+ input) length)
          for third-present = (< (+ input 2) length)
          for second = (if second-present (aref octets (1+ input)) 0)
          for third = (if third-present (aref octets (+ input 2)) 0)
          for value = (logior (ash first 16) (ash second 8) third)
          do (setf (char result output)
                   (char +websocket-base64-alphabet+ (ldb (byte 6 18) value))
                   (char result (+ output 1))
                   (char +websocket-base64-alphabet+ (ldb (byte 6 12) value))
                   (char result (+ output 2))
                   (if second-present
                       (char +websocket-base64-alphabet+ (ldb (byte 6 6) value))
                       #\=)
                   (char result (+ output 3))
                   (if third-present
                       (char +websocket-base64-alphabet+ (ldb (byte 6 0) value))
                       #\=)))
    result))

(defun %websocket-base64-value (character)
  (position character +websocket-base64-alphabet+ :test #'char=))

(defun %websocket-base64-decode (string)
  (unless (and (stringp string) (zerop (mod (length string) 4)))
    (%websocket-protocol-error
     "A WebSocket Sec-WebSocket-Key must be valid Base64." string))
  (let ((result (make-array 0
                            :element-type '(unsigned-byte 8)
                            :adjustable t
                            :fill-pointer 0)))
    (loop for offset from 0 below (length string) by 4
          for first = (char string offset)
          for second = (char string (+ offset 1))
          for third = (char string (+ offset 2))
          for fourth = (char string (+ offset 3))
          for first-value = (%websocket-base64-value first)
          for second-value = (%websocket-base64-value second)
          for third-value = (unless (char= third #\=)
                             (%websocket-base64-value third))
          for fourth-value = (unless (char= fourth #\=)
                              (%websocket-base64-value fourth))
          do (unless (and first-value second-value
                          (or (char= third #\=) third-value)
                          (or (char= fourth #\=) fourth-value)
                          (or (not (char= third #\=))
                              (char= fourth #\=))
                          (or (not (char= fourth #\=))
                              (= offset (- (length string) 4))))
               (%websocket-protocol-error
                "A WebSocket Sec-WebSocket-Key has invalid Base64 padding."
                string))
             (let ((value (logior (ash first-value 18)
                                  (ash second-value 12)
                                  (ash (or third-value 0) 6)
                                  (or fourth-value 0))))
               (when (and (char= third #\=)
                          (not (zerop (logand second-value #x0f))))
                 (%websocket-protocol-error
                  "A Base64 value has non-zero unused bits."
                  string))
               (when (and (char= fourth #\=)
                          (not (zerop (logand (or third-value 0) #x03))))
                 (%websocket-protocol-error
                  "A Base64 value has non-zero unused bits."
                  string))
               (vector-push-extend (ldb (byte 8 16) value) result)
               (unless (char= third #\=)
                 (vector-push-extend (ldb (byte 8 8) value) result))
               (unless (char= fourth #\=)
                 (vector-push-extend (ldb (byte 8 0) value) result))))
    (let ((copy (make-array (length result)
                            :element-type '(unsigned-byte 8))))
      (replace copy result)
      copy)))

(defun %websocket-rol32 (value count)
  (logand #xffffffff
          (logior (ash value count)
                  (ash value (- count 32)))))

(defun %websocket-sha1 (octets)
  (let* ((length (length octets))
         (with-one (1+ length))
         (zero-count (mod (- 56 (mod with-one 64)) 64))
         (padded-length (+ with-one zero-count 8))
         (padded (make-array padded-length
                             :element-type '(unsigned-byte 8)))
         (h0 #x67452301)
         (h1 #xefcdab89)
         (h2 #x98badcfe)
         (h3 #x10325476)
         (h4 #xc3d2e1f0))
    (replace padded octets)
    (setf (aref padded length) #x80)
    (%websocket-store-integer padded (- padded-length 8) 8 (* 8 length))
    (loop for block-start from 0 below padded-length by 64
          with words = (make-array 80 :element-type '(unsigned-byte 32))
          do (loop for index below 16
                   do (setf (aref words index)
                            (%websocket-read-integer padded
                                                     (+ block-start (* index 4))
                                                     4)))
             (loop for index from 16 below 80
                   do (setf (aref words index)
                            (%websocket-rol32
                             (logxor (aref words (- index 3))
                                     (aref words (- index 8))
                                     (aref words (- index 14))
                                     (aref words (- index 16)))
                             1)))
             (let ((a h0)
                   (b h1)
                   (c h2)
                   (d h3)
                   (e h4))
               (loop for index below 80
                     for function = (cond ((< index 20)
                                           (logior (logand b c)
                                                   (logand (lognot b) d)))
                                          ((< index 40)
                                           (logxor b c d))
                                          ((< index 60)
                                           (logior (logand b c)
                                                   (logand b d)
                                                   (logand c d)))
                                          (t (logxor b c d)))
                     for constant = (cond ((< index 20) #x5a827999)
                                          ((< index 40) #x6ed9eba1)
                                          ((< index 60) #x8f1bbcdc)
                                          (t #xca62c1d6))
                     for temporary =
                       (logand #xffffffff
                               (+ (%websocket-rol32 a 5)
                                  function
                                  e
                                  constant
                                  (aref words index)))
                     do (setf e d
                              d c
                              c (%websocket-rol32 b 30)
                              b a
                              a temporary))
               (setf h0 (logand #xffffffff (+ h0 a))
                     h1 (logand #xffffffff (+ h1 b))
                     h2 (logand #xffffffff (+ h2 c))
                     h3 (logand #xffffffff (+ h3 d))
                     h4 (logand #xffffffff (+ h4 e)))))
    (let ((digest (make-array 20 :element-type '(unsigned-byte 8))))
      (%websocket-store-integer digest 0 4 h0)
      (%websocket-store-integer digest 4 4 h1)
      (%websocket-store-integer digest 8 4 h2)
      (%websocket-store-integer digest 12 4 h3)
      (%websocket-store-integer digest 16 4 h4)
      digest)))

(defun websocket-accept-key (sec-websocket-key)
  "Return the RFC 6455 Sec-WebSocket-Accept value for a client key."
  (unless (stringp sec-websocket-key)
    (%websocket-protocol-error
     "Sec-WebSocket-Key must be a Base64 string."
     sec-websocket-key))
  (let ((decoded (%websocket-base64-decode sec-websocket-key)))
    (unless (= (length decoded) 16)
      (%websocket-protocol-error
       "Sec-WebSocket-Key must decode to exactly 16 octets."
       (length decoded)))
    (%websocket-base64-encode
     (%websocket-sha1
      (let* ((key (http-utf8-octets sec-websocket-key))
             (guid (http-utf8-octets +websocket-close-guid+))
             (input (make-array (+ (length key) (length guid))
                                :element-type '(unsigned-byte 8))))
        (replace input key)
        (replace input guid :start1 (length key))
        input)))))

(defun %websocket-header-tokens (value)
  (unless (stringp value)
    (%websocket-protocol-error "A WebSocket header value must be a string." value))
  (let ((tokens '())
        (start 0)
        (length (length value)))
    (loop
      (let* ((comma (position #\, value :start start))
             (end (or comma length))
             (token (string-trim '(#\Space #\Tab)
                                 (subseq value start end))))
        (when (plusp (length token))
          (push token tokens))
        (if comma
            (setf start (1+ comma))
            (return (nreverse tokens)))))))

(defun %websocket-header-has-token-p (headers name token)
  (some (lambda (value)
          (some (lambda (candidate)
                  (string-equal candidate token))
                (%websocket-header-tokens value)))
        (http-header-values headers name)))

(defun %websocket-single-header-value (headers name)
  (let ((values (http-header-values headers name)))
    (when (= (length values) 1)
      (string-trim '(#\Space #\Tab) (first values)))))

(defun %websocket-token-string-p (value)
  (and (stringp value)
       (plusp (length value))
       (loop for character across value
             for code = (char-code character)
             always (and (<= #x21 code #x7e)
                         (not (find character
                                    '(#\( #\) #\< #\> #\@ #\, #\;
                                      #\: #\\ #\" #\/ #\[ #\] #\?
                                      #\= #\{ #\} #\Space #\Tab)
                                    :test #'char=))))))

(defun websocket-upgrade-request-p (request)
  "Return true when REQUEST satisfies the RFC 6455 HTTP/1.1 handshake."
  (and (http-request-p request)
       (string-equal (http-request-method request) "GET")
       (string= (http-request-protocol-version request) "HTTP/1.1")
       (%websocket-header-has-token-p (http-request-headers request)
                                      "Upgrade" "websocket")
       (%websocket-header-has-token-p (http-request-headers request)
                                      "Connection" "upgrade")
       (let ((version (%websocket-single-header-value
                       (http-request-headers request)
                       "Sec-WebSocket-Version"))
             (key (%websocket-single-header-value
                   (http-request-headers request)
                   "Sec-WebSocket-Key")))
         (and version
              (string= version "13")
              key
              (handler-case
                  (progn (websocket-accept-key key) t)
                (http-protocol-error () nil))))))

(defun %websocket-extra-header-name (header)
  (cond ((http-header-p header)
         (http-header-name header))
        ((and (consp header) (stringp (car header)))
         (car header))
        (t nil)))

(defun %websocket-reserved-header-p (name)
  (member (string-downcase name)
          '("upgrade" "connection" "sec-websocket-accept"
            "sec-websocket-protocol" "sec-websocket-extensions")
          :test #'string=))

(defun websocket-upgrade-response
    (request &key protocol extensions headers)
  "Create a validated HTTP 101 response for REQUEST.

PROTOCOL, when supplied, must have been offered by the client.  EXTENSIONS is
the already-negotiated extension value; this API does not silently negotiate
an extension it does not understand."
  (unless (websocket-upgrade-request-p request)
    (%websocket-protocol-error
     "An HTTP request does not satisfy the WebSocket upgrade handshake."))
  (when (and protocol
             (or (not (%websocket-token-string-p protocol))
                 (not (%websocket-header-has-token-p
                       (http-request-headers request)
                       "Sec-WebSocket-Protocol"
                       protocol))))
    (%websocket-protocol-error
     "The selected WebSocket subprotocol was not offered by the client."
     protocol))
  (when (and extensions (not (stringp extensions)))
    (%websocket-protocol-error
     "WebSocket extensions must be a string or NIL."
     extensions))
  (dolist (header headers)
    (let ((name (%websocket-extra-header-name header)))
      (when (and name (%websocket-reserved-header-p name))
        (%websocket-protocol-error
         "Custom WebSocket handshake headers cannot replace reserved headers."
         name))))
  (let ((response-headers
          (list (make-http-header "Upgrade" "websocket")
                (make-http-header "Connection" "Upgrade")
                (make-http-header
                 "Sec-WebSocket-Accept"
                 (websocket-accept-key
                  (%websocket-single-header-value
                   (http-request-headers request)
                   "Sec-WebSocket-Key"))))))
    (when protocol
      (setf response-headers
            (append response-headers
                    (list (make-http-header "Sec-WebSocket-Protocol"
                                             protocol)))))
    (when extensions
      (setf response-headers
            (append response-headers
                    (list (make-http-header "Sec-WebSocket-Extensions"
                                             extensions)))))
    (make-http-response :status 101
                        :reason "Switching Protocols"
                        :protocol-version "HTTP/1.1"
                        :headers (append response-headers headers))))

(defun %websocket-client-reserved-header-p (name)
  (member (string-downcase name)
          '("upgrade" "connection" "sec-websocket-version"
            "sec-websocket-key" "sec-websocket-protocol"
            "sec-websocket-extensions")
          :test #'string=))

(defun make-websocket-upgrade-request
    (uri &key key protocols extensions headers)
  "Create an HTTP/1.1 WebSocket client upgrade request for URI.

KEY is the already-generated Base64 value for Sec-WebSocket-Key.  This API
requires the caller to supply it so that key generation can use the
application's cryptographically secure random source.  PROTOCOLS is a list of
offered subprotocol tokens.  EXTENSIONS is an optional already-serialized
Sec-WebSocket-Extensions value; extension negotiation is intentionally left to
the caller.

The reserved handshake headers are generated by this function and cannot be
overridden through HEADERS."
  (unless (stringp key)
    (%websocket-protocol-error
     "Sec-WebSocket-Key must be a Base64 string."
     key))
  (let ((key (string-trim '(#\Space #\Tab) key)))
    (websocket-accept-key key)
    (unless (or (null protocols) (listp protocols))
      (%websocket-protocol-error
       "WebSocket subprotocols must be supplied as a list."
       protocols))
    (dolist (protocol protocols)
      (unless (%websocket-token-string-p protocol)
        (%websocket-protocol-error
         "A WebSocket subprotocol must be a token."
         protocol)))
    (when (and extensions (not (stringp extensions)))
      (%websocket-protocol-error
       "WebSocket extensions must be a string or NIL."
       extensions))
    (unless (listp headers)
      (%websocket-protocol-error
       "Additional WebSocket handshake headers must be a list."
       headers))
    (dolist (header headers)
      (let ((name (%websocket-extra-header-name header)))
        (when (and name (%websocket-client-reserved-header-p name))
          (%websocket-protocol-error
           "Custom WebSocket handshake headers cannot replace reserved headers."
           name))))
    (make-http-request
     :method "GET"
     :uri uri
     :headers
     (append
      (list (make-http-header "Upgrade" "websocket")
            (make-http-header "Connection" "Upgrade")
            (make-http-header "Sec-WebSocket-Version" "13")
            (make-http-header "Sec-WebSocket-Key" key))
      (when protocols
        (list (make-http-header "Sec-WebSocket-Protocol"
                                (format nil "~{~A~^, ~}" protocols))))
      (when extensions
        (list (make-http-header "Sec-WebSocket-Extensions" extensions)))
      headers))))

(defun websocket-client-handshake
    (stream request &key timeout deadline max-header-bytes max-body-bytes
                         (clock-function #'%monotonic-time))
  "Send REQUEST on STREAM and validate its RFC 6455 HTTP/1.1 response.

The HTTP response is returned as the primary value and the conservative
HTTP-RESPONSE-REUSABLE-P result is returned as the second value.  STREAM stays
open, including after a successful 101 response, so the caller can immediately
use READ-WEBSOCKET-FRAME or READ-WEBSOCKET-MESSAGE on it.  The caller owns the
stream and must close it when the handshake or subsequent WebSocket session
ends."
  (unless (streamp stream)
    (%websocket-protocol-error
     "The WebSocket client handshake requires an open stream."
     stream))
  (unless (websocket-upgrade-request-p request)
    (%websocket-protocol-error
     "An HTTP request does not satisfy the WebSocket upgrade handshake."
     request))
  (multiple-value-bind (response reusable-p)
      (send-http-request-over-open-stream
       request stream
       :timeout timeout
       :deadline deadline
       :max-header-bytes max-header-bytes
       :max-body-bytes max-body-bytes
       :collect-body-p nil
       :clock-function clock-function)
    (unless (and (http-response-p response)
                 (= 101 (http-response-status response))
                 (string= "HTTP/1.1"
                          (http-response-protocol-version response))
                 (%websocket-header-has-token-p
                  (http-response-headers response) "Upgrade" "websocket")
                 (%websocket-header-has-token-p
                  (http-response-headers response) "Connection" "upgrade"))
      (%websocket-protocol-error
       "The server response is not a valid WebSocket 101 upgrade response."
       response))
    (let* ((request-headers (http-request-headers request))
           (response-headers (http-response-headers response))
           (key (%websocket-single-header-value
                 request-headers "Sec-WebSocket-Key"))
           (accept (%websocket-single-header-value
                   response-headers "Sec-WebSocket-Accept")))
      (unless (and accept key
                   (string= accept (websocket-accept-key key)))
        (%websocket-protocol-error
         "The server returned an invalid Sec-WebSocket-Accept value."
         accept))
      (let ((selected-protocol-values
              (http-header-values response-headers "Sec-WebSocket-Protocol"))
            (requested-protocol-values
              (http-header-values request-headers "Sec-WebSocket-Protocol")))
        (when selected-protocol-values
          (let ((selected-protocol
                  (%websocket-single-header-value
                   response-headers "Sec-WebSocket-Protocol")))
            (unless (and (= 1 (length selected-protocol-values))
                         (%websocket-token-string-p selected-protocol)
                         requested-protocol-values
                         (%websocket-header-has-token-p
                          request-headers
                          "Sec-WebSocket-Protocol"
                          selected-protocol))
              (%websocket-protocol-error
               "The server selected an invalid WebSocket subprotocol."
               selected-protocol))))
        (when (and (http-header-values response-headers
                                       "Sec-WebSocket-Extensions")
                   (null (http-header-values request-headers
                                              "Sec-WebSocket-Extensions")))
          (%websocket-protocol-error
           "The server selected a WebSocket extension that was not offered."))))
    (values response reusable-p)))

(defun websocket-valid-close-code-p (code)
  (and (integerp code)
       (or (member code '(1000 1001 1002 1003 1007 1008 1009 1010 1011)
                   :test #'=)
           (<= 3000 code 4999))))

(defun %websocket-utf8-continuation-p (byte)
  (<= #x80 byte #xbf))

(defun %websocket-utf8-string (octets)
  (unless (%websocket-octet-vector-p octets)
    (%websocket-protocol-error "A WebSocket reason must be UTF-8 octets." octets))
  (with-output-to-string (result)
    (loop with index = 0
          while (< index (length octets))
          do (let ((first (aref octets index)))
               (cond ((<= first #x7f)
                      (write-char (code-char first) result)
                      (incf index))
                     ((<= #xc2 first #xdf)
                      (when (> (+ index 1) (1- (length octets)))
                        (%websocket-protocol-error
                         "A WebSocket close reason ended in a partial UTF-8 sequence."))
                      (let ((second (aref octets (1+ index))))
                        (unless (%websocket-utf8-continuation-p second)
                          (%websocket-protocol-error
                           "A WebSocket close reason contains invalid UTF-8."
                           octets))
                        (write-char
                         (code-char (+ (ash (logand first #x1f) 6)
                                       (logand second #x3f)))
                         result)
                        (incf index 2)))
                     ((<= #xe0 first #xef)
                      (when (> (+ index 2) (1- (length octets)))
                        (%websocket-protocol-error
                         "A WebSocket close reason ended in a partial UTF-8 sequence."))
                      (let ((second (aref octets (1+ index)))
                            (third (aref octets (+ index 2))))
                        (unless (and (%websocket-utf8-continuation-p second)
                                     (%websocket-utf8-continuation-p third)
                                     (or (/= first #xe0) (>= second #xa0))
                                     (or (/= first #xed) (<= second #x9f)))
                          (%websocket-protocol-error
                           "A WebSocket close reason contains invalid UTF-8."
                           octets))
                        (write-char
                         (code-char (+ (ash (logand first #x0f) 12)
                                       (ash (logand second #x3f) 6)
                                       (logand third #x3f)))
                         result)
                        (incf index 3)))
                     ((<= #xf0 first #xf4)
                      (when (> (+ index 3) (1- (length octets)))
                        (%websocket-protocol-error
                         "A WebSocket close reason ended in a partial UTF-8 sequence."))
                      (let ((second (aref octets (1+ index)))
                            (third (aref octets (+ index 2)))
                            (fourth (aref octets (+ index 3))))
                        (unless (and (%websocket-utf8-continuation-p second)
                                     (%websocket-utf8-continuation-p third)
                                     (%websocket-utf8-continuation-p fourth)
                                     (or (/= first #xf0) (>= second #x90))
                                     (or (/= first #xf4) (<= second #x8f)))
                          (%websocket-protocol-error
                           "A WebSocket close reason contains invalid UTF-8."
                           octets))
                        (write-char
                         (code-char (+ (ash (logand first #x07) 18)
                                       (ash (logand second #x3f) 12)
                                       (ash (logand third #x3f) 6)
                                       (logand fourth #x3f)))
                         result)
                        (incf index 4)))
                     (t
                      (%websocket-protocol-error
                       "A WebSocket close reason contains invalid UTF-8."
                       octets)))))))

(defun make-websocket-close-payload (&key (code 1000) (reason ""))
  "Construct the payload for a WebSocket close control frame."
  (unless (websocket-valid-close-code-p code)
    (%websocket-protocol-error "The WebSocket close code is not permitted." code))
  (unless (stringp reason)
    (%websocket-protocol-error "The WebSocket close reason must be a string." reason))
  (let ((reason-octets (http-utf8-octets reason)))
    (when (> (length reason-octets) 123)
      (%websocket-size-error
       "A WebSocket close reason exceeded its 123-octet limit."
       123 (length reason-octets)))
    (let ((payload (make-array (+ 2 (length reason-octets))
                               :element-type '(unsigned-byte 8))))
      (%websocket-store-integer payload 0 2 code)
      (replace payload reason-octets :start1 2)
      payload)))

(defun parse-websocket-close-payload (payload)
  "Parse a close payload and return its code and UTF-8 reason."
  (unless (%websocket-octet-vector-p payload)
    (%websocket-protocol-error "A WebSocket close payload must be octets." payload))
  (cond ((zerop (length payload))
         (values nil ""))
        ((= (length payload) 1)
         (%websocket-protocol-error
          "A WebSocket close payload cannot contain one octet."))
        (t
         (let ((code (%websocket-read-integer payload 0 2)))
           (unless (websocket-valid-close-code-p code)
             (%websocket-protocol-error
              "The WebSocket close code is not permitted."
              code))
           (values code (%websocket-utf8-string (subseq payload 2)))))))

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
                ((= opcode 0)
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
         (http-utf8-octets payload))
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
                  (http-utf8-octets payload))
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
   (if payload
       payload
       (make-websocket-close-payload :code (or code 1000)
                                     :reason (or reason "")))
   :mask-p mask-p
   :masking-key masking-key
   :masking-key-function masking-key-function
   :finish-output-p finish-output-p))

(defun %websocket-session-close-code (condition)
  (cond ((typep condition 'http-size-limit-exceeded) 1009)
        ((typep condition 'http-protocol-error) 1002)
        (t 1011)))

(defun serve-websocket-session
    (stream handler &key
                     (max-message-bytes +websocket-default-max-payload-bytes+)
                     (max-payload-bytes +websocket-default-max-payload-bytes+)
                     (max-messages nil)
                     (require-mask-p t)
                     (allow-unmasked-p nil)
                     on-control
                     on-error
                     (close-on-error-p t)
                     (close-stream #'close))
  "Serve messages on an already-upgraded WebSocket STREAM.

HANDLER is called as (STREAM PAYLOAD OPCODE) for every complete text or
binary message.  It may return :CLOSE to start a normal close handshake.
The server automatically replies to Ping frames and echoes a valid peer
Close frame.  Client frames are required to be masked by default.

The function returns two values: the number of messages delivered and a
termination keyword (:PEER-CLOSE, :HANDLER-CLOSE, or :MAX-MESSAGES).  On a
protocol, size, or handler error it sends an appropriate Close frame when
CLOSE-ON-ERROR-P is true, invokes ON-ERROR with the condition, and re-signals
the condition.  CLOSE-STREAM is called at the end unless it is NIL, which is
useful when the caller owns the upgraded stream lifecycle."
  (unless (streamp stream)
    (%websocket-protocol-error
     "A WebSocket session requires a stream." stream))
  (unless (functionp handler)
    (%websocket-protocol-error
     "A WebSocket session handler must be callable." handler))
  (%websocket-validate-limit max-message-bytes "MAX-MESSAGE-BYTES")
  (%websocket-validate-limit max-payload-bytes "MAX-PAYLOAD-BYTES")
  (when (and max-messages
             (or (not (integerp max-messages)) (minusp max-messages)))
    (%websocket-protocol-error
     "MAX-MESSAGES must be NIL or a non-negative integer."
     max-messages))
  (when (and on-control (not (functionp on-control)))
    (%websocket-protocol-error
     "ON-CONTROL must be a function or NIL." on-control))
  (when (and on-error (not (functionp on-error)))
    (%websocket-protocol-error
     "ON-ERROR must be a function or NIL." on-error))
  (when (and close-stream (not (functionp close-stream)))
    (%websocket-protocol-error
     "CLOSE-STREAM must be a function or NIL." close-stream))
  (let ((message-count 0)
        (close-sent-p nil)
        (close-tag (gensym "WEBSOCKET-CLOSE-")))
    (labels ((send-close (&key payload code reason)
               (unless close-sent-p
                 (setf close-sent-p t)
                 (if payload
                     (websocket-close stream :payload payload)
                     (websocket-close stream
                                      :code (or code 1000)
                                      :reason (or reason "")))))
             (handle-control (frame)
               (let ((opcode (websocket-frame-opcode frame))
                     (payload (websocket-frame-payload frame)))
                 (when on-control
                   (funcall on-control frame))
                 (case opcode
                   (9
                    (websocket-pong stream :payload payload))
                   (8
                    (parse-websocket-close-payload payload)
                    (send-close :payload payload)
                    (throw close-tag :peer-close))
                   (10 nil)))))
      (unwind-protect
           (handler-case
               (let ((termination
                       (if (and max-messages (zerop max-messages))
                           (progn
                             (send-close :code 1000)
                             :max-messages)
                           (catch close-tag
                             (loop
                               (multiple-value-bind (payload opcode)
                                   (read-websocket-message
                                    stream
                                    :max-message-bytes max-message-bytes
                                    :max-payload-bytes max-payload-bytes
                                    :require-mask-p require-mask-p
                                    :allow-unmasked-p allow-unmasked-p
                                    :on-control #'handle-control)
                                 (incf message-count)
                                 (when (eq :close
                                           (funcall handler
                                                    stream payload opcode))
                                   (send-close :code 1000)
                                   (return :handler-close))
                                 (when (and max-messages
                                            (>= message-count max-messages))
                                   (send-close :code 1000)
                                   (return :max-messages))))))))
                 (values message-count termination))
             (error (condition)
               (when close-on-error-p
                 (ignore-errors
                   (send-close
                    :code (%websocket-session-close-code condition)
                    :reason "WebSocket session error")))
               (when on-error
                 (funcall on-error condition))
               (error condition)))
        (when close-stream
          (funcall close-stream stream))))))

(in-package #:http-kit/client)

(defconstant +http-sse-default-max-events+ 10000)
(defconstant +http-sse-default-max-line-bytes+ 65536)
(defconstant +http-sse-default-max-data-bytes+ (* 16 1024 1024))

(defun %sse-protocol-error (message &optional detail)
  (error 'http-protocol-error
         :message message
         :operation :sse
         :detail detail))

(defun %sse-size-error (message limit observed kind)
  (error 'http-size-limit-exceeded
         :message message
         :operation :sse
         :detail (list :kind kind :limit limit :observed observed)
         :limit limit
         :observed observed
         :kind kind))

(defun %sse-validate-limit (value name)
  (unless (or (null value)
              (and (integerp value) (>= value 0)))
    (%sse-protocol-error
     (format nil "~A must be NIL or a non-negative integer." name)
     value))
  value)

(defun %sse-no-line-breaks-p (value)
  (and (stringp value)
       (not (find-if (lambda (character)
                       (or (char= character #\Return)
                           (char= character #\Linefeed)))
                     value))))

(defun %sse-normalize-comments (comments)
  (let ((normalized
          (cond ((null comments) nil)
                ((stringp comments) (list comments))
                ((listp comments)
                 (handler-case
                     (and (every #'stringp comments)
                          (copy-list comments))
                   (type-error () nil)))
                (t nil))))
    (unless (and (or (null comments) normalized)
                 (every #'%sse-no-line-breaks-p normalized))
      (%sse-protocol-error
       "SSE comments must be strings without line breaks."
       comments))
    normalized))

(defstruct (http-sse-event
             (:constructor %make-http-sse-event
                 (&key event data id retry comments)))
  event
  data
  id
  retry
  comments)

(defun make-http-sse-event (&key (event "message") (data "") id retry comments)
  "Construct a server-sent event value.

EVENT and DATA are strings.  ID and RETRY are optional; COMMENTS may be a
string or a list of strings and is emitted as SSE comment lines."
  (unless (and (stringp event) (%sse-no-line-breaks-p event))
    (%sse-protocol-error
     "An SSE event name must be a string without line breaks."
     event))
  (unless (stringp data)
    (%sse-protocol-error "An SSE event data value must be a string." data))
  (when (and id
             (or (not (stringp id))
                 (not (%sse-no-line-breaks-p id))
                 (find #\Null id :test #'char=)))
    (%sse-protocol-error
     "An SSE event ID must be a string without line breaks or NUL."
     id))
  (when (and retry
             (or (not (integerp retry)) (< retry 0)))
    (%sse-protocol-error
     "An SSE retry value must be a non-negative integer or NIL."
     retry))
  (%make-http-sse-event
   :event event
   :data data
   :id id
   :retry retry
   :comments (%sse-normalize-comments comments)))

(defun %sse-utf8-continuation-p (byte)
  (<= #x80 byte #xbf))

(defun %sse-utf8-string (octets)
  (unless (%client-octet-vector-p octets)
    (%sse-protocol-error "SSE input lines must contain octets." octets))
  (with-output-to-string (result)
    (loop with index = 0
          with length = (length octets)
          while (< index length)
          do (let ((first (aref octets index)))
               (cond ((<= first #x7f)
                      (write-char (code-char first) result)
                      (incf index))
                     ((<= #xc2 first #xdf)
                      (when (>= (+ index 1) length)
                        (%sse-protocol-error
                         "An SSE input line ended in a partial UTF-8 sequence."
                         octets))
                      (let ((second (aref octets (1+ index))))
                        (unless (%sse-utf8-continuation-p second)
                          (%sse-protocol-error
                           "An SSE input line contains invalid UTF-8."
                           octets))
                        (write-char
                         (code-char (+ (ash (logand first #x1f) 6)
                                       (logand second #x3f)))
                         result)
                        (incf index 2)))
                     ((<= #xe0 first #xef)
                      (when (>= (+ index 2) length)
                        (%sse-protocol-error
                         "An SSE input line ended in a partial UTF-8 sequence."
                         octets))
                      (let ((second (aref octets (1+ index)))
                            (third (aref octets (+ index 2))))
                        (unless (and (%sse-utf8-continuation-p second)
                                     (%sse-utf8-continuation-p third)
                                     (or (/= first #xe0) (>= second #xa0))
                                     (or (/= first #xed) (<= second #x9f)))
                          (%sse-protocol-error
                           "An SSE input line contains invalid UTF-8."
                           octets))
                        (write-char
                         (code-char (+ (ash (logand first #x0f) 12)
                                       (ash (logand second #x3f) 6)
                                       (logand third #x3f)))
                         result)
                        (incf index 3)))
                     ((<= #xf0 first #xf4)
                      (when (>= (+ index 3) length)
                        (%sse-protocol-error
                         "An SSE input line ended in a partial UTF-8 sequence."
                         octets))
                      (let ((second (aref octets (1+ index)))
                            (third (aref octets (+ index 2)))
                            (fourth (aref octets (+ index 3))))
                        (unless (and (%sse-utf8-continuation-p second)
                                     (%sse-utf8-continuation-p third)
                                     (%sse-utf8-continuation-p fourth)
                                     (or (/= first #xf0) (>= second #x90))
                                     (or (/= first #xf4) (<= second #x8f)))
                          (%sse-protocol-error
                           "An SSE input line contains invalid UTF-8."
                           octets))
                        (write-char
                         (code-char (+ (ash (logand first #x07) 18)
                                       (ash (logand second #x3f) 12)
                                       (ash (logand third #x3f) 6)
                                       (logand fourth #x3f)))
                         result)
                        (incf index 4)))
                     (t
                      (%sse-protocol-error
                       "An SSE input line contains invalid UTF-8."
                       octets)))))))

(defstruct (%sse-state
             (:constructor %make-sse-state
                 (&key max-events max-line-bytes max-data-bytes
                       on-event collect-events-p)))
  max-events
  max-line-bytes
  max-data-bytes
  on-event
  collect-events-p
  (event-field nil)
  (data-lines nil)
  (data-bytes 0)
  (id-field nil)
  (retry nil)
  (comments nil)
  (line (make-array 0
                    :element-type '(unsigned-byte 8)
                    :adjustable t
                    :fill-pointer 0))
  (first-line-p t)
  (pending-cr-p nil)
  (event-count 0)
  (events nil))

(defun %sse-make-state
    (&key max-events max-line-bytes max-data-bytes on-event collect-events-p)
  (%sse-validate-limit max-events "MAX-EVENTS")
  (%sse-validate-limit max-line-bytes "MAX-LINE-BYTES")
  (%sse-validate-limit max-data-bytes "MAX-DATA-BYTES")
  (when (and on-event (not (functionp on-event)))
    (%sse-protocol-error "ON-EVENT must be a function or NIL." on-event))
  (%make-sse-state
   :max-events max-events
   :max-line-bytes max-line-bytes
   :max-data-bytes max-data-bytes
   :on-event on-event
   :collect-events-p collect-events-p))

(defun %sse-state-reset-event (state)
  (setf (%sse-state-event-field state) nil
        (%sse-state-data-lines state) nil
        (%sse-state-data-bytes state) 0
        (%sse-state-id-field state) nil
        (%sse-state-retry state) nil
        (%sse-state-comments state) nil)
  state)

(defun %sse-state-dispatch (state)
  (when (plusp (length (%sse-state-data-lines state)))
    (when (and (%sse-state-max-events state)
               (>= (%sse-state-event-count state)
                   (%sse-state-max-events state)))
      (%sse-size-error
       "An SSE input exceeded its event-count limit."
       (%sse-state-max-events state)
       (1+ (%sse-state-event-count state))
       :events))
    (let ((event
            (%make-http-sse-event
             :event (if (and (%sse-state-event-field state)
                            (plusp (length (%sse-state-event-field state))))
                        (%sse-state-event-field state)
                        "message")
             :data (with-output-to-string (result)
                     (loop for data-line in
                             (reverse (%sse-state-data-lines state))
                           for firstp = t then nil
                           do (unless firstp
                                (write-char #\Linefeed result))
                              (write-string data-line result)))
             :id (%sse-state-id-field state)
             :retry (%sse-state-retry state)
             :comments (reverse (%sse-state-comments state)))))
      (incf (%sse-state-event-count state))
      (when (%sse-state-collect-events-p state)
        (push event (%sse-state-events state)))
      (when (%sse-state-on-event state)
        (funcall (%sse-state-on-event state) event)))
    (%sse-state-reset-event state))
  state)

(defun %sse-parse-retry (octets)
  (when (and (plusp (length octets))
             (loop for byte across octets
                   always (<= #x30 byte #x39)))
    (parse-integer (%sse-utf8-string octets) :radix 10)))

(defun %sse-state-process-line (state line)
  (let ((start 0)
        (end (length line)))
    (when (%sse-state-first-line-p state)
      (setf (%sse-state-first-line-p state) nil)
      (when (and (>= end 3)
                 (= (aref line 0) #xef)
                 (= (aref line 1) #xbb)
                 (= (aref line 2) #xbf))
        (setf start 3)))
    (if (= start end)
        (%sse-state-dispatch state)
        (if (= (aref line start) #x3a)
            (push (%sse-utf8-string (subseq line (1+ start) end))
                  (%sse-state-comments state))
            (let* ((colon (loop for index from start below end
                                when (= (aref line index) #x3a)
                                  return index))
                   (field-end (or colon end))
                   (value-start (if colon (1+ colon) end)))
              (when (and (< value-start end)
                         (= (aref line value-start) #x20))
                (incf value-start))
              (let* ((field (%sse-utf8-string (subseq line start field-end)))
                     (value-octets (subseq line value-start end))
                     (value (%sse-utf8-string value-octets)))
                (cond ((string= field "data")
                       (let ((addition
                               (+ (length value-octets)
                                  (if (plusp
                                       (length (%sse-state-data-lines state)))
                                      1
                                      0))))
                         (when (and (%sse-state-max-data-bytes state)
                                    (> (+ (%sse-state-data-bytes state)
                                          addition)
                                       (%sse-state-max-data-bytes state)))
                           (%sse-size-error
                            "An SSE event exceeded its data-size limit."
                            (%sse-state-max-data-bytes state)
                            (+ (%sse-state-data-bytes state) addition)
                            :data))
                         (incf (%sse-state-data-bytes state) addition)
                         (push value (%sse-state-data-lines state))))
                      ((string= field "event")
                       (setf (%sse-state-event-field state) value))
                      ((string= field "id")
                       (unless (find #x00 value-octets)
                         (setf (%sse-state-id-field state) value)))
                      ((string= field "retry")
                       (let ((retry (%sse-parse-retry value-octets)))
                         (when retry
                           (setf (%sse-state-retry state) retry)))))))))))

(defun %sse-state-finish-line (state)
  (let ((line (%sse-state-line state)))
    (%sse-state-process-line state line)
    (setf (fill-pointer line) 0))
  state)

(defun %sse-state-append-byte (state byte)
  (unless (and (integerp byte) (<= 0 byte #xff))
    (%sse-protocol-error "An SSE input byte is outside the octet range." byte))
  (when (and (%sse-state-max-line-bytes state)
             (>= (length (%sse-state-line state))
                 (%sse-state-max-line-bytes state)))
    (%sse-size-error
     "An SSE input line exceeded its size limit."
     (%sse-state-max-line-bytes state)
     (1+ (length (%sse-state-line state)))
     :line))
  (vector-push-extend byte (%sse-state-line state))
  state)

(defun %sse-state-feed-byte (state byte)
  (if (%sse-state-pending-cr-p state)
      (progn
        (setf (%sse-state-pending-cr-p state) nil)
        (if (= byte #x0a)
            (%sse-state-finish-line state)
            (progn
              (%sse-state-finish-line state)
              (%sse-state-feed-byte state byte))))
      (cond ((= byte #x0d)
             (setf (%sse-state-pending-cr-p state) t))
            ((= byte #x0a)
             (%sse-state-finish-line state))
            (t
             (%sse-state-append-byte state byte)))))

(defun %sse-state-finish (state)
  (when (%sse-state-pending-cr-p state)
    (setf (%sse-state-pending-cr-p state) nil)
    (%sse-state-finish-line state))
  (when (plusp (length (%sse-state-line state)))
    (%sse-state-finish-line state))
  ;; A final event without a blank line is useful for complete finite bodies;
  ;; a live stream still dispatches normally as soon as it receives a blank
  ;; line.
  (%sse-state-dispatch state)
  state)

(defun %sse-state-result (state)
  (if (%sse-state-collect-events-p state)
      (nreverse (%sse-state-events state))
      nil))

(defun %sse-input-octets (input)
  (cond ((stringp input) (http-utf8-octets input))
        ((%client-octet-vector-p input) (%copy-client-octets input))
        (t
         (%sse-protocol-error
          "SSE input must be a string or a vector of octets."
          input))))

(defun parse-http-sse-events
    (input &key (max-events +http-sse-default-max-events+)
                 (max-line-bytes +http-sse-default-max-line-bytes+)
                 (max-data-bytes +http-sse-default-max-data-bytes+))
  "Parse an SSE body into HTTP-SSE-EVENT values.

The parser accepts UTF-8 strings or octet vectors, recognizes CRLF, LF, and
CR line endings, strips an initial UTF-8 BOM, and dispatches a final event at
EOF even when the body has no trailing blank line.  Limits are safety bounds;
NIL disables an individual bound."
  (let ((state (%sse-make-state
                :max-events max-events
                :max-line-bytes max-line-bytes
                :max-data-bytes max-data-bytes
                :collect-events-p t)))
    (loop for byte across (%sse-input-octets input)
          do (%sse-state-feed-byte state byte))
    (%sse-state-finish state)
    (%sse-state-result state)))

(defun read-http-sse-events
    (stream &key (max-events +http-sse-default-max-events+)
                  (max-line-bytes +http-sse-default-max-line-bytes+)
                  (max-data-bytes +http-sse-default-max-data-bytes+)
                  on-event (collect-events-p t))
  "Read SSE events from STREAM.

STREAM may be a binary or character stream.  ON-EVENT is called for each
dispatched event.  The return value is the collected event list when
COLLECT-EVENTS-P is true, otherwise NIL."
  (unless (streamp stream)
    (%sse-protocol-error "SSE input must be a stream." stream))
  (let ((state (%sse-make-state
                :max-events max-events
                :max-line-bytes max-line-bytes
                :max-data-bytes max-data-bytes
                :on-event on-event
                :collect-events-p collect-events-p)))
    (if (handler-case
            (subtypep (stream-element-type stream) 'character)
          (error () nil))
        (loop for character = (read-char stream nil :eof)
              until (eq character :eof)
              do (loop for byte across (http-utf8-octets (string character))
                       do (%sse-state-feed-byte state byte)))
        (loop for byte = (read-byte stream nil :eof)
              until (eq byte :eof)
              do (%sse-state-feed-byte state byte)))
    (%sse-state-finish state)
    (%sse-state-result state)))

(defun %sse-append-octets (result octets)
  (loop for byte across octets
        do (vector-push-extend byte result))
  result)

(defun %sse-append-string (result string)
  (%sse-append-octets result (http-utf8-octets string)))

(defun %sse-emit-line (result prefix value)
  (%sse-append-string result prefix)
  (when value
    (%sse-append-string result value))
  (%sse-append-octets result #(13 10)))

(defun %sse-string-lines (string)
  (let ((start 0)
        (length (length string))
        (lines nil)
        (index 0))
    (loop while (< index length)
          do (if (or (char= (char string index) #\Return)
                     (char= (char string index) #\Linefeed))
                 (progn
                   (push (subseq string start index) lines)
                   (when (and (char= (char string index) #\Return)
                              (< (1+ index) length)
                              (char= (char string (1+ index)) #\Linefeed))
                     (incf index))
                   (incf index)
                   (setf start index))
                 (incf index)))
    (push (subseq string start length) lines)
    (nreverse lines)))

(defun serialize-http-sse-event (event)
  "Serialize one HTTP-SSE-EVENT as UTF-8 octets ending in a blank line."
  (unless (http-sse-event-p event)
    (%sse-protocol-error "Expected an HTTP-SSE-EVENT value." event))
  (let ((event-name (http-sse-event-event event))
        (data (http-sse-event-data event))
        (id (http-sse-event-id event))
        (retry (http-sse-event-retry event))
        (comments (%sse-normalize-comments
                   (http-sse-event-comments event)))
        (result (make-array 0
                            :element-type '(unsigned-byte 8)
                            :adjustable t
                            :fill-pointer 0)))
    (unless (and (stringp event-name) (%sse-no-line-breaks-p event-name))
      (%sse-protocol-error
       "An SSE event name must be a string without line breaks."
       event-name))
    (unless (stringp data)
      (%sse-protocol-error "An SSE event data value must be a string." data))
    (when (and id
               (or (not (stringp id))
                   (not (%sse-no-line-breaks-p id))
                   (find #\Null id :test #'char=)))
      (%sse-protocol-error
       "An SSE event ID must be a string without line breaks or NUL."
       id))
    (when (and retry
               (or (not (integerp retry)) (< retry 0)))
      (%sse-protocol-error
       "An SSE retry value must be a non-negative integer or NIL."
       retry))
    (dolist (comment comments)
      (%sse-emit-line result ":" comment))
    (when (and (plusp (length event-name))
               (not (string= event-name "message")))
      (%sse-emit-line result "event:" event-name))
    (when id
      (%sse-emit-line result "id:" id))
    (when retry
      (%sse-emit-line result "retry:" (princ-to-string retry)))
    (dolist (data-line (%sse-string-lines data))
      (%sse-emit-line result "data:" data-line))
    (%sse-emit-line result "" nil)
    (let ((copy (make-array (length result)
                            :element-type '(unsigned-byte 8))))
      (replace copy result)
      copy)))
