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
       ((and (consp field)
             (consp (cdr field))
             (null (cddr field))
             (stringp (first field)))
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
          do (when (or (char= character #\")
                       (char= character #\\))
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
    (body &key content-type boundary (max-parts 1000) (max-header-bytes 65536)
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
            (part-count 0)
            (body-bytes 0))
        (when (eq kind :final)
          (return-from parse-http-multipart-body))
        (incf position 2)
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
                    (when (and max-parts (>= part-count max-parts))
                      (error 'http-size-limit-exceeded
                             :message "A multipart body exceeded its part-count limit."
                             :operation :multipart
                             :detail (list :limit max-parts
                                           :observed (1+ part-count))))
                    (incf part-count)
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
