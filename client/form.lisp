(in-package #:http-kit/client)

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
      (and (< byte #x80) safe
           (find (code-char byte) safe :test #'char=))))

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
