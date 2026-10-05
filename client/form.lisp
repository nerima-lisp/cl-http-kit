(in-package #:http-kit/client)

(defun %hex-digit (value)
  (char "0123456789ABCDEF" value))

(defun %percent-safe-byte-p (byte safe)
  (or (and (<= (char-code #\A) byte) (<= byte (char-code #\Z)))
      (and (<= (char-code #\a) byte) (<= byte (char-code #\z)))
      (and (<= (char-code #\0) byte) (<= byte (char-code #\9)))
      (and safe (find (code-char byte) safe :test #'char=))))

(defun http-percent-encode (string &key (safe "-._~") (space-as-plus-p nil))
  "Percent-encode STRING as UTF-8 octets.

SAFE contains additional ASCII bytes that remain literal.  When
SPACE-AS-PLUS-P is true, spaces are emitted as plus signs; all other bytes
outside the safe set use uppercase hexadecimal escapes."
  (unless (stringp string)
    (%client-protocol-error "Percent encoding requires a string." string))
  (unless (or (null safe) (stringp safe))
    (%client-protocol-error "The percent-encoding safe set must be a string or NIL."
                            safe))
  (let ((octets (cl-codec-kit:string-to-octets string :encoding :utf-8)))
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
       ((and (consp field) (consp (cdr field))
             (null (cddr field)) (stringp (first field)))
        (values (first field) (second field)))
       (t
        (%client-protocol-error "Form fields must be name/value pairs." field))))

(defun http-form-urlencode (fields)
  "Encode FIELDS as an application/x-www-form-urlencoded string.

Each field is a two-element list containing a string name and a string or NIL
value.  NIL values are represented by an empty value."
  (unless (listp fields)
    (%client-protocol-error "Form fields must be a list." fields))
  (with-output-to-string (stream)
    (loop for field in fields
          for firstp = t then nil
          do (multiple-value-bind (name value) (%form-field field)
               (unless (or (null value) (stringp value))
                 (%client-protocol-error
                  "Form field values must be strings or NIL." value))
               (unless firstp (write-char #\& stream))
               (write-string (http-percent-encode name :safe "-._*"
                                                  :space-as-plus-p t)
                             stream)
               (write-char #\= stream)
               (write-string (http-percent-encode (or value "") :safe "-._*"
                                                  :space-as-plus-p t)
                             stream)))))

(defun http-form-urlencoded-octets (fields)
  "Encode FIELDS as UTF-8 octets for an HTTP request body."
  (cl-codec-kit:string-to-octets (http-form-urlencode fields) :encoding :utf-8))
