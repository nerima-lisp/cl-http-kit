(in-package #:http-kit/client)

(defun %content-coding-trim (value)
  (string-trim '(#\Space #\Tab) value))

(defun %content-coding-token-p (value)
  (and (stringp value)
       (not (string= value ""))
       (every (lambda (character)
                (let ((code (char-code character)))
                  (and (<= #x21 code #x7e)
                       (not (find character "()<>@,;:\\\\\"/[]?={} \t"
                                   :test #'char=)))))
              value)))

(defstruct (http-content-coding
             (:constructor %make-http-content-coding)
             (:conc-name http-content-coding-))
  name
  encoder
  decoder)

(defun make-http-content-coding (&key name encoder decoder)
  "Create a content-coding adapter for NAME.

ENCODER and DECODER are deliberately supplied by the application so this
layer does not select a compression implementation or hide its licensing and
resource limits."
  (unless (%content-coding-token-p name)
    (%client-protocol-error
     "Content-coding names must be valid HTTP tokens."
     name))
  (when encoder
    (%ensure-function encoder "A content-coding encoder must be a function."))
  (when decoder
    (%ensure-function decoder "A content-coding decoder must be a function."))
  (%make-http-content-coding
   :name (string-downcase name)
   :encoder encoder
   :decoder decoder))

(defun %content-coding-split (value separator)
  (let ((parts nil)
        (start 0))
    (loop for position = (position separator value :start start)
          do (push (%content-coding-trim
                    (subseq value start (or position (length value))))
                   parts)
          if position
            do (setf start (1+ position))
          else
            do (return (nreverse parts)))))

(defun %content-coding-split-assignment (value)
  (let ((position (position #\= value)))
    (if position
        (values (%content-coding-trim (subseq value 0 position))
                (%content-coding-trim (subseq value (1+ position))))
        (values (%content-coding-trim value) nil))))

(defun %content-coding-qvalue (value)
  (let ((value (string-downcase (%content-coding-trim value))))
    (cond
      ((string= value "0") 0)
      ((string= value "1") 1)
      ((and (> (length value) 2)
            (char= (char value 1) #\.)
            (member (char value 0) '(#\0 #\1)))
       (let ((digits (subseq value 2)))
         (when (and (<= 1 (length digits) 3)
                    (every #'digit-char-p digits))
           (let ((fraction (/ (parse-integer digits)
                              (expt 10 (length digits)))))
             (if (char= (char value 0) #\0)
                 fraction
                 (and (zerop (parse-integer digits))
                      1))))))
      (t nil))))

(defun %content-coding-header-values (value)
  (cond ((null value) nil)
        ((stringp value) (list value))
        ((listp value) value)
        (t
         (%client-protocol-error
          "Accept-Encoding must be a string or a list of strings."
          value))))

(defun parse-http-accept-encoding (value)
  "Parse Accept-Encoding into a list of (NAME . QUALITY) pairs.

Names are lower-case strings and QUALITY is a number from 0 through 1.
Malformed quality values are treated as unacceptable rather than being
silently promoted to an acceptable encoding."
  (let ((result nil))
    (dolist (header (%content-coding-header-values value) (nreverse result))
      (unless (stringp header)
        (%client-protocol-error
         "Accept-Encoding values must be strings."
         header))
      (dolist (item (%content-coding-split header #\,))
        (unless (string= item "")
          (let ((parts (%content-coding-split item #\;))
                (quality 1))
            (let ((name (string-downcase (first parts))))
              (when (%content-coding-token-p name)
                (dolist (parameter (rest parts))
                  (multiple-value-bind (parameter-name parameter-value)
                      (%content-coding-split-assignment parameter)
                    (when (string= (string-downcase parameter-name) "q")
                      (setf quality
                            (or (and parameter-value
                                     (%content-coding-qvalue parameter-value))
                                0)))))
                (push (cons name quality) result)))))))))

(defun %content-coding-value-name (value)
  (cond ((http-content-coding-p value)
         (http-content-coding-name value))
        ((stringp value) (string-downcase value))
        ((symbolp value) (string-downcase (symbol-name value)))
        (t
         (%client-protocol-error
          "Supported content codings must be strings, symbols, or adapters."
          value))))

(defun %content-coding-quality-for (name parsed header-present-p)
  (let ((explicit (assoc name parsed :test #'string=))
         (wildcard (assoc "*" parsed :test #'string=)))
    (cond (explicit (cdr explicit))
          ((and wildcard (not (string= name "identity")))
           (cdr wildcard))
          ((string= name "identity") 1)
          (header-present-p 0)
          (t 1))))

(defun http-select-content-coding
    (accept-encoding supported &key (identity-p t))
  "Select the best SUPPORTED coding accepted by ACCEPT-ENCODING.

The return value is the original supported value, preserving an adapter
  object when one was supplied.  NIL means that no offered coding is
  acceptable.  When ACCEPT-ENCODING is absent, the first supported coding is
  selected."
  (let ((header-present-p (not (null accept-encoding)))
        (parsed (parse-http-accept-encoding accept-encoding))
        (candidates nil)
        (index 0))
    (dolist (coding supported)
      (let* ((name (%content-coding-value-name coding))
             (quality (%content-coding-quality-for
                       name parsed header-present-p)))
        (when (plusp quality)
          (push (list coding quality index) candidates)))
      (incf index))
    (when identity-p
      (let ((quality (%content-coding-quality-for
                      "identity" parsed header-present-p)))
        (when (plusp quality)
          (push (list "identity" quality index) candidates)))
      (incf index))
    (let ((selected
            (stable-sort candidates
                         (lambda (left right)
                           (or (> (second left) (second right))
                               (and (= (second left) (second right))
                                    (< (third left) (third right))))))))
      (and selected (first (first selected))))))

(defun http-content-coding-encode (coding octets)
  "Encode OCTETS with a content-coding adapter."
  (unless (http-content-coding-p coding)
    (%client-protocol-error
     "Content-coding encoding requires an HTTP-CONTENT-CODING adapter."
     coding))
  (unless (http-content-coding-encoder coding)
    (%client-protocol-error
     "The selected content coding has no encoder."
     (http-content-coding-name coding)))
  (funcall (http-content-coding-encoder coding) octets))

(defun http-content-coding-decode (coding octets)
  "Decode OCTETS with a content-coding adapter."
  (unless (http-content-coding-p coding)
    (%client-protocol-error
     "Content-coding decoding requires an HTTP-CONTENT-CODING adapter."
     coding))
  (unless (http-content-coding-decoder coding)
    (%client-protocol-error
     "The selected content coding has no decoder."
     (http-content-coding-name coding)))
  (funcall (http-content-coding-decoder coding) octets))
