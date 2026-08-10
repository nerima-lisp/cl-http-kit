(in-package #:http-kit)

(defun %monotonic-time ()
  (/ (float (get-internal-real-time))
     (float internal-time-units-per-second)))

(defun http-deadline (timeout &key deadline (clock-function #'%monotonic-time))
  "Return an absolute monotonic deadline from TIMEOUT and DEADLINE.
Both values are seconds.  NIL means that no bound was requested."
  (unless (or (null timeout) (and (realp timeout) (>= timeout 0)))
    (error 'http-protocol-error
           :message "HTTP timeout must be a non-negative real number."
           :operation :deadline
           :detail timeout))
  (unless (or (null deadline) (realp deadline))
    (error 'http-protocol-error
           :message "HTTP deadline must be a real number."
           :operation :deadline
           :detail deadline))
  (let ((timeout-deadline (and timeout (+ (funcall clock-function) timeout))))
    (cond ((and timeout-deadline deadline) (min timeout-deadline deadline))
          (timeout-deadline timeout-deadline)
          (deadline deadline)
          (t nil))))

(defun %check-deadline (deadline clock-function &optional (kind :read))
  (when (and deadline (>= (funcall clock-function) deadline))
    (error 'http-timeout
           :message "The HTTP operation exceeded its deadline."
           :operation kind
           :kind kind)))

(defun %copy-octets (value &key (allow-list t))
  (cond
    ((and (arrayp value) (= (array-rank value) 1)
          (not (stringp value))
          (loop for item across value
                always (and (integerp item) (<= 0 item 255))))
     (let ((copy (make-array (length value) :element-type '(unsigned-byte 8))))
       (replace copy value)
       copy))
    ((and allow-list (listp value)
          (every (lambda (item) (and (integerp item) (<= 0 item 255))) value))
     (make-array (length value) :element-type '(unsigned-byte 8)
                 :initial-contents value))
    (t
     (error 'http-protocol-error
            :message "HTTP message bodies must be one-dimensional octet vectors."
            :operation :body
            :detail (type-of value)))))

(defun %empty-octets ()
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun %string-octets (string &key (context :text))
  (unless (stringp string)
    (error 'http-protocol-error
           :message "Expected a string."
           :operation context
           :detail (type-of string)))
  (let ((result (make-array (length string) :element-type '(unsigned-byte 8))))
    (loop for character across string
          for index from 0
          for code = (char-code character)
          do (if (<= code #xff)
                 (setf (aref result index) code)
                 (error 'http-protocol-error
                        :message "A string contains a character outside the octet range."
                        :operation context
                        :detail code)))
    result))

(defun %octets-string (octets)
  (unless (typep octets '(or vector list))
    (error 'http-protocol-error
           :message "Expected an octet sequence."
           :operation :text
           :detail (type-of octets)))
  (let ((result (make-string (length octets))))
    (loop for index below (length octets)
          for octet = (elt octets index)
          do (unless (and (integerp octet) (<= 0 octet #xff))
               (error 'http-protocol-error
                      :message "An octet sequence contains an invalid value."
                      :operation :text
                      :detail octet))
             (setf (aref result index) (code-char octet)))
    result))

(defun %ascii-lowercase (string)
  (string-downcase string))

(defun %ascii-name-char-p (character)
  (let ((code (char-code character)))
    (or (and (<= (char-code #\0) code)
             (<= code (char-code #\9)))
        (and (<= (char-code #\A) code)
             (<= code (char-code #\Z)))
        (and (<= (char-code #\a) code)
             (<= code (char-code #\z)))
        (and (<= code #x7f)
             (find character "!#$%&'*+-.^_`|~" :test #'char=)))))

(defun %token-p (string)
  (and (plusp (length string))
       (every #'%ascii-name-char-p string)))

(defun %header-name-p (string)
  (%token-p string))

(defun %header-value-p (string)
  (every (lambda (character)
           (let ((code (char-code character)))
             (or (= code #x09)
                 (<= #x20 code #x7e)
                 (<= #x80 code #xff))))
         string))

(defun %trim-ows (string)
  (string-trim '(#\Space #\Tab) string))

(defun %join-octets (&rest vectors)
  (let ((result (make-array (reduce #'+ vectors :key #'length)
                            :element-type '(unsigned-byte 8)))
        (position 0))
    (dolist (vector vectors result)
      (replace result vector :start1 position)
      (incf position (length vector)))))

(defun %append-octet (vector octet)
  (let ((result (make-array (1+ (length vector))
                            :element-type '(unsigned-byte 8))))
    (replace result vector)
    (setf (aref result (length vector)) octet)
    result))

(defun %decimal-string-p (string)
  (and (plusp (length string))
       (every (lambda (character)
                (let ((code (char-code character)))
                  (and (<= (char-code #\0) code)
                       (<= code (char-code #\9)))))
              string)))

(defun %parse-decimal (string)
  (unless (%decimal-string-p string)
    (error 'http-protocol-error
           :message "Expected an ASCII decimal integer."
           :operation :integer
           :detail string))
  (parse-integer string))

(defun %hex-digit (character)
  (let ((code (char-code character)))
    (cond ((and (<= (char-code #\0) code)
                (<= code (char-code #\9)))
           (- code (char-code #\0)))
          ((and (<= (char-code #\A) code)
                (<= code (char-code #\F)))
           (+ 10 (- code (char-code #\A))))
          ((and (<= (char-code #\a) code)
                (<= code (char-code #\f)))
           (+ 10 (- code (char-code #\a))))
          (t
           (error 'http-protocol-error
                  :message "Expected a hexadecimal digit."
                  :operation :chunk-size
                  :detail character)))))

(defun %bounded-diagnostic (value &optional (limit 160))
  (let ((text (princ-to-string value)))
    (if (> (length text) limit)
        (concatenate 'string (subseq text 0 limit) "...")
        text)))
