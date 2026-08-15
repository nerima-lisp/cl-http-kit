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

(defun %forbidden-trailer-field-name-p (name)
  (member (string-downcase name)
          '("age" "authorization" "cache-control" "connection"
            "content-encoding" "content-language" "content-length"
            "content-location" "content-range" "content-type" "cookie"
            "date" "expect" "expires" "host" "if-match"
            "if-modified-since" "if-none-match" "if-range"
            "if-unmodified-since" "keep-alive" "location" "max-forwards"
            "proxy-authenticate" "proxy-authentication-info"
            "proxy-authorization" "proxy-connection" "range" "retry-after"
            "set-cookie" "te" "trailer" "transfer-encoding" "upgrade"
            "vary" "via" "warning" "www-authenticate")
          :test #'string=))

(defun %http1-chunk-size-text (line)
  (labels ((ows-p (character)
             (or (char= character #\Space)
                 (char= character #\Tab)))
           (skip-ows (index)
             (loop while (and (< index (length line))
                              (ows-p (char line index)))
                   do (incf index)
                   finally (return index)))
           (skip-token (index)
             (loop while (and (< index (length line))
                              (%ascii-name-char-p (char line index)))
                   do (incf index)
                   finally (return index)))
           (skip-quoted-string (index)
             (loop with escaped-p = nil
                   for cursor from (1+ index) below (length line)
                   for character = (char line cursor)
                   for code = (char-code character)
                   do (cond
                        (escaped-p
                         (unless (or (= code #x09)
                                     (<= #x20 code #x7e)
                                     (<= #x80 code #xff))
                           (return nil))
                         (setf escaped-p nil))
                        ((char= character #\\)
                         (setf escaped-p t))
                        ((char= character #\")
                         (return (1+ cursor)))
                        ((not (or (= code #x09)
                                  (= code #x20)
                                  (= code #x21)
                                  (<= #x23 code #x5b)
                                  (<= #x5d code #x7e)
                                  (<= #x80 code #xff)))
                         (return nil)))
                   finally (return nil))))
    (let ((size-end 0)
          (cursor 0))
      (loop while (and (< size-end (length line))
                       (%hex-character-p (char line size-end)))
            do (incf size-end))
      (when (zerop size-end)
        (return-from %http1-chunk-size-text nil))
      (setf cursor size-end)
      (loop
        (when (= cursor (length line))
          (return (subseq line 0 size-end)))
        (let ((extension-start cursor))
          (setf cursor (skip-ows cursor))
          (unless (and (< cursor (length line))
                       (char= (char line cursor) #\;))
            (return nil))
          (incf cursor)
          (setf cursor (skip-ows cursor))
          (let ((name-start cursor))
            (setf cursor (skip-token cursor))
            (when (= cursor name-start)
              (return nil)))
          (setf cursor (skip-ows cursor))
          (when (and (< cursor (length line))
                     (char= (char line cursor) #\=))
            (incf cursor)
            (setf cursor (skip-ows cursor))
            (cond
              ((and (< cursor (length line))
                    (char= (char line cursor) #\"))
               (setf cursor (skip-quoted-string cursor)))
              (t
               (let ((value-start cursor))
                 (setf cursor (skip-token cursor))
                 (when (= cursor value-start)
                   (return nil)))))
            (unless cursor
              (return nil)))
          (when (= extension-start cursor)
            (return nil)))))))

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

(defun %bounded-diagnostic (value &optional (limit 160))
  (let ((text (princ-to-string value)))
    (if (> (length text) limit)
        (concatenate 'string (subseq text 0 limit) "...")
        text)))

(defun format-http-priority-field-value (&key (urgency 3) incremental)
  "Return a canonical RFC 9218 Priority field value."
  (unless (and (integerp urgency) (<= 0 urgency 7))
    (error 'http-protocol-error
           :message "HTTP priority urgency must be an integer from 0 through 7."
           :operation :http-priority
           :detail urgency))
  (unless (typep incremental 'boolean)
    (error 'http-protocol-error
           :message "HTTP priority incremental must be a boolean."
           :operation :http-priority
           :detail incremental))
  (format nil "u=~D~:[~;, i~]" urgency incremental))
