(in-package #:http-kit/client)

(defconstant +http-sse-default-max-events+ 10000)

(defconstant +http-sse-default-max-line-bytes+ 65536)

(defconstant +http-sse-default-max-data-bytes+ (* 16 1024 1024))

(defconstant +http-sse-default-max-event-bytes+ (* 16 1024 1024))

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
             (or (not (integerp retry)) (minusp retry)))
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
                      (when (>= (1+ index) length)
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
