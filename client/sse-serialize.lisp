(in-package #:http-kit/client)

(defun %sse-append-octets (result octets)
  (loop for byte across octets
        do (vector-push-extend byte result))
  result)

(defun %sse-append-string (result string)
  (%sse-append-octets result (cl-codec-kit:string-to-octets string :encoding :utf-8)))

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
               (or (not (integerp retry)) (minusp retry)))
      (%sse-protocol-error
       "An SSE retry value must be a non-negative integer or NIL."
       retry))
    (dolist (comment comments)
      (%sse-emit-line result ":" comment))
    (when (and (not (string= event-name ""))
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
