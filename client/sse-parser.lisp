(in-package #:http-kit/client)

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
  (when (consp (%sse-state-data-lines state))
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
    (parse-integer (%sse-utf8-string octets))))

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
                                  (if (consp (%sse-state-data-lines state))
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
  (loop
    (when (%sse-state-pending-cr-p state)
      (setf (%sse-state-pending-cr-p state) nil)
      (%sse-state-finish-line state)
      (when (= byte #x0a)
        (return state)))
    (cond ((= byte #x0d)
           (setf (%sse-state-pending-cr-p state) t)
           (return state))
          ((= byte #x0a)
           (%sse-state-finish-line state)
           (return state))
          (t
           (%sse-state-append-byte state byte)
           (return state)))))

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
  (when (%sse-state-collect-events-p state)
      (nreverse (%sse-state-events state))))

(defun %sse-input-octets (input)
  (cond ((stringp input) (cl-codec-kit:string-to-octets input :encoding :utf-8))
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
          (error () nil)) ; paredit:ignore handler-case-swallows-error -- streams may hide their element type; binary reading is the safe fallback.
        (loop for character = (read-char stream nil :eof)
              until (eq character :eof)
              do (loop for byte across (cl-codec-kit:string-to-octets (string character) :encoding :utf-8)
                       do (%sse-state-feed-byte state byte)))
        (loop for byte = (read-byte stream nil :eof)
              until (eq byte :eof)
              do (%sse-state-feed-byte state byte)))
    (%sse-state-finish state)
    (%sse-state-result state)))
