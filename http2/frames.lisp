(in-package #:http-kit/http2)

(defstruct (%h2-frame (:constructor %make-h2-frame))
  length
  type
  flags
  stream-id
  payload)

(progn
  (defstruct (%h2-reader (:constructor %make-h2-reader))
    vector
    position
    stream
    (frame-header (make-array 9 :element-type '(unsigned-byte 8)))
    (frame-header-position 0)
    frame-metadata
    frame-payload
    (frame-payload-position 0)
    stream-eof-p)

  (progn
    (defgeneric %h2-reader-read-available (stream count timeout))
    (defgeneric %h2-reader-stateful-input-p (stream))
    (defmethod %h2-reader-stateful-input-p ((stream stream))
      (declare (ignore stream))
      nil)
    #+sbcl
    (defmethod %h2-reader-stateful-input-p ((stream sb-sys:fd-stream))
      (declare (ignore stream))
      t))

  (defmethod %h2-reader-read-available ((stream stream) count timeout)
    (check-type count (integer 0 *))
    (check-type timeout (or null (real 0 *)))
    (let* ((buffer (make-array count
                               :element-type (quote (unsigned-byte 8))))
           (one-byte (make-array 1
                                 :element-type (quote (unsigned-byte 8))))
           (position 0)
           (eof-p nil)
           (timeout (if timeout (min timeout 0.1d0) 0.1d0))
           (deadline (+ (http-kit::%monotonic-time) timeout))
           (listen-supported-p
             #+sbcl
             (not (null (sb-mop:compute-applicable-methods
                         #'sb-gray:stream-listen
                         (list stream))))
             #-sbcl
             t))
      (loop while (< position count)
            do (let* ((remaining (- deadline
                                    (http-kit::%monotonic-time)))
                      (end
                        (progn
                          (when (<= remaining 0)
                            (return))
                          (if listen-supported-p
                              (if (listen stream)
                                  (read-sequence one-byte stream)
                                  (progn
                                    (when (zerop position)
                                      (sleep remaining))
                                    (return)))
                              #+sbcl
                              (handler-case
                                  (sb-ext:with-timeout (min timeout remaining)
                                    (read-sequence one-byte stream))
                                (sb-ext:timeout ()
                                  (error 'http-kit:http-connection-error
                                         :message "The HTTP/2 stream read timed out with an uncertain input position."
                                         :operation :http2-read
                                         :cause :ambiguous-read-timeout)))
                              #-sbcl
                              (read-sequence one-byte stream)))))
                 (if (= end 1)
                     (progn
                       (setf (aref buffer position) (aref one-byte 0))
                       (incf position))
                     (progn
                       (setf eof-p t)
                       (return)))))
      (values (subseq buffer 0 position)
              (cond
                ((plusp position) :data)
                (eof-p :eof)
                (t :would-block))
              eof-p)))
  #+sbcl
  (defmethod %h2-reader-read-available
      ((stream sb-sys:fd-stream) count timeout)
    ;; A timed read can consume an octet before SBCL delivers the timeout.
    ;; Wait for readiness first, then save each octet without async interruption.
    (check-type count (integer 0 *))
    (check-type timeout (or null (real 0 *)))
    (let ((buffer (make-array (min count 16384)
                              :element-type (quote (unsigned-byte 8))))
          (read-limit (min count 16384))
          (position 0)
          (eof-p nil)
          (waited-p nil)
          (timeout (if timeout (min timeout 0.1d0) 0.1d0)))
      (loop while (< position read-limit)
            do (let ((octet
                       (if (listen stream)
                           (read-byte stream nil :eof)
                           (if (or waited-p (zerop timeout))
                               :would-block
                               (progn
                                 (setf waited-p t)
                                 (if (sb-sys:wait-until-fd-usable
                                      (sb-sys:fd-stream-fd stream)
                                      :input timeout nil)
                                     (read-byte stream nil :eof)
                                     :would-block))))))
                 (cond
                   ((eq octet :would-block) (return))
                   ((eq octet :eof)
                    (setf eof-p t)
                    (return))
                   (t
                    (setf (aref buffer position) octet)
                    (incf position)))))
      (values (subseq buffer 0 position)
              (cond
                ((plusp position) :data)
                (eof-p :eof)
                (t :would-block))
              eof-p))))

(defun %h2-reader-for (input)
  (cond
    ((and (arrayp input) (= (array-rank input) 1))
     (%make-h2-reader :vector (http-kit::%copy-octets input) :position 0))
    ((and (listp input)
          (every (lambda (octet) (and (integerp octet) (<= 0 octet 255))) input))
     (%make-h2-reader :vector (http-kit::%copy-octets input) :position 0))
    ((streamp input)
     (%make-h2-reader :stream input))
    (t
     (error 'http-protocol-error
            :message "An HTTP/2 input must be an octet vector or binary stream."
            :operation :http2-read
            :detail (type-of input)))))

(defun %h2-reader-read (reader count deadline clock-function
                       &key allow-eof deadline-function on-first-byte)
  (unless (and (integerp count) (>= count 0))
    (error 'http-protocol-error
           :message "An HTTP/2 read length is invalid."
           :operation :http2-read
           :detail count))
  (let ((result (make-array count :element-type '(unsigned-byte 8)))
        (position 0)
        (first-byte-pending-p (not (null on-first-byte))))
    (labels ((read-stream ()
               (if first-byte-pending-p
                   (read-sequence result (%h2-reader-stream reader)
                                  :start position :end (1+ position))
                   (read-sequence result (%h2-reader-stream reader)
                                  :start position))))
      (loop while (< position count)
            do (let* ((active-deadline
                        (if deadline-function
                            (funcall deadline-function)
                            deadline))
                      (remaining
                        (and active-deadline
                             (max 0 (- active-deadline
                                       (funcall clock-function))))))
                 (http-kit::%check-deadline active-deadline
                                            clock-function :read)
                 (if (%h2-reader-stream reader)
                     (let ((end nil))
                       #+sbcl
                       (setf end
                             (if active-deadline
                                 (handler-case
                                     (sb-ext:with-timeout
                                         remaining
                                       (read-stream))
                                   (sb-ext:timeout ()
                                     (error 'http-kit:http-timeout
                                            :message "The HTTP/2 read operation exceeded its deadline."
                                            :operation :read
                                            :kind :read)))
                                 (read-stream)))
                       #-sbcl
                       (setf end (read-stream))
                       (when (= end position)
                         (if (and allow-eof (zerop position))
                             (return-from %h2-reader-read :eof)
                             (error 'http-protocol-error
                                    :message "The HTTP/2 input ended before a complete frame was read."
                                    :operation :http2-read
                                    :detail count)))
                       (setf position end))
                     (let ((vector (%h2-reader-vector reader))
                           (source-position (%h2-reader-position reader)))
                       (when (>= source-position (length vector))
                         (if (and allow-eof (zerop position))
                             (return-from %h2-reader-read :eof)
                             (error 'http-protocol-error
                                    :message "The HTTP/2 input ended before a complete frame was read."
                                    :operation :http2-read
                                    :detail count)))
                       (let ((available (min (- count position)
                                             (- (length vector)
                                                source-position)
                                             (if first-byte-pending-p 1 count))))
                         (replace result vector
                                  :start1 position
                                  :start2 source-position
                                  :end2 (+ source-position available))
                         (incf position available)
                         (incf (%h2-reader-position reader) available))))
                 (when first-byte-pending-p
                   (setf first-byte-pending-p nil)
                   (funcall on-first-byte))))
      result)))

(defun %h2-u24 (octets position)
  (logior (ash (aref octets position) 16)
          (ash (aref octets (1+ position)) 8)
          (aref octets (+ position 2))))

(defun %h2-u16 (octets position)
  (logior (ash (aref octets position) 8)
          (aref octets (1+ position))))

(defun %h2-u32 (octets position)
  (logior (ash (aref octets position) 24)
          (ash (aref octets (1+ position)) 16)
          (ash (aref octets (+ position 2)) 8)
          (aref octets (+ position 3))))

(defun %h2-put-u24 (octets position value)
  (setf (aref octets position) (ldb (byte 8 16) value)
        (aref octets (1+ position)) (ldb (byte 8 8) value)
        (aref octets (+ position 2)) (ldb (byte 8 0) value))
  octets)

(defun %h2-put-u16 (octets position value)
  (setf (aref octets position) (ldb (byte 8 8) value)
        (aref octets (1+ position)) (ldb (byte 8 0) value))
  octets)

(defun %h2-put-u32 (octets position value)
  (setf (aref octets position) (ldb (byte 8 24) value)
        (aref octets (1+ position)) (ldb (byte 8 16) value)
        (aref octets (+ position 2)) (ldb (byte 8 8) value)
        (aref octets (+ position 3)) (ldb (byte 8 0) value))
  octets)

(defun %h2-frame-wire (type flags stream-id payload)
  (let ((payload (http-kit::%copy-octets payload)))
    (unless (and (integerp type) (<= 0 type 255)
                 (integerp flags) (<= 0 flags 255)
                 (integerp stream-id) (<= 0 stream-id #x7fffffff)
                 (<= (length payload) #xffffff))
      (error 'http-protocol-error
             :message "An HTTP/2 frame has an invalid header."
             :operation :http2-write
             :detail (list type flags stream-id (length payload))))
    (let ((wire (make-array (+ 9 (length payload))
                            :element-type '(unsigned-byte 8))))
      (%h2-put-u24 wire 0 (length payload))
      (setf (aref wire 3) type
            (aref wire 4) flags)
      (%h2-put-u32 wire 5 stream-id)
      (replace wire payload :start1 9)
      wire)))

(defun %h2-read-frame
    (reader max-frame-size deadline clock-function
     &key deadline-function on-first-byte on-header slice-p)
  (when slice-p
    (return-from %h2-read-frame
      (let* ((now (funcall clock-function))
       (active-deadline
         (if deadline-function
             (funcall deadline-function)
             deadline))
       (slice-deadline
         (if active-deadline
             (min active-deadline (+ now 0.1d0))
             (+ now 0.1d0)))
       (header (progn (http-kit::%check-deadline active-deadline clock-function :read) (%h2-reader-frame-header reader)))
       (header-position (%h2-reader-frame-header-position reader))
       (metadata (%h2-reader-frame-metadata reader))
       (payload (%h2-reader-frame-payload reader))
       (payload-position (%h2-reader-frame-payload-position reader)))
  (labels ((fill-buffer (buffer position count)
             (if (>= position count)
                 (values position :data)
                 (multiple-value-bind (octets status eof-p)
                     (%h2-reader-read-available
                          (%h2-reader-stream reader)
                          (- count position)
                          (max 0 (- slice-deadline
                                    (funcall clock-function))))
                   (when (plusp (length octets))
                     (replace buffer octets :start1 position)
                     (incf position (length octets)))
                   (when eof-p
                     (setf (%h2-reader-stream-eof-p reader) t))
                   (values position status)))))
    (when (< header-position 9)
      (let ((first-byte-pending-p
              (and on-first-byte (zerop header-position))))
        (multiple-value-bind (new-position status)
            (fill-buffer header header-position 9)
          (declare (ignore status))
          (setf header-position new-position
                (%h2-reader-frame-header-position reader) new-position)
          (when (and first-byte-pending-p (plusp new-position))
            (funcall on-first-byte))))
      (when (< header-position 9)
        (if (%h2-reader-stream-eof-p reader)
            (if (zerop header-position)
                (return-from %h2-read-frame :eof)
                (error 'http-protocol-error
                       :message "The HTTP/2 input ended before a complete frame header was read."
                       :operation :http2-read
                       :detail 9))
            (return-from %h2-read-frame :would-block)))
      (let* ((length (%h2-u24 header 0))
             (type (aref header 3))
             (flags (aref header 4))
             (stream-word (%h2-u32 header 5)))
        (when (> length max-frame-size)
          (error 'http-protocol-error
                 :message "The HTTP/2 frame exceeds this transport's configured receive maximum."
                 :operation :http2-read
                 :detail (list length max-frame-size)))
        (when (/= 0 (logand stream-word #x80000000))
          (error 'http-protocol-error
                 :message "The reserved HTTP/2 stream identifier bit is set."
                 :operation :http2-read
                 :detail stream-word))
        (setf metadata (list length type flags stream-word)
              payload (make-array length
                                  :element-type (quote (unsigned-byte 8)))
              payload-position 0
              (%h2-reader-frame-metadata reader) metadata
              (%h2-reader-frame-payload reader) payload)
        (when on-header
          (apply on-header metadata))))
    (setf payload-position
          (fill-buffer payload payload-position (length payload))
          (%h2-reader-frame-payload-position reader) payload-position)
    (when (< payload-position (length payload))
      (if (%h2-reader-stream-eof-p reader)
          (error 'http-protocol-error
                 :message "The HTTP/2 input ended before a complete frame payload was read."
                 :operation :http2-read
                 :detail (length payload))
          (return-from %h2-read-frame :would-block)))
    (destructuring-bind (length type flags stream-id) metadata
      (prog1
          (%make-h2-frame :length length
                          :type type
                          :flags flags
                          :stream-id stream-id
                          :payload payload)
        (setf (%h2-reader-frame-header-position reader) 0
              (%h2-reader-frame-metadata reader) nil
              (%h2-reader-frame-payload reader) nil
              (%h2-reader-frame-payload-position reader) 0
              (%h2-reader-stream-eof-p reader) nil)))))))
  (unless slice-p
    (return-from %h2-read-frame
      (if (and (%h2-reader-stream reader)
               (%h2-reader-stateful-input-p (%h2-reader-stream reader)))
          (loop for frame = (%h2-read-frame
                             reader max-frame-size deadline clock-function
                             :deadline-function deadline-function
                             :on-first-byte on-first-byte
                             :on-header on-header
                             :slice-p t)
                unless (eq frame :would-block)
                  return frame)
        (let* ((clock-function
                 (or clock-function
                     (function http-kit::%monotonic-time)))
               (header
                 (%h2-reader-read
                  reader 9 deadline clock-function
                  :allow-eof t
                  :deadline-function deadline-function
                  :on-first-byte on-first-byte)))
          (when (eq header :eof)
            (return-from %h2-read-frame :eof))
          (let* ((length (%h2-u24 header 0))
                 (type (aref header 3))
                 (flags (aref header 4))
                 (stream-word (%h2-u32 header 5))
                 (metadata (list length type flags stream-word)))
            (when (> length max-frame-size)
              (error 'http-protocol-error
                     :message "The HTTP/2 frame exceeds this transport's configured receive maximum."
                     :operation :http2-read
                     :detail (list length max-frame-size)))
            (when (/= 0 (logand stream-word #x80000000))
              (error 'http-protocol-error
                     :message "The reserved HTTP/2 stream identifier bit is set."
                     :operation :http2-read
                     :detail stream-word))
            (when on-header
              (apply on-header metadata))
            (%make-h2-frame
             :length length
             :type type
             :flags flags
             :stream-id stream-word
             :payload
             (%h2-reader-read
              reader length deadline clock-function
              :deadline-function deadline-function))))))))
