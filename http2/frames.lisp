(in-package #:http-kit/http2)

(defstruct (%h2-frame (:constructor %make-h2-frame))
  length
  type
  flags
  stream-id
  payload)

(defstruct (%h2-reader (:constructor %make-h2-reader))
  vector
  position
  stream)

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

(defun %h2-reader-read (reader count deadline clock-function &key allow-eof)
  (unless (and (integerp count) (>= count 0))
    (error 'http-protocol-error
           :message "An HTTP/2 read length is invalid."
           :operation :http2-read
           :detail count))
  (let ((result (make-array count :element-type '(unsigned-byte 8)))
        (position 0))
    (loop while (< position count)
          do (http-kit::%check-deadline deadline clock-function :read)
             (if (%h2-reader-stream reader)
                 (let ((end (read-sequence result (%h2-reader-stream reader)
                                            :start position)))
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
                                         (- (length vector) source-position))))
                     (replace result vector :start1 position
                              :start2 source-position
                              :end2 (+ source-position available))
                     (incf position available)
                     (incf (%h2-reader-position reader) available)))))
    result))

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

(defun %h2-read-frame (reader max-frame-size deadline clock-function)
  (let ((header (%h2-reader-read reader 9 deadline clock-function :allow-eof t)))
    (when (eq header :eof)
      (return-from %h2-read-frame :eof))
    (let ((length (%h2-u24 header 0))
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
      (%make-h2-frame :length length
                      :type type
                      :flags flags
                      :stream-id stream-word
                      :payload (%h2-reader-read reader length deadline clock-function)))))
