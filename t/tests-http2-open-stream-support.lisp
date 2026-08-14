(in-package #:http-kit/test)

#+sbcl
(progn
  (defclass binary-session-stream
      (sb-gray:fundamental-binary-input-stream
       sb-gray:fundamental-binary-output-stream)
    ((input
       :initarg :input
       :reader binary-session-input)
     (input-position
       :initform 0
       :accessor binary-session-input-position)
     (output
       :initform (make-array 0
                             :element-type '(unsigned-byte 8)
                             :adjustable t
                             :fill-pointer 0)
       :reader binary-session-output)))

  (defmethod sb-gray:stream-read-sequence
      ((stream binary-session-stream) sequence &optional (start 0) end)
    (let* ((end (or end (length sequence)))
           (input (binary-session-input stream))
           (position (binary-session-input-position stream))
           (count (min (- end start) (- (length input) position))))
      (when (plusp count)
        (replace sequence input
                 :start1 start
                 :end1 (+ start count)
                 :start2 position
                 :end2 (+ position count))
        (incf (binary-session-input-position stream) count))
      (+ start count)))

  (defmethod sb-gray:stream-write-sequence
      ((stream binary-session-stream) sequence &optional (start 0) end)
    (let ((end (or end (length sequence)))
          (output (binary-session-output stream)))
      (loop for index from start below end
            do (vector-push-extend (aref sequence index) output)))
    sequence)

  (defmethod sb-gray:stream-finish-output ((stream binary-session-stream))
    (declare (ignore stream))
    nil)

  (defun h2-output-frames (stream)
    (let ((reader
            (http-kit/http2::%h2-reader-for
             (subseq (binary-session-output stream)
                     (length (h2-preface)))))
          (frames nil))
      (loop for frame = (http-kit/http2::%h2-read-frame
                         reader 16384 nil nil)
            until (eq frame :eof)
            do (push frame frames))
      (nreverse frames)))

  (defun h2-output-data-summary (stream)
    (let ((data-body
            (make-array 0
                        :element-type '(unsigned-byte 8)
                        :adjustable t
                        :fill-pointer 0))
          (data-frame-count 0)
          (settings-ack-count 0))
      (dolist (frame (h2-output-frames stream))
        (cond
          ((= 0 (http-kit/http2::%h2-frame-type frame))
           (incf data-frame-count)
           (loop for octet across (http-kit/http2::%h2-frame-payload frame)
                 do (vector-push-extend octet data-body)))
          ((and (= 4 (http-kit/http2::%h2-frame-type frame))
                (= 1 (http-kit/http2::%h2-frame-flags frame)))
           (incf settings-ack-count))))
      (values data-body data-frame-count settings-ack-count))))

  (defun make-octet-body-producer (body &key on-maximum-size)
    (let ((position 0)
          (calls 0))
      (values
       (lambda (maximum-size)
         (incf calls)
         (when on-maximum-size
           (funcall on-maximum-size maximum-size))
         (when (< position (length body))
           (let* ((size (min maximum-size
                             (- (length body) position)))
                  (chunk (make-array size
                                     :element-type '(unsigned-byte 8))))
             (replace chunk body
                      :start2 position
                      :end2 (+ position size))
             (incf position size)
             chunk)))
       (lambda () position)
       (lambda () calls))))
