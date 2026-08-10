(in-package #:http-kit/test)

(defmacro deftest (name &body body)
  `(it ,(string-downcase (symbol-name name)) ,@body))

(defmacro ensure-true (condition &optional control &rest arguments)
  (declare (ignore control arguments))
  `(expect ,condition))

(defmacro ensure-equal (expected actual &optional description)
  (declare (ignore description))
  `(expect ,actual :to-equalp ,expected)) ; paredit:ignore macro-parameter-reordering -- cl-weave EXPECT takes the actual expression before the expected matcher value.

(defun octets (&rest values)
  (let ((result (make-array (length values)
                            :element-type '(unsigned-byte 8))))
    (loop for value in values
          for index from 0
          do (ensure-true (and (integerp value) (<= 0 value #xff))
                          "Test octet is invalid: ~S." value)
             (setf (aref result index) value))
    result))

(defun ascii (string)
  (let ((result (make-array 0
                            :element-type '(unsigned-byte 8)
                            :adjustable t
                            :fill-pointer 0)))
    (loop with index = 0
          while (< index (length string))
          do (if (and (<= (+ index 6) (length string))
                      (string= "|CRLF|" string
                               :start1 0
                               :end1 6
                               :start2 index
                               :end2 (+ index 6)))
                 (progn
                   (vector-push-extend 13 result)
                   (vector-push-extend 10 result)
                   (incf index 6))
                 (progn
                   (vector-push-extend
                    (char-code (char string index))
                    result)
                   (incf index))))
    (let ((copy (make-array (length result)
                            :element-type '(unsigned-byte 8))))
      (replace copy result)
      copy)))

(defun concatenate-octets (&rest vectors)
  (let ((result (make-array (reduce #'+ vectors :key #'length :initial-value 0)
                            :element-type '(unsigned-byte 8)))
        (position 0))
    (dolist (vector vectors result)
      (replace result vector :start1 position)
      (incf position (length vector)))))

(defun octets-as-string (vector)
  (map 'string #'code-char vector))

(defun h2-frame (type flags stream-id payload)
  (let* ((length (length payload))
         (result (make-array (+ 9 length)
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (setf (aref result 0) (ldb (byte 8 16) length)
          (aref result 1) (ldb (byte 8 8) length)
          (aref result 2) (ldb (byte 8 0) length)
          (aref result 3) type
          (aref result 4) flags
          (aref result 5) (ldb (byte 8 24) stream-id)
          (aref result 6) (ldb (byte 8 16) stream-id)
          (aref result 7) (ldb (byte 8 8) stream-id)
          (aref result 8) (ldb (byte 8 0) stream-id))
    (replace result payload :start1 9)
    result))

(defun h2-frame-object (type flags stream-id payload)
  (http-kit/http2::%make-h2-frame
   :length (length payload)
   :type type
   :flags flags
   :stream-id stream-id
   :payload payload))

(defun h2-reader-from-frames (&rest frames)
  (http-kit/http2::%h2-reader-for
   (apply #'concatenate-octets frames)))

(defun h2-header-block (&rest fields)
  (http-kit/http2::%hpack-encode-block fields))

(defun h2-response-wire (body)
  (concatenate-octets
   (h2-frame 4 0 0 (octets))
   (h2-frame 1 (if (zerop (length body)) 5 4) 1 (octets #x88))
   (if (zerop (length body))
       (octets)
       (h2-frame 0 1 1 body))))

(defun h2-preface ()
  (ascii "PRI * HTTP/2.0|CRLF||CRLF|SM|CRLF||CRLF|"))
