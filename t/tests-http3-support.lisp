(in-package #:http-kit/test)

(defstruct http3-test-stream
  kind
  writes
  reads
  closed-p)

(defun http3-test-concat-octets (&rest parts)
  (let ((result (make-array (reduce #'+ parts :key #'length :initial-value 0)
                            :element-type '(unsigned-byte 8))))
    (loop with position = 0
          for part in parts
          do (replace result part :start1 position)
             (incf position (length part)))
    result))
