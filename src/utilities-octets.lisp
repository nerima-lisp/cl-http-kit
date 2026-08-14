(in-package #:http-kit)

(defun %empty-octets ()
  (make-array 0 :element-type '(unsigned-byte 8)))
