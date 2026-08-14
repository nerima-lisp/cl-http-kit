(in-package #:http-kit)

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
