(in-package #:http-kit)

(defun %string-octets (string &key (context :text))
  (unless (stringp string)
    (error 'http-protocol-error
           :message "Expected a string."
           :operation context
           :detail (type-of string)))
  (coerce
   (map 'vector
        (lambda (character)
          (let ((code (char-code character)))
            (unless (<= code #xff)
              (error 'http-protocol-error
                     :message "A string contains a character outside the octet range."
                     :operation context
                     :detail code))
            code))
        string)
   '(simple-array (unsigned-byte 8) (*))))

(defun %octets-string (octets)
  (unless (typep octets '(or vector list))
    (funcall #'error 'http-protocol-error
             :message "Expected an octet sequence."
             :operation :text
             :detail (type-of octets)))
  (let ((result (make-string (length octets))))
    (loop for index below (length octets)
          for octet = (elt octets index)
          do (unless (and (integerp octet) (<= 0 octet #xff))
               (funcall #'error 'http-protocol-error
                        :message "An octet sequence contains an invalid value."
                        :operation :text
                        :detail octet))
             (setf (aref result index) (code-char octet)))
    result))
