(in-package #:http-kit)

(defun %copy-octets (value &key (allow-list t))
  (cond
    ((and (arrayp value) (= (array-rank value) 1)
          (not (stringp value))
          (loop for item across value
                always (and (integerp item) (<= 0 item 255))))
     (let ((copy (make-array (length value) :element-type '(unsigned-byte 8))))
       (replace copy value)
       copy))
    ((and allow-list (listp value)
          (every (lambda (item) (and (integerp item) (<= 0 item 255))) value))
     (make-array (length value) :element-type '(unsigned-byte 8)
                 :initial-contents value))
    (t
     (error 'http-protocol-error
            :message "HTTP message bodies must be one-dimensional octet vectors."
            :operation :body
            :detail (type-of value)))))
