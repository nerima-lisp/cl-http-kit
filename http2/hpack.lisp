(in-package #:http-kit/http2)

(defun %hpack-read-integer (octets position prefix-bits)
  (unless (and (<= 1 prefix-bits 8) (< position (length octets)))
    (error 'http-protocol-error
           :message "An HPACK integer is truncated."
           :operation :hpack
           :detail position))
  (let* ((mask (1- (ash 1 prefix-bits)))
         (first (aref octets position))
         (value (logand first mask))
         (position (1+ position)))
    (if (< value mask)
        (values value position)
        (let ((shift 0))
          (loop
            (when (>= position (length octets))
              (error 'http-protocol-error
                     :message "An HPACK integer continuation is truncated."
                     :operation :hpack
                     :detail position))
            (let ((octet (aref octets position)))
              (incf position)
              (let ((increment (ash (logand octet #x7f) shift)))
                (when (> increment #xffffffff)
                  (error 'http-protocol-error
                         :message "An HPACK integer is too large."
                         :operation :hpack
                         :detail value))
                (incf value increment))
              (when (> value #xffffffff)
                (error 'http-protocol-error
                       :message "An HPACK integer is too large."
                       :operation :hpack
                       :detail value))
              (if (zerop (logand octet #x80))
                  (return (values value position))
                  (progn
                    (incf shift 7)
                    (when (> shift 28)
                      (error 'http-protocol-error
                             :message "An HPACK integer is too large."
                             :operation :hpack
                             :detail value))))))))))

(defun %hpack-encode-integer (value prefix-bits prefix)
  (unless (and (integerp value) (>= value 0)
               (<= 1 prefix-bits 8)
               (integerp prefix) (<= 0 prefix #xff))
    (error 'http-protocol-error
           :message "Cannot encode an invalid HPACK integer."
           :operation :hpack
           :detail value))
  (let* ((mask (1- (ash 1 prefix-bits)))
         (prefix (logand prefix #xff)))
    (if (< value mask)
        (let ((result (make-array 1 :element-type '(unsigned-byte 8))))
          (setf (aref result 0) (logior prefix value))
          result)
        (let ((result (make-array 1 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 1)))
          (setf (aref result 0) (logior prefix mask))
          (decf value mask)
          (loop while (>= value 128)
                do (vector-push-extend (logior #x80 (logand value #x7f)) result)
                   (setf value (ash value -7)))
          (vector-push-extend value result)
          (let ((copy (make-array (length result)
                                  :element-type '(unsigned-byte 8))))
            (replace copy result)
            copy)))))

