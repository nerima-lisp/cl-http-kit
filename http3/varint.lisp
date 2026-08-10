(in-package #:http-kit/http3)

(defparameter +http3-max-varint+ #x3fffffffffffffff)

(defun %http3-octet-vector-p (octets)
  (and (vectorp octets)
       (= 1 (array-rank octets))
       (every (lambda (octet)
                (and (integerp octet) (<= 0 octet 255)))
              octets)))

(defun %http3-varint-error (message &optional detail)
  (error 'http-protocol-error
         :message message
         :operation :http3-varint
         :detail detail))

(defun %http3-make-octets (&rest values)
  (let ((octets (make-array (length values)
                            :element-type '(unsigned-byte 8))))
    (loop for value in values
          for position from 0
          do (setf (aref octets position) value))
    octets))

(defun http3-varint-encode (value)
  "Encode VALUE using the QUIC variable-length integer format."
  (unless (and (integerp value) (<= 0 value +http3-max-varint+))
    (%http3-varint-error
     "HTTP/3 variable-length integers must be non-negative and fit in 62 bits."
     value))
  (cond
    ((<= value #x3f)
     (%http3-make-octets value))
    ((<= value #x3fff)
     (%http3-make-octets
      (logior #x40 (ldb (byte 6 8) value))
      (ldb (byte 8 0) value)))
    ((<= value #x3fffffff)
     (%http3-make-octets
      (logior #x80 (ldb (byte 6 24) value))
      (ldb (byte 8 16) value)
      (ldb (byte 8 8) value)
      (ldb (byte 8 0) value)))
    (t
     (%http3-make-octets
      (logior #xc0 (ldb (byte 6 56) value))
      (ldb (byte 8 48) value)
      (ldb (byte 8 40) value)
      (ldb (byte 8 32) value)
      (ldb (byte 8 24) value)
      (ldb (byte 8 16) value)
      (ldb (byte 8 8) value)
      (ldb (byte 8 0) value)))))

(defun http3-varint-decode (octets &key (position 0) allow-incomplete-p)
  "Decode one QUIC variable-length integer.

Returns VALUE and the position immediately following it.  With
ALLOW-INCOMPLETE-P, an incomplete value returns NIL and POSITION instead of
signalling, which is useful to incremental stream parsers."
  (unless (%http3-octet-vector-p octets)
    (%http3-varint-error "HTTP/3 variable-length integer input must be an octet vector."
                         (type-of octets)))
  (unless (and (integerp position) (<= 0 position (length octets)))
    (%http3-varint-error "HTTP/3 variable-length integer position is outside the input."
                         position))
  (if (= position (length octets))
      (if allow-incomplete-p
          (values nil position)
          (%http3-varint-error "HTTP/3 variable-length integer input is incomplete."))
      (let* ((first (aref octets position))
             (prefix (logand first #xc0))
             (width (case prefix (#x00 1) (#x40 2) (#x80 4) (#xc0 8))))
        (if (> (+ position width) (length octets))
            (if allow-incomplete-p
                (values nil position)
                (%http3-varint-error
                 "HTTP/3 variable-length integer input is incomplete."))
            (values
             (case width
               (1 (logand first #x3f))
               (2 (logior (ash (logand first #x3f) 8)
                          (aref octets (+ position 1))))
               (4 (logior (ash (logand first #x3f) 24)
                          (ash (aref octets (+ position 2)) 8)
                          (ash (aref octets (+ position 1)) 16)
                          (aref octets (+ position 3))))
               (8 (logior (ash (logand first #x3f) 56)
                          (ash (aref octets (+ position 1)) 48)
                          (ash (aref octets (+ position 2)) 40)
                          (ash (aref octets (+ position 3)) 32)
                          (ash (aref octets (+ position 4)) 24)
                          (ash (aref octets (+ position 5)) 16)
                          (ash (aref octets (+ position 6)) 8)
                          (aref octets (+ position 7)))))
             (+ position width))))))
