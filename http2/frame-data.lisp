(in-package #:http-kit/http2)

(defparameter +http2-connection-preface+
  (http-kit::%join-octets
   (http-kit::%string-octets "PRI * HTTP/2.0")
   (make-array 10
               :element-type '(unsigned-byte 8)
               :initial-contents '(13 10 13 10 83 77 13 10 13 10))))

(defconstant +http2-data-type+ 0)
(defconstant +http2-headers-type+ 1)
(defconstant +http2-priority-type+ 2)
(defconstant +http2-rst-stream-type+ 3)
(defconstant +http2-settings-type+ 4)
(defconstant +http2-push-promise-type+ 5)
(defconstant +http2-ping-type+ 6)
(defconstant +http2-goaway-type+ 7)
(defconstant +http2-window-update-type+ 8)
(defconstant +http2-continuation-type+ 9)
(defconstant +http2-priority-update-type+ #x10)

(defconstant +http2-end-stream-flag+ #x1)
(defconstant +http2-end-headers-flag+ #x4)
(defconstant +http2-padded-flag+ #x8)
(defconstant +http2-priority-flag+ #x20)
(defconstant +http2-ack-flag+ #x1)
(defconstant +http2-default-max-frame-size+ 16384)
(defconstant +http2-default-window-size+ 65535)

;; RFC 9113, section 7.  Keep these values next to the frame constants so
;; every HTTP/2 transport path uses the same wire error-code vocabulary.
(defconstant +http2-no-error+ 0)
(defconstant +http2-protocol-error+ 1)
(defconstant +http2-internal-error+ 2)
(defconstant +http2-flow-control-error+ 3)
(defconstant +http2-refused-stream+ 7)
(defconstant +http2-cancel+ 8)
(defconstant +http2-compression-error+ 9)
(defconstant +http2-connect-error+ 10)
(defconstant +http2-enhance-your-calm+ 11)
(defconstant +http2-inadequate-security+ 12)
(defconstant +http2-http-1-1-required+ 13)

(defun %h2-priority-field-value-p (octets &optional (start 0))
  (loop for position from start below (length octets)
        for octet = (aref octets position)
        always (or (= octet #x09)
                   (<= #x20 octet #x7e))))
