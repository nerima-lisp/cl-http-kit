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

(defconstant +http2-end-stream-flag+ #x1)
(defconstant +http2-end-headers-flag+ #x4)
(defconstant +http2-padded-flag+ #x8)
(defconstant +http2-priority-flag+ #x20)
(defconstant +http2-ack-flag+ #x1)
(defconstant +http2-default-max-frame-size+ 16384)
(defconstant +http2-default-window-size+ 65535)
