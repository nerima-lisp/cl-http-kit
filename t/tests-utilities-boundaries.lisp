(in-package #:http-kit/test)

(deftest utilities-conversion-boundaries
  (signals http-protocol-error
    (http-kit::%string-octets 1))
  (signals http-protocol-error
    (http-kit::%string-octets (string (code-char #x100))))
  (signals http-protocol-error
    (http-kit::%octets-string 1))
  (signals http-protocol-error
    (http-kit::%octets-string (list 256)))
  (ensure-equal 0 (http-kit::%hex-digit #\0))
  (ensure-equal 10 (http-kit::%hex-digit #\A))
  (ensure-equal 15 (http-kit::%hex-digit #\f)))
