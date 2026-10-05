(in-package #:http-kit/test-core)

(deftest utilities-conversion-boundaries
  (ensure-conversion-cases http-kit::%string-octets
    ("AB" (octets 65 66)))
  (ensure-signals-cases http-protocol-error
    (http-kit::%string-octets 1)
    (http-kit::%string-octets (string (code-char #x100))))
  (ensure-conversion-cases http-kit::%octets-string
    ((octets 65 66) "AB"))
  (ensure-signals-cases http-protocol-error
    (http-kit::%octets-string 1)
    (http-kit::%octets-string (list 256))
    (http-kit::%octets-string (list :invalid)))
  (ensure-equal 0 (http-kit::%hex-digit #\0))
  (ensure-equal 10 (http-kit::%hex-digit #\A))
  (ensure-equal 15 (http-kit::%hex-digit #\f)))
