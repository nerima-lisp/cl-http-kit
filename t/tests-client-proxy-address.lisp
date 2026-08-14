(in-package #:http-kit/test)

#+sbcl
(deftest client-proxy-ipv4-address-boundaries
  (ensure-equal
   (octets 192 0 2 10)
   (http-kit/client::%proxy-ipv4-octets "192.0.2.10"))
  (ensure-equal nil
                (http-kit/client::%proxy-ipv4-octets "192.0.2"))
  (ensure-equal nil
                (http-kit/client::%proxy-ipv4-octets "192.0.2.999"))
  (ensure-equal nil
                (http-kit/client::%proxy-ipv4-octets "192.0..10")))

#+sbcl
(deftest client-proxy-ipv6-address-boundaries
  (ensure-equal
   (octets 32 1 13 184 0 0 0 0 0 0 0 0 0 0 0 1)
   (http-kit/client::%proxy-ipv6-octets "2001:db8::1"))
  (ensure-equal
   (octets 0 0 0 0 0 0 0 0 0 0 255 255 192 0 2 1)
   (http-kit/client::%proxy-ipv6-octets "::ffff:192.0.2.1"))
  (ensure-equal nil
                (http-kit/client::%proxy-ipv6-octets "2001::db8::1"))
  (ensure-equal nil
                (http-kit/client::%proxy-ipv6-octets "[2001:db8::1]")))

#+sbcl
(deftest client-proxy-resolved-address-boundaries
  (ensure-equal
   (octets 203 0 113 7)
   (http-kit/client::%proxy-resolved-address
    "service.example"
    (lambda (host)
      (ensure-equal "service.example" host)
      "203.0.113.7")))
  (signals http-proxy-error
    (http-kit/client::%proxy-resolved-address
     "service.example"
     (lambda (host)
       (declare (ignore host))
       "not-an-ip"))))

#+sbcl
(deftest client-proxy-socks-address-boundaries
  (ensure-equal
   (octets 3 12 101 120 97 109 112 108 101 46 116 101 115 116)
   (http-kit/client::%proxy-builder-vector
    (http-kit/client::%proxy-socks-address "example.test" t nil)))
  (ensure-equal
   (octets 1 192 0 2 1)
   (http-kit/client::%proxy-builder-vector
    (http-kit/client::%proxy-socks-address "192.0.2.1" nil nil)))
  (ensure-equal
   (octets 4 32 1 13 184 0 0 0 0 0 0 0 0 0 0 0 9)
   (http-kit/client::%proxy-builder-vector
    (http-kit/client::%proxy-socks-address "2001:db8::9" nil nil)))
  (signals http-proxy-error
    (http-kit/client::%proxy-socks-address "service.example" nil nil))
  (signals http-proxy-error
    (http-kit/client::%proxy-socks-address
     ""
     t
     nil)))
