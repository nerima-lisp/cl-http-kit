(in-package #:http-kit/test)

(deftest uri-authority-and-component-boundaries
  (dolist (authority '(""
                       "user@host"
                       "[::1"
                       "[::1]x"
                       "[::1]:"
                       "[::1]:not-a-port"
                       "host:not-a-port"
                       "host:65536"
                       "1:2:3"
                       ":80"
                       "host/"
                       "host?query"
                       "host#fragment"))
    (signals http-invalid-uri
      (make-http-uri :authority authority)))
  (signals http-invalid-uri
    (make-http-uri :scheme "ftp" :authority "example.com"))
  (signals http-invalid-uri
    (make-http-uri :authority 42))
  (signals http-invalid-uri
    (make-http-uri :authority "example.com" :path ""))
  (signals http-invalid-uri
    (make-http-uri :authority "example.com" :path "relative"))
  (signals http-invalid-uri
    (make-http-uri :authority "example.com" :path "/#fragment"))
  (signals http-invalid-uri
    (make-http-uri :authority "example.com" :path "/?query"))
  (signals http-invalid-uri
    (make-http-uri :authority "example.com" :query 42))
  (signals http-invalid-uri
    (make-http-uri :authority "example.com" :query "x#fragment"))
  (let ((uri (make-http-uri :authority "example.com")))
    (ensure-equal "http" (http-uri-scheme uri) "default URI scheme")
    (ensure-equal "/" (http-uri-path uri) "default URI path"))
  (ensure-true (http-kit::%hex-character-p #\0))
  (ensure-true (http-kit::%hex-character-p #\A))
  (ensure-true (http-kit::%hex-character-p #\a))
  (ensure-true (not (http-kit::%hex-character-p #\/)))
  (ensure-true (not (http-kit::%hex-character-p (code-char #x40))))
  (ensure-true (not (http-kit::%hex-character-p (code-char #x60))))
  (let ((uri (make-http-uri :scheme "HTTPS"
                            :authority "[2001:DB8::1]:443")))
    (ensure-equal "https" (http-uri-scheme uri) "normalized URI scheme")
    (ensure-equal "2001:db8::1" (http-uri-host uri) "normalized IPv6 host")
    (ensure-equal "[2001:db8::1]:443"
                  (http-uri-authority uri)
                  "formatted IPv6 authority")
    (ensure-equal "https://[2001:db8::1]:443/"
                  (http-uri-string uri)
                  "URI without query")
    (let ((printed (with-output-to-string (stream)
                     (write uri :stream stream :escape nil))))
      (ensure-true (not (search "?<redacted>" printed))))))

(deftest uri-parser-and-printing-boundaries
  (signals http-invalid-uri
    (parse-http-uri 42))
  (signals http-invalid-uri
    (parse-http-uri "http/example"))
  (signals http-invalid-uri
    (parse-http-uri "http:///path"))
  (let ((uri (parse-http-uri "http://example.com")))
    (ensure-equal "/" (http-uri-path uri) "default URI path")
    (ensure-equal "http://example.com/"
                  (http-uri-string uri)
                  "default path round trip"))
  (let* ((uri (parse-http-uri "http://example.com/path?secret=value"))
         (printed (with-output-to-string (stream)
                    (write uri :stream stream :escape nil))))
    (ensure-true (search "?<redacted>" printed)
                 "URI printing redacts query")
    (ensure-true (not (search "secret=value" printed))
                 "URI printing omits query contents")))
