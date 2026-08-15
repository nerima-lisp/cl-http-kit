(in-package #:http-kit/test-core)

(deftest uri-authority-and-component-boundaries
  (dolist (authority '(""
                       "user@host"
                       "[::1"
                       "[::1]x"
                       "[::1]:"
                       "[::1]:not-a-port"
                       "[not-an-ip]"
                       "[2001:db8:::1]"
                       "[2001:db8::1::2]"
                       "[2001:db8::12345]"
                       "[::ffff:192.0.2.256]"
                       "[::ffff:192.00.2.1]"
                       "[fe80::1%25en0]"
                       "[v.example]"
                       "[v1.]"
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
  (dolist (character '(#\\ #\[ #\] #\^ #\| #\{ #\}))
    (signals http-invalid-uri
      (make-http-uri
       :authority "example.com"
       :path (concatenate 'string "/path" (string character))))
    (signals http-invalid-uri
      (make-http-uri
       :authority "example.com"
       :query (concatenate 'string "value" (string character)))))
  (let ((uri (make-http-uri
              :authority "example.com"
              :path "/a:@!$&'()*+,;=%20"
              :query "x=/?:@!$&'()*+,;=%20")))
    (ensure-equal "/a:@!$&'()*+,;=%20" (http-uri-path uri))
    (ensure-equal "x=/?:@!$&'()*+,;=%20" (http-uri-query uri)))
  (let* ((authority (copy-seq "example.com"))
         (path (copy-seq "/original"))
         (query (copy-seq "key=original"))
         (uri (make-http-uri :authority authority :path path :query query)))
    (setf (char authority 0) #\X
          (char path 1) #\X
          (char query 0) #\X)
    (ensure-equal "example.com" (http-uri-authority uri))
    (ensure-equal "/original" (http-uri-path uri))
    (ensure-equal "key=original" (http-uri-query uri))
    (let ((returned-authority (http-uri-authority uri))
          (returned-host (http-uri-host uri))
          (returned-path (http-uri-path uri))
          (returned-query (http-uri-query uri)))
      (setf (char returned-authority 0) #\X
            (char returned-host 0) #\X
            (char returned-path 1) #\X
            (char returned-query 0) #\X)
      (ensure-equal "example.com" (http-uri-authority uri))
      (ensure-equal "example.com" (http-uri-host uri))
      (ensure-equal "/original" (http-uri-path uri))
      (ensure-equal "key=original" (http-uri-query uri))))
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
      (ensure-true (not (search "?<redacted>" printed)))))
  (dolist (authority '("[::]"
                       "[::ffff:192.0.2.128]"
                       "[2001:db8:0:1:1:1:1:1]"
                       "[v1.example:token]"))
    (ensure-equal authority
                  (http-uri-authority (make-http-uri :authority authority))
                  "valid IP-literal authority")))

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
  (let ((uri (parse-http-uri "http://example.com/path?secret=value")))
    (ensure-printed=
     "#<HTTP-URI http://example.com/path?<redacted>>"
     uri)
    (ensure-true
     (not (search "secret=value" (princ-to-string uri)))
     "URI printing omits query contents")))
