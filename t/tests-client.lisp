(in-package #:http-kit/test)

(defun client-test-response (status &key headers body protocol-version)
  (make-http-response :status status
                      :protocol-version (or protocol-version "HTTP/1.1")
                      :headers headers
                      :body (or body (octets))))

(deftest client-content-digest
  (let ((content (ascii "hello"))
        (field "sha-256=:LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=:"))
    (ensure-equal field (http-content-digest content))
    (ensure-true (http-content-digest-valid-p content field))
    (ensure-true
     (http-content-digest-valid-p
      content
      (list "example=:AA==:"
            "sha-256=:LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=:")))
    (ensure-true
     (http-content-digest-valid-p
      content (concatenate 'string field ";source=network;verified")))
    (ensure-false (http-content-digest-valid-p (ascii "Hello") field))
    (ensure-false
     (http-content-digest-valid-p content "example=:AA==:"))
    (signals http-protocol-error
      (http-content-digest-valid-p content "sha-256=:not base64=:"))
    (signals http-protocol-error
      (http-content-digest-valid-p content (concatenate 'string field ";bad=")))
    (signals http-protocol-error
      (http-content-digest-valid-p
       content (concatenate 'string field ";a=1;a=2")))
    (signals http-protocol-error
      (http-content-digest-valid-p
       content (concatenate 'string field ";n=1234567890123456")))
    (signals http-protocol-error
      (http-content-digest-valid-p
       content (concatenate 'string field ";date=@0")))
    (signals http-protocol-error
      (http-content-digest-valid-p content (concatenate 'string field ",")))
    (signals http-protocol-error
      (http-content-digest-valid-p
       content (list field field)))
    (signals http-protocol-error
      (http-content-digest "hello"))
    (signals http-protocol-error
      (http-content-digest content :algorithm :md5))))

(deftest client-form-utf8-scalar-boundaries
  (dolist (case '((#x00 #x00) (#x7f #x7f) (#x80 #xc2 #x80)
                  (#x7ff #xdf #xbf) (#x800 #xe0 #xa0 #x80)
                  (#xd7ff #xed #x9f #xbf) (#xe000 #xee #x80 #x80)
                  (#xffff #xef #xbf #xbf) (#x10000 #xf0 #x90 #x80 #x80)
                  (#x10ffff #xf4 #x8f #xbf #xbf)))
    (ensure-equal (apply #'octets (rest case))
                  (http-utf8-octets (string (code-char (first case))))))
  (ensure-equal (octets) (http-utf8-octets ""))
  (signals http-protocol-error (http-utf8-octets '(#\a)))
  (dolist (code '(#xd800 #xdfff #x110000))
    (signals http-protocol-error
      (http-kit/client::%push-utf8-code-point
       code (make-array 0 :element-type '(unsigned-byte 8)
                          :adjustable t :fill-pointer 0)))))

(deftest client-form-percent-encoding-boundaries
  (ensure-equal "AZaz09-._~%20%2B%2F%25"
                (http-percent-encode "AZaz09-._~ +/%"))
  (ensure-equal "%2D%2E%5F%7E%20%2B"
                (http-percent-encode "-._~ +" :safe nil))
  (ensure-equal "a+b%2Bc"
                (http-percent-encode "a b+c" :space-as-plus-p t))
  (ensure-equal "a/b%20c"
                (http-percent-encode "a/b c" :safe "/"))
  (ensure-equal "%C3%A9%E3%81%82%F0%9F%98%80"
                (http-percent-encode "éあ😀"))
  (ensure-equal "%C3%A9"
                (http-percent-encode "é" :safe "Ã©"))
  (ensure-equal "" (http-percent-encode "" :safe ""))
  (signals http-protocol-error (http-percent-encode 42))
  (signals http-protocol-error (http-percent-encode "a" :safe 42)))

(deftest client-form-field-boundaries
  (ensure-equal "" (http-form-urlencode nil))
  (ensure-equal "a=one&b=&c=%C3%A9&d=-._*%7E"
                (http-form-urlencode '(("a" . "one") ("b" nil)
                                       ("c" "é") ("d" "-._*~"))))
  (ensure-equal (ascii "q=a+b%26c%3Dd")
                (http-form-urlencoded-octets '(("q" "a b&c=d"))))
  (signals http-protocol-error (http-form-urlencode #("a" "b")))
  (dolist (field '(nil "a" (42 "b") ("a") ("a" "b" "c")
                   ("a" . 42) ("a" 42)))
    (signals http-protocol-error (http-form-urlencode (list field)))))

(deftest client-uri-encoding-and-authentication
  (let ((resolved (resolve-http-uri
                   "http://example.test/a/b"
                   "../c?x=1#fragment")))
    (ensure-equal "http://example.test/c?x=1"
                  (http-uri-string resolved))
    (ensure-true (http-same-origin-p resolved "http://example.test:80/")))
  (ensure-equal "http://example.test/a//c"
                (http-uri-string
                 (resolve-http-uri "http://example.test/root"
                                   "/a//b/../c")))
  (ensure-equal "http://example.test/a/"
                (http-uri-string
                 (resolve-http-uri "http://example.test/root"
                                   "/a/b/..")))
  (ensure-equal "http://example.test/"
                (http-uri-string
                 (resolve-http-uri "http://example.test/root"
                                   "/../../")))
  (ensure-equal "q=a+b&x=1%2B2"
                (http-form-urlencode '(("q" "a b") ("x" "1+2"))))
  (ensure-equal "Basic dXNlcjpwYXNz"
                (http-basic-authorization "user" "pass"))
  (ensure-equal "Basic dXNlcjpwYTpzcw=="
                (http-basic-authorization "user" "pa:ss"))
  (signals http-protocol-error
    (http-basic-authorization "user:name" "pass"))
  (handler-case
      (http-basic-authorization "secret:user" "pass")
    (http-protocol-error (condition)
      (ensure-equal :username
                    (http-protocol-error-detail condition))))
  (signals http-protocol-error
    (http-basic-authorization (format nil "user~Cname" (code-char 1))
                              "pass"))
  (signals http-protocol-error
    (http-basic-authorization "user"
                              (format nil "pass~Cword" (code-char 127))))
  (handler-case
      (http-basic-authorization "user"
                                (format nil "secret~Cpassword" (code-char 127)))
    (http-protocol-error (condition)
      (ensure-equal :password
                    (http-protocol-error-detail condition))))
  (ensure-equal "Bearer token"
                (http-bearer-authorization "token"))
  (ensure-equal "Bearer mF_9.B5f-4.1JqM+/=="
                (http-bearer-authorization "mF_9.B5f-4.1JqM+/=="))
  (signals http-protocol-error
    (http-bearer-authorization ""))
  (signals http-protocol-error
    (http-bearer-authorization "token value"))
  (signals http-protocol-error
    (http-bearer-authorization "token=value"))
  (signals http-protocol-error
    (http-bearer-authorization "töken"))
  (handler-case
      (http-bearer-authorization "secret token")
    (http-protocol-error (condition)
      (ensure-equal :token
                    (http-protocol-error-detail condition))))
  (let ((challenges
          (http-parse-authentication-challenges
           '("Digest realm=\"api, internal\", qop=\"auth,auth-int\", nonce=\"n\\\"1\", Basic realm=\"fallback\""
             "Bearer mF_9.B5f-4.1JqM+/=="))))
    (ensure-equal 3 (length challenges))
    (let ((digest (first challenges))
          (basic (second challenges))
          (bearer (third challenges)))
      (ensure-equal "Digest"
                    (http-authentication-challenge-scheme digest))
      (ensure-equal "api, internal"
                    (http-authentication-challenge-parameter digest "realm"))
      (ensure-equal "auth,auth-int"
                    (http-authentication-challenge-parameter digest "QOP"))
      (ensure-equal "n\"1"
                    (http-authentication-challenge-parameter digest "nonce"))
      (ensure-equal "fallback"
                    (http-authentication-challenge-parameter basic "realm"))
      (ensure-equal "mF_9.B5f-4.1JqM+/=="
                    (http-authentication-challenge-token68 bearer))))
  (ensure-equal nil
                (http-parse-authentication-challenges
                 "Digest realm=\"one\", REALM=\"two\""))
  (ensure-equal nil
                (http-parse-authentication-challenges
                 (format nil "Basic realm=\"bad~Cvalue\"" (code-char 10))))
  (signals http-protocol-error
    (http-parse-authentication-challenges '("Basic" 42))))

(deftest client-digest-authorization-rfc7616-vectors
  (ensure-equal
   "c672b8d1ef56ed28ab87c3622c5114069bdd3ad7b8f9737498d0c01ecef0967a"
   (http-kit/client::%digest-hash-string :sha-512-256 ""))
  (ensure-equal
   "53048e2681941ef99b2e29b76b4c7dabe4c2d0c634fc6d46e0e2f13107e7af23"
   (http-kit/client::%digest-hash-string :sha-512-256 "abc"))
  (flet ((challenge (algorithm)
           (first
            (http-parse-authentication-challenges
             (format nil
                     "Digest realm=\"http-auth@example.org\", qop=\"auth, auth-int\", algorithm=~A, nonce=\"7ypf/xlj9XXwfDPEoM4URrv/xwf94BcCAzFZH4GiTo0v\", opaque=\"FQhe/qaU925kfnzjCev0ciny7QMkPqMAFRtzCUYo5tdS\""
                     algorithm)))))
    (let ((cnonce "f2/wE4q74E6zIJEtWaHKaf5wv/H5QzzpXusqGemxURZJ"))
      (ensure-equal
       "Digest username=\"Mufasa\", realm=\"http-auth@example.org\", uri=\"/dir/index.html\", algorithm=SHA-256, nonce=\"7ypf/xlj9XXwfDPEoM4URrv/xwf94BcCAzFZH4GiTo0v\", nc=00000001, cnonce=\"f2/wE4q74E6zIJEtWaHKaf5wv/H5QzzpXusqGemxURZJ\", qop=auth, response=\"753927fa0e85d155564e2e272a28d1802ca10daf4496794697cf8db5856cb6c1\", opaque=\"FQhe/qaU925kfnzjCev0ciny7QMkPqMAFRtzCUYo5tdS\""
       (http-digest-authorization
        (challenge "SHA-256") "GET" "/dir/index.html"
        "Mufasa" "Circle of Life" cnonce))
      (ensure-equal
       "Digest username=\"Mufasa\", realm=\"http-auth@example.org\", uri=\"/dir/index.html\", algorithm=MD5, nonce=\"7ypf/xlj9XXwfDPEoM4URrv/xwf94BcCAzFZH4GiTo0v\", nc=00000001, cnonce=\"f2/wE4q74E6zIJEtWaHKaf5wv/H5QzzpXusqGemxURZJ\", qop=auth, response=\"8ca523f5e9506fed4657c9700eebdbec\", opaque=\"FQhe/qaU925kfnzjCev0ciny7QMkPqMAFRtzCUYo5tdS\""
       (http-digest-authorization
        (challenge "MD5") "GET" "/dir/index.html"
        "Mufasa" "Circle of Life" cnonce))
      (ensure-equal
       "Digest username=\"793263caabb707a56211940d90411ea4a575adeccb7e360aeb624ed06ece9b0b\", realm=\"api@example.org\", uri=\"/doe.json\", algorithm=SHA-512-256, nonce=\"5TsQWLVdgBdmrQ0XsxbDODV+57QdFR34I9HAbC/RVvkK\", nc=00000001, cnonce=\"NTg6RKcb9boFIAS3KrFK9BGeh+iDa/sm6jUMp2wds69v\", qop=auth, response=\"3798d4131c277846293534c3edc11bd8a5e4cdcbff78b05db9d95eeb1cec68a5\", opaque=\"HRPCssKJSGjCrkzDg8OhwpzCiGPChXYjwrI2QmXDnsOS\", userhash=true"
       (http-digest-authorization
        (first
         (http-parse-authentication-challenges
          "Digest realm=\"api@example.org\", qop=auth, algorithm=SHA-512-256, nonce=\"5TsQWLVdgBdmrQ0XsxbDODV+57QdFR34I9HAbC/RVvkK\", opaque=\"HRPCssKJSGjCrkzDg8OhwpzCiGPChXYjwrI2QmXDnsOS\", charset=UTF-8, userhash=true"))
        "GET" "/doe.json" "Jäsøn Doe" "Secret, or not?"
        "NTg6RKcb9boFIAS3KrFK9BGeh+iDa/sm6jUMp2wds69v"))
      (ensure-equal
       "Digest username*=UTF-8''J%C3%A4s%C3%B8n%20Doe, realm=\"api@example.org\", uri=\"/doe.json\", algorithm=SHA-512-256, nonce=\"5TsQWLVdgBdmrQ0XsxbDODV+57QdFR34I9HAbC/RVvkK\", nc=00000001, cnonce=\"NTg6RKcb9boFIAS3KrFK9BGeh+iDa/sm6jUMp2wds69v\", qop=auth, response=\"3798d4131c277846293534c3edc11bd8a5e4cdcbff78b05db9d95eeb1cec68a5\", opaque=\"HRPCssKJSGjCrkzDg8OhwpzCiGPChXYjwrI2QmXDnsOS\", userhash=false"
       (http-digest-authorization
        (first
         (http-parse-authentication-challenges
          "Digest realm=\"api@example.org\", qop=auth, algorithm=SHA-512-256, nonce=\"5TsQWLVdgBdmrQ0XsxbDODV+57QdFR34I9HAbC/RVvkK\", opaque=\"HRPCssKJSGjCrkzDg8OhwpzCiGPChXYjwrI2QmXDnsOS\", charset=UTF-8, userhash=false"))
        "GET" "/doe.json" "Jäsøn Doe" "Secret, or not?"
        "NTg6RKcb9boFIAS3KrFK9BGeh+iDa/sm6jUMp2wds69v"))
      (ensure-equal
       "Digest username=\"a947aad205e80e429958a387394944c6b496301e79f89d35a4cc23b6ee12b5b6\", realm=\"http-auth@example.org\", uri=\"/dir/index.html\", algorithm=SHA-256, nonce=\"7ypf/xlj9XXwfDPEoM4URrv/xwf94BcCAzFZH4GiTo0v\", nc=00000001, cnonce=\"f2/wE4q74E6zIJEtWaHKaf5wv/H5QzzpXusqGemxURZJ\", qop=auth, response=\"753927fa0e85d155564e2e272a28d1802ca10daf4496794697cf8db5856cb6c1\", opaque=\"FQhe/qaU925kfnzjCev0ciny7QMkPqMAFRtzCUYo5tdS\", userhash=true"
       (http-digest-authorization
        (first
         (http-parse-authentication-challenges
          "Digest realm=\"http-auth@example.org\", qop=auth, algorithm=SHA-256, nonce=\"7ypf/xlj9XXwfDPEoM4URrv/xwf94BcCAzFZH4GiTo0v\", opaque=\"FQhe/qaU925kfnzjCev0ciny7QMkPqMAFRtzCUYo5tdS\", userhash=true"))
        "GET" "/dir/index.html" "Mufasa" "Circle of Life" cnonce)))))

(deftest client-challenge-authentication-retries-once
  (let ((calls 0)
        (provider-calls 0))
    (let ((client
            (make-http-client
             :cache nil
             :challenge-auth-provider
             (lambda (request response challenges)
               (incf provider-calls)
               (ensure-equal 401 (http-response-status response))
               (ensure-equal "Basic"
                             (http-authentication-challenge-scheme
                              (first challenges)))
               (ensure-false
                (http-header-present-p
                 (http-request-headers request) "Authorization"))
               (http-basic-authorization "user" "pass"))
             :transport-function
             (lambda (request &key &allow-other-keys)
               (incf calls)
               (if (= calls 1)
                   (client-test-response
                    401
                    :headers
                    (list (make-http-header
                           "WWW-Authenticate" "Basic realm=\"api\"")))
                   (progn
                     (ensure-equal
                      "Basic dXNlcjpwYXNz"
                      (http-header-value
                       (http-request-headers request) "Authorization"))
                     (client-test-response 200)))))))
      (multiple-value-bind (response effective-request)
          (http-client-send
           client
           (http-client-request client "GET" "http://example.test/private"))
        (ensure-equal 200 (http-response-status response))
        (ensure-equal
         "Basic dXNlcjpwYXNz"
         (http-header-value
          (http-request-headers effective-request) "Authorization")))
      (ensure-equal 2 calls)
      (ensure-equal 1 provider-calls))))

(deftest client-forwards-response-field-limit
  (let* ((observed-limit nil)
         (client
           (make-http-client
            :max-fields 17
            :transport-function
            (lambda (request &key max-fields &allow-other-keys)
              (declare (ignore request))
              (setf observed-limit max-fields)
              (client-test-response 200)))))
    (http-client-send
     client
     (http-client-request client "GET" "http://example.test/"))
    (ensure-equal 17 observed-limit))
  (signals http-protocol-error
    (make-http-client :max-fields 0
                      :transport-function
                      (lambda (&rest arguments)
                        (declare (ignore arguments))))))

(deftest client-challenge-authentication-requires-replayable-body
  (let ((calls 0)
        (provider-calls 0))
    (let ((client
            (make-http-client
             :cache nil
             :challenge-auth-provider
             (lambda (&rest arguments)
               (declare (ignore arguments))
               (incf provider-calls)
               "Basic credentials")
             :transport-function
             (lambda (request &key &allow-other-keys)
               (declare (ignore request))
               (incf calls)
               (client-test-response
                401
                :headers
                (list (make-http-header
                       "WWW-Authenticate" "Basic realm=\"api\"")))))))
      (let ((response
              (http-client-send
               client
               (http-client-request client "POST" "http://example.test/private")
               :request-body-function (lambda (stream) (declare (ignore stream))))))
        (ensure-equal 401 (http-response-status response)))
      (ensure-equal 1 calls)
      (ensure-equal 0 provider-calls))))

(deftest client-proxy-challenge-authentication-retries-once
  (let ((calls 0)
        (provider-calls 0))
    (let ((client
            (make-http-client
             :cache nil
             :proxy (make-http-proxy :scheme :http
                                     :host "proxy.example"
                                     :port 8080)
             :proxy-challenge-auth-provider
             (lambda (request response proxy-plan challenges)
               (incf provider-calls)
               (ensure-equal 407 (http-response-status response))
               (ensure-equal :forward (getf proxy-plan :mode))
               (ensure-equal "Basic"
                             (http-authentication-challenge-scheme
                              (first challenges)))
               (ensure-false
                (http-header-present-p
                 (http-request-headers request) "Proxy-Authorization"))
               (http-basic-authorization "proxy-user" "proxy-pass"))
             :transport-function
             (lambda (request &key proxy-plan &allow-other-keys)
               (incf calls)
               (ensure-equal :forward (getf proxy-plan :mode))
               (if (= calls 1)
                   (client-test-response
                    407
                    :headers
                    (list (make-http-header
                           "Proxy-Authenticate" "Basic realm=\"proxy\"")))
                   (progn
                     (ensure-equal
                      "Basic cHJveHktdXNlcjpwcm94eS1wYXNz"
                      (http-header-value
                       (http-request-headers request) "Proxy-Authorization"))
                     (client-test-response 200)))))))
      (multiple-value-bind (response effective-request)
          (http-client-send
           client
           (http-client-request client "GET" "http://example.test/private"))
        (ensure-equal 200 (http-response-status response))
        (ensure-equal
         "Basic cHJveHktdXNlcjpwcm94eS1wYXNz"
         (http-header-value
          (http-request-headers effective-request) "Proxy-Authorization")))
      (ensure-equal 2 calls)
      (ensure-equal 1 provider-calls))))

(deftest client-proxy-challenge-authentication-requires-replayable-body
  (let ((calls 0)
        (provider-calls 0))
    (let ((client
            (make-http-client
             :cache nil
             :proxy-challenge-auth-provider
             (lambda (&rest arguments)
               (declare (ignore arguments))
               (incf provider-calls)
               "Basic credentials")
             :transport-function
             (lambda (request &key &allow-other-keys)
               (declare (ignore request))
               (incf calls)
               (client-test-response
                407
                :headers
                (list (make-http-header
                       "Proxy-Authenticate" "Basic realm=\"proxy\"")))))))
      (let ((response
              (http-client-send
               client
               (http-client-request client "POST" "http://example.test/private")
               :request-body-function (lambda (stream) (declare (ignore stream))))))
        (ensure-equal 407 (http-response-status response)))
      (ensure-equal 1 calls)
      (ensure-equal 0 provider-calls))))

(deftest client-request-trailers-through-policy
  (let ((seen nil)
        (trailers (list (make-http-header "X-Checksum" "abc"))))
    (let* ((client
             (make-http-client
              :cache nil
              :transport-function
              (lambda (request &key proxy-plan &allow-other-keys)
                (declare (ignore proxy-plan))
                (setf seen request)
                (client-test-response 200))))
           (request (http-client-request
                     client "POST" "http://example.test/upload"
                     :trailers trailers)))
      (ensure-equal "abc"
                    (http-header-value
                     (http-request-trailers request) "X-Checksum"))
      (multiple-value-bind (response effective)
          (http-client-send client request)
        (declare (ignore response))
        (ensure-equal "abc"
                      (http-header-value
                       (http-request-trailers effective) "X-Checksum"))
        (ensure-equal "abc"
                      (http-header-value
                      (http-request-trailers seen) "X-Checksum"))))))

(deftest client-decodes-gzip-response-content
  (let* ((encoded
           (octets #x1f #x8b #x08 #x00 #x00 #x00 #x00 #x00 #x00 #x03
                   #xcb #x48 #xcd #xc9 #xc9 #x07 #x00 #x86 #xa6 #x10
                   #x36 #x05 #x00 #x00 #x00))
         (client
           (make-http-client
            :transport-function
            (lambda (request &rest arguments)
              (declare (ignore request arguments))
              (client-test-response
               200
               :headers (list (make-http-header "Content-Encoding" "gzip")
                              (make-http-header "Content-Length" "25"))
               :body encoded))))
         (request (http-client-request client "GET" "http://example.test/")))
    (multiple-value-bind (response effective-request)
        (http-client-send client request)
      (ensure-equal "gzip, deflate, br, zstd"
                    (http-header-value
                     (http-request-headers effective-request)
                     "Accept-Encoding"))
      (ensure-equal (ascii "hello") (http-response-body response))
      (ensure-false (http-header-present-p
                     (http-response-headers response) "Content-Encoding"))
      (ensure-false (http-header-present-p
                     (http-response-headers response) "Content-Length")))
    (ensure-equal
     (ascii "hello")
     (http-response-body
      (decode-http-response-content
       (client-test-response
        200
        :headers (list (make-http-header "Content-Encoding" "gzip"))
        :body encoded)
       :max-body-bytes 5)))
    (signals http-size-limit-exceeded
      (decode-http-response-content
       (client-test-response
        200
        :headers (list (make-http-header "Content-Encoding" "gzip"))
        :body encoded)
       :max-body-bytes 4))))

(deftest client-rejects-malformed-compressed-response-content
  (signals http-protocol-error
    (decode-http-response-content
     (client-test-response
      200
      :headers (list (make-http-header "Content-Encoding" "gzip"))
      :body (octets 1 2 3)))))

(deftest client-does-not-decode-contentless-response-content
  (dolist (case '(("GET" 205) ("CONNECT" 200)))
    (destructuring-bind (method status) case
      (let* ((decoder-called-p nil)
             (client
               (make-http-client
                :content-decoders
                (make-http-content-decoders
                 (cons "test"
                       (lambda (body max-body-bytes)
                         (declare (ignore body max-body-bytes))
                         (setf decoder-called-p t)
                         (ascii "decoded"))))
                :transport-function
                (lambda (request &rest arguments)
                  (declare (ignore request arguments))
                  (client-test-response
                   status
                   :headers (list (make-http-header "Content-Encoding" "test"))))))
             (response
               (http-client-send
                client
                (http-client-request client method "http://example.test/"))))
        (ensure-false decoder-called-p)
        (ensure-equal (octets) (http-response-body response))
        (ensure-equal "test"
                      (http-header-value
                       (http-response-headers response)
                       "Content-Encoding"))))))

(deftest client-decodes-deflate-and-stacked-response-content
  (dolist (case
           (list
            (list "deflate"
                  (octets #x78 #x9c #xcb #x48 #xcd #xc9 #xc9
                          #x07 #x00 #x06 #x2c #x02 #x15))
            (list "gzip, deflate"
                  (octets #x78 #x9c #x93 #xef #xe6 #x60 #x00 #x01
                          #xa6 #xff #xa7 #x3d #xce #x9e #x3c #xc9
                          #xce #xd0 #xb6 #x4c #xc0 #x8c #x15 #x28
                          #x00 #x00 #x57 #xc1 #x06 #xa4))))
    (destructuring-bind (content-encoding encoded) case
      (let ((response
              (decode-http-response-content
               (client-test-response
                200
                :headers
                (list (make-http-header "Content-Encoding" content-encoding)
                      (make-http-header "Content-Length"
                                        (princ-to-string (length encoded))))
                :body encoded))))
        (ensure-equal (ascii "hello") (http-response-body response))
        (ensure-false
         (http-header-present-p
          (http-response-headers response) "Content-Encoding"))
        (ensure-false
         (http-header-present-p
          (http-response-headers response) "Content-Length"))))))

(deftest client-supports-custom-content-decoders
  (let* ((calls nil)
         (decoders
           (make-http-content-decoders
            (cons "br"
                  (lambda (body max-body-bytes)
                    (push (list body max-body-bytes) calls)
                    (ascii "decoded")))))
         (client
           (make-http-client
            :content-decoders decoders
            :max-body-bytes 32
            :transport-function
            (lambda (request &rest arguments)
              (declare (ignore arguments))
              (ensure-equal "gzip, deflate, zstd, br"
                            (http-header-value
                             (http-request-headers request)
                             "Accept-Encoding"))
              (client-test-response
               200
               :headers (list (make-http-header "Content-Encoding" "br"))
               :body (ascii "encoded"))))))
    (ensure-equal (ascii "decoded")
                  (http-response-body
                   (http-client-send
                    client
                    (http-client-request client "GET" "http://example.test/"))))
    (ensure-equal (list (list (ascii "encoded") 32)) calls)))

(deftest client-content-decoder-registry-boundaries
  (signals http-protocol-error
    (make-http-content-decoders (cons "not a token" #'identity)))
  (let* ((response
           (client-test-response
            200
            :headers (list (make-http-header "Content-Encoding" "unknown, br")
                           (make-http-header "Content-Length" "7"))
            :body (ascii "encoded")))
         (decoded
           (decode-http-response-content
            response
            :content-decoders
            (make-http-content-decoders
             (cons "br"
                   (lambda (body limit)
                     (declare (ignore body limit))
                     (ascii "decoded")))))))
    (ensure-true (eq response decoded))
    (ensure-equal "unknown, br"
                  (http-header-value
                   (http-response-headers decoded) "Content-Encoding"))))

(deftest client-can-disable-response-content-decoding
  (let* ((encoded
           (octets #x1f #x8b #x08 #x00 #x00 #x00 #x00 #x00 #x00 #x03
                   #xcb #x48 #xcd #xc9 #xc9 #x07 #x00 #x86 #xa6 #x10
                   #x36 #x05 #x00 #x00 #x00))
         (client
           (make-http-client
            :automatic-decompression-p nil
            :transport-function
            (lambda (request &rest arguments)
              (declare (ignore request arguments))
              (client-test-response
               200
               :headers (list (make-http-header "Content-Encoding" "gzip"))
               :body encoded))))
         (request (http-client-request client "GET" "http://example.test/")))
    (multiple-value-bind (response effective-request)
        (http-client-send client request)
      (ensure-false (http-header-present-p
                     (http-request-headers effective-request)
                     "Accept-Encoding"))
      (ensure-equal encoded (http-response-body response))
      (ensure-equal "gzip"
                    (http-header-value
                     (http-response-headers response) "Content-Encoding")))))

(deftest client-preserves-explicit-accept-encoding
  (let* ((client
           (make-http-client
            :transport-function
            (lambda (request &rest arguments)
              (declare (ignore arguments))
              (ensure-equal "identity"
                            (http-header-value
                             (http-request-headers request)
                             "Accept-Encoding"))
              (client-test-response 200))))
         (request
           (http-client-request
            client "GET" "http://example.test/"
            :headers (list (make-http-header "Accept-Encoding" "identity")))))
    (multiple-value-bind (response effective-request)
        (http-client-send client request)
      (declare (ignore response))
      (ensure-equal "identity"
                    (http-header-value
                     (http-request-headers effective-request)
                     "Accept-Encoding")))))
(deftest client-send-rejects-invalid-cancellation-token
  (let* ((transport-calls 0)
         (client
           (make-http-client
            :cache nil
            :transport-function
            (lambda (request &key &allow-other-keys)
              (declare (ignore request))
              (incf transport-calls)
              (client-test-response 200))))
         (caught
           (handler-case
               (progn
                 (http-client-send
                  client "GET" "http://example.test/invalid-cancel-token"
                  :cancel-token :invalid)
                 nil)
             (http-protocol-error (condition) condition))))
    (ensure-true caught)
    (ensure-equal :client (http-error-operation caught))
    (ensure-equal :invalid (http-protocol-error-detail caught))
    (ensure-equal 0 transport-calls)))

(deftest client-multipart-parser-round-trip
  (multiple-value-bind (body content-type)
      (make-http-multipart-body
       (list (make-http-multipart-part :name "field" :value "value")
             (make-http-multipart-part
              :name "upload"
              :value (octets 0 1 2 255)
              :filename "data.bin"
              :content-type "application/octet-stream"))
       :boundary "boundary")
    (ensure-equal "multipart/form-data; boundary=\"boundary\"" content-type)
    (let ((original-body (copy-seq body)))
      (let ((parts (parse-http-multipart-body
                    body
                    :content-type "multipart/form-data; boundary=\"boundary\"")))
        (ensure-octets-equal original-body body)
        (ensure-equal 2 (length parts))
        (let ((field (first parts))
              (upload (second parts)))
          (ensure-equal "field" (http-multipart-part-name field))
          (ensure-equal (octets-as-string (octets 118 97 108 117 101))
                        (octets-as-string (http-multipart-part-value field)))
          (ensure-equal "upload" (http-multipart-part-name upload))
          (ensure-equal "data.bin" (http-multipart-part-filename upload))
          (ensure-equal "application/octet-stream"
                        (http-multipart-part-content-type upload))
          (ensure-equal (octets 0 1 2 255)
                        (http-multipart-part-value upload))
          (let ((payload-offset (position 0 body)))
            (ensure-true payload-offset)
            (setf (aref body payload-offset) 42)
            (ensure-equal (octets 0 1 2 255)
                          (http-multipart-part-value upload))))))))

(deftest client-multipart-generator-rejects-header-control-characters
  (dolist (part (list (make-http-multipart-part
                       :name (format nil "field~Cname" #\Null)
                       :value "value")
                      (make-http-multipart-part
                       :name "upload"
                       :value "value"
                       :filename (format nil "file~Cname" #\Tab))
                      (make-http-multipart-part
                       :name "field"
                       :value "value"
                       :content-type (format nil "text/plain~C" (code-char 127)))))
    (signals http-protocol-error
      (make-http-multipart-body (list part) :boundary "boundary"))))

(deftest client-multipart-parser-enforces-limits
  (multiple-value-bind (body content-type)
      (make-http-multipart-body
       (list (make-http-multipart-part :name "field" :value "value"))
       :boundary "boundary")
    (signals http-size-limit-exceeded
      (parse-http-multipart-body body :content-type content-type :max-parts 0))
    (signals http-size-limit-exceeded
      (parse-http-multipart-body body :content-type content-type :max-body-bytes 2))
    (signals http-size-limit-exceeded
      (parse-http-multipart-body body :content-type content-type
                                      :max-input-bytes (1- (length body))))
    (ensure-equal 1
                  (length
                   (parse-http-multipart-body body :content-type content-type
                                                   :max-input-bytes
                                                   (length body))))
    (signals http-protocol-error
      (parse-http-multipart-body body
                                 :content-type "multipart/form-data; boundary=other"))))

(deftest client-multipart-parser-header-limit-is-exact
  (let* ((headers
           (ascii "Content-Disposition: form-data; name=\"field\"|CRLF||CRLF|"))
         (body
           (ascii "--boundary|CRLF|Content-Disposition: form-data; name=\"field\"|CRLF||CRLF|value|CRLF|--boundary--|CRLF|")))
    (ensure-equal 1
                  (length
                   (parse-http-multipart-body
                    body :boundary "boundary"
                         :max-header-bytes (length headers))))
    (signals http-size-limit-exceeded
      (parse-http-multipart-body
       body :boundary "boundary" :max-header-bytes (1- (length headers))))))

(deftest client-multipart-parser-ignores-boundary-prefix-in-content
  (let* ((value (ascii "before|CRLF|--boundaryXafter"))
         (part (make-http-multipart-part :name "field" :value value)))
    (multiple-value-bind (body content-type)
        (make-http-multipart-body (list part) :boundary "boundary")
      (let ((parsed (parse-http-multipart-body body :content-type content-type)))
        (ensure-equal 1 (length parsed))
        (ensure-equal value (http-multipart-part-value (first parsed)))))))

(deftest client-multipart-generator-rejects-delimiter-collisions
  (dolist (value (list (ascii "--boundary|CRLF|payload")
                       (ascii "before|CRLF|--boundary--|CRLF|after")))
    (signals http-protocol-error
      (make-http-multipart-body
       (list (make-http-multipart-part :name "field" :value value))
       :boundary "boundary")))
  (multiple-value-bind (body content-type)
      (make-http-multipart-body
       (list (make-http-multipart-part
              :name "field"
              :value (ascii "before|CRLF|--boundaryXafter")))
       :boundary "boundary")
    (ensure-equal 1
                  (length (parse-http-multipart-body
                           body :content-type content-type)))))

(deftest client-multipart-enforces-boundary-grammar
  (dolist (boundary '("bad\"boundary" "bad[boundary" "trailing "))
    (signals http-protocol-error
      (make-http-multipart-body nil :boundary boundary)))
  (multiple-value-bind (body content-type)
      (make-http-multipart-body nil :boundary "valid boundary")
    (ensure-equal "multipart/form-data; boundary=\"valid boundary\""
                  content-type)
    (ensure-equal nil
                  (parse-http-multipart-body body :content-type content-type))))

(deftest client-multipart-parser-enforces-parameter-token-grammar
  (dolist (content-type '("multipart/form-data; boundary=bad boundary"
                          "multipart/form-data; bound ary=boundary"))
    (signals http-protocol-error
      (parse-http-multipart-body (octets) :content-type content-type)))
  (ensure-equal nil
                (parse-http-multipart-body
                 (ascii "--valid boundary--|CRLF|")
                 :content-type
                 "multipart/form-data; boundary=\"valid boundary\"")))

(deftest client-multipart-parser-validates-delimiter-lines
  (signals http-protocol-error
    (parse-http-multipart-body
     (ascii "--boundary|CRLF|Content-Disposition: form-data; name=\"field\"|CRLF||CRLF|value|CRLF|--boundary--evil")
     :boundary "boundary"))
  (let ((parts
          (parse-http-multipart-body
           (ascii "--boundary  |CRLF|Content-Disposition: form-data; name=\"field\"|CRLF||CRLF|value|CRLF|--boundary--  |CRLF|epilogue")
           :boundary "boundary")))
    (ensure-equal 1 (length parts))
    (ensure-equal (ascii "value")
                  (http-multipart-part-value (first parts)))))

(deftest client-multipart-parser-rejects-ambiguous-part-metadata
  (dolist (headers
            '("Content-Disposition: form-data; name=\"first\"|CRLF|Content-Disposition: form-data; name=\"second\""
              "Content-Disposition: form-data; name=\"field\"|CRLF|Content-Type: text/plain|CRLF|Content-Type: application/json"))
    (signals http-protocol-error
      (parse-http-multipart-body
       (ascii (format nil "--boundary|CRLF|~A|CRLF||CRLF|value|CRLF|--boundary--|CRLF|"
                      headers))
       :boundary "boundary"))))

(deftest client-cookie-jar
  (let* ((jar (make-http-cookie-jar :clock-function (lambda () 1000)))
         (response (client-test-response
                    200
                    :headers (list (make-http-header
                                    "Set-Cookie"
                                    "sid=abc; Path=/; Max-Age=60")))))
    (http-cookie-jar-accept-response
     jar "http://example.test/login" response :now 1000)
    (ensure-equal "sid=abc"
                  (http-cookie-jar-cookie-header
                   jar "http://example.test/dashboard" :now 1001))
    (ensure-equal nil
                  (http-cookie-jar-cookie-header
                   jar "http://other.example/dashboard" :now 1001))))

(deftest client-cookie-jar-enforces-secure-acceptance
  (let ((jar (make-http-cookie-jar)))
    (http-cookie-jar-accept-response
     jar "http://example.test/"
     (client-test-response
      200 :headers (list (make-http-header "Set-Cookie" "secure=1; Secure")
                         (make-http-header "Set-Cookie"
                                           "none=1; SameSite=None")
                         (make-http-header "Set-Cookie"
                                           "__Secure-id=1; Secure")))
     :now 1000)
    (ensure-equal nil (http-cookie-jar-cookies jar))
    (http-cookie-jar-accept-response
     jar "https://example.test/"
     (client-test-response
      200 :headers (list (make-http-header
                          "Set-Cookie" "none=1; SameSite=None; Secure")
                         (make-http-header
                          "Set-Cookie" "__Secure-id=1; Secure")))
     :now 1000)
    (let ((header (http-cookie-jar-cookie-header
                   jar "https://example.test/" :now 1001)))
      (ensure-true (search "none=1" header))
      (ensure-true (search "__Secure-id=1" header)))))

(deftest client-cookie-jar-partitions-secure-cookies
  (let ((jar (make-http-cookie-jar)))
    (dolist (value '("missing-context=1; Secure; Partitioned"
                     "insecure=1; Partitioned"))
      (http-cookie-jar-accept-response
       jar "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Set-Cookie" value)))
       :now 1000))
    (ensure-equal nil (http-cookie-jar-cookies jar))
    (http-cookie-jar-accept-response
     jar "https://example.test/"
     (client-test-response
      200 :headers (list (make-http-header
                          "Set-Cookie" "sid=partitioned; Secure; Partitioned")))
     :now 1001 :partition-key "https://top.example")
    (let ((cookie (first (http-cookie-jar-cookies jar))))
      (ensure-equal "https://top.example"
                    (http-cookie-partition-key cookie)))
    (ensure-equal nil
                  (http-cookie-jar-cookie-header
                   jar "https://example.test/" :now 1002))
    (ensure-equal nil
                  (http-cookie-jar-cookie-header
                   jar "https://example.test/" :now 1002
                   :partition-key "https://other.example"))
    (ensure-equal "sid=partitioned"
                  (http-cookie-jar-cookie-header
                   jar "https://example.test/" :now 1002
                   :partition-key "https://top.example")))
  (signals http-protocol-error
    (make-http-cookie :name "sid" :value "1" :domain "example.test"
                      :partition-key "https://top.example"))
  (signals http-protocol-error
    (http-cookie-jar-cookie-header
     (make-http-cookie-jar) "https://example.test/" :partition-key "")))

(deftest client-cookie-jar-limits-each-partition-separately
  (let ((jar (make-http-cookie-jar :max-cookies 10
                                   :max-cookies-per-domain 1)))
    (dolist (entry '(("a=1; Secure; Partitioned" "https://a.example")
                     ("b=1; Secure; Partitioned" "https://b.example")))
      (http-cookie-jar-accept-response
       jar "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Set-Cookie" (first entry))))
       :now 1000 :partition-key (second entry)))
    (ensure-equal 2 (length (http-cookie-jar-cookies jar)))))

(deftest client-cookie-jar-enforces-byte-limits
  (let ((jar (make-http-cookie-jar :max-cookie-bytes 8
                                   :max-total-cookie-bytes 15)))
    (dolist (value '("oversized=1" "a=1" "b=2"))
      (http-cookie-jar-accept-response
       jar "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Set-Cookie" value)))
       :now 1000))
    (ensure-equal 1 (length (http-cookie-jar-cookies jar)))
    (ensure-equal "b=2"
                  (http-cookie-jar-cookie-header
                   jar "https://example.test/" :now 1001))))

(deftest client-cookie-jar-enforces-host-prefix
  (let ((jar (make-http-cookie-jar)))
    (dolist (value '("__Host-a=1; Secure; Domain=example.test; Path=/"
                     "__Host-b=1; Secure; Path=/nested"
                     "__Host-c=1; Path=/"
                     "__Host-d=1; Secure"
                     "__hOsT-e=1; Secure"))
      (http-cookie-jar-accept-response
       jar "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Set-Cookie" value)))
       :now 1000))
    (ensure-equal nil (http-cookie-jar-cookies jar))
    (http-cookie-jar-accept-response
     jar "https://example.test/"
     (client-test-response
      200 :headers (list (make-http-header
                          "Set-Cookie" "__Host-session=1; Secure; Path=/")))
     :now 1000)
    (ensure-equal "__Host-session=1"
                  (http-cookie-jar-cookie-header
                   jar "https://example.test/" :now 1001))))

(deftest client-cookie-jar-preserves-creation-and-refreshes-expiry
  (let ((jar (make-http-cookie-jar)))
    (http-cookie-jar-accept-response
     jar "https://example.test/"
     (client-test-response
      200 :headers (list (make-http-header
                          "Set-Cookie" "sid=old; Max-Age=10")))
     :now 1000)
    (http-cookie-jar-accept-response
     jar "https://example.test/"
     (client-test-response
      200 :headers (list (make-http-header
                          "Set-Cookie" "sid=new; Max-Age=10")))
     :now 1005)
    (ensure-equal 1000
                  (http-cookie-creation-time
                   (first (http-cookie-jar-cookies jar))))
    (ensure-equal "sid=new"
                  (http-cookie-jar-cookie-header
                   jar "https://example.test/" :now 1011))
    (ensure-equal nil
                  (http-cookie-jar-cookie-header
                   jar "https://example.test/" :now 1015))
    (ensure-equal nil (http-cookie-jar-cookies jar))))

(deftest client-cookie-values-use-cookie-octets
  (let ((jar (make-http-cookie-jar)))
    (http-cookie-jar-accept-response
     jar "https://example.test/"
     (client-test-response
      200 :headers (list (make-http-header "Set-Cookie"
                                           "quoted=\"value\"")))
     :now 1000)
    (ensure-equal "quoted=\"value\""
                  (http-cookie-jar-cookie-header
                   jar "https://example.test/" :now 1001)))
  (signals http-protocol-error
    (make-http-cookie :name "bad" :value "quoted\"value"
                      :domain "example.test"))
  (signals http-protocol-error
    (make-http-cookie :name "bad" :value "comma,value"
                      :domain "example.test"))
  (signals http-protocol-error
    (make-http-cookie :name "bad" :value "back\\slash"
                      :domain "example.test"))
  (signals http-protocol-error
    (make-http-cookie :name "none" :value "1" :domain "example.test"
                      :same-site :none))
  (signals http-protocol-error
    (make-http-cookie :name "__Secure-id" :value "1"
                      :domain "example.test"))
  (signals http-protocol-error
    (make-http-cookie :name "__Host-id" :value "1"
                      :domain "example.test" :secure-p t)))

(deftest client-cookie-domain-rejects-ip-suffixes
  (let ((jar (make-http-cookie-jar
              :public-suffix-p-function (constantly nil))))
    (http-cookie-jar-accept-response
     jar "http://127.0.0.1/"
     (client-test-response
      200 :headers (list (make-http-header "Set-Cookie"
                                           "bad=1; Domain=0.0.1")
                         (make-http-header "Set-Cookie"
                                           "exact=1; Domain=127.0.0.1")))
     :now 1000)
    (ensure-equal "exact=1"
                  (http-cookie-jar-cookie-header
                   jar "http://127.0.0.1/" :now 1001))))

(deftest client-cookie-secure-cookie-blocks-insecure-overlay
  (let ((jar (make-http-cookie-jar)))
    (http-cookie-jar-accept-response
     jar "https://example.test/login"
     (client-test-response
      200 :headers (list (make-http-header "Set-Cookie"
                                           "sid=secure; Secure; Path=/login")))
     :now 1000)
    (http-cookie-jar-accept-response
     jar "http://example.test/login/en"
     (client-test-response
      200 :headers (list (make-http-header "Set-Cookie"
                                           "sid=attack; Path=/login/en")))
     :now 1001)
    (ensure-equal "sid=secure"
                  (http-cookie-jar-cookie-header
                   jar "https://example.test/login/en" :now 1002))))

(deftest client-cookie-identity-ignores-host-only-flag
  (let ((jar (make-http-cookie-jar
              :public-suffix-p-function (constantly nil))))
    (dolist (value '("sid=host-only" "sid=domain; Domain=example.test"))
      (http-cookie-jar-accept-response
       jar "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Set-Cookie" value)))
       :now 1000))
    (ensure-equal 1 (length (http-cookie-jar-cookies jar)))
    (ensure-equal "sid=domain"
                  (http-cookie-jar-cookie-header
                   jar "https://example.test/" :now 1001))))

(deftest client-cookie-jar-rejects-domain-cookie-without-public-suffix-policy
  (let ((jar (make-http-cookie-jar)))
    (http-cookie-jar-accept-response
     jar "https://evil.co.uk/"
     (client-test-response
      200 :headers (list (make-http-header "Set-Cookie"
                                           "sid=domain; Domain=co.uk; Secure")
                         (make-http-header "Set-Cookie"
                                           "host=only; Secure")))
     :now 1000)
    (ensure-equal "host=only"
                  (http-cookie-jar-cookie-header
                   jar "https://evil.co.uk/" :now 1001))
    (ensure-equal nil
                  (http-cookie-jar-cookie-header
                   jar "https://bank.co.uk/" :now 1001))))

(deftest client-cookie-names-are-case-sensitive
  (let ((jar (make-http-cookie-jar)))
    (http-cookie-jar-accept-response
     jar "https://example.test/"
     (client-test-response
      200 :headers (list (make-http-header "Set-Cookie" "SID=upper; Secure")))
     :now 1000)
    (http-cookie-jar-accept-response
     jar "https://example.test/"
     (client-test-response
      200 :headers (list (make-http-header "Set-Cookie" "sid=lower")))
     :now 1001)
    (http-cookie-jar-accept-response
     jar "http://example.test/"
     (client-test-response
      200 :headers (list (make-http-header "Set-Cookie" "sid=replaced")))
     :now 1002)
    (ensure-equal 2 (length (http-cookie-jar-cookies jar)))
    (ensure-equal "SID=upper; sid=replaced"
                  (http-cookie-jar-cookie-header
                   jar "https://example.test/" :now 1003))))

(deftest client-cookie-jar-enforces-public-suffix-policy
  (let ((jar (make-http-cookie-jar
              :public-suffix-p-function
              (lambda (domain) (string-equal domain "com")))))
    (http-cookie-jar-accept-response
     jar "https://example.com/"
     (client-test-response
      200 :headers (list (make-http-header "Set-Cookie"
                                           "sid=bad; Domain=com")))
     :now 1000)
    (ensure-equal nil (http-cookie-jar-cookies jar))
    (http-cookie-jar-accept-response
     jar "https://com/"
     (client-test-response
      200 :headers (list (make-http-header "Set-Cookie"
                                           "sid=host; Domain=com")))
     :now 1001)
    (ensure-true (http-cookie-host-only-p
                  (first (http-cookie-jar-cookies jar))))))

(deftest client-cookie-jar-evicts-least-recently-used
  (let ((jar (make-http-cookie-jar :max-cookies 2
                                   :max-cookies-per-domain 2)))
    (dolist (entry '(("a=1" 1000) ("b=2" 1001)))
      (http-cookie-jar-accept-response
       jar "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Set-Cookie" (first entry))))
       :now (second entry)))
    (ensure-equal "a=1; b=2"
                  (http-cookie-jar-cookie-header
                   jar "https://example.test/" :now 1002))
    (http-cookie-jar-accept-response
     jar "https://example.test/"
     (client-test-response
      200 :headers (list (make-http-header "Set-Cookie" "c=3")))
     :now 1003)
    (let ((header (http-cookie-jar-cookie-header
                   jar "https://example.test/" :now 1004)))
      (ensure-equal nil (search "a=1" header))
      (ensure-true (search "b=2" header))
      (ensure-true (search "c=3" header)))))

(deftest client-cookie-jar-enforces-same-site-context
  (let ((jar (make-http-cookie-jar)))
    (dolist (value '("strict=1; SameSite=Strict"
                     "lax=1; SameSite=Lax"
                     "default=1"
                     "none=1; SameSite=None; Secure"))
      (http-cookie-jar-accept-response
       jar "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Set-Cookie" value)))
       :now 1000))
    (ensure-equal "none=1"
                  (http-cookie-jar-cookie-header
                   jar "https://example.test/" :now 1001
                   :same-site-p nil))
    (let ((header (http-cookie-jar-cookie-header
                   jar "https://example.test/" :now 1002
                   :same-site-p nil :top-level-navigation-p t
                   :method "GET")))
      (ensure-equal nil (search "strict=1" header))
      (ensure-true (search "lax=1" header))
      (ensure-true (search "default=1" header))
      (ensure-true (search "none=1" header)))
    (ensure-equal "none=1"
                  (http-cookie-jar-cookie-header
                   jar "https://example.test/" :now 1003
                   :same-site-p nil :top-level-navigation-p t
                   :method "POST"))))

(deftest client-cookie-jar-enforces-same-site-creation-context
  (let ((jar (make-http-cookie-jar))
        (response
          (client-test-response
           200 :headers
           (mapcar (lambda (value) (make-http-header "Set-Cookie" value))
                   '("strict=1; SameSite=Strict"
                     "lax=1; SameSite=Lax"
                     "default=1"
                     "none=1; SameSite=None; Secure")))))
    (http-cookie-jar-accept-response
     jar "https://example.test/" response
     :now 1000 :same-site-p nil)
    (let ((header (http-cookie-jar-cookie-header
                   jar "https://example.test/" :now 1001
                   :same-site-p t)))
      (ensure-true (search "default=1" header))
      (ensure-true (search "none=1" header))
      (ensure-equal nil (search "strict=1" header))
      (ensure-equal nil (search "lax=1" header)))
    (http-cookie-jar-clear jar)
    (http-cookie-jar-accept-response
     jar "https://example.test/" response
     :now 1002 :same-site-p nil :top-level-navigation-p t)
    (let ((header (http-cookie-jar-cookie-header
                   jar "https://example.test/" :now 1003
                   :same-site-p t)))
      (ensure-true (search "strict=1" header))
      (ensure-true (search "lax=1" header)))))

(deftest client-send-applies-same-site-cookie-creation-context
  (let* ((jar (make-http-cookie-jar))
         (client
           (make-http-client
            :cache nil
            :cookie-jar jar
            :transport-function
            (lambda (request &key &allow-other-keys)
              (declare (ignore request))
              (client-test-response
               200 :headers
               (list (make-http-header
                      "Set-Cookie" "strict=1; SameSite=Strict")))))))
    (http-client-send
     client
     (http-client-request client "GET" "https://example.test/")
     :cookie-same-site-p nil)
    (ensure-equal nil
                  (http-cookie-jar-cookie-header
                   jar "https://example.test/" :same-site-p t))))

(deftest client-send-applies-same-site-cookie-context
  (let ((seen-cookie nil)
        (jar (make-http-cookie-jar)))
    (dolist (value '("strict=1; SameSite=Strict"
                     "lax=1; SameSite=Lax"
                     "none=1; SameSite=None; Secure"))
      (http-cookie-jar-accept-response
       jar "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Set-Cookie" value)))
       :now 1000))
    (let ((client
            (make-http-client
             :cookie-jar jar
             :transport-function
             (lambda (request &key proxy-plan &allow-other-keys)
               (declare (ignore proxy-plan))
               (setf seen-cookie
                     (http-header-value
                      (http-request-headers request) "Cookie"))
               (client-test-response 200)))))
      (http-client-send
       client
       (http-client-request client "GET" "https://example.test/")
       :cookie-same-site-p nil
       :cookie-top-level-navigation-p t)
      (ensure-equal nil (search "strict=1" seen-cookie))
      (ensure-true (search "lax=1" seen-cookie))
      (ensure-true (search "none=1" seen-cookie)))))

(deftest client-send-applies-cookie-partition-context
  (let ((calls 0)
        (seen-cookie nil))
    (let ((client
            (make-http-client
             :cache nil
             :transport-function
             (lambda (request &key &allow-other-keys)
               (incf calls)
               (setf seen-cookie
                     (http-header-value
                      (http-request-headers request) "Cookie"))
               (if (= calls 1)
                   (client-test-response
                    200 :headers
                    (list (make-http-header
                           "Set-Cookie"
                           "sid=partitioned; Secure; Partitioned")))
                   (client-test-response 200))))))
      (dotimes (index 2)
        (declare (ignore index))
        (http-client-send
         client
         (http-client-request client "GET" "https://example.test/")
         :cookie-partition-key "https://top.example"))
      (ensure-equal "sid=partitioned" seen-cookie))))

(deftest client-strict-transport-store-policy
  (let* ((now 1000)
         (store (make-http-strict-transport-store
                 :clock-function (lambda () now))))
    (http-strict-transport-store-note-response
     store "http://example.test/"
     (client-test-response
      200 :headers (list (make-http-header
                          "Strict-Transport-Security"
                          "max-age=60; includeSubDomains"))))
    (ensure-equal nil
                  (http-strict-transport-store-known-host-p
                   store "example.test"))
    (http-strict-transport-store-note-response
     store "https://example.test/"
     (client-test-response
      200 :headers (list (make-http-header
                          "Strict-Transport-Security"
                          "max-age=60; includeSubDomains"))))
    (ensure-true (http-strict-transport-store-known-host-p
                  store "example.test"))
    (ensure-true (http-strict-transport-store-known-host-p
                  store "api.example.test"))
    (ensure-equal "https://api.example.test:443/path?q=1"
                  (http-uri-string
                   (http-strict-transport-store-upgrade-uri
                    store "http://api.example.test:80/path?q=1")))
    (ensure-equal "https://api.example.test:8080/path"
                  (http-uri-string
                   (http-strict-transport-store-upgrade-uri
                    store "http://api.example.test:8080/path")))
    (http-strict-transport-store-note-response
     store "https://127.0.0.1/"
     (client-test-response
      200 :headers (list (make-http-header
                          "Strict-Transport-Security" "max-age=60"))))
    (ensure-equal nil
                  (http-strict-transport-store-known-host-p
                   store "127.0.0.1"))
    (http-strict-transport-store-note-response
     store "https://example.test/"
     (client-test-response
      200 :headers (list (make-http-header
                          "Strict-Transport-Security"
                          "max-age=30; max-age=60"))))
    (ensure-true (http-strict-transport-store-known-host-p
                  store "example.test"))
    (http-strict-transport-store-note-response
     store "https://quoted.test/"
     (client-test-response
      200 :headers (list (make-http-header
                          "Strict-Transport-Security" "max-age=\"60\""))))
    (ensure-true (http-strict-transport-store-known-host-p
                  store "quoted.test"))
    (http-strict-transport-store-note-response
     store "https://first-header.test/"
     (client-test-response
      200 :headers (list (make-http-header
                          "Strict-Transport-Security" "invalid")
                         (make-http-header
                          "Strict-Transport-Security" "max-age=60"))))
    (ensure-equal nil
                  (http-strict-transport-store-known-host-p
                   store "first-header.test"))
    (setf now 1060)
    (ensure-equal nil
                  (http-strict-transport-store-known-host-p
                   store "example.test"))
    (setf now 2000)
    (http-strict-transport-store-note-response
     store "https://example.test/"
     (client-test-response
      200 :headers (list (make-http-header
                          "Strict-Transport-Security" "max-age=60"))))
    (http-strict-transport-store-note-response
     store "https://example.test/"
     (client-test-response
      200 :headers (list (make-http-header
                          "Strict-Transport-Security" "max-age=0"))))
    (ensure-equal nil
                  (http-strict-transport-store-known-host-p
                   store "example.test"))))

(deftest client-strict-transport-send-and-redirect
  (let ((seen-uris nil)
        (calls 0))
    (let ((client
            (make-http-client
             :cache nil
             :transport-function
             (lambda (request &key &allow-other-keys)
               (incf calls)
               (push (http-uri-string (http-request-uri request)) seen-uris)
               (case calls
                 (1 (client-test-response
                     200 :headers (list (make-http-header
                                         "Strict-Transport-Security"
                                         "max-age=60; includeSubDomains"))))
                 (3 (client-test-response
                     302 :headers (list (make-http-header
                                         "Location"
                                         "http://api.example.test:8080/final"))))
                 (t (client-test-response 200)))))))
      (http-client-send
       client (http-client-request client "GET" "https://example.test/learn"))
      (http-client-send
       client (http-client-request client "GET" "http://example.test/next"))
      (http-client-send
       client (http-client-request client "GET" "https://example.test/redirect"))
      (ensure-equal
       '("https://example.test/learn"
         "https://example.test/next"
         "https://example.test/redirect"
         "https://api.example.test:8080/final")
       (reverse seen-uris)))))

(deftest client-default-port-origin-normalization
  (dolist (case '(("http://example.test/path" "http://example.test:80/other"
                  "http://example.test" "http://example.test:8080")
                 ("https://example.test/path" "https://example.test:443/other"
                  "https://example.test" "https://example.test:8443")
                 ("https://[::1]/path" "https://[::1]:443/other"
                  "https://[::1]" "https://[::1]:8443")))
    (destructuring-bind (implicit explicit origin nondefault) case
      (ensure-equal origin (http-uri-origin implicit))
      (ensure-equal origin (http-uri-origin explicit))
      (ensure-true (http-same-origin-p implicit explicit))
      (ensure-false (http-same-origin-p implicit nondefault))
      (ensure-equal nondefault (http-uri-origin nondefault)))))

(deftest client-alternative-service-default-port-origin
  (dolist (origins '(("https://example.test/resource" "https://example.test:443/other")
                    ("https://example.test:443/resource" "https://example.test/other")))
    (destructuring-bind (noted-origin equivalent-origin) origins
      (let ((store (make-http-alternative-service-store)))
        (http-alternative-service-store-note-response
         store noted-origin
         (client-test-response
          200 :headers (list (make-http-header "Alt-Svc" "h3=\":443\""))))
        (ensure-equal 1 (length (http-alternative-service-store-services
                                 store noted-origin)))
        (ensure-equal 1 (length (http-alternative-service-store-services
                                 store equivalent-origin)))
        (ensure-equal nil (http-alternative-service-store-services
                           store "https://example.test:8443/"))
        (http-alternative-service-store-note-response
         store equivalent-origin
         (client-test-response
          200 :headers (list (make-http-header "Alt-Svc" "clear"))))
        (ensure-equal nil (http-alternative-service-store-services
                           store noted-origin))))))

(deftest client-alternative-service-store-policy
  (let* ((now 1000)
         (store (make-http-alternative-service-store
                 :clock-function (lambda () now)))
         (origin "https://example.test/resource"))
    (http-alternative-service-store-note-response
     store origin
     (client-test-response
      200 :headers
      (list (make-http-header
             "Alt-Svc"
             "h3=\":443\"; ma=120; persist=1, h2=\"alt.example:8443\"; ma=60")
            (make-http-header "Age" "30"))))
    (let ((services (http-alternative-service-store-services store origin)))
      (ensure-equal 2 (length services))
      (let ((h3 (first services))
            (h2 (second services)))
        (ensure-equal "h3" (http-alternative-service-protocol-id h3))
        (ensure-equal "example.test" (http-alternative-service-host h3))
        (ensure-equal 443 (http-alternative-service-port h3))
        (ensure-equal 1090 (http-alternative-service-expires-at h3))
        (ensure-true (http-alternative-service-persist-p h3))
        (ensure-equal "h2" (http-alternative-service-protocol-id h2))
        (ensure-equal "alt.example" (http-alternative-service-host h2))
        (ensure-equal 8443 (http-alternative-service-port h2))
        (ensure-equal 1030 (http-alternative-service-expires-at h2))))
    (setf now 1030)
    (ensure-equal 1
                  (length (http-alternative-service-store-services
                           store origin)))
    (http-alternative-service-store-network-changed store)
    (ensure-equal 1
                  (length (http-alternative-service-store-services
                           store origin)))
    (http-alternative-service-store-note-response
     store origin
     (client-test-response
      421 :headers (list (make-http-header "Alt-Svc" "clear"))))
    (ensure-equal 1
                  (length (http-alternative-service-store-services
                           store origin)))
    (http-alternative-service-store-note-response
     store origin
     (client-test-response
      200 :headers (list (make-http-header
                          "Alt-Svc" "clear, h2=\":443\""))))
    (ensure-equal nil
                  (http-alternative-service-store-services store origin))))

(deftest client-alternative-service-validation-and-defaults
  (let* ((now 2000)
         (store (make-http-alternative-service-store
                 :clock-function (lambda () now)))
         (origin "https://example.test/"))
    (http-alternative-service-store-note-response
     store origin
     (client-test-response
      200 :headers
      (list (make-http-header
             "Alt-Svc"
             "h%32=\":443\", h%3a=\":443\", x%25y=\":8443\""))))
    (let ((services (http-alternative-service-store-services store origin)))
      (ensure-equal 1 (length services))
      (ensure-equal "x%25y"
                    (http-alternative-service-protocol-id (first services)))
      (ensure-equal 8443
                    (http-alternative-service-port (first services)))
      (ensure-equal (+ now 86400)
                    (http-alternative-service-expires-at
                     (first services)))
      (signals http-protocol-error
        (http-alternative-service-store-remove store origin "h3")))
    (http-alternative-service-store-note-response
     store origin
     (client-test-response
      200 :headers (list (make-http-header "Alt-Svc" "invalid"))))
    (ensure-equal nil
                  (http-alternative-service-store-services store origin))))

(deftest client-learns-alternative-services
  (let ((client
          (make-http-client
           :cache nil
           :clock-function (lambda () 3000)
           :transport-function
           (lambda (request &key &allow-other-keys)
             (declare (ignore request))
             (client-test-response
              200 :headers (list (make-http-header
                                  "Alt-Svc" "h3=\":443\"; ma=60")))))))
    (http-client-send
     client (http-client-request client "GET" "https://example.test/"))
    (let ((services
            (http-alternative-service-store-services
             (http-client-alternative-service-store client)
             "https://example.test/")))
      (ensure-equal 1 (length services))
      (ensure-equal "h3"
                    (http-alternative-service-protocol-id
                     (first services))))))

(deftest client-alt-svc-selects-http3-and-falls-back-to-tcp
  (let ((tcp-calls 0)
        (http3-calls 0)
        (http3-failure t)
        (client nil))
    (setf client
          (make-http-client
           :cache nil
           :clock-function (lambda () 4000)
           :transport-function
           (lambda (request &key &allow-other-keys)
             (incf tcp-calls)
             (ensure-false (http-request-http3-p request))
             (client-test-response 200 :body (ascii "tcp")))
           :http3-transport-function
           (lambda (request &key alternative-service &allow-other-keys)
             (incf http3-calls)
             (ensure-true (http-request-http3-p request))
             (ensure-equal "h3"
                           (http-alternative-service-protocol-id
                            alternative-service))
             (if http3-failure
                 (error 'http-connection-error
                        :message "synthetic HTTP/3 connection failure."
                        :operation :http3
                        :request-sent-p nil
                        :cause :unreachable)
                 (client-test-response 200
                                       :protocol-version "HTTP/3"
                                       :body (ascii "h3"))))))
    (http-client-send
     client (http-client-request client "GET" "https://example.test/"))
    (http-alternative-service-store-note-response
     (http-client-alternative-service-store client)
     "https://example.test/"
     (client-test-response
      200 :headers (list (make-http-header "Alt-Svc" "h3=\":443\"; ma=60"))))
    (let ((response (http-client-send
                     client
                     (http-client-request client "GET"
                                          "https://example.test/"))))
      (ensure-equal 200 (http-response-status response))
      (ensure-octets-equal (ascii "tcp") (http-response-body response)))
    (ensure-equal 1 http3-calls)
    (ensure-equal 2 tcp-calls)
    (ensure-equal nil
                  (http-alternative-service-store-services
                   (http-client-alternative-service-store client)
                   "https://example.test/"))
    (setf http3-failure nil)
    (http-alternative-service-store-note-response
     (http-client-alternative-service-store client)
     "https://example.test/"
     (client-test-response
      200 :headers (list (make-http-header "Alt-Svc" "h3=\":443\"; ma=60"))))
    (let ((response (http-client-send
                     client
                     (http-client-request client "GET"
                                          "https://example.test/"))))
      (ensure-equal "HTTP/3" (http-response-protocol-version response))
      (ensure-octets-equal (ascii "h3") (http-response-body response)))
    (ensure-equal 2 http3-calls)
    (ensure-equal 2 tcp-calls)))

(deftest client-alt-svc-does-not-replay-uncertain-failures
  (dolist (condition
           (list (make-condition 'simple-error
                                 :format-control "synthetic transport failure")
                 (make-condition 'http-connection-error
                                 :message "unknown send state")
                 (make-condition 'http-timeout
                                 :message "Ordinary transport timeout.")
                 (make-condition 'http-connection-error
                                 :message "request already sent"
                                 :request-sent-p t)))
    (let ((tcp-calls 0)
          (http3-calls 0)
          (caught nil))
      (let ((client
              (make-http-client
               :cache nil
               :transport-function
               (lambda (request &key &allow-other-keys)
                 (declare (ignore request))
                 (incf tcp-calls)
                 (client-test-response 200))
               :http3-transport-function
               (lambda (request &key &allow-other-keys)
                 (declare (ignore request))
                 (incf http3-calls)
                 (error condition)))))
        (http-alternative-service-store-note-response
         (http-client-alternative-service-store client)
         "https://example.test/"
         (client-test-response
          200 :headers (list (make-http-header "Alt-Svc" "h3=\":443\"; ma=60"))))
        (handler-case
            (http-client-send
             client (http-client-request client "POST" "https://example.test/"))
          (error (failure) (setf caught failure))))
      (ensure-true (eq condition caught))
      (ensure-equal 1 http3-calls)
      (ensure-equal 0 tcp-calls))))

(deftest client-alt-svc-does-not-replay-producers-or-callbacks
  (dolist (event '(:producer :producer-failure :body :information))
    (let* ((condition (make-condition 'http-connection-error
                                     :message "synthetic pre-send failure"
                                     :request-sent-p nil))
           (tcp-calls 0)
           (http3-calls 0)
           (observations 0)
           (caught nil)
           (producer
             (lambda (limit)
               (declare (ignore limit))
               (incf observations)
               (when (eq event :producer-failure)
                 (error condition))
               (ascii "body")))
           (callback
             (lambda (value)
               (declare (ignore value))
               (incf observations)
               (error condition)))
           (client
             (make-http-client
              :cache nil
              :transport-function
              (lambda (request &key &allow-other-keys)
                (declare (ignore request))
                (incf tcp-calls)
                (client-test-response 200))
              :http3-transport-function
              (lambda (request &key request-body-function on-body-chunk
                                 on-information &allow-other-keys)
                (declare (ignore request))
                (incf http3-calls)
                (ecase event
                  ((:producer :producer-failure)
                   (funcall request-body-function 65536))
                  (:body (funcall on-body-chunk (ascii "body")))
                  (:information
                   (funcall on-information (client-test-response 103))))
                (error condition)))))
      (http-alternative-service-store-note-response
       (http-client-alternative-service-store client)
       "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Alt-Svc" "h3=\":443\"; ma=60"))))
      (handler-case
          (apply #'http-client-send
                 client (http-client-request client "POST" "https://example.test/")
                 (ecase event
                   ((:producer :producer-failure)
                    (list :request-body-function producer :request-body-length 4))
                   (:body (list :on-body-chunk callback))
                   (:information (list :on-information callback))))
        (error (failure) (setf caught failure)))
      (ensure-true (eq condition caught))
      (ensure-equal 1 http3-calls)
      (ensure-equal 1 observations)
      (ensure-equal 0 tcp-calls))))

(deftest client-alt-svc-pre-send-fallback-preserves-stream
  (dolist (body-kind '(:producer :factory))
    (let* ((tcp-calls 0)
           (http3-calls 0)
           (producer-calls 0)
           (factory-calls 0)
           (producer
             (lambda (limit)
               (declare (ignore limit))
               (incf producer-calls)
               (ascii "body")))
           (client
             (make-http-client
              :cache nil
              :transport-function
              (lambda (request &key request-body-function &allow-other-keys)
                (declare (ignore request))
                (incf tcp-calls)
                (ensure-octets-equal (ascii "body")
                                     (funcall request-body-function 65536))
                (client-test-response 200))
              :http3-transport-function
              (lambda (request &key &allow-other-keys)
                (declare (ignore request))
                (incf http3-calls)
                (error 'http-connection-error
                       :message "synthetic pre-send failure"
                       :request-sent-p nil)))))
      (http-alternative-service-store-note-response
       (http-client-alternative-service-store client)
       "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Alt-Svc" "h3=\":443\"; ma=60"))))
      (ensure-equal
       200
       (http-response-status
        (apply #'http-client-send
               client (http-client-request client "POST" "https://example.test/")
               :request-body-length 4
               (ecase body-kind
                 (:producer (list :request-body-function producer))
                 (:factory
                  (list :request-body-factory
                        (lambda ()
                          (incf factory-calls)
                          producer)))))))
      (ensure-equal 1 tcp-calls)
      (ensure-equal 1 http3-calls)
      (ensure-equal 1 producer-calls)
      (ensure-equal (if (eq body-kind :factory) 1 0) factory-calls))))
(deftest client-alt-svc-active-cancellation-token-allows-pre-send-fallback
  (let* ((http3-calls 0)
         (tcp-calls 0)
         (token (make-http-cancellation-token))
         (client
           (make-http-client
            :cache nil
            :transport-function
            (lambda (request &key &allow-other-keys)
              (declare (ignore request))
              (incf tcp-calls)
              (client-test-response 200))
            :http3-transport-function
            (lambda (request &key &allow-other-keys)
              (declare (ignore request))
              (incf http3-calls)
              (error 'http-connection-error
                     :message "synthetic pre-send failure"
                     :request-sent-p nil)))))
    (http-alternative-service-store-note-response
     (http-client-alternative-service-store client)
     "https://example.test/"
     (client-test-response
      200 :headers (list (make-http-header "Alt-Svc" "h3=\":443\"; ma=60"))))
    (ensure-equal
     200
     (http-response-status
      (http-client-send client "GET" "https://example.test/"
                        :cancel-token token)))
    (ensure-equal 1 http3-calls)
    (ensure-equal 1 tcp-calls)))

(deftest client-alt-svc-connect-timeout-preserves-producer-and-deadline
  (dolist (body-kind '(:producer :factory))
    (let* ((tcp-calls 0)
           (http3-calls 0)
           (producer-calls 0)
           (factory-calls 0)
           (original (make-condition 'http-timeout :message "Connect expired."
                                     :operation :connect :kind :deadline))
           (producer (lambda (limit)
                       (declare (ignore limit))
                       (incf producer-calls)
                       (ascii "body")))
           (client
             (make-http-client
              :cache nil :clock-function (constantly 100)
              :wall-clock-function (constantly 100)
              :transport-function
              (lambda (request &key request-body-function deadline timeout
                                 &allow-other-keys)
                (declare (ignore request))
                (incf tcp-calls)
                (ensure-equal 110 deadline)
                (ensure-equal 10 timeout)
                (ensure-octets-equal (ascii "body")
                                     (funcall request-body-function 65536))
                (client-test-response 200))
              :http3-transport-function
              (lambda (request &key deadline timeout &allow-other-keys)
                (declare (ignore request))
                (incf http3-calls)
                (ensure-equal 109 deadline)
                (ensure-equal 10 timeout)
                (ensure-equal 0 producer-calls)
                (http-kit/http3::%http3-quic-adapter-connect
                 (lambda () (error original)))))))
      (http-alternative-service-store-note-response
       (http-client-alternative-service-store client)
       "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Alt-Svc" "h3=\":443\"; ma=60"))))
      (ensure-equal
       1 (length (http-alternative-service-store-services
                  (http-client-alternative-service-store client)
                  "https://example.test/")))
      (ensure-equal
       200 (http-response-status
            (apply #'http-client-send
                   client (http-client-request client "POST" "https://example.test/")
                   :timeout 10 :deadline 110 :request-body-length 4
                   (ecase body-kind
                     (:producer (list :request-body-function producer))
                     (:factory
                      (list :request-body-factory
                            (lambda () (incf factory-calls) producer)))))))
      (ensure-equal 1 tcp-calls)
      (ensure-equal 1 http3-calls)
      (ensure-equal 1 producer-calls)
      (ensure-equal (if (eq body-kind :factory) 1 0) factory-calls))))

(deftest client-alt-svc-quic-operation-timeouts-do-not-fallback
  (dolist (phase '(:read :write :control :settings))
    (let* ((quic (make-http3-quic-adapter-test-client))
           (stream (cl-quic-kit:client-open-stream quic nil))
           (http3 (http-kit/http3::%make-http3-client
                   :peer-control-state
                   (http-kit/http3:make-http3-control-state :peer-role :server)))
           (tcp-calls 0)
           (http3-calls 0)
           (caught nil)
           (client
             (make-http-client
              :cache nil
              :transport-function
              (lambda (request &key &allow-other-keys)
                (declare (ignore request))
                (incf tcp-calls)
                (client-test-response 200))
              :http3-transport-function
              (lambda (request &key &allow-other-keys)
                (declare (ignore request))
                (incf http3-calls)
                (ecase phase
                  (:read
                   (http-kit/http3::%http3-quic-adapter-read
                    quic nil stream :deadline 0 :poll-interval 0))
                  (:write
                   (cl-quic-kit:client-receive-frame quic (cl-quic-kit:make-frame :max-data :maximum 0) :level :1-rtt)
                   (cl-quic-kit:client-write-stream quic stream (octets 1))
                   (ensure-true (cl-quic-kit:client-stream-write-pending-p quic stream))
                   (http-kit/http3::%http3-quic-adapter-await-stream-write
                    quic stream :deadline 0))
                  (:control
                   (http-kit/http3::%http3-quic-adapter-await-peer-control-data
                    quic :deadline 0))
                  (:settings
                   (http-kit/http3::%http3-quic-adapter-await-peer-settings
                    quic http3 :deadline 0)))))))
      (unwind-protect
           (progn
             (http-alternative-service-store-note-response
              (http-client-alternative-service-store client)
              "https://example.test/"
              (client-test-response
               200 :headers (list (make-http-header "Alt-Svc" "h3=\":443\"; ma=60"))))
             (handler-case
                 (http-client-send
                  client (http-client-request client "POST" "https://example.test/"))
               (http-timeout (condition) (setf caught condition)))
             (ensure-true (typep caught 'http-timeout))
             (ensure-false (typep caught 'http-connection-error))
             (ensure-equal :deadline (http-timeout-kind caught))
             (ensure-equal :http3-quic-adapter (http-error-operation caught))
             (ensure-equal 1 http3-calls)
             (ensure-equal 0 tcp-calls))
        (http-kit/http3:close-http3-client http3)
        (cl-quic-kit:client-close quic)))))

(deftest client-connect-timeout-retry-policy-precedence
  (dolist (flags '((nil nil) (nil t) (t nil) (t t)))
    (destructuring-bind (timeout-p connection-p) flags
      (let* ((original (make-condition 'http-timeout :message "Connect expired."))
             (failure
               (handler-case
                   (http-kit/http3::%http3-quic-adapter-connect
                    (lambda () (error original)))
                 (http-timeout (condition) condition)))
             (policy (make-http-retry-policy
                      :max-attempts 2 :base-delay 0 :max-delay 0
                      :retry-on-timeout-p timeout-p
                      :retry-on-connection-error-p connection-p))
             (calls 0)
             (caught nil)
             (response nil)
             (client
               (make-http-client
                :cache nil :retry-policy policy
                :transport-function
                (lambda (request &key &allow-other-keys)
                  (declare (ignore request))
                  (incf calls)
                  (if (= calls 1) (error failure) (client-test-response 200))))))
        (ensure-true (typep failure 'http-connection-error))
        (ensure-equal timeout-p
                      (http-kit/client::%client-retryable-condition-p policy failure))
        (handler-case
            (setf response
                  (http-client-send
                   client (http-client-request client "GET" "http://example.test/")))
          (http-timeout (condition) (setf caught condition)))
        (ensure-equal (if timeout-p 2 1) calls)
        (if timeout-p
            (progn (ensure-false caught)
                   (ensure-equal 200 (http-response-status response)))
            (progn (ensure-true (eq failure caught))
                   (ensure-false response)))))))

(deftest client-explicit-http3-connect-timeout-does-not-fallback
  (let ((tcp-calls 0)
        (http3-calls 0)
        (original (make-condition 'http-timeout :message "Connect expired."))
        (caught nil))
    (let ((client
            (make-http-client
             :cache nil
             :transport-function
             (lambda (request &key &allow-other-keys)
               (declare (ignore request))
               (incf tcp-calls)
               (client-test-response 200))
             :http3-transport-function
             (lambda (request &key &allow-other-keys)
               (incf http3-calls)
               (ensure-true (http-request-http3-p request))
               (http-kit/http3::%http3-quic-adapter-connect
                (lambda () (error original)))))))
      (handler-case
          (http-client-send
           client (http-client-request client "POST" "https://example.test/"
                                       :protocol-version "HTTP/3"))
        (http-timeout (condition) (setf caught condition))))
    (ensure-true (typep caught 'http-timeout))
    (ensure-true (typep caught 'http-connection-error))
    (ensure-true (eq original (http-connection-error-cause caught)))
    (ensure-false (http-connection-error-request-sent-p caught))
    (ensure-equal 1 http3-calls)
    (ensure-equal 0 tcp-calls)))

(deftest client-alt-svc-without-http3-uses-tcp
  (let ((tcp-calls 0))
    (let ((client
            (make-http-client
             :cache nil
             :http3-transport-function nil
             :transport-function
             (lambda (request &key &allow-other-keys)
               (ensure-false (http-request-http3-p request))
               (incf tcp-calls)
               (client-test-response 200)))))
      (http-alternative-service-store-note-response
       (http-client-alternative-service-store client)
       "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Alt-Svc" "h3=\":443\"; ma=60"))))
      (ensure-equal 200 (http-response-status
                         (http-client-send
                          client (http-client-request
                                  client "GET" "https://example.test/")))))
    (ensure-equal 1 tcp-calls)))

(deftest client-explicit-http3-does-not-fallback
  (let ((tcp-calls 0)
        (http3-calls 0))
    (let ((client
            (make-http-client
             :cache nil
             :transport-function
             (lambda (request &key &allow-other-keys)
               (declare (ignore request))
               (incf tcp-calls)
               (client-test-response 200 :body (ascii "tcp")))
             :http3-transport-function
             (lambda (request &key alternative-service &allow-other-keys)
               (declare (ignore alternative-service))
               (incf http3-calls)
               (ensure-true (http-request-http3-p request))
               (error 'http-connection-error
                      :message "synthetic explicit HTTP/3 failure."
                      :operation :http3)))))
      (signals http-connection-error
        (http-client-send
         client
         (http-client-request client "GET" "https://example.test/"
                              :protocol-version "HTTP/3"))))
    (ensure-equal 1 http3-calls)
    (ensure-equal 0 tcp-calls)))

(deftest client-cache-integration
  (let ((calls 0)
        (cache (make-http-cache :clock-function (lambda () 1000))))
    (let* ((client
             (make-http-client
              :cache cache
              :automatic-decompression-p nil
              :transport-function
              (lambda (request &key proxy-plan &allow-other-keys)
                (declare (ignore request proxy-plan))
                (incf calls)
                (client-test-response
                 200
                 :headers (list (make-http-header
                                 "Cache-Control" "max-age=60"))
                 :body (ascii "cached")))))
           (request (http-client-request client "GET"
                                         "http://example.test/resource")))
      (multiple-value-bind (response effective)
          (http-client-send client request)
        (ensure-equal 200 (http-response-status response))
        (ensure-equal request effective))
      (multiple-value-bind (response effective)
          (http-client-send client request)
        (ensure-equal 200 (http-response-status response))
        (ensure-equal request effective))
      (ensure-equal 1 calls)
      (ensure-equal 1 (length (http-cache-entries cache))))))

(deftest client-cache-status-validates-identifier
  (let ((cache (make-http-cache :status-identifier "edge \"one\"")))
    (ensure-equal "edge \"one\"" (http-cache-status-identifier cache)))
  (signals http-protocol-error
    (make-http-cache :status-identifier ""))
  (signals http-protocol-error
    (make-http-cache :status-identifier (format nil "edge~%one")))
  (signals http-protocol-error
    (make-http-cache :status-identifier "edge-λ")))

(deftest client-cache-status-reports-forward-and-hit
  (let ((calls 0)
        (cache (make-http-cache :clock-function (lambda () 1000)
                                :status-identifier "edge \"one\"")))
    (let* ((client
             (make-http-client
              :cache cache
              :transport-function
              (lambda (request &key &allow-other-keys)
                (declare (ignore request))
                (incf calls)
                (client-test-response
                 200
                 :headers
                 (list (make-http-header "Cache-Status" "origin; hit")
                       (make-http-header "Cache-Control" "max-age=60"))
                 :body (ascii "cached")))))
           (uri "http://example.test/cache-status")
           (request (http-client-request client "GET" uri))
           (reload
             (http-client-request
              client "GET" uri
              :headers (list (make-http-header "Cache-Control" "no-cache")))))
      (let ((response (http-client-send client request)))
        (ensure-equal
         '("origin; hit" "\"edge \\\"one\\\"\"; fwd=miss; stored")
         (http-header-values (http-response-headers response)
                             "Cache-Status")))
      (let ((response (http-client-send client request)))
        (ensure-equal
         '("origin; hit" "\"edge \\\"one\\\"\"; hit; ttl=60")
         (http-header-values (http-response-headers response)
                             "Cache-Status")))
      (let ((response (http-client-send client reload)))
        (ensure-equal
         '("origin; hit" "\"edge \\\"one\\\"\"; fwd=request; stored")
         (http-header-values (http-response-headers response)
                             "Cache-Status")))
      (ensure-equal 2 calls))))

(deftest client-cache-merges-identified-304-metadata-safely
  (let ((calls 0)
        (now 1000))
    (let* ((cache (make-http-cache :clock-function (lambda () now)
                                   :status-identifier "edge"))
           (client
             (make-http-client
              :cache cache
              :transport-function
              (lambda (request &key &allow-other-keys)
                (incf calls)
                (if (= calls 1)
                    (make-http-response
                     :status 200
                     :headers
                     (list (make-http-header "Cache-Control" "max-age=0")
                           (make-http-header "ETag" "\"v1\"")
                           (make-http-header "Content-Length" "6")
                           (make-http-header "Content-Type" "text/plain"))
                     :trailers (list (make-http-header "X-Saved" "yes"))
                     :body (ascii "cached"))
                    (progn
                      (ensure-equal
                       "\"v1\""
                       (http-header-value (http-request-headers request)
                                          "If-None-Match"))
                      (client-test-response
                       304
                       :headers
                       (list
                        (make-http-header "Cache-Control" "max-age=60")
                        (make-http-header "ETag" "\"v1\"")
                        (make-http-header "Content-Length" "999")
                        (make-http-header "Content-Encoding" "gzip")
                        (make-http-header "Content-Type"
                                          "application/octet-stream")
                        (make-http-header "Connection" "X-Hop")
                        (make-http-header "X-Hop" "discard"))))))))
           (request (http-client-request
                     client "GET" "http://example.test/revalidate")))
      (http-client-send client request)
      (setf now 1001)
      (let ((response (http-client-send client request)))
        (ensure-equal 200 (http-response-status response))
        (ensure-equal
         '("\"edge\"; fwd=stale; fwd-status=304; stored")
         (http-header-values (http-response-headers response)
                             "Cache-Status"))
        (ensure-equal (ascii "cached") (http-response-body response))
        (ensure-equal "6" (http-header-value
                            (http-response-headers response)
                            "Content-Length"))
        (ensure-equal "application/octet-stream"
                      (http-header-value (http-response-headers response)
                                         "Content-Type"))
        (ensure-false (http-header-present-p (http-response-headers response)
                                             "Content-Encoding"))
        (ensure-false (http-header-present-p (http-response-headers response)
                                             "X-Hop"))
        (ensure-equal "yes" (http-header-value
                              (http-response-trailers response)
                              "X-Saved")))
      (let ((response (http-client-send client request)))
        (ensure-equal '("\"edge\"; hit; ttl=60")
                      (http-header-values (http-response-headers response)
                                          "Cache-Status")))
      (ensure-equal 2 calls))))

(deftest client-cache-304-honors-no-cache-fields
  (let* ((cached
           (client-test-response
            200
            :headers (list (make-http-header "ETag" "\"v1\"")
                           (make-http-header "Set-Cookie" "old=value"))))
         (updated
           (http-kit/client::%client-response-merge-304
            cached
            (client-test-response
             304
             :headers
             (list (make-http-header "ETag" "\"v1\"")
                   (make-http-header
                    "Cache-Control"
                    "max-age=60, no-cache=\"Set-Cookie, X-Secret\"")
                   (make-http-header "Set-Cookie" "session=secret")
                   (make-http-header "X-Secret" "discard"))))))
    (ensure-equal "old=value"
                  (http-header-value (http-response-headers updated)
                                     "Set-Cookie"))
    (ensure-false (http-header-present-p (http-response-headers updated)
                                         "X-Secret"))))

(deftest client-cache-304-no-store-removes-existing-entry
  (let ((calls 0)
        (now 1000))
    (let* ((cache (make-http-cache :clock-function (lambda () now)))
           (client
             (make-http-client
              :cache cache
              :transport-function
              (lambda (request &key &allow-other-keys)
                (declare (ignore request))
                (incf calls)
                (if (= calls 1)
                    (client-test-response
                     200
                     :headers (list (make-http-header "ETag" "\"v1\"")
                                    (make-http-header "Cache-Control"
                                                      "max-age=0"))
                     :body (ascii "cached"))
                    (client-test-response
                     304
                     :headers (list (make-http-header "ETag" "\"v1\"")
                                    (make-http-header "Cache-Control"
                                                      "no-store")))))))
           (request (http-client-request
                     client "GET" "http://example.test/no-store-304")))
      (http-client-send client request)
      (setf now 1001)
      (ensure-equal 200 (http-response-status
                         (http-client-send client request)))
      (ensure-equal 2 calls)
      (ensure-equal 0 (length (http-cache-entries cache))))))

(deftest client-cache-rejects-304-for-another-validator
  (signals http-protocol-error
    (http-kit/client::%client-response-merge-304
     (client-test-response
      200
      :headers (list (make-http-header "ETag" "\"cached\""))
      :body (ascii "cached"))
     (client-test-response
      304
      :headers (list (make-http-header "ETag" "\"other\""))))))

(deftest client-cache-invalidates-successful-unsafe-responses
  (let* ((cache (make-http-cache :clock-function (lambda () 1000)))
         (headers (list (make-http-header "Cache-Control" "max-age=60")))
         (target (make-http-request
                  :method "GET" :uri "http://example.test/resource"))
         (location (make-http-request
                    :method "GET" :uri "http://example.test/location"))
         (content-location
           (make-http-request
            :method "GET" :uri "http://example.test/content"))
         (external (make-http-request
                    :method "GET" :uri "http://other.test/content")))
    (flet ((store (request)
             (http-cache-store cache request
                               (client-test-response 200 :headers headers)
                               :now 1000))
           (state (request)
             (nth-value 1 (http-cache-lookup cache request :now 1001))))
      (dolist (request (list target location content-location external))
        (store request))
      (let ((client
              (make-http-client
               :cache cache
               :transport-function
               (lambda (request &key &allow-other-keys)
                 (declare (ignore request))
                 (client-test-response 400)))))
        (http-client-send
         client
         (http-client-request client "POST" "http://example.test/resource")))
      (ensure-equal :fresh (state target))
      (let ((client
              (make-http-client
               :cache cache
               :transport-function
               (lambda (request &key &allow-other-keys)
                 (declare (ignore request))
                 (client-test-response 204)))))
        (http-client-send
         client
         (http-client-request client "OPTIONS"
                              "http://example.test/resource")))
      (ensure-equal :fresh (state target))
      (let ((client
              (make-http-client
               :cache cache
               :transport-function
               (lambda (request &key &allow-other-keys)
                 (declare (ignore request))
                 (client-test-response
                  204
                  :headers
                  (list (make-http-header "Location" "/location")
                        (make-http-header "Content-Location" "/content")
                        (make-http-header "Content-Location"
                                          "http://other.test/content")))))))
        (http-client-send
         client
         (http-client-request client "POST" "http://example.test/resource")))
      (ensure-equal :miss (state target))
      (ensure-equal :miss (state location))
      (ensure-equal :miss (state content-location))
      (ensure-equal :fresh (state external)))))

(deftest client-cache-keeps-get-when-head-is-stored
  (let* ((cache (make-http-cache :clock-function (lambda () 1000)))
         (get-request (make-http-request
                       :method "GET"
                       :uri "http://example.test/resource"))
         (head-request (make-http-request
                        :method "HEAD"
                        :uri "http://example.test/resource"))
         (headers (list (make-http-header "Cache-Control" "max-age=60"))))
    (http-cache-store cache get-request
                      (client-test-response 200
                                             :headers headers
                                             :body (ascii "get"))
                      :now 1000)
    (http-cache-store cache head-request
                      (client-test-response 200 :headers headers)
                      :now 1000)
    (multiple-value-bind (response state entry)
        (http-cache-lookup cache get-request :now 1001)
      (declare (ignore entry))
      (ensure-equal :fresh state)
      (ensure-equal "get"
                    (map 'string #'code-char (http-response-body response))))
    (multiple-value-bind (response state entry)
        (http-cache-lookup cache head-request :now 1001)
      (declare (ignore entry))
      (ensure-equal :fresh state)
      (ensure-equal "HEAD" (http-request-method head-request))
      (ensure-equal 200 (http-response-status response)))
    (ensure-equal 2 (length (http-cache-entries cache)))))

(deftest client-cache-keeps-vary-variants
  (let* ((cache (make-http-cache :clock-function (lambda () 1000)))
         (headers (list (make-http-header "Cache-Control" "max-age=60")
                        (make-http-header "Vary" "Accept-Language")))
         (english-request
           (make-http-request
            :method "GET"
            :uri "http://example.test/resource"
            :headers (list (make-http-header "Accept-Language" "en"))))
         (french-request
           (make-http-request
            :method "GET"
            :uri "http://example.test/resource"
            :headers (list (make-http-header "Accept-Language" "fr"))))
         (german-request
           (make-http-request
            :method "GET"
            :uri "http://example.test/resource"
            :headers (list (make-http-header "Accept-Language" "de")))))
    (http-cache-store cache english-request
                      (client-test-response 200
                                            :headers headers
                                            :body (ascii "English"))
                      :now 1000)
    (http-cache-store cache french-request
                      (client-test-response 200
                                            :headers headers
                                            :body (ascii "French"))
                      :now 1000)
    (http-cache-store cache english-request
                      (client-test-response 200
                                            :headers headers
                                            :body (ascii "Updated English"))
                      :now 1001)
    (dolist (request-and-body (list (cons english-request "Updated English")
                                    (cons french-request "French")))
      (multiple-value-bind (response state entry)
          (http-cache-lookup cache (car request-and-body) :now 1002)
        (declare (ignore entry))
        (ensure-equal :fresh state)
        (ensure-equal (cdr request-and-body)
                      (map 'string #'code-char
                           (http-response-body response)))))
    (multiple-value-bind (response state entry)
        (http-cache-lookup cache german-request :now 1002)
      (ensure-equal nil response)
      (ensure-equal :miss state)
      (ensure-equal nil entry))
    (ensure-equal 2 (length (http-cache-entries cache)))))

(deftest client-cache-honors-valueless-control-directives
  (let* ((cache (make-http-cache :clock-function (lambda () 1000)))
         (uri "http://example.test/private")
         (request (make-http-request :method "GET" :uri uri))
         (no-store-request
           (make-http-request
            :method "GET"
            :uri uri
            :headers (list (make-http-header "Cache-Control" "no-store"))))
         (no-cache-request
           (make-http-request
            :method "GET"
            :uri uri
            :headers (list (make-http-header "Cache-Control" "no-cache"))))
         (pragma-no-cache-request
           (make-http-request
            :method "GET"
            :uri uri
            :headers (list (make-http-header "Pragma" "foo, no-cache"))))
         (cache-control-overrides-pragma-request
           (make-http-request
            :method "GET"
            :uri uri
            :headers (list (make-http-header "Cache-Control" "max-age=60")
                           (make-http-header "Pragma" "no-cache"))))
         (fresh-response
           (client-test-response
            200
            :headers (list (make-http-header "Cache-Control" "max-age=60"))
            :body (ascii "secret"))))
    (ensure-equal nil
                  (http-cache-store
                   cache request
                   (client-test-response
                    200
                    :headers (list (make-http-header "Cache-Control"
                                                    "no-store")))))
    (ensure-equal 0 (length (http-cache-entries cache)))
    (http-cache-store cache request fresh-response :now 1000)
    (dolist (controlled-request
             (list no-cache-request pragma-no-cache-request no-store-request))
      (multiple-value-bind (response state entry)
          (http-cache-lookup cache controlled-request :now 1001)
        (ensure-equal nil response)
        (ensure-equal (if (eq controlled-request no-store-request)
                          :miss
                          :stale)
                      state)
        (ensure-true (or entry (eq state :miss)))))
    (multiple-value-bind (response state entry)
        (http-cache-lookup cache cache-control-overrides-pragma-request
                           :now 1001)
      (ensure-true response)
      (ensure-equal :fresh state)
      (ensure-true entry))
    (ensure-equal nil
                  (http-cache-store cache no-store-request fresh-response
                                    :now 1001))
    (ensure-equal 1 (length (http-cache-entries cache)))))

(deftest client-private-cache-stores-authenticated-cookie-responses
  (let* ((cache (make-http-cache))
         (request
           (make-http-request
            :method "GET"
            :uri "http://example.test/account"
            :headers (list (make-http-header "Authorization" "Bearer token"))))
         (response
           (client-test-response
            200
            :headers (list (make-http-header "Cache-Control" "private, max-age=60")
                           (make-http-header "Set-Cookie" "session=updated"))
            :body (ascii "account"))))
    (ensure-true (http-cache-store cache request response :now 1000))
    (multiple-value-bind (cached state entry)
        (http-cache-lookup cache request :now 1001)
      (declare (ignore entry))
      (ensure-equal :fresh state)
      (ensure-equal (ascii "account") (http-response-body cached)))))

(deftest client-private-cache-partitions-credentialed-responses
  (let* ((cache (make-http-cache))
         (alice (make-http-request
                 :method "GET" :uri "http://example.test/account"
                 :headers (list (make-http-header "Authorization"
                                                  "Bearer alice"))))
         (bob (make-http-request
               :method "GET" :uri "http://example.test/account"
               :headers (list (make-http-header "Authorization"
                                                "Bearer bob"))))
         (anonymous (make-http-request
                     :method "GET" :uri "http://example.test/account"))
         (response
           (client-test-response
            200
            :headers (list (make-http-header "Cache-Control"
                                             "private, max-age=60"))
            :body (ascii "alice-account"))))
    (ensure-true (http-cache-store cache alice response :now 1000))
    (dolist (request (list bob anonymous))
      (multiple-value-bind (cached state entry)
          (http-cache-lookup cache request :now 1001)
        (ensure-equal nil cached)
        (ensure-equal :miss state)
        (ensure-equal nil entry)))
    (let ((public-request
            (make-http-request
             :method "GET" :uri "http://example.test/public"))
          (authenticated-public-request
            (make-http-request
             :method "GET" :uri "http://example.test/public"
             :headers (list (make-http-header "Authorization"
                                              "Bearer alice")))))
      (ensure-true (http-cache-store cache public-request response :now 1000))
      (multiple-value-bind (cached state entry)
          (http-cache-lookup cache authenticated-public-request :now 1001)
        (ensure-equal nil cached)
        (ensure-equal :miss state)
        (ensure-equal nil entry)))))

(deftest client-cache-observes-status-cacheability
  (let ((cache (make-http-cache))
        (request (make-http-request :method "GET"
                                    :uri "http://example.test/status")))
    (dolist (status '(405 414 501))
      (ensure-true
       (http-cache-store cache request (client-test-response status)
                         :now 1000)))
    (ensure-equal nil
                  (http-cache-store cache request
                                    (client-test-response 302)
                                    :now 1000))
    (ensure-true
     (http-cache-store
      cache request
      (client-test-response
       302
       :headers (list (make-http-header "Cache-Control" "max-age=60")))
      :now 1000))
    (ensure-equal nil
                  (http-cache-store cache request
                                    (client-test-response 418)
                                    :now 1000))
    (ensure-true
     (http-cache-store
      cache request
      (client-test-response
       418
       :headers (list (make-http-header "Cache-Control" "public")))
      :now 1000))
    (ensure-equal
     nil
     (http-cache-store
      cache request
      (client-test-response
       599
       :headers (list (make-http-header
                       "Cache-Control" "public, must-understand")))
      :now 1000))
    (ensure-true
     (http-cache-store
      cache request
      (client-test-response
       200
       :headers (list (make-http-header
                       "Cache-Control"
                       "max-age=60, must-understand, no-store")))
      :now 1000))
    (ensure-equal
     nil
     (http-cache-store
      cache request
      (client-test-response
       599
       :headers (list (make-http-header
                       "Cache-Control"
                       "public, must-understand, no-store")))
      :now 1000))))

(deftest client-cache-does-not-store-connection-or-proxy-fields
  (let* ((cache (make-http-cache))
         (request
           (make-http-request :method "GET"
                              :uri "http://example.test/metadata"))
         (response
           (client-test-response
            200
            :headers
            (list (make-http-header "Cache-Control" "max-age=60")
                  (make-http-header "Content-Type" "text/plain")
                  (make-http-header "Connection" "X-Hop")
                  (make-http-header "X-Hop" "connection-local")
                  (make-http-header "Keep-Alive" "timeout=5")
                  (make-http-header "Proxy-Authenticate" "Basic")
                  (make-http-header "Proxy-Authentication-Info" "nextnonce=x")
                  (make-http-header "Proxy-Authorization" "Basic secret"))
            :body (ascii "stored"))))
    (ensure-true (http-cache-store cache request response :now 1000))
    (multiple-value-bind (cached state entry)
        (http-cache-lookup cache request :now 1001)
      (declare (ignore entry))
      (let ((headers (http-response-headers cached)))
        (ensure-equal :fresh state)
        (ensure-equal "text/plain" (http-header-value headers "Content-Type"))
        (dolist (name '("Connection" "X-Hop" "Keep-Alive"
                        "Proxy-Authenticate" "Proxy-Authentication-Info"
                        "Proxy-Authorization"))
          (ensure-false (http-header-present-p headers name)))))))

(deftest client-cache-does-not-store-no-cache-field-list
  (let* ((cache (make-http-cache))
         (request
           (make-http-request :method "GET"
                              :uri "http://example.test/sensitive-metadata"))
         (response
           (client-test-response
            200
            :headers
            (list (make-http-header
                   "Cache-Control"
                   "max-age=60, no-cache=\"Set-Cookie, X-Secret\"")
                  (make-http-header "Content-Type" "text/plain")
                  (make-http-header "Set-Cookie" "session=secret")
                  (make-http-header "X-Secret" "private"))
            :body (ascii "stored")))
         (entry (http-cache-store cache request response :now 1000))
         (headers
           (http-response-headers (http-cache-entry-response entry))))
    (ensure-true entry)
    (ensure-equal "text/plain" (http-header-value headers "Content-Type"))
    (ensure-false (http-header-present-p headers "Set-Cookie"))
    (ensure-false (http-header-present-p headers "X-Secret"))
    (ensure-equal :stale
                  (nth-value 1
                             (http-cache-lookup cache request :now 1001)))))

(deftest client-cache-does-not-reuse-partial-responses
  (let* ((cache (make-http-cache :clock-function (lambda () 1000)))
         (uri "http://example.test/resource")
         (request (make-http-request :method "GET" :uri uri))
         (range-request
           (make-http-request
            :method "GET"
            :uri uri
            :headers (list (make-http-header "Range" "bytes=0-4"))))
         (fresh-headers
           (list (make-http-header "Cache-Control" "max-age=60"))))
    (ensure-equal nil
                  (http-cache-store
                   cache request
                   (client-test-response 206
                                         :headers fresh-headers
                                         :body (ascii "part"))
                   :now 1000))
    (ensure-equal nil
                  (http-cache-store
                   cache range-request
                   (client-test-response 200
                                         :headers fresh-headers
                                         :body (ascii "whole"))
                   :now 1000))
    (http-cache-store cache request
                      (client-test-response 200
                                            :headers fresh-headers
                                            :body (ascii "whole"))
                      :now 1000)
    (multiple-value-bind (response state entry)
        (http-cache-lookup cache range-request :now 1001)
      (ensure-equal nil response)
      (ensure-equal :miss state)
      (ensure-equal nil entry))
    (ensure-true
     (http-cache-store cache request
                       (client-test-response 308 :headers fresh-headers)
                       :now 1001))))

(deftest client-http-date-grammar
  (let* ((expected (encode-universal-time 37 49 8 6 11 1994 0))
         (now (encode-universal-time 0 0 0 1 1 2026 0)))
    (ensure-equal expected
                  (http-parse-date "Sun, 06 Nov 1994 08:49:37 GMT"))
    (ensure-equal expected
                  (http-parse-date "Sunday, 06-Nov-94 08:49:37 GMT"
                                   :now now))
    (ensure-equal expected
                  (http-parse-date "Sun Nov  6 08:49:37 1994"))
    (dolist (invalid '("Sun, 06 Nov 1994 08:49:37 PST"
                       "Sun, 06 nov 1994 08:49:37 GMT"
                       "Sun, 6 Nov 1994 08:49:37 GMT"
                       "Sun,\t06 Nov 1994 08:49:37 GMT"
                       "sun Nov  6 08:49:37 1994"
                       "Sun Nov 6 08:49:37 1994"))
      (ensure-equal nil (http-parse-date invalid :now now)))
    (ensure-equal 2076
                  (nth-value
                   5
                   (decode-universal-time
                    (http-parse-date "Sunday, 06-Nov-76 08:49:37 GMT"
                                     :now now)
                    0)))
    (ensure-equal 1977
                  (nth-value
                   5
                   (decode-universal-time
                    (http-parse-date "Sunday, 06-Nov-77 08:49:37 GMT"
                                     :now now)
                    0)))))

(deftest client-http-date-calendar-boundaries
  (loop for month in '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                      "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
        for month-number from 1
        for last-day in '(31 29 31 30 31 30 31 31 30 31 30 31)
        do (ensure-equal
            (encode-universal-time 59 59 23 last-day month-number 2000 0)
            (http-parse-date
             (format nil "Sun, ~2,'0D ~A 2000 23:59:59 GMT" last-day month)))
           (ensure-equal
            nil
            (http-parse-date
             (format nil "Sun, ~2,'0D ~A 2000 23:59:59 GMT"
                     (1+ last-day) month))))
  (dolist (year '(1900 1999 2100))
    (ensure-equal
     (encode-universal-time 0 0 0 28 2 year 0)
     (http-parse-date (format nil "Sun, 28 Feb ~D 00:00:00 GMT" year)))
    (ensure-equal
     nil
     (http-parse-date (format nil "Sun, 29 Feb ~D 00:00:00 GMT" year))))
  (ensure-equal (encode-universal-time 0 0 0 29 2 1996 0)
                (http-parse-date "Sun, 29 Feb 1996 00:00:00 GMT"))
  (ensure-equal 0 (http-parse-date "Mon, 01 Jan 1900 00:00:00 GMT"))
  (ensure-equal (encode-universal-time 59 59 23 31 12 9999 0)
                (http-parse-date "Fri, 31 Dec 9999 23:59:59 GMT"))
  (dolist (invalid '("Sun, 00 Jan 2000 00:00:00 GMT"
                     "Sun, 01 Jan 1899 00:00:00 GMT"
                     "Sun, 01 Jan 10000 00:00:00 GMT"
                     "Sun, 01 Foo 2000 00:00:00 GMT"))
    (ensure-equal nil (http-parse-date invalid))))

(deftest client-http-date-time-boundaries
  (ensure-equal (encode-universal-time 59 59 23 31 12 2016 0)
                (http-parse-date "Sat, 31 Dec 2016 23:59:60 GMT"))
  (dolist (invalid '("Sat, 31 Dec 2016 24:00:00 GMT"
                     "Sat, 31 Dec 2016 23:60:00 GMT"
                     "Sat, 31 Dec 2016 23:59:61 GMT"
                     "Sat, 31 Dec 2016 3:59:59 GMT"
                     "Sat, 31 Dec 2016 23-59:59 GMT"
                     "Sat, 31 Dec 2016 23:59-59 GMT"
                     "Sat, 31 Dec 2016 23:59:5X GMT"
                     "Sat, 31 Dec 2016 23:59:59 GMT "
                     " Sat, 31 Dec 2016 23:59:59 GMT"))
    (ensure-equal nil (http-parse-date invalid)))
  (dolist (value '(nil 0 #() #\A))
    (ensure-equal nil (http-parse-date value))))

(deftest client-cache-uses-corrected-response-age
  (let* ((cache (make-http-cache))
         (uri "http://example.test/aged")
         (request (make-http-request :method "GET" :uri uri))
         (request-max-age
           (make-http-request
            :method "GET"
            :uri uri
            :headers (list (make-http-header "Cache-Control" "max-age=30"))))
         (now (http-parse-date "Sun, 06 Nov 1994 08:49:57 GMT")))
    (http-cache-store
     cache request
     (client-test-response
      200
      :headers (list (make-http-header "Cache-Control" "max-age=30")
                     (make-http-header "Date"
                                       "Sun, 06 Nov 1994 08:49:37 GMT")
                     (make-http-header "Age" "10")))
     :now now)
    (multiple-value-bind (response state entry)
        (http-cache-lookup cache request :now (+ now 9))
      (declare (ignore entry))
      (ensure-equal :fresh state)
      (ensure-equal "29"
                    (http-header-value
                     (http-response-headers response) "Age")))
    (multiple-value-bind (response state entry)
        (http-cache-lookup cache request :now (+ now 10))
      (declare (ignore response entry))
      (ensure-equal :stale state))
    (http-cache-store
     cache request
     (client-test-response
      200
      :headers (list (make-http-header "Cache-Control" "max-age=60")
                     (make-http-header "Date"
                                       "Sun, 06 Nov 1994 08:49:57 GMT")
                     (make-http-header "Age" "40")))
     :now now)
    (multiple-value-bind (response state entry)
        (http-cache-lookup cache request-max-age :now now)
      (declare (ignore response entry))
      (ensure-equal :stale state))))

(deftest client-cache-adds-response-delay-to-age
  (let* ((cache (make-http-cache))
         (request
           (make-http-request :method "GET"
                              :uri "http://example.test/delayed-age"))
         (response-time
           (http-parse-date "Sun, 06 Nov 1994 08:49:57 GMT")))
    (http-cache-store
     cache request
     (client-test-response
      200
      :headers (list (make-http-header "Cache-Control" "max-age=35")
                     (make-http-header "Date"
                                       "Sun, 06 Nov 1994 08:49:57 GMT")
                     (make-http-header "Age" "20")))
     :now response-time
     :request-time (- response-time 10)
     :response-time response-time)
    (multiple-value-bind (response state entry)
        (http-cache-lookup cache request :now (+ response-time 4))
      (declare (ignore entry))
      (ensure-equal :fresh state)
      (ensure-equal "34"
                    (http-header-value
                     (http-response-headers response) "Age")))
    (ensure-equal :stale
                  (nth-value 1
                             (http-cache-lookup
                              cache request :now (+ response-time 5))))))

(deftest client-cache-treats-invalid-response-max-age-as-stale
  (let ((request
          (make-http-request :method "GET"
                             :uri "http://example.test/invalid-max-age")))
    (dolist (cache-control '("max-age=invalid"
                            "max-age=-1"
                            "max-age=60, max-age=120"))
      (let ((cache (make-http-cache)))
        (ensure-true
         (http-cache-store
          cache request
          (client-test-response
           200
           :headers
           (list (make-http-header "Cache-Control" cache-control)
                 (make-http-header "Date"
                                   "Sun, 06 Nov 1994 08:49:37 GMT")
                 (make-http-header "Expires"
                                   "Sun, 06 Nov 1994 09:49:37 GMT")))
          :now 1000))
        (ensure-equal :stale
                      (nth-value 1
                                 (http-cache-lookup cache request
                                                    :now 1000)))))))

(deftest client-cache-honors-freshness-request-directives
  (let* ((cache (make-http-cache))
         (uri "http://example.test/freshness")
         (plain (make-http-request :method "GET" :uri uri))
         (min-fresh
           (make-http-request
            :method "GET" :uri uri
            :headers (list (make-http-header "Cache-Control" "min-fresh=5"))))
         (max-stale-five
           (make-http-request
            :method "GET" :uri uri
            :headers (list (make-http-header "Cache-Control" "max-stale=5"))))
         (max-stale-four
           (make-http-request
            :method "GET" :uri uri
            :headers (list (make-http-header "Cache-Control" "max-stale=4"))))
         (max-stale-any
           (make-http-request
            :method "GET" :uri uri
            :headers (list (make-http-header "Cache-Control" "max-stale")))))
    (http-cache-store
     cache plain
     (client-test-response
      200 :headers (list (make-http-header "Cache-Control" "max-age=10")))
     :now 1000)
    (ensure-equal :fresh
                  (nth-value 1
                             (http-cache-lookup cache min-fresh :now 1005)))
    (ensure-equal :stale
                  (nth-value 1
                             (http-cache-lookup cache min-fresh :now 1006)))
    (multiple-value-bind (response state)
        (http-cache-lookup cache max-stale-five :now 1015)
      (ensure-equal :stale-allowed state)
      (ensure-true response)
      (ensure-equal "15" (http-header-value
                           (http-response-headers response) "Age")))
    (multiple-value-bind (response state)
        (http-cache-lookup cache max-stale-four :now 1015)
      (ensure-equal :stale state)
      (ensure-true (null response)))
    (multiple-value-bind (response state)
        (http-cache-lookup cache max-stale-any :now 1100)
      (ensure-equal :stale-allowed state)
      (ensure-true response)
      (ensure-equal "100" (http-header-value
                            (http-response-headers response) "Age")))
    (http-cache-store
     cache plain
     (client-test-response
      200
      :headers (list (make-http-header
                      "Cache-Control" "max-age=10, must-revalidate")))
     :now 2000)
    (ensure-equal :stale
                  (nth-value 1
                             (http-cache-lookup cache max-stale-five
                                                :now 2011)))
    (http-cache-store
     cache plain
     (client-test-response
      200
      :headers (list (make-http-header
                      "Cache-Control" "max-age=10, no-cache")))
     :now 3000)
    (ensure-equal :stale
                  (nth-value 1
                             (http-cache-lookup cache max-stale-any
                                                :now 3001)))))

(deftest client-cache-honors-restrictive-duplicate-request-directives
  (let* ((cache (make-http-cache))
         (uri "http://example.test/duplicate-request-directives")
         (plain (make-http-request :method "GET" :uri uri)))
    (http-cache-store
     cache plain
     (client-test-response
      200 :headers (list (make-http-header "Cache-Control" "max-age=10")))
     :now 1000)
    (dolist (cache-control '("max-age=60, max-age=0"
                             "max-age=0, max-age=60"
                             "min-fresh=1, min-fresh=10"
                             "min-fresh=10, min-fresh=1"))
      (let ((request
              (make-http-request
               :method "GET" :uri uri
               :headers (list (make-http-header "Cache-Control"
                                                cache-control)))))
        (multiple-value-bind (response state)
            (http-cache-lookup cache request :now 1001)
          (ensure-equal :stale state)
          (ensure-true (null response)))))
    (dolist (cache-control '("max-stale, max-stale=4"
                             "max-stale=4, max-stale"
                             "max-stale, max-stale=invalid"))
      (let ((request
              (make-http-request
               :method "GET" :uri uri
               :headers (list (make-http-header "Cache-Control"
                                                cache-control)))))
        (multiple-value-bind (response state)
            (http-cache-lookup cache request :now 1015)
          (ensure-equal :stale state)
          (ensure-true (null response)))))))

(deftest client-cache-serves-request-authorized-stale-response
  (let ((calls 0)
        (now 1000))
    (let* ((cache (make-http-cache :clock-function (lambda () now)))
           (client
             (make-http-client
              :cache cache
              :transport-function
              (lambda (request &key &allow-other-keys)
                (declare (ignore request))
                (incf calls)
                (client-test-response
                 200
                 :headers (list (make-http-header
                                 "Cache-Control" "max-age=10"))
                 :body (ascii "cached")))))
           (uri "http://example.test/allowed-stale"))
      (http-client-send client (http-client-request client "GET" uri))
      (setf now 1015)
      (let ((response
              (http-client-send
               client
               (http-client-request
                client "GET" uri
                :headers (list (make-http-header
                                "Cache-Control" "max-stale=5"))))))
        (ensure-equal 1 calls)
        (ensure-octets-equal (ascii "cached") (http-response-body response))
        (ensure-equal "15" (http-header-value
                            (http-response-headers response) "Age"))))))

(deftest client-cache-honors-immutable-on-authenticated-reloads
  (dolist (case '(("https://example.test/immutable" "immutable" nil :fresh)
                  ("https://example.test/immutable-argument"
                   "immutable=ignored" nil :fresh)
                  ("http://example.test/immutable" "immutable" nil :stale)
                  ("https://example.test/force-reload"
                   "immutable" "no-cache" :stale)))
    (destructuring-bind (uri immutable request-directive expected-state) case
      (let* ((cache (make-http-cache))
             (plain (make-http-request :method "GET" :uri uri))
             (reload
               (make-http-request
                :method "GET"
                :uri uri
                :headers
                (list (make-http-header
                       "Cache-Control"
                       (or request-directive "max-age=0"))))))
        (http-cache-store
         cache plain
         (client-test-response
          200
          :headers
          (list (make-http-header
                 "Cache-Control"
                 (format nil "max-age=60, ~A" immutable))))
         :now 1000)
        (ensure-equal expected-state
                      (nth-value 1
                                 (http-cache-lookup cache reload :now 1001)))
        (ensure-equal :stale
                      (nth-value 1
                                 (http-cache-lookup cache reload :now 1060)))))))

(deftest client-cache-only-request-never-uses-transport
  (let ((calls 0)
        (cache (make-http-cache)))
    (let* ((client
             (make-http-client
              :cache cache
              :transport-function
              (lambda (request &key &allow-other-keys)
                (declare (ignore request))
                (incf calls)
                (client-test-response 200))))
           (request
             (http-client-request
              client "GET" "http://example.test/missing"
              :headers (list (make-http-header
                              "Cache-Control" "only-if-cached")))))
      (multiple-value-bind (response effective-request)
          (http-client-send client request)
        (ensure-equal 504 (http-response-status response))
        (ensure-equal "http://example.test/missing"
                      (http-uri-string
                       (http-request-uri effective-request))))
      (ensure-equal 0 calls))))
(deftest client-only-if-cached-without-cache-preserves-effective-request
  (let* ((calls 0)
        (client
          (make-http-client
           :cache nil
           :transport-function
           (lambda (request &key &allow-other-keys)
             (declare (ignore request))
             (incf calls)
             (client-test-response 200))))
        (request
          (http-client-request
           client "GET" "http://example.test/missing-without-cache"
           :headers (list (make-http-header
                           "Cache-Control" "only-if-cached")))))
    (multiple-value-bind (response effective-request)
        (http-client-send client request)
      (ensure-equal 504 (http-response-status response))
      (ensure-equal "http://example.test/missing-without-cache"
                    (http-uri-string
                     (http-request-uri effective-request))))
    (ensure-equal 0 calls)))

(deftest client-cache-forwards-existing-request-preconditions
  (dolist (precondition
           '(("If-Match" . "\"client\"")
             ("If-None-Match" . "\"client\"")
             ("If-Modified-Since" . "Sun, 06 Nov 1994 08:49:37 GMT")
             ("If-Unmodified-Since" . "Sun, 06 Nov 1994 08:49:37 GMT")
             ("If-Range" . "\"client\"")))
    (let* ((name (car precondition))
           (value (cdr precondition))
           (calls 0)
           (uri "http://example.test/conditional")
           (cache (make-http-cache :clock-function (lambda () 1000)))
           (plain-request (make-http-request :method "GET" :uri uri))
           (client
             (make-http-client
              :cache cache
              :transport-function
              (lambda (request &key &allow-other-keys)
                (incf calls)
                (ensure-equal
                 value
                 (http-header-value (http-request-headers request) name))
                (client-test-response 304))))
           (conditional-request
             (http-client-request
              client "GET" uri
              :headers (list (make-http-header name value)))))
      (http-cache-store
       cache plain-request
       (client-test-response
        200
        :headers (list (make-http-header "Cache-Control" "max-age=60")
                       (make-http-header "ETag" "\"cached\""))
        :body (ascii "cached"))
       :now 1000)
      (ensure-equal :miss
                    (nth-value 1
                               (http-cache-lookup cache conditional-request
                                                  :now 1001)))
      (ensure-equal 304
                    (http-response-status
                     (http-client-send client conditional-request)))
      (ensure-equal 1 calls))))

(deftest client-cache-serves-stale-response-on-server-error
  (let* ((now 1000)
         (calls 0)
         (uri "http://example.test/stale-if-error")
         (cache (make-http-cache :clock-function (lambda () now)))
         (client
           (make-http-client
            :cache cache
            :transport-function
            (lambda (request &key &allow-other-keys)
              (declare (ignore request))
              (incf calls)
              (client-test-response 503 :body (ascii "unavailable")))))
         (request (http-client-request client "GET" uri)))
    (http-cache-store
     cache request
     (client-test-response
      200
      :headers (list (make-http-header
                      "Cache-Control" "max-age=10, stale-if-error=20"))
      :body (ascii "cached"))
     :now now)
    (setf now 1015)
    (multiple-value-bind (response effective-request)
        (http-client-send client request)
      (declare (ignore effective-request))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "cached" (octets-as-string (http-response-body response)))
      (ensure-equal "15"
                    (http-header-value (http-response-headers response) "Age")))
    (ensure-equal 1 calls)
    (setf now 1031)
    (ensure-equal 503
                  (http-response-status
                   (http-client-send client request)))
    (ensure-equal 2 calls)))

(deftest client-cache-stale-while-revalidate-schedules-conditional-request
  (let* ((now 1000)
         (calls 0)
         (scheduled nil)
         (uri "http://example.test/stale-while-revalidate")
         (cache (make-http-cache :clock-function (lambda () now)))
         (client
           (make-http-client
            :cache cache
            :stale-while-revalidate-scheduler
            (lambda (client request response revalidate)
              (declare (ignore client request response))
              (setf scheduled revalidate)
              t)
            :transport-function
            (lambda (request &key &allow-other-keys)
              (incf calls)
              (ensure-equal "\"v1\""
                            (http-header-value
                             (http-request-headers request) "If-None-Match"))
              (client-test-response
               304 :headers (list (make-http-header "ETag" "\"v1\""))))))
         (request (http-client-request client "GET" uri)))
    (http-cache-store
     cache request
     (client-test-response
      200
      :headers (list
                (make-http-header
                 "Cache-Control" "max-age=10, stale-while-revalidate=20")
                (make-http-header "ETag" "\"v1\""))
      :body (ascii "cached"))
     :now now)
    (setf now 1015)
    (let ((response (http-client-send client request)))
      (ensure-equal "cached"
                    (octets-as-string (http-response-body response)))
      (ensure-equal "15"
                    (http-header-value (http-response-headers response) "Age")))
    (ensure-equal 0 calls)
    (ensure-true (functionp scheduled))
    (funcall scheduled)
    (ensure-equal 1 calls)
    (ensure-equal :fresh
                  (nth-value 1 (http-cache-lookup cache request :now now)))))
(deftest client-cache-304-callback-sees-only-merged-response
  (let* ((now 1000)
         (transport-calls 0)
         (callback-count 0)
         (callback-status nil)
         (callback-etag nil)
         (cache (make-http-cache :clock-function (lambda () now)))
         (client
           (make-http-client
            :cache cache
            :automatic-decompression-p nil
            :transport-function
            (lambda (request &key on-response-headers &allow-other-keys)
              (incf transport-calls)
              (ensure-equal
               "\"v1\""
               (http-header-value (http-request-headers request)
                                  "If-None-Match"))
              (let ((response
                      (client-test-response
                       304 :headers
                       (list (make-http-header
                              "Cache-Control" "max-age=60")
                             (make-http-header "ETag" "\"v1\"")))))
                (funcall on-response-headers
                         304 (http-response-headers response))
                response))))
         (request
           (http-client-request client "GET"
                                "http://example.test/304-callback"))
         (stale-entry nil))
    (http-cache-store
     cache request
     (client-test-response
      200
      :headers (list (make-http-header "Cache-Control" "max-age=0")
                     (make-http-header "ETag" "\"v1\"")
                     (make-http-header "Content-Type" "text/plain"))
      :body (ascii "cached"))
     :now now)
    (setf now 1001)
    (multiple-value-bind (cached-response state entry)
        (http-cache-lookup cache request :now now)
      (declare (ignore cached-response))
      (ensure-equal :stale state)
      (setf stale-entry entry))
    (let ((response
            (http-client-send
             client request
             :cache-revalidation-entry stale-entry
             :on-response-headers
             (lambda (status headers)
               (incf callback-count)
               (setf callback-status status
                     callback-etag (http-header-value headers "ETag"))))))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "cached" (octets-as-string (http-response-body response))))
    (ensure-equal 1 transport-calls)
    (ensure-equal 1 callback-count)
    (ensure-equal 200 callback-status)
    (ensure-equal "\"v1\"" callback-etag)))

(deftest client-cache-request-no-cache-suppresses-stale-while-revalidate
  (let* ((now 2000)
         (calls 0)
         (scheduled 0)
         (cache (make-http-cache :clock-function (lambda () now)))
         (client
           (make-http-client
            :cache cache
            :stale-while-revalidate-scheduler
            (lambda (&rest arguments)
              (declare (ignore arguments))
              (incf scheduled)
              t)
            :transport-function
            (lambda (request &key &allow-other-keys)
              (declare (ignore request))
              (incf calls)
              (client-test-response 200 :body (ascii "validated")))))
         (stored-request
           (http-client-request client "GET" "http://example.test/no-cache"))
         (request
           (http-client-request
            client "GET" "http://example.test/no-cache"
            :headers (list (make-http-header "Cache-Control" "no-cache")))))
    (http-cache-store
     cache stored-request
     (client-test-response
      200
      :headers (list (make-http-header
                      "Cache-Control" "max-age=1, stale-while-revalidate=30"))
      :body (ascii "cached"))
     :now now)
    (setf now 2002)
    (ensure-equal
     "validated"
     (octets-as-string (http-response-body (http-client-send client request))))
    (ensure-equal 1 calls)
    (ensure-equal 0 scheduled)))

(deftest client-cache-request-can-enable-stale-if-error
  (let* ((now 2000)
         (uri "http://example.test/request-stale-if-error")
         (cache (make-http-cache :clock-function (lambda () now)))
         (client
           (make-http-client
            :cache cache
            :transport-function
            (lambda (request &key &allow-other-keys)
              (declare (ignore request))
              (client-test-response 500))))
         (stored-request (http-client-request client "GET" uri))
         (request
           (http-client-request
            client "GET" uri
            :headers (list (make-http-header
                            "Cache-Control" "stale-if-error=5")))))
    (http-cache-store
     cache stored-request
     (client-test-response
      200
      :headers (list (make-http-header "Cache-Control" "max-age=1"))
      :body (ascii "request-authorized"))
     :now now)
    (setf now 2006)
    (ensure-equal
     "request-authorized"
     (octets-as-string
     (http-response-body (http-client-send client request))))))

(deftest client-cache-stale-if-error-uses-stricter-combined-window
  (let* ((now 2500)
         (uri "http://example.test/strict-stale-if-error")
         (cache (make-http-cache :clock-function (lambda () now)))
         (client
           (make-http-client
            :cache cache
            :transport-function
            (lambda (request &key &allow-other-keys)
              (declare (ignore request))
              (client-test-response 500))))
         (stored-request (http-client-request client "GET" uri))
         (request
           (http-client-request
            client "GET" uri
            :headers (list (make-http-header
                            "Cache-Control" "stale-if-error=5")))))
    (http-cache-store
     cache stored-request
     (client-test-response
      200
      :headers (list (make-http-header
                      "Cache-Control" "max-age=1, stale-if-error=30"))
      :body (ascii "too-stale-for-request"))
     :now now)
    (setf now 2507)
    (ensure-equal 500 (http-response-status
                       (http-client-send client request)))))

(deftest client-cache-stale-if-error-is-independent-of-retry-policy
  (let* ((now 3000)
         (uri "http://example.test/stale-connection-error")
         (cache (make-http-cache :clock-function (lambda () now)))
         (client
           (make-http-client
            :cache cache
            :retry-policy
            (make-http-retry-policy :retry-on-connection-error-p nil)
            :transport-function
            (lambda (request &key &allow-other-keys)
              (declare (ignore request))
              (error 'http-connection-error :message "unavailable"))))
         (request (http-client-request client "GET" uri)))
    (http-cache-store
     cache request
     (client-test-response
      200
      :headers (list (make-http-header
                      "Cache-Control" "max-age=1, stale-if-error=10"))
      :body (ascii "cached-after-connection-error"))
     :now now)
    (setf now 3002)
    (ensure-equal
     "cached-after-connection-error"
     (octets-as-string
      (http-response-body (http-client-send client request))))))
(deftest client-cache-stale-if-error-delivers-headers-before-response
  (let* ((now 4000)
         (events nil)
         (uri "http://example.test/stale-headers-on-error")
         (cache (make-http-cache :clock-function (lambda () now)))
         (client
           (make-http-client
            :cache cache
            :retry-policy
            (make-http-retry-policy :retry-on-connection-error-p nil)
            :on-response
            (lambda (response request attempt)
              (declare (ignore request attempt))
              (push (list :response
                          (http-response-status response)
                          (octets-as-string (http-response-body response)))
                    events))
            :transport-function
            (lambda (request &key &allow-other-keys)
              (declare (ignore request))
              (error 'http-connection-error :message "unavailable"))))
         (request (http-client-request client "GET" uri)))
    (http-cache-store
     cache request
     (client-test-response
      200
      :headers (list (make-http-header
                      "Cache-Control" "max-age=1, stale-if-error=10")
                     (make-http-header "Content-Type" "text/plain"))
      :body (ascii "cached"))
     :now now)
    (setf now 4002)
    (multiple-value-bind (response effective-request)
        (http-client-send
         client request
         :on-response-headers
         (lambda (status headers)
           (push (list :headers
                       status
                       (http-header-value headers "Content-Type"))
                 events)))
      (declare (ignore effective-request))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "cached"
                    (octets-as-string (http-response-body response))))
    (ensure-equal '((:headers 200 "text/plain")
                    (:response 200 "cached"))
                  (reverse events))))

(deftest client-redirects-and-hooks
  (let ((calls 0)
        (methods nil)
        (uris nil)
        (request-events 0)
        (response-events 0))
    (let* ((client
             (make-http-client
              :cache nil
              :on-request (lambda (request attempt)
                            (declare (ignore request attempt))
                            (incf request-events))
              :on-response (lambda (response request attempt)
                             (declare (ignore response request attempt))
                             (incf response-events))
              :transport-function
              (lambda (request &key proxy-plan &allow-other-keys)
                (declare (ignore proxy-plan))
                (incf calls)
                (push (http-request-method request) methods)
                (push (http-uri-string (http-request-uri request)) uris)
                (if (= calls 1)
                    (client-test-response
                     302
                     :headers (list (make-http-header
                                     "Location" "/final")))
                    (client-test-response 200 :body (ascii "done"))))))
           (request (http-client-request client "POST"
                                         "http://example.test/start"
                                         :body (ascii "payload"))))
      (multiple-value-bind (response effective)
          (http-client-send client request)
        (ensure-equal 200 (http-response-status response))
        (ensure-equal "http://example.test/final"
                      (http-uri-string (http-request-uri effective)))
        (ensure-equal '(("POST" . "http://example.test/start")
                        ("GET" . "http://example.test/final"))
                      (mapcar #'cons (reverse methods) (reverse uris)))
        (ensure-equal 2 calls)
        (ensure-equal 2 request-events)
        (ensure-equal 2 response-events)))))

(deftest client-cross-site-redirect-downgrades-cookie-context
  (let ((calls 0)
        (seen-cookie nil)
        (jar (make-http-cookie-jar)))
    (http-cookie-jar-accept-response
     jar "https://target.test/"
     (client-test-response
      200 :headers (list (make-http-header
                          "Set-Cookie" "strict=1; SameSite=Strict; Secure")))
     :now 1000)
    (let ((client
            (make-http-client
             :cache nil
             :cookie-jar jar
             :transport-function
             (lambda (request &key &allow-other-keys)
               (incf calls)
               (when (= calls 2)
                 (setf seen-cookie
                       (http-header-value (http-request-headers request)
                                          "Cookie")))
               (if (= calls 1)
                   (client-test-response
                    302 :headers (list (make-http-header
                                        "Location"
                                        "https://target.test/final")))
                   (client-test-response 200))))))
      (http-client-send
       client (http-client-request client "GET" "https://source.test/start"))
      (ensure-equal 2 calls)
      (ensure-equal nil seen-cookie))))

(deftest client-cross-origin-redirect-isolates-authority-and-challenge-auth
  (let ((calls 0)
        (provider-calls 0)
        (redirect-host :not-observed)
        (redirect-origin :not-observed)
        (redirect-referer :not-observed))
    (let ((client
            (make-http-client
             :cache nil
             :challenge-auth-provider
             (lambda (&rest arguments)
               (declare (ignore arguments))
               (incf provider-calls)
               "Basic credentials")
             :transport-function
             (lambda (request &key &allow-other-keys)
               (incf calls)
               (if (= calls 1)
                   (client-test-response
                    302 :headers (list (make-http-header
                                        "Location"
                                        "https://target.test/private")))
                   (progn
                     (setf redirect-host
                           (http-header-value
                            (http-request-headers request) "Host")
                           redirect-origin
                           (http-header-value
                            (http-request-headers request) "Origin")
                           redirect-referer
                           (http-header-value
                            (http-request-headers request) "Referer"))
                     (client-test-response
                      401 :headers (list (make-http-header
                                          "WWW-Authenticate"
                                          "Basic realm=\"target\"")))))))))
      (let ((response
              (http-client-send
               client
               (http-client-request
                client "GET" "https://source.test/start"
                :headers (list
                          (make-http-header "Host" "source.test")
                          (make-http-header "Origin" "https://source.test")
                          (make-http-header
                           "Referer" "https://source.test/page"))))))
        (ensure-equal 401 (http-response-status response)))
      (ensure-equal 2 calls)
      (ensure-equal 0 provider-calls)
      (ensure-equal nil redirect-host)
      (ensure-equal nil redirect-origin)
      (ensure-equal nil redirect-referer))))

(deftest client-downgrade-redirect-drops-authorization-even-when-preserved
  (let ((calls 0)
        (seen-authorization :not-observed))
    (let ((client
            (make-http-client
             :cache nil
             :redirect-policy
             (make-http-redirect-policy
              :allow-downgrade-p t
              :preserve-authorization-p t)
             :transport-function
             (lambda (request &key &allow-other-keys)
               (incf calls)
               (if (= calls 1)
                   (client-test-response
                    302 :headers (list (make-http-header
                                        "Location" "http://target.test/final")))
                   (progn
                     (setf seen-authorization
                           (http-header-value
                            (http-request-headers request) "Authorization"))
                     (client-test-response 200)))))))
      (let ((response
              (http-client-send
               client
               (http-client-request
                client "GET" "https://source.test/start"
                :headers (list (make-http-header
                                "Authorization" "Basic secret"))))))
        (ensure-equal 200 (http-response-status response)))
      (ensure-equal 2 calls)
      (ensure-equal nil seen-authorization))))

(deftest client-challenge-auth-does-not-cross-origin-on-redirect
  (let ((calls 0)
        (provider-calls 0)
        (target-authorization :not-observed))
    (let ((client
            (make-http-client
             :cache nil
             :redirect-policy
             (make-http-redirect-policy :preserve-authorization-p t)
             :challenge-auth-provider
             (lambda (&rest arguments)
               (declare (ignore arguments))
               (incf provider-calls)
               "Basic source-credentials")
             :transport-function
             (lambda (request &key &allow-other-keys)
               (incf calls)
               (case calls
                 (1
                  (client-test-response
                   401 :headers (list (make-http-header
                                       "WWW-Authenticate"
                                       "Basic realm=\"source\""))))
                 (2
                  (ensure-equal
                   "Basic source-credentials"
                   (http-header-value
                    (http-request-headers request) "Authorization"))
                  (client-test-response
                   302 :headers (list (make-http-header
                                       "Location"
                                       "https://target.test/private"))))
                 (otherwise
                  (setf target-authorization
                        (http-header-value
                         (http-request-headers request) "Authorization"))
                  (client-test-response 200)))))))
      (let ((response
              (http-client-send
               client
               (http-client-request
                client "GET" "https://source.test/private"))))
        (ensure-equal 200 (http-response-status response)))
      (ensure-equal 3 calls)
      (ensure-equal 1 provider-calls)
      (ensure-equal nil target-authorization))))

(deftest client-redirect-drops-trailers-when-method-changes
  (let ((calls 0)
        (seen-trailers nil)
        (seen-headers nil))
    (let* ((client
             (make-http-client
              :cache nil
              :transport-function
              (lambda (request &key proxy-plan &allow-other-keys)
                (declare (ignore proxy-plan))
                (incf calls)
                (push (http-request-trailers request) seen-trailers)
                (push (http-request-headers request) seen-headers)
                (if (= calls 1)
                    (client-test-response
                     302
                     :headers (list (make-http-header
                                     "Location" "/final")))
                    (client-test-response 200)))))
           (request (http-client-request
                     client "POST" "http://example.test/start"
                     :headers (list
                               (make-http-header "Content-Encoding" "gzip")
                               (make-http-header "Content-Language" "en")
                               (make-http-header "Content-Location" "/source")
                               (make-http-header "Content-Type" "text/plain")
                               (make-http-header "Expect" "100-continue")
                               (make-http-header "Trailer" "X-Checksum"))
                     :trailers (list (make-http-header "X-Checksum" "abc")))))
      (multiple-value-bind (response effective)
          (http-client-send client request)
        (declare (ignore response))
        (ensure-equal "GET" (http-request-method effective))
        (ensure-equal nil (http-request-trailers effective)))
      (ensure-equal 2 calls)
      (ensure-equal nil (first seen-trailers))
      (dolist (name '("Content-Encoding" "Content-Language"
                      "Content-Location" "Content-Type" "Expect" "Trailer"))
        (ensure-false (http-header-present-p (first seen-headers) name)))
      (ensure-equal "abc"
                    (http-header-value (second seen-trailers) "X-Checksum")))))

(deftest client-redirect-does-not-forward-proxy-credentials
  (let ((calls 0)
        (seen-authorization nil)
        (proxy
          (make-http-proxy :scheme :http
                           :host "proxy.example"
                           :port 8080
                           :username "user"
                           :password "pass")))
    (let ((client
            (make-http-client
             :cache nil
             :proxy (lambda (uri)
                      (and (string= "first.example" (http-uri-host uri))
                           proxy))
             :transport-function
             (lambda (request &key proxy-plan &allow-other-keys)
               (incf calls)
               (push (list (getf proxy-plan :mode)
                           (http-header-value
                            (http-request-headers request)
                            "Proxy-Authorization"))
                     seen-authorization)
               (if (= calls 1)
                   (client-test-response
                    302
                    :headers (list (make-http-header
                                    "Location"
                                    "http://second.example/final")))
                   (client-test-response 200))))))
      (http-client-send
       client
       (http-client-request client "GET" "http://first.example/start"))
      (ensure-equal '((:forward "Basic dXNlcjpwYXNz")
                      (:direct nil))
                    (reverse seen-authorization)))))

(deftest client-proxy-authorization-only-reaches-forward-proxy
  (dolist (case '(("http://direct.example/path" nil :direct)
                  ("https://origin.example/path"
                   (:http "proxy.example" 8080)
                   :connect)))
    (destructuring-bind (uri proxy-spec expected-mode) case
      (let ((seen-authorization :not-observed))
        (let ((client
                (make-http-client
                 :cache nil
                 :proxy (and proxy-spec
                             (make-http-proxy
                              :scheme (first proxy-spec)
                              :host (second proxy-spec)
                              :port (third proxy-spec)))
                 :transport-function
                 (lambda (request &key proxy-plan &allow-other-keys)
                   (ensure-equal expected-mode (getf proxy-plan :mode))
                   (setf seen-authorization
                         (http-header-value
                          (http-request-headers request)
                          "Proxy-Authorization"))
                   (client-test-response 200)))))
          (http-client-send
           client
           (http-client-request
            client "GET" uri
            :headers
            (list (make-http-header
                   "Proxy-Authorization" "Basic origin-leak"))))
          (ensure-equal nil seen-authorization))))))

(deftest client-retries
  (signals http-protocol-error
    (make-http-retry-policy :jitter-ratio -0.01))
  (signals http-protocol-error
    (make-http-retry-policy :jitter-ratio 1.01))
  (let* ((extension-policy (make-http-retry-policy :methods '("probe")))
         (extension-request
           (make-http-request :method "probe" :uri "http://example.test/"))
         (lowercase-get-request
           (make-http-request :method "get" :uri "http://example.test/")))
    (ensure-equal '("probe")
                  (http-retry-policy-methods extension-policy))
    (ensure-true
     (http-kit/client::%client-retry-method-p extension-policy
                                              extension-request))
    (ensure-false
     (http-kit/client::%client-retry-method-p (make-http-retry-policy)
                                              lowercase-get-request))
    (ensure-false
     (http-kit/client::%client-cacheable-method-p lowercase-get-request))
    (ensure-true
     (http-kit/client::%client-mutating-method-p lowercase-get-request))
    (ensure-false (http-kit/client::%cookie-safe-method-p "get")))
  (let ((calls 0)
        (sleeps nil))
    (let* ((client
             (make-http-client
              :cache nil
              :sleep-function (lambda (seconds)
                                (push seconds sleeps))
              :retry-policy (make-http-retry-policy
                             :max-attempts 2
                             :base-delay 0.25
                             :max-delay 0.25)
              :transport-function
              (lambda (request &key proxy-plan &allow-other-keys)
                (declare (ignore request proxy-plan))
                (incf calls)
                (if (= calls 1)
                    (client-test-response 503)
                    (client-test-response 200 :body (ascii "ok"))))))
           (request (http-client-request client "GET"
                                         "http://example.test/retry")))
      (multiple-value-bind (response effective)
          (http-client-send client request)
        (declare (ignore effective))
        (ensure-equal 200 (http-response-status response))
        (ensure-equal "ok"
                      (octets-as-string (http-response-body response))))
      (ensure-equal 2 calls)
      (ensure-equal '(0.25) sleeps)))
  (let ((calls 0)
        (sleeps nil)
        (random-calls 0))
    (let ((client
            (make-http-client
             :cache nil
             :sleep-function (lambda (seconds) (push seconds sleeps))
             :random-function (lambda (limit)
                                (ensure-equal 1.0 limit)
                                (incf random-calls)
                                0.75)
             :retry-policy (make-http-retry-policy
                            :max-attempts 2
                            :base-delay 4
                            :max-delay 10
                            :jitter-ratio 0.5)
             :transport-function
             (lambda (request &key proxy-plan &allow-other-keys)
               (declare (ignore request proxy-plan))
               (incf calls)
               (if (= calls 1)
                   (client-test-response 503)
                   (client-test-response 200))))))
      (http-client-send
       client
       (http-client-request client "GET" "http://example.test/retry-jitter"))
      (ensure-equal '(5.0) sleeps)
      (ensure-equal 1 random-calls)))
  (let ((calls 0)
        (sleeps nil))
    (let ((client
            (make-http-client
             :cache nil
             :sleep-function (lambda (seconds) (push seconds sleeps))
             :random-function (lambda (limit)
                                (declare (ignore limit))
                                (error "Retry-After must not use jitter"))
             :retry-policy (make-http-retry-policy
                            :max-attempts 2
                            :base-delay 0.25
                            :max-delay 10
                            :jitter-ratio 1)
             :transport-function
             (lambda (request &key proxy-plan &allow-other-keys)
               (declare (ignore request proxy-plan))
               (incf calls)
               (if (= calls 1)
                   (client-test-response
                    503
                    :headers (list (make-http-header "Retry-After" "5")))
                   (client-test-response 200))))))
      (http-client-send
       client
       (http-client-request client "GET" "http://example.test/retry-after"))
      (ensure-equal '(5.0) sleeps)))
  (let ((calls 0)
        (sleeps nil))
    (let ((client
            (make-http-client
             :cache nil
             :clock-function
             (lambda () (encode-universal-time 32 49 8 6 11 1994 0))
             :sleep-function (lambda (seconds) (push seconds sleeps))
             :retry-policy (make-http-retry-policy
                            :max-attempts 2
                            :base-delay 0.25
                            :max-delay 10)
             :transport-function
             (lambda (request &key proxy-plan &allow-other-keys)
               (declare (ignore request proxy-plan))
               (incf calls)
               (if (= calls 1)
                   (client-test-response
                    503
                    :headers
                    (list (make-http-header
                           "Retry-After"
                           "Sun, 06 Nov 1994 08:49:37 GMT")))
                   (client-test-response 200))))))
      (http-client-send
       client
       (http-client-request client "GET" "http://example.test/retry-after-date"))
      (ensure-equal '(5.0) sleeps)))
  (let ((calls 0)
        (sleeps nil))
    (let ((client
            (make-http-client
             :cache nil
             :sleep-function (lambda (seconds) (push seconds sleeps))
             :retry-policy (make-http-retry-policy
                            :max-attempts 2
                            :base-delay 0.25
                            :max-delay 10)
             :transport-function
             (lambda (request &key proxy-plan &allow-other-keys)
               (declare (ignore request proxy-plan))
               (incf calls)
               (if (= calls 1)
                   (client-test-response
                    503
                    :headers (list (make-http-header "Retry-After" "later")))
                   (client-test-response 200))))))
      (http-client-send
       client
       (http-client-request client "GET" "http://example.test/retry-after-invalid"))
      (ensure-equal '(0.25) sleeps)))
  (let ((calls 0)
        (condition nil))
    (let* ((client
             (make-http-client
              :cache nil
              :retry-policy (make-http-retry-policy :max-attempts 2)
              :transport-function
              (lambda (request &key proxy-plan &allow-other-keys)
                (declare (ignore request proxy-plan))
                (incf calls)
                (client-test-response 503))))
           (request (http-client-request client "GET"
                                         "http://example.test/exhausted")))
      (handler-case
          (http-client-send client request)
        (http-retry-exhausted (caught)
          (setf condition caught)))
      (ensure-true condition)
      (ensure-equal 2 calls)
      (ensure-equal 2 (http-retry-exhausted-attempts condition))
      (ensure-equal 503
                     (http-response-status
                     (http-retry-exhausted-last-response condition))))))

(deftest client-retries-unprocessed-http2-post
  (dolist (cause '((:goaway 0 1) (:rst-stream 1 7)))
    (let ((calls 0))
      (let ((client
              (make-http-client
               :cache nil
               :retry-policy (make-http-retry-policy
                              :max-attempts 2
                              :base-delay 0
                              :max-delay 0)
               :transport-function
               (lambda (request &key proxy-plan &allow-other-keys)
                 (declare (ignore request proxy-plan))
                 (incf calls)
                 (if (= calls 1)
                     (error 'http-connection-error
                            :message "HTTP/2 request was not processed."
                            :cause cause)
                     (client-test-response 200))))))
        (http-client-send
         client
         (http-client-request client "POST" "https://example.test/retry"
                                     :body (ascii "payload")))
        (ensure-equal 2 calls)))))

(deftest client-does-not-retry-processed-or-unreplayable-http2-post
  (dolist (cause '((:rst-stream 1 8) (:connection-closed)))
    (let ((calls 0))
      (let ((client
              (make-http-client
               :cache nil
               :retry-policy (make-http-retry-policy
                              :max-attempts 2
                              :base-delay 0
                              :max-delay 0)
               :transport-function
               (lambda (request &key proxy-plan &allow-other-keys)
                 (declare (ignore request proxy-plan))
                 (incf calls)
                 (error 'http-connection-error
                        :message "HTTP/2 request failed."
                        :cause cause)))))
        (signals http-connection-error
          (http-client-send
           client
           (http-client-request client "POST" "https://example.test/no-retry"
                                       :body (ascii "payload"))))
        (ensure-equal 1 calls))))
  (let ((calls 0)
        (producer-calls 0))
    (let ((client
            (make-http-client
             :cache nil
             :retry-policy (make-http-retry-policy
                            :max-attempts 2
                            :base-delay 0
                            :max-delay 0)
             :transport-function
             (lambda (request &key request-body-function &allow-other-keys)
               (declare (ignore request))
               (incf calls)
               (funcall request-body-function 65536)
               (error 'http-connection-error
                      :message "HTTP/2 stream was refused."
                      :cause '(:rst-stream 1 7))))))
      (signals http-connection-error
        (http-client-send
         client
         (http-client-request client "POST" "https://example.test/producer")
         :request-body-function
         (lambda (maximum-size)
           (declare (ignore maximum-size))
           (incf producer-calls)
           nil)))
      (ensure-equal 1 calls)
      (ensure-equal 1 producer-calls))))

(deftest client-enforces-one-deadline-across-retries
  (let ((calls 0)
        (observed-timeout nil)
        (observed-deadline nil))
    (let ((client
            (make-http-client
             :cache nil
             :wall-clock-function (lambda () 100)
             :transport-function
             (lambda (request &key timeout deadline &allow-other-keys)
               (declare (ignore request))
               (incf calls)
               (setf observed-timeout timeout
                     observed-deadline deadline)
               (client-test-response 200)))))
      (http-client-send
       client
       (http-client-request client "GET" "http://example.test/deadline")
       :timeout 4)
      (ensure-equal 1 calls)
      (ensure-equal 4 observed-timeout)
      (ensure-equal 104 observed-deadline)))
  (let ((calls 0)
        (sleeps nil)
        (condition nil))
    (let ((client
            (make-http-client
             :cache nil
             :wall-clock-function (lambda () 100)
             :sleep-function (lambda (seconds) (push seconds sleeps))
             :retry-policy (make-http-retry-policy
                            :max-attempts 2
                            :base-delay 5
                            :max-delay 5)
             :transport-function
             (lambda (request &key timeout deadline &allow-other-keys)
               (declare (ignore request timeout deadline))
               (incf calls)
               (client-test-response 503)))))
      (handler-case
          (http-client-send
           client
           (http-client-request client "GET" "http://example.test/deadline-retry")
           :deadline 103)
        (http-timeout (caught)
          (setf condition caught)))
      (ensure-true condition)
      (ensure-equal :retry (http-timeout-kind condition))
      (ensure-equal 1 calls)
      (ensure-equal nil sleeps)))
  (let ((calls 0)
        (now 100)
        (condition nil))
    (let ((client
            (make-http-client
             :cache nil
             :wall-clock-function (lambda () now)
             :transport-function
             (lambda (request &key &allow-other-keys)
               (declare (ignore request))
               (incf calls)
               (setf now 105)
               (client-test-response
                302
                :headers
                (list (make-http-header
                       "Location" "http://example.test/final")))))))
      (handler-case
          (http-client-send
           client
           (http-client-request
            client "GET" "http://example.test/deadline-redirect")
           :timeout 4)
        (http-timeout (caught)
          (setf condition caught)))
      (ensure-true condition)
      (ensure-equal :deadline (http-timeout-kind condition))
      (ensure-equal 1 calls))))

(deftest client-default-timeout-is-finite-and-explicit-nil-is-unbounded
  (let ((observed nil))
    (let ((client
            (make-http-client
             :cache nil
             :wall-clock-function (lambda () 100)
             :transport-function
             (lambda (request &key timeout deadline &allow-other-keys)
               (declare (ignore request deadline))
               (setf observed timeout)
               (client-test-response 200)))))
      (http-client-send client
                        (http-client-request client "GET" "http://example.test/"))
      (ensure-equal 30.0 observed))
    (let ((client
            (make-http-client
             :cache nil
             :transport-function
             (lambda (request &key timeout &allow-other-keys)
               (declare (ignore request))
               (setf observed timeout)
               (client-test-response 200)))))
      (http-client-send client
                        (http-client-request client "GET" "http://example.test/")
                        :timeout nil)
      (ensure-equal nil observed))))

(deftest client-does-not-retry-after-streaming-response-bytes
  (let ((calls 0)
        (chunks nil))
    (let ((client
            (make-http-client
             :cache nil
             :retry-policy (make-http-retry-policy :max-attempts 2)
             :transport-function
             (lambda (request &key on-body-chunk &allow-other-keys)
               (declare (ignore request))
               (incf calls)
               (funcall on-body-chunk (ascii "partial"))
               (client-test-response 503)))))
      (let ((response
              (http-client-send
               client
               (http-client-request
                client "GET" "http://example.test/streamed-retry")
               :on-body-chunk
               (lambda (chunk)
                 (push (octets-as-string chunk) chunks))
               :collect-body-p nil)))
        (ensure-equal 503 (http-response-status response)))
      (ensure-equal 1 calls)
      (ensure-equal '("partial") (reverse chunks)))))

(deftest client-preserves-error-after-streaming-response-bytes
  (let ((calls 0)
        (condition nil))
    (let ((client
            (make-http-client
             :cache nil
             :retry-policy (make-http-retry-policy :max-attempts 2)
             :transport-function
             (lambda (request &key on-body-chunk &allow-other-keys)
               (declare (ignore request))
               (incf calls)
               (funcall on-body-chunk (ascii "partial"))
               (error 'http-connection-error :message "interrupted")))))
      (handler-case
          (http-client-send
           client
           (http-client-request
            client "GET" "http://example.test/interrupted-stream")
           :on-body-chunk (lambda (chunk) (declare (ignore chunk)))
           :collect-body-p nil)
        (http-connection-error (caught)
          (setf condition caught)))
      (ensure-true condition)
      (ensure-equal "interrupted" (http-error-message condition))
      (ensure-equal 1 calls))))

(deftest client-does-not-redirect-after-streaming-response-bytes
  (let ((calls 0)
        (chunks nil))
    (let ((client
            (make-http-client
             :cache nil
             :transport-function
             (lambda (request &key on-body-chunk &allow-other-keys)
               (declare (ignore request))
               (incf calls)
               (funcall on-body-chunk (ascii "redirect-body"))
               (client-test-response
                302
                :headers
                (list (make-http-header
                       "Location" "http://example.test/final")))))))
      (let ((response
              (http-client-send
               client
               (http-client-request
                client "GET" "http://example.test/redirect-stream")
               :on-body-chunk
               (lambda (chunk)
                 (push (octets-as-string chunk) chunks))
               :collect-body-p nil)))
        (ensure-equal 302 (http-response-status response)))
      (ensure-equal 1 calls)
      (ensure-equal '("redirect-body") (reverse chunks)))))

(deftest client-streaming-request-body-replay-factory
  (let ((attempts 0)
        (factory-calls 0)
        (bodies nil))
    (let ((client
            (make-http-client
             :cache nil
             :retry-policy (make-http-retry-policy
                            :max-attempts 2
                            :methods '("POST")
                            :base-delay 0
                            :max-delay 0)
             :transport-function
             (lambda (request &key request-body-function &allow-other-keys)
               (declare (ignore request))
               (incf attempts)
               (let ((chunks nil))
                 (loop for chunk = (funcall request-body-function 65536)
                       while chunk
                       do (push (octets-as-string chunk) chunks))
                 (push (nreverse chunks) bodies))
               (if (= attempts 1)
                   (client-test-response 503)
                   (client-test-response 200 :body (ascii "ok")))))))
      (multiple-value-bind (response effective)
          (http-client-send
           client
           (http-client-request client "POST" "http://example.test/retry-body")
           :request-body-factory
           (lambda ()
             (incf factory-calls)
             (let ((chunks (list (ascii "abc")
                                 (ascii "de"))))
               (lambda (maximum-size)
                 (ensure-equal 65536 maximum-size)
                 (pop chunks))))
           :request-body-length 5)
        (declare (ignore effective))
        (ensure-equal 200 (http-response-status response))
        (ensure-equal "ok" (octets-as-string (http-response-body response))))
      (ensure-equal 2 attempts)
      (ensure-equal 2 factory-calls)
      (ensure-equal '(("abc" "de") ("abc" "de"))
                    (reverse bodies)))))

(deftest client-streaming-request-body-does-not-retry-without-factory
  (let ((attempts 0)
        (bodies nil)
        (chunks (list (ascii "abc")
                      (ascii "de"))))
    (let ((client
            (make-http-client
             :cache nil
             :retry-policy (make-http-retry-policy :max-attempts 2)
             :transport-function
             (lambda (request &key request-body-function &allow-other-keys)
               (declare (ignore request))
               (incf attempts)
               (let ((received nil))
                 (loop for chunk = (funcall request-body-function 65536)
                       while chunk
                       do (push (octets-as-string chunk) received))
                 (push (nreverse received) bodies))
               (client-test-response 503)))))
      (multiple-value-bind (response effective)
          (http-client-send
           client
           (http-client-request client "POST" "http://example.test/no-replay")
           :request-body-function
           (lambda (maximum-size)
             (ensure-equal 65536 maximum-size)
             (pop chunks))
           :request-body-length 5)
        (declare (ignore effective))
        (ensure-equal 503 (http-response-status response)))
      (ensure-equal 1 attempts)
      (ensure-true (null chunks))
      (ensure-equal '(("abc" "de")) bodies))))

(deftest client-streaming-request-body-does-not-follow-same-method-redirect
  (let ((calls 0)
        (chunks (list (ascii "abc")
                      (ascii "de"))))
    (let ((client
            (make-http-client
             :cache nil
             :transport-function
             (lambda (request &key request-body-function &allow-other-keys)
               (declare (ignore request))
               (incf calls)
               (when request-body-function
                 (loop for chunk = (funcall request-body-function 65536)
                       while chunk))
               (client-test-response
                307
                :headers (list (make-http-header "Location" "/next")))))))
      (multiple-value-bind (response effective)
          (http-client-send
           client
           (http-client-request client "PUT" "http://example.test/start")
           :request-body-function
           (lambda (maximum-size)
             (ensure-equal 65536 maximum-size)
             (pop chunks))
           :request-body-length 5)
        (ensure-equal 307 (http-response-status response))
        (ensure-equal "http://example.test/start"
                      (http-uri-string (http-request-uri effective))))
      (ensure-equal 1 calls)
      (ensure-true (null chunks)))))

(deftest client-proxy-plans
  (let ((proxy (make-http-proxy :scheme :http
                                :host "proxy.example"
                                :port 8080
                                :username "user"
                                :password "pass"
                                :no-proxy "bypass.example")))
    (let ((plan (http-proxy-plan proxy "http://example.test/path")))
      (ensure-equal :forward (getf plan :mode))
      (ensure-equal "http://example.test/path"
                    (getf plan :request-target))
      (ensure-equal "proxy.example" (getf plan :connect-host))
      (ensure-equal 8080 (getf plan :connect-port))
      (ensure-equal "Basic dXNlcjpwYXNz"
                    (getf plan :proxy-authorization)))
    (let ((plan (http-proxy-plan proxy "https://example.test/path")))
      (ensure-equal :connect (getf plan :mode))
      (ensure-equal "example.test" (getf plan :connect-host))
      (ensure-equal 443 (getf plan :connect-port)))
    (ensure-true (http-proxy-no-proxy-p
                 proxy "http://bypass.example/path")))
    (let* ((proxy (make-http-proxy :scheme :socks5
                                   :host "proxy.example"
                                   :port 1080
                                   :username "user:name"
                                   :password "pass"))
         (plan (http-proxy-plan proxy "http://example.test/path")))
    (ensure-equal :socks5 (getf plan :mode))
    (ensure-false (getf plan :proxy-authorization))))

(deftest client-proxy-no-proxy-domain-and-cidr-rules
  (flet ((matches-p (rules uri)
           (http-proxy-no-proxy-p
            (make-http-proxy :scheme :http
                             :host "proxy.example"
                             :port 8080
                             :no-proxy rules)
            uri)))
    (ensure-true (matches-p "example.com" "http://example.com/"))
    (ensure-true (matches-p "example.com" "http://www.example.com/"))
    (ensure-false (matches-p "example.com" "http://notexample.com/"))
    (ensure-true (matches-p ".example.com" "http://www.example.com/"))
    (ensure-true (matches-p "example.com:8080"
                            "http://www.example.com:8080/"))
    (ensure-false (matches-p "example.com:8080"
                             "http://www.example.com:8081/"))
    (ensure-true (matches-p "192.168.0.0/16" "http://192.168.4.2/"))
    (ensure-true (matches-p "192.168.4.128/25" "http://192.168.4.255/"))
    (ensure-false (matches-p "192.168.4.128/25" "http://192.168.4.127/"))
    (ensure-false (matches-p "192.168.0.0/16" "http://192.169.4.2/"))
    (ensure-true (matches-p "2001:db8::/32" "http://[2001:db8:1::1]/"))
    (ensure-false (matches-p "2001:db8::/32" "http://[2001:db9::1]/"))))

#+sbcl
(deftest client-connection-pool-route-keys-separate-proxy-credentials
  (let* ((request
           (make-http-request :method "GET" :uri "http://example.test/"))
         (first-proxy
           (make-http-proxy :scheme :socks5
                            :host "proxy.example"
                            :port 1080
                            :username "alice"
                            :password "first-secret"))
         (second-proxy
           (make-http-proxy :scheme :socks5
                            :host "proxy.example"
                            :port 1080
                            :username "bob"
                            :password "second-secret"))
         (first-plan (http-proxy-plan first-proxy (http-request-uri request)))
         (second-plan (http-proxy-plan second-proxy (http-request-uri request))))
    (ensure-false
     (equal (http-kit/client::%client-connection-key request first-plan)
            (http-kit/client::%client-connection-key request second-plan)))))

#+sbcl
(deftest client-connection-pool-reuses-reusable-stream
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (ascii
             "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|abcHTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|def")))
         (opened 0)
         (closed 0)
         (pool
           (make-http-connection-pool
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (incf opened)
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (incf closed))))
         (client (make-http-client :cache nil
                                   :automatic-decompression-p nil
                                   :connection-pool pool)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://example.test/one"))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "abc" (octets-as-string (http-response-body response)))
      (ensure-equal "http://example.test/one"
                    (http-uri-string (http-request-uri effective))))
    (ensure-equal 1 opened)
    (ensure-equal 0 closed)
    (let ((stats (http-connection-pool-stats pool)))
      (ensure-equal 1 (getf stats :idle-count))
      (ensure-false (member :keys stats)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://example.test/two"))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "def" (octets-as-string (http-response-body response)))
      (ensure-equal "http://example.test/two"
                    (http-uri-string (http-request-uri effective))))
    (ensure-equal 1 opened)
    (ensure-equal 0 closed)
    (ensure-equal 1 (http-connection-pool-clear pool))
    (ensure-equal 1 closed)
    (ensure-equal
     (ascii
      "GET /one HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 0|CRLF||CRLF|GET /two HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 0|CRLF||CRLF|")
     (binary-test-output stream))))
#+sbcl
(deftest client-connection-pool-clear-retires-active-stream
  (let* ((first-stream
           (make-instance
            'binary-test-stream
            :input
            (ascii
             "HTTP/1.1 103 Early Hints|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|abc")))
         (second-stream
           (make-instance
            'binary-test-stream
            :input
            (ascii
             "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|def")))
         (opened 0)
         (closed-streams nil)
         (pool
           (make-http-connection-pool
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (incf opened)
              (if (= opened 1) first-stream second-stream))
            :close-stream
            (lambda (closed-stream)
              (push closed-stream closed-streams))))
         (client (make-http-client :cache nil
                                   :automatic-decompression-p nil
                                   :connection-pool pool)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://example.test/first")
         :on-information
         (lambda (information)
           (ensure-equal 103 (http-response-status information))
           (ensure-equal 0 (http-connection-pool-clear pool))
           (ensure-false (member first-stream closed-streams :test #'eq))))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "abc" (octets-as-string (http-response-body response))))
    (ensure-equal 1 opened)
    (ensure-true (member first-stream closed-streams :test #'eq))
    (ensure-equal 0
                  (getf (http-connection-pool-stats pool) :idle-count))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://example.test/second"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "def" (octets-as-string (http-response-body response))))
    (ensure-equal 2 opened)
    (ensure-false (member second-stream closed-streams :test #'eq))
    (ensure-equal 1 (http-connection-pool-clear pool))
    (ensure-true (member second-stream closed-streams :test #'eq))))

#+sbcl
(deftest client-connection-pool-closes-non-reusable-stream
  (let* ((first-stream
           (make-instance
            'binary-test-stream
            :input (ascii "HTTP/1.1 200 OK|CRLF||CRLF|abc")))
         (second-stream
           (make-instance
            'binary-test-stream
            :input
            (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|def")))
         (streams (list first-stream second-stream))
         (opened 0)
         (closed-streams nil)
         (pool
           (make-http-connection-pool
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (incf opened)
              (pop streams))
            :close-stream
            (lambda (stream)
              (push stream closed-streams))))
         (client (make-http-client :cache nil :connection-pool pool)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://example.test/first"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "abc" (octets-as-string (http-response-body response))))
    (ensure-equal 1 opened)
    (ensure-equal 1 (length closed-streams))
    (ensure-equal first-stream (first closed-streams))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://example.test/second"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "def" (octets-as-string (http-response-body response))))
    (ensure-equal 2 opened)
    (ensure-equal 1 (getf (http-connection-pool-stats pool) :idle-count))
    (ensure-equal 1 (http-connection-pool-clear pool))
    (ensure-equal 2 (length closed-streams))
    (ensure-true (member second-stream closed-streams :test #'eq))))

#+sbcl
(deftest client-connection-pool-expires-idle-streams
  (let* ((now 0)
         (streams
           (list
            (make-instance
             'binary-test-stream
             :input
             (ascii
              "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|abc"))
            (make-instance
             'binary-test-stream
             :input
             (ascii
              "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|def"))))
         (opened 0)
         (closed 0)
         (pool
           (make-http-connection-pool
            :idle-timeout 10
            :clock-function (lambda () now)
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (incf opened)
              (pop streams))
            :close-stream (lambda (stream)
                            (declare (ignore stream))
                            (incf closed))))
         (client (make-http-client :cache nil :connection-pool pool)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://example.test/"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response)))
    (ensure-equal 1 opened)
    (setf now 11)
    (ensure-equal 0 (getf (http-connection-pool-stats pool) :idle-count))
    (ensure-equal 1 closed)
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://example.test/"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "def" (octets-as-string (http-response-body response))))
    (ensure-equal 2 opened)
    (ensure-equal 1 (http-connection-pool-clear pool))
    (ensure-equal 2 closed)))

#+sbcl
(deftest client-connection-pool-expires-old-active-streams
  (let* ((now 0)
         (streams
           (list
            (make-instance
             'binary-test-stream
             :input
             (ascii
              "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|abcHTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|def"))
            (make-instance
             'binary-test-stream
             :input
             (ascii
              "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|ghi"))))
         (opened 0)
         (closed 0)
         (pool
           (make-http-connection-pool
            :max-connection-age 10
            :clock-function (lambda () now)
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (incf opened)
              (pop streams))
            :close-stream (lambda (stream)
                            (declare (ignore stream))
                            (incf closed))))
         (client (make-http-client :cache nil :connection-pool pool)))
    (dolist (time '(0 9 10))
      (setf now time)
      (multiple-value-bind (response effective)
          (http-client-send
           client
           (http-client-request client "GET" "http://example.test/"))
        (declare (ignore effective))
        (ensure-equal 200 (http-response-status response))))
    (ensure-equal 10 (http-connection-pool-max-connection-age pool))
    (ensure-equal 10
                  (getf (http-connection-pool-stats pool)
                        :max-connection-age))
    (ensure-equal 2 opened)
    (ensure-equal 1 closed)
    (ensure-equal 1 (http-connection-pool-clear pool))
    (ensure-equal 2 closed)))

(deftest client-connection-pool-validates-max-connection-age
  (let ((opener
          (lambda (request &key timeout deadline proxy-plan proxy
                            &allow-other-keys)
            (declare (ignore request timeout deadline proxy-plan proxy))
            nil)))
    (signals http-protocol-error
      (make-http-connection-pool
       :open-stream opener
       :max-connection-age -1))
    (signals http-protocol-error
      (make-http-connection-pool
       :open-stream opener
       :max-connection-age "later"))))

#+sbcl
(deftest client-connection-pool-proxy-tls-and-resolver-boundary
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (concatenate-octets
             (octets 5 0
                     5 0 0 1 0 0 0 0 0 0)
             (ascii
              "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok"))))
         (resolved-hosts nil)
         (resolved-timeouts nil)
         (resolved-deadlines nil)
         (upgraded-uris nil)
         (opened-plans nil)
         (closed 0)
         (proxy
           (make-http-proxy :scheme :socks5
                            :host "proxy.example"
                            :port 1080))
         (pool
           (make-http-connection-pool
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy))
              (push proxy-plan opened-plans)
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (incf closed))
            :resolve-host
            (lambda (host &key timeout deadline)
              (push host resolved-hosts)
              (push timeout resolved-timeouts)
              (push deadline resolved-deadlines)
              "192.0.2.10")
            :tls-upgrade
            (lambda (received-stream uri &key timeout deadline)
              (declare (ignore timeout deadline))
              (push uri upgraded-uris)
              received-stream)))
         (client
           (make-http-client :cache nil
                             :proxy proxy
                             :connection-pool pool)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "https://example.test/path"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (ensure-equal '("example.test") (reverse resolved-hosts))
    (ensure-equal '(30.0) (reverse resolved-timeouts))
    (ensure-true (realp (first resolved-deadlines)))
    (ensure-equal 1 (length upgraded-uris))
    (ensure-equal "https" (http-uri-scheme (first upgraded-uris)))
    (ensure-equal :socks5 (getf (first opened-plans) :mode))
    (let ((output (binary-test-output stream)))
      (ensure-equal
       (octets 5 1 0
               5 1 0 1 192 0 2 10 1 187)
       (subseq output 0 13))
      (ensure-true
       (search "GET /path HTTP/1.1"
               (octets-as-string (subseq output 13)))))
    (ensure-equal 1 (http-connection-pool-clear pool))
    (ensure-equal 1 closed)))

#+sbcl
(deftest client-streaming-response-over-direct-stream
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|abc")))
         (chunks nil)
         (closed nil)
         (client
           (make-http-client
            :cache nil
            :open-stream
            (lambda (request &key timeout deadline &allow-other-keys)
              (declare (ignore request timeout deadline))
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (setf closed t)))))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://example.test/stream")
         :on-body-chunk
         (lambda (chunk)
           (push (octets-as-string chunk) chunks))
         :collect-body-p nil)
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "" (octets-as-string (http-response-body response)))
      (ensure-equal '("abc") (reverse chunks)))
    (ensure-true closed)))

#+sbcl
(deftest client-response-headers-precede-body-chunks
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|abc")))
         (events nil)
         (client
           (make-http-client
            :cache nil
            :open-stream
            (lambda (request &key timeout deadline &allow-other-keys)
              (declare (ignore request timeout deadline))
              stream)
            :close-stream (lambda (closed-stream)
                            (ensure-equal stream closed-stream)))))
    (multiple-value-bind (response effective)
        (http-client-send
         client "GET" "http://example.test/stream"
         :on-response-headers
         (lambda (status headers)
           (push (list :headers status
                       (http-header-value headers "Content-Length"))
                 events))
         :on-body-chunk
         (lambda (chunk)
           (push (list :body (octets-as-string chunk)) events))
         :collect-body-p nil)
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal '((:headers 200 "3") (:body "abc"))
                    (reverse events)))))

#+sbcl
(deftest client-cancel-from-response-headers-skips-body
  (let* ((body-start-position
           (length
            (ascii "HTTP/1.1 401 Unauthorized|CRLF|Content-Length: 3|CRLF||CRLF|")))
         (stream
           (make-instance
            'binary-test-stream
            :input
            (ascii "HTTP/1.1 401 Unauthorized|CRLF|Content-Length: 3|CRLF||CRLF|bad")))
         (token (make-http-cancellation-token))
         (header-status nil)
         (body-callback-called-p nil)
         (close-count 0)
         (client
           (make-http-client
            :cache nil
            :open-stream
            (lambda (request &key timeout deadline &allow-other-keys)
              (declare (ignore request timeout deadline))
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (incf close-count))))
         (condition
           (handler-case
               (progn
                 (http-client-send
                  client "GET" "http://example.test/unauthorized"
                  :cancel-token token
                  :on-response-headers
                  (lambda (status headers)
                    (declare (ignore headers))
                    (setf header-status status)
                    (ensure-true (http-client-cancel-request token)))
                  :on-body-chunk
                  (lambda (chunk)
                    (declare (ignore chunk))
                    (setf body-callback-called-p t)))
                 nil)
             (http-client-request-cancelled (cancelled)
               cancelled))))
    (ensure-equal 401 header-status)
    (ensure-true (typep condition 'http-client-request-cancelled))
    (ensure-false body-callback-called-p)
    (ensure-equal body-start-position
                  (http-kit/test-core::binary-test-input-position stream))
    (ensure-equal 1 close-count)
    (ensure-false (http-client-cancel-request token))))

#+sbcl
(progn
  (defclass blocking-client-cancellation-stream (binary-test-stream)
    ((read-started
       :initarg :read-started
       :reader blocking-client-cancellation-stream-read-started)
     (release-read
       :initarg :release-read
       :reader blocking-client-cancellation-stream-release-read)
     (closed-p
       :initform nil
       :accessor blocking-client-cancellation-stream-closed-p)))

  (defun %blocking-client-cancellation-stream-wait (stream)
    (unless (blocking-client-cancellation-stream-closed-p stream)
      (sb-thread:signal-semaphore
       (blocking-client-cancellation-stream-read-started stream))
      (sb-thread:wait-on-semaphore
       (blocking-client-cancellation-stream-release-read stream)))
    stream)

  (defmethod sb-gray:stream-read-byte
      ((stream blocking-client-cancellation-stream))
    (let ((position
            (http-kit/test-core::binary-test-input-position stream))
          (input (http-kit/test-core::binary-test-input stream)))
      (if (< position (length input))
          (call-next-method)
          (progn
            (%blocking-client-cancellation-stream-wait stream)
            :eof))))

  (defmethod sb-gray:stream-read-sequence
      ((stream blocking-client-cancellation-stream) sequence
       &optional (start 0) end)
    (let* ((end (or end (length sequence)))
           (position
             (http-kit/test-core::binary-test-input-position stream))
           (input (http-kit/test-core::binary-test-input stream)))
      (cond ((or (= start end) (< position (length input)))
               (call-next-method))
            (t
             (%blocking-client-cancellation-stream-wait stream)
             start))))

  (deftest client-cancel-closes-blocked-stream-and-exits-worker
    (let* ((read-started (sb-thread:make-semaphore :count 0))
           (release-read (sb-thread:make-semaphore :count 0))
           (stream
             (make-instance
              'blocking-client-cancellation-stream
              :read-started read-started
              :release-read release-read
              :input
              (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 1|CRLF||CRLF|")))
           (token (make-http-cancellation-token))
           (worker-done (sb-thread:make-semaphore :count 0))
           (close-count 0)
           (body-callback-called-p nil)
           (cancel-condition nil)
           (worker
             (sb-thread:make-thread
              (lambda ()
                (unwind-protect
                     (handler-case
                         (http-client-send
                          (make-http-client
                           :cache nil
                           :open-stream
                           (lambda (request &key timeout deadline &allow-other-keys)
                             (declare (ignore request timeout deadline))
                             stream)
                           :close-stream
                           (lambda (closed-stream)
                             (ensure-equal stream closed-stream)
                             (incf close-count)
                             (setf (blocking-client-cancellation-stream-closed-p
                                    stream)
                                   t)
                             (sb-thread:signal-semaphore release-read)))
                          "GET" "http://example.test/blocked"
                          :cancel-token token
                          :on-body-chunk
                          (lambda (chunk)
                            (declare (ignore chunk))
                            (setf body-callback-called-p t)))
                       (http-client-request-cancelled (condition)
                         (setf cancel-condition condition)))
                  (sb-thread:signal-semaphore worker-done))))))
      (unwind-protect
           (progn
             (let ((read-started-p
                     (sb-thread:wait-on-semaphore read-started :timeout 5)))
               (ensure-true read-started-p)
               (ensure-true (http-client-cancel-request token))
               (ensure-false (http-client-cancel-request token)))
             (ensure-true
              (sb-thread:wait-on-semaphore worker-done :timeout 5))
             (sb-thread:join-thread worker :timeout 5)
             (ensure-true
              (typep cancel-condition 'http-client-request-cancelled))
             (ensure-equal 1 close-count)
             (ensure-false body-callback-called-p))
        (setf (blocking-client-cancellation-stream-closed-p stream) t)
        (sb-thread:signal-semaphore release-read)
        (sb-thread:join-thread worker :timeout 5)))))

#+sbcl
(deftest client-informational-response-over-direct-stream
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (ascii
             "HTTP/1.1 100 Continue|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok")))
         (statuses nil)
         (response-header-statuses nil)
         (closed nil)
         (client
           (make-http-client
            :cache nil
            :automatic-decompression-p nil
            :open-stream
            (lambda (request &key timeout deadline &allow-other-keys)
              (declare (ignore request timeout deadline))
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (setf closed t)))))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request
          client
          "POST"
          "http://example.test/continue"
          :headers (list (make-http-header "Expect" "100-continue"))
          :body "abc")
         :on-information
         (lambda (information)
           (push (http-response-status information) statuses))
         :on-response-headers
         (lambda (status headers)
           (declare (ignore headers))
           (push status response-header-statuses)))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (ensure-equal '(100) (reverse statuses))
    (ensure-equal '(200) (reverse response-header-statuses))
    (ensure-true closed)
    (ensure-equal
     (ascii
      "POST /continue HTTP/1.1|CRLF|Expect: 100-continue|CRLF|Host: example.test|CRLF|Content-Length: 3|CRLF||CRLF|abc")
     (binary-test-output stream))))

#+sbcl
(deftest client-streaming-request-body-over-direct-stream
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok")))
         (chunks (list (octets 97 98 99)
                       (octets 100 101)
                       nil))
         (calls 0)
         (closed nil)
         (client
           (make-http-client
            :cache nil
            :automatic-decompression-p nil
            :open-stream
            (lambda (request &key timeout deadline &allow-other-keys)
              (declare (ignore request timeout deadline))
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (setf closed t)))))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "POST" "http://example.test/upload")
         :request-body-function
         (lambda (maximum-size)
           (ensure-equal 65536 maximum-size)
           (incf calls)
           (pop chunks))
         :request-body-length 5)
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (ensure-equal 3 calls)
    (ensure-true (null chunks))
    (ensure-true closed)
    (ensure-equal
     (ascii "POST /upload HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 5|CRLF||CRLF|abcde")
     (binary-test-output stream))))

#+sbcl
(deftest client-streaming-request-body-over-connection-pool
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok")))
         (opened 0)
         (closed 0)
         (chunks (list (octets 97 98 99)
                       (octets 100 101)
                       nil))
         (pool
           (make-http-connection-pool
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy-plan proxy))
              (incf opened)
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (incf closed))))
         (client (make-http-client :cache nil
                                   :automatic-decompression-p nil
                                   :connection-pool pool)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "POST" "http://example.test/upload")
         :request-body-function
         (lambda (maximum-size)
           (ensure-equal 65536 maximum-size)
           (pop chunks)))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (ensure-equal 1 opened)
    (ensure-true (null chunks))
    (ensure-equal
     (ascii "POST /upload HTTP/1.1|CRLF|Host: example.test|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|3|CRLF|abc|CRLF|2|CRLF|de|CRLF|0|CRLF||CRLF|")
     (binary-test-output stream))
    (ensure-equal 1 (http-connection-pool-clear pool))
    (ensure-equal 1 closed)))

#+sbcl
(deftest client-http-forward-proxy-wire
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok")))
         (opened 0)
         (closed 0)
         (proxy
           (make-http-proxy :scheme :http
                            :host "proxy.example"
                            :port 8080
                            :username "user"
                            :password "pass"))
         (client
           (make-http-client
            :cache nil
            :proxy proxy
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy))
              (ensure-equal :forward (getf proxy-plan :mode))
              (incf opened)
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (incf closed)))))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET"
                              "http://example.test/path?x=1"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (let ((wire (octets-as-string (binary-test-output stream))))
      (ensure-true (search "GET http://example.test/path?x=1 HTTP/1.1" wire))
      (ensure-true (search "Proxy-Authorization: Basic dXNlcjpwYXNz" wire)))
    (ensure-equal 1 opened)
    (ensure-equal 1 closed)))

#+sbcl
(deftest client-http-connect-proxy-wire
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (ascii
             "HTTP/1.1 200 Connection Established|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok")))
         (upgrades 0)
         (proxy
           (make-http-proxy :scheme :http
                            :host "proxy.example"
                            :port 8080
                            :username "user"
                            :password "pass"))
         (client
           (make-http-client
            :cache nil
            :proxy proxy
            :tls-upgrade
            (lambda (received-stream uri &key timeout deadline)
              (declare (ignore timeout deadline))
              (ensure-equal "https" (http-uri-scheme uri))
              (incf upgrades)
              received-stream)
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy))
              (ensure-equal :connect (getf proxy-plan :mode))
              stream)
            :close-stream #'close)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "https://example.test/path"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (let* ((wire (octets-as-string (binary-test-output stream)))
           (authorization "Proxy-Authorization: Basic dXNlcjpwYXNz")
           (first-authorization (search authorization wire)))
      (ensure-true (search "CONNECT example.test:443 HTTP/1.1" wire))
      (ensure-true (search "GET /path HTTP/1.1" wire))
      (ensure-true first-authorization)
      (ensure-equal nil
                    (search authorization wire
                            :start2 (1+ first-authorization))))
    (ensure-equal 1 upgrades)))

#+sbcl
(deftest client-connect-proxy-challenge-authentication-reopens-tunnel-once
  (let* ((first
           (make-instance
            'binary-test-stream
            :input
            (ascii
             "HTTP/1.1 407 Proxy Authentication Required|CRLF|Proxy-Authenticate: Basic realm=\"proxy\"|CRLF|Content-Length: 0|CRLF||CRLF|")))
         (second
           (make-instance
            'binary-test-stream
            :input
            (ascii
             "HTTP/1.1 200 Connection Established|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok")))
         (streams (list first second))
         (opened 0)
         (closed nil)
         (provider-calls 0)
         (proxy
           (make-http-proxy :scheme :http
                            :host "proxy.example"
                            :port 8080))
         (client
           (make-http-client
            :cache nil
            :proxy proxy
            :proxy-challenge-auth-provider
            (lambda (request response proxy-plan challenges)
              (incf provider-calls)
              (ensure-equal "GET" (http-request-method request))
              (ensure-equal 407 (http-response-status response))
              (ensure-equal :connect (getf proxy-plan :mode))
              (ensure-equal "Basic"
                            (http-authentication-challenge-scheme
                             (first challenges)))
              "Basic dXNlcjpwYXNz")
            :tls-upgrade
            (lambda (stream uri &key timeout deadline)
              (declare (ignore uri timeout deadline))
              stream)
            :open-stream
            (lambda (request &key &allow-other-keys)
              (declare (ignore request))
              (incf opened)
              (pop streams))
            :close-stream
            (lambda (stream)
              (push stream closed)))))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "https://example.test/path"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (ensure-equal 2 opened)
    (ensure-equal 1 provider-calls)
    (ensure-true (member first closed))
    (ensure-false
     (search "Proxy-Authorization" (octets-as-string (binary-test-output first))))
    (ensure-true
     (search "Proxy-Authorization: Basic dXNlcjpwYXNz"
             (octets-as-string (binary-test-output second))))))

#+sbcl
(deftest client-proxy-negotiation-failure-closes-stream
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (ascii
             "HTTP/1.1 407 Proxy Authentication Required|CRLF|Content-Length: 0|CRLF||CRLF|")))
         (closed 0)
         (proxy
           (make-http-proxy :scheme :http
                            :host "proxy.example"
                            :port 8080))
         (client
           (make-http-client
            :cache nil
            :proxy proxy
            :tls-upgrade
            (lambda (received-stream uri &key timeout deadline)
              (declare (ignore received-stream uri timeout deadline))
              (error "TLS upgrade must not be reached."))
            :open-stream
            (lambda (request &key &allow-other-keys)
              (declare (ignore request))
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (incf closed)))))
    (signals http-proxy-error
      (http-client-send
       client
       (http-client-request client "GET" "https://example.test/path")))
    (ensure-equal 1 closed))
  (let* ((stream
           (make-instance 'binary-test-stream :input (ascii "")))
         (closed 0)
         (client
           (make-http-client
            :cache nil
            :tls-upgrade
            (lambda (received-stream uri &key timeout deadline)
              (declare (ignore received-stream uri timeout deadline))
              (error "TLS setup failed."))
            :open-stream
            (lambda (request &key &allow-other-keys)
              (declare (ignore request))
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (incf closed)))))
    (signals http-proxy-error
      (http-client-send
       client
       (http-client-request client "GET" "https://example.test/path")))
    (ensure-equal 1 closed)))

#+sbcl
(deftest client-socks5-proxy-wire
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (concatenate-octets
             (octets 5 0
                     5 0 0 1 0 0 0 0 0 0)
             (ascii
              "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok"))))
         (proxy
           (make-http-proxy :scheme :socks5
                            :host "proxy.example"
                            :port 1080))
         (client
           (make-http-client
            :cache nil
            :proxy proxy
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy))
              (ensure-equal :socks5 (getf proxy-plan :mode))
              stream)
            :close-stream #'close)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://192.0.2.1/path"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (let ((output (binary-test-output stream)))
      (ensure-equal
       (octets 5 1 0
               5 1 0 1 192 0 2 1 0 80)
       (subseq output 0 13))
      (ensure-true
       (search "GET /path HTTP/1.1"
               (octets-as-string (subseq output 13)))))))

#+sbcl
(deftest client-socks5-proxy-username-password-wire
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (concatenate-octets
             (octets 5 2
                     1 0
                     5 0 0 1 0 0 0 0 0 0)
             (ascii
              "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok"))))
         (proxy
           (make-http-proxy :scheme :socks5
                            :host "proxy.example"
                            :port 1080
                            :username "user:name"
                            :password "pass"))
         (client
           (make-http-client
            :cache nil
            :proxy proxy
            :open-stream
            (lambda (request &key &allow-other-keys)
              (declare (ignore request))
              stream)
            :close-stream #'close)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://192.0.2.1/path"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response)))
    (let ((output (binary-test-output stream)))
      (ensure-equal
       (concatenate-octets
        (octets 5 1 2
                1 9)
        (ascii "user:name")
        (octets 4)
        (ascii "pass")
        (octets 5 1 0 1 192 0 2 1 0 80))
       (subseq output 0 29)))))

#+sbcl
(deftest client-socks5-rejects-empty-credentials-before-writing
  (dolist (credentials '(("" "pass") ("user" "")))
    (let* ((stream (make-instance 'binary-test-stream :input (octets)))
           (proxy
             (make-http-proxy :scheme :socks5
                              :host "proxy.example"
                              :port 1080
                              :username (first credentials)
                              :password (second credentials)))
           (client
             (make-http-client
              :cache nil
              :proxy proxy
              :open-stream
              (lambda (request &key &allow-other-keys)
                (declare (ignore request))
                stream)
              :close-stream #'close)))
      (signals http-proxy-error
        (http-client-send
         client
         (http-client-request client "GET" "http://192.0.2.1/path")))
      (ensure-equal 0 (length (binary-test-output stream))))))

(deftest client-cookie-jar-enforces-partitioning-and-prefixes
  (let* ((jar (make-http-cookie-jar :clock-function (lambda () 1000)))
         (partition-key "https://top.example")
         (response (client-test-response
                    200
                    :headers
                    (list (make-http-header
                           "Set-Cookie"
                           "__Host-sid=abc; Secure; Path=/; SameSite=None; Partitioned")
                          (make-http-header
                           "Set-Cookie"
                           "__Host-bad=def; Path=/")
                          (make-http-header
                           "Set-Cookie"
                           "insecure=ghi; Path=/; SameSite=None")))))
    (http-cookie-jar-accept-response
     jar "https://example.test/login" response
     :now 1000
     :partition-key partition-key)
    (ensure-equal "__Host-sid=abc"
                  (http-cookie-jar-cookie-header
                   jar "https://example.test/dashboard"
                   :now 1001
                   :partition-key partition-key
                   :same-site-context :same-site
                   :method "GET"))
    (ensure-equal nil
                  (http-cookie-jar-cookie-header
                   jar "https://example.test/dashboard"
                   :now 1001
                   :partition-key "https://other.example"
                   :same-site-context :same-site
                   :method "GET"))
    (ensure-equal nil
                  (http-cookie-jar-cookie-header
                   jar "https://example.test/dashboard"
                   :now 1001
                   :same-site-context :same-site
                   :method "GET"))))

(deftest client-content-coding-selection
  (let* ((adapter (make-http-content-coding
                   :name "br"
                   :decoder #'identity))
         (parsed (parse-http-accept-encoding "gzip;q=0.1, br;q=0.8, identity;q=0")))
    (ensure-equal '(("gzip" . 0.1) ("br" . 0.8) ("identity" . 0))
                  parsed)
    (ensure-true (eq adapter
                     (http-select-content-coding
                      "gzip;q=0.1, br;q=0.8, identity;q=0"
                      (list "gzip" adapter))))
    (ensure-equal nil
                  (http-select-content-coding
                   "gzip;q=0, *;q=0, identity;q=0"
                   (list "gzip" adapter)))
    (ensure-equal "identity"
                  (http-select-content-coding
                   ""
                   (list "gzip" adapter)))
    (ensure-equal "identity"
                  (http-select-content-coding
                   "gzip;q=0.5"
                   (list "br" adapter)))))

(deftest client-protocol-alpn-selection
  (ensure-equal "http/1.1" (http-alpn-protocol-name :http1))
  (ensure-equal "h2" (http-alpn-protocol-name "HTTP2"))
  (ensure-equal "h3" (http-alpn-protocol-name "http-3"))
  (ensure-equal nil (http-alpn-protocol-name "spdy"))
  (ensure-equal "h2"
                (http-select-protocol
                 '("spdy/3" "h2" "http/1.1")
                 '(:http1 :http2))))

(deftest client-cache-request-directives
  (let* ((cache (make-http-cache :clock-function (lambda () 1000)))
         (request (make-http-request
                   :method "GET"
                   :uri "http://example.test/cache"))
         (response (client-test-response
                    200
                    :headers (list (make-http-header
                                    "Cache-Control" "max-age=1"))
                    :body (ascii "cached"))))
    (http-cache-store cache request response :now 1000)
    (multiple-value-bind (cached-response state entry)
        (http-cache-lookup
         cache
         (make-http-request
          :method "GET"
          :uri "http://example.test/cache"
          :headers (list (make-http-header "Cache-Control" "max-stale=10")))
         :now 1005)
      (declare (ignore entry))
      (ensure-equal :stale-allowed state)
      (ensure-equal "cached"
                    (octets-as-string (http-response-body cached-response))))
    (multiple-value-bind (cached-response state entry)
        (http-cache-lookup
         cache
         (make-http-request
          :method "GET"
          :uri "http://example.test/cache"
          :headers (list (make-http-header "Cache-Control" "no-store")))
         :now 1001)
      (declare (ignore cached-response entry))
      (ensure-equal :miss state))))

(deftest client-cache-revalidation-refreshes-stored-response
  (let ((calls 0)
        (seen-if-none-match nil)
        (seen-bodies nil)
        (revalidation-events nil)
        (revalidation-status nil)
        (revalidation-headers nil)
        (now 1000))
    (with-test-client (client
                       (lambda (request &key proxy-plan &allow-other-keys)
                         (declare (ignore proxy-plan))
                         (incf calls)
                         (setf seen-if-none-match
                               (http-header-value
                                (http-request-headers request)
                                "If-None-Match"))
                         (if (= calls 1)
                             (client-test-response
                              200
                              :headers (list (make-http-header "ETag" "\"v1\"")
                                             (make-http-header
                                              "Cache-Control" "max-age=0"))
                              :body (ascii "cached"))
                             (client-test-response
                              304
                              :headers (list (make-http-header
                                              "Cache-Control" "max-age=60")
                                             (make-http-header
                                              "ETag" "\"v1\"")))))
                       :cache (make-http-cache :clock-function (lambda () now))
                       :automatic-decompression-p nil)
      (let ((request (http-client-request client "GET"
                                          "http://example.test/resource")))
        (multiple-value-bind (response effective)
            (http-client-send client request)
          (ensure-equal 200 (http-response-status response))
          (ensure-equal request effective))
        (setf now 1001)
        (multiple-value-bind (response effective)
            (http-client-send
             client request
             :on-response-headers
             (lambda (status headers)
               (setf revalidation-status status
                     revalidation-headers headers)
               (push (list :headers status) revalidation-events))
             :on-body-chunk
             (lambda (chunk)
               (push (list :body (octets-as-string chunk))
                     revalidation-events)))
          (ensure-equal 200 (http-response-status response))
          (ensure-equal request effective)
          (ensure-equal "cached" (octets-as-string (http-response-body response)))
          (ensure-equal (http-response-status response) revalidation-status)
          (ensure-equal
           (mapcar (lambda (header)
                     (cons (http-header-name header)
                           (http-header-content header)))
                   (http-response-headers response))
           (mapcar (lambda (header)
                     (cons (http-header-name header)
                           (http-header-content header)))
                   revalidation-headers))
          (ensure-equal "\"v1\""
                        (http-header-value revalidation-headers "ETag"))
          (ensure-equal "\"v1\""
                        (http-header-value (http-response-headers response)
                                           "ETag"))
          (ensure-equal '((:headers 200) (:body "cached"))
                        (reverse revalidation-events)))
        (ensure-equal "\"v1\"" seen-if-none-match)
        (setf now 1002)
        (multiple-value-bind (response effective)
            (http-client-send
             client request
             :on-body-chunk (lambda (chunk)
                              (push (octets-as-string chunk) seen-bodies)))
          (ensure-equal 200 (http-response-status response))
          (ensure-equal request effective))
        (ensure-equal 2 calls)
        (ensure-equal '("cached") seen-bodies)))))

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

(deftest client-proxy-resolver-program-error-is-not-retried
  (let ((calls 0))
    (handler-case
        (http-kit/client::%proxy-resolve-host
         (lambda (host &key timeout deadline)
           (declare (ignore host timeout deadline))
           (incf calls)
           (error 'program-error))
         "service.example" 10.0 20.0 :keywords)
      (program-error ()))
    (ensure-equal 1 calls)))

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

(deftest client-http-date-imf-lexical-boundaries
  (ensure-equal (encode-universal-time 37 49 8 6 11 1994 0)
                (http-parse-date "Sun, 06 Nov 1994 08:49:37 GMT"))
  (dolist (invalid '("Funday, 06 Nov 1994 08:49:37 GMT"
                     "Sun, /6 Nov 1994 08:49:37 GMT"
                     "Sun, +6 Nov 1994 08:49:37 GMT"
                     "Sun, 06 Nov 199/ 08:49:37 GMT"))
    (ensure-equal nil (http-parse-date invalid)))
  (ensure-equal
   nil
   (http-parse-date (format nil "Sun,~C06 Nov 1994 08:49:37 GMT" #\Tab))))

(deftest client-http-date-rfc850-lexical-boundaries
  (let ((now (encode-universal-time 0 0 0 1 1 2026 0)))
    (ensure-equal (encode-universal-time 37 49 8 6 11 1994 0)
                  (http-parse-date "Sunday, 06-Nov-94 08:49:37 GMT"
                                   :now now))
    (dolist (invalid '("Funday, 06-Nov-94 08:49:37 GMT"
                       "Sunday, 06Nov94 08:49:37 GMT"
                       "Sunday, 6-Nov-94 08:49:37 GMT"
                       "Sunday, 06-No-94 08:49:37 GMT"
                       "Sunday, 06-Nov94 08:49:37 GMT"
                       "Sunday, 06-Nov-1994 08:49:37 GMT"
                       "Sunday, 06-Nov-9/ 08:49:37 GMT"
                       "Sunday, 06-Nov-XX 08:49:37 GMT"
                       "Sunday, /6-Nov-94 08:49:37 GMT"
                       "Sunday, 06-Foo-94 08:49:37 GMT"))
      (ensure-equal nil (http-parse-date invalid :now now)))))

(deftest client-http-date-asctime-lexical-boundaries
  (ensure-equal (encode-universal-time 37 49 8 6 11 1994 0)
                (http-parse-date "Sun Nov  6 08:49:37 1994"))
  (ensure-equal (encode-universal-time 37 49 8 10 11 1994 0)
                (http-parse-date "Thu Nov 10 08:49:37 1994"))
  (dolist (invalid '("Sun Nov 006 08:49:37 1994"
                     "Sun Nov  X 08:49:37 1994"
                     "Sun Nov  / 08:49:37 1994"
                     "Sun Nov 6X 08:49:37 1994"
                     "Sun Nov   6 08:49:37 1994"))
    (ensure-equal nil (http-parse-date invalid))))

(deftest client-form-percent-encoding-keyword-defaults
  (ensure-equal "-._~%20%2A%2B" (http-percent-encode "-._~ *+"))
  (ensure-equal "-._~+%2A%2B"
                (http-percent-encode "-._~ *+" :space-as-plus-p t))
  (ensure-equal "%2D%2E%5F%7E%20*%2B"
                (http-percent-encode "-._~ *+" :safe "*"))
  (ensure-equal "+%2B"
                (http-percent-encode " +" :safe " " :space-as-plus-p t)))

(deftest client-http-date-empty-retry-after
  (dolist (retry-after '("" "5"))
    (let ((calls 0)
          (sleeps nil))
      (let ((client
              (make-http-client
               :cache nil
               :sleep-function (lambda (seconds) (push seconds sleeps))
               :retry-policy (make-http-retry-policy
                              :max-attempts 2
                              :base-delay 0.25
                              :max-delay 10)
               :transport-function
               (lambda (request &key proxy-plan &allow-other-keys)
                 (declare (ignore request proxy-plan))
                 (incf calls)
                 (if (= calls 1)
                     (client-test-response
                      503 :headers (list (make-http-header "Retry-After"
                                                           retry-after)))
                     (client-test-response 200))))))
        (ensure-equal
         200
         (http-response-status
          (http-client-send
           client
           (http-client-request client "GET"
                                "http://example.test/retry-after-empty"))))
        (ensure-equal 2 calls)
        (ensure-equal (if (string= retry-after "") '(0.25) '(5.0))
                      sleeps)))))

#+sbcl
(deftest client-socks5-connect-reserved-byte-boundaries
  (dolist (reserved '(0 1))
    (let* ((stream
             (make-instance
              'binary-test-stream
              :input
              (concatenate-octets
               (octets 5 0 5 0 reserved 1 0 0 0 0 0 0)
               (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok"))))
           (opened 0)
           (closed 0)
           (caught nil)
           (response nil)
           (client
             (make-http-client
              :cache nil
              :proxy (make-http-proxy :scheme :socks5
                                      :host "proxy.example" :port 1080)
              :open-stream
              (lambda (request &key proxy-plan &allow-other-keys)
                (declare (ignore request))
                (ensure-equal :socks5 (getf proxy-plan :mode))
                (incf opened)
                stream)
              :close-stream
              (lambda (closed-stream)
                (ensure-true (eq stream closed-stream))
                (incf closed)
                (close closed-stream)))))
      (handler-case
          (setf response
                (http-client-send
                 client
                 (http-client-request client "GET" "http://192.0.2.1/path")))
        (http-proxy-error (condition) (setf caught condition)))
      (ensure-equal 1 opened)
      (ensure-equal 1 closed)
      (let ((output (binary-test-output stream))
            (negotiation (octets 5 1 0 5 1 0 1 192 0 2 1 0 80)))
        (if (zerop reserved)
            (progn
              (ensure-false caught)
              (ensure-equal 200 (http-response-status response))
              (ensure-equal (ascii "ok") (http-response-body response))
              (ensure-equal negotiation (subseq output 0 13))
              (ensure-true
               (search "GET /path HTTP/1.1"
                       (octets-as-string (subseq output 13)))))
            (progn
              (ensure-false response)
              (ensure-true (typep caught 'http-proxy-error))
              (ensure-equal :proxy (http-kit:http-error-operation caught))
              (ensure-equal '(:detail :reply :cause nil)
                            (http-client-error-detail caught))
              (ensure-equal negotiation output)))))))

(deftest client-multipart-casefold-duplicate-boundary
  (multiple-value-bind (body content-type)
      (make-http-multipart-body
       (list (make-http-multipart-part :name "field" :value "value"))
       :boundary "boundary")
    (dolist (header (list content-type "multipart/form-data; BOUNDARY=boundary"))
      (let ((parts (parse-http-multipart-body body :content-type header)))
        (ensure-equal 1 (length parts))
        (ensure-equal "field" (http-multipart-part-name (first parts)))
        (ensure-equal (ascii "value") (http-multipart-part-value (first parts)))))
    (dolist (duplicate '("boundary" "other"))
      (let ((caught nil))
        (handler-case
            (parse-http-multipart-body
             body :content-type
             (format nil "multipart/form-data; boundary=boundary; BOUNDARY=~A"
                     duplicate))
          (http-protocol-error (condition) (setf caught condition)))
        (ensure-true (typep caught 'http-protocol-error))
        (ensure-equal :client (http-kit:http-error-operation caught))
        (ensure-equal "boundary" (http-kit:http-protocol-error-detail caught))))))

(deftest client-multipart-explicit-boundary-conflict
  (multiple-value-bind (body content-type)
      (make-http-multipart-body
       (list (make-http-multipart-part :name "field" :value "value"))
       :boundary "boundary")
    (dolist (header (list content-type nil))
      (let ((parts (parse-http-multipart-body
                    body :boundary "boundary" :content-type header)))
        (ensure-equal 1 (length parts))
        (ensure-equal "field" (http-multipart-part-name (first parts)))
        (ensure-equal (ascii "value") (http-multipart-part-value (first parts)))))
    (let ((caught nil))
      (handler-case
          (parse-http-multipart-body body :boundary "other"
                                         :content-type content-type)
        (http-protocol-error (condition) (setf caught condition)))
      (ensure-true (typep caught 'http-protocol-error))
      (ensure-equal :client (http-kit:http-error-operation caught))
      (ensure-equal '("other" "boundary")
                    (http-kit:http-protocol-error-detail caught)))))

(deftest client-auth-cache-boundary-digest-auth-int
  ;; Fixed independent hashlib/OpenSSL vectors; MD5 auth is RFC 2617's anchor.
  (flet ((challenge (algorithm qop)
           (first (http-parse-authentication-challenges
                   (format nil "Digest realm=\"testrealm@host.com\", nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", algorithm=~A, qop=\"~A\""
                           algorithm qop))))
         (expected (algorithm qop response)
           (format nil "Digest username=\"Mufasa\", realm=\"testrealm@host.com\", uri=\"/dir/index.html\", algorithm=~A, nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", nc=00000001, cnonce=\"0a4f113b\", qop=~A, response=\"~A\""
                   algorithm qop response)))
    (dolist (vector
             '(("MD5" "de72ddd8b1d5f35e7ac00c2eb6a4fa55"
                "5e6610ecf9ba3017a4870ad48e3ad30b"
                "6629fae49393a05397450978507c4ef1")
               ("SHA-256" "7212bb23545b99ab4cdb6e7e0f244cabab856de32cecc638c91d249244157cfd"
                "73ef36480ea6a777c5d2f7cdcdb7882040806d8cd3340b7d5125792a82921f90"
                "5abdd07184ba512a22c53f41470e5eea7dcaa3a93a59b630c13dfe0a5dc6e38b")
               ("SHA-512-256" "20734492c5c36bea45b87c7e1013c706f0f77f2456129f1678a3feec1db0bb8c"
                "26d9d345442cc35976069b5aba9960f5d8e66abc7453835fd11e14fba96f616f"
                "f23c08ec7334a881f8286e68450ddbd9f0cd91c41481f0e1433604da8113c6dc")))
      (destructuring-bind (algorithm binary empty auth) vector
        (ensure-equal
         (expected algorithm "auth-int" binary)
         (http-digest-authorization
          (challenge algorithm "auth, auth-int") "GET" "/dir/index.html"
          "Mufasa" "Circle Of Life" "0a4f113b"
          :qop "auth-int" :entity-body (octets 0 1 127 128 255)))
        (ensure-equal
         (expected algorithm "auth-int" binary)
         (http-digest-authorization
          (challenge algorithm "auth-int") "GET" "/dir/index.html"
          "Mufasa" "Circle Of Life" "0a4f113b"
          :entity-body (octets 0 1 127 128 255)))
        (ensure-equal
         (expected algorithm "auth-int" empty)
         (http-digest-authorization
          (challenge algorithm "auth-int") "GET" "/dir/index.html"
          "Mufasa" "Circle Of Life" "0a4f113b" :qop "auth-int"))
        (ensure-equal
         (expected algorithm "auth" auth)
         (http-digest-authorization
          (challenge algorithm "auth, auth-int") "GET" "/dir/index.html"
          "Mufasa" "Circle Of Life" "0a4f113b"
          :entity-body (octets 0 1 127 128 255)))))))

(deftest client-auth-cache-boundary-digest-session
  (flet ((authorization (algorithm)
           (http-digest-authorization
            (first (http-parse-authentication-challenges
                    (format nil "Digest realm=\"testrealm@host.com\", nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", algorithm=~A, qop=auth"
                            algorithm)))
            "GET" "/dir/index.html" "Mufasa" "Circle Of Life" "0a4f113b"))
         (expected (algorithm response)
           (format nil "Digest username=\"Mufasa\", realm=\"testrealm@host.com\", uri=\"/dir/index.html\", algorithm=~A, nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", nc=00000001, cnonce=\"0a4f113b\", qop=auth, response=\"~A\""
                   algorithm response)))
    (dolist (vector
             '(("MD5" "8e3825c57e897f5a0dec6c2d4e5059d0"
                "6629fae49393a05397450978507c4ef1")
               ("SHA-256" "b8822e12417cb7750f4e2b8515f0dcf25b7dd26993e80bee1426201446a7f59b"
                "5abdd07184ba512a22c53f41470e5eea7dcaa3a93a59b630c13dfe0a5dc6e38b")
               ("SHA-512-256" "0d21f0db3ec5cda5b850c0afa3bc29b4a3c5a6191959ff1baf511d4b38eb6b1e"
                "f23c08ec7334a881f8286e68450ddbd9f0cd91c41481f0e1433604da8113c6dc")))
      (destructuring-bind (algorithm session plain) vector
        (let ((session-algorithm (concatenate 'string algorithm "-sess")))
          (ensure-equal (expected session-algorithm session)
                        (authorization session-algorithm))
          (ensure-equal (expected algorithm plain)
                        (authorization algorithm)))))))

(deftest client-auth-cache-boundary-digest-negotiation
  (flet ((challenge (algorithm offered)
           (first (http-parse-authentication-challenges
                   (format nil "Digest realm=\"testrealm@host.com\", nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", algorithm=~A, qop=\"~A\""
                           algorithm offered)))))
    (dolist (case '(("MD5" "auth" "auth-int" "auth-int")
                    ("MD5" "auth-int" "auth" "auth")
                    ("MD5" "unknown" nil "unknown")
                    ("MD5" "auth-int" nil "auth-int")
                    ("SHA-999" "auth" nil "SHA-999")))
      (destructuring-bind (algorithm offered requested detail) case
        (let ((caught nil))
          (handler-case
              (http-digest-authorization
               (challenge algorithm offered) "GET" "/dir/index.html"
               "Mufasa" "Circle Of Life" "0a4f113b" :qop requested)
            (http-protocol-error (condition) (setf caught condition)))
          (ensure-true (typep caught 'http-protocol-error))
          (ensure-equal :client (http-kit:http-error-operation caught))
          (ensure-equal detail (http-kit:http-protocol-error-detail caught)))))
    (dolist (offered '("auth" "AUTH, auth-int"))
      (ensure-equal
       "Digest username=\"Mufasa\", realm=\"testrealm@host.com\", uri=\"/dir/index.html\", algorithm=MD5, nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", nc=00000001, cnonce=\"0a4f113b\", qop=auth, response=\"6629fae49393a05397450978507c4ef1\""
       (http-digest-authorization
        (challenge "MD5" offered) "GET" "/dir/index.html"
        "Mufasa" "Circle Of Life" "0a4f113b" :qop "AUTH")))))

(deftest client-auth-cache-boundary-vary-star
  (let ((english
          (make-http-request
           :method "GET" :uri "http://example.test/vary-star"
           :headers (list (make-http-header "Accept-Language" "en"))))
        (french
          (make-http-request
           :method "GET" :uri "http://example.test/vary-star"
           :headers (list (make-http-header "Accept-Language" "fr")))))
    (dolist (vary '("*" "Accept-Language, *"))
      (let ((cache (make-http-cache :clock-function (lambda () 1000))))
        (ensure-false
         (http-cache-store
          cache english
          (client-test-response
           200 :body (ascii "uncacheable")
           :headers (list (make-http-header "Cache-Control" "max-age=60")
                          (make-http-header "Vary" vary)))
          :now 1000))
        (ensure-equal nil (http-cache-entries cache))
        (multiple-value-bind (response state entry reason)
            (http-cache-lookup cache english :now 1001)
          (ensure-false response)
          (ensure-equal :miss state)
          (ensure-false entry)
          (ensure-equal :miss reason))
        (ensure-true
         (http-cache-store
          cache english
          (client-test-response
           200 :body (ascii "English")
           :headers (list (make-http-header "Cache-Control" "max-age=60")
                          (make-http-header "Vary" "Accept-Language")))
          :now 1000))
        (multiple-value-bind (response state entry reason)
            (http-cache-lookup cache english :now 1001)
          (ensure-equal :fresh state)
          (ensure-equal (ascii "English") (http-response-body response))
          (ensure-true entry)
          (ensure-false reason))
        (multiple-value-bind (response state entry reason)
            (http-cache-lookup cache french :now 1001)
          (ensure-false response)
          (ensure-equal :miss state)
          (ensure-false entry)
          (ensure-equal :miss reason))
        (ensure-equal 1 (length (http-cache-entries cache)))))))

(deftest client-auth-cache-boundary-lru
  (let ((requests
          (loop for name in '("a" "b" "c")
                collect (make-http-request
                         :method "GET"
                         :uri (format nil "http://example.test/lru/~A" name)))))
    (dolist (touch-a '(nil t))
      (let ((cache (make-http-cache :max-entries 2
                                    :clock-function (lambda () 1000))))
        (loop for request in (subseq requests 0 2)
              for body in '("a" "b")
              do (ensure-true
                  (http-cache-store
                   cache request
                   (client-test-response
                    200 :body (ascii body)
                    :headers (list (make-http-header "Cache-Control" "max-age=60")))
                   :now 1000)))
        (ensure-equal
         (mapcar #'http-cache-key (list (second requests) (first requests)))
         (mapcar #'http-cache-entry-key (http-cache-entries cache)))
        (when touch-a
          (multiple-value-bind (response state entry)
              (http-cache-lookup cache (first requests) :now 1001)
            (ensure-equal :fresh state)
            (ensure-equal (ascii "a") (http-response-body response))
            (ensure-equal 1001 (http-cache-entry-accessed-at entry)))
          (ensure-equal
           (mapcar #'http-cache-key (subseq requests 0 2))
           (mapcar #'http-cache-entry-key (http-cache-entries cache))))
        (ensure-true
         (http-cache-store
          cache (third requests)
          (client-test-response
           200 :body (ascii "c")
           :headers (list (make-http-header "Cache-Control" "max-age=60")))
          :now 1002))
        (let ((retained (if touch-a (first requests) (second requests)))
              (evicted (if touch-a (second requests) (first requests))))
          (ensure-equal
           (mapcar #'http-cache-key (list (third requests) retained))
           (mapcar #'http-cache-entry-key (http-cache-entries cache)))
          (ensure-equal 2 (length (http-cache-entries cache)))
          (multiple-value-bind (response state entry reason)
              (http-cache-lookup cache evicted :now 1003)
            (ensure-false response)
            (ensure-equal :miss state)
            (ensure-false entry)
            (ensure-equal :miss reason))
          (dolist (request-and-body
                    (list (cons retained (if touch-a "a" "b"))
                          (cons (third requests) "c")))
            (multiple-value-bind (response state entry reason)
                (http-cache-lookup cache (car request-and-body) :now 1003)
              (ensure-equal :fresh state)
              (ensure-equal (ascii (cdr request-and-body))
                            (http-response-body response))
              (ensure-true entry)
              (ensure-false reason))))))))

(deftest client-auth-cache-boundary-freshness
  (let* ((now (http-parse-date "Sun, 06 Nov 1994 08:49:57 GMT"))
         (request (make-http-request :method "GET"
                                     :uri "http://example.test/freshness")))
    (dolist (fields
              '((("Expires" "Sun, 06 Nov 1994 08:50:07 GMT"))
                (("Last-Modified" "Sun, 06 Nov 1994 08:48:17 GMT"))
                (("Expires" "Sun, 06 Nov 1994 08:50:07 GMT")
                 ("Last-Modified" "Sun, 06 Nov 1994 08:33:17 GMT"))
                (("Cache-Control" "max-age=10")
                 ("Expires" "Sun, 06 Nov 1994 09:49:57 GMT"))))
      (let* ((cache (make-http-cache :clock-function (lambda () now)))
             (entry
               (http-cache-store
                cache request
                (client-test-response
                 200 :body (ascii "fresh")
                 :headers (cons (make-http-header "Date"
                                                  "Sun, 06 Nov 1994 08:49:57 GMT")
                                (mapcar (lambda (field)
                                          (apply #'make-http-header field))
                                        fields)))
                :now now)))
        (ensure-true entry)
        (ensure-equal (+ now 10) (http-cache-entry-expires-at entry))
        (multiple-value-bind (response state snapshot reason)
            (http-cache-lookup cache request :now (+ now 9))
          (ensure-equal :fresh state)
          (ensure-equal (ascii "fresh") (http-response-body response))
          (ensure-equal "9" (http-header-value (http-response-headers response) "Age"))
          (ensure-equal (+ now 10) (http-cache-entry-expires-at snapshot))
          (ensure-false reason))
        (multiple-value-bind (response state snapshot reason)
            (http-cache-lookup cache request :now (+ now 10))
          (ensure-false response)
          (ensure-equal :stale state)
          (ensure-equal (+ now 10) (http-cache-entry-expires-at snapshot))
          (ensure-equal :stale reason))))
    (dolist (last-modified '("Sun, 06 Nov 1994 08:49:57 GMT"
                            "Sun, 06 Nov 1994 08:50:07 GMT"))
      (let ((cache (make-http-cache :clock-function (lambda () now))))
        (let ((entry
                (http-cache-store
                 cache request
                 (client-test-response
                  200 :body (ascii "stale")
                  :headers (list (make-http-header "Date"
                                                  "Sun, 06 Nov 1994 08:49:57 GMT")
                                 (make-http-header "Last-Modified" last-modified)))
                 :now now)))
          (ensure-true entry)
          (ensure-equal now (http-cache-entry-expires-at entry)))
        (multiple-value-bind (response state entry reason)
            (http-cache-lookup cache request :now now)
          (ensure-false response)
          (ensure-equal :stale state)
          (ensure-true entry)
          (ensure-equal :stale reason))))))

(deftest client-auth-cache-completion-session-auth-int
  ;; Fixed vectors are independently derived from RFC 7616 A1/A2 with hashlib
  ;; and OpenSSL SHA-512/256, including embedded NUL and non-ASCII octets.
  (flet ((authorization (algorithm)
           (http-digest-authorization
            (first (http-parse-authentication-challenges
                    (format nil "Digest realm=\"testrealm@host.com\", nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", algorithm=~A, qop=\"auth, auth-int\""
                            algorithm)))
            "GET" "/dir/index.html" "Mufasa" "Circle Of Life" "0a4f113b"
            :qop "auth-int" :entity-body (octets 0 1 127 128 255)))
         (expected (algorithm response)
           (format nil "Digest username=\"Mufasa\", realm=\"testrealm@host.com\", uri=\"/dir/index.html\", algorithm=~A, nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", nc=00000001, cnonce=\"0a4f113b\", qop=auth-int, response=\"~A\""
                   algorithm response)))
    (dolist (vector
              '(("MD5" "f551a95d2094039c8e7c3611b11f48d1"
                 "de72ddd8b1d5f35e7ac00c2eb6a4fa55")
                ("SHA-256" "347eea880778d4ff72ab5026cacb72341c10cc813d7e9c0bdfe924958687c992"
                 "7212bb23545b99ab4cdb6e7e0f244cabab856de32cecc638c91d249244157cfd")
                ("SHA-512-256" "32b4618c0a4647f4b73c2575f9fdf9689c27379f9741e1e2bb762dba502e90b9"
                 "20734492c5c36bea45b87c7e1013c706f0f77f2456129f1678a3feec1db0bb8c")))
      (destructuring-bind (algorithm session plain) vector
        (let ((session-algorithm (concatenate 'string algorithm "-sess")))
          (ensure-equal (expected session-algorithm session)
                        (authorization session-algorithm))
          (ensure-equal (expected algorithm plain)
                        (authorization algorithm)))))))

(deftest client-auth-cache-completion-empty-octet-auth-int
  (flet ((authorization (algorithm requested)
           (http-digest-authorization
            (first (http-parse-authentication-challenges
                    (format nil "Digest realm=\"testrealm@host.com\", nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", algorithm=~A, qop=auth-int"
                            algorithm)))
            "GET" "/dir/index.html" "Mufasa" "Circle Of Life" "0a4f113b"
            :qop requested :entity-body (octets)))
         (expected (algorithm response)
           (format nil "Digest username=\"Mufasa\", realm=\"testrealm@host.com\", uri=\"/dir/index.html\", algorithm=~A, nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", nc=00000001, cnonce=\"0a4f113b\", qop=auth-int, response=\"~A\""
                   algorithm response)))
    (dolist (vector
              '(("MD5" "5e6610ecf9ba3017a4870ad48e3ad30b")
                ("MD5-sess" "0c9976da714ad8905eb2720abe326a63")
                ("SHA-256" "73ef36480ea6a777c5d2f7cdcdb7882040806d8cd3340b7d5125792a82921f90")
                ("SHA-256-sess" "6d80a12b361a15d2f54b18e8db571f74a1d8c7cfa25cc3f04a34cb2e7575b0d8")
                ("SHA-512-256" "26d9d345442cc35976069b5aba9960f5d8e66abc7453835fd11e14fba96f616f")
                ("SHA-512-256-sess" "36b749873d2253a662d65946480206084ffd7ab8fcd88f6354ca1d33bf173923")))
      (destructuring-bind (algorithm response) vector
        ;; An empty octet vector supplies an entity even though its length is zero.
        (ensure-equal (expected algorithm response) (authorization algorithm nil))
        (ensure-equal (expected algorithm response)
                      (authorization algorithm "auth-int"))))))

(deftest client-auth-cache-completion-clock-callback
  (let* ((base (http-parse-date "Sun, 06 Nov 1994 08:49:57 GMT"))
         (request (make-http-request :method "GET"
                                     :uri "http://example.test/clock-callback"))
         (origin-response
           (client-test-response
            200 :body (ascii "clocked")
            :headers (list (make-http-header "Date"
                                            "Sun, 06 Nov 1994 08:49:57 GMT")
                           (make-http-header "Cache-Control" "max-age=10")))))
    (dolist (explicit-now '(nil t))
      (let* ((clock-value (if explicit-now (+ base 1000) base))
             (clock-calls 0)
             (cache (make-http-cache
                     :clock-function (lambda () (incf clock-calls) clock-value)))
             (stored (if explicit-now
                         (http-cache-store cache request origin-response :now base)
                         (http-cache-store cache request origin-response))))
        (ensure-true stored)
        (ensure-equal (if explicit-now 0 1) clock-calls)
        (ensure-equal base (http-cache-entry-stored-at stored))
        (ensure-equal (+ base 10) (http-cache-entry-expires-at stored))
        (loop for offset in '(0 9 10 11)
              for lookup-number from 1
              do (multiple-value-bind (response state entry reason)
                     (if explicit-now
                         (http-cache-lookup cache request :now (+ base offset))
                         (progn
                           (setf clock-value (+ base offset))
                           (http-cache-lookup cache request)))
                   (ensure-equal (if explicit-now 0 (1+ lookup-number)) clock-calls)
                   (ensure-true entry)
                   (ensure-equal (+ base 10) (http-cache-entry-expires-at entry))
                   (if (< offset 10)
                       (progn
                         (ensure-equal :fresh state)
                         (ensure-false reason)
                         (ensure-equal (ascii "clocked") (http-response-body response))
                         (ensure-equal (write-to-string offset)
                                       (http-header-value
                                        (http-response-headers response) "Age"))
                         (ensure-equal (+ base offset)
                                       (http-cache-entry-accessed-at entry)))
                       (progn
                         (ensure-false response)
                         (ensure-equal :stale state)
                         (ensure-equal :stale reason)))))))))

(deftest client-protocol-boundary-aliases-and-offered-order
  (dolist (case '(("http/1.1" "http/1.1")
                  ("http1" "http/1.1")
                  ("http-1.1" "http/1.1")
                  ("http-1-1" "http/1.1")
                  ("h2" "h2")
                  ("http2" "h2")
                  ("http-2" "h2")
                  ("h3" "h3")
                  ("http3" "h3")
                  ("http-3" "h3")))
    (destructuring-bind (alias expected) case
      (let ((input (copy-seq alias)))
        (ensure-equal expected (http-alpn-protocol-name input))
        (ensure-equal alias input))
      (ensure-equal expected (http-alpn-protocol-name (string-upcase alias)))
      (ensure-equal expected
                    (http-alpn-protocol-name
                     (intern (string-upcase alias) :keyword)))))
  (dolist (input '(nil t 42 #\h #("h2") ("h2") "spdy" :spdy "" :http4))
    (ensure-false (http-alpn-protocol-name input)))
  (dolist (case '((("h3" "h2" "http/1.1") (:http1 :http2 :http3) "h3")
                  ((:http1 :http2 :http3) ("h3" "h2" "http/1.1") "http/1.1")
                  (("SPDY" :http-2 :http-3) (42 :unknown "HTTP2" "h3") "h2")
                  ((42 nil "unknown" "HTTP-3") (:http3) "h3")
                  (("h2" "h2" "h3") (:http3 :http2 :http2) "h2")
                  (("h3") (:http1 :http2) nil)
                  (("h2") nil nil)
                  (nil (:http2) nil)))
    (destructuring-bind (offered supported expected) case
      (ensure-equal expected (http-select-protocol offered supported)))))

(deftest client-protocol-boundary-http3-request-predicate
  (dolist (input (list nil t 42 "HTTP/3" (client-test-response 200)
                      '(:protocol-version "HTTP/3") #()))
    (ensure-false (http-request-http3-p input)))
  (dolist (version '("HTTP/1.0" "HTTP/1.1" "HTTP/2" "HTTP/3"))
    (let ((request (make-http-request :method "GET"
                                      :uri "https://example.test/"
                                      :protocol-version version)))
      (if (string= version "HTTP/3")
          (ensure-true (http-request-http3-p request))
          (ensure-false (http-request-http3-p request))))))

#+sbcl
(deftest client-proxy-socks5-resolver-preserves-ipv6-hex-case
  (dolist (address '("::a" "::A"))
    (let* ((stream
             (make-instance
              'binary-test-stream
              :input
              (concatenate-octets
               (octets 5 0 5 0 0 1 0 0 0 0 0 0)
               (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|"))))
           (resolved-hosts nil)
           (opened-plans nil)
           (closed 0)
           (pool
             (make-http-connection-pool
              :open-stream
              (lambda (request &key timeout deadline proxy-plan proxy
                                &allow-other-keys)
                (declare (ignore request timeout deadline proxy))
                (push proxy-plan opened-plans)
                stream)
              :close-stream
              (lambda (closed-stream)
                (ensure-equal stream closed-stream)
                (incf closed))
              :resolve-host
              (lambda (host &key timeout deadline)
                (declare (ignore timeout deadline))
                (push host resolved-hosts)
                address)))
           (client
             (make-http-client
              :cache nil
              :proxy (make-http-proxy :scheme :socks5
                                      :host "proxy.test" :port 1080)
              :connection-pool pool)))
      (let ((response (http-client-send client "GET" "http://example.test/")))
        (ensure-equal 204 (http-response-status response))
        (ensure-equal #() (http-response-body response)))
      (ensure-equal '("example.test") (reverse resolved-hosts))
      (ensure-equal :socks5 (getf (first opened-plans) :mode))
      (let ((output (binary-test-output stream)))
        (ensure-equal
         (concatenate-octets
          (octets 5 1 0 5 1 0 4)
          (make-array 15 :element-type '(unsigned-byte 8) :initial-element 0)
          (octets 10 0 80))
         (subseq output 0 25))
        (ensure-equal
         0 (search "GET / HTTP/1.1" (octets-as-string (subseq output 25)))))
      (ensure-equal 1 (http-connection-pool-clear pool))
      (ensure-equal 1 closed))))

(deftest client-strict-transport-quoted-extension-accepted
  (dolist (extension '("extension=\"\""
                       "extension=\"plain\""
                       "extension=\"a\\\"b\\\\c\""))
    (let* ((now 1000)
           (store (make-http-strict-transport-store
                   :clock-function (lambda () now))))
      (ensure-true
       (eq store
           (http-strict-transport-store-note-response
            store "https://example.test/learn"
            (client-test-response
             200 :headers
             (list (make-http-header
                    "Strict-Transport-Security"
                    (format nil "max-age=60; ~A; includeSubDomains"
                            extension)))))))
      (let ((policies (http-strict-transport-store-policies store)))
        (ensure-equal 1 (length policies))
        (ensure-equal "example.test"
                      (http-strict-transport-policy-host (first policies)))
        (ensure-equal 1060
                      (http-strict-transport-policy-expires-at (first policies)))
        (ensure-true
         (http-strict-transport-policy-include-subdomains-p (first policies))))
      (ensure-equal
       "https://api.example.test:443/path?q=1"
       (http-uri-string
        (http-strict-transport-store-upgrade-uri
         store "http://api.example.test:80/path?q=1")))
      (setf now 1059)
      (ensure-true
       (http-strict-transport-store-known-host-p store "api.example.test"))
      (setf now 1060)
      (ensure-true
       (not (http-strict-transport-store-known-host-p store "example.test")))
      (ensure-true
       (not (http-strict-transport-store-known-host-p store "api.example.test"))))))

(deftest client-strict-transport-quoted-extension-rejected-preserves-policy
  (dolist (case '((0 "extension=")
                  (600 "extension=\"open")
                  (0 "extension=\"a\"b\"")
                  (600 "extension=\"tail\\\"")))
    (destructuring-bind (max-age extension) case
      (let* ((now 1000)
             (store (make-http-strict-transport-store
                     :clock-function (lambda () now))))
        (http-strict-transport-store-note-response
         store "https://example.test/learn"
         (client-test-response
          200 :headers
          (list (make-http-header "Strict-Transport-Security"
                                  "max-age=60; includeSubDomains"))))
        (ensure-true
         (http-strict-transport-store-known-host-p store "api.example.test"))
        (ensure-equal
         1060 (http-strict-transport-policy-expires-at
               (first (http-strict-transport-store-policies store))))
        (setf now 1010)
        (ensure-true
         (eq store
             (http-strict-transport-store-note-response
              store "https://example.test/learn"
              (client-test-response
               200 :headers
               (list (make-http-header
                      "Strict-Transport-Security"
                      (format nil "max-age=~D; ~A" max-age extension)))))))
        (let ((policies (http-strict-transport-store-policies store)))
          (ensure-equal 1 (length policies))
          (ensure-equal
           1060 (http-strict-transport-policy-expires-at (first policies)))
          (ensure-true
           (http-strict-transport-policy-include-subdomains-p (first policies))))
        (ensure-equal
         "https://api.example.test:443/path?q=1"
         (http-uri-string
          (http-strict-transport-store-upgrade-uri
           store "http://api.example.test:80/path?q=1")))
        (setf now 1059)
        (ensure-true
         (http-strict-transport-store-known-host-p store "api.example.test"))
        (setf now 1060)
        (ensure-true
         (not (http-strict-transport-store-known-host-p store "example.test")))))))

(deftest client-strict-transport-parser-quoted-semicolon-accepted
  (dolist (extension '("extension=token"
                       "extension=\"a;b\""
                       "extension=\"a\\\";b\""))
    (let ((store (make-http-strict-transport-store
                  :clock-function (lambda () 1000))))
      (http-strict-transport-store-note-response
       store "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header
                            "Strict-Transport-Security"
                            (format nil "max-age=60; ~A; includeSubDomains"
                                    extension)))))
      (let ((policies (http-strict-transport-store-policies store)))
        (ensure-equal 1 (length policies))
        (ensure-equal 1060 (http-strict-transport-policy-expires-at (first policies)))
        (ensure-true (http-strict-transport-policy-include-subdomains-p
                      (first policies))))
      (ensure-true (http-strict-transport-store-known-host-p store "api.example.test"))
      (ensure-equal "https://api.example.test:443/path?q=1"
                    (http-uri-string (http-strict-transport-store-upgrade-uri
                                      store "http://api.example.test:80/path?q=1"))))))

(deftest client-strict-transport-parser-malformed-quoted-semicolon
  (dolist (extension '("extension=\"open;b"
                       "extension=\"a\"b;c\""
                       "extension=\"tail\\\";"))
    (let* ((now 1000)
           (store (make-http-strict-transport-store
                   :clock-function (lambda () now))))
      (http-strict-transport-store-note-response
       store "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Strict-Transport-Security"
                                            "max-age=60; includeSubDomains"))))
      (ensure-true (http-strict-transport-store-known-host-p store "api.example.test"))
      (ensure-equal 1060 (http-strict-transport-policy-expires-at
                          (first (http-strict-transport-store-policies store))))
      (setf now 1010)
      (http-strict-transport-store-note-response
       store "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Strict-Transport-Security"
                                            (format nil "max-age=0; ~A" extension)))))
      (let ((policies (http-strict-transport-store-policies store)))
        (ensure-equal 1 (length policies))
        (ensure-equal 1060 (http-strict-transport-policy-expires-at (first policies)))
        (ensure-true (http-strict-transport-policy-include-subdomains-p
                      (first policies))))
      (ensure-true (http-strict-transport-store-known-host-p store "api.example.test")))))

(deftest client-strict-transport-parser-empty-directives-accepted
  (dolist (value '("max-age=60; includeSubDomains"
                   "; max-age=60;; includeSubDomains;"
                   " ; max-age=\"60\"; extension=\"a;b\";; includeSubDomains; "))
    (let ((store (make-http-strict-transport-store
                  :clock-function (lambda () 1000))))
      (http-strict-transport-store-note-response
       store "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Strict-Transport-Security" value))))
      (let ((policies (http-strict-transport-store-policies store)))
        (ensure-equal 1 (length policies))
        (ensure-equal 1060 (http-strict-transport-policy-expires-at (first policies)))
        (ensure-true (http-strict-transport-policy-include-subdomains-p
                      (first policies))))
      (ensure-true (http-strict-transport-store-known-host-p store "api.example.test"))
      (ensure-equal "https://api.example.test:443/path"
                    (http-uri-string (http-strict-transport-store-upgrade-uri
                                      store "http://api.example.test:80/path"))))))

(deftest client-strict-transport-parser-empty-directives-without-max-age
  (dolist (value '("" ";" ";;;" " ; extension=token;; " "extension=\"a;b\""))
    (let* ((now 1000)
           (store (make-http-strict-transport-store
                   :clock-function (lambda () now))))
      (http-strict-transport-store-note-response
       store "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Strict-Transport-Security" value))))
      (ensure-equal nil (http-strict-transport-store-policies store))
      (ensure-false (http-strict-transport-store-known-host-p store "example.test"))
      (http-strict-transport-store-note-response
       store "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Strict-Transport-Security"
                                            "max-age=60; includeSubDomains"))))
      (ensure-true (http-strict-transport-store-known-host-p store "api.example.test"))
      (setf now 1010)
      (http-strict-transport-store-note-response
       store "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Strict-Transport-Security" value))))
      (let ((policies (http-strict-transport-store-policies store)))
        (ensure-equal 1 (length policies))
        (ensure-equal 1060 (http-strict-transport-policy-expires-at (first policies)))
        (ensure-true (http-strict-transport-policy-include-subdomains-p
                      (first policies))))
      (ensure-true (http-strict-transport-store-known-host-p store "api.example.test")))))

(deftest client-strict-transport-parser-empty-directives-invalid-nonempty
  (dolist (value '("; max-age=0;; max-age=60;"
                   "; max-age=0;; MAX-AGE=60;"
                   "; max-age=0;; =token;"
                   "; max-age=0;; bad name=token;"
                   "; max-age=0;; extension=;"
                   "; max-age=0;; extension=\"open;b;"
                   "; max-age=0;; includeSubDomains=token;"))
    (let ((store (make-http-strict-transport-store
                  :clock-function (lambda () 1000))))
      (http-strict-transport-store-note-response
       store "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Strict-Transport-Security"
                                            "max-age=60; includeSubDomains"))))
      (ensure-true (http-strict-transport-store-known-host-p store "api.example.test"))
      (http-strict-transport-store-note-response
       store "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Strict-Transport-Security" value))))
      (let ((policies (http-strict-transport-store-policies store)))
        (ensure-equal 1 (length policies))
        (ensure-equal 1060 (http-strict-transport-policy-expires-at (first policies)))
        (ensure-true (http-strict-transport-policy-include-subdomains-p
                      (first policies))))
      (ensure-true (http-strict-transport-store-known-host-p store "api.example.test")))))

(deftest client-strict-transport-parser-quoted-max-age-accepted
  (dolist (value '("60" "\"60\"" "\"\\6\\0\""))
    (let* ((now 1000)
           (store (make-http-strict-transport-store
                   :clock-function (lambda () now))))
      (http-strict-transport-store-note-response
       store "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header
                            "Strict-Transport-Security"
                            (format nil "max-age=~A; includeSubDomains" value)))))
      (ensure-true (http-strict-transport-store-known-host-p store "example.test"))
      (ensure-equal 1060 (http-strict-transport-policy-expires-at
                          (first (http-strict-transport-store-policies store))))
      (ensure-true (http-strict-transport-store-known-host-p store "api.example.test"))
      (setf now 1059)
      (ensure-true (http-strict-transport-store-known-host-p store "example.test"))
      (setf now 1060)
      (ensure-false (http-strict-transport-store-known-host-p store "example.test"))))
  (dolist (value '("0" "\"0\"" "\"\\0\""))
    (let ((store (make-http-strict-transport-store
                  :clock-function (lambda () 1000))))
      (http-strict-transport-store-note-response
       store "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Strict-Transport-Security"
                                            "max-age=60; includeSubDomains"))))
      (ensure-true (http-strict-transport-store-known-host-p store "api.example.test"))
      (http-strict-transport-store-note-response
       store "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Strict-Transport-Security"
                                            (format nil "max-age=~A" value)))))
      (ensure-equal nil (http-strict-transport-store-policies store))
      (ensure-false (http-strict-transport-store-known-host-p store "example.test"))
      (ensure-false (http-strict-transport-store-known-host-p store "api.example.test")))))

(deftest client-strict-transport-parser-quoted-max-age-nondigits
  (dolist (value '("\"sixty\"" "\"6x\"" "\"\"" "\"\\x\""))
    (let* ((now 1000)
           (store (make-http-strict-transport-store
                   :clock-function (lambda () now))))
      (http-strict-transport-store-note-response
       store "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Strict-Transport-Security"
                                            "max-age=60; includeSubDomains"))))
      (ensure-true (http-strict-transport-store-known-host-p store "api.example.test"))
      (setf now 1010)
      (http-strict-transport-store-note-response
       store "https://example.test/"
       (client-test-response
        200 :headers (list (make-http-header "Strict-Transport-Security"
                                            (format nil "max-age=~A" value)))))
      (let ((policies (http-strict-transport-store-policies store)))
        (ensure-equal 1 (length policies))
        (ensure-equal 1060 (http-strict-transport-policy-expires-at (first policies)))
        (ensure-true (http-strict-transport-policy-include-subdomains-p
                      (first policies))))
      (ensure-true (http-strict-transport-store-known-host-p store "api.example.test")))))

(deftest client-behavior-uri-empty-and-network-references
  (let* ((original "https://base.example.test/a/b?old=1")
         (base (parse-http-uri original)))
    (dolist (case '(("" "https://base.example.test/a/b?old=1")
                    ("#fragment" "https://base.example.test/a/b?old=1")
                    ("?new=2" "https://base.example.test/a/b?new=2")
                    ("?" "https://base.example.test/a/b?")
                    ("." "https://base.example.test/a/")
                    ("//other.example.test/p?q=3#fragment"
                     "https://other.example.test/p?q=3")))
      (let ((result (resolve-http-uri base (first case))))
        (ensure-equal (second case) (http-uri-string result))
        (ensure-false (eq base result))
        (ensure-equal original (http-uri-string base))))))

(deftest client-behavior-uri-typed-reference-and-rejections
  (let* ((base (parse-http-uri "https://base.example.test/a/b"))
         (reference (parse-http-uri "http://other.example.test/c?q=2"))
         (result (resolve-http-uri base reference)))
    (ensure-equal "http://other.example.test/c?q=2" (http-uri-string result))
    (ensure-false (eq result base))
    (ensure-false (eq result reference))
    (ensure-equal "https://base.example.test/a/b" (http-uri-string base))
    (ensure-equal "http://other.example.test/c?q=2" (http-uri-string reference))
    (dolist (case '((42 "x" 42)
                    ("https://base.example.test/" 42 42)
                    ("https://base.example.test/" "custom+v1:x" "custom+v1:x")
                    ("https://base.example.test/" "custom-v1:x" "custom-v1:x")
                    ("https://base.example.test/" "custom.v1:x" "custom.v1:x")))
      (let ((caught (handler-case
                        (resolve-http-uri (first case) (second case))
                      (http-protocol-error (condition) condition))))
        (ensure-true (typep caught 'http-protocol-error))
        (ensure-equal :client (http-error-operation caught))
        (ensure-equal (third case) (http-protocol-error-detail caught))))))

(deftest client-behavior-multipart-escaped-and-generated-boundaries
  (multiple-value-bind (body content-type)
      (make-http-multipart-body
       (list (make-http-multipart-part :name "a\\b" :filename "c\\d"
                                      :value "value"))
       :boundary "b")
    (ensure-equal "multipart/form-data; boundary=\"b\"" content-type)
    (ensure-octets-equal
     (ascii "--b|CRLF|Content-Disposition: form-data; name=\"a\\\\b\"; filename=\"c\\\\d\"|CRLF||CRLF|value|CRLF|--b--|CRLF|")
     body)
    (let ((parts (parse-http-multipart-body body :content-type content-type)))
      (ensure-equal 1 (length parts))
      (ensure-equal "a\\b" (http-multipart-part-name (first parts)))
      (ensure-equal "c\\d" (http-multipart-part-filename (first parts)))
      (ensure-octets-equal (ascii "value") (http-multipart-part-value (first parts)))))
  (let ((payload (make-array 3 :element-type '(unsigned-byte 8)
                               :initial-contents '(0 128 255))))
    (multiple-value-bind (body content-type)
        (make-http-multipart-body
         (list (make-http-multipart-part :name "binary" :value payload)))
      (ensure-true (plusp (length body)))
      (ensure-true (plusp (length content-type)))
      (let* ((parts (parse-http-multipart-body body :content-type content-type))
             (value (http-multipart-part-value (first parts))))
        (ensure-equal 1 (length parts))
        (ensure-equal "binary" (http-multipart-part-name (first parts)))
        (ensure-octets-equal #(0 128 255) value)
        (ensure-false (eq payload value))
        (setf (aref payload 0) 42)
        (ensure-octets-equal #(0 128 255) value)))))

(deftest client-behavior-multipart-content-type-parameter-errors
  (let* ((body (ascii "--b|CRLF|Content-Disposition: form-data; name=\"a;b\"; filename=\"a\\\";b\"|CRLF||CRLF|value|CRLF|--b--|CRLF|"))
         (parts (parse-http-multipart-body
                 body :content-type "multipart/form-data; boundary=\"b\"")))
    (ensure-equal 1 (length parts))
    (ensure-equal "a;b" (http-multipart-part-name (first parts)))
    (ensure-equal "a\";b" (http-multipart-part-filename (first parts)))
    (ensure-octets-equal (ascii "value") (http-multipart-part-value (first parts)))
    (dolist (case '((42 42)
                    ("text/plain; boundary=b" "text/plain")
                    ("multipart/form-data" "multipart/form-data")
                    ("; boundary=b" "; boundary=b")
                    ("multipart/form-data; boundary" "boundary")
                    ("multipart/form-data; boundary=" "boundary=")
                    ("multipart/form-data; boundary=\"b"
                     "multipart/form-data; boundary=\"b")
                    ("multipart/form-data; boundary=b\"x\""
                     "boundary=b\"x\"")))
      (let ((caught (handler-case
                        (parse-http-multipart-body body :content-type (first case))
                      (http-protocol-error (condition) condition))))
        (ensure-true (typep caught 'http-protocol-error))
        (ensure-equal :client (http-error-operation caught))
        (ensure-equal (second case) (http-protocol-error-detail caught))))))

(deftest client-behavior-multipart-malformed-part-metadata
  (let ((parts (parse-http-multipart-body
                (ascii "--b|CRLF|Content-Disposition: form-data; name=field|CRLF||CRLF|value|CRLF|--b--|CRLF|")
                :boundary "b")))
    (ensure-equal 1 (length parts))
    (ensure-equal "field" (http-multipart-part-name (first parts)))
    (ensure-octets-equal (ascii "value") (http-multipart-part-value (first parts))))
  (dolist (case '(("Missing-Colon" "Missing-Colon")
                  (": value" ": value")
                  ("Content-Disposition: attachment; name=field" "attachment; name=field")
                  ("Content-Disposition: form-data" "form-data")
                  ("Content-Disposition: form-data; name=\"\"" "form-data; name=\"\"")))
    (let* ((body (ascii (format nil "--b|CRLF|~A|CRLF||CRLF|value|CRLF|--b--|CRLF|"
                               (first case))))
           (caught (handler-case
                       (parse-http-multipart-body body :boundary "b")
                     (http-protocol-error (condition) condition))))
      (ensure-true (typep caught 'http-protocol-error))
      (ensure-equal :client (http-error-operation caught))
      (ensure-equal (second case) (http-protocol-error-detail caught))))
  (let* ((body (ascii "--b|CRLF|unfinished|CRLF|--b--|CRLF|"))
         (caught (handler-case
                     (parse-http-multipart-body body :boundary "b")
                   (http-protocol-error (condition) condition))))
    (ensure-true (typep caught 'http-protocol-error))
    (ensure-equal :client (http-error-operation caught))
    (ensure-equal (list :start 5 :end 15) (http-protocol-error-detail caught))))

(deftest client-behavior-multipart-input-contracts
  (flet ((rejects (thunk detail)
           (let ((caught (handler-case (funcall thunk)
                           (http-protocol-error (condition) condition))))
             (ensure-true (typep caught 'http-protocol-error))
             (ensure-equal :client (http-error-operation caught))
             (ensure-equal detail (http-protocol-error-detail caught)))))
    (let* ((body (ascii "--b|CRLF|Content-Disposition: form-data; name=field|CRLF||CRLF|value|CRLF|--b--|CRLF|"))
           (original (copy-seq body)))
      (ensure-equal 1 (length (parse-http-multipart-body body :boundary "b")))
      (dolist (invalid (list "not octets" #(256) #(-1) #(1.5)
                            (make-array '(1 1) :initial-element 0)))
        (rejects (lambda () (parse-http-multipart-body invalid :boundary "b"))
                 invalid))
      (dolist (limit '(:max-input-bytes :max-parts :max-header-bytes :max-body-bytes))
        (dolist (invalid '(-1 1/2 :invalid))
          (rejects (lambda ()
                     (apply #'parse-http-multipart-body body :boundary "b"
                            (list limit invalid)))
                   invalid)))
      (dolist (boundary '(nil "" 42))
        (rejects (lambda () (parse-http-multipart-body body :boundary boundary))
                 boundary))
      (ensure-octets-equal original body))
    (rejects (lambda () (make-http-multipart-body 42 :boundary "b")) 42)
    (rejects (lambda () (make-http-multipart-body '(42) :boundary "b")) 42)
    (dolist (invalid '(#(256) #(-1) #(1.5)))
      (rejects (lambda ()
                 (make-http-multipart-body
                  (list (make-http-multipart-part :name "field" :value invalid))
                  :boundary "b"))
               invalid))))

(deftest client-public-boundary-retry-random-result
  (let ((attempts 0) (random-calls 0) (sleeps nil))
    (let* ((client
             (make-http-client
              :cache nil
              :retry-policy (make-http-retry-policy :max-attempts 2
                                                    :base-delay 4
                                                    :jitter-ratio 0.5)
              :random-function (lambda (limit)
                                 (ensure-equal 1.0 limit)
                                 (incf random-calls)
                                 1.0)
              :sleep-function (lambda (delay) (push delay sleeps))
              :transport-function
              (lambda (request &key &allow-other-keys)
                (declare (ignore request))
                (incf attempts)
                (client-test-response 503))))
           (caught
             (handler-case
                 (http-client-send
                  client (http-client-request client "GET" "http://example.test/retry"))
               (http-protocol-error (condition) condition))))
      (ensure-true (typep caught 'http-protocol-error))
      (ensure-equal :client (http-error-operation caught))
      (ensure-equal 1.0 (http-protocol-error-detail caught))
      (ensure-equal 1 attempts)
      (ensure-equal 1 random-calls)
      (ensure-equal nil sleeps))))

(deftest client-public-boundary-auth-provider-result
  (dolist (value (list 42 (make-http-header "Authorization" "Bearer fixture")))
    (let ((provider-calls 0) (transport-calls 0))
      (let* ((client
               (make-http-client
                :cache nil
                :auth-provider (lambda (request)
                                 (ensure-equal "GET" (http-request-method request))
                                 (incf provider-calls)
                                 value)
                :transport-function
                (lambda (request &key &allow-other-keys)
                  (incf transport-calls)
                  (ensure-equal "Bearer fixture"
                                (http-header-value (http-request-headers request)
                                                   "Authorization"))
                  (client-test-response 204))))
             (result
               (handler-case
                   (http-client-send
                    client (http-client-request client "GET" "http://example.test/auth"))
                 (http-protocol-error (condition) condition))))
        (if (integerp value)
            (progn
              (ensure-true (typep result 'http-protocol-error))
              (ensure-equal :client (http-error-operation result))
              (ensure-equal value (http-protocol-error-detail result))
              (ensure-equal 0 transport-calls))
            (progn
              (ensure-equal 204 (http-response-status result))
              (ensure-equal 1 transport-calls)))
        (ensure-equal 1 provider-calls)))))

#+sbcl
(deftest client-public-boundary-https-proxy-upgrade-order
  (let* ((stream (make-instance 'binary-test-stream
                               :input (ascii "HTTP/1.1 200 Connection Established|CRLF||CRLF|HTTP/1.1 204 No Content|CRLF||CRLF|")))
         (opened 0) (closed 0) (upgrades nil)
         (client
           (make-http-client
            :cache nil :automatic-decompression-p nil
            :proxy (make-http-proxy :scheme :https :host "proxy.example" :port 8443)
            :open-stream (lambda (request &key &allow-other-keys)
                           (declare (ignore request))
                           (incf opened)
                           stream)
            :close-stream (lambda (owned)
                            (ensure-true (eq stream owned))
                            (incf closed)
                            (close owned))
            :tls-upgrade (lambda (owned uri &key timeout deadline)
                           (declare (ignore timeout deadline))
                           (ensure-true (eq stream owned))
                           (push (http-uri-string uri) upgrades)
                           (if (= (length upgrades) 1)
                               (ensure-equal 0 (length (binary-test-output stream)))
                               (ensure-octets-equal
                                (ascii "CONNECT example.test:443 HTTP/1.1|CRLF|Host: example.test:443|CRLF|Content-Length: 0|CRLF||CRLF|")
                                (binary-test-output stream)))
                           stream))))
    (let ((response (http-client-send
                     client (http-client-request client "GET" "https://example.test/path"))))
      (ensure-equal 204 (http-response-status response))
      (ensure-equal 0 (length (http-response-body response))))
    (ensure-equal '("https://proxy.example:8443/" "https://example.test/path")
                  (reverse upgrades))
    (ensure-equal 1 opened)
    (ensure-equal 1 closed)
    (ensure-octets-equal
     (ascii "CONNECT example.test:443 HTTP/1.1|CRLF|Host: example.test:443|CRLF|Content-Length: 0|CRLF||CRLF|GET /path HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 0|CRLF||CRLF|")
     (binary-test-output stream))))

#+sbcl
(deftest client-public-boundary-connect-header-authentication
  (let* ((first (make-instance 'binary-test-stream
                              :input (ascii "HTTP/1.1 407 Proxy Authentication Required|CRLF|Proxy-Authenticate: Basic realm=\"fixture\"|CRLF|Content-Length: 0|CRLF||CRLF|")))
         (second (make-instance 'binary-test-stream
                               :input (ascii "HTTP/1.1 200 Connection Established|CRLF||CRLF|HTTP/1.1 204 No Content|CRLF||CRLF|")))
         (streams (list first second)) (opened 0) (closed nil) (provider-calls 0)
         (client
           (make-http-client
            :cache nil :automatic-decompression-p nil
            :proxy (make-http-proxy :scheme :http :host "proxy.example" :port 8080)
            :proxy-challenge-auth-provider
            (lambda (request response plan challenges)
              (incf provider-calls)
              (ensure-equal "GET" (http-request-method request))
              (ensure-equal 407 (http-response-status response))
              (ensure-equal :connect (getf plan :mode))
              (ensure-equal "Basic" (http-authentication-challenge-scheme (first challenges)))
              (make-http-header "Proxy-Authorization" "Basic ZmFrZTpmYWtl"))
            :open-stream (lambda (request &key &allow-other-keys)
                           (declare (ignore request))
                           (incf opened)
                           (pop streams))
            :close-stream (lambda (owned) (push owned closed) (close owned))
            :tls-upgrade (lambda (owned uri &key timeout deadline)
                           (declare (ignore uri timeout deadline))
                           (ensure-true (eq second owned))
                           owned))))
    (let ((response (http-client-send
                     client (http-client-request client "GET" "https://example.test/path"))))
      (ensure-equal 204 (http-response-status response))
      (ensure-equal 0 (length (http-response-body response))))
    (ensure-equal 2 opened)
    (ensure-equal 1 provider-calls)
    (ensure-equal 1 (count first closed :test #'eq))
    (ensure-equal 1 (count second closed :test #'eq))
    (ensure-octets-equal
     (ascii "CONNECT example.test:443 HTTP/1.1|CRLF|Host: example.test:443|CRLF|Content-Length: 0|CRLF||CRLF|")
     (binary-test-output first))
    (ensure-octets-equal
     (ascii "CONNECT example.test:443 HTTP/1.1|CRLF|Host: example.test:443|CRLF|Proxy-Authorization: Basic ZmFrZTpmYWtl|CRLF|Content-Length: 0|CRLF||CRLF|GET /path HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 0|CRLF||CRLF|")
     (binary-test-output second))))

#+sbcl
(deftest client-public-boundary-socks-method-rejection
  (let* ((stream (make-instance 'binary-test-stream
                               :input (make-array 2 :element-type '(unsigned-byte 8)
                                                  :initial-contents '(5 255))))
         (closed 0)
         (client (make-http-client
                  :cache nil
                  :proxy (make-http-proxy :scheme :socks5 :host "proxy.example" :port 1080)
                  :open-stream (lambda (request &key &allow-other-keys)
                                 (declare (ignore request)) stream)
                  :close-stream (lambda (owned)
                                  (ensure-true (eq stream owned))
                                  (incf closed) (close owned))))
         (caught (handler-case
                     (http-client-send
                      client (http-client-request client "GET" "http://192.0.2.1/path"))
                   (http-proxy-error (condition) condition))))
    (ensure-true (typep caught 'http-proxy-error))
    (ensure-equal :proxy (http-error-operation caught))
    (ensure-equal '(:detail :authentication :cause nil) (http-client-error-detail caught))
    (ensure-equal 2 (http-kit/test-core::binary-test-input-position stream))
    (ensure-octets-equal #(5 1 0) (binary-test-output stream))
    (ensure-equal 1 closed)))

#+sbcl
(deftest client-public-boundary-socks-password-rejection
  (let* ((stream (make-instance 'binary-test-stream
                               :input (make-array 4 :element-type '(unsigned-byte 8)
                                                  :initial-contents '(5 2 1 1))))
         (closed 0)
         (client (make-http-client
                  :cache nil
                  :proxy (make-http-proxy :scheme :socks5 :host "proxy.example" :port 1080
                                          :username "u" :password "p")
                  :open-stream (lambda (request &key &allow-other-keys)
                                 (declare (ignore request)) stream)
                  :close-stream (lambda (owned)
                                  (ensure-true (eq stream owned))
                                  (incf closed) (close owned))))
         (caught (handler-case
                     (http-client-send
                      client (http-client-request client "GET" "http://192.0.2.1/path"))
                   (http-proxy-error (condition) condition))))
    (ensure-true (typep caught 'http-proxy-error))
    (ensure-equal :proxy (http-error-operation caught))
    (ensure-equal '(:detail :authentication :cause nil) (http-client-error-detail caught))
    (ensure-equal 4 (http-kit/test-core::binary-test-input-position stream))
    (ensure-octets-equal #(5 1 2 1 1 117 1 112) (binary-test-output stream))
    (ensure-equal 1 closed)))

#+sbcl
(deftest client-public-boundary-socks-domain-reply
  (let* ((input (concatenate-octets
                 (make-array 10 :element-type '(unsigned-byte 8)
                                :initial-contents '(5 0 5 0 0 3 1 97 0 0))
                 (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|")))
         (stream (make-instance 'binary-test-stream :input input))
         (closed 0)
         (client (make-http-client
                  :cache nil :automatic-decompression-p nil
                  :proxy (make-http-proxy :scheme :socks5 :host "proxy.example" :port 1080)
                  :open-stream (lambda (request &key &allow-other-keys)
                                 (declare (ignore request)) stream)
                  :close-stream (lambda (owned)
                                  (ensure-true (eq stream owned))
                                  (incf closed) (close owned)))))
    (let ((response (http-client-send
                     client (http-client-request client "GET" "http://192.0.2.1/path"))))
      (ensure-equal 204 (http-response-status response))
      (ensure-equal 0 (length (http-response-body response))))
    (ensure-equal (length input) (http-kit/test-core::binary-test-input-position stream))
    (ensure-octets-equal #(5 1 0 5 1 0 1 192 0 2 1 0 80)
                         (subseq (binary-test-output stream) 0 13))
    (ensure-octets-equal
     (ascii "GET /path HTTP/1.1|CRLF|Host: 192.0.2.1|CRLF|Content-Length: 0|CRLF||CRLF|")
     (subseq (binary-test-output stream) 13))
    (ensure-equal 1 closed)))

#+sbcl
(deftest client-public-boundary-pool-capacity-eviction
  (let* ((reply (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|"))
         (first (make-instance 'binary-test-stream :input reply))
         (second (make-instance 'binary-test-stream :input (concatenate-octets reply reply)))
         (streams (list first second)) (opened 0) (closed nil) (now 1)
         (pool (make-http-connection-pool
                :max-idle 1
                :clock-function (lambda () now)
                :open-stream (lambda (request &key &allow-other-keys)
                               (declare (ignore request))
                               (incf opened) (pop streams))
                :close-stream (lambda (owned) (push owned closed) (close owned))))
         (client (make-http-client :cache nil :automatic-decompression-p nil :connection-pool pool)))
    (unwind-protect
         (progn
           (let ((response (http-client-send
                            client (http-client-request client "GET" "http://a.example/one"))))
             (ensure-equal 204 (http-response-status response)))
           (ensure-equal 1 opened)
           (ensure-equal nil closed)
           (ensure-equal 1 (getf (http-connection-pool-stats pool) :idle-count))
           (setf now 2)
           (let ((response (http-client-send
                            client (http-client-request client "GET" "http://b.example/two"))))
             (ensure-equal 204 (http-response-status response)))
           (ensure-equal 2 opened)
           (ensure-equal 1 (count first closed :test #'eq))
           (ensure-equal 0 (count second closed :test #'eq))
           (ensure-equal 1 (getf (http-connection-pool-stats pool) :idle-count))
           (setf now 3)
           (let ((response (http-client-send
                            client (http-client-request client "GET" "http://b.example/three"))))
             (ensure-equal 204 (http-response-status response)))
           (ensure-equal 2 opened)
           (ensure-equal 1 (count first closed :test #'eq))
           (ensure-equal 0 (count second closed :test #'eq))
           (ensure-equal 1 (http-connection-pool-clear pool))
           (ensure-equal 1 (count second closed :test #'eq))
           (ensure-equal 0 (getf (http-connection-pool-stats pool) :idle-count))
           (ensure-octets-equal
            (ascii "GET /one HTTP/1.1|CRLF|Host: a.example|CRLF|Content-Length: 0|CRLF||CRLF|")
            (binary-test-output first))
           (ensure-octets-equal
            (ascii "GET /two HTTP/1.1|CRLF|Host: b.example|CRLF|Content-Length: 0|CRLF||CRLF|GET /three HTTP/1.1|CRLF|Host: b.example|CRLF|Content-Length: 0|CRLF||CRLF|")
            (binary-test-output second)))
      (http-connection-pool-clear pool))))


(deftest client-public-boundary-wave2-digest-guards
  (let ((challenge (first (http-parse-authentication-challenges
                           "Digest realm=\"fixture\", nonce=\"n\", charset=UTF-8"))))
    (dolist (nc '(0 -1 1/2 #x100000000))
      (let ((caught (handler-case
                        (http-digest-authorization challenge "GET" "/" "u" "p" "c"
                                                   :nonce-count nc)
                      (http-protocol-error (condition) condition))))
        (ensure-true (typep caught 'http-protocol-error))
        (ensure-equal :client (http-error-operation caught))
        (ensure-equal nc (http-protocol-error-detail caught))))
    (ensure-true (search "response=" (http-digest-authorization
                                    challenge "GET" "/" "u" "p" "c" :nonce-count 1))))
  (dolist (wire '("Digest realm=\"fixture\"" "Digest nonce=\"n\""))
    (let* ((challenge (first (http-parse-authentication-challenges wire)))
           (caught (handler-case
                       (http-digest-authorization challenge "GET" "/" "u" "p" "c")
                     (http-protocol-error (condition) condition))))
      (ensure-true (typep caught 'http-protocol-error))
      (ensure-equal :client (http-error-operation caught))
      (ensure-true (eq challenge (http-protocol-error-detail caught)))))
  (let* ((challenge (first (http-parse-authentication-challenges
                            "Digest realm=\"fixture\", nonce=\"n\", charset=ISO-8859-1")))
         (caught (handler-case
                     (http-digest-authorization challenge "GET" "/" "u" "p" "c")
                   (http-protocol-error (condition) condition))))
    (ensure-true (typep caught 'http-protocol-error))
    (ensure-equal :client (http-error-operation caught))
    (ensure-equal "ISO-8859-1" (http-protocol-error-detail caught))))

(deftest client-public-boundary-wave2-digest-no-qop
  ;; Independent MD5 vectors use HA1=4945ecf42b1bb868634058a845bedde8
  ;; and HA2=39aff3a2bab6126f332b942af96d3366, not the production hash helper.
  (dolist (row '(("Digest realm=\"testrealm@host.com\", nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\""
                 "Digest username=\"Mufasa\", realm=\"testrealm@host.com\", uri=\"/dir/index.html\", algorithm=MD5, nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", response=\"1949323746fe6a43ef61f9606e7febea\"")
                ("Digest realm=\"testrealm@host.com\", nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", qop=\"auth\""
                 "Digest username=\"Mufasa\", realm=\"testrealm@host.com\", uri=\"/dir/index.html\", algorithm=MD5, nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", nc=00000001, cnonce=\"0a4f113b\", qop=auth, response=\"e31b30d5e407246518d94833d7f0deea\"")))
    (destructuring-bind (challenge-wire expected) row
      (ensure-equal expected
                    (http-digest-authorization
                     (first (http-parse-authentication-challenges challenge-wire))
                     "GET" "/dir/index.html" "Mufasa" "CircleOfLife" "0a4f113b")))))

#+sbcl
(deftest client-public-boundary-wave2-pool-clock
  (dolist (phase '(:before-open :after-response :normal))
    (let* ((reply (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|"))
           (stream (make-instance 'binary-test-stream
                                  :input (concatenate-octets reply reply)))
           (opened 0) (closed 0) (clock-calls 0) (bad-p t)
           (pool (make-http-connection-pool
                  :clock-function
                  (lambda ()
                    (incf clock-calls)
                    (if (and bad-p
                             (or (eq phase :before-open)
                                 (and (eq phase :after-response)
                                      (plusp (http-kit/test-core::binary-test-input-position
                                              stream)))))
                        :bad-clock 10))
                  :open-stream (lambda (request &key &allow-other-keys)
                                 (declare (ignore request)) (incf opened) stream)
                  :close-stream (lambda (owned)
                                  (ensure-true (eq stream owned))
                                  (incf closed) (close owned))))
           (client (make-http-client :cache nil :automatic-decompression-p nil
                                     :connection-pool pool)))
      (unwind-protect
           (let ((result (handler-case
                             (http-client-send
                              client (http-client-request client "GET" "http://example.test/"))
                           (http-protocol-error (condition) condition))))
             (if (eq phase :normal)
                 (progn
                   (ensure-equal 204 (http-response-status result))
                   (ensure-equal 204 (http-response-status
                                      (http-client-send
                                       client (http-client-request client "GET"
                                                                   "http://example.test/"))))
                   (ensure-equal 1 opened)
                   (ensure-equal 0 closed)
                   (ensure-equal 1 (getf (http-connection-pool-stats pool) :idle-count)))
                 (progn
                   (ensure-true (typep result 'http-protocol-error))
                   (ensure-equal :connection-pool (http-error-operation result))
                   (ensure-equal :bad-clock (http-protocol-error-detail result))
                   (ensure-equal (if (eq phase :before-open) 0 1) opened)
                   (ensure-equal (if (eq phase :before-open) 0 1) closed)
                   (ensure-equal (if (eq phase :before-open) 0 (length reply))
                                 (http-kit/test-core::binary-test-input-position stream))
                   (ensure-octets-equal
                    (if (eq phase :before-open)
                        #()
                        (ascii "GET / HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 0|CRLF||CRLF|"))
                    (binary-test-output stream))))
             (ensure-true (plusp clock-calls))
             (setf bad-p nil)
             (ensure-equal (if (eq phase :normal) 1 0)
                           (http-connection-pool-clear pool))
             (ensure-equal (if (eq phase :before-open) 0 1) closed)
             (ensure-equal 0 (getf (http-connection-pool-stats pool) :idle-count)))
        (setf bad-p nil)
        (http-connection-pool-clear pool)
        (close stream)))))

#+sbcl
(deftest client-public-boundary-wave2-pool-options
  (let* ((reply (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|"))
         (stream (make-instance 'binary-test-stream :input reply))
         (opened 0) (closed 0)
         (opener (lambda (request &key &allow-other-keys)
                   (declare (ignore request)) (incf opened) stream))
         (closer (lambda (owned) (ensure-true (eq stream owned))
                   (incf closed) (close owned))))
    (unwind-protect
         (progn
           (dolist (row '((:max-idle -1) (:max-idle 1/2)
                          (:idle-timeout -1) (:idle-timeout "later")))
             (let ((caught (handler-case
                               (apply #'make-http-connection-pool
                                      :open-stream opener :close-stream closer row)
                             (http-protocol-error (condition) condition))))
               (ensure-true (typep caught 'http-protocol-error))
               (ensure-equal :connection-pool (http-error-operation caught))
               (ensure-equal (second row) (http-protocol-error-detail caught))
               (ensure-equal 0 opened)
               (ensure-equal 0 closed)
               (ensure-equal 0 (http-kit/test-core::binary-test-input-position stream))
               (ensure-octets-equal #() (binary-test-output stream))))
           (let* ((pool (make-http-connection-pool
                         :open-stream opener :close-stream closer
                         :max-idle 1 :idle-timeout 0 :clock-function (lambda () 10)))
                  (client (make-http-client :cache nil :automatic-decompression-p nil
                                            :connection-pool pool)))
             (unwind-protect
                  (progn
                    (ensure-equal 204 (http-response-status
                                       (http-client-send
                                        client (http-client-request client "GET"
                                                                    "http://example.test/"))))
                    (ensure-equal 1 opened)
                    (ensure-equal 0 closed)
                    (ensure-equal 1 (http-connection-pool-clear pool))
                    (ensure-equal 1 closed)
                    (ensure-equal (length reply)
                                  (http-kit/test-core::binary-test-input-position stream)))
               (http-connection-pool-clear pool))))
      (close stream))))

#+sbcl
(deftest client-public-boundary-wave2-pool-zero-capacity
  (dolist (capacity '(0 1))
    (let* ((reply (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|"))
           (first (make-instance 'binary-test-stream
                                 :input (if (zerop capacity) reply
                                            (concatenate-octets reply reply))))
           (second (make-instance 'binary-test-stream :input reply))
           (streams (list first second)) (opened 0) (closed nil)
           (pool (make-http-connection-pool
                  :max-idle capacity :clock-function (lambda () 10)
                  :open-stream (lambda (request &key &allow-other-keys)
                                 (declare (ignore request)) (incf opened) (pop streams))
                  :close-stream (lambda (owned) (push owned closed) (close owned))))
           (client (make-http-client :cache nil :automatic-decompression-p nil
                                     :connection-pool pool)))
      (unwind-protect
           (progn
             (dotimes (index 2)
               (ensure-equal 204 (http-response-status
                                  (http-client-send
                                   client (http-client-request client "GET"
                                                               "http://example.test/"))))
               (ensure-equal capacity (getf (http-connection-pool-stats pool) :idle-count))
               (ensure-equal (if (zerop capacity) (1+ index) 1) opened))
             (ensure-equal (if (zerop capacity) 1 0) (count first closed :test #'eq))
             (ensure-equal (if (zerop capacity) 1 0) (count second closed :test #'eq))
             (ensure-equal capacity (http-connection-pool-clear pool))
             (ensure-equal 1 (count first closed :test #'eq))
             (ensure-equal (if (zerop capacity) 1 0) (count second closed :test #'eq))
             (ensure-equal 0 (getf (http-connection-pool-stats pool) :idle-count))
             (ensure-octets-equal
              (ascii (if (zerop capacity)
                         "GET / HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 0|CRLF||CRLF|"
                         "GET / HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 0|CRLF||CRLF|GET / HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 0|CRLF||CRLF|"))
              (binary-test-output first))
             (ensure-octets-equal
              (if (zerop capacity)
                  (ascii "GET / HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 0|CRLF||CRLF|")
                  #())
              (binary-test-output second)))
        (http-connection-pool-clear pool)
        (close first) (close second)))))

#+sbcl
(deftest client-public-boundary-wave2-socks-wire-errors
  (dolist (row '(((4 0) 1 :version nil)
                ((5 2 2 0) 3 :authentication t)
                ((5 0 4 0 0 1 0 0 0 0 0 0) 3 :reply nil)
                ((5 0 5 1 0 1 0 0 0 0 0 0) 4 1 nil)
                ((5 0 5 0 0 9 0 0) 6 9 nil)
                ((5 0 5 0 0 4 0) 7 :eof nil)
                ((5 0 5 0 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                 :success nil nil)))
    (destructuring-bind (bytes consumed detail credentials-p) row
      (let* ((success-p (eq consumed :success))
             (input (concatenate-octets
                     (make-array (length bytes) :element-type '(unsigned-byte 8)
                                               :initial-contents bytes)
                     (if success-p (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|") #())))
             (stream (make-instance 'binary-test-stream :input input))
             (opened 0) (closed 0)
             (client (make-http-client
                      :cache nil :automatic-decompression-p nil
                      :proxy (make-http-proxy :scheme :socks5 :host "proxy.example" :port 1080
                                              :username (and credentials-p "u")
                                              :password (and credentials-p "p"))
                      :open-stream (lambda (request &key &allow-other-keys)
                                     (declare (ignore request)) (incf opened) stream)
                      :close-stream (lambda (owned)
                                      (ensure-true (eq stream owned))
                                      (incf closed) (close owned))))
             (result (handler-case
                         (http-client-send
                          client (http-client-request client "GET" "http://192.0.2.1/path"))
                       (http-proxy-error (condition) condition))))
        (if success-p
            (progn
              (ensure-equal 204 (http-response-status result))
              (ensure-equal 0 (length (http-response-body result))))
            (progn
              (ensure-true (typep result 'http-proxy-error))
              (ensure-equal :proxy (http-error-operation result))
              (ensure-equal (list :detail detail :cause nil) (http-client-error-detail result))))
        (ensure-equal (if success-p (length input) consumed)
                      (http-kit/test-core::binary-test-input-position stream))
        (ensure-octets-equal
         (cond (success-p
                (concatenate-octets
                 #(5 1 0 5 1 0 1 192 0 2 1 0 80)
                 (ascii "GET /path HTTP/1.1|CRLF|Host: 192.0.2.1|CRLF|Content-Length: 0|CRLF||CRLF|")))
               (credentials-p #(5 1 2 1 1 117 1 112))
               ((eq detail :version) #(5 1 0))
               (t #(5 1 0 5 1 0 1 192 0 2 1 0 80)))
         (binary-test-output stream))
        (ensure-equal 1 opened)
        (ensure-equal 1 closed)))))

#+sbcl
(deftest client-public-boundary-wave2-proxy-tls-callback
  (dolist (mode '(:missing :wrong-result :normal))
    (let* ((reply (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|"))
           (stream (make-instance 'binary-test-stream :input reply))
           (opened 0) (closed 0) (upgraded 0)
           (client (make-http-client :cache nil))
           (request (http-client-request client "GET" "https://example.test/path"))
           (closer (lambda (owned)
                     (ensure-true (eq stream owned)) (incf closed) (close owned)))
           (opener (make-http-proxy-stream-opener
                    (lambda (actual &key timeout deadline &allow-other-keys)
                      (ensure-true (eq actual request))
                      (ensure-equal nil timeout)
                      (ensure-equal nil deadline)
                      (incf opened) stream)
                    closer
                    :tls-upgrade
                    (unless (eq mode :missing)
                      (lambda (owned uri &key timeout deadline)
                        (ensure-true (eq owned stream))
                        (ensure-equal "https://example.test/path" (http-uri-string uri))
                        (ensure-equal nil timeout)
                        (ensure-equal nil deadline)
                        (incf upgraded)
                        (if (eq mode :wrong-result) 17 owned)))))
           (result (handler-case (funcall opener request)
                     (http-error (condition) condition))))
      (if (eq mode :normal)
          (unwind-protect
               (progn
                 (ensure-true (eq result stream))
                 (ensure-equal 0 closed)
                 (ensure-equal 204 (http-response-status
                                    (send-http-request-over-open-stream request result)))
                 (ensure-equal (length reply)
                               (http-kit/test-core::binary-test-input-position stream))
                 (ensure-octets-equal
                  (ascii "GET /path HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 0|CRLF||CRLF|")
                  (binary-test-output stream)))
            (funcall closer result))
          (progn
            (ensure-equal :proxy (http-error-operation result))
            (if (eq mode :missing)
                (progn
                  (ensure-true (typep result 'http-unsupported-feature))
                  (ensure-equal :tls-upgrade (http-unsupported-feature-name result))
                  (ensure-equal "https://example.test/path"
                                (http-uri-string (http-protocol-error-detail result))))
                (progn
                  (ensure-true (typep result 'http-proxy-error))
                  (ensure-equal '(:detail 17 :cause nil) (http-client-error-detail result))))
            (ensure-equal 0 (http-kit/test-core::binary-test-input-position stream))
            (ensure-octets-equal #() (binary-test-output stream))))
      (ensure-equal 1 opened)
      (ensure-equal (if (eq mode :missing) 0 1) upgraded)
      (ensure-equal 1 closed))))

(deftest client-public-boundary-wave2-alt-svc-quoted-ipv6
  (let* ((now 0)
         (store (make-http-alternative-service-store :clock-function (lambda () now))))
    (dolist (origin '("https://[2001:db8::1]/" "https://example.test/"))
      (http-alternative-service-store-note-response
       store origin
       (client-test-response
        200 :headers (list (make-http-header "Alt-Svc" "h3=\":443\"; ma=\"6\\0\""))))
      (let ((services (http-alternative-service-store-services store origin)))
        (ensure-equal 1 (length services))
        (let ((service (first services)))
          (ensure-equal "h3" (http-alternative-service-protocol-id service))
          (ensure-equal (if (search "[" origin) "2001:db8::1" "example.test")
                        (http-alternative-service-host service))
          (ensure-equal 443 (http-alternative-service-port service))
          (ensure-equal 60 (http-alternative-service-expires-at service)))))
    (setf now 59)
    (ensure-equal 1 (length (http-alternative-service-store-services
                            store "https://[2001:db8::1]/")))
    (setf now 60)
    (ensure-equal nil (http-alternative-service-store-services store "https://[2001:db8::1]/"))
    (ensure-equal nil (http-alternative-service-store-services store "https://example.test/")))
  (dolist (wire '("h3=\":bogus\"" "h3=\":443" "h3=\":443\\" "h3=\":443\"; ma=foo"))
    (let ((store (make-http-alternative-service-store :clock-function (lambda () 0))))
      (http-alternative-service-store-note-response
       store "https://[2001:db8::1]/"
       (client-test-response 200 :headers (list (make-http-header "Alt-Svc" wire))))
      (ensure-equal nil (http-alternative-service-store-services store "https://[2001:db8::1]/")))))

(deftest client-coding-boundary-callbacks-and-passthrough
  (let* ((body (make-array 3 :element-type '(unsigned-byte 8)
                            :initial-contents '(0 128 255)))
         (encoded (make-array 3 :element-type '(unsigned-byte 8)
                               :initial-contents '(255 128 0)))
         (encodes 0)
         (decodes 0)
         (coding (make-http-content-coding
                  :name "X-Test"
                  :encoder (lambda (input)
                             (incf encodes)
                             (ensure-true (eq body input))
                             encoded)
                  :decoder (lambda (input)
                             (incf decodes)
                             (ensure-true (eq encoded input))
                             body)))
         (identity (make-http-content-coding :name "identity")))
    (ensure-equal "x-test" (http-content-coding-name coding))
    (ensure-true (eq encoded (http-content-coding-encode coding body)))
    (ensure-true (eq body (http-content-coding-decode coding encoded)))
    (ensure-equal 1 encodes)
    (ensure-equal 1 decodes)
    (ensure-true (eq body (http-content-coding-encode identity body)))
    (ensure-true (eq body (http-content-coding-decode identity body)))))

(deftest client-coding-boundary-invalid-configuration
  (flet ((rejected (thunk input)
           (let ((result (handler-case (funcall thunk)
                           (http-protocol-error (condition) condition))))
             (ensure-true (typep result 'http-protocol-error))
             (ensure-equal :client (http-error-operation result))
             (ensure-true (eq input (http-protocol-error-detail result))))))
    (dolist (name '(nil 17 "" "two words"))
      (rejected (lambda () (make-http-content-coding :name name)) name))
    (dolist (callback '(17 identity))
      (rejected (lambda () (make-http-content-coding :name "br" :encoder callback))
                callback)
      (rejected (lambda () (make-http-content-coding :name "br" :decoder callback))
                callback))
    (dolist (value '(nil 17))
      (rejected (lambda () (parse-http-accept-encoding value)) value)))
  (let ((coding (make-http-content-coding :name "BR")))
    (ensure-equal "br" (http-content-coding-name coding))
    (ensure-equal nil (http-content-coding-encoder coding))
    (ensure-equal nil (http-content-coding-decoder coding)))
  (ensure-equal '(("br" . 0.5) ("identity" . 0))
                (parse-http-accept-encoding "BR; q=0.5, identity;q=0")))

(deftest client-coding-boundary-response-validation-and-identity
  (let* ((body (make-array 3 :element-type '(unsigned-byte 8)
                            :initial-contents '(0 128 255)))
         (trailers (list (make-http-header "X-End" "complete")))
         (response (make-http-response
                    :status 200 :reason "OK" :protocol-version "HTTP/1.1"
                    :headers (list (make-http-header "Content-Encoding" "IDENTITY")
                                   (make-http-header "content-length" "3")
                                   (make-http-header "X-Retained" "yes"))
                    :trailers trailers :body body)))
    (flet ((rejected (thunk input)
             (let ((result (handler-case (funcall thunk)
                             (http-protocol-error (condition) condition))))
               (ensure-true (typep result 'http-protocol-error))
               (ensure-equal :client (http-error-operation result))
               (ensure-true (eq input (http-protocol-error-detail result))))))
      (dolist (input '(nil 17))
        (rejected (lambda () (decode-http-response-content input)) input))
      (dolist (limit '(-1 1/2 "3"))
        (rejected (lambda () (decode-http-response-content response :max-body-bytes limit))
                  limit)))
    (dolist (limit '(nil 3))
      (let ((decoded (decode-http-response-content response :max-body-bytes limit)))
        (ensure-false (eq response decoded))
        (ensure-octets-equal body (http-response-body decoded))
        (ensure-equal "HTTP/1.1" (http-response-protocol-version decoded))
        (ensure-equal 200 (http-response-status decoded))
        (ensure-equal "OK" (http-response-reason decoded))
        (ensure-equal 1 (length (http-response-trailers decoded)))
        (ensure-equal "complete" (http-header-value (http-response-trailers decoded) "X-End"))
        (ensure-equal "yes" (http-header-value (http-response-headers decoded) "X-Retained"))
        (ensure-equal 1 (length (http-response-headers decoded)))
        (ensure-false (http-header-present-p (http-response-headers decoded) "Content-Encoding"))
        (ensure-false (http-header-present-p (http-response-headers decoded) "Content-Length"))))))

(deftest client-coding-boundary-decoder-registration-replacement
  (dolist (input (list nil 17 (cons 17 #'identity) (cons "br" 17)))
    (let ((result (handler-case (make-http-content-decoders input)
                    (http-protocol-error (condition) condition))))
      (ensure-true (typep result 'http-protocol-error))
      (ensure-equal :client (http-error-operation result))
      (ensure-true (eq input (http-protocol-error-detail result)))))
  (let* ((body (make-array 2 :element-type '(unsigned-byte 8)
                            :initial-contents '(128 255)))
         (decoded (make-array 2 :element-type '(unsigned-byte 8)
                               :initial-contents '(255 128)))
         (old-calls 0)
         (new-calls 0)
         (registry (make-http-content-decoders
                    (cons "BR" (lambda (input limit)
                                 (declare (ignore input limit))
                                 (incf old-calls)
                                 body))
                    (cons "br" (lambda (input limit)
                                 (incf new-calls)
                                 (ensure-octets-equal body input)
                                 (ensure-equal 2 limit)
                                 decoded))))
         (response (decode-http-response-content
                    (client-test-response
                     200 :body body :headers (list (make-http-header "Content-Encoding" "Br")))
                    :max-body-bytes 2 :content-decoders registry)))
    (ensure-equal '("gzip" "deflate" "zstd" "br") (mapcar #'car registry))
    (ensure-octets-equal decoded (http-response-body response))
    (ensure-equal 0 old-calls)
    (ensure-equal 1 new-calls)
    (ensure-false (http-header-present-p (http-response-headers response) "Content-Encoding"))))

(deftest client-auth-native-challenge-quoted-boundaries
  (let* ((text (format nil "Digest, realm=\"a~C~Cb\", nonce=\"n\", qop=\"auth\", Basic realm=\"fallback\""
                       #\\ (code-char 128)))
         (challenges (http-parse-authentication-challenges text)))
    (ensure-equal '("Digest" "Basic")
                  (mapcar #'http-authentication-challenge-scheme challenges))
    (ensure-equal (format nil "a~Cb" (code-char 128))
                  (http-authentication-challenge-parameter (first challenges) "REALM"))
    (ensure-equal "n" (http-authentication-challenge-parameter (first challenges) "nonce"))
    (ensure-equal "fallback"
                  (http-authentication-challenge-parameter (second challenges) "realm")))
  (dolist (invalid (list (format nil "Digest realm=\"a~C~Cb\", nonce=\"n\""
                                #\\ (code-char 1))
                        "Digest realm=\"unfinished"
                        "Digest realm=\"r\", realm=\"duplicate\", nonce=\"n\""))
    (let ((challenges (http-parse-authentication-challenges
                       (list invalid "Basic realm=\"fallback\""))))
      (ensure-equal 1 (length challenges))
      (ensure-equal "Basic" (http-authentication-challenge-scheme (first challenges)))
      (ensure-equal "fallback"
                    (http-authentication-challenge-parameter (first challenges) "realm")))))

(deftest client-auth-native-parameter-lookup-contract
  (let ((challenge (first (http-parse-authentication-challenges "Digest realm=\"r\", nonce=\"n\""))))
    (ensure-equal "r" (http-authentication-challenge-parameter challenge "ReAlM"))
    (ensure-false (http-authentication-challenge-parameter challenge "missing"))
    (dolist (arguments (list (list nil "realm") (list challenge 17)))
      (let ((condition (handler-case
                           (apply #'http-authentication-challenge-parameter arguments)
                         (http-protocol-error (condition) condition))))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :client (http-error-operation condition))
        (ensure-equal arguments (http-protocol-error-detail condition))))))

(deftest client-auth-native-digest-input-contract
  (let ((challenge (first (http-parse-authentication-challenges
                          "Digest realm=\"r\", nonce=\"n\", qop=\"auth\""))))
    (dolist (invalid (list nil (first (http-parse-authentication-challenges "Basic realm=\"r\""))))
      (let ((condition (handler-case
                           (http-digest-authorization invalid "GET" "/" "u" "p" "c")
                         (http-protocol-error (condition) condition))))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :client (http-error-operation condition))
        (ensure-true (eq invalid (http-protocol-error-detail condition)))))
    (loop for index from 0 below 5
          for label in '(:method :request-target :username :password :cnonce)
          do (dolist (invalid (list 17 (string (code-char 1)) (string (code-char 127))))
               (let ((arguments (list "GET" "/" "u" "p" "c")))
                 (setf (nth index arguments) invalid)
                 (let ((condition (handler-case
                                      (apply #'http-digest-authorization challenge arguments)
                                    (http-protocol-error (condition) condition))))
                   (ensure-true (typep condition 'http-protocol-error))
                   (ensure-equal :client (http-error-operation condition))
                   (ensure-equal label (http-protocol-error-detail condition))))))
    (ensure-equal
     "Digest username=\"u\", realm=\"r\", uri=\"/\", algorithm=MD5, nonce=\"n\", nc=00000001, cnonce=\"c\", qop=auth, response=\"490ab2d682267b3630d3509d981b982d\""
     (http-digest-authorization challenge "GET" "/" "u" "p" "c"))))

(deftest client-auth-native-digest-entity-body-contract
  (let ((challenge (first (http-parse-authentication-challenges
                          "Digest realm=\"r\", nonce=\"n\", qop=\"auth-int\""))))
    (dolist (body (list "abc" '(0 128 255) #(0 256) #(-1)
                       (make-array '(1 1) :initial-element 0)))
      (let ((condition (handler-case
                           (http-digest-authorization challenge "POST" "/" "u" "p" "c"
                                                      :entity-body body)
                         (http-protocol-error (condition) condition))))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :client (http-error-operation condition))
        (ensure-equal :entity-body (http-protocol-error-detail condition))))
    (ensure-equal
     "Digest username=\"u\", realm=\"r\", uri=\"/\", algorithm=MD5, nonce=\"n\", nc=00000001, cnonce=\"c\", qop=auth-int, response=\"d1df60970bfa697e4ccd0812574c2955\""
     (http-digest-authorization challenge "POST" "/" "u" "p" "c"
                                :entity-body #(0 128 255)))))

(deftest client-auth-native-digest-nonce-count-and-algorithm
  (dolist (row '(("mD5" 10 "0000000A" "9ae8f9b889f99ec3cf0b4edf1a4c3b37")
                 ("mD5" 4294967295 "FFFFFFFF" "90ebb2b1a5f8dfefe3027b7b26eeadc6")
                 ("sHa-256" 10 "0000000A" "f5616c32498d6414c775308bcc4145b4a5726bdf3894f2c8b050deb0e4802176")
                 ("sHa-256" 4294967295 "FFFFFFFF" "d1898b9c16fb6af3cb7a9e28653eac873d64243d4e30a6a514de540505114162")))
    (destructuring-bind (algorithm count nc response) row
      (let ((challenge (first (http-parse-authentication-challenges
                              (format nil "Digest realm=\"r\", nonce=\"n\", qop=\"auth\", algorithm=~A"
                                      algorithm)))))
        (ensure-equal
         (format nil "Digest username=\"u\", realm=\"r\", uri=\"/\", algorithm=~A, nonce=\"n\", nc=~A, cnonce=\"c\", qop=auth, response=\"~A\""
                 algorithm nc response)
         (http-digest-authorization challenge "GET" "/" "u" "p" "c" :nonce-count count))))))

(deftest client-auth-native-digest-header-escaping-and-extended-no-qop
  (let* ((challenge (first (http-parse-authentication-challenges
                           "Digest realm=\"r\\\"\\\\\", nonce=\"n\\\"\\\\\", qop=\"auth\", opaque=\"o\\\"\\\\\"")))
         (header (http-digest-authorization challenge "GET" "/\"\\" "u\"\\" "p" "c\"\\")))
    (ensure-equal
     "Digest username=\"u\\\"\\\\\", realm=\"r\\\"\\\\\", uri=\"/\\\"\\\\\", algorithm=MD5, nonce=\"n\\\"\\\\\", nc=00000001, cnonce=\"c\\\"\\\\\", qop=auth, response=\"4e06dee5a9f21c045bdc29fe489341e7\", opaque=\"o\\\"\\\\\""
     header)
    (ensure-false (find #\Newline header))
    (ensure-false (find #\Return header)))
  (let ((challenge (first (http-parse-authentication-challenges
                          "Digest realm=\"r\", nonce=\"n\", charset=UTF-8"))))
    (ensure-equal
     "Digest username*=UTF-8''%C3%A9, realm=\"r\", uri=\"/\", algorithm=MD5, nonce=\"n\", response=\"951a5c017b150a6fffc591bf61e99f6a\""
     (http-digest-authorization challenge "GET" "/" (string (code-char 233)) "p" "c"))))

(deftest client-auth-native-content-digest-structured-parameters
  (let* ((body (make-array 0 :element-type '(unsigned-byte 8)))
         (field (http-content-digest body)))
    (dolist (parameters '(";*ext=-999999999999.999" ";decimal=0.001"
                          ";text=\"a;\\\"b\\\\c\"" ";flag;yes=?1;no=?0"
                          ";bytes=::" ";bytes=:AA==:" ";token=*value/path"))
      (ensure-true (http-content-digest-valid-p
                    body (concatenate 'string field parameters))))
    (dolist (parameters '(";decimal=1000000000000.1" ";decimal=1.0000"
                          ";decimal=.1" ";decimal=1." ";text=\"a\\q\""
                          ";bool=?2" ";bytes=:AB==:" ";=1" ";x=1;x=2"))
      (let ((condition (handler-case
                           (http-content-digest-valid-p
                            body (concatenate 'string field parameters))
                         (http-protocol-error (condition) condition))))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :client (http-error-operation condition))
        (ensure-equal :field (http-protocol-error-detail condition))))
    (dolist (input (list nil 17 '("sha-256=:AA==:" 17)))
      (let ((condition (handler-case (http-content-digest-valid-p body input)
                         (http-protocol-error (condition) condition))))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :client (http-error-operation condition))
        (ensure-equal :field (http-protocol-error-detail condition))))
    (ensure-true (http-content-digest-valid-p body (list field)))
    (ensure-false (http-content-digest-valid-p
                   (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0) field))))

(deftest client-auth-native-real-wire-fallback-and-provider-failure
  (dolist (fail-provider-p '(nil t))
    (let* ((first (make-instance 'binary-test-stream
                                :input (ascii "HTTP/1.1 401 Unauthorized|CRLF|Content-Length: 0|CRLF|Connection: close|CRLF|WWW-Authenticate: Digest realm=\"bad\", realm=\"duplicate\", nonce=\"n\"|CRLF|WWW-Authenticate: Basic realm=\"fallback\"|CRLF||CRLF|")))
           (second (make-instance 'binary-test-stream
                                 :input (ascii "HTTP/1.1 204 No Content|CRLF|Connection: close|CRLF||CRLF|")))
           (streams (list first second))
           (opened 0)
           (closed nil)
           (provider-calls 0)
           (original (handler-case (http-authentication-challenge-parameter nil "realm")
                       (http-protocol-error (condition) condition)))
           (client (make-http-client
                    :cache nil :automatic-decompression-p nil
                    :open-stream (lambda (request &key &allow-other-keys)
                                   (declare (ignore request))
                                   (incf opened)
                                   (pop streams))
                    :close-stream (lambda (owned) (push owned closed) (close owned))
                    :challenge-auth-provider
                    (lambda (request response challenges)
                      (incf provider-calls)
                      (ensure-equal 401 (http-response-status response))
                      (ensure-false (http-header-present-p (http-request-headers request) "Authorization"))
                      (ensure-equal 1 (length challenges))
                      (ensure-equal "Basic" (http-authentication-challenge-scheme (first challenges)))
                      (ensure-equal "fallback"
                                    (http-authentication-challenge-parameter (first challenges) "realm"))
                      (ensure-equal 1 (count first closed :test #'eq))
                      (if fail-provider-p (error original) (http-basic-authorization "u" "p")))))
           (result (handler-case
                       (http-client-send client (http-client-request client "GET" "http://example.test/private"))
                     (http-protocol-error (condition) condition))))
      (ensure-equal 1 provider-calls)
      (ensure-false (search (ascii "Authorization:") (binary-test-output first)))
      (ensure-equal 0 (search (ascii "GET /private HTTP/1.1|CRLF|") (binary-test-output first)))
      (ensure-equal 1 (count first closed :test #'eq))
      (if fail-provider-p
          (progn
            (ensure-true (eq original result))
            (ensure-equal :client (http-error-operation result))
            (ensure-equal 1 opened)
            (ensure-equal 0 (length (binary-test-output second)))
            (ensure-equal 0 (count second closed :test #'eq))
            (close second))
          (progn
            (ensure-equal 204 (http-response-status result))
            (ensure-equal 2 opened)
            (ensure-equal 1 (count second closed :test #'eq))
            (ensure-equal 0 (search (ascii "GET /private HTTP/1.1|CRLF|") (binary-test-output second)))
            (ensure-true (search (ascii "Authorization: Basic dTpw|CRLF|") (binary-test-output second)))
            (ensure-equal 2 (length closed)))))))
(deftest client-auth-independent-bearer-padding-only
  (let ((condition (handler-case (http-bearer-authorization "=")
                     (http-protocol-error (condition) condition))))
    (ensure-true (typep condition 'http-protocol-error))
    (ensure-equal :client (http-error-operation condition))
    (ensure-equal :token (http-protocol-error-detail condition)))
  (ensure-equal "Bearer A=" (http-bearer-authorization "A=")))

(deftest client-auth-independent-challenge-token-boundaries
  (dolist (field '("Digest realm=a/b" "Ba/sic realm=r"
                   "Bearer abc, realm=r"))
    (ensure-false (http-parse-authentication-challenges field)))
  (dolist (entry '(("Digest realm=r" "Digest")
                   ("Basic realm=r" "Basic") ("Bearer abc" "Bearer")))
    (let ((challenges (http-parse-authentication-challenges (first entry))))
      (ensure-equal 1 (length challenges))
      (ensure-equal (second entry)
                    (http-authentication-challenge-scheme (first challenges)))))
  (ensure-equal "abc" (http-authentication-challenge-token68
                       (first (http-parse-authentication-challenges "Bearer abc")))))

(deftest client-auth-independent-challenge-input-types
  (dolist (input '(42 ("Basic realm=r" 42)))
    (let ((condition (handler-case (http-parse-authentication-challenges input)
                       (http-protocol-error (condition) condition))))
      (ensure-true (typep condition 'http-protocol-error))
      (ensure-equal :client (http-error-operation condition))
      (ensure-equal input (http-protocol-error-detail condition))))
  (ensure-false (http-parse-authentication-challenges nil))
  (ensure-equal 1 (length (http-parse-authentication-challenges
                          '("Basic realm=r")))))

(deftest client-auth-independent-content-digest-field-shapes
  (let* ((body (make-array 0 :element-type '(unsigned-byte 8)))
         (field (http-content-digest body)))
    (dolist (input '("sha@256=:AA==:" "Bad=:AA==:"
                     "sha-256=:" "sha-256=AA:"))
      (let ((condition (handler-case (http-content-digest-valid-p body input)
                         (http-protocol-error (condition) condition))))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :client (http-error-operation condition))
        (ensure-equal :field (http-protocol-error-detail condition))))
    (ensure-true (http-content-digest-valid-p body field))))

(deftest client-auth-independent-content-digest-unpadded-bytes
  (let* ((body (make-array 0 :element-type '(unsigned-byte 8)))
         (field (http-content-digest body)))
    (ensure-false (http-content-digest-valid-p body "example=:AAAA:"))
    (ensure-false (http-content-digest-valid-p body "example=:AA==:"))
    (ensure-true (http-content-digest-valid-p
                  body (concatenate 'string field ";bytes=:AAAA:")))
    (let ((condition (handler-case
                         (http-content-digest-valid-p body "example=:AA=A:")
                       (http-protocol-error (condition) condition))))
      (ensure-true (typep condition 'http-protocol-error))
      (ensure-equal :client (http-error-operation condition))
      (ensure-equal "example" (http-protocol-error-detail condition)))))

(deftest client-auth-independent-content-digest-token-delimiters
  (let* ((body (make-array 0 :element-type '(unsigned-byte 8)))
         (field (http-content-digest body))
         (condition (handler-case
                        (http-content-digest-valid-p
                         body (concatenate 'string field ";token=a["))
                      (http-protocol-error (condition) condition))))
    (ensure-true (typep condition 'http-protocol-error))
    (ensure-equal :client (http-error-operation condition))
    (ensure-equal :field (http-protocol-error-detail condition))
    (ensure-true (http-content-digest-valid-p
                  body (concatenate 'string field ";token=a/path")))))

(deftest client-auth-independent-content-digest-parameter-quote-state
  (let* ((body (make-array 0 :element-type '(unsigned-byte 8)))
         (field (http-content-digest body))
         (condition (handler-case
                        (http-content-digest-valid-p
                         body "example=:AA\"AA:;x=\"a")
                      (http-protocol-error (condition) condition))))
    (ensure-true (typep condition 'http-protocol-error))
    (ensure-equal :client (http-error-operation condition))
    (ensure-equal :field (http-protocol-error-detail condition))
    (ensure-true (http-content-digest-valid-p
                  body (concatenate 'string field ";x=\"a\"")))))

(deftest client-auth-independent-digest-quoted-tab-values
  (dolist (key '("realm" "nonce" "opaque"))
    (let* ((value (format nil "a~Cb" #\Tab))
           (field (cond ((string= key "realm")
                         (format nil "Digest realm=~S, nonce=\"n\"" value))
                        ((string= key "nonce")
                         (format nil "Digest realm=\"r\", nonce=~S" value))
                        (t (format nil "Digest realm=\"r\", nonce=\"n\", opaque=~S"
                                   value))))
           (challenges (http-parse-authentication-challenges field)))
      (ensure-equal 1 (length challenges))
      (ensure-equal value (http-authentication-challenge-parameter
                           (first challenges) key))
      (let ((condition (handler-case
                           (http-digest-authorization
                            (first challenges) "GET" "/" "u" "p" "c")
                         (http-protocol-error (condition) condition))))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :client (http-error-operation condition))
        (ensure-equal value (http-protocol-error-detail condition)))))
  (let* ((challenge (first (http-parse-authentication-challenges
                           "Digest realm=\"r\", nonce=\"n\", opaque=\"o\"")))
         (header (http-digest-authorization challenge "GET" "/" "u" "p" "c")))
    (ensure-equal 0 (search "Digest " header))
    (ensure-true (search "realm=\"r\"" header))
    (ensure-true (search "nonce=\"n\"" header))
    (ensure-true (search "opaque=\"o\"" header))
    (ensure-false (find #\Tab header))))


(deftest client-uri-independent-scheme-like-relative-paths
  (let* ((original "https://base.example.test/a/b?old=1")
         (base (parse-http-uri original)))
    (dolist (case '((":x" "https://base.example.test/a/:x")
                    ("1x:y" "https://base.example.test/a/1x:y")
                    ("a/b:c" "https://base.example.test/a/a/b:c")
                    ("a@b:c" "https://base.example.test/a/a@b:c")))
      (let ((result (resolve-http-uri base (first case))))
        (ensure-equal (second case) (http-uri-string result))
        (ensure-false (eq base result))
        (ensure-equal original (http-uri-string base))))))

(deftest client-uri-independent-alphanumeric-scheme-rejection
  (let* ((original "https://base.example.test/a/b")
         (base (parse-http-uri original)))
    (dolist (reference '("a1:x" "a:x"))
      (let ((condition (handler-case (resolve-http-uri base reference)
                         (http-protocol-error (condition) condition))))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :client (http-error-operation condition))
        (ensure-equal reference (http-protocol-error-detail condition))))
    (let ((result (resolve-http-uri base "HTTP://other.example.test/c")))
      (ensure-equal "http://other.example.test/c" (http-uri-string result))
      (ensure-false (eq base result))
      (ensure-equal original (http-uri-string base)))))

(deftest client-uri-independent-origin-identity-boundaries
  (dolist (case '(("http://base.example.test:443/a"
                  "https://base.example.test:443/b"
                  "http://base.example.test:443" "https://base.example.test" nil)
                 ("https://base.example.test/a"
                  "https://other.example.test:443/b"
                  "https://base.example.test" "https://other.example.test" nil)
                 ("HTTPS://BASE.EXAMPLE.TEST/a"
                  "https://base.example.test:443/b"
                  "https://base.example.test" "https://base.example.test" t)
                 ("https://[2001:db8::1]/a"
                  "https://[2001:db8::2]:443/b"
                  "https://[2001:db8::1]" "https://[2001:db8::2]" nil)))
    (destructuring-bind (left right left-origin right-origin same-p) case
      (ensure-equal left-origin (http-uri-origin left))
      (ensure-equal right-origin (http-uri-origin right))
      (ensure-equal same-p (not (null (http-same-origin-p left right)))))))
#+sbcl
(deftest client-cancellation-unregister-waits-for-running-handler
  (let* ((transport-entered (sb-thread:make-semaphore :count 0))
         (release-transport (sb-thread:make-semaphore :count 0))
         (handler-started (sb-thread:make-semaphore :count 0))
         (release-handler (sb-thread:make-semaphore :count 0))
         (cleanup-started (sb-thread:make-semaphore :count 0))
         (cleanup-finished (sb-thread:make-semaphore :count 0))
         (worker-finished (sb-thread:make-semaphore :count 0))
         (cancel-finished (sb-thread:make-semaphore :count 0))
         (token (make-http-cancellation-token))
         (cleanup-returned-p nil)
         (worker-condition nil)
         (cancel-result nil)
         (cancel-error nil)
         (worker
           (sb-thread:make-thread
            (lambda ()
              (unwind-protect
                   (handler-case
                       (http-client-send
                        (make-http-client
                         :cache nil
                         :transport-function
                         (lambda (request &key on-cancel &allow-other-keys)
                           (declare (ignore request))
                           (let ((unregister
                                   (funcall
                                    on-cancel
                                    (lambda ()
                                      (sb-thread:signal-semaphore handler-started)
                                      (sb-thread:wait-on-semaphore
                                       release-handler)))))
                             (unwind-protect
                                  (progn
                                    (sb-thread:signal-semaphore transport-entered)
                                    (sb-thread:wait-on-semaphore release-transport)
                                    (client-test-response 200))
                               (sb-thread:signal-semaphore cleanup-started)
                               (funcall unregister)
                               (setf cleanup-returned-p t)
                               (sb-thread:signal-semaphore cleanup-finished)))))
                        "GET" "http://example.test/cancel-cleanup"
                        :cancel-token token)
                     (http-client-request-cancelled (condition)
                       (setf worker-condition condition))
                     (error (condition)
                       (setf worker-condition condition)))
                (sb-thread:signal-semaphore worker-finished)))))
         (cancel-thread nil))
    (unwind-protect
         (progn
           (ensure-true
            (sb-thread:wait-on-semaphore transport-entered :timeout 5))
           (setf cancel-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (setf cancel-result
                                   (http-client-cancel-request token))
                           (error (condition)
                             (setf cancel-error condition)))
                      (sb-thread:signal-semaphore cancel-finished)))))
           (ensure-true
            (sb-thread:wait-on-semaphore handler-started :timeout 5))
           (sb-thread:signal-semaphore release-transport)
           (ensure-true
            (sb-thread:wait-on-semaphore cleanup-started :timeout 5))
           (ensure-false
            (sb-thread:wait-on-semaphore cleanup-finished :timeout 0.05))
           (sb-thread:signal-semaphore release-handler)
           (ensure-true
            (sb-thread:wait-on-semaphore cleanup-finished :timeout 5))
           (ensure-true
            (sb-thread:wait-on-semaphore cancel-finished :timeout 5))
           (ensure-true
            (sb-thread:wait-on-semaphore worker-finished :timeout 5))
           (sb-thread:join-thread cancel-thread :timeout 5)
           (sb-thread:join-thread worker :timeout 5)
           (ensure-true (typep worker-condition
                               'http-client-request-cancelled))
           (ensure-true cancel-result)
           (ensure-false cancel-error)
           (ensure-true cleanup-returned-p))
      (sb-thread:signal-semaphore release-transport)
      (sb-thread:signal-semaphore release-handler)
      (when cancel-thread
        (sb-thread:join-thread cancel-thread :timeout 5))
      (sb-thread:join-thread worker :timeout 5))))
