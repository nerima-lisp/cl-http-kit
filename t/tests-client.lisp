(in-package #:http-kit/test)

(defun client-test-response (status &key headers body)
  (make-http-response :status status
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
      (ensure-equal "gzip, deflate"
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
              (ensure-equal "gzip, deflate, br"
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
    (let ((parts (parse-http-multipart-body
                  body
                  :content-type "multipart/form-data; boundary=\"boundary\"")))
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
                      (http-multipart-part-value upload))))))

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
    (ensure-equal nil
                  (http-strict-transport-store-known-host-p
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
            (lambda (host)
              (push host resolved-hosts)
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
(deftest client-informational-response-over-direct-stream
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (ascii
             "HTTP/1.1 100 Continue|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok")))
         (statuses nil)
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
           (push (http-response-status information) statuses)))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (ensure-equal '(100) (reverse statuses))
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
    (signals http-protocol-error
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
    (signals http-protocol-error
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

(deftest websocket-rfc6455-frame-boundaries
  (let* ((masked-frame
           (make-websocket-frame
            :fin-p t
            :opcode 1
            :mask-p t
            :masking-key (octets #x37 #xfa #x21 #x3d)
            :payload (ascii "Hello")))
         (masked-wire (serialize-websocket-frame masked-frame)))
    (ensure-equal
     (octets #x81 #x85 #x37 #xfa #x21 #x3d
             #x7f #x9f #x4d #x51 #x58)
     masked-wire)
    (multiple-value-bind (parsed consumed)
        (parse-websocket-frame masked-wire :require-mask-p t)
      (ensure-equal 11 consumed)
      (ensure-true (websocket-frame-fin-p parsed))
      (ensure-equal 1 (websocket-frame-opcode parsed))
      (ensure-equal (ascii "Hello") (websocket-frame-payload parsed)))
    (let* ((payload (make-array 126
                                :element-type '(unsigned-byte 8)
                                :initial-element #x61))
           (frame (make-websocket-frame :opcode 2 :payload payload))
           (wire (serialize-websocket-frame frame)))
      (ensure-equal #x7e (aref wire 1))
      (ensure-equal 130 (length wire))
      (multiple-value-bind (parsed consumed)
          (parse-websocket-frame wire)
        (ensure-equal (length wire) consumed)
        (ensure-equal payload (websocket-frame-payload parsed))))
    (signals http-protocol-error
      (parse-websocket-frame
       (serialize-websocket-frame
        (make-websocket-frame :payload (ascii "unmasked")))
       :require-mask-p t))
    (signals http-protocol-error
      (parse-websocket-frame (octets #x81 #x7e 0 125)))
    (signals http-protocol-error
      (parse-websocket-frame
       (octets #x81 #x7f 0 0 0 0 0 0 0 125)))))

#+sbcl
(deftest websocket-stream-rejects-nonminimal-lengths
  (dolist (wire (list (octets #x81 #x7e 0 125)
                      (octets #x81 #x7f 0 0 0 0 0 0 0 125)))
    (signals http-protocol-error
      (read-websocket-frame
       (make-instance 'binary-test-stream :input wire)))))

#+sbcl
(deftest websocket-stream-rejects-invalid-header-before-payload
  (dolist (wire (list (octets #xc1 0)
                      (octets #x83 0)
                      (octets #x09 0)
                      (octets #x89 126)))
    (signals http-protocol-error
      (read-websocket-frame
       (make-instance 'binary-test-stream :input wire)))))

(deftest websocket-http-upgrade-and-close-payload
  (let* ((request
           (make-http-request
            :method "GET"
            :uri "http://example.test/chat"
            :headers
            (list (make-http-header "Host" "example.test")
                  (make-http-header "Upgrade" "websocket")
                  (make-http-header "Connection" "keep-alive, Upgrade")
                  (make-http-header "Sec-WebSocket-Key"
                                    "dGhlIHNhbXBsZSBub25jZQ==")
                  (make-http-header "Sec-WebSocket-Version" "13")
                  (make-http-header "Sec-WebSocket-Protocol"
                                    "chat, superchat")
                  (make-http-header "Sec-WebSocket-Extensions"
                                    "permessage-deflate"))))
         (response
           (websocket-upgrade-response
            request
            :protocol "chat"
            :extensions "permessage-deflate")))
    (ensure-true (websocket-upgrade-request-p request))
    (dolist (protocol-value '("chat,,superchat"
                              "chat, chat"
                              "chat, bad protocol"))
      (ensure-true
       (not
        (websocket-upgrade-request-p
         (make-http-request
          :method "GET"
          :uri "http://example.test/chat"
          :headers
          (list (make-http-header "Host" "example.test")
                (make-http-header "Upgrade" "websocket")
                (make-http-header "Connection" "Upgrade")
                (make-http-header "Sec-WebSocket-Key"
                                  "dGhlIHNhbXBsZSBub25jZQ==")
                (make-http-header "Sec-WebSocket-Version" "13")
                (make-http-header "Sec-WebSocket-Protocol"
                                  protocol-value)))))))
    (ensure-equal "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
                  (websocket-accept-key
                   "dGhlIHNhbXBsZSBub25jZQ=="))
    (ensure-equal 101 (http-response-status response))
    (ensure-equal "websocket"
                  (http-header-value (http-response-headers response)
                                     "Upgrade"))
    (ensure-equal "chat"
                  (http-header-value (http-response-headers response)
                                     "Sec-WebSocket-Protocol"))
    (ensure-equal "permessage-deflate"
                  (http-header-value (http-response-headers response)
                                     "Sec-WebSocket-Extensions"))
    (signals http-protocol-error
      (websocket-upgrade-response request :protocol "not-offered"))
    (let ((close-payload
            (make-websocket-close-payload :code 1000 :reason "bye")))
      (multiple-value-bind (code reason)
          (parse-websocket-close-payload close-payload)
        (ensure-equal 1000 code)
        (ensure-equal "bye" reason)))
    (signals http-protocol-error
      (make-websocket-close-payload :code 1004))
    (dolist (code '(1012 1013 1014))
      (multiple-value-bind (parsed-code reason)
          (parse-websocket-close-payload
           (make-websocket-close-payload :code code))
        (ensure-equal code parsed-code)
        (ensure-equal "" reason)))
    (signals http-protocol-error
      (make-websocket-close-payload :code 1015))
    (signals http-protocol-error
      (parse-websocket-close-payload (octets 3)))))

(deftest websocket-extension-negotiation-validation
  (let* ((extensions
           (parse-websocket-extensions
            "permessage-deflate; client_max_window_bits; server_max_window_bits=12, x-test; mode=\"fast\""))
         (compression (first extensions))
         (custom (second extensions)))
    (ensure-equal 2 (length extensions))
    (ensure-equal "permessage-deflate"
                  (websocket-extension-name compression))
    (ensure-equal '(("client_max_window_bits")
                    ("server_max_window_bits" . "12"))
                  (websocket-extension-parameters compression))
    (ensure-equal "x-test" (websocket-extension-name custom))
    (ensure-equal '(("mode" . "fast"))
                  (websocket-extension-parameters custom)))
  (dolist (value '(""
                   "permessage-deflate,"
                   "permessage-deflate; mode=\"not a token\""
                   "permessage-deflate; client_max_window_bits=07"
                   "permessage-deflate; client_max_window_bits=16"
                   "permessage-deflate; client_max_window_bits=10; client_max_window_bits=11"
                   "permessage-deflate; unknown=true"))
    (signals http-protocol-error
      (make-websocket-upgrade-request
       "http://example.test/chat"
       :key "dGhlIHNhbXBsZSBub25jZQ=="
       :extensions value)))
  (let ((request
          (make-websocket-upgrade-request
           "http://example.test/chat"
           :key "dGhlIHNhbXBsZSBub25jZQ=="
           :extensions "permessage-deflate")))
    (ensure-equal
     "permessage-deflate; server_max_window_bits=12"
     (http-header-value
      (http-response-headers
       (websocket-upgrade-response
        request
        :extensions "permessage-deflate; server_max_window_bits=12"))
      "Sec-WebSocket-Extensions"))
    (signals http-protocol-error
      (websocket-upgrade-response
       request
       :extensions "permessage-deflate; client_max_window_bits=12"))))

(deftest websocket-client-key-generation
  (let ((requested-length nil)
        (expected-octets (octets 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))
    (flet ((random-octets (length)
             (setf requested-length length)
             expected-octets))
      (ensure-equal "AAECAwQFBgcICQoLDA0ODw=="
                    (make-websocket-client-key #'random-octets))
      (ensure-equal 16 requested-length)
      (let ((request
              (make-websocket-upgrade-request
               "http://example.test/chat"
               :random-octets-function #'random-octets)))
        (ensure-true (websocket-upgrade-request-p request))
        (ensure-equal
         "AAECAwQFBgcICQoLDA0ODw=="
         (http-header-value (http-request-headers request)
                            "Sec-WebSocket-Key")))))
  (signals http-protocol-error
    (make-websocket-client-key nil))
  (signals http-protocol-error
    (make-websocket-client-key
     (lambda (length)
       (declare (ignore length))
       (make-array 15 :element-type '(unsigned-byte 8)))))
  (signals http-protocol-error
    (make-websocket-client-key
     (lambda (length)
       (declare (ignore length))
       "not octets")))
  (signals http-protocol-error
    (make-websocket-upgrade-request
     "http://example.test/chat"
     :key "dGhlIHNhbXBsZSBub25jZQ=="
     :random-octets-function
     (lambda (length)
       (make-array length :element-type '(unsigned-byte 8)))))
  (signals http-protocol-error
    (make-websocket-upgrade-request "http://example.test/chat")))

#+sbcl
(deftest websocket-client-handshake-keeps-stream-open
  (let* ((key "dGhlIHNhbXBsZSBub25jZQ==")
         (request
           (make-websocket-upgrade-request
            "http://example.test/chat"
            :key key
            :protocols '("chat" "superchat")
            :headers (list (make-http-header "Origin" "http://example.test"))))
         (frame-wire
           (serialize-websocket-frame
            (make-websocket-frame :opcode 1 :payload (ascii "hello"))))
         (stream
           (make-instance
            'binary-test-stream
            :input
            (concatenate-octets
             (ascii
              (concatenate
               'string
               "HTTP/1.1 101 Switching Protocols|CRLF|"
               "Upgrade: websocket|CRLF|"
               "Connection: Upgrade|CRLF|"
               "Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=|CRLF|"
               "Sec-WebSocket-Protocol: chat|CRLF||CRLF|"))
             frame-wire))))
    (ensure-true (websocket-upgrade-request-p request))
    (multiple-value-bind (response reusable-p)
        (websocket-client-handshake stream request)
      (ensure-equal 101 (http-response-status response))
      (ensure-true (not reusable-p))
      (multiple-value-bind (frame consumed)
          (read-websocket-frame stream)
        (ensure-equal (length frame-wire) consumed)
        (ensure-equal 1 (websocket-frame-opcode frame))
        (ensure-equal (ascii "hello") (websocket-frame-payload frame))))
    (let ((wire (octets-as-string (binary-test-output stream))))
      (ensure-true (search "GET /chat HTTP/1.1" wire))
      (ensure-true (search (concatenate 'string "Sec-WebSocket-Key: " key) wire))
      (ensure-true (search "Sec-WebSocket-Protocol: chat, superchat" wire))
      (ensure-true (search "Origin: http://example.test" wire)))
    (signals http-protocol-error
      (make-websocket-upgrade-request
       "http://example.test/chat"
       :key key
       :headers (list (make-http-header "Upgrade" "other"))))
    (signals http-protocol-error
      (make-websocket-upgrade-request
       "http://example.test/chat"
       :key key
       :protocols '("chat" "chat")))
    (let ((bad-stream
            (make-instance
             'binary-test-stream
             :input
             (ascii
              (concatenate
               'string
               "HTTP/1.1 101 Switching Protocols|CRLF|"
               "Upgrade: websocket|CRLF|"
               "Connection: Upgrade|CRLF|"
               "Sec-WebSocket-Accept: invalid|CRLF||CRLF|")))))
      (signals http-protocol-error
        (websocket-client-handshake bad-stream request)))
    (let* ((extension-request
             (make-websocket-upgrade-request
              "http://example.test/chat"
              :key key
              :extensions "permessage-deflate"))
           (bad-extension-stream
             (make-instance
              'binary-test-stream
              :input
              (ascii
               (concatenate
                'string
                "HTTP/1.1 101 Switching Protocols|CRLF|"
                "Upgrade: websocket|CRLF|"
                "Connection: Upgrade|CRLF|"
                "Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=|CRLF|"
                "Sec-WebSocket-Extensions: unknown-extension|CRLF||CRLF|")))))
      (signals http-protocol-error
        (websocket-client-handshake
         bad-extension-stream extension-request)))))

#+sbcl
(deftest websocket-fragmented-message-and-control-frames
  (let* ((wire
           (concatenate-octets
            (serialize-websocket-frame
             (make-websocket-frame :opcode 9 :payload (ascii "ping")))
            (serialize-websocket-frame
             (make-websocket-frame :fin-p nil
                                   :opcode 1
                                   :payload (ascii "Hel")))
            (serialize-websocket-frame
             (make-websocket-frame :fin-p t
                                   :opcode 0
                                   :payload (ascii "lo")))))
         (stream (make-instance 'binary-test-stream :input wire))
         (control-opcodes nil))
    (multiple-value-bind (message opcode)
        (read-websocket-message
         stream
         :on-control (lambda (frame)
                       (push (websocket-frame-opcode frame)
                             control-opcodes)))
    (ensure-equal (ascii "Hello") message)
    (ensure-equal 1 opcode))
    (ensure-equal '(9) control-opcodes)))

#+sbcl
(deftest websocket-text-messages-require-complete-valid-utf8
  (flet ((frame-wire (fin-p opcode payload)
           (serialize-websocket-frame
            (make-websocket-frame :fin-p fin-p
                                  :opcode opcode
                                  :payload payload))))
    (signals http-protocol-error
      (read-websocket-message
       (make-instance 'binary-test-stream
                      :input (frame-wire t 1 (octets #xc0 #x80)))))
    (signals http-protocol-error
      (read-websocket-message
       (make-instance
        'binary-test-stream
        :input (concatenate-octets
                (frame-wire nil 1 (octets #xe3))
                (frame-wire t 0 (octets #x81))))))
    (multiple-value-bind (message opcode)
        (read-websocket-message
         (make-instance
          'binary-test-stream
          :input (concatenate-octets
                  (frame-wire nil 1 (octets #xe3))
                  (frame-wire t 0 (octets #x81 #x82)))))
      (ensure-equal (octets #xe3 #x81 #x82) message)
      (ensure-equal 1 opcode))))

#+sbcl
(deftest websocket-compressed-message-wire-and-limits
  (flet ((reverse-octets (value)
           (let ((result (make-array (length value)
                                     :element-type '(unsigned-byte 8))))
             (loop for source downfrom (1- (length value)) to 0
                   for target from 0
                   do (setf (aref result target) (aref value source)))
             result)))
    (let ((output (make-instance 'binary-test-stream :input (octets))))
      (ensure-equal
       '(3 5)
       (multiple-value-list
        (write-websocket-message output "Hello"
                                 :opcode 1
                                 :max-frame-payload-bytes 2
                                 :compress-function #'reverse-octets)))
      (let ((wire (binary-test-output output)))
        (signals http-protocol-error
          (parse-websocket-frame wire))
        (multiple-value-bind (first consumed)
            (parse-websocket-frame wire :allow-rsv1-p t)
          (ensure-true (websocket-frame-rsv1-p first))
          (ensure-equal 1 (websocket-frame-opcode first))
          (multiple-value-bind (second second-consumed)
              (parse-websocket-frame (subseq wire consumed)
                                     :allow-rsv1-p t)
            (ensure-false (websocket-frame-rsv1-p second))
            (ensure-equal 0 (websocket-frame-opcode second))
            (multiple-value-bind (third third-consumed)
                (parse-websocket-frame
                 (subseq wire (+ consumed second-consumed))
                 :allow-rsv1-p t)
              (ensure-false (websocket-frame-rsv1-p third))
              (ensure-equal (length wire)
                            (+ consumed second-consumed third-consumed)))))
        (multiple-value-bind (message opcode)
            (read-websocket-message
             (make-instance 'binary-test-stream :input wire)
             :decompress-function #'reverse-octets)
          (ensure-equal (ascii "Hello") message)
          (ensure-equal 1 opcode))))
    (signals http-protocol-error
      (make-websocket-frame :opcode 0 :rsv1-p t))
    (let ((wire (serialize-websocket-frame
                 (make-websocket-frame :opcode 2 :rsv1-p t
                                       :payload (octets 1)))))
      (signals http-size-limit-exceeded
        (read-websocket-message
         (make-instance 'binary-test-stream :input wire)
         :max-message-bytes 2
         :decompress-function (lambda (value)
                                (declare (ignore value))
                                (octets 1 2 3)))))))

#+sbcl
(deftest websocket-message-and-control-frame-writers
  (let* ((stream
           (make-instance 'binary-test-stream :input (octets)))
         (masking-key-count 0)
         (message-result
           (multiple-value-list
            (write-websocket-message
             stream "Hello"
             :opcode 1
             :max-frame-payload-bytes 2
             :mask-p t
             :masking-key-function
             (lambda ()
               (incf masking-key-count)
               (octets 1 2 3 4)))))
         (wire (binary-test-output stream)))
    (ensure-equal '(3 5) message-result)
    (ensure-equal 3 masking-key-count)
    (multiple-value-bind (first first-consumed)
        (parse-websocket-frame wire :require-mask-p t)
      (multiple-value-bind (second second-consumed)
          (parse-websocket-frame (subseq wire first-consumed)
                                 :require-mask-p t
                                 :allow-unmasked-p nil)
        (multiple-value-bind (third third-consumed)
            (parse-websocket-frame
             (subseq wire (+ first-consumed second-consumed))
             :require-mask-p t)
          (ensure-equal 1 (websocket-frame-opcode first))
          (ensure-true (not (websocket-frame-fin-p first)))
          (ensure-equal (ascii "He") (websocket-frame-payload first))
          (ensure-equal 0 (websocket-frame-opcode second))
          (ensure-true (not (websocket-frame-fin-p second)))
          (ensure-equal (ascii "ll") (websocket-frame-payload second))
          (ensure-equal 0 (websocket-frame-opcode third))
          (ensure-true (websocket-frame-fin-p third))
          (ensure-equal (ascii "o") (websocket-frame-payload third))
          (ensure-equal (length wire)
                        (+ first-consumed second-consumed third-consumed))))))
  (let ((stream (make-instance 'binary-test-stream :input (octets))))
    (signals http-protocol-error
      (write-websocket-message
       stream (ascii "too long for one fixed key")
       :max-frame-payload-bytes 2
       :mask-p t
       :masking-key (octets 1 2 3 4)))))

#+sbcl
(deftest websocket-control-frame-writers
  (let ((stream (make-instance 'binary-test-stream :input (octets))))
    (websocket-ping stream
                    :payload "ping"
                    :mask-p t
                    :masking-key (octets 4 3 2 1))
    (websocket-pong stream :payload (octets 1 2 3))
    (websocket-close stream :code 1000 :reason "bye")
    (let ((wire (binary-test-output stream)))
      (multiple-value-bind (ping ping-consumed)
          (parse-websocket-frame wire :require-mask-p t)
        (multiple-value-bind (pong pong-consumed)
            (parse-websocket-frame (subseq wire ping-consumed))
          (multiple-value-bind (close close-consumed)
              (parse-websocket-frame
               (subseq wire (+ ping-consumed pong-consumed)))
            (ensure-equal 9 (websocket-frame-opcode ping))
            (ensure-equal (ascii "ping") (websocket-frame-payload ping))
            (ensure-equal 10 (websocket-frame-opcode pong))
            (ensure-equal (octets 1 2 3) (websocket-frame-payload pong))
            (ensure-equal 8 (websocket-frame-opcode close))
            (multiple-value-bind (code reason)
                (parse-websocket-close-payload
                 (websocket-frame-payload close))
              (ensure-equal 1000 code)
              (ensure-equal "bye" reason))
            (ensure-equal (length wire)
                          (+ ping-consumed pong-consumed close-consumed))))))
  (let ((stream (make-instance 'binary-test-stream :input (octets))))
    (signals http-protocol-error
      (websocket-close stream :payload (octets 0) :code 1000))
    (signals http-protocol-error
      (websocket-close stream :payload (octets 0)))
    (signals http-protocol-error
      (websocket-close stream :payload (octets 3 236)))
    (signals http-protocol-error
      (websocket-close stream :payload (octets 3 232 192 128))))))

#+sbcl
(deftest websocket-server-session-ping-close-and-masking
  (let* ((close-payload
           (make-websocket-close-payload :code 1000 :reason "bye"))
         (input
           (concatenate-octets
            (serialize-websocket-frame
             (make-websocket-frame
              :opcode 9
              :mask-p t
              :masking-key (octets 1 2 3 4)
              :payload (ascii "ping")))
            (serialize-websocket-frame
             (make-websocket-frame
              :opcode 1
              :mask-p t
              :masking-key (octets 5 6 7 8)
              :payload (ascii "hello")))
            (serialize-websocket-frame
             (make-websocket-frame
              :opcode 8
              :mask-p t
              :masking-key (octets 9 10 11 12)
              :payload close-payload))))
         (stream (make-instance 'binary-test-stream :input input))
         (messages nil)
         (control-opcodes nil))
    (multiple-value-bind (count termination)
        (serve-websocket-session
         stream
         (lambda (received-stream payload opcode)
           (declare (ignore received-stream))
           (push (list payload opcode) messages))
         :close-stream nil
         :on-control
         (lambda (frame)
           (push (websocket-frame-opcode frame) control-opcodes)))
      (ensure-equal 1 count)
      (ensure-equal :peer-close termination))
    (ensure-equal (list (list (ascii "hello") 1)) (nreverse messages))
    (ensure-equal '(9 8) (nreverse control-opcodes))
    (let ((wire (binary-test-output stream)))
      (multiple-value-bind (pong pong-consumed)
          (parse-websocket-frame wire)
        (multiple-value-bind (close close-consumed)
            (parse-websocket-frame (subseq wire pong-consumed))
          (ensure-equal 10 (websocket-frame-opcode pong))
          (ensure-equal (ascii "ping") (websocket-frame-payload pong))
          (ensure-equal 8 (websocket-frame-opcode close))
          (ensure-equal close-payload (websocket-frame-payload close))
          (multiple-value-bind (code reason)
              (parse-websocket-close-payload
               (websocket-frame-payload close))
            (ensure-equal 1000 code)
            (ensure-equal "bye" reason))
          (ensure-equal (length wire) (+ pong-consumed close-consumed)))))))

#+sbcl
(deftest websocket-server-session-requires-masked-client-frames
  (let* ((input
           (serialize-websocket-frame
            (make-websocket-frame :opcode 1 :payload (ascii "bad"))))
         (stream (make-instance 'binary-test-stream :input input))
         (condition nil))
    (signals http-protocol-error
      (serve-websocket-session
       stream
       (lambda (received-stream payload opcode)
         (declare (ignore received-stream payload opcode)))
       :close-stream nil
       :on-error (lambda (seen-condition)
                   (setf condition seen-condition))))
    (ensure-true (typep condition 'http-protocol-error))
    (let ((wire (binary-test-output stream)))
      (multiple-value-bind (close consumed)
          (parse-websocket-frame wire)
        (ensure-equal 8 (websocket-frame-opcode close))
        (multiple-value-bind (code reason)
            (parse-websocket-close-payload
             (websocket-frame-payload close))
          (ensure-equal 1002 code)
          (ensure-equal "WebSocket session error" reason))
        (ensure-equal (length wire) consumed)))))

#+sbcl
(deftest websocket-server-session-invalid-utf8-closes-with-1007
  (let* ((input
           (serialize-websocket-frame
            (make-websocket-frame
             :opcode 1
             :mask-p t
             :masking-key (octets 1 2 3 4)
             :payload (octets #xc0 #x80))))
         (stream (make-instance 'binary-test-stream :input input))
         (condition nil))
    (signals http-protocol-error
      (serve-websocket-session
       stream
       (lambda (received-stream payload opcode)
         (declare (ignore received-stream payload opcode)))
       :close-stream nil
       :on-error (lambda (seen-condition)
                   (setf condition seen-condition))))
    (ensure-equal :websocket-utf8 (http-error-operation condition))
    (let ((wire (binary-test-output stream)))
      (multiple-value-bind (close consumed)
          (parse-websocket-frame wire)
        (ensure-equal 8 (websocket-frame-opcode close))
        (multiple-value-bind (code reason)
            (parse-websocket-close-payload
             (websocket-frame-payload close))
          (ensure-equal 1007 code)
          (ensure-equal "WebSocket session error" reason))
        (ensure-equal (length wire) consumed)))))

(deftest client-sse-parse-and-serialize
  (let* ((linefeed (string #\Linefeed))
         (input
           (concatenate-octets
            (octets #xef #xbb #xbf)
            (ascii
             "event: update|CRLF|data: hello|CRLF|data: world|CRLF|id: 7|CRLF|")
            (ascii "retry: 1500|CRLF||CRLF|:keepalive")
            (octets #x0a)
            (ascii "data: final|CRLF||CRLF|")))
         (events (parse-http-sse-events input)))
    (ensure-equal 2 (length events))
    (let ((first (first events))
          (second (second events)))
      (ensure-equal "update" (http-sse-event-event first))
      (ensure-equal (concatenate 'string "hello" linefeed "world")
                    (http-sse-event-data first))
      (ensure-equal "7" (http-sse-event-id first))
      (ensure-equal 1500 (http-sse-event-retry first))
      (ensure-equal "message" (http-sse-event-event second))
      (ensure-equal "final" (http-sse-event-data second))
      (ensure-equal "7" (http-sse-event-id second))
      (ensure-equal '("keepalive") (http-sse-event-comments second)))
    (let ((comment-separated
            (parse-http-sse-events
             (ascii ":discard|CRLF||CRLF|data:kept|CRLF||CRLF|"))))
      (ensure-equal nil (http-sse-event-comments (first comment-separated))))
    (ensure-equal nil
                  (parse-http-sse-events
                   (ascii "data: incomplete|CRLF|")))
    (let* ((event
             (make-http-sse-event
              :event "notice"
              :data (concatenate 'string "a" linefeed "b")
              :id "9"
              :retry 10
              :comments '("c" "d")))
           (wire (serialize-http-sse-event event)))
      (ensure-equal
       (ascii
        (concatenate
         'string
         ":c|CRLF|:d|CRLF|event:notice|CRLF|id:9|CRLF|retry:10|CRLF|"
         "data:a|CRLF|data:b|CRLF||CRLF|"))
       wire)
      (let ((round-trip (first (parse-http-sse-events wire))))
        (ensure-equal "notice" (http-sse-event-event round-trip))
        (ensure-equal (concatenate 'string "a" linefeed "b")
                      (http-sse-event-data round-trip))
        (ensure-equal "9" (http-sse-event-id round-trip))
        (ensure-equal 10 (http-sse-event-retry round-trip))
        (ensure-equal '("c" "d") (http-sse-event-comments round-trip))))))

#+sbcl
(deftest client-sse-stream-callback-and-limits
  (let* ((linefeed (string #\Linefeed))
         (wire
           (concatenate-octets
            (ascii "data:one|CRLF|data:two")
            (octets #x0a #x0a)))
         (stream (make-instance 'binary-test-stream :input wire))
         (seen nil)
         (events
           (read-http-sse-events
            stream
            :on-event (lambda (event)
                        (push (http-sse-event-data event) seen)))))
    (ensure-equal (list (concatenate 'string "one" linefeed "two"))
                  (mapcar #'http-sse-event-data events))
    (ensure-equal (list (concatenate 'string "one" linefeed "two"))
                  (nreverse seen)))
  (signals http-size-limit-exceeded
    (parse-http-sse-events (concatenate-octets (ascii "data:one")
                                               (octets #x0a #x0a))
                           :max-line-bytes 4))
  (signals http-size-limit-exceeded
    (parse-http-sse-events
     (concatenate-octets
      (ascii "data:one")
      (octets #x0a #x0a)
      (ascii "data:two")
      (octets #x0a #x0a))
     :max-events 1))
  (signals http-size-limit-exceeded
    (parse-http-sse-events
     (ascii ":one|CRLF|:two|CRLF|data:x|CRLF||CRLF|")
     :max-event-bytes 10))
  (signals http-protocol-error
    (parse-http-sse-events
     (octets #x64 #x61 #x74 #x61 #x3a #xc3 #x28 #x0a #x0a))))
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
    (ensure-equal nil
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
                                              "Cache-Control" "max-age=60")))))
                       :cache (make-http-cache :clock-function (lambda () now)))
      (let ((request (http-client-request client "GET"
                                          "http://example.test/resource")))
        (multiple-value-bind (response effective)
            (http-client-send client request)
          (ensure-equal 200 (http-response-status response))
          (ensure-equal request effective))
        (setf now 1001)
        (multiple-value-bind (response effective)
            (http-client-send client request)
          (ensure-equal 200 (http-response-status response))
          (ensure-equal request effective)
          (ensure-equal "cached" (octets-as-string (http-response-body response))))
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
