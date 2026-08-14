(in-package #:http-kit/test)

(deftest client-uri-encoding-and-authentication
  (let ((resolved (resolve-http-uri
                   "http://example.test/a/b"
                   "../c?x=1#fragment")))
    (ensure-equal "http://example.test/c?x=1"
                  (http-uri-string resolved))
    (ensure-true (http-same-origin-p resolved "http://example.test:80/")))
  (ensure-equal "q=a+b&x=1%2B2"
                (http-form-urlencode '(("q" "a b") ("x" "1+2"))))
  (ensure-equal "Basic dXNlcjpwYXNz"
                (http-basic-authorization "user" "pass"))
  (ensure-equal "Bearer token"
                (http-bearer-authorization "token")))

(deftest client-request-trailers-through-policy
  (let ((seen nil)
        (trailers (list (make-http-header "X-Checksum" "abc"))))
    (with-test-client (client
                       (lambda (request &key proxy-plan &allow-other-keys)
                         (declare (ignore proxy-plan))
                         (setf seen request)
                         (client-test-response 200))
                       :cache nil)
      (let ((request (http-client-request
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
                         (http-request-trailers seen) "X-Checksum")))))))

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
    (ensure-equal "multipart/form-data; boundary=boundary" content-type)
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

(deftest client-multipart-parser-enforces-limits
  (multiple-value-bind (body content-type)
      (make-http-multipart-body
       (list (make-http-multipart-part :name "field" :value "value"))
       :boundary "boundary")
    (signals http-size-limit-exceeded
      (parse-http-multipart-body body :content-type content-type :max-parts 0))
    (signals http-size-limit-exceeded
      (parse-http-multipart-body body :content-type content-type :max-body-bytes 2))
    (signals http-protocol-error
      (parse-http-multipart-body body
                                 :content-type "multipart/form-data; boundary=other"))))

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

(deftest client-cache-integration
  (let ((calls 0)
        (cache (make-http-cache :clock-function (lambda () 1000))))
    (with-test-client (client
                       (lambda (request &key proxy-plan &allow-other-keys)
                         (declare (ignore request proxy-plan))
                         (incf calls)
                         (client-test-response
                          200
                          :headers (list (make-http-header
                                          "Cache-Control" "max-age=60"))
                          :body (ascii "cached")))
                       :cache cache)
      (let ((request (http-client-request client "GET"
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
        (ensure-equal 1 (length (http-cache-entries cache)))))))

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
                  proxy "http://bypass.example/path"))))
