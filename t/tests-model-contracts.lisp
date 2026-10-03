(in-package #:http-kit/test-core)

(deftest uri-model-contract
  (let ((uri (make-http-uri :scheme "HTTPS"
                            :authority "[2001:DB8::1]:443"
                            :path "/resource"
                            :query "q=one")))
    (ensure-true (http-uri-p uri))
    (ensure-equal "https" (http-uri-scheme uri))
    (ensure-equal "2001:db8::1" (http-uri-host uri))
    (ensure-equal 443 (http-uri-port uri))
    (ensure-equal "[2001:db8::1]:443" (http-uri-authority uri))
    (ensure-equal "/resource" (http-uri-path uri))
    (ensure-equal "q=one" (http-uri-query uri))
    (ensure-equal "https://[2001:db8::1]:443/resource?q=one"
                  (http-uri-string uri)))
  (signals http-invalid-uri
    (make-http-uri :authority 7)))

(deftest header-model-contract
  (let* ((header (make-http-header "X-Test" "  value  "))
         (request (make-http-request
                   :method "GET"
                   :uri "http://example.test/"
                   :headers (list (cons "X-String" "one")
                                  (list "X-List" "two")
                                  header)))
         (headers (http-request-headers request)))
    (ensure-true (http-header-p header))
    (ensure-equal "X-Test" (http-header-name header))
    (ensure-equal "value" (http-header-content header))
    (ensure-equal '("one") (http-header-values headers "x-string"))
    (ensure-equal '("two") (http-header-values headers "X-LIST"))
    (ensure-equal "value" (http-header-value headers "x-test"))
    (ensure-true (http-header-present-p headers "X-TEST")))
  (signals http-invalid-header
    (make-http-header "X-Test" 7)))

(deftest request-model-contract
  (let* ((input-uri (copy-seq "HTTP://EXAMPLE.TEST/path?q=one"))
         (input-method (copy-seq "GET"))
         (request (make-http-request
                   :method input-method
                   :protocol "WebSocket"
                   :uri input-uri
                   :headers (list (cons "X-Test" "value"))
                   :body (octets 1 2 3))))
    (setf (char input-uri 0) #\x
          (char input-method 0) #\x)
    (ensure-true (http-request-p request))
    (ensure-equal "GET" (http-request-method request))
    (ensure-equal "websocket" (http-request-protocol request))
    (ensure-equal "example.test" (http-request-authority request))
    (ensure-equal "/path" (http-request-path request))
    (ensure-equal "q=one" (http-request-query request))
    (ensure-equal "value"
                  (http-header-value (http-request-headers request) "x-test"))
    (ensure-equal (octets 1 2 3) (http-request-body request)))
  (signals http-protocol-error
    (make-http-request :method "GET /" :uri "http://example.test/")))

(deftest response-model-contract
  (let* ((input-body (octets #x80 #xff))
         (response (make-http-response
                    :status 200
                    :headers (list (cons "X-Result" "ok"))
                    :trailers (list (list "X-Trailer" "done"))
                    :body input-body)))
    (setf (aref input-body 0) 0)
    (ensure-true (http-response-p response))
    (ensure-equal 200 (http-response-status response))
    (ensure-equal "OK" (http-response-reason response))
    (ensure-equal "ok"
                  (http-header-value (http-response-headers response)
                                     "x-result"))
    (ensure-equal "done"
                  (http-header-value (http-response-trailers response)
                                     "x-trailer"))
    (let ((returned-body (http-response-body response)))
      (setf (aref returned-body 1) 0)
      (ensure-equal (octets #x80 #xff) (http-response-body response))))
  (signals http-invalid-status
    (make-http-response :status "200")))
