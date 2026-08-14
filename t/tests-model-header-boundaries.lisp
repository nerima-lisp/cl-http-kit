(in-package #:http-kit/test-core)

(deftest message-model-boundaries
  (dolist (entry '((100 "Continue")
                   (101 "Switching Protocols")
                   (102 "Processing")
                   (103 "Early Hints")
                   (200 "OK")
                   (201 "Created")
                   (202 "Accepted")
                   (204 "No Content")
                   (206 "Partial Content")
                   (301 "Moved Permanently")
                   (302 "Found")
                   (304 "Not Modified")
                   (307 "Temporary Redirect")
                   (308 "Permanent Redirect")
                   (400 "Bad Request")
                   (401 "Unauthorized")
                   (403 "Forbidden")
                   (404 "Not Found")
                   (405 "Method Not Allowed")
                   (408 "Request Timeout")
                   (409 "Conflict")
                   (429 "Too Many Requests")
                   (500 "Internal Server Error")
                   (501 "Not Implemented")
                   (502 "Bad Gateway")
                   (503 "Service Unavailable")
                   (504 "Gateway Timeout")
                   (505 "HTTP Version Not Supported")))
    (let ((response (make-http-response :status (first entry))))
      (ensure-equal (second entry)
                    (http-response-reason response)
                    "known HTTP reason phrase")))
  (ensure-equal ""
                (http-response-reason (make-http-response :status 418))
                "unknown HTTP reason phrase")
  (let ((response (make-http-response
                   :status 299
                   :reason "Application-defined"
                   :headers (list (cons "X-Result" "ok"))
                   :trailers (list (list "X-Trailer" "done"))
                   :body (octets 1 2 3))))
    (ensure-equal "Application-defined" (http-response-reason response))
    (ensure-equal "ok"
                  (http-header-value (http-response-headers response)
                                     "x-result"))
    (ensure-equal "done"
                  (http-header-value (http-response-trailers response)
                                     "x-trailer"))
    (ensure-equal (octets 1 2 3) (http-response-body response))
    (ensure-summary=
     "HTTP/1.1 299 Application-defined headers=1 trailers=1 body-bytes=3"
     (http-response-summary response)))
  (let* ((request (make-http-request
                   :method "GET"
                   :uri "http://127.0.0.1/"))
         (summary (http-request-summary request)))
    (ensure-summary=
     "GET http://127.0.0.1/ headers=0 trailers=0 body-bytes=0"
     summary)
    (ensure-summary-contains
     summary
     "GET"
     "http://127.0.0.1/"
     "body-bytes=0"))
  (signals http-protocol-error
    (make-http-request :method nil :uri "http://127.0.0.1/"))
  (signals http-protocol-error
    (make-http-request :method "GET /" :uri "http://127.0.0.1/"))
  (signals http-invalid-uri
    (make-http-request :method "GET" :uri 7))
  (signals http-invalid-status
    (make-http-response :status 99))
  (signals http-invalid-status
    (make-http-response :status 600))
  (signals http-invalid-status
    (make-http-response :status "200"))
  (signals http-invalid-status
    (make-http-response :status 200 :reason 7))
  (signals http-invalid-status
    (make-http-response
     :status 200
     :reason (concatenate 'string "bad" (string (code-char 1)))))
  (let ((response (make-http-response :status 200
                                     :protocol-version "HTTP/1.0")))
    (ensure-equal "HTTP/1.0"
                  (http-response-protocol-version response)
                  "response protocol version"))
  (let ((response (make-http-response :status 200
                                     :protocol-version "HTTP/3")))
    (ensure-equal "HTTP/3"
                  (http-response-protocol-version response)
                  "HTTP/3 response protocol version")))

(deftest header-normalization-boundaries
  (signals http-invalid-header
    (make-http-header "" "value"))
  (signals http-invalid-header
    (make-http-header 7 "value"))
  (signals http-invalid-header
    (make-http-header "X-Test" 7))
  (signals http-invalid-header
    (make-http-header
     "X-Test"
     (concatenate 'string "bad" (string (code-char 1)))))
  (let* ((request (make-http-request
                   :method "GET"
                   :uri "http://127.0.0.1/"
                   :headers (list (cons "X-String" "one")
                                  (list "X-List" "two")
                                  (make-http-header "X-Object" "three"))))
         (headers (http-request-headers request)))
    (ensure-equal '("one") (http-header-values headers "x-string"))
    (ensure-equal '("two") (http-header-values headers "X-LIST"))
    (ensure-equal "three" (http-header-value headers "x-object"))
    (ensure-equal "fallback" (http-header-value headers "x-missing" "fallback"))
    (ensure-true (http-header-present-p headers "x-object"))
    (ensure-true (not (http-header-present-p headers "x-missing")))
    (ensure-printed=
     "#<HTTP-HEADER X-Visible: visible>"
     (make-http-header "X-Visible" "visible"))
    (ensure-printed=
     "#<HTTP-HEADER Authorization: <redacted>>"
     (make-http-header "Authorization" "secret-token")))
  (signals http-invalid-header
    (make-http-request :method "GET"
                       :uri "http://127.0.0.1/"
                       :headers (list 7)))
  (signals http-invalid-header
    (make-http-request :method "GET"
                       :uri "http://127.0.0.1/"
                       :headers (list (list "X-Test"))))
  (signals http-invalid-header
    (make-http-request :method "GET"
                       :uri "http://127.0.0.1/"
                       :headers (list (list "X-Test" "one" "two"))))
  (signals http-invalid-header
    (http-header-values '() ""))
  (signals http-invalid-header
    (http-header-values '() 7)))
