(in-package #:http-kit/test-core)
(deftest p4a-shared-message-symbol-identity
  (dolist (name '("HTTP-CONNECTION-ERROR" "HTTP-CONNECTION-ERROR-CAUSE"
                  "HTTP-DEADLINE" "HTTP-ERROR" "HTTP-ERROR-MESSAGE"
                  "HTTP-ERROR-OPERATION" "HTTP-HEADER" "HTTP-HEADER-CONTENT"
                  "HTTP-HEADER-NAME" "HTTP-HEADER-P" "HTTP-HEADER-PRESENT-P"
                  "HTTP-HEADER-VALUE" "HTTP-HEADER-VALUES" "HTTP-INVALID-HEADER"
                  "HTTP-INVALID-HEADER-NAME" "HTTP-INVALID-HEADER-REASON"
                  "HTTP-INVALID-STATUS" "HTTP-INVALID-STATUS-CODE"
                  "HTTP-INVALID-STATUS-LINE" "HTTP-INVALID-URI"
                  "HTTP-INVALID-URI-INPUT" "HTTP-PROTOCOL-ERROR"
                  "HTTP-PROTOCOL-ERROR-DETAIL" "HTTP-REQUEST"
                  "HTTP-REQUEST-AUTHORITY" "HTTP-REQUEST-BODY"
                  "HTTP-REQUEST-HEADERS" "HTTP-REQUEST-METHOD" "HTTP-REQUEST-P"
                  "HTTP-REQUEST-PATH" "HTTP-REQUEST-PROTOCOL"
                  "HTTP-REQUEST-PROTOCOL-VERSION" "HTTP-REQUEST-QUERY"
                  "HTTP-REQUEST-TARGET" "HTTP-REQUEST-TRAILERS" "HTTP-REQUEST-URI"
                  "HTTP-RESPONSE" "HTTP-RESPONSE-BODY" "HTTP-RESPONSE-HEADERS"
                  "HTTP-RESPONSE-P" "HTTP-RESPONSE-PROTOCOL-VERSION"
                  "HTTP-RESPONSE-REASON" "HTTP-RESPONSE-STATUS"
                  "HTTP-RESPONSE-STREAM" "HTTP-RESPONSE-STREAM-BODY-FUNCTION"
                  "HTTP-RESPONSE-STREAM-BODY-LENGTH" "HTTP-RESPONSE-STREAM-HEADERS"
                  "HTTP-RESPONSE-STREAM-P" "HTTP-RESPONSE-STREAM-PROTOCOL-VERSION"
                  "HTTP-RESPONSE-STREAM-REASON" "HTTP-RESPONSE-STREAM-STATUS"
                  "HTTP-RESPONSE-STREAM-TRAILERS" "HTTP-RESPONSE-TRAILERS"
                  "HTTP-SIZE-LIMIT-EXCEEDED" "HTTP-SIZE-LIMIT-EXCEEDED-KIND"
                  "HTTP-SIZE-LIMIT-EXCEEDED-LIMIT"
                  "HTTP-SIZE-LIMIT-EXCEEDED-OBSERVED" "HTTP-TIMEOUT"
                  "HTTP-TIMEOUT-KIND" "HTTP-UNSUPPORTED-FEATURE"
                  "HTTP-UNSUPPORTED-FEATURE-NAME" "HTTP-URI" "HTTP-URI-AUTHORITY"
                  "HTTP-URI-HOST" "HTTP-URI-P" "HTTP-URI-PATH" "HTTP-URI-PORT"
                  "HTTP-URI-QUERY" "HTTP-URI-SCHEME" "HTTP-URI-STRING"
                  "MAKE-HTTP-HEADER" "MAKE-HTTP-REQUEST" "MAKE-HTTP-RESPONSE"
                  "MAKE-HTTP-RESPONSE-STREAM" "MAKE-HTTP-URI" "PARSE-HTTP-URI"
                  "WITH-HTTP-DEADLINE"))
    (ensure-true (eq (find-symbol name "HTTP-KIT")
                     (find-symbol name "HTTP-MESSAGE-KIT")))
    (ensure-equal :external
                  (nth-value 1 (find-symbol name "HTTP-KIT")))
    (ensure-equal :external
                  (nth-value 1 (find-symbol name "HTTP-MESSAGE-KIT"))))
  (dolist (name '("HTTP-REQUEST-SUMMARY" "HTTP-RESPONSE-SUMMARY"
                  "SERIALIZE-HTTP-REQUEST" "SERIALIZE-HTTP-RESPONSE"
                  "PARSE-HTTP-REQUEST" "PARSE-HTTP-RESPONSE"))
    (ensure-equal :external
                  (nth-value 1 (find-symbol name "HTTP-KIT"))))
  (ensure-false (eq :external
                    (nth-value 1 (find-symbol "QPACK-ENCODER" "HTTP-KIT")))))
(deftest p4a-uri-and-request-target-validation-boundaries
  (let ((uri (make-http-uri :scheme 'https
                            :authority "example.test"
                            :path "/resource")))
    (ensure-equal "https" (http-uri-scheme uri)))
  (let ((request (make-http-request
                  :method "GET"
                  :uri "http://example.test/"
                  :request-target "not-origin")))
    (ensure-equal "not-origin" (http-request-target request))
    (signals http-protocol-error
      (serialize-http-request request)))
  (signals http-protocol-error
    (make-http-request :method "GET"
                       :uri "http://example.test/"
                       :request-target "/contains space")))
(deftest p4a-header-raw-accessor-and-message-revalidation
  (let ((header (make-http-header "X-Test" "safe")))
    (unwind-protect
         (progn
           (ensure-equal 7 (setf (http-header-content header) 7))
           (ensure-equal 7 (http-header-content header))
           (setf (http-header-content header) "safe")
           (let ((request (make-http-request
                           :method "GET"
                           :uri "http://example.test/"
                           :headers (list header))))
             (setf (http-header-content header) "changed")
             (ensure-equal "safe"
                           (http-header-content
                            (first (http-request-headers request)))))
           (setf (http-header-content header) 7)
           (signals http-invalid-header
             (make-http-request :method "GET"
                                :uri "http://example.test/"
                                :headers (list header))))
      (setf (http-header-content header) "safe")))
)
(deftest p4a-bodyless-response-validation-at-serialization
  (dolist (status '(100 204 304))
    (let ((body-response (make-http-response :status status
                                             :body (octets 65)))
          (trailer-response (make-http-response
                             :status status
                             :trailers (list (make-http-header "X-End" "done")))))
      (ensure-true (http-response-p body-response))
      (ensure-true (http-response-p trailer-response))
      (signals http-protocol-error
        (serialize-http-response body-response))
      (signals http-protocol-error
        (serialize-http-response trailer-response)))))
(deftest p4a-prohibited-trailer-rejection-at-http1-serialization
  (let ((request (make-http-request
                  :method "GET"
                  :uri "http://example.test/"
                  :headers (list (make-http-header "Transfer-Encoding" "chunked"))
                  :trailers (list (make-http-header "Content-Length" "1"))
                  :body (octets 65)))
        (response (make-http-response
                   :status 200
                   :headers (list (make-http-header "Transfer-Encoding" "chunked"))
                   :trailers (list (make-http-header "Content-Length" "1"))
                   :body (octets 65))))
    (ensure-true (http-request-p request))
    (ensure-true (http-response-p response))
    (signals http-invalid-header
      (serialize-http-request request))
    (signals http-invalid-header
      (serialize-http-response response))))
(deftest p4a-response-stream-body-function-identity
  (let* ((producer (lambda () (octets 1 2 3)))
         (response (make-http-response-stream :status 200
                                               :body-function producer
                                               :body-length 3)))
    (ensure-true (eq producer (http-response-stream-body-function response)))
    (ensure-equal (octets 1 2 3)
                  (funcall (http-response-stream-body-function response))))
)