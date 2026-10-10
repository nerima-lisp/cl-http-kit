(in-package #:http-kit/test-core)

(deftest http1-input-and-line-boundaries
  (let* ((wire (concatenate-octets
                (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|")
                (octets 1 2)))
         (response (parse-http-response (coerce wire 'list))))
    (ensure-equal 200 (http-response-status response) "list input status")
    (ensure-equal (octets 1 2)
                  (http-response-body response)
                  "list input body"))
  (signals http-protocol-error
    (parse-http-response "HTTP/1.1 200 OK"))
  (signals http-protocol-error
    (parse-http-response (make-array '(1 1)
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0)))
  (signals http-protocol-error
    (parse-http-response '(1 :bad)))
  (signals http-protocol-error
    (parse-http-response (ascii "HTTP/1.1 200 OK")))
  (signals http-protocol-error
    (parse-http-response
     (concatenate-octets (ascii "HTTP/1.1 200 OK") (octets 10))))
  (signals http-protocol-error
    (parse-http-response
     (concatenate-octets (ascii "HTTP/1.1 200 OK") (octets 13) (ascii "X"))))
  (signals http-protocol-error
    (parse-http-response
     (concatenate-octets (ascii "HTTP/1.1 200 OK") (octets 1))))
  (signals http-invalid-status
    (parse-http-response (ascii "HTTP/1.1 600 Error|CRLF||CRLF|")))
  (let ((response
          (parse-http-response (ascii "HTTP/1.0 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|"))))
    (ensure-equal "HTTP/1.0"
                  (http-response-protocol-version response)
                  "HTTP/1.0 response version")
    (ensure-equal 200 (http-response-status response)
                  "HTTP/1.0 response status"))
  (signals http-invalid-status
    (parse-http-response (ascii "HTTP/1.1X200 OK|CRLF||CRLF|")))
  (let ((response
          (parse-http-response
           (ascii "HTTP/1.1 200 |CRLF||CRLF|"))))
    (ensure-equal "" (http-response-reason response))))

(deftest http1-head-and-304-metadata-length-budget
  (dolist (case '(("HEAD" 200) ("GET" 304)))
    (let ((response
            (parse-http-response
             (ascii (format nil "HTTP/1.1 ~D OK|CRLF|Content-Length: 100|CRLF||CRLF|"
                            (second case)))
             :request-method (first case)
             :max-body-bytes 0)))
      (ensure-equal 0 (length (http-response-body response)))
      (ensure-equal "100" (http-header-value
                           (http-response-headers response) "Content-Length"))))
  (signals http-size-limit-exceeded
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 100|CRLF||CRLF|")
     :max-body-bytes 0))
  (dolist (headers '("Content-Length: nope"
                     "Content-Length: 100|CRLF|Content-Length: 101"))
    (signals http-invalid-header
      (parse-http-response
       (ascii (format nil "HTTP/1.1 200 OK|CRLF|~A|CRLF||CRLF|" headers))
       :request-method "HEAD"
       :max-body-bytes 0))))

(deftest http1-chunked-payload-and-framing-budgets
  (let* ((payload (make-string 80 :initial-element #\a))
         (prefix (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|"))
         (wire (concatenate-octets
                prefix (ascii (format nil "50|CRLF|~A|CRLF|0|CRLF||CRLF|" payload))))
         (header-limit (+ (length prefix) 11)))
    (ensure-equal payload
                  (octets-as-string
                   (http-response-body
                    (parse-http-response
                     (ascii (format nil "HTTP/1.1 200 OK|CRLF|Content-Length: 80|CRLF||CRLF|~A"
                                    payload))
                     :max-header-bytes header-limit :max-body-bytes 80))))
    (ensure-equal payload
                  (octets-as-string
                   (http-response-body
                    (parse-http-response wire :max-header-bytes header-limit
                                              :max-body-bytes 80))))
    (let ((received 0))
      (ensure-equal 0
                    (length (http-response-body
                             (parse-http-response
                              wire :max-header-bytes header-limit :max-body-bytes 80
                              :collect-body-p nil
                              :on-body-chunk (lambda (chunk)
                                               (incf received (length chunk)))))))
      (ensure-equal 80 received))
    (signals http-size-limit-exceeded
      (parse-http-response wire :max-header-bytes (1- header-limit)
                               :max-body-bytes 80))
    (signals http-size-limit-exceeded
      (parse-http-response wire :max-header-bytes header-limit :max-body-bytes 79))
    (dolist (suffix '("1;long-extension=value|CRLF|a|CRLF|0|CRLF||CRLF|"
                      "1|CRLF|a|CRLF|0|CRLF|X-Trailer: long-value|CRLF||CRLF|"))
      (signals http-size-limit-exceeded
        (parse-http-response (concatenate-octets prefix (ascii suffix))
                             :max-header-bytes header-limit :max-body-bytes 80)))))

(deftest http1-head-205-chunked-metadata
  (dolist (status '(200 205))
    (let ((response
            (parse-http-response
             (ascii (format nil "HTTP/1.1 ~D OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|"
                            status))
             :request-method "HEAD" :max-body-bytes 0
             :on-body-chunk (lambda (chunk)
                              (declare (ignore chunk))
                              (error "A HEAD response must not deliver a body.")))))
      (ensure-equal status (http-response-status response))
      (ensure-equal 0 (length (http-response-body response)))
      (ensure-equal nil (http-response-trailers response)))))

(deftest http1-body-and-trailer-boundaries
  (let ((response
          (parse-http-response
           (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1 ; foo = \"bar\\\"baz\";flag|CRLF|a|CRLF|0|CRLF||CRLF|"))))
    (ensure-equal "a" (octets-as-string (http-response-body response))
                  "chunk extensions are ignored"))
  (dolist (chunk-line '("1;" "1;=value" "1;name=" "1;name=\"unterminated"
                        "1;name=bad value" "1 " "1;na(me=value"))
    (signals http-protocol-error
      (parse-http-response
       (ascii (format nil
                      "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|~A|CRLF|a|CRLF|0|CRLF||CRLF|"
                      chunk-line)))))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 1|CRLF||CRLF|")))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|Z|CRLF|0|CRLF||CRLF|")))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF||CRLF|")))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|2|CRLF|a")))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1|CRLF|a")))
  (signals http-size-limit-exceeded
    (parse-http-response
     (ascii (format nil
                    "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|~A|CRLF|"
                    (make-string 10000 :initial-element #\f)))
     :max-body-bytes 0))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|0|CRLF|Broken|CRLF||CRLF|")))
  (dolist (name '("Authorization" "If-None-Match" "Content-Type"
                  "Cache-Control" "Set-Cookie"))
    (signals http-invalid-header
      (parse-http-response
       (ascii (format nil
                      "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|0|CRLF|~A: forbidden|CRLF||CRLF|"
                      name)))))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|1")))
  (let ((response
          (parse-http-response
           (ascii "HTTP/1.1 101 Switching Protocols|CRLF|Connection: Upgrade|CRLF|Upgrade: websocket|CRLF||CRLF|"))))
    (ensure-equal 101 (http-response-status response)
                  "101 status")
    (ensure-equal "HTTP/1.1"
                  (http-response-protocol-version response)
                  "101 response version")
    (ensure-equal (octets) (http-response-body response)
                  "101 response body"))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 101 Switching Protocols|CRLF|Content-Length: 1|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 101 Switching Protocols|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 101 Switching Protocols|CRLF|Content-Length: 0|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 100 Continue|CRLF|Content-Length: 0|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 103 Early Hints|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 204 No Content|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|0|CRLF||CRLF|")))
  (let ((response
          (parse-http-response
           (ascii "HTTP/1.1 205 Reset Content|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|0|CRLF||CRLF|"))))
    (ensure-equal 205 (http-response-status response))
    (ensure-equal 0 (length (http-response-body response))))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 205 Reset Content|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1|CRLF|a|CRLF|0|CRLF||CRLF|")))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 205 Reset Content|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1|CRLF|a|CRLF|0|CRLF||CRLF|")
     :collect-body-p nil))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 200 Connection Established|CRLF|Content-Length: 0|CRLF||CRLF|")
     :request-method "CONNECT"))
  (let ((response
          (parse-http-response
           (ascii "HTTP/1.1 304 Not Modified|CRLF|Content-Length: 1|CRLF||CRLF|"))))
    (ensure-equal 304 (http-response-status response)
                  "304 may describe the selected representation length")))

(deftest http1-framing-and-header-error-boundaries
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1|CRLF|aX")))
  (signals http-protocol-error
    (parse-http-response
     (concatenate-octets
      (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1|CRLF|a")
      (octets 13)
      (ascii "X"))))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF| Content: folded|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked,,gzip|CRLF||CRLF|")))
  (signals http-unsupported-feature
    (parse-http-response
     (ascii "HTTP/1.0 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: nope|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 1|CRLF|Content-Length: 2|CRLF||CRLF|")))
  (let ((response
          (parse-http-response
           (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 16|CRLF||CRLF|0123456789abcdef")
           :max-body-bytes 16)))
    (ensure-equal 16 (length (http-response-body response))))
  (signals http-size-limit-exceeded
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 17|CRLF||CRLF|")
     :max-body-bytes 16))
  (signals http-size-limit-exceeded
    (parse-http-response
     (ascii (format nil
                    "HTTP/1.1 200 OK|CRLF|Content-Length: ~A|CRLF||CRLF|"
                    (make-string 1000 :initial-element #\9)))
     :max-body-bytes 16))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF||CRLF|")
     :max-body-bytes -1))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF||CRLF|")
     :max-header-bytes "invalid"))
  (let ((response
          (parse-http-response
           (ascii "HTTP/1.1 200 OK|CRLF|X-One: 1|CRLF|X-Two: 2|CRLF||CRLF|")
           :max-fields 2)))
    (ensure-equal 2 (length (http-response-headers response))))
  (let ((response
          (parse-http-response
           (ascii "HTTP/1.1 103 Early Hints|CRLF|Link: </one>|CRLF|Link: </two>|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|X-One: 1|CRLF|X-Two: 2|CRLF||CRLF|")
           :max-fields 2)))
    (ensure-equal 200 (http-response-status response))
    (ensure-equal 2 (length (http-response-headers response))))
  (signals http-size-limit-exceeded
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|X-One: 1|CRLF|X-Two: 2|CRLF||CRLF|")
     :max-fields 1))
  (signals http-size-limit-exceeded
    (parse-http-response
     (ascii "HTTP/1.1 103 Early Hints|CRLF|Link: </one>|CRLF|Link: </two>|CRLF|Link: </three>|CRLF||CRLF|HTTP/1.1 200 OK|CRLF||CRLF|")
     :max-fields 2))
  (signals http-size-limit-exceeded
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|0|CRLF|X-One: 1|CRLF|X-Two: 2|CRLF||CRLF|")
     :max-fields 1))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF||CRLF|")
     :max-fields 0))
  (signals http-protocol-error
    (parse-http-response
     (ascii "HTTP/1.1 200 OK|CRLF||CRLF|")
     :max-body-bytes "invalid"))
  (signals http-invalid-header
    (http-kit::%parse-response-header-line "")))

(deftest http1-response-option-rejects-nonfunction-body-callback
  (let ((condition
          (handler-case
              (parse-http-response (octets) :on-body-chunk :invalid)
            (http-protocol-error (condition) condition))))
    (ensure-true (typep condition 'http-protocol-error))
    (ensure-equal :response-parse (http-error-operation condition))
    (ensure-equal :invalid (http-protocol-error-detail condition))))

(deftest http1-response-option-rejects-nonfunction-information-callback
  (let ((condition
          (handler-case
              (parse-http-response (octets) :on-information 42)
            (http-protocol-error (condition) condition))))
    (ensure-true (typep condition 'http-protocol-error))
    (ensure-equal :response-parse (http-error-operation condition))
    (ensure-equal 42 (http-protocol-error-detail condition))))

(deftest http1-response-option-rejects-nonboolean-collection
  (let ((condition
          (handler-case
              (parse-http-response (octets) :collect-body-p 1)
            (http-protocol-error (condition) condition))))
    (ensure-true (typep condition 'http-protocol-error))
    (ensure-equal :response-parse (http-error-operation condition))
    (ensure-equal 1 (http-protocol-error-detail condition))))

(deftest http1-response-option-callbacks-with-and-without-collection
  (dolist (collect-body-p '(nil t))
    (let* ((chunks '())
           (information '())
           (response
             (parse-http-response
              (ascii "HTTP/1.1 103 Early Hints|CRLF|Link: </style.css>|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|abc")
              :collect-body-p collect-body-p
              :on-body-chunk (lambda (chunk) (push chunk chunks))
              :on-information (lambda (response) (push response information)))))
      (ensure-equal 200 (http-response-status response))
      (ensure-octets-equal (if collect-body-p (ascii "abc") (octets))
                           (http-response-body response))
      (ensure-equal 1 (length chunks))
      (ensure-octets-equal (ascii "abc") (first chunks))
      (ensure-equal 1 (length information))
      (ensure-equal 103 (http-response-status (first information)))
      (ensure-equal '("</style.css>")
                    (http-header-values
                     (http-response-headers (first information)) "link"))
      (ensure-equal 0 (length (http-response-body (first information)))))))

(deftest http1-persistence-boundaries
  (let ((request (make-http-request :method "GET" :uri "http://127.0.0.1/")))
    (ensure-true
     (http-response-reusable-p
      request
      (parse-http-response
       (ascii "HTTP/1.0 200 OK|CRLF|Content-Length: 0|CRLF|Connection: keep-alive|CRLF||CRLF|")))
     "HTTP/1.0 keep-alive is reusable")
    (ensure-true
     (not
      (http-response-reusable-p
       request
       (parse-http-response
        (ascii "HTTP/1.0 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|"))))
     "HTTP/1.0 without keep-alive is not reusable")
    (ensure-true
     (http-response-reusable-p
      request
      (parse-http-response
       (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|")))
     "HTTP/1.1 self-delimited response is reusable")
    (ensure-true
     (not
      (http-response-reusable-p
       request
       (parse-http-response
        (ascii "HTTP/1.1 101 Switching Protocols|CRLF||CRLF|"))))
     "101 switches cannot be reused as HTTP responses")))
