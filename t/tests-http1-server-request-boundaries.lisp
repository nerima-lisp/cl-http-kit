(in-package #:http-kit/test-core)

(deftest http1-request-target-boundaries
  (with-http1-request
      (request
       (ascii "POST /upload?x=1 HTTP/1.1|CRLF|Host: Example.COM:80|CRLF|Content-Length: 2|CRLF||CRLF|ab"))
    (ensure-equal "POST" (http-request-method request))
    (ensure-equal "/upload?x=1" (http-request-target request))
    (ensure-equal "example.com" (http-uri-host (http-request-uri request)))
    (ensure-equal 80 (http-uri-port (http-request-uri request)))
    (ensure-equal "/upload" (http-request-path request))
    (ensure-equal "x=1" (http-request-query request))
    (ensure-equal "ab" (octets-as-string (http-request-body request))))
  (with-http1-request
      (request
       (ascii "OPTIONS * HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|"))
    (ensure-equal "*" (http-request-target request))
    (ensure-equal "/" (http-request-path request)))
  (with-http1-request
      (request
       (ascii "CONNECT example.com:443 HTTP/1.1|CRLF|Host: example.com:443|CRLF||CRLF|"))
    (ensure-equal "example.com:443" (http-request-target request))
    (ensure-equal "example.com" (http-uri-host (http-request-uri request)))
    (ensure-equal 443 (http-uri-port (http-request-uri request))))
  (with-http1-request
      (request
       (ascii "GET https://Example.COM/resource HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|"))
    (ensure-equal "https" (http-uri-scheme (http-request-uri request)))
    (ensure-equal "example.com" (http-uri-host (http-request-uri request)))
    (ensure-equal "/resource" (http-request-path request)))
  (with-http1-request
      (request
       (ascii "GET /legacy HTTP/1.0|CRLF||CRLF|")
       :default-authority "legacy.example:80")
    (ensure-equal "legacy.example" (http-uri-host (http-request-uri request)))
    (ensure-equal 80 (http-uri-port (http-request-uri request))))
  (signals http-invalid-header
    (parse-http-request
     (ascii "GET / HTTP/1.1|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-request
     (ascii "GET / HTTP/1.1|CRLF|Host: one.example|CRLF|Host: two.example|CRLF||CRLF|")))
  (signals http-protocol-error
    (parse-http-request
     (ascii "GET * HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|"))))

(deftest http1-request-body-and-trailer-boundaries
  (with-http1-request
      (request
       (ascii "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|4|CRLF|Wiki|CRLF|5;name=value|CRLF|pedia|CRLF|0|CRLF|X-Checksum: ok|CRLF||CRLF|"))
    (ensure-equal "Wikipedia" (octets-as-string (http-request-body request)))
    (ensure-equal "ok"
                  (http-header-value (http-request-trailers request) "x-checksum")))
  (let* ((chunks '())
         (request nil))
    (with-http1-request
        (parsed-request
         (ascii "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Content-Length: 3|CRLF||CRLF|abc")
         :collect-body-p nil
         :on-body-chunk (lambda (chunk)
                          (push (octets-as-string chunk) chunks)))
      (setf request parsed-request))
    (ensure-equal (octets) (http-request-body request))
    (ensure-equal '("abc") (nreverse chunks)))
  (signals http-invalid-header
    (parse-http-request
     (ascii "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1|CRLF|a|CRLF|0|CRLF|Content-Length: 1|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-request
     (ascii "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Content-Length: 1|CRLF|Content-Length: 2|CRLF||CRLF|a")))
  (signals http-unsupported-feature
    (parse-http-request
     (ascii "POST / HTTP/1.0|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|0|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-request
     (ascii "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1|CRLF|a|CRLF|0|CRLF|Content-Length: 1|CRLF||CRLF|")))
  (signals http-invalid-header
    (parse-http-request
     (ascii "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|0|CRLF|Host: forbidden|CRLF||CRLF|"))))

(deftest http1-request-framing-error-boundaries
  (signals http-protocol-error
    (parse-http-request
     (ascii "GET / HTTP/1.1|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|Z|CRLF|0|CRLF||CRLF|")))
  (signals http-protocol-error
    (parse-http-request
     (ascii "GET / HTTP/1.1|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|1|CRLF|aX")))
  (signals http-size-limit-exceeded
    (parse-http-request
     (ascii "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Content-Length: 2|CRLF||CRLF|ab")
     :max-body-bytes 1))
  (signals http-size-limit-exceeded
    (parse-http-request
     (ascii (format nil
                    "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Content-Length: ~A|CRLF||CRLF|"
                    (make-string 10000 :initial-element #\9)))
     :max-body-bytes 0))
  (signals http-size-limit-exceeded
    (parse-http-request
     (ascii (format nil
                    "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|~A|CRLF|"
                    (make-string 10000 :initial-element #\f)))
     :max-body-bytes 0))
  (signals http-protocol-error
    (parse-http-request
     (ascii "GET / HTTP/1.1|CRLF| Host: folded|CRLF||CRLF|")))
  (signals http-protocol-error
    (parse-http-request
     (ascii "GET / HTTP/1.1 extra|CRLF|Host: example.com|CRLF||CRLF|"))))

(deftest http1-request-expect-continue-hook
  (let ((observed nil))
    (with-http1-request
        (request
         (ascii "POST /upload HTTP/1.1|CRLF|Host: example.com|CRLF|Expect: 100-continue|CRLF|Content-Length: 3|CRLF||CRLF|abc")
         :on-expect-continue
         (lambda (metadata)
           (setf observed metadata)
           (ensure-equal "POST" (http-request-method metadata))
           (ensure-equal "/upload" (http-request-target metadata))
           (ensure-equal 0 (length (http-request-body metadata)))))
      (ensure-true observed)
      (ensure-equal "abc" (octets-as-string (http-request-body request))))))

(deftest http1-request-unsupported-expectation
  (let ((caught nil))
    (handler-case
        (parse-http-request
         (ascii "POST /upload HTTP/1.1|CRLF|Host: example.com|CRLF|Expect: 102-processing|CRLF|Content-Length: 3|CRLF||CRLF|abc"))
      (http-unsupported-feature (condition)
        (setf caught condition)))
    (ensure-true caught)
    (ensure-equal :http1-expectation
                  (http-unsupported-feature-name caught))))

(deftest http1-request-ambiguous-framing-rejected-before-body
  (dolist (case '(("Content-Length: " "content-length" :value)
                  ("Content-Length: +1" "content-length" :value)
                  ("Content-Length: -1" "content-length" :value)
                  ("Content-Length: 1, 1" "content-length" :value)
                  ("Content-Length: 1x" "content-length" :value)
                  ("Transfer-Encoding: " "transfer-encoding" :empty-item)
                  ("Transfer-Encoding: ,chunked" "transfer-encoding" :empty-item)
                  ("Transfer-Encoding: chunked," "transfer-encoding" :empty-item)
                  ("Transfer-Encoding: chunked,,gzip" "transfer-encoding" :empty-item)
                  ("Transfer-Encoding: chunked|CRLF|Content-Length: 0"
                   "content-length" :framing-conflict)
                  ("Transfer-Encoding: chunked|CRLF|Content-Length: 1"
                   "content-length" :framing-conflict)))
    (destructuring-bind (fields name reason) case
      (let ((caught nil)
            (body-calls 0)
            (continue-calls 0))
        (handler-case
            (parse-http-request
             (ascii (format nil
                            "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|~A|CRLF|Expect: 100-continue|CRLF||CRLF|"
                            fields))
             :on-body-chunk (lambda (chunk)
                              (declare (ignore chunk))
                              (incf body-calls))
             :on-expect-continue (lambda (request)
                                   (declare (ignore request))
                                   (incf continue-calls)))
          (http-invalid-header (condition)
            (setf caught condition)))
        (ensure-true caught)
        (ensure-equal :request-parse (http-error-operation caught))
        (ensure-equal name (http-invalid-header-name caught))
        (ensure-equal reason (http-invalid-header-reason caught))
        (ensure-equal 0 body-calls)
        (ensure-equal 0 continue-calls))))
  (with-http1-request
      (request
       (ascii "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Content-Length: 01|CRLF|Content-Length: 1|CRLF||CRLF|a"))
    (ensure-equal "a" (octets-as-string (http-request-body request))))
  (with-http1-request
      (request
       (ascii "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Transfer-Encoding: ChUnKeD|CRLF||CRLF|1|CRLF|a|CRLF|0|CRLF||CRLF|"))
    (ensure-equal "a" (octets-as-string (http-request-body request)))))

(deftest http1-request-unsupported-codings-and-expectations
  (dolist (case '(("Transfer-Encoding: gzip" :http1-request-transfer-encoding)
                  ("Transfer-Encoding: gzip, chunked" :http1-request-transfer-encoding)
                  ("Transfer-Encoding: chunked, chunked" :http1-request-transfer-encoding)
                  ("Transfer-Encoding: chunked|CRLF|Transfer-Encoding: chunked"
                   :http1-request-transfer-encoding)
                  ("Expect: 100-continue, 102-processing" :http1-expectation)))
    (destructuring-bind (fields feature) case
      (let ((caught nil)
            (continue-calls 0))
        (handler-case
            (parse-http-request
             (ascii (format nil
                            "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|~A|CRLF|Content-Length: 1|CRLF||CRLF|"
                            fields))
             :on-expect-continue (lambda (request)
                                   (declare (ignore request))
                                   (incf continue-calls)))
          (http-unsupported-feature (condition)
            (setf caught condition)))
        (ensure-true caught)
        (ensure-equal :request-parse (http-error-operation caught))
        (ensure-equal feature (http-unsupported-feature-name caught))
        (ensure-equal 0 continue-calls))))
  (dolist (value '("" ",100-continue" "100-continue,"))
    (let ((caught nil))
      (handler-case
          (parse-http-request
           (ascii (format nil
                          "POST / HTTP/1.1|CRLF|Host: example.com|CRLF|Expect: ~A|CRLF|Content-Length: 1|CRLF||CRLF|"
                          value)))
        (http-invalid-header (condition)
          (setf caught condition)))
      (ensure-true caught)
      (ensure-equal "expect" (http-invalid-header-name caught))
      (ensure-equal :empty-item (http-invalid-header-reason caught)))))

(deftest http1-request-connect-authority-boundaries
  (dolist (target '("/" "*" "http://example.com:443/"))
    (signals http-protocol-error
      (parse-http-request
       (ascii (format nil
                      "CONNECT ~A HTTP/1.1|CRLF|Host: example.com:443|CRLF||CRLF|"
                      target)))))
  (signals http-protocol-error
    (parse-http-request
     (ascii "CONNECT example.com HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|")))
  (dolist (host '("other.example:443" "example.com:80"))
    (let ((caught nil))
      (handler-case
          (parse-http-request
           (ascii (format nil
                          "CONNECT example.com:443 HTTP/1.1|CRLF|Host: ~A|CRLF||CRLF|"
                          host)))
        (http-invalid-header (condition)
          (setf caught condition)))
      (ensure-true caught)
      (ensure-equal :request-parse (http-error-operation caught))
      (ensure-equal "host" (http-invalid-header-name caught))
      (ensure-equal :host-authority-mismatch
                    (http-invalid-header-reason caught))))
  (with-http1-request
      (request
       (ascii "CONNECT Example.COM:80 HTTP/1.1|CRLF|Host: example.com|CRLF||CRLF|"))
    (ensure-equal "example.com" (http-uri-host (http-request-uri request)))
    (ensure-equal 80 (http-uri-port (http-request-uri request))))
  (with-http1-request
      (request
       (ascii "CONNECT [::1]:443 HTTP/1.1|CRLF|Host: [::1]:443|CRLF||CRLF|"))
    (ensure-equal "::1" (http-uri-host (http-request-uri request)))
    (ensure-equal 443 (http-uri-port (http-request-uri request)))))

(deftest core-coverage-http1-request-line-error-metadata
  (dolist (case '(("G/ET / HTTP/1.1" "G/ET")
                  ("GET  HTTP/1.1" "")
                  ("GET / HTTP/9" "HTTP/9")))
    (destructuring-bind (line detail) case
      (let ((condition
              (handler-case
                  (parse-http-request
                   (ascii (format nil "~A|CRLF|Host: example.test|CRLF||CRLF|" line)))
                (http-protocol-error (condition) condition))))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :request-parse (http-error-operation condition))
        (ensure-equal detail (http-protocol-error-detail condition))))))

(deftest core-coverage-http1-request-option-errors
  (dolist (option '(:max-header-bytes :max-fields :max-body-bytes
                    :on-body-chunk :on-expect-continue :collect-body-p :allow-eof-p))
    (let* ((value (if (eq option :max-body-bytes) -1 0))
           (condition
             (handler-case
                 (apply #'parse-http-request (octets) (list option value))
               (http-protocol-error (condition) condition))))
      (ensure-true (typep condition 'http-protocol-error))
      (ensure-equal :request-parse (http-error-operation condition))
      (ensure-equal value (http-protocol-error-detail condition))))
  (ensure-equal nil (parse-http-request (octets) :allow-eof-p t)))

(deftest core-coverage-http1-request-authority-errors
  (dolist (case '(("GET / HTTP/1.0|CRLF|Host: example.test:bad|CRLF||CRLF|"
                  nil "host" "example.test:bad")
                 ("GET / HTTP/1.0|CRLF||CRLF|"
                  "example.test:bad" "default-authority" "example.test:bad")
                 ("CONNECT example.test:bad HTTP/1.1|CRLF|Host: example.test|CRLF||CRLF|"
                  nil "request-target" "example.test:bad")))
    (destructuring-bind (wire default-authority name authority) case
      (let ((condition
              (handler-case
                  (parse-http-request (ascii wire) :default-authority default-authority)
                (http-invalid-header (condition) condition))))
        (ensure-true (typep condition 'http-invalid-header))
        (ensure-equal :request-parse (http-error-operation condition))
        (ensure-equal name (http-invalid-header-name condition))
        (ensure-equal :value (http-invalid-header-reason condition))
        (let ((detail (http-protocol-error-detail condition)))
          (ensure-equal 2 (length detail))
          (ensure-equal authority (first detail))
          (ensure-true (and (stringp (second detail)) (plusp (length (second detail)))))))))
  (dolist (case '(("GET / HTTP/1.0" nil "/")
                  ("OPTIONS * HTTP/1.0" nil "*")
                  ("GET relative HTTP/1.0" nil "relative")
                  ("GET / HTTP/1.0" 7 7)))
    (destructuring-bind (line default-authority detail) case
      (let ((condition
              (handler-case
                  (parse-http-request
                   (ascii (format nil "~A|CRLF||CRLF|" line))
                   :default-authority default-authority)
                (http-protocol-error (condition) condition))))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :request-parse (http-error-operation condition))
        (ensure-equal detail (http-protocol-error-detail condition)))))
  (let* ((target "https://example.test/resource")
         (condition
           (handler-case
               (parse-http-request
                (ascii (format nil "GET ~A HTTP/1.1|CRLF|Host: other.test|CRLF||CRLF|" target)))
             (http-invalid-header (condition) condition))))
    (ensure-true (typep condition 'http-invalid-header))
    (ensure-equal :request-parse (http-error-operation condition))
    (ensure-equal "host" (http-invalid-header-name condition))
    (ensure-equal :host-authority-mismatch (http-invalid-header-reason condition))
    (ensure-equal target (http-protocol-error-detail condition))))

(deftest core-coverage-http1-request-header-error-metadata
  (dolist (case '(("Broken" nil :missing-colon "Broken")
                  ("Bad Name: value" "Bad Name" :name nil)))
    (destructuring-bind (line name reason detail) case
      (let ((condition
              (handler-case
                  (parse-http-request
                   (ascii (format nil "GET / HTTP/1.1|CRLF|Host: example.test|CRLF|~A|CRLF||CRLF|" line)))
                (http-invalid-header (condition) condition))))
        (ensure-true (typep condition 'http-invalid-header))
        (ensure-equal :request-parse (http-error-operation condition))
        (ensure-equal name (http-invalid-header-name condition))
        (ensure-equal reason (http-invalid-header-reason condition))
        (ensure-equal detail (http-protocol-error-detail condition))))))
