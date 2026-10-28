(in-package #:http-kit/test-core)

(defun http1-request-for-test (&key headers body trailers)
  (make-http-request :method "POST"
                     :uri "http://127.0.0.1/upload"
                     :headers headers
                     :trailers trailers
                     :body (or body (octets))))

(deftest http1-absolute-request-target-authority-boundaries
  (labels ((request (&key (uri "http://example.com/resource")
                         request-target headers)
             (make-http-request :method "GET"
                                :uri uri
                                :request-target request-target
                                :headers headers))
           (ensure-authority-mismatch (thunk)
             (handler-case
                 (progn
                   (funcall thunk)
                   (ensure-true nil "authority mismatch was accepted"))
               (http-invalid-header (condition)
                 (ensure-equal "host" (http-invalid-header-name condition))
                 (ensure-equal :host-authority-mismatch
                               (http-invalid-header-reason condition))))))
    (ensure-authority-mismatch
     (lambda ()
       (serialize-http-request
        (request :request-target "http://other.example/resource"))))
    (ensure-authority-mismatch
     (lambda ()
       (serialize-http-request
        (request :request-target "http://example.com:81/resource"))))
    (ensure-authority-mismatch
     (lambda ()
       (serialize-http-request
        (request)
        :request-target "http://other.example/resource")))
    (ensure-true
     (search "GET http://example.com/resource HTTP/1.1"
             (octets-as-string
              (serialize-http-request
               (request :request-target "http://example.com/resource"))))
     "matching request-carried absolute target is serialized")
    (ensure-true
     (search "GET https://example.com:443/resource HTTP/1.1"
             (octets-as-string
              (serialize-http-request
               (request :uri "https://example.com/resource"
                        :headers (list (make-http-header "Host"
                                                        "EXAMPLE.COM:443")))
               :request-target "https://example.com:443/resource")))
     "default HTTPS port and normalized host are equivalent")
    (signals http-invalid-uri
      (serialize-http-request
       (request)
       :request-target "http://example.com:bad/resource"))))

(deftest http1-asterisk-request-target-boundaries
  (ensure-true
   (search "OPTIONS * HTTP/1.1"
           (octets-as-string
            (serialize-http-request
             (make-http-request :method "OPTIONS"
                                :uri "http://example.test/"
                                :request-target "*")))))
  (signals http-protocol-error
    (serialize-http-request
     (make-http-request :method "GET"
                        :uri "http://example.test/"
                        :request-target "*"))))

(deftest http1-request-target-form-boundaries
  (ensure-true
   (search "CONNECT example.test:443 HTTP/1.1"
           (octets-as-string
            (serialize-http-request
             (make-http-request :method "CONNECT"
                                :uri "https://example.test:443/"
                                :request-target "example.test:443")))))
  (dolist (target '("/tunnel" "*" "https://example.test:443/"
                    "example.test"))
    (signals http-protocol-error
      (serialize-http-request
       (make-http-request :method "CONNECT"
                          :uri "https://example.test:443/"
                          :request-target target))))
  (signals http-invalid-header
    (serialize-http-request
     (make-http-request :method "CONNECT"
                        :uri "https://example.test:443/"
                        :request-target "other.example:443")))
  (signals http-protocol-error
    (serialize-http-request
     (make-http-request :method "GET"
                        :uri "http://example.test/"
                        :request-target "example.test:80")))
  (signals http-protocol-error
    (serialize-http-request
     (make-http-request :method "GET"
                        :uri "http://example.test/"
                        :request-target "ftp://example.test/resource"))))

(deftest http1-serialization-request-target-override-boundaries
  (let ((request (http1-request-for-test)))
    (dolist (target (list "" 42 "/with space"
                         (format nil "/with~Ctab" #\Tab)
                         (format nil "/with~Cline" #\Newline)
                         (format nil "/with~Cdelete" (code-char 127))))
      (handler-case
          (progn
            (serialize-http-request request :request-target target)
            (ensure-true nil "invalid request-target override was serialized"))
        (http-protocol-error (condition)
          (ensure-equal :serialization (http-error-operation condition))
          (ensure-equal target (http-protocol-error-detail condition)))))
    (ensure-octets-equal
     (ascii "POST /upload HTTP/1.1|CRLF|Host: 127.0.0.1|CRLF|Content-Length: 0|CRLF||CRLF|")
     (serialize-http-request request :request-target nil))
    (ensure-octets-equal
     (ascii "POST /replacement HTTP/1.1|CRLF|Host: 127.0.0.1|CRLF|Content-Length: 0|CRLF||CRLF|")
     (serialize-http-request request :request-target "/replacement"))))

(deftest http1-serialization-rejects-versioned-protocols
  (dolist (version '("HTTP/2" "HTTP/3"))
    (let ((request (make-http-request :method "POST"
                                      :uri "http://127.0.0.1/upload"
                                      :protocol-version version)))
      (handler-case
          (progn
            (serialize-http-request request)
            (ensure-true nil "versioned protocol was serialized as HTTP/1"))
        (http-unsupported-feature (condition)
          (ensure-equal :serialization (http-error-operation condition))
          (ensure-equal :http1-request-version
                        (http-unsupported-feature-name condition))
          (ensure-equal version (http-protocol-error-detail condition))))))
  (ensure-octets-equal
   (ascii "POST /upload HTTP/1.0|CRLF|Host: 127.0.0.1|CRLF|Content-Length: 0|CRLF||CRLF|")
   (serialize-http-request
    (make-http-request :method "POST"
                       :uri "http://127.0.0.1/upload"
                       :protocol-version "HTTP/1.0"))))

(deftest http1-serialization-rejects-http10-transfer-coding
  (handler-case
      (progn
        (serialize-http-request
         (make-http-request :method "POST"
                            :uri "http://127.0.0.1/upload"
                            :protocol-version "HTTP/1.0"
                            :headers (list (make-http-header "Transfer-Encoding"
                                                            "chunked"))
                            :body (octets 97 98)))
        (ensure-true nil "HTTP/1.0 transfer coding was serialized"))
    (http-unsupported-feature (condition)
      (ensure-equal :serialization (http-error-operation condition))
      (ensure-equal :http1-request-transfer-encoding
                    (http-unsupported-feature-name condition))
      (ensure-equal :chunked (http-protocol-error-detail condition))))
  (ensure-octets-equal
   (ascii "POST /upload HTTP/1.0|CRLF|Host: 127.0.0.1|CRLF|Content-Length: 2|CRLF||CRLF|ab")
   (serialize-http-request
    (make-http-request :method "POST"
                       :uri "http://127.0.0.1/upload"
                       :protocol-version "HTTP/1.0"
                       :body (octets 97 98)))))

(deftest http1-request-framing-boundaries
  (let* ((body (octets 1 2))
         (wire (serialize-http-request
                (http1-request-for-test
                 :headers (list (make-http-header "Host" "127.0.0.1")
                                (make-http-header "Content-Length" "2")
                                (make-http-header "content-length" "2"))
                 :body body)))
         (text (octets-as-string wire)))
    (ensure-true (search "Host: 127.0.0.1" text)
                 "matching Host is serialized")
    (ensure-true (search "Content-Length: 2" text)
                 "first Content-Length is serialized")
    (ensure-true (search "content-length: 2" text)
                 "duplicate Content-Length is serialized"))
  (ensure-serialization-cases
   serialize-http-request
   ((concatenate-octets
     (ascii "POST /upload HTTP/1.1|CRLF|Transfer-Encoding: chunked|CRLF|Host: 127.0.0.1|CRLF||CRLF|2|CRLF|")
     (octets 1 2)
     (ascii "|CRLF|0|CRLF||CRLF|"))
    (http1-request-for-test
     :headers (list (make-http-header "Transfer-Encoding" "chunked"))
     :body (octets 1 2)))
   ((ascii "POST /upload HTTP/1.1|CRLF|transfer-encoding: CHUNKED|CRLF|Host: 127.0.0.1|CRLF||CRLF|0|CRLF||CRLF|")
    (http1-request-for-test
     :headers (list (make-http-header "transfer-encoding" "CHUNKED"))))
   ((ascii "POST /upload HTTP/1.1|CRLF|Transfer-Encoding: chunked|CRLF|Host: 127.0.0.1|CRLF|Trailer: X-Checksum|CRLF||CRLF|2|CRLF|ab|CRLF|0|CRLF|X-Checksum: ok|CRLF||CRLF|")
    (http1-request-for-test
     :headers (list (make-http-header "Transfer-Encoding" "chunked"))
     :body (octets 97 98)
     :trailers (list (make-http-header "X-Checksum" "ok"))))
   ((ascii "POST /upload HTTP/1.1|CRLF|Transfer-Encoding: chunked|CRLF|Trailer: X-Checksum|CRLF|Host: 127.0.0.1|CRLF||CRLF|2|CRLF|ab|CRLF|0|CRLF|X-Checksum: ok|CRLF||CRLF|")
    (http1-request-for-test
     :headers (list (make-http-header "Transfer-Encoding" "chunked")
                    (make-http-header "Trailer" "X-Checksum"))
     :body (octets 97 98)
     :trailers (list (make-http-header "X-Checksum" "ok"))))
  ((ascii "POST /upload HTTP/1.1|CRLF|Transfer-Encoding: chunked|CRLF|Trailer: X-Checksum, x-checksum|CRLF|Host: 127.0.0.1|CRLF||CRLF|1|CRLF|a|CRLF|0|CRLF|X-Checksum: ok|CRLF||CRLF|")
    (http1-request-for-test
     :headers (list (make-http-header "Transfer-Encoding" "chunked")
                    (make-http-header "Trailer" "X-Checksum, x-checksum"))
     :body (octets 97)
     :trailers (list (make-http-header "X-Checksum" "ok")))))
  (signals http-invalid-header
    (serialize-http-request
     (http1-request-for-test
      :headers (list (make-http-header "Transfer-Encoding" "chunked")
                     (make-http-header "Trailer" "X-Other"))
      :body (octets 97)
      :trailers (list (make-http-header "X-Checksum" "ok")))))
  (signals http-invalid-header
    (serialize-http-request
     (http1-request-for-test
      :headers (list (make-http-header "Trailer" "Content-Length"))
      :body (octets 97))))
  (dolist (name '("Authorization" "If-None-Match" "Content-Type"
                  "Cache-Control" "Set-Cookie" "Via"))
    (signals http-invalid-header
      (serialize-http-request
       (http1-request-for-test
        :headers (list (make-http-header "Transfer-Encoding" "chunked"))
        :trailers (list (make-http-header name "forbidden"))))))
  (signals http-unsupported-feature
    (serialize-http-request
     (http1-request-for-test
      :headers (list (make-http-header "Transfer-Encoding" "gzip")))))
  (signals http-unsupported-feature
    (serialize-http-request
     (http1-request-for-test
      :headers (list (make-http-header "Transfer-Encoding" "gzip, chunked")))))
  (signals http-invalid-header
    (serialize-http-request
     (http1-request-for-test
      :headers (list (make-http-header "Transfer-Encoding" "chunked")
                     (make-http-header "Content-Length" "2"))
      :body (octets 1 2))))
  (signals http-invalid-header
    (serialize-http-request
     (http1-request-for-test
      :headers (list (make-http-header "Transfer-Encoding" "chunked,")))))
  (signals http-invalid-header
    (serialize-http-request
     (http1-request-for-test
      :headers (list (make-http-header "Content-Length" "not-a-number")))))
  (signals http-invalid-header
    (serialize-http-request
     (http1-request-for-test
      :headers (list (make-http-header "Content-Length" "2")
                     (make-http-header "content-length" "1"))
      :body (octets 1 2))))
  (signals http-invalid-header
    (serialize-http-request
      (http1-request-for-test
       :headers (list (make-http-header "Content-Length" "1"))
      :body (octets 1 2))))
  (signals http-invalid-header
    (serialize-http-request
     (http1-request-for-test
      :headers (list (make-http-header "Host" "127.0.0.1")
                     (make-http-header "host" "127.0.0.1"))))))

(deftest core-coverage-http1-trailer-declaration-errors
  (dolist (case '(("" "trailer" :empty-item)
                  ("X-One, " "trailer" :empty-item)
                  ("bad name" "trailer" :value)
                  ("Content-Length" "Content-Length" :forbidden-trailer)))
    (destructuring-bind (value name reason) case
      (let ((condition
              (handler-case
                  (serialize-http-request
                   (http1-request-for-test
                    :headers (list (make-http-header "Transfer-Encoding" "chunked")
                                   (make-http-header "Trailer" value))))
                (http-invalid-header (condition) condition))))
        (ensure-true (typep condition 'http-invalid-header))
        (ensure-equal :serialization (http-error-operation condition))
        (ensure-equal name (http-invalid-header-name condition))
        (ensure-equal reason (http-invalid-header-reason condition)))))
  (let ((condition
          (handler-case
              (serialize-http-request
               (http1-request-for-test
                :headers (list (make-http-header "Content-Length" "0")
                               (make-http-header "Trailer" "X-Checksum"))))
            (http-invalid-header (condition) condition))))
    (ensure-true (typep condition 'http-invalid-header))
    (ensure-equal :serialization (http-error-operation condition))
    (ensure-equal "trailer" (http-invalid-header-name condition))
    (ensure-equal :framing (http-invalid-header-reason condition))))

(deftest core-coverage-http1-trailer-declaration-membership
  (let ((condition
          (handler-case
              (serialize-http-request
               (http1-request-for-test
                :headers (list (make-http-header "Trailer" "X-One"))
                :trailers (list (make-http-header "X-Checksum" "done"))))
            (http-invalid-header (condition) condition))))
    (ensure-true (typep condition 'http-invalid-header))
    (ensure-equal :serialization (http-error-operation condition))
    (ensure-equal "trailer" (http-invalid-header-name condition))
    (ensure-equal :undeclared-trailer (http-invalid-header-reason condition))
    (ensure-equal '(:declared ("X-One") :actual ("X-Checksum"))
                  (http-protocol-error-detail condition)))
  (ensure-equal
   (ascii "POST /upload HTTP/1.1|CRLF|Trailer: X-Checksum, x-checksum|CRLF|Host: 127.0.0.1|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|0|CRLF|X-Checksum: done|CRLF||CRLF|")
   (serialize-http-request
    (http1-request-for-test
     :headers (list (make-http-header "Trailer" "X-Checksum, x-checksum"))
     :trailers (list (make-http-header "X-Checksum" "done"))))))

(defun %ensure-http1-quoted-extension-octet (byte escaped-p)
  (let* ((extension (concatenate-octets
                     (ascii ";name=\"")
                     (if escaped-p (octets #x5c) (octets))
                     (octets byte)
                     (ascii "\"")))
         (body (concatenate-octets
                (ascii "2") extension (ascii "|CRLF|ab|CRLF|1|CRLF|c|CRLF|0")
                extension (ascii "|CRLF|X-Checksum: done|CRLF||CRLF|"))))
    (dolist (request-p '(nil t))
      (let* ((header (if request-p
                         (ascii "POST /upload HTTP/1.1|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF|Trailer: X-Checksum|CRLF||CRLF|")
                         (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF|Trailer: X-Checksum|CRLF||CRLF|")))
             (wire (concatenate-octets header body))
             (input (concatenate-octets wire (ascii "NEXT"))))
        (dolist (collect-body-p '(nil t))
          (dolist (stream-p '(nil #+sbcl t))
            (let* ((source (if stream-p
                               #+sbcl (make-instance 'binary-test-stream :input input)
                               #-sbcl input
                               input))
                   (chunks nil)
                   (message (funcall (if request-p
                                         #'parse-http-request
                                         #'parse-http-response)
                                     source
                                     :collect-body-p collect-body-p
                                     :on-body-chunk
                                     (lambda (chunk)
                                       (push (octets-as-string chunk) chunks))))
                   (payload (if request-p
                                (http-request-body message)
                                (http-response-body message)))
                   (trailers (if request-p
                                 (http-request-trailers message)
                                 (http-response-trailers message))))
              (ensure-equal (if collect-body-p "abc" "")
                            (octets-as-string payload))
              (ensure-equal '("ab" "c") (nreverse chunks))
              (ensure-equal 1 (length trailers))
              (ensure-equal "done" (http-header-value trailers "x-checksum"))
              (if request-p
                  (progn
                    (ensure-equal "POST" (http-request-method message))
                    (ensure-equal "/upload" (http-request-target message))
                    (ensure-equal "HTTP/1.1" (http-request-protocol-version message)))
                  (progn
                    (ensure-equal 200 (http-response-status message))
                    (ensure-equal "HTTP/1.1" (http-response-protocol-version message))))
              #+sbcl
              (when stream-p
                (ensure-equal (length wire) (binary-test-input-position source))
                (ensure-equal (char-code #\N) (read-byte source))))))))))

(deftest http1-public-boundary-quoted-extension-80-unescaped
  (%ensure-http1-quoted-extension-octet #x80 nil))

(deftest http1-public-boundary-quoted-extension-ff-unescaped
  (%ensure-http1-quoted-extension-octet #xff nil))

(deftest http1-public-boundary-quoted-extension-80-escaped
  (%ensure-http1-quoted-extension-octet #x80 t))

(deftest http1-public-boundary-quoted-extension-ff-escaped
  (%ensure-http1-quoted-extension-octet #xff t))

(defun %ensure-http1-quoted-extension-line-controls (parser operation header)
  (dolist (byte '(0 1 #x7f))
    (dolist (escaped-p '(nil t))
      (let* ((prefix (concatenate-octets
                      header (ascii "1;name=\"")
                      (if escaped-p (octets #x5c) (octets))))
             (wire (concatenate-octets
                    prefix (octets byte)
                    (ascii "\"|CRLF|a|CRLF|0|CRLF||CRLF|"))))
        (dolist (stream-p '(nil #+sbcl t))
          (let* ((source (if stream-p
                             #+sbcl (make-instance 'binary-test-stream :input wire)
                             #-sbcl wire
                             wire))
                 (body-calls 0)
                 (condition
                   (handler-case
                       (funcall parser source
                                :on-body-chunk
                                (lambda (chunk)
                                  (declare (ignore chunk))
                                  (incf body-calls)))
                     (http-protocol-error (condition) condition))))
            (ensure-true (typep condition 'http-protocol-error))
            (ensure-equal operation (http-error-operation condition))
            (ensure-equal (list :line-control byte)
                          (http-protocol-error-detail condition))
            (ensure-equal 0 body-calls)
            #+sbcl
            (when stream-p
              (ensure-equal (1+ (length prefix))
                            (binary-test-input-position source))
              (ensure-equal (char-code #\") (read-byte source)))))))))

(deftest http1-public-boundary-request-extension-line-controls
  (%ensure-http1-quoted-extension-line-controls
   #'parse-http-request :request-parse
   (ascii "POST /upload HTTP/1.1|CRLF|Host: example.com|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|")))

(deftest http1-public-boundary-response-extension-line-controls
  (%ensure-http1-quoted-extension-line-controls
   #'parse-http-response :response-parse
   (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|")))

(deftest http1-second-wave-informational-transfer-encoding-metadata
  (dolist (stream-p '(nil #+sbcl t))
    (let* ((wire (ascii "HTTP/1.1 103 Early Hints|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|abc"))
           (source (if stream-p
                       #+sbcl (make-instance 'binary-test-stream :input wire)
                       #-sbcl wire
                       wire))
           (information-count 0)
           (body-count 0)
           (failure
             (handler-case
                 (parse-http-response
                  source
                  :on-information (lambda (response)
                                    (declare (ignore response))
                                    (incf information-count))
                  :on-body-chunk (lambda (chunk)
                                   (declare (ignore chunk))
                                   (incf body-count)))
               (http-invalid-header (condition) condition))))
      (ensure-true (typep failure 'http-invalid-header))
      (ensure-equal :response-parse (http-error-operation failure))
      (ensure-equal "transfer-encoding" (http-invalid-header-name failure))
      (ensure-equal :forbidden (http-invalid-header-reason failure))
      (ensure-equal 0 information-count)
      (ensure-equal 0 body-count)
      #+sbcl
      (when stream-p
        (ensure-equal
         (length (ascii "HTTP/1.1 103 Early Hints|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|"))
         (binary-test-input-position source))))
    (let* ((wire (ascii "HTTP/1.1 103 Early Hints|CRLF|Link: </style.css>; rel=preload|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|abc"))
           (source (if stream-p
                       #+sbcl (make-instance 'binary-test-stream :input wire)
                       #-sbcl wire
                       wire))
           (information '())
           (chunks '())
           (response
             (parse-http-response
              source
              :on-information (lambda (interim) (push interim information))
              :on-body-chunk (lambda (chunk) (push chunk chunks)))))
      (ensure-equal 1 (length information))
      (ensure-equal 103 (http-response-status (first information)))
      (ensure-equal "</style.css>; rel=preload"
                    (http-header-value (http-response-headers (first information))
                                       "link"))
      (ensure-octets-equal (octets) (http-response-body (first information)))
      (ensure-equal 200 (http-response-status response))
      (ensure-octets-equal (ascii "abc") (http-response-body response))
      (ensure-equal 1 (length chunks))
      (ensure-octets-equal (ascii "abc") (first chunks))
      #+sbcl
      (when stream-p
        (ensure-equal (length wire) (binary-test-input-position source))))))

#+sbcl
(deftest http1-second-wave-expect-body-copy-independence
  (let* ((original (ascii "abc"))
         (request (make-http-request
                   :method "POST" :uri "http://127.0.0.1/"
                   :headers (list (make-http-header "Expect" "100-continue"))
                   :body original))
         (head (ascii "POST / HTTP/1.1|CRLF|Expect: 100-continue|CRLF|Host: 127.0.0.1|CRLF|Content-Length: 3|CRLF||CRLF|"))
         (wire (ascii "HTTP/1.1 100 Continue|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|"))
         (stream (make-instance 'binary-test-stream :input wire))
         (information-count 0))
    (multiple-value-bind (response reusable)
        (send-http-request-over-open-stream
         request stream
         :on-information
         (lambda (information)
           (incf information-count)
           (ensure-equal 100 (http-response-status information))
           (ensure-octets-equal head (binary-test-output stream))
           (let ((copy (http-request-body request)))
             (replace original (ascii "def"))
             (replace copy (ascii "xyz"))
             (ensure-octets-equal (ascii "def") original)
             (ensure-octets-equal (ascii "xyz") copy)
             (ensure-octets-equal (ascii "abc") (http-request-body request)))))
      (ensure-equal 1 information-count)
      (ensure-equal 200 (http-response-status response))
      (ensure-octets-equal (octets) (http-response-body response))
      (ensure-true reusable)
      (ensure-octets-equal (concatenate-octets head (ascii "abc"))
                          (binary-test-output stream))
      (ensure-octets-equal (ascii "abc") (http-request-body request))
      (ensure-equal (length wire) (binary-test-input-position stream)))))

(deftest http1-second-wave-close-body-chunk-boundary
  (let* ((body (make-array 4097 :element-type '(unsigned-byte 8)))
         (wire nil))
    (dotimes (index (length body))
      (setf (aref body index) (mod index 256)))
    (setf wire
          (concatenate-octets
           (ascii "HTTP/1.1 200 OK|CRLF|Connection: close|CRLF||CRLF|")
           body))
    (dolist (stream-p '(nil #+sbcl t))
      (dolist (collect-p '(t nil))
        (let* ((source (if stream-p
                           #+sbcl (make-instance 'binary-test-stream :input wire)
                           #-sbcl wire
                           wire))
               (chunks '())
               (response
                 (parse-http-response
                  source :collect-body-p collect-p
                  :on-body-chunk (lambda (chunk) (push chunk chunks)))))
          (setf chunks (nreverse chunks))
          (ensure-equal 200 (http-response-status response))
          (ensure-equal '(4096 1) (mapcar #'length chunks))
          (ensure-octets-equal body (apply #'concatenate-octets chunks))
          (ensure-octets-equal (if collect-p body (octets))
                              (http-response-body response))
          (ensure-equal '() (http-response-trailers response))
          #+sbcl
          (when stream-p
            (ensure-equal (length wire) (binary-test-input-position source))
            (ensure-equal :eof (read-byte source nil :eof))))))))

#+sbcl
(deftest http1-public-status-line-short-boundaries
  (dolist (line '("HTTP" "HTTP/1.1 20"))
    (let* ((wire (concatenate-octets (ascii line) (ascii "|CRLF|!")))
           (stream (make-instance 'binary-test-stream :input wire))
           (callback-count 0))
      (unwind-protect
           (let ((caught
                   (handler-case
                       (progn
                         (parse-http-response
                          stream
                          :on-body-chunk (lambda (chunk)
                                           (declare (ignore chunk))
                                           (incf callback-count))
                          :on-information (lambda (response)
                                            (declare (ignore response))
                                            (incf callback-count)))
                         nil)
                     (http-invalid-status (condition) condition))))
             (ensure-true (typep caught 'http-invalid-status))
             (ensure-equal :response-parse (http-error-operation caught))
             (ensure-equal line (http-invalid-status-line caught))
             (ensure-equal nil (http-invalid-status-code caught))
             (ensure-equal 0 callback-count)
             (ensure-equal (+ (length line) 2)
                           (binary-test-input-position stream))
             (ensure-equal (char-code #\!) (read-byte stream))
             (ensure-octets-equal (octets) (binary-test-output stream)))
        (close stream))))
  (let* ((wire (ascii "HTTP/1.1 204|CRLF||CRLF|!"))
         (stream (make-instance 'binary-test-stream :input wire)))
    (unwind-protect
         (let ((response (parse-http-response stream)))
           (ensure-equal 204 (http-response-status response))
           (ensure-equal "" (http-response-reason response))
           (ensure-octets-equal (octets) (http-response-body response))
           (ensure-equal (1- (length wire)) (binary-test-input-position stream))
           (ensure-equal (char-code #\!) (read-byte stream)))
      (close stream))))

#+sbcl
(deftest http1-public-noninteger-parser-limits
  (dolist (request-p '(t nil))
    (let ((wire
            (ascii (if request-p
                       "POST / HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 3|CRLF||CRLF|abc!"
                       "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|abc!")))
          (parser (if request-p #'parse-http-request #'parse-http-response))
          (operation (if request-p :request-parse :response-parse)))
      (dolist (option (if request-p
                          '(:max-header-bytes :max-fields :max-body-bytes)
                          '(:max-fields)))
        (dolist (invalid '("1" 1/2))
          (let ((stream (make-instance 'binary-test-stream :input wire))
                (callback-count 0))
            (unwind-protect
                 (let ((caught
                         (handler-case
                             (progn
                               (apply parser stream
                                      :on-body-chunk
                                      (lambda (chunk)
                                        (declare (ignore chunk))
                                        (incf callback-count))
                                      (list option invalid))
                               nil)
                           (http-protocol-error (condition) condition))))
                   (ensure-true (typep caught 'http-protocol-error))
                   (ensure-equal operation (http-error-operation caught))
                   (ensure-equal invalid (http-protocol-error-detail caught))
                   (ensure-equal 0 callback-count)
                   (ensure-equal 0 (binary-test-input-position stream))
                   (ensure-octets-equal (octets) (binary-test-output stream)))
              (close stream)))))
      (let ((stream (make-instance 'binary-test-stream :input wire))
            (chunks nil))
        (unwind-protect
             (let ((message
                     (funcall parser stream :max-header-bytes 1024
                                            :max-fields 2 :max-body-bytes 3
                                            :on-body-chunk
                                            (lambda (chunk) (push chunk chunks)))))
               (ensure-octets-equal (ascii "abc")
                                   (if request-p (http-request-body message)
                                       (http-response-body message)))
               (ensure-equal 1 (length chunks))
               (ensure-octets-equal (ascii "abc") (first chunks))
               (ensure-equal (1- (length wire)) (binary-test-input-position stream))
               (ensure-equal (char-code #\!) (read-byte stream))
               (ensure-octets-equal (octets) (binary-test-output stream)))
          (close stream))))))

(deftest http1-public-short-request-target
  (dolist (override-p '(nil t))
    (let* ((request
             (make-http-request :method "GET" :uri "http://example.test/"
                                :request-target (if override-p "/" "abc")))
           (caught
             (handler-case
                 (progn
                   (if override-p
                       (serialize-http-request request :request-target "abc")
                       (serialize-http-request request))
                   nil)
               (http-protocol-error (condition) condition))))
      (ensure-true (typep caught 'http-protocol-error))
      (ensure-equal :serialization (http-error-operation caught))
      (ensure-equal "abc" (http-protocol-error-detail caught)))
    (dolist (target '("/" "http://example.test/"))
      (let* ((request
               (make-http-request :method "GET" :uri "http://example.test/"
                                  :request-target (if override-p "/" target)))
             (wire
               (if override-p
                   (serialize-http-request request :request-target target)
                   (serialize-http-request request)))
             (parsed (parse-http-request wire)))
        (ensure-octets-equal
         (concatenate-octets
          (ascii (format nil "GET ~A HTTP/1.1|CRLF|" target))
          (ascii "Host: example.test|CRLF|Content-Length: 0|CRLF||CRLF|"))
         wire)
        (ensure-equal target (http-request-target parsed))
        (ensure-equal "GET" (http-request-method parsed))
        (ensure-equal '("example.test")
                      (http-header-values (http-request-headers parsed) "host"))
        (ensure-octets-equal (octets) (http-request-body parsed))))))

#+sbcl
(deftest http1-public-case-insensitive-actual-trailers
  (dolist (request-p '(t nil))
    (dolist (duplicate-p '(nil t))
      (let* ((trailers
               (append (list (make-http-header "X-Checksum" "one"))
                       (when duplicate-p
                         (list (make-http-header "x-checksum" "two")))))
             (expected-values (if duplicate-p '("one" "two") '("one")))
             (expected-names
               (if duplicate-p '("X-Checksum" "x-checksum") '("X-Checksum")))
             (wire
               (if request-p
                   (serialize-http-request
                    (make-http-request :method "POST"
                                       :uri "http://example.test/upload"
                                       :body (ascii "abc") :trailers trailers))
                   (serialize-http-response
                    (make-http-response :status 200 :body (ascii "abc")
                                        :trailers trailers))))
             (parser (if request-p #'parse-http-request #'parse-http-response)))
        (ensure-octets-equal
         (concatenate-octets
          (ascii (if request-p
                     "POST /upload HTTP/1.1|CRLF|Host: example.test|CRLF|"
                     "HTTP/1.1 200 OK|CRLF|"))
          (ascii "Transfer-Encoding: chunked|CRLF|Trailer: X-Checksum|CRLF||CRLF|3|CRLF|abc|CRLF|0|CRLF|X-Checksum: one|CRLF|")
          (if duplicate-p (ascii "x-checksum: two|CRLF|") (octets))
          (ascii "|CRLF|"))
         wire)
        (dolist (stream-p '(nil t))
          (let ((source
                  (if stream-p
                      (make-instance 'binary-test-stream
                                     :input (concatenate-octets wire (ascii "!")))
                      wire))
                (chunks nil))
            (unwind-protect
                 (let* ((parsed
                          (funcall parser source :on-body-chunk
                                   (lambda (chunk) (push chunk chunks))))
                        (headers
                          (if request-p (http-request-headers parsed)
                              (http-response-headers parsed)))
                        (parsed-trailers
                          (if request-p (http-request-trailers parsed)
                              (http-response-trailers parsed))))
                   (ensure-equal '("X-Checksum")
                                 (http-header-values headers "trailer"))
                   (ensure-equal '("chunked")
                                 (http-header-values headers "transfer-encoding"))
                   (ensure-equal expected-values
                                 (http-header-values parsed-trailers "X-CHECKSUM"))
                   (ensure-equal expected-names
                                 (mapcar #'http-header-name parsed-trailers))
                   (ensure-octets-equal
                    (ascii "abc")
                    (if request-p (http-request-body parsed)
                        (http-response-body parsed)))
                   (ensure-equal 1 (length chunks))
                   (ensure-octets-equal (ascii "abc") (first chunks))
                   (when stream-p
                     (ensure-equal (length wire)
                                   (binary-test-input-position source))
                     (ensure-equal (char-code #\!) (read-byte source))
                     (ensure-equal :eof (read-byte source nil :eof))
                     (ensure-octets-equal (octets) (binary-test-output source))))
              (when stream-p (close source)))))))))

#+sbcl
(deftest http1-public-empty-chunked-response
  (dolist (text '("" "abc"))
    (let* ((body (ascii text))
           (wire
             (serialize-http-response
              (make-http-response
               :status 200 :body body
               :headers (list (make-http-header "Transfer-Encoding" "chunked"))))))
      (ensure-octets-equal
       (concatenate-octets
        (ascii "HTTP/1.1 200 OK|CRLF|Transfer-Encoding: chunked|CRLF||CRLF|")
        (if (string= text "") (octets) (ascii "3|CRLF|abc|CRLF|"))
        (ascii "0|CRLF||CRLF|"))
       wire)
      (dolist (stream-p '(nil t))
        (dolist (collect-p '(t nil))
          (let ((source
                  (if stream-p
                      (make-instance 'binary-test-stream
                                     :input (concatenate-octets wire (ascii "!")))
                      wire))
                (chunks nil))
            (unwind-protect
                 (let ((response
                         (parse-http-response
                          source :collect-body-p collect-p
                          :on-body-chunk (lambda (chunk) (push chunk chunks)))))
                   (ensure-equal 200 (http-response-status response))
                   (ensure-equal '("chunked")
                                 (http-header-values
                                  (http-response-headers response)
                                  "transfer-encoding"))
                   (ensure-equal '() (http-response-trailers response))
                   (ensure-octets-equal (if collect-p body (octets))
                                       (http-response-body response))
                   (ensure-equal (if (string= text "") 0 1) (length chunks))
                   (ensure-octets-equal body
                                       (apply #'concatenate-octets chunks))
                   (when stream-p
                     (ensure-equal (length wire)
                                   (binary-test-input-position source))
                     (ensure-equal (char-code #\!) (read-byte source))
                     (ensure-equal :eof (read-byte source nil :eof))
                     (ensure-octets-equal (octets) (binary-test-output source))))
              (when stream-p (close source)))))))))

(deftest http1-final-response-headers-callback-precedes-body
  (dolist (stream-p '(nil #+sbcl t))
    (let* ((wire (ascii "HTTP/1.1 103 Early Hints|CRLF|Link: </style.css>; rel=preload|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|abc"))
           (source (if stream-p
                       #+sbcl (make-instance 'binary-test-stream :input wire)
                       #-sbcl wire
                       wire))
           (events '())
           (response
             (parse-http-response
              source
              :on-information
              (lambda (information)
                (push (list :information
                            (http-response-status information))
                      events))
              :on-response-headers
              (lambda (status headers)
                (push (list :headers status
                            (http-header-value headers "content-length"))
                      events))
              :on-body-chunk
              (lambda (chunk)
                (push (list :body (octets-as-string chunk)) events)))))
      (ensure-equal 200 (http-response-status response))
      (ensure-octets-equal (ascii "abc") (http-response-body response))
      (ensure-equal '((:information 103)
                      (:headers 200 "3")
                      (:body "abc"))
                    (nreverse events))
      #+sbcl
      (when stream-p
        (ensure-equal (length wire) (binary-test-input-position source))))
  #+sbcl
  (let* ((header-wire (ascii
                       "HTTP/1.1 101 Switching Protocols|CRLF|Upgrade: websocket|CRLF||CRLF|"))
         (wire (concatenate-octets header-wire (ascii "upgraded")))
         (source (make-instance 'binary-test-stream :input wire))
         (events nil)
         (response
           (parse-http-response
            source
            :on-response-headers
            (lambda (status headers)
              (push (list status (http-header-value headers "upgrade"))
                    events)))))
    (ensure-equal 101 (http-response-status response))
    (ensure-equal 0 (length (http-response-body response)))
    (ensure-equal '() (nreverse events))
    (ensure-equal (length header-wire)
                  (binary-test-input-position source)))))
(deftest http1-response-and-cancellation-callback-arguments-are-validated
  (let ((wire (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|"))
        (invalid-response-callback :invalid-response-callback)
        (invalid-cancel-callback :invalid-cancel-callback)
        (invalid-cancel-function :invalid-cancel-function)
        (invalid-cleanup-result :invalid-cleanup-result))
    (labels ((expect-parse-error (thunk detail)
               (let ((caught
                       (handler-case
                           (progn (funcall thunk) nil)
                         (http-protocol-error (condition) condition))))
                 (ensure-true (typep caught 'http-protocol-error))
                 (ensure-equal :response-parse (http-error-operation caught))
                 (ensure-equal detail (http-protocol-error-detail caught))
                 caught)))
      (expect-parse-error
       (lambda ()
         (parse-http-response wire
                              :on-response-headers invalid-response-callback))
       invalid-response-callback)
      (expect-parse-error
       (lambda ()
         (parse-http-response wire :on-cancel invalid-cancel-callback))
       invalid-cancel-callback)
      (let ((registration-count 0))
        (expect-parse-error
         (lambda ()
           (parse-http-response
            wire
            :on-cancel
            (lambda (cancel-function)
              (declare (ignore cancel-function))
              (incf registration-count)
              nil)
            :cancel-stream-function invalid-cancel-function))
         invalid-cancel-function)
        (ensure-equal 0 registration-count))
      (let* ((cancel-handler (lambda (cancel-function)
                               (declare (ignore cancel-function))
                               nil))
             (caught
               (expect-parse-error
                (lambda ()
                  (parse-http-response wire :on-cancel cancel-handler))
                cancel-handler)))
        (ensure-true (eq cancel-handler
                         (http-protocol-error-detail caught))))
      (let ((registration-count 0))
        (expect-parse-error
         (lambda ()
           (parse-http-response
            wire
            :on-cancel
            (lambda (cancel-function)
              (ensure-true (functionp cancel-function))
              (incf registration-count)
              invalid-cleanup-result)
            :cancel-stream-function (lambda () nil)))
         invalid-cleanup-result)
        (ensure-equal 1 registration-count)))))
(deftest http1-direct-parser-cancel-handler-unregisters-after-response
  (let* ((events '())
        (response
          (parse-http-response
           (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 0|CRLF||CRLF|")
           :on-cancel
           (lambda (cancel-function)
             (ensure-true (functionp cancel-function))
             (push :registered events)
             (lambda ()
               (push :unregistered events)))
           :cancel-stream-function
           (lambda ()
             (push :cancelled events)))))
    (ensure-equal 200 (http-response-status response))
    (ensure-equal '(:registered :unregistered) (nreverse events))))

#+sbcl
(deftest http1-final-response-headers-callback-failure-leaves-body-unread
  (let* ((header-wire (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|"))
         (wire (concatenate-octets header-wire (ascii "abc")))
         (source (make-instance 'binary-test-stream :input wire))
         (callback-error
           (make-condition 'simple-error
                           :format-control "response header callback failed"
                           :format-arguments '()))
         (body-calls 0)
         (caught
           (handler-case
               (progn
                 (parse-http-response
                  source
                  :on-response-headers
                  (lambda (status headers)
                    (ensure-equal 200 status)
                    (ensure-equal "3"
                                  (http-header-value headers "content-length"))
                    (error callback-error))
                  :on-body-chunk
                  (lambda (chunk)
                    (declare (ignore chunk))
                    (incf body-calls)))
                 nil)
             (simple-error (condition) condition))))
    (ensure-true (eq callback-error caught))
    (ensure-equal 0 body-calls)
    (ensure-equal (length header-wire)
                  (binary-test-input-position source))))

#+sbcl
(deftest http1-direct-stream-cancel-closes-once-and-unregisters
  (let* ((request (make-http-request :method "GET"
                                    :uri "http://127.0.0.1/"))
         (wire (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 3|CRLF||CRLF|abc"))
         (stream (make-instance 'binary-test-stream :input wire))
         (events '())
         (response
           (send-http-request-over-stream
            request
            :open-stream
            (lambda (received-request &key timeout deadline)
              (declare (ignore timeout deadline))
              (ensure-equal request received-request)
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (push :closed events))
            :on-cancel
            (lambda (cancel-function)
              (push :registered events)
              (funcall cancel-function)
              (lambda () (push :unregistered events)))
            :on-response-headers
            (lambda (status headers)
              (ensure-equal 200 status)
              (ensure-equal "3" (http-header-value headers "content-length"))
              (push :headers events))
            :on-body-chunk
            (lambda (chunk)
              (ensure-octets-equal (ascii "abc") chunk)
              (push :body events)))))
    (ensure-equal 200 (http-response-status response))
    (ensure-equal '(:registered :closed :headers :body :unregistered)
                  (nreverse events))))
