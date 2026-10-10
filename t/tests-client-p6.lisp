(in-package #:http-kit/test)

(deftest client-p6-auth-validated-parameter-boundaries
  (let ((challenges (http-parse-authentication-challenges
                     "Digest ReAlM=\"a\\\"b\\\\c,d\", nonce=n")))
    (ensure-equal 1 (length challenges))
    (ensure-equal "a\"b\\c,d"
                  (http-authentication-challenge-parameter (first challenges) "realm"))
    (ensure-equal "n"
                  (http-authentication-challenge-parameter (first challenges) "nonce")))
  (dolist (field '("Digest bad/name=x" "Digest realm=\"a\\\""
                   "Digest realm=\"a\\" "Digest realm=\"a\"b\""))
    (ensure-false (http-parse-authentication-challenges field))))

(deftest client-p6-auth-canonical-digest-boundaries
  (let* ((body (ascii "content")) (field (http-content-digest body)))
    (dolist (bytes '("" "AAAA" "AAA=" "AA=="))
      (ensure-false
       (http-content-digest-valid-p body (format nil "example=:~A:" bytes))))
    (dolist (bytes '("AA" "AB==" "AAB=" "AA=A" "AA==AAAA"))
      (let ((caught (handler-case
                        (http-content-digest-valid-p body (format nil "example=:~A:" bytes))
                      (http-protocol-error (condition) condition))))
        (ensure-true (typep caught 'http-protocol-error))
        (ensure-equal :client (http-error-operation caught))
        (ensure-equal "example" (http-protocol-error-detail caught))))
    (ensure-true (http-content-digest-valid-p body field))
    (dolist (index (list 0 (1- (length body))))
      (let ((changed (copy-seq body)))
        (incf (aref changed index))
        (ensure-false (http-content-digest-valid-p changed field))))
    (ensure-false (http-content-digest-valid-p body "sha-256=:AA==:"))
    (dolist (parameters '(";flag" ";quoted=\"a;b\\\"c\\\\d\";flag"
                          ";bytes=:AAA=:;number=12;token=a/path;bool=?0"))
      (ensure-true (http-content-digest-valid-p body (concatenate 'string field parameters))))
    (dolist (parameters '(";flag;flag" ";=x" ";quoted=\"a\\\"" ";"))
      (let ((caught (handler-case
                        (http-content-digest-valid-p body (concatenate 'string field parameters))
                      (http-protocol-error (condition) condition))))
        (ensure-true (typep caught 'http-protocol-error))
        (ensure-equal :client (http-error-operation caught))
        (ensure-equal :field (http-protocol-error-detail caught))))))

#+sbcl
(deftest client-p6-socks-public-plan-nonstring-host
  (dolist (host '(42 nil))
    (let* ((request (make-http-request :method "GET" :uri "http://example.test/"))
           (proxy (make-http-proxy :scheme :socks5 :host "proxy.test" :port 1080))
           (plan (http-proxy-plan proxy (http-request-uri request)))
           (stream (make-instance 'binary-test-stream
                                  :input (octets 5 0 5 0 0 1 0 0 0 0 0 0)))
           (opened 0) (resolved 0) (closed 0)
           (opener (make-http-proxy-stream-opener
                    (lambda (&rest arguments)
                      (declare (ignore arguments)) (incf opened) stream)
                    (lambda (owned)
                      (ensure-true (eq stream owned)) (incf closed) (close owned))
                    :clock-function (lambda () 0)
                    :resolve-host
                    (lambda (seen &key timeout deadline)
                      (ensure-true (eql host seen))
                      (ensure-equal 2 timeout)
                      (ensure-false deadline)
                      (incf resolved) "127.0.0.1"))))
      (setf (getf plan :connect-host) host)
      (unwind-protect
           (progn
             (ensure-true (eq stream (funcall opener request :timeout 2 :proxy-plan plan)))
             (ensure-equal 1 opened)
             (ensure-equal 1 resolved)
             (ensure-equal 0 closed)
             (ensure-equal 12 (http-kit/test-core::binary-test-input-position stream))
             (ensure-octets-equal (octets 5 1 0 5 1 0 1 127 0 0 1 0 80)
                                  (binary-test-output stream))
             (ensure-true (open-stream-p stream)))
        (when (open-stream-p stream) (close stream)))
      (ensure-false (open-stream-p stream)))))

(deftest client-p6-convenience-url-method
  (let* ((seen-request nil)
         (client
           (make-http-client
            :transport-function
            (lambda (request &rest arguments)
              (declare (ignore arguments))
              (setf seen-request request)
              (make-http-response
               :status 204
               :headers nil
               :body (make-array 0 :element-type '(unsigned-byte 8)))))))
    (multiple-value-bind (response request)
        (http-client-send client "GET" "http://example.test/resource")
      (ensure-equal 204 (http-response-status response))
      (ensure-equal "GET" (http-request-method request))
      (ensure-equal request seen-request))))

(deftest client-p6-proxy-environment-resolution
  (let ((values '(("https_proxy" . "http://user:secret@proxy.test:8080")
                  ("NO_PROXY" . "example.test"))))
      (let ((http-kit/client::*proxy-environment-function*
            (lambda (name)
              (cdr (assoc name values :test #'string=)))))
      (ensure-false (http-proxy-for-uri nil "https://example.test/resource"))
      (let ((proxy (http-proxy-for-uri nil "https://other.test/resource")))
        (ensure-equal :http (http-proxy-scheme proxy))
        (ensure-equal "proxy.test" (http-proxy-host proxy))
        (ensure-false (http-proxy-no-proxy-p proxy "https://other.test/resource"))))))

(deftest client-p6-proxy-environment-credential-escapes
  (dolist (value '("http://%4A%6b:%3A%2f%2F%25@proxy.test:8080"
                   "http://%4a%6B:%3a%2F%2f%25@proxy.test:8080"))
    (let ((http-kit/client::*proxy-environment-function*
            (lambda (name)
              (when (string= name "http_proxy") value))))
      (let ((proxy (http-proxy-for-uri nil "http://other.test/resource")))
        (ensure-true (string= "Jk" (http-proxy-username proxy)))
        (ensure-true (string= "://%" (http-proxy-password proxy)))
        (ensure-equal "proxy.test" (http-proxy-host proxy))
        (ensure-equal 8080 (http-proxy-port proxy))))))

(deftest client-p6-proxy-environment-authority-defaults
  (dolist (case '(("proxy.test" :http "proxy.test" 80 nil nil)
                  ("HTTPS://proxy.test/" :https "proxy.test" 443 nil nil)
                  ("socks5://proxy.test" :socks5 "proxy.test" 1080 nil nil)
                  ("socks5h://[::1]" :socks5h "::1" 1080 nil nil)
                  ("http://[::1]:65535" :http "::1" 65535 nil nil)
                  ("http://user@proxy.test:1" :http "proxy.test" 1 "user" nil)))
    (destructuring-bind (value scheme host port username password) case
      (let ((http-kit/client::*proxy-environment-function*
              (lambda (name)
                (when (string= name "HTTP_PROXY") value))))
        (let ((proxy (http-proxy-for-uri nil "http://other.test/resource")))
          (ensure-equal scheme (http-proxy-scheme proxy))
          (ensure-equal host (http-proxy-host proxy))
          (ensure-equal port (http-proxy-port proxy))
          (ensure-equal username (http-proxy-username proxy))
          (ensure-equal password (http-proxy-password proxy)))))))

(deftest client-p6-proxy-environment-rejects-malformed-values
  (dolist (value '(42 "ftp://proxy.test" "http://proxy.test/path"
                  "http:///" "http://@" "http://[::1"
                  "http://[::1]suffix" "http://[::1]:"
                  "http://proxy.test:0" "http://proxy.test:65536"
                  "http://proxy.test:invalid" "http://%G0@proxy.test"
                  "http://%0G@proxy.test" "http://user:%@proxy.test"
                  "http://user:%0@proxy.test"))
    (let ((http-kit/client::*proxy-environment-function*
            (lambda (name)
              (when (string= name "http_proxy") value))))
      (signals http-protocol-error
        (http-proxy-for-uri nil "http://other.test/resource")))))

(deftest client-p6-proxy-environment-scheme-fallback-and-empty-values
  (dolist (values '(nil (("https_proxy" . "")) (("http_proxy" . "  "))))
    (let ((http-kit/client::*proxy-environment-function*
            (lambda (name) (cdr (assoc name values :test #'string=)))))
      (ensure-false (http-proxy-for-uri nil "https://other.test/resource"))))
  (let ((http-kit/client::*proxy-environment-function*
          (lambda (name)
            (when (string= name "http_proxy") "http://fallback.test"))))
    (ensure-equal "fallback.test"
                  (http-proxy-host
                   (http-proxy-for-uri nil "https://other.test/resource")))))

(deftest client-p6-no-proxy-ipv4-zero-prefix
  (let ((proxy (make-http-proxy :host "proxy.test" :port 8080
                                :no-proxy "192.0.2.0/0")))
    (dolist (uri '("http://0.0.0.0/" "http://255.255.255.255/"
                   "http://198.51.100.1/"))
      (ensure-true (http-proxy-no-proxy-p proxy uri)))
    (dolist (uri '("http://example.test/" "http://[2001:db8::1]/"))
      (ensure-false (http-proxy-no-proxy-p proxy uri))
      (ensure-true (eq proxy (http-proxy-for-uri proxy uri))))))

(deftest client-p6-no-proxy-ipv4-full-prefix
  (let ((proxy (make-http-proxy :host "proxy.test" :port 8080
                                :no-proxy "192.0.2.128/32")))
    (ensure-true (http-proxy-no-proxy-p proxy "http://192.0.2.128/"))
    (ensure-false (http-proxy-for-uri proxy "http://192.0.2.128/"))
    (dolist (uri '("http://192.0.2.127/" "http://192.0.2.129/"
                   "http://198.51.100.128/"))
      (ensure-false (http-proxy-no-proxy-p proxy uri))
      (ensure-true (eq proxy (http-proxy-for-uri proxy uri))))))

(deftest client-p6-no-proxy-invalid-ipv4-and-prefix
  (dolist (token '("192.0.2.256/24" "192.0.2.-1/24" "192.0.2/24"
                   "192.0.2.1.1/24" "192.0..1/24"
                   "192.0.2.0/33" "192.0.2.0/-1" "192.0.2.0/+1"
                   "192.0.2.0/" "192.0.2.0/invalid" "192.0.2.0/24junk"))
    (let ((proxy (make-http-proxy :host "proxy.test" :port 8080
                                  :no-proxy token)))
      (ensure-false (http-proxy-no-proxy-p proxy "http://192.0.2.1/"))
      (ensure-true (eq proxy (http-proxy-for-uri proxy "http://192.0.2.1/")))))
  (ensure-true
   (http-proxy-no-proxy-p
    (make-http-proxy :host "proxy.test" :port 8080 :no-proxy "192.0.2.0/24")
    "http://192.0.2.1/")))

(deftest client-p6-no-proxy-effective-port-and-cidr
  (dolist (case '(("example.test:80" "http://example.test/"
                   "https://example.test/")
                  ("example.test:443" "https://example.test/"
                   "http://example.test/")
                  ("192.0.2.0/24:80" "http://192.0.2.1/"
                   "http://192.0.2.1:81/")))
    (destructuring-bind (token included excluded) case
      (let ((proxy (make-http-proxy :host "proxy.test" :port 8080
                                    :no-proxy token)))
        (ensure-true (http-proxy-no-proxy-p proxy included))
        (ensure-false (http-proxy-for-uri proxy included))
        (ensure-false (http-proxy-no-proxy-p proxy excluded))
        (ensure-true (eq proxy (http-proxy-for-uri proxy excluded)))))))

(deftest client-p6-no-proxy-domain-isolation
  (let ((proxy (make-http-proxy :host "proxy.test" :port 8080
                                :no-proxy ".EXAMPLE.TEST")))
    (dolist (uri '("http://example.test/" "http://a.b.example.test/"))
      (ensure-true (http-proxy-no-proxy-p proxy uri)))
    (dolist (uri '("http://evil-example.test/" "http://example.test.attacker.test/"
                   "http://example.testattacker/" "http://notexample.test/"))
      (ensure-false (http-proxy-no-proxy-p proxy uri))
      (ensure-true (eq proxy (http-proxy-for-uri proxy uri))))))

(deftest client-p6-no-proxy-ipv6-prefix-boundaries
  (dolist (case '(("2001:db8::/0" "http://[ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff]/"
                   "http://192.0.2.1/")
                  ("2001:db8::1/128" "http://[2001:0DB8:0:0:0:0:0:1]/"
                   "http://[2001:db8::2]/")
                  ("2001:db8:0:0:8000::/65" "http://[2001:db8:0:0:ffff::1]/"
                   "http://[2001:db8:0:0:7fff::1]/")))
    (destructuring-bind (token included excluded) case
      (let ((proxy (make-http-proxy :host "proxy.test" :port 8080
                                    :no-proxy token)))
        (ensure-true (http-proxy-no-proxy-p proxy included))
        (ensure-false (http-proxy-no-proxy-p proxy excluded))
        (ensure-true (eq proxy (http-proxy-for-uri proxy excluded))))))
  (dolist (token '("2001:db8::/129" "2001:db8::/-1" "2001:db8::/+64"
                   "2001:db8::/" "2001:db8:::1/64" "2001:db8::gg/64"))
    (ensure-false
     (http-proxy-no-proxy-p
      (make-http-proxy :host "proxy.test" :port 8080 :no-proxy token)
      "http://[2001:db8::1]/"))))

(deftest client-p6-no-proxy-bracketed-ipv6-port
  (let ((proxy (make-http-proxy :host "proxy.test" :port 8080
                                :no-proxy "[2001:db8::1]:443")))
    (ensure-true (http-proxy-no-proxy-p proxy "https://[2001:db8::1]/"))
    (dolist (uri '("http://[2001:db8::1]/" "https://[2001:db8::1]:444/"
                   "https://[2001:db8::2]/"))
      (ensure-false (http-proxy-no-proxy-p proxy uri))
      (ensure-true (eq proxy (http-proxy-for-uri proxy uri)))))
  (dolist (token '("[2001:db8::1]suffix" "[2001:db8::1]:"))
    (ensure-false
     (http-proxy-no-proxy-p
      (make-http-proxy :host "proxy.test" :port 8080 :no-proxy token)
      "https://[2001:db8::1]/"))))

(deftest client-p6-no-proxy-explicit-port-invalid-domain
  (dolist (suffix '("invalid" "" "0" "65536" "0x1bb" "4.43"
                    "+443" "-443" "443junk" "4 43"))
    (let ((proxy (make-http-proxy :host "proxy.test" :port 8080
                                  :no-proxy (concatenate 'string "example.test:" suffix))))
      (dolist (uri '("http://example.test/" "https://example.test/"))
        (ensure-false (http-proxy-no-proxy-p proxy uri))
        (ensure-true (eq proxy (http-proxy-for-uri proxy uri)))))))

(deftest client-p6-no-proxy-explicit-port-invalid-ipv6
  (dolist (suffix '("invalid" "" "0" "65536" "0x1bb" "4.43"
                    "+443" "-443" "443junk" "4 43"))
    (let ((proxy (make-http-proxy :host "proxy.test" :port 8080
                                  :no-proxy (concatenate 'string "[2001:db8::1]:" suffix))))
      (dolist (uri '("http://[2001:db8::1]/" "https://[2001:db8::1]/"))
        (ensure-false (http-proxy-no-proxy-p proxy uri))
        (ensure-true (eq proxy (http-proxy-for-uri proxy uri)))))))

(deftest client-p6-no-proxy-explicit-port-invalid-cidr
  (dolist (suffix '("invalid" "" "0" "65536" "0x1bb" "4.43"
                    "+443" "-443" "443junk" "4 43"))
    (let ((proxy (make-http-proxy :host "proxy.test" :port 8080
                                  :no-proxy (concatenate 'string "192.0.2.0/24:" suffix))))
      (dolist (uri '("http://192.0.2.1/" "https://192.0.2.1/"))
        (ensure-false (http-proxy-no-proxy-p proxy uri))
        (ensure-true (eq proxy (http-proxy-for-uri proxy uri)))))))

(deftest client-p6-no-proxy-explicit-port-omitted-controls
  (dolist (case '(("example.test" "example.test")
                  ("2001:db8::1" "[2001:db8::1]")
                  ("[2001:db8::1]" "[2001:db8::1]")
                  ("192.0.2.0/24" "192.0.2.1")))
    (destructuring-bind (token host) case
      (let ((proxy (make-http-proxy :host "proxy.test" :port 8080 :no-proxy token)))
        (dolist (template '("http://~A/" "https://~A/" "https://~A:444/"))
          (let ((uri (format nil template host)))
            (ensure-true (http-proxy-no-proxy-p proxy uri))
            (ensure-false (http-proxy-for-uri proxy uri))))))))

(deftest client-p6-no-proxy-explicit-port-valid-controls
  (dolist (case '(("example.test:443" "example.test")
                  ("[2001:db8::1]:443" "[2001:db8::1]")
                  ("192.0.2.0/24:443" "192.0.2.1")))
    (destructuring-bind (token host) case
      (let ((proxy (make-http-proxy :host "proxy.test" :port 8080 :no-proxy token)))
        (dolist (template '("https://~A/" "http://~A:443/" "https://~A:443/"))
          (let ((uri (format nil template host)))
            (ensure-true (http-proxy-no-proxy-p proxy uri))
            (ensure-false (http-proxy-for-uri proxy uri))))
        (dolist (template '("http://~A/" "https://~A:444/"))
          (let ((uri (format nil template host)))
            (ensure-false (http-proxy-no-proxy-p proxy uri))
            (ensure-true (eq proxy (http-proxy-for-uri proxy uri)))))))))

(deftest client-p6-no-proxy-list-validation
  (let ((proxy (make-http-proxy :host "proxy.test" :port 8080
                                :no-proxy '("" "  example.test  " " "))))
    (ensure-true (http-proxy-no-proxy-p proxy "http://example.test/"))
    (ensure-false (http-proxy-no-proxy-p proxy "http://other.test/")))
  (dolist (value '(42 :invalid ("example.test" 42) ("*" :invalid)
                   ("example.test" nil)))
    (let ((proxy (make-http-proxy :host "proxy.test" :port 8080
                                  :no-proxy value)))
      (signals http-protocol-error
        (http-proxy-no-proxy-p proxy "http://example.test/"))
      (signals http-protocol-error
        (http-proxy-for-uri proxy "http://example.test/")))))

#+sbcl
(deftest client-p6-cookie-jar-concurrent-updates
  (let* ((jar (make-http-cookie-jar))
         (threads
           (loop for thread-index below 8
                 collect
                 (let ((name (format nil "thread-~D" thread-index)))
                   (sb-thread:make-thread
                    (lambda ()
                      (dotimes (iteration 20)
                        (declare (ignore iteration))
                        (http-cookie-jar-accept-response
                         jar
                         "https://example.test/"
                         (make-http-response
                          :status 200
                          :headers
                          (list
                           (make-http-header
                            "Set-Cookie"
                            (format nil "~A=value; Path=/" name)))
                          :body nil)))))))))
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    (ensure-equal 8 (length (http-cookie-jar-cookies jar)))))

#+sbcl
(deftest client-p6-pool-concurrent-observation
  (let ((pool
          (make-http-connection-pool
           :max-idle 4
           :open-stream (lambda (&rest arguments)
                          (declare (ignore arguments))
                          (error "The observation test must not open a stream.")))))
    (let ((threads
            (loop repeat 8
                  collect
                  (sb-thread:make-thread
                   (lambda ()
                     (dotimes (iteration 20)
                       (declare (ignore iteration))
                       (ensure-equal 0
                                     (getf (http-connection-pool-stats pool)
                                           :idle-count))))))))
      (dolist (thread threads)
        (sb-thread:join-thread thread)))))

(deftest client-p6-proxy-resolver-result-contract
  (let* ((uri (parse-http-uri "http://example.test/"))
         (accessors (list #'http-uri-scheme #'http-uri-authority
                          #'http-uri-path #'http-uri-query))
         (original-fields
           (loop for accessor in accessors
                 for value = (funcall accessor uri)
                 collect (if (stringp value) (copy-seq value) value)))
         (proxy (make-http-proxy :host "proxy.test" :port 8080)))
    (dolist (result (list 42 "proxy" (list proxy)))
      (let ((calls 0) (argument nil))
        (signals http-protocol-error
          (http-proxy-for-uri
           (lambda (parsed) (incf calls) (setf argument parsed) result) uri))
        (ensure-equal 1 calls)
        (loop for accessor in accessors
              for expected in original-fields
              do (ensure-equal expected (funcall accessor argument)))))
    (let ((calls 0))
      (ensure-true (eq proxy (http-proxy-for-uri
                             (lambda (parsed)
                               (incf calls)
                               (loop for accessor in accessors
                                     for expected in original-fields
                                     do (ensure-equal expected (funcall accessor parsed)))
                               proxy)
                             uri)))
      (ensure-equal 1 calls))
    (let ((calls 0))
      (ensure-false (http-proxy-for-uri
                     (lambda (parsed) (declare (ignore parsed)) (incf calls) nil)
                     uri))
      (ensure-equal 1 calls))
    (loop for accessor in accessors
          for expected in original-fields
          do (ensure-equal expected (funcall accessor uri)))))

(deftest client-p6-proxy-resolver-list-order-and-short-circuit
  (let* ((uri (parse-http-uri "http://example.test/"))
         (accessors (list #'http-uri-scheme #'http-uri-authority
                          #'http-uri-path #'http-uri-query))
         (original-fields
           (loop for accessor in accessors
                 for value = (funcall accessor uri)
                 collect (if (stringp value) (copy-seq value) value)))
         (bypassed (make-http-proxy :host "skip.test" :port 8080 :no-proxy "example.test"))
         (chosen (make-http-proxy :host "chosen.test" :port 8080))
         (calls nil))
    (ensure-true
     (eq chosen
         (http-proxy-for-uri
          (list (lambda (parsed)
                  (loop for accessor in accessors
                        for expected in original-fields
                        do (ensure-equal expected (funcall accessor parsed)))
                  (push :miss calls) nil)
                bypassed
                (lambda (parsed)
                  (loop for accessor in accessors
                        for expected in original-fields
                        do (ensure-equal expected (funcall accessor parsed)))
                  (push :chosen calls) chosen)
                (lambda (parsed) (declare (ignore parsed)) (error "Unexpected tail resolver call.")))
          uri)))
    (ensure-equal '(:miss :chosen) (reverse calls))
    (loop for accessor in accessors
          for expected in original-fields
          do (ensure-equal expected (funcall accessor uri)))))

(deftest client-p6-proxy-resolver-list-all-miss-and-error-boundary
  (let* ((uri "http://example.test/")
         (bypassed (make-http-proxy :host "skip.test" :port 8080 :no-proxy "example.test"))
         (chosen (make-http-proxy :host "chosen.test" :port 8080))
         (calls 0))
    (ensure-false
     (http-proxy-for-uri
      (list (lambda (parsed) (declare (ignore parsed)) (incf calls) nil)
            bypassed
            (lambda (parsed) (declare (ignore parsed)) (incf calls) nil)) uri))
    (ensure-equal 2 calls)
    (setf calls 0)
    (signals http-protocol-error
      (http-proxy-for-uri
       (list 42 (lambda (parsed) (declare (ignore parsed)) (incf calls) chosen)) uri))
    (ensure-equal 0 calls)
    (ensure-true (eq chosen (http-proxy-for-uri (list chosen 42) uri)))))

(deftest client-p6-proxy-invalid-arguments
  (dolist (invalid '(42 "proxy" :invalid))
    (signals http-protocol-error
      (http-proxy-for-uri invalid "http://example.test/"))
    (signals http-protocol-error
      (http-proxy-no-proxy-p invalid "http://example.test/")))
  (ensure-false (http-proxy-no-proxy-p nil "http://example.test/")))

(deftest client-p6-no-proxy-single-colon-ipv6-cidr
  (let ((uri "http://[2001:db8::1]/"))
    (dolist (rule '("2001:db8:0:0:0:0:0:1:/128" ":2001:db8:0:0:0:0:0:1/128"))
      (let ((proxy (make-http-proxy :host "proxy.test" :port 8080 :no-proxy rule)))
        (ensure-false (http-proxy-no-proxy-p proxy uri))
        (ensure-true (eq proxy (http-proxy-for-uri proxy uri)))))
    (let ((proxy (make-http-proxy :host "proxy.test" :port 8080
                                  :no-proxy "2001:db8:0:0:0:0:0:1/128")))
      (ensure-true (http-proxy-no-proxy-p proxy uri))
      (ensure-false (http-proxy-for-uri proxy uri)))))

(deftest client-p6-no-proxy-short-host-and-empty-port-host
  (dolist (host '("x" "ab"))
    (let ((proxy (make-http-proxy :host "proxy.test" :port 8080 :no-proxy host))
          (uri (format nil "http://~A/" host)))
      (ensure-true (http-proxy-no-proxy-p proxy uri))
      (ensure-false (http-proxy-for-uri proxy uri))))
  (dolist (case '((":80" "http://example.test/") (":443" "https://example.test/")))
    (let ((proxy (make-http-proxy :host "proxy.test" :port 8080 :no-proxy (first case))))
      (ensure-false (http-proxy-no-proxy-p proxy (second case)))
      (ensure-true (eq proxy (http-proxy-for-uri proxy (second case)))))))

#+sbcl
(deftest client-p6-proxy-opener-error-ownership
  (let ((request (make-http-request :method "GET" :uri "http://example.test/"))
        (opened 0)
        (closed 0))
    (signals http-protocol-error
      (make-http-proxy-stream-opener
       (lambda (&rest arguments) (declare (ignore arguments)) (incf opened))
       (lambda (stream) (declare (ignore stream)) (incf closed))
       :resolve-host-style :invalid))
    (ensure-equal 0 opened)
    (ensure-equal 0 closed)
    (let* ((opener (make-http-proxy-stream-opener
                    (lambda (&rest arguments) (declare (ignore arguments))
                      (incf opened) 17)
                    (lambda (stream) (declare (ignore stream)) (incf closed)))))
      (signals http-proxy-error (funcall opener request))
      (ensure-equal 1 opened)
      (ensure-equal 0 closed))
    (let* ((stream (make-instance 'binary-test-stream :input (octets)))
           (opener (make-http-proxy-stream-opener
                    (lambda (&rest arguments) (declare (ignore arguments))
                      (incf opened) stream)
                    (lambda (owned)
                      (ensure-true (eq stream owned))
                      (incf closed) (close owned)))))
      (unwind-protect
           (progn
             (signals http-proxy-error
               (funcall opener request :proxy-plan '(:mode :invalid)))
             (ensure-equal 2 opened)
             (ensure-equal 1 closed)
             (ensure-false (open-stream-p stream))
             (ensure-equal 0 (length (binary-test-output stream))))
        (when (open-stream-p stream) (close stream))))
    (dolist (original (list (make-condition 'http-timeout :message "probe"
                                          :operation :read :kind :read)
                           (make-condition 'simple-error :format-control "probe")))
      (let* ((plan (list :mode :forward :proxy :private
                         :proxy-authorization "test-only-authorization"
                         :proxy-challenge-auth-provider #'identity :marker 42))
             (opener (make-http-proxy-stream-opener
                      (lambda (&rest arguments) (declare (ignore arguments))
                        (incf opened) (error original))
                      (lambda (stream) (declare (ignore stream)) (incf closed))))
             (caught (handler-case (funcall opener request :proxy-plan plan)
                       (http-error (condition) condition))))
        (if (typep original 'http-error)
            (ensure-true (eq original caught))
            (progn
              (ensure-true (typep caught 'http-proxy-error))
              (ensure-equal :proxy (http-error-operation caught))
              (ensure-equal '(:mode :forward :marker 42)
                            (getf (http-client-error-detail caught) :detail))
              (ensure-true (eq original (getf (http-client-error-detail caught) :cause)))))))
    (ensure-equal 4 opened)
    (ensure-equal 1 closed)))

#+sbcl
(deftest client-p6-https-proxy-ipv6-connect-upgrade-order
  (let* ((request (make-http-request :method "GET" :uri "https://[2001:db8::1]/path"))
         (proxy (make-http-proxy :scheme :https :host "proxy.test" :port 8443))
         (plan (http-proxy-plan proxy (http-request-uri request)))
         (stream (make-instance 'binary-test-stream
                                :input (ascii "HTTP/1.1 200 Connection Established|CRLF||CRLF|")))
         (opened 0)
         (closed 0)
         (upgrades nil)
         (opener (make-http-proxy-stream-opener
                  (lambda (seen &key timeout deadline proxy-plan proxy)
                    (ensure-true (eq request seen))
                    (ensure-true (eq plan proxy-plan))
                    (ensure-equal "proxy.test" (http-proxy-host proxy))
                    (ensure-equal 3 timeout)
                    (ensure-equal 10 deadline)
                    (incf opened) stream)
                  (lambda (owned) (ensure-true (eq stream owned))
                    (incf closed) (close owned))
                  :clock-function (lambda () 0)
                  :tls-upgrade
                  (lambda (owned uri &key timeout deadline)
                    (ensure-true (eq stream owned))
                    (ensure-equal 3 timeout)
                    (ensure-equal 10 deadline)
                    (push (list (http-uri-scheme uri) (http-uri-host uri)
                                (http-uri-port uri) (length (binary-test-output stream)))
                          upgrades)
                    owned))))
    (unwind-protect
         (progn
           (ensure-true (eq stream (funcall opener request :timeout 3 :deadline 10
                                           :proxy-plan plan :proxy proxy)))
           (ensure-equal 1 opened)
           (ensure-equal 0 closed)
           (ensure-true (open-stream-p stream))
           (let ((wire (ascii "CONNECT [2001:db8::1]:443 HTTP/1.1|CRLF|Host: [2001:db8::1]:443|CRLF|Content-Length: 0|CRLF||CRLF|")))
             (ensure-octets-equal wire (binary-test-output stream))
             (ensure-equal (list '("https" "proxy.test" 8443 0)
                                 (list "https" "2001:db8::1" nil (length wire)))
                           (nreverse upgrades)))
           (ensure-equal (length (ascii "HTTP/1.1 200 Connection Established|CRLF||CRLF|"))
                         (http-kit/test-core::binary-test-input-position stream)))
      (close stream))
    (ensure-false (open-stream-p stream))))

#+sbcl
(deftest client-p6-explicit-connect-plain-http-target
  (dolist (scheme '(:http :https))
    (let* ((request (make-http-request :method "GET" :uri "http://example.test/path"))
           (proxy (make-http-proxy :scheme scheme :host "proxy.test" :port 8080))
           (plan (list :mode :connect :target (http-request-uri request)
                       :proxy proxy :connect-host "example.test" :connect-port 80
                       :proxy-host "proxy.test" :proxy-port 8080 :tunnel-p t))
           (stream (make-instance 'binary-test-stream
                                  :input (ascii "HTTP/1.1 200 Connection Established|CRLF||CRLF|")))
           (opened 0)
           (closed 0)
           (upgrades nil)
           (opener
             (make-http-proxy-stream-opener
              (lambda (seen &key timeout deadline proxy-plan proxy)
                (ensure-true (eq request seen))
                (ensure-true (eq plan proxy-plan))
                (ensure-equal scheme (http-proxy-scheme proxy))
                (ensure-equal 3 timeout)
                (ensure-equal 10 deadline)
                (incf opened)
                stream)
              (lambda (owned)
                (ensure-true (eq stream owned))
                (incf closed)
                (close owned))
              :clock-function (lambda () 0)
              :tls-upgrade
              (lambda (owned uri &key timeout deadline)
                (ensure-true (eq stream owned))
                (ensure-equal 3 timeout)
                (ensure-equal 10 deadline)
                (push (list (http-uri-scheme uri) (http-uri-host uri)
                            (http-uri-port uri) (length (binary-test-output stream)))
                      upgrades)
                owned))))
      (unwind-protect
           (progn
             (ensure-true (eq stream (funcall opener request :timeout 3 :deadline 10
                                             :proxy-plan plan :proxy proxy)))
             (ensure-equal 1 opened)
             (ensure-equal 0 closed)
             (ensure-true (open-stream-p stream))
             (ensure-octets-equal
              (ascii "CONNECT example.test:80 HTTP/1.1|CRLF|Host: example.test:80|CRLF|Content-Length: 0|CRLF||CRLF|")
              (binary-test-output stream))
             (ensure-equal (if (eq scheme :https)
                               '(("https" "proxy.test" 8080 0))
                               nil)
                           (nreverse upgrades))
             (ensure-equal (length (ascii "HTTP/1.1 200 Connection Established|CRLF||CRLF|"))
                           (http-kit/test-core::binary-test-input-position stream)))
        (close stream))
      (ensure-false (open-stream-p stream)))))

#+sbcl
(deftest client-p6-proxy-connect-rejected-provider-results
  (dolist (case '((407 nil 1) (407 17 1) (403 "test-only-authorization" 0)))
    (destructuring-bind (status provider-result expected-calls) case
      (let* ((request (make-http-request :method "GET" :uri "https://example.test/"))
             (proxy (make-http-proxy :host "proxy.test" :port 8080))
             (plan (http-proxy-plan proxy (http-request-uri request)))
             (stream (make-instance 'binary-test-stream
                                   :input (ascii (format nil "HTTP/1.1 ~D Rejected|CRLF|Proxy-Authenticate: Basic realm=\"proxy\"|CRLF|Content-Length: 0|CRLF||CRLF|" status))))
             (opened 0) (closed 0) (provider-calls 0)
             (opener (make-http-proxy-stream-opener
                      (lambda (&rest arguments) (declare (ignore arguments))
                        (incf opened) stream)
                      (lambda (owned) (ensure-true (eq stream owned))
                        (incf closed) (close owned)))))
        (setf (getf plan :proxy-challenge-auth-provider)
              (lambda (seen response seen-plan challenges)
                (ensure-true (eq request seen))
                (ensure-true (eq plan seen-plan))
                (ensure-equal status (http-response-status response))
                (ensure-equal 1 (length challenges))
                (incf provider-calls) provider-result))
        (unwind-protect
             (progn
               (signals http-proxy-error (funcall opener request :proxy-plan plan))
               (ensure-equal 1 opened)
               (ensure-equal 1 closed)
               (ensure-equal expected-calls provider-calls)
               (ensure-false (open-stream-p stream))
               (ensure-octets-equal
                (ascii "CONNECT example.test:443 HTTP/1.1|CRLF|Host: example.test:443|CRLF|Content-Length: 0|CRLF||CRLF|")
                (binary-test-output stream)))
          (when (open-stream-p stream) (close stream)))))))

#+sbcl
(deftest client-p6-socks-unoffered-authentication-method
  (dolist (authenticated-p '(nil t))
    (let* ((request (make-http-request :method "GET" :uri "http://example.test/"))
           (proxy (make-http-proxy :scheme :socks5h :host "proxy.test" :port 1080
                                   :username (and authenticated-p "user")
                                   :password (and authenticated-p "password")))
           (plan (http-proxy-plan proxy (http-request-uri request)))
           (method (if authenticated-p 0 2))
           (stream (make-instance 'binary-test-stream :input (octets 5 method)))
           (closed 0)
           (opener (make-http-proxy-stream-opener
                    (lambda (&rest arguments) (declare (ignore arguments)) stream)
                    (lambda (owned) (ensure-true (eq stream owned))
                      (incf closed) (close owned)))))
      (unwind-protect
           (progn
             (signals http-proxy-error (funcall opener request :proxy-plan plan))
             (ensure-equal 1 closed)
             (ensure-equal 2 (http-kit/test-core::binary-test-input-position stream))
             (ensure-octets-equal (octets 5 1 (if authenticated-p 2 0))
                                  (binary-test-output stream))
             (ensure-false (open-stream-p stream)))
        (when (open-stream-p stream) (close stream))))))

#+sbcl
(deftest client-p6-socks-nonstring-resolver-result
  (let* ((request (make-http-request :method "GET" :uri "http://example.test/"))
         (proxy (make-http-proxy :scheme :socks5 :host "proxy.test" :port 1080))
         (plan (http-proxy-plan proxy (http-request-uri request)))
         (stream (make-instance 'binary-test-stream :input (octets 5 0)))
         (resolved 0) (closed 0)
         (opener (make-http-proxy-stream-opener
                  (lambda (&rest arguments) (declare (ignore arguments)) stream)
                  (lambda (owned) (ensure-true (eq stream owned))
                    (incf closed) (close owned))
                  :resolve-host (lambda (host &key timeout deadline)
                                  (ensure-equal "example.test" host)
                                  (ensure-equal 2 timeout)
                                  (ensure-false deadline)
                                  (incf resolved) 17))))
    (unwind-protect
         (progn
           (signals http-proxy-error (funcall opener request :timeout 2 :proxy-plan plan))
           (ensure-equal 1 resolved)
           (ensure-equal 1 closed)
           (ensure-octets-equal (octets 5 1 0) (binary-test-output stream))
           (ensure-false (open-stream-p stream)))
      (when (open-stream-p stream) (close stream)))))

#+sbcl
(deftest client-p6-https-forward-proxy-upgrade
  (let* ((request (make-http-request :method "GET" :uri "http://example.test/path"))
         (proxy (make-http-proxy :scheme :https :host "proxy.test" :port 8443))
         (plan (http-proxy-plan proxy (http-request-uri request)))
         (stream (make-instance 'binary-test-stream :input (octets)))
         (upgrades 0) (closed 0)
         (opener (make-http-proxy-stream-opener
                  (lambda (&rest arguments) (declare (ignore arguments)) stream)
                  (lambda (owned) (ensure-true (eq stream owned))
                    (incf closed) (close owned))
                  :tls-upgrade (lambda (owned uri &key timeout deadline)
                                 (ensure-true (eq stream owned))
                                 (ensure-equal "https" (http-uri-scheme uri))
                                 (ensure-equal "proxy.test" (http-uri-host uri))
                                 (ensure-equal 8443 (http-uri-port uri))
                                 (ensure-equal 2 timeout)
                                 (ensure-false deadline)
                                 (incf upgrades) owned))))
    (unwind-protect
         (progn
           (ensure-equal :forward (getf plan :mode))
           (ensure-equal "http://example.test/path" (getf plan :request-target))
           (ensure-true (eq stream (funcall opener request :timeout 2 :proxy-plan plan)))
           (ensure-equal 1 upgrades)
           (ensure-equal 0 closed)
           (ensure-equal 0 (length (binary-test-output stream)))
           (ensure-true (open-stream-p stream)))
      (close stream))
    (ensure-false (open-stream-p stream))))

#+sbcl
(deftest client-p6-proxy-negotiation-deadline-ownership
  (dolist (case '((nil :write 0 0) ((0 1) :write 3 0)
                  ((0 0 1) :read 3 0) ((0 0 0 1) :read 3 1)))
    (destructuring-bind (ticks kind output-length input-position) case
      (let* ((request (make-http-request :method "GET" :uri "http://example.test/"))
             (proxy (make-http-proxy :scheme :socks5h :host "proxy.test" :port 1080))
             (plan (http-proxy-plan proxy (http-request-uri request)))
             (stream (make-instance 'binary-test-stream :input (octets 5 0)))
             (remaining (copy-list ticks))
             (closed 0)
             (arguments
               (when ticks
                 (list :clock-function (lambda ()
                                         (ensure-true remaining)
                                         (pop remaining)))))
             (opener (apply #'make-http-proxy-stream-opener
                            (lambda (&rest arguments) (declare (ignore arguments)) stream)
                            (lambda (owned) (ensure-true (eq stream owned))
                              (incf closed) (close owned))
                            arguments)))
        (unwind-protect
             (let ((caught (handler-case
                               (funcall opener request :deadline (if ticks 1 0) :proxy-plan plan)
                             (http-timeout (condition) condition))))
               (ensure-true (typep caught 'http-timeout))
               (ensure-equal kind (http-timeout-kind caught))
               (ensure-equal kind (http-error-operation caught))
               (ensure-equal 1 closed)
               (ensure-equal output-length (length (binary-test-output stream)))
               (ensure-equal input-position (http-kit/test-core::binary-test-input-position stream))
               (when ticks (ensure-false remaining))
               (ensure-false (open-stream-p stream)))
          (when (open-stream-p stream) (close stream)))))))

(deftest client-p6-pool-public-contract
  (let ((opened 0)
        (closed 0))
    (flet ((open-stream (&rest arguments)
             (declare (ignore arguments)) (incf opened))
           (close-stream (stream)
             (declare (ignore stream)) (incf closed)))
      (dolist (options '((:open-stream nil) (:close-stream 17)
                         (:tls-upgrade 17) (:resolve-host 17)
                         (:clock-function 17) (:resolve-host-style :invalid)))
        (signals http-protocol-error
          (apply #'make-http-connection-pool
                 (append options (list :open-stream #'open-stream
                                       :close-stream #'close-stream)))))
      (let* ((clock (lambda () 10))
             (upgrade #'identity)
             (resolver #'identity)
             (pool (make-http-connection-pool
                    :open-stream #'open-stream :close-stream #'close-stream
                    :clock-function clock :tls-upgrade upgrade :resolve-host resolver
                    :idle-timeout nil :max-connection-age nil :max-idle 0)))
        (ensure-equal 0 (http-connection-pool-max-idle pool))
        (ensure-false (http-connection-pool-idle-timeout pool))
        (ensure-false (http-connection-pool-max-connection-age pool))
        (ensure-true (eq clock (http-connection-pool-clock-function pool)))
        (ensure-true (eq upgrade (http-connection-pool-tls-upgrade pool)))
        (ensure-true (eq resolver (http-connection-pool-resolve-host pool)))
        (ensure-equal '(:idle-count 0 :max-idle 0 :idle-timeout nil
                       :max-connection-age nil)
                      (http-connection-pool-stats pool))
        (signals http-protocol-error (http-connection-pool-send pool 17)))
      (dolist (accessor (list #'http-connection-pool-max-idle
                             #'http-connection-pool-idle-timeout
                             #'http-connection-pool-max-connection-age
                             #'http-connection-pool-clock-function
                             #'http-connection-pool-tls-upgrade
                             #'http-connection-pool-resolve-host
                             #'http-connection-pool-stats
                             #'http-connection-pool-clear))
        (signals http-protocol-error (funcall accessor 17)))
      (signals http-protocol-error
        (http-connection-pool-send 17 (make-http-request :method "GET" :uri "http://example.test/")))
      (ensure-equal 0 opened)
      (ensure-equal 0 closed))))

#+sbcl
(deftest client-p6-pool-default-origin-reuse
  (let* ((reply (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|"))
         (stream (make-instance 'binary-test-stream :input (concatenate-octets reply reply)))
         (opened 0) (closed 0)
         (pool (make-http-connection-pool
                :idle-timeout nil :max-connection-age nil
                :clock-function (lambda () 10)
                :open-stream (lambda (&rest arguments)
                               (declare (ignore arguments)) (incf opened) stream)
                :close-stream (lambda (owned)
                                (ensure-true (eq stream owned)) (incf closed) (close owned)))))
    (unwind-protect
         (progn
           (dolist (uri '("http://example.test/first" "http://example.test/second"))
             (ensure-equal 204 (http-response-status
                                (http-connection-pool-send
                                 pool (make-http-request :method "GET" :uri uri)))))
           (ensure-equal 1 opened)
           (ensure-equal 0 closed)
           (ensure-equal 1 (getf (http-connection-pool-stats pool) :idle-count))
           (ensure-equal (* 2 (length reply))
                         (http-kit/test-core::binary-test-input-position stream))
           (ensure-octets-equal
            (ascii "GET /first HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 0|CRLF||CRLF|GET /second HTTP/1.1|CRLF|Host: example.test|CRLF|Content-Length: 0|CRLF||CRLF|")
            (binary-test-output stream))
           (ensure-equal 1 (http-connection-pool-clear pool))
           (ensure-equal 1 closed)
           (ensure-false (open-stream-p stream)))
      (http-connection-pool-clear pool)
      (when (open-stream-p stream) (close stream)))))

#+sbcl
(deftest client-p6-pool-clock-error-ownership
  (dolist (phase '(:before-open :after-response))
    (let* ((original (make-condition 'simple-error :format-control "probe"))
           (reply (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|"))
           (stream (make-instance 'binary-test-stream :input reply))
           (opened 0) (closed 0) (bad-p t)
           (pool (make-http-connection-pool
                  :idle-timeout nil :max-connection-age nil
                  :clock-function
                  (lambda ()
                    (if (and bad-p
                             (or (eq phase :before-open)
                                 (plusp (http-kit/test-core::binary-test-input-position stream))))
                        (error original) 10))
                  :open-stream (lambda (&rest arguments)
                                 (declare (ignore arguments)) (incf opened) stream)
                  :close-stream (lambda (owned)
                                  (ensure-true (eq stream owned)) (incf closed) (close owned)))))
      (unwind-protect
           (let ((caught (handler-case
                             (http-connection-pool-send
                              pool (make-http-request :method "GET" :uri "http://example.test/"))
                           (http-connection-error (condition) condition))))
             (ensure-true (typep caught 'http-connection-error))
             (ensure-true (eq original (http-connection-error-cause caught)))
             (ensure-equal :transport (http-error-operation caught))
             (ensure-equal (if (eq phase :before-open) 0 1) opened)
             (ensure-equal (if (eq phase :before-open) 0 1) closed)
             (ensure-equal (if (eq phase :before-open) 0 (length reply))
                           (http-kit/test-core::binary-test-input-position stream))
             (ensure-equal (eq phase :before-open) (open-stream-p stream))
             (setf bad-p nil)
             (ensure-equal 0 (getf (http-connection-pool-stats pool) :idle-count)))
        (setf bad-p nil)
        (http-connection-pool-clear pool)
        (when (open-stream-p stream) (close stream))))))
