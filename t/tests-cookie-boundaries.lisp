(in-package #:http-kit/test)

(deftest client-cookie-constructor-argument-boundaries
  (let ((cookie (make-http-cookie :name "sid" :value "1" :domain ".EXAMPLE.TEST"
                                  :expires 2000 :max-age 10 :http-only-p t
                                  :same-site :lax :creation-time 1000)))
    (ensure-equal "sid" (http-cookie-name cookie))
    (ensure-equal "example.test" (http-cookie-domain cookie))
    (ensure-equal "/" (http-cookie-path cookie))
    (ensure-equal 2000 (http-cookie-expires cookie))
    (ensure-equal 1010 (http-cookie-expiry-time cookie))
    (ensure-equal 10 (http-cookie-max-age cookie))
    (ensure-true (http-cookie-http-only-p cookie))
    (ensure-equal :lax (http-cookie-same-site cookie))
    (ensure-equal 1000 (http-cookie-creation-time cookie)))
  (let ((cookie (make-http-cookie :name "sid" :value "1" :domain "example.test"
                                  :expires 2000 :creation-time 1000)))
    (ensure-equal 2000 (http-cookie-expiry-time cookie))
    (ensure-false (http-cookie-max-age cookie)))
  (dolist (arguments '((:name "") (:value ";") (:domain "") (:path "relative")
                       (:expires "soon") (:max-age "1") (:creation-time "now")
                       (:same-site :unsupported)))
    (let ((caught nil))
      (handler-case
          (apply #'make-http-cookie
                 (append arguments '(:name "sid" :value "1" :domain "example.test")))
        (http-protocol-error (condition) (setf caught condition)))
      (ensure-true (typep caught 'http-protocol-error))
      (ensure-equal :client (http-error-operation caught)))))

(deftest client-cookie-jar-argument-boundaries-and-copy
  (dolist (call (list (lambda () (http-cookie-jar-cookies nil))
                     (lambda () (http-cookie-jar-clear nil))
                     (lambda () (http-cookie-jar-accept-response nil "https://example.test/"
                                                                (make-http-response :status 200)))
                     (lambda () (http-cookie-jar-cookie-header nil "https://example.test/"))
                     (lambda () (make-http-cookie-jar :clock-function nil))
                     (lambda () (make-http-cookie-jar :public-suffix-p-function 1))
                     (lambda () (make-http-cookie-jar :max-cookies 0))
                     (lambda () (make-http-cookie-jar :max-cookies-per-domain 0))
                     (lambda () (make-http-cookie-jar :max-cookie-bytes 0))
                     (lambda () (make-http-cookie-jar :max-total-cookie-bytes 0))))
    (let ((caught nil))
      (handler-case (funcall call)
        (http-protocol-error (condition) (setf caught condition)))
      (ensure-true (typep caught 'http-protocol-error))
      (ensure-equal :client (http-error-operation caught))))
  (let ((jar (make-http-cookie-jar :clock-function (lambda () 1000))))
    (ensure-true
     (eq jar (http-cookie-jar-accept-response
              jar "https://example.test/"
              (make-http-response :status 200
                                  :headers (list (make-http-header "Set-Cookie" "sid=1"))))))
    (let ((first (first (http-cookie-jar-cookies jar)))
          (second (first (http-cookie-jar-cookies jar))))
      (ensure-false (eq first second))
      (setf (http-cookie-name first) "changed"))
    (ensure-equal "sid=1" (http-cookie-jar-cookie-header jar "https://example.test/"))
    (ensure-true (eq jar (http-cookie-jar-clear jar)))
    (ensure-false (http-cookie-jar-cookies jar))))

(deftest client-cookie-response-expiration-and-invalid-value
  (let ((jar (make-http-cookie-jar :public-suffix-p-function (lambda (domain)
                                                            (declare (ignore domain)) nil)))
        (expires (encode-universal-time 0 0 0 1 1 2030 0)))
    (http-cookie-jar-accept-response
     jar "https://sub.example.test/"
     (make-http-response
      :status 200
      :headers (list (make-http-header "Set-Cookie" "bad=a b")
                     (make-http-header "Set-Cookie"
                                       "sid=1; Domain=.EXAMPLE.TEST; HttpOnly; Expires=Tue, 01 Jan 2030 00:00:00 GMT")))
     :now 1000)
    (let ((cookies (http-cookie-jar-cookies jar)))
      (ensure-equal 1 (length cookies))
      (let ((cookie (first cookies)))
        (ensure-equal "sid" (http-cookie-name cookie))
        (ensure-equal "example.test" (http-cookie-domain cookie))
        (ensure-false (http-cookie-host-only-p cookie))
        (ensure-true (http-cookie-http-only-p cookie))
        (ensure-equal expires (http-cookie-expires cookie))
        (ensure-equal expires (http-cookie-expiry-time cookie))))
    (ensure-equal "sid=1" (http-cookie-jar-cookie-header jar "https://sub.example.test/" :now (1- expires)))
    (ensure-false (http-cookie-jar-cookie-header jar "https://sub.example.test/" :now expires))
    (ensure-false (http-cookie-jar-cookies jar))))

(deftest client-cookie-global-limit-across-domains
  (let ((jar (make-http-cookie-jar :max-cookies 2 :max-cookies-per-domain 3)))
    (loop for host in '("a.example.test" "b.example.test")
          for now from 1000
          do (ensure-true
              (eq jar (http-cookie-jar-accept-response
                       jar (format nil "https://~A/" host)
                       (make-http-response :status 200
                                           :headers (list (make-http-header "Set-Cookie" "sid=1")))
                       :now now))))
    (ensure-equal "sid=1" (http-cookie-jar-cookie-header jar "https://a.example.test/" :now 1002))
    (ensure-true
     (eq jar (http-cookie-jar-accept-response
              jar "https://c.example.test/"
              (make-http-response :status 200 :headers (list (make-http-header "Set-Cookie" "sid=1")))
              :now 1003)))
    (ensure-equal 2 (length (http-cookie-jar-cookies jar)))
    (ensure-false (http-cookie-jar-cookie-header jar "https://b.example.test/" :now 1004))
    (ensure-equal "sid=1" (http-cookie-jar-cookie-header jar "https://a.example.test/" :now 1004))
    (ensure-equal "sid=1" (http-cookie-jar-cookie-header jar "https://c.example.test/" :now 1004))))

(deftest client-cookie-partition-storage-counts-utf8-bytes
  (let ((partition (coerce (mapcar #'code-char '(#xe9 #x20ac #x1f600)) 'string))
        (response (make-http-response
                   :status 200
                   :headers (list (make-http-header "Set-Cookie" "sid=1; Secure; Partitioned")))))
    (let ((jar (make-http-cookie-jar :max-total-cookie-bytes 26)))
      (http-cookie-jar-accept-response jar "https://example.test/" response :now 1000 :partition-key partition)
      (ensure-equal 1 (length (http-cookie-jar-cookies jar)))
      (ensure-equal partition (http-cookie-partition-key (first (http-cookie-jar-cookies jar))))
      (ensure-equal "sid=1" (http-cookie-jar-cookie-header jar "https://example.test/" :now 1001 :partition-key partition)))
    (let ((jar (make-http-cookie-jar :max-total-cookie-bytes 25)))
      (http-cookie-jar-accept-response jar "https://example.test/" response :now 1000 :partition-key partition)
      (ensure-false (http-cookie-jar-cookies jar))
      (ensure-false (http-cookie-jar-cookie-header jar "https://example.test/" :now 1001 :partition-key partition)))
    (let ((jar (make-http-cookie-jar :max-total-cookie-bytes 20)))
      (http-cookie-jar-accept-response jar "https://example.test/" response :now 1000 :partition-key "abc")
      (ensure-equal 1 (length (http-cookie-jar-cookies jar)))
      (ensure-equal "sid=1" (http-cookie-jar-cookie-header jar "https://example.test/" :now 1001 :partition-key "abc")))))

(deftest client-cookie-secure-parent-prevents-child-overlay
  (let ((jar (make-http-cookie-jar :public-suffix-p-function (lambda (domain)
                                                            (declare (ignore domain)) nil))))
    (http-cookie-jar-accept-response
     jar "https://sub.example.test/"
     (make-http-response :status 200
                         :headers (list (make-http-header "Set-Cookie" "sid=secure; Domain=example.test; Secure")))
     :now 1000)
    (http-cookie-jar-accept-response
     jar "http://sub.example.test/"
     (make-http-response :status 200 :headers (list (make-http-header "Set-Cookie" "sid=insecure")))
     :now 1001)
    (ensure-equal 1 (length (http-cookie-jar-cookies jar)))
    (ensure-equal "sid=secure" (http-cookie-jar-cookie-header jar "https://sub.example.test/" :now 1002))
    (ensure-false (http-cookie-jar-cookie-header jar "http://sub.example.test/" :now 1002))
    (http-cookie-jar-accept-response
     jar "http://sub.example.test/"
     (make-http-response :status 200 :headers (list (make-http-header "Set-Cookie" "other=1")))
     :now 1003)
    (ensure-equal 2 (length (http-cookie-jar-cookies jar)))
    (ensure-equal "other=1" (http-cookie-jar-cookie-header jar "http://sub.example.test/" :now 1004))))

(deftest client-cookie-constructor-token-and-prefix-boundaries
  (dolist (arguments '((:name "a:b") (:name "a@b") (:name "a[b")
                       (:name "a)b") (:name "a{b") (:value 1)
                       (:domain 1) (:path 1) (:path "")
                       (:partition-key 1) (:partition-key "")
                       (:name "__Host-sid" :secure-p t :host-only-p t :path "/login")))
    (let ((caught nil))
      (handler-case
          (apply #'make-http-cookie
                 (append arguments '(:name "sid" :value "1" :domain "example.test")))
        (http-protocol-error (condition) (setf caught condition)))
      (ensure-true (typep caught 'http-protocol-error))
      (ensure-equal :client (http-error-operation caught))))
  (dolist (domain '("." "x"))
    (ensure-equal domain
                  (http-cookie-domain
                   (make-http-cookie :name "sid" :value "1" :domain domain))))
  (let ((cookie (make-http-cookie :name "__Host-sid" :value "1"
                                  :domain "example.test" :secure-p t
                                  :host-only-p t :partition-key "site")))
    (ensure-equal "/" (http-cookie-path cookie))
    (ensure-equal "site" (http-cookie-partition-key cookie))
    (ensure-true (http-cookie-secure-p cookie))))

(deftest client-cookie-empty-attributes-and-path-boundaries
  (dolist (attributes '("Domain; Path; SameSite" "Domain=; Path=; SameSite=unknown"
                        "Path=relative"))
    (let ((jar (make-http-cookie-jar)))
      (http-cookie-jar-accept-response
       jar "https://example.test/dir/page"
       (make-http-response :status 200
                           :headers (list (make-http-header
                                           "Set-Cookie" (concatenate 'string "sid=1; " attributes))))
       :now 1000)
      (let ((cookies (http-cookie-jar-cookies jar)))
        (ensure-equal 1 (length cookies))
        (ensure-equal "/dir" (http-cookie-path (first cookies)))
        (ensure-true (http-cookie-host-only-p (first cookies)))
        (ensure-false (http-cookie-same-site (first cookies))))
      (ensure-equal "sid=1" (http-cookie-jar-cookie-header jar "https://example.test/dir" :now 1001))
      (ensure-equal "sid=1" (http-cookie-jar-cookie-header jar "https://example.test/dir/child" :now 1001))
      (ensure-false (http-cookie-jar-cookie-header jar "https://example.test/directory" :now 1001))
      (ensure-false (http-cookie-jar-cookie-header jar "https://example.test/other" :now 1001))))
  (dolist (field '("missing-value" "=1" "sid=1; Domain=."))
    (let ((jar (make-http-cookie-jar)))
      (http-cookie-jar-accept-response
       jar "https://example.test/"
       (make-http-response :status 200 :headers (list (make-http-header "Set-Cookie" field)))
       :now 1000)
      (ensure-false (http-cookie-jar-cookies jar))
      (ensure-false (http-cookie-jar-cookie-header jar "https://example.test/" :now 1001)))))

(deftest client-cookie-domain-callback-and-empty-ip-component
  (dolist (entry '(("https://127..0.1/" "0.1" t)
                   ("https://127.0.0.1/" "0.1" nil)
                   ("https://sub.example.test/" "example.test" t)))
    (destructuring-bind (uri domain accepted) entry
      (let ((calls nil)
            (jar nil))
        (setf jar (make-http-cookie-jar
                   :public-suffix-p-function (lambda (value) (push value calls) nil)))
        (http-cookie-jar-accept-response
         jar uri (make-http-response :status 200
                                     :headers (list (make-http-header "Set-Cookie"
                                                                     (format nil "sid=1; Domain=~A" domain))))
         :now 1000)
        (ensure-equal (if accepted (list domain) nil) calls)
        (ensure-equal (if accepted "sid=1" nil)
                      (http-cookie-jar-cookie-header jar uri :now 1001))
        (ensure-equal (if accepted 1 0) (length (http-cookie-jar-cookies jar))))))
  (let ((calls nil)
        (jar nil))
    (setf jar (make-http-cookie-jar
               :public-suffix-p-function (lambda (domain) (push domain calls) t)))
    (http-cookie-jar-accept-response
     jar "https://example.test/" (make-http-response :status 200
                                                    :headers (list (make-http-header "Set-Cookie"
                                                                                    "sid=1; Domain=example.test")))
     :now 1000)
    (ensure-equal '("example.test") calls)
    (ensure-true (http-cookie-host-only-p (first (http-cookie-jar-cookies jar))))
    (ensure-equal "sid=1" (http-cookie-jar-cookie-header jar "https://example.test/" :now 1001))
    (ensure-false (http-cookie-jar-cookie-header jar "https://sub.example.test/" :now 1001))
    (http-cookie-jar-clear jar)
    (setf calls nil)
    (http-cookie-jar-accept-response
     jar "https://sub.example.test/" (make-http-response :status 200
                                                        :headers (list (make-http-header "Set-Cookie"
                                                                                        "sid=1; Domain=example.test")))
     :now 1000)
    (ensure-equal '("example.test") calls)
    (ensure-false (http-cookie-jar-cookies jar))))

(deftest client-cookie-secure-overlay-is-partition-scoped
  (let ((jar (make-http-cookie-jar)))
    (http-cookie-jar-accept-response
     jar "https://example.test/"
     (make-http-response :status 200 :headers (list (make-http-header "Set-Cookie"
                                                                            "sid=partition; Secure; Partitioned")))
     :partition-key "site" :now 1000)
    (http-cookie-jar-accept-response
     jar "http://example.test/"
     (make-http-response :status 200 :headers (list (make-http-header "Set-Cookie" "sid=plain")))
     :now 1001)
    (ensure-equal 2 (length (http-cookie-jar-cookies jar)))
    (ensure-equal "sid=plain" (http-cookie-jar-cookie-header jar "http://example.test/" :now 1002))
    (ensure-equal "sid=plain" (http-cookie-jar-cookie-header jar "https://example.test/" :now 1002))
    (ensure-equal "sid=partition; sid=plain"
                  (http-cookie-jar-cookie-header jar "https://example.test/" :partition-key "site" :now 1002))
    (ensure-equal "sid=plain"
                  (http-cookie-jar-cookie-header jar "https://example.test/" :partition-key "other" :now 1002))
    (http-cookie-jar-clear jar)
    (ensure-false (http-cookie-jar-cookies jar))))

(deftest client-cookie-header-orders-equal-and-unequal-paths
  (let ((jar (make-http-cookie-jar)))
    (loop for field in '("old=1; Path=/" "new=2; Path=/" "deep=3; Path=/dir/")
          for now from 1000
          do (http-cookie-jar-accept-response
              jar "https://example.test/dir/page"
              (make-http-response :status 200 :headers (list (make-http-header "Set-Cookie" field)))
              :now now))
    (ensure-equal "deep=3; old=1; new=2"
                  (http-cookie-jar-cookie-header jar "https://example.test/dir/page" :now 1003))
    (ensure-equal "old=1; new=2"
                  (http-cookie-jar-cookie-header jar "https://example.test/directory" :now 1003))
    (ensure-equal 3 (length (http-cookie-jar-cookies jar)))
    (http-cookie-jar-clear jar)
    (ensure-false (http-cookie-jar-cookie-header jar "https://example.test/" :now 1004))))
(deftest client-cookie-name-ascii
  (dolist (code '(#x20 #x7f #x80))
    (let ((name (string (code-char code))) (caught nil))
      (handler-case (make-http-cookie :name name :value "1" :domain "example.test")
        (http-protocol-error (condition) (setf caught condition)))
      (ensure-true (typep caught 'http-protocol-error))
      (ensure-equal :client (http-error-operation caught))
      (ensure-equal name (http-protocol-error-detail caught))))
  (dolist (name '("!" "~"))
    (ensure-equal name (http-cookie-name (make-http-cookie :name name :value "1"
                                                        :domain "example.test")))))

(deftest client-cookie-path-case-length
  (let ((jar (make-http-cookie-jar :clock-function (lambda () 1000))))
    (http-cookie-jar-accept-response
     jar "https://example.test/dir"
     (make-http-response :status 200 :headers
                         (list (make-http-header "Set-Cookie" "sid=1; Path=/dir"))))
    (ensure-false (http-cookie-jar-cookie-header jar "https://example.test/DIR"))
    (ensure-equal "sid=1" (http-cookie-jar-cookie-header jar "https://example.test/dir"))
    (ensure-equal "sid=1" (http-cookie-jar-cookie-header jar "https://example.test/dir/child"))
    (ensure-false (http-cookie-jar-cookie-header jar "https://example.test/directory"))
    (ensure-equal 1 (length (http-cookie-jar-cookies jar)))))

(deftest client-cookie-unknown-attribute
  (dolist (field '("sid=1; Extension=ignored; SameSite=Lax" "sid=1; SameSite=Lax"))
    (let ((jar (make-http-cookie-jar :clock-function (lambda () 1000))))
      (ensure-true
       (eq jar (http-cookie-jar-accept-response
                jar "https://example.test/"
                (make-http-response :status 200 :headers
                                    (list (make-http-header "Set-Cookie" field))))))
      (let ((cookies (http-cookie-jar-cookies jar)))
        (ensure-equal 1 (length cookies))
        (ensure-equal :lax (http-cookie-same-site (first cookies)))
        (ensure-equal "example.test" (http-cookie-domain (first cookies))))
      (ensure-equal "sid=1" (http-cookie-jar-cookie-header jar "https://example.test/")))))

(deftest client-cookie-expired-replacement
  (let ((jar (make-http-cookie-jar)))
    (dolist (row '((1000 "sid=old; Max-Age=60" "sid=old" 1)
                  (1001 "sid=deleted; Max-Age=0" nil 0)
                  (1001 "sid=new; Max-Age=1" "sid=new" 1)))
      (ensure-true
       (eq jar (http-cookie-jar-accept-response
                jar "https://example.test/"
                (make-http-response :status 200 :headers
                                    (list (make-http-header "Set-Cookie" (second row))))
                :now (first row))))
      (ensure-equal (fourth row) (length (http-cookie-jar-cookies jar)))
      (ensure-equal (third row) (http-cookie-jar-cookie-header jar "https://example.test/"
                                                             :now (first row))))
    (ensure-equal 1002 (http-cookie-expiry-time (first (http-cookie-jar-cookies jar))))
    (ensure-false (http-cookie-jar-cookie-header jar "https://example.test/" :now 1002))
    (ensure-false (http-cookie-jar-cookies jar))))
