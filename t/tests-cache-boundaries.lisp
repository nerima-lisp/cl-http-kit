(in-package #:http-kit/test)

(deftest client-cache-public-argument-boundaries
  (let* ((clock (lambda () 1000))
         (cache (make-http-cache :max-entries 2 :clock-function clock))
         (request (make-http-request :method "GET" :uri "http://example.test/cache"))
         (response (make-http-response :status 200)))
    (ensure-equal 2 (http-cache-max-entries cache))
    (ensure-true (eq clock (http-cache-clock-function cache)))
    (ensure-false (http-cache-status-identifier cache))
    (dolist (call (list (lambda () (http-cache-max-entries nil))
                       (lambda () (http-cache-clock-function nil))
                       (lambda () (http-cache-status-identifier nil))
                       (lambda () (http-cache-entries nil))
                       (lambda () (http-cache-clear nil))
                       (lambda () (http-cache-invalidate nil request))
                       (lambda () (http-cache-lookup nil request))
                       (lambda () (http-cache-store nil request response))
                       (lambda () (http-cache-lookup cache nil))
                       (lambda () (http-cache-store cache nil response))
                       (lambda () (http-cache-store cache request nil))))
      (let ((caught nil))
        (handler-case (funcall call)
          (http-protocol-error (condition) (setf caught condition)))
        (ensure-true (typep caught 'http-protocol-error))
        (ensure-equal :client (http-error-operation caught))))))

(deftest client-cache-head-fallback-and-clear
  (let* ((cache (make-http-cache :clock-function (lambda () 1000)))
         (get (make-http-request :method "GET" :uri "http://example.test/cache"))
         (head (make-http-request :method "HEAD" :uri "http://example.test/cache"))
         (post (make-http-request :method "POST" :uri "http://example.test/cache"))
         (body (ascii "cached")))
    (ensure-true
     (http-cache-store cache get
                       (make-http-response
                        :status 200 :body body
                        :headers (list (make-http-header "Cache-Control" "max-age=60")
                                       (make-http-header "Content-Length" "6")))
                       :now 1000))
    (multiple-value-bind (response state entry reason)
        (http-cache-lookup cache head :now 1001)
      (ensure-equal :fresh state)
      (ensure-equal 0 (length (http-response-body response)))
      (ensure-equal "6" (http-header-value (http-response-headers response) "Content-Length"))
      (ensure-true entry)
      (ensure-false reason))
    (multiple-value-bind (response state) (http-cache-lookup cache get :now 1001)
      (ensure-equal :fresh state)
      (ensure-octets-equal body (http-response-body response)))
    (multiple-value-bind (response state entry reason)
        (http-cache-lookup cache post :now 1001)
      (ensure-false response)
      (ensure-equal :miss state)
      (ensure-false entry)
      (ensure-equal :method reason))
    (ensure-true (eq cache (http-cache-clear cache)))
    (ensure-false (http-cache-entries cache))
    (multiple-value-bind (response state entry reason)
        (http-cache-lookup cache get :now 1001)
      (ensure-false response)
      (ensure-equal :miss state)
      (ensure-false entry)
      (ensure-equal :miss reason))))

(deftest client-cache-invalidation-removes-all-methods
  (let* ((cache (make-http-cache :clock-function (lambda () 1000)))
         (uri "http://example.test/cache")
         (requests (loop for method in '("GET" "HEAD")
                         collect (make-http-request :method method :uri uri))))
    (dolist (request requests)
      (ensure-true
       (http-cache-store cache request
                         (make-http-response
                          :status 200
                          :headers (list (make-http-header "Cache-Control" "max-age=60")))
                         :now 1000)))
    (ensure-equal 2 (length (http-cache-entries cache)))
    (ensure-true (eq cache (http-cache-invalidate cache uri)))
    (dolist (request requests)
      (multiple-value-bind (response state entry reason)
          (http-cache-lookup cache request :now 1001)
        (ensure-false response)
        (ensure-equal :miss state)
        (ensure-false entry)
        (ensure-equal :miss reason)))))

(deftest client-cache-escaped-directives-and-empty-vary
  (let* ((cache (make-http-cache :clock-function (lambda () 1000)))
         (request (make-http-request :method "GET" :uri "http://example.test/cache")))
    (ensure-true
     (http-cache-store
      cache request
      (make-http-response
       :status 200 :body (ascii "cached")
       :headers (list (make-http-header "Cache-Control"
                                        "max-age=\"\\6\\0\", extension=\"x\\\"y,z\"")
                      (make-http-header "Vary" "")))
      :now 1000))
    (multiple-value-bind (response state entry reason)
        (http-cache-lookup cache request :now 1059)
      (ensure-equal :fresh state)
      (ensure-octets-equal (ascii "cached") (http-response-body response))
      (ensure-true entry)
      (ensure-false reason))
    (multiple-value-bind (response state entry reason)
        (http-cache-lookup cache request :now 1060)
      (ensure-false response)
      (ensure-equal :stale state)
      (ensure-true entry)
      (ensure-equal :stale reason))))

(deftest client-cache-empty-fields-and-duplicate-vary
  (let* ((cache (make-http-cache))
         (request (make-http-request :method "GET" :uri "https://example.test/fields"
                                     :headers (list (make-http-header "Cookie" "sid=1")
                                                    (make-http-header "Authorization" "Basic test")))))
    (ensure-true
     (http-cache-store
      cache request
      (make-http-response :status 200 :body (ascii "cached")
                          :headers (list (make-http-header "Cache-Control" "max-age=60, no-cache=\", X-Secret, \"")
                                         (make-http-header "Vary" "Cookie, cookie, Authorization, AUTHORIZATION")
                                         (make-http-header "Connection" ", X-Hop, ")
                                         (make-http-header "X-Hop" "drop")
                                         (make-http-header "X-Secret" "drop")
                                         (make-http-header "X-Public" "keep")))
      :now 1000))
    (let* ((entries (http-cache-entries cache))
           (entry (first entries)))
      (ensure-equal 1 (length entries))
      (ensure-equal '("cookie" "authorization") (http-cache-entry-vary entry))
      (let ((response (http-cache-entry-response entry)))
        (ensure-octets-equal (ascii "cached") (http-response-body response))
        (ensure-false (http-header-value (http-response-headers response) "Connection"))
        (ensure-false (http-header-value (http-response-headers response) "X-Hop"))
        (ensure-false (http-header-value (http-response-headers response) "X-Secret"))
        (ensure-equal "keep" (http-header-value (http-response-headers response) "X-Public"))))
    (multiple-value-bind (response state entry reason) (http-cache-lookup cache request :now 1001)
      (ensure-false response)
      (ensure-equal :stale state)
      (ensure-true entry)
      (ensure-equal :stale reason))
    (let ((other (make-http-request :method "GET" :uri "https://example.test/fields"
                                   :headers (list (make-http-header "Cookie" "sid=2")
                                                  (make-http-header "Authorization" "Basic test")))))
      (ensure-equal :miss (nth-value 1 (http-cache-lookup cache other :now 1001))))))

(deftest client-cache-request-delta-validation-and-stale-min-fresh
  (let* ((cache (make-http-cache))
         (uri "https://example.test/delta")
         (request (make-http-request :method "GET" :uri uri)))
    (ensure-true
     (http-cache-store cache request
                       (make-http-response :status 200 :body (ascii "cached")
                                           :headers (list (make-http-header "Cache-Control" "max-age=60")))
                       :now 1000))
    (dolist (directives '("max-age" "max-age=-1" "max-age=bad"
                          "min-fresh" "min-fresh=-1" "min-fresh=bad"
                          "max-age=60, max-age=-1" "min-fresh=1, min-fresh=bad"))
      (multiple-value-bind (response state entry reason)
          (http-cache-lookup cache (make-http-request :method "GET" :uri uri
                                                     :headers (list (make-http-header "Cache-Control" directives)))
                             :now 1001)
        (ensure-false response)
        (ensure-equal :stale state)
        (ensure-true entry)
        (ensure-equal :request reason)))
    (let ((valid (make-http-request :method "GET" :uri uri
                                    :headers (list (make-http-header "Cache-Control"
                                                                    "max-age=10, max-age=5, min-fresh=1, min-fresh=59")))))
      (multiple-value-bind (response state) (http-cache-lookup cache valid :now 1001)
        (ensure-equal :fresh state)
        (ensure-octets-equal (ascii "cached") (http-response-body response))))
    (dolist (entry '(("max-stale=1" :stale-allowed) ("max-stale=1, min-fresh=0" :stale)))
      (destructuring-bind (directives expected) entry
        (multiple-value-bind (response state)
            (http-cache-lookup cache (make-http-request :method "GET" :uri uri
                                                       :headers (list (make-http-header "Cache-Control" directives)))
                               :now 1061)
          (ensure-equal expected state)
          (if response
              (ensure-octets-equal (ascii "cached") (http-response-body response))
              (ensure-false response)))))))

(deftest client-cache-immutable-reload-negative-boundaries
  (dolist (entry '(("https" "max-age=60, immutable" "max-age=0" 1001 :fresh)
                   ("https" "max-age=60" "max-age=0" 1001 :stale)
                   ("http" "max-age=60, immutable" "max-age=0" 1001 :stale)
                   ("https" "max-age=60, immutable" "max-age=0, min-fresh=60" 1001 :stale)
                   ("https" "max-age=60, immutable" "max-age=0, no-cache" 1001 :stale)
                   ("https" "max-age=60, immutable" "max-age=0" 1060 :stale)))
    (destructuring-bind (scheme response-directives request-directives now expected) entry
      (let* ((cache (make-http-cache))
             (uri (format nil "~A://example.test/immutable-boundary" scheme))
             (stored (make-http-request :method "GET" :uri uri))
             (request (make-http-request :method "GET" :uri uri
                                         :headers (list (make-http-header "Cache-Control" request-directives)))))
        (ensure-true
         (http-cache-store cache stored
                           (make-http-response :status 200 :body (ascii "cached")
                                               :headers (list (make-http-header "Cache-Control" response-directives)))
                           :now 1000))
        (multiple-value-bind (response state found reason) (http-cache-lookup cache request :now now)
          (ensure-equal expected state)
          (ensure-true found)
          (if (eq expected :fresh)
              (progn (ensure-false reason)
                     (ensure-octets-equal (ascii "cached") (http-response-body response)))
              (progn (ensure-equal (if (= now 1060) :stale :request) reason)
                     (ensure-false response))))))))

(deftest client-cache-swr-request-and-window-boundaries
  (dolist (entry '((nil 1011 t) (nil 1012 nil)
                   ("max-age=100" 1011 nil) ("min-fresh=0" 1011 nil)))
    (destructuring-bind (directives now stale-p) entry
      (let* ((calls 0)
             (scheduled nil)
             (delivered nil)
             (cache (make-http-cache :clock-function (lambda () now)))
             (client (make-http-client
                      :cache cache
                      :on-response (lambda (response request attempt)
                                     (declare (ignore request attempt))
                                     (push (http-response-status response) delivered))
                      :stale-while-revalidate-scheduler
                      (lambda (client request response revalidate)
                        (declare (ignore client))
                        (ensure-equal "GET" (http-request-method request))
                        (ensure-octets-equal (ascii "cached") (http-response-body response))
                        (ensure-true (functionp revalidate))
                        (setf scheduled revalidate)
                        t)
                      :transport-function
                      (lambda (request &key &allow-other-keys)
                        (incf calls)
                        (ensure-equal "\"v1\"" (http-header-value (http-request-headers request) "If-None-Match"))
                        (make-http-response :status 200 :body (ascii "network")
                                            :headers (list (make-http-header "Cache-Control" "max-age=60"))))))
             (uri "https://example.test/swr-boundary")
             (stored (http-client-request client "GET" uri))
             (request (http-client-request client "GET" uri
                                           :headers (and directives (list (make-http-header "Cache-Control" directives))))))
        (ensure-true
         (http-cache-store cache stored
                           (make-http-response :status 200 :body (ascii "cached")
                                               :headers (list (make-http-header "Cache-Control" "max-age=1, stale-while-revalidate=10")
                                                              (make-http-header "ETag" "\"v1\"")))
                           :now 1000))
        (let ((response (http-client-send client request)))
          (ensure-equal 200 (http-response-status response))
          (ensure-octets-equal (ascii (if stale-p "cached" "network")) (http-response-body response)))
        (ensure-equal '(200) delivered)
        (ensure-equal (if stale-p 0 1) calls)
        (ensure-equal stale-p (not (null scheduled)))
        (when scheduled
          (let ((revalidate scheduled))
            (setf scheduled nil)
            (ensure-octets-equal (ascii "network") (http-response-body (funcall revalidate)))))
        (ensure-equal 1 calls)
        (ensure-false scheduled)
        (multiple-value-bind (response state) (http-cache-lookup cache stored :now now)
          (ensure-equal :fresh state)
          (ensure-octets-equal (ascii "network") (http-response-body response)))))))
(deftest client-cache-head-foreign-key
  (let ((cache (make-http-cache :clock-function (lambda () 1000))))
    (dolist (uri '("http://example.test/a" "http://example.test/b"))
      (ensure-true
       (http-cache-store
        cache (make-http-request :method "GET" :uri uri)
        (make-http-response :status 200 :body (ascii "cached") :headers
                            (list (make-http-header "Cache-Control" "max-age=60")
                                  (make-http-header "Content-Length" "6")))
        :now 1000)))
    (ensure-equal 2 (length (http-cache-entries cache)))
    (multiple-value-bind (response state entry reason)
        (http-cache-lookup cache (make-http-request :method "HEAD" :uri "http://example.test/a")
                           :now 1001)
      (ensure-equal :fresh state)
      (ensure-equal 200 (http-response-status response))
      (ensure-equal 0 (length (http-response-body response)))
      (ensure-equal "6" (http-header-value (http-response-headers response) "Content-Length"))
      (ensure-equal "GET" (http-cache-entry-method entry))
      (ensure-equal "http://example.test/a" (http-cache-entry-key entry))
      (ensure-false reason))
    (multiple-value-bind (response state)
        (http-cache-lookup cache (make-http-request :method "GET" :uri "http://example.test/a")
                           :now 1001)
      (ensure-equal :fresh state)
      (ensure-octets-equal (ascii "cached") (http-response-body response)))
    (multiple-value-bind (response state entry reason)
        (http-cache-lookup cache (make-http-request :method "HEAD" :uri "http://example.test/missing")
                           :now 1001)
      (ensure-false response)
      (ensure-equal :miss state)
      (ensure-false entry)
      (ensure-equal :miss reason))))
