(in-package #:http-kit/test)

(deftest client-contract-boundary-conflicts
  (let ((calls 0))
    (let* ((callback (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (incf calls)))
           (pool (make-http-connection-pool :open-stream callback)))
      (flet ((reject (options detail)
               (let ((condition
                       (handler-case (apply #'make-http-client options)
                         (http-protocol-error (caught) caught))))
                 (ensure-true (typep condition 'http-protocol-error))
                 (ensure-equal :client (http-error-operation condition))
                 (ensure-equal detail (http-protocol-error-detail condition))
                 (ensure-equal 0 calls))))
        (reject (list :transport-function callback :open-stream callback)
                (list callback callback nil nil))
        (reject (list :transport-function callback :connection-pool pool)
                (list callback nil pool nil))
        (reject (list :open-stream callback :connection-pool pool)
                (list nil callback pool nil))
        (reject (list :close-stream callback :connection-pool pool)
                (list nil nil pool callback))
        (reject (list :close-stream callback) nil)
        (dolist (boundary (list (list :transport-function callback)
                               (list :connection-pool pool)))
          (reject (append boundary (list :tls-upgrade callback))
                  (list callback nil))
          (reject (append boundary (list :resolve-host callback))
                  (list nil callback))))
      (ensure-true (http-client-p (make-http-client)))
      (ensure-true (http-client-p
                    (make-http-client :transport-function callback)))
      (ensure-true (http-client-p
                    (make-http-client :connection-pool pool)))
      (ensure-true (http-client-p
                    (make-http-client :open-stream callback :close-stream callback
                                      :tls-upgrade callback :resolve-host callback)))
      (ensure-equal 0 calls))))

(deftest client-contract-function-options
  (let ((calls 0))
    (let ((callback (lambda (&rest arguments)
                      (declare (ignore arguments))
                      (incf calls))))
      (flet ((reject (key value)
               (let ((condition
                       (handler-case
                           (apply #'make-http-client
                                  (append (unless (member key '(:transport-function
                                                               :open-stream))
                                            (list :open-stream callback))
                                          (list key value)))
                         (http-protocol-error (caught) caught))))
                 (ensure-true (typep condition 'http-protocol-error))
                 (ensure-equal :client (http-error-operation condition))
                 (ensure-equal value (http-protocol-error-detail condition))
                 (ensure-equal 0 calls))))
        (dolist (key '(:transport-function :http3-transport-function :open-stream
                       :close-stream :clock-function :wall-clock-function
                       :sleep-function :random-function :auth-provider
                       :challenge-auth-provider :proxy-challenge-auth-provider
                       :stale-while-revalidate-scheduler :tls-upgrade :resolve-host
                       :on-request :on-response))
          (reject key :not-a-function))
        (dolist (key '(:clock-function :wall-clock-function
                       :sleep-function :random-function))
          (reject key nil)))
      (ensure-true
       (http-client-p
        (make-http-client :open-stream callback :close-stream callback
                          :http3-transport-function callback
                          :clock-function callback :wall-clock-function callback
                          :sleep-function callback :random-function callback
                          :auth-provider callback :challenge-auth-provider callback
                          :proxy-challenge-auth-provider callback
                          :stale-while-revalidate-scheduler callback
                          :tls-upgrade callback :resolve-host callback
                          :on-request callback :on-response callback)))
      (ensure-equal 0 calls))))

(deftest client-contract-scalar-options
  (let ((calls 0))
    (let ((transport (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (incf calls))))
      (flet ((reject (key value)
               (let ((condition
                       (handler-case
                           (apply #'make-http-client :transport-function transport
                                  (list key value))
                         (http-protocol-error (caught) caught))))
                 (ensure-true (typep condition 'http-protocol-error))
                 (ensure-equal :client (http-error-operation condition))
                 (ensure-equal value (http-protocol-error-detail condition))
                 (ensure-equal 0 calls))))
        (dolist (value '(-1 :bad #C(1 1)))
          (reject :default-timeout value))
        (reject :resolve-host-style :other)
        (dolist (key '(:max-header-bytes :max-body-bytes))
          (dolist (value '(-1 1/2 :bad)) (reject key value)))
        (dolist (value '(0 -1 1/2)) (reject :max-fields value))
        (dolist (value '(1 :yes)) (reject :automatic-decompression-p value)))
      (dolist (timeout '(nil 0 1/2))
        (let ((client
                (make-http-client :transport-function transport
                                  :default-timeout timeout :max-header-bytes 0
                                  :max-body-bytes 0 :max-fields 1
                                  :automatic-decompression-p nil
                                  :resolve-host-style :host-only)))
          (ensure-true (http-client-p client))
          (ensure-equal timeout (http-client-default-timeout client))))
      (ensure-true
       (http-client-p
        (make-http-client :transport-function transport
                          :max-header-bytes nil :max-body-bytes nil
                          :max-fields nil :automatic-decompression-p t)))
      (ensure-equal 0 calls))))

(deftest client-contract-store-policy-options
  (let ((calls 0))
    (let ((transport (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (incf calls))))
      (flet ((reject (key value)
               (let ((condition
                       (handler-case
                           (apply #'make-http-client
                                  (append (unless (eq key :connection-pool)
                                            (list :transport-function transport))
                                          (list key value)))
                         (http-protocol-error (caught) caught))))
                 (ensure-true (typep condition 'http-protocol-error))
                 (ensure-equal :client (http-error-operation condition))
                 (ensure-equal value (http-protocol-error-detail condition))
                 (ensure-equal 0 calls))))
        (dolist (key '(:connection-pool :cookie-jar :cache :strict-transport-store
                       :alternative-service-store :redirect-policy :retry-policy))
          (reject key :wrong-type))
        (dolist (key '(:cookie-jar :redirect-policy :retry-policy))
          (reject key nil)))
      (ensure-true
       (http-client-p
        (make-http-client :transport-function transport
                          :cookie-jar (make-http-cookie-jar)
                          :cache (make-http-cache)
                          :strict-transport-store (make-http-strict-transport-store)
                          :alternative-service-store
                          (make-http-alternative-service-store)
                          :redirect-policy (make-http-redirect-policy)
                          :retry-policy (make-http-retry-policy))))
      (ensure-true
       (http-client-p
        (make-http-client :transport-function transport :cache nil
                          :strict-transport-store nil :alternative-service-store nil)))
      (ensure-equal 0 calls))))

(deftest client-contract-decoder-alist
  (let ((calls 0))
    (let ((callback (lambda (&rest arguments)
                      (declare (ignore arguments))
                      (incf calls))))
      (dolist (decoders
               (list :bad #(1) '(invalid) '((42 . invalid))
                     (list (cons "" callback))
                     (list (cons "a b" callback))
                     '(("gzip" . invalid))
                     (list (list "gzip" callback))))
        (let ((condition
                (handler-case
                    (make-http-client :transport-function callback
                                      :content-decoders decoders)
                  (http-protocol-error (caught) caught))))
          (ensure-true (typep condition 'http-protocol-error))
          (ensure-equal :client (http-error-operation condition))
          (ensure-equal decoders (http-protocol-error-detail condition))
          (ensure-equal 0 calls)))
      (dolist (decoders (list nil (list (cons "custom-codec" callback))))
        (ensure-true
         (http-client-p
          (make-http-client :transport-function callback :content-decoders decoders))))
      (ensure-equal 0 calls))))

(deftest client-contract-send-method-uri-options
  (let ((calls 0) (seen nil) (options nil))
    (let ((client
            (make-http-client
             :default-headers '(("X-Default" . "kept") ("X-Override" . "old"))
             :transport-function
             (lambda (request &rest arguments &key &allow-other-keys)
               (incf calls)
               (setf seen request options arguments)
               (make-http-response :status 204)))))
      (multiple-value-bind (response request)
          (http-client-send client :post "http://contract.example/upload"
                            :headers '(("x-override" . "new") ("X-Request" . "present"))
                            :trailers '(("X-Trailer" . "done"))
                            :body #(65 0 255) :protocol-version "HTTP/1.1"
                            :timeout 2)
        (ensure-equal 204 (http-response-status response))
        (ensure-equal 0 (length (http-response-body response)))
        (ensure-equal
         (list (http-request-method request)
               (http-request-protocol-version request)
               (http-request-headers request)
               (http-request-trailers request)
               (http-request-body request))
         (list (http-request-method seen)
               (http-request-protocol-version seen)
               (http-request-headers seen)
               (http-request-trailers seen)
               (http-request-body seen)))
        (ensure-equal "POST" (http-request-method request))
        (ensure-equal "HTTP/1.1" (http-request-protocol-version request))
        (ensure-octets-equal #(65 0 255) (http-request-body request))
        (ensure-equal "kept" (http-header-value (http-request-headers request) "x-default"))
        (ensure-equal "new" (http-header-value (http-request-headers request) "x-override"))
        (ensure-equal "present" (http-header-value (http-request-headers request) "x-request"))
        (ensure-equal "done" (http-header-value (http-request-trailers request) "x-trailer"))
        (dolist (key '(:headers :trailers :body :protocol-version))
          (ensure-false (member key options)))
        (ensure-true (getf options :timeout))
        (ensure-equal 1 calls)))))

(deftest client-contract-send-uri-method-options
  (let ((calls 0) (seen nil) (options nil))
    (let ((client
            (make-http-client
             :transport-function
             (lambda (request &rest arguments &key &allow-other-keys)
               (incf calls)
               (setf seen request options arguments)
               (make-http-response :status 204)))))
      (multiple-value-bind (response request)
          (http-client-send client "http://contract.example/upload" :method 'put
                            :headers '(("X-Request" . "present"))
                            :trailers '(("X-Trailer" . "done"))
                            :body "content" :protocol-version "HTTP/1.0")
        (ensure-equal 204 (http-response-status response))
        (ensure-equal 0 (length (http-response-body response)))
        (ensure-equal
         (list (http-request-method request)
               (http-request-protocol-version request)
               (http-request-headers request)
               (http-request-trailers request)
               (http-request-body request))
         (list (http-request-method seen)
               (http-request-protocol-version seen)
               (http-request-headers seen)
               (http-request-trailers seen)
               (http-request-body seen)))
        (ensure-equal "PUT" (http-request-method request))
        (ensure-equal "HTTP/1.0" (http-request-protocol-version request))
        (ensure-octets-equal (ascii "content") (http-request-body request))
        (ensure-equal "present" (http-header-value (http-request-headers request) "x-request"))
        (ensure-equal "done" (http-header-value (http-request-trailers request) "x-trailer"))
        (dolist (key '(:method :headers :trailers :body :protocol-version))
          (ensure-false (member key options)))
        (ensure-equal 1 calls)))))

(deftest client-contract-send-invalid-inputs
  (let ((calls 0))
    (let ((client
            (make-http-client
             :transport-function
             (lambda (request &rest arguments &key &allow-other-keys)
               (declare (ignore request arguments))
               (incf calls)
               (make-http-response :status 204))))
          (uri "http://contract.example/"))
      (flet ((reject (arguments operation detail)
               (let ((condition
                       (handler-case (apply #'http-client-send client arguments)
                         (http-protocol-error (caught) caught))))
                 (ensure-true (typep condition 'http-protocol-error))
                 (ensure-equal operation (http-error-operation condition))
                 (ensure-equal detail (http-protocol-error-detail condition))
                 (ensure-equal 0 calls))))
        (reject (list 17 uri) :client 17)
        (reject (list "B AD" uri) :request "B AD")
        (reject (list "GET" uri :body) :client '(:body))
        (reject (list "GET" uri 7 nil) :client '(7 nil))
        (reject (list uri :method 17) :client 17)
        (reject (list "GET" uri :body 17) :client 17)
        (reject (list "GET" uri :body #(256)) :client 256)
        (reject (list "GET" uri :body #(invalid)) :client 'invalid)
        (let ((body (make-array '(1 1) :initial-element 0)))
          (reject (list "GET" uri :body body) :client body))
        (reject (list "GET" uri :protocol-version "HTTP/9") :request "HTTP/9")
        (reject (list "TRACE" uri :body "x") :request :trace-content))
      (dolist (case (list (list :not-a-client "GET" uri :not-a-client)
                         (list client "GET" 17 17)))
        (destructuring-bind (target method target-uri detail) case
          (let ((condition
                  (handler-case (http-client-request target method target-uri)
                    (http-protocol-error (caught) caught))))
            (ensure-true (typep condition 'http-protocol-error))
            (ensure-equal :client (http-error-operation condition))
            (ensure-equal detail (http-protocol-error-detail condition))
            (ensure-equal 0 calls))))
      (multiple-value-bind (response request) (http-client-send client uri)
        (ensure-equal 204 (http-response-status response))
        (ensure-equal "GET" (http-request-method request))
        (ensure-equal 0 (length (http-request-body request)))
        (ensure-equal 1 calls)))))

(deftest client-contract-runtime-transport-return-type
  (dolist (invalid-p '(t nil))
    (let ((calls 0)
          (response-calls 0)
          (caught nil)
          (result nil)
          (expected (make-http-response :status 204)))
      (let ((client
              (make-http-client
               :cache nil
               :transport-function
               (lambda (request &key &allow-other-keys)
                 (declare (ignore request))
                 (incf calls)
                 (unless invalid-p expected))
               :on-response
               (lambda (response request attempt)
                 (declare (ignore response request attempt))
                 (incf response-calls)))))
        (handler-case
            (setf result (http-client-send client "http://contract.example/"))
          (http-protocol-error (condition) (setf caught condition)))
        (if invalid-p
            (progn
              (ensure-true (typep caught 'http-protocol-error))
              (ensure-equal :client (http-error-operation caught))
              (ensure-false (http-protocol-error-detail caught))
              (ensure-false result))
            (progn
              (ensure-false caught)
              (ensure-true (eq expected result))
              (ensure-equal 204 (http-response-status result))
              (ensure-equal 0 (length (http-response-body result)))))
        (ensure-equal 1 calls)
        (ensure-equal (if invalid-p 0 1) response-calls)))))

(deftest client-contract-runtime-connection-retry-exhaustion
  (dolist (max-attempts '(2 1))
    (let ((calls 0)
          (response-calls 0)
          (caught nil)
          (result nil)
          (first-condition
            (make-condition 'http-connection-error
                            :message "Fixture connection failure."
                            :operation :connect))
          (last-condition
            (make-condition 'http-connection-error
                            :message "Fixture connection failure."
                            :operation :connect)))
      (let ((client
              (make-http-client
               :cache nil
               :retry-policy
               (make-http-retry-policy
                :max-attempts max-attempts :base-delay 0 :max-delay 0
                :retry-on-connection-error-p t)
               :transport-function
               (lambda (request &key &allow-other-keys)
                 (declare (ignore request))
                 (incf calls)
                 (error (if (= calls 1) first-condition last-condition)))
               :on-response
               (lambda (response request attempt)
                 (declare (ignore response request attempt))
                 (incf response-calls)))))
        (handler-case
            (setf result (http-client-send client "http://contract.example/"))
          (http-error (condition) (setf caught condition)))
        (ensure-true
         (typep caught (if (= max-attempts 2)
                          'http-retry-exhausted
                          'http-connection-error)))
        (ensure-equal (if (= max-attempts 2) :retry :connect)
                      (http-error-operation caught))
        (ensure-equal max-attempts calls)
        (ensure-equal 0 response-calls)
        (ensure-false result)
        (if (= max-attempts 2)
            (progn
              (ensure-equal 2 (http-retry-exhausted-attempts caught))
              (ensure-true
               (eq last-condition (http-retry-exhausted-last-condition caught)))
              (ensure-false (http-retry-exhausted-last-response caught)))
            (ensure-true (eq first-condition caught)))))))

(deftest client-contract-alt-svc-protocol-encoding
  (dolist (case '(("h%2F3" t) ("h%80" t) ("h%25" t)
                  ("h%2f3" nil) ("h%G0" nil) ("h%0G" nil)
                  ("h%A" nil) ("h%68" nil) ("h/3" nil)))
    (destructuring-bind (protocol valid-p) case
      (let* ((clock-calls 0)
             (uri (parse-http-uri "https://origin.example/resource"))
             (store (make-http-alternative-service-store
                     :clock-function (lambda () (incf clock-calls) 100)))
             (response (make-http-response
                        :status 204
                        :headers (list (make-http-header
                                        "Alt-Svc"
                                        (format nil "~A=\":443\"; ma=60"
                                                protocol))))))
        (ensure-true
         (eq store (http-alternative-service-store-note-response
                    store uri response)))
        (let ((services (http-alternative-service-store-services store uri)))
          (ensure-equal (if valid-p 1 0) (length services))
          (ensure-equal 2 clock-calls)
          (when valid-p
            (let ((service (first services)))
              (ensure-equal protocol (http-alternative-service-protocol-id service))
              (ensure-equal "origin.example" (http-alternative-service-host service))
              (ensure-equal 443 (http-alternative-service-port service))
              (ensure-equal 160 (http-alternative-service-expires-at service))
              (ensure-false (http-alternative-service-persist-p service)))))))))

(deftest client-contract-alt-svc-authority-boundaries
  (dolist (case '((":1" "origin.example" 1)
                  (":65535" "origin.example" 65535)
                  ("alt.example:443" "alt.example" 443)
                  ("" nil nil) ("alt.example" nil nil)
                  (":0" nil nil) (":65536" nil nil)
                  ("alt.example:443/extra" nil nil)))
    (destructuring-bind (authority host port) case
      (let* ((clock-calls 0)
             (uri (parse-http-uri "https://origin.example/resource"))
             (store (make-http-alternative-service-store
                     :clock-function (lambda () (incf clock-calls) 100)))
             (response (make-http-response
                        :status 204
                        :headers (list (make-http-header
                                        "Alt-Svc"
                                        (format nil "h3=~S; ma=60" authority))))))
        (ensure-true
         (eq store (http-alternative-service-store-note-response
                    store uri response)))
        (let ((services (http-alternative-service-store-services store uri)))
          (ensure-equal (if host 1 0) (length services))
          (ensure-equal 2 clock-calls)
          (when host
            (let ((service (first services)))
              (ensure-equal "h3" (http-alternative-service-protocol-id service))
              (ensure-equal host (http-alternative-service-host service))
              (ensure-equal port (http-alternative-service-port service))
              (ensure-equal 160 (http-alternative-service-expires-at service))
              (ensure-false (http-alternative-service-persist-p service)))))))))

(deftest client-contract-alt-svc-invalid-context
  (let* ((clock-calls 0)
         (uri (parse-http-uri "https://origin.example/resource"))
         (store (make-http-alternative-service-store
                 :clock-function (lambda () (incf clock-calls) 100)))
         (response (make-http-response
                    :status 204
                    :headers (list (make-http-header "Alt-Svc" "h3=\":443\"; ma=60")))))
    (flet ((reject (function detail)
             (let ((caught nil))
               (handler-case (funcall function)
                 (http-protocol-error (condition) (setf caught condition)))
               (ensure-true (typep caught 'http-protocol-error))
               (ensure-equal :client (http-error-operation caught))
               (ensure-equal detail (http-protocol-error-detail caught))
               (ensure-equal 0 clock-calls))))
      (reject (lambda () (http-alternative-service-store-services nil uri)) nil)
      (reject (lambda () (http-alternative-service-store-remove nil uri nil)) nil)
      (reject (lambda () (http-alternative-service-store-network-changed nil)) nil)
      (reject (lambda () (http-alternative-service-store-clear nil)) nil)
      (reject (lambda () (http-alternative-service-store-note-response nil uri response))
              (list nil uri response))
      (reject (lambda () (http-alternative-service-store-note-response store uri nil))
              (list store uri nil))
      (reject (lambda () (http-alternative-service-store-remove store uri nil)) nil))
    (ensure-true
     (eq store (http-alternative-service-store-note-response store uri response)))
    (ensure-equal 1 clock-calls)
    (ensure-equal 1 (length (http-alternative-service-store-services store uri)))
    (ensure-equal 2 clock-calls)))

(deftest client-contract-alt-svc-origin-lifecycle
  (let* ((store (make-http-alternative-service-store :clock-function (lambda () 100)))
         (first-uri (parse-http-uri "https://first.example/resource"))
         (second-uri (parse-http-uri "https://second.example/resource"))
         (response (make-http-response
                    :status 204
                    :headers (list (make-http-header "Alt-Svc" "h3=\":443\"; ma=60")))))
    (ensure-true
     (eq store (http-alternative-service-store-note-response store first-uri response)))
    (ensure-true
     (eq store (http-alternative-service-store-note-response store second-uri response)))
    (let* ((first-list (http-alternative-service-store-services store first-uri))
           (first-service (first first-list))
           (second-list (http-alternative-service-store-services store second-uri))
           (second-service (first second-list)))
      (ensure-equal 1 (length first-list))
      (ensure-equal 1 (length second-list))
      (ensure-true
       (eq store (http-alternative-service-store-remove store first-uri second-service)))
      (ensure-true
       (eq first-service (first (http-alternative-service-store-services store first-uri))))
      (ensure-true
       (eq second-service (first (http-alternative-service-store-services store second-uri))))
      (setf (car first-list) nil)
      (ensure-true
       (eq first-service (first (http-alternative-service-store-services store first-uri))))
      (ensure-true
       (eq store (http-alternative-service-store-remove store first-uri first-service)))
      (ensure-false (http-alternative-service-store-services store first-uri))
      (ensure-true
       (eq second-service (first (http-alternative-service-store-services store second-uri))))
      (ensure-true
       (eq store (http-alternative-service-store-note-response store first-uri response)))
      (ensure-true (eq store (http-alternative-service-store-clear store first-uri)))
      (ensure-false (http-alternative-service-store-services store first-uri))
      (ensure-true
       (eq second-service (first (http-alternative-service-store-services store second-uri))))
      (ensure-true (eq store (http-alternative-service-store-clear store first-uri)))
      (ensure-true
       (eq second-service (first (http-alternative-service-store-services store second-uri))))
      (ensure-true
       (eq store (http-alternative-service-store-note-response store first-uri response)))
      (ensure-true (eq store (http-alternative-service-store-clear store)))
      (ensure-false (http-alternative-service-store-services store first-uri))
      (ensure-false (http-alternative-service-store-services store second-uri))
      (ensure-true
       (eq store (http-alternative-service-store-remove store first-uri first-service)))
      (ensure-false (http-alternative-service-store-services store first-uri)))))

(deftest client-contract-streaming-options-before-transport
  (let ((transport-calls 0) (producer-calls 0) (factory-calls 0)
        (request-calls 0) (response-calls 0))
    (let* ((producer (lambda (limit)
                       (ensure-equal 65536 limit)
                       (incf producer-calls)
                       nil))
           (factory (lambda () (incf factory-calls) producer))
           (request (make-http-request :method "POST" :uri "https://example.test/upload"))
           (body-request
             (make-http-request :method "POST" :uri "https://example.test/upload"
                                :body (make-array 1 :element-type '(unsigned-byte 8)
                                                   :initial-element 90)))
           (client
             (make-http-client
              :on-request (lambda (request attempt)
                            (declare (ignore request attempt)) (incf request-calls))
              :on-response (lambda (response request attempt)
                             (declare (ignore response request attempt))
                             (incf response-calls))
              :transport-function
              (lambda (request &key request-body-function request-body-length
                                 &allow-other-keys)
                (declare (ignore request))
                (incf transport-calls)
                (ensure-equal 0 request-body-length)
                (ensure-true (functionp request-body-function))
                (ensure-false (funcall request-body-function 65536))
                (make-http-response :status 204)))))
      (flet ((reject (input options detail)
               (let ((caught (handler-case (apply #'http-client-send client input options)
                               (http-protocol-error (condition) condition))))
                 (ensure-true (typep caught 'http-protocol-error))
                 (ensure-equal :client (http-error-operation caught))
                 (ensure-true (eql detail (http-protocol-error-detail caught)))
                 (ensure-equal '(0 0 0 0 0)
                               (list transport-calls producer-calls factory-calls
                                     request-calls response-calls)))))
        (reject request '(:request-body-function 42) 42)
        (reject request '(:request-body-factory 42) 42)
        (reject request (list :request-body-function producer :request-body-factory factory)
                request)
        (dolist (length '(-1 1/2))
          (reject request (list :request-body-function producer :request-body-length length)
                  length))
        (dolist (length '(0 1))
          (reject request (list :request-body-length length) length))
        (reject body-request (list :request-body-function producer) body-request)
        (reject body-request (list :request-body-factory factory) body-request))
      (dolist (options (list (list :request-body-function producer :request-body-length 0)
                            (list :request-body-factory factory :request-body-length 0)))
        (ensure-equal 204 (http-response-status (apply #'http-client-send client request options))))
      (ensure-equal '(2 2 1 2 2)
                    (list transport-calls producer-calls factory-calls
                          request-calls response-calls)))))

(deftest client-contract-send-option-policy-validation
  (let ((transport-calls 0) (request-calls 0) (response-calls 0))
    (let ((request (make-http-request :method "GET" :uri "https://example.test/"))
          (client
            (make-http-client
             :on-request (lambda (request attempt)
                           (declare (ignore request attempt)) (incf request-calls))
             :on-response (lambda (response request attempt)
                            (declare (ignore response request attempt)) (incf response-calls))
             :transport-function (lambda (request &key &allow-other-keys)
                                   (declare (ignore request))
                                   (incf transport-calls)
                                   (make-http-response :status 204)))))
      (dolist (key '(:collect-body-p :on-body-chunk :on-information
                     :redirect-policy :retry-policy))
        (let ((caught (handler-case (http-client-send client request key 42)
                        (http-protocol-error (condition) condition))))
          (ensure-true (typep caught 'http-protocol-error))
          (ensure-equal :client (http-error-operation caught))
          (ensure-equal 42 (http-protocol-error-detail caught))
          (ensure-equal '(0 0 0) (list transport-calls request-calls response-calls))))
      (dolist (collect '(nil t))
        (ensure-equal 204
                      (http-response-status
                       (http-client-send client request :collect-body-p collect
                                         :redirect-policy (make-http-redirect-policy)
                                         :retry-policy (make-http-retry-policy)))))
      (ensure-equal '(2 2 2) (list transport-calls request-calls response-calls)))))

(deftest client-contract-303-head-post-request-lifecycle
  (dolist (method '("HEAD" "POST"))
    (let* ((calls 0) (request-calls 0) (response-calls 0) (seen nil)
           (request-seen nil) (response-seen nil)
           (body (make-array 1 :element-type '(unsigned-byte 8) :initial-element 90))
           (request
             (make-http-request
              :method method :uri "https://example.test/start" :body body
              :headers (list (make-http-header "Content-Type" "application/octet-stream")
                             (make-http-header "Content-Length" "1")
                             (make-http-header "Trailer" "X-End"))
              :trailers (list (make-http-header "X-End" "done"))))
           (client
             (make-http-client
              :on-request (lambda (request attempt)
                            (declare (ignore attempt))
                            (push request request-seen)
                            (incf request-calls))
              :on-response (lambda (response request attempt)
                             (declare (ignore response attempt))
                             (push request response-seen)
                             (incf response-calls))
              :transport-function
              (lambda (current &key &allow-other-keys)
                (push current seen)
                (if (= 1 (incf calls))
                    (make-http-response :status 303
                                        :headers (list (make-http-header "Location" "/next")))
                    (make-http-response :status 204))))))
      (multiple-value-bind (response final) (http-client-send client request)
        (ensure-equal 204 (http-response-status response))
        (ensure-equal '(2 2 2) (list calls request-calls response-calls))
        (ensure-true (eq final (first request-seen)))
        (ensure-true (eq final (first response-seen)))
        ;; The transport may receive a rebuilt request with proxy headers removed.
        (dolist (current (list final (first seen)))
          (ensure-equal "/next" (http-kit:http-uri-path (http-request-uri current)))
          (ensure-equal (if (string= method "HEAD") "HEAD" "GET")
                        (http-request-method current))
          (ensure-equal (if (string= method "HEAD") 1 0)
                        (length (http-request-body current)))
          (dolist (name '("Content-Type" "Content-Length" "Trailer"))
            (if (string= method "HEAD")
                (ensure-equal (http-header-value (http-request-headers request) name)
                              (http-header-value (http-request-headers current) name))
                (ensure-false (http-header-value (http-request-headers current) name))))
          (if (string= method "HEAD")
              (ensure-equal "done" (http-header-value (http-request-trailers current) "X-End"))
              (ensure-false (http-request-trailers current))))
        (ensure-equal method (http-request-method request))
        (ensure-equal 1 (length (http-request-body request)))
        (ensure-equal "done" (http-header-value (http-request-trailers request) "X-End"))
        (ensure-equal "1" (http-header-value (http-request-headers request) "Content-Length"))))))

(deftest client-contract-redirect-downgrade-before-second-transport
  (dolist (location '("http://example.test/next" "https://example.test/next"))
    (let* ((calls 0) (request-calls 0) (response-calls 0) (seen nil)
           (request (make-http-request
                     :method "GET" :uri "https://example.test/start"
                     :headers (list (make-http-header "Authorization" "test-value"))))
           (client
             (make-http-client
              :on-request (lambda (request attempt)
                            (declare (ignore request attempt)) (incf request-calls))
              :on-response (lambda (response request attempt)
                             (declare (ignore response request attempt)) (incf response-calls))
              :transport-function
              (lambda (current &key &allow-other-keys)
                (push current seen)
                (if (= 1 (incf calls))
                    (make-http-response :status 302
                                        :headers (list (make-http-header "Location" location)))
                    (make-http-response :status 204))))))
      (if (string= location "http://example.test/next")
          (let ((caught (handler-case (http-client-send client request)
                          (http-client-error (condition) condition))))
            (ensure-true (typep caught 'http-client-error))
            (ensure-equal :redirect (http-error-operation caught))
            (ensure-equal "http"
                          (http-kit:http-uri-scheme (http-client-error-detail caught)))
            (ensure-equal "/next"
                          (http-kit:http-uri-path (http-client-error-detail caught)))
            (ensure-equal '(1 1 1) (list calls request-calls response-calls)))
          (progn
            (ensure-equal 204 (http-response-status (http-client-send client request)))
            (ensure-equal '(2 2 2) (list calls request-calls response-calls))
            (ensure-equal "test-value"
                          (http-header-value (http-request-headers (first seen)) "Authorization"))))
      (ensure-equal "test-value"
                    (http-header-value (http-request-headers request) "Authorization")))))

(deftest client-contract-cache-boundary-last-modified-304
  (dolist (modified '("Wed, 21 Oct 2015 07:28:00 GMT"
                      "Wed, 21 Oct 2015 07:29:00 GMT"))
    (let* ((original-date "Wed, 21 Oct 2015 07:28:00 GMT")
           (calls 0) (requests nil) (callbacks nil)
           (not-modified
             (make-http-response :status 304
                                 :headers (list (make-http-header "Last-Modified" modified))))
           (client
             (make-http-client
              :cache (make-http-cache :clock-function (lambda () 3000))
              :on-response (lambda (response request attempt)
                             (push (list response request attempt) callbacks))
              :transport-function
              (lambda (request &key &allow-other-keys)
                (push request requests)
                (if (= 1 (incf calls))
                    (make-http-response
                     :status 200 :body (ascii "cached")
                     :headers (list (make-http-header "Cache-Control" "max-age=0")
                                    (make-http-header "Last-Modified" original-date)))
                    not-modified)))))
      (ensure-equal 200 (http-response-status
                         (http-client-send client "GET" "https://example.test/modified")))
      (if (string= modified original-date)
          (let ((response (http-client-send client "GET" "https://example.test/modified")))
            (ensure-equal 200 (http-response-status response))
            (ensure-equal "cached" (octets-as-string (http-response-body response))))
          (let ((caught (handler-case
                            (http-client-send client "GET" "https://example.test/modified")
                          (http-protocol-error (condition) condition))))
            (ensure-true (typep caught 'http-protocol-error))
            (ensure-equal :client (http-error-operation caught))
            (ensure-true (eq not-modified (http-protocol-error-detail caught)))))
      (ensure-equal 2 calls)
      (ensure-equal original-date
                    (http-header-value (http-request-headers (first requests))
                                       "If-Modified-Since"))
      (ensure-false (http-header-value (http-request-headers (first requests))
                                      "If-None-Match"))
      (ensure-equal 2 (length callbacks))
      (ensure-true (eq not-modified (first (first callbacks))))
      (ensure-equal 1 (third (first callbacks))))))

(deftest client-contract-cache-boundary-validatorless-304
  (dolist (headers (list nil
                        (list (make-http-header "ETag" "\"new\""))
                        (list (make-http-header "Last-Modified"
                                                "Wed, 21 Oct 2015 07:28:00 GMT"))))
    (let* ((calls 0) (requests nil)
           (not-modified (make-http-response :status 304 :headers headers))
           (client
             (make-http-client
              :cache (make-http-cache :clock-function (lambda () 3000))
              :transport-function
              (lambda (request &key &allow-other-keys)
                (push request requests)
                (if (= 1 (incf calls))
                    (make-http-response
                     :status 200 :body (ascii "cached")
                     :headers (list (make-http-header "Cache-Control" "max-age=0")))
                    not-modified)))))
      (ensure-equal 200 (http-response-status
                         (http-client-send client "GET" "https://example.test/no-validator")))
      (if (null headers)
          (let ((response (http-client-send client "GET" "https://example.test/no-validator")))
            (ensure-equal 200 (http-response-status response))
            (ensure-equal "cached" (octets-as-string (http-response-body response))))
          (let ((caught (handler-case
                            (http-client-send client "GET" "https://example.test/no-validator")
                          (http-protocol-error (condition) condition))))
            (ensure-true (typep caught 'http-protocol-error))
            (ensure-equal :client (http-error-operation caught))
            (ensure-true (eq not-modified (http-protocol-error-detail caught)))))
      (ensure-equal 2 calls)
      (ensure-false (http-header-value (http-request-headers (first requests))
                                      "If-Modified-Since"))
      (ensure-false (http-header-value (http-request-headers (first requests))
                                      "If-None-Match")))))

(deftest client-contract-cache-boundary-response-callbacks
  (let* ((now 3000) (calls 0) (callbacks nil)
         (original (make-condition 'http-connection-error :message "unavailable"))
         (cache (make-http-cache :clock-function (lambda () now)))
         (client
           (make-http-client
            :cache cache
            :retry-policy (make-http-retry-policy :retry-on-connection-error-p nil)
            :on-response (lambda (response request attempt)
                           (push (list response request attempt) callbacks))
            :transport-function
            (lambda (request &key &allow-other-keys)
              (declare (ignore request))
              (incf calls)
              (error original))))
         (request (http-client-request client "GET" "https://example.test/cached")))
    (http-cache-store
     cache request
     (make-http-response
      :status 200 :body (ascii "cached")
      :headers (list (make-http-header "Cache-Control" "max-age=1, stale-if-error=10")))
     :now now)
    (let ((response (http-client-send client request)))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "cached" (octets-as-string (http-response-body response)))
      (ensure-equal 0 calls)
      (ensure-equal 1 (length callbacks))
      (ensure-true (eq response (first (first callbacks))))
      (ensure-equal 0 (third (first callbacks))))
    (setf callbacks nil)
    (let ((response
            (http-client-send
             client "GET" "https://example.test/missing"
             :headers (list (make-http-header "Cache-Control" "only-if-cached")))))
      (ensure-equal 504 (http-response-status response))
      (ensure-equal 0 calls)
      (ensure-equal 1 (length callbacks))
      (ensure-true (eq response (first (first callbacks))))
      (ensure-equal 0 (third (first callbacks))))
    (setf now 3002 callbacks nil)
    (let ((response (http-client-send client request)))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "cached" (octets-as-string (http-response-body response)))
      (ensure-equal 1 calls)
      (ensure-equal 1 (length callbacks))
      (ensure-true (eq response (first (first callbacks))))
      (ensure-equal 1 (third (first callbacks))))
    (setf now 3020 callbacks nil)
    (let ((caught (handler-case (http-client-send client request)
                    (http-connection-error (condition) condition))))
      (ensure-true (eq original caught))
      (ensure-equal 2 calls)
      (ensure-false callbacks))))

(defun client-contract-assert-protocol-detail (thunk detail)
  (let ((caught (handler-case (funcall thunk)
                  (http-protocol-error (condition) condition))))
    (ensure-true (typep caught 'http-protocol-error))
    (ensure-equal :client (http-error-operation caught))
    (ensure-true (eq detail (http-protocol-error-detail caught)))))

(deftest client-contract-strict-redirect-policy-validation
  (dolist (value '(nil invalid -1))
    (client-contract-assert-protocol-detail
     (lambda () (make-http-redirect-policy :max-redirects value)) value))
  (dolist (value '(invalid (invalid) (299) (400)))
    (client-contract-assert-protocol-detail
     (lambda () (make-http-redirect-policy :statuses value)) value))
  (let* ((statuses (list 300 399))
         (policy (make-http-redirect-policy :max-redirects 0 :statuses statuses
                                           :allow-downgrade-p :enabled
                                           :preserve-authorization-p :enabled)))
    (ensure-equal 0 (http-redirect-policy-max-redirects policy))
    (ensure-false (eq statuses (http-redirect-policy-statuses policy)))
    (setf (first statuses) 301)
    (ensure-equal '(300 399) (http-redirect-policy-statuses policy))
    (ensure-equal t (http-redirect-policy-allow-downgrade-p policy))
    (ensure-equal t (http-redirect-policy-preserve-authorization-p policy)))
  (ensure-false (http-redirect-policy-statuses
                 (make-http-redirect-policy :statuses nil))))

(deftest client-contract-strict-retry-policy-validation
  (dolist (entry '((:max-attempts invalid) (:max-attempts 0)
                   (:methods invalid) (:methods (invalid))
                   (:statuses invalid) (:statuses (invalid))
                   (:statuses (99)) (:statuses (600))
                   (:base-delay invalid) (:base-delay -1)
                   (:max-delay invalid) (:max-delay 0)
                   (:jitter-ratio invalid) (:jitter-ratio -1) (:jitter-ratio 2)))
    (client-contract-assert-protocol-detail
     (lambda () (apply #'make-http-retry-policy entry)) (second entry)))
  (let* ((methods (list "GET")) (statuses (list 100 599))
         (policy (make-http-retry-policy
                  :max-attempts 1 :methods methods :statuses statuses
                  :base-delay 0 :max-delay 0 :jitter-ratio 1
                  :respect-retry-after-p nil :retry-on-timeout-p nil
                  :retry-on-connection-error-p :enabled)))
    (ensure-false (eq methods (http-retry-policy-methods policy)))
    (ensure-false (eq statuses (http-retry-policy-statuses policy)))
    (setf (first methods) "POST" (first statuses) 500)
    (ensure-equal '("GET") (http-retry-policy-methods policy))
    (ensure-equal '(100 599) (http-retry-policy-statuses policy))
    (ensure-equal 1 (http-retry-policy-max-attempts policy))
    (ensure-equal 0 (http-retry-policy-base-delay policy))
    (ensure-equal 0 (http-retry-policy-max-delay policy))
    (ensure-equal 1 (http-retry-policy-jitter-ratio policy))
    (ensure-false (http-retry-policy-respect-retry-after-p policy))
    (ensure-false (http-retry-policy-retry-on-timeout-p policy))
    (ensure-equal t (http-retry-policy-retry-on-connection-error-p policy))))

(deftest client-contract-strict-multipart-proxy-validation
  (dolist (value '(nil invalid ""))
    (client-contract-assert-protocol-detail
     (lambda () (make-http-multipart-part :name value :value "value")) value))
  (dolist (value (list nil 'invalid (make-array '(1 1) :initial-element 0)))
    (client-contract-assert-protocol-detail
     (lambda () (make-http-multipart-part :name "field" :value value)) value))
  (dolist (key '(:filename :content-type))
    (client-contract-assert-protocol-detail
     (lambda () (apply #'make-http-multipart-part :name "field" :value "value"
                      (list key 7))) 7))
  (let* ((value (octets))
         (part (make-http-multipart-part :name "field" :value value
                                         :filename "" :content-type "")))
    (ensure-equal "field" (http-multipart-part-name part))
    (ensure-true (eq value (http-multipart-part-value part)))
    (ensure-equal "" (http-multipart-part-filename part))
    (ensure-equal "" (http-multipart-part-content-type part)))
  (dolist (entry '((:scheme :ftp) (:host nil) (:host "") (:host invalid)
                   (:port invalid) (:port 0) (:port -1) (:port 65536)
                   (:username 7) (:password 7)))
    (client-contract-assert-protocol-detail
     (lambda () (apply #'make-http-proxy
                      (append entry
                              (unless (eq :host (first entry)) (list :host "proxy.test"))
                              (unless (eq :port (first entry)) (list :port 1)))))
     (second entry)))
  (dolist (scheme '(:http :https :socks5 :socks5h))
    (dolist (port '(1 65535))
      (let ((proxy (make-http-proxy :scheme scheme :host "PROXY.TEST" :port port
                                    :username "" :password "")))
        (ensure-equal scheme (http-proxy-scheme proxy))
        (ensure-equal "proxy.test" (http-proxy-host proxy))
        (ensure-equal port (http-proxy-port proxy))
        (ensure-equal "" (http-proxy-username proxy))
        (ensure-equal "" (http-proxy-password proxy))))))

(deftest client-contract-strict-hsts-validation-state
  (let* ((now 100) (store (make-http-strict-transport-store :clock-function (lambda () now)))
         (uri "https://example.test/") (response (make-http-response :status 200)))
    (dolist (function (list #'http-strict-transport-store-policies
                            #'http-strict-transport-store-clear
                            (lambda (value) (http-strict-transport-store-known-host-p value "example.test"))
                            (lambda (value) (http-strict-transport-store-note-response value uri response))
                            (lambda (value) (http-strict-transport-store-upgrade-uri value uri))))
      (client-contract-assert-protocol-detail (lambda () (funcall function :invalid)) :invalid))
    (dolist (host '(nil invalid ""))
      (client-contract-assert-protocol-detail
       (lambda () (http-strict-transport-store-known-host-p store host)) host))
    (client-contract-assert-protocol-detail
     (lambda () (http-strict-transport-store-note-response store uri :invalid)) :invalid)
    (flet ((note (value)
             (http-strict-transport-store-note-response
              store uri (make-http-response :status 200
                                            :headers (list (make-http-header "Strict-Transport-Security" value))))))
      (note "max-age=10; includeSubDomains")
      (let* ((policies (http-strict-transport-store-policies store)) (policy (first policies)))
        (ensure-equal 1 (length policies))
        (ensure-equal "example.test" (http-strict-transport-policy-host policy))
        (ensure-equal 110 (http-strict-transport-policy-expires-at policy))
        (ensure-true (http-strict-transport-policy-include-subdomains-p policy))
        (setf (first policies) nil)
        (dolist (value '("max-age=20; x=" "max-age=20; x=a/b"
                         "max-age=20; x=\"unterminated" "max-age=20; x=\"a\"b\""
                         "max-age=20; x=\"a\\\""))
          (note value)
          (ensure-true (eq policy (first (http-strict-transport-store-policies store)))))
        (ensure-true (http-strict-transport-store-known-host-p store "sub.example.test"))
        (setf now 110)
        (ensure-false (http-strict-transport-store-known-host-p store "example.test"))
        (ensure-false (http-strict-transport-store-policies store)))
      (note "max-age=10")
      (ensure-true (eq store (http-strict-transport-store-clear store)))
      (ensure-false (http-strict-transport-store-policies store))
      (note "max-age=10")
      (note "max-age=0")
      (ensure-false (http-strict-transport-store-known-host-p store "example.test")))))

(deftest client-contract-strict-only-if-cached-early-callbacks
  (dolist (method '("GET" "POST"))
    (let* ((calls 0) (events nil)
           (request (make-http-request :method method :uri "https://example.test/cache"
                                       :headers (list (make-http-header "Cache-Control" "only-if-cached"))))
           (client (make-http-client
                    :cache (when (string= method "POST") (make-http-cache))
                    :transport-function (lambda (request &key &allow-other-keys)
                                          (declare (ignore request)) (incf calls)
                                          (make-http-response :status 200))
                    :on-request (lambda (request attempt)
                                  (declare (ignore request attempt)) (push :request events))
                    :on-response (lambda (response effective attempt)
                                   (push (list :response response effective attempt) events)))))
      (multiple-value-bind (response effective)
          (http-client-send client request :on-body-chunk
                            (lambda (chunk) (push (list :body (copy-seq chunk)) events)))
        (ensure-equal 504 (http-response-status response))
        (ensure-equal 0 (length (http-response-body response)))
        (ensure-equal 0 calls)
        (ensure-equal 1 (length events))
        (ensure-equal :response (first (first events)))
        (ensure-true (eq response (second (first events))))
        (ensure-true (eq effective (third (first events))))
        (ensure-equal 0 (fourth (first events)))
        (ensure-equal method (http-request-method effective))
        (ensure-equal "/cache" (http-kit:http-uri-path (http-request-uri effective)))))))

(deftest client-contract-strict-redirect-limit-callback-order
  (dolist (limit '(0 1))
    (let* ((calls 0) (events nil)
           (request (make-http-request :method "GET" :uri "https://example.test/loop"))
           (client (make-http-client
                    :redirect-policy (make-http-redirect-policy :max-redirects limit)
                    :on-request (lambda (request attempt)
                                  (declare (ignore request)) (push (list :request attempt) events))
                    :on-response (lambda (response request attempt)
                                   (declare (ignore response request)) (push (list :response attempt) events))
                    :transport-function (lambda (request &key &allow-other-keys)
                                          (declare (ignore request)) (incf calls)
                                          (make-http-response :status 302
                                                              :headers (list (make-http-header "Location" "/loop"))))))
           (caught (handler-case (http-client-send client request)
                     (http-redirect-limit-exceeded (condition) condition))))
      (ensure-true (typep caught 'http-redirect-limit-exceeded))
      (ensure-equal :redirect (http-error-operation caught))
      (ensure-equal limit (http-redirect-limit-exceeded-redirects caught))
      (ensure-equal "/loop" (http-kit:http-uri-path (http-redirect-limit-exceeded-uri caught)))
      (ensure-true (eq (http-redirect-limit-exceeded-uri caught)
                       (getf (http-client-error-detail caught) :uri)))
      (ensure-equal limit (getf (http-client-error-detail caught) :redirects))
      (ensure-equal (1+ limit) calls)
      (ensure-equal (loop repeat (1+ limit)
                          append (list (list :request 1) (list :response 1)))
                    (reverse events))
      (ensure-equal "/loop" (http-kit:http-uri-path (http-request-uri request))))))

(deftest client-contract-strict-pool-opener-condition-identity
  (let* ((calls 0) (closes 0)
         (original (make-condition 'http-connection-error :message "opener failure"
                                   :operation :connect :request-sent-p nil))
         (pool (make-http-connection-pool
                :open-stream (lambda (request &key &allow-other-keys)
                               (declare (ignore request)) (incf calls) (error original))
                :close-stream (lambda (stream) (declare (ignore stream)) (incf closes)))))
    (let ((caught (handler-case
                      (http-connection-pool-send pool (make-http-request :method "GET" :uri "http://example.test/"))
                    (http-connection-error (condition) condition))))
      (ensure-true (eq original caught))
      (ensure-equal :connect (http-error-operation caught))
      (ensure-false (http-connection-error-request-sent-p caught))
      (ensure-equal 1 calls)
      (ensure-equal 0 closes)
      (ensure-equal 0 (getf (http-connection-pool-stats pool) :idle-count))
      (ensure-equal 0 (http-connection-pool-clear pool))
      (ensure-equal 0 closes))))

(deftest client-contract-strict-alt-svc-parameter-state
  (dolist (case '(("ma=30; persist=1" t t)
                  ("ma=\"30\"; persist=\"1\"" t t)
                  ("ma=30; persist=0" t nil)
                  ("ma=30; persist=\"\"" t nil)
                  ("ma=30; persist=" t nil)
                  ("ma=30; persist=a/b" t nil)
                  ("ma=30; ignored; =1" t nil)
                  ("ma=30; x=\"\"" t nil)
                  ("ma=" nil nil)
                  ("ma=\"\"" nil nil)
                  ("ma=a/b" nil nil)
                  ("ma=-1" nil nil)))
    (destructuring-bind (parameters valid-p persist-p) case
      (let* ((now 100)
             (store (make-http-alternative-service-store :clock-function (lambda () now)))
             (uri (parse-http-uri "https://origin.example/resource"))
             (other-uri (parse-http-uri "https://other.example/resource"))
             (seed (make-http-response
                    :status 200 :headers (list (make-http-header "Alt-Svc"
                                                                "h3=\":444\"; ma=60; persist=1"))))
             (response (make-http-response
                        :status 200
                        :headers (list (make-http-header "Alt-Svc"
                                                        (format nil "h3=\":443\"; ~A" parameters))
                                       (make-http-header "Age" "2")))))
        (http-alternative-service-store-note-response store uri seed)
        (http-alternative-service-store-note-response store other-uri seed)
        (let ((old (first (http-alternative-service-store-services store uri)))
              (other (first (http-alternative-service-store-services store other-uri))))
          (ensure-true (eq store (http-alternative-service-store-note-response store uri response)))
          (ensure-true (eq other (first (http-alternative-service-store-services store other-uri))))
          (let* ((services (http-alternative-service-store-services store uri))
                 (service (first services)))
            (ensure-equal (if valid-p 1 0) (length services))
            (ensure-false (eq old service))
            (when valid-p
              (ensure-equal (http-uri-origin uri) (http-alternative-service-origin service))
              (ensure-equal "h3" (http-alternative-service-protocol-id service))
              (ensure-equal "origin.example" (http-alternative-service-host service))
              (ensure-equal 443 (http-alternative-service-port service))
              (ensure-equal 128 (http-alternative-service-expires-at service))
              (ensure-equal persist-p (http-alternative-service-persist-p service)))
            (ensure-true (eq store (http-alternative-service-store-network-changed store)))
            (ensure-equal (if persist-p (list service) nil)
                          (http-alternative-service-store-services store uri))
            (ensure-true (eq other (first (http-alternative-service-store-services store other-uri))))
            (setf now 128)
            (ensure-false (http-alternative-service-store-services store uri))
            (ensure-true (eq other (first (http-alternative-service-store-services store other-uri))))))))))

(deftest client-contract-final-pool-opener-proxy-error-contract
  (let* ((events nil)
         (original (make-condition 'simple-error :format-control "opener failed"))
         (pool (make-http-connection-pool
                :open-stream (lambda (request &key &allow-other-keys)
                               (declare (ignore request))
                               (push :open events)
                               (error original))
                :close-stream (lambda (stream)
                                (declare (ignore stream))
                                (push :close events))))
         (request (make-http-request :method "GET" :uri "http://example.test/"))
         (caught (handler-case (http-connection-pool-send pool request)
                   (http-proxy-error (condition) condition))))
    (ensure-true (typep caught 'http-proxy-error))
    (ensure-true (eq original (getf (http-client-error-detail caught) :cause)))
    (ensure-equal nil (getf (http-client-error-detail caught) :detail))
    (ensure-equal :proxy (http-error-operation caught))
    (ensure-equal "Proxy negotiation signaled an error."
                  (http-error-message caught))
    (ensure-equal '(:open) (reverse events))
    (ensure-equal 0 (getf (http-connection-pool-stats pool) :idle-count))
    (ensure-equal 0 (http-connection-pool-clear pool))
    (ensure-equal '(:open) (reverse events))))

(deftest client-contract-final-auth-provider-declines
  (dolist (value (list nil "test-auth" (make-http-header "Authorization" "test-auth")))
    (let* ((events nil) (provider-requests nil)
           (response (make-http-response :status 204))
           (request (make-http-request :method "GET" :uri "https://example.test/auth"))
           (client
             (make-http-client
              :auth-provider (lambda (prepared)
                               (push :auth events)
                               (push prepared provider-requests)
                               value)
              :on-request (lambda (prepared attempt)
                            (push :request events)
                            (ensure-equal 1 attempt)
                            (ensure-equal (and value "test-auth")
                                          (http-header-value (http-request-headers prepared)
                                                             "Authorization")))
              :on-response (lambda (received prepared attempt)
                             (declare (ignore prepared))
                             (push :response events)
                             (ensure-true (eq response received))
                             (ensure-equal 1 attempt))
              :transport-function
              (lambda (prepared &key &allow-other-keys)
                (push :transport events)
                (ensure-equal (and value "test-auth")
                              (http-header-value (http-request-headers prepared) "Authorization"))
                response))))
      (multiple-value-bind (received prepared) (http-client-send client request)
        (ensure-true (eq response received))
        (ensure-equal (and value "test-auth")
                      (http-header-value (http-request-headers prepared) "Authorization")))
      (ensure-equal (if value '(:auth :request :transport :response)
                             '(:auth :auth :request :transport :response))
                    (reverse events))
      (ensure-equal (if value 1 2) (length provider-requests))
      (dolist (prepared provider-requests)
        (ensure-false (http-header-present-p (http-request-headers prepared) "Authorization")))
      (ensure-false (http-header-present-p (http-request-headers request) "Authorization")))))

(deftest client-contract-final-cache-invalidation-location-boundaries
  (dolist (status '(204 400))
    (dolist (location '("ftp://example.test/related" "https://other.test/related"))
      (let* ((cache (make-http-cache :clock-function (lambda () 3000)))
             (events nil)
             (uris '("https://example.test/item" "https://example.test/related"
                     "https://other.test/related"))
             (requests (mapcar (lambda (uri) (make-http-request :method "GET" :uri uri)) uris))
             (response (make-http-response
                        :status status
                        :headers (list (make-http-header "Location" location)
                                       (make-http-header "Content-Location" "/related"))))
             (client (make-http-client
                      :cache cache
                      :on-request (lambda (request attempt)
                                    (declare (ignore request))
                                    (push :request events)
                                    (ensure-equal 1 attempt))
                      :on-response (lambda (received request attempt)
                                     (declare (ignore request))
                                     (push :response events)
                                     (ensure-true (eq response received))
                                     (ensure-equal 1 attempt)
                                     (ensure-equal 3 (length (http-cache-entries cache))))
                      :transport-function (lambda (request &key &allow-other-keys)
                                            (push :transport events)
                                            (ensure-equal "POST" (http-request-method request))
                                            response))))
        (dolist (request requests)
          (ensure-true
           (http-cache-store cache request
                             (make-http-response
                              :status 200 :body (ascii "cached")
                              :headers (list (make-http-header "Cache-Control" "max-age=60")))
                             :now 3000))
          (multiple-value-bind (cached state) (http-cache-lookup cache request :now 3000)
            (ensure-true (http-response-p cached))
            (ensure-equal :fresh state)
            (ensure-equal "cached" (octets-as-string (http-response-body cached)))))
        (ensure-equal status
                      (http-response-status
                       (http-client-send client "POST" (first uris) :body (ascii "change"))))
        (ensure-equal '(:request :transport :response) (reverse events))
        (loop for request in requests
              for index from 0
              for invalidated-p = (and (= status 204) (< index 2))
              do (multiple-value-bind (cached state) (http-cache-lookup cache request :now 3000)
                   (ensure-equal (if invalidated-p :miss :fresh) state)
                   (if invalidated-p
                       (ensure-false cached)
                       (ensure-equal "cached" (octets-as-string (http-response-body cached))))))
        (ensure-equal (if (= status 204) 1 3) (length (http-cache-entries cache)))))))

(deftest client-contract-final-hsts-quoted-tab-preserves-policy
  (let* ((now 100)
         (store (make-http-strict-transport-store :clock-function (lambda () now)))
         (uri "https://example.test/")
         (seed (make-http-response
                :status 200
                :headers (list (make-http-header "Strict-Transport-Security"
                                                "max-age=10; includeSubDomains")))))
    (http-strict-transport-store-note-response store uri seed)
    (let ((policy (first (http-strict-transport-store-policies store))))
      (dolist (value (list (format nil "max-age=20; x=~Ca~Cb~C" #\" #\Tab #\")
                          (format nil "max-age=20; x=~Ca~C~Cb~C" #\" #\\ #\Tab #\")))
        (let ((header (make-http-header "Strict-Transport-Security" value)))
          (ensure-equal value (http-header-content header))
          (ensure-true (eq store (http-strict-transport-store-note-response
                                 store uri (make-http-response :status 200 :headers (list header)))))
          (ensure-true (eq policy (first (http-strict-transport-store-policies store))))
          (ensure-equal 110 (http-strict-transport-policy-expires-at policy))
          (ensure-true (http-strict-transport-policy-include-subdomains-p policy))))
      (ensure-true (http-strict-transport-store-known-host-p store "example.test"))
      (ensure-true (http-strict-transport-store-known-host-p store "a.example.test"))
      (ensure-false (http-strict-transport-store-known-host-p store "ample.test"))
      (ensure-false (http-strict-transport-store-known-host-p store "notexample.test"))
      (setf now 110)
      (ensure-false (http-strict-transport-store-known-host-p store "example.test"))
      (ensure-false (http-strict-transport-store-policies store)))))

(deftest client-contract-final-http3-short-fallback-budget
  (dolist (timeout '(0.5 1.0 2.0))
    (let* ((events nil) (deadlines nil)
           (store (make-http-alternative-service-store :clock-function (lambda () 100)))
           (uri "https://example.test/budget")
           (response (make-http-response :status 204))
           (failure (make-condition 'http-connection-error :message "failed before sending"
                                    :operation :connect :request-sent-p nil))
           (client
             (make-http-client
              :alternative-service-store store
              :wall-clock-function (lambda () 100.0)
              :on-request (lambda (request attempt)
                            (declare (ignore request))
                            (push :request events)
                            (ensure-equal 1 attempt))
              :on-response (lambda (received request attempt)
                             (declare (ignore request))
                             (push :response events)
                             (ensure-true (eq response received))
                             (ensure-equal 1 attempt))
              :http3-transport-function
              (lambda (request &key deadline alternative-service &allow-other-keys)
                (push :h3 events)
                (push deadline deadlines)
                (ensure-true (http-request-http3-p request))
                (ensure-equal 444 (http-alternative-service-port alternative-service))
                (error failure))
              :transport-function
              (lambda (request &key deadline &allow-other-keys)
                (push :tcp events)
                (push deadline deadlines)
                (ensure-false (http-request-http3-p request))
                response))))
      (http-alternative-service-store-note-response
       store uri (make-http-response :status 200
                                     :headers (list (make-http-header "Alt-Svc" "h3=\":444\"; ma=60"))))
      (ensure-equal 1 (length (http-alternative-service-store-services store uri)))
      (ensure-true (eq response (http-client-send client "GET" uri :timeout timeout)))
      (ensure-equal '(:request :h3 :tcp :response) (reverse events))
      (ensure-equal (list (+ 100.0 (if (> timeout 1.0) (- timeout 1.0) (* timeout 0.5)))
                          (+ 100.0 timeout))
                    (reverse deadlines))
      (ensure-false (http-alternative-service-store-services store uri)))))

(deftest client-contract-final-observed-body-blocks-auth-replay
  (dolist (status '(401 407))
    (let* ((events nil) (provider-calls 0)
           (chunk (ascii "challenge body"))
           (response (make-http-response
                      :status status
                      :headers (list (make-http-header
                                      (if (= status 401) "WWW-Authenticate" "Proxy-Authenticate")
                                      "Basic realm=\"test\""))))
           (provider (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (incf provider-calls)
                       "test-auth"))
           (client
             (make-http-client
              :challenge-auth-provider provider
              :proxy-challenge-auth-provider provider
              :on-request (lambda (request attempt)
                            (declare (ignore request))
                            (push :request events)
                            (ensure-equal 1 attempt))
              :on-response (lambda (received request attempt)
                             (declare (ignore request))
                             (push :response events)
                             (ensure-true (eq response received))
                             (ensure-equal 1 attempt))
              :transport-function
              (lambda (request &key on-body-chunk collect-body-p &allow-other-keys)
                (push :transport events)
                (ensure-false collect-body-p)
                (ensure-false (http-header-present-p (http-request-headers request) "Authorization"))
                (ensure-false (http-header-present-p (http-request-headers request) "Proxy-Authorization"))
                (funcall on-body-chunk chunk)
                response))))
      (ensure-true
       (eq response (http-client-send
                     client "GET" "https://example.test/challenge" :collect-body-p nil
                     :on-body-chunk (lambda (received)
                                      (push :body events)
                                      (ensure-octets-equal chunk received)))))
      (ensure-equal 0 provider-calls)
      (ensure-equal '(:request :transport :body :response) (reverse events)))))

(deftest client-contract-final-explicit-http3-requires-transport
  (let* ((events nil)
         (request (make-http-request :method "GET" :uri "https://example.test/h3"
                                     :protocol-version "HTTP/3"))
         (client (make-http-client
                  :http3-transport-function nil
                  :on-request (lambda (prepared attempt)
                                (push :request events)
                                (ensure-true (http-request-http3-p prepared))
                                (ensure-equal 1 attempt))
                  :on-response (lambda (&rest arguments)
                                 (declare (ignore arguments))
                                 (push :response events))
                  :transport-function (lambda (&rest arguments)
                                        (declare (ignore arguments))
                                        (push :tcp events)
                                        (make-http-response :status 204))))
         (caught (handler-case (http-client-send client request)
                   (http-unsupported-feature (condition) condition))))
    (ensure-true (typep caught 'http-unsupported-feature))
    (ensure-equal :http3-transport (http-unsupported-feature-name caught))
    (ensure-equal :client (http-error-operation caught))
    (ensure-equal "HTTP-KIT/CLIENT" (http-protocol-error-detail caught))
    (ensure-equal "The default HTTP client requires HTTP3-TRANSPORT from HTTP-KIT/CLIENT."
                  (http-error-message caught))
    (ensure-equal '(:request) (reverse events))
    (ensure-true (http-request-http3-p request))))

(deftest client-contract-post-review-304-requires-cached-validator
  (dolist (validator '(("ETag" "\"v1\"" "If-None-Match" "If-Modified-Since")
                       ("Last-Modified" "Wed, 21 Oct 2015 07:28:00 GMT"
                        "If-Modified-Since" "If-None-Match")))
    (destructuring-bind (name value conditional absent) validator
      (let* ((calls 0) (events nil) (callbacks nil)
             (headers (list (make-http-header "X-Input" "unchanged")))
             (request (make-http-request :method "GET"
                                         :uri "https://example.test/validator"
                                         :headers headers))
             (not-modified (make-http-response :status 304))
             (client
               (make-http-client
                :cache (make-http-cache :clock-function (lambda () 3000))
                :on-request (lambda (prepared attempt)
                              (declare (ignore prepared))
                              (push :request events)
                              (ensure-equal 1 attempt))
                :on-response (lambda (response prepared attempt)
                               (declare (ignore prepared))
                               (push :response events)
                               (push response callbacks)
                               (ensure-equal 1 attempt))
                :transport-function
                (lambda (prepared &key &allow-other-keys)
                  (push :transport events)
                  (if (= 1 (incf calls))
                      (make-http-response
                       :status 200 :body (ascii "cached")
                       :headers (list (make-http-header "Cache-Control" "max-age=0")
                                      (make-http-header name value)))
                      (progn
                        (ensure-equal value
                                      (http-header-value (http-request-headers prepared)
                                                         conditional))
                        (ensure-false (http-header-present-p
                                       (http-request-headers prepared) absent))
                        not-modified))))))
        (let ((response (http-client-send client request)))
          (ensure-equal 200 (http-response-status response))
          (ensure-equal "cached" (octets-as-string (http-response-body response))))
        (let ((caught (handler-case (http-client-send client request)
                        (http-protocol-error (condition) condition))))
          (ensure-true (typep caught 'http-protocol-error))
          (ensure-equal :client (http-error-operation caught))
          (ensure-equal "A 304 response did not identify the cached response validator."
                        (http-error-message caught))
          (ensure-true (eq not-modified (http-protocol-error-detail caught))))
        (ensure-equal 2 calls)
        (ensure-equal 2 (length callbacks))
        (ensure-true (eq not-modified (first callbacks)))
        (ensure-equal '(:request :transport :response :request :transport :response)
                      (reverse events))
        (ensure-equal headers (http-request-headers request))
        (ensure-equal "unchanged" (http-header-value headers "X-Input"))
        (ensure-false (http-header-present-p (http-request-headers request) conditional))
        (ensure-equal "GET" (http-request-method request))
        (ensure-equal "/validator" (http-uri-path (http-request-uri request)))))))

(deftest client-contract-post-review-etag-precedes-last-modified
  (let* ((calls 0) (callbacks nil)
         (date "Wed, 21 Oct 2015 07:28:00 GMT")
         (request (make-http-request :method "GET" :uri "https://example.test/both"))
         (not-modified (make-http-response
                        :status 304 :headers (list (make-http-header "ETag" "\"v1\""))))
         (client
           (make-http-client
            :cache (make-http-cache :clock-function (lambda () 3000))
            :on-response (lambda (response prepared attempt)
                           (declare (ignore prepared))
                           (ensure-equal 1 attempt)
                           (push response callbacks))
            :transport-function
            (lambda (prepared &key &allow-other-keys)
              (if (= 1 (incf calls))
                  (make-http-response
                   :status 200 :body (ascii "cached")
                   :headers (list (make-http-header "Cache-Control" "max-age=0")
                                  (make-http-header "ETag" "\"v1\"")
                                  (make-http-header "Last-Modified" date)))
                  (progn
                    (ensure-equal "\"v1\""
                                  (http-header-value (http-request-headers prepared)
                                                     "If-None-Match"))
                    (ensure-false (http-header-present-p
                                   (http-request-headers prepared) "If-Modified-Since"))
                    not-modified))))))
    (ensure-equal 200 (http-response-status (http-client-send client request)))
    (let ((response (http-client-send client request)))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "cached" (octets-as-string (http-response-body response)))
      (ensure-equal date (http-header-value (http-response-headers response) "Last-Modified"))
      (ensure-false (eq not-modified response)))
    (ensure-equal 2 calls)
    (ensure-equal 2 (length callbacks))
    (ensure-true (eq not-modified (first callbacks)))
    (ensure-false (http-header-present-p (http-request-headers request) "If-None-Match"))
    (ensure-false (http-header-present-p (http-request-headers request) "If-Modified-Since"))
    (ensure-equal "/both" (http-uri-path (http-request-uri request)))))

(deftest client-contract-post-review-single-use-post-303
  (let* ((calls 0) (producer-calls 0) (events nil)
         (chunks (list (ascii "stream")))
         (headers (list (make-http-header "Content-Type" "text/plain")
                        (make-http-header "X-Input" "unchanged")))
         (request (make-http-request :method "POST" :uri "https://example.test/start"
                                     :headers headers))
         (producer (lambda (maximum-size)
                     (ensure-equal 65536 maximum-size)
                     (incf producer-calls)
                     (pop chunks)))
         (client
           (make-http-client
            :cache nil
            :on-request (lambda (prepared attempt)
                          (declare (ignore prepared))
                          (push :request events)
                          (ensure-equal 1 attempt))
            :on-response (lambda (response prepared attempt)
                           (declare (ignore response prepared))
                           (push :response events)
                           (ensure-equal 1 attempt))
            :transport-function
            (lambda (prepared &key request-body-function request-body-length
                                  &allow-other-keys)
              (push :transport events)
              (case (incf calls)
                (1
                 (ensure-equal "POST" (http-request-method prepared))
                 (ensure-equal "/start" (http-uri-path (http-request-uri prepared)))
                 (ensure-true (functionp request-body-function))
                 (ensure-equal 6 request-body-length)
                 (ensure-octets-equal (ascii "stream") (funcall request-body-function 65536))
                 (ensure-false (funcall request-body-function 65536))
                 (make-http-response
                  :status 303 :headers (list (make-http-header "Location" "/next"))))
                (2
                 (ensure-equal "GET" (http-request-method prepared))
                 (ensure-equal "/next" (http-uri-path (http-request-uri prepared)))
                 (ensure-false request-body-function)
                 (ensure-false request-body-length)
                 (ensure-equal 0 (length (http-request-body prepared)))
                 (ensure-false (http-header-present-p
                                (http-request-headers prepared) "Content-Type"))
                 (ensure-equal "unchanged"
                               (http-header-value (http-request-headers prepared) "X-Input"))
                 (make-http-response :status 200))
                (otherwise (error "Unexpected additional transport call.")))))))
    (multiple-value-bind (response effective)
        (http-client-send client request :request-body-function producer :request-body-length 6)
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "GET" (http-request-method effective))
      (ensure-equal "/next" (http-uri-path (http-request-uri effective))))
    (ensure-equal 2 calls)
    (ensure-equal 2 producer-calls)
    (ensure-equal '(:request :transport :response :request :transport :response)
                  (reverse events))
    (ensure-equal "POST" (http-request-method request))
    (ensure-equal "/start" (http-uri-path (http-request-uri request)))
    (ensure-equal headers (http-request-headers request))
    (ensure-equal "text/plain" (http-header-value (http-request-headers request) "Content-Type"))
    (ensure-equal 0 (length (http-request-body request)))))

(deftest client-contract-post-review-same-origin-challenge-redirect
  (let* ((calls 0) (provider-calls 0) (events nil)
         (request (make-http-request :method "GET" :uri "https://example.test/start"))
         (client
           (make-http-client
            :cache nil
            :redirect-policy (make-http-redirect-policy :preserve-authorization-p t)
            :challenge-auth-provider
            (lambda (prepared response challenges)
              (push :provider events)
              (incf provider-calls)
              (ensure-equal "/start" (http-uri-path (http-request-uri prepared)))
              (ensure-equal 401 (http-response-status response))
              (ensure-true challenges)
              (ensure-false (http-header-present-p (http-request-headers prepared) "Authorization"))
              "Basic source-fixture")
            :transport-function
            (lambda (prepared &key &allow-other-keys)
              (case (incf calls)
                (1
                 (push :unauthorized events)
                 (ensure-false (http-header-present-p (http-request-headers prepared) "Authorization"))
                 (make-http-response
                  :status 401 :headers (list (make-http-header "WWW-Authenticate"
                                                             "Basic realm=\"source\""))))
                (2
                 (push :redirect events)
                 (ensure-equal "/start" (http-uri-path (http-request-uri prepared)))
                 (ensure-equal "Basic source-fixture"
                               (http-header-value (http-request-headers prepared) "Authorization"))
                 (make-http-response
                  :status 302 :headers (list (make-http-header "Location" "/next"))))
                (3
                 (push :ok events)
                 (ensure-equal "/next" (http-uri-path (http-request-uri prepared)))
                 (ensure-equal "Basic source-fixture"
                               (http-header-value (http-request-headers prepared) "Authorization"))
                 (make-http-response :status 200))
                (otherwise (error "Unexpected additional transport call.")))))))
    (multiple-value-bind (response effective) (http-client-send client request)
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "/next" (http-uri-path (http-request-uri effective)))
      (ensure-equal "Basic source-fixture"
                    (http-header-value (http-request-headers effective) "Authorization")))
    (ensure-equal 3 calls)
    (ensure-equal 1 provider-calls)
    (ensure-equal '(:unauthorized :provider :redirect :ok) (reverse events))
    (ensure-equal "/start" (http-uri-path (http-request-uri request)))
    (ensure-false (http-header-present-p (http-request-headers request) "Authorization"))
    (ensure-equal 0 (length (http-request-body request)))))

(deftest client-contract-post-review-explicit-forward-proxy-authorization
  (let* ((calls 0)
         (proxy (make-http-proxy :scheme :http :host "proxy.example" :port 8080
                                 :username "user" :password "pass"))
         (headers (list (make-http-header "Proxy-Authorization" "Basic explicit-fixture")))
         (request (make-http-request :method "GET" :uri "http://example.test/path"
                                     :headers headers))
         (client
           (make-http-client
            :cache nil :proxy proxy
            :transport-function
            (lambda (prepared &key proxy-plan &allow-other-keys)
              (incf calls)
              (ensure-equal :forward (getf proxy-plan :mode))
              (ensure-true (eq proxy (getf proxy-plan :proxy)))
              (ensure-equal "Basic dXNlcjpwYXNz" (getf proxy-plan :proxy-authorization))
              (ensure-equal '("Basic explicit-fixture")
                            (http-header-values (http-request-headers prepared) "Proxy-Authorization"))
              (make-http-response :status 200)))))
    (multiple-value-bind (response effective) (http-client-send client request)
      (ensure-equal 200 (http-response-status response))
      (ensure-equal '("Basic explicit-fixture")
                    (http-header-values (http-request-headers effective) "Proxy-Authorization")))
    (ensure-equal 1 calls)
    (ensure-equal headers (http-request-headers request))
    (ensure-equal '("Basic explicit-fixture")
                  (http-header-values (http-request-headers request) "Proxy-Authorization"))
    (ensure-equal "/path" (http-uri-path (http-request-uri request)))
    (ensure-equal "GET" (http-request-method request))))

(defun client-contract-check-alt-svc-malformed-authority (wire)
  (let* ((store (make-http-alternative-service-store
                 :clock-function (lambda () 100)))
         (uri (parse-http-uri "https://origin.example/resource?key=value"))
         (other-uri (parse-http-uri "https://other.example/resource?key=other"))
         (uri-before (http-uri-string uri))
         (other-uri-before (http-uri-string other-uri))
         (wire-before (copy-seq wire))
         (seed (make-http-response
                :status 204
                :headers (list (make-http-header "Alt-Svc" "h3=\":443\"; ma=30"))))
         (response (make-http-response
                    :status 204
                    :headers (list (make-http-header "Alt-Svc" wire))))
         (headers-before (http-response-headers response)))
    (dolist (origin-uri (list uri other-uri))
      (ensure-true
       (eq store (http-alternative-service-store-note-response store origin-uri seed)))
      (let* ((services (http-alternative-service-store-services store origin-uri))
             (service (first services)))
        (ensure-equal 1 (length services))
        (ensure-equal (http-uri-origin origin-uri) (http-alternative-service-origin service))
        (ensure-equal "h3" (http-alternative-service-protocol-id service))
        (ensure-equal (http-uri-host origin-uri) (http-alternative-service-host service))
        (ensure-equal 443 (http-alternative-service-port service))
        (ensure-equal 130 (http-alternative-service-expires-at service))
        (ensure-false (http-alternative-service-persist-p service))))
    (let ((other (first (http-alternative-service-store-services store other-uri))))
      (ensure-true
       (eq store (http-alternative-service-store-note-response store uri response)))
      (ensure-false (http-alternative-service-store-services store uri))
      (let ((others (http-alternative-service-store-services store other-uri)))
        (ensure-equal 1 (length others))
        (ensure-true (eq other (first others))))
      (ensure-equal wire-before wire)
      (ensure-equal uri-before (http-uri-string uri))
      (ensure-equal other-uri-before (http-uri-string other-uri))
      (ensure-equal 204 (http-response-status response))
      (ensure-equal headers-before (http-response-headers response))
      (ensure-equal (list wire-before)
                    (http-header-values (http-response-headers response) "Alt-Svc"))
      (ensure-equal '("h3=\":443\"; ma=30")
                    (http-header-values (http-response-headers seed) "Alt-Svc")))))

(deftest client-contract-uri-malformed-alt-svc-empty-authority
  (client-contract-check-alt-svc-malformed-authority "h3="))

(deftest client-contract-uri-malformed-alt-svc-one-character-authority
  (client-contract-check-alt-svc-malformed-authority "h3=x"))

(deftest client-contract-uri-malformed-alt-svc-unquoted-authority
  (client-contract-check-alt-svc-malformed-authority "h3=:443"))

(deftest client-contract-uri-malformed-alt-svc-quoted-authority-suffix
  (client-contract-check-alt-svc-malformed-authority "h3=\":443\"x"))

(deftest uri-resolution-validation-boundaries
  (let* ((base-text "https://example.test/a/b/start?old=1")
         (base (parse-http-uri base-text)))
    (dolist (reference '("//" "//[::1" "//host:65536/path" "%" "?x=%" " "))
      (signals http-invalid-uri
        (http-kit/client:resolve-http-uri base reference)))
    (ensure-equal
     "https://example.test/a/%2e%2e/%2E?x=a:b/c"
     (http-uri-string
      (http-kit/client:resolve-http-uri base "../%2e%2e/%2E?x=a:b/c#ignored")))
    (ensure-equal
     "https://[2001:db8::1]:8443/path"
     (http-uri-string
      (http-kit/client:resolve-http-uri base "//[2001:db8::1]:8443/path")))
    (ensure-equal base-text (http-uri-string base))))

(defun client-contract-public-request-snapshot (request)
  (list (http-uri-string (http-request-uri request))
        (http-request-headers request)
        (http-request-trailers request)
        (http-request-body request)
        (list (copy-seq (http-request-method request))
              (http-uri-string (http-request-uri request))
              (http-request-protocol-version request)
              (mapcar (lambda (header)
                        (list (copy-seq (http-header-name header))
                              (copy-seq (http-header-content header))))
                      (http-request-headers request))
              (mapcar (lambda (header)
                        (list (copy-seq (http-header-name header))
                              (copy-seq (http-header-content header))))
                      (http-request-trailers request))
              (copy-seq (http-request-body request)))))

(defun client-contract-public-request-unchanged (request snapshot)
  (let ((actual (client-contract-public-request-snapshot request)))
    (loop for expected in (subseq snapshot 0 4)
          for observed in actual
          do (ensure-equal expected observed))
    (ensure-equal (fifth snapshot) (fifth actual))))

(deftest client-contract-public-hsts-lone-quote
  (let* ((store (make-http-strict-transport-store :clock-function (lambda () 100)))
         (uri (parse-http-uri "https://example.test/security")))
    (flet ((note (value)
             (let* ((header (make-http-header "Strict-Transport-Security" value))
                    (response (make-http-response :status 200 :headers (list header))))
               (ensure-true
                (eq store (http-strict-transport-store-note-response store uri response)))
               (ensure-equal value (http-header-content header))
               (ensure-equal "https://example.test/security" (http-uri-string uri)))))
      (note "max-age=10; includeSubDomains")
      (let ((policy (first (http-strict-transport-store-policies store))))
        (dolist (value '("max-age=\"" "max-age=0; x=\""))
          (note value)
          (ensure-equal 1 (length (http-strict-transport-store-policies store)))
          (ensure-true (eq policy (first (http-strict-transport-store-policies store))))
          (ensure-equal 110 (http-strict-transport-policy-expires-at policy))
          (ensure-true (http-strict-transport-policy-include-subdomains-p policy))
          (ensure-true (http-strict-transport-store-known-host-p store "sub.example.test"))))
      (note "max-age=20")
      (ensure-equal 120 (http-strict-transport-policy-expires-at
                         (first (http-strict-transport-store-policies store))))
      (ensure-false (http-strict-transport-store-known-host-p store "sub.example.test"))
      (note "max-age=0")
      (ensure-false (http-strict-transport-store-policies store)))))

(deftest client-contract-public-post-atomic-failure-no-replay
  (dolist (failure (list (make-condition 'http-timeout :message "Fixture read timeout."
                                                    :operation :read :kind :read)
                         (make-condition 'http-connection-error :message "Fixture lost connection."
                                                               :operation :request :cause nil)
                         (make-condition 'http-connection-error :message "Fixture lost connection."
                                                               :operation :request :cause :opaque)
                         (make-condition 'http-connection-error :message "Fixture unprocessed request."
                                                               :operation :request :cause '(:goaway 0 1))))
    (let* ((calls 0) (sleeps nil)
           (request (make-http-request :method "POST" :uri "https://example.test/replay"
                                       :headers (list (make-http-header "X-Fixture" "unchanged"))
                                       :body (ascii "payload")))
           (snapshot (client-contract-public-request-snapshot request))
           (client (make-http-client
                    :cache nil :content-decoders nil
                    :retry-policy (make-http-retry-policy :max-attempts 2 :base-delay 0 :max-delay 0)
                    :sleep-function (lambda (seconds) (push seconds sleeps))
                    :transport-function
                    (lambda (prepared &key &allow-other-keys)
                      (incf calls)
                      (ensure-equal "POST" (http-request-method prepared))
                      (ensure-octets-equal (ascii "payload") (http-request-body prepared))
                      (if (= calls 1) (error failure) (make-http-response :status 200))))))
      (if (and (typep failure 'http-connection-error)
               (equal '(:goaway 0 1) (http-connection-error-cause failure)))
          (progn
            (ensure-equal 200 (http-response-status (http-client-send client request)))
            (ensure-equal 2 calls))
          (let ((caught (handler-case (http-client-send client request)
                          (http-error (condition) condition))))
            (ensure-true (eq failure caught))
            (ensure-equal (type-of failure) (type-of caught))
            (ensure-equal (http-error-message failure) (http-error-message caught))
            (ensure-equal (http-error-operation failure) (http-error-operation caught))
            (if (typep caught 'http-timeout)
                (ensure-equal :read (http-timeout-kind caught))
                (progn
                  (ensure-equal (http-connection-error-cause failure)
                                (http-connection-error-cause caught))
                  (ensure-true (http-connection-error-request-sent-p caught))))
            (ensure-equal 1 calls)))
      (ensure-false sleeps)
      (client-contract-public-request-unchanged request snapshot))))

(deftest client-contract-public-nonnumeric-jitter
  (dolist (random-value '(:invalid 0.5))
    (let* ((calls 0) (random-calls 0) (sleeps nil)
           (request (make-http-request :method "GET" :uri "https://example.test/retry"
                                       :headers (list (make-http-header "X-Fixture" "unchanged"))))
           (snapshot (client-contract-public-request-snapshot request))
           (client (make-http-client
                    :cache nil :content-decoders nil
                    :retry-policy (make-http-retry-policy :max-attempts 2 :base-delay 4
                                                         :max-delay 4 :jitter-ratio 0.5)
                    :random-function (lambda (limit)
                                       (incf random-calls)
                                       (ensure-equal 1.0 limit)
                                       random-value)
                    :sleep-function (lambda (seconds) (push seconds sleeps))
                    :transport-function (lambda (prepared &key &allow-other-keys)
                                          (ensure-equal "GET" (http-request-method prepared))
                                          (make-http-response :status (if (= (incf calls) 1) 503 200))))))
      (if (eq random-value :invalid)
          (let ((caught (handler-case (http-client-send client request)
                          (http-protocol-error (condition) condition))))
            (ensure-true (typep caught 'http-protocol-error))
            (ensure-equal :client (http-error-operation caught))
            (ensure-equal :invalid (http-protocol-error-detail caught))
            (ensure-equal "The client random function must return a number in [0, 1)."
                          (http-error-message caught))
            (ensure-equal 1 calls)
            (ensure-false sleeps))
          (progn
            (ensure-equal 200 (http-response-status (http-client-send client request)))
            (ensure-equal 2 calls)
            (ensure-equal '(4.0) sleeps)))
      (ensure-equal 1 random-calls)
      (client-contract-public-request-unchanged request snapshot))))

(deftest client-contract-public-invalid-client-explicit-nil-timeout
  (let* ((calls 0)
         (request (make-http-request :method "GET" :uri "https://example.test/timeout"
                                     :headers (list (make-http-header "X-Fixture" "unchanged"))))
         (snapshot (client-contract-public-request-snapshot request))
         (caught (handler-case (http-client-send :invalid request :timeout nil)
                   (http-protocol-error (condition) condition))))
    (ensure-true (typep caught 'http-protocol-error))
    (ensure-equal :client (http-error-operation caught))
    (ensure-equal :invalid (http-protocol-error-detail caught))
    (ensure-equal "The client must be an HTTP-CLIENT." (http-error-message caught))
    (client-contract-public-request-unchanged request snapshot)
    (let ((client (make-http-client
                   :content-decoders nil :default-timeout 5
                   :transport-function (lambda (prepared &key timeout deadline &allow-other-keys)
                                         (ensure-equal "GET" (http-request-method prepared))
                                         (ensure-false timeout)
                                         (ensure-false deadline)
                                         (incf calls)
                                         (make-http-response :status 200)))))
      (ensure-equal 200 (http-response-status (http-client-send client request :timeout nil)))
      (ensure-equal 1 calls))
    (client-contract-public-request-unchanged request snapshot)))

(deftest client-contract-public-nil-content-decoders
  (dolist (enabled '(nil t))
    (let* ((calls 0)
           (request (make-http-request :method "GET" :uri "https://example.test/encoding"
                                       :headers (list (make-http-header "X-Fixture" "unchanged"))))
           (snapshot (client-contract-public-request-snapshot request))
           (body (ascii "plain"))
           (client (make-http-client
                    :automatic-decompression-p t
                    :content-decoders (when enabled (make-http-content-decoders))
                    :transport-function
                    (lambda (prepared &key &allow-other-keys)
                      (incf calls)
                      (ensure-equal (if enabled '("gzip, deflate, br, zstd") nil)
                                    (http-header-values (http-request-headers prepared) "Accept-Encoding"))
                      (make-http-response :status 200 :body body)))))
      (let ((response (http-client-send client request)))
        (ensure-equal 200 (http-response-status response))
        (ensure-octets-equal body (http-response-body response)))
      (ensure-equal 1 calls)
      (ensure-false (http-header-present-p (http-request-headers request) "Accept-Encoding"))
      (client-contract-public-request-unchanged request snapshot))))

(deftest client-contract-public-upgrade-post-retains-cache
  (let* ((calls 0)
         (get (make-http-request :method "GET" :uri "http://example.test/upgrade"))
         (post (make-http-request
                :method "POST" :uri "http://example.test/upgrade"
                :headers (list (make-http-header "Connection" "Upgrade")
                               (make-http-header "Upgrade" "fixture/1"))
                :body (ascii "payload")))
         (get-snapshot (client-contract-public-request-snapshot get))
         (post-snapshot (client-contract-public-request-snapshot post))
         (client (make-http-client
                  :cache (make-http-cache :clock-function (lambda () 100))
                  :clock-function (lambda () 100) :content-decoders nil
                  :transport-function
                  (lambda (prepared &key &allow-other-keys)
                    (incf calls)
                    (cond
                      ((= calls 1)
                       (ensure-equal "GET" (http-request-method prepared))
                       (make-http-response :status 200
                                           :headers (list (make-http-header "Cache-Control" "max-age=60"))
                                           :body (ascii "cached")))
                      ((= calls 2)
                       (ensure-equal "POST" (http-request-method prepared))
                       (ensure-equal '("Upgrade") (http-header-values (http-request-headers prepared) "Connection"))
                       (ensure-equal '("fixture/1") (http-header-values (http-request-headers prepared) "Upgrade"))
                       (ensure-octets-equal (ascii "payload") (http-request-body prepared))
                       (parse-http-response
                        (ascii "HTTP/1.1 101 Switching Protocols|CRLF|Connection: Upgrade|CRLF|Upgrade: fixture/1|CRLF||CRLF|")
                        :request-method "POST"))
                      ((= calls 3)
                       (ensure-equal "POST" (http-request-method prepared))
                       (make-http-response :status 200))
                      ((= calls 4)
                       (ensure-equal "GET" (http-request-method prepared))
                       (make-http-response :status 200 :body (ascii "fresh")))
                      (t (error "Unexpected fixture transport call.")))))))
    (ensure-octets-equal (ascii "cached") (http-response-body (http-client-send client get)))
    (ensure-equal 1 calls)
    (let ((response (http-client-send client post)))
      (ensure-equal 101 (http-response-status response))
      (ensure-equal "HTTP/1.1" (http-response-protocol-version response))
      (ensure-equal '("fixture/1") (http-header-values (http-response-headers response) "Upgrade"))
      (ensure-equal 0 (length (http-response-body response))))
    (ensure-equal 2 calls)
    (ensure-octets-equal (ascii "cached") (http-response-body (http-client-send client get)))
    (ensure-equal 2 calls)
    (ensure-equal 200 (http-response-status (http-client-send client post)))
    (ensure-equal 3 calls)
    (ensure-octets-equal (ascii "fresh") (http-response-body (http-client-send client get)))
    (ensure-equal 4 calls)
    (client-contract-public-request-unchanged get get-snapshot)
    (client-contract-public-request-unchanged post post-snapshot)))

(deftest client-contract-default-policy-list-ownership
  (let ((first-redirect (make-http-redirect-policy))
        (second-redirect (make-http-redirect-policy))
        (first-retry (make-http-retry-policy))
        (second-retry (make-http-retry-policy)))
    (flet ((check (first second fresh expected replacement)
             (ensure-equal expected first)
             (ensure-equal expected second)
             (ensure-false (eq first second))
             (setf (car first) replacement)
             (ensure-equal replacement (car first))
             (ensure-equal expected second)
             (ensure-equal expected (funcall fresh))))
      (check (http-redirect-policy-statuses first-redirect)
             (http-redirect-policy-statuses second-redirect)
             (lambda () (http-redirect-policy-statuses (make-http-redirect-policy)))
             '(301 302 303 307 308) 399)
      (check (http-retry-policy-methods first-retry)
             (http-retry-policy-methods second-retry)
             (lambda () (http-retry-policy-methods (make-http-retry-policy)))
             '("GET" "HEAD" "OPTIONS" "PUT" "DELETE" "TRACE") "POST")
      (check (http-retry-policy-statuses first-retry)
             (http-retry-policy-statuses second-retry)
             (lambda () (http-retry-policy-statuses (make-http-retry-policy)))
             '(408 425 429 500 502 503 504) 599))))

(deftest client-contract-default-store-clocks
  (let* ((before (get-universal-time))
         (uri (parse-http-uri "https://example.com/default-clock"))
         (request (make-http-request :method "GET" :uri uri))
         (response (make-http-response
                    :status 200 :body (ascii "ok")
                    :headers '(("Set-Cookie" . "default=value; Max-Age=60; Secure; Path=/")
                               ("Cache-Control" . "max-age=60")
                               ("Strict-Transport-Security" . "max-age=60")
                               ("Alt-Svc" . "h3=\":443\"; ma=60"))))
         (jar (make-http-cookie-jar))
         (cache (make-http-cache))
         (strict (make-http-strict-transport-store))
         (alternative (make-http-alternative-service-store)))
    (http-cookie-jar-accept-response jar uri response)
    (let* ((entry (http-cache-store cache request response))
           (cookies (http-cookie-jar-cookies jar)))
      (ensure-true (http-cache-entry-p entry))
      (ensure-equal 1 (length cookies))
      (ensure-true (eq strict (http-strict-transport-store-note-response strict uri response)))
      (ensure-true (eq alternative (http-alternative-service-store-note-response alternative uri response)))
      (let ((after (get-universal-time))
            (policies (http-strict-transport-store-policies strict))
            (services (http-alternative-service-store-services alternative uri)))
        (ensure-true (<= before (http-cookie-creation-time (first cookies)) after))
        (ensure-equal (+ 60 (http-cookie-creation-time (first cookies)))
                      (http-cookie-expiry-time (first cookies)))
        (ensure-true (<= before (http-cache-entry-stored-at entry) after))
        (ensure-equal (+ 60 (http-cache-entry-stored-at entry)) (http-cache-entry-expires-at entry))
        (ensure-equal 1 (length policies))
        (ensure-equal 1 (length services))
        (ensure-true (<= (+ before 60) (http-strict-transport-policy-expires-at (first policies)) (+ after 60)))
        (ensure-true (<= (+ before 60) (http-alternative-service-expires-at (first services)) (+ after 60))))
      (multiple-value-bind (cached state) (http-cache-lookup cache request)
        (ensure-equal :fresh state)
        (ensure-octets-equal (ascii "ok") (http-response-body cached)))
      (ensure-true (http-strict-transport-store-known-host-p strict "example.com")))
    (flet ((check-clear (store clear)
             (ensure-true (eq store (funcall clear)))
             (ensure-true (eq store (funcall clear)))))
      (check-clear jar (lambda () (http-cookie-jar-clear jar)))
      (check-clear cache (lambda () (http-cache-clear cache)))
      (check-clear strict (lambda () (http-strict-transport-store-clear strict)))
      (check-clear alternative (lambda () (http-alternative-service-store-clear alternative))))
    (ensure-false (http-cookie-jar-cookies jar))
    (ensure-false (http-cache-entries cache))
    (ensure-false (http-strict-transport-store-policies strict))
    (ensure-false (http-alternative-service-store-services alternative uri))))

(deftest client-contract-default-native-dns
  (let* ((client (make-http-client))
         (pool (http-client-connection-pool client))
         (resolver (http-connection-pool-resolve-host pool)))
    (ensure-true (functionp resolver))
    (ensure-equal "127.0.0.1" (funcall resolver "127.0.0.1" :timeout 2.0))
    (ensure-equal "0000:0000:0000:0000:0000:0000:0000:0001"
                  (funcall resolver "::1" :timeout 2.0))
    (ensure-equal 0 (getf (http-connection-pool-stats pool) :idle-count))
    (ensure-equal 0 (http-connection-pool-clear pool))))

#+sbcl
(defun client-contract-partial-result (form &key network)
  ;; A fresh process proves optional-system absence without changing loaded functions.
  (let ((arguments (list (namestring sb-ext:*runtime-pathname*)
                         "--core" (namestring (truename sb-ext:*core-pathname*))
                         "--noinform" "--no-userinit" "--no-sysinit" "--non-interactive"
                         "--eval" "(require :asdf)"))
        (bridge (find-package "HTTP-COVERAGE-CHILD-MERGE"))
        (ticket nil)
        (finish nil))
    (when bridge
      (let ((collector (find-symbol "*COLLECTOR*" bridge)))
        (when (and collector (boundp collector) (symbol-value collector))
          (multiple-value-bind (child startup shutdown)
              (funcall (find-symbol "PREPARE-CHILD" bridge))
            (setf ticket child finish shutdown
                  arguments (append arguments startup))))))
    (setf arguments (append arguments
                            (list "--eval" "(asdf:load-system \"cl-http-kit/client\")"
                                  "--eval" "(defpackage #:http-kit/test (:use #:cl))")))
    (when network
      (setf arguments (append arguments (list "--eval" "(asdf:load-system \"cl-http-kit/network\")"))))
    (let ((*package* (find-package :cl-user)))
      (setf arguments
            (append arguments
                    (list "--eval"
                          (prin1-to-string
                           `(format t "~&CLIENT-PARTIAL-RESULT ~S~%" ,form)))
                    finish)))
    ;; Files rather than pipes: output is read only after the child exits, so a
    ;; pipe left unread could fill and block the child until the timeout.
    (uiop:with-temporary-file (:pathname output-file)
      (uiop:with-temporary-file (:pathname error-file)
        (let* ((process (uiop:launch-program arguments
                                             :output output-file
                                             :if-output-exists :supersede
                                             :error-output error-file
                                             :if-error-output-exists :supersede))
               (exit (handler-case (sb-ext:with-timeout 20 (uiop:wait-process process))
                       (sb-ext:timeout ()
                         (uiop:terminate-process process :urgent t)
                         (sb-ext:with-timeout 5 (uiop:wait-process process))
                         (error "The partial-system child process did not exit within 20 seconds:~%~A"
                                (uiop:read-file-string error-file)))))
               (output (uiop:read-file-string output-file))
               (errors (uiop:read-file-string error-file)))
          (when (and ticket (not (eql 0 exit)))
            (write-string errors *error-output*))
          (ensure-equal 0 exit)
          (when ticket (funcall (find-symbol "ACCEPT-CHILD" bridge) ticket exit))
          (let* ((marker "CLIENT-PARTIAL-RESULT ")
                 (position (search marker output :from-end t)))
            (ensure-true position)
            (let ((*read-eval* nil))
              (read-from-string output nil nil :start (+ position (length marker))))))))))

#+sbcl
(deftest client-contract-partial-missing-tcp
  (ensure-equal
   '(t t :native-tcp :client "HTTP-KIT/NETWORK" 0 0 t)
   (client-contract-partial-result
    '(let* ((client (http-kit/client:make-http-client))
            (pool (http-kit/client:http-client-connection-pool client))
            (request (http-kit:make-http-request :method "GET" :uri "http://127.0.0.1/"
                                                 :body (make-array 3 :element-type '(unsigned-byte 8)
                                                                     :initial-contents '(1 2 3))))
            (body (http-kit:http-request-body request))
            (condition (handler-case (http-kit/client:http-client-send client request :timeout 2.0)
                         (http-kit:http-unsupported-feature (caught) caught))))
       (list (null (find-package "HTTP-KIT/NETWORK"))
             (null (find-package "HTTP-KIT/TLS"))
             (http-kit:http-unsupported-feature-name condition)
             (http-kit:http-error-operation condition)
             (http-kit:http-protocol-error-detail condition)
             (getf (http-kit/client:http-connection-pool-stats pool) :idle-count)
             (http-kit/client:http-connection-pool-clear pool)
             (and (equalp body #(1 2 3))
                  (not (eq body (http-kit:http-request-body request)))
                  (equalp body (http-kit:http-request-body request))
                  (string= "GET" (http-kit:http-request-method request))
                  (string= "http://127.0.0.1/" (http-kit:http-uri-string (http-kit:http-request-uri request)))))))))

#+sbcl
(deftest client-contract-partial-missing-dns
  (ensure-equal
   '(t t :native-dns :client "HTTP-KIT/NETWORK" 0 0)
   (client-contract-partial-result
    '(let* ((client (http-kit/client:make-http-client))
            (pool (http-kit/client:http-client-connection-pool client))
            (condition (handler-case
                           (funcall (http-kit/client:http-connection-pool-resolve-host pool) "127.0.0.1" :timeout 2.0)
                         (http-kit:http-unsupported-feature (caught) caught))))
       (list (null (find-package "HTTP-KIT/NETWORK"))
             (null (find-package "HTTP-KIT/TLS"))
             (http-kit:http-unsupported-feature-name condition)
             (http-kit:http-error-operation condition)
             (http-kit:http-protocol-error-detail condition)
             (getf (http-kit/client:http-connection-pool-stats pool) :idle-count)
             (http-kit/client:http-connection-pool-clear pool))))))

#+sbcl
(deftest client-contract-partial-missing-tls
  (ensure-equal
   '(t t :tls :client "HTTP-KIT/TLS" :eof t 0 0 t)
   (client-contract-partial-result
    '(let* ((listener (http-kit/network:open-http-tcp-listener :host "127.0.0.1"))
            (peer nil)
            (client (http-kit/client:make-http-client))
            (pool (http-kit/client:http-client-connection-pool client))
            (request (http-kit:make-http-request
                      :method "GET" :uri (format nil "https://127.0.0.1:~D/" (http-kit/network:http-network-listener-port listener))
                      :body (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3))))
            (body (http-kit:http-request-body request))
            (uri (http-kit:http-uri-string (http-kit:http-request-uri request))))
       (unwind-protect
            (let* ((condition (handler-case (http-kit/client:http-client-send client request :timeout 2.0)
                                (http-kit:http-unsupported-feature (caught) caught)))
                   (eof (progn
                          (setf peer (http-kit/network:accept-http-tcp-stream listener :timeout 2.0))
                          (sb-ext:with-timeout 2.0 (read-byte peer nil :eof)))))
              (http-kit/network:close-http-tcp-stream peer)
              (list (not (null (find-package "HTTP-KIT/NETWORK")))
                    (null (find-package "HTTP-KIT/TLS"))
                    (http-kit:http-unsupported-feature-name condition)
                    (http-kit:http-error-operation condition)
                    (http-kit:http-protocol-error-detail condition)
                    eof (not (open-stream-p peer))
                    (getf (http-kit/client:http-connection-pool-stats pool) :idle-count)
                    (http-kit/client:http-connection-pool-clear pool)
                    (and (equalp body #(1 2 3))
                         (not (eq body (http-kit:http-request-body request)))
                         (equalp body (http-kit:http-request-body request))
                         (string= "GET" (http-kit:http-request-method request))
                         (string= uri (http-kit:http-uri-string (http-kit:http-request-uri request))))))
         (when peer (http-kit/network:close-http-tcp-stream peer))
         (http-kit/network:close-http-tcp-listener listener)))
    :network t)))

#+sbcl
(deftest client-contract-public-pool-opener-recovery
  (let* ((original (make-condition 'simple-error :format-control "Public opener failure."))
         (timeout-condition (make-condition 'http-timeout :message "Public opener timeout." :operation :connect))
         (mode :error)
         (events nil)
         (stream (make-instance 'binary-test-stream
                               :input (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok")))
         (request (make-http-request :method "GET" :uri "http://example.com/recovery"))
         (snapshot (client-contract-public-request-snapshot request))
         (pool (make-http-connection-pool
                :open-stream (lambda (received &key timeout deadline)
                               (declare (ignore timeout deadline))
                               (ensure-true (eq request received))
                               (push :open events)
                               (ecase mode (:error (error original)) (:invalid 17)
                                 (:timeout (error timeout-condition)) (:success stream)))
                :close-stream (lambda (received)
                                (ensure-true (eq stream received))
                                (push :close events)
                                (close received)))))
    (unwind-protect
         (progn
           (let* ((plan (cons :mode 17))
                  (caught (handler-case
                              (http-connection-pool-send pool request :timeout 2.0 :proxy-plan plan)
                            (http-connection-error (condition) condition))))
             (ensure-true (typep caught 'http-connection-error))
             (ensure-equal :connect (http-error-operation caught))
             (ensure-equal "The connection pool opener signaled an error." (http-error-message caught))
             (ensure-true (typep (http-connection-error-cause caught) 'type-error))
             ;; Unknown opener failures must not permit an unsafe replay.
             (ensure-true (http-connection-error-request-sent-p caught))
             (ensure-equal '(:mode . 17) plan)
             (ensure-equal nil events)
             (ensure-equal 0 (getf (http-connection-pool-stats pool) :idle-count))
             (ensure-equal 0 (http-connection-pool-clear pool)))
           (let ((caught (handler-case (http-connection-pool-send pool request :timeout 2.0)
                           (http-proxy-error (condition) condition))))
             (ensure-true (typep caught 'http-proxy-error))
             (ensure-equal :proxy (http-error-operation caught))
             (ensure-equal "Proxy negotiation signaled an error." (http-error-message caught))
             (ensure-true (eq original (getf (http-client-error-detail caught) :cause))))
           (ensure-equal '(:open) (reverse events))
           (ensure-equal 0 (getf (http-connection-pool-stats pool) :idle-count))
           (setf mode :invalid)
           (let ((caught (handler-case (http-connection-pool-send pool request :timeout 2.0)
                           (http-proxy-error (condition) condition))))
             (ensure-true (typep caught 'http-proxy-error))
             (ensure-equal :proxy (http-error-operation caught))
             (ensure-equal "The proxy stream opener did not return a stream." (http-error-message caught))
             (ensure-equal 17 (getf (http-client-error-detail caught) :detail)))
           (setf mode :timeout)
           (ensure-true (eq timeout-condition (handler-case (http-connection-pool-send pool request :timeout 2.0)
                                     (http-timeout (condition) condition))))
           (ensure-equal '(:open :open :open) (reverse events))
           (ensure-equal 0 (http-connection-pool-clear pool))
           (setf mode :success)
           (let ((response (http-connection-pool-send pool request :timeout 2.0)))
             (ensure-equal 200 (http-response-status response))
             (ensure-octets-equal (ascii "ok") (http-response-body response)))
           (ensure-equal 1 (getf (http-connection-pool-stats pool) :idle-count))
           (ensure-equal '(:open :open :open :open) (reverse events))
           (ensure-equal 1 (http-connection-pool-clear pool))
           (ensure-equal '(:open :open :open :open :close) (reverse events))
           (ensure-equal 0 (http-connection-pool-clear pool))
           (ensure-equal '(:open :open :open :open :close) (reverse events))
           (ensure-true (search "GET /recovery HTTP/1.1" (octets-as-string (binary-test-output stream))))
           (client-contract-public-request-unchanged request snapshot))
      (http-connection-pool-clear pool)
      (close stream))))

(deftest client-contract-locationless-302-original-response
  (let* ((calls 0)
         (seen nil)
         (request (make-http-request :method "GET" :uri "https://example.test/start"))
         (snapshot (client-contract-public-request-snapshot request))
         (original (make-http-response :status 302 :body (ascii "no location")))
         (client (make-http-client
                  :cache nil
                  :transport-function
                  (lambda (current &key &allow-other-keys)
                    (incf calls)
                    (push current seen)
                    original))))
    (multiple-value-bind (response final) (http-client-send client request)
      (ensure-true (eq original response))
      (ensure-equal 1 calls)
      (ensure-equal 1 (length seen))
      (ensure-equal 302 (http-response-status response))
      (ensure-octets-equal (ascii "no location") (http-response-body response))
      (ensure-equal "https://example.test/start" (http-uri-string (http-request-uri final)))
      (ensure-equal "https://example.test/start" (http-uri-string (http-request-uri (first seen)))))
    (client-contract-public-request-unchanged request snapshot)))

(deftest client-contract-replayable-post-307-preserves-request
  (let* ((calls 0)
         (seen nil)
         (body (ascii "payload"))
         (request
           (make-http-request
            :method "POST" :uri "https://example.test/start" :body body
            :headers (list (make-http-header "Content-Type" "application/octet-stream")
                           (make-http-header "Content-Length" "7")
                           (make-http-header "Trailer" "X-End"))
            :trailers (list (make-http-header "X-End" "done"))))
         (snapshot (client-contract-public-request-snapshot request))
         (original (make-http-response :status 204))
         (client
           (make-http-client
            :cache nil
            :transport-function
            (lambda (current &key &allow-other-keys)
              (push current seen)
              (if (= 1 (incf calls))
                  (make-http-response :status 307
                                      :headers (list (make-http-header "Location" "/next")))
                  original)))))
    (multiple-value-bind (response final) (http-client-send client request)
      (ensure-true (eq original response))
      (ensure-equal 2 calls)
      (ensure-equal 2 (length seen))
      (let ((ordered (reverse seen)))
        (ensure-equal "/start" (http-kit:http-uri-path (http-request-uri (first ordered))))
        (ensure-equal "/next" (http-kit:http-uri-path (http-request-uri (second ordered))))
        (ensure-equal "/next" (http-kit:http-uri-path (http-request-uri final)))
        (dolist (current (append ordered (list final)))
          (ensure-equal "POST" (http-request-method current))
          (ensure-octets-equal body (http-request-body current))
          (ensure-equal "done" (http-header-value (http-request-trailers current) "X-End"))
          (ensure-equal "application/octet-stream"
                        (http-header-value (http-request-headers current) "Content-Type"))
          (ensure-equal "7" (http-header-value (http-request-headers current) "Content-Length"))
          (ensure-equal "X-End" (http-header-value (http-request-headers current) "Trailer")))))
    (client-contract-public-request-unchanged request snapshot)))

(defun client-contract-cache-wire-preserves-entry
    (header-name header-value status-line expected-status expected-body reason)
  (let* ((warm (make-instance 'binary-test-stream
                             :input (ascii "HTTP/1.1 200 OK|CRLF|Cache-Control: max-age=60|CRLF|Content-Length: 3|CRLF|Connection: close|CRLF||CRLF|old")))
         (forwarded
           (make-instance
            'binary-test-stream
            :input (ascii (format nil "HTTP/1.1 ~A|CRLF|Cache-Control: max-age=60|CRLF|Content-Length: ~D|CRLF|~AConnection: close|CRLF||CRLF|~A"
                                  status-line (length expected-body)
                                  (if (= expected-status 206)
                                      "Content-Range: bytes 0-0/3|CRLF|" "")
                                  expected-body))))
         (streams (list warm forwarded))
         (opened 0)
         (closed nil)
         (cache (make-http-cache :clock-function (lambda () 1000)
                                :status-identifier "edge"))
         (client
           (make-http-client
            :cache cache :automatic-decompression-p nil
            :clock-function (lambda () 1000)
            :open-stream
            (lambda (request &key &allow-other-keys)
              (ensure-equal "GET" (http-request-method request))
              (ensure-equal "/wire-cache" (http-kit:http-uri-path (http-request-uri request)))
              (ensure-true streams)
              (incf opened)
              (pop streams))
            :close-stream
            (lambda (stream)
              (ensure-true (member stream (list warm forwarded) :test #'eq))
              (ensure-false (member stream closed :test #'eq))
              (push stream closed)
              (close stream))))
         (request (http-client-request client "GET" "http://example.test/wire-cache"))
         (control (http-client-request client "GET" "http://example.test/wire-cache"
                                       :headers (list (make-http-header header-name header-value))))
         (request-snapshot (client-contract-public-request-snapshot request))
         (control-snapshot (client-contract-public-request-snapshot control)))
    (unwind-protect
         (progn
           (let ((response (http-client-send client request)))
             (ensure-equal 200 (http-response-status response))
             (ensure-octets-equal (ascii "old") (http-response-body response))
             (ensure-equal "\"edge\"; fwd=miss; stored"
                           (http-header-value (http-response-headers response) "Cache-Status")))
           (ensure-equal 1 opened)
           (ensure-equal 1 (length closed))
           (ensure-true (eq warm (first closed)))
           (ensure-equal 1 (length (http-cache-entries cache)))
           (multiple-value-bind (response state entry forward-reason)
               (http-cache-lookup cache control)
             (ensure-false response)
             (ensure-equal :miss state)
             (ensure-false entry)
             (ensure-equal reason forward-reason))
           (let ((response (http-client-send client control)))
             (ensure-equal expected-status (http-response-status response))
             (ensure-octets-equal (ascii expected-body) (http-response-body response))
             (ensure-equal (format nil "\"edge\"; fwd=~(~A~)" reason)
                           (http-header-value (http-response-headers response) "Cache-Status")))
           (ensure-equal 2 opened)
           (ensure-equal 2 (length closed))
           (ensure-true (eq forwarded (first closed)))
           (ensure-true (eq warm (second closed)))
           (ensure-false streams)
           (ensure-false (open-stream-p warm))
           (ensure-false (open-stream-p forwarded))
           (ensure-equal 1 (length (http-cache-entries cache)))
           (ensure-equal 0 (search (ascii "GET /wire-cache HTTP/1.1|CRLF|")
                                   (binary-test-output warm)))
           (ensure-equal 0 (search (ascii "GET /wire-cache HTTP/1.1|CRLF|")
                                   (binary-test-output forwarded)))
           (ensure-true (search (ascii (format nil "~A: ~A|CRLF|" header-name header-value))
                                (binary-test-output forwarded)))
           (ensure-false (search (ascii (format nil "~A:" header-name))
                                 (binary-test-output warm)))
           (let ((response (http-client-send client request)))
             (ensure-equal 200 (http-response-status response))
             (ensure-octets-equal (ascii "old") (http-response-body response))
             (ensure-equal "\"edge\"; hit; ttl=60"
                           (http-header-value (http-response-headers response) "Cache-Status")))
           (ensure-equal 2 opened)
           (ensure-equal 2 (length closed))
           (ensure-equal 1 (length (http-cache-entries cache)))
           (client-contract-public-request-unchanged request request-snapshot)
           (client-contract-public-request-unchanged control control-snapshot))
      (dolist (stream (list warm forwarded))
        (when (open-stream-p stream) (close stream))))))

(deftest client-contract-cache-no-store-wire-preserves-entry
  (client-contract-cache-wire-preserves-entry
   "Cache-Control" "no-store" "200 OK" 200 "new" :bypass))

(deftest client-contract-cache-range-wire-preserves-entry
  (client-contract-cache-wire-preserves-entry
   "Range" "bytes=0-0" "206 Partial Content" 206 "o" :partial))

(deftest client-contract-cookie-positive-signed-max-age
  (let* ((uri (parse-http-uri "https://example.test/account"))
         (uri-before (copy-seq (http-uri-string uri)))
         (jar (make-http-cookie-jar :clock-function (lambda () 1000)))
         (field (copy-seq "sid=live; Path=/; Max-Age=+60"))
         (field-before (copy-seq field))
         (headers (list (make-http-header "Set-Cookie" field)))
         (response (client-test-response 200 :headers headers)))
    (unwind-protect
         (progn
           (ensure-true (eq jar (http-cookie-jar-accept-response
                                 jar uri response :now 1000)))
           (let ((cookies (http-cookie-jar-cookies jar)))
             (ensure-equal 1 (length cookies))
             (let ((cookie (first cookies)))
               (ensure-equal "sid" (http-cookie-name cookie))
               (ensure-equal "live" (http-cookie-value cookie))
               (ensure-equal "example.test" (http-cookie-domain cookie))
               (ensure-equal "/" (http-cookie-path cookie))
               (ensure-equal 60 (http-cookie-max-age cookie))
               (ensure-equal 1000 (http-cookie-creation-time cookie))
               (ensure-equal 1060 (http-cookie-expiry-time cookie))))
           (ensure-equal "sid=live"
                         (http-cookie-jar-cookie-header jar uri :now 1059))
           (ensure-false (http-cookie-jar-cookie-header jar uri :now 1060))
           (ensure-false (http-cookie-jar-cookies jar))
           (ensure-equal 200 (http-response-status response))
           (ensure-equal field-before
                         (http-header-value (http-response-headers response)
                                            "Set-Cookie"))
           (ensure-equal field-before (http-header-value headers "Set-Cookie"))
           (ensure-equal field-before field)
           (ensure-equal uri-before (http-uri-string uri)))
      (ensure-true (eq jar (http-cookie-jar-clear jar))))))

(deftest client-contract-cookie-signed-max-age-replacement-controls
  (dolist (value '("-1" "+0" "0" "+" "-" "+1x" "12x"))
    (let* ((uri (parse-http-uri "https://example.test/"))
           (uri-before (copy-seq (http-uri-string uri)))
           (jar (make-http-cookie-jar :clock-function (lambda () 1000)))
           (field (format nil "sid=new; Path=/; Max-Age=~A" value))
           (field-before (copy-seq field))
           (headers (list (make-http-header "Set-Cookie" field)))
           (response (client-test-response 200 :headers headers)))
      (unwind-protect
           (progn
             (ensure-true
              (eq jar (http-cookie-jar-accept-response
                       jar uri
                       (client-test-response
                        200 :headers (list (make-http-header
                                            "Set-Cookie"
                                            "sid=old; Path=/; Max-Age=60")))
                       :now 1000)))
             (ensure-equal "sid=old"
                           (http-cookie-jar-cookie-header jar uri :now 1000))
             (ensure-true (eq jar (http-cookie-jar-accept-response
                                   jar uri response :now 1000)))
             (if (member value '("-1" "+0" "0") :test #'string=)
                 (progn
                   (ensure-false (http-cookie-jar-cookie-header jar uri :now 1000))
                   (ensure-false (http-cookie-jar-cookies jar)))
                 (let ((cookies (http-cookie-jar-cookies jar)))
                   (ensure-equal 1 (length cookies))
                   (let ((cookie (first cookies)))
                     (ensure-equal "new" (http-cookie-value cookie))
                     (ensure-false (http-cookie-max-age cookie))
                     (ensure-false (http-cookie-expiry-time cookie))
                     (ensure-equal 1000 (http-cookie-creation-time cookie)))
                   (ensure-equal "sid=new"
                                 (http-cookie-jar-cookie-header jar uri :now 2000))))
             (ensure-equal 200 (http-response-status response))
             (ensure-equal field-before
                           (http-header-value (http-response-headers response)
                                              "Set-Cookie"))
             (ensure-equal field-before (http-header-value headers "Set-Cookie"))
             (ensure-equal field-before field)
             (ensure-equal uri-before (http-uri-string uri)))
        (ensure-true (eq jar (http-cookie-jar-clear jar)))))))

