(in-package #:http-kit/client)

(defun %client-empty-octets ()
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun %client-body-octets (body)
  (cond
    ((null body)
     (%client-empty-octets))
    ((stringp body)
     (http-utf8-octets body))
    ((and (arrayp body) (= (array-rank body) 1))
     (let ((octets (make-array (array-total-size body)
                               :element-type '(unsigned-byte 8))))
       (dotimes (index (length octets) octets)
         (let ((value (row-major-aref body index)))
           (unless (and (integerp value) (<= 0 value 255))
             (%client-protocol-error
              "Request bodies must contain unsigned octets."
              value))
           (setf (aref octets index) value)))))
    (t
     (%client-protocol-error
      "Request bodies must be strings, one-dimensional arrays, or NIL."
      body))))

(defun %client-copy-header-list (headers)
  (mapcar (lambda (header)
            (make-http-header (http-header-name header)
                              (http-header-content header)))
          headers))

(defun %client-normalize-headers (headers)
  ;; Let the core request constructor retain its single header validation path.
  (http-request-headers
   (make-http-request :method "GET"
                      :uri "http://example.com/"
                      :headers headers
                      :body (%client-empty-octets))))

(defun %client-header-named-p (header names)
  (member (http-header-name header) names :test #'string-equal))

(defun %client-remove-headers (headers names)
  (remove-if (lambda (header)
               (%client-header-named-p header names))
             headers))

(defun %client-header-value (headers name)
  (http-header-value headers name nil))

(defun %client-header-set (headers name value)
  (append (%client-remove-headers headers (list name))
          (list (make-http-header name value))))

(defun %client-request-rebuild
    (request &key method uri headers body (trailers nil trailers-supplied-p))
  (make-http-request :method (or method (http-request-method request))
                     :protocol-version (http-request-protocol-version request)
                     :uri (or uri (http-request-uri request))
                     :headers (or headers (http-request-headers request))
                     :trailers (if trailers-supplied-p
                                   trailers
                                   (http-kit:http-request-trailers request))
                     :body (if (null body)
                               (%client-empty-octets)
                               body)))

(defun %client-monotonic-time ()
  (/ (float (get-internal-real-time))
     internal-time-units-per-second))

(defun %client-validate-limit (value message)
  (when value
    (%ensure-nonnegative-integer value message))
  value)

(defun %client-validate-request-body-stream
    (request request-body-function request-body-factory request-body-length)
  (when request-body-function
    (%ensure-function request-body-function
                      "The request body producer must be a function."))
  (when request-body-factory
    (%ensure-function request-body-factory
                      "The request body factory must be a function."))
  (when (and request-body-function request-body-factory)
    (%client-protocol-error
     "A request body producer cannot be combined with a request body factory."
     request))
  (%client-validate-limit
   request-body-length
   "The request body length must be a non-negative integer or NIL.")
  (when (and request-body-length
             (null (or request-body-function request-body-factory)))
    (%client-protocol-error
     "A request body length requires a request body producer or factory."
     request-body-length))
  (when (and (or request-body-function request-body-factory)
             (plusp (length (http-request-body request))))
    (%client-protocol-error
     "A request body producer or factory cannot be combined with an in-memory request body."
     request))
  (values request-body-function request-body-factory request-body-length))

(defun %client-request-body-for-attempt
    (request-body-function request-body-factory)
  (if request-body-factory
      (%ensure-function
       (funcall request-body-factory)
       "A request body factory must return a fresh producer function.")
      request-body-function))

(defun %client-transport-from-stream-boundary (open-stream close-stream)
  (lambda (request &key timeout deadline max-header-bytes max-body-bytes
                         request-target request-body-function request-body-length
                         on-body-chunk on-information (collect-body-p t)
                         proxy-plan proxy)
    (send-http-request-over-stream
     request
     :open-stream
     (lambda (stream-request &key timeout deadline)
       (funcall open-stream
                stream-request
                :timeout timeout
                :deadline deadline
                :proxy-plan proxy-plan
                :proxy proxy))
     :close-stream close-stream
     :timeout timeout
     :deadline deadline
     :max-header-bytes max-header-bytes
     :max-body-bytes max-body-bytes
     :request-target
     (or request-target
         (and (eq (getf proxy-plan :mode) :forward)
              (getf proxy-plan :request-target)))
     :request-body-function request-body-function
     :request-body-length request-body-length
     :on-body-chunk on-body-chunk
     :on-information on-information
     :collect-body-p collect-body-p
     :clock-function #'%client-monotonic-time)))

(defun %client-connection-key (request proxy-plan)
  (let ((proxy (and proxy-plan (getf proxy-plan :proxy))))
    (list (http-uri-origin (http-request-uri request))
          (and proxy-plan (getf proxy-plan :mode))
          (and proxy-plan (getf proxy-plan :connect-host))
          (and proxy-plan (getf proxy-plan :connect-port))
          (and proxy-plan (getf proxy-plan :proxy-host))
          (and proxy-plan (getf proxy-plan :proxy-port))
          (and proxy-plan (getf proxy-plan :proxy-authorization))
          (and proxy
               (list (http-proxy-scheme proxy)
                     (http-proxy-host proxy)
                     (http-proxy-port proxy))))))

(defun %client-transport-from-connection-pool (connection-pool)
  (lambda (request &key timeout deadline max-header-bytes max-body-bytes
                         request-target request-body-function request-body-length
                         on-body-chunk on-information (collect-body-p t)
                         proxy-plan proxy)
    (http-connection-pool-send
     connection-pool
     request
     :key (%client-connection-key request proxy-plan)
     :timeout timeout
     :deadline deadline
     :max-header-bytes max-header-bytes
     :max-body-bytes max-body-bytes
     :proxy-plan proxy-plan
     :proxy proxy
     :request-target request-target
     :request-body-function request-body-function
     :request-body-length request-body-length
     :on-body-chunk on-body-chunk
     :on-information on-information
     :collect-body-p collect-body-p)))

(defun make-http-client
    (&key transport-function open-stream close-stream connection-pool
          (default-headers nil)
          (cookie-jar (make-http-cookie-jar))
          (cache nil)
          (redirect-policy (make-http-redirect-policy))
          (retry-policy (make-http-retry-policy))
          proxy tls-upgrade resolve-host auth-provider
          (clock-function #'get-universal-time)
          (wall-clock-function #'%client-monotonic-time)
          (sleep-function #'sleep)
          max-header-bytes max-body-bytes
          on-request on-response)
  "Construct a policy-driven HTTP client around an injected transport.

TRANSPORT-FUNCTION is called with REQUEST and keyword arguments including
:TIMEOUT, :DEADLINE, :MAX-HEADER-BYTES, :MAX-BODY-BYTES, :PROXY, and
:PROXY-PLAN.  Alternatively OPEN-STREAM and CLOSE-STREAM can be supplied to
use the core HTTP/1.1 stream transport.  CONNECTION-POOL supplies a reusable
stream boundary and owns opening and closing pooled streams.  TLS-UPGRADE and
RESOLVE-HOST customize the injected stream boundary when OPEN-STREAM is used."
  (when (or (and transport-function open-stream)
            (and transport-function connection-pool)
            (and open-stream connection-pool)
            (and close-stream connection-pool))
    (%client-protocol-error
     "Specify only one of :TRANSPORT-FUNCTION, :OPEN-STREAM, or :CONNECTION-POOL."
     (list transport-function open-stream connection-pool close-stream)))
  (unless (or transport-function open-stream connection-pool)
    (%client-protocol-error
     "An HTTP client requires :TRANSPORT-FUNCTION, :OPEN-STREAM, or :CONNECTION-POOL."
     nil))
  (when transport-function
    (%ensure-function transport-function
                      "The client transport must be a function."))
  (when open-stream
    (%ensure-function open-stream
                      "The stream opener must be a function."))
  (when connection-pool
    (unless (http-connection-pool-p connection-pool)
      (%client-protocol-error
       "The client connection pool must be an HTTP-CONNECTION-POOL."
       connection-pool)))
  (when (and (or transport-function connection-pool)
             (or tls-upgrade resolve-host))
    (%client-protocol-error
     ":TLS-UPGRADE and :RESOLVE-HOST require the client :OPEN-STREAM boundary."
     (list tls-upgrade resolve-host)))
  (%ensure-function clock-function "The client clock must be a function.")
  (%ensure-function wall-clock-function
                    "The client wall clock must be a function.")
  (%ensure-function sleep-function "The client sleep function must be a function.")
  (when close-stream
    (%ensure-function close-stream "The stream close function must be a function."))
  (when auth-provider
    (%ensure-function auth-provider "The authentication provider must be a function."))
  (when tls-upgrade
    (%ensure-function tls-upgrade "The TLS upgrade function must be a function."))
  (when resolve-host
    (%ensure-function resolve-host "The host resolver must be a function."))
  (when on-request
    (%ensure-function on-request "The request hook must be a function."))
  (when on-response
    (%ensure-function on-response "The response hook must be a function."))
  (unless (http-cookie-jar-p cookie-jar)
    (%client-protocol-error "The client cookie jar must be an HTTP-COOKIE-JAR." cookie-jar))
  (when cache
    (unless (http-cache-p cache)
      (%client-protocol-error "The client cache must be an HTTP-CACHE." cache)))
  (unless (http-redirect-policy-p redirect-policy)
    (%client-protocol-error
     "The client redirect policy must be an HTTP-REDIRECT-POLICY."
     redirect-policy))
  (unless (http-retry-policy-p retry-policy)
    (%client-protocol-error
     "The client retry policy must be an HTTP-RETRY-POLICY."
     retry-policy))
  (%client-validate-limit
   max-header-bytes
   "The maximum header size must be a non-negative integer or NIL.")
  (%client-validate-limit
   max-body-bytes
   "The maximum body size must be a non-negative integer or NIL.")
  (let* ((effective-close-stream (or close-stream #'close))
         (transport (or transport-function
                        (and connection-pool
                             (%client-transport-from-connection-pool
                              connection-pool))
                        (%client-transport-from-stream-boundary
                         (make-http-proxy-stream-opener
                          open-stream effective-close-stream
                          :tls-upgrade tls-upgrade
                          :resolve-host resolve-host
                          :clock-function #'%client-monotonic-time)
                         effective-close-stream))))
    (%make-http-client
     :transport-function transport
     :connection-pool connection-pool
     :default-headers (%client-normalize-headers default-headers)
     :cookie-jar cookie-jar
     :cache cache
     :redirect-policy redirect-policy
     :retry-policy retry-policy
     :proxy proxy
     :tls-upgrade tls-upgrade
     :resolve-host resolve-host
     :auth-provider auth-provider
     :clock-function clock-function
     :wall-clock-function wall-clock-function
     :sleep-function sleep-function
     :max-header-bytes max-header-bytes
     :max-body-bytes max-body-bytes
     :on-request on-request
     :on-response on-response)))

(defun http-client-request
    (client method uri &key headers trailers body)
  "Build a validated HTTP-REQUEST using CLIENT defaults.

BODY may be a string, a one-dimensional octet array, or NIL.  TRAILERS is a
list of HTTP headers sent after a streaming or chunked request body.  Explicit
headers replace client default headers with the same case-insensitive name."
  (unless (http-client-p client)
    (%client-protocol-error "The client must be an HTTP-CLIENT." client))
  (let* ((defaults (http-client-default-headers client))
         (explicit (%client-normalize-headers headers))
         (names (remove-duplicates (mapcar #'http-header-name explicit)
                                   :test #'string-equal))
         (combined (append (%client-remove-headers defaults names)
                           explicit)))
    (make-http-request :method method
                       :uri (%client-uri uri)
                       :headers combined
                       :trailers trailers
                       :body (%client-body-octets body))))

(defun %client-request-with
    (request &key headers body method uri (trailers nil trailers-supplied-p))
  (%client-request-rebuild
   request
   :method method
   :uri uri
   :headers headers
   :trailers (if trailers-supplied-p
                 trailers
                 (http-kit:http-request-trailers request))
   :body (if (null body)
             (http-request-body request)
             (%client-body-octets body))))

(defun %client-validate-collect-body-p (value)
  (unless (member value '(nil t) :test #'eq)
    (%client-protocol-error
     "The body collection flag must be NIL or T."
     value))
  value)

(defun %client-deliver-body-chunk (response on-body-chunk)
  (let ((body (http-response-body response)))
    (when (and on-body-chunk body (plusp (length body)))
      (funcall on-body-chunk body)))
  response)

(defun %client-request-with-proxy-authorization (request proxy-plan)
  (let ((authorization
          (and proxy-plan
               (eq (getf proxy-plan :mode) :forward)
               (getf proxy-plan :proxy-authorization))))
    (if (and authorization
             (not (http-header-present-p
                   (http-request-headers request)
                   "Proxy-Authorization")))
        (%client-request-with
         request
         :headers (%client-header-set
                   (http-request-headers request)
                   "Proxy-Authorization"
                   authorization))
        request)))

(defun %client-retry-method-p (policy request)
  (member (string-upcase (http-request-method request))
          (http-retry-policy-methods policy)
          :test #'string-equal))

(defun %client-retry-status-p (policy response)
  (member (http-response-status response)
          (http-retry-policy-statuses policy)
          :test #'eql))

(defun %client-retryable-condition-p (policy condition)
  (or (and (typep condition 'http-timeout)
           (http-retry-policy-retry-on-timeout-p policy))
      (and (typep condition 'http-connection-error)
           (http-retry-policy-retry-on-connection-error-p policy))))

(defun %client-retry-delay (client policy response attempt)
  (let* ((retry-after (and (http-retry-policy-respect-retry-after-p policy)
                           response
                           (%retry-after-seconds
                            (http-response-headers response)
                            (funcall (http-client-clock-function client)))))
         (exponential (* (float (http-retry-policy-base-delay policy))
                         (expt 2 (1- attempt)))))
    (min (float (http-retry-policy-max-delay policy))
         (float (or retry-after exponential)))))

(defun %client-sleep-before-retry (client policy response attempt)
  (let ((delay (%client-retry-delay client policy response attempt)))
    (when (plusp delay)
      (funcall (http-client-sleep-function client) delay))))

(defun %client-call-transport
    (client request proxy-plan &key timeout deadline request-body-function
                                      request-body-length on-body-chunk
                                      on-information
                                      (collect-body-p t))
  (let* ((arguments
           (list :timeout timeout
                 :deadline deadline
                 :max-header-bytes (http-client-max-header-bytes client)
                 :max-body-bytes (http-client-max-body-bytes client)
                 :proxy (http-client-proxy client)
                 :proxy-plan proxy-plan))
         (effective-request
           (%client-request-with-proxy-authorization request proxy-plan)))
    ;; Preserve compatibility with transports that predate the streaming
    ;; keywords while exposing them whenever the caller uses the feature.
    (when on-body-chunk
      (setf arguments
            (append arguments (list :on-body-chunk on-body-chunk))))
    (when on-information
      (setf arguments
            (append arguments (list :on-information on-information))))
    (when (or request-body-function request-body-length)
      (setf arguments
            (append arguments
                    (list :request-body-function request-body-function
                          :request-body-length request-body-length))))
    (unless collect-body-p
      (setf arguments
            (append arguments (list :collect-body-p nil))))
    (let ((response
            (apply (http-client-transport-function client)
                   effective-request
                   arguments)))
    (unless (http-response-p response)
      (%client-protocol-error
       "The client transport must return an HTTP-RESPONSE."
       response))
      response)))

(defun %client-attempt
    (client request proxy-plan policy &key timeout deadline request-body-function
                                                request-body-factory request-body-length
                                                on-body-chunk
                                                on-information
                                                (collect-body-p t))
  (let* ((attempt 1)
         (max-attempts (http-retry-policy-max-attempts policy))
         (last-response nil)
         (retryable-request-p
           (and (%client-retry-method-p policy request)
                (or (null request-body-function)
                    request-body-factory))))
    (loop
      (when (http-client-on-request client)
        (funcall (http-client-on-request client) request attempt))
      (let ((attempt-body-function
              (%client-request-body-for-attempt
               request-body-function request-body-factory)))
        (handler-case
          (let ((response (%client-call-transport
                           client request proxy-plan
                           :timeout timeout
                           :deadline deadline
                           :request-body-function attempt-body-function
                           :request-body-length request-body-length
                           :on-body-chunk on-body-chunk
                           :on-information on-information
                           :collect-body-p collect-body-p)))
            (setf last-response response)
            (http-cookie-jar-accept-response
             (http-client-cookie-jar client)
             (http-request-uri request)
             response)
            (if (and (< attempt max-attempts)
                     retryable-request-p
                     (%client-retry-status-p policy response))
                (progn
                  (%client-sleep-before-retry client policy response attempt)
                  (incf attempt))
                (progn
                  (when (and (> max-attempts 1)
                             retryable-request-p
                             (%client-retry-status-p policy response))
                    (%client-retry-exhausted-error
                     attempt
                     :request request
                     :last-response response))
                  (when (http-client-on-response client)
                    (funcall (http-client-on-response client) response request attempt))
                  (return (values response attempt)))))
        (http-error (condition)
          (if (and (< attempt max-attempts)
                   retryable-request-p
                   (%client-retryable-condition-p policy condition))
              (progn
                (%client-sleep-before-retry client policy last-response attempt)
                (incf attempt))
               (if (and (> max-attempts 1)
                        retryable-request-p
                        (%client-retryable-condition-p policy condition))
                   (%client-retry-exhausted-error
                    attempt
                    :request request
                   :last-condition condition
                   :last-response last-response)
                   (error condition)))))))))

(defun %client-authorization-value (value)
  (cond
    ((null value) nil)
    ((stringp value) value)
    ((http-header-p value) (http-header-content value))
    (t
     (%client-protocol-error
      "An authentication provider must return a string, HTTP-HEADER, or NIL."
      value))))

(defun %client-prepare-request (client request initial-uri &key redirect-p)
  (let ((headers (http-request-headers request)))
    (when redirect-p
      (setf headers (%client-remove-headers headers '("Cookie"))))
    (let ((cookie-value
            (http-cookie-jar-cookie-header
             (http-client-cookie-jar client)
             (http-request-uri request))))
      (when cookie-value
        (setf headers (%client-header-set headers "Cookie" cookie-value))))
    (when (and (http-client-auth-provider client)
               (http-same-origin-p initial-uri (http-request-uri request))
               (not (http-header-present-p headers "Authorization")))
      (let ((value (%client-authorization-value
                    (funcall (http-client-auth-provider client) request))))
        (when value
          (setf headers (%client-header-set headers "Authorization" value)))))
    (%client-request-with request :headers headers)))

(defun %client-response-merge-304 (cached response)
  (let* ((new-headers (http-response-headers response))
         (names (remove-duplicates (mapcar #'http-header-name new-headers)
                                   :test #'string-equal))
         (headers (append (%client-remove-headers
                           (http-response-headers cached) names)
                          (%client-copy-header-list new-headers))))
    (make-http-response
     :protocol-version (http-response-protocol-version cached)
     :status (http-response-status cached)
     :reason (http-response-reason cached)
     :headers headers
     :trailers (http-response-trailers response)
     :body (http-response-body cached))))

(defun %client-redirect-method (method status)
  (cond
    ((= status 303)
     (if (string-equal method "HEAD") "HEAD" "GET"))
    ((and (member status '(301 302) :test #'eql)
          (string-equal method "POST"))
     "GET")
    (t method)))

(defun %client-redirect-request
    (request response policy initial-uri redirect-count)
  (declare (ignore redirect-count))
  (let ((location (%client-header-value
                   (http-response-headers response) "Location")))
    (unless location
      (return-from %client-redirect-request nil))
    (let* ((target (resolve-http-uri (http-request-uri request) location))
           (old-scheme (http-uri-scheme (http-request-uri request)))
           (new-scheme (http-uri-scheme target))
           (downgrade (and (string-equal old-scheme "https")
                           (string-equal new-scheme "http")))
           (new-method (%client-redirect-method
                        (http-request-method request)
                        (http-response-status response)))
           (method-changed (not (string-equal new-method
                                               (http-request-method request))))
           (headers (%client-remove-headers
                     (http-request-headers request)
                     '("Cookie"))))
      (when (and downgrade
                 (not (http-redirect-policy-allow-downgrade-p policy)))
        (%client-error
         'http-client-error
         "The redirect policy rejected an HTTPS to HTTP downgrade."
         :detail target
         :operation :redirect))
      (unless (or (http-redirect-policy-preserve-authorization-p policy)
                  (http-same-origin-p initial-uri target))
        (setf headers (%client-remove-headers headers '("Authorization"))))
      (when method-changed
        (setf headers (%client-remove-headers
                       headers '("Content-Length" "Content-Type"
                                 "Transfer-Encoding"))))
      (%client-request-rebuild
       request
       :method new-method
       :uri target
       :headers headers
       :trailers (unless method-changed
                   (http-kit:http-request-trailers request))
       :body (if method-changed
                 (%client-empty-octets)
                 (http-request-body request))))))

(defun %client-cacheable-method-p (request)
  (member (string-upcase (http-request-method request)) '("GET" "HEAD")
          :test #'string=))

(defun %client-mutating-method-p (request)
  (not (%client-cacheable-method-p request)))

(defun %client-conditional-request (request entry)
  (let ((headers (http-request-headers request)))
    (when (http-cache-entry-etag entry)
      (setf headers (%client-header-set
                     headers "If-None-Match" (http-cache-entry-etag entry))))
    (when (and (http-cache-entry-last-modified entry)
               (not (http-cache-entry-etag entry)))
      (setf headers (%client-header-set
                     headers "If-Modified-Since"
                     (http-cache-entry-last-modified entry))))
    (%client-request-with request :headers headers)))

(defun http-client-send
    (client request &key timeout deadline redirect-policy retry-policy
                         request-body-function request-body-factory
                         request-body-length
                         on-body-chunk on-information (collect-body-p t))
  "Execute REQUEST with redirects, retries, cookies, cache, auth, and proxy policy.

Returns the final HTTP-RESPONSE as the primary value and the effective
HTTP-REQUEST as a secondary value.  TIMEOUT and DEADLINE are passed through to
the configured transport boundary.  REQUEST-BODY-FACTORY, when supplied,
must return a fresh producer function on every invocation.  Streaming request
bodies without a factory are sent once and are not automatically retried or
resent across same-method redirects."
  (unless (http-client-p client)
    (%client-protocol-error "The client must be an HTTP-CLIENT." client))
  (unless (http-request-p request)
    (%client-protocol-error "The request must be an HTTP-REQUEST." request))
  (%client-validate-request-body-stream
   request request-body-function request-body-factory request-body-length)
  (when on-body-chunk
    (%ensure-function on-body-chunk
                      "The response body callback must be a function."))
  (when on-information
    (%ensure-function on-information
                      "The informational response callback must be a function."))
  (%client-validate-collect-body-p collect-body-p)
  (let* ((redirect-policy (or redirect-policy
                             (http-client-redirect-policy client)))
         (retry-policy (or retry-policy
                           (http-client-retry-policy client)))
         (initial-uri (http-request-uri request))
         (current-request request)
         (redirect-count 0)
         (stale-entry nil))
    (unless (http-redirect-policy-p redirect-policy)
      (%client-protocol-error "The redirect policy must be an HTTP-REDIRECT-POLICY."
                              redirect-policy))
    (unless (http-retry-policy-p retry-policy)
      (%client-protocol-error "The retry policy must be an HTTP-RETRY-POLICY."
                              retry-policy))
    (when (and (http-client-cache client)
               (%client-cacheable-method-p current-request))
      (multiple-value-bind (response state entry)
          (http-cache-lookup (http-client-cache client) current-request)
        (when (eq state :fresh)
          (%client-deliver-body-chunk response on-body-chunk)
          (when (http-client-on-response client)
            (funcall (http-client-on-response client) response current-request 0))
          (return-from http-client-send (values response current-request)))
        (when (eq state :stale)
          (setf stale-entry entry
                current-request (%client-conditional-request
                                 current-request entry)))))
    (loop
      (let* ((redirect-p (plusp redirect-count))
             (prepared (%client-prepare-request
                        client current-request initial-uri :redirect-p redirect-p))
             (proxy-plan (http-proxy-plan
                          (http-client-proxy client)
                          (http-request-uri prepared))))
        (multiple-value-bind (response)
            (%client-attempt client prepared proxy-plan retry-policy
                             :timeout timeout :deadline deadline
                             :request-body-function request-body-function
                             :request-body-factory request-body-factory
                             :request-body-length request-body-length
                             :on-body-chunk on-body-chunk
                             :on-information on-information
                             :collect-body-p collect-body-p)
          (when (and stale-entry (= (http-response-status response) 304))
            (setf response (%client-response-merge-304
                            (http-cache-entry-response stale-entry)
                            response))
            (%client-deliver-body-chunk response on-body-chunk))
          (let* ((redirect-request
                   (and (member (http-response-status response)
                                (http-redirect-policy-statuses redirect-policy)
                                :test #'eql)
                        (%client-header-value
                         (http-response-headers response) "Location")))
                 (next-request
                   (and redirect-request
                        (%client-redirect-request
                         prepared response redirect-policy initial-uri
                         redirect-count)))
                 (follow-redirect-p
                   (and next-request
                        (or (null request-body-function)
                            request-body-factory
                            (not (string-equal
                                  (http-request-method next-request)
                                  (http-request-method prepared)))))))
            (if follow-redirect-p
                (if (>= redirect-count
                        (http-redirect-policy-max-redirects redirect-policy))
                    (%client-redirect-limit-error
                     (http-request-uri prepared)
                     redirect-count)
                    (progn
                      (when (not (string-equal
                                  (http-request-method next-request)
                                  (http-request-method prepared)))
                        (setf request-body-function nil
                              request-body-factory nil
                              request-body-length nil))
                      (setf current-request next-request)
                      (setf redirect-count (1+ redirect-count)
                            stale-entry nil)
                      (when (and (http-client-cache client)
                                 (%client-mutating-method-p current-request))
                        (http-cache-clear (http-client-cache client)))))
                (progn
                  (when (http-client-cache client)
                    (if (= (http-response-status response) 304)
                        (http-cache-store
                         (http-client-cache client)
                         prepared
                         response)
                        (if (%client-cacheable-method-p prepared)
                            (http-cache-store
                             (http-client-cache client)
                             prepared
                             response)
                            (when (%client-mutating-method-p prepared)
                              (http-cache-invalidate
                               (http-client-cache client) prepared)))))
                  (return (values response prepared))))))))))
