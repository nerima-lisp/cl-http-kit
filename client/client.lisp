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

(defun make-http-strict-transport-store (&key (clock-function #'get-universal-time))
  (%ensure-function clock-function "The strict transport clock must be a function.")
  (%make-http-strict-transport-store :clock-function clock-function))

(defun %strict-transport-prune (store)
  (let ((now (funcall (%http-strict-transport-store-clock-function store))))
    (setf (%http-strict-transport-store-policies store)
          (delete-if (lambda (policy)
                       (<= (http-strict-transport-policy-expires-at policy) now))
                     (%http-strict-transport-store-policies store)))))

(defun http-strict-transport-store-policies (store)
  (unless (http-strict-transport-store-p store)
    (%client-protocol-error
     "Expected an HTTP-STRICT-TRANSPORT-STORE value." store))
  (%strict-transport-prune store)
  (copy-list (%http-strict-transport-store-policies store)))

(defun http-strict-transport-store-clear (store)
  (unless (http-strict-transport-store-p store)
    (%client-protocol-error
     "Expected an HTTP-STRICT-TRANSPORT-STORE value." store))
  (setf (%http-strict-transport-store-policies store) nil)
  store)

(defun %strict-transport-token-character-p (character)
  (and (<= 33 (char-code character) 126)
       (not (find character "()<>@,;:\"/[]?={}\\" :test #'char=))))

(defun %strict-transport-value (text)
  (let ((text (string-trim '(#\Space #\Tab) text)))
    (cond
      ((zerop (length text)) nil)
      ((char= (char text 0) #\")
       (when (and (> (length text) 1)
                  (char= (char text (1- (length text))) #\"))
         (with-output-to-string (output)
           (loop with escaped-p = nil
                 for index from 1 below (1- (length text))
                 for character = (char text index)
                 do (cond
                      (escaped-p
                       (when (or (< (char-code character) 32)
                                 (= (char-code character) 127))
                         (return-from %strict-transport-value nil))
                       (write-char character output)
                       (setf escaped-p nil))
                      ((char= character #\\)
                       (setf escaped-p t))
                      ((or (< (char-code character) 32)
                           (= (char-code character) 127)
                           (char= character #\"))
                       (return-from %strict-transport-value nil))
                      (t (write-char character output)))
                 finally (when escaped-p
                           (return-from %strict-transport-value nil))))))
      ((every #'%strict-transport-token-character-p text) text)
      (t nil))))

(defun %strict-transport-parse (value)
  (let ((start 0)
        (seen (make-hash-table :test #'equalp))
        (max-age nil)
        (include-subdomains-p nil))
    (loop for separator = (position #\; value :start start)
          for segment = (string-trim '(#\Space #\Tab)
                                     (subseq value start separator))
          do (when (zerop (length segment))
               (return-from %strict-transport-parse nil))
             (let* ((equals (position #\= segment))
                      (name (string-trim '(#\Space #\Tab)
                                         (subseq segment 0 equals)))
                      (raw-value (and equals (subseq segment (1+ equals)))))
                 (when (or (zerop (length name))
                           (not (every #'%strict-transport-token-character-p name))
                           (gethash name seen))
                   (return-from %strict-transport-parse nil))
                 (setf (gethash name seen) t)
                 (cond
                   ((string-equal name "max-age")
                    (let ((parsed (and raw-value
                                       (string-trim '(#\Space #\Tab)
                                                    raw-value))))
                      (unless (and parsed
                                   (plusp (length parsed))
                                   (every #'digit-char-p parsed))
                        (return-from %strict-transport-parse nil))
                      (setf max-age (parse-integer parsed))))
                   ((string-equal name "includeSubDomains")
                    (when raw-value
                      (return-from %strict-transport-parse nil))
                    (setf include-subdomains-p t))
                   (raw-value
                    (unless (%strict-transport-value raw-value)
                      (return-from %strict-transport-parse nil)))))
          while separator
          do (setf start (1+ separator)))
    (and max-age (list max-age include-subdomains-p))))

(defun %strict-transport-host-policy (store host)
  (%strict-transport-prune store)
  (find-if
   (lambda (policy)
     (let ((policy-host (http-strict-transport-policy-host policy)))
       (or (string-equal host policy-host)
           (and (http-strict-transport-policy-include-subdomains-p policy)
                (> (length host) (length policy-host))
                (char= (char host (- (length host) (length policy-host) 1)) #\.)
                (string-equal policy-host host
                              :start2 (- (length host) (length policy-host)))))))
   (%http-strict-transport-store-policies store)))

(defun http-strict-transport-store-known-host-p (store host)
  (unless (http-strict-transport-store-p store)
    (%client-protocol-error
     "Expected an HTTP-STRICT-TRANSPORT-STORE value." store))
  (unless (and (stringp host) (plusp (length host)))
    (%client-protocol-error "A strict transport host must be a non-empty string." host))
  (and (not (%cookie-ip-address-p host))
       (not (null (%strict-transport-host-policy store host)))))

(defun http-strict-transport-store-note-response (store uri response)
  (unless (http-strict-transport-store-p store)
    (%client-protocol-error
     "Expected an HTTP-STRICT-TRANSPORT-STORE value." store))
  (unless (http-response-p response)
    (%client-protocol-error "Expected an HTTP-RESPONSE value." response))
  (let* ((uri (%client-uri uri))
         (host (http-uri-host uri)))
    (when (and (string-equal (http-uri-scheme uri) "https")
               (not (%cookie-ip-address-p host)))
      (let ((value (first (http-header-values
                           (http-response-headers response)
                           "Strict-Transport-Security"))))
        (when value
          (let ((directives (%strict-transport-parse value)))
            (when directives
              (let ((max-age (first directives)))
                (setf (%http-strict-transport-store-policies store)
                      (delete host (%http-strict-transport-store-policies store)
                              :key #'http-strict-transport-policy-host
                              :test #'string-equal))
                (unless (zerop max-age)
                  (push (%make-http-strict-transport-policy
                         :host (string-downcase host)
                         :expires-at (+ (funcall
                                         (%http-strict-transport-store-clock-function
                                          store))
                                        max-age)
                         :include-subdomains-p (second directives))
                        (%http-strict-transport-store-policies store)))))))))
    store))

(defun http-strict-transport-store-upgrade-uri (store uri)
  (unless (http-strict-transport-store-p store)
    (%client-protocol-error
     "Expected an HTTP-STRICT-TRANSPORT-STORE value." store))
  (let ((uri (%client-uri uri)))
    (if (and (string-equal (http-uri-scheme uri) "http")
             (http-strict-transport-store-known-host-p
              store (http-uri-host uri)))
        (let ((port (http-uri-port uri)))
          (make-http-uri
           :scheme "https"
           :authority (format nil "~A~@[:~D~]"
                              (http-uri-host uri)
                              (and port (if (= port 80) 443 port)))
           :path (http-uri-path uri)
           :query (http-uri-query uri)))
        uri)))

(defun %client-request-rebuild
    (request &key method uri headers body protocol-version
                    (trailers nil trailers-supplied-p))
  (make-http-request :method (or method (http-request-method request))
                     :protocol-version (or protocol-version
                                           (http-request-protocol-version request))
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
  (lambda (request &key timeout deadline max-header-bytes max-fields max-body-bytes
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
     :max-fields max-fields
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
          (and proxy-plan (getf proxy-plan :remote-dns-p))
          (and proxy-plan (getf proxy-plan :proxy-authorization))
          (and proxy
               (list (http-proxy-scheme proxy)
                     (http-proxy-host proxy)
                     (http-proxy-port proxy)
                     (http-proxy-username proxy)
                     (http-proxy-password proxy))))))

(defun %client-transport-from-connection-pool (connection-pool)
  (lambda (request &key timeout deadline max-header-bytes max-fields max-body-bytes
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
     :max-fields max-fields
     :max-body-bytes max-body-bytes
     :proxy-plan proxy-plan
     :proxy proxy
     :request-target request-target
     :request-body-function request-body-function
     :request-body-length request-body-length
     :on-body-chunk on-body-chunk
     :on-information on-information
     :collect-body-p collect-body-p)))

(defun %client-runtime-function (package-name function-name)
  (let* ((package (find-package package-name))
         (symbol (and package (find-symbol function-name package))))
    (and symbol
         (fboundp symbol)
         (symbol-function symbol))))

(defun %client-missing-native-feature (feature package-name)
  (error 'http-unsupported-feature
         :message (format nil
                          "The default HTTP client requires ~A from ~A."
                          feature package-name)
         :operation :client
         :detail package-name
         :feature feature))

(defun %client-native-open-stream-function ()
  (let ((factory (%client-runtime-function
                  "HTTP-KIT/NETWORK" "MAKE-HTTP-NETWORK-STREAM-OPENER")))
    (unless factory
      (%client-missing-native-feature :native-tcp "HTTP-KIT/NETWORK"))
    (let ((opener (funcall factory)))
      (unless (functionp opener)
        (%client-protocol-error
         "The native network stream opener factory must return a function."
         opener))
      opener)))

(defun %client-native-open-stream ()
  (let ((opener nil))
    (lambda (request &key timeout deadline proxy-plan proxy &allow-other-keys)
      (unless opener
        (setf opener (%client-native-open-stream-function)))
      (funcall opener request
               :timeout timeout
               :deadline deadline
               :proxy-plan proxy-plan
               :proxy proxy))))

(defun %client-native-resolve-host (host &key timeout deadline)
  (let ((resolver (%client-runtime-function
                   "HTTP-KIT/NETWORK" "HTTP-NETWORK-RESOLVE-HOST")))
    (unless resolver
      (%client-missing-native-feature :native-dns "HTTP-KIT/NETWORK"))
    (funcall resolver host :timeout timeout :deadline deadline)))

(defun %client-native-tls-upgrade ()
  (let ((upgrader nil))
    (lambda (stream uri &key timeout deadline &allow-other-keys)
      (unless upgrader
        (let ((factory (%client-runtime-function
                        "HTTP-KIT/TLS" "MAKE-HTTP-TLS-UPGRADER")))
          (unless factory
            (%client-missing-native-feature :tls "HTTP-KIT/TLS"))
          (setf upgrader
                (funcall factory :alpn-protocols '("h2" "http/1.1")))))
      (funcall upgrader stream uri :timeout timeout :deadline deadline))))

(defun make-http-client
    (&key transport-function open-stream close-stream connection-pool
          http3-transport-function
          (default-headers nil)
          (cookie-jar (make-http-cookie-jar))
          (cache nil)
          (strict-transport-store nil strict-transport-store-supplied-p)
          (alternative-service-store
            nil alternative-service-store-supplied-p)
          (redirect-policy (make-http-redirect-policy))
          (retry-policy (make-http-retry-policy))
          proxy tls-upgrade resolve-host auth-provider challenge-auth-provider
          proxy-challenge-auth-provider
          stale-while-revalidate-scheduler
          (clock-function #'get-universal-time)
          (wall-clock-function #'%client-monotonic-time)
          (sleep-function #'sleep)
          (random-function #'random)
          max-header-bytes max-fields max-body-bytes
          (automatic-decompression-p t)
          (content-decoders (make-http-content-decoders))
          on-request on-response)
  "Construct a policy-driven HTTP client around an injected or native transport.

TRANSPORT-FUNCTION is called with REQUEST and keyword arguments including
:TIMEOUT, :DEADLINE, :MAX-HEADER-BYTES, :MAX-FIELDS, :MAX-BODY-BYTES, :PROXY, and
:PROXY-PLAN.  Alternatively OPEN-STREAM and CLOSE-STREAM can be supplied to
use the core HTTP/1.1 stream transport.  CONNECTION-POOL supplies a reusable
stream boundary and owns opening and closing pooled streams.  TLS-UPGRADE and
RESOLVE-HOST customize the injected stream boundary when OPEN-STREAM is used.
When STRICT-TRANSPORT-STORE is omitted, the client creates an RFC 6797 store
and upgrades known hosts before sending.  When ALTERNATIVE-SERVICE-STORE is
omitted, the client creates an RFC 7838 discovery store.  When no transport
boundary is supplied, a native TCP/TLS connection pool is created lazily.
Pass NIL explicitly to disable either store."
  (when (or (and transport-function open-stream)
            (and transport-function connection-pool)
            (and open-stream connection-pool)
            (and close-stream connection-pool))
    (%client-protocol-error
     "Specify only one of :TRANSPORT-FUNCTION, :OPEN-STREAM, or :CONNECTION-POOL."
     (list transport-function open-stream connection-pool close-stream)))
  (unless (or transport-function open-stream connection-pool
              (and (null transport-function)
                   (null open-stream)
                   (null connection-pool)
                   (null close-stream)))
    (%client-protocol-error
     "An HTTP client requires a transport boundary or the native default boundary."
     nil))
  (when transport-function
    (%ensure-function transport-function
                      "The client transport must be a function."))
  (when http3-transport-function
    (%ensure-function http3-transport-function
                      "The HTTP/3 transport must be a function."))
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
  (%ensure-function random-function "The client random function must be a function.")
  (when close-stream
    (%ensure-function close-stream "The stream close function must be a function."))
  (when auth-provider
    (%ensure-function auth-provider "The authentication provider must be a function."))
  (when challenge-auth-provider
    (%ensure-function
     challenge-auth-provider
     "The challenge authentication provider must be a function."))
  (when proxy-challenge-auth-provider
    (%ensure-function
     proxy-challenge-auth-provider
     "The proxy challenge authentication provider must be a function."))
  (when stale-while-revalidate-scheduler
    (%ensure-function
     stale-while-revalidate-scheduler
     "The stale-while-revalidate scheduler must be a function."))
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
  (when (and strict-transport-store-supplied-p strict-transport-store
             (not (http-strict-transport-store-p strict-transport-store)))
    (%client-protocol-error
     "The client strict transport store must be an HTTP-STRICT-TRANSPORT-STORE or NIL."
     strict-transport-store))
  (when (and alternative-service-store-supplied-p alternative-service-store
             (not (http-alternative-service-store-p alternative-service-store)))
    (%client-protocol-error
     "The client alternative service store must be an HTTP-ALTERNATIVE-SERVICE-STORE or NIL."
     alternative-service-store))
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
  (unless (or (null max-fields)
              (and (integerp max-fields) (plusp max-fields)))
    (%client-protocol-error
     "The maximum field count must be a positive integer or NIL."
     max-fields))
  (%client-validate-limit
   max-body-bytes
   "The maximum body size must be a non-negative integer or NIL.")
  (unless (member automatic-decompression-p '(nil t) :test #'eq)
    (%client-protocol-error
     "The automatic decompression flag must be NIL or T."
     automatic-decompression-p))
  (unless (and (listp content-decoders)
               (every (lambda (decoder)
                        (and (consp decoder)
                             (stringp (car decoder))
                             (http-kit::%token-p (car decoder))
                             (functionp (cdr decoder))))
                      content-decoders))
    (%client-protocol-error
     "The client content decoders must be an association list of names and functions."
     content-decoders))
  (let* ((native-default-p
           (and (null transport-function)
                (null open-stream)
                (null connection-pool)
                (null close-stream)))
         (native-open-stream
           (and native-default-p (%client-native-open-stream)))
         (native-tls-upgrade
           (and native-default-p (%client-native-tls-upgrade)))
         (native-resolve-host
           (and native-default-p #'%client-native-resolve-host))
         (effective-open-stream (or open-stream native-open-stream))
         (effective-tls-upgrade (or tls-upgrade native-tls-upgrade))
         (effective-resolve-host (or resolve-host native-resolve-host))
         (strict-transport-store
           (if strict-transport-store-supplied-p
               strict-transport-store
               (make-http-strict-transport-store
                :clock-function clock-function)))
         (alternative-service-store
           (if alternative-service-store-supplied-p
               alternative-service-store
               (make-http-alternative-service-store
                :clock-function clock-function)))
         (effective-close-stream (or close-stream #'close))
         (effective-connection-pool
           (or connection-pool
               (and native-default-p
                    (make-http-connection-pool
                     :open-stream effective-open-stream
                     :close-stream effective-close-stream
                     :tls-upgrade effective-tls-upgrade
                     :resolve-host effective-resolve-host))))
         (transport (or transport-function
                        (and effective-connection-pool
                             (%client-transport-from-connection-pool
                              effective-connection-pool))
                        (%client-transport-from-stream-boundary
                         (make-http-proxy-stream-opener
                          effective-open-stream effective-close-stream
                          :tls-upgrade effective-tls-upgrade
                          :resolve-host effective-resolve-host
                          :clock-function #'%client-monotonic-time)
                         effective-close-stream))))
    (%make-http-client
     :transport-function transport
     :http3-transport-function http3-transport-function
     :connection-pool effective-connection-pool
     :default-headers (%client-normalize-headers default-headers)
     :cookie-jar cookie-jar
     :cache cache
     :strict-transport-store strict-transport-store
     :alternative-service-store alternative-service-store
     :redirect-policy redirect-policy
     :retry-policy retry-policy
     :proxy proxy
     :tls-upgrade effective-tls-upgrade
     :resolve-host effective-resolve-host
     :auth-provider auth-provider
     :challenge-auth-provider challenge-auth-provider
     :proxy-challenge-auth-provider proxy-challenge-auth-provider
     :stale-while-revalidate-scheduler stale-while-revalidate-scheduler
     :clock-function clock-function
     :wall-clock-function wall-clock-function
     :sleep-function sleep-function
     :random-function random-function
     :max-header-bytes max-header-bytes
     :max-fields max-fields
     :max-body-bytes max-body-bytes
     :automatic-decompression-p automatic-decompression-p
     :content-decoders (copy-list content-decoders)
     :on-request on-request
     :on-response on-response)))

(defun http-client-request
    (client method uri &key headers trailers body
                              (protocol-version "HTTP/1.1"))
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
                       :protocol-version protocol-version
                       :uri (%client-uri uri)
                       :headers combined
                       :trailers trailers
                       :body (%client-body-octets body))))

(defun %client-request-with
    (request &key headers body method uri protocol-version
                    (trailers nil trailers-supplied-p))
  (%client-request-rebuild
   request
   :method method
   :uri uri
   :protocol-version protocol-version
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
  (let* ((forward-p (and proxy-plan
                         (eq (getf proxy-plan :mode) :forward)))
         (authorization
           (and forward-p (getf proxy-plan :proxy-authorization)))
         (headers (http-request-headers request)))
    (cond ((not forward-p)
           (%client-request-with
            request
            :headers (%client-remove-headers
                      headers '("Proxy-Authorization"))))
          ((and authorization
                (not (http-header-present-p headers "Proxy-Authorization")))
           (%client-request-with
            request
            :headers (%client-header-set
                      headers "Proxy-Authorization" authorization)))
          (t request))))

(defun %client-retry-method-p (policy request)
  (member (http-request-method request)
          (http-retry-policy-methods policy)
          :test #'string=))

(defun %client-retry-status-p (policy response)
  (member (http-response-status response)
          (http-retry-policy-statuses policy)
          :test #'eql))

(defun %client-retryable-condition-p (policy condition)
  (or (and (typep condition 'http-timeout)
           (http-retry-policy-retry-on-timeout-p policy))
      (and (typep condition 'http-connection-error)
           (http-retry-policy-retry-on-connection-error-p policy))))

(defun %client-unprocessed-condition-p (condition)
  (and (typep condition 'http-connection-error)
       (let ((cause (http-connection-error-cause condition)))
         (and (consp cause)
              (or (eq (first cause) :goaway)
                  (and (eq (first cause) :rst-stream)
                       (eql (third cause) 7)))))))

(defun %client-stale-if-error-condition-p (condition)
  (or (typep condition 'http-timeout)
      (typep condition 'http-connection-error)))

(defun %client-retry-delay (client policy response attempt)
  (let* ((retry-after (and (http-retry-policy-respect-retry-after-p policy)
                           response
                           (%retry-after-seconds
                            (%client-header-value
                             (http-response-headers response)
                             "Retry-After")
                            (funcall (http-client-clock-function client)))))
         (exponential (* (float (http-retry-policy-base-delay policy))
                         (expt 2 (1- attempt))))
         (jitter-ratio (float (http-retry-policy-jitter-ratio policy)))
         (random-unit
           (and (null retry-after)
                (plusp jitter-ratio)
                (funcall (http-client-random-function client) 1.0))))
    (when (and random-unit
               (not (and (realp random-unit) (<= 0 random-unit) (< random-unit 1))))
      (%client-protocol-error
       "The client random function must return a number in [0, 1)."
       random-unit))
    (min (float (http-retry-policy-max-delay policy))
         (float (or retry-after
                    (* exponential
                       (+ (- 1.0 jitter-ratio)
                          (* 2.0 jitter-ratio (or random-unit 0.0)))))))))

(defun %client-sleep-before-retry (client policy response attempt deadline)
  (let* ((delay (%client-retry-delay client policy response attempt))
         (remaining (and deadline
                         (- deadline
                            (funcall
                             (http-client-wall-clock-function client))))))
    (when (and remaining (<= remaining delay))
      (error 'http-timeout
             :message "The HTTP operation cannot retry before its deadline."
             :operation :retry
             :kind :retry))
    (when (plusp delay)
      (funcall (http-client-sleep-function client) delay))))

(defun %client-http3-fallback-deadline (client deadline)
  (when deadline
    (let* ((now (funcall (http-client-wall-clock-function client)))
           (remaining (- deadline now))
           (fallback-minimum
             (if (> remaining 1.0)
                 1.0
                 (* remaining 0.5))))
      (max now (- deadline fallback-minimum)))))

(defun %client-call-transport
    (client request proxy-plan &key timeout deadline request-body-function
                                      request-body-length on-body-chunk
                                      on-information
                                      (collect-body-p t))
  (let* ((arguments
           (list :timeout timeout
                 :deadline deadline
                 :max-header-bytes (http-client-max-header-bytes client)
                 :max-fields (http-client-max-fields client)
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
    (let* ((explicit-http3-p (http-request-http3-p request))
           (alternative
             (and (not explicit-http3-p)
                  (http-client-alternative-service-store client)
                  (find-if
                   (lambda (service)
                     (string= (http-alpn-protocol-name
                               (http-alternative-service-protocol-id service))
                              "h3"))
                   (http-alternative-service-store-services
                    (http-client-alternative-service-store client)
                    (http-request-uri request)))))
           (http3-transport (http-client-http3-transport-function client)))
      (labels ((call-tcp ()
                 (apply (http-client-transport-function client)
                        effective-request arguments))
              (call-http3 (service &optional attempt-deadline)
                 (unless http3-transport
                   (%client-missing-native-feature :http3-transport
                                                   "HTTP-KIT/CLIENT"))
                 (apply http3-transport
                        (%client-request-with
                         effective-request :protocol-version "HTTP/3")
                        (append (if attempt-deadline
                                    (let ((limited (copy-list arguments)))
                                      (setf (getf limited :deadline)
                                            attempt-deadline)
                                      limited)
                                    arguments)
                                (list :alternative-service service)))))
        (let ((response
                (cond
                  (explicit-http3-p
                   ;; An explicit HTTP/3 request is a hard requirement.
                   (call-http3 nil))
                  (alternative
                   (handler-case
                       (call-http3
                        alternative
                        (%client-http3-fallback-deadline client deadline))
                     (error (condition)
                       ;; Failed Alt-Svc knowledge is stale.  Retry via TCP.
                       (declare (ignore condition))
                       (http-alternative-service-store-remove
                        (http-client-alternative-service-store client)
                        (http-request-uri request)
                        alternative)
                       (call-tcp))))
                  (t (call-tcp)))))
          (unless (http-response-p response)
            (%client-protocol-error
             "The client transport must return an HTTP-RESPONSE."
             response))
          response)))))

(defun %client-attempt
    (client request proxy-plan policy &key timeout deadline request-body-function
                                                request-body-factory request-body-length
                                                on-body-chunk
                                                on-information
                                                (cookie-same-site-p t)
                                                (cookie-top-level-navigation-p nil)
                                                cookie-partition-key
                                                stale-if-error-function
                                                (collect-body-p t))
  (let* ((attempt 1)
         (max-attempts (http-retry-policy-max-attempts policy))
         (last-response nil)
         (response-body-observed-p nil)
         (effective-on-body-chunk
           (and on-body-chunk
                (lambda (chunk)
                  (setf response-body-observed-p t)
                  (funcall on-body-chunk chunk))))
         (body-replayable-p
           (or (null request-body-function)
               request-body-factory))
         (retryable-status-request-p
           (and (%client-retry-method-p policy request)
                body-replayable-p)))
    (loop
      (when (and deadline
                 (>= (funcall (http-client-wall-clock-function client))
                     deadline))
        (error 'http-timeout
               :message "The HTTP operation exceeded its deadline."
               :operation :request
               :kind :deadline))
      (when (http-client-on-request client)
        (funcall (http-client-on-request client) request attempt))
      (let ((attempt-body-function
              (%client-request-body-for-attempt
               request-body-function request-body-factory))
            (request-time
              (and (http-client-cache client)
                   (funcall
                    (http-cache-clock-function (http-client-cache client)))))
            (response-time nil))
        (handler-case
          (let ((response (%client-call-transport
                           client request proxy-plan
                           :timeout timeout
                           :deadline deadline
                           :request-body-function attempt-body-function
                           :request-body-length request-body-length
                           :on-body-chunk effective-on-body-chunk
                           :on-information on-information
                           :collect-body-p collect-body-p)))
            (when (http-client-cache client)
              (setf response-time
                    (funcall
                     (http-cache-clock-function (http-client-cache client)))))
            (when (http-client-strict-transport-store client)
              (http-strict-transport-store-note-response
               (http-client-strict-transport-store client)
               (http-request-uri request)
               response))
            (when (http-client-alternative-service-store client)
              (http-alternative-service-store-note-response
               (http-client-alternative-service-store client)
               (http-request-uri request)
               response))
            (when (and collect-body-p
                       (null on-body-chunk)
                       (let ((method (http-request-method request))
                             (status (http-response-status response)))
                         (not (or (string= method "HEAD")
                                  (member status '(204 205 304) :test #'eql)
                                  (and (string= method "CONNECT")
                                       (<= 200 status 299)))))
                       (http-client-automatic-decompression-p client))
              (setf response
                    (decode-http-response-content
                     response
                     :max-body-bytes (http-client-max-body-bytes client)
                     :content-decoders
                     (http-client-content-decoders client))))
            (setf last-response response)
            (http-cookie-jar-accept-response
             (http-client-cookie-jar client)
             (http-request-uri request)
             response
             :same-site-p cookie-same-site-p
             :top-level-navigation-p cookie-top-level-navigation-p
             :partition-key cookie-partition-key)
            (if (and (< attempt max-attempts)
                     retryable-status-request-p
                     (not response-body-observed-p)
                     (%client-retry-status-p policy response))
                (progn
                  (%client-sleep-before-retry
                   client policy response attempt deadline)
                  (incf attempt))
                (let ((stale-response
                        (and stale-if-error-function
                             (member (http-response-status response)
                                     '(500 502 503 504)
                                     :test #'eql)
                             (funcall stale-if-error-function))))
                  (when (and (null stale-response)
                             (> max-attempts 1)
                             retryable-status-request-p
                             (not response-body-observed-p)
                             (%client-retry-status-p policy response))
                    (%client-retry-exhausted-error
                     attempt
                     :request request
                     :last-response response))
                  (when stale-response
                    (setf response stale-response))
                  (when (http-client-on-response client)
                    (funcall (http-client-on-response client) response request attempt))
                  (return (values response attempt (not (null stale-response))
                                  request-time response-time)))))
        (http-error (condition)
          (if (and (< attempt max-attempts)
                   body-replayable-p
                   (or (%client-retry-method-p policy request)
                       (%client-unprocessed-condition-p condition))
                   (not response-body-observed-p)
                   (%client-retryable-condition-p policy condition))
              (progn
                (%client-sleep-before-retry
                 client policy last-response attempt deadline)
                (incf attempt))
               (let ((stale-response
                       (and stale-if-error-function
                            (%client-stale-if-error-condition-p condition)
                            (funcall stale-if-error-function))))
                 (if stale-response
                     (progn
                       (when (http-client-on-response client)
                         (funcall (http-client-on-response client)
                                  stale-response request attempt))
                       (return (values stale-response attempt t)))
                     (if (and (> max-attempts 1)
                              body-replayable-p
                              (or (%client-retry-method-p policy request)
                                  (%client-unprocessed-condition-p condition))
                              (not response-body-observed-p)
                              (%client-retryable-condition-p policy condition))
                         (%client-retry-exhausted-error
                          attempt
                          :request request
                          :last-condition condition
                          :last-response last-response)
                         (error condition)))))))))))

(defun %client-authorization-value (value)
  (cond
    ((null value) nil)
    ((stringp value) value)
    ((http-header-p value) (http-header-content value))
    (t
     (%client-protocol-error
      "An authentication provider must return a string, HTTP-HEADER, or NIL."
      value))))

(defun %client-prepare-request
    (client request initial-uri
     &key redirect-p (cookie-same-site-p t)
       (cookie-top-level-navigation-p nil) cookie-partition-key)
  (let ((headers (http-request-headers request)))
    (when redirect-p
      (setf headers (%client-remove-headers headers '("Cookie"))))
    (let ((cookie-value
            (http-cookie-jar-cookie-header
             (http-client-cookie-jar client)
             (http-request-uri request)
             :same-site-p cookie-same-site-p
             :top-level-navigation-p cookie-top-level-navigation-p
             :method (http-request-method request)
             :partition-key cookie-partition-key)))
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

(defun %client-304-update-excluded-fields (headers)
  (append (%cache-storage-excluded-fields headers)
          (%cache-no-cache-fields (%cache-control-directives headers))
          '("Content-Length" "Content-Encoding" "Content-Range")))

(defun %client-304-matches-response-p (cached response)
  (let* ((cached-headers (http-response-headers cached))
         (new-headers (http-response-headers response))
         (new-etag (%client-header-value new-headers "ETag"))
         (new-last-modified
           (%client-header-value new-headers "Last-Modified")))
    (cond
      (new-etag
       (equal new-etag (%client-header-value cached-headers "ETag")))
      (new-last-modified
       (equal new-last-modified
              (%client-header-value cached-headers "Last-Modified")))
      (t
       (and (null (%client-header-value cached-headers "ETag"))
            (null (%client-header-value cached-headers "Last-Modified")))))))

(defun %client-response-merge-304 (cached response)
  (unless (%client-304-matches-response-p cached response)
    (%client-protocol-error
     "A 304 response did not identify the cached response validator."
     response))
  (let* ((response-headers (http-response-headers response))
         (excluded (%client-304-update-excluded-fields response-headers))
         (new-headers (%client-remove-headers response-headers excluded))
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
     :trailers (http-response-trailers cached)
     :body (http-response-body cached))))

(defun %client-redirect-method (method status)
  (cond
    ((= status 303)
     (if (string= method "HEAD") "HEAD" "GET"))
    ((and (member status '(301 302) :test #'eql)
          (string= method "POST"))
     "GET")
    (t method)))

(defun %client-redirect-request
    (client request response policy initial-uri redirect-count)
  (declare (ignore redirect-count))
  (let ((location (%client-header-value
                   (http-response-headers response) "Location")))
    (unless location
      (return-from %client-redirect-request nil))
    (let* ((target (resolve-http-uri (http-request-uri request) location))
           (target
             (if (http-client-strict-transport-store client)
                 (http-strict-transport-store-upgrade-uri
                  (http-client-strict-transport-store client)
                  target)
                 target))
           (old-scheme (http-uri-scheme (http-request-uri request)))
           (new-scheme (http-uri-scheme target))
           (downgrade (and (string-equal old-scheme "https")
                           (string-equal new-scheme "http")))
           (new-method (%client-redirect-method
                        (http-request-method request)
                        (http-response-status response)))
           (method-changed (not (string= new-method
                                         (http-request-method request))))
           (headers (%client-remove-headers
                     (http-request-headers request)
                     '("Cookie" "Proxy-Authorization"))))
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
      (unless (http-same-origin-p (http-request-uri request) target)
        (setf headers (%client-remove-headers
                       headers '("Host" "Origin" "Referer"))))
      (when method-changed
        (setf headers (%client-remove-headers
                       headers '("Content-Digest" "Content-Encoding"
                                 "Content-Language" "Content-Length"
                                 "Content-Location" "Content-MD5"
                                 "Content-Type" "Digest" "Expect"
                                 "Repr-Digest" "Trailer"
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
  (member (http-request-method request) '("GET" "HEAD")
          :test #'string=))

(defun %client-mutating-method-p (request)
  (not (member (http-request-method request)
               '("GET" "HEAD" "OPTIONS" "TRACE")
               :test #'string=)))

(defun %client-cache-invalidate-response (cache request response)
  (let ((status (http-response-status response))
        (request-uri (http-request-uri request)))
    (when (and (<= 200 status) (< status 400))
      (http-cache-invalidate cache request-uri)
      (dolist (name '("Location" "Content-Location"))
        (dolist (value (http-header-values
                        (http-response-headers response) name))
          (let ((target (ignore-errors (resolve-http-uri request-uri value))))
            (when (and target (http-same-origin-p request-uri target))
              (http-cache-invalidate cache target)))))))
  cache)

(defun %client-only-if-cached-p (request)
  (%cache-directive-present-p
   (%cache-request-directives (http-request-headers request))
   "only-if-cached"))

(defun %client-only-if-cached-response (client request on-body-chunk)
  (let ((response (make-http-response :status 504)))
    (%client-deliver-body-chunk response on-body-chunk)
    (when (http-client-on-response client)
      (funcall (http-client-on-response client) response request 0))
    (values response request)))

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

(defun %http-client-send-request
    (client request &key timeout deadline redirect-policy retry-policy
                         request-body-function request-body-factory
                         request-body-length
                         on-body-chunk on-information (collect-body-p t)
                         (cookie-same-site-p t)
                         (cookie-top-level-navigation-p nil)
                         cookie-partition-key
                         cache-revalidation-entry
                         (allow-stale-while-revalidate-p t))
  "Execute REQUEST with redirects, retries, cookies, cache, auth, and proxy policy.

Returns the final HTTP-RESPONSE as the primary value and the effective
HTTP-REQUEST as a secondary value.  TIMEOUT and DEADLINE are passed through to
the configured transport boundary.  REQUEST-BODY-FACTORY, when supplied,
must return a fresh producer function on every invocation.  Streaming request
bodies without a factory are sent once and are not automatically retried or
resent across same-method redirects.  COOKIE-SAME-SITE-P and
COOKIE-TOP-LEVEL-NAVIGATION-P describe the request context used by SameSite
cookie policy.  COOKIE-PARTITION-KEY identifies the top-level site for
Partitioned cookies.  ALLOW-STALE-WHILE-REVALIDATE-P controls whether an
eligible stale cache entry may be returned through the configured scheduler."
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
  (%cookie-partition-key cookie-partition-key)
  (let* ((effective-deadline
           (http-deadline
            timeout
            :deadline deadline
            :clock-function (http-client-wall-clock-function client)))
         (request
           (if (and (http-client-automatic-decompression-p client)
                    (http-client-content-decoders client)
                    collect-body-p
                    (null on-body-chunk)
                    (not (http-header-present-p
                          (http-request-headers request) "Accept-Encoding")))
               (%client-request-with
                request
                :headers (%client-header-set
                          (http-request-headers request)
                          "Accept-Encoding"
                          (format nil "~{~A~^, ~}"
                                  (mapcar #'car
                                          (http-client-content-decoders client)))))
               request))
         (request
           (if (http-client-strict-transport-store client)
               (%client-request-with
                request
                :uri (http-strict-transport-store-upgrade-uri
                      (http-client-strict-transport-store client)
                      (http-request-uri request)))
               request))
         (redirect-policy (or redirect-policy
                             (http-client-redirect-policy client)))
         (retry-policy (or retry-policy
                           (http-client-retry-policy client)))
         (initial-uri (http-request-uri request))
         (cache-request request)
         (current-request request)
         (only-if-cached-p (%client-only-if-cached-p request))
         (redirect-count 0)
         (challenge-auth-count 0)
         (challenge-authorization-p nil)
         (proxy-challenge-auth-count 0)
         (response-body-observed-p nil)
         (cache-forward-reason nil)
         (effective-on-body-chunk
           (and on-body-chunk
                (lambda (chunk)
                  (setf response-body-observed-p t)
                  (funcall on-body-chunk chunk))))
         (stale-entry cache-revalidation-entry))
    (unless (http-redirect-policy-p redirect-policy)
      (%client-protocol-error "The redirect policy must be an HTTP-REDIRECT-POLICY."
                              redirect-policy))
    (unless (http-retry-policy-p retry-policy)
      (%client-protocol-error "The retry policy must be an HTTP-RETRY-POLICY."
                              retry-policy))
    (setf current-request
          (%client-prepare-request
           client current-request initial-uri
           :cookie-same-site-p cookie-same-site-p
           :cookie-top-level-navigation-p cookie-top-level-navigation-p
           :cookie-partition-key cookie-partition-key))
    (when (and only-if-cached-p
               (or (null (http-client-cache client))
                   (not (%client-cacheable-method-p current-request))))
      (return-from %http-client-send-request
        (%client-only-if-cached-response client current-request on-body-chunk)))
    (when (and (http-client-cache client)
               (%client-cacheable-method-p current-request))
      (multiple-value-bind (response state entry forward-reason)
          (http-cache-lookup (http-client-cache client) current-request)
        (setf cache-forward-reason
              (or forward-reason
                  (if (eq state :stale) :stale :miss)))
        (when response
          (setf response
                (%cache-status-hit-response
                 (http-client-cache client) response entry))
          (%client-deliver-body-chunk response on-body-chunk)
          (when (http-client-on-response client)
            (funcall (http-client-on-response client) response current-request 0))
          (return-from %http-client-send-request
            (values response current-request)))
        (when only-if-cached-p
          (return-from %http-client-send-request
            (%client-only-if-cached-response
             client current-request on-body-chunk)))
        (when (eq state :stale)
          (let* ((now (funcall (http-cache-clock-function
                                (http-client-cache client))))
                 (stale-response
                   (and allow-stale-while-revalidate-p
                        (http-client-stale-while-revalidate-scheduler client)
                        collect-body-p
                        (null on-body-chunk)
                        (%cache-stale-while-revalidate-response
                         entry current-request now)))
                 (revalidation-request
                   (%client-conditional-request current-request entry)))
            (when (and stale-response
                       (funcall
                        (http-client-stale-while-revalidate-scheduler client)
                        client current-request stale-response
                        (lambda ()
                          (http-client-send
                           client revalidation-request
                           :timeout timeout
                           :redirect-policy redirect-policy
                           :retry-policy retry-policy
                           :cookie-same-site-p cookie-same-site-p
                           :cookie-top-level-navigation-p
                           cookie-top-level-navigation-p
                           :cookie-partition-key cookie-partition-key
                           :cache-revalidation-entry entry
                           :allow-stale-while-revalidate-p nil))))
              (setf stale-response
                    (%cache-status-hit-response
                     (http-client-cache client) stale-response entry now))
              (when (http-client-on-response client)
                (funcall (http-client-on-response client)
                         stale-response current-request 0))
              (return-from %http-client-send-request
                (values stale-response current-request)))
            (setf stale-entry entry
                  current-request revalidation-request)))
        ))
    (loop
      (block next-attempt
        (let* ((redirect-p (plusp redirect-count))
             (prepared (%client-prepare-request
                        client current-request initial-uri
                        :redirect-p redirect-p
                        :cookie-same-site-p cookie-same-site-p
                        :cookie-top-level-navigation-p
                        cookie-top-level-navigation-p
                        :cookie-partition-key cookie-partition-key))
             (proxy-plan (http-proxy-plan
                          (http-client-proxy client)
                          (http-request-uri prepared))))
        (when (and proxy-plan
                   (eq (getf proxy-plan :mode) :connect)
                   (http-client-proxy-challenge-auth-provider client))
          (setf (getf proxy-plan :proxy-challenge-auth-provider)
                (http-client-proxy-challenge-auth-provider client)))
        (multiple-value-bind
            (response attempt stale-fallback-p request-time response-time)
            (%client-attempt client prepared proxy-plan retry-policy
                             :timeout timeout :deadline effective-deadline
                             :request-body-function request-body-function
                             :request-body-factory request-body-factory
                             :request-body-length request-body-length
                             :on-body-chunk effective-on-body-chunk
                             :on-information on-information
                             :cookie-same-site-p cookie-same-site-p
                             :cookie-top-level-navigation-p
                             cookie-top-level-navigation-p
                             :cookie-partition-key cookie-partition-key
                             :stale-if-error-function
                             (and stale-entry
                                  collect-body-p
                                  (null on-body-chunk)
                                  (lambda ()
                                    (%cache-stale-if-error-response
                                     stale-entry
                                     prepared
                                     (funcall
                                      (http-cache-clock-function
                                       (http-client-cache client))))))
                             :collect-body-p collect-body-p)
          (declare (ignore attempt))
          (let ((forwarded-status (http-response-status response)))
          (when (and stale-entry (= (http-response-status response) 304))
            (setf response (%client-response-merge-304
                            (http-cache-entry-response stale-entry)
                            response))
            (%client-deliver-body-chunk response on-body-chunk))
          (let* ((challenge-values
                   (and (= (http-response-status response) 401)
                        (http-header-values
                         (http-response-headers response) "WWW-Authenticate")))
                 (challenges
                   (and challenge-values
                        (http-parse-authentication-challenges challenge-values)))
                 (challenge-value
                   (and (http-client-challenge-auth-provider client)
                        (http-same-origin-p
                         initial-uri (http-request-uri prepared))
                        (zerop challenge-auth-count)
                        (not response-body-observed-p)
                        (or (null request-body-function) request-body-factory)
                        challenges
                        (%client-authorization-value
                         (funcall
                          (http-client-challenge-auth-provider client)
                          prepared response challenges)))))
            (when challenge-value
              (setf current-request
                    (%client-request-with
                     prepared
                     :headers (%client-header-set
                               (%client-remove-headers
                                (http-request-headers prepared)
                               '("Authorization"))
                               "Authorization" challenge-value))
                    challenge-auth-count 1
                    challenge-authorization-p t)
              (return-from next-attempt)))
          (let* ((challenge-values
                   (and (= (http-response-status response) 407)
                        (http-header-values
                         (http-response-headers response) "Proxy-Authenticate")))
                 (challenges
                   (and challenge-values
                        (http-parse-authentication-challenges challenge-values)))
                 (challenge-value
                   (and (http-client-proxy-challenge-auth-provider client)
                        (zerop proxy-challenge-auth-count)
                        (not response-body-observed-p)
                        (or (null request-body-function) request-body-factory)
                        challenges
                        (%client-authorization-value
                         (funcall
                          (http-client-proxy-challenge-auth-provider client)
                          prepared response proxy-plan challenges)))))
            (when challenge-value
              (setf current-request
                    (%client-request-with
                     prepared
                     :headers (%client-header-set
                               (%client-remove-headers
                                (http-request-headers prepared)
                                '("Proxy-Authorization"))
                               "Proxy-Authorization" challenge-value))
                    proxy-challenge-auth-count 1)
              (return-from next-attempt)))
          (when (and (http-client-cache client)
                     (%client-mutating-method-p prepared))
            (%client-cache-invalidate-response
             (http-client-cache client) prepared response))
          (let* ((redirect-request
                   (and (member (http-response-status response)
                                (http-redirect-policy-statuses redirect-policy)
                                :test #'eql)
                        (%client-header-value
                         (http-response-headers response) "Location")))
                 (next-request
                   (and redirect-request
                        (%client-redirect-request
                         client prepared response redirect-policy initial-uri
                         redirect-count)))
                 (follow-redirect-p
                   (and next-request
                        (not response-body-observed-p)
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
                      (unless (and
                               (string-equal
                                (http-uri-scheme (http-request-uri prepared))
                                (http-uri-scheme (http-request-uri next-request)))
                               (string-equal
                                (http-uri-host (http-request-uri prepared))
                                (http-uri-host (http-request-uri next-request))))
                        (setf cookie-same-site-p nil))
                      (when (not (string-equal
                                  (http-request-method next-request)
                                  (http-request-method prepared)))
                        (setf request-body-function nil
                              request-body-factory nil
                              request-body-length nil))
                      (setf current-request
                            (if (and challenge-authorization-p
                                     (not (http-same-origin-p
                                           (http-request-uri prepared)
                                           (http-request-uri next-request))))
                                (%client-request-with
                                 next-request
                                 :headers (%client-remove-headers
                                           (http-request-headers next-request)
                                           '("Authorization")))
                                next-request))
                      (setf redirect-count (1+ redirect-count)
                            stale-entry nil
                            cache-forward-reason nil)))
                (progn
                  (let ((stored-entry nil))
                    (when (and (http-client-cache client)
                               (not stale-fallback-p))
                      (if (%cache-directive-present-p
                           (%cache-control-directives
                            (http-response-headers response))
                           "no-store")
                          (http-cache-invalidate
                           (http-client-cache client)
                           (http-request-uri prepared))
                          (when (or (= forwarded-status 304)
                                    (%client-cacheable-method-p prepared))
                            (setf stored-entry
                                  (http-cache-store
                                   (http-client-cache client)
                                   prepared
                                   response
                                   :request-time request-time
                                   :response-time response-time)))))
                    (when (and (http-client-cache client)
                               cache-forward-reason)
                      (setf response
                            (if stale-fallback-p
                                (%cache-status-hit-response
                                 (http-client-cache client)
                                 response stale-entry)
                                (%cache-status-forward-response
                                 (http-client-cache client)
                                 response cache-forward-reason
                                 :forwarded-status
                                 (and (/= forwarded-status
                                          (http-response-status response))
                                      forwarded-status)
                                 :stored-p (not (null stored-entry)))))))
                  (return
                    (values response
                            (if (and stale-entry (= forwarded-status 304))
                                cache-request
                                prepared)))))))))
      ))))

(defun %client-send-keyword-plist-p (arguments)
  (and (evenp (length arguments))
       (loop for tail on arguments by #'cddr
             always (keywordp (first tail)))))

(defun %client-convenience-request
    (client method uri arguments strip-method-p)
  (unless (%client-send-keyword-plist-p arguments)
    (%client-protocol-error
     "The URL client arguments must be a keyword plist."
     arguments))
  (unless (or (stringp method) (symbolp method))
    (%client-protocol-error
     "The URL client method must be a string or symbol."
     method))
  (let ((request-options nil)
        (send-options nil))
    (loop for (key value) on arguments by #'cddr
          do (cond
               ((member key '(:headers :trailers :body :protocol-version) :test #'eq)
                (setf request-options
                      (append request-options (list key value))))
               ((and strip-method-p (eq key :method)) nil)
               (t
                (setf send-options
                      (append send-options (list key value))))))
    (values
     (apply #'http-client-request
            client
            (string-upcase (string method))
            uri
            request-options)
     send-options)))

(defun %client-parse-send-arguments (client request-or-method arguments)
  (if (http-request-p request-or-method)
      (values request-or-method arguments)
      (let ((first (first arguments)))
        (cond
          ((and arguments
                (or (stringp first) (http-uri-p first)))
           (%client-convenience-request
            client request-or-method first (rest arguments) nil))
          (t
           (%client-convenience-request
            client
            (or (getf arguments :method) "GET")
            request-or-method
            arguments
            t))))))

(defun http-client-send (client request-or-method &rest arguments)
  "Send a request through CLIENT.

The compatible form accepts an HTTP-REQUEST followed by the existing keyword
arguments.  The convenience form accepts METHOD and URI positionally, or URI
with an optional :METHOD keyword, and builds the request with CLIENT defaults.
Convenience-only :HEADERS, :TRAILERS, and :BODY options are consumed while
all other keywords retain the HTTP-CLIENT-SEND contract."
  (multiple-value-bind (request send-options)
      (%client-parse-send-arguments client request-or-method arguments)
    (apply #'%http-client-send-request client request send-options)))
