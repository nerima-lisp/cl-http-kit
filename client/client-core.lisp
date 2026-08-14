(in-package #:http-kit/client)

(defun make-http-client
    (&key transport-function open-stream close-stream connection-pool
          (default-headers nil)
          (cookie-jar (make-http-cookie-jar))
          cookie-partition-key cookie-same-site-context
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
  (when (and cookie-partition-key
             (or (not (stringp cookie-partition-key))
                 (string= cookie-partition-key "")))
    (%client-protocol-error
     "The client cookie partition key must be a non-empty string or NIL."
     cookie-partition-key))
  (when (and cookie-same-site-context
             (not (member cookie-same-site-context
                         '(:same-site :cross-site)
                         :test #'string-equal)))
    (%client-protocol-error
     "The client cookie SameSite context must be SAME-SITE, CROSS-SITE, or NIL."
     cookie-same-site-context))
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
     :cookie-partition-key cookie-partition-key
     :cookie-same-site-context cookie-same-site-context
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
