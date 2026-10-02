(in-package #:http-kit/client)

(defparameter *default-redirect-statuses* '(301 302 303 307 308))
(defparameter *default-retry-methods*
  '("GET" "HEAD" "OPTIONS" "PUT" "DELETE" "TRACE"))
(defparameter *default-retry-statuses* '(408 425 429 500 502 503 504))

#+sbcl
(defun %make-client-lock (name)
  (sb-thread:make-mutex :name name))

#-sbcl
(defun %make-client-lock (name)
  (declare (ignore name))
  nil)

(defmacro %with-client-lock ((lock) &body body)
  #+sbcl
  `(sb-thread:with-mutex (,lock)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun %client-protocol-error (message detail)
  (error 'http-protocol-error
         :message message
         :operation :client
         :detail detail))

(defun %client-error (type message &key detail operation)
  (error type
         :message message
         :operation (or operation :client)
         :detail detail))

(defun %client-redirect-limit-error (uri redirects)
  (error 'http-redirect-limit-exceeded
         :message "The HTTP redirect policy exhausted its redirect limit."
         :operation :redirect
         :detail (list :uri uri :redirects redirects)
         :uri uri
         :redirects redirects))

(defun %client-retry-exhausted-error
    (attempts &key request last-condition last-response)
  (error 'http-retry-exhausted
         :message "The HTTP retry policy exhausted its attempts."
         :operation :retry
         :detail (list :request request
                       :condition last-condition
                       :response last-response)
         :attempts attempts
         :last-condition last-condition
         :last-response last-response))

(defun %ensure-function (value message)
  (unless (functionp value)
    (%client-protocol-error message value))
  value)

(defun %ensure-nonnegative-integer (value message)
  (unless (and (integerp value) (>= value 0))
    (%client-protocol-error message value))
  value)

(defun %ensure-positive-integer (value message)
  (unless (and (integerp value) (plusp value))
    (%client-protocol-error message value))
  value)

(defun %client-parse-integer
    (string &key (start 0) end (allow-sign-p t))
  "Parse a complete decimal integer without signaling on untrusted input.

START and END delimit the input.  A result is returned only when every
character in that range is a decimal digit, apart from an optional sign when
  ALLOW-SIGN-P is true."
  (when (stringp string)
    (let ((text (the string string)))
      (let* ((end (or end (length text)))
             (index start)
             (sign 1))
      (when (and (<= 0 start end)
                 (<= end (length text)))
        (when (and allow-sign-p
                   (< index end)
                   (member (char text index) '(#\+ #\-) :test #'char=))
          (when (char= (char text index) #\-)
            (setf sign -1))
          (incf index))
        (when (< index end)
          (let ((value 0)
                (digits 0))
            (loop while (< index end)
                  for digit = (position
                               (subseq text index (1+ index))
                               #("0" "1" "2" "3" "4"
                                 "5" "6" "7" "8" "9")
                               :test #'string=)
                  do (unless digit (return-from %client-parse-integer nil))
                     (setf value (+ (* value 10) digit))
                     (incf digits)
                     (incf index))
            (when (and (plusp digits) (= index end))
              (* sign value)))))))))

(defstruct (http-redirect-policy
             (:constructor %make-http-redirect-policy)
             (:conc-name http-redirect-policy-))
  (max-redirects 10)
  (statuses (copy-list *default-redirect-statuses*))
  (allow-downgrade-p nil)
  (preserve-authorization-p nil))

(defun make-http-redirect-policy
    (&key (max-redirects 10)
          (statuses *default-redirect-statuses*)
          (allow-downgrade-p nil)
          (preserve-authorization-p nil))
  (%ensure-nonnegative-integer
   max-redirects
   "The redirect limit must be a non-negative integer.")
  (unless (and (listp statuses)
               (every (lambda (status)
                        (and (integerp status) (<= 300 status 399)))
                      statuses))
    (%client-protocol-error "Redirect statuses must be a list of 3xx integers."
                            statuses))
  (%make-http-redirect-policy
   :max-redirects max-redirects
   :statuses (copy-list statuses)
   :allow-downgrade-p (not (null allow-downgrade-p))
   :preserve-authorization-p (not (null preserve-authorization-p))))

(defstruct (http-retry-policy
             (:constructor %make-http-retry-policy)
             (:conc-name http-retry-policy-))
  (max-attempts 1)
  (methods (copy-list *default-retry-methods*))
  (statuses (copy-list *default-retry-statuses*))
  (base-delay 0.25)
  (max-delay 30.0)
  (jitter-ratio 0.0)
  (respect-retry-after-p t)
  (retry-on-timeout-p t)
  (retry-on-connection-error-p t))

(defun make-http-retry-policy
    (&key (max-attempts 1)
          (methods *default-retry-methods*)
          (statuses *default-retry-statuses*)
          (base-delay 0.25)
          (max-delay 30.0)
          (jitter-ratio 0.0)
          (respect-retry-after-p t)
          (retry-on-timeout-p t)
          (retry-on-connection-error-p t))
  (%ensure-positive-integer
   max-attempts
   "The retry attempt limit must be a positive integer.")
  (unless (and (listp methods) (every #'stringp methods))
    (%client-protocol-error "Retry methods must be a list of strings." methods))
  (unless (and (listp statuses)
               (every (lambda (status)
                        (and (integerp status) (<= 100 status 599)))
                      statuses))
    (%client-protocol-error "Retry statuses must be a list of HTTP status integers."
                            statuses))
  (unless (and (realp base-delay) (>= base-delay 0))
    (%client-protocol-error "The retry base delay must be non-negative." base-delay))
  (unless (and (realp max-delay) (>= max-delay base-delay))
    (%client-protocol-error
     "The retry maximum delay must be at least the base delay."
     max-delay))
  (unless (and (realp jitter-ratio) (<= 0 jitter-ratio 1))
    (%client-protocol-error
     "The retry jitter ratio must be between zero and one."
     jitter-ratio))
  (%make-http-retry-policy
   :max-attempts max-attempts
   :methods (copy-list methods)
   :statuses (copy-list statuses)
   :base-delay base-delay
   :max-delay max-delay
   :jitter-ratio jitter-ratio
   :respect-retry-after-p (not (null respect-retry-after-p))
   :retry-on-timeout-p (not (null retry-on-timeout-p))
   :retry-on-connection-error-p (not (null retry-on-connection-error-p))))

(defstruct (http-multipart-part
             (:constructor %make-http-multipart-part)
             (:conc-name http-multipart-part-))
  name
  value
  filename
  content-type)

(defun make-http-multipart-part
    (&key name value filename content-type)
  (unless (and (stringp name) (not (string= name "")))
    (%client-protocol-error "A multipart part requires a non-empty name." name))
  (unless (or (stringp value)
              (and (arrayp value) (= (array-rank value) 1)))
    (%client-protocol-error
     "A multipart part value must be a string or one-dimensional octet vector."
     value))
  (when (and filename (not (stringp filename)))
    (%client-protocol-error "A multipart filename must be a string." filename))
  (when (and content-type (not (stringp content-type)))
    (%client-protocol-error "A multipart content type must be a string." content-type))
  (%make-http-multipart-part :name name
                             :value value
                             :filename filename
                             :content-type content-type))

(defstruct (http-cookie
             (:constructor %make-http-cookie)
             (:conc-name http-cookie-))
  name
  value
  domain
  path
  expires
  max-age
  expiry-time
  secure-p
  http-only-p
  same-site
  partition-key
  host-only-p
  creation-time
  last-access-time)

(defstruct (http-cookie-jar
             (:constructor %make-http-cookie-jar)
             (:conc-name %http-cookie-jar-))
  (cookies nil)
  lock
  (clock-function #'get-universal-time)
  public-suffix-p-function
  (max-cookies 3000)
  (max-cookies-per-domain 180)
  (max-cookie-bytes 4096)
  (max-total-cookie-bytes 12288000))

(defstruct (http-proxy
             (:constructor %make-http-proxy)
             (:conc-name http-proxy-))
  scheme
  host
  port
  username
  password
  no-proxy)

(defun make-http-proxy
    (&key (scheme :http) host port username password no-proxy)
  (unless (member scheme '(:http :https :socks5 :socks5h)
                  :test (lambda (left right)
                          (string= (string left) (string right))))
    (%client-protocol-error "Proxy scheme must be HTTP, HTTPS, SOCKS5, or SOCKS5H."
                            scheme))
  (unless (and (stringp host) (not (string= host "")))
    (%client-protocol-error "A proxy requires a non-empty host." host))
  (%ensure-positive-integer port "A proxy port must be a positive integer.")
  (when (> port 65535)
    (%client-protocol-error "A proxy port must fit in an unsigned 16-bit value." port))
  (when (and username (not (stringp username)))
    (%client-protocol-error "A proxy username must be a string." username))
  (when (and password (not (stringp password)))
    (%client-protocol-error "A proxy password must be a string." password))
  (%make-http-proxy :scheme (intern (string-upcase (string scheme)) :keyword)
                    :host (string-downcase host)
                    :port port
                    :username username
                    :password password
                    :no-proxy no-proxy))

(defstruct (http-cache
             (:constructor %make-http-cache)
             (:conc-name %http-cache-))
  (entries nil)
  (max-entries 256)
  (clock-function #'get-universal-time)
  status-identifier)

(defstruct (http-strict-transport-policy
             (:constructor %make-http-strict-transport-policy)
             (:conc-name http-strict-transport-policy-))
  host
  expires-at
  include-subdomains-p)

(defstruct (http-strict-transport-store
             (:constructor %make-http-strict-transport-store)
             (:conc-name %http-strict-transport-store-))
  (policies nil)
  (clock-function #'get-universal-time))

(defstruct (http-alternative-service
             (:constructor %make-http-alternative-service)
             (:conc-name http-alternative-service-))
  origin
  protocol-id
  host
  port
  expires-at
  persist-p)

(defstruct (http-alternative-service-store
             (:constructor %make-http-alternative-service-store)
             (:conc-name %http-alternative-service-store-))
  (entries nil)
  (clock-function #'get-universal-time))

(defstruct (http-authentication-challenge
             (:constructor %make-http-authentication-challenge)
             (:conc-name http-authentication-challenge-))
  scheme
  token68
  (parameters nil))

(defstruct (http-client
             (:constructor %make-http-client)
             (:conc-name http-client-))
  transport-function
  connection-pool
  (default-headers nil)
  cookie-jar
  cookie-partition-key
  cookie-same-site-context
  cache
  strict-transport-store
  alternative-service-store
  redirect-policy
  retry-policy
  proxy
  tls-upgrade
  resolve-host
  auth-provider
  challenge-auth-provider
  proxy-challenge-auth-provider
  stale-while-revalidate-scheduler
  clock-function
  wall-clock-function
  sleep-function
  random-function
  max-header-bytes
  max-fields
  max-body-bytes
  (automatic-decompression-p t)
  (content-decoders nil)
  on-request
  on-response)
