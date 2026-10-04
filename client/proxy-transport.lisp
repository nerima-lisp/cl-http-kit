(in-package #:http-kit/client)

(defun %proxy-monotonic-time ()
  (/ (float (get-internal-real-time))
     internal-time-units-per-second))

(defun %proxy-error (message detail &optional cause)
  (%client-error 'http-proxy-error
                 message
                 :detail (list :detail detail :cause cause)
                 :operation :proxy))

(defun %proxy-public-plan (plan)
  (loop for (key value) on plan by #'cddr
        unless (member key '(:proxy :proxy-authorization
                             :proxy-challenge-auth-provider))
          append (list key value)))

(defun %proxy-check-deadline (deadline clock-function &optional (kind :proxy))
  (when (and deadline (>= (funcall clock-function) deadline))
    (error 'http-timeout
           :message "The proxy operation exceeded its deadline."
           :operation kind
           :kind kind)))

(defun %proxy-read-byte (stream deadline clock-function)
  (%proxy-check-deadline deadline clock-function :read)
  (let ((byte (read-byte stream nil nil)))
    (unless byte
      (%proxy-error "The proxy closed the stream during negotiation." :eof))
    (%proxy-check-deadline deadline clock-function :read)
    byte))

(defun %proxy-read-exact (stream count deadline clock-function)
  (unless (and (integerp count) (>= count 0))
    (%proxy-error "The proxy read length must be a non-negative integer." count))
  (let ((result (make-array count :element-type '(unsigned-byte 8))))
    (dotimes (index count result)
      (setf (aref result index)
            (%proxy-read-byte stream deadline clock-function)))))

(defun %proxy-write-octets (stream octets deadline clock-function)
  (%proxy-check-deadline deadline clock-function :write)
  (write-sequence octets stream)
  (finish-output stream)
  (%proxy-check-deadline deadline clock-function :write)
  stream)

(defun %proxy-byte-builder ()
  (make-array 32
              :element-type '(unsigned-byte 8)
              :adjustable t
              :fill-pointer 0))

(defun %proxy-builder-byte (builder byte)
  (unless (and (integerp byte) (<= 0 byte 255))
    (%proxy-error "A proxy negotiation byte is outside the octet range." byte))
  (vector-push-extend byte builder)
  builder)

(defun %proxy-builder-octets (builder octets)
  (loop for byte across octets
        do (%proxy-builder-byte builder byte))
  builder)

(defun %proxy-builder-vector (builder)
  (let ((result (make-array (length builder)
                            :element-type '(unsigned-byte 8))))
    (replace result builder)
    result))

(defun %proxy-resolve-host (resolve-host host timeout deadline)
  (handler-case
      (funcall resolve-host host :timeout timeout :deadline deadline)
    (program-error ()
      ;; Host-only resolver callbacks predate the timeout-aware contract.
      (funcall resolve-host host))))

(defun %proxy-resolved-address (host resolve-host &optional timeout deadline)
  (or (%proxy-ipv4-octets host)
      (%proxy-ipv6-octets host)
      (when resolve-host
        (let ((resolved (%proxy-resolve-host resolve-host host timeout deadline)))
          (or (and (stringp resolved)
                   (or (%proxy-ipv4-octets resolved)
                       (%proxy-ipv6-octets resolved)))
              (%proxy-error
               "The local proxy resolver must return a numeric IPv4 or IPv6 address."
               resolved))))))

(defun %proxy-socks-address (host remote-dns-p resolve-host
                             &optional timeout deadline)
  (if remote-dns-p
      (let ((octets (http-utf8-octets host)))
        (when (or (zerop (length octets)) (> (length octets) 255))
          (%proxy-error "A SOCKS5 domain name must fit in one octet length." host))
        (let ((builder (%proxy-byte-builder)))
          (%proxy-builder-byte builder 3)
          (%proxy-builder-byte builder (length octets))
          (%proxy-builder-octets builder octets)
          builder))
      (let ((address (%proxy-resolved-address host resolve-host
                                              timeout deadline)))
        (unless address
          (%proxy-error
           "A numeric address or a local resolver is required for SOCKS5."
           host))
        (cond
          ((= (length address) 4)
           (let ((builder (%proxy-byte-builder)))
             (%proxy-builder-byte builder 1)
             (%proxy-builder-octets builder address)
             builder))
          ((= (length address) 16)
           (let ((builder (%proxy-byte-builder)))
             (%proxy-builder-byte builder 4)
             (%proxy-builder-octets builder address)
             builder))
          (t (%proxy-error "The SOCKS5 address has an unsupported length.
"                            (length address)))))))

(defun %proxy-socks-negotiate
    (stream proxy-plan timeout deadline clock-function resolve-host)
  (let* ((proxy (getf proxy-plan :proxy))
         (username (http-proxy-username proxy))
         (password (or (http-proxy-password proxy) ""))
         (use-auth-p (not (null username)))
         (user-octets (and use-auth-p (http-utf8-octets username)))
         (password-octets (and use-auth-p (http-utf8-octets password)))
         (methods (if use-auth-p #(2) #(0)))
         (greeting (make-array (+ 2 (length methods))
                               :element-type '(unsigned-byte 8))))
    (when (and use-auth-p
               (or (not (<= 1 (length user-octets) 255))
                   (not (<= 1 (length password-octets) 255))))
      (%proxy-error
       "SOCKS5 username and password must each contain 1 through 255 UTF-8 octets."
       :authentication))
    (setf (aref greeting 0) 5
          (aref greeting 1) (length methods))
    (replace greeting methods :start1 2)
    (%proxy-write-octets stream greeting deadline clock-function)
    (unless (= (%proxy-read-byte stream deadline clock-function) 5)
      (%proxy-error "The SOCKS5 proxy returned an invalid version." :version))
    (let ((method (%proxy-read-byte stream deadline clock-function)))
      (cond
        ((= method #xff)
         (%proxy-error "The SOCKS5 proxy rejected every authentication method."
                       :authentication))
        ((and use-auth-p (= method 2))
         (let ((authentication (%proxy-byte-builder)))
           (%proxy-builder-byte authentication 1)
           (%proxy-builder-byte authentication (length user-octets))
           (%proxy-builder-octets authentication user-octets)
           (%proxy-builder-byte authentication (length password-octets))
           (%proxy-builder-octets authentication password-octets)
           (%proxy-write-octets
            stream
            (%proxy-builder-vector authentication)
            deadline clock-function))
         (unless (= (%proxy-read-byte stream deadline clock-function) 1)
           (%proxy-error "The SOCKS5 proxy returned an invalid authentication version."
                         :authentication))
         (unless (zerop (%proxy-read-byte stream deadline clock-function))
           (%proxy-error "The SOCKS5 proxy rejected username/password authentication."
                         :authentication)))
        ((and (not use-auth-p) (zerop method)) nil)
        (t (%proxy-error "The SOCKS5 proxy selected an unsupported authentication method."
                         method))))
    (let* ((host (getf proxy-plan :connect-host))
           (port (getf proxy-plan :connect-port))
           (address (%proxy-socks-address host
                                          (getf proxy-plan :remote-dns-p)
                                          resolve-host timeout deadline))
           (request (%proxy-byte-builder)))
      (%proxy-builder-byte request 5)
      (%proxy-builder-byte request 1)
      (%proxy-builder-byte request 0)
      (%proxy-builder-octets request (%proxy-builder-vector address))
      (%proxy-builder-byte request (ldb (byte 8 8) port))
      (%proxy-builder-byte request (ldb (byte 8 0) port))
      (%proxy-write-octets stream (%proxy-builder-vector request)
                           deadline clock-function)
      (unless (= (%proxy-read-byte stream deadline clock-function) 5)
        (%proxy-error "The SOCKS5 proxy returned an invalid reply version."
                      :reply))
      (let ((reply (%proxy-read-byte stream deadline clock-function)))
        (unless (zerop reply)
          (%proxy-error "The SOCKS5 proxy rejected the CONNECT request." reply)))
      (unless (zerop (%proxy-read-byte stream deadline clock-function))
        (%proxy-error "The SOCKS5 proxy returned an invalid reserved byte." :reply))
      (let ((address-type (%proxy-read-byte stream deadline clock-function)))
        (case address-type
          (1 (%proxy-read-exact stream 4 deadline clock-function))
          (3 (let ((length (%proxy-read-byte stream deadline clock-function)))
               (%proxy-read-exact stream length deadline clock-function)))
          (4 (%proxy-read-exact stream 16 deadline clock-function))
          (t (%proxy-error "The SOCKS5 proxy returned an invalid address type."
                           address-type))))
      (%proxy-read-exact stream 2 deadline clock-function)
      stream)))

(defun %proxy-authority-for (host port)
  (if (find #\: host)
      (format nil "[~A]:~D" host port)
      (format nil "~A:~D" host port)))

(defun %proxy-uri-for (scheme host port)
  (make-http-uri :scheme (string-downcase (string scheme))
                 :authority (%proxy-authority-for host port)
                 :path "/"))

(defun %proxy-open-raw
    (open-stream request timeout deadline proxy-plan proxy)
  (if (or (null proxy-plan)
          (eq (getf proxy-plan :mode) :direct))
      (funcall open-stream request :timeout timeout :deadline deadline)
      (funcall open-stream request
               :timeout timeout
               :deadline deadline
               :proxy-plan proxy-plan
               :proxy proxy)))

(defun %proxy-upgrade
    (stream tls-upgrade uri timeout deadline)
  (unless tls-upgrade
    (error 'http-unsupported-feature
           :message "A TLS upgrade function is required for this proxied HTTPS connection."
           :operation :proxy
           :detail uri
           :feature :tls-upgrade))
  (let ((upgraded (funcall tls-upgrade stream uri
                           :timeout timeout
                           :deadline deadline)))
    (unless (streamp upgraded)
      (%proxy-error "The TLS upgrade function did not return a stream." upgraded))
    upgraded))

(defun %proxy-connect
    (stream proxy-plan timeout deadline clock-function)
  (let* ((target (getf proxy-plan :target))
         (authority
           (%proxy-authority-for
            (http-uri-host target)
            (getf proxy-plan :connect-port)))
         (connect-uri (%proxy-uri-for :http
                                      (http-uri-host target)
                                      (getf proxy-plan :connect-port)))
         (headers (list (make-http-header "Host"
                                          authority)))
         (authorization (getf proxy-plan :proxy-authorization)))
    (when authorization
      (push (make-http-header "Proxy-Authorization" authorization) headers))
    (let ((request (make-http-request
                    :method "CONNECT"
                    :uri connect-uri
                    :headers (nreverse headers)
                    :body (make-array 0 :element-type '(unsigned-byte 8)))))
      (multiple-value-bind (response reusable-p)
          (send-http-request-over-open-stream
           request stream
           :timeout timeout
           :deadline deadline
           :request-target authority
           :clock-function clock-function)
        (declare (ignore reusable-p))
        (if (<= 200 (http-response-status response) 299)
            (values stream nil)
            (values nil response))))))

(defun %proxy-open-plan-once
    (open-stream close-stream request proxy-plan proxy tls-upgrade resolve-host
                 timeout deadline clock-function)
  (let* ((mode (or (getf proxy-plan :mode) :direct))
         (proxy (or proxy (getf proxy-plan :proxy)))
         (target (or (getf proxy-plan :target)
                     (http-request-uri request)))
         (stream (%proxy-open-raw open-stream request timeout deadline
                                  proxy-plan proxy))
         (retained-p nil))
    (unwind-protect
         (progn
           (unless (streamp stream)
             (%proxy-error "The proxy stream opener did not return a stream." stream))
           (setf stream
                 (case mode
                   (:direct
                    (if (string= (http-uri-scheme target) "https")
                        (%proxy-upgrade stream tls-upgrade target timeout deadline)
                        stream))
                   (:forward
                    (when (eq (http-proxy-scheme proxy) :https)
                      (setf stream
                            (%proxy-upgrade
                             stream tls-upgrade
                             (%proxy-uri-for :https (http-proxy-host proxy)
                                             (http-proxy-port proxy))
                             timeout deadline)))
                    stream)
                   (:connect
                    (when (eq (http-proxy-scheme proxy) :https)
                      (setf stream
                            (%proxy-upgrade
                             stream tls-upgrade
                             (%proxy-uri-for :https (http-proxy-host proxy)
                                             (http-proxy-port proxy))
                             timeout deadline)))
                    (multiple-value-bind (connected response)
                        (%proxy-connect stream proxy-plan timeout deadline
                                        clock-function)
                      (if response
                          (return-from %proxy-open-plan-once
                            (values nil response))
                          (if (string= (http-uri-scheme target) "https")
                              (%proxy-upgrade connected tls-upgrade target
                                              timeout deadline)
                              connected))))
                   (:socks5
                    (%proxy-socks-negotiate stream proxy-plan timeout deadline
                                            clock-function resolve-host)
                    (if (string= (http-uri-scheme target) "https")
                        (%proxy-upgrade stream tls-upgrade target
                                        timeout deadline)
                        stream))
                   (otherwise
                    (%proxy-error "The proxy plan contains an unsupported mode." mode))))
           (setf retained-p t)
           stream)
      (unless retained-p
        (when (streamp stream)
          (http-kit::%with-http-cleanup
            (funcall close-stream stream)))))))

(defun %proxy-auth-provider-value (value)
  (cond
    ((null value) nil)
    ((stringp value) value)
    ((http-header-p value) (http-header-content value))
    (t
     (%proxy-error
      "A proxy authentication provider must return a string, HTTP-HEADER, or NIL."
      value))))

(defun %proxy-open-plan
    (open-stream close-stream request proxy-plan proxy tls-upgrade resolve-host
                 timeout deadline clock-function)
  (let ((plan proxy-plan)
        (challenge-count 0))
    (loop
      (multiple-value-bind (stream response)
          (%proxy-open-plan-once
           open-stream close-stream request plan proxy tls-upgrade resolve-host
           timeout deadline clock-function)
        (when stream
          (return stream))
        (let* ((challenge-values
                 (and (= (http-response-status response) 407)
                      (http-header-values
                       (http-response-headers response) "Proxy-Authenticate")))
               (challenges
                 (and challenge-values
                      (http-parse-authentication-challenges challenge-values)))
               (provider (getf plan :proxy-challenge-auth-provider))
               (authorization
                 (and provider
                      (zerop challenge-count)
                      challenges
                      (%proxy-auth-provider-value
                       (funcall provider request response plan challenges)))))
          (unless authorization
            (%proxy-error "The HTTP proxy rejected the CONNECT request."
                          (list :status (http-response-status response)
                                :reason (http-response-reason response))))
          (setf plan (copy-list plan)
                (getf plan :proxy-authorization) authorization
                challenge-count 1))))))

(defun make-http-proxy-stream-opener
    (open-stream close-stream &key tls-upgrade resolve-host
                            (clock-function #'%proxy-monotonic-time))
  "Return an OPEN-STREAM function that performs HTTP and SOCKS proxy setup.

The returned function accepts REQUEST and :TIMEOUT, :DEADLINE, :PROXY-PLAN, and
:PROXY.  OPEN-STREAM is the raw endpoint opener.  For direct requests it is
called with only :TIMEOUT and :DEADLINE, preserving the original stream
boundary contract.  TLS-UPGRADE receives STREAM, a target URI, and the same
timeout keywords and must return a stream.  RESOLVE-HOST receives HOST,
:TIMEOUT, and :DEADLINE for SOCKS5 local DNS.  Host-only resolver callbacks
remain supported as a compatibility fallback.  RESOLVE-HOST is used for
numeric address resolution when a SOCKS5 (rather than SOCKS5H) proxy is
selected."
  (%ensure-function open-stream
                    "A proxy stream opener requires an :OPEN-STREAM function.")
  (%ensure-function close-stream
                    "A proxy stream opener requires a :CLOSE-STREAM function.")
  (%ensure-function clock-function
                    "A proxy stream opener clock must be a function.")
  (when tls-upgrade
    (%ensure-function tls-upgrade
                      "The proxy TLS upgrade function must be a function."))
  (when resolve-host
    (%ensure-function resolve-host
                      "The proxy host resolver must be a function."))
  (lambda (request &key timeout deadline proxy-plan proxy)
    (let ((stream nil)
          (retained-p nil))
      (handler-case
          (unwind-protect
               (progn
                 (setf stream
                       (%proxy-open-plan
                        open-stream close-stream request proxy-plan proxy tls-upgrade
                        resolve-host timeout deadline clock-function))
                 (setf retained-p t)
                 stream)
            (unless retained-p
              (when stream
                (http-kit::%with-http-cleanup
                  (funcall close-stream stream)))))
        (http-error (condition)
          (error condition))
        (error (condition)
          (%proxy-error "Proxy negotiation signaled an error."
                        (%proxy-public-plan proxy-plan)
                        condition))))))
