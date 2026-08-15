(defpackage #:http-kit/tls
  (:use #:cl)
  (:import-from #:http-kit
                #:http-deadline
                #:http-protocol-error
                #:http-timeout
                #:http-uri-host)
  (:export #:http-tls-selected-alpn-protocol
           #:make-http-tls-upgrader
           #:make-http-tls-server-wrapper))
(in-package #:http-kit/tls)

(defun %tls-monotonic-time ()
  (/ (float (get-internal-real-time))
     (float internal-time-units-per-second)))

(defun %tls-timeout ()
  (error 'http-timeout
         :message "The TLS handshake exceeded its deadline."
         :operation :tls
         :kind :tls))

(defun %call-with-tls-deadline (thunk deadline clock-function)
  (let ((remaining (and deadline (- deadline (funcall clock-function)))))
    (when (and remaining (<= remaining 0))
      (%tls-timeout))
    (let ((result
            #+sbcl
            (if remaining
                (handler-case
                    (sb-ext:with-timeout remaining
                      (funcall thunk))
                  (sb-ext:timeout ()
                    (%tls-timeout)))
                (funcall thunk))
            #-sbcl
            (funcall thunk)))
      (when (and deadline (>= (funcall clock-function) deadline))
        (%tls-timeout))
      result)))

(defun %valid-alpn-protocol-p (protocol)
  (and (stringp protocol)
       (<= 1 (length protocol) 255)
       (every (lambda (character)
                (<= 1 (char-code character) 127))
              protocol)))

(defun %validate-certificate-pair (certificate key role &key required)
  (unless (if required
              (and certificate key)
              (or (and certificate key)
                  (and (null certificate) (null key))))
    (error 'http-protocol-error
           :message (format nil
                            "TLS ~A certificates require both CERTIFICATE and KEY."
                            role)
           :operation :tls
           :detail (list :certificate certificate :key key))))

(defun http-tls-selected-alpn-protocol (stream)
  "Return the negotiated ALPN protocol name, or NIL if none was selected."
  (unless (streamp stream)
    (error 'http-protocol-error
           :message "TLS ALPN lookup requires a Lisp stream."
           :operation :tls
           :detail stream))
  (cl+ssl:get-selected-alpn-protocol stream))

(defun make-http-tls-upgrader
    (&key (verify :required) alpn-protocols certificate key password
          (unwrap-stream-p nil) (clock-function #'%tls-monotonic-time))
  "Return a CLIENT TLS-UPGRADE callback backed by CL+SSL.

The callback preserves the binary stream contract used by the client layer.
VERIFY defaults to :REQUIRED; set it to NIL only for explicitly trusted test
or private-network endpoints."
  (unless (member verify '(nil :optional :required))
    (error 'http-protocol-error
           :message "TLS VERIFY must be NIL, :OPTIONAL, or :REQUIRED."
           :operation :tls
           :detail verify))
  (unless (or (null alpn-protocols)
              (and (listp alpn-protocols)
                   (every #'%valid-alpn-protocol-p alpn-protocols)))
    (error 'http-protocol-error
           :message "TLS ALPN protocol names must contain 1 to 255 ASCII characters."
           :operation :tls
           :detail alpn-protocols))
  (%validate-certificate-pair certificate key "client")
  (unless (functionp clock-function)
    (error 'http-protocol-error
           :message "TLS CLOCK-FUNCTION must be a function."
           :operation :tls
           :detail clock-function))
  (lambda (stream uri &key timeout deadline &allow-other-keys)
    (unless (streamp stream)
      (error 'http-protocol-error
             :message "TLS upgrade requires a Lisp stream."
             :operation :tls
             :detail stream))
    (%call-with-tls-deadline
     (lambda ()
       (cl+ssl:make-ssl-client-stream
        stream
        :unwrap-stream-p unwrap-stream-p
        :hostname (http-uri-host uri)
        :external-format nil
        :verify verify
        :alpn-protocols alpn-protocols
        :certificate certificate
        :key key
        :password password))
     (http-deadline timeout :deadline deadline
                          :clock-function clock-function)
     clock-function)))

(defun make-http-tls-server-wrapper
    (&key certificate key password (unwrap-stream-p nil)
          (clock-function #'%tls-monotonic-time))
  "Return a function that upgrades an accepted TCP STREAM to TLS."
  (%validate-certificate-pair certificate key "server" :required t)
  (unless (functionp clock-function)
    (error 'http-protocol-error
           :message "TLS CLOCK-FUNCTION must be a function."
           :operation :tls
           :detail clock-function))
  (lambda (stream &key timeout deadline &allow-other-keys)
    (unless (streamp stream)
      (error 'http-protocol-error
             :message "TLS upgrade requires a Lisp stream."
             :operation :tls
             :detail stream))
    (%call-with-tls-deadline
     (lambda ()
       (cl+ssl:make-ssl-server-stream
        stream
        :unwrap-stream-p unwrap-stream-p
        :external-format nil
        :certificate certificate
        :key key
        :password password))
     (http-deadline timeout :deadline deadline
                          :clock-function clock-function)
     clock-function)))
