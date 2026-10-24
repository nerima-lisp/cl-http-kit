(in-package #:http-kit/test)
#+sbcl
(progn
  (defclass %tls-server-test-stream (binary-test-stream)
    ((closed-p :initform nil :accessor %tls-server-test-stream-closed-p)))

  (defmethod open-stream-p ((stream %tls-server-test-stream))
    (not (%tls-server-test-stream-closed-p stream)))

  (defmethod close ((stream %tls-server-test-stream) &key abort)
    (declare (ignore abort))
    (setf (%tls-server-test-stream-closed-p stream) t)
    t)

  (defun %tls-server-test-read-octets (pathname)
    (with-open-file (stream pathname :direction :input
                                      :element-type '(unsigned-byte 8))
      (let ((bytes (make-array (file-length stream)
                               :element-type '(unsigned-byte 8))))
        (unless (= (read-sequence bytes stream) (length bytes))
          (error "TLS server test input was truncated."))
        bytes)))

  (defun %tls-server-test-private-key (der)
    (let* ((root (cl-tls-kit:der-decode der))
           (children (cl-tls-kit:der-node-children root))
           (scalar-node (second children))
           (scalar (and scalar-node
                        (= (cl-tls-kit:der-node-tag scalar-node) 4)
                        (cl-tls-kit:der-node-value scalar-node)))
           (package (find-package "CRYPTO-KIT"))
           (constructor (and package
                             (find-symbol "MAKE-EC-PRIVATE-KEY" package))))
      (unless (and (= (cl-tls-kit:der-node-tag root) 16)
                   scalar
                   (= (length scalar) 32)
                   constructor
                   (fboundp constructor))
        (error "TLS server test key material is invalid."))
      (funcall (symbol-function constructor) :p256 scalar)))

  (defun %tls-server-test-material ()
    (let* ((directory
             (merge-pathnames
              (format nil "cl-http-kit-tls-server-~D-~D/"
                      (get-universal-time) (random 1000000))
              (uiop:temporary-directory)))
           (certificate (merge-pathnames "server.crt" directory))
           (key (merge-pathnames "server.key" directory))
           (der-key (merge-pathnames "server.der" directory)))
      (ensure-directories-exist certificate)
      (flet ((run-openssl (arguments)
               (multiple-value-bind (output errors status)
                   (uiop:run-program (cons "openssl" arguments)
                                     :output :string :error-output :string
                                     :ignore-error-status t)
                 (declare (ignore output errors))
                 (unless (zerop status)
                   (error "TLS server test material generation failed.")))))
        (run-openssl
         (list "req" "-x509" "-newkey" "ec"
               "-pkeyopt" "ec_paramgen_curve:P-256" "-nodes"
               "-days" "1" "-subj" "/CN=localhost"
               "-addext" "subjectAltName=DNS:localhost"
               "-addext" "basicConstraints=critical,CA:FALSE"
               "-addext" "keyUsage=critical,digitalSignature"
               "-addext" "extendedKeyUsage=serverAuth"
               "-keyout" (namestring key) "-out" (namestring certificate)))
        (run-openssl
         (list "ec" "-in" (namestring key) "-outform" "DER"
               "-out" (namestring der-key))))
      (let* ((blocks (cl-tls-kit:pem-decode (uiop:read-file-string certificate)))
             (block (find "CERTIFICATE" blocks
                          :key #'cl-tls-kit:pem-block-label
                          :test #'string=)))
        (unless block
          (error "TLS server test certificate is unavailable."))
        (list :directory directory
              :files (list certificate key der-key)
              :certificate-chain (list (cl-tls-kit:pem-block-der block))
              :private-key (%tls-server-test-private-key
                            (%tls-server-test-read-octets der-key))))))

  (defun %tls-server-test-remove-material (material)
    (dolist (pathname (getf material :files))
      (when (probe-file pathname)
        (delete-file pathname)))
    (uiop:delete-empty-directory (getf material :directory))
    nil)

  (defun %tls-server-test-condition (thunk)
    (handler-case
        (progn (funcall thunk) nil)
      (http-protocol-error (condition) condition)
      (error () nil)))

  (defun %tls-server-test-safe-condition-p (condition marker)
    (and condition
         (not (search marker (http-error-message condition)))
         (not (search marker (princ-to-string condition)))))

  (defun %tls-server-test-client-connection
      (port ticket-store trust-anchor expected-resumed-p)
    (let* ((request
             (make-http-request
              :method "GET"
              :uri (format nil "http://127.0.0.1:~D/" port)))
           (raw (open-http-tcp-stream request :timeout 5))
           (driver nil))
      (unwind-protect
           (progn
             (setf driver
                   (cl-tls-kit:make-tls13-client-driver
                    :provider (cl-tls-kit:make-cl-crypto-kit-provider)
                    :key-exchange (http-kit/tls::%tls-key-exchange)
                    :transport (http-kit/tls::%tls-transport raw)
                    :hostname "localhost"
                    :alpn '("h2" "http/1.1")
                    :supported-groups '(#x001d #x0017)
                    :ticket-store ticket-store
                    :trust-anchors (list trust-anchor)
                    :verify-signature #'crypto-kit:verify-signature))
             (cl-tls-kit:tls13-client-driver-connect driver)
             (ensure-equal "h2"
                           (cl-tls-kit:tls13-client-driver-negotiated-alpn driver))
             (ensure-equal expected-resumed-p
                           (cl-tls-kit:tls13-client-driver-resumed-p driver))
             (multiple-value-bind (kind bytes)
                 (cl-tls-kit:tls13-client-driver-read-application-data driver)
               (ensure-equal :data kind)
               (ensure-octets-equal (ascii "tls-ok") bytes)))
        (when driver
          (handler-case
              (cl-tls-kit:tls13-client-driver-close driver)
            (error () nil)))
        (when (and raw (open-stream-p raw))
          (close raw :abort t)))))

  (deftest tls-server-wrapper-loopback-alpn-and-resumption
    (let* ((material (%tls-server-test-material))
           (trust-anchor
             (cl-tls-kit.x509:parse-certificate-der
              (first (getf material :certificate-chain))))
           (server-store (cl-tls-kit:make-tls13-server-ticket-key-store))
           (client-store (cl-tls-kit:make-tls13-ticket-store))
           (listener (open-http-tcp-listener :host "127.0.0.1"))
           (wrapper
             (make-http-tls-server-wrapper
              :certificate-chain (getf material :certificate-chain)
              :private-key (getf material :private-key)
              :ticket-key-store server-store
              :alpn-protocols '("h2" "http/1.1")))
           (port (http-network-listener-port listener))
           (server-alpns nil)
           (server-failed-p nil)
           (thread nil)
           (original-maker (fdefinition 'cl-tls-kit:make-tls13-server-driver))
           (observed-stores nil))
      (unwind-protect
           (progn
             (setf (fdefinition 'cl-tls-kit:make-tls13-server-driver)
                   (lambda (&rest arguments &key ticket-key-store &allow-other-keys)
                     (push ticket-key-store observed-stores)
                     (apply original-maker arguments))
                   thread
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (loop repeat 2
                                do (let ((raw (accept-http-tcp-stream listener
                                                                     :timeout 5))
                                          (secure nil))
                              (unwind-protect
                                   (progn
                                     (setf secure (funcall wrapper raw :timeout 5)
                                           raw nil)
                                     (push (http-tls-selected-alpn-protocol secure)
                                           server-alpns)
                                     (write-sequence (ascii "tls-ok") secure)
                                     (finish-output secure)
                                     (close secure))
                                (when (and raw (open-stream-p raw))
                                  (close raw :abort t)))))
                        (error ()
                          (setf server-failed-p t))))
                    :name "TLS server wrapper test")))
             (%tls-server-test-client-connection
              port client-store trust-anchor nil)
             (%tls-server-test-client-connection
              port client-store trust-anchor t)
             (sb-thread:join-thread thread)
             (ensure-false server-failed-p)
             (ensure-equal '("h2" "h2") (nreverse server-alpns))
             (ensure-equal 2 (length observed-stores))
             (ensure-true (every (lambda (store) (eq server-store store))
                                 observed-stores)))
        (when thread
          (close-http-tcp-listener listener)
          (sb-thread:join-thread thread))
        (setf (fdefinition 'cl-tls-kit:make-tls13-server-driver)
              original-maker)
        (close-http-tcp-listener listener)
        (%tls-server-test-remove-material material))))

  (deftest tls-server-wrapper-rejects-early-data-after-hrr
    (let* ((hello
             (cl-tls-kit:make-tls13-client-hello
              #x0303
              (make-array 32 :element-type '(unsigned-byte 8)
                              :initial-element 1)
              (octets)
              #( #x1301)
              (list (cl-tls-kit:make-tls-extension 42 (octets)))))
           (hello-wire (cl-tls-kit:encode-handshake 1 hello))
           (hrr-wire
             (cl-tls-kit:encode-handshake
              2
              (cl-tls-kit:make-tls13-hello-retry-request
               #x0303 #x0017
               (list (cl-tls-kit:make-tls-extension 43 (octets 3 4)))
               #x1301)))
           (hello-stream
             (make-instance '%tls-server-test-stream :input (octets)))
           (hello-state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider nil :transport-stream hello-stream)))
      (handler-case
          (http-kit/tls::%tls-server-step-handshake-message hello-state hello-wire)
        (http-protocol-error () nil))
      (ensure-true
       (http-kit/tls::http-tls-server-state-early-data-offered-p hello-state))
      (close hello-stream :abort t)
      (let* ((raw (make-instance '%tls-server-test-stream :input (octets)))
             (state (http-kit/tls::%make-http-tls-server-state
                     :driver nil :provider nil :transport-stream raw)))
        (setf (http-kit/tls::http-tls-server-state-early-data-offered-p state) t)
        (http-kit/tls::%tls-server-handle-events
         state
         (list (list :type :send
                     :level :initial
                     :content-type :handshake
                     :wire hrr-wire)))
        (ensure-true
         (http-kit/tls::http-tls-server-state-hello-retry-request-sent-p state))
        (let ((malformed-state
                (http-kit/tls::%make-http-tls-server-state
                 :driver nil :provider nil :transport-stream nil)))
          (signals cl-tls-kit:tls13-decode-error
            (http-kit/tls::%tls-server-handle-events
             malformed-state
             (list (list :type :send
                         :level :initial
                         :content-type :handshake
                         :wire (octets 2 0 0 1))))))
        (setf (fill-pointer (binary-test-output raw)) 0)
        (let ((condition
                (%tls-server-test-condition
                 (lambda ()
                   (http-kit/tls::%tls-server-process-record
                    state (octets 23 3 3 0 1 0))))))
          (ensure-equal :early-data-rejected
                        (http-protocol-error-detail condition))
          (ensure-octets-equal #(21 3 3 0 2 2 10)
                               (binary-test-output raw)))
        (close raw :abort t))))

  (deftest tls-server-wrapper-sanitizes-peer-and-setup-failures
    (let* ((material (%tls-server-test-material))
           (marker "CLIENT-HELLO-SECRET-MARKER")
           (marker-bytes (ascii marker))
           (handshake
             (concatenate-octets
              (octets 1 0 0 (length marker-bytes))
              marker-bytes))
           (wire
             (concatenate-octets
              (octets 22 3 3 0 (length handshake))
              handshake))
           (stream (make-instance '%tls-server-test-stream :input wire))
           (wrapper
             (make-http-tls-server-wrapper
              :certificate-chain (getf material :certificate-chain)
              :private-key (getf material :private-key)
              :alpn-protocols '("h2"))))
      (unwind-protect
           (let ((condition
                   (%tls-server-test-condition
                    (lambda () (funcall wrapper stream :timeout 2)))))
             (ensure-equal :handshake-failed
                           (http-protocol-error-detail condition))
             (ensure-true (%tls-server-test-safe-condition-p condition marker))
             (ensure-false (open-stream-p stream)))
        (when (open-stream-p stream)
          (close stream :abort t))
        (%tls-server-test-remove-material material)))
    (let* ((marker "PRIVATE-KEY-SECRET-MARKER")
           (store (cl-tls-kit:make-tls13-server-ticket-key-store))
           (stream (make-instance '%tls-server-test-stream :input (octets)))
           (wrapper
             (make-http-tls-server-wrapper
              :certificate-chain (list (ascii marker))
              :private-key marker
              :ticket-key-store store
              :alpn-protocols '("h2"))))
      (unwind-protect
           (let ((condition
                   (%tls-server-test-condition
                    (lambda () (funcall wrapper stream :timeout 2)))))
             (ensure-equal :handshake-failed
                           (http-protocol-error-detail condition))
             (ensure-true (%tls-server-test-safe-condition-p condition marker))
             (ensure-false (open-stream-p stream)))
        (when (open-stream-p stream)
          (close stream :abort t)))))

