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
        (close raw :abort t)))
    (let* ((stream (make-instance '%tls-server-test-stream :input (octets)))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider nil :transport-stream stream)))
      (setf (http-kit/tls::http-tls-server-state-early-data-offered-p state) t
            (http-kit/tls::http-tls-server-state-handshake-complete-p state) t)
      (let ((condition
              (%tls-server-test-condition
               (lambda ()
                 (http-kit/tls::%tls-server-process-record
                  state (octets 23 3 3 0 1 0))))))
        (ensure-equal :unexpected-record
                      (http-protocol-error-detail condition)))
      (close stream :abort t)))
(deftest tls-server-wrapper-does-not-issue-session-tickets
  (%tls-server-test-with-spy
   'cl-tls-kit:make-tls13-server-ticket-key-store
   (lambda (&rest arguments)
     (declare (ignore arguments))
     (error "Session ticket store creation is disabled."))
   (lambda ()
     (let* ((material (%tls-server-test-material))
            (trust-anchor
              (cl-tls-kit.x509:parse-certificate-der
               (first (getf material :certificate-chain))))
            (client-store (cl-tls-kit:make-tls13-ticket-store))
            (listener (open-http-tcp-listener :host "127.0.0.1"))
            (wrapper
              (make-http-tls-server-wrapper
               :certificate-chain (getf material :certificate-chain)
               :private-key (getf material :private-key)
               :session-tickets-p nil
               :alpn-protocols '("h2" "http/1.1")))
            (port (http-network-listener-port listener))
            (server-failed-p nil)
            (thread nil))
       (unwind-protect
            (progn
              (setf thread
                    (sb-thread:make-thread
                     (lambda ()
                       (handler-case
                           (let ((raw (accept-http-tcp-stream listener :timeout 5))
                                 (secure nil))
                             (unwind-protect
                                  (progn
                                    (setf secure (funcall wrapper raw :timeout 5)
                                          raw nil)
                                    (write-sequence (ascii "tls-ok") secure)
                                    (finish-output secure)
                                    (close secure))
                               (when (and raw (open-stream-p raw))
                                 (close raw :abort t))))
                         (error ()
                           (setf server-failed-p t))))
                     :name "TLS server wrapper without tickets"))
              (%tls-server-test-client-connection
               port client-store trust-anchor nil)
              (sb-thread:join-thread thread)
              (ensure-false server-failed-p)
              (ensure-equal 0 (cl-tls-kit:tls13-ticket-store-count client-store)))
         (when thread
           (close-http-tcp-listener listener)
           (sb-thread:join-thread thread))
         (close-http-tcp-listener listener)
         (%tls-server-test-remove-material material))))))
(deftest tls-server-wrapper-destroys-driver-owned-ticket-stores
         (let* ((material (%tls-server-test-material))
                (shared-store (cl-tls-kit:make-tls13-server-ticket-key-store))
                (original-maker (fdefinition 'cl-tls-kit:make-tls13-server-driver)))
           (labels ((exercise-close (abort-p supplied-store-p)
                                    (let* ((raw (make-instance '%tls-server-test-stream
                                                               :input (octets)))
                                           (wrapper
                                            (if supplied-store-p
                                                (make-http-tls-server-wrapper
                                                 :certificate-chain (getf material :certificate-chain)
                                                 :private-key (getf material :private-key)
                                                 :ticket-key-store shared-store
                                                 :alpn-protocols '("h2"))
                                              (make-http-tls-server-wrapper
                                               :certificate-chain (getf material :certificate-chain)
                                               :private-key (getf material :private-key)
                                               :alpn-protocols '("h2"))))
                                           (secure nil)
                                           (driver nil)
                                           (driver-store nil))
                                      (unwind-protect
                                          (%tls-server-test-with-spy
                                           'http-kit/tls::%tls-server-handshake
                                           (lambda (state)
                                             (setf (http-kit/tls::http-tls-server-state-handshake-complete-p
                                                    state)
                                                   t)
                                             state)
                                           (lambda ()
                                             (setf secure (funcall wrapper raw)
                                                   raw nil
                                                   driver
                                                   (http-kit/tls::http-tls-server-state-driver
                                                    (http-kit/tls::%tls-stream-driver secure))
                                                   driver-store
                                                   (cl-tls-kit:tls13-server-driver-ticket-key-store driver))
                                             (ensure-true (cl-tls-kit:tls13-server-ticket-key-store-p driver-store))
                                             (ensure-equal (if supplied-store-p shared-store driver-store)
                                                           driver-store)
                                             (close secure :abort abort-p)
                                             (ensure-equal (not supplied-store-p)
                                                           (cl-tls-kit:tls13-server-ticket-key-store-destroyed-p driver-store))))
                                        (when secure
                                          (http-kit::%with-http-cleanup
                                           (close secure :abort t)))
                                        (when (and raw (open-stream-p raw))
                                          (close raw :abort t))))))
             (unwind-protect
                 (progn
                   (dolist (abort-p '(nil t))
                     (exercise-close abort-p nil)
                     (exercise-close abort-p t))
                   (let* ((raw (make-instance '%tls-server-test-stream
                                              :input (octets)))
                          (wrapper
                           (make-http-tls-server-wrapper
                            :certificate-chain (getf material :certificate-chain)
                            :private-key (getf material :private-key)
                            :alpn-protocols '("h2")))
                          (driver nil)
                          (driver-store nil)
                          (condition nil))
                     (unwind-protect
                         (%tls-server-test-with-spy
                          'cl-tls-kit:make-tls13-server-driver
                          (lambda (&rest arguments)
                            (setf driver (apply original-maker arguments)
                                  driver-store
                                  (cl-tls-kit:tls13-server-driver-ticket-key-store driver))
                            driver)
                          (lambda ()
                            (setf condition
                                  (%tls-server-test-condition
                                   (lambda () (funcall wrapper raw :timeout 2))))
                            (ensure-equal :handshake-failed
                                          (http-protocol-error-detail condition))
                            (ensure-true (cl-tls-kit:tls13-server-ticket-key-store-p driver-store))
                            (ensure-true
                             (cl-tls-kit:tls13-server-ticket-key-store-destroyed-p driver-store))
                            (ensure-false (open-stream-p raw))))
                       (when (open-stream-p raw)
                         (close raw :abort t))))
                   (cl-tls-kit:destroy-tls13-server-ticket-key-store shared-store)
                   (%tls-server-test-remove-material material))))))
(deftest tls-server-wrapper-closes-driver-after-close-notify-failure
  (let* ((material (%tls-server-test-material))
         (transport (make-instance '%tls-server-test-stream :input (octets)))
         (wrapper
           (make-http-tls-server-wrapper
            :certificate-chain (getf material :certificate-chain)
            :private-key (getf material :private-key)
            :alpn-protocols '("h2")))
         (secure nil)
         (driver nil)
         (driver-store nil)
         (close-failed-p nil))
    (unwind-protect
         (%tls-server-test-with-spy
          'http-kit/tls::%tls-server-handshake
          (lambda (state)
            (setf (http-kit/tls::http-tls-server-state-handshake-complete-p state) t)
            state)
          (lambda ()
            (setf secure (funcall wrapper transport :timeout 2)
                  driver
                  (http-kit/tls::http-tls-server-state-driver
                   (http-kit/tls::%tls-stream-driver secure))
                  driver-store
                  (cl-tls-kit:tls13-server-driver-ticket-key-store driver))
            (ensure-true (cl-tls-kit:tls13-server-ticket-key-store-p driver-store))
            (%tls-server-test-with-spy
             'http-kit/tls::%tls-server-send-close-notify
             (lambda (state &rest arguments)
               (declare (ignore state arguments))
               (error "Injected TLS close_notify failure."))
             (lambda ()
               (handler-case
                   (close secure)
                 (error ()
                   (setf close-failed-p t)))))
            (ensure-true close-failed-p)
            (ensure-true
             (cl-tls-kit:tls13-server-ticket-key-store-destroyed-p driver-store))
            (ensure-false (open-stream-p transport))))
      (when secure
        (http-kit::%with-http-cleanup (close secure :abort t)))
      (when (open-stream-p transport)
        (close transport :abort t))
      (%tls-server-test-remove-material material))))

  (progn
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
           (progn
             (close stream :abort t)
             (let ((condition
                     (%tls-server-test-condition
                      (lambda () (funcall wrapper stream :timeout 2)))))
               (ensure-equal :handshake-failed
                             (http-protocol-error-detail condition))
               (ensure-true (%tls-server-test-safe-condition-p condition marker))
               (ensure-false (open-stream-p stream))))
        (when (open-stream-p stream)
          (close stream :abort t)))))
(deftest tls-server-wrapper-constructor-validation-boundaries
  (dolist (alpn (list 1 '("h2" "h2") (list (string (code-char 128)))))
    (let ((condition
            (%tls-server-test-condition
             (lambda ()
               (make-http-tls-server-wrapper
                :certificate-chain (list t) :private-key t
                :alpn-protocols alpn)))))
      (ensure-equal :invalid-alpn
                    (http-protocol-error-detail condition))))
  (let ((condition
          (%tls-server-test-condition
           (lambda ()
             (make-http-tls-server-wrapper
              :certificate-chain (list t) :private-key t
              :ticket-key-store :not-a-ticket-key-store)))))
    (ensure-equal :invalid-ticket-key-store
                  (http-protocol-error-detail condition)))
  (%tls-server-test-with-spy
   'cl-tls-kit:make-tls13-server-ticket-key-store
   (lambda (&rest arguments)
     (declare (ignore arguments))
     (error "Unexpected wrapper-level ticket key store creation."))
   (lambda ()
     (let ((wrapper
             (make-http-tls-server-wrapper
              :certificate-chain (list t) :private-key t)))
       (ensure-true (functionp wrapper)))))
  (let* ((wrapper (make-http-tls-server-wrapper
                   :certificate-chain (list t) :private-key t))
         (condition
           (%tls-server-test-condition
            (lambda () (funcall wrapper :not-a-stream)))))
    (ensure-equal :invalid-stream
                  (http-protocol-error-detail condition)))
  (let* ((stream (make-instance '%tls-server-test-stream :input (octets)))
         (wrapper
           (make-http-tls-server-wrapper
            :certificate-chain (list t) :private-key t
            :clock-function (lambda () 1)))
         (condition
           (handler-case
               (progn (funcall wrapper stream :deadline 0) nil)
             (http-timeout (condition) condition))))
    (ensure-true condition)
    (ensure-equal :tls (http-timeout-kind condition))
    (ensure-false (open-stream-p stream))))

  (defclass %tls-server-failure-test-stream (%tls-server-test-stream)
    ((fail-on :initarg :fail-on :reader %tls-server-failure-test-stream-fail-on)))

  (defmethod sb-gray:stream-read-sequence
      ((stream %tls-server-failure-test-stream) sequence &optional (start 0) end)
    (declare (ignore sequence start end))
    (let ((failure (%tls-server-failure-test-stream-fail-on stream)))
      (cond ((eq failure :read)
             (error "Injected TLS transport read failure."))
            #+sbcl
            ((eq failure :sbcl-timeout)
             (signal 'sb-ext:timeout))
            ((typep failure 'condition)
             (error failure))))
    (call-next-method))

  (defmethod sb-gray:stream-write-sequence
      ((stream %tls-server-failure-test-stream) sequence &optional (start 0) end)
    (declare (ignore sequence start end))
    (let ((failure (%tls-server-failure-test-stream-fail-on stream)))
      (cond ((eq failure :write)
             (error "Injected TLS transport write failure."))
            ((typep failure 'condition)
             (error failure))))
    (call-next-method))

  (defun %tls-server-test-with-spy (symbol replacement thunk)
    (let ((original (fdefinition symbol)))
      (unwind-protect
           (progn
             (setf (fdefinition symbol) replacement)
             (funcall thunk))
        (setf (fdefinition symbol) original))))

  (deftest tls-client-driver-read-status-boundaries
    (let* ((raw (make-instance '%tls-server-test-stream :input (octets)))
           (stream (http-kit/tls::%make-http-tls-stream :client raw)))
      (unwind-protect
           (%tls-server-test-with-spy
            'cl-tls-kit:tls13-client-driver-read-application-data
            (lambda (driver)
              (declare (ignore driver))
              (values :eof nil))
            (lambda ()
              (setf (http-kit/tls::%tls-stream-input stream) (octets 17 18)
                    (http-kit/tls::%tls-stream-input-position stream) 0)
              (ensure-equal 17 (read-byte stream))
              (ensure-equal 18 (read-byte stream))
              (ensure-equal :eof (read-byte stream nil :eof))))
        (close stream :abort t)))
    (dolist (status '(:data :eof :would-block))
      (let* ((raw (make-instance '%tls-server-test-stream :input (octets)))
             (stream (http-kit/tls::%make-http-tls-stream :client raw))
             (reader
               (let ((read-count 0))
                 (lambda (driver)
                   (declare (ignore driver))
                   (case status
                     (:data (values :data (octets 42)))
                     (:eof (values :eof nil))
                     (:would-block
                      (if (= (incf read-count) 1)
                          (values :would-block nil)
                          (values :data (octets 42)))))))
             ))
        (unwind-protect
             (%tls-server-test-with-spy
              'cl-tls-kit:tls13-client-driver-read-application-data
              reader
              (lambda ()
                (let* ((value nil)
                       (condition
                         (%tls-server-test-condition
                          (lambda ()
                            (setf value (read-byte stream nil :eof))))))
                  (case status
                    (:data
                     (ensure-equal nil condition)
                     (ensure-equal 42 value))
                    (:eof
                     (ensure-equal nil condition)
                     (ensure-equal :eof value))
                    (:would-block
                     (ensure-equal nil condition)
                     (ensure-equal 42 value))))))
          (close stream :abort t)))))
  (deftest tls-public-upgrader-http2-reader-status-boundaries
  (labels ((public-request (reader-function &optional timeout)
             (let ((raw (%tls-server-test-controlled-stream (octets)))
                   (read-count 0))
               (%tls-server-test-call-with-public-client-fixture
                raw
                (lambda (actual-driver)
                  (incf read-count)
                  (funcall reader-function actual-driver))
                (lambda (tls-stream driver transport written)
                  (declare (ignore driver transport written))
                  (let ((connection
                          (http-kit/http2:make-http2-connection
                           :stream tls-stream
                           :close-stream
                           (lambda (stream)
                             (close stream :abort t)))))
                    (unwind-protect
                         (values
                          (handler-case
                              (http-kit/http2:send-http2-request-over-connection
                               connection
                               (make-http-request
                                :method "GET"
                                :uri "https://example.test/")
                               :timeout timeout)
                            (error (condition) condition))
                          read-count)
                      (http-kit/http2:close-http2-connection connection))))))))
    (let ((response-wire (h2-response-wire (octets)))
          (driver-reads 0))
      (multiple-value-bind (result read-count)
          (public-request
           (lambda (driver)
             (declare (ignore driver))
             (if (= (incf driver-reads) 1)
                 (values :data response-wire)
                 (values :would-block nil)))
           1d0)
        (ensure-equal 200 (http-response-status result))
        (ensure-equal 1 read-count)
        (ensure-equal 1 driver-reads)))
    (multiple-value-bind (condition read-count)
        (public-request
         (lambda (driver)
           (declare (ignore driver))
           (values :eof nil)))
      (ensure-true (typep condition 'http-connection-error))
      (ensure-equal :eof (http-connection-error-cause condition))
      (ensure-equal :http2-read (http-error-operation condition))
      (ensure-true (plusp read-count)))
    (multiple-value-bind (condition read-count)
        (public-request
         (lambda (driver)
           (declare (ignore driver))
           (values :would-block nil))
         0.03d0)
      (ensure-true (typep condition 'http-timeout))
      (ensure-equal :read (http-timeout-kind condition))
      (ensure-true (plusp read-count)))
    (multiple-value-bind (condition read-count)
        (public-request
         (lambda (driver)
           (declare (ignore driver))
           (values :data (octets)))
         0.03d0)
      (ensure-true (typep condition 'http-timeout))
      (ensure-equal :read (http-timeout-kind condition))
      (ensure-true (> read-count 1)))
    (dolist (case (list (list :eof :missing-close-notify)
                        (list '(:peer-alert 42) '(:peer-alert 42))))
      (destructuring-bind (reason expected-detail) case
        (multiple-value-bind (condition read-count)
            (public-request
             (lambda (driver)
               (declare (ignore driver))
               (error
                (make-condition 'cl-tls-kit:tls13-client-driver-error
                                :reason reason)))
             1d0)
          (ensure-true (typep condition 'http-protocol-error))
          (ensure-equal expected-detail
                        (http-protocol-error-detail condition))
          (ensure-true (plusp read-count)))))))
  (deftest tls-public-upgrader-application-record-read-boundaries
  (labels ((with-public-record-reader (input action)
             (let ((raw (%tls-server-test-controlled-stream input)))
               (%tls-server-test-call-with-public-client-fixture
                raw
                (lambda (actual-driver)
                  (declare (ignore actual-driver))
                  (values :would-block nil))
                (lambda (tls-stream driver transport written)
                  (declare (ignore written))
                  (ensure-true
                   (typep tls-stream 'http-kit/tls::http-tls-stream))
                  (ensure-true (functionp (getf transport :read)))
                  (funcall action raw driver (getf transport :read)))))))
    (let ((wire (octets 22 3 3 0 3 1 2 3)))
      (with-public-record-reader
       (subseq wire 0 4)
       (lambda (raw driver read-record)
         (ensure-equal :would-block (funcall read-record driver))
         (%tls-upgrader-controlled-stream-feed raw (subseq wire 4))
         (ensure-octets-equal wire (funcall read-record driver)))))
    (let* ((length (1+ cl-tls-kit:+tls-plaintext-limit+))
           (header
             (octets 23 3 3 (ldb (byte 8 8) length)
                     (ldb (byte 8 0) length))))
      (with-public-record-reader
       header
       (lambda (raw driver read-record)
         (declare (ignore raw))
         (let ((condition
                 (%tls-server-test-condition
                  (lambda () (funcall read-record driver)))))
           (ensure-equal :record-overflow
                         (http-protocol-error-detail condition))))))
    (with-public-record-reader
     (octets)
     (lambda (raw driver read-record)
       (%tls-upgrader-controlled-stream-mark-eof raw)
       (ensure-equal nil (funcall read-record driver))))
    (with-public-record-reader
     (octets 22 3 3)
     (lambda (raw driver read-record)
       (%tls-upgrader-controlled-stream-mark-eof raw)
       (let ((condition
               (%tls-server-test-condition
                (lambda () (funcall read-record driver)))))
         (ensure-equal :truncated-record
                       (http-protocol-error-detail condition)))))
    (with-public-record-reader
     (octets 22 3 3 0 3 1)
     (lambda (raw driver read-record)
       (%tls-upgrader-controlled-stream-mark-eof raw)
       (let ((condition
               (%tls-server-test-condition
                (lambda () (funcall read-record driver)))))
         (ensure-equal :truncated-record
                       (http-protocol-error-detail condition)))))))
  (progn
  (defclass %tls-upgrader-controlled-stream (binary-test-stream)
    ((eof-p :initform nil
            :accessor %tls-upgrader-controlled-stream-eof-p)))

  (defmethod sb-gray:stream-listen ((stream %tls-upgrader-controlled-stream))
    (or (%tls-upgrader-controlled-stream-eof-p stream)
        (< (http-kit/test-core::binary-test-input-position stream)
           (length (http-kit/test-core::binary-test-input stream)))))

  (defun %tls-server-test-controlled-stream (octets)
    (make-instance
     '%tls-upgrader-controlled-stream
     :input (make-array (length octets)
                        :element-type '(unsigned-byte 8)
                        :initial-contents octets
                        :adjustable t
                        :fill-pointer t)))

  (defun %tls-upgrader-controlled-stream-feed (stream octets)
    (loop for octet across octets
          do (vector-push-extend
              octet (http-kit/test-core::binary-test-input stream)))
    stream)

  (defun %tls-upgrader-controlled-stream-mark-eof (stream)
    (setf (%tls-upgrader-controlled-stream-eof-p stream) t)
    stream)

  (defun %tls-server-test-call-with-public-client-fixture
      (raw read-function action)
    (let ((driver nil)
          (captured-transport nil)
          (written nil)
          (tls-stream nil)
          (upgrader
            (make-http-tls-upgrader :verify :optional
                                    :trust-anchors #()
                                    :alpn-protocols '("h2")))
          (original-maker (fdefinition 'cl-tls-kit:make-tls13-client-driver))
          (original-connect
            (fdefinition 'cl-tls-kit:tls13-client-driver-connect))
          (original-read
            (fdefinition 'cl-tls-kit:tls13-client-driver-read-application-data))
          (original-write
            (fdefinition 'cl-tls-kit:tls13-client-driver-write)))
      (unwind-protect
           (progn
             (setf (fdefinition 'cl-tls-kit:make-tls13-client-driver)
                   (lambda (&rest arguments &key transport &allow-other-keys)
                     (setf captured-transport transport
                           driver (apply original-maker arguments)))
                   (fdefinition 'cl-tls-kit:tls13-client-driver-connect)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     nil)
                   (fdefinition
                    'cl-tls-kit:tls13-client-driver-read-application-data)
                   (lambda (actual-driver)
                     (funcall read-function actual-driver))
                   (fdefinition 'cl-tls-kit:tls13-client-driver-write)
                   (lambda (actual-driver bytes)
                     (declare (ignore actual-driver))
                     (push (copy-seq bytes) written)))
             (setf tls-stream
                   (funcall
                    upgrader raw
                    (http-request-uri
                     (make-http-request :method "GET"
                                        :uri "https://example.test/"))
                    :timeout 1d0))
             (funcall action tls-stream driver captured-transport
                      (nreverse written)))
        (when tls-stream
          (close tls-stream :abort t))
        (setf (fdefinition 'cl-tls-kit:make-tls13-client-driver) original-maker
              (fdefinition 'cl-tls-kit:tls13-client-driver-connect)
              original-connect
              (fdefinition
               'cl-tls-kit:tls13-client-driver-read-application-data)
              original-read
              (fdefinition 'cl-tls-kit:tls13-client-driver-write)
              original-write))))
)
  (deftest tls-fd-transport-preserves-partial-record-across-read-slices
  (flet ((call-with-cleanups (body &rest cleanup-thunks)
           (let ((body-values nil)
                 (body-condition nil)
                 (cleanup-conditions nil))
             (unwind-protect
                  (handler-case
                      (setf body-values
                            (multiple-value-list (funcall body)))
                    (error (condition)
                      (setf body-condition condition)))
               (dolist (cleanup cleanup-thunks)
                 (handler-case
                     (funcall cleanup)
                   (error (condition)
                     (push condition cleanup-conditions)))))
             (cond
               (body-condition
                (error body-condition))
               (cleanup-conditions
                (error "TLS test cleanup failed: ~{~A~^; ~}"
                       (nreverse
                        (mapcar (function princ-to-string)
                                cleanup-conditions))))
               (t
                (values-list body-values))))))
    (let ((body-condition nil)
          (cleanup-condition nil)
          (cleanup-count 0))
      (handler-case
          (call-with-cleanups
           (lambda () (error "primary TLS test failure"))
           (lambda ()
             (incf cleanup-count)
             (error "first cleanup failure"))
           (lambda ()
             (incf cleanup-count)
             (error "second cleanup failure")))
        (error (condition)
          (setf body-condition condition)))
      (ensure-equal 2 cleanup-count)
      (ensure-true
       (search "primary TLS test failure"
               (princ-to-string body-condition)))
      (ensure-false
       (search "cleanup failure"
               (princ-to-string body-condition)))
      (setf cleanup-count 0)
      (handler-case
          (call-with-cleanups
           (lambda () (values :success 42))
           (lambda ()
             (incf cleanup-count)
             (error "first cleanup failure"))
           (lambda ()
             (incf cleanup-count)
             (error "second cleanup failure")))
        (error (condition)
          (setf cleanup-condition condition)))
      (ensure-equal 2 cleanup-count)
      (ensure-true
       (search "first cleanup failure"
               (princ-to-string cleanup-condition)))
      (ensure-true
       (search "second cleanup failure"
               (princ-to-string cleanup-condition))))
    (let* ((listener
             (make-instance (quote sb-bsd-sockets:inet-socket)
                            :type :stream
                            :protocol :tcp))
           (port nil)
           (client-socket nil)
           (client-stream nil)
           (accepted-socket nil)
           (server-stream nil)
           (server-thread nil)
           (server-error nil)
           (transport nil)
           (read-function nil)
           (enable-application-read nil)
           (record-wire (octets 22 3 3 0 4 1 2 3 4))
           (first-write-ready (sb-thread:make-semaphore :count 0))
           (send-rest (sb-thread:make-semaphore :count 0))
           (rest-write-ready (sb-thread:make-semaphore :count 0))
           (server-done (sb-thread:make-semaphore :count 0)))
      (call-with-cleanups
       (lambda ()
         (progn
           (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen listener 1)
           (setf port
                 (nth-value 1 (sb-bsd-sockets:socket-name listener))
                 client-socket
                 (make-instance (quote sb-bsd-sockets:inet-socket)
                                :type :stream
                                :protocol :tcp))
           (sb-bsd-sockets:socket-connect
            client-socket #(127 0 0 1) port)
           (setf client-stream
                 (sb-bsd-sockets:socket-make-stream
                  client-socket
                  :input t
                  :output t
                  :element-type (quote (unsigned-byte 8))
                  :buffering :none)
                 server-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (progn
                               (setf accepted-socket
                                     (sb-bsd-sockets:socket-accept listener)
                                     server-stream
                                     (sb-bsd-sockets:socket-make-stream
                                      accepted-socket
                                      :input t
                                      :output t
                                      :element-type
                                      (quote (unsigned-byte 8))
                                      :buffering :none))
                               (write-sequence
                                (subseq record-wire 0 4) server-stream)
                               (finish-output server-stream)
                               (sb-thread:signal-semaphore first-write-ready)
                               (unless
                                   (sb-thread:wait-on-semaphore
                                    send-rest :timeout 5)
                                 (error
                                  "The TLS test timed out before sending the remainder."))
                               (write-sequence
                                (subseq record-wire 4) server-stream)
                               (finish-output server-stream)
                               (sb-thread:signal-semaphore rest-write-ready))
                           (error (condition)
                             (setf server-error condition)))
                      (sb-thread:signal-semaphore server-done)))))
           (setf transport (http-kit/tls::%tls-transport client-stream)
                 read-function (getf transport :read)
                 enable-application-read
                 (getf transport :enable-application-read))
           (funcall enable-application-read)
           (ensure-true
            (sb-thread:wait-on-semaphore first-write-ready :timeout 5))
           (ensure-equal :would-block (funcall read-function nil))
           (sb-thread:signal-semaphore send-rest)
           (ensure-true
            (sb-thread:wait-on-semaphore rest-write-ready :timeout 5))
           (ensure-equal record-wire (funcall read-function nil))
           (ensure-true
            (sb-thread:wait-on-semaphore server-done :timeout 5))
           (sb-thread:join-thread server-thread)
           (setf server-thread nil)
           (ensure-false server-error)))
       (lambda ()
         (when server-thread
           (sb-thread:signal-semaphore send-rest)))
       (lambda ()
         (when client-stream
           (let ((stream-closed-p nil))
             (unwind-protect
                  (progn
                    (close client-stream :abort t)
                    (setf stream-closed-p t))
               (unless stream-closed-p
                 (when client-socket
                   (sb-bsd-sockets:socket-close client-socket)))))))
       (lambda ()
         (when (and client-socket (null client-stream))
           (sb-bsd-sockets:socket-close client-socket)))
       (lambda ()
         (when server-stream
           (let ((stream-closed-p nil))
             (unwind-protect
                  (progn
                    (close server-stream :abort t)
                    (setf stream-closed-p t))
               (unless stream-closed-p
                 (when accepted-socket
                   (sb-bsd-sockets:socket-close accepted-socket)))))))
       (lambda ()
         (when (and accepted-socket (null server-stream))
           (sb-bsd-sockets:socket-close accepted-socket)))
       (lambda ()
         (sb-bsd-sockets:socket-close listener))
       (lambda ()
         (when server-thread
           (sb-thread:join-thread server-thread)))))))
  (deftest tls-client-driver-error-boundaries
  (let* ((raw (make-instance '%tls-server-test-stream :input (octets)))
         (stream (http-kit/tls::%make-http-tls-stream :client raw)))
    (unwind-protect
         (%tls-server-test-with-spy
          'cl-tls-kit:tls13-client-driver-read-application-data
          (lambda (driver)
            (declare (ignore driver))
            (values :invalid nil))
          (lambda ()
            (let ((condition
                    (%tls-server-test-condition
                     (lambda () (read-byte stream nil :eof)))))
              (ensure-equal :invalid-read-status
                            (http-protocol-error-detail condition)))))
      (close stream :abort t)))
  (dolist (case (list (list :eof :missing-close-notify)
                      (list (list :peer-alert 40) (list :peer-alert 40))
                      (list :unexpected :handshake-failed)))
    (destructuring-bind (reason expected) case
      (let* ((raw (make-instance '%tls-server-test-stream :input (octets)))
             (stream (http-kit/tls::%make-http-tls-stream :client raw))
             (driver-error
               (make-condition 'cl-tls-kit:tls13-client-driver-error
                               :reason reason)))
        (unwind-protect
             (%tls-server-test-with-spy
              'cl-tls-kit:tls13-client-driver-read-application-data
              (lambda (driver)
                (declare (ignore driver))
                (error driver-error))
              (lambda ()
                (let ((condition
                        (%tls-server-test-condition
                         (lambda () (read-byte stream nil :eof)))))
                  (ensure-equal expected
                                (http-protocol-error-detail condition)))))
          (close stream :abort t))))))

  (deftest tls-client-server-hello-framing-boundaries
    (multiple-value-bind (ciphertext-p pending)
        (http-kit/tls::%tls-client-track-server-hello (octets) (octets 1 0 0))
      (ensure-false ciphertext-p)
      (ensure-octets-equal #(1 0 0) pending))
    (multiple-value-bind (ciphertext-p pending)
        (http-kit/tls::%tls-client-track-server-hello
         (octets) (octets 1 0 0 3 4))
      (ensure-false ciphertext-p)
      (ensure-octets-equal #(1 0 0 3 4) pending))
    (let* ((wire
              (cl-tls-kit:encode-handshake
               2
               (cl-tls-kit:make-tls13-server-hello
                #x0303
                (make-array 32 :element-type '(unsigned-byte 8)
                                :initial-element 1)
                (octets)
                #x1301
                (list
                 (cl-tls-kit:make-tls-extension
                  51
                  (cl-tls-kit:encode-key-share-server-extension
                   #x001d
                   (make-array 32 :element-type '(unsigned-byte 8)
                                   :initial-element 2)))
                 (cl-tls-kit:make-tls-extension 43 #(3 4)))))))
      (multiple-value-bind (ciphertext-p pending)
          (http-kit/tls::%tls-client-track-server-hello (octets) wire)
        (ensure-true ciphertext-p)
        (ensure-equal 0 (length pending))))
    (let ((hrr-wire
            (cl-tls-kit:encode-handshake
             2
             (cl-tls-kit:make-tls13-hello-retry-request
              #x0303 #x0017
              (list (cl-tls-kit:make-tls-extension 43 (octets 3 4)))
              #x1301))))
      (multiple-value-bind (ciphertext-p pending)
          (http-kit/tls::%tls-client-track-server-hello (octets) hrr-wire)
        (ensure-false ciphertext-p)
        (ensure-equal 0 (length pending))))
    (multiple-value-bind (ciphertext-p pending)
        (http-kit/tls::%tls-client-track-server-hello
         (octets) (octets 2 0 0 1 0))
      (ensure-false ciphertext-p)
      (ensure-equal 0 (length pending))))

  (deftest tls-server-record-and-handshake-boundaries
    (let ((state
            (http-kit/tls::%make-http-tls-server-state
             :driver nil :provider nil
             :transport-stream
             (make-instance '%tls-server-test-stream :input (octets)))))
      (ensure-equal cl-tls-kit:+tls-content-type-handshake+
                    (http-kit/tls::%tls-server-content-type :handshake))
      (ensure-equal cl-tls-kit:+tls-content-type-alert+
                    (http-kit/tls::%tls-server-content-type :alert))
      (multiple-value-bind (ciphertext-p pending)
          (http-kit/tls::%tls-client-track-server-hello (octets) (octets 2 0 0 1 0))
        (ensure-false ciphertext-p)
        (ensure-equal 0 (length pending)))
      (http-kit/tls::%tls-server-feed-handshake state (octets 1 0 0 5 7))
      (ensure-octets-equal #(1 0 0 5 7)
                           (http-kit/tls::http-tls-server-state-handshake-buffer state))
      (let ((condition
              (%tls-server-test-condition
               (lambda ()
                 (setf (http-kit/tls::http-tls-server-state-handshake-buffer state)
                       (make-array 0
                                   :element-type '(unsigned-byte 8)
                                   :adjustable t
                                   :fill-pointer 0))
                 (http-kit/tls::%tls-server-feed-handshake
                  state (octets 1 16 0 1))))))
        (ensure-equal :handshake-message-too-large
                      (http-protocol-error-detail condition))))
    (let* ((read-stream
             (make-instance '%tls-server-failure-test-stream
                            :input (octets) :fail-on :read))
           (write-stream
             (make-instance '%tls-server-failure-test-stream
                            :input (octets) :fail-on :write))
           (read-state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider nil :transport-stream read-stream))
           (write-state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider nil :transport-stream write-stream)))
      (ensure-equal :transport-error
                    (http-protocol-error-detail
                     (%tls-server-test-condition
                      (lambda () (http-kit/tls::%tls-server-read-record read-state)))))
      (ensure-equal :transport-error
                    (http-protocol-error-detail
                     (%tls-server-test-condition
                      (lambda ()
                        (http-kit/tls::%tls-server-write-record
                         write-state (octets 1))))))
      #+sbcl
      (let* ((stream
               (make-instance
                '%tls-server-failure-test-stream
                :input (octets)
                :fail-on
                (make-condition 'sb-bsd-sockets:operation-timeout-error)))
             (state
               (http-kit/tls::%make-http-tls-server-state
                :driver nil :provider nil :transport-stream stream))
             (condition
               (handler-case
                   (progn (http-kit/tls::%tls-server-read-record state) nil)
                 (http-timeout (condition) condition))))
        (ensure-true condition)
        (close stream :abort t))
      #+sbcl
      (let* ((stream
               (make-instance '%tls-server-failure-test-stream
                              :input (octets)
                              :fail-on :sbcl-timeout))
             (state
               (http-kit/tls::%make-http-tls-server-state
                :driver nil :provider nil :transport-stream stream))
             (condition
               (handler-case
                   (progn (http-kit/tls::%tls-server-read-record state) nil)
                 (http-timeout (condition) condition))))
        (ensure-true condition)
        (close stream :abort t))
      (let* ((stream
               (make-instance '%tls-server-test-stream
                              :input (octets 22 3 3 0)))
             (state
               (http-kit/tls::%make-http-tls-server-state
                :driver nil :provider nil :transport-stream stream
                :deadline 1
                :clock-function (lambda () 1)))
             (condition
               (handler-case
                   (progn (http-kit/tls::%tls-server-read-record state) nil)
                 (http-timeout (condition) condition)
                 (http-protocol-error (condition) condition))))
        (ensure-true (typep condition 'http-timeout))
        (close stream :abort t))
      (let* ((stream
               (make-instance '%tls-server-test-stream :input (octets)))
             (state
               (http-kit/tls::%make-http-tls-server-state
                :driver nil :provider nil :transport-stream stream
                :deadline 1
                :clock-function (lambda () 1)))
             (condition
               (handler-case
                   (progn (http-kit/tls::%tls-server-read-record state) nil)
                 (http-timeout (condition) condition)
                 (http-protocol-error (condition) condition))))
        (ensure-true (typep condition 'http-timeout))
        (close stream :abort t))
      (close read-stream :abort t)
      (close write-stream :abort t))
    (let* ((raw (make-instance '%tls-server-test-stream
                               :input (octets 22 3 3 64 1)))
           (state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider nil :transport-stream raw))
           (condition
             (%tls-server-test-condition
              (lambda () (http-kit/tls::%tls-server-read-next-record state)))))
      (ensure-equal :record-overflow (http-protocol-error-detail condition))
      (ensure-octets-equal #(21 3 3 0 2 2 22) (binary-test-output raw))
      (close raw :abort t)))
  (deftest tls-server-decrypt-and-handshake-failure-boundaries
    (let* ((timeout (make-condition 'http-timeout
                                    :message "TLS transport read expired."
                                    :operation :tls :kind :tls))
           (stream (make-instance '%tls-server-failure-test-stream
                                  :input (octets) :fail-on timeout))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider nil :transport-stream stream))
           (observed
             (handler-case
                 (progn (http-kit/tls::%tls-server-read-record state) nil)
               (http-timeout (condition) condition))))
      (ensure-true (eq timeout observed))
      (close stream :abort t))
    (let* ((protocol-error
             (make-condition 'http-protocol-error
                             :message "TLS record input rejected."
                             :operation :tls :detail :truncated-record))
           (stream (make-instance '%tls-server-failure-test-stream
                                  :input (octets) :fail-on protocol-error))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider nil :transport-stream stream)))
      (ensure-true
       (eq protocol-error
           (%tls-server-test-condition
            (lambda () (http-kit/tls::%tls-server-read-record state)))))
      (close stream :abort t))
    (let* ((timeout (make-condition 'http-timeout
                                    :message "TLS transport write expired."
                                    :operation :tls :kind :tls))
           (stream (make-instance '%tls-server-failure-test-stream
                                  :input (octets) :fail-on timeout))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider nil :transport-stream stream))
           (observed
             (handler-case
                 (progn
                   (http-kit/tls::%tls-server-write-record state (octets 1))
                   nil)
               (http-timeout (condition) condition))))
      (ensure-true (eq timeout observed))
      (close stream :abort t))
    (dolist (case '((nil nil) (t nil) (t t)))
      (destructuring-bind (early-data-p handshake-complete-p) case
        (let* ((stream (make-instance '%tls-server-test-stream :input (octets)))
               (state (http-kit/tls::%make-http-tls-server-state
                       :driver nil :provider nil :transport-stream stream))
               (condition nil))
          (setf (http-kit/tls::http-tls-server-state-early-data-offered-p state)
                early-data-p
                (http-kit/tls::http-tls-server-state-handshake-complete-p state)
                handshake-complete-p)
          (when handshake-complete-p
            (setf (http-kit/tls::http-tls-server-state-write-state state)
                  :write-state))
          (%tls-server-test-with-spy
           'cl-tls-kit:encrypt-tls13-traffic-record
           (lambda (plaintext traffic-state)
             (declare (ignore plaintext traffic-state))
             #(21 3 3 0 2 2 20))
           (lambda ()
             (%tls-server-test-with-spy
              'cl-tls-kit:decrypt-tls13-traffic-record
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (error "Injected TLS record authentication failure."))
              (lambda ()
                (setf condition
                      (%tls-server-test-condition
                       (lambda ()
                         (http-kit/tls::%tls-server-decrypt-record
                          state (octets 23 3 3 0 1 0)))))
                (ensure-equal (if (and early-data-p (not handshake-complete-p))
                                  :early-data-rejected
                                  :record-authentication-failed)
                              (http-protocol-error-detail condition))
                (ensure-octets-equal
                 (if (and early-data-p (not handshake-complete-p))
                     #(21 3 3 0 2 2 10)
                     #(21 3 3 0 2 2 20))
                 (binary-test-output stream))))))
          (close stream :abort t))))
    (let* ((stream (make-instance '%tls-server-test-stream :input (octets)))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider nil :transport-stream stream))
           (condition nil))
      (%tls-server-test-with-spy
       'cl-tls-kit:decrypt-tls13-traffic-record
       (lambda (&rest arguments)
         (declare (ignore arguments))
         (error 'cl-tls-kit:tls-record-overflow))
       (lambda ()
         (setf condition
               (%tls-server-test-condition
                (lambda ()
                  (http-kit/tls::%tls-server-decrypt-record
                   state (octets 23 3 3 0 1 0)))))
         (ensure-equal :record-overflow
                       (http-protocol-error-detail condition))
         (ensure-octets-equal #(21 3 3 0 2 2 22)
                              (binary-test-output stream))))
      (close stream :abort t))
    (let* ((stream (make-instance '%tls-server-test-stream :input (octets)))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider nil :transport-stream stream))
           (wire (make-array (+ 5 16 2 cl-tls-kit:+tls-plaintext-limit+)
                             :element-type '(unsigned-byte 8)
                             :initial-element 0))
           (condition nil))
      (%tls-server-test-with-spy
       'cl-tls-kit:decrypt-tls13-traffic-record
       (lambda (&rest arguments)
         (declare (ignore arguments))
         (cl-tls-kit:make-tls-plaintext
          cl-tls-kit:+tls-content-type-handshake+ (octets 1)))
       (lambda ()
         (setf condition
               (%tls-server-test-condition
                (lambda ()
                  (http-kit/tls::%tls-server-decrypt-record state wire))))
         (ensure-equal :record-overflow
                       (http-protocol-error-detail condition))
         (ensure-octets-equal #(21 3 3 0 2 2 22)
                              (binary-test-output stream))))
      (close stream :abort t))

    (let* ((state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider nil :transport-stream nil
                   :suite #x1301))
           (calls 0)
           (condition nil))
      (%tls-server-test-with-spy
       'cl-tls-kit:make-tls13-traffic-state
       (lambda (&rest arguments)
         (declare (ignore arguments))
         (incf calls)
         (error "Injected TLS traffic-state setup failure."))
       (lambda ()
         (setf condition
               (%tls-server-test-condition
                (lambda ()
                  (http-kit/tls::%tls-server-traffic-state state (octets 1)))))
         (ensure-equal 1 calls)
         (ensure-equal :traffic-key-setup-failed
                       (http-protocol-error-detail condition)))))
    (let* ((timeout (make-condition 'http-timeout
                                    :message "TLS handshake expired."
                                    :operation :tls :kind :tls))
           (stream (make-instance '%tls-server-failure-test-stream
                                  :input (octets) :fail-on timeout))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider nil :transport-stream stream))
           (observed
             (handler-case
                 (progn (http-kit/tls::%tls-server-handshake state) nil)
               (http-timeout (condition) condition))))
      (ensure-true (eq timeout observed))
      (close stream :abort t))
    (let* ((state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider nil :transport-stream nil))
           (levels nil)
           (condition nil))
      (setf (http-kit/tls::http-tls-server-state-handshake-complete-p state) t)
      (%tls-server-test-with-spy
       'http-kit/tls::%tls-server-send-plaintext
       (lambda (&rest arguments)
         (push (second arguments) levels))
       (lambda ()
         (setf condition
               (%tls-server-test-condition
                (lambda () (http-kit/tls::%tls-server-handshake state))))))
      (ensure-equal :missing-alpn
                    (http-protocol-error-detail condition))
      (ensure-equal '(:application) levels)))

  (deftest tls-server-dispatch-state-boundaries
    (let* ((raw (make-instance '%tls-server-test-stream :input (octets)))
           (state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider nil :transport-stream raw)))
      (let ((condition
              (%tls-server-test-condition
               (lambda ()
                 (http-kit/tls::%tls-server-dispatch-plaintext
                  state (cl-tls-kit:make-tls-plaintext
                         cl-tls-kit:+tls-content-type-application-data+
                         (octets 1)))))))
        (ensure-equal :unexpected-application-data
                      (and condition
                           (http-protocol-error-detail condition)))
        (ensure-octets-equal #(21 3 3 0 2 2 10) (binary-test-output raw)))
      (setf (fill-pointer (binary-test-output raw)) 0
            (http-kit/tls::http-tls-server-state-early-data-offered-p state) t)
      (let ((condition
              (%tls-server-test-condition
               (lambda ()
                 (http-kit/tls::%tls-server-dispatch-plaintext
                  state (cl-tls-kit:make-tls-plaintext
                         cl-tls-kit:+tls-content-type-application-data+
                         (octets 1)))))))
        (ensure-equal :early-data-rejected
                      (and condition
                           (http-protocol-error-detail condition))))
      (setf (fill-pointer (binary-test-output raw)) 0
            (http-kit/tls::http-tls-server-state-handshake-complete-p state) t)
      (http-kit/tls::%tls-server-dispatch-plaintext
       state (cl-tls-kit:make-tls-plaintext
              cl-tls-kit:+tls-content-type-application-data+ (octets 4 5)))
      (ensure-octets-equal #(4 5)
                           (http-kit/tls::http-tls-server-state-input-buffer state))
      (ensure-equal 0 (http-kit/tls::http-tls-server-state-input-position state))
      (http-kit/tls::%tls-server-dispatch-plaintext
       state (cl-tls-kit:make-tls-plaintext
              cl-tls-kit:+tls-content-type-alert+ (octets 1 0)))
      (ensure-true (http-kit/tls::http-tls-server-state-close-notify-p state))
      (let ((condition
              (%tls-server-test-condition
               (lambda ()
                 (http-kit/tls::%tls-server-dispatch-plaintext
                  state (cl-tls-kit:make-tls-plaintext
                         cl-tls-kit:+tls-content-type-alert+ (octets 2 40)))))))
        (ensure-equal :peer-alert
                      (and condition
                           (http-protocol-error-detail condition))))
      (let ((condition
              (%tls-server-test-condition
               (lambda ()
                 (http-kit/tls::%tls-server-dispatch-plaintext
                  state (cl-tls-kit:make-tls-plaintext 20 (octets)))))))
        (ensure-equal :unsupported-record-type
                      (and condition
                           (http-protocol-error-detail condition))))
      (close raw :abort t))

    (dolist (wire '((22 3 4 0 0)
                    (20 3 3 0 1 0)
                    (99 3 3 0 0)))
      (let* ((raw (make-instance '%tls-server-test-stream :input (octets)))
             (state
               (http-kit/tls::%make-http-tls-server-state
                :driver nil :provider nil :transport-stream raw))
             (condition
               (%tls-server-test-condition
                (lambda ()
                  (http-kit/tls::%tls-server-process-record state (apply #'octets wire))))))
        (ensure-true condition)
        (close raw :abort t))))
  (deftest network-tls-server-transport-timeout-and-truncation-boundaries
    (let* ((timeout (make-condition (quote http-timeout)
                                    :message "TLS plaintext write expired."
                                    :operation :tls :kind :tls))
           (stream (make-instance (quote %tls-server-failure-test-stream)
                                  :input (octets) :fail-on timeout))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider nil :transport-stream stream))
           (observed
             (handler-case
                 (progn
                   (http-kit/tls::%tls-server-send-plaintext
                    state :initial cl-tls-kit:+tls-content-type-handshake+ (octets 1))
                   nil)
               (http-timeout (condition) condition))))
      (ensure-true (eq timeout observed))
      (close stream :abort t))
    (dolist (input (list (octets 22 3 3 0)
                         (octets 22 3 3 0 2 1)))
      (let* ((raw (make-instance (quote %tls-server-test-stream) :input input))
             (state (http-kit/tls::%make-http-tls-server-state
                     :driver nil :provider nil :transport-stream raw))
             (condition
               (%tls-server-test-condition
                (lambda () (http-kit/tls::%tls-server-read-next-record state)))))
        (ensure-equal :truncated-record (http-protocol-error-detail condition))
        (ensure-equal 0 (length (binary-test-output raw)))
        (close raw :abort t)))
    (let* ((raw (make-instance (quote %tls-server-test-stream) :input (octets)))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider nil :transport-stream raw))
           (condition
             (%tls-server-test-condition
              (lambda ()
                (http-kit/tls::%tls-server-process-record
                 state (octets 21 3 3 0 0))))))
      (ensure-equal :invalid-record (http-protocol-error-detail condition))
      (ensure-equal 0 (length (binary-test-output raw)))
      (close raw :abort t))
    (let* ((provider (cl-tls-kit:make-cl-crypto-kit-provider))
           (secret (make-array 32 :element-type (quote (unsigned-byte 8))
                               :initial-element 7))
           (timeout (make-condition (quote http-timeout)
                                    :message "TLS application write expired."
                                    :operation :tls :kind :tls))
           (raw (make-instance (quote %tls-server-failure-test-stream)
                               :input (octets) :fail-on timeout))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider provider :transport-stream raw :suite #x1301))
           (secure (make-instance (quote http-kit/tls::http-tls-stream)
                                  :driver state :transport-stream raw))
           (observed nil))
      (setf (http-kit/tls::http-tls-server-state-write-state state)
            (cl-tls-kit:make-tls13-traffic-state
             provider :sha256 :aes-128-gcm secret 16 12))
      (unwind-protect
           (progn
             (setf observed
                   (handler-case
                       (progn (write-sequence (octets 65 66) secure) nil)
                     (http-timeout (condition) condition)))
             (ensure-true (eq timeout observed))
             (ensure-equal 0 (length (binary-test-output raw))))
        (close secure :abort t)))
    (let* ((provider (cl-tls-kit:make-cl-crypto-kit-provider))
           (secret (make-array 32 :element-type (quote (unsigned-byte 8))
                               :initial-element 7))
           (raw (make-instance (quote %tls-server-failure-test-stream)
                               :input (octets) :fail-on :write))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider provider :transport-stream raw :suite #x1301))
           (secure (make-instance (quote http-kit/tls::http-tls-stream)
                                  :driver state :transport-stream raw))
           (condition nil))
      (setf (http-kit/tls::http-tls-server-state-write-state state)
            (cl-tls-kit:make-tls13-traffic-state
             provider :sha256 :aes-128-gcm secret 16 12))
      (unwind-protect
           (progn
             (setf condition
                   (%tls-server-test-condition
                    (lambda () (write-sequence (octets 67 68) secure))))
             (ensure-equal :transport-error
                           (http-protocol-error-detail condition))
             (ensure-equal 0 (length (binary-test-output raw))))
        (close secure :abort t)))
    (let* ((provider (cl-tls-kit:make-cl-crypto-kit-provider))
           (secret (make-array 32 :element-type (quote (unsigned-byte 8))
                               :initial-element 7))
           (raw (make-instance (quote %tls-server-test-stream) :input (octets)))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider provider :transport-stream raw)))
      (setf (http-kit/tls::http-tls-server-state-write-state state)
            (cl-tls-kit:make-tls13-traffic-state
             provider :sha256 :aes-128-gcm secret 16 12))
      (http-kit/tls::%tls-server-send-close-notify state)
      (let ((first-record (copy-seq (binary-test-output raw))))
        (ensure-true (plusp (length first-record)))
        (ensure-true (http-kit/tls::http-tls-server-state-close-sent-p state))
        (http-kit/tls::%tls-server-send-close-notify state)
        (ensure-octets-equal first-record (binary-test-output raw)))
      (close raw :abort t)))
  (deftest network-tls-server-invalid-driver-and-handshake-timeout-boundaries
    (let* ((provider (cl-tls-kit:make-cl-crypto-kit-provider))
           (secret (make-array 32 :element-type '(unsigned-byte 8)
                               :initial-element 7))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider provider :transport-stream nil)))
      (setf (http-kit/tls::http-tls-server-state-read-state state)
            (cl-tls-kit:make-tls13-traffic-state
             provider :sha256 :aes-128-gcm secret 16 12))
      (let* ((write-state
               (cl-tls-kit:make-tls13-traffic-state
                provider :sha256 :aes-128-gcm secret 16 12))
             (wire
               (cl-tls-kit:encrypt-tls13-traffic-record
                (cl-tls-kit:make-tls-plaintext
                 cl-tls-kit:+tls-content-type-application-data+ (octets 65))
                write-state))
             (condition
               (%tls-server-test-condition
                (lambda ()
                  (http-kit/tls::%tls-server-decrypt-record state wire)))))
        (progn
          (ensure-equal :invalid-driver-state
                        (http-protocol-error-detail condition))
          (let* ((stream
                   (make-instance '%tls-server-failure-test-stream
                                  :input (octets) :fail-on :read))
                 (state
                   (http-kit/tls::%make-http-tls-server-state
                    :driver nil :provider nil :transport-stream stream
                    :deadline 1 :clock-function (lambda () 1))))
            (unwind-protect
                 (let ((condition
                         (handler-case
                             (progn (http-kit/tls::%tls-server-read-record state) nil)
                           (http-timeout (condition) condition))))
                   (ensure-true (typep condition 'http-timeout))
                   (ensure-equal :tls (http-timeout-kind condition)))
              (close stream :abort t)))
          (let ((state
                  (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider nil :transport-stream nil)))
            (%tls-server-test-with-spy
             'cl-tls-kit:tls13-server-driver-step
             (lambda (&rest arguments)
               (declare (ignore arguments))
               (signal 'sb-ext:timeout))
             (lambda ()
               (let ((condition
                       (handler-case
                           (progn
                             (http-kit/tls::%tls-server-step-handshake-message
                              state (octets 2 0 0 0))
                             nil)
                         (http-timeout (condition) condition))))
                 (ensure-true (typep condition 'http-timeout))
                 (ensure-equal :tls (http-timeout-kind condition))))))
          (let* ((timeout
                   (make-condition 'http-timeout
                                   :message "TLS plaintext processing expired."
                                   :operation :tls :kind :tls))
                 (state
                   (http-kit/tls::%make-http-tls-server-state
                    :driver nil :provider nil :transport-stream nil)))
            (%tls-server-test-with-spy
             'cl-tls-kit:make-tls-plaintext
             (lambda (&rest arguments)
               (declare (ignore arguments))
               (error timeout))
             (lambda ()
               (let ((observed
                       (handler-case
                           (progn
                             (http-kit/tls::%tls-server-process-record
                              state (octets 22 3 3 0 0))
                             nil)
                         (http-timeout (condition) condition))))
                 (ensure-true (eq timeout observed)))))
          (let ((state
                  (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider nil :transport-stream nil)))
            (%tls-server-test-with-spy
             'cl-tls-kit:make-tls-plaintext
             (lambda (&rest arguments)
               (declare (ignore arguments))
               (signal 'sb-ext:timeout))
             (lambda ()
               (let ((condition
                       (handler-case
                           (progn
                             (http-kit/tls::%tls-server-process-record
                              state (octets 22 3 3 0 0))
                             nil)
                         (http-timeout (condition) condition))))
                 (ensure-true (typep condition 'http-timeout))
                 (ensure-equal :tls (http-timeout-kind condition))))))))))
    (let* ((material (%tls-server-test-material))
           (client-stream
             (make-instance '%tls-server-test-stream :input (octets)))
           (timeout
             (make-condition (quote http-timeout)
                             :message "TLS server handshake write expired."
                             :operation :tls :kind :tls))
           (server-stream
             (make-instance '%tls-server-failure-test-stream
                            :input (octets) :fail-on timeout))
           (observed nil))
      (unwind-protect
           (let* ((client-provider (cl-tls-kit:make-cl-crypto-kit-provider))
                  (client-driver
                    (cl-tls-kit:make-tls13-client-driver
                     :provider client-provider
                     :key-exchange (http-kit/tls::%tls-key-exchange)
                     :transport (http-kit/tls::%tls-transport client-stream)
                     :hostname "localhost"
                     :alpn '("h2" "http/1.1")
                     :supported-groups '(#x001d #x0017)
                     :ticket-store (cl-tls-kit:make-tls13-ticket-store)
                     :trust-anchors nil
                     :verify-signature #'crypto-kit:verify-signature))
                  (server-provider (cl-tls-kit:make-cl-crypto-kit-provider))
                  (server-driver
                    (cl-tls-kit:make-tls13-server-driver
                     :provider server-provider
                     :certificate-chain (getf material :certificate-chain)
                     :private-key (getf material :private-key)
                     :ticket-key-store
                     (cl-tls-kit:make-tls13-server-ticket-key-store)
                     :alpn-protocols '("h2" "http/1.1")))
                  (state
                    (http-kit/tls::%make-http-tls-server-state
                     :driver server-driver
                     :provider server-provider
                     :transport-stream server-stream)))
             (handler-case
                 (cl-tls-kit:tls13-client-driver-connect client-driver)
               (cl-tls-kit:tls13-client-driver-error () nil))
             (let ((client-wire (binary-test-output client-stream)))
               (ensure-true (> (length client-wire) 5))
               (ensure-equal cl-tls-kit:+tls-content-type-handshake+
                             (aref client-wire 0))
               (let ((record-length (+ (ash (aref client-wire 3) 8)
                                       (aref client-wire 4))))
                 (ensure-true (>= (length client-wire) (+ 5 record-length)))
                 (let ((hello-wire (subseq client-wire 5 (+ 5 record-length))))
                   (setf observed
                         (handler-case
                             (progn
                               (http-kit/tls::%tls-server-step-handshake-message
                                state hello-wire)
                               nil)
                           (http-timeout (condition) condition)))
                   (ensure-true (eq timeout observed))))))
        (close client-stream :abort t)
        (close server-stream :abort t)
        (%tls-server-test-remove-material material))))



  (deftest tls-server-wrapper-reports-driver-store-destroy-failure-after-handshake-error
           (let* ((material (%tls-server-test-material))
                  (raw (make-instance '%tls-server-test-stream :input (octets)))
                  (wrapper
                   (make-http-tls-server-wrapper
                    :certificate-chain (getf material :certificate-chain)
                    :private-key (getf material :private-key)
                    :alpn-protocols '("h2")))
                  (original-driver-maker
                   (fdefinition 'cl-tls-kit:make-tls13-server-driver))
                  (original-destroy
                   (fdefinition 'cl-tls-kit:destroy-tls13-server-ticket-key-store))
                  (destroy-condition
                   (make-condition 'simple-error
                                   :format-control "Injected ticket store destroy failure."
                                   :format-arguments nil))
                  (driver nil)
                  (driver-store nil)
                  (primary-condition nil)
                  (cleanup-warning nil))
             (unwind-protect
                 (progn
                   (progn
                     (%tls-server-test-with-spy
                      'cl-tls-kit:destroy-tls13-server-ticket-key-store
                      (lambda (store)
                        (if (eq store driver-store)
                            (error destroy-condition)
                          (funcall original-destroy store)))
                      (lambda ()
                        (handler-bind
                            ((http-kit/tls::tls-server-cleanup-warning
                              (lambda (warning)
                                (progn
                                  (setf cleanup-warning warning)
                                  (ensure-equal "TLS server driver cleanup failed."
                                                (format nil "~A" warning)))
                                (muffle-warning warning))))
                          (%tls-server-test-with-spy
                           'cl-tls-kit:make-tls13-server-driver
                           (lambda (&rest arguments)
                             (setf driver (apply original-driver-maker arguments)
                                   driver-store
                                   (cl-tls-kit:tls13-server-driver-ticket-key-store driver))
                             driver)
                           (lambda ()
                             (handler-case
                                 (funcall wrapper raw :timeout 2)
                               (error (condition)
                                      (setf primary-condition condition))))))))
                     (ensure-true (cl-tls-kit:tls13-server-ticket-key-store-p driver-store))
                     (ensure-true (typep primary-condition 'http-protocol-error))
                     (ensure-equal :handshake-failed
                                   (http-protocol-error-detail primary-condition))
                     (progn
                       (ensure-true cleanup-warning)
                       (let ((warning-output (make-string-output-stream)))
                         (close warning-output)
                         (%tls-server-test-with-spy
                          'cl-tls-kit:destroy-tls13-server-ticket-key-store
                          (lambda (store)
                            (if (eq store driver-store)
                                (error destroy-condition)
                              (funcall original-destroy store)))
                          (lambda ()
                            (let ((*error-output* warning-output))
                              (ensure-false
                               (http-kit/tls::%tls-server-cleanup-driver-close driver)))))))
                     (ensure-true
                      (eq destroy-condition
                          (http-kit/tls::tls-server-cleanup-warning-cause cleanup-warning)))
                     (ensure-false (open-stream-p raw)))
                   (when driver-store
                     (funcall original-destroy driver-store))
                   (when (open-stream-p raw)
                     (close raw :abort t))
                   (%tls-server-test-remove-material material)))))

  (deftest tls-server-wrapper-reports-driver-store-destroy-failure-after-close-notify-error
           (let* ((material (%tls-server-test-material))
                  (transport (make-instance '%tls-server-test-stream :input (octets)))
                  (wrapper
                   (make-http-tls-server-wrapper
                    :certificate-chain (getf material :certificate-chain)
                    :private-key (getf material :private-key)
                    :alpn-protocols '("h2")))
                  (close-condition
                   (make-condition 'simple-error
                                   :format-control "Injected TLS close_notify failure."
                                   :format-arguments nil))
                  (destroy-condition
                   (make-condition 'simple-error
                                   :format-control "Injected ticket store destroy failure."
                                   :format-arguments nil))
                  (original-destroy
                   (fdefinition 'cl-tls-kit:destroy-tls13-server-ticket-key-store))
                  (secure nil)
                  (driver nil)
                  (driver-store nil)
                  (primary-condition nil)
                  (cleanup-warning nil))
             (unwind-protect
                 (progn
                   (%tls-server-test-with-spy
                    'http-kit/tls::%tls-server-handshake
                    (lambda (state)
                      (setf (http-kit/tls::http-tls-server-state-handshake-complete-p state)
                            t)
                      state)
                    (lambda ()
                      (setf secure (funcall wrapper transport :timeout 2)
                            driver
                            (http-kit/tls::http-tls-server-state-driver
                             (http-kit/tls::%tls-stream-driver secure))
                            driver-store
                            (cl-tls-kit:tls13-server-driver-ticket-key-store driver))))
                   (ensure-true (cl-tls-kit:tls13-server-ticket-key-store-p driver-store))
                   (progn
                     (%tls-server-test-with-spy
                      'cl-tls-kit:destroy-tls13-server-ticket-key-store
                      (lambda (store)
                        (if (eq store driver-store)
                            (error destroy-condition)
                          (funcall original-destroy store)))
                      (lambda ()
                        (handler-bind
                            ((http-kit/tls::tls-server-cleanup-warning
                              (lambda (warning)
                                (setf cleanup-warning warning)
                                (muffle-warning warning))))
                          (%tls-server-test-with-spy
                           'http-kit/tls::%tls-server-send-close-notify
                           (lambda (state &key allow-closing deadline)
                             (declare (ignore state allow-closing deadline))
                             (error close-condition))
                           (lambda ()
                             (handler-case
                                 (close secure)
                               (error (condition)
                                      (setf primary-condition condition))))))))
                     (ensure-true (eq close-condition primary-condition))
                     (ensure-true cleanup-warning)
                     (ensure-true
                      (eq destroy-condition
                          (http-kit/tls::tls-server-cleanup-warning-cause cleanup-warning)))
                     (ensure-false
                      (cl-tls-kit:tls13-server-ticket-key-store-destroyed-p driver-store))
                     (ensure-false (open-stream-p transport)))
                   (when secure
                     (http-kit::%with-http-cleanup
                      (close secure :abort t)))
                   (when (and driver-store
                              (not (cl-tls-kit:tls13-server-ticket-key-store-destroyed-p driver-store)))
                     (funcall original-destroy driver-store))
                   (when (open-stream-p transport)
                     (close transport :abort t))
                   (%tls-server-test-remove-material material)))))
  (deftest tls-server-rejects-late-driver-send-after-handshake-deadline
  (let* ((now 9)
         (raw (make-instance '%tls-server-test-stream :input (octets)))
         (state
           (http-kit/tls::%make-http-tls-server-state
            :driver nil
            :provider nil
            :transport-stream raw
            :deadline 10
            :clock-function (lambda () now)))
         (driver-step-called-p nil)
         (condition nil))
    (unwind-protect
         (progn
           (%tls-server-test-with-spy
            'cl-tls-kit:tls13-server-driver-step
            (lambda (driver wire &key level)
              (declare (ignore wire level))
              (setf now 11
                    driver-step-called-p t)
              (values driver
                      (list (list :type :send
                                  :level :initial
                                  :content-type :handshake
                                  :wire (octets 68 65 74)))))
            (lambda ()
              (setf condition
                    (handler-case
                        (progn
                          (http-kit/tls::%tls-server-step-handshake-message
                           state (octets 3 0 0 0))
                          nil)
                      (http-timeout (caught) caught)))
              (ensure-true driver-step-called-p)
              (ensure-octets-equal (octets) (binary-test-output raw))
              (ensure-true (typep condition 'http-timeout)))))
      (close raw :abort t))))
  (progn
  (defclass %tls-server-backpressure-stream
      (sb-gray:fundamental-binary-output-stream)
      ((stream :initarg :stream :reader %tls-server-backpressure-stream-stream)
       (buffer :initform (make-array 0 :element-type '(unsigned-byte 8)
                                     :adjustable t :fill-pointer 0)
               :reader %tls-server-backpressure-stream-buffer)
       (aborted-p :initform nil :accessor %tls-server-backpressure-stream-aborted-p)
       (closed-p :initform nil :accessor %tls-server-backpressure-stream-closed-p)))
  (defclass %tls-server-backpressure-observer-stream
      (sb-gray:fundamental-binary-output-stream)
      ((stream :initarg :stream :reader %tls-server-backpressure-observer-stream-stream)
       (flush-entered :initarg :flush-entered
                      :reader %tls-server-backpressure-observer-stream-flush-entered)))

  (defmethod open-stream-p ((stream %tls-server-backpressure-stream))
    (and (not (%tls-server-backpressure-stream-closed-p stream))
         (open-stream-p (%tls-server-backpressure-stream-stream stream))))
  (defmethod open-stream-p ((stream %tls-server-backpressure-observer-stream))
    (open-stream-p (%tls-server-backpressure-observer-stream-stream stream)))

  (defmethod close ((stream %tls-server-backpressure-stream) &key abort)
    (setf (%tls-server-backpressure-stream-closed-p stream) t
          (%tls-server-backpressure-stream-aborted-p stream) abort)
    (close (%tls-server-backpressure-stream-stream stream) :abort abort))
  (defmethod close ((stream %tls-server-backpressure-observer-stream) &key abort)
    (close (%tls-server-backpressure-observer-stream-stream stream) :abort abort))

  (defmethod sb-gray:stream-write-byte
      ((stream %tls-server-backpressure-stream) byte)
    (vector-push-extend byte (%tls-server-backpressure-stream-buffer stream))
    byte)

  (defmethod sb-gray:stream-write-sequence
      ((stream %tls-server-backpressure-stream) sequence &optional (start 0) end)
    (let ((end (or end (length sequence)))
          (buffer (%tls-server-backpressure-stream-buffer stream)))
      (loop for index from start below end
            do (vector-push-extend (aref sequence index) buffer))
      sequence))
  (defmethod sb-gray:stream-write-sequence
      ((stream %tls-server-backpressure-observer-stream) sequence &optional (start 0) end)
    (write-sequence sequence
                    (%tls-server-backpressure-observer-stream-stream stream)
                    :start start
                    :end end))

  (defmethod sb-gray:stream-finish-output
      ((stream %tls-server-backpressure-stream))
    (let ((buffer (%tls-server-backpressure-stream-buffer stream)))
      (when (plusp (fill-pointer buffer))
        (write-sequence buffer (%tls-server-backpressure-stream-stream stream))
        (finish-output (%tls-server-backpressure-stream-stream stream))
        (setf (fill-pointer buffer) 0))))
  (defmethod sb-gray:stream-finish-output
      ((stream %tls-server-backpressure-observer-stream))
    (sb-thread:signal-semaphore
     (%tls-server-backpressure-observer-stream-flush-entered stream))
    (finish-output
     (%tls-server-backpressure-observer-stream-stream stream)))

  (defun %tls-server-backpressure-drain (socket output)
    (let ((buffer (make-array 4096
                              :element-type (quote (unsigned-byte 8)))))
      (loop
        (multiple-value-bind (received-buffer count)
            (handler-case
                (sb-bsd-sockets:socket-receive socket buffer (length buffer))
              (sb-bsd-sockets:socket-error ()
                (values nil nil)))
          (unless (and received-buffer count (plusp count))
            (return))
          (dotimes (index count)
            (vector-push-extend (aref received-buffer index) output))))
      output))

  (deftest tls-server-aborts-backpressured-record-flush-after-handshake-deadline
    (let* ((listener
             (make-instance 'sb-bsd-sockets:inet-socket
                            :type :stream
                            :protocol :tcp))
           (client-socket
             (make-instance 'sb-bsd-sockets:inet-socket
                            :type :stream
                            :protocol :tcp))
           (server-socket nil)
           (server-stream nil)
           (transport nil)
           (finish-entered (sb-thread:make-semaphore :count 0))
           (writer-done (sb-thread:make-semaphore :count 0))
           (deadline nil)
           (writer-result nil)
           (writer-error nil)
           (writer-thread nil)
           (received (make-array 0
                                 :element-type '(unsigned-byte 8)
                                 :adjustable t
                                 :fill-pointer 0))
           (buffer-full-p nil)
           (filled-bytes 0)
           (completed-before-peer-read nil)
           (writer-finished-p nil)
           (record (octets 22 3 3 0 3 68 65 74)))
      (unwind-protect
           (progn
             (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
             (sb-bsd-sockets:socket-listen listener 1)
             (multiple-value-bind (address port)
                 (sb-bsd-sockets:socket-name listener)
               (sb-bsd-sockets:socket-connect client-socket address port))
             (setf server-socket (sb-bsd-sockets:socket-accept listener)
                   (sb-bsd-sockets:sockopt-receive-buffer client-socket) 4096
                   (sb-bsd-sockets:sockopt-send-buffer server-socket) 4096)
             (let ((previous-mode
                     (sb-bsd-sockets:non-blocking-mode server-socket))
                   (payload (make-array 4096 :element-type '(unsigned-byte 8)
                                        :initial-element 0))
                   (server-fd
                     (sb-bsd-sockets:socket-file-descriptor server-socket)))
               (unwind-protect
                    (progn
                      (setf (sb-bsd-sockets:non-blocking-mode server-socket) t)
                      (loop repeat 16384
                            for sent =
                              (sb-bsd-sockets:socket-send
                               server-socket payload (length payload))
                            do (if (and sent (plusp sent))
                                   (incf filled-bytes sent)
                                   (unless
                                         (sb-sys:wait-until-fd-usable
                                          server-fd :output 0.05d0)
                                       (when
                                           (loop repeat 20
                                                 always
                                                 (and
                                                  (not
                                                   (sb-sys:wait-until-fd-usable
                                                    server-fd :output 0.05d0))
                                                  (let ((probe-sent
                                                         (sb-bsd-sockets:socket-send
                                                          server-socket payload 1)))
                                                    (if (and probe-sent
                                                             (plusp probe-sent))
                                                        (progn
                                                          (incf filled-bytes
                                                                probe-sent)
                                                          nil)
                                                        t))))
                                         (setf buffer-full-p t)
                                         (return)))))
                      (ensure-true buffer-full-p)
                      (ensure-true (plusp filled-bytes)))
                 (setf (sb-bsd-sockets:non-blocking-mode server-socket)
                       previous-mode)))
             (sb-bsd-sockets:socket-close listener)
             (setf listener nil
                   (sb-bsd-sockets:non-blocking-mode server-socket) t
                   server-stream
                   (sb-bsd-sockets:socket-make-stream
                    server-socket
                    :input nil
                    :output t
                    :element-type '(unsigned-byte 8)
                    :buffering :full)
                   server-socket nil
                   transport
                   (make-instance '%tls-server-backpressure-stream
                                  :stream
                                  (make-instance '%tls-server-backpressure-observer-stream
                                                 :stream server-stream
                                                 :flush-entered finish-entered)))
             (setf deadline
                   (+ (http-kit/tls::%tls-monotonic-time) 1.5d0))
             (let ((state
                     (http-kit/tls::%make-http-tls-server-state
                      :driver nil
                      :provider nil
                      :transport-stream transport
                      :deadline deadline)))
               (setf writer-thread
                     (sb-thread:make-thread
                      (lambda ()
                        (unwind-protect
                             (setf writer-result
                                   (handler-case
                                       (progn
                                         (http-kit/tls::%tls-server-write-record
                                          state record)
                                         :returned)
                                     (http-timeout () :timed-out)
                                     (error (condition)
                                       (setf writer-error condition)
                                       :error)))
                          (sb-thread:signal-semaphore writer-done)))
                      :name "TLS backpressured deadline writer")))
             (unless (sb-thread:wait-on-semaphore finish-entered :timeout 2)
               (error "TLS writer did not enter finish-output: ~S ~S"
                      writer-result
                      writer-error))
             (ensure-true
              (< (http-kit/tls::%tls-monotonic-time) deadline))
             (ensure-equal nil
              (when (sb-thread:wait-on-semaphore writer-done :timeout 0.05d0)
                (list writer-result writer-error)))
             (setf completed-before-peer-read
                   (sb-thread:wait-on-semaphore writer-done :timeout 2.5))
             (setf (sb-bsd-sockets:non-blocking-mode client-socket) t)
             (unless completed-before-peer-read
               (loop repeat 300
                     do (%tls-server-backpressure-drain client-socket received)
                        (setf writer-finished-p
                              (sb-thread:wait-on-semaphore writer-done
                                                           :timeout 0.001d0))
                        (when writer-finished-p
                          (return))
                        (sb-sys:wait-until-fd-usable
                         (sb-bsd-sockets:socket-file-descriptor client-socket)
                         :input 0.01d0))
               (unless writer-finished-p
                 (setf writer-finished-p
                       (sb-thread:wait-on-semaphore writer-done :timeout 2)))
               (ensure-true writer-finished-p))
             (%tls-server-backpressure-drain client-socket received)
             (ensure-equal nil (find-if-not (function zerop) received))
             (ensure-true completed-before-peer-read)
             (ensure-equal :timed-out writer-result)
             (ensure-false writer-error)
             (ensure-true
              (%tls-server-backpressure-stream-aborted-p transport))
             (ensure-false (open-stream-p transport)))
        (when (and writer-thread (sb-thread:thread-alive-p writer-thread))
          (when (and transport (open-stream-p transport))
            (close transport :abort t))
          (sb-thread:join-thread writer-thread :timeout 2 :default nil))
        (when (and transport (open-stream-p transport))
          (close transport :abort t))
        (when writer-thread
          (sb-thread:join-thread writer-thread :timeout 3 :default nil))
        (when (and server-stream (open-stream-p server-stream))
          (close server-stream :abort t))
        (when server-socket
          (sb-bsd-sockets:socket-close server-socket))
        (when client-socket
          (sb-bsd-sockets:socket-close client-socket))
        (when listener
          (sb-bsd-sockets:socket-close listener)))))
  (deftest tls-server-graceful-close-unblocks-backpressured-write
    (let* ((listener
             (make-instance 'sb-bsd-sockets:inet-socket
                            :type :stream
                            :protocol :tcp))
           (client-socket
             (make-instance 'sb-bsd-sockets:inet-socket
                            :type :stream
                            :protocol :tcp))
           (server-socket nil)
           (server-stream nil)
           (transport nil)
           (state nil)
           (secure nil)
           (provider (cl-tls-kit:make-cl-crypto-kit-provider))
           (write-state nil)
           (secret (make-array 32 :element-type '(unsigned-byte 8)
                               :initial-element 53))
           (key-material nil)
           (finish-entered (sb-thread:make-semaphore :count 0))
           (writer-done (sb-thread:make-semaphore :count 0))
           (closer-started (sb-thread:make-semaphore :count 0))
           (closer-done (sb-thread:make-semaphore :count 0))
           (writer-result nil)
           (writer-error nil)
           (close-result nil)
           (close-error nil)
           (writer-thread nil)
           (closer-thread nil)
           (buffer-full-p nil)
           (filled-bytes 0))
      (unwind-protect
           (progn
             (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
             (sb-bsd-sockets:socket-listen listener 1)
             (multiple-value-bind (address port)
                 (sb-bsd-sockets:socket-name listener)
               (sb-bsd-sockets:socket-connect client-socket address port))
             (setf server-socket (sb-bsd-sockets:socket-accept listener)
                   (sb-bsd-sockets:sockopt-receive-buffer client-socket) 4096
                   (sb-bsd-sockets:sockopt-send-buffer server-socket) 4096)
             (let ((previous-mode
                     (sb-bsd-sockets:non-blocking-mode server-socket))
                   (payload (make-array 4096 :element-type '(unsigned-byte 8)
                                        :initial-element 0))
                   (server-fd
                     (sb-bsd-sockets:socket-file-descriptor server-socket)))
               (unwind-protect
                    (progn
                      (setf (sb-bsd-sockets:non-blocking-mode server-socket) t)
                      (loop repeat 16384
                            for sent =
                              (sb-bsd-sockets:socket-send
                               server-socket payload (length payload))
                            do (if (and sent (plusp sent))
                                   (incf filled-bytes sent)
                                   (unless
                                         (sb-sys:wait-until-fd-usable
                                          server-fd :output 0.05d0)
                                       (when
                                           (loop repeat 20
                                                 always
                                                 (and
                                                  (not
                                                   (sb-sys:wait-until-fd-usable
                                                    server-fd :output 0.05d0))
                                                  (let ((probe-sent
                                                         (sb-bsd-sockets:socket-send
                                                          server-socket payload 1)))
                                                    (if (and probe-sent
                                                             (plusp probe-sent))
                                                        (progn
                                                          (incf filled-bytes
                                                                probe-sent)
                                                          nil)
                                                        t))))
                                         (setf buffer-full-p t)
                                         (return)))))
                      (ensure-true buffer-full-p)
                      (ensure-true (plusp filled-bytes)))
                 (setf (sb-bsd-sockets:non-blocking-mode server-socket)
                       previous-mode)))
             (sb-bsd-sockets:socket-close listener)
             (setf listener nil
                   server-stream
                   (sb-bsd-sockets:socket-make-stream
                    server-socket
                    :input nil
                    :output t
                    :element-type '(unsigned-byte 8)
                    :buffering :full)
                   transport
                   (make-instance '%tls-server-backpressure-stream
                                  :stream
                                  (make-instance '%tls-server-backpressure-observer-stream
                                                 :stream server-stream
                                                 :flush-entered finish-entered)))
             (http-kit/network::%network-record-stream-socket
              transport server-socket)
             (setf state
                   (http-kit/tls::%make-http-tls-server-state
                    :driver nil
                    :provider provider
                    :transport-stream transport)
                   secure (http-kit/tls::%make-http-tls-stream state transport))
             (setf write-state
                   (cl-tls-kit:make-tls13-traffic-state
                    provider :sha256 :aes-128-gcm secret 16 12)
                   key-material
                   (list (cl-tls-kit:tls13-traffic-state-secret write-state)
                         (cl-tls-kit:tls13-traffic-state-key write-state)
                         (cl-tls-kit:tls13-traffic-state-iv write-state)))
             (setf (http-kit/tls::http-tls-server-state-write-state state)
                   write-state
                   (http-kit/tls::http-tls-server-state-handshake-complete-p state)
                   t
                   (http-kit/tls::http-tls-server-state-selected-alpn state) "h2")
             (setf writer-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (handler-case
                               (progn
                                 (write-sequence (octets 65) secure)
                                 (setf writer-result :returned))
                             (error (condition)
                               (setf writer-error condition
                                     writer-result :error)))
                        (sb-thread:signal-semaphore writer-done)))))
             (unless (sb-thread:wait-on-semaphore finish-entered :timeout 3)
               (error "The TLS application write did not reach the blocked flush."))
             (ensure-false (sb-thread:wait-on-semaphore writer-done :timeout 0.1d0))
             (setf closer-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (handler-case
                               (progn
                                 (sb-thread:signal-semaphore closer-started)
                                 (close secure)
                                 (setf close-result :returned))
                             (error (condition)
                               (setf close-error condition)))
                        (sb-thread:signal-semaphore closer-done)))))
             (unless (sb-thread:wait-on-semaphore closer-started :timeout 2)
               (error "The TLS graceful close did not start."))
             (unless (sb-thread:wait-on-semaphore closer-done :timeout 3)
               (error "The TLS graceful close did not abort the blocked write."))
             (unless (sb-thread:wait-on-semaphore writer-done :timeout 3)
               (error "The TLS application writer did not leave the socket after shutdown."))
             (sb-thread:join-thread writer-thread :timeout 2 :default nil)
             (sb-thread:join-thread closer-thread :timeout 2 :default nil)
             (ensure-equal :returned close-result)
             (ensure-false close-error)
             (ensure-equal :error writer-result)
             (ensure-true (typep writer-error 'http-protocol-error))
             (ensure-equal :transport-error
                           (http-protocol-error-detail writer-error))
             (ensure-equal 1
                           (cl-tls-kit:tls13-traffic-state-sequence-number
                            write-state))
             (ensure-true
              (every (lambda (sequence)
                       (every (lambda (octet) (zerop octet)) sequence))
                     key-material))
             (ensure-false (open-stream-p secure))
             (ensure-true (%tls-server-backpressure-stream-aborted-p transport)))
        (when server-socket
          (http-kit/network::%network-shutdown-socket server-socket))
        (when writer-thread
          (sb-thread:join-thread writer-thread :timeout 3 :default nil))
        (when closer-thread
          (sb-thread:join-thread closer-thread :timeout 3 :default nil))
        (when (and
       (not (or (and writer-thread
                     (sb-thread:thread-alive-p writer-thread))
                (and closer-thread
                     (sb-thread:thread-alive-p closer-thread))))
       transport
       (open-stream-p transport))
  (close transport :abort t))
        (when (and
       (not (or (and writer-thread
                     (sb-thread:thread-alive-p writer-thread))
                (and closer-thread
                     (sb-thread:thread-alive-p closer-thread))))
       server-stream
       (open-stream-p server-stream))
  (close server-stream :abort t))
        (when (and server-socket (null server-stream))
          (sb-bsd-sockets:socket-close server-socket))
        (when client-socket
          (sb-bsd-sockets:socket-close client-socket))
        (when listener
          (sb-bsd-sockets:socket-close listener))
        (when (and
       state
       (not (or (and writer-thread
                     (sb-thread:thread-alive-p writer-thread))
                (and closer-thread
                     (sb-thread:thread-alive-p closer-thread)))))
  (http-kit/tls::%tls-server-clear-traffic-states state)))))
  (deftest tls-server-close-notify-times-out-waiting-for-write-lock
  (let* ((transport
           (make-instance (quote %tls-server-test-stream) :input (octets)))
         (state
           (http-kit/tls::%make-http-tls-server-state
            :driver nil :provider nil :transport-stream transport))
         (lock (http-kit/tls::http-tls-server-state-write-lock state))
         (lock-held (sb-thread:make-semaphore :count 0))
         (release-lock (sb-thread:make-semaphore :count 0))
         (holder nil))
    (unwind-protect
         (progn
           (setf holder
                 (sb-thread:make-thread
                  (lambda ()
                    (sb-thread:grab-mutex lock)
                    (unwind-protect
                         (progn
                           (sb-thread:signal-semaphore lock-held)
                           (sb-thread:wait-on-semaphore release-lock :timeout 5))
                      (sb-thread:release-mutex lock)))))
           (unless (sb-thread:wait-on-semaphore lock-held :timeout 2)
             (error "The TLS writer lock was not acquired."))
           (let ((condition
                   (handler-case
                       (progn
                         (http-kit/tls::%tls-server-send-close-notify
                          state :allow-closing t
                          :deadline (+ (http-kit/tls::%tls-monotonic-time)
                                       0.02d0))
                         nil)
                     (http-timeout (condition) condition))))
             (ensure-true (typep condition (quote http-timeout)))))
      (sb-thread:signal-semaphore release-lock)
      (when holder
        (sb-thread:join-thread holder :timeout 2 :default nil))
      (when (open-stream-p transport)
        (close transport :abort t)))))
  (deftest tls-server-close-returns-before-held-operation-and-defers-zeroization
    (let* ((transport
             (make-instance '%tls-server-test-stream :input (octets)))
           (provider (cl-tls-kit:make-cl-crypto-kit-provider))
           (secret (make-array 32 :element-type '(unsigned-byte 8)
                               :initial-element 61))
           (traffic-state
             (cl-tls-kit:make-tls13-traffic-state
              provider :sha256 :aes-128-gcm secret 16 12))
           (state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider provider :transport-stream transport))
           (secure (http-kit/tls::%make-http-tls-stream state transport))
           (key-material
             (list (cl-tls-kit:tls13-traffic-state-secret traffic-state)
                   (cl-tls-kit:tls13-traffic-state-key traffic-state)
                   (cl-tls-kit:tls13-traffic-state-iv traffic-state)))
           (operation-entered (sb-thread:make-semaphore :count 0))
           (release-operation (sb-thread:make-semaphore :count 0))
           (operation-done (sb-thread:make-semaphore :count 0))
           (close-done (sb-thread:make-semaphore :count 0))
           (operation-thread nil)
           (close-thread nil)
           (operation-error nil)
           (close-result nil)
           (close-error nil))
      (setf (http-kit/tls::http-tls-server-state-write-state state)
            traffic-state)
      (unwind-protect
           (progn
             (setf operation-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (handler-case
                               (http-kit/tls::%tls-server-call-with-direction-lock
                                state :write
                                (lambda ()
                                  (sb-thread:signal-semaphore operation-entered)
                                  (unless (sb-thread:wait-on-semaphore
                                           release-operation :timeout 5)
                                    (error "The TLS close test did not release the operation."))))
                             (error (condition)
                               (setf operation-error condition)))
                        (sb-thread:signal-semaphore operation-done)))))
             (unless (sb-thread:wait-on-semaphore operation-entered :timeout 2)
               (error "The TLS operation did not acquire the write lock."))
             (setf close-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (handler-case
                               (progn
                                 (close secure :abort t)
                                 (setf close-result :returned))
                             (error (condition)
                               (setf close-error condition)))
                        (sb-thread:signal-semaphore close-done)))))
             (ensure-true
              (sb-thread:wait-on-semaphore close-done :timeout 2.5d0))
             (ensure-equal :returned close-result)
             (ensure-false close-error)
             (ensure-true
              (some (lambda (octets)
                      (some (lambda (byte) (not (zerop byte))) octets))
                    key-material))
             (sb-thread:signal-semaphore release-operation)
             (ensure-true
              (sb-thread:wait-on-semaphore operation-done :timeout 2))
             (sb-thread:join-thread operation-thread :timeout 2 :default nil)
             (ensure-false operation-error)
             (ensure-true
              (every (lambda (octets) (every #'zerop octets)) key-material))
             (ensure-true (http-kit/tls::http-tls-server-state-closed-p state)))
        (sb-thread:signal-semaphore release-operation)
        (when operation-thread
          (sb-thread:join-thread operation-thread :timeout 3 :default nil))
        (when close-thread
          (sb-thread:join-thread close-thread :timeout 3 :default nil))
        (when (open-stream-p secure)
          (close secure :abort t))
        (when (open-stream-p transport)
          (close transport :abort t)))))
  (deftest tls-server-second-close-bounds-condition-wait
    (let* ((transport
             (make-instance '%tls-server-test-stream :input (octets)))
           (state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider nil :transport-stream transport))
           (secure (http-kit/tls::%make-http-tls-stream state transport))
           (second-close-started (sb-thread:make-semaphore :count 0))
           (second-close-done (sb-thread:make-semaphore :count 0))
           (second-close-thread nil)
           (second-close-result nil)
           (second-close-error nil))
      (ensure-true (http-kit/tls::%tls-server-begin-close state))
      (unwind-protect
           (progn
             (setf second-close-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (progn
                             (sb-thread:signal-semaphore second-close-started)
                             (handler-case
                                 (progn
                                   (close secure :abort t)
                                   (setf second-close-result :returned))
                               (error (condition)
                                 (setf second-close-error condition))))
                        (sb-thread:signal-semaphore second-close-done)))))
             (ensure-true
              (sb-thread:wait-on-semaphore second-close-started :timeout 2))
             (ensure-false
              (sb-thread:wait-on-semaphore second-close-done :timeout 0.01d0))
             (ensure-true
              (sb-thread:wait-on-semaphore second-close-done :timeout 2.5d0))
             (ensure-equal :returned second-close-result)
             (ensure-false second-close-error)
             (ensure-false
              (http-kit/tls::http-tls-server-state-closed-p state)))
        (sb-thread:with-mutex
            ((http-kit/tls::http-tls-server-state-lifecycle-lock state))
          (setf (http-kit/tls::http-tls-server-state-closed-p state) t)
          (sb-thread:condition-broadcast
           (http-kit/tls::http-tls-server-state-lifecycle-waitqueue state)))
        (when second-close-thread
          (sb-thread:join-thread second-close-thread :timeout 3 :default nil))
        (setf (http-kit/tls::%tls-stream-closed-p secure) t)
        (when (open-stream-p transport)
          (close transport :abort t)))))
  (deftest tls-server-second-close-expired-deadline-after-lock-acquire
    (let* ((state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider nil :transport-stream nil))
           (lifecycle-lock
             (http-kit/tls::http-tls-server-state-lifecycle-lock state))
           (clock-calls 0))
      (setf (http-kit/tls::http-tls-server-state-closing-p state) t
          (http-kit/tls::http-tls-server-state-close-deadline state) 100.0)
      (%tls-server-test-with-spy
       'http-kit/tls::%tls-monotonic-time
       (lambda ()
         (incf clock-calls)
         (case clock-calls
           (1 99.0)
           (2 99.0)
           (otherwise 101.0)))
       (lambda ()
         (multiple-value-bind (close-owner-p deadline)
             (http-kit/tls::%tls-server-begin-close state)
           (ensure-false close-owner-p)
           (ensure-equal 100.0 deadline)
           (ensure-equal 3 clock-calls))))
      (ensure-true (sb-thread:grab-mutex lifecycle-lock :timeout 0))
      (sb-thread:release-mutex lifecycle-lock)
      (ensure-false (http-kit/tls::http-tls-server-state-closed-p state))))
  (progn
    (deftest tls-server-mark-close-cleanup-pending-covers-closed-and-default-deadline
      (let ((state
              (http-kit/tls::%make-http-tls-server-state
               :driver nil :provider nil :transport-stream nil)))
        (setf (http-kit/tls::http-tls-server-state-closed-p state) t)
        (ensure-true (http-kit/tls::%tls-server-mark-close-cleanup-pending state))
        (ensure-false
         (http-kit/tls::http-tls-server-state-close-cleanup-pending-p state))
        (ensure-true (http-kit/tls::http-tls-server-state-closed-p state)))
      (let* ((provider (cl-tls-kit:make-cl-crypto-kit-provider))
             (secret (make-array 32 :element-type '(unsigned-byte 8)
                                 :initial-element 64))
             (traffic-state
               (cl-tls-kit:make-tls13-traffic-state
                provider :sha256 :aes-128-gcm secret 16 12))
             (state
               (http-kit/tls::%make-http-tls-server-state
                :driver nil :provider provider :transport-stream nil))
             (key-material
               (list (cl-tls-kit:tls13-traffic-state-secret traffic-state)
                     (cl-tls-kit:tls13-traffic-state-key traffic-state)
                     (cl-tls-kit:tls13-traffic-state-iv traffic-state))))
        (setf (http-kit/tls::http-tls-server-state-write-state state)
              traffic-state)
        (ensure-true (http-kit/tls::%tls-server-mark-close-cleanup-pending state))
        (ensure-true (http-kit/tls::http-tls-server-state-closed-p state))
        (ensure-false
         (http-kit/tls::http-tls-server-state-close-cleanup-pending-p state))
        (ensure-true
         (every (lambda (octets) (every #'zerop octets)) key-material))))
    (deftest tls-server-deferred-close-skips-after-close-wins-lock-race
      (let* ((state
               (http-kit/tls::%make-http-tls-server-state
                :driver nil :provider nil :transport-stream nil))
             (read-lock (http-kit/tls::http-tls-server-state-read-lock state))
             (write-lock (http-kit/tls::http-tls-server-state-write-lock state)))
        (setf (http-kit/tls::http-tls-server-state-close-cleanup-pending-p state)
              t)
        (%tls-server-test-with-spy
         'http-kit/tls::%tls-server-try-grab-direction-lock
         (let ((original
                 (symbol-function
                  'http-kit/tls::%tls-server-try-grab-direction-lock)))
           (lambda (lock)
             (let ((grabbed-p (funcall original lock)))
               (when (and grabbed-p (eq lock write-lock))
                 (setf (http-kit/tls::http-tls-server-state-closed-p state) t
                       (http-kit/tls::http-tls-server-state-close-cleanup-pending-p
                        state)
                       nil))
               grabbed-p)))
         (lambda ()
           (ensure-false
            (http-kit/tls::%tls-server-try-finish-deferred-close state))))
        (ensure-true (http-kit/tls::http-tls-server-state-closed-p state))
        (ensure-false
         (http-kit/tls::http-tls-server-state-close-cleanup-pending-p state))
        (setf (http-kit/tls::http-tls-server-state-closed-p state) nil
              (http-kit/tls::http-tls-server-state-close-cleanup-pending-p state)
              t)
        (%tls-server-test-with-spy
         'http-kit/tls::%tls-server-try-grab-direction-lock
         (let ((original
                 (symbol-function
                  'http-kit/tls::%tls-server-try-grab-direction-lock)))
           (lambda (lock)
             (let ((grabbed-p (funcall original lock)))
               (when (and grabbed-p (eq lock write-lock))
                 (setf (http-kit/tls::http-tls-server-state-closed-p state) t
                       (http-kit/tls::http-tls-server-state-close-cleanup-pending-p
                        state)
                       nil))
               grabbed-p)))
         (lambda ()
           (ensure-false
            (http-kit/tls::%tls-server-finish-deferred-close state))))
        (ensure-true (http-kit/tls::http-tls-server-state-closed-p state))
        (ensure-false
         (http-kit/tls::http-tls-server-state-close-cleanup-pending-p state))
        (setf (http-kit/tls::http-tls-server-state-closed-p state) nil
              (http-kit/tls::http-tls-server-state-close-cleanup-pending-p state)
              t)
        (%tls-server-test-with-spy
         'http-kit/tls::%tls-server-try-grab-direction-lock
         (let ((original
                 (symbol-function
                  'http-kit/tls::%tls-server-try-grab-direction-lock)))
           (lambda (lock)
             (let ((grabbed-p (funcall original lock)))
               (when (and grabbed-p (eq lock write-lock))
                 (setf (http-kit/tls::http-tls-server-state-closed-p state) t))
               grabbed-p)))
         (lambda ()
           (ensure-false
            (http-kit/tls::%tls-server-finish-deferred-close state))))
        (ensure-true (http-kit/tls::http-tls-server-state-closed-p state))
        (ensure-true
         (http-kit/tls::http-tls-server-state-close-cleanup-pending-p state))))
    (deftest tls-server-deferred-close-retries-after-interrupted-finalizer
      (let* ((provider (cl-tls-kit:make-cl-crypto-kit-provider))
             (secret (make-array 32 :element-type '(unsigned-byte 8)
                                 :initial-element 66))
             (traffic-state
               (cl-tls-kit:make-tls13-traffic-state
                provider :sha256 :aes-128-gcm secret 16 12))
             (state
               (http-kit/tls::%make-http-tls-server-state
                :driver nil :provider provider :transport-stream nil))
             (key-material
               (list (cl-tls-kit:tls13-traffic-state-secret traffic-state)
                     (cl-tls-kit:tls13-traffic-state-key traffic-state)
                     (cl-tls-kit:tls13-traffic-state-iv traffic-state))))
        (setf (http-kit/tls::http-tls-server-state-write-state state)
              traffic-state
              (http-kit/tls::http-tls-server-state-close-cleanup-pending-p state)
              t)
        (ensure-equal
         :interrupted
         (catch 'tls-server-deferred-close-interrupted
           (%tls-server-test-with-spy
            'http-kit/tls::%tls-server-close-state-unlocked
            (lambda (candidate-state driver)
              (declare (ignore candidate-state driver))
              (throw 'tls-server-deferred-close-interrupted :interrupted))
            (lambda ()
              (http-kit/tls::%tls-server-finish-deferred-close state)))))
        (ensure-false
         (http-kit/tls::http-tls-server-state-close-cleanup-running-p state))
        (ensure-true
         (http-kit/tls::http-tls-server-state-close-cleanup-pending-p state))
        (ensure-true (http-kit/tls::%tls-server-finish-deferred-close state))
        (ensure-true
         (every (lambda (octets) (every #'zerop octets)) key-material))
        (ensure-true (http-kit/tls::http-tls-server-state-closed-p state))))
  )
  (deftest tls-server-final-operation-release-finishes-deferred-close-once
    (let* ((transport
             (make-instance '%tls-server-test-stream :input (octets)))
           (provider (cl-tls-kit:make-cl-crypto-kit-provider))
           (read-secret (make-array 32 :element-type '(unsigned-byte 8)
                                    :initial-element 61))
           (write-secret (make-array 32 :element-type '(unsigned-byte 8)
                                     :initial-element 62))
           (read-state
             (cl-tls-kit:make-tls13-traffic-state
              provider :sha256 :aes-128-gcm read-secret 16 12))
           (write-state
             (cl-tls-kit:make-tls13-traffic-state
              provider :sha256 :aes-128-gcm write-secret 16 12))
           (state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider provider :transport-stream transport))
           (secure (http-kit/tls::%make-http-tls-stream state transport))
           (key-material
             (append
              (list (cl-tls-kit:tls13-traffic-state-secret read-state)
                    (cl-tls-kit:tls13-traffic-state-key read-state)
                    (cl-tls-kit:tls13-traffic-state-iv read-state))
              (list (cl-tls-kit:tls13-traffic-state-secret write-state)
                    (cl-tls-kit:tls13-traffic-state-key write-state)
                    (cl-tls-kit:tls13-traffic-state-iv write-state))))
           (read-entered (sb-thread:make-semaphore :count 0))
           (write-entered (sb-thread:make-semaphore :count 0))
           (release-read (sb-thread:make-semaphore :count 0))
           (release-write (sb-thread:make-semaphore :count 0))
           (read-done (sb-thread:make-semaphore :count 0))
           (write-done (sb-thread:make-semaphore :count 0))
           (close-done (sb-thread:make-semaphore :count 0))
           (finalizer-entered (sb-thread:make-semaphore :count 0))
           (resume-finalizer (sb-thread:make-semaphore :count 0))
           (read-thread nil)
           (write-thread nil)
           (close-thread nil)
           (read-error nil)
           (write-error nil)
           (close-error nil)
           (close-result nil)
           (close-state-calls 0)
           (original-close-state
             (fdefinition 'http-kit/tls::%tls-server-close-state-unlocked)))
      (setf (http-kit/tls::http-tls-server-state-read-state state) read-state
            (http-kit/tls::http-tls-server-state-write-state state) write-state)
      (unwind-protect
           (%tls-server-test-with-spy
            'http-kit/tls::%tls-server-close-state-unlocked
            (lambda (candidate-state driver)
              (incf close-state-calls)
              (sb-thread:signal-semaphore finalizer-entered)
              (unless (sb-thread:wait-on-semaphore resume-finalizer :timeout 5)
                (error "The TLS deferred finalizer did not resume."))
              (funcall original-close-state candidate-state driver))
            (lambda ()
              (setf read-thread
                    (sb-thread:make-thread
                     (lambda ()
                       (unwind-protect
                            (handler-case
                                (http-kit/tls::%tls-server-call-with-direction-lock
                                 state :read
                                 (lambda ()
                                   (sb-thread:signal-semaphore read-entered)
                                   (unless (sb-thread:wait-on-semaphore
                                            release-read :timeout 8)
                                     (error "The TLS read operation was not released."))))
                              (error (condition)
                                (setf read-error condition)))
                         (sb-thread:signal-semaphore read-done)))))
              (setf write-thread
                    (sb-thread:make-thread
                     (lambda ()
                       (unwind-protect
                            (handler-case
                                (http-kit/tls::%tls-server-call-with-direction-lock
                                 state :write
                                 (lambda ()
                                   (sb-thread:signal-semaphore write-entered)
                                   (unless (sb-thread:wait-on-semaphore
                                            release-write :timeout 8)
                                     (error "The TLS write operation was not released."))))
                              (error (condition)
                                (setf write-error condition)))
                         (sb-thread:signal-semaphore write-done)))))
              (unless (sb-thread:wait-on-semaphore read-entered :timeout 2)
                (error "The TLS read operation did not enter."))
              (unless (sb-thread:wait-on-semaphore write-entered :timeout 2)
                (error "The TLS write operation did not enter."))
              (setf close-thread
                    (sb-thread:make-thread
                     (lambda ()
                       (unwind-protect
                            (handler-case
                                (progn
                                  (close secure :abort t)
                                  (setf close-result :returned))
                              (error (condition)
                                (setf close-error condition)))
                         (sb-thread:signal-semaphore close-done)))))
              (ensure-true
               (sb-thread:wait-on-semaphore close-done :timeout 2.5d0))
              (ensure-equal :returned close-result)
              (ensure-false close-error)
              (ensure-true
               (http-kit/tls::http-tls-server-state-close-cleanup-pending-p state))
              (ensure-equal 0 close-state-calls)
              (ensure-true
               (some (lambda (octets)
                       (some (lambda (byte) (not (zerop byte))) octets))
                     key-material))
              (sb-thread:signal-semaphore release-read)
              (ensure-true (sb-thread:wait-on-semaphore read-done :timeout 2))
              (ensure-false read-error)
              (ensure-true
               (some (lambda (octets)
                       (some (lambda (byte) (not (zerop byte))) octets))
                     key-material))
              (sb-thread:signal-semaphore release-write)
              (ensure-true (sb-thread:wait-on-semaphore finalizer-entered :timeout 2))
              (ensure-false
               (http-kit/tls::%tls-server-finish-deferred-close state))
              (ensure-equal 1 close-state-calls)
              (ensure-true
               (some (lambda (octets)
                       (some (lambda (byte) (not (zerop byte))) octets))
                     key-material))
              (sb-thread:signal-semaphore resume-finalizer)
              (ensure-true (sb-thread:wait-on-semaphore write-done :timeout 2))
              (ensure-false write-error)
              (sb-thread:join-thread read-thread :timeout 2 :default nil)
              (sb-thread:join-thread write-thread :timeout 2 :default nil)
              (sb-thread:join-thread close-thread :timeout 2 :default nil)
              (ensure-true
               (every (lambda (octets) (every #'zerop octets)) key-material))
              (ensure-true (http-kit/tls::http-tls-server-state-closed-p state))
              (ensure-equal 1 close-state-calls)))
        (sb-thread:signal-semaphore release-read)
        (sb-thread:signal-semaphore release-write)
        (sb-thread:signal-semaphore resume-finalizer)
        (when read-thread
          (sb-thread:join-thread read-thread :timeout 3 :default nil))
        (when write-thread
          (sb-thread:join-thread write-thread :timeout 3 :default nil))
        (when close-thread
          (sb-thread:join-thread close-thread :timeout 3 :default nil))
        (when (open-stream-p secure)
          (close secure :abort t))
        (when (open-stream-p transport)
          (close transport :abort t)))))
  (deftest tls-server-final-operation-release-zeroizes-with-lifecycle-lock-held
    (let* ((transport
             (make-instance '%tls-server-test-stream :input (octets)))
           (provider (cl-tls-kit:make-cl-crypto-kit-provider))
           (secret (make-array 32 :element-type '(unsigned-byte 8)
                               :initial-element 65))
           (traffic-state
             (cl-tls-kit:make-tls13-traffic-state
              provider :sha256 :aes-128-gcm secret 16 12))
           (state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider provider :transport-stream transport))
           (secure (http-kit/tls::%make-http-tls-stream state transport))
           (lifecycle-lock
             (http-kit/tls::http-tls-server-state-lifecycle-lock state))
           (key-material
             (list (cl-tls-kit:tls13-traffic-state-secret traffic-state)
                   (cl-tls-kit:tls13-traffic-state-key traffic-state)
                   (cl-tls-kit:tls13-traffic-state-iv traffic-state)))
           (operation-entered (sb-thread:make-semaphore :count 0))
           (release-operation (sb-thread:make-semaphore :count 0))
           (operation-done (sb-thread:make-semaphore :count 0))
           (close-done (sb-thread:make-semaphore :count 0))
           (cleanup-done (sb-thread:make-semaphore :count 0))
           (original-close-state
             (fdefinition 'http-kit/tls::%tls-server-close-state-unlocked))
           (operation-thread nil)
           (close-thread nil)
           (lifecycle-locked-p nil)
           (close-result nil)
           (close-error nil)
           (operation-error nil))
      (setf (http-kit/tls::http-tls-server-state-write-state state)
            traffic-state)
      (unwind-protect
           (%tls-server-test-with-spy
            'http-kit/tls::%tls-server-close-state-unlocked
            (lambda (candidate-state driver)
              (prog1 (funcall original-close-state candidate-state driver)
                (sb-thread:signal-semaphore cleanup-done)))
            (lambda ()
              (setf operation-thread
                    (sb-thread:make-thread
                     (lambda ()
                       (unwind-protect
                            (handler-case
                                (http-kit/tls::%tls-server-call-with-direction-lock
                                 state :write
                                 (lambda ()
                                   (sb-thread:signal-semaphore operation-entered)
                                   (unless (sb-thread:wait-on-semaphore
                                            release-operation :timeout 5)
                                     (error "The TLS operation was not released."))))
                              (error (condition)
                                (setf operation-error condition)))
                         (sb-thread:signal-semaphore operation-done)))))
              (unless (sb-thread:wait-on-semaphore operation-entered :timeout 2)
                (error "The TLS operation did not acquire the write lock."))
              (setf close-thread
                    (sb-thread:make-thread
                     (lambda ()
                       (unwind-protect
                            (handler-case
                                (progn
                                  (close secure :abort t)
                                  (setf close-result :returned))
                              (error (condition)
                                (setf close-error condition)))
                         (sb-thread:signal-semaphore close-done)))))
              (ensure-true
               (sb-thread:wait-on-semaphore close-done :timeout 2.5d0))
              (ensure-equal :returned close-result)
              (ensure-false close-error)
              (ensure-true
               (some (lambda (octets)
                       (some (lambda (byte) (not (zerop byte))) octets))
                     key-material))
              (setf lifecycle-locked-p
                    (sb-thread:grab-mutex lifecycle-lock))
              (ensure-true lifecycle-locked-p)
              (sb-thread:signal-semaphore release-operation)
              (ensure-true
               (sb-thread:wait-on-semaphore cleanup-done :timeout 2.5d0))
              (ensure-true
               (every (lambda (octets) (every #'zerop octets)) key-material))
              (ensure-false
               (sb-thread:wait-on-semaphore operation-done :timeout 0.01d0))
              (sb-thread:release-mutex lifecycle-lock)
              (setf lifecycle-locked-p nil)
              (ensure-true
               (sb-thread:wait-on-semaphore operation-done :timeout 2.5d0))
              (sb-thread:join-thread operation-thread :timeout 2 :default nil)
              (ensure-false operation-error)
              (ensure-true
               (http-kit/tls::http-tls-server-state-closed-p state))))
        (sb-thread:signal-semaphore release-operation)
        (when (and lifecycle-locked-p
                   (sb-thread:holding-mutex-p lifecycle-lock))
          (sb-thread:release-mutex lifecycle-lock))
        (when operation-thread
          (sb-thread:join-thread operation-thread :timeout 3 :default nil))
        (when close-thread
          (sb-thread:join-thread close-thread :timeout 3 :default nil))
        (when (open-stream-p secure)
          (close secure :abort t))
        (when (open-stream-p transport)
          (close transport :abort t)))))
  (deftest tls-server-rejected-operation-releases-final-lock-and-zeroizes
    (let* ((transport
             (make-instance '%tls-server-test-stream :input (octets)))
           (provider (cl-tls-kit:make-cl-crypto-kit-provider))
           (secret (make-array 32 :element-type '(unsigned-byte 8)
                               :initial-element 91))
           (traffic-state
             (cl-tls-kit:make-tls13-traffic-state
              provider :sha256 :aes-128-gcm secret 16 12))
           (state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider provider :transport-stream transport))
           (secure (http-kit/tls::%make-http-tls-stream state transport))
           (key-material
             (list (cl-tls-kit:tls13-traffic-state-secret traffic-state)
                   (cl-tls-kit:tls13-traffic-state-key traffic-state)
                   (cl-tls-kit:tls13-traffic-state-iv traffic-state)))
           (check-entered (sb-thread:make-semaphore :count 0))
           (release-check (sb-thread:make-semaphore :count 0))
           (operation-done (sb-thread:make-semaphore :count 0))
           (close-done (sb-thread:make-semaphore :count 0))
           (cleanup-done (sb-thread:make-semaphore :count 0))
           (original-check
             (fdefinition 'http-kit/tls::%tls-server-check-open-state))
           (original-close-state
             (fdefinition 'http-kit/tls::%tls-server-close-state-unlocked))
           (operation-thread nil)
           (close-thread nil)
           (operation-error nil)
           (close-error nil)
           (close-result nil))
      (setf (http-kit/tls::http-tls-server-state-write-state state)
            traffic-state)
      (unwind-protect
           (%tls-server-test-with-spy
            'http-kit/tls::%tls-server-check-open-state
            (lambda (&rest arguments)
              (sb-thread:signal-semaphore check-entered)
              (unless (sb-thread:wait-on-semaphore release-check :timeout 5)
                (error "The TLS open-state check was not released."))
              (apply original-check arguments))
            (lambda ()
              (%tls-server-test-with-spy
               'http-kit/tls::%tls-server-close-state-unlocked
               (lambda (candidate-state driver)
                 (prog1 (funcall original-close-state candidate-state driver)
                   (sb-thread:signal-semaphore cleanup-done)))
               (lambda ()
                 (setf operation-thread
                       (sb-thread:make-thread
                        (lambda ()
                          (unwind-protect
                               (handler-case
                                   (http-kit/tls::%tls-server-call-with-direction-lock
                                    state :write
                                    (lambda ()
                                      (error "A closing TLS operation reached its body.")))
                                 (error (condition)
                                   (setf operation-error condition)))
                            (sb-thread:signal-semaphore operation-done)))))
                 (ensure-true
                  (sb-thread:wait-on-semaphore check-entered :timeout 2))
                 (setf close-thread
                       (sb-thread:make-thread
                        (lambda ()
                          (unwind-protect
                               (handler-case
                                   (progn
                                     (close secure :abort t)
                                     (setf close-result :returned))
                                 (error (condition)
                                   (setf close-error condition)))
                            (sb-thread:signal-semaphore close-done)))))
                 (ensure-true
                  (sb-thread:wait-on-semaphore close-done :timeout 2.5d0))
                 (ensure-equal :returned close-result)
                 (ensure-false close-error)
                 (ensure-true
                  (http-kit/tls::http-tls-server-state-close-cleanup-pending-p
                   state))
                 (ensure-true
                  (some (lambda (octets)
                          (some (lambda (byte) (not (zerop byte))) octets))
                        key-material))
                 (sb-thread:signal-semaphore release-check)
                 (ensure-true
                  (sb-thread:wait-on-semaphore operation-done :timeout 2.5d0))
                 (sb-thread:join-thread operation-thread :timeout 2 :default nil)
                 (ensure-true (typep operation-error 'http-protocol-error))
                 (ensure-equal :closed
                               (http-protocol-error-detail operation-error))
                 (ensure-true
                  (sb-thread:wait-on-semaphore cleanup-done :timeout 2.5d0))
                 (ensure-true
                  (every (lambda (octets) (every #'zerop octets)) key-material))
                 (ensure-true
                  (http-kit/tls::http-tls-server-state-closed-p state))))))
        (sb-thread:signal-semaphore release-check)
        (when operation-thread
          (sb-thread:join-thread operation-thread :timeout 3 :default nil))
        (when close-thread
          (sb-thread:join-thread close-thread :timeout 3 :default nil))
        (dolist (octets key-material)
          (fill octets 0))
        (when (open-stream-p secure)
          (close secure :abort t))
        (when (open-stream-p transport)
          (close transport :abort t)))))
  (deftest tls-server-close-cleanup-closes-open-transport-after-notify-error
  (dolist (close-during-shutdown-p (list nil t))
    (let* ((transport
             (make-instance (quote %tls-server-test-stream) :input (octets)))
           (state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider nil :transport-stream transport))
           (secure (http-kit/tls::%make-http-tls-stream state transport)))
      (unwind-protect
           (%tls-server-test-with-spy
            (quote http-kit/tls::%tls-server-shutdown-transport)
            (lambda (candidate)
              (ensure-true (eq candidate transport))
              (when close-during-shutdown-p
                (close candidate :abort t))
              t)
            (lambda ()
              (%tls-server-test-with-spy
               (quote http-kit/tls::%tls-server-send-close-notify)
               (lambda (candidate &rest arguments)
                 (declare (ignore candidate arguments))
                 (error "Injected TLS close_notify failure."))
               (lambda ()
                 (let ((condition
                         (handler-case
                             (progn (close secure) nil)
                           (error (condition) condition))))
                   (ensure-true (typep condition (quote simple-error)))
                   (ensure-false (open-stream-p secure))
                   (ensure-false (open-stream-p transport)))))))
        (when (open-stream-p secure)
          (close secure :abort t))
        (when (open-stream-p transport)
          (close transport :abort t))))))

  (deftest tls-server-check-handshake-deadline-follows-handoff
    (let ((state
            (http-kit/tls::%make-http-tls-server-state
             :driver nil
             :provider nil
             :deadline 10
             :clock-function (lambda () 10))))
      (setf (http-kit/tls::http-tls-server-state-handshake-complete-p state) t)
      (let ((condition
              (handler-case
                  (progn
                    (http-kit/tls::%tls-server-check-handshake-deadline state)
                    nil)
                (http-timeout (condition) condition))))
        (ensure-true (typep condition 'http-timeout)))
      (setf (http-kit/tls::http-tls-server-state-handshake-deadline-active-p state) nil)
      (ensure-false
       (http-kit/tls::%tls-server-check-handshake-deadline state))))
  (deftest tls-server-does-not-send-alpn-alert-after-handshake-deadline
    (let* ((listener
             (make-instance (quote sb-bsd-sockets:inet-socket)
                            :type :stream
                            :protocol :tcp))
           (client-socket
             (make-instance (quote sb-bsd-sockets:inet-socket)
                            :type :stream
                            :protocol :tcp))
           (server-socket nil)
           (transport nil)
           (received (make-array 0
                                 :element-type (quote (unsigned-byte 8))
                                 :adjustable t
                                 :fill-pointer 0))
           (condition nil))
      (unwind-protect
           (progn
             (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
             (sb-bsd-sockets:socket-listen listener 1)
             (multiple-value-bind (address port)
                 (sb-bsd-sockets:socket-name listener)
               (sb-bsd-sockets:socket-connect client-socket address port))
             (setf server-socket (sb-bsd-sockets:socket-accept listener)
                   transport
                   (sb-bsd-sockets:socket-make-stream
                    server-socket
                    :input nil
                    :output t
                    :element-type (quote (unsigned-byte 8))
                    :buffering :full))
             (sb-bsd-sockets:socket-close listener)
             (setf listener nil
                   (sb-bsd-sockets:non-blocking-mode client-socket) t)
             (let ((state
                     (http-kit/tls::%make-http-tls-server-state
                      :driver nil
                      :provider nil
                      :transport-stream transport
                      :deadline 10
                      :clock-function (lambda () 10))))
               (setf (http-kit/tls::http-tls-server-state-handshake-complete-p state) t
                     (http-kit/tls::http-tls-server-state-write-state state) :write-state)
               (%tls-server-test-with-spy
                (quote cl-tls-kit:encrypt-tls13-traffic-record)
                (lambda (plaintext traffic-state)
                  (declare (ignore plaintext traffic-state))
                  #(23 3 3 0 2 2 120))
                (lambda ()
                  (setf condition
                        (handler-case
                            (progn
                              (http-kit/tls::%tls-server-handshake state)
                              nil)
                          (http-timeout (caught) caught)
                          (http-protocol-error (caught) caught)))))
               (ensure-true
                (sb-sys:wait-until-fd-usable
                 (sb-bsd-sockets:socket-file-descriptor client-socket)
                 :input 1d0))
               (%tls-server-backpressure-drain client-socket received)
               (ensure-equal 0 (length received))
               (ensure-true (typep condition (quote http-timeout)))))
        (when (and transport (open-stream-p transport))
          (close transport :abort t))
        (when (and server-socket (null transport))
          (sb-bsd-sockets:socket-close server-socket))
        (when client-socket
          (sb-bsd-sockets:socket-close client-socket))
        (when listener
          (sb-bsd-sockets:socket-close listener)))))
  (deftest tls-server-write-record-without-deadline
    (let* ((wire (octets 22 3 3 0 1 1))
           (raw (make-instance '%tls-server-test-stream :input (octets)))
           (state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider nil :transport-stream raw)))
      (unwind-protect
           (progn
             (http-kit/tls::%tls-server-write-record state wire)
             (ensure-octets-equal wire (binary-test-output raw)))
        (when (open-stream-p raw)
          (close raw :abort t)))))
  (deftest tls-server-write-record-aborts-after-write-crosses-deadline
    (let* ((clock-calls 0)
           (wire (octets 22 3 3 0 1 1))
           (raw (make-instance '%tls-server-test-stream :input (octets)))
           (transport
             (make-instance '%tls-server-backpressure-stream :stream raw))
           (state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil
              :provider nil
              :transport-stream transport
              :deadline 10
              :clock-function
              (lambda ()
                (prog1 (if (zerop clock-calls) 9 10)
                  (incf clock-calls))))))
      (unwind-protect
           (let ((condition
                   (handler-case
                       (progn
                         (http-kit/tls::%tls-server-write-record state wire)
                         nil)
                     (http-timeout (condition) condition))))
             (ensure-true (typep condition 'http-timeout))
             (ensure-equal 2 clock-calls)
             (ensure-octets-equal
              wire (%tls-server-backpressure-stream-buffer transport))
             (ensure-equal 0 (length (binary-test-output raw)))
             (ensure-true (%tls-server-backpressure-stream-aborted-p transport))
             (ensure-false (open-stream-p transport)))
        (when (open-stream-p transport)
          (close transport :abort t)))))
  (deftest tls-server-write-record-handles-socket-timeouts-by-handshake-phase
    (dolist (case '((nil :handshake-timeout)
                    (t :transport-error)))
      (let* ((raw
               (make-instance '%tls-server-failure-test-stream
                              :input (octets)
                              :fail-on
                              (make-condition
                               'sb-bsd-sockets:operation-timeout-error)))
             (state
               (http-kit/tls::%make-http-tls-server-state
                :driver nil :provider nil :transport-stream raw)))
        (when (first case)
          (setf (http-kit/tls::http-tls-server-state-handshake-complete-p state) t
                (http-kit/tls::http-tls-server-state-handshake-deadline-active-p state) nil))
        (unwind-protect
             (let ((condition
                     (handler-case
                         (progn
                           (http-kit/tls::%tls-server-write-record state (octets 1))
                           nil)
                       (http-timeout (condition) condition)
                       (http-protocol-error (condition) condition))))
               (if (eq (second case) :handshake-timeout)
                   (ensure-true (typep condition 'http-timeout))
                 (ensure-equal :transport-error
                               (http-protocol-error-detail condition)))
               (ensure-false (open-stream-p raw)))
          (when (open-stream-p raw)
            (close raw :abort t))))))
  (deftest tls-server-write-record-skips-abort-for-closed-transport
    (let* ((raw (make-instance '%tls-server-test-stream :input (octets)))
           (state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil
              :provider nil
              :transport-stream raw
              :deadline 10
              :clock-function (lambda () 10))))
      (close raw :abort t)
      (let ((condition
              (handler-case
                  (progn
                    (http-kit/tls::%tls-server-write-record state (octets 1))
                    nil)
                (http-timeout (condition) condition))))
        (ensure-true (typep condition 'http-timeout))
        (ensure-false (open-stream-p raw)))))
  (deftest tls-server-concurrent-close-rejects-stream-operations
    (let* ((raw (make-instance (quote %tls-server-test-stream) :input (octets)))
           (state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider nil :transport-stream raw))
           (secure (http-kit/tls::%make-http-tls-stream state raw))
           (notify-entered (sb-thread:make-semaphore :count 0))
           (release-notify (sb-thread:make-semaphore :count 0))
           (second-close-entered (sb-thread:make-semaphore :count 0))
           (first-close-done (sb-thread:make-semaphore :count 0))
           (second-close-done (sb-thread:make-semaphore :count 0))
           (original-notify
             (fdefinition (quote http-kit/tls::%tls-server-send-close-notify)))
           (original-begin-close
             (fdefinition (quote http-kit/tls::%tls-server-begin-close)))
           (first-close-result nil)
           (first-close-error nil)
           (second-close-result nil)
           (second-close-error nil)
           (first-thread nil)
           (second-thread nil))
      (unwind-protect
           (progn
             (setf (fdefinition (quote http-kit/tls::%tls-server-send-close-notify))
                   (lambda (candidate &rest arguments)
                     (sb-thread:signal-semaphore notify-entered)
                     (unless (sb-thread:wait-on-semaphore release-notify :timeout 5)
                       (error "The TLS close test did not release close_notify."))
                     (apply original-notify candidate arguments))
                   (fdefinition (quote http-kit/tls::%tls-server-begin-close))
                   (lambda (candidate)
                     (when (http-kit/tls::http-tls-server-state-closing-p candidate)
                       (sb-thread:signal-semaphore second-close-entered))
                     (funcall original-begin-close candidate)))
             (setf first-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (handler-case
                               (progn
                                 (close secure)
                                 (setf first-close-result :returned))
                             (error (condition)
                               (setf first-close-error condition)))
                        (sb-thread:signal-semaphore first-close-done)))))
             (unless (sb-thread:wait-on-semaphore notify-entered :timeout 2)
               (error "The first TLS close did not reach close_notify."))
             (let ((condition
                     (%tls-server-test-condition
                      (lambda () (write-sequence (octets 9) secure)))))
               (ensure-equal :closed (http-protocol-error-detail condition)))
             (setf second-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (handler-case
                               (progn
                                 (close secure)
                                 (setf second-close-result :returned))
                             (error (condition)
                               (setf second-close-error condition)))
                        (sb-thread:signal-semaphore second-close-done)))))
             (unless (sb-thread:wait-on-semaphore second-close-entered :timeout 2)
               (error "The second TLS close did not observe the closing state."))
             (ensure-false (sb-thread:wait-on-semaphore second-close-done :timeout 0.1))
             (sb-thread:signal-semaphore release-notify)
             (unless (sb-thread:wait-on-semaphore first-close-done :timeout 3)
               (error "The first TLS close did not finish."))
             (unless (sb-thread:wait-on-semaphore second-close-done :timeout 3)
               (error "The second TLS close did not finish."))
             (sb-thread:join-thread first-thread :timeout 2 :default nil)
             (sb-thread:join-thread second-thread :timeout 2 :default nil)
             (ensure-equal :returned first-close-result)
             (ensure-false first-close-error)
             (ensure-equal :returned second-close-result)
             (ensure-false second-close-error)
             (ensure-false (open-stream-p secure))
             (ensure-false (funcall original-begin-close state))
             (ensure-true (http-kit/tls::%tls-server-close-adapter secure state nil t))
             (let ((condition
                     (%tls-server-test-condition
                      (lambda () (read-byte secure)))))
               (ensure-equal :closed (http-protocol-error-detail condition))))
        (sb-thread:signal-semaphore release-notify)
        (when first-thread
          (sb-thread:join-thread first-thread :timeout 3 :default nil))
        (when second-thread
          (sb-thread:join-thread second-thread :timeout 3 :default nil))
        (setf (fdefinition (quote http-kit/tls::%tls-server-send-close-notify))
              original-notify
              (fdefinition (quote http-kit/tls::%tls-server-begin-close))
              original-begin-close)
        (when (open-stream-p raw)
          (close raw :abort t)))))

  (deftest tls-server-fill-returns-eof-after-close-notify
    (let* ((raw (make-instance (quote %tls-server-test-stream) :input (octets)))
           (state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider nil :transport-stream raw)))
      (setf (http-kit/tls::http-tls-server-state-close-notify-p state) t)
      (ensure-false (http-kit/tls::%tls-server-fill state))
      (ensure-equal :eof (http-kit/tls::%tls-server-read-byte state))
      (close raw :abort t)))

  (deftest tls-server-graceful-close-skips-already-closed-transport
    (let* ((raw (make-instance (quote %tls-server-test-stream) :input (octets)))
           (state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider nil :transport-stream raw))
           (secure (http-kit/tls::%make-http-tls-stream state raw)))
      (close raw :abort t)
      (close secure)
      (ensure-false (open-stream-p raw))
      (ensure-false (open-stream-p secure))
      (ensure-true (http-kit/tls::http-tls-server-state-closed-p state)))))
)

#+sbcl
(deftest tls-server-bounds-handshake-buffer-growth-for-one-byte-fragments
  (let* ((message-size (+ 4 (* 1024 1024)))
         (message
           (make-array message-size
                       :element-type (quote (unsigned-byte 8))
                       :initial-element 0))
         (fragment (make-array 1 :element-type (quote (unsigned-byte 8))))
         (state (http-kit/tls::%make-http-tls-server-state))
         (buffer (http-kit/tls::http-tls-server-state-handshake-buffer state))
         (last-capacity (array-total-size buffer))
         (growth-count 0)
         (estimated-copied-bytes 0))
    (setf (aref message 0) 1
          (aref message 1) #x10)
    (loop for position below (1- message-size)
          for buffer =
            (http-kit/tls::http-tls-server-state-handshake-buffer state)
          for buffered-before = (fill-pointer buffer)
          do (setf (aref fragment 0) (aref message position))
             (http-kit/tls::%tls-server-feed-handshake state fragment)
             (let* ((buffer
                      (http-kit/tls::http-tls-server-state-handshake-buffer
                       state))
                    (capacity (array-total-size buffer)))
               (unless (= capacity last-capacity)
                 (incf growth-count)
                 (incf estimated-copied-bytes buffered-before)
                 (setf last-capacity capacity)
                 (ensure-true (<= growth-count 20)))))
    (let ((buffer
            (http-kit/tls::http-tls-server-state-handshake-buffer state)))
      (ensure-equal (1- message-size) (fill-pointer buffer))
      (ensure-true (<= growth-count 20))
      (ensure-true
       (<= estimated-copied-bytes (* 2 (fill-pointer buffer)))))))
#+sbcl
(progn
  (defun %tls-server-test-traffic-state (provider seed)
    (let* ((secret
             (make-array 32
                         :element-type (quote (unsigned-byte 8))
                         :initial-element seed))
           (traffic-state
             (cl-tls-kit:make-tls13-traffic-state
              provider :sha256 :aes-128-gcm secret 16 12)))
      (values traffic-state
              (list (cl-tls-kit:tls13-traffic-state-secret traffic-state)
                    (cl-tls-kit:tls13-traffic-state-key traffic-state)
                    (cl-tls-kit:tls13-traffic-state-iv traffic-state)))))

  (defun %tls-server-test-zeroed-octets-p (sequences)
    (every (lambda (sequence)
             (and sequence
                  (every (lambda (octet) (zerop octet)) sequence)))
           sequences))

  (deftest tls-server-wrapper-zeroizes-adapter-traffic-states-on-timeout
    (let* ((material (%tls-server-test-material))
           (raw (make-instance (quote %tls-server-test-stream) :input (octets)))
           (wrapper
             (make-http-tls-server-wrapper
              :certificate-chain (getf material :certificate-chain)
              :private-key (getf material :private-key)
              :alpn-protocols (list "h2")))
           (timeout
             (make-condition (quote http-timeout)
                             :message "Injected handshake timeout."
                             :operation :tls
                             :kind :tls))
           (state nil)
           (secrets nil)
           (observed nil))
      (unwind-protect
           (%tls-server-test-with-spy
            (quote http-kit/tls::%tls-server-handshake)
            (lambda (candidate)
              (let* ((provider
                       (http-kit/tls::http-tls-server-state-provider candidate))
                     (read-state nil)
                     (read-secrets nil)
                     (write-state nil)
                     (write-secrets nil))
                (multiple-value-setq (read-state read-secrets)
                  (%tls-server-test-traffic-state provider 7))
                (multiple-value-setq (write-state write-secrets)
                  (%tls-server-test-traffic-state provider 11))
                (setf state candidate
                      (http-kit/tls::http-tls-server-state-read-state candidate)
                      read-state
                      (http-kit/tls::http-tls-server-state-write-state candidate)
                      write-state
                      secrets (append read-secrets write-secrets))
                (error timeout)))
            (lambda ()
              (setf observed
                    (handler-case
                        (progn (funcall wrapper raw :timeout 2) nil)
                      (http-timeout (condition) condition)))
              (ensure-true (eq timeout observed))
              (ensure-true state)
              (ensure-true (%tls-server-test-zeroed-octets-p secrets))
              (ensure-false (http-kit/tls::http-tls-server-state-read-state state))
              (ensure-false (http-kit/tls::http-tls-server-state-write-state state))
              (ensure-false (open-stream-p raw))))
        (when (open-stream-p raw)
          (close raw :abort t))
        (%tls-server-test-remove-material material)))))
#+sbcl
(deftest tls-server-wrapper-zeroizes-adapter-traffic-states-on-close
  (let* ((material (%tls-server-test-material))
         (transport (make-instance (quote %tls-server-test-stream)
                                   :input (octets)))
         (wrapper
           (make-http-tls-server-wrapper
            :certificate-chain (getf material :certificate-chain)
            :private-key (getf material :private-key)
            :alpn-protocols (list "h2")))
         (secure nil)
         (state nil)
         (secrets nil))
    (unwind-protect
         (%tls-server-test-with-spy
          (quote http-kit/tls::%tls-server-handshake)
          (lambda (candidate)
            (let* ((provider
                     (http-kit/tls::http-tls-server-state-provider candidate))
                   (read-state nil)
                   (read-secrets nil)
                   (write-state nil)
                   (write-secrets nil))
              (multiple-value-setq (read-state read-secrets)
                (%tls-server-test-traffic-state provider 13))
              (multiple-value-setq (write-state write-secrets)
                (%tls-server-test-traffic-state provider 17))
              (setf state candidate
                    (http-kit/tls::http-tls-server-state-read-state candidate)
                    read-state
                    (http-kit/tls::http-tls-server-state-write-state candidate)
                    write-state
                    (http-kit/tls::http-tls-server-state-handshake-complete-p
                     candidate)
                    t
                    (http-kit/tls::http-tls-server-state-selected-alpn candidate)
                    "h2"
                    secrets (append read-secrets write-secrets))
              candidate))
          (lambda ()
            (setf secure (funcall wrapper transport :timeout 2))
            (ensure-equal
             "h2" (http-kit/tls:http-tls-selected-alpn-protocol secure))
            (close secure)
            (ensure-false (open-stream-p secure))
            (ensure-true (%tls-server-test-zeroed-octets-p secrets))
            (ensure-false (http-kit/tls::http-tls-server-state-read-state state))
            (ensure-false (http-kit/tls::http-tls-server-state-write-state state))
            (ensure-true (plusp (length (binary-test-output transport))))
            (ensure-false (open-stream-p transport))))
      (when (and secure (open-stream-p secure))
        (close secure :abort t))
      (when (open-stream-p transport)
        (close transport :abort t))
      (%tls-server-test-remove-material material))))
#+sbcl
(deftest tls-server-replaces-adapter-traffic-state-before-releasing-old-secrets
  (let* ((provider (cl-tls-kit:make-cl-crypto-kit-provider))
         (state
           (http-kit/tls::%make-http-tls-server-state
            :driver nil :provider provider :suite #x1301))
         (old-state nil)
         (old-secrets nil)
         (new-state nil)
         (new-secrets nil))
    (multiple-value-setq (old-state old-secrets)
      (%tls-server-test-traffic-state provider 19))
    (multiple-value-setq (new-state new-secrets)
      (%tls-server-test-traffic-state provider 23))
    (setf (http-kit/tls::http-tls-server-state-read-state state) old-state)
    (%tls-server-test-with-spy
     (quote http-kit/tls::%tls-server-traffic-state)
     (lambda (&rest arguments)
       (declare (ignore arguments))
       new-state)
     (lambda ()
       (http-kit/tls::%tls-server-handle-events
        state
        (list
         (list :type :secret
               :direction :read
               :level :application
               :secret
               (cl-tls-kit:tls13-traffic-state-secret new-state))))))
    (ensure-true
     (eq new-state (http-kit/tls::http-tls-server-state-read-state state)))
    (ensure-true (%tls-server-test-zeroed-octets-p old-secrets))
    (ensure-false (%tls-server-test-zeroed-octets-p new-secrets))
    (let* ((invalid-secret
             (make-array 32
                         :element-type (quote (unsigned-byte 8))
                         :initial-element 29))
           (calls 0)
           (condition nil))
      (%tls-server-test-with-spy
       (quote http-kit/tls::%tls-server-traffic-state)
       (lambda (&rest arguments)
         (declare (ignore arguments))
         (incf calls)
         new-state)
       (lambda ()
         (setf condition
               (%tls-server-test-condition
                (lambda ()
                  (http-kit/tls::%tls-server-handle-events
                   state
                   (list
                    (list :type :secret
                          :direction :invalid
                          :level :application
                          :secret invalid-secret))))))))
      (ensure-equal 0 calls)
      (ensure-equal :invalid-driver-state
                    (http-protocol-error-detail condition))
      (ensure-true (%tls-server-test-zeroed-octets-p (list invalid-secret)))
      (ensure-true
       (eq new-state (http-kit/tls::http-tls-server-state-read-state state))))
    (setf (http-kit/tls::http-tls-server-state-write-state state) new-state)
    (%tls-server-test-with-spy
     (quote http-kit/tls::%tls-server-traffic-state)
     (lambda (&rest arguments)
       (declare (ignore arguments))
       new-state)
     (lambda ()
       (http-kit/tls::%tls-server-handle-events
        state
        (list (list :type :secret
                    :direction :read
                    :level :application
                    :secret
                    (cl-tls-kit:tls13-traffic-state-secret new-state))
              (list :type :secret
                    :direction :write
                    :level :application
                    :secret
                    (cl-tls-kit:tls13-traffic-state-secret new-state))))))
    (ensure-false (%tls-server-test-zeroed-octets-p new-secrets))
    (http-kit/tls::%tls-server-clear-traffic-state old-state)
    (http-kit/tls::%tls-server-clear-traffic-states state)
    (ensure-true (%tls-server-test-zeroed-octets-p new-secrets))
    (ensure-false (http-kit/tls::http-tls-server-state-read-state state))
    (ensure-false (http-kit/tls::http-tls-server-state-write-state state))
    (let ((failure-secret
            (make-array 32
                        :element-type (quote (unsigned-byte 8))
                        :initial-element 31))
          (failure-signaled-p nil))
      (%tls-server-test-with-spy
       (quote http-kit/tls::%tls-server-traffic-state)
       (lambda (&rest arguments)
         (declare (ignore arguments))
         (error "Injected TLS traffic-state setup failure."))
       (lambda ()
         (handler-case
             (http-kit/tls::%tls-server-handle-events
              state
              (list (list :type :secret
                          :direction :read
                          :level :application
                          :secret failure-secret)))
           (error ()
             (setf failure-signaled-p t)))))
      (ensure-true failure-signaled-p)
      (ensure-true (%tls-server-test-zeroed-octets-p (list failure-secret))))))
#+sbcl
(progn
  (deftest tls-server-zeroizes-unprocessed-secret-after-send-timeout
    (let* ((raw (make-instance '%tls-server-test-stream :input (octets)))
           (secret (make-array 32
                               :element-type '(unsigned-byte 8)
                               :initial-element 47))
           (state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil
              :provider nil
              :transport-stream raw
              :deadline 10
              :clock-function (lambda () 10)))
           (condition
             (handler-case
                 (progn
                   (http-kit/tls::%tls-server-handle-events
                    state
                    (list (list :type :send
                                :level :initial
                                :content-type :handshake
                                :wire (octets 1))
                          (list :type :secret
                                :direction :read
                                :level :application
                                :secret secret)))
                   nil)
               (http-timeout (caught) caught))))
      (ensure-true (typep condition 'http-timeout))
      (ensure-true (%tls-server-test-zeroed-octets-p (list secret)))
      (ensure-equal (octets) (binary-test-output raw))
      (close raw :abort t)))

  (deftest tls-server-serializes-write-and-close-traffic-state
    (let* ((provider (cl-tls-kit:make-cl-crypto-kit-provider))
           (state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil
              :provider provider
              :transport-stream
              (make-instance '%tls-server-test-stream :input (octets))))
           (raw (http-kit/tls::http-tls-server-state-transport-stream state))
           (tls-stream (http-kit/tls::%make-http-tls-stream state raw))
           (traffic-state nil)
           (key-material nil)
           (monitor-lock (sb-thread:make-mutex :name "TLS race test monitor"))
           (encrypt-entered (sb-thread:make-semaphore :count 0))
           (second-encrypt-entered (sb-thread:make-semaphore :count 0))
           (closer-started (sb-thread:make-semaphore :count 0))
           (release-first-encrypt (sb-thread:make-semaphore :count 0))
           (writer-done (sb-thread:make-semaphore :count 0))
           (closer-done (sb-thread:make-semaphore :count 0))
           (original-encrypt (fdefinition 'cl-tls-kit:encrypt-tls13-traffic-record))
           (encrypt-count 0)
           (observed-sequences nil)
           (zeroized-key-used-p nil)
           (overlapped-p nil)
           (writer-result nil)
           (writer-error nil)
           (close-result nil)
           (close-error nil)
           (writer-thread nil)
           (closer-thread nil))
      (multiple-value-setq (traffic-state key-material)
        (%tls-server-test-traffic-state provider 37))
      (setf (http-kit/tls::http-tls-server-state-write-state state) traffic-state
            (http-kit/tls::http-tls-server-state-handshake-complete-p state) t
            (http-kit/tls::http-tls-server-state-selected-alpn state) "h2")
      (unwind-protect
           (progn
             (setf (fdefinition 'cl-tls-kit:encrypt-tls13-traffic-record)
                   (lambda (plaintext candidate)
                     (let ((call-index
                             (sb-thread:with-mutex (monitor-lock)
                               (incf encrypt-count)
                               (push
                                (cl-tls-kit:tls13-traffic-state-sequence-number candidate)
                                observed-sequences)
                               (when (or (null (cl-tls-kit:tls13-traffic-state-key candidate))
                                         (%tls-server-test-zeroed-octets-p
                                          (list (cl-tls-kit:tls13-traffic-state-key candidate))))
                                 (setf zeroized-key-used-p t))
                               encrypt-count)))
                       (when (= call-index 1)
                         (sb-thread:signal-semaphore encrypt-entered)
                         (unless (sb-thread:wait-on-semaphore
                                  release-first-encrypt :timeout 4)
                           (error "The TLS race test writer was not released."))
                         (sb-thread:with-mutex (monitor-lock)
                           (when (%tls-server-test-zeroed-octets-p key-material)
                             (setf zeroized-key-used-p t))))
                       (when (= call-index 2)
                         (sb-thread:signal-semaphore second-encrypt-entered))
                       (funcall original-encrypt plaintext candidate))))
             (setf writer-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (handler-case
                               (progn
                                 (http-kit/tls::%tls-server-write-application-data
                                  state (octets 5) 0 1)
                                 (setf writer-result :returned))
                             (error (condition)
                               (setf writer-error condition)))
                        (sb-thread:signal-semaphore writer-done))
                    :name "TLS application writer")))
             (unless (sb-thread:wait-on-semaphore encrypt-entered :timeout 2)
               (error "The TLS race test writer did not enter encryption."))
             (setf closer-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (handler-case
                               (progn
                                 (sb-thread:signal-semaphore closer-started)
                                 (close tls-stream)
                                 (setf close-result :returned))
                             (error (condition)
                               (setf close-error condition)))
                        (sb-thread:signal-semaphore closer-done))
                    :name "TLS concurrent closer")))
             (unless (sb-thread:wait-on-semaphore closer-started :timeout 2)
               (error "The TLS race test closer did not start."))
             (setf overlapped-p
                   (sb-thread:wait-on-semaphore
                    second-encrypt-entered :timeout 0.05d0))
             (sb-thread:signal-semaphore release-first-encrypt)
             (unless (sb-thread:wait-on-semaphore writer-done :timeout 3)
               (error "The TLS race test writer did not finish."))
             (unless (or overlapped-p
                         (sb-thread:wait-on-semaphore
                          second-encrypt-entered :timeout 3))
               (error "The TLS race test closer did not send close_notify."))
             (unless (sb-thread:wait-on-semaphore closer-done :timeout 3)
               (error "The TLS race test closer did not finish."))
             (sb-thread:join-thread writer-thread :timeout 2 :default nil)
             (sb-thread:join-thread closer-thread :timeout 2 :default nil)
             (ensure-false overlapped-p)
             (ensure-equal (list 0 1) (reverse observed-sequences))
             (ensure-false zeroized-key-used-p)
             (ensure-equal :returned writer-result)
             (ensure-false writer-error)
             (ensure-equal :returned close-result)
             (ensure-false close-error)
             (ensure-true (%tls-server-test-zeroed-octets-p key-material))
             (ensure-false (open-stream-p tls-stream)))
        (sb-thread:signal-semaphore release-first-encrypt)
        (when writer-thread
          (sb-thread:join-thread writer-thread :timeout 3 :default nil))
        (when closer-thread
          (sb-thread:join-thread closer-thread :timeout 3 :default nil))
        (setf (fdefinition 'cl-tls-kit:encrypt-tls13-traffic-record)
              original-encrypt)
        (when (open-stream-p raw)
          (close raw :abort t))
        (http-kit/tls::%tls-server-clear-traffic-states state)))))

#+sbcl
(progn
  (deftest tls-server-deferred-close-try-helper-covers-finalizer-branches
    (let* ((state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider nil :transport-stream nil))
           (read-lock (http-kit/tls::http-tls-server-state-read-lock state))
           (write-lock (http-kit/tls::http-tls-server-state-write-lock state)))
      (ensure-false (http-kit/tls::%tls-server-try-finish-deferred-close state))
      (setf (http-kit/tls::http-tls-server-state-close-cleanup-pending-p state) t
            (http-kit/tls::http-tls-server-state-closed-p state) t)
      (ensure-false (http-kit/tls::%tls-server-try-finish-deferred-close state))
      (setf (http-kit/tls::http-tls-server-state-closed-p state) nil)
      (let ((lock-held (sb-thread:make-semaphore :count 0))
            (release-lock (sb-thread:make-semaphore :count 0))
            (holder nil))
        (unwind-protect
             (progn
               (setf holder
                     (sb-thread:make-thread
                      (lambda ()
                        (sb-thread:grab-mutex read-lock)
                        (unwind-protect
                             (progn
                               (sb-thread:signal-semaphore lock-held)
                               (unless
                                   (sb-thread:wait-on-semaphore
                                    release-lock :timeout 5)
                                 (error "The deferred-close test did not release its lock.")))
                          (sb-thread:release-mutex read-lock)))))
               (unless (sb-thread:wait-on-semaphore lock-held :timeout 2)
                 (error "The deferred-close read-lock holder did not start."))
               (ensure-false
                (http-kit/tls::%tls-server-try-finish-deferred-close state)))
          (sb-thread:signal-semaphore release-lock)
          (when holder
            (sb-thread:join-thread holder :timeout 2 :default nil))))
      (let ((lock-held (sb-thread:make-semaphore :count 0))
            (release-lock (sb-thread:make-semaphore :count 0))
            (holder nil))
        (unwind-protect
             (progn
               (setf holder
                     (sb-thread:make-thread
                      (lambda ()
                        (sb-thread:grab-mutex write-lock)
                        (unwind-protect
                             (progn
                               (sb-thread:signal-semaphore lock-held)
                               (unless
                                   (sb-thread:wait-on-semaphore
                                    release-lock :timeout 5)
                                 (error "The deferred-close test did not release its lock.")))
                          (sb-thread:release-mutex write-lock)))))
               (unless (sb-thread:wait-on-semaphore lock-held :timeout 2)
                 (error "The deferred-close write-lock holder did not start."))
               (ensure-false
                (http-kit/tls::%tls-server-try-finish-deferred-close state)))
          (sb-thread:signal-semaphore release-lock)
          (when holder
            (sb-thread:join-thread holder :timeout 2 :default nil))))
      (%tls-server-test-with-spy
       'http-kit/tls::%tls-server-try-grab-direction-lock
       (let ((original
               (symbol-function
                'http-kit/tls::%tls-server-try-grab-direction-lock)))
         (lambda (lock)
           (let ((grabbed-p (funcall original lock)))
             (when (and grabbed-p (eq lock write-lock))
                 (setf (http-kit/tls::http-tls-server-state-closed-p state) t))
             grabbed-p)))
       (lambda ()
         (ensure-false
          (http-kit/tls::%tls-server-try-finish-deferred-close state))))
      (ensure-true (http-kit/tls::http-tls-server-state-closed-p state))
      (ensure-true
       (http-kit/tls::http-tls-server-state-close-cleanup-pending-p state))
      (setf (http-kit/tls::http-tls-server-state-close-cleanup-pending-p state)
            nil))
    (let* ((provider (cl-tls-kit:make-cl-crypto-kit-provider))
           (secret (make-array 32 :element-type '(unsigned-byte 8)
                               :initial-element 63))
           (traffic-state
             (cl-tls-kit:make-tls13-traffic-state
              provider :sha256 :aes-128-gcm secret 16 12))
           (state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider provider :transport-stream nil))
           (key-material
             (list (cl-tls-kit:tls13-traffic-state-secret traffic-state)
                   (cl-tls-kit:tls13-traffic-state-key traffic-state)
                   (cl-tls-kit:tls13-traffic-state-iv traffic-state))))
      (setf (http-kit/tls::http-tls-server-state-write-state state) traffic-state
            (http-kit/tls::http-tls-server-state-close-cleanup-pending-p state) t)
      (ensure-true (http-kit/tls::%tls-server-try-finish-deferred-close state))
      (ensure-true
       (every (lambda (octets) (every #'zerop octets)) key-material))
      (ensure-true (http-kit/tls::http-tls-server-state-closed-p state)))
    (let ((state
            (http-kit/tls::%make-http-tls-server-state
             :driver :fake :provider nil :transport-stream nil)))
      (setf (http-kit/tls::http-tls-server-state-close-cleanup-pending-p state) t)
      (let ((warning-output (make-string-output-stream)))
        (close warning-output)
        (let ((*error-output* warning-output))
          (%tls-server-test-with-spy
           'cl-tls-kit:tls13-server-driver-close
           (lambda (driver)
             (declare (ignore driver))
             (error "Injected TLS driver close failure."))
           (lambda ()
             (ensure-true
              (http-kit/tls::%tls-server-try-finish-deferred-close state))))))
      (ensure-true (http-kit/tls::http-tls-server-state-closed-p state)))
    (let ((state
            (http-kit/tls::%make-http-tls-server-state
             :driver nil :provider nil :transport-stream nil)))
      (ensure-false (http-kit/tls::%tls-server-finish-deferred-close state))
      (setf (http-kit/tls::http-tls-server-state-close-cleanup-pending-p state) t
            (http-kit/tls::http-tls-server-state-closed-p state) t)
      (ensure-false (http-kit/tls::%tls-server-finish-deferred-close state)))
    (let ((state
            (http-kit/tls::%make-http-tls-server-state
             :driver :fake :provider nil :transport-stream nil)))
      (setf (http-kit/tls::http-tls-server-state-close-cleanup-pending-p state) t)
      (let ((warning-output (make-string-output-stream)))
        (close warning-output)
        (let ((*error-output* warning-output))
          (%tls-server-test-with-spy
           'cl-tls-kit:tls13-server-driver-close
           (lambda (driver)
             (declare (ignore driver))
             (error "Injected TLS driver close failure."))
           (lambda ()
             (ensure-true
              (http-kit/tls::%tls-server-finish-deferred-close state))))))
      (ensure-true (http-kit/tls::http-tls-server-state-closed-p state)))
    (let ((state
            (http-kit/tls::%make-http-tls-server-state
             :driver nil :provider nil :transport-stream nil)))
      (setf (http-kit/tls::http-tls-server-state-closing-p state) t)
      (multiple-value-bind (close-owner-p deadline)
          (http-kit/tls::%tls-server-begin-close state)
        (ensure-false close-owner-p)
        (ensure-true deadline)))
    (let* ((transport
             (make-instance '%tls-server-test-stream :input (octets)))
           (state
             (http-kit/tls::%make-http-tls-server-state
              :driver nil :provider nil :transport-stream transport))
           (secure (http-kit/tls::%make-http-tls-stream state transport))
           (close-state-calls 0))
      (%tls-server-test-with-spy
       'http-kit/tls::%tls-server-close-state-unlocked
       (lambda (candidate-state driver)
         (declare (ignore candidate-state driver))
         (incf close-state-calls)
         (when (= close-state-calls 1)
           (make-condition 'simple-error
                           :format-control "Injected TLS driver close failure."
                           :format-arguments nil)))
       (lambda ()
         (let ((condition
                 (handler-case
                     (progn (close secure :abort t) nil)
                   (simple-error (condition) condition))))
           (ensure-true (typep condition 'simple-error))
           (ensure-equal 2 close-state-calls)
           (ensure-true
            (http-kit/tls::http-tls-server-state-closed-p state))
           (ensure-false (open-stream-p secure)))))
      (when (open-stream-p transport)
        (close transport :abort t)))))
