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
     (error "Injected ticket key store construction failure."))
   (lambda ()
     (let ((condition
             (%tls-server-test-condition
              (lambda ()
                (make-http-tls-server-wrapper
                 :certificate-chain (list t) :private-key t)))))
       (ensure-equal :invalid-ticket-key-store
                     (http-protocol-error-detail condition)))))
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
               (lambda (driver)
                 (declare (ignore driver))
                 (case status
                   (:data (values :data (octets 42)))
                   (:eof (values :eof nil))
                   (:would-block (values :would-block nil))))))
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
                     (ensure-equal :would-block
                                   (http-protocol-error-detail condition)))))))
          (close stream :abort t)))))
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
                       (octets))
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
        (ensure-equal :invalid-driver-state
                      (http-protocol-error-detail condition))))
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


  )
