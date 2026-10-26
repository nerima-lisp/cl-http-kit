(defpackage #:http-kit/tls
  (:use #:cl)
  (:import-from #:http-kit
                #:http-deadline
                #:http-protocol-error
                #:http-timeout
                #:http-unsupported-feature
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

#+sbcl
(defun %tls-call-with-timeout (thunk remaining)
  (if remaining
      (handler-case
          (sb-ext:with-timeout remaining
            (funcall thunk))
        (sb-ext:timeout ()
          (%tls-timeout)))
      (funcall thunk)))

#-sbcl
(defun %tls-call-with-timeout (thunk remaining)
  (declare (ignore remaining))
  (funcall thunk))

(defun %call-with-tls-deadline (thunk deadline clock-function)
  (let ((remaining (and deadline (- deadline (funcall clock-function)))))
    (when (and remaining (<= remaining 0))
      (%tls-timeout))
    (let ((result (%tls-call-with-timeout thunk remaining)))
      (when (and deadline (>= (funcall clock-function) deadline))
        (%tls-timeout))
      result)))

(defun %tls-protocol-error (message &optional detail)
  (error 'http-protocol-error
         :message message
         :operation :tls
         :detail detail))

(defun %tls-unsupported (feature &optional detail)
  (error 'http-unsupported-feature
         :message "The requested TLS operation is not exposed by cl-tls-kit."
         :operation :tls
         :detail detail
         :feature feature))

(defun %valid-alpn-protocol-p (protocol)
  (and (stringp protocol)
       (<= 1 (length protocol) 255)
       (every (lambda (character)
                (<= 1 (char-code character) 127))
              protocol)))

(defun %validate-certificate-pair (certificate key role)
  (unless (or (and certificate key)
              (and (null certificate) (null key)))
    (%tls-protocol-error
     (format nil "TLS ~A certificates require both CERTIFICATE and KEY."
             role)
     (list :role role :required nil))))

(defun http-tls-selected-alpn-protocol (stream)
  "Return the ALPN protocol negotiated by the TLS peer."
  (unless (streamp stream)
    (%tls-protocol-error "TLS ALPN lookup requires a Lisp stream."
                         :invalid-stream))
  #+sbcl
  (if (typep stream 'http-tls-stream)
      (let ((driver (%tls-stream-driver stream)))
        (if (typep driver 'http-tls-server-state)
            (http-tls-server-state-selected-alpn driver)
            (cl-tls-kit:tls13-client-driver-negotiated-alpn driver)))
      (%tls-unsupported :tls-alpn-result))
  #-sbcl
  (%tls-unsupported :tls-alpn-result))

(defun %tls-read-exact (stream buffer)
  (loop with position = 0
        while (< position (length buffer))
        for next-position = (read-sequence buffer stream :start position)
        do (if (= next-position position)
               (return (if (zerop position) :eof :truncated))
               (setf position next-position))
        finally (return buffer)))

(defun %tls-read-record (stream &optional driver &key ciphertext-p)
  (declare (ignore driver))
  (let ((header (make-array 5 :element-type '(unsigned-byte 8))))
    (case (%tls-read-exact stream header)
      (:eof nil)
      (:truncated
       (%tls-protocol-error "The TLS transport ended in a record header."
                            :truncated-record))
      (otherwise
       (let* ((length (+ (ash (aref header 3) 8) (aref header 4)))
              (limit (if (and ciphertext-p
                               (= (aref header 0)
                                  cl-tls-kit:+tls-content-type-application-data+))
                         cl-tls-kit:+tls-ciphertext-limit+
                         cl-tls-kit:+tls-plaintext-limit+)))
         (when (> length limit)
           (%tls-protocol-error "The TLS record exceeds its length limit."
                                :record-overflow))
         (let ((body (make-array length :element-type '(unsigned-byte 8))))
           (when (member (%tls-read-exact stream body) '(:eof :truncated))
             (%tls-protocol-error
              "The TLS transport ended in a record body."
              :truncated-record))
           (concatenate '(vector (unsigned-byte 8)) header body)))))))

(progn
(defun %tls-client-track-server-hello (buffer fragment)
  (let ((pending (concatenate '(vector (unsigned-byte 8)) buffer fragment))
        (ciphertext-p nil))
    (loop
      (when (< (length pending) 4) (return))
      (let* ((body-length (+ (ash (aref pending 1) 16)
                             (ash (aref pending 2) 8)
                             (aref pending 3)))
             (wire-length (+ 4 body-length)))
        (when (> wire-length (length pending)) (return))
        (let ((wire (subseq pending 0 wire-length)))
          (setf pending (subseq pending wire-length))
          (when (= (aref wire 0) 2)
            (handler-case
                (when (typep (cl-tls-kit:decode-handshake wire)
                             'cl-tls-kit:tls13-server-hello)
                  (setf ciphertext-p t
                        pending (make-array 0 :element-type '(unsigned-byte 8)))
                  (return))
              (cl-tls-kit:tls13-decode-error () nil))))))
    (values ciphertext-p pending)))
(defun %tls-transport (stream)
  (let ((ciphertext-p nil)
        (application-read-p nil)
        (handshake-buffer (make-array 0 :element-type '(unsigned-byte 8)))
        (record-buffer (make-array 0 :element-type '(unsigned-byte 8)
                                   :adjustable t :fill-pointer 0)))
    (labels ((read-available-record ()
               (loop
                 (let ((filled (fill-pointer record-buffer)))
                   (when (>= filled 5)
                     (let* ((length (+ (ash (aref record-buffer 3) 8)
                                       (aref record-buffer 4)))
                            (limit
                              (if (and ciphertext-p
                                       (= (aref record-buffer 0)
                                          cl-tls-kit:+tls-content-type-application-data+))
                                  cl-tls-kit:+tls-ciphertext-limit+
                                  cl-tls-kit:+tls-plaintext-limit+))
                            (record-length (+ 5 length)))
                       (when (> length limit)
                         (%tls-protocol-error
                          "The TLS record exceeds its length limit."
                          :record-overflow))
                       (when (>= filled record-length)
                         (let ((wire (subseq record-buffer 0 record-length)))
                           (setf (fill-pointer record-buffer) 0)
                           (return (values wire :data)))))))
                 (unless (listen stream)
                   (return (values nil :would-block)))
                 (let ((byte (read-byte stream nil :eof)))
                   (when (eq byte :eof)
                     (if (zerop (fill-pointer record-buffer))
                         (return (values nil :eof))
                         (%tls-protocol-error
                          (if (< (fill-pointer record-buffer) 5)
                              "The TLS transport ended in a record header."
                              "The TLS transport ended in a record body.")
                          :truncated-record)))
                   (vector-push-extend byte record-buffer)))))
      (list :read
            (lambda (driver)
              (declare (ignore driver))
              (if application-read-p
                  (multiple-value-bind (wire status)
                      (read-available-record)
                    (case status
                      (:data wire)
                      (:would-block :would-block)
                      (:eof nil)))
                  (let ((wire (%tls-read-record
                               stream nil :ciphertext-p ciphertext-p)))
                    (when (and wire
                               (not ciphertext-p)
                               (= (aref wire 0)
                                  cl-tls-kit:+tls-content-type-handshake+))
                      (multiple-value-setq (ciphertext-p handshake-buffer)
                        (%tls-client-track-server-hello
                         handshake-buffer (subseq wire 5))))
                    wire)))
            :enable-application-read
            (lambda ()
              (setf application-read-p t))
            :write (lambda (driver bytes)
                     (declare (ignore driver))
                     (write-sequence bytes stream)
                     (finish-output stream))
            :close (lambda (driver)
                     (declare (ignore driver))
                     (close stream)))))))

(defun %tls-key-exchange ()
  (list
   :random #'crypto-kit:random-octets
   :generate
   (lambda (group)
     (case group
       (#x001d
        (let ((private (crypto-kit:random-octets 32)))
          (values private
                  (nth-value 0 (crypto-kit:x25519-base private)))))
       (#x0017
        (crypto-kit:p256-generate-keypair))
       (otherwise
        (%tls-unsupported :tls-key-exchange (list :group group)))))
   :shared-secret
   (lambda (group private peer-public)
     (case group
       (#x001d
        (multiple-value-bind (secret all-zero-p)
            (crypto-kit:x25519 private peer-public)
          (when all-zero-p
            (%tls-protocol-error
             "The TLS X25519 shared secret was the all-zero value."
             :invalid-shared-secret))
          secret))
       (#x0017
        (crypto-kit:p256-ecdh private peer-public))
       (otherwise
        (%tls-unsupported :tls-key-exchange (list :group group)))))))

(defun %tls-read-text-file (pathname)
  (with-open-file (stream pathname :direction :input)
    (with-output-to-string (text)
      (loop for line = (read-line stream nil nil)
            while line
            do (write-line line text)))))

(defun %tls-load-trust-anchors (verify explicit-trust-anchors)
  (when explicit-trust-anchors
    (return-from %tls-load-trust-anchors explicit-trust-anchors))
  (let ((path (cl-tls-kit:default-trust-store-path)))
    (unless path
      (when (eq verify :required)
        (%tls-unsupported :tls-trust-store :not-found))
      (return-from %tls-load-trust-anchors nil))
    (handler-case
        (let* ((selected-path (cl-tls-kit:load-trust-store :ca-file path))
               (text (%tls-read-text-file selected-path))
               (blocks (remove-if-not
                        (lambda (block)
                          (string= "CERTIFICATE"
                                   (cl-tls-kit:pem-block-label block)))
                        (cl-tls-kit:pem-decode text)))
               (anchors
                 (mapcar #'cl-tls-kit.x509:parse-certificate-der
                         (mapcar #'cl-tls-kit:pem-block-der blocks))))
          (if anchors
              anchors
              (%tls-protocol-error
               "The TLS trust store contains no CERTIFICATE blocks."
               :invalid-trust-store)))
      (error ()
        (%tls-protocol-error
         "The TLS trust store could not be loaded."
         :invalid-trust-store)))))

(defun %tls-driver-error (condition)
  (let ((reason (cl-tls-kit:tls13-client-driver-error-reason condition)))
    (%tls-protocol-error
     "The TLS 1.3 client handshake failed."
     (if (and (consp reason)
              (member (first reason)
                      (quote (:certificate-verification-failed :peer-alert))))
         reason
         :handshake-failed))))

#+sbcl
(progn
  (defclass http-tls-stream
      (sb-gray:fundamental-binary-input-stream
       sb-gray:fundamental-binary-output-stream)
    ((driver :initarg :driver :reader %tls-stream-driver)
     (transport-stream :initarg :transport-stream
                       :reader %tls-stream-transport-stream)
     (input :initform (make-array 0
                                  :element-type '(unsigned-byte 8))
            :accessor %tls-stream-input)
     (input-position :initform 0
                     :accessor %tls-stream-input-position)
     (closed-p :initform nil
               :accessor %tls-stream-closed-p)))

  (defun %tls-stream-fill (stream)
    (loop
      (handler-case
          (multiple-value-bind (status plaintext)
              (cl-tls-kit:tls13-client-driver-read-application-data
               (%tls-stream-driver stream))
            (case status
              (:eof (return nil))
              (:would-block (sleep 0.001d0))
              (:data
               (setf (%tls-stream-input stream) plaintext
                     (%tls-stream-input-position stream) 0)
               (unless (zerop (length plaintext))
                 (return t)))
              (otherwise
               (%tls-protocol-error
                "The TLS driver returned an invalid read status."
                :invalid-read-status))))
        (cl-tls-kit:tls13-client-driver-error (condition)
          (if (eq (cl-tls-kit:tls13-client-driver-error-reason condition)
                  :eof)
              (%tls-protocol-error
               "The TLS transport ended without close_notify."
               :missing-close-notify)
              (%tls-driver-error condition))))))

  (defmethod http-kit/http2::%h2-reader-read-available
      ((stream http-tls-stream) count timeout)
    (check-type count (integer 0 *))
    (check-type timeout (or null (real 0 *)))
    (let* ((driver (%tls-stream-driver stream))
           (empty (make-array 0 :element-type '(unsigned-byte 8)))
           (deadline
             (+ (%tls-monotonic-time)
                (if timeout (min timeout 0.1d0) 0.1d0))))
      (cond
        ((typep driver 'http-tls-server-state)
         (values empty :would-block nil))
        ((zerop count)
         (values empty :data nil))
        (t
         (loop
           (let* ((input (%tls-stream-input stream))
                  (position (%tls-stream-input-position stream))
                  (length (length input)))
             (when (< position length)
               (let* ((end (+ position (min count (- length position))))
                      (slice (subseq input position end)))
                 (setf (%tls-stream-input-position stream) end)
                 (return (values slice :data nil)))))
           (handler-case
               (multiple-value-bind (status plaintext)
                   (cl-tls-kit:tls13-client-driver-read-application-data driver)
                 (case status
                   (:eof (return (values empty :eof t)))
                   (:would-block
                    (let ((remaining (- deadline (%tls-monotonic-time))))
                      (if (plusp remaining)
                          (sleep (min 0.001d0 remaining))
                          (return (values empty :would-block nil)))))
                   (:data
                    (setf (%tls-stream-input stream) plaintext
                          (%tls-stream-input-position stream) 0)
                    (when (and (zerop (length plaintext))
                               (>= (%tls-monotonic-time) deadline))
                      (return (values empty :would-block nil))))
                   (otherwise
                    (%tls-protocol-error
                     "The TLS driver returned an invalid read status."
                     :invalid-read-status))))
             (cl-tls-kit:tls13-client-driver-error (condition)
               (if (eq (cl-tls-kit:tls13-client-driver-error-reason condition)
                       :eof)
                   (%tls-protocol-error
                    "The TLS transport ended without close_notify."
                    :missing-close-notify)
                   (%tls-driver-error condition)))))))))

  (defmethod sb-gray:stream-read-sequence
      ((stream http-tls-stream) sequence &optional (start 0) end)
    (let ((end (or end (length sequence)))
          (position start))
      (loop while (< position end)
            for byte = (sb-gray:stream-read-byte stream)
            do (if (eq byte :eof)
                   (return position)
                   (progn
                     (setf (aref sequence position) byte)
                     (incf position))))
      position))

  (defmethod sb-gray:stream-finish-output ((stream http-tls-stream))
    (declare (ignore stream))
    nil)

  (defmethod sb-gray:stream-force-output ((stream http-tls-stream))
    (declare (ignore stream))
    nil)

  (defmethod open-stream-p ((stream http-tls-stream))
    (not (%tls-stream-closed-p stream))))

(defun %make-http-tls-stream (driver stream)
  #+sbcl
  (make-instance 'http-tls-stream
                 :driver driver
                 :transport-stream stream)
  #-sbcl
  (declare (ignore driver stream)))

(defun make-http-tls-upgrader
    (&key (verify :required) alpn-protocols trust-anchors certificate key password
          (unwrap-stream-p nil) (clock-function #'%tls-monotonic-time))
  "Return a CLIENT TLS-UPGRADE callback backed by cl-tls-kit.

The cl-tls-kit driver is TLS 1.3 only.  It owns record protection and the
handshake, while this callback adapts an already-open binary stream to the
driver's transport callbacks."
  (when (null verify)
    (%tls-unsupported :tls-certificate-verification-disable :disabled))
  (unless (member verify '(:optional :required))
    (%tls-protocol-error
     "TLS VERIFY must be :OPTIONAL or :REQUIRED."
     :invalid-verify))
  (unless (or (null alpn-protocols)
              (and (listp alpn-protocols)
                   (every #'%valid-alpn-protocol-p alpn-protocols)))
    (%tls-protocol-error
     "TLS ALPN protocol names must contain 1 to 255 ASCII characters."
     :invalid-alpn))
  (%validate-certificate-pair certificate key "client")
  (when (or certificate key password)
    (%tls-unsupported :tls-client-certificate-auth))
  (when unwrap-stream-p
    (%tls-unsupported :tls-stream-unwrapping))
  (unless (functionp clock-function)
    (%tls-protocol-error "TLS CLOCK-FUNCTION must be a function."
                         :invalid-clock-function))
  (lambda (stream uri &key timeout deadline &allow-other-keys)
    (unless (streamp stream)
      (%tls-protocol-error "TLS upgrade requires a Lisp stream." stream))
    #+sbcl
(let ((connected-p nil))
  (unwind-protect
       (let* ((provider (cl-tls-kit:make-cl-crypto-kit-provider))
              (trust-anchors (%tls-load-trust-anchors verify trust-anchors))
              (transport (%tls-transport stream))
              (driver
                (cl-tls-kit:make-tls13-client-driver
                 :provider provider
                 :key-exchange (%tls-key-exchange)
                 :transport transport
                 :hostname (http-uri-host uri)
                 :alpn alpn-protocols
                 :supported-groups '(#x001d #x0017)
                 :trust-anchors trust-anchors
                 :verify-signature #'crypto-kit:verify-signature)))
         (handler-case
             (%call-with-tls-deadline
              (lambda ()
                (cl-tls-kit:tls13-client-driver-connect driver))
              (http-deadline timeout :deadline deadline
                             :clock-function clock-function)
              clock-function)
           (cl-tls-kit:tls13-client-driver-error (condition)
             (%tls-driver-error condition)))
         (funcall (getf transport :enable-application-read))
         (prog1 (%make-http-tls-stream driver stream)
           (setf connected-p t)))
    (unless connected-p
      (http-kit::%with-http-cleanup
        (close stream :abort t)))))
    #-sbcl
    (%tls-unsupported :tls-native-client)))

(progn
  (defstruct (http-tls-server-state
              (:constructor %make-http-tls-server-state))
    driver provider transport-stream deadline
    (clock-function (function %tls-monotonic-time))
    (phase :initial)
    suite read-state write-state
    (selected-alpn nil)
    (handshake-buffer
     (make-array 0
                 :element-type '(unsigned-byte 8)
                 :adjustable t
                 :fill-pointer 0))
    (input-buffer (make-array 0 :element-type '(unsigned-byte 8)))
    (input-position 0)
    (handshake-complete-p nil)
    (client-hello-received-p nil)
    (early-data-offered-p nil)
    (hello-retry-request-sent-p nil)
    (close-notify-p nil)
    (close-sent-p nil))

  (defmethod print-object ((state http-tls-server-state) output)
    (print-unreadable-object (state output :type t)
      (write-string "TLS server state" output)))

  (defun %tls-server-fail (message detail)
    (%tls-protocol-error message detail))

  (defun %tls-server-read-record (state)
    (labels ((deadline-expired-p ()
               (let ((deadline (http-tls-server-state-deadline state)))
                 (and deadline
                      (>= (funcall
                           (http-tls-server-state-clock-function state))
                          deadline)))))
      (handler-case
          (let ((wire
                  (%tls-read-record
                   (http-tls-server-state-transport-stream state)
                   nil
                   :ciphertext-p
                   (or (http-tls-server-state-read-state state)
                       (http-tls-server-state-early-data-offered-p state)))))
            (when (and (null wire) (deadline-expired-p))
              (%tls-timeout))
            wire)
        (http-timeout (condition) (error condition))
        (http-protocol-error (condition)
          (if (deadline-expired-p)
              (%tls-timeout)
              (error condition)))
        #+sbcl (sb-ext:timeout () (%tls-timeout))
        #+sbcl (sb-bsd-sockets:operation-timeout-error () (%tls-timeout))
        (error ()
          (if (deadline-expired-p)
              (%tls-timeout)
              (%tls-server-fail "TLS transport read failed." :transport-error))))))

  (defun %tls-server-write-record (state bytes)
    (handler-case
        (let ((stream (http-tls-server-state-transport-stream state)))
          (write-sequence bytes stream)
          (finish-output stream))
      (http-timeout (condition) (error condition))
      (error ()
        (%tls-server-fail "TLS transport write failed." :transport-error))))

  (defun %tls-server-content-type (name)
    (cond ((eq name :handshake) cl-tls-kit:+tls-content-type-handshake+)
          ((eq name :application-data) cl-tls-kit:+tls-content-type-application-data+)
          ((eq name :alert) cl-tls-kit:+tls-content-type-alert+)
          (t (%tls-server-fail "TLS driver returned an unsupported record type."
                               :unsupported-record-type))))

  (defun %tls-server-cipher-algorithm (suite)
    (let ((name (cl-tls-kit:tls13-cipher-suite-name suite)))
      (cond ((eq name :aes-128-gcm-sha256) :aes-128-gcm)
            ((eq name :aes-256-gcm-sha384) :aes-256-gcm)
            ((eq name :chacha20-poly1305-sha256) :chacha20-poly1305)
            ((eq name :aes-128-ccm-sha256) :aes-128-ccm)
            (t (%tls-server-fail "TLS driver selected an unsupported cipher suite."
                                 :unsupported-cipher-suite)))))

  (defun %tls-server-traffic-state (state secret)
    (let ((suite (http-tls-server-state-suite state)))
      (unless suite
        (%tls-server-fail "TLS traffic keys arrived before a cipher suite."
                          :invalid-driver-state))
      (handler-case
          (cl-tls-kit:make-tls13-traffic-state
           (http-tls-server-state-provider state)
           (cl-tls-kit:tls13-cipher-suite-hash suite)
           (%tls-server-cipher-algorithm suite)
           secret (cl-tls-kit:tls13-cipher-suite-key-length suite) 12)
        (error ()
               (%tls-server-fail "TLS traffic key setup failed."
                                 :traffic-key-setup-failed)))))

  (progn
(defun %tls-server-send-plaintext (state level content-type fragment)
  (handler-case
      (let* ((plaintext (cl-tls-kit:make-tls-plaintext content-type fragment))
             (wire (if (eq level :initial)
                       (cl-tls-kit:encode-tls-plaintext plaintext)
                     (let ((traffic-state
                            (http-tls-server-state-write-state state)))
                       (unless (and traffic-state
                                    (member level (list :handshake :application)))
                         (%tls-server-fail
                          "TLS driver selected an invalid output level."
                          :invalid-driver-state))
                       (cl-tls-kit:encrypt-tls13-traffic-record
                        plaintext traffic-state)))))
        (%tls-server-write-record state wire))
    (http-timeout (condition) (error condition))
    (http-protocol-error (condition) (error condition))
    (error ()
           (%tls-server-fail "TLS record protection failed."
                             :record-protection-failed))))
(defun %tls-server-fatal-alert (state description)
    (let ((level
            (cond ((http-tls-server-state-handshake-complete-p state) :application)
                  ((http-tls-server-state-write-state state) :handshake)
                  (t :initial))))
      (%tls-server-send-plaintext
       state level cl-tls-kit:+tls-content-type-alert+ (vector 2 description))))

  (defun %tls-server-record-overflow (state)
    (%tls-server-fatal-alert state 22)
    (%tls-server-fail "TLS record exceeds its length limit." :record-overflow))

  (defun %tls-server-reject-early-data (state)
    (%tls-server-fatal-alert state 10)
    (%tls-server-fail "TLS early data is not accepted." :early-data-rejected))

  (defun %tls-server-authentication-failure (state)
    (if (and (http-tls-server-state-early-data-offered-p state)
             (not (http-tls-server-state-handshake-complete-p state)))
        (%tls-server-reject-early-data state)
        (progn
          (%tls-server-fatal-alert state 20)
          (%tls-server-fail "TLS record authentication failed."
                            :record-authentication-failed))))

  (defun %tls-server-read-next-record (state)
    (handler-case
        (%tls-server-read-record state)
      (http-timeout (condition) (error condition))
      (http-protocol-error (condition)
        (if (eq (http-kit:http-protocol-error-detail condition)
                :record-overflow)
            (%tls-server-record-overflow state)
            (error condition)))))

  (defun %tls-server-decrypt-record (state wire)
    (let ((plaintext
           (handler-case
               (nth-value 0
                          (cl-tls-kit:decrypt-tls13-traffic-record
                           wire (http-tls-server-state-read-state state)))
             (cl-tls-kit:tls-record-overflow ()
                                             (return-from %tls-server-decrypt-record
                                                          (%tls-server-record-overflow state)))
             (error ()
                    (return-from %tls-server-decrypt-record
                                 (%tls-server-authentication-failure state))))))
      (when (> (- (length wire) 5 16)
               (1+ cl-tls-kit:+tls-plaintext-limit+))
        (%tls-server-record-overflow state))
      (when (= (cl-tls-kit:tls-plaintext-content-type plaintext)
               cl-tls-kit:+tls-content-type-application-data+)
        (handler-case
            (cl-tls-kit:tls13-server-driver-note-application-data-received
             (http-tls-server-state-driver state))
          (error ()
                 (%tls-server-fail "TLS driver state could not be updated."
                                   :invalid-driver-state))))
      plaintext)))

  (defun %tls-server-handle-events (state events)
    (let ((alert-p nil))
      (dolist (event events)
        (case (getf event :type)
          (:cipher-suite
           (setf (http-tls-server-state-suite state) (getf event :suite)))
          (:secret
           (let ((traffic-state
                  (%tls-server-traffic-state state (getf event :secret))))
             (case (getf event :direction)
               (:read
                (setf (http-tls-server-state-read-state state) traffic-state))
               (:write
                (setf (http-tls-server-state-write-state state) traffic-state))
               (otherwise
                (%tls-server-fail
                 "TLS driver returned an invalid secret direction."
                 :invalid-driver-state)))
             (when (eq (getf event :level) :handshake)
               (setf (http-tls-server-state-phase state) :handshake))))
          (:alpn
           (setf (http-tls-server-state-selected-alpn state)
                 (getf event :protocol)))
          (:send
           (let* ((level (getf event :level))
                  (content-type
                   (%tls-server-content-type (getf event :content-type)))
                  (wire (getf event :wire)))
             (when (and (eq level :initial)
                        (= content-type cl-tls-kit:+tls-content-type-handshake+)
                        (plusp (length wire))
                        (= (aref wire 0) 2))
               (let ((message (cl-tls-kit:decode-handshake wire)))
                 (when (typep message 'cl-tls-kit:tls13-hello-retry-request)
                   (setf (http-tls-server-state-hello-retry-request-sent-p state)
                         t))))
             (%tls-server-send-plaintext state level content-type wire)
             (when (= content-type cl-tls-kit:+tls-content-type-alert+)
               (setf alert-p t))))
          (:handshake-complete
           (setf (http-tls-server-state-handshake-complete-p state) t
                 (http-tls-server-state-phase state) :application))
          ((:ticket-issued :alert :peer-quic-transport-parameters) nil)))
      (when alert-p
        (%tls-server-fail "TLS handshake failed." :handshake-failed))))

  (defun %tls-server-step-handshake-message (state wire)
    (when (and (eq (http-tls-server-state-phase state) :initial)
               (not (http-tls-server-state-client-hello-received-p state))
               (plusp (length wire))
               (= (aref wire 0) 1))
      (setf (http-tls-server-state-client-hello-received-p state) t)
      (handler-case
          (let* ((hello (cl-tls-kit:decode-handshake wire))
                 (extensions (cl-tls-kit:tls13-client-hello-extensions hello)))
            (setf (http-tls-server-state-early-data-offered-p state)
                  (not (null
                        (find 42 extensions
                              :key #'cl-tls-kit:tls-extension-type)))))
        (cl-tls-kit:tls13-decode-error () nil)))
    (handler-case
        (multiple-value-bind (driver events)
            (cl-tls-kit:tls13-server-driver-step
             (http-tls-server-state-driver state) wire
             :level (http-tls-server-state-phase state))
          (setf (http-tls-server-state-driver state) driver)
          (%tls-server-handle-events state events))
      (http-protocol-error (condition) (error condition))
      #+sbcl (sb-ext:timeout () (%tls-timeout))
      (error (condition)
             (when (typep condition 'http-timeout)
               (error condition))
             (%tls-server-fail "TLS handshake failed." :handshake-failed))))

  (defun %tls-server-feed-handshake (state fragment)
  (let ((position 0))
    (labels ((append-fragment (count)
               (let* ((buffer
                        (http-tls-server-state-handshake-buffer state))
                      (current (fill-pointer buffer))
                      (required (+ current count))
                      (capacity (array-total-size buffer))
                      (maximum (+ 4 (* 1024 1024))))
                 (when (< capacity required)
                   (let ((new-capacity
                           (min maximum
                                (max required
                                     (if (zerop capacity)
                                         16
                                         (* 2 capacity))))))
                     (setf buffer
                           (adjust-array buffer new-capacity
                                         :fill-pointer current)
                           (http-tls-server-state-handshake-buffer state)
                           buffer)))
                 (setf (fill-pointer buffer) required)
                 (replace buffer fragment
                          :start1 current
                          :start2 position
                          :end2 (+ position count))
                 (incf position count)))
             (message-length (buffer)
               (+ (ash (aref buffer 1) 16)
                  (ash (aref buffer 2) 8)
                  (aref buffer 3)))
             (process-message (buffer)
               (let ((wire (subseq buffer 0 (fill-pointer buffer))))
                 (setf (http-tls-server-state-handshake-buffer state)
                       (make-array 0
                                   :element-type '(unsigned-byte 8)
                                   :adjustable t
                                   :fill-pointer 0))
                 (%tls-server-step-handshake-message state wire))))
      (loop
        for buffer = (http-tls-server-state-handshake-buffer state)
        do (when (>= (fill-pointer buffer) 4)
             (let* ((body-length (message-length buffer))
                    (wire-length (+ 4 body-length)))
               (when (> body-length (* 1024 1024))
                 (%tls-server-fail
                  "TLS handshake message exceeds its size limit."
                  :handshake-message-too-large))
               (when (= wire-length (fill-pointer buffer))
                 (process-message buffer))))
           (when (= position (length fragment))
             (return))
           (let* ((buffer (http-tls-server-state-handshake-buffer state))
                  (current (fill-pointer buffer))
                  (remaining (- (length fragment) position))
                  (needed (if (< current 4)
                              (- 4 current)
                              (- (+ 4 (message-length buffer)) current))))
             (append-fragment (min remaining needed)))))))


  (defun %tls-server-dispatch-plaintext (state plaintext)
    (let ((content-type (cl-tls-kit:tls-plaintext-content-type plaintext))
          (fragment (cl-tls-kit:tls-plaintext-fragment plaintext)))
      (cond
       ((= content-type cl-tls-kit:+tls-content-type-handshake+)
        (%tls-server-feed-handshake state fragment))
       ((= content-type cl-tls-kit:+tls-content-type-application-data+)
        (unless (http-tls-server-state-handshake-complete-p state)
          (if (http-tls-server-state-early-data-offered-p state)
              (%tls-server-reject-early-data state)
            (progn
              (%tls-server-fatal-alert state 10)
              (%tls-server-fail
               "Application data arrived before the TLS handshake completed."
               :unexpected-application-data))))
        (setf (http-tls-server-state-input-buffer state) fragment
              (http-tls-server-state-input-position state) 0))
       ((= content-type cl-tls-kit:+tls-content-type-alert+)
        (if (cl-tls-kit:tls-close-notify-p plaintext)
            (if (http-tls-server-state-handshake-complete-p state)
                (setf (http-tls-server-state-close-notify-p state) t)
              (%tls-server-fail "TLS closed during the handshake."
                                :handshake-failed))
          (%tls-server-fail "TLS peer sent an alert." :peer-alert)))
       (t (%tls-server-fail "TLS peer sent an unsupported record."
                            :unsupported-record-type)))))

  (defun %tls-server-process-record (state wire)
    (let* ((outer-type (aref wire 0))
           (read-state (http-tls-server-state-read-state state)))
      (cond
       ((= outer-type 20)
        (unless (equalp (subseq wire 5) #(1))
          (%tls-server-fail "TLS peer sent an invalid compatibility record."
                            :invalid-record))
        nil)
       ((and (= outer-type cl-tls-kit:+tls-content-type-application-data+)
             (http-tls-server-state-early-data-offered-p state)
             (not (http-tls-server-state-handshake-complete-p state))
             (or (http-tls-server-state-hello-retry-request-sent-p state)
                 (null read-state)))
        (%tls-server-reject-early-data state))
       (read-state
        (unless (= outer-type cl-tls-kit:+tls-content-type-application-data+)
          (%tls-server-fail "TLS peer sent an unprotected record after encryption began."
                            :unexpected-record))
        (%tls-server-dispatch-plaintext
         state (%tls-server-decrypt-record state wire)))
       (t
        (unless (member outer-type
                        (list cl-tls-kit:+tls-content-type-handshake+
                              cl-tls-kit:+tls-content-type-alert+))
          (%tls-server-fail "TLS peer sent an unexpected plaintext record."
                            :unexpected-record))
        (handler-case
            (%tls-server-dispatch-plaintext
             state
             (cl-tls-kit:make-tls-plaintext outer-type (subseq wire 5)))
          (http-protocol-error (condition) (error condition))
          #+sbcl (sb-ext:timeout () (%tls-timeout))
          (error (condition)
                 (when (typep condition 'http-timeout)
                   (error condition))
                 (%tls-server-fail "TLS plaintext record is invalid."
                                   :invalid-record)))))))

  (defun %tls-server-fill (state)
    (loop
     (when (< (http-tls-server-state-input-position state)
              (length (http-tls-server-state-input-buffer state)))
       (return t))
     (when (http-tls-server-state-close-notify-p state) (return nil))
     (let ((wire (%tls-server-read-next-record state)))
       (unless wire
         (%tls-server-fail "TLS transport ended without close_notify."
                           :missing-close-notify))
       (%tls-server-process-record state wire))))

  (defun %tls-server-write-application-data (state sequence start end)
    (handler-case
        (progn
          (loop for position from start below end
                  by cl-tls-kit:+tls-plaintext-limit+
                for next = (min end (+ position cl-tls-kit:+tls-plaintext-limit+))
                for plaintext =
                  (cl-tls-kit:make-tls-plaintext
                   cl-tls-kit:+tls-content-type-application-data+
                   (subseq sequence position next))
                for wire =
                  (cl-tls-kit:encrypt-tls13-traffic-record
                   plaintext (http-tls-server-state-write-state state))
                do (%tls-server-write-record state wire))
          sequence)
      (http-timeout (condition) (error condition))
      (http-protocol-error (condition) (error condition))
      (error ()
        (%tls-server-fail "TLS application record encryption failed."
                          :record-encryption-failed))))

  (defun %tls-server-send-close-notify (state)
    (unless (http-tls-server-state-close-sent-p state)
      (when (http-tls-server-state-write-state state)
        (%tls-server-send-plaintext
         state :application cl-tls-kit:+tls-content-type-alert+ #(1 0))
        (setf (http-tls-server-state-close-sent-p state) t)))
    t)

  (defun %tls-server-reject-missing-alpn (state)
    (%tls-server-send-plaintext
     state :application cl-tls-kit:+tls-content-type-alert+ #(2 120))
    (%tls-server-fail "TLS handshake completed without negotiated ALPN."
                      :missing-alpn))

  (defun %tls-server-handshake (state)
    (loop until (http-tls-server-state-handshake-complete-p state)
          do (let ((wire (%tls-server-read-next-record state)))
               (unless wire
                 (%tls-server-fail "TLS transport ended during the handshake."
                                   :handshake-failed))
               (%tls-server-process-record state wire)))
    (unless (http-tls-server-state-selected-alpn state)
      (%tls-server-reject-missing-alpn state))
    state)

  #+sbcl
  (progn
    (defmethod print-object ((stream http-tls-stream) output)
      (print-unreadable-object (stream output :type t)
        (write-string "TLS stream" output)))

    (defmethod sb-gray:stream-read-byte ((stream http-tls-stream))
      (let ((driver (%tls-stream-driver stream)))
        (if (typep driver 'http-tls-server-state)
            (let ((state driver))
              (if (< (http-tls-server-state-input-position state)
                     (length (http-tls-server-state-input-buffer state)))
                  (prog1
                      (aref (http-tls-server-state-input-buffer state)
                            (http-tls-server-state-input-position state))
                    (incf (http-tls-server-state-input-position state)))
                  (if (%tls-server-fill state)
                      (sb-gray:stream-read-byte stream)
                      :eof)))
            (let ((input (%tls-stream-input stream))
                  (position (%tls-stream-input-position stream)))
              (if (< position (length input))
                  (prog1 (aref input position)
                    (incf (%tls-stream-input-position stream)))
                  (if (%tls-stream-fill stream)
                      (sb-gray:stream-read-byte stream)
                      :eof))))))

    (defmethod sb-gray:stream-write-byte ((stream http-tls-stream) byte)
      (let ((driver (%tls-stream-driver stream)))
        (if (typep driver 'http-tls-server-state)
            (%tls-server-write-application-data driver (vector byte) 0 1)
            (cl-tls-kit:tls13-client-driver-write driver (vector byte))))
      byte)

    (defmethod sb-gray:stream-write-sequence
        ((stream http-tls-stream) sequence &optional (start 0) end)
      (let* ((end (or end (length sequence)))
             (driver (%tls-stream-driver stream)))
        (when (< start end)
          (if (typep driver 'http-tls-server-state)
              (%tls-server-write-application-data driver sequence start end)
              (cl-tls-kit:tls13-client-driver-write
               driver (subseq sequence start end))))
        sequence))

    (define-condition tls-server-cleanup-warning (warning)
      ((cause :initarg :cause :reader tls-server-cleanup-warning-cause))
      (:report (lambda (condition stream)
                 (declare (ignore condition))
                 (write-string "TLS server driver cleanup failed." stream))))

    (defun %tls-server-cleanup-driver-close (driver)
      (when driver
        (handler-case
            (cl-tls-kit:tls13-server-driver-close driver)
          (error (condition)
            (handler-case
                (warn 'tls-server-cleanup-warning :cause condition)
              (error () nil)))))
      nil)

    (defmethod close ((stream http-tls-stream) &key abort)
      (unless (%tls-stream-closed-p stream)
        (let* ((completed-p nil)
               (transport (%tls-stream-transport-stream stream))
               (driver (%tls-stream-driver stream))
               (server-state (and (typep driver 'http-tls-server-state)
                                  driver))
               (server-driver
                 (and server-state
                      (http-tls-server-state-driver server-state)))
               (driver-closed-p nil))
          (unwind-protect
               (progn
                 (unless abort
                   (if server-state
                       (%tls-server-send-close-notify server-state)
                       (cl-tls-kit:tls13-client-driver-close driver)))
                 (when server-driver
                   (cl-tls-kit:tls13-server-driver-close server-driver)
                   (setf driver-closed-p t))
                 (setf completed-p t))
            (unwind-protect
                 (unwind-protect
                     (when (open-stream-p transport)
                       (if completed-p
                           (close transport :abort abort)
                           (http-kit::%with-http-cleanup
                             (close transport :abort t))))
                   (when (and server-driver (not driver-closed-p))
                     (%tls-server-cleanup-driver-close server-driver)))
              (setf (%tls-stream-closed-p stream) t)))))
      t))

  (defun make-http-tls-server-wrapper
      (&key certificate-chain private-key ticket-key-store alpn-protocols
            (session-tickets-p t)
            (clock-function #'%tls-monotonic-time))
    "Return a server wrapper that adapts the cl-tls-kit TLS 1.3 driver."
    (unless (functionp clock-function)
      (%tls-protocol-error "TLS CLOCK-FUNCTION must be a function."
                           :invalid-clock-function))
    (unless (and certificate-chain private-key)
      (%tls-protocol-error
       "TLS server credentials require both CERTIFICATE-CHAIN and PRIVATE-KEY."
       '(:role "server" :required t)))
    (unless (or (null alpn-protocols)
                (and (listp alpn-protocols)
                     (every #'%valid-alpn-protocol-p alpn-protocols)
                     (= (length alpn-protocols)
                        (length (remove-duplicates alpn-protocols :test #'string=)))))
      (%tls-protocol-error
       "TLS ALPN protocol names must be unique ASCII strings."
       :invalid-alpn))
    (when (and ticket-key-store
               (not (cl-tls-kit:tls13-server-ticket-key-store-p ticket-key-store)))
      (%tls-protocol-error "TLS ticket key store is invalid."
                           :invalid-ticket-key-store))
    #+sbcl
    (let ((server-ticket-key-store (and session-tickets-p ticket-key-store))
          (server-alpn (copy-list alpn-protocols)))
      (lambda (stream &key timeout deadline &allow-other-keys)
        (unless (streamp stream)
          (%tls-protocol-error "TLS server wrapper requires a Lisp stream."
                               :invalid-stream))
        (let ((driver nil))
          (handler-case
              (let ((effective-deadline
                      (http-deadline timeout
                                     :deadline
                                     deadline
                                     :clock-function
                                     clock-function))
                    (completed-p nil))
                (unwind-protect
                     (prog1
                       (%call-with-tls-deadline
                        (lambda ()
                          (let ((provider
                                  (cl-tls-kit:make-cl-crypto-kit-provider)))
                            (setf driver
                                  (cl-tls-kit:make-tls13-server-driver
                                   :provider provider
                                   :certificate-chain certificate-chain
                                   :private-key private-key
                                   :ticket-key-store server-ticket-key-store
                                   :session-tickets session-tickets-p
                                   :alpn-protocols server-alpn))
                            (let ((state
                                    (%make-http-tls-server-state
                                     :driver driver
                                     :provider provider
                                     :transport-stream stream
                                     :deadline effective-deadline
                                     :clock-function clock-function)))
                              (%tls-server-handshake state)
                              (make-instance 'http-tls-stream
                                             :driver state
                                             :transport-stream stream))))
                        effective-deadline
                        clock-function)
                       (setf completed-p t))
                  (unless completed-p
                    (%tls-server-cleanup-driver-close driver)
                    (http-kit::%with-http-cleanup
                      (when (open-stream-p stream)
                        (close stream :abort t))))))
            (http-timeout (condition)
              (error condition))
            (http-protocol-error (condition)
              (error condition))
            (error ()
              (%tls-server-fail "TLS server handshake failed." :handshake-failed))))))
    #-sbcl
    (%tls-unsupported :tls-native-server)))
