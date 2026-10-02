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

(defun %validate-certificate-pair (certificate key role &key required)
  (unless (if required
              (and certificate key)
              (or (and certificate key)
                  (and (null certificate) (null key))))
    (%tls-protocol-error
     (format nil "TLS ~A certificates require both CERTIFICATE and KEY."
             role)
     (list :role role :required required))))

(defun http-tls-selected-alpn-protocol (stream)
  "Return the ALPN protocol negotiated by the cl-tls-kit client driver."
  (unless (streamp stream)
    (%tls-protocol-error "TLS ALPN lookup requires a Lisp stream." stream))
  #+sbcl
  (if (typep stream 'http-tls-stream)
      (cl-tls-kit:tls13-client-driver-negotiated-alpn
       (%tls-stream-driver stream))
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

(defun %tls-read-record (stream)
  (let ((header (make-array 5 :element-type '(unsigned-byte 8))))
    (case (%tls-read-exact stream header)
      (:eof nil)
      (:truncated (%tls-protocol-error
                   "The TLS transport ended in a record header."
                   :truncated-record))
      (otherwise
       (let* ((length (+ (ash (aref header 3) 8) (aref header 4)))
              (body (make-array length :element-type '(unsigned-byte 8))))
         (when (eq (%tls-read-exact stream body) :truncated)
           (%tls-protocol-error
            "The TLS transport ended in a record body."
            :truncated-record))
         (concatenate '(vector (unsigned-byte 8)) header body))))))

(defun %tls-transport (stream)
  (list :read (lambda (driver)
                (declare (ignore driver))
                (%tls-read-record stream))
        :write (lambda (driver bytes)
                 (declare (ignore driver))
                 (write-sequence bytes stream)
                 (finish-output stream))
        :close (lambda (driver)
                 (declare (ignore driver))
                 (close stream))))

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
  (unless (member verify '(:optional :required))
    (return-from %tls-load-trust-anchors nil))
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
  (%tls-protocol-error
   "The TLS 1.3 client driver rejected the handshake."
   (cl-tls-kit:tls13-client-driver-error-reason condition)))

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
          (let ((plaintext
                  (cl-tls-kit:tls13-client-driver-read-record
                   (%tls-stream-driver stream))))
            (when plaintext
              (setf (%tls-stream-input stream)
                    (cl-tls-kit:tls-plaintext-fragment plaintext)
                    (%tls-stream-input-position stream) 0)
              (return t)))
        (cl-tls-kit:tls13-client-driver-error (condition)
          (if (eq (cl-tls-kit:tls13-client-driver-error-reason condition)
                  :eof)
              (return nil)
              (%tls-driver-error condition))))))

  (defmethod sb-gray:stream-read-byte ((stream http-tls-stream))
    (let ((input (%tls-stream-input stream))
          (position (%tls-stream-input-position stream)))
      (if (< position (length input))
          (prog1 (aref input position)
            (incf (%tls-stream-input-position stream)))
          (if (%tls-stream-fill stream)
              (sb-gray:stream-read-byte stream)
              :eof))))

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

  (defmethod sb-gray:stream-write-byte ((stream http-tls-stream) byte)
    (cl-tls-kit:tls13-client-driver-write
     (%tls-stream-driver stream)
     (vector byte))
    byte)

  (defmethod sb-gray:stream-write-sequence
      ((stream http-tls-stream) sequence &optional (start 0) end)
    (let ((end (or end (length sequence))))
      (when (< start end)
        (cl-tls-kit:tls13-client-driver-write
         (%tls-stream-driver stream)
         (subseq sequence start end)))
      sequence))

  (defmethod sb-gray:stream-finish-output ((stream http-tls-stream))
    (declare (ignore stream))
    nil)

  (defmethod sb-gray:stream-force-output ((stream http-tls-stream))
    (declare (ignore stream))
    nil)

  (defmethod open-stream-p ((stream http-tls-stream))
    (not (%tls-stream-closed-p stream)))

  (defmethod close ((stream http-tls-stream) &key abort)
    (unless (%tls-stream-closed-p stream)
      (unwind-protect
           (if abort
               (close (%tls-stream-transport-stream stream) :abort t)
               (cl-tls-kit:tls13-client-driver-close
                (%tls-stream-driver stream)))
        (setf (%tls-stream-closed-p stream) t)))
    t))

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
  (unless (member verify '(nil :optional :required))
    (%tls-protocol-error
     "TLS VERIFY must be NIL, :OPTIONAL, or :REQUIRED."
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
    (let* ((provider (cl-tls-kit:make-cl-crypto-kit-provider))
           (trust-anchors (%tls-load-trust-anchors verify trust-anchors))
           (driver
             (cl-tls-kit:make-tls13-client-driver
              :provider provider
              :key-exchange (%tls-key-exchange)
              :transport (%tls-transport stream)
              :hostname (http-uri-host uri)
              :alpn alpn-protocols
              :supported-groups '(#x001d #x0017)
              :trust-anchors trust-anchors
              :verify-signature #'crypto-kit:verify-signature))
           (connected-p nil))
      (unwind-protect
           (progn
             (handler-case
                 (%call-with-tls-deadline
                  (lambda ()
                    (cl-tls-kit:tls13-client-driver-connect driver))
                  (http-deadline timeout :deadline deadline
                                 :clock-function clock-function)
                  clock-function)
               (cl-tls-kit:tls13-client-driver-error (condition)
                 (%tls-driver-error condition)))
             (setf connected-p t)
             (%make-http-tls-stream driver stream))
        (unless connected-p
          (http-kit::%with-http-cleanup
            (close stream :abort t)))))
    #-sbcl
    (%tls-unsupported :tls-native-client)))

(defun make-http-tls-server-wrapper
    (&key certificate key password (unwrap-stream-p nil)
          (clock-function #'%tls-monotonic-time))
  "Return a server wrapper that reports the missing cl-tls-kit server API."
  (declare (ignore password unwrap-stream-p))
  (%validate-certificate-pair certificate key "server" :required t)
  (unless (functionp clock-function)
    (%tls-protocol-error "TLS CLOCK-FUNCTION must be a function."
                         :invalid-clock-function))
  (lambda (stream &key timeout deadline &allow-other-keys)
    (declare (ignore timeout deadline))
    (unless (streamp stream)
      (%tls-protocol-error "TLS upgrade requires a Lisp stream." stream))
    (%tls-unsupported :tls-server)))
