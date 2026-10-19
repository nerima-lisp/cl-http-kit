(in-package #:http-kit/test)

(deftest http3-adapter-contract-required-protection-bindings
  (let ((configure (symbol-function 'cl-quic-kit.protection:configure-crypto-backend))
        (bindings nil)
        (calls 0))
    (unwind-protect
         (progn
           (setf (symbol-function 'cl-quic-kit.protection:configure-crypto-backend)
                 (lambda (&rest arguments)
                   (incf calls)
                   (setf bindings arguments)
                   (apply configure arguments)))
           (signals http-timeout
             (http-kit/http3:make-http3-quic-adapter
              :connection (cl-quic-kit:make-quic-connection)
              :hostname "example.test" :timeout 0))
           (ensure-equal 1 calls)
           (ensure-equal 14 (length bindings))
           (loop for (key function) on
                 (list :hkdf-extract #'crypto-kit:hkdf-extract
                       :hkdf-expand #'crypto-kit:hkdf-expand
                       :aead-seal #'crypto-kit:aead-seal
                       :aead-open #'crypto-kit:aead-open
                       :aes-ecb #'crypto-kit:aes-encrypt-block
                       :chacha20 #'crypto-kit:chacha20-keystream
                       :constant-time-equal #'crypto-kit:constant-time-equal)
                 by #'cddr
                 do (ensure-true (eq function (getf bindings key)))))
      (setf (symbol-function 'cl-quic-kit.protection:configure-crypto-backend) configure))
    (ensure-true
     (eq configure (symbol-function 'cl-quic-kit.protection:configure-crypto-backend)))))

(deftest http3-adapter-contract-protection-failure-before-network
  (let ((configure (symbol-function 'cl-quic-kit.protection:configure-crypto-backend))
        (failure (make-condition 'simple-error :format-control "Protection setup failed."))
        (caught nil)
        (calls 0)
        (writes 0))
    (unwind-protect
         (progn
           (setf (symbol-function 'cl-quic-kit.protection:configure-crypto-backend)
                 (lambda (&rest arguments)
                   (ensure-equal 14 (length arguments))
                   (incf calls)
                   (error failure)))
           (handler-case
               (http-kit/http3:make-http3-quic-adapter
                :connection (cl-quic-kit:make-quic-connection
                             :io-write (lambda (&rest arguments)
                                         (declare (ignore arguments))
                                         (incf writes)))
                :hostname "example.test" :timeout 0)
             (simple-error (condition) (setf caught condition)))
           (ensure-true (eq failure caught))
           (ensure-equal 1 calls)
           (ensure-equal 0 writes))
      (setf (symbol-function 'cl-quic-kit.protection:configure-crypto-backend) configure))
    (ensure-true
     (eq configure (symbol-function 'cl-quic-kit.protection:configure-crypto-backend)))))

(deftest http3-adapter-contract-preflight-types
  (let ((writes 0))
    (flet ((write-packet (connection bytes)
             (declare (ignore connection bytes))
             (incf writes)))
      (dolist (value (list nil "0" #C(1 1) (octets 0)))
        (let ((failure
                (handler-case
                    (http-kit/http3:make-http3-quic-adapter
                     :hostname "example.test" :poll-interval value
                     :io-write #'write-packet)
                  (http-protocol-error (condition) condition))))
          (ensure-true (typep failure 'http-protocol-error))
          (ensure-equal :http3-quic-adapter (http-error-operation failure))
          (ensure-true (eq value (http-protocol-error-detail failure)))
          (ensure-equal 0 writes)))
      (dolist (value (list "" 'invalid-hostname (octets 1)))
        (let ((failure
                (handler-case
                    (http-kit/http3:make-http3-quic-adapter
                     :hostname value :io-write #'write-packet)
                  (http-protocol-error (condition) condition))))
          (ensure-true (typep failure 'http-protocol-error))
          (ensure-equal :http3-quic-adapter (http-error-operation failure))
          (ensure-true (eq value (http-protocol-error-detail failure)))
          (ensure-equal 0 writes)))
      (dolist (value (list 1 "options" (vector :max-header-size 1024)))
        (let ((failure
                (handler-case
                    (http-kit/http3:make-http3-quic-adapter
                     :hostname "example.test" :http3-options value
                     :io-write #'write-packet)
                  (http-protocol-error (condition) condition))))
          (ensure-true (typep failure 'http-protocol-error))
          (ensure-equal :http3-quic-adapter (http-error-operation failure))
          (ensure-true (eq value (http-protocol-error-detail failure)))
          (ensure-equal 0 writes)))
      ;; Accepted real intervals proceed to the next public validation.
      (dolist (interval '(0 0.0 1/10))
        (let ((hostname "")
              (failure nil))
          (handler-case
              (http-kit/http3:make-http3-quic-adapter
               :hostname hostname :poll-interval interval :io-write #'write-packet)
            (http-protocol-error (condition) (setf failure condition)))
          (ensure-true (typep failure 'http-protocol-error))
          (ensure-true (eq hostname (http-protocol-error-detail failure)))
          (ensure-equal 0 writes))))))

(deftest http3-adapter-contract-default-alpn-initial-wire
  (let* ((constructor (symbol-function 'cl-quic-kit:make-quic-client))
         (arguments nil)
         (client nil)
         (writes nil)
         (local-id (octets 1 2 3 4))
         (destination-id (octets 5 6 7 8))
         (connection
           (cl-quic-kit:make-quic-connection
            :local-connection-id local-id
            :io-write (lambda (actual bytes)
                        (push (cons actual (copy-seq bytes)) writes)))))
    (unwind-protect
         (progn
           (setf (symbol-function 'cl-quic-kit:make-quic-client)
                 (lambda (&rest options)
                   (setf arguments options
                         client (apply constructor options))))
           (ensure-http3-quic-adapter-timeout
            (lambda ()
              (http-kit/http3:make-http3-quic-adapter
               :connection connection :hostname "example.test"
               :tls-provider (cl-tls-kit:make-cl-crypto-kit-provider)
               :local-connection-id local-id
               :destination-connection-id destination-id :timeout 0))
            :connect-p t)
           (ensure-equal '("h3") (getf arguments :alpn))
           (ensure-true (eq connection (cl-quic-kit:quic-client-connection client)))
           (ensure-true (cl-quic-kit:quic-client-closed-p client))
           (ensure-equal :closing (cl-quic-kit:connection-state connection))
           (ensure-true (consp writes))
           (let ((first (car (last writes))))
             (ensure-true (eq connection (car first)))
             (ensure-true (>= (length (cdr first)) 1200))
             ;; Only invariant, unprotected header fields are inspected here.
             (multiple-value-bind (header end)
                 (cl-quic-kit:decode-packet-header (cdr first))
               (ensure-equal :initial (cl-quic-kit:packet-header-type header))
               (ensure-equal 1 (cl-quic-kit:packet-header-version header))
               (ensure-true
                (equalp destination-id
                        (cl-quic-kit:packet-header-destination-connection-id header)))
               (ensure-true
                (equalp local-id (cl-quic-kit:packet-header-source-connection-id header)))
               (ensure-true (plusp (length (cl-quic-kit:packet-header-payload header))))
               (ensure-equal (length (cdr first)) end))))
      (setf (symbol-function 'cl-quic-kit:make-quic-client) constructor))
    (ensure-true (eq constructor (symbol-function 'cl-quic-kit:make-quic-client)))))

(deftest http3-adapter-contract-positive-handshake-poll-deadline
  (let* ((symbols '(cl-quic-kit:client-start cl-quic-kit:client-poll cl-quic-kit:client-close))
         (originals (mapcar #'symbol-function symbols))
         (writes nil)
         (local-id (octets 1 2 3 4))
         (connection
           (cl-quic-kit:make-quic-connection
            :local-connection-id local-id
            :io-write (lambda (actual bytes)
                        (push (cons actual (copy-seq bytes)) writes)))))
    (call-with-http3-quic-lifecycle-spies
     (lambda (calls)
       (ensure-http3-quic-adapter-timeout
        (lambda ()
          (http-kit/http3:make-http3-quic-adapter
           :connection connection :hostname "example.test" :alpn '("h3")
           :tls-provider (cl-tls-kit:make-cl-crypto-kit-provider)
           :local-connection-id local-id :destination-connection-id (octets 5 6 7 8)
           :poll-interval 1/1000 :timeout 0.1))
        :connect-p t)
       (ensure-equal 1 (length (gethash 'cl-quic-kit:client-start calls)))
       (ensure-true (plusp (length (gethash 'cl-quic-kit:client-poll calls))))
       (ensure-equal 1 (length (gethash 'cl-quic-kit:client-close calls)))
       (let ((client (caar (gethash 'cl-quic-kit:client-start calls))))
         (ensure-true (eq connection (cl-quic-kit:quic-client-connection client)))
         (ensure-true (cl-quic-kit:quic-client-closed-p client)))))
    (ensure-true (consp writes))
    (ensure-true (every (lambda (write)
                          (and (eq connection (car write)) (plusp (length (cdr write)))))
                        writes))
    (ensure-equal :closing (cl-quic-kit:connection-state connection))
    (loop for symbol in symbols for original in originals
          do (ensure-true (eq original (symbol-function symbol))))))

(deftest http3-adapter-contract-positive-read-poll-after-empty
  (dolist (payload (list (octets 17 23 42) (octets)))
    (call-with-http3-factory-prefix-fixture
     (lambda (client quic frames)
       (declare (ignore frames))
       (multiple-value-bind (stream id)
           (funcall (http-kit/http3:http3-client-open-stream client)
                    nil :stream-type :request)
         (ensure-equal 0 id)
         (let* ((poll-name 'cl-quic-kit:client-poll)
                (read-name 'cl-quic-kit:client-read-stream)
                (poll (symbol-function poll-name))
                (read-stream (symbol-function read-name))
                (polls 0)
                (reads 0)
                (empty-reads 0))
           (unwind-protect
                (progn
                  (setf (symbol-function poll-name)
                        (lambda (actual &optional at)
                          (ensure-true (eq quic actual))
                          (prog1 (funcall poll actual at)
                            (when (= 2 (incf polls))
                              (cl-quic-kit:stream-receive-data
                               stream 0 payload :fin t)))))
                  (setf (symbol-function read-name)
                        (lambda (&rest arguments)
                          (ensure-true (eq quic (first arguments)))
                          (ensure-true (eq stream (second arguments)))
                          (ensure-equal 1 (getf (cddr arguments) :timeout))
                          (incf reads)
                          (multiple-value-bind (bytes fin-p)
                              (apply read-stream arguments)
                            (when (and (zerop (length bytes)) (not fin-p))
                              (incf empty-reads))
                            (values bytes fin-p))))
                  (multiple-value-bind (bytes fin-p)
                      (funcall (http-kit/http3:http3-client-read-stream client)
                               stream :timeout 1)
                    (ensure-octets-equal payload bytes)
                    (ensure-true fin-p))
                  (ensure-equal 2 polls)
                  (ensure-equal 2 reads)
                  (ensure-equal 1 empty-reads)
                  (ensure-equal 0 (cl-quic-kit:stream-readable-bytes stream)))
             (setf (symbol-function poll-name) poll
                   (symbol-function read-name) read-stream))
           (ensure-true (eq poll (symbol-function poll-name)))
           (ensure-true (eq read-stream (symbol-function read-name))))))
     :poll-interval 1/1000)))

(deftest http3-adapter-contract-send-failure-preserves-cause-during-cleanup
  (let* ((symbols '(cl-quic-kit:client-start cl-quic-kit:client-poll cl-quic-kit:client-close))
         (originals (mapcar #'symbol-function symbols))
         (send-error (make-condition 'simple-error :format-control "Initial send failed."))
         (cleanup-error (make-condition 'simple-error :format-control "Close send failed."))
         (writes 0)
         (local-id (octets 1 2 3 4))
         (connection nil)
         (failure nil))
    (setf connection
          (cl-quic-kit:make-quic-connection
           :local-connection-id local-id
           :io-write (lambda (actual bytes)
                       (ensure-true (eq actual connection))
                       (ensure-true (plusp (length bytes)))
                       (incf writes)
                       (error (if (= writes 1) send-error cleanup-error)))))
    (call-with-http3-quic-lifecycle-spies
     (lambda (calls)
       (handler-case
           (http-kit/http3:make-http3-quic-adapter
            :connection connection :hostname "example.test"
            :tls-provider (cl-tls-kit:make-cl-crypto-kit-provider)
            :local-connection-id local-id :destination-connection-id (octets 5 6 7 8)
            :timeout 0)
         (http-connection-error (condition) (setf failure condition)))
       (ensure-true (typep failure 'http-connection-error))
       (ensure-true (eq send-error (http-connection-error-cause failure)))
       (ensure-false (http-connection-error-request-sent-p failure))
       (ensure-equal :http3-quic-adapter (http-error-operation failure))
       (ensure-equal 1 (length (gethash 'cl-quic-kit:client-start calls)))
       (ensure-false (gethash 'cl-quic-kit:client-poll calls))
       (ensure-equal 1 (length (gethash 'cl-quic-kit:client-close calls)))
       (let ((client (caar (gethash 'cl-quic-kit:client-start calls))))
         (ensure-true (cl-quic-kit:quic-client-closed-p client)))))
    (ensure-equal 2 writes)
    (ensure-equal :closing (cl-quic-kit:connection-state connection))
    (loop for symbol in symbols for original in originals
          do (ensure-true (eq original (symbol-function symbol))))))

(deftest http3-adapter-contract-nonlocal-send-preserves-exit-during-cleanup
  (let* ((symbols '(cl-quic-kit:client-start cl-quic-kit:client-poll cl-quic-kit:client-close))
         (originals (mapcar #'symbol-function symbols))
         (tag (gensym "SEND-EXIT"))
         (token (list :nonlocal-exit))
         (cleanup-error (make-condition 'simple-error :format-control "Close send failed."))
         (writes 0)
         (closed-callbacks 0)
         (local-id (octets 1 2 3 4))
         (connection nil))
    (setf connection
          (cl-quic-kit:make-quic-connection
           :local-connection-id local-id
           :io-write (lambda (actual bytes)
                       (ensure-true (eq actual connection))
                       (ensure-true (plusp (length bytes)))
                       (incf writes)
                       (if (= writes 1)
                           (throw tag token)
                           (error cleanup-error)))))
    (call-with-http3-quic-lifecycle-spies
     (lambda (calls)
       (ensure-true
        (eq token
            (catch tag
              (http-kit/http3:make-http3-quic-adapter
               :connection connection :hostname "example.test"
               :tls-provider (cl-tls-kit:make-cl-crypto-kit-provider)
               :local-connection-id local-id :destination-connection-id (octets 5 6 7 8)
               :on-close (lambda (&rest arguments)
                           (declare (ignore arguments))
                           (incf closed-callbacks))
               :timeout 0)
              :returned-without-throw)))
       (ensure-equal 1 (length (gethash 'cl-quic-kit:client-start calls)))
       (ensure-equal 0 (length (gethash 'cl-quic-kit:client-poll calls)))
       (ensure-equal 1 (length (gethash 'cl-quic-kit:client-close calls)))
       (let ((client (caar (gethash 'cl-quic-kit:client-start calls))))
         (ensure-true (eq client (caar (gethash 'cl-quic-kit:client-close calls))))
         (ensure-true (cl-quic-kit:quic-client-closed-p client)))))
    (ensure-equal 2 writes)
    (ensure-equal :closing (cl-quic-kit:connection-state connection))
    (ensure-equal 0 closed-callbacks)
    (loop for symbol in symbols for original in originals
          do (ensure-true (eq original (symbol-function symbol))))))

#+sbcl
(defun call-with-http3-adapter-real-peer (function)
  (let* ((directory
           (uiop:ensure-directory-pathname
            (string-trim '(#\Return #\Linefeed)
                         (uiop:run-program '("mktemp" "-d")
                                           :output :string :error-output :string))))
         (certificate (namestring (merge-pathnames "peer.crt" directory)))
         (key (namestring (merge-pathnames "peer.key" directory)))
         (configuration (merge-pathnames "Caddyfile" directory))
         (openssl (or (uiop:getenv "OPENSSL") "openssl"))
         (caddy (or (uiop:getenv "CADDY") "caddy"))
         (listener nil) (process nil) (log nil) (port nil))
    (unwind-protect
         (progn
           (sb-ext:with-timeout 5
             (uiop:run-program
              (list openssl "ecparam" "-name" "prime256v1" "-genkey" "-noout" "-out" key)
              :output :string :error-output :string)
             (uiop:run-program
              (list openssl "req" "-x509" "-new" "-sha256" "-key" key
                    "-out" certificate "-days" "1" "-subj" "/CN=localhost"
                    "-addext" "subjectAltName=DNS:localhost"
                    "-addext" "basicConstraints=critical,CA:TRUE"
                    "-addext" "keyUsage=critical,keyCertSign,digitalSignature")
              :output :string :error-output :string))
           (setf listener (open-http-tcp-listener :host "127.0.0.1")
                 port (http-network-listener-port listener))
           (ensure-true (and (integerp port) (< 0 port 65536)))
           (close-http-tcp-listener listener)
           (with-open-file (out configuration :direction :output :if-exists :error)
             (format out "{~% admin off~% auto_https off~% servers {~% protocols h1 h2 h3~% }~%}~%")
             (format out "localhost:~D {~% bind 127.0.0.1~% tls ~S ~S~% respond ~S~%}~%"
                     port certificate key "ok"))
           (setf log (open (merge-pathnames "peer.log" directory)
                          :direction :output :if-exists :error)
                 process
                 (uiop:launch-program
                  (list "env"
                        (format nil "XDG_CONFIG_HOME=~A" (merge-pathnames "config/" directory))
                        (format nil "XDG_DATA_HOME=~A" (merge-pathnames "data/" directory))
                        caddy "run" "--config" (namestring configuration) "--adapter" "caddyfile")
                  :input nil :output log :error-output :output :wait nil))
           (handler-bind
               ((error (lambda (condition)
                         (handler-case
                             (progn
                               (finish-output log)
                               (format *error-output* "~&Caddy peer startup log:~%~A~%"
                                       (uiop:read-file-string
                                        (merge-pathnames "peer.log" directory))))
                           (error () (error condition))))))
             (ensure-true (%e2e-wait-for-tcp "127.0.0.1" port :timeout 5)))
           (let ((block
                   (find "CERTIFICATE" (cl-tls-kit:pem-decode
                                       (uiop:read-file-string certificate))
                         :key #'cl-tls-kit:pem-block-label :test #'string=)))
             (ensure-true block)
             (funcall function port
                      (cl-tls-kit.x509:parse-certificate-der
                       (cl-tls-kit:pem-block-der block)))))
      (when listener (close-http-tcp-listener listener))
      (when process
        (uiop:terminate-process process :urgent t)
        (sb-ext:with-timeout 5 (uiop:wait-process process)))
      (when log (close log))
      (uiop:delete-directory-tree directory :validate t))
    (ensure-false (uiop:process-alive-p process))
    (ensure-true (integerp (uiop:wait-process process)))
    (ensure-false (probe-file directory))))

#+sbcl
(deftest http3-adapter-contract-real-peer-empty-fin-flushes-local-qpack-stream
  (cl-weave:expect-assertions 33)
  (call-with-http3-adapter-real-peer
   (lambda (port anchor)
     (let ((adapter nil) (closed 0))
       (unwind-protect
            (progn
              (setf adapter
                    (http-kit/http3:make-http3-quic-adapter
                     :server-host "127.0.0.1" :server-port port :hostname "localhost"
                     :tls-trust-anchors (list anchor)
                     :tls-verify-signature #'crypto-kit:verify-signature
                     :tls-signature-algorithms #(1027)
                     :now-fn (lambda ()
                               (/ (get-internal-real-time) internal-time-units-per-second))
                     :timeout 5 :idle-timeout 30
                     :on-close (lambda (&rest arguments)
                                 (declare (ignore arguments)) (incf closed))))
              (ensure-true (http-kit/http3:http3-quic-adapter-p adapter))
              (ensure-true (eq adapter (http-kit/http3:http3-quic-adapter-start adapter)))
              (let* ((quic (http-kit/http3:http3-quic-adapter-quic-client adapter))
                     (client (http-kit/http3:http3-quic-adapter-http3-client adapter)))
                (ensure-true (http-kit/http3:http3-client-open-p client))
                (ensure-equal :established
                              (cl-quic-kit:connection-state
                               (cl-quic-kit:quic-client-connection quic)))
                (ensure-true (http-kit/http3:http3-client-peer-control-stream client))
                (ensure-true
                 (http-kit/http3:http3-control-state-settings-received-p
                  (http-kit/http3:http3-client-peer-control-state client)))
                (let ((response
                        (http-kit/http3:send-http3-request
                         client (make-http-request
                                 :method "GET" :uri (format nil "https://localhost:~D/" port))
                         :timeout 5)))
                  (ensure-equal 200 (http-response-status response))
                  (ensure-equal "ok" (map 'string #'code-char (http-response-body response))))
                (let* ((stream (http-kit/http3:http3-client-qpack-encoder-stream client))
                       (offset (cl-quic-kit:stream-send-offset stream))
                       (empty (make-array 0 :element-type '(unsigned-byte 8))))
                  (ensure-true stream)
                  (ensure-true (cl-quic-kit:stream-local-p stream))
                  (ensure-true (plusp offset))
                  (ensure-false (cl-quic-kit:client-stream-write-pending-p quic stream))
                  (let ((frame
                          (funcall (http-kit/http3:http3-client-write-stream client)
                                   stream empty :fin-p t :timeout 5)))
                    (ensure-true (typep frame 'cl-quic-kit:frame))
                    (ensure-equal :stream (cl-quic-kit:frame-type frame))
                    (ensure-equal (cl-quic-kit:stream-id stream)
                                  (cl-quic-kit:frame-field frame :stream-id))
                    (ensure-equal offset (cl-quic-kit:frame-field frame :offset))
                    (ensure-equal empty (cl-quic-kit:frame-field frame :data))
                    (ensure-true (cl-quic-kit:frame-field frame :fin))
                    (ensure-true (cl-quic-kit:frame-field frame :len-present)))
                  (ensure-equal offset (cl-quic-kit:stream-send-offset stream))
                  ;; Flush completion does not imply peer acknowledgment or peer closure.
                  (ensure-false (cl-quic-kit:client-stream-write-pending-p quic stream)))
                (ensure-true (http-kit/http3:http3-client-open-p client))
                (ensure-equal 0 closed)))
         (when adapter (http-kit/http3:close-http3-quic-adapter adapter)))
       (ensure-true (cl-quic-kit:quic-client-closed-p
                     (http-kit/http3:http3-quic-adapter-quic-client adapter)))
       (ensure-false (http-kit/http3:http3-client-open-p
                      (http-kit/http3:http3-quic-adapter-http3-client adapter)))
       (ensure-equal :closing
                     (cl-quic-kit:connection-state
                      (cl-quic-kit:quic-client-connection
                       (http-kit/http3:http3-quic-adapter-quic-client adapter))))
       (ensure-equal 0 closed)))))
