(in-package #:http-kit/test)

#+sbcl
(defun http3-peer-settings-command-exit (arguments)
  (multiple-value-bind (output error-output exit)
      (sb-ext:with-timeout 5
        (uiop:run-program arguments :output :string :error-output :string
                                   :ignore-error-status t))
    (declare (ignore output error-output))
    exit))

#+sbcl
(defun call-with-http3-settings-peer (mode message)
  (let* ((peer (or (uiop:getenv "HTTP_QUIC_PEER")
                   (error "HTTP_QUIC_PEER must name the quic-peer fixture executable.")))
         (directory
           (uiop:ensure-directory-pathname
            (string-trim '(#\Return #\Linefeed)
                         (uiop:run-program '("mktemp" "-d")
                                           :output :string :error-output :string))))
         (certificate (namestring (merge-pathnames "peer.crt" directory)))
         (key (namestring (merge-pathnames "peer.key" directory)))
         (openssl (or (uiop:getenv "OPENSSL") "openssl"))
         (process nil) (adapter nil) (socket nil) (log nil)
         (port nil) (local-port nil) (condition nil) (elapsed nil))
    (unwind-protect
         (progn
           (ensure-equal
            0 (http3-peer-settings-command-exit
               (list openssl "ecparam" "-name" "prime256v1" "-genkey" "-noout" "-out" key)))
           (ensure-equal
            0 (http3-peer-settings-command-exit
               (list openssl "req" "-x509" "-new" "-sha256" "-key" key
                     "-out" certificate "-days" "1" "-subj" "/CN=localhost"
                     "-addext" "subjectAltName=DNS:localhost"
                     "-addext" "basicConstraints=critical,CA:TRUE"
                     "-addext" "keyUsage=critical,keyCertSign,digitalSignature")))
           (let ((block
                   (find "CERTIFICATE"
                         (cl-tls-kit:pem-decode (uiop:read-file-string certificate))
                         :key #'cl-tls-kit:pem-block-label :test #'string=)))
             (ensure-true block)
             (let ((anchor
                     (cl-tls-kit.x509:parse-certificate-der
                      (cl-tls-kit:pem-block-der block))))
               (setf log (open (merge-pathnames "peer.stderr.log" directory)
                               :direction :output :if-exists :error)
                     process
                     (uiop:launch-program
                      (list peer "--mode" mode "--cert" certificate "--key" key)
                      :input :stream :output :stream :error-output log :wait nil))
               (let ((ready
                       (sb-ext:with-timeout 5
                         (read-line (uiop:process-info-output process)))))
                 (ensure-true (and (>= (length ready) 6)
                                   (string= "READY " ready :end2 6)))
                 (setf port (parse-integer ready :start 6)))
               (ensure-true (< 0 port 65536))
               (ensure-true (uiop:process-alive-p process))
               (setf socket
                     (cl-quic-kit:make-udp-socket :local-host "127.0.0.1"
                                                  :remote-host "127.0.0.1" :remote-port port)
                     local-port (cl-quic-kit:udp-socket-local-port socket))
               (ensure-true (< 0 local-port 65536))
               (let ((start (get-internal-real-time)))
                 (handler-case
                     ;; Omitted timeout and deadline exercise the public NIL defaults.
                     (setf adapter
                           (http-kit/http3:make-http3-quic-adapter
                            :udp-socket socket :server-host "127.0.0.1" :server-port port
                            :hostname "localhost" :tls-trust-anchors (list anchor)
                            :tls-verify-signature #'crypto-kit:verify-signature
                            :tls-signature-algorithms #(1027)
                            :now-fn (lambda ()
                                      (/ (get-internal-real-time)
                                         internal-time-units-per-second))))
                   (http-protocol-error (error) (setf condition error)))
                 (setf elapsed (/ (- (get-internal-real-time) start)
                                  internal-time-units-per-second)))
               (format t "~&QUIC-PEER ~A SECONDS ~,6F CONDITION ~S~%"
                       mode elapsed (type-of condition))
               (if message
                   (progn
                     (ensure-false adapter)
                     (ensure-true (typep condition 'http-protocol-error))
                     (ensure-equal message (http-error-message condition))
                     (ensure-equal :http3-quic-adapter (http-error-operation condition))
                     (ensure-equal :established (http-protocol-error-detail condition))
                     (ensure-true (>= elapsed 15)))
                   (progn
                     (ensure-false condition)
                     (ensure-true (http-kit/http3:http3-quic-adapter-p adapter))
                     (let ((http3 (http-kit/http3:http3-quic-adapter-http3-client adapter))
                           (quic (http-kit/http3:http3-quic-adapter-quic-client adapter)))
                       (ensure-true (http-kit/http3:http3-client-open-p http3))
                       (ensure-true (http-kit/http3:http3-client-peer-control-stream http3))
                       (ensure-true
                        (http-kit/http3:http3-control-state-settings-received-p
                         (http-kit/http3:http3-client-peer-control-state http3)))
                       (ensure-equal
                        :established
                        (cl-quic-kit:connection-state (cl-quic-kit:quic-client-connection quic)))
                       (ensure-true
                        (eq adapter (http-kit/http3:http3-quic-adapter-start adapter)))
                       (when (string= mode "normal-delayed-stream-batch")
                         (let ((streams (cl-quic-kit::quic-client-streams quic)))
                           (loop repeat 1000
                                 until (and (gethash 7 streams) (gethash 11 streams))
                                 do (http-kit/http3:http3-quic-adapter-poll adapter)
                                    (sleep 0.005))
                           (loop for id in '(7 11)
                                 for accessor in
                                   (list #'http-kit/http3:http3-client-peer-qpack-encoder-stream
                                         #'http-kit/http3:http3-client-peer-qpack-decoder-stream)
                                 for stream = (gethash id streams)
                                 do (ensure-true stream)
                                    (ensure-equal id (cl-quic-kit:stream-id stream))
                                    (ensure-false (cl-quic-kit:stream-local-p stream))
                                    (unless (funcall accessor http3)
                                      (http-kit/http3:process-http3-peer-unidirectional-stream
                                       http3 stream :timeout 5))
                                    (ensure-true (eq stream (funcall accessor http3))))))
                       (ensure-true (http-kit/http3:close-http3-quic-adapter adapter))
                       (ensure-true (cl-quic-kit:quic-client-closed-p quic))
                       (ensure-false (http-kit/http3:http3-client-open-p http3))
                       (ensure-true (http-kit/http3:close-http3-quic-adapter adapter)))
                     (ensure-true (>= elapsed 0.25))))
               (ensure-true (uiop:process-alive-p process))
               (ensure-true
                (handler-case
                    (progn
                      (cl-quic-kit:udp-send
                       socket (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0))
                      nil)
                  (cl-quic-kit:quic-error () t)))
               (ensure-equal
                0 (http3-peer-settings-command-exit
                   (list peer "--check-port" (write-to-string local-port))))
               (close (uiop:process-info-input process))
               (let* ((exit (sb-ext:with-timeout 5 (uiop:wait-process process)))
                      (transcript
                        (uiop:slurp-stream-string (uiop:process-info-output process))))
                 (ensure-equal 0 exit)
                 (ensure-false (uiop:process-alive-p process))
                 (ensure-true (search "ACCEPT h3 localhost TLS13" transcript))
                 (ensure-true (search "CLEANUP" transcript))
                 (ensure-true
                  (if (string= mode "control-none")
                      (not (search "PREFIX" transcript))
                      (search "PREFIX 0" transcript)))
                 (ensure-true
                  (if message (not (search "SETTINGS" transcript))
                      (search "SETTINGS 4 0" transcript)))
                 (when (string= mode "normal-delayed-stream-batch")
                   (ensure-true (search "STREAM 7 TYPE 2" transcript))
                   (ensure-true (search "STREAM 11 TYPE 3" transcript))))
               (ensure-equal
                0 (http3-peer-settings-command-exit
                   (list peer "--check-port" (write-to-string port)))))))
      (unwind-protect
           (progn
             (when adapter (http-kit/http3:close-http3-quic-adapter adapter))
             (when socket (cl-quic-kit:udp-close socket)))
        (unwind-protect
             (when process
               (when (open-stream-p (uiop:process-info-input process))
                 (close (uiop:process-info-input process)))
               (handler-case
                   (sb-ext:with-timeout 5 (uiop:wait-process process))
                 (sb-ext:timeout ()
                   (uiop:terminate-process process :urgent t)
                   (sb-ext:with-timeout 5 (uiop:wait-process process))
                   (error "Owned QUIC peer did not stop after stdin close."))))
          (when log (close log))
          (uiop:delete-directory-tree directory :validate t))))
    (ensure-false (probe-file directory))))

#+sbcl
(deftest http3-peer-settings-known-good-default-deadline
  (cl-weave:expect-assertions 30)
  (call-with-http3-settings-peer "normal-delayed-settings" nil))

#+sbcl
(deftest http3-peer-settings-delayed-critical-streams-default-deadline
  (cl-weave:expect-assertions 40)
  (call-with-http3-settings-peer "normal-delayed-stream-batch" nil))

#+sbcl
(deftest http3-peer-settings-control-none-default-deadline
  (cl-weave:expect-assertions 24)
  (call-with-http3-settings-peer
   "control-none" "The peer HTTP/3 control stream did not become readable."))

#+sbcl
(deftest http3-peer-settings-control-prefix-only-default-deadline
  (cl-weave:expect-assertions 24)
  (call-with-http3-settings-peer
   "control-prefix-no-settings" "The peer HTTP/3 SETTINGS did not arrive."))
