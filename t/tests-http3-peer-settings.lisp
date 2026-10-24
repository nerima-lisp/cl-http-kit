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
         (port nil) (local-port nil) (condition nil) (elapsed nil)
         (original-poll (symbol-function 'cl-quic-kit:client-poll))
         (accepted-peer-streams nil)
         (handshake-polls 0) (control-polls 0)
         (control-poll-delegations 0)
         (control-sleep-seconds 0))
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
             (let* ((anchor
                     (cl-tls-kit.x509:parse-certificate-der
                      (cl-tls-kit:pem-block-der block)))
                    (adapter-maker
                      (lambda ()
                        (http-kit/http3:make-http3-quic-adapter
                         :udp-socket socket :server-host "127.0.0.1" :server-port port
                         :hostname "localhost" :tls-trust-anchors (list anchor)
                         :tls-verify-signature #'crypto-kit:verify-signature
                         :tls-signature-algorithms #(1027)
                         :now-fn (lambda ()
                                   (/ (get-internal-real-time)
                                      internal-time-units-per-second))))))
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
                 (when (member mode
                               (list "control-none" "control-prefix-no-settings")
                               :test
                               (function string=))
                   (ensure-true
                    (eq (function sleep)
                        http-kit/http3::*http3-quic-adapter-peer-control-sleep-function*)))
                 (handler-case
                     ;; Omitted timeout and deadline exercise the public NIL defaults.
                     (setf adapter
                               (cond
                                 ((member mode
                                          (list "control-none" "control-prefix-no-settings")
                                          :test
                                          (function string=))
                               (let* ((sleep-function
                                       (lambda (duration)
                                         (incf control-sleep-seconds (rationalize duration))))
                                      (http-kit/http3::*http3-quic-adapter-peer-control-sleep-function*
                                       sleep-function))
                                 (call-with-http3-quic-lifecycle-spies
                                  (lambda (calls)
                                    (declare (ignore calls))
                                    (funcall adapter-maker))
                                  :client-poll
                                  (lambda (client &rest arguments)
                                    (cond
                                      ((not
                                        (eq :established
                                            (cl-quic-kit:connection-state
                                             (cl-quic-kit:quic-client-connection client))))
                                        (incf handshake-polls)
                                        (apply original-poll client arguments))
                                      ((string= mode "control-none")
                                       (incf control-polls)
                                       (multiple-value-prog1
                                           (apply original-poll client arguments)
                                         (incf control-poll-delegations)))
                                      ((and (string= mode "control-prefix-no-settings")
                                            (let ((control-stream
                                                   (find 3 accepted-peer-streams
                                                         :key #'cl-quic-kit:stream-id)))
                                              (and control-stream
                                                   (or (plusp (cl-quic-kit:stream-readable-bytes control-stream))
                                                       (>= (cl-quic-kit:stream-read-offset control-stream) 1)))))
                                       (incf control-polls)
                                       (multiple-value-prog1
                                           (apply original-poll client arguments)
                                         (incf control-poll-delegations)))
                                      (t
                                        (incf handshake-polls)
                                        (apply original-poll client arguments))))
                                  :client-accept
                                  (lambda (client stream)
                                    (declare (ignore client))
                                    (when stream
                                      (push stream accepted-peer-streams))))))
                                 ((string= mode "normal-delayed-stream-batch")
                                  (call-with-http3-quic-lifecycle-spies
                                   (lambda (calls)
                                     (declare (ignore calls))
                                     (funcall adapter-maker))
                                   :client-accept
                                   (lambda (client stream)
                                     (declare (ignore client))
                                     (when stream
                                       (push stream accepted-peer-streams)))))
                                 (t (funcall adapter-maker))))
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
                     (when (string= mode "control-none")
                       (ensure-true (plusp handshake-polls)))
                     (when (member mode
                                   (list "control-none" "control-prefix-no-settings")
                                   :test
                                   (function string=))
                       (ensure-true (plusp control-polls))
                       (ensure-equal control-polls control-poll-delegations))
                     (ensure-true
                      (if (member mode
                                  (list "control-none" "control-prefix-no-settings")
                                  :test
                                  (function string=))
                          (>= control-sleep-seconds 15)
                          (>= elapsed 15))))
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
                         (let ((streams (copy-list accepted-peer-streams)))
                           (loop repeat 1000
                                 until (and (find 7 streams :key #'cl-quic-kit:stream-id)
                                            (find 11 streams :key #'cl-quic-kit:stream-id))
                                 do (http-kit/http3:http3-quic-adapter-poll adapter)
                                    (loop for stream =
                                            (cl-quic-kit:client-accept-peer-stream
                                             quic :type :unidirectional)
                                          while stream
                                          do (push stream streams))
                                    (sleep 0.005))
                           (loop for id in '(7 11)
                                 for accessor in
                                   (list #'http-kit/http3:http3-client-peer-qpack-encoder-stream
                                         #'http-kit/http3:http3-client-peer-qpack-decoder-stream)
                                 for stream = (find id streams :key #'cl-quic-kit:stream-id)
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
  (cl-weave:expect-assertions 28)
  (call-with-http3-settings-peer
   "control-none" "The peer HTTP/3 control stream did not become readable."))

#+sbcl
(deftest http3-peer-settings-control-prefix-only-default-deadline
  (cl-weave:expect-assertions 27)
  (call-with-http3-settings-peer
   "control-prefix-no-settings" "The peer HTTP/3 SETTINGS did not arrive."))
#+sbcl
(deftest http3-peer-native-finished-unknown-stream-is-retained
  (let* ((unknown-stream
           (make-http3-test-stream :kind :peer-extension))
         (extension-prefix (http-kit/http3:http3-varint-encode 64))
         (pending-peer-streams nil)
         (accepted-count 0)
         (first-poll-p nil)
         (unknown-read-count 0)
         (response-wire (http3-test-native-response-wire))
         (response-sent-p nil)
         (client
           (http3-test-make-native-client
            :poll-function
            (lambda ()
              (unless first-poll-p
                (setf first-poll-p t
                      pending-peer-streams (list unknown-stream))))
            :accept-peer-unidirectional-stream
            (lambda ()
              (let ((stream (pop pending-peer-streams)))
                (when stream (incf accepted-count))
                stream))
            :try-read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore timeout deadline))
              (cond
                ((eq stream unknown-stream)
                 (incf unknown-read-count)
                 (if (= unknown-read-count 1)
                     (values extension-prefix t :data)
                     (values nil nil :would-block)))
                ((stringp (http3-test-stream-kind stream))
                 (if (and (plusp unknown-read-count) (not response-sent-p))
                     (progn
                       (setf response-sent-p t)
                       (values response-wire t :data))
                     (values nil nil :would-block)))
                (t (values nil nil :would-block)))))))
    (unwind-protect
         (let ((response
                 (http-kit/http3:send-http3-request
                  client
                  (make-http-request
                   :method "GET"
                   :uri "https://example.test/finished-unknown-stream"))))
           (ensure-equal 200 (http-response-status response))
           (ensure-true first-poll-p)
           (ensure-equal 1 accepted-count)
           (ensure-equal 1 unknown-read-count)
           (ensure-equal
            :finished
            (cdr
             (assoc
              unknown-stream
              (http-kit/http3::http3-client-discard-peer-unidirectional-streams
               client)
              :test (function eq))))
           (ensure-true (http-kit/http3:http3-client-open-p client)))
      (when (http-kit/http3:http3-client-open-p client)
        (http-kit/http3:close-http3-client client)))))
#+sbcl
(deftest http3-peer-native-cancels-unknown-stream-after-classification
  (let* ((unknown-stream
           (make-http3-test-stream :kind :peer-extension))
         (extension-prefix (http-kit/http3:http3-varint-encode 64))
         (pending-peer-streams nil)
         (accepted-count 0)
         (first-poll-p nil)
         (unknown-read-count 0)
         (cancel-count 0)
         (cancelled-stream nil)
         (cancel-error-code nil)
         (response-wire (http3-test-native-response-wire))
         (response-sent-p nil)
         (client
           (http3-test-make-native-client
            :poll-function
            (lambda ()
              (unless first-poll-p
                (setf first-poll-p t
                      pending-peer-streams (list unknown-stream))))
            :accept-peer-unidirectional-stream
            (lambda ()
              (let ((stream (pop pending-peer-streams)))
                (when stream (incf accepted-count))
                stream))
            :cancel-stream
            (lambda (stream &key error-code timeout deadline)
              (declare (ignore timeout deadline))
              (incf cancel-count)
              (setf cancelled-stream stream
                    cancel-error-code error-code))
            :try-read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore timeout deadline))
              (cond
                ((eq stream unknown-stream)
                 (incf unknown-read-count)
                 (if (= unknown-read-count 1)
                     (values extension-prefix nil :data)
                     (values nil nil :would-block)))
                ((stringp (http3-test-stream-kind stream))
                 (if (and (= cancel-count 1) (not response-sent-p))
                     (progn
                       (setf response-sent-p t)
                       (values response-wire t :data))
                     (values nil nil :would-block)))
                (t (values nil nil :would-block)))))))
    (unwind-protect
         (let ((response
                 (http-kit/http3:send-http3-request
                  client
                  (make-http-request
                   :method "GET"
                   :uri "https://example.test/cancel-unknown-stream"))))
           (ensure-equal 200 (http-response-status response))
           (ensure-true first-poll-p)
           (ensure-equal 1 accepted-count)
           (ensure-equal 1 unknown-read-count)
           (ensure-equal 1 cancel-count)
           (ensure-true (eq unknown-stream cancelled-stream))
           (ensure-equal #x103 cancel-error-code)
           (ensure-equal
            :cancelled
            (cdr
             (assoc
              unknown-stream
              (http-kit/http3::http3-client-discard-peer-unidirectional-streams
               client)
              :test (function eq))))
           (ensure-true (http-kit/http3:http3-client-open-p client)))
      (when (http-kit/http3:http3-client-open-p client)
        (http-kit/http3:close-http3-client client)))))
#+sbcl
(deftest http3-peer-native-accept-reports-truncated-prefix-fin
  (cl-weave:expect-assertions 16)
  (dolist
      (case
       (list
        (list
         (octets #x40)
         "A peer HTTP/3 unidirectional stream ended before its type prefix.")
        (list
         (http3-test-concat-octets
          (http-kit/http3:http3-varint-encode
           http-kit/http3:+http3-push-stream-type+)
          (octets #x40))
         "An HTTP/3 push stream ended with a truncated push ID.")))
    (destructuring-bind (wire message) case
      (let* ((stream
               (make-http3-test-stream :reads (list (list wire t))))
             (client (http3-transport-boundary-client))
             (failure
               (handler-case
                   (progn
                     (http-kit/http3:accept-http3-peer-unidirectional-stream
                      client stream)
                     nil)
                 (http-protocol-error (condition) condition))))
        (unwind-protect
             (progn
               (ensure-true (typep failure 'http-protocol-error))
               (ensure-equal :http3-transport (http-error-operation failure))
               (ensure-equal message (http-error-message failure))
               (ensure-equal
                :h3-stream-creation-error
                (http-protocol-error-detail failure))
               (ensure-equal
                1
                (http-kit/http3::http3-client-peer-unidirectional-stream-count
                 client))
               (ensure-equal nil (http3-test-stream-reads stream))
               (ensure-equal
                nil
                (http-kit/http3::http3-client-prefetched-stream-inputs client)))
          (when (http-kit/http3:http3-client-open-p client)
            (http-kit/http3:close-http3-client client)))))))

#+sbcl
(progn
  (deftest http3-peer-native-scheduler-rejects-peer-stream-count-overflow
    (cl-weave:expect-assertions 9)
    (let* ((peer-streams
             (list (make-http3-test-stream :kind :peer-one)
                   (make-http3-test-stream :kind :peer-two)))
           (pending-peer-streams nil)
           (accepted-count 0)
           (peer-read-count 0)
           (request-read-count 0)
           (first-poll-p nil)
           (response-wire (http3-test-native-response-wire))
           (response-sent-p nil)
           (client
             (http3-test-make-native-client
              :poll-function
              (lambda ()
                (unless first-poll-p
                  (setf first-poll-p t
                        pending-peer-streams (copy-list peer-streams))))
              :accept-peer-unidirectional-stream
              (lambda ()
                (let ((stream (pop pending-peer-streams)))
                  (when stream (incf accepted-count))
                  stream))
              :try-read-stream
              (lambda (stream &key timeout deadline)
                (declare (ignore timeout deadline))
                (cond
                  ((member stream peer-streams :test (function eq))
                   (incf peer-read-count)
                   (values nil nil :would-block))
                  ((stringp (http3-test-stream-kind stream))
                   (incf request-read-count)
                   (if response-sent-p
                       (values nil nil :would-block)
                       (progn
                         (setf response-sent-p t)
                         (values response-wire t :data))))
                  (t (values nil nil :would-block)))))))
      (setf
       (http-kit/http3::http3-client-max-peer-unidirectional-streams client)
       1)
      (unwind-protect
           (let* ((failure
                    (http3-boundary-error
                     (lambda ()
                       (http-kit/http3:send-http3-request
                        client
                        (make-http-request
                         :method "GET"
                         :uri "https://example.test/peer-stream-limit")))))
                  (protocol-failure
                    (if (typep failure 'http-connection-error)
                        (http-connection-error-cause failure)
                        failure)))
             (http3-public-error-check
              protocol-failure
              "The HTTP/3 peer unidirectional-stream limit was exceeded."
              :h3-excessive-load)
             (ensure-equal 2 accepted-count)
             (ensure-equal
              1
              (http-kit/http3::http3-client-peer-unidirectional-stream-count
               client))
             (ensure-equal 0 peer-read-count)
             (ensure-equal 0 request-read-count)
             (ensure-equal
              nil
              (http-kit/http3::http3-client-pending-peer-unidirectional-streams
               client)))
        (when (http-kit/http3:http3-client-open-p client)
          (http-kit/http3:close-http3-client client)))))

  (deftest http3-peer-native-scheduler-rejects-truncated-prefix-fin
    (cl-weave:expect-assertions 20)
    (dolist
        (case
         (list
          (list
           :push-id
           (http3-test-concat-octets
            (http-kit/http3:http3-varint-encode
             http-kit/http3:+http3-push-stream-type+)
            (subseq (http-kit/http3:http3-varint-encode 64) 0 1))
           "An HTTP/3 push stream ended with a truncated push ID.")
          (list
           :type-prefix
           (subseq (http-kit/http3:http3-varint-encode 16384) 0 1)
           "A peer HTTP/3 unidirectional stream ended before its type prefix.")))
      (destructuring-bind (case-name wire message) case
        (let* ((peer-stream (make-http3-test-stream :kind :peer-prefix))
               (pending-peer-streams nil)
               (accepted-count 0)
               (peer-read-count 0)
               (request-read-count 0)
               (first-poll-p nil)
               (response-wire (http3-test-native-response-wire))
               (response-sent-p nil)
               (client
                 (http3-test-make-native-client
                  :poll-function
                  (lambda ()
                    (unless first-poll-p
                      (setf first-poll-p t
                            pending-peer-streams (list peer-stream))))
                  :accept-peer-unidirectional-stream
                  (lambda ()
                    (let ((stream (pop pending-peer-streams)))
                      (when stream (incf accepted-count))
                      stream))
                  :try-read-stream
                  (lambda (stream &key timeout deadline)
                    (declare (ignore timeout deadline))
                    (cond
                      ((eq stream peer-stream)
                       (incf peer-read-count)
                       (if (= peer-read-count 1)
                           (values wire t :data)
                           (values nil nil :would-block)))
                      ((stringp (http3-test-stream-kind stream))
                       (incf request-read-count)
                       (if response-sent-p
                           (values nil nil :would-block)
                           (progn
                             (setf response-sent-p t)
                             (values response-wire t :data))))
                      (t (values nil nil :would-block)))))))
          (unwind-protect
               (let* ((failure
                        (http3-boundary-error
                         (lambda ()
                           (http-kit/http3:send-http3-request
                            client
                            (make-http-request
                             :method "GET"
                             :uri
                             (format nil "https://example.test/~(~A~)"
                                     case-name))))))
                      (protocol-failure
                        (if (typep failure 'http-connection-error)
                            (http-connection-error-cause failure)
                            failure)))
                 (http3-public-error-check
                  protocol-failure message :h3-stream-creation-error)
                 (ensure-equal 1 accepted-count)
                 (ensure-equal 1 peer-read-count)
                 (ensure-equal 0 request-read-count)
                 (ensure-equal
                  1
                  (http-kit/http3::http3-client-peer-unidirectional-stream-count
                   client))
                 (ensure-true
                  (member
                   peer-stream
                   (http-kit/http3::http3-client-pending-peer-unidirectional-streams
                    client)
                   :test (function eq)))
                 (ensure-false
                  (assoc
                   peer-stream
                   (http-kit/http3::http3-client-peer-unidirectional-prefix-buffers
                    client)
                   :test (function eq))))
            (when (http-kit/http3:http3-client-open-p client)
              (http-kit/http3:close-http3-client client)))))))

  (deftest http3-peer-native-scheduler-does-not-queue-push-after-owner-close
    (cl-weave:expect-assertions 11)
    (let* ((push-stream (make-http3-test-stream :kind :peer-push))
           (push-wire
             (http3-test-concat-octets
              (http-kit/http3:http3-varint-encode
               http-kit/http3:+http3-push-stream-type+)
              (http-kit/http3:http3-varint-encode 2)))
           (pending-peer-streams nil)
           (accepted-count 0)
           (push-read-count 0)
           (first-poll-p nil)
           (response-wire (http3-test-native-response-wire))
           (response-sent-p nil)
           (next-stream-id -4)
           (prefetched-before-close-p nil)
           (close-result nil)
           (push-stream-closed-p nil)
           (client nil))
      (setf client
            (http-kit/http3:make-http3-client
             :open-stream
             (lambda (request &key stream-type timeout deadline)
               (declare (ignore timeout deadline))
               (let ((stream
                       (make-http3-test-stream
                        :kind
                        (if request
                            (http-kit:http-request-target request)
                            stream-type))))
                 (if request
                     (values stream (incf next-stream-id 4))
                     stream)))
             :write-stream
             (lambda (stream bytes &key fin-p timeout deadline)
               (declare (ignore stream bytes fin-p timeout deadline)))
             :read-stream
             (lambda (stream &key timeout deadline)
               (declare (ignore stream timeout deadline))
               (values nil t))
             :close-stream
             (lambda (stream &key condition)
               (declare (ignore condition))
               (when (eq stream push-stream)
                 (setf push-stream-closed-p t))
               (setf (http3-test-stream-closed-p stream) t))
             :poll-function
             (lambda ()
               (unless first-poll-p
                 (setf first-poll-p t
                       pending-peer-streams (list push-stream))))
             :try-read-stream
             (lambda (stream &key timeout deadline)
               (declare (ignore timeout deadline))
               (cond
                 ((eq stream push-stream)
                  (incf push-read-count)
                  (if (= push-read-count 1)
                      (values push-wire nil :data)
                      (values nil nil :would-block)))
                 ((stringp (http3-test-stream-kind stream))
                  (if response-sent-p
                      (values nil nil :would-block)
                      (progn
                        (setf response-sent-p t)
                        (values response-wire t :data))))
                 (t (values nil nil :would-block))))
             :stream-write-pending-p
             (lambda (stream)
               (declare (ignore stream))
               nil)
             :accept-peer-unidirectional-stream
             (lambda ()
               (let ((stream (pop pending-peer-streams)))
                 (when stream (incf accepted-count))
                 stream))
             :serialize
             (lambda (thunk)
               (let ((result (funcall thunk)))
                 (when (and
                        (null close-result)
                        (assoc
                         push-stream
                         (http-kit/http3::http3-client-prefetched-stream-inputs
                          client)
                         :test (function eq)))
                   (setf prefetched-before-close-p t
                         close-result
                         (http-kit/http3:close-http3-client client)))
                 result))
             :max-push-id 2))
      (unwind-protect
           (let ((failure
                   (http3-boundary-error
                    (lambda ()
                      (http-kit/http3:send-http3-request
                       client
                       (make-http-request
                        :method "GET"
                        :uri "https://example.test/close-during-push-discovery"))))))
             (ensure-true (typep failure 'http-connection-error))
             (ensure-equal :http3-request (http-kit:http-error-operation failure))
             (ensure-equal
              "The HTTP/3 client was closed."
              (http-kit:http-error-message failure))
             (ensure-true close-result)
             (ensure-false (http-kit/http3:http3-client-open-p client))
             (ensure-equal 1 accepted-count)
             (ensure-equal 1 push-read-count)
             (ensure-true prefetched-before-close-p)
             (ensure-true push-stream-closed-p)
             (ensure-equal
              nil
              (http-kit/http3::http3-client-pending-push-streams client))
             (ensure-equal
              nil
              (http-kit/http3::http3-client-prefetched-stream-inputs client)))
        (when (http-kit/http3:http3-client-open-p client)
          (http-kit/http3:close-http3-client client)))))


  (deftest http3-peer-unknown-cancel-failure-closes-all-streams-and-propagates
    (cl-weave:expect-assertions 11)
    (let* ((unknown-stream
             (make-http3-test-stream
              :kind :peer-extension
              :reads
              (list
               (list (http-kit/http3:http3-varint-encode 64) nil))))
           (client-streams nil)
           (close-calls nil)
           (close-count 0)
           (close-failed-p nil)
           (cancel-calls nil)
           (cancel-condition
             (make-condition 'simple-error
                             :format-control "Unknown peer stream cancel failed."))
           (close-condition
             (make-condition 'simple-error
                             :format-control "First stream close failed."))
           (client
             (http-kit/http3:make-http3-client
              :open-stream
              (lambda (request &key stream-type timeout deadline)
                (declare (ignore request timeout deadline))
                (let ((stream (make-http3-test-stream :kind stream-type)))
                  (setf client-streams (append client-streams (list stream)))
                  stream))
              :write-stream
              (lambda (stream octets &key fin-p timeout deadline)
                (declare (ignore stream octets fin-p timeout deadline)))
              :read-stream
              (lambda (stream &key timeout deadline)
                (declare (ignore timeout deadline))
                (let ((entry (pop (http3-test-stream-reads stream))))
                  (if entry
                      (values (first entry) (second entry))
                      (values nil t))))
              :close-stream
              (lambda (stream &key condition)
                (push (list stream condition) close-calls)
                (incf close-count)
                (when (= close-count 1)
                  (setf close-failed-p t)
                  (error close-condition))
                (setf (http3-test-stream-closed-p stream) t))
              :cancel-stream
              (lambda (stream &key error-code timeout deadline)
                (declare (ignore timeout deadline))
                (push (list stream error-code) cancel-calls)
                (error cancel-condition))))
           (failure
             (http3-boundary-error
              (lambda ()
                (http-kit/http3:accept-http3-peer-unidirectional-stream
                 client unknown-stream))))
           (ordered-close-calls (nreverse close-calls)))
      (unwind-protect
           (progn
             (ensure-true (typep failure 'simple-error))
             (ensure-true (eq cancel-condition failure))
             (ensure-false (http-kit/http3:http3-client-open-p client))
             (ensure-equal 2 (length cancel-calls))
             (ensure-true
              (every (lambda (call)
                       (and (eq unknown-stream (first call))
                            (= #x103 (second call))))
                     cancel-calls))
             (ensure-equal 4 (length ordered-close-calls))
             (ensure-true close-failed-p)
             (ensure-true
              (every #'eq
                     (append client-streams (list unknown-stream))
                     (mapcar #'first ordered-close-calls)))
             (ensure-true
              (every (lambda (call) (eq cancel-condition (second call)))
                     ordered-close-calls))
             (ensure-true
              (every #'http3-test-stream-closed-p
                     (rest (append client-streams (list unknown-stream)))))
             (ensure-false
              (http3-test-stream-closed-p (first client-streams))))
        (when (http-kit/http3:http3-client-open-p client)
          (handler-case
              (http-kit/http3:close-http3-client client)
            (error (condition) (declare (ignore condition))))))))
)


