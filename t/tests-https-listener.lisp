(in-package #:http-kit/test)

#+sbcl
(progn
  (defstruct (%https-listener-test-context
               (:constructor %make-https-listener-test-context))
    (lock (sb-thread:make-mutex :name "https listener test"))
    (waitqueue (sb-thread:make-waitqueue :name "https listener test"))
    (alpn "http/1.1")
    (handshake-action
      (lambda (stream timeout deadline)
        (declare (ignore timeout deadline))
        stream))
    (session-action
      (lambda (protocol stream handler options)
        (declare (ignore protocol stream handler options))))
    wrapper-options
    wrapper-call-options
    (wrapper-calls 0)
    (session-calls nil)
    (events nil)
    (errors nil)
    (handshakes-entered 0)
    (release-handshakes-p nil)
    (shutdown-observed-p nil))

  (defun %https-listener-test-symbols ()
    (list 'http-kit/tls:make-http-tls-server-wrapper
          'http-kit/tls:http-tls-selected-alpn-protocol
          'http-kit:serve-http1-session
          'http-kit/http2:serve-http2-session))

  (defun %https-listener-test-install-stubs (context)
    (setf (fdefinition 'http-kit/tls:make-http-tls-server-wrapper)
          (lambda (&key certificate-chain private-key ticket-key-store
                         alpn-protocols clock-function)
            (sb-thread:with-mutex
                ((%https-listener-test-context-lock context))
              (setf (%https-listener-test-context-wrapper-options context)
                    (list :certificate-chain certificate-chain
                          :private-key private-key
                          :ticket-key-store ticket-key-store
                          :alpn-protocols alpn-protocols
                          :clock-function clock-function)))
            (lambda (stream &key timeout deadline &allow-other-keys)
              (let ((action nil))
                (sb-thread:with-mutex
                    ((%https-listener-test-context-lock context))
                  (incf (%https-listener-test-context-wrapper-calls context))
                  (setf (%https-listener-test-context-wrapper-call-options
                         context)
                        (list :timeout timeout :deadline deadline)
                        action
                        (%https-listener-test-context-handshake-action
                         context)))
                (funcall action stream timeout deadline))))
          (fdefinition 'http-kit/tls:http-tls-selected-alpn-protocol)
          (lambda (stream)
            (declare (ignore stream))
            (sb-thread:with-mutex
                ((%https-listener-test-context-lock context))
              (%https-listener-test-context-alpn context)))
          (fdefinition 'http-kit:serve-http1-session)
          (lambda (stream handler &rest options)
            (%https-listener-test-dispatch-session
             context :http1 stream handler options))
          (fdefinition 'http-kit/http2:serve-http2-session)
          (lambda (stream handler &rest options)
            (%https-listener-test-dispatch-session
             context :http2 stream handler options)))
    context)

  (defun %https-listener-test-dispatch-session
      (context protocol stream handler options)
    (let ((action nil))
      (sb-thread:with-mutex
          ((%https-listener-test-context-lock context))
        (push (list protocol handler options)
              (%https-listener-test-context-session-calls context))
        (setf action (%https-listener-test-context-session-action context)))
      (funcall action protocol stream handler options)))

  (defun %https-listener-test-restore-stubs (originals)
    (loop for symbol in (%https-listener-test-symbols)
          for original in originals
          do (setf (fdefinition symbol) original))
    nil)

  (defmacro %with-https-listener-test-stubs ((context) &body body)
    (let ((originals (gensym "ORIGINALS")))
      `(let* ((,context (%make-https-listener-test-context))
              (,originals
                (mapcar #'fdefinition (%https-listener-test-symbols))))
         (unwind-protect
              (progn
                (%https-listener-test-install-stubs ,context)
                ,@body)
           (%https-listener-test-restore-stubs ,originals)))))

  (defun %https-listener-make-server (&rest overrides)
    (let ((options
            (list :transport :https
                  :host "127.0.0.1"
                  :port 0
                  :handler (lambda (request)
                             (declare (ignore request))
                             (make-http-response :status 204))
                  :certificate-chain '(:test-certificate)
                  :private-key "https-listener-private-key-marker")))
      (loop for (key value) on overrides by #'cddr
            do (setf (getf options key) value))
      (apply #'make-http-server options)))

  (defun %https-listener-connect (server)
    (open-http-tcp-stream
     (make-http-request
      :method "GET"
      :uri (format nil "http://127.0.0.1:~D/" (http-server-port server)))
     :timeout 2))

  (defun %https-listener-wait (predicate &optional (timeout 5))
    (let ((deadline
            (+ (get-internal-real-time)
               (* timeout internal-time-units-per-second))))
      (loop
        (when (funcall predicate)
          (return t))
        (when (>= (get-internal-real-time) deadline)
          (error "Timed out waiting for HTTPS listener test state."))
        (sleep 0.005))))

  (defun %https-listener-record-event (context event)
    (sb-thread:with-mutex
        ((%https-listener-test-context-lock context))
      (push event (%https-listener-test-context-events context))
      (sb-thread:condition-broadcast
       (%https-listener-test-context-waitqueue context)))
    event)

  (defun %https-listener-record-error (context condition details)
    (sb-thread:with-mutex
        ((%https-listener-test-context-lock context))
      (push (list condition details)
            (%https-listener-test-context-errors context))
      (sb-thread:condition-broadcast
       (%https-listener-test-context-waitqueue context)))
    nil)

  (defun %https-listener-session-count (context)
    (sb-thread:with-mutex
        ((%https-listener-test-context-lock context))
      (length (%https-listener-test-context-session-calls context))))

  (defun %https-listener-events (context)
    (sb-thread:with-mutex
        ((%https-listener-test-context-lock context))
      (copy-list (%https-listener-test-context-events context))))

  (defun %https-listener-errors (context)
    (sb-thread:with-mutex
        ((%https-listener-test-context-lock context))
      (copy-list (%https-listener-test-context-errors context))))

  (defun %https-listener-connection-count (server)
    (http-kit::%with-http-server-lock (server)
      (http-kit::http-server-connection-count server)))

  (defun %https-listener-handshake-count (server)
    (http-kit::%with-http-server-lock (server)
      (http-kit::http-server-handshake-count server)))

  (defun %https-listener-event-present-p (context type reason-code)
    (some (lambda (event)
            (and (eq (getf event :type) type)
                 (or (null reason-code)
                     (eq (getf event :reason-code) reason-code))))
          (%https-listener-events context)))

  (deftest https-listener-validates-finite-configuration
    (%with-https-listener-test-stubs (context)
      (let* ((server (%https-listener-make-server))
             (store (http-kit::http-server-ticket-key-store server)))
        (ensure-true (http-server-p server))
        (ensure-equal 0 (http-server-port server))
        (ensure-equal 128 (http-kit::http-server-max-connections server))
        (ensure-equal 32 (http-kit::http-server-max-handshakes server))
        (ensure-equal 100
                      (http-kit::http-server-max-concurrent-streams server))
        (ensure-equal 10 (http-kit::http-server-handshake-timeout server))
        (ensure-equal 30 (http-kit::http-server-request-timeout server))
        (ensure-equal 60 (http-kit::http-server-idle-timeout server))
        (ensure-equal 30 (http-kit::http-server-graceful-timeout server))
        (ensure-equal 65536 (http-kit::http-server-max-header-bytes server))
        (ensure-equal 100 (http-kit::http-server-max-fields server))
        (ensure-equal 16777216 (http-kit::http-server-max-body-bytes server))
        (ensure-true (cl-tls-kit:tls13-server-ticket-key-store-p store))
        (ensure-equal nil (http-server-failure server))
        (ensure-equal store
                      (getf (%https-listener-test-context-wrapper-options
                             context)
                            :ticket-key-store)))
      (dolist (overrides
                '((:transport :http)
                  (:host "")
                  (:port -1)
                  (:port 65536)
                  (:handler nil)
                  (:certificate-chain nil)
                  (:private-key nil)
                  (:max-connections nil)
                  (:max-connections 0)
                  (:max-handshakes nil)
                  (:max-concurrent-streams nil)
                  (:max-header-bytes nil)
                  (:max-fields nil)
                  (:max-body-bytes nil)
                  (:handshake-timeout nil)
                  (:handshake-timeout 0)
                  (:request-timeout nil)
                  (:idle-timeout nil)
                  (:graceful-timeout nil)
                  (:max-handshakes 2 :max-connections 1)))
        (signals http-server-error
          (apply #'%https-listener-make-server overrides)))))

  (deftest https-listener-interoperates-with-curl-and-openssl
    (let* ((material (%tls-server-test-material))
           (error-lock (sb-thread:make-mutex :name "HTTPS listener errors"))
           (server-errors nil)
           (server
             (make-http-server
              :host "127.0.0.1"
              :port 0
              :handler (lambda (request)
                         (declare (ignore request))
                         (make-http-response :status 200
                                             :body (ascii "listener-ok")))
              :certificate-chain (getf material :certificate-chain)
              :private-key (getf material :private-key)
              :handshake-timeout 5
              :request-timeout 5
              :idle-timeout 5
              :graceful-timeout 2
              :on-error
              (lambda (condition context)
                (sb-thread:with-mutex (error-lock)
                  (push (list (http-server-error-reason-code condition)
                              (http-server-error-phase condition)
                              context)
                        server-errors)))))
           (port nil)
           (url nil))
      (unwind-protect
           (progn
             (start-http-server server)
             (setf port (http-server-port server)
                   url (format nil "https://localhost:~D/" port))
             (dolist (protocol '("http/1.1" "h2"))
               (multiple-value-bind (output errors status)
                   (uiop:run-program
                    (list "curl" "--insecure" "--show-error" "--verbose"
                          "--fail" "--max-time" "10" "--noproxy" "*"
                          "--resolve"
                          (format nil "localhost:~D:127.0.0.1" port)
                          (if (string= protocol "h2") "--http2" "--http1.1")
                          "--output" "-"
                          "--write-out" (format nil "~%%{http_version}")
                          url)
                    :output :string
                    :error-output :string
                    :ignore-error-status t)
                 (unless (zerop status)
                   (error "curl ~A request failed (~D): ~A~%listener errors: ~S"
                          protocol status errors
                          (sb-thread:with-mutex (error-lock)
                            (reverse server-errors))))
                 (ensure-equal 0 status)
                 (ensure-true (search "listener-ok" output))
                 (let ((version-start
                         (position #\Newline output :from-end t)))
                   (ensure-true version-start)
                   (ensure-true
                    (member
                     (subseq output (1+ version-start))
                     (if (string= protocol "h2")
                         '("2" "2.0")
                         '("1.1"))
                     :test #'string=)))))
             (dolist (protocol '("http/1.1" "h2"))
               (multiple-value-bind (output errors status)
                   (uiop:run-program
                    (list "openssl" "s_client"
                          "-connect" (format nil "127.0.0.1:~D" port)
                          "-servername" "localhost"
                          "-alpn" protocol)
                    :input #P"/dev/null"
                    :output :string
                    :error-output :string
                    :ignore-error-status t)
                 (ensure-equal 0 status)
                 (unless
                     (or (search (format nil "ALPN protocol: ~A" protocol)
                                 output)
                         (search (format nil "ALPN protocol: ~A" protocol)
                                 errors))
                   (error "openssl s_client did not report ALPN ~A: stdout=~S stderr=~S"
                          protocol output errors))))
             (ensure-equal :stopped
                           (http-kit::http-server-state
                            (stop-http-server server :graceful-timeout 2))))
        (when (eq :running (http-kit::http-server-state server))
          (stop-http-server server :graceful-timeout 2))
        (%tls-server-test-remove-material material))))

  (deftest https-listener-routes-negotiated-alpn
    (%with-https-listener-test-stubs (context)
      (let ((server (%https-listener-make-server)))
        (unwind-protect
             (progn
               (start-http-server server)
               (ensure-true (plusp (http-server-port server)))
               (dolist (case '(("h2" :http2) ("http/1.1" :http1)))
                 (destructuring-bind (alpn expected-protocol) case
                   (sb-thread:with-mutex
                       ((%https-listener-test-context-lock context))
                     (setf (%https-listener-test-context-alpn context) alpn))
                   (let ((client (%https-listener-connect server)))
                     (close-http-tcp-stream client))
                   (%https-listener-wait
                    (lambda ()
                      (>= (%https-listener-session-count context)
                          (if (eq expected-protocol :http2) 1 2))))
                   (%https-listener-wait
                    (lambda ()
                      (zerop (%https-listener-connection-count server))))))
               (let ((routes
                       (sb-thread:with-mutex
                           ((%https-listener-test-context-lock context))
                         (mapcar #'first
                                 (reverse
                                  (%https-listener-test-context-session-calls
                                   context))))))
                 (ensure-equal '(:http2 :http1) routes))
               (let ((wrapper-options
                       (%https-listener-test-context-wrapper-options context))
                     (wrapper-call-options
                       (%https-listener-test-context-wrapper-call-options
                        context)))
                 (ensure-equal '("h2" "http/1.1")
                               (getf wrapper-options :alpn-protocols))
                 (ensure-equal "https-listener-private-key-marker"
                               (getf wrapper-options :private-key))
                 (ensure-true
                  (eq (getf wrapper-options :ticket-key-store)
                      (http-kit::http-server-ticket-key-store server)))
                 (ensure-equal 10 (getf wrapper-call-options :timeout))
                 (ensure-true (numberp (getf wrapper-call-options :deadline)))
                 (ensure-equal 2
                               (%https-listener-test-context-wrapper-calls
                                context))))
          (stop-http-server server :graceful-timeout 1)))))

  (deftest https-listener-enforces-handshake-cap-and-recovers
    (%with-https-listener-test-stubs (context)
      (let ((server nil)
            (first-client nil)
            (second-client nil))
        (setf (%https-listener-test-context-handshake-action context)
              (lambda (stream timeout deadline)
                (declare (ignore timeout deadline))
                (sb-thread:with-mutex
                    ((%https-listener-test-context-lock context))
                  (incf (%https-listener-test-context-handshakes-entered
                         context))
                  (sb-thread:condition-broadcast
                   (%https-listener-test-context-waitqueue context))
                  (loop until (%https-listener-test-context-release-handshakes-p
                               context)
                        do (sb-thread:condition-wait
                            (%https-listener-test-context-waitqueue context)
                            (%https-listener-test-context-lock context)
                            :timeout 0.1)))
                stream))
        (unwind-protect
             (progn
               (setf server
                     (%https-listener-make-server
                      :max-connections 2
                      :max-handshakes 1
                      :on-event
                      (lambda (event)
                        (http-server-failure server)
                        (%https-listener-record-event context event))))
               (start-http-server server)
               (setf first-client (%https-listener-connect server))
               (close-http-tcp-stream first-client)
               (%https-listener-wait
                (lambda ()
                  (sb-thread:with-mutex
                      ((%https-listener-test-context-lock context))
                    (= 1
                       (%https-listener-test-context-handshakes-entered
                        context)))))
               (setf second-client (%https-listener-connect server))
               (close-http-tcp-stream second-client)
               (%https-listener-wait
                (lambda ()
                  (%https-listener-event-present-p
                   context :connection-rejected :max-handshakes)))
               (ensure-equal 1 (%https-listener-handshake-count server))
               (ensure-equal 1 (%https-listener-connection-count server))
               (ensure-equal 1
                             (%https-listener-test-context-wrapper-calls
                              context))
               (sb-thread:with-mutex
                   ((%https-listener-test-context-lock context))
                 (setf (%https-listener-test-context-release-handshakes-p
                        context)
                       t)
                 (sb-thread:condition-broadcast
                  (%https-listener-test-context-waitqueue context)))
               (%https-listener-wait
                (lambda ()
                  (and (zerop (%https-listener-handshake-count server))
                       (zerop (%https-listener-connection-count server)))))
               (let ((client (%https-listener-connect server)))
                 (close-http-tcp-stream client))
               (%https-listener-wait
                (lambda ()
                  (and (= 2 (%https-listener-test-context-wrapper-calls
                             context))
                       (= 2 (%https-listener-session-count context)))))
               (ensure-equal 0 (%https-listener-handshake-count server))
               (ensure-equal 0 (%https-listener-connection-count server)))
          (when server
            (sb-thread:with-mutex
                ((%https-listener-test-context-lock context))
              (setf (%https-listener-test-context-release-handshakes-p context)
                    t)
              (sb-thread:condition-broadcast
               (%https-listener-test-context-waitqueue context)))
            (stop-http-server server :graceful-timeout 1))))))

  (deftest https-listener-enforces-connection-cap-and-graceful-stop
    (%with-https-listener-test-stubs (context)
      (let ((server nil))
        (setf (%https-listener-test-context-session-action context)
              (lambda (protocol stream handler options)
                (declare (ignore protocol stream handler))
                (let ((shutdown-p (getf options :shutdown-p)))
                  (loop repeat 1000
                        when (funcall shutdown-p)
                          do (setf (%https-listener-test-context-shutdown-observed-p
                                    context)
                                   t)
                             (return)
                        do (sleep 0.005)))))
        (unwind-protect
             (progn
               (setf server
                     (%https-listener-make-server
                      :max-connections 1
                      :max-handshakes 1
                      :on-event
                      (lambda (event)
                        (http-server-failure server)
                        (%https-listener-record-event context event))))
               (start-http-server server)
               (let ((client (%https-listener-connect server)))
                 (close-http-tcp-stream client))
               (%https-listener-wait
                (lambda () (= 1 (%https-listener-session-count context))))
               (let ((client (%https-listener-connect server)))
                 (close-http-tcp-stream client))
               (%https-listener-wait
                (lambda ()
                  (%https-listener-event-present-p
                   context :connection-rejected :max-connections)))
               (ensure-equal 1 (%https-listener-connection-count server))
               (stop-http-server server :graceful-timeout 2)
               (stop-http-server server :graceful-timeout 2)
               (ensure-equal 0 (%https-listener-connection-count server))
               (ensure-true
                (%https-listener-test-context-shutdown-observed-p context))
               (ensure-true
                (%https-listener-event-present-p context :listener-stopped nil))
               (ensure-true
                (eq :stopped (http-kit::http-server-state server))))
          (when server
            (stop-http-server server :graceful-timeout 1))))))

  (deftest https-listener-rejects-missing-and-unknown-alpn-safely
    (%with-https-listener-test-stubs (context)
      (let ((server nil))
        (unwind-protect
             (progn
               (setf server
                     (%https-listener-make-server
                      :on-event
                      (lambda (event)
                        (http-server-failure server)
                        (%https-listener-record-event context event))
                      :on-error
                      (lambda (condition details)
                        (http-server-failure server)
                        (%https-listener-record-error
                         context condition details))))
               (start-http-server server)
               (dolist (alpn '(nil "h3"))
                 (sb-thread:with-mutex
                     ((%https-listener-test-context-lock context))
                   (setf (%https-listener-test-context-alpn context) alpn))
                 (let ((client (%https-listener-connect server)))
                   (close-http-tcp-stream client))
                 (%https-listener-wait
                  (lambda ()
                    (>= (length (%https-listener-errors context))
                        (if alpn 2 1))))
                 (%https-listener-wait
                  (lambda ()
                    (zerop (%https-listener-connection-count server)))))
               (ensure-equal nil (%https-listener-test-context-session-calls
                                  context))
               (dolist (entry (%https-listener-errors context))
                 (ensure-equal :unsupported-alpn
                               (http-server-error-reason-code (first entry)))
                 (ensure-false
                  (search "https-listener-private-key-marker"
                          (princ-to-string entry))))
               (dolist (event (%https-listener-events context))
                 (ensure-false
                  (search "https-listener-private-key-marker"
                          (princ-to-string event))))
               (ensure-false
                (search "https-listener-private-key-marker"
                        (princ-to-string server))))
          (when server
            (stop-http-server server :graceful-timeout 1))))))

  (deftest https-listener-event-callback-failure-releases-connection
    (%with-https-listener-test-stubs (context)
      (let ((server nil))
        (unwind-protect
             (progn
               (setf server
                     (%https-listener-make-server
                      :on-event
                      (lambda (event)
                        (when (eq (getf event :type) :connection-accepted)
                          (error "event callback marker failure")))))
               (start-http-server server)
               (let ((client (%https-listener-connect server)))
                 (close-http-tcp-stream client))
               (%https-listener-wait
                (lambda () (http-server-failure server)))
               (%https-listener-wait
                (lambda ()
                  (and (zerop (%https-listener-connection-count server))
                       (zerop (%https-listener-handshake-count server)))))
               (let ((condition (http-server-failure server)))
                 (ensure-true (typep condition 'http-server-callback-error))
                 (ensure-equal :on-event
                               (http-server-callback-error-callback condition))
                 (ensure-equal :event-callback-failed
                               (http-server-error-reason-code condition)))
               (ensure-equal 0 (%https-listener-connection-count server)))
          (when server
            (stop-http-server server :graceful-timeout 1))))))

  (deftest https-listener-callback-failure-is-fatal-and-redacted
    (%with-https-listener-test-stubs (context)
      (let ((server nil))
        (unwind-protect
             (progn
               (setf server
                     (%https-listener-make-server
                      :on-event
                      (lambda (event)
                        (%https-listener-record-event context event))
                      :on-error
                      (lambda (condition details)
                        (declare (ignore condition details))
                        (error "callback private marker failure"))))
               (sb-thread:with-mutex
                   ((%https-listener-test-context-lock context))
                 (setf (%https-listener-test-context-alpn context) "h3"))
               (start-http-server server)
               (let ((client (%https-listener-connect server)))
                 (close-http-tcp-stream client))
               (%https-listener-wait
                (lambda () (http-server-failure server)))
               (let ((condition (http-server-failure server)))
                 (ensure-true (typep condition 'http-server-callback-error))
                 (ensure-equal :on-error
                               (http-server-callback-error-callback condition))
                 (ensure-equal :error-callback-failed
                               (http-server-error-reason-code condition))
                 (ensure-false
                  (search "callback private marker failure"
                          (princ-to-string condition)))
                 (ensure-false
                  (search "https-listener-private-key-marker"
                          (princ-to-string condition))))
               (ensure-false
                (%https-listener-event-present-p context :alpn-selected nil)))
          (when server
            (stop-http-server server :graceful-timeout 1)))))))

