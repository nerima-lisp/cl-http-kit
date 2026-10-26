(in-package #:http-kit/test)
#+sbcl
(deftest https-listener-shares-body-budget-across-protocols
  (let* ((context
           (funcall (symbol-function '%make-https-listener-test-context)))
         (originals
           (mapcar #'fdefinition
                   (funcall
                    (symbol-function '%https-listener-test-symbols))))
         (server nil))
    (unwind-protect
         (progn
           (funcall
            (symbol-function '%https-listener-test-install-stubs) context)
           (let ((default-server
                   (funcall
                    (symbol-function '%https-listener-make-server))))
             (ensure-equal
              (* 64 1024 1024)
              (http-kit::http-server-max-buffered-body-bytes
               default-server)))
           (let ((derived-server
                   (funcall
                    (symbol-function '%https-listener-make-server)
                    :max-body-bytes 67108865)))
             (ensure-equal
              134217730
              (http-kit::http-server-max-buffered-body-bytes
               derived-server)))
           (signals http-server-error
             (funcall
              (symbol-function '%https-listener-make-server)
              :max-buffered-body-bytes nil))
           (setf server
                 (funcall
                  (symbol-function '%https-listener-make-server)
                  :max-buffered-body-bytes 5))
           (start-http-server server)
           (let ((client
                   (funcall
                    (symbol-function '%https-listener-connect) server)))
             (close-http-tcp-stream client))
           (funcall
            (symbol-function '%https-listener-wait)
            (lambda ()
              (= 1
                 (funcall
                  (symbol-function '%https-listener-session-count) context))))
           (funcall
            (fdefinition
             '(setf %https-listener-test-context-alpn))
            "h2" context)
           (let ((client
                   (funcall
                    (symbol-function '%https-listener-connect) server)))
             (close-http-tcp-stream client))
           (funcall
            (symbol-function '%https-listener-wait)
            (lambda ()
              (and
               (= 2
                  (funcall
                   (symbol-function '%https-listener-session-count)
                   context))
               (zerop
                (funcall
                 (symbol-function '%https-listener-connection-count) server)))))
           (let* ((calls
                    (sb-thread:with-mutex
                        ((funcall
                          (symbol-function '%https-listener-test-context-lock)
                          context))
                      (reverse
                       (funcall
                        (symbol-function
                         '%https-listener-test-context-session-calls)
                        context))))
                  (first-call (first calls))
                  (second-call (second calls))
                  (first-options (third first-call))
                  (second-options (third second-call))
                  (reserve-first (getf first-options :reserve-body-bytes))
                  (release-first (getf first-options :release-body-bytes))
                  (reserve-second (getf second-options :reserve-body-bytes))
                  (release-second (getf second-options :release-body-bytes)))
             (ensure-equal 2 (length calls))
             (ensure-equal :http1 (first first-call))
             (ensure-equal :http2 (first second-call))
             (funcall reserve-first 3)
             (let ((observed
                     (handler-case
                         (progn
                           (funcall reserve-second 3)
                           nil)
                       (http-size-limit-exceeded (condition) condition))))
               (ensure-true
                (typep observed 'http-size-limit-exceeded))
               (ensure-equal
                5 (http-size-limit-exceeded-limit observed))
               (ensure-equal
                6 (http-size-limit-exceeded-observed observed)))
             (ensure-equal
              3 (http-kit::http-server-buffered-body-bytes server))
             (funcall release-first 3)
             (ensure-equal
              0 (http-kit::http-server-buffered-body-bytes server))
             (funcall reserve-second 2)
             (funcall release-second 2)
             (ensure-equal
              0 (http-kit::http-server-buffered-body-bytes server)))
           (stop-http-server server :graceful-timeout 1)
           (setf server nil))
      (when server
        (stop-http-server server :graceful-timeout 1))
      (funcall
       (symbol-function '%https-listener-test-restore-stubs)
       originals))))
#+sbcl
(deftest https-listener-restarts-without-stale-acceptors
  (let* ((context (funcall (symbol-function '%make-https-listener-test-context)))
         (originals
           (mapcar #'fdefinition
                   (funcall
                    (symbol-function '%https-listener-test-symbols))))
         (server nil))
    (unwind-protect
         (progn
           (funcall
            (symbol-function '%https-listener-test-install-stubs)
            context)
           (setf server
                 (funcall
                  (symbol-function '%https-listener-make-server)
                  :on-event
                  (lambda (event)
                    (funcall
                     (symbol-function '%https-listener-record-event)
                     context event))))
           (labels ((event-count (type)
                      (count type
                             (funcall
                              (symbol-function '%https-listener-events)
                              context)
                             :key (lambda (event) (getf event :type))
                             :test #'eq)))
             (loop for iteration from 1 to 32
                   do (setf (http-kit::http-server-port server) 0)
                      (start-http-server server)
                      (let* ((supervisor
                               (http-kit::http-server-supervisor server))
                             (client
                               (funcall
                                (symbol-function '%https-listener-connect)
                                server)))
                        (close-http-tcp-stream client)
                        (funcall
                         (symbol-function '%https-listener-wait)
                         (lambda ()
                           (>= (event-count :connection-accepted)
                               iteration)))
                        (funcall
                         (symbol-function '%https-listener-wait)
                         (lambda ()
                           (>= (event-count :connection-closed)
                               iteration)))
                        (stop-http-server server :graceful-timeout 1)
                        (ensure-false
                         (sb-thread:thread-alive-p supervisor))
                        (ensure-equal
                         iteration
                         (event-count :connection-accepted))
                        (ensure-equal :stopped
                                      (http-kit::http-server-state server))))))
      (when (and server
                 (not (eq :stopped
                          (http-kit::http-server-state server))))
        (stop-http-server server :graceful-timeout 1))
      (funcall
       (symbol-function '%https-listener-test-restore-stubs)
       originals))))

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

  (defun %https-listener-socket-options (server)
    (let* ((listener (http-kit::http-server-listener server))
           (socket
             (and listener
                  (http-kit/network::http-network-listener-socket listener))))
      (list :so-reuseaddr
            (and socket (sb-bsd-sockets:sockopt-reuse-address socket))
            :so-reuseport :not-configured-by-http-listener)))

  (defun %https-listener-ss-snapshot (port)
    (handler-case
        (multiple-value-bind (output errors status)
            (uiop:run-program
             (list "ss" "-ltnp" (format nil "sport = :~D" port))
             :output :string
             :error-output :string
             :ignore-error-status t)
          (list :status status
                :listening-socket-count
                (with-input-from-string (stream output)
                  (loop for line = (read-line stream nil nil)
                        while line
                        count (and (>= (length line) 6)
                                   (string= line "LISTEN"
                                            :end1 6
                                            :end2 6))))
                :output output
                :errors errors))
      (error (condition)
        (list :status :unavailable
              :error (princ-to-string condition)))))

  (defun %https-listener-network-diagnostics (server)
    (let* ((listener (http-kit::http-server-listener server))
           (port (http-kit::http-server-port server)))
      (list :address
            (and listener
                 (http-kit/network:http-network-listener-address listener))
            :port port
            :socket-options (%https-listener-socket-options server)
            :same-port-sockets (%https-listener-ss-snapshot port))))

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
           (observation-lock
             (sb-thread:make-mutex :name "HTTPS listener observations"))
           (server-events nil)
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
              :on-event
              (lambda (event)
                (sb-thread:with-mutex (observation-lock)
                  (push (copy-list event) server-events)))
              :on-error
              (lambda (condition context)
                (sb-thread:with-mutex (observation-lock)
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
                          "--trace" "/dev/stderr"
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
                   (let ((state-before-cleanup
                           (http-kit::http-server-state server))
                         (failure-time (get-universal-time))
                         (connection-count-before-cleanup
                           (%https-listener-connection-count server))
                         (handshake-count-before-cleanup
                           (%https-listener-handshake-count server))
                         (network-diagnostics-before-cleanup
                           (%https-listener-network-diagnostics server))
                         (events-before-cleanup nil)
                         (listener-errors-before-cleanup nil))
                     (sb-thread:with-mutex (observation-lock)
                       (setf events-before-cleanup
                             (reverse (copy-list server-events))
                             listener-errors-before-cleanup
                             (reverse (copy-list server-errors))))
                     (unless (eq :stopped state-before-cleanup)
                       (stop-http-server
                        server :graceful-timeout 2))
                     (let ((wait-error
                             (handler-case
                                 (progn
                                   (%https-listener-wait
                                    (lambda ()
                                      (zerop
                                       (%https-listener-connection-count
                                        server))))
                                   nil)
                               (error (condition) condition))))
                       (let ((connection-count-after-cleanup
                               (%https-listener-connection-count server))
                             (handshake-count-after-cleanup
                               (%https-listener-handshake-count server))
                             (events-after-cleanup nil)
                             (listener-errors-after-cleanup nil))
                         (sb-thread:with-mutex (observation-lock)
                           (setf events-after-cleanup
                                 (reverse (copy-list server-events))
                                 listener-errors-after-cleanup
                                 (reverse (copy-list server-errors))))
                         (multiple-value-bind (curl-version-output
                                               curl-version-errors
                                               curl-version-status)
                             (uiop:run-program
                              (list "curl" "--version")
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
                           (error
                            "curl ~A request failed (~D): ~A~%curl version (~D): stdout=~S stderr=~S~%failure time (UTC universal seconds): ~D~%listener config: handshake-timeout=~D max-connections=~D max-handshakes=~D~%listener state before cleanup: ~S~%listener counts before cleanup: connections=~D handshakes=~D~%listener events before cleanup: ~S~%listener errors before cleanup: ~S~%listener state after cleanup: ~S~%listener counts after cleanup: connections=~D handshakes=~D~%listener events after cleanup: ~S~%listener errors after cleanup: ~S~%listener wait error: ~S~%listener network diagnostics at failure: ~S"
                            protocol status errors curl-version-status
                            curl-version-output curl-version-errors
                            failure-time
                            (http-kit::http-server-handshake-timeout server)
                            (http-kit::http-server-max-connections server)
                            (http-kit::http-server-max-handshakes server)
                            state-before-cleanup
                            connection-count-before-cleanup
                            handshake-count-before-cleanup
                            events-before-cleanup listener-errors-before-cleanup
                            (http-kit::http-server-state server)
                            connection-count-after-cleanup
                            handshake-count-after-cleanup
                            events-after-cleanup listener-errors-after-cleanup
                            wait-error
                            network-diagnostics-before-cleanup))))))
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
                    (list "openssl" "s_client" "-trace"
                          "-groups"
                          "X25519MLKEM768:X25519:secp256r1:X448:secp384r1:secp521r1:ffdhe2048:ffdhe3072"
                          "-ign_eof"
                          "-ignore_unexpected_eof"
                          "-connect" (format nil "127.0.0.1:~D" port)
                          "-servername" "localhost"
                          "-alpn" protocol)
                    :input #P"/dev/null"
                    :output :string
                    :error-output :string
                    :ignore-error-status t)
                 (unless (zerop status)
                   (let ((state-before-cleanup
                           (http-kit::http-server-state server))
                         (failure-time (get-universal-time))
                         (connection-count-before-cleanup
                           (%https-listener-connection-count server))
                         (network-diagnostics-before-cleanup
                           (%https-listener-network-diagnostics server))
                         (events-before-cleanup nil)
                         (listener-errors-before-cleanup nil))
                     (sb-thread:with-mutex (observation-lock)
                       (setf events-before-cleanup
                             (reverse (copy-list server-events))
                             listener-errors-before-cleanup
                             (reverse (copy-list server-errors))))
                     (multiple-value-bind (version-output version-errors
                                           version-status)
                         (uiop:run-program
                          (list "openssl" "version")
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
                       (error
                        "openssl s_client for ALPN ~A failed (~D); OpenSSL version (~D): stdout=~S stderr=~S~%s_client stdout=~S stderr=~S~%failure time (UTC universal seconds): ~D~%listener config: handshake-timeout=~D max-connections=~D max-handshakes=~D~%listener state before cleanup: ~S~%listener connection count before cleanup: ~D~%listener events before cleanup: ~S~%listener errors before cleanup: ~S~%listener network diagnostics at failure: ~S"
                        protocol status version-status version-output
                        version-errors output errors failure-time
                        (http-kit::http-server-handshake-timeout server)
                        (http-kit::http-server-max-connections server)
                        (http-kit::http-server-max-handshakes server)
                        state-before-cleanup
                        connection-count-before-cleanup events-before-cleanup
                        listener-errors-before-cleanup
                        network-diagnostics-before-cleanup))))
                 (ensure-equal 0 status)
                 (unless
                     (or (search (format nil "ALPN protocol: ~A" protocol)
                                 output)
                         (search (format nil "ALPN protocol: ~A" protocol)
                                 errors))
                   (let ((state-before-cleanup
                           (http-kit::http-server-state server))
                         (failure-time (get-universal-time))
                         (connection-count-before-cleanup
                           (%https-listener-connection-count server))
                         (network-diagnostics-before-cleanup
                           (%https-listener-network-diagnostics server))
                         (events-before-cleanup nil)
                         (listener-errors-before-cleanup nil))
                     (sb-thread:with-mutex (observation-lock)
                       (setf events-before-cleanup
                             (reverse (copy-list server-events))
                             listener-errors-before-cleanup
                             (reverse (copy-list server-errors))))
                     (multiple-value-bind (version-output version-errors
                                           version-status)
                         (uiop:run-program
                          (list "openssl" "version")
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
                       (error
                        "openssl s_client did not report ALPN ~A; OpenSSL version (~D): stdout=~S stderr=~S~%s_client stdout=~S stderr=~S~%failure time (UTC universal seconds): ~D~%listener config: handshake-timeout=~D max-connections=~D max-handshakes=~D~%listener state before cleanup: ~S~%listener connection count before cleanup: ~D~%listener events before cleanup: ~S~%listener errors before cleanup: ~S~%listener network diagnostics at failure: ~S"
                        protocol version-status version-output version-errors
                        output errors failure-time
                        (http-kit::http-server-handshake-timeout server)
                        (http-kit::http-server-max-connections server)
                        (http-kit::http-server-max-handshakes server)
                        state-before-cleanup
                        connection-count-before-cleanup events-before-cleanup
                        listener-errors-before-cleanup
                        network-diagnostics-before-cleanup))))))
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
                        (%https-listener-record-event context event))
                      :on-error
                      (lambda (condition details)
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

  (deftest https-listener-alpn-event-callback-failure-releases-connection
    (%with-https-listener-test-stubs (context)
      (let ((server nil))
        (unwind-protect
             (progn
               (setf server
                     (%https-listener-make-server
                      :on-event
                      (lambda (event)
                        (when (eq (getf event :type) :alpn-selected)
                          (%https-listener-record-event context event)
                          (error "ALPN event callback failure")))))
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
               (ensure-true
                (%https-listener-event-present-p context :alpn-selected nil))
               (ensure-equal 0 (%https-listener-connection-count server)))
          (when server
            (stop-http-server server :graceful-timeout 1))))))

  (deftest https-listener-accept-event-callback-failure-releases-connection
    (%with-https-listener-test-stubs (context)
      (let ((server nil))
        (unwind-protect
             (progn
               (setf server
                     (%https-listener-make-server
                      :on-event
                      (lambda (event)
                        (when (eq (getf event :type) :connection-accepted)
                          (error "accept event callback failure")))))
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
                 (setf (%https-listener-test-context-alpn context) nil))
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

#+sbcl
(deftest https-listener-covers-configuration-failure-paths
  (%with-https-listener-test-stubs (context)
    (ensure-true (%https-listener-test-context-p context))
    (dolist (overrides
              '((:on-event :invalid-callback)
                (:on-error :invalid-callback)
                (:ticket-key-store :invalid-store)))
      (let ((condition
              (handler-case
                  (progn
                    (apply #'%https-listener-make-server overrides)
                    nil)
                (http-server-error (condition) condition))))
        (ensure-true condition)
        (ensure-equal
         (if (eq (getf overrides :ticket-key-store) :invalid-store)
             :invalid-ticket-key-store
             (if (eq (getf overrides :on-event) :invalid-callback)
                 :invalid-event-callback
                 :invalid-error-callback))
         (http-server-error-reason-code condition))))
    (let ((nan (sb-kernel:make-single-float #x7fc00000))
          (infinity (sb-kernel:make-single-float #x7f800000)))
      (ensure-true (http-kit::%http-server-positive-real-p 1.5))
      (ensure-false (http-kit::%http-server-positive-real-p nan))
      (ensure-false (http-kit::%http-server-positive-real-p infinity)))
    (let ((factory (fdefinition 'cl-tls-kit:make-tls13-server-ticket-key-store)))
      (unwind-protect
           (progn
             (setf (fdefinition 'cl-tls-kit:make-tls13-server-ticket-key-store)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error "ticket store factory failure")))
             (let ((condition
                     (handler-case
                         (progn (%https-listener-make-server) nil)
                       (http-server-error (condition) condition))))
               (ensure-true condition)
               (ensure-equal :ticket-key-store-unavailable
                             (http-server-error-reason-code condition))))
        (setf (fdefinition 'cl-tls-kit:make-tls13-server-ticket-key-store)
              factory)))
    (let ((wrapper-factory
            (fdefinition 'http-kit/tls:make-http-tls-server-wrapper)))
      (unwind-protect
           (progn
             (setf (fdefinition 'http-kit/tls:make-http-tls-server-wrapper)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     :not-a-wrapper))
             (let ((condition
                     (handler-case
                         (progn (%https-listener-make-server) nil)
                       (http-server-error (condition) condition))))
               (ensure-true condition)
               (ensure-equal :invalid-tls-wrapper
                             (http-server-error-reason-code condition)))
             (setf (fdefinition 'http-kit/tls:make-http-tls-server-wrapper)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error "TLS wrapper factory failure")))
             (let ((condition
                     (handler-case
                         (progn (%https-listener-make-server) nil)
                       (http-server-error (condition) condition))))
               (ensure-true condition)
               (ensure-equal :invalid-tls-credentials
                             (http-server-error-reason-code condition)))
             (setf (fdefinition 'http-kit/tls:make-http-tls-server-wrapper)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error 'http-server-error
                            :reason-code :wrapper-originated
                            :phase :wrapper)))
             (let ((condition
                     (handler-case
                         (progn (%https-listener-make-server) nil)
                       (http-server-error (condition) condition))))
               (ensure-true condition)
               (ensure-equal :wrapper-originated
                             (http-server-error-reason-code condition))))
        (setf (fdefinition 'http-kit/tls:make-http-tls-server-wrapper)
              wrapper-factory)))))
#+sbcl
(progn
  (defclass %https-listener-client-stream
      (sb-gray:fundamental-binary-input-stream
       sb-gray:fundamental-binary-output-stream)
    ((transport
       :initarg :transport
       :reader %https-listener-client-stream-transport)
     (mode
       :initarg :mode
       :reader %https-listener-client-stream-mode)
     (fragment-size
       :initarg :fragment-size
       :reader %https-listener-client-stream-fragment-size)
     (client-hello-p
       :initform t
       :accessor %https-listener-client-stream-client-hello-p)
     (record-count
       :initform 0
       :accessor %https-listener-client-stream-record-count)
     (lock
       :initform (sb-thread:make-mutex :name "HTTPS TLS client stream")
       :reader %https-listener-client-stream-lock)
     (waitqueue
       :initform (sb-thread:make-waitqueue :name "HTTPS TLS client stream")
       :reader %https-listener-client-stream-waitqueue)
     (client-hello-sent-p
       :initform nil
       :accessor %https-listener-client-stream-client-hello-sent-p)
     (release-p
       :initform nil
       :accessor %https-listener-client-stream-release-p)))

  (defmethod sb-gray:stream-read-byte ((stream %https-listener-client-stream))
    (read-byte (%https-listener-client-stream-transport stream) nil :eof))

  (defmethod sb-gray:stream-read-sequence
      ((stream %https-listener-client-stream) sequence &optional (start 0) end)
    (read-sequence sequence
                   (%https-listener-client-stream-transport stream)
                   :start start
                   :end end))

  (defmethod sb-gray:stream-write-byte
      ((stream %https-listener-client-stream) byte)
    (write-byte byte (%https-listener-client-stream-transport stream)))

  (defun %https-listener-client-hello-record-p (bytes)
    (and (>= (length bytes) 9)
         (= (aref bytes 0) cl-tls-kit:+tls-content-type-handshake+)
         (= (aref bytes 5) 1)
         (<= (+ 5 (ash (aref bytes 3) 8) (aref bytes 4))
             (length bytes))))

  (defun %https-listener-write-client-hello-fragments
      (stream bytes &optional (start-position 5))
    (let* ((transport (%https-listener-client-stream-transport stream))
           (fragment-size (%https-listener-client-stream-fragment-size stream))
           (record-end (+ 5 (ash (aref bytes 3) 8) (aref bytes 4)))
           (position start-position))
      (loop while (< position record-end)
            for end = (min record-end (+ position fragment-size))
            for length = (- end position)
            for header = (vector
                          cl-tls-kit:+tls-content-type-handshake+
                          (aref bytes 1)
                          (aref bytes 2)
                          (ldb (byte 8 8) length)
                          (ldb (byte 8 0) length))
            do (write-sequence header transport)
               (write-sequence bytes transport :start position :end end)
               (incf (%https-listener-client-stream-record-count stream))
               (setf position end))
      (when (< record-end (length bytes))
        (write-sequence bytes transport
                        :start record-end
                        :end (length bytes)))))

  (defun %https-listener-stream-write-client-hello-and-stall (stream bytes)
    (let* ((transport (%https-listener-client-stream-transport stream))
           (record-end (+ 5 (ash (aref bytes 3) 8) (aref bytes 4))))
      (write-sequence bytes transport :end record-end)
      (incf (%https-listener-client-stream-record-count stream))
      (finish-output transport)
      (sb-thread:with-mutex
          ((%https-listener-client-stream-lock stream))
        (setf (%https-listener-client-stream-client-hello-sent-p stream) t)
        (sb-thread:condition-broadcast
         (%https-listener-client-stream-waitqueue stream))
        (let ((deadline (+ (get-internal-real-time)
                           (* 10 internal-time-units-per-second))))
          (loop until (%https-listener-client-stream-release-p stream)
                do (when (>= (get-internal-real-time) deadline)
                     (error "Timed out waiting to release the TLS client stream."))
                   (sb-thread:condition-wait
                    (%https-listener-client-stream-waitqueue stream)
                    (%https-listener-client-stream-lock stream)
                    :timeout 0.1))))
      (when (< record-end (length bytes))
        (write-sequence bytes transport :start record-end))))

  (defmethod sb-gray:stream-write-sequence
      ((stream %https-listener-client-stream) sequence &optional (start 0) end)
    (let* ((end (or end (length sequence)))
           (bytes (subseq sequence start end))
           (transport (%https-listener-client-stream-transport stream)))
      (if (and (%https-listener-client-stream-client-hello-p stream)
               (%https-listener-client-hello-record-p bytes))
          (progn
            (setf (%https-listener-client-stream-client-hello-p stream) nil)
            (ecase (%https-listener-client-stream-mode stream)
              (:fragment
               (%https-listener-write-client-hello-fragments stream bytes))
              (:stall
               (%https-listener-stream-write-client-hello-and-stall
                stream bytes))))
          (write-sequence bytes transport))
      (finish-output transport)
      sequence))

  (defmethod sb-gray:stream-finish-output
      ((stream %https-listener-client-stream))
    (finish-output (%https-listener-client-stream-transport stream)))

  (defmethod open-stream-p ((stream %https-listener-client-stream))
    (open-stream-p (%https-listener-client-stream-transport stream)))

  (defmethod close ((stream %https-listener-client-stream) &key abort)
    (close (%https-listener-client-stream-transport stream) :abort abort))

  (defun %https-listener-open-real-tls-transport
      (port &key (mode :fragment) (fragment-size 16))
    (let* ((request
             (make-http-request
              :method "GET"
              :uri (format nil "http://127.0.0.1:~D/" port)))
           (transport (open-http-tcp-stream request :timeout 5)))
      (values
       (make-instance '%https-listener-client-stream
                      :transport transport
                      :mode mode
                      :fragment-size fragment-size)
       request)))

  (defun %https-listener-real-tls-upgrader (material &optional alpn-protocols)
    (make-http-tls-upgrader
     :verify :required
     :trust-anchors
     (list
      (cl-tls-kit.x509:parse-certificate-der
       (first (getf material :certificate-chain))))
     :alpn-protocols (or alpn-protocols '("http/1.1"))))

  (defun %https-listener-wait-for-client-hello (stream)
    (%https-listener-wait
     (lambda ()
       (sb-thread:with-mutex
           ((%https-listener-client-stream-lock stream))
         (%https-listener-client-stream-client-hello-sent-p stream)))
     5))

  (defun %https-listener-release-client-hello (stream)
    (sb-thread:with-mutex
        ((%https-listener-client-stream-lock stream))
      (setf (%https-listener-client-stream-release-p stream) t)
      (sb-thread:condition-broadcast
       (%https-listener-client-stream-waitqueue stream)))
    nil)

  (deftest https-listener-reassembles-fragmented-clienthello
    (let* ((material (%tls-server-test-material))
           (server
             (make-http-server
              :host "127.0.0.1"
              :port 0
              :handler (lambda (request)
                         (declare (ignore request))
                         (make-http-response :status 204))
              :certificate-chain (getf material :certificate-chain)
              :private-key (getf material :private-key)
              :handshake-timeout 5
              :request-timeout 5
              :idle-timeout 5
              :graceful-timeout 2))
           (transport nil)
           (tls-stream nil))
      (unwind-protect
           (progn
             (start-http-server server)
             (setf transport
                   (nth-value
                    0
                    (%https-listener-open-real-tls-transport
                     (http-server-port server)
                     :fragment-size 8)))
             (setf tls-stream
                   (funcall
                    (%https-listener-real-tls-upgrader material)
                    transport
                    (parse-http-uri
                     (format nil "https://localhost:~D/" (http-server-port server)))
                    :timeout 5))
             (ensure-equal
              "http/1.1"
              (http-kit/tls:http-tls-selected-alpn-protocol tls-stream))
             (ensure-true
              (>= (%https-listener-client-stream-record-count transport) 10)))
        (when (and tls-stream (open-stream-p tls-stream))
          (close tls-stream :abort t))
        (when (eq :running (http-kit::http-server-state server))
          (stop-http-server server :graceful-timeout 2))
        (%tls-server-test-remove-material material)))))


#+sbcl
(deftest https-listener-covers-server-error-and-state-helpers
  (%with-https-listener-test-stubs (context)
    (let* ((reported nil)
           (events nil)
           (server
             (%https-listener-make-server
              :on-event (lambda (event) (push event events))
              :on-error
              (lambda (condition details)
                (push (list condition details) reported))))
           (timeout-condition
             (make-condition 'http-timeout :message "request timed out"))
           (protocol-condition
             (make-condition 'http-protocol-error :message "bad request")))
      (signals http-server-error (http-server-failure nil))
      (ensure-equal :timeout
                    (http-server-error-reason-code
                     (http-kit::%http-server-safe-error
                      timeout-condition :test)))
      (ensure-equal :protocol-error
                    (http-server-error-reason-code
                     (http-kit::%http-server-safe-error
                      protocol-condition :test)))
      (ensure-equal :connection-error
                    (http-server-error-reason-code
                     (http-kit::%http-server-safe-error
                      (make-condition 'error) :test)))
      (let ((safe
              (http-kit::%http-server-report-error
               server protocol-condition :session
               :protocol :http1 :peer-address "127.0.0.1" :peer-port 443)))
        (ensure-equal :protocol-error
                      (http-server-error-reason-code safe))
        (ensure-equal :session (getf (second (first reported)) :phase))
        (ensure-equal :http1 (getf (second (first reported)) :protocol))
        (ensure-equal "127.0.0.1"
                      (getf (second (first reported)) :peer-address))
        (ensure-equal 443 (getf (second (first reported)) :peer-port)))
      (http-kit::%http-server-emit-event-once
       server :started '(:type :listener-started))
      (http-kit::%http-server-emit-event-once
       server :started '(:type :listener-started))
      (http-kit::%http-server-emit-event-once
       server :stopping '(:type :listener-stopping))
      (http-kit::%http-server-emit-event-once
       server :stopping '(:type :listener-stopping))
      (http-kit::%http-server-emit-event-once
       server :stopped '(:type :listener-stopped))
      (http-kit::%http-server-emit-event-once
       server :stopped '(:type :listener-stopped))
      (ensure-equal 3 (length events))
      (ensure-false (http-kit::%http-server-running-p server))
      (ensure-true (http-kit::%http-server-shutdown-p server))
      (ensure-equal
       (http-kit::http-server-graceful-timeout server)
       (http-kit::%http-server-positive-stop-timeout server nil nil))
      (signals http-server-error
        (http-kit::%http-server-positive-stop-timeout server t 0)))
    (let* ((server (%https-listener-make-server))
           (failure (http-kit::%http-server-callback-condition :on-event :test)))
      (setf (http-kit::http-server-on-error server)
            (lambda (condition details)
              (declare (ignore condition details))
              (error "error callback test failure")))
      (let ((observed
              (handler-case
                  (progn
                    (http-kit::%http-server-report-callback-error
                     server failure)
                    nil)
                (http-server-callback-error (condition) condition))))
        (ensure-true observed)
        (ensure-equal :error-callback-failed
                      (http-server-error-reason-code observed))
        (ensure-equal observed (http-server-failure server))))
    (let* ((server (%https-listener-make-server))
           (callback
             (http-kit::%http-server-session-error-callback
              server
              (http-kit::%make-http-server-connection
               :peer-address "192.0.2.4" :peer-port 1234)
              :http2
              (list nil)))
           (observed nil))
      (setf (http-kit::http-server-on-error server)
            (lambda (condition details)
              (push (list condition details) observed)))
      (funcall callback
               (make-condition 'http-protocol-error :message "session error")
               nil)
      (ensure-equal :session
                    (getf (second (first observed)) :phase))
      (ensure-equal :http2
                    (getf (second (first observed)) :protocol))
      (ensure-equal "192.0.2.4"
                    (getf (second (first observed)) :peer-address))
      (ensure-equal 1234
                    (getf (second (first observed)) :peer-port))))
  (%with-https-listener-test-stubs (context)
    (let* ((listener (gensym "LISTENER"))
           (server (%https-listener-make-server))
           (raw-stream (make-string-input-stream "raw"))
           (tls-stream (make-string-input-stream "tls"))
           (connection nil)
           (accounting-error nil))
      (setf (http-kit::http-server-state server) :running
            (http-kit::http-server-listener server) listener)
      (multiple-value-bind (rejected rejection)
          (http-kit::%http-server-reserve-connection
           server (gensym "OTHER-LISTENER") raw-stream nil nil)
        (declare (ignore rejected))
        (ensure-equal :server-stopping rejection))
      (multiple-value-bind (reserved rejection)
          (http-kit::%http-server-reserve-connection
           server listener raw-stream "127.0.0.1" 4444)
        (ensure-false rejection)
        (setf connection reserved))
      (ensure-equal 1 (http-kit::http-server-connection-count server))
      (ensure-equal 1 (http-kit::http-server-handshake-count server))
      (ensure-equal tls-stream
                    (http-kit::%http-server-set-tls-stream
                     server connection tls-stream))
      (http-kit::%http-server-release-handshake server connection)
      (http-kit::%http-server-release-handshake server connection)
      (ensure-equal 0 (http-kit::http-server-handshake-count server))
      (http-kit::%http-server-close-connection-stream server connection)
      (http-kit::%http-server-close-connection-stream server connection)
      (ensure-false (open-stream-p tls-stream))
      (ensure-true (open-stream-p raw-stream))
      (http-kit::%http-server-release-connection server connection)
      (http-kit::%http-server-release-connection server connection)
      (ensure-equal 0 (http-kit::http-server-connection-count server))
      (close raw-stream)
      (setf (%https-listener-test-context-session-action context)
            (lambda (protocol stream handler options)
              (declare (ignore protocol stream handler))
              (setf accounting-error
                    (handler-case
                        (progn
                          (funcall (getf options :release-body-bytes) -1)
                          nil)
                      (http-server-error (condition) condition)))))
      (http-kit::%http-server-serve-session
       server connection (make-string-input-stream "session") :http1 (list nil))
      (ensure-true accounting-error)
      (ensure-equal :body-buffer-accounting
                    (http-server-error-reason-code accounting-error)))))

#+sbcl
(deftest https-listener-covers-startup-and-worker-failures
  (%with-https-listener-test-stubs (context)
    (let* ((opener 'http-kit/network:open-http-tcp-listener)
           (original (fdefinition opener))
           (server (%https-listener-make-server)))
      (unwind-protect
           (progn
             (setf (fdefinition opener)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error "listener opener failure")))
             (let ((condition
                     (handler-case
                         (progn (start-http-server server) nil)
                       (http-server-error (condition) condition))))
               (ensure-true condition)
               (ensure-equal :listener-open-failed
                             (http-server-error-reason-code condition))
               (ensure-equal :failed (http-kit::http-server-state server))))
        (setf (fdefinition opener) original)))
    (let* ((thread-maker 'sb-thread:make-thread)
           (original (fdefinition thread-maker))
           (server (%https-listener-make-server)))
      (unwind-protect
           (progn
             (sb-ext:without-package-locks
               (setf (fdefinition thread-maker)
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (error "supervisor thread creation failure"))))
             (start-http-server server)
             (ensure-equal :failed (http-kit::http-server-state server))
             (ensure-equal :supervisor-start-failed
                           (http-server-error-reason-code
                            (http-server-failure server)))
             (ensure-false (http-kit::http-server-listener server)))
        (sb-ext:without-package-locks
          (setf (fdefinition thread-maker) original))))
    (let* ((thread-maker 'sb-thread:make-thread)
           (original-thread-maker (fdefinition thread-maker))
           (acceptor 'http-kit/network:accept-http-tcp-stream)
           (original-acceptor (fdefinition acceptor))
           (worker-start-failed-p nil)
           (accepted-stream nil)
           (server (%https-listener-make-server))
           (client nil))
      (unwind-protect
           (progn
             (sb-ext:without-package-locks
               (setf (fdefinition acceptor)
                     (lambda (listener)
                       (multiple-value-bind (stream peer-address peer-port)
                           (funcall original-acceptor listener)
                         (setf accepted-stream stream)
                         (values stream peer-address peer-port)))
                     (fdefinition thread-maker)
                     (lambda (&rest arguments)
                       (if (equal (getf (cdr arguments) :name)
                                  "http server connection")
                           (progn
                             (setf worker-start-failed-p t)
                             (error "worker thread creation failure"))
                           (apply original-thread-maker arguments)))))
             (start-http-server server)
             (setf client (%https-listener-connect server))
             (close-http-tcp-stream client)
             (setf client nil)
             (%https-listener-wait (lambda () (http-server-failure server)))
             (ensure-true worker-start-failed-p)
             (ensure-equal :worker-start-failed
                           (http-server-error-reason-code
                            (http-server-failure server)))
             (ensure-true accepted-stream)
             (ensure-false (open-stream-p accepted-stream))
             (ensure-equal 0 (%https-listener-connection-count server))
             (ensure-equal 0 (%https-listener-handshake-count server)))
        (sb-ext:without-package-locks
          (setf (fdefinition thread-maker) original-thread-maker
                (fdefinition acceptor) original-acceptor))
        (when client
          (close-http-tcp-stream client))
        (stop-http-server server :graceful-timeout 1)))))

#+sbcl
(deftest https-listener-stop-sets-deadline-during-startup-error-callback
  (%with-https-listener-test-stubs (context)
    (ensure-true context)
    (let* ((thread-maker (quote sb-thread:make-thread))
           (original (fdefinition thread-maker))
           (thread-attempts 0)
           (events nil)
           (stop-result nil)
           (server nil))
      (setf server
            (%https-listener-make-server
             :on-event
             (lambda (event)
               (push (getf event :type) events))
             :on-error
             (lambda (condition details)
               (declare (ignore condition details))
               (setf stop-result
                     (stop-http-server server :graceful-timeout 5)))))
      (unwind-protect
           (progn
             (sb-ext:without-package-locks
               (setf (fdefinition thread-maker)
                     (lambda (&rest arguments)
                       (incf thread-attempts)
                       (if (= thread-attempts 1)
                           (error "supervisor thread creation failure")
                           (apply original arguments)))))
             (start-http-server server)
             (ensure-equal
              :supervisor-start-failed
              (http-server-error-reason-code
               (http-server-failure server)))
             (ensure-equal server stop-result)
             (ensure-equal 2 thread-attempts)
             (ensure-true (member :listener-stopped events)))
        (sb-ext:without-package-locks
          (setf (fdefinition thread-maker) original))
        (unless (member :listener-stopped events)
          (stop-http-server server :graceful-timeout 5))))))


#+sbcl
(progn
  (defclass %https-listener-close-error-stream (binary-test-stream)
    ((close-count :initform 0
                  :accessor %https-listener-close-error-count)))

  (defmethod close ((stream %https-listener-close-error-stream) &key abort)
    (declare (ignore abort))
    (incf (%https-listener-close-error-count stream))
    (error "Intentional HTTPS listener close failure."))

  (deftest https-listener-covers-connection-close-failure
    (%with-https-listener-test-stubs (context)
      (ensure-true context)
      (let* ((server (%https-listener-make-server))
             (stream
               (make-instance
                (quote %https-listener-close-error-stream)
                :input (make-array 0
                                   :element-type (quote (unsigned-byte 8)))))
             (connection
               (http-kit::%make-http-server-connection
                :raw-stream stream)))
        (http-kit::%http-server-close-connection-stream server connection)
        (ensure-equal
         :connection-close-failed
         (http-server-error-reason-code (http-server-failure server)))
        (ensure-equal :failed (http-kit::http-server-state server))
        (http-kit::%http-server-close-connection-stream server connection)
        (ensure-equal 1 (%https-listener-close-error-count stream))))))
#+sbcl
(progn
  (defclass %https-listener-abort-close-error-stream (binary-test-stream)
    ((closed-p :initform nil
               :accessor %https-listener-abort-close-error-stream-closed-p)
     (close-before-error-p :initarg :close-before-error-p
                           :reader %https-listener-abort-close-before-error-p)
     (close-notification :initarg :close-notification
                         :reader %https-listener-abort-close-notification)))

  (defmethod open-stream-p
      ((stream %https-listener-abort-close-error-stream))
    (not (%https-listener-abort-close-error-stream-closed-p stream)))

  (defmethod close
      ((stream %https-listener-abort-close-error-stream) &key abort)
    (if abort
        (progn
          (when (%https-listener-abort-close-before-error-p stream)
            (setf (%https-listener-abort-close-error-stream-closed-p stream)
                  t))
          (sb-thread:signal-semaphore
           (%https-listener-abort-close-notification stream))
          (error "injected abort-close failure"))
        (progn
          (setf (%https-listener-abort-close-error-stream-closed-p stream) t)
          t)))

  (deftest https-listener-abort-close-error-distinguishes-open-stream-state
    (%with-https-listener-test-stubs (context)
      (ensure-true context)
      (dolist (close-before-error-p '(nil t))
        (let* ((session-entered (sb-thread:make-semaphore :count 0))
               (close-notification (sb-thread:make-semaphore :count 0))
               (stream
                 (make-instance
                  '%https-listener-abort-close-error-stream
                  :input (octets)
                  :close-before-error-p close-before-error-p
                  :close-notification close-notification))
               (server nil)
               (client nil)
               (stop-error nil))
          (setf (%https-listener-test-context-handshake-action context)
                (lambda (raw-stream timeout deadline)
                  (declare (ignore raw-stream timeout deadline))
                  stream)
                (%https-listener-test-context-session-action context)
                (lambda (protocol session-stream handler options)
                  (declare (ignore protocol session-stream handler options))
                  (sb-thread:signal-semaphore session-entered)
                  (sb-thread:wait-on-semaphore close-notification :timeout 5)))
          (unwind-protect
               (progn
                 (setf server
                       (%https-listener-make-server :graceful-timeout 0.05))
                 (start-http-server server)
                 (setf client (%https-listener-connect server))
                 (ensure-true
                  (sb-thread:wait-on-semaphore session-entered :timeout 5))
                 (setf stop-error
                       (handler-case
                           (progn
                             (stop-http-server
                              server :graceful-timeout 0.05)
                             nil)
                         (error (condition) condition)))
                 (progn
                   (ensure-false stop-error)
                   (%https-listener-wait
                    (lambda ()
                      (member (http-kit::http-server-state server)
                              (quote (:stopped :failed))))
                    5))
                 (if close-before-error-p
                     (progn
                       (ensure-false (open-stream-p stream))
                       (ensure-false (http-server-failure server)))
                     (progn
                       (ensure-true (open-stream-p stream))
                       (ensure-equal
                        :connection-close-failed
                        (http-server-error-reason-code
                         (http-server-failure server))))))
            (sb-thread:signal-semaphore close-notification)
            (when (and client (open-stream-p client))
              (close-http-tcp-stream client))
            (when (and server
                       (not (member (http-kit::http-server-state server)
                                    '(:stopped :failed))))
              (stop-http-server server :graceful-timeout 1))))))))
#+sbcl
(deftest https-listener-rotates-owned-ticket-stores-through-public-api
  (%with-https-listener-test-stubs (context)
    (let* ((original-clock http-kit::*http-server-clock-function*)
           (original-store-maker
             (fdefinition 'cl-tls-kit:make-tls13-server-ticket-key-store))
           (clock-offset 0.0d0)
           (clock
             (lambda ()
               (+ clock-offset
                  (/ (get-internal-real-time)
                     internal-time-units-per-second))))
           (owned-key-generation-count 0)
           (owned-stores nil)
           (external-key-generation-count 0)
           (external-store
             (funcall
              original-store-maker
              :random
              (lambda (size)
                (incf external-key-generation-count)
                (make-array size
                            :element-type '(unsigned-byte 8)
                            :initial-element external-key-generation-count))))
           (owned-server nil)
           (external-server nil)
           (initial-owned-store nil)
           (current-owned-store nil)
           (owned-client nil)
           (external-client nil))
      (unwind-protect
           (progn
             (setf http-kit::*http-server-clock-function* clock
                   (fdefinition
                    'cl-tls-kit:make-tls13-server-ticket-key-store)
                   (lambda (&rest arguments)
                     (let ((store
                             (apply original-store-maker
                                    :random
                                    (lambda (size)
                                      (incf owned-key-generation-count)
                                      (make-array
                                       size
                                       :element-type '(unsigned-byte 8)
                                       :initial-element
                                       owned-key-generation-count))
                                    arguments)))
                       (push store owned-stores)
                       store)))
             (setf owned-server (%https-listener-make-server)
                   initial-owned-store (first owned-stores)
                   external-server
                   (%https-listener-make-server
                    :ticket-key-store external-store))
             (ensure-equal 1 owned-key-generation-count)
             (start-http-server owned-server)
             (setf clock-offset (* 13 60 60)
                   owned-client (%https-listener-connect owned-server))
             (close-http-tcp-stream owned-client)
             (setf owned-client nil)
             (%https-listener-wait
              (lambda ()
                (and (= 1 (%https-listener-session-count context))
                     (zerop
                      (%https-listener-connection-count owned-server)))))
             (ensure-equal 2 owned-key-generation-count)
             (setf current-owned-store (first owned-stores))
             (ensure-false (eq initial-owned-store current-owned-store))
             (signals error
               (cl-tls-kit:rotate-tls13-server-ticket-key-store
                initial-owned-store))
             (ensure-true current-owned-store)
             (setf clock-offset (* 26 60 60))
             (start-http-server external-server)
             (setf external-client (%https-listener-connect external-server))
             (close-http-tcp-stream external-client)
             (setf external-client nil)
             (%https-listener-wait
              (lambda ()
                (and (= 2 (%https-listener-session-count context))
                     (zerop
                      (%https-listener-connection-count external-server)))))
             (ensure-equal 2 owned-key-generation-count)
             (ensure-equal 1 external-key-generation-count)
             (ensure-true
              (cl-tls-kit:tls13-server-ticket-key-store-p external-store))
             (stop-http-server owned-server :graceful-timeout 1)
             (signals error
               (cl-tls-kit:rotate-tls13-server-ticket-key-store
                current-owned-store))
             (stop-http-server external-server :graceful-timeout 1)
             (ensure-true
              (cl-tls-kit:tls13-server-ticket-key-store-p external-store))
             (ensure-equal
              external-store
              (cl-tls-kit:rotate-tls13-server-ticket-key-store external-store)))
        (when (and owned-client (open-stream-p owned-client))
          (close-http-tcp-stream owned-client))
        (when (and external-client (open-stream-p external-client))
          (close-http-tcp-stream external-client))
        (dolist (server (list owned-server external-server))
          (when (and server
                     (not (eq :stopped
                              (http-kit::http-server-state server))))
            (stop-http-server server :graceful-timeout 1)))
        (setf http-kit::*http-server-clock-function* original-clock
              (fdefinition
               'cl-tls-kit:make-tls13-server-ticket-key-store)
              original-store-maker)
        (dolist (store owned-stores)
          (http-kit::%with-http-cleanup
            (cl-tls-kit:destroy-tls13-server-ticket-key-store store)))
        (http-kit::%with-http-cleanup
          (cl-tls-kit:destroy-tls13-server-ticket-key-store external-store))))))
#+sbcl
(deftest https-listener-does-not-rotate-owned-ticket-store-after-stop-begins
  (%with-https-listener-test-stubs (context)
    (ensure-true context)
    (let* ((original-clock http-kit::*http-server-clock-function*)
           (original-store-maker
             (fdefinition 'cl-tls-kit:make-tls13-server-ticket-key-store))
           (clock-offset 0.0d0)
           (clock
             (lambda ()
               (+ clock-offset
                  (/ (get-internal-real-time)
                     internal-time-units-per-second))))
           (accepted (sb-thread:make-semaphore :count 0))
           (release-accepted (sb-thread:make-semaphore :count 0))
           (client-finished (sb-thread:make-semaphore :count 0))
           (stop-finished (sb-thread:make-semaphore :count 0))
           (store-creations 0)
           (store nil)
           (server nil)
           (client nil)
           (client-error nil)
           (client-thread nil)
           (stop-error nil)
           (stop-result nil)
           (stop-thread nil))
      (unwind-protect
           (progn
             (setf http-kit::*http-server-clock-function* clock
                   (fdefinition
                    'cl-tls-kit:make-tls13-server-ticket-key-store)
                   (lambda (&rest arguments)
                     (incf store-creations)
                     (setf store
                           (apply original-store-maker arguments)))
                   server
                   (%https-listener-make-server
                    :on-event
                    (lambda (event)
                      (when (eq :connection-accepted (getf event :type))
                        (sb-thread:signal-semaphore accepted)
                        (sb-thread:wait-on-semaphore release-accepted)))))
             (ensure-equal 1 store-creations)
             (start-http-server server)
             (setf client-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (setf client
                                 (handler-case
                                     (%https-listener-connect server)
                                   (error (condition)
                                     (setf client-error condition))))
                        (sb-thread:signal-semaphore client-finished)))))
             (ensure-true (sb-thread:wait-on-semaphore accepted :timeout 5))
             (setf stop-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (setf stop-result
                                 (handler-case
                                     (stop-http-server
                                      server :graceful-timeout 2)
                                   (error (condition)
                                     (setf stop-error condition))))
                        (sb-thread:signal-semaphore stop-finished)))))
             (%https-listener-wait
              (lambda ()
                (eq :stopping (http-kit::http-server-state server)))
              5)
             (setf clock-offset (* 13 60 60))
             (sb-thread:signal-semaphore release-accepted)
             (ensure-true (sb-thread:wait-on-semaphore stop-finished :timeout 5))
             (ensure-true
              (sb-thread:wait-on-semaphore client-finished :timeout 5))
             (ensure-false stop-error)
             (ensure-false client-error)
             (ensure-equal server stop-result)
             (ensure-equal 1 store-creations)
             (ensure-equal :stopped (http-kit::http-server-state server))
             (signals error
               (cl-tls-kit:rotate-tls13-server-ticket-key-store store)))
        (sb-thread:signal-semaphore release-accepted)
        (when stop-thread (sb-thread:join-thread stop-thread))
        (when client-thread (sb-thread:join-thread client-thread))
        (when (and client (open-stream-p client))
          (close-http-tcp-stream client))
        (setf http-kit::*http-server-clock-function* original-clock
              (fdefinition
               'cl-tls-kit:make-tls13-server-ticket-key-store)
              original-store-maker)
        (when (and server
                   (not (eq :stopped
                            (http-kit::http-server-state server))))
          (stop-http-server server :graceful-timeout 1))
        (when store
          (http-kit::%with-http-cleanup
            (cl-tls-kit:destroy-tls13-server-ticket-key-store store)))))))
#+sbcl
(deftest https-listener-defers-retired-ticket-store-destruction-for-active-connections
  (%with-https-listener-test-stubs (context)
    (let* ((original-clock http-kit::*http-server-clock-function*)
           (original-store-maker
             (fdefinition 'cl-tls-kit:make-tls13-server-ticket-key-store))
           (clock-offset 0.0d0)
           (clock
             (lambda ()
               (+ clock-offset
                  (/ (get-internal-real-time)
                     internal-time-units-per-second))))
           (stores nil)
           (session-number 0)
           (session-entered (sb-thread:make-semaphore :count 0))
           (release-session (sb-thread:make-semaphore :count 0))
           (server nil)
           (old-client nil)
           (second-old-client nil)
           (rotation-client nil)
           (old-store nil)
           (current-store nil))
      (unwind-protect
           (progn
             (setf http-kit::*http-server-clock-function* clock
                   (%https-listener-test-context-session-action context)
                   (lambda (protocol stream handler options)
                     (declare (ignore protocol stream handler options))
                     (let ((session-index
                             (sb-thread:with-mutex
                                 ((%https-listener-test-context-lock context))
                               (incf session-number))))
                       (when (<= session-index 2)
                         (sb-thread:signal-semaphore session-entered)
                         (sb-thread:wait-on-semaphore release-session))))
                   (fdefinition
                    'cl-tls-kit:make-tls13-server-ticket-key-store)
                   (lambda (&rest arguments)
                     (let ((store (apply original-store-maker arguments)))
                       (push store stores)
                       store)))
             (setf server (%https-listener-make-server)
                   old-store (first stores))
             (start-http-server server)
             (setf old-client (%https-listener-connect server))
             (ensure-true
              (sb-thread:wait-on-semaphore session-entered :timeout 5))
             (setf second-old-client (%https-listener-connect server))
             (ensure-true
              (sb-thread:wait-on-semaphore session-entered :timeout 5))
             (setf clock-offset (* 13 60 60)
                   rotation-client (%https-listener-connect server))
             (close-http-tcp-stream rotation-client)
             (setf rotation-client nil)
             (%https-listener-wait
              (lambda ()
                (and (= 3 (%https-listener-session-count context))
                     (= 2 (%https-listener-connection-count server)))))
             (ensure-equal 2 (length stores))
             (setf current-store (first stores))
             (ensure-false (eq old-store current-store))
             (close-http-tcp-stream old-client)
             (setf old-client nil)
             (sb-thread:signal-semaphore release-session)
             (%https-listener-wait
              (lambda () (= 1 (%https-listener-connection-count server))))
             (ensure-equal
              old-store
              (cl-tls-kit:rotate-tls13-server-ticket-key-store old-store))
             (close-http-tcp-stream second-old-client)
             (setf second-old-client nil)
             (sb-thread:signal-semaphore release-session)
             (%https-listener-wait
              (lambda () (zerop (%https-listener-connection-count server))))
             (signals error
               (cl-tls-kit:rotate-tls13-server-ticket-key-store old-store))
             (stop-http-server server :graceful-timeout 1)
             (signals error
               (cl-tls-kit:rotate-tls13-server-ticket-key-store current-store)))
        (sb-thread:signal-semaphore release-session)
        (sb-thread:signal-semaphore release-session)
        (when (and old-client (open-stream-p old-client))
          (close-http-tcp-stream old-client))
        (when (and second-old-client (open-stream-p second-old-client))
          (close-http-tcp-stream second-old-client))
        (when (and rotation-client (open-stream-p rotation-client))
          (close-http-tcp-stream rotation-client))
        (when (and server
                   (not (eq :stopped
                            (http-kit::http-server-state server))))
          (stop-http-server server :graceful-timeout 1))
        (setf http-kit::*http-server-clock-function* original-clock
              (fdefinition
               'cl-tls-kit:make-tls13-server-ticket-key-store)
              original-store-maker)
        (dolist (store stores)
          (http-kit::%with-http-cleanup
            (cl-tls-kit:destroy-tls13-server-ticket-key-store store)))))))
#+sbcl
(deftest https-listener-does-not-report-stopped-while-handler-worker-runs
  (%with-https-listener-test-stubs (context)
    (let* ((handler-entered (sb-thread:make-semaphore :count 0))
           (release-handler (sb-thread:make-semaphore :count 0))
           (server nil))
      (unwind-protect
           (progn
             (setf (%https-listener-test-context-session-action context)
                   (lambda (protocol stream handler options)
                     (declare (ignore protocol stream options))
                     (funcall handler nil))
                   server
                   (%https-listener-make-server
                    :graceful-timeout 0.1
                    :handler
                    (lambda (request)
                      (declare (ignore request))
                      (sb-thread:signal-semaphore handler-entered)
                      (sb-thread:wait-on-semaphore release-handler)
                      (make-http-response :status 204))
                    :on-event
                    (lambda (event)
                      (%https-listener-record-event context event))))
             (start-http-server server)
             (let ((client (%https-listener-connect server)))
               (close-http-tcp-stream client))
             (ensure-true
              (sb-thread:wait-on-semaphore handler-entered :timeout 5))
             (stop-http-server server :graceful-timeout 0.1)
             (ensure-equal :stopping (http-kit::http-server-state server))
             (ensure-equal
              1
              (hash-table-count (http-kit::http-server-workers server)))
             (ensure-equal 1 (%https-listener-connection-count server))
             (ensure-false
              (%https-listener-event-present-p
               context :listener-stopped nil))
             (sb-thread:signal-semaphore release-handler)
             (%https-listener-wait
              (lambda ()
                (%https-listener-event-present-p
                 context :listener-stopped nil)))
             (ensure-equal :stopped (http-kit::http-server-state server))
             (ensure-equal
              0
              (hash-table-count (http-kit::http-server-workers server)))
             (ensure-equal 0 (%https-listener-connection-count server)))
        (when server
          (sb-thread:signal-semaphore release-handler)
          (stop-http-server server :graceful-timeout 1)
          (%https-listener-wait
           (lambda ()
             (or (%https-listener-event-present-p
                  context :listener-stopped nil)
                 (eq :failed (http-kit::http-server-state server))))))))))
#+sbcl
(deftest https-listener-preserves-external-ticket-store-through-public-stop
  (%with-https-listener-test-stubs (context)
    (ensure-true context)
    (let* ((store (cl-tls-kit:make-tls13-server-ticket-key-store))
           (server (%https-listener-make-server :ticket-key-store store)))
      (unwind-protect
           (progn
             (start-http-server server)
             (ensure-equal
              server
              (stop-http-server server :graceful-timeout 1))
             (ensure-true (cl-tls-kit:tls13-server-ticket-key-store-p store))
             (ensure-true
              (eq store
                  (cl-tls-kit:rotate-tls13-server-ticket-key-store store))))
        (stop-http-server server :graceful-timeout 1)
        (http-kit::%with-http-cleanup
          (cl-tls-kit:destroy-tls13-server-ticket-key-store store))))))
#+sbcl
(deftest https-listener-destroys-owned-ticket-store-through-public-stop
  (%with-https-listener-test-stubs (context)
    (ensure-true context)
    (let* ((original-store-maker
             (fdefinition 'cl-tls-kit:make-tls13-server-ticket-key-store))
           (store nil)
           (server nil))
      (unwind-protect
           (progn
             (setf (fdefinition
                    'cl-tls-kit:make-tls13-server-ticket-key-store)
                   (lambda (&rest arguments)
                     (setf store (apply original-store-maker arguments))))
             (setf server (%https-listener-make-server))
             (ensure-true store)
             (start-http-server server)
             (ensure-equal
              server
              (stop-http-server server :graceful-timeout 1))
             (signals error
               (cl-tls-kit:rotate-tls13-server-ticket-key-store store)))
        (when (and server
                   (not (eq :stopped
                            (http-kit::http-server-state server))))
          (stop-http-server server :graceful-timeout 1))
        (setf (fdefinition
               'cl-tls-kit:make-tls13-server-ticket-key-store)
              original-store-maker)
        (when store
          (http-kit::%with-http-cleanup
            (cl-tls-kit:destroy-tls13-server-ticket-key-store store)))))))
#+sbcl
(deftest https-listener-preserves-stop-error-when-ticket-store-destroy-fails
  (%with-https-listener-test-stubs (context)
    (ensure-true context)
    (let* ((original-store-maker
             (fdefinition 'cl-tls-kit:make-tls13-server-ticket-key-store))
           (original-store-destroyer
             (fdefinition 'cl-tls-kit:destroy-tls13-server-ticket-key-store))
           (store nil)
           (server nil)
           (destroy-attempts 0)
           (cleanup-error-reported-p nil)
           (fail-stop-callback-p t)
           (first-cleanup-error nil)
           (first-stop-error nil)
           (second-stop-error nil))
      (unwind-protect
           (progn
             (setf (fdefinition
                    'cl-tls-kit:make-tls13-server-ticket-key-store)
                   (lambda (&rest arguments)
                     (setf store (apply original-store-maker arguments)))
                   (fdefinition
                    'cl-tls-kit:destroy-tls13-server-ticket-key-store)
                   (lambda (candidate)
                     (incf destroy-attempts)
                     (if (or (null first-cleanup-error)
                             (null second-stop-error))
                         (error "injected ticket store destruction failure")
                         (funcall original-store-destroyer candidate)))
                   server
                   (%https-listener-make-server
                    :on-event
                    (lambda (event)
                      (when (and fail-stop-callback-p
                                 (eq :listener-stopping
                                     (getf event :type)))
                        (setf fail-stop-callback-p nil)
                        (error "injected listener stop callback failure")))
                    :on-error
                    (lambda (condition details)
                      (declare (ignore details))
                      (when (eq :ticket-key-store-destroy-failed
                                (http-server-error-reason-code condition))
                        (setf cleanup-error-reported-p t)))))
             (start-http-server server)
             (setf first-stop-error
                   (handler-case
                       (progn
                         (stop-http-server server :graceful-timeout 1)
                         nil)
                     (error (condition) condition)))
             (ensure-true
              (typep first-stop-error 'http-server-callback-error))
             (ensure-equal :event-callback-failed
                           (http-server-error-reason-code first-stop-error))
             (ensure-true cleanup-error-reported-p)
             (ensure-true (plusp destroy-attempts))
             (ensure-equal store
                           (http-kit::http-server-ticket-key-store server))
             (setf first-cleanup-error
                   (http-kit::http-server-ticket-key-store-cleanup-error
                    server)
                   destroy-attempts 0)
             (ensure-true first-cleanup-error)
             (setf second-stop-error
                   (handler-case
                       (progn
                         (stop-http-server server :graceful-timeout 1)
                         nil)
                     (http-server-error (condition) condition)))
             (ensure-true second-stop-error)
             (ensure-equal :ticket-key-store-destroy-failed
                           (http-server-error-reason-code second-stop-error))
             (ensure-true
              (eq first-cleanup-error
                  (http-kit::http-server-ticket-key-store-cleanup-error
                   server)))
             (ensure-true (plusp destroy-attempts))
             (ensure-equal store
                           (http-kit::http-server-ticket-key-store server))
             (progn
               (setf destroy-attempts 0)
               (ensure-equal server
                             (stop-http-server server :graceful-timeout 1)))
             (ensure-true (plusp destroy-attempts))
             (ensure-false (http-kit::http-server-ticket-key-store server))
             (ensure-false
              (http-kit::http-server-ticket-key-store-cleanup-error server)))
        (setf fail-stop-callback-p nil
              (fdefinition 'cl-tls-kit:make-tls13-server-ticket-key-store)
              original-store-maker
              (fdefinition
               'cl-tls-kit:destroy-tls13-server-ticket-key-store)
              original-store-destroyer)
        (when (and server
                   (not (eq :stopped
                            (http-kit::http-server-state server))))
          (stop-http-server server :graceful-timeout 1))
        (when (and store
                   (cl-tls-kit:tls13-server-ticket-key-store-p store))
          (http-kit::%with-http-cleanup
            (cl-tls-kit:destroy-tls13-server-ticket-key-store store)))))))
#+sbcl
(deftest https-listener-destroys-owned-ticket-store-if-wrapper-construction-fails
  (%with-https-listener-test-stubs (context)
    (ensure-true context)
    (let* ((original-store-maker
             (fdefinition 'cl-tls-kit:make-tls13-server-ticket-key-store))
           (original-wrapper-maker
             (fdefinition 'http-kit/tls:make-http-tls-server-wrapper))
           (store nil))
      (unwind-protect
           (progn
             (setf (fdefinition
                    'cl-tls-kit:make-tls13-server-ticket-key-store)
                   (lambda (&rest arguments)
                     (setf store (apply original-store-maker arguments)))
                   (fdefinition 'http-kit/tls:make-http-tls-server-wrapper)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error "injected TLS wrapper construction failure")))
             (signals http-server-error
               (%https-listener-make-server))
             (ensure-true store)
             (signals error
               (cl-tls-kit:rotate-tls13-server-ticket-key-store store)))
        (setf (fdefinition 'cl-tls-kit:make-tls13-server-ticket-key-store)
              original-store-maker
              (fdefinition 'http-kit/tls:make-http-tls-server-wrapper)
              original-wrapper-maker)
        (when store
          (http-kit::%with-http-cleanup
            (cl-tls-kit:destroy-tls13-server-ticket-key-store store)))))))
#+sbcl
(deftest https-listener-destroys-uninstalled-restart-ticket-store
  (%with-https-listener-test-stubs (context)
    (ensure-true context)
    (let* ((original-store-maker
             (fdefinition 'cl-tls-kit:make-tls13-server-ticket-key-store))
           (original-wrapper-maker
             (fdefinition 'http-kit/tls:make-http-tls-server-wrapper))
           (stores nil)
           (server nil))
      (unwind-protect
           (progn
             (setf (fdefinition
                    'cl-tls-kit:make-tls13-server-ticket-key-store)
                   (lambda (&rest arguments)
                     (let ((store (apply original-store-maker arguments)))
                       (push store stores)
                       store)))
             (setf server (%https-listener-make-server))
             (ensure-equal 1 (length stores))
             (start-http-server server)
             (ensure-equal
              server
              (stop-http-server server :graceful-timeout 1))
             (signals error
               (cl-tls-kit:rotate-tls13-server-ticket-key-store
                (first stores)))
             (setf (fdefinition 'http-kit/tls:make-http-tls-server-wrapper)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error "injected restart TLS wrapper construction failure")))
             (signals http-server-error
               (start-http-server server))
             (ensure-equal 2 (length stores))
             (signals error
               (cl-tls-kit:rotate-tls13-server-ticket-key-store
                (second stores))))
        (setf (fdefinition 'cl-tls-kit:make-tls13-server-ticket-key-store)
              original-store-maker
              (fdefinition 'http-kit/tls:make-http-tls-server-wrapper)
              original-wrapper-maker)
        (when (and server
                   (not (eq :stopped
                            (http-kit::http-server-state server))))
          (stop-http-server server :graceful-timeout 1))
        (dolist (store stores)
          (http-kit::%with-http-cleanup
            (cl-tls-kit:destroy-tls13-server-ticket-key-store store)))))))
#+sbcl
(deftest https-listener-destroys-uninstalled-rotation-ticket-store
  (%with-https-listener-test-stubs (context)
    (ensure-true context)
    (let* ((original-clock http-kit::*http-server-clock-function*)
           (original-store-maker
             (fdefinition 'cl-tls-kit:make-tls13-server-ticket-key-store))
           (original-wrapper-maker
             (fdefinition 'http-kit/tls:make-http-tls-server-wrapper))
           (clock-offset 0.0d0)
           (clock
             (lambda ()
               (+ clock-offset
                  (/ (get-internal-real-time)
                     internal-time-units-per-second))))
           (stores nil)
           (wrapper-error-reported (sb-thread:make-semaphore :count 0))
           (wrapper-error-reason nil)
           (server nil)
           (client nil))
      (unwind-protect
           (progn
             (setf http-kit::*http-server-clock-function* clock
                   (fdefinition
                    'cl-tls-kit:make-tls13-server-ticket-key-store)
                   (lambda (&rest arguments)
                     (let ((store (apply original-store-maker arguments)))
                       (push store stores)
                       store)))
             (setf server
                   (%https-listener-make-server
                    :on-error
                    (lambda (condition details)
                      (declare (ignore details))
                      (setf wrapper-error-reason
                            (http-server-error-reason-code condition))
                      (sb-thread:signal-semaphore wrapper-error-reported))))
             (ensure-equal 1 (length stores))
             (start-http-server server)
             (setf clock-offset (* 13 60 60)
                   (fdefinition 'http-kit/tls:make-http-tls-server-wrapper)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error "injected rotation wrapper construction failure"))
                   client (%https-listener-connect server))
             (close-http-tcp-stream client)
             (setf client nil)
             (ensure-true
              (sb-thread:wait-on-semaphore wrapper-error-reported :timeout 5))
             (ensure-equal :invalid-tls-credentials wrapper-error-reason)
             (%https-listener-wait
              (lambda () (zerop (%https-listener-connection-count server))))
             (ensure-equal 2 (length stores))
             (let ((replacement-store (first stores))
                   (initial-store (second stores)))
               (ensure-equal
                initial-store
                (cl-tls-kit:rotate-tls13-server-ticket-key-store initial-store))
               (signals error
                 (cl-tls-kit:rotate-tls13-server-ticket-key-store
                  replacement-store))
               (stop-http-server server :graceful-timeout 1)
               (signals error
                 (cl-tls-kit:rotate-tls13-server-ticket-key-store
                  initial-store))))
        (when (and client (open-stream-p client))
          (close-http-tcp-stream client))
        (setf http-kit::*http-server-clock-function* original-clock
              (fdefinition
               'cl-tls-kit:make-tls13-server-ticket-key-store)
              original-store-maker
              (fdefinition 'http-kit/tls:make-http-tls-server-wrapper)
              original-wrapper-maker)
        (when (and server
                   (not (eq :stopped
                            (http-kit::http-server-state server))))
          (stop-http-server server :graceful-timeout 1))
        (dolist (store stores)
          (http-kit::%with-http-cleanup
            (cl-tls-kit:destroy-tls13-server-ticket-key-store store)))))))
#+sbcl
(deftest https-listener-stop-joins-aborted-shutdown-coordinator
  (%with-https-listener-test-stubs (context)
    (ensure-true context)
    (let* ((stopped-event-seen nil)
           (terminate-coordinator-on-stopped t)
           (server
             (%https-listener-make-server
              :on-event
              (lambda (event)
                (when (and terminate-coordinator-on-stopped
                           (eq :listener-stopped (getf event :type)))
                  (setf stopped-event-seen t
                        terminate-coordinator-on-stopped nil)
                  (sb-thread:terminate-thread sb-thread:*current-thread*)))))
           (stop-error nil))
      (unwind-protect
           (progn
             (setf stop-error
                   (handler-case
                       (progn
                         (stop-http-server server :graceful-timeout 5)
                         nil)
                     (sb-thread:join-thread-error (condition) condition)))
             (ensure-true stopped-event-seen)
             (ensure-true
              (typep stop-error (quote sb-thread:join-thread-error)))
             (ensure-false (http-server-failure server)))
        (setf terminate-coordinator-on-stopped nil)
        (stop-http-server server :graceful-timeout 5)))))
#+sbcl
(deftest https-listener-skips-coordinator-join-after-deadline
  (%with-https-listener-test-stubs (context)
    (ensure-true context)
    (let* ((stopped-event-entered (sb-thread:make-semaphore :count 0))
           (release-stopped-event (sb-thread:make-semaphore :count 0))
           (stopping-event-seen nil)
           (stopped-event-completed nil)
           (coordinator nil)
           (server
             (%https-listener-make-server
              :on-event
              (lambda (event)
                (case (getf event :type)
                  (:listener-stopping
                   (setf stopping-event-seen t)
                   (sleep 0.2))
                  (:listener-stopped
                   (setf coordinator sb-thread:*current-thread*)
                   (sb-thread:signal-semaphore stopped-event-entered)
                   (setf stopped-event-completed
                         (sb-thread:wait-on-semaphore
                          release-stopped-event :timeout 5))))))))
      (unwind-protect
           (progn
             (ensure-equal
              server
              (stop-http-server server :graceful-timeout 0.05))
             (ensure-true stopping-event-seen)
             (ensure-true
              (sb-thread:wait-on-semaphore stopped-event-entered :timeout 5))
             (ensure-true (sb-thread:thread-alive-p coordinator))
             (ensure-false stopped-event-completed)
             (sb-thread:signal-semaphore release-stopped-event)
             (sb-thread:join-thread coordinator :timeout 5 :default nil)
             (ensure-false (sb-thread:thread-alive-p coordinator))
             (ensure-true stopped-event-completed))
        (sb-thread:signal-semaphore release-stopped-event)
        (when coordinator
          (sb-thread:join-thread coordinator :timeout 5 :default nil))
        (stop-http-server server :graceful-timeout 1)))))
#+sbcl
(deftest https-listener-times-out-slow-real-tls-handshake
  (let* ((material (%tls-server-test-material))
         (context (%make-https-listener-test-context))
         (server nil)
         (transport nil)
         (client-thread nil)
         (client-tls-stream nil)
         (client-condition nil))
    (unwind-protect
         (progn
           (setf server
                 (make-http-server
                  :host "127.0.0.1"
                  :port 0
                  :handler
                  (lambda (request)
                    (declare (ignore request))
                    (make-http-response :status 204))
                  :certificate-chain (getf material :certificate-chain)
                  :private-key (getf material :private-key)
                  :handshake-timeout 0.25
                  :request-timeout 5
                  :idle-timeout 5
                  :graceful-timeout 2
                  :on-event
                  (lambda (event)
                    (%https-listener-record-event context event))
                  :on-error
                  (lambda (condition details)
                    (%https-listener-record-error
                     context condition details))))
           (start-http-server server)
           (setf transport
                 (nth-value
                  0
                  (%https-listener-open-real-tls-transport
                   (http-server-port server)
                   :mode :stall)))
           (setf client-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (setf client-tls-stream
                              (funcall
                               (%https-listener-real-tls-upgrader material)
                               transport
                               (parse-http-uri
                                (format nil "https://localhost:~D/"
                                        (http-server-port server)))
                               :timeout 5))
                      (error (condition)
                        (setf client-condition condition))))))
           (ensure-true (%https-listener-wait-for-client-hello transport))
           (%https-listener-wait
            (lambda () (%https-listener-errors context)))
           (let* ((entry (first (%https-listener-errors context)))
                  (condition (first entry))
                  (details (second entry)))
             (ensure-equal 1 (length (%https-listener-errors context)))
             (ensure-equal :timeout
                           (http-server-error-reason-code condition))
             (ensure-equal :handshake
                           (http-server-error-phase condition))
             (ensure-equal :handshake (getf details :phase)))
           (ensure-equal 0 (%https-listener-session-count context))
           (ensure-false
            (%https-listener-event-present-p context :alpn-selected nil))
           (%https-listener-release-client-hello transport)
           (sb-thread:join-thread client-thread :default nil :timeout 5)
           (ensure-false (sb-thread:thread-alive-p client-thread))
           (ensure-false client-tls-stream)
           (ensure-true client-condition)
           (%https-listener-wait
            (lambda ()
              (and (zerop (%https-listener-connection-count server))
                   (zerop (%https-listener-handshake-count server))))))
      (when transport
        (%https-listener-release-client-hello transport)
        (when (open-stream-p transport)
          (close-http-tcp-stream transport)))
      (when (and client-thread
                 (sb-thread:thread-alive-p client-thread))
        (sb-thread:join-thread client-thread :default nil :timeout 5))
      (when (and client-tls-stream (open-stream-p client-tls-stream))
        (close client-tls-stream :abort t))
      (when (and server
                 (not (eq :stopped (http-kit::http-server-state server))))
        (stop-http-server server :graceful-timeout 2))
      (%tls-server-test-remove-material material))))
#+sbcl
(deftest https-listener-rejects-unoffered-alpn-over-real-tls
  (let* ((material (%tls-server-test-material))
         (context (%make-https-listener-test-context))
         (server nil)
         (transport nil)
         (client-tls-stream nil)
         (client-condition nil))
    (unwind-protect
         (progn
           (setf server
                 (make-http-server
                  :host "127.0.0.1"
                  :port 0
                  :handler
                  (lambda (request)
                    (declare (ignore request))
                    (make-http-response :status 204))
                  :certificate-chain (getf material :certificate-chain)
                  :private-key (getf material :private-key)
                  :handshake-timeout 5
                  :request-timeout 5
                  :idle-timeout 5
                  :graceful-timeout 2
                  :on-event
                  (lambda (event)
                    (%https-listener-record-event context event))
                  :on-error
                  (lambda (condition details)
                    (%https-listener-record-error
                     context condition details))))
           (start-http-server server)
           (setf transport
                 (open-http-tcp-stream
                  (make-http-request
                   :method "GET"
                   :uri (format nil "http://127.0.0.1:~D/"
                                (http-server-port server)))
                  :timeout 5))
           (handler-case
               (setf client-tls-stream
                     (funcall
                      (%https-listener-real-tls-upgrader material '("h3"))
                      transport
                      (parse-http-uri
                       (format nil "https://localhost:~D/"
                               (http-server-port server)))
                      :timeout 5))
             (error (condition)
               (setf client-condition condition)))
           (%https-listener-wait
            (lambda () (%https-listener-errors context)))
           (let* ((entry (first (%https-listener-errors context)))
                  (condition (first entry))
                  (details (second entry)))
             (ensure-equal 1 (length (%https-listener-errors context)))
             (ensure-equal :protocol-error
                           (http-server-error-reason-code condition))
             (ensure-equal :handshake
                           (http-server-error-phase condition))
             (ensure-equal :handshake (getf details :phase)))
           (ensure-true client-condition)
           (ensure-false client-tls-stream)
           (ensure-equal 0 (%https-listener-session-count context))
           (ensure-false
            (%https-listener-event-present-p context :alpn-selected nil))
           (%https-listener-wait
            (lambda () (zerop (%https-listener-connection-count server)))))
      (when (and client-tls-stream (open-stream-p client-tls-stream))
        (close client-tls-stream :abort t))
      (when (and transport (open-stream-p transport))
        (close-http-tcp-stream transport))
      (when (and server
                 (not (eq :stopped (http-kit::http-server-state server))))
        (stop-http-server server :graceful-timeout 2))
      (%tls-server-test-remove-material material))))
#+sbcl
(deftest https-listener-sends-close-notify-after-request-close
  (let* ((material (%tls-server-test-material))
         (server
           (make-http-server
            :host "127.0.0.1"
            :port 0
            :handler (lambda (request)
                       (declare (ignore request))
                       (make-http-response :status 204))
            :certificate-chain (getf material :certificate-chain)
            :private-key (getf material :private-key)
            :handshake-timeout 5
            :request-timeout 5
            :idle-timeout 5
            :graceful-timeout 2))
         (transport nil)
         (tls-stream nil))
    (unwind-protect
         (progn
           (start-http-server server)
           (setf transport
                 (open-http-tcp-stream
                  (make-http-request
                   :method "GET"
                   :uri (format nil "http://127.0.0.1:~D/"
                                (http-server-port server)))
                  :timeout 5))
           (setf tls-stream
                 (funcall
                  (%https-listener-real-tls-upgrader material)
                  transport
                  (parse-http-uri
                   (format nil "https://localhost:~D/"
                           (http-server-port server)))
                  :timeout 5))
           (write-sequence
            (ascii "GET / HTTP/1.1|CRLF|Host: localhost|CRLF|Connection: close|CRLF||CRLF|")
            tls-stream)
           (finish-output tls-stream)
           (ensure-equal
            204
            (http-response-status
             (parse-http-response tls-stream :timeout 5)))
           (ensure-equal
            :eof
            (sb-ext:with-timeout 5
              (read-byte tls-stream nil :eof))))
      (when (and tls-stream (open-stream-p tls-stream))
        (close tls-stream :abort t))
      (when (and transport (open-stream-p transport))
        (close-http-tcp-stream transport))
      (unless (eq :stopped (http-kit::http-server-state server))
        (stop-http-server server :graceful-timeout 2))
      (%tls-server-test-remove-material material))))
#+sbcl
(deftest https-listener-does-not-create-ticket-store-after-start-is-stopped
  (%with-https-listener-test-stubs (context)
    (ensure-true context)
    (let* ((original-store-maker
             (fdefinition 'cl-tls-kit:make-tls13-server-ticket-key-store))
           (original-start-listener
             (fdefinition 'http-kit::%http-server-start-listener))
           (stores nil)
           (start-entered (sb-thread:make-semaphore :count 0))
           (release-start (sb-thread:make-semaphore :count 0))
           (server nil)
           (start-thread nil)
           (start-result nil)
           (start-error nil))
      (unwind-protect
           (progn
             (setf (fdefinition
                    'cl-tls-kit:make-tls13-server-ticket-key-store)
                   (lambda (&rest arguments)
                     (let ((store (apply original-store-maker arguments)))
                       (push store stores)
                       store))
                   server (%https-listener-make-server))
             (ensure-equal 1 (length stores))
             (setf (fdefinition 'http-kit::%http-server-start-listener)
                   (lambda (candidate start-token)
                     (sb-thread:signal-semaphore start-entered)
                     (unless (sb-thread:wait-on-semaphore release-start
                                                           :timeout 5)
                       (error "Timed out waiting to release stale startup."))
                     (funcall original-start-listener candidate start-token)))
             (setf start-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (setf start-result (start-http-server server))
                        (error (condition)
                          (setf start-error condition))))
                    :name "ticket store stale startup test"))
             (ensure-true
              (sb-thread:wait-on-semaphore start-entered :timeout 5))
             (ensure-equal
              server
              (stop-http-server server :graceful-timeout 1))
             (ensure-false (http-kit::http-server-ticket-key-store server))
             (sb-thread:signal-semaphore release-start)
             (sb-thread:join-thread start-thread :timeout 5 :default nil)
             (ensure-false (sb-thread:thread-alive-p start-thread))
             (ensure-false start-error)
             (ensure-equal server start-result)
             (ensure-equal 1 (length stores)))
        (sb-thread:signal-semaphore release-start)
        (when (and start-thread (sb-thread:thread-alive-p start-thread))
          (sb-thread:join-thread start-thread :timeout 5 :default nil))
        (setf (fdefinition 'cl-tls-kit:make-tls13-server-ticket-key-store)
              original-store-maker
              (fdefinition 'http-kit::%http-server-start-listener)
              original-start-listener)
        (when (and server
                   (not (eq :stopped (http-kit::http-server-state server))))
          (stop-http-server server :graceful-timeout 1))
        (dolist (store stores)
          (when (cl-tls-kit:tls13-server-ticket-key-store-p store)
            (http-kit::%with-http-cleanup
              (cl-tls-kit:destroy-tls13-server-ticket-key-store store))))))))
#+sbcl
(deftest https-listener-bounds-blocked-close-notify-by-stop-deadline
  (let* ((material (%tls-server-test-material))
         (close-notify-entered (sb-thread:make-semaphore :count 0))
         (close-notify-transport nil)
         (force-close-at nil)
         (force-close-deadline nil)
         (original-close-notify
           (fdefinition 'http-kit/tls::%tls-server-send-close-notify))
         (original-force-close
           (fdefinition 'http-kit::%http-server-force-close-connections))
         (graceful-timeout 0.1)
         (server
           (make-http-server
            :host "127.0.0.1"
            :port 0
            :handler (lambda (request)
                       (declare (ignore request))
                       (make-http-response :status 204))
            :certificate-chain (getf material :certificate-chain)
            :private-key (getf material :private-key)
            :handshake-timeout 5
            :request-timeout 5
            :idle-timeout 5
            :graceful-timeout graceful-timeout))
         (transport nil)
         (tls-stream nil))
    (unwind-protect
         (progn
           (sb-ext:without-package-locks
             (setf (fdefinition 'http-kit/tls::%tls-server-send-close-notify)
                   (lambda (state)
                     (setf close-notify-transport
                           (http-kit/tls::http-tls-server-state-transport-stream
                            state))
                     (sb-thread:signal-semaphore close-notify-entered)
                     (let ((deadline
                             (+ (get-internal-real-time)
                                (* 5 internal-time-units-per-second))))
                       (loop while (open-stream-p close-notify-transport)
                             do (when (>= (get-internal-real-time) deadline)
                                  (error "Timed out waiting for TLS transport abort."))
                                (sleep 0.005)))
                     t)))
           (sb-ext:without-package-locks
             (setf (fdefinition
                    'http-kit::%http-server-force-close-connections)
                   (lambda (candidate)
                     (unless force-close-at
                       (setf force-close-deadline
                             (http-kit::%http-server-stop-deadline candidate)
                             force-close-at (http-kit::%http-server-now)))
                     (funcall original-force-close candidate))))
           (start-http-server server)
           (setf transport
                 (open-http-tcp-stream
                  (make-http-request
                   :method "GET"
                   :uri (format nil "http://127.0.0.1:~D/"
                                (http-server-port server)))
                  :timeout 5))
           (setf tls-stream
                 (funcall
                  (%https-listener-real-tls-upgrader material)
                  transport
                  (parse-http-uri
                   (format nil "https://localhost:~D/"
                           (http-server-port server)))
                  :timeout 5))
           (write-sequence
            (ascii "GET / HTTP/1.1|CRLF|Host: localhost|CRLF|Connection: close|CRLF||CRLF|")
            tls-stream)
           (finish-output tls-stream)
           (ensure-true
            (sb-thread:wait-on-semaphore close-notify-entered :timeout 5))
           (ensure-equal server (stop-http-server server))
           (ensure-false (open-stream-p close-notify-transport))
           ;; A blocked close_notify must be released only after the stored
           ;; stop deadline causes the coordinator to abort the raw transport.
           (ensure-true force-close-at)
           (ensure-true force-close-deadline)
           (ensure-true (>= force-close-at force-close-deadline)))
      (sb-ext:without-package-locks
        (setf (fdefinition 'http-kit/tls::%tls-server-send-close-notify)
              original-close-notify
              (fdefinition 'http-kit::%http-server-force-close-connections)
              original-force-close))
      (when (and tls-stream (open-stream-p tls-stream))
        (close tls-stream :abort t))
      (when (and transport (open-stream-p transport))
        (close-http-tcp-stream transport))
      (unless (eq :stopped (http-kit::http-server-state server))
        (stop-http-server server :graceful-timeout 1))
      (%tls-server-test-remove-material material))))
#+sbcl
(deftest https-listener-aborts-http2-after-size-limit
  (%with-https-listener-test-stubs (context)
    (let* ((server (%https-listener-make-server))
           (raw-stream (make-string-input-stream "raw"))
           (original-close
             (fdefinition 'http-kit::%http-server-close-connection-stream))
           (connection nil)
           (session-called-p nil)
           (close-abort nil)
           (termination
             (make-condition 'http-size-limit-exceeded
                             :message "request field limit exceeded"
                             :kind :fields
                             :limit 3
                             :observed 4)))
      (unwind-protect
           (progn
             (setf (http-kit::http-server-state server) :running
                   (%https-listener-test-context-alpn context) "h2"
                   (%https-listener-test-context-session-action context)
                   (lambda (protocol stream handler options)
                     (declare (ignore stream handler options))
                     (setf session-called-p (eq protocol :http2))
                     (values 0 termination)))
             (sb-ext:without-package-locks
               (setf (fdefinition
                      'http-kit::%http-server-close-connection-stream)
                     (lambda (candidate connection &key (abort t))
                       (setf close-abort abort)
                       (funcall original-close candidate connection
                                :abort abort))))
             (multiple-value-bind (reserved rejection)
                 (http-kit::%http-server-reserve-connection
                  server nil raw-stream "127.0.0.1" 1234)
               (ensure-false rejection)
               (setf connection reserved))
             (http-kit::%http-server-run-connection server connection)
             (ensure-true session-called-p)
             (ensure-equal t close-abort)
             (ensure-false (open-stream-p raw-stream))
             (ensure-equal 0 (http-kit::http-server-connection-count server))
             (ensure-equal :http2
                           (first
                            (first
                             (%https-listener-test-context-session-calls
                              context)))))
        (sb-ext:without-package-locks
          (setf (fdefinition
                 'http-kit::%http-server-close-connection-stream)
                original-close))
        (when (open-stream-p raw-stream)
          (close raw-stream :abort t))
        (unless (eq :stopped (http-kit::http-server-state server))
          (stop-http-server server :graceful-timeout 1))))))
