(in-package #:http-kit/test)

#+sbcl
(progn
  (defstruct (%http3-listener-observations
               (:constructor %make-http3-listener-observations))
    (lock (sb-thread:make-mutex :name "HTTP/3 listener test observations"))
    (events nil)
    (errors nil))

  (defun %http3-listener-record-event (observations event)
    (sb-thread:with-mutex ((%http3-listener-observations-lock observations))
      (push (copy-tree event)
            (%http3-listener-observations-events observations))))

  (defun %http3-listener-record-error (observations condition context)
    (sb-thread:with-mutex ((%http3-listener-observations-lock observations))
      (push (list :type (type-of condition)
                  :condition condition
                  :printed (princ-to-string condition)
                  :context (copy-tree context))
            (%http3-listener-observations-errors observations))))

  (defun %http3-listener-events (observations)
    (sb-thread:with-mutex ((%http3-listener-observations-lock observations))
      (reverse (copy-tree (%http3-listener-observations-events observations)))))

  (defun %http3-listener-errors (observations)
    (sb-thread:with-mutex ((%http3-listener-observations-lock observations))
      (reverse (copy-list (%http3-listener-observations-errors observations)))))

  (defun %http3-listener-error-summary (observations)
    (mapcar
     (lambda (record)
       (let ((condition (getf record :condition))
             (context (getf record :context)))
         (list (getf record :type)
               (getf record :printed)
               (and (typep condition 'http-kit:http-error)
                    (list :message (http-kit:http-error-message condition)
                          :operation (http-kit:http-error-operation condition)
                          :timeout-kind
                          (and (typep condition 'http-kit:http-timeout)
                               (http-kit:http-timeout-kind condition))
                          :protocol-detail
                          (and (typep condition 'http-kit:http-protocol-error)
                               (http-kit:http-protocol-error-detail condition))
                          :quic-error-code
                          (and (typep condition
                                      'http-kit/http3::http3-connection-protocol-error)
                               (http-kit/http3::http3-connection-protocol-error-quic-error-code
                                condition))))
               (and (typep condition
                           'http-kit/http3:http3-server-connection-closed)
                    (list
                     :close-kind
                     (http-kit/http3:http3-server-connection-close-kind
                      condition)
                     :close-error-code
                     (http-kit/http3:http3-server-connection-close-error-code
                      condition)
                     :close-reason
                     (http-kit/http3:http3-server-connection-close-reason
                      condition)))
               (and (typep condition 'http-server-error)
                    (http-server-error-reason-code condition))
               (getf context :phase)
               (getf context :protocol)
               (getf context :source))))
     (%http3-listener-errors observations)))

  (defun %http3-listener-safe-error-p (record reason-code)
    (let ((condition (getf record :condition))
          (context (getf record :context)))
      (and (typep condition 'http-server-error)
           (eq reason-code (http-server-error-reason-code condition))
           (eq :session (http-server-error-phase condition))
           (eq :session (getf context :phase))
           (eq :http3 (getf context :protocol)))))

  (defun %http3-listener-event (observations type)
    (find-if (lambda (event) (eq (getf event :type) type))
             (%http3-listener-events observations)))

  (defun %http3-listener-wait-until (predicate &optional (timeout 5))
    (let ((deadline
            (+ (get-internal-real-time)
               (ceiling (* timeout internal-time-units-per-second)))))
      (loop
        (when (funcall predicate)
          (return t))
        (when (>= (get-internal-real-time) deadline)
          (return nil))
        (sleep 0.01))))

  (defun %http3-listener-test-material ()
    (let* ((directory
             (merge-pathnames
              (format nil "cl-http-kit-http3-listener-~D-~D/"
                      (get-universal-time) (random 1000000))
              (uiop:temporary-directory)))
           (certificate (merge-pathnames "server.crt" directory))
           (key (merge-pathnames "server.key" directory))
           (der-key (merge-pathnames "server.der" directory)))
      (ensure-directories-exist certificate)
      (flet ((run-openssl (arguments)
               (multiple-value-bind (output errors status)
                   (uiop:run-program (cons (or (uiop:getenv "OPENSSL") "openssl")
                                           arguments)
                                     :output :string :error-output :string
                                     :ignore-error-status t)
                 (declare (ignore output errors))
                 (unless (zerop status)
                   (error "HTTP/3 listener test certificate generation failed.")))))
        (run-openssl
         (list "req" "-x509" "-newkey" "ec"
               "-pkeyopt" "ec_paramgen_curve:P-256" "-nodes"
               "-days" "1" "-subj" "/CN=localhost"
               "-addext" "subjectAltName=DNS:localhost"
               "-addext" "basicConstraints=critical,CA:TRUE"
               "-addext" "keyUsage=critical,keyCertSign,digitalSignature"
               "-addext" "extendedKeyUsage=serverAuth"
               "-keyout" (namestring key) "-out" (namestring certificate)))
        (run-openssl
         (list "ec" "-in" (namestring key) "-outform" "DER"
               "-out" (namestring der-key))))
      (let* ((blocks
               (cl-tls-kit:pem-decode (uiop:read-file-string certificate)))
             (block
               (find "CERTIFICATE" blocks
                     :key #'cl-tls-kit:pem-block-label :test #'string=)))
        (unless block
          (error "HTTP/3 listener test certificate is unavailable."))
        (list :directory directory
              :files (list certificate key der-key)
              :certificate-path certificate
              :key-path key
              :der-key-path der-key
              :certificate-chain (list (cl-tls-kit:pem-block-der block))
              :private-key
              (%tls-server-test-private-key
               (%tls-server-test-read-octets der-key))))))

  (defun %call-with-http3-listener
      (handler function &key options event-callback error-callback
                             server-error-callback)
    (let* ((material (%http3-listener-test-material))
           (observations (%make-http3-listener-observations))
           (server
             (apply
              #'make-http-server
              (append
               options
               (list :transport :http3
                     :host "127.0.0.1"
                     :port 0
                     :handler handler
                     :certificate-chain (getf material :certificate-chain)
                     :private-key (getf material :private-key)
                     :handshake-timeout 2
                     :request-timeout 3
                     :idle-timeout 10
                     :graceful-timeout 2
                     :on-event
                     (lambda (event)
                       (%http3-listener-record-event observations event)
                       (when event-callback
                         (funcall event-callback event)))
                     :on-error
                     (or error-callback
                         (lambda (condition context)
                           (%http3-listener-record-error
                            observations condition context))))))))
      (unwind-protect
           (progn
             (start-http-server server)
             (if server-error-callback
                 (let ((original-report
                         (symbol-function
                          'http-kit::%http-server-report-error)))
                   (%http3-listener-test-call-with-function-stubs
                    (list
                     (cons 'http-kit::%http-server-report-error
                           (lambda (actual-server condition phase &rest arguments)
                             (funcall server-error-callback
                                      condition phase observations)
                             (apply original-report actual-server condition phase
                                    arguments))))
                    (lambda ()
                      (funcall function server material observations))))
               (funcall function server material observations)))
        (when (member (http-kit::http-server-state server)
                      '(:running :starting :stopping :failed))
          (stop-http-server server :graceful-timeout 0.5))
        (%tls-server-test-remove-material material))))

  (defun %http3-listener-peer-arguments
      (material mode port &rest arguments)
    (append
     (list (or (uiop:getenv "HTTP_QUIC_PEER")
               (error "HTTP_QUIC_PEER must name the quic-peer fixture executable."))
           "--mode" mode
           "--target-port" (princ-to-string port)
           "--ca" (namestring (getf material :certificate-path))
           "--timeout" "20s")
     arguments))

  (defun %http3-listener-run-peer (material mode port &rest arguments)
    (uiop:run-program
     (apply #'%http3-listener-peer-arguments material mode port arguments)
     :output :string :error-output :string :ignore-error-status t))

  (defun %http3-listener-launch-peer (material mode port &rest arguments)
    (uiop:launch-program
     (apply #'%http3-listener-peer-arguments material mode port arguments)
     :input :stream :output :stream :error-output :stream :wait nil))

  (defun %http3-listener-peer-line (process &optional (expected "the expected line"))
    (let ((line
            (sb-ext:with-timeout 12
              (read-line (uiop:process-info-output process) nil :eof))))
      (when (eq line :eof)
        (let* ((alive-p (uiop:process-alive-p process))
               (exit-code (and (not alive-p)
                               (uiop:wait-process process)))
               (errors (unless alive-p
                         (read-line (uiop:process-info-error-output process)
                                    nil ""))))
          (error "The HTTP/3 fixture closed its output before ~A (exit ~S): ~A"
                 expected exit-code errors)))
      line))

  (defun %http3-listener-peer-release (process)
    (write-line "continue" (uiop:process-info-input process))
    (force-output (uiop:process-info-input process)))

  (defun %http3-listener-peer-wait (process)
    (sb-ext:with-timeout 8
      (uiop:wait-process process)))

  (defun %http3-listener-peer-cleanup (process)
    (when (and process (uiop:process-alive-p process))
      (ignore-errors (%http3-listener-peer-release process))
      (ignore-errors (uiop:terminate-process process))
      (ignore-errors (uiop:wait-process process)))
    nil)

  (defun %http3-listener-connection-count (server)
    (http-kit::http-server-connection-count server))

  (defstruct (%http3-listener-gate (:constructor %make-http3-listener-gate))
    (lock (sb-thread:make-mutex :name "HTTP/3 listener test gate"))
    (waitqueue (sb-thread:make-waitqueue :name "HTTP/3 listener test gate"))
    (entered-semaphore (sb-thread:make-semaphore :count 0))
    (entered 0)
    (released-p nil))

  (defun %http3-listener-gate-block (gate)
    (sb-thread:with-mutex ((%http3-listener-gate-lock gate))
      (incf (%http3-listener-gate-entered gate))
      (sb-thread:signal-semaphore
       (%http3-listener-gate-entered-semaphore gate))
      (sb-thread:condition-broadcast (%http3-listener-gate-waitqueue gate))
      (loop until (%http3-listener-gate-released-p gate)
            do (sb-thread:condition-wait
                (%http3-listener-gate-waitqueue gate)
                (%http3-listener-gate-lock gate)))))

  (defun %http3-listener-gate-wait-for-entry (gate timeout)
    (sb-thread:wait-on-semaphore
     (%http3-listener-gate-entered-semaphore gate)
     :timeout timeout))

  (defun %http3-listener-gate-count (gate)
    (sb-thread:with-mutex ((%http3-listener-gate-lock gate))
      (%http3-listener-gate-entered gate)))

  (defun %http3-listener-gate-release (gate)
    (sb-thread:with-mutex ((%http3-listener-gate-lock gate))
      (setf (%http3-listener-gate-released-p gate) t)
      (sb-thread:condition-broadcast (%http3-listener-gate-waitqueue gate)))
    nil)

  (defun %http3-listener-version-at-least-p (version major minor patch)
    (let ((parts
            (loop with start = 0
                  for end = (position #\. version :start start)
                  collect (parse-integer version :start start :end end)
                  while end
                  do (setf start (1+ end)))))
      (loop for expected in (list major minor patch)
            for actual in (append parts '(0 0 0))
            when (> actual expected) return t
            when (< actual expected) return nil
            finally (return t))))

  (defun %http3-listener-secret-representations (material)
    (let* ((der (%tls-server-test-read-octets
                 (getf material :der-key-path)))
           (pem (uiop:read-file-string (getf material :key-path))))
      (list pem
            (let ((*print-array* t))
              (write-to-string der))
            (with-output-to-string (stream)
              (loop for octet across der do (format stream "~2,'0X" octet))))))

  (defun %http3-listener-observation-text (observations)
    (with-output-to-string (stream)
      (let ((*print-array* t) (*print-circle* t))
        (prin1 (%http3-listener-events observations) stream)
        (prin1 (%http3-listener-errors observations) stream))))

  (defun %http3-listener-gated-response (gate)
    (let ((index 0))
      (make-http-response-stream
       :status 200
       :body-function
       (lambda ()
         (case (prog1 index (incf index))
           (0 (ascii "first"))
           (1 (progn
                (%http3-listener-gate-block gate)
                (ascii "second")))
           (otherwise nil))))))

  (deftest http3-listener-validates-handshake-timeout-configuration
    (let ((material (%http3-listener-test-material)))
      (unwind-protect
           (progn
             (let ((server
                     (make-http-server
                      :transport :http3
                      :host "127.0.0.1"
                      :port 0
                      :handler (lambda (request)
                                 (declare (ignore request))
                                 (make-http-response :status 200))
                      :certificate-chain (getf material :certificate-chain)
                      :private-key (getf material :private-key)
                      :handshake-timeout 0.25)))
               (ensure-equal 0.25
                             (http-kit::http-server-handshake-timeout server)))
             (dolist (timeout '(nil 0 -1))
               (signals http-server-error
                 (make-http-server
                  :transport :http3
                  :host "127.0.0.1"
                  :port 0
                  :handler (lambda (request)
                             (declare (ignore request))
                             (make-http-response :status 200))
                  :certificate-chain (getf material :certificate-chain)
                  :private-key (getf material :private-key)
                  :handshake-timeout timeout))))
        (%tls-server-test-remove-material material))))

  (deftest http3-listener-covers-server-configuration-guards
    (let* ((material (%http3-listener-test-material))
           (handler (lambda (request)
                      (declare (ignore request))
                      (make-http-response :status 200))))
      (unwind-protect
           (progn
             (let ((ticket-store
                     (cl-tls-kit:make-tls13-server-ticket-key-store)))
               (unwind-protect
                    (let ((condition
                            (handler-case
                                (progn
                                  (make-http-server
                                   :transport :http3
                                   :host "127.0.0.1"
                                   :port 0
                                   :handler handler
                                   :certificate-chain
                                   (getf material :certificate-chain)
                                   :private-key (getf material :private-key)
                                   :ticket-key-store ticket-store)
                                  nil)
                              (http-server-error (condition) condition))))
                      (ensure-true condition)
                      (ensure-equal :invalid-ticket-key-store
                                    (http-server-error-reason-code condition)))
                 (cl-tls-kit:destroy-tls13-server-ticket-key-store ticket-store)))
             (let* ((server
                      (make-http-server
                       :transport :http3
                       :host "127.0.0.1"
                       :port 0
                       :handler handler
                       :certificate-chain (getf material :certificate-chain)
                       :private-key (getf material :private-key)))
                    (condition
                      (let ((http-kit::*http-server-http3-hooks* nil))
                        (handler-case
                            (progn (start-http-server server) nil)
                          (http-server-error (condition) condition)))))
               (ensure-true condition)
               (ensure-equal :unsupported-transport
                             (http-server-error-reason-code condition))
               (ensure-equal :failed (http-kit::http-server-state server))
               (ensure-true (eq condition (http-server-failure server)))))
        (%tls-server-test-remove-material material))))

  (deftest http3-listener-retry-request-stream-response-and-alpn
    (let ((seen-request nil))
      (%call-with-http3-listener
       (lambda (request)
         (setf seen-request request)
         (let ((chunks (list (ascii "first") (ascii "second") nil)))
           (make-http-response-stream
            :status 201
            :body-function (lambda () (pop chunks)))))
       (lambda (server material observations)
         (multiple-value-bind (output errors status)
             (%http3-listener-run-peer
              material "http3-client" (http-server-port server)
              "--method" "POST" "--body" "request-data"
              "--header" "X-Listener-Test: present")
           (unless (zerop status)
             (error "The HTTP/3 request failed: ~A; listener observations: ~A"
                    errors (%http3-listener-observation-text observations)))
           (ensure-equal 0 status)
           (ensure-true (search "HTTP3 STATUS=201 RETRY=true" output))
           (ensure-true (search "BODY=\"firstsecond\"" output)))
         (ensure-true seen-request)
         (ensure-equal "POST" (http-request-method seen-request))
         (ensure-equal "request-data"
                       (octets-as-string (http-request-body seen-request)))
         (ensure-equal "present"
                       (http-header-value (http-request-headers seen-request)
                                          "x-listener-test"))
         (ensure-true
          (let ((event (%http3-listener-event observations :connection-opened)))
            (and event (eq :http3 (getf event :protocol)))))
         (multiple-value-bind (output errors status)
             (%http3-listener-run-peer
              material "alpn-mismatch" (http-server-port server))
           (unless (zerop status)
             (error "The HTTP/3 ALPN-mismatch probe failed: ~A" errors))
           (ensure-equal 0 status)
           (ensure-true (search "ALPN_MISMATCH REJECTED=true" output)))))))

  (deftest http3-listener-connection-limit-releases-after-close
    (%call-with-http3-listener
     (lambda (request)
       (declare (ignore request))
       (make-http-response :status 200 :body (ascii "connection-ok")))
     (lambda (server material observations)
       (declare (ignorable observations))
       (let ((peer nil))
         (unwind-protect
              (progn
                (setf peer
                      (%http3-listener-launch-peer
                       material "http3-client" (http-server-port server)
                       "--hold-after-response"))
                (ensure-true
                 (search "HTTP3 HEADERS STATUS=200"
                         (%http3-listener-peer-line peer)))
                (ensure-true
                 (search "HTTP3 STATUS=200"
                         (%http3-listener-peer-line peer)))
                (ensure-true
                 (%http3-listener-wait-until
                  (lambda () (= 1 (%http3-listener-connection-count server)))))
                (multiple-value-bind (output errors status)
                    (%http3-listener-run-peer
                     material "http3-client" (http-server-port server)
                     "--timeout" "700ms")
                  (declare (ignore output errors))
                  (ensure-false (zerop status)))
                (%http3-listener-peer-release peer)
                (ensure-equal 0 (%http3-listener-peer-wait peer))
                (ensure-true
                 (%http3-listener-wait-until
                  (lambda () (zerop (%http3-listener-connection-count server)))))
                (ensure-true
                 (%http3-listener-event observations :connection-closed)))
           (%http3-listener-peer-cleanup peer))))
     :options '(:max-connections 1 :max-handshakes 1)))

  (deftest http3-listener-handshake-limit-and-timeout-capability
    (%call-with-http3-listener
     (lambda (request)
       (declare (ignore request))
       (make-http-response :status 200 :body (ascii "handshake-ok")))
     (lambda (server material observations)
       (let* ((peer nil)
              (version (asdf:component-version
                        (asdf:find-system "cl-quic-kit"))))
         (ensure-true
          (%http3-listener-version-at-least-p version 0 2 2))
         (unwind-protect
              (progn
                (setf peer
                      (%http3-listener-launch-peer
                       material "pending-handshake" (http-server-port server)
                       "--timeout" "8s"))
                (ensure-equal "PENDING READY RETRY=true"
                              (%http3-listener-peer-line
                               peer "post-Retry handshake readiness"))
                (ensure-false
                 (%http3-listener-wait-until
                  (lambda ()
                    (%http3-listener-event
                     observations :handshake-timeout-enforcement-unavailable))
                  0.5))
                (multiple-value-bind (output errors status)
                    (%http3-listener-run-peer
                     material "http3-client" (http-server-port server)
                     "--timeout" "700ms")
                  (declare (ignore output errors))
                  (ensure-false (zerop status)))
                (sleep 2.3)
                (ensure-true (uiop:process-alive-p peer))
                (multiple-value-bind (output errors status)
                    (%http3-listener-run-peer
                     material "http3-client" (http-server-port server))
                  (unless (zerop status)
                    (error "The post-timeout HTTP/3 client failed: ~A" errors))
                  (ensure-equal 0 status)
                  (ensure-true (search "handshake-ok" output)))
                (%http3-listener-peer-release peer)
                (ensure-equal
                 "PENDING CLOSED"
                 (%http3-listener-peer-line peer "handshake process closure"))
                (ensure-equal 0 (%http3-listener-peer-wait peer))
                (multiple-value-bind (output errors status)
                    (%http3-listener-run-peer
                     material "http3-client" (http-server-port server))
                  (unless (zerop status)
                    (error "The post-close HTTP/3 client failed: ~A" errors))
                  (ensure-equal 0 status)
                  (ensure-true (search "handshake-ok" output))))
           (%http3-listener-peer-cleanup peer))))
     :options '(:max-connections 4 :max-handshakes 1 :handshake-timeout 2)))

  (deftest http3-listener-active-stream-limit-rejects-and-retries-excess-stream
    (let ((gate (%make-http3-listener-gate)))
      (unwind-protect
           (%call-with-http3-listener
            (lambda (request)
              (if (string= "/__cl_http_kit_warmup"
                           (http-uri-path (http-request-uri request)))
                  (make-http-response :status 204)
                  (progn
                    (%http3-listener-gate-block gate)
                    (make-http-response :status 200
                                        :body (ascii "stream-ok")))))
            (lambda (server material observations)
              (declare (ignorable observations))
              (let ((peer
                      (%http3-listener-launch-peer
                       material "http3-concurrent" (http-server-port server)
                       "--requests" "2" "--warm-up" "--timeout" "20s")))
                (unwind-protect
                     (progn
                       (ensure-true
                        (%http3-listener-wait-until
                         (lambda ()
                           (find-if
                            (lambda (event)
                              (and (eq (getf event :type)
                                       :http3-request-stream-rejected)
                                   (eq (getf event :reason)
                                       :max-concurrent-streams)))
                            (%http3-listener-events observations)))
                         5))
                       ;; quic-go retries H3_REQUEST_REJECTED on a fresh connection.
                       (ensure-true
                        (%http3-listener-wait-until
                         (lambda ()
                           (>= (count :connection-opened
                                      (%http3-listener-events observations)
                                      :key (lambda (event)
                                             (getf event :type)))
                               2))
                         5))
                       (ensure-equal
                        2
                        (count :connection-opened
                               (%http3-listener-events observations)
                               :key (lambda (event) (getf event :type))))
                       (unless
                           (%http3-listener-gate-wait-for-entry gate 10)
                         (error "No HTTP/3 request reached its handler: ~A"
                                (%http3-listener-observation-text observations)))
                       (unless
                           (%http3-listener-gate-wait-for-entry gate 5)
                         (error
                          "The retried HTTP/3 request did not reach its handler: ~A"
                          (%http3-listener-observation-text observations)))
                       (%http3-listener-gate-release gate)
                       (loop repeat 2
                             do (ensure-true
                                 (search "STATUS=200"
                                         (%http3-listener-peer-line peer))))
                       (ensure-equal 2 (%http3-listener-gate-count gate))
                       (ensure-equal 0 (%http3-listener-peer-wait peer)))
                  (%http3-listener-gate-release gate)
                  (%http3-listener-peer-cleanup peer))))
            :options '(:max-concurrent-streams 1 :idle-timeout 30))
        (%http3-listener-gate-release gate))))

  (deftest http3-listener-header-field-and-body-limits
    (dolist (case
              '((:header-bytes 256
                 "--header" "X-Large: <value>")
                (:fields 4
                 "--header" "X-One: 1" "--header" "X-Two: 2"
                 "--header" "X-Three: 3" "--header" "X-Four: 4"
                 "--header" "X-Five: 5" "--header" "X-Six: 6")))
      (let* ((kind (first case))
             (limit (second case))
             (arguments (cddr case))
             (handler-calls 0)
             (client-result nil)
             (options
               (if (eq kind :header-bytes)
                   (list :max-header-bytes limit)
                   (list :max-header-bytes 4096 :max-fields 4))))
        (when (eq kind :header-bytes)
          (setf arguments
                (list "--header"
                      (concatenate 'string "X-Large: "
                                   (make-string 1024 :initial-element #\x)))))
        (%call-with-http3-listener
         (lambda (request)
           (declare (ignore request))
           (incf handler-calls)
           (make-http-response :status 200 :body (ascii "unexpected")))
         (lambda (server material observations)
           (multiple-value-bind (output errors status)
               (apply #'%http3-listener-run-peer
                      material "http3-client" (http-server-port server)
                      arguments)
             (setf client-result
                   (list :status status :output output :error errors))
             (unless (not (zerop status))
               (error "The ~S limit client succeeded: ~A; ~A"
                      kind output errors)))
           (ensure-equal 0 handler-calls)
           (unless
               (%http3-listener-wait-until
                (lambda () (%http3-listener-errors observations)))
             (error "The header limit produced no HTTP/3 error: listener ~A; client ~S"
                    (%http3-listener-observation-text observations)
                    client-result))
           (let* ((record
                    (find-if
                     (lambda (candidate)
                       (%http3-listener-safe-error-p
                        candidate :protocol-error))
                     (%http3-listener-errors observations)))
                  )
             (ensure-true record)))
         :options options)))
    (let ((handler-calls 0)
          (client-result nil))
      (%call-with-http3-listener
       (lambda (request)
         (declare (ignore request))
         (incf handler-calls)
         (make-http-response :status 200 :body (ascii "unexpected")))
       (lambda (server material observations)
         (ensure-equal 4 (http-kit::http-server-max-body-bytes server))
         (multiple-value-bind (output errors status)
             (%http3-listener-run-peer
              material "http3-client" (http-server-port server)
              "--method" "POST" "--body" "12345")
           (setf client-result
                 (list :status status :output output :error errors))
           (unless (and (not (zerop status))
                        (or (search "H3_EXCESSIVE_LOAD" output)
                            (search "H3_EXCESSIVE_LOAD" errors)))
             (error "Body limit response: ~A; ~A" output errors)))
         (ensure-equal 0 handler-calls)
         (unless
             (%http3-listener-wait-until
              (lambda () (%http3-listener-errors observations)))
           (error "The body limit produced no HTTP/3 error: listener ~A; client ~S"
                  (%http3-listener-observation-text observations)
                  client-result))
         (let ((error-record
                 (find-if
                  (lambda (record)
                    (%http3-listener-safe-error-p
                     record :protocol-error))
                  (%http3-listener-errors observations))))
           (ensure-true error-record)
           (let ((printed (%http3-listener-observation-text observations)))
             (dolist (needle (%http3-listener-secret-representations material))
               (ensure-false (search needle printed))))))
       :options '(:max-body-bytes 4 :max-header-bytes 4096))))

  (deftest http3-listener-aggregate-body-limit-releases-budget
    (let ((gate (%make-http3-listener-gate)))
      (unwind-protect
           (%call-with-http3-listener
            (lambda (request)
              (declare (ignore request))
              (%http3-listener-gate-block gate)
              (make-http-response :status 200 :body (ascii "aggregate-ok")))
            (lambda (server material observations)
              (let ((peer
                      (%http3-listener-launch-peer
                       material "http3-concurrent" (http-server-port server)
                       "--requests" "2" "--method" "POST" "--body" "1234")))
                (unwind-protect
                     (progn
                       (ensure-true
                        (%http3-listener-wait-until
                         (lambda () (= 1 (%http3-listener-gate-count gate)))))
                       (let ((result (%http3-listener-peer-line peer)))
                         (ensure-true (search "ERROR=" result)))
                       (ensure-equal 8
                                     (http-kit::http-server-buffered-body-bytes
                                      server))
                       (ensure-true
                        (some
                         (lambda (record)
                           (%http3-listener-safe-error-p
                            record :protocol-error))
                         (%http3-listener-errors observations)))
                       (%http3-listener-gate-release gate)
                       (let ((result (%http3-listener-peer-line peer)))
                         (ensure-true (search "STATUS=200" result)))
                       (ensure-equal 0 (%http3-listener-peer-wait peer))
                       (ensure-equal 0
                                     (http-kit::http-server-buffered-body-bytes
                                      server)))
                  (%http3-listener-gate-release gate)
                  (%http3-listener-peer-cleanup peer))))
            :options '(:max-body-bytes 8 :max-buffered-body-bytes 12
                       :max-header-bytes 4096))
        (%http3-listener-gate-release gate))))

  (deftest http3-listener-request-deadline-is-absolute
    (%call-with-http3-listener
     (lambda (request)
       (declare (ignore request))
       (make-http-response :status 200 :body (ascii "unexpected")))
     (lambda (server material observations)
       (multiple-value-bind (output errors status)
           (%http3-listener-run-peer
            material "http3-client" (http-server-port server)
            "--method" "POST" "--body" "123456"
            "--slow-body-interval" "120ms")
         (declare (ignore output errors))
         (ensure-false (zerop status)))
       (ensure-true
        (%http3-listener-wait-until
         (lambda ()
           (some (lambda (record)
                   (%http3-listener-safe-error-p record :timeout))
                 (%http3-listener-errors observations)))))
       (ensure-true
        (some (lambda (record)
                (%http3-listener-safe-error-p record :timeout))
              (%http3-listener-errors observations))))
     :options '(:request-timeout 0.3 :max-body-bytes 16
                :max-header-bytes 4096)))

  (deftest http3-listener-goaway-drains-active-stream
    (let ((gate (%make-http3-listener-gate))
          (stopper nil)
          (peer nil))
      (unwind-protect
           (%call-with-http3-listener
            (lambda (request)
              (declare (ignore request))
              (%http3-listener-gated-response gate))
            (lambda (server material observations)
              (setf peer
                    (%http3-listener-launch-peer
                     material "http3-client" (http-server-port server)
                     "--hold-after-headers"))
              (ensure-true
               (search "HTTP3 HEADERS STATUS=200"
                       (%http3-listener-peer-line peer)))
              (ensure-true
               (%http3-listener-wait-until
                (lambda () (= 1 (%http3-listener-gate-count gate)))))
              (setf stopper
                    (sb-thread:make-thread
                     (lambda () (stop-http-server server :graceful-timeout 2))
                     :name "HTTP/3 listener graceful-stop test"))
              (ensure-true
               (%http3-listener-wait-until
                (lambda ()
                  (%http3-listener-event observations :http3-goaway-sent))))
              (let ((event (%http3-listener-event observations :http3-goaway-sent)))
                (ensure-true (member :goaway-id event)))
              (%http3-listener-gate-release gate)
              (%http3-listener-peer-release peer)
              (ensure-true
               (search "BODY=\"firstsecond\""
                       (%http3-listener-peer-line peer)))
              (ensure-equal 0 (%http3-listener-peer-wait peer))
              (ensure-true
               (sb-ext:with-timeout 5
                 (sb-thread:join-thread stopper)
                 (%http3-listener-wait-until
                  (lambda ()
                    (eq :stopped (http-kit::http-server-state server))))))
              (ensure-equal :stopped (http-kit::http-server-state server))))
        (%http3-listener-gate-release gate)
        (%http3-listener-peer-cleanup peer)
        (when (and stopper (sb-thread:thread-alive-p stopper))
          (sb-thread:join-thread stopper)))))

  (deftest http3-listener-grace-timeout-closes-live-connection
    (let ((gate (%make-http3-listener-gate))
          (stopper nil)
          (peer nil))
      (unwind-protect
           (%call-with-http3-listener
            (lambda (request)
              (declare (ignore request))
              (%http3-listener-gated-response gate))
            (lambda (server material observations)
              (setf peer
                    (%http3-listener-launch-peer
                     material "http3-client" (http-server-port server)
                     "--hold-after-headers"))
              (ensure-true
               (search "HTTP3 HEADERS STATUS=200"
                       (%http3-listener-peer-line peer)))
              (ensure-true
               (%http3-listener-wait-until
                (lambda () (= 1 (%http3-listener-gate-count gate)))))
              (setf stopper
                    (sb-thread:make-thread
                     (lambda () (stop-http-server server :graceful-timeout 0.15))
                     :name "HTTP/3 listener grace-timeout test"))
              (ensure-true
               (%http3-listener-wait-until
                (lambda ()
                  (%http3-listener-event observations :http3-goaway-sent))))
              (sleep 0.3)
              (%http3-listener-gate-release gate)
              (%http3-listener-peer-release peer)
              (ensure-false (zerop (%http3-listener-peer-wait peer)))
              (ensure-true
               (sb-ext:with-timeout 5
                 (sb-thread:join-thread stopper)))
              (ensure-equal :stopped (http-kit::http-server-state server))
              (ensure-equal 0 (%http3-listener-connection-count server))))
        (%http3-listener-gate-release gate)
        (%http3-listener-peer-cleanup peer)
        (when (and stopper (sb-thread:thread-alive-p stopper))
          (sb-thread:join-thread stopper)))))

  (defun %http3-listener-test-call-with-function-stubs (bindings thunk)
    (let ((originals
            (mapcar (lambda (binding)
                      (cons (car binding) (fdefinition (car binding))))
                    bindings)))
      (unwind-protect
           (progn
             (sb-ext:without-package-locks
               (dolist (binding bindings)
                 (setf (fdefinition (car binding)) (cdr binding))))
             (funcall thunk))
        (sb-ext:without-package-locks
          (dolist (binding originals)
            (setf (fdefinition (car binding)) (cdr binding)))))))

  (defun %http3-listener-test-unit-server (&rest options)
    (apply #'http-kit::%make-http-server
           (append options
                   '(:transport :http3
                     :host "127.0.0.1"
                     :port 0
                     :state :running
                     :max-connections 8
                     :max-handshakes 4
                     :max-concurrent-streams 4
                     :handshake-timeout 1
                     :request-timeout 1
                     :idle-timeout nil
                     :graceful-timeout 1
                     :max-header-bytes 4096
                     :max-fields 32
                     :max-body-bytes 4096
                     :max-buffered-body-bytes 8192
                     :handler (lambda (request)
                                (declare (ignore request))
                                (make-http-response :status 200))))))

  (defun %http3-listener-test-unit-connection
      (&key quic-connection (active-request-streams 0))
    (let ((raw (or quic-connection (gensym "QUIC-CONNECTION-"))))
      (http-kit/http3::%make-http3-server-connection
       :quic-connection raw
       :server-connection
       (http-kit::%make-http-server-connection
        :raw-stream raw :handshake-p nil :protocol :http3)
       :active-request-streams active-request-streams
       :highest-request-stream-id 0
       :last-activity 0)))

  (deftest http3-listener-coverage-startup-and-close-cleanup
    (let ((mode :quic-server-fails)
          (udp-close-count 0)
          (quic-server nil)
          (original-listener-constructor
            (symbol-function
             'http-kit/http3::%make-http3-server-listener)))
      (%http3-listener-test-call-with-function-stubs
       (list
        (cons 'http-kit/http3::%http3-quic-adapter-configure-protection
              (lambda () nil))
        (cons 'cl-quic-kit:make-udp-socket
              (lambda (&rest arguments)
                (declare (ignore arguments))
                :udp-socket))
        (cons 'cl-quic-kit:make-quic-server
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (if (eq mode :quic-server-fails)
                    (error 'http-server-error
                           :reason-code :listener-open-failed
                           :phase :session)
                    (setf quic-server
                          (cl-quic-kit::%make-quic-server
                           :udp-socket :udp-socket
                           :connections (make-hash-table :test #'equalp)
                           :retry-tokens (make-hash-table :test #'equalp)
                           :paths (make-hash-table :test #'equalp))))))
        (cons 'http-kit/http3::%make-http3-server-listener
              (lambda (&rest arguments)
                (if (eq mode :listener-constructor-fails)
                    (error "listener allocation failed")
                    (apply original-listener-constructor arguments))))
        (cons 'cl-quic-kit:udp-close
              (lambda (socket)
                (ensure-equal :udp-socket socket)
                (incf udp-close-count)))
        (cons 'http-kit::%http-server-configuration-error
              (lambda (reason)
                (error 'http-server-error
                       :reason-code reason :phase :session))))
       (lambda ()
         (let ((server (%http3-listener-test-unit-server)))
           (ensure-true
            (handler-case
                (progn
                  (http-kit/http3::%http3-server-start server nil)
                  nil)
              (error () t)))
           (ensure-equal 1 udp-close-count)
           (setf mode :listener-constructor-fails)
           (ensure-true
            (handler-case
                (progn
                  (http-kit/http3::%http3-server-start server nil)
                  nil)
              (error () t)))
           (ensure-true
            (cl-quic-kit::quic-server-closed-p quic-server))
           (setf mode :success)
           (let* ((listener
                    (http-kit/http3::%make-http3-server-listener
                     :server server :quic-server :quic-server
                     :socket :udp-socket))
                  (close-count 0))
             (%http3-listener-test-call-with-function-stubs
              (list
               (cons 'cl-quic-kit:quic-server-close
                     (lambda (quic-server)
                       (ensure-equal :quic-server quic-server)
                       (incf close-count))))
              (lambda ()
                (http-kit/http3::%http3-server-close-listener listener)
                (http-kit/http3::%http3-server-close-listener listener)))
             (ensure-true (http-kit/http3::%http3-server-listener-closed-p listener))
             (ensure-equal 1 close-count)))))))

  (deftest http3-listener-coverage-stream-flow-control-and-cancellation
    (let* ((server (%http3-listener-test-unit-server))
           (connection (%http3-listener-test-unit-connection))
           (stream (cl-quic-kit:make-stream 1))
           (offset 0)
           (pending-write-p nil)
           (advance-write-p t)
           (reset-event-mode :event)
           (stop-event-mode nil)
           (now-values '(1.0d0 2.0d0))
           (release-offset-on-wait-p nil)
           (wait-count 0)
           (cancel-count 0)
           (close-stream-count 0)
           (expected-error-code #x10c))
      (%http3-listener-test-call-with-function-stubs
       (list
        (cons 'cl-quic-kit:stream-send-offset
              (lambda (target)
                (ensure-equal stream target)
                offset))
        (cons 'cl-quic-kit::%quic-connection-stream-engine
              (lambda (target)
                (declare (ignore target))
                :test-engine))
        (cons 'cl-quic-kit:client-stream-write-pending-p
              (lambda (engine target)
                (ensure-equal :test-engine engine)
                (ensure-equal stream target)
                pending-write-p))
        (cons 'cl-quic-kit:quic-connection-write-stream
              (lambda (quic-connection target octets &key fin-p)
                (declare (ignore quic-connection fin-p))
                (ensure-equal stream target)
                (when advance-write-p
                  (incf offset (length octets))
                  ;; SBCL may inline the stream accessor in the listener.
                  (incf (cl-quic-kit:stream-send-offset target)
                        (length octets)))))
        (cons 'cl-quic-kit:quic-connection-flush (lambda (quic-connection)
                                                  (declare (ignore quic-connection))
                                                  nil))
        (cons 'http-kit/http3::%http3-server-now
              (lambda ()
                (or (pop now-values) 2.0d0)))
        (cons 'http-kit::%http-server-wait-for-listener-wakeup
              (lambda (target deadline)
                (ensure-equal server target)
                (ensure-true (numberp deadline))
                (incf wait-count)
                ;; Keep this loop test independent of thread scheduling.
                (when release-offset-on-wait-p
                  (setf offset 2
                        (cl-quic-kit:stream-send-offset stream) 2
                        pending-write-p nil))))
        (cons 'cl-quic-kit:stream-reset-send
              (lambda (target error-code)
                (declare (ignore target))
                (ensure-equal expected-error-code error-code)
                (case reset-event-mode
                  (:empty nil)
                  (:invalid :invalid)
                  (otherwise (list :type :reset-stream)))))
        (cons 'cl-quic-kit:stream-stop-sending
              (lambda (target error-code)
                (declare (ignore target))
                (ensure-equal expected-error-code error-code)
                (case stop-event-mode
                  ((nil :empty) nil)
                  (:invalid :invalid)
                  (otherwise (list :type :stop-sending)))))
        (cons 'cl-quic-kit:quic-connection-open-stream
              (lambda (quic-connection &key type)
                (declare (ignore quic-connection type))
                stream))
        (cons 'http-kit/http3::%http3-server-queue-stream-event
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf cancel-count)))
        (cons 'cl-quic-kit:quic-connection-close-stream
              (lambda (target target-stream)
                (ensure-equal
                 (http-kit/http3::%http3-server-connection-quic-connection
                  connection)
                 target)
                (ensure-equal stream target-stream)
                (incf close-stream-count)))
        )
       (lambda ()
         (ensure-true
          (http-kit/http3::%http3-server-stream-write
           server connection stream #() :fin-p t))
         (setf offset 0
               (cl-quic-kit:stream-send-offset stream) 0
               now-values '(1.0d0 1.0d0))
         (ensure-true
          (http-kit/http3::%http3-server-stream-write
           server connection stream #(1 2) :fin-p t))
         (ensure-equal 2 offset)
         (setf offset 0
               pending-write-p t
               release-offset-on-wait-p t
               wait-count 0
               now-values '(1.0d0 1.0d0)
               (cl-quic-kit:stream-send-offset stream) 0)
         (ensure-true
          (http-kit/http3::%http3-server-stream-write
           server connection stream #(1 2) :fin-p t))
         (ensure-equal 1 wait-count)
         (ensure-false pending-write-p)
         (setf offset 0
               (cl-quic-kit:stream-send-offset stream) 0
               advance-write-p nil release-offset-on-wait-p t
               now-values '(1.0d0 1.0d0))
         (ensure-true
          (http-kit/http3::%http3-server-stream-write
           server connection stream #(1 2) :fin-p t))
         (ensure-equal 2 offset)
         (ensure-equal 2 wait-count)
         (ensure-true
          (handler-case
              (progn
                (setf offset 0
                      (cl-quic-kit:stream-send-offset stream) 0
                      release-offset-on-wait-p nil
                      now-values '(1.0d0 2.0d0))
                (http-kit/http3::%http3-server-stream-write
                 server connection stream #(1 2) :deadline 1.5d0)
                nil)
            (http-timeout () t)))
         (setf offset 0
               (cl-quic-kit:stream-send-offset stream) 0)
         (http-kit/http3::%http3-server-stream-cancel-locked
          connection stream #x10c)
         (ensure-equal 1 cancel-count)
         (setf stop-event-mode t)
         (http-kit/http3::%http3-server-stream-cancel-locked
          connection stream #x10c)
         (ensure-equal 3 cancel-count)
         (setf reset-event-mode :empty
               stop-event-mode :empty)
         (http-kit/http3::%http3-server-stream-cancel-locked
          connection stream #x10c)
         (ensure-equal 3 cancel-count)
         (setf reset-event-mode :invalid
               stop-event-mode :invalid)
         (http-kit/http3::%http3-server-stream-cancel-locked
          connection stream #x10c)
         (ensure-equal 3 cancel-count)
         (setf reset-event-mode :event
               stop-event-mode :event
               offset 0
               (cl-quic-kit:stream-send-offset stream) 0
               advance-write-p t)
         (let* ((client
                  (http-kit/http3::%http3-server-make-client
                   server connection))
                (cancel (http-kit/http3::http3-client-cancel-stream client)))
           (funcall cancel stream)
           (ensure-equal 5 cancel-count)
           (setf expected-error-code #x123)
           (funcall cancel stream :error-code expected-error-code)
           (ensure-equal 7 cancel-count)
           (ensure-equal 6 close-stream-count))))))

  (deftest http3-listener-coverage-connection-cleanup-and-registration
    (let* ((server (%http3-listener-test-unit-server))
           (listener
             (http-kit/http3::%make-http3-server-listener
              :server server :quic-server :quic-server :socket :udp-socket))
           (quic-connection :quic-connection)
           (connection
             (%http3-listener-test-unit-connection
              :quic-connection quic-connection))
           (close-state :established)
           (close-error-count 0)
           (report-count 0)
           (release-count 0)
           (flush-count 0))
      (setf (gethash quic-connection
                     (http-kit/http3::%http3-server-listener-connections listener))
            connection)
      (%http3-listener-test-call-with-function-stubs
       (list
        (cons 'http-kit::%http-server-connection-snapshot
              (lambda (target)
                (ensure-equal server target)
                (list
                 (http-kit/http3::%http3-server-connection-server-connection
                  connection))))
        (cons 'cl-quic-kit:quic-connection-close
              (lambda (target &key error-code reason)
                (declare (ignore error-code))
                (if (equal reason "HTTP/3 server shutdown")
                    (error "QUIC close failed")
                    (progn
                      (ensure-equal quic-connection target)
                      (incf close-error-count)))))
        (cons 'cl-quic-kit:connection-state
              (lambda (target)
                (declare (ignore target))
                close-state))
        (cons 'cl-quic-kit:quic-connection-flush
              (lambda (target)
                (ensure-equal quic-connection target)
                (incf flush-count)))
        (cons 'http-kit::%http-server-report-error
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf report-count)))
        (cons 'http-kit::%http-server-release-connection
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf release-count)))
        (cons 'http-kit::%http-server-emit-event
              (lambda (&rest arguments)
                (declare (ignore arguments))
                nil))
        (cons 'http-kit::%http-server-broadcast
              (lambda (&rest arguments)
                (declare (ignore arguments))
                nil)))
       (lambda ()
         (http-kit/http3::%http3-server-force-close-connections server)
         (ensure-equal 1 report-count)
         (ensure-equal 1 release-count)
         (setf release-count 0)
         (http-kit/http3::%http3-server-close-connection server connection)
         (ensure-equal 1 close-error-count)
         (ensure-equal 1 flush-count)
         (setf close-state :closed)
         (http-kit/http3::%http3-server-close-connection server connection)
         (ensure-equal 1 close-error-count)
         (ensure-equal 1 flush-count)
         (setf (http-kit::http-server-state server) :stopping)
         (multiple-value-bind (registered rejection)
             (http-kit/http3::%http3-server-register-connection
              server listener :rejected-while-stopping)
           (ensure-false registered)
           (ensure-equal :server-stopping rejection))
         (setf (http-kit::http-server-state server) :running
               (http-kit::http-server-max-connections server) 1
               (http-kit::http-server-connection-count server) 1)
         (multiple-value-bind (registered rejection)
             (http-kit/http3::%http3-server-register-connection
              server listener :rejected-at-capacity)
           (ensure-false registered)
           (ensure-equal :max-connections rejection))
         (setf (http-kit::http-server-connection-count server) 1)
         (http-kit/http3::%http3-server-release-connection
          server listener connection)
         (http-kit/http3::%http3-server-release-connection
          server listener connection)
         (ensure-equal 1 release-count)
         (let ((condition
                 (handler-case
                     (progn
                       (http-kit/http3::%http3-server-release-body-bytes server "bad")
                       nil)
                   (http-server-error (condition) condition))))
           (ensure-equal :body-buffer-accounting
                         (http-server-error-reason-code condition)))
         (let ((condition
                 (handler-case
                     (progn
                       (http-kit/http3::%http3-server-release-body-bytes server 1)
                       nil)
                   (http-server-error (condition) condition))))
           (ensure-equal :body-buffer-accounting
                         (http-server-error-reason-code condition)))))))

  (deftest http3-listener-reports-typed-connection-close
    (let* ((callback-condition nil)
           (callback-context nil)
           (condition
             (make-condition
              'http-kit/http3:http3-server-connection-closed
              :message "The HTTP/3 connection closed before peer SETTINGS arrived."
              :operation :http3-server
              :reason-code :connection-error
              :phase :session
              :close-kind :remote
              :close-error-code #x200
              :close-reason
              (http-kit/http3::%http3-server-sanitize-close-reason
               (format nil "peer~%close reason"))))
           (server
             (%http3-listener-test-unit-server
              :on-error
              (lambda (reported-condition context)
                (setf callback-condition reported-condition
                      callback-context context)))))
      (http-kit/http3::%http3-server-report-error
       server nil condition :session)
      (ensure-true (eq condition callback-condition))
      (ensure-true
       (typep callback-condition 'http-kit/http3:http3-server-connection-closed))
      (ensure-true (typep callback-condition 'http-kit:http-server-error))
      (ensure-equal :connection-error
                    (http-kit:http-server-error-reason-code callback-condition))
      (ensure-equal :session
                    (http-kit:http-server-error-phase callback-condition))
      (ensure-equal :remote
                    (http-kit/http3:http3-server-connection-close-kind
                     callback-condition))
      (ensure-equal #x200
                    (http-kit/http3:http3-server-connection-close-error-code
                     callback-condition))
      (ensure-equal "peer close reason"
                    (http-kit/http3:http3-server-connection-close-reason
                     callback-condition))
      (ensure-equal
       '(:phase :session :protocol :http3 :peer-address nil :peer-port nil)
       callback-context)
      (ensure-equal
       256
       (length
        (http-kit/http3::%http3-server-sanitize-close-reason
         (make-string 300 :initial-element #\x))))))

  (deftest http3-listener-coverage-poll-drain-idle-and-close
    (let* ((server (%http3-listener-test-unit-server :idle-timeout 1))
           (listener
             (http-kit/http3::%make-http3-server-listener
              :server server :quic-server :quic-server :socket :udp-socket))
           (quic-connection
             (cl-quic-kit::%make-quic-connection
              :state :established
              :stream-engine
              (cl-quic-kit::%make-quic-client
               :streams (make-hash-table :test #'eql))))
           (connection
             (%http3-listener-test-unit-connection
              :quic-connection quic-connection))
           (stream-queue nil)
           (poll-closed-p nil)
           (connection-closed-p nil)
           (now 10)
           (cancel-count 0)
           (close-reasons nil)
           (release-count 0))
      (setf (gethash quic-connection
                     (http-kit/http3::%http3-server-listener-connections listener))
            connection)
      (%http3-listener-test-call-with-function-stubs
       (list
        (cons 'cl-quic-kit:quic-server-poll
              (lambda (target at)
                (declare (ignore target at))
                poll-closed-p))
        (cons 'cl-quic-kit:quic-server-accept-connection
              (lambda (&rest arguments)
                (declare (ignore arguments))
                nil))
        (cons 'cl-quic-kit:quic-connection-accept-stream
              (lambda (target &key type)
                (declare (ignore target))
                (when (eq type :bidirectional)
                  (pop stream-queue))))
        (cons 'cl-quic-kit:stream-id
              (lambda (target)
                (declare (ignore target))
                0))
        (cons 'http-kit/http3::%http3-server-now (lambda () now))
        (cons 'http-kit/http3::%http3-server-connection-closed-p
              (lambda (&rest arguments)
                (declare (ignore arguments))
                connection-closed-p))
        (cons 'http-kit/http3::%http3-server-stream-cancel-locked
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf cancel-count)))
        (cons 'http-kit/http3::%http3-server-start-request-worker
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (error "draining listener must not start a request worker")))
        (cons 'cl-quic-kit:quic-connection-close
              (lambda (target &key error-code reason)
                (declare (ignore target error-code))
                (push reason close-reasons)))
        (cons 'cl-quic-kit:quic-connection-flush
              (lambda (&rest arguments)
                (declare (ignore arguments))
                nil))
        (cons 'http-kit::%http-server-broadcast
              (lambda (&rest arguments)
                (declare (ignore arguments))
                nil))
        (cons 'http-kit::%http-server-emit-event
              (lambda (&rest arguments)
                (declare (ignore arguments))
                nil))
        (cons 'http-kit/http3::%http3-server-release-connection
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf release-count))))
       (lambda ()
         (setf (http-kit/http3::%http3-server-listener-draining-p listener) t
               stream-queue (list (cl-quic-kit:make-stream 0))
               now 10)
         (http-kit/http3::%http3-server-poll-listener server listener)
         (ensure-equal 1 cancel-count)
         (ensure-true (member "HTTP/3 request drain complete" close-reasons
                              :test #'equal))
         (setf (http-kit/http3::%http3-server-listener-draining-p listener) nil
               stream-queue nil
               close-reasons nil
               (http-kit/http3::%http3-server-connection-last-activity connection) 0
               now 10)
         (http-kit/http3::%http3-server-poll-listener server listener)
         (ensure-true (member "HTTP/3 idle timeout" close-reasons :test #'equal))
         (setf poll-closed-p t
               connection-closed-p nil
               release-count 0)
         (ensure-true (http-kit/http3::%http3-server-poll-listener server listener))
         (ensure-true (http-kit/http3::%http3-server-listener-closed-p listener))
         (ensure-equal 1 release-count)))))

  (deftest http3-listener-poll-isolates-peer-flow-control-error
    (let* ((server (%http3-listener-test-unit-server))
           (listener
             (http-kit/http3::%make-http3-server-listener
              :server server :quic-server :quic-server :socket :udp-socket))
           (stream (cl-quic-kit:make-stream 0))
           (offender-engine
             (cl-quic-kit::%make-quic-client
              :streams (make-hash-table :test #'eql)))
           (other-engine
             (cl-quic-kit::%make-quic-client
              :streams (make-hash-table :test #'eql)))
           (offender-quic
             (cl-quic-kit::%make-quic-connection
              :state :established :stream-engine offender-engine))
           (other-quic
             (cl-quic-kit::%make-quic-connection
              :state :established :stream-engine other-engine))
           (offender (%http3-listener-test-unit-connection
                      :quic-connection offender-quic))
           (other (%http3-listener-test-unit-connection
                   :quic-connection other-quic))
           (poll-mode :peer-error)
           (close-count 0)
           (flush-count 0)
           (report-count 0)
           (release-count 0))
      (setf (gethash 0 (cl-quic-kit::quic-client-streams offender-engine)) stream
            (gethash offender-quic
                     (http-kit/http3::%http3-server-listener-connections listener))
            offender
            (gethash other-quic
                     (http-kit/http3::%http3-server-listener-connections listener))
            other)
      (%http3-listener-test-call-with-function-stubs
       (list
        (cons 'cl-quic-kit:quic-server-poll
              (lambda (target now)
                (declare (ignore target now))
                (case poll-mode
                  (:peer-error
                   (cl-quic-kit:stream-stop-sending-receive stream #x124)
                   (error 'cl-quic-kit:flow-control-error))
                  (:unassociated-flow-control
                   (error 'cl-quic-kit:flow-control-error))
                  (:listener-error (error "UDP socket failed")))))
        (cons 'cl-quic-kit:quic-server-accept-connection
              (lambda (&rest arguments) (declare (ignore arguments)) nil))
        (cons 'cl-quic-kit:quic-connection-accept-stream
              (lambda (&rest arguments) (declare (ignore arguments)) nil))
        (cons 'cl-quic-kit:quic-connection-close
              (lambda (target &key error-code reason)
                (declare (ignore error-code reason))
                (incf close-count)
                (setf (cl-quic-kit::quic-connection-state target) :closing)))
        (cons 'cl-quic-kit:quic-connection-flush
              (lambda (target)
                (ensure-equal other-quic target)
                (incf flush-count)))
        (cons 'http-kit::%http-server-report-error
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf report-count)))
        (cons 'http-kit::%http-server-emit-event
              (lambda (&rest arguments) (declare (ignore arguments)) nil))
        (cons 'http-kit::%http-server-broadcast
              (lambda (&rest arguments) (declare (ignore arguments)) nil))
        (cons 'http-kit/http3::%http3-server-release-connection
              (lambda (actual-server actual-listener connection)
                (ensure-equal server actual-server)
                (remhash
                 (http-kit/http3::%http3-server-connection-quic-connection
                  connection)
                 (http-kit/http3::%http3-server-listener-connections
                  actual-listener))
                (incf release-count)))
        )
       (lambda ()
         (http-kit/http3::%http3-server-poll-listener server listener)
         (ensure-equal 1 close-count)
         (ensure-equal 1 report-count)
         (ensure-equal 1 release-count)
         (ensure-equal 1 flush-count)
         (ensure-equal :running (http-kit::http-server-state server))
         (ensure-true
          (gethash other-quic
                   (http-kit/http3::%http3-server-listener-connections listener)))
         (setf poll-mode :unassociated-flow-control)
         (signals cl-quic-kit:flow-control-error
           (http-kit/http3::%http3-server-poll-listener server listener))
         (setf poll-mode :listener-error)
         (signals simple-error
           (http-kit/http3::%http3-server-poll-listener server listener))))))

  (deftest http3-listener-covers-peer-stop-snapshot-branches
    (let* ((server (%http3-listener-test-unit-server))
           (listener
             (http-kit/http3::%make-http3-server-listener
              :server server :quic-server :quic-server :socket :udp-socket))
           (stream (cl-quic-kit:make-stream 0))
           (stream-engine
             (cl-quic-kit::%make-quic-client
              :streams (make-hash-table :test #'eql)))
           (missing-table-engine
             (cl-quic-kit::%make-quic-client :streams nil))
           (quic-connection
             (cl-quic-kit::%make-quic-connection
              :state :established :stream-engine stream-engine))
           (missing-table-quic-connection
             (cl-quic-kit::%make-quic-connection
              :state :established :stream-engine missing-table-engine))
           (connection
             (%http3-listener-test-unit-connection
              :quic-connection quic-connection))
           (missing-table-connection
             (%http3-listener-test-unit-connection
              :quic-connection missing-table-quic-connection)))
      (setf (gethash 0 (cl-quic-kit::quic-client-streams stream-engine)) stream
            (gethash quic-connection
                     (http-kit/http3::%http3-server-listener-connections listener))
            connection
            (gethash missing-table-quic-connection
                     (http-kit/http3::%http3-server-listener-connections listener))
            missing-table-connection)
      (let ((snapshot
              (http-kit/http3::%http3-server-stream-stop-snapshot listener)))
        (ensure-true (hash-table-p snapshot))
        (ensure-equal nil
                      (http-kit/http3::%http3-server-newly-stopped-connections
                       listener snapshot))
        (ensure-equal nil
                      (http-kit/http3::%http3-server-newly-stopped-connections
                       listener (make-hash-table :test #'eq))))
      (cl-quic-kit:stream-stop-sending-receive stream #x124)
      (ensure-equal
       (list connection)
       (http-kit/http3::%http3-server-newly-stopped-connections
        listener (make-hash-table :test #'eq)))
      (let ((snapshot
              (http-kit/http3::%http3-server-stream-stop-snapshot listener)))
        (ensure-equal nil
                      (http-kit/http3::%http3-server-newly-stopped-connections
                       listener snapshot)))))

  (deftest http3-listener-poll-isolates-per-connection-flush-error
    (let* ((server (%http3-listener-test-unit-server))
           (listener
             (http-kit/http3::%make-http3-server-listener
              :server server :quic-server :quic-server :socket :udp-socket))
           (engine
             (cl-quic-kit::%make-quic-client
              :streams (make-hash-table :test #'eql)))
           (quic-connection
             (cl-quic-kit::%make-quic-connection
              :state :established :stream-engine engine))
           (connection
             (%http3-listener-test-unit-connection
              :quic-connection quic-connection))
           (close-count 0)
           (report-count 0)
           (release-count 0))
      (setf (gethash quic-connection
                     (http-kit/http3::%http3-server-listener-connections listener))
            connection)
      (%http3-listener-test-call-with-function-stubs
       (list
        (cons 'cl-quic-kit:quic-server-poll
              (lambda (&rest arguments)
                (declare (ignore arguments))
                nil))
        (cons 'cl-quic-kit:quic-server-accept-connection
              (lambda (&rest arguments)
                (declare (ignore arguments))
                nil))
        (cons 'cl-quic-kit:quic-connection-accept-stream
              (lambda (&rest arguments)
                (declare (ignore arguments))
                nil))
        (cons 'cl-quic-kit:quic-connection-flush
              (lambda (target)
                (ensure-equal quic-connection target)
                (error 'cl-quic-kit:flow-control-error)))
        (cons 'cl-quic-kit:quic-connection-close
              (lambda (target &key error-code reason)
                (declare (ignore error-code reason))
                (ensure-equal quic-connection target)
                (incf close-count)
                (error 'cl-quic-kit:flow-control-error)))
        (cons 'http-kit::%http-server-report-error
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf report-count)))
        (cons 'http-kit::%http-server-emit-event
              (lambda (&rest arguments)
                (declare (ignore arguments))
                nil))
        (cons 'http-kit::%http-server-broadcast
              (lambda (&rest arguments)
                (declare (ignore arguments))
                nil))
        (cons 'http-kit/http3::%http3-server-release-connection
              (lambda (actual-server actual-listener actual-connection)
                (ensure-equal server actual-server)
                (remhash quic-connection
                         (http-kit/http3::%http3-server-listener-connections
                          actual-listener))
                (ensure-equal connection actual-connection)
                (incf release-count))))
       (lambda ()
         (http-kit/http3::%http3-server-poll-listener server listener)
         (ensure-equal :running (http-kit::http-server-state server))
         (ensure-equal 1 close-count)
         (ensure-equal 1 report-count)
         (ensure-equal 1 release-count)
         (ensure-equal nil
                       (gethash
                        quic-connection
                        (http-kit/http3::%http3-server-listener-connections
                         listener)))
         (setf (cl-quic-kit::quic-connection-state quic-connection) :closed)
         (http-kit/http3::%http3-server-isolate-peer-error
          server listener connection (make-condition 'simple-error
                                                      :format-control "closed peer"))
         (ensure-equal 1 close-count)
         (ensure-equal 2 report-count)
         (ensure-equal 2 release-count)))))

  (deftest http3-listener-coverage-control-streams-and-settings
    (let* ((server (%http3-listener-test-unit-server))
           (connection
             (let ((quic-connection
                     (cl-quic-kit:make-quic-connection :role :server)))
               (setf (cl-quic-kit:quic-connection-closed-reason quic-connection)
                     "peer close reason")
               (%http3-listener-test-unit-connection
                :quic-connection quic-connection)))
           (listener
             (http-kit/http3::%make-http3-server-listener
              :server server :quic-server :quic-server :socket :udp-socket))
           (kind :control)
           (error-mode nil)
           (closed-p nil)
           (running-p t)
           (now 1.0d0)
           (encoder-reads 0)
           (decoder-reads 0)
           (discard-reads 0)
           (dispatch-count 0)
           (close-count 0)
           (error-count 0)
           (cancel-count 0))
      (%http3-listener-test-call-with-function-stubs
       (list
        (cons 'http-kit/http3:serve-http3-control-stream
              (lambda (stream &key on-settings on-goaway &allow-other-keys)
                (declare (ignore stream))
                (funcall on-settings nil nil)
                (funcall on-goaway 12 nil)))
        (cons 'http-kit/http3::%read-http3-qpack-encoder-stream-serialized
              (lambda (client &key deadline)
                (declare (ignore client deadline))
                (incf encoder-reads)
                (values nil (>= encoder-reads 2))))
        (cons 'http-kit/http3::%read-http3-qpack-decoder-stream-serialized
              (lambda (client &key deadline)
                (declare (ignore client deadline))
                (incf decoder-reads)
                (values nil (>= decoder-reads 2))))
        (cons 'http-kit/http3:accept-http3-peer-unidirectional-stream
              (lambda (client stream &rest arguments)
                (declare (ignore client stream arguments))
                (if error-mode
                    (error error-mode)
                    kind)))
        (cons 'http-kit/http3::%http3-server-stream-read
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf discard-reads)
                (if (= discard-reads 1)
                    (values #(1) nil)
                    (values nil t))))
        (cons 'http-kit/http3::%http3-server-connection-closed-p
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (if closed-p '(:closed) nil)))
        (cons 'cl-quic-kit:connection-close-frame
              (lambda (target)
                (ensure-equal
                 (http-kit/http3::%http3-server-connection-quic-connection
                  connection)
                 target)
                :frame))
        (cons 'cl-quic-kit:connection-close-kind
              (lambda (target)
                (ensure-equal
                 (http-kit/http3::%http3-server-connection-quic-connection
                  connection)
                 target)
                :remote))
        (cons 'cl-quic-kit:frame-field
              (lambda (frame field)
                (ensure-equal :frame frame)
                (ensure-equal :error-code field)
                #x200))
        (cons 'cl-quic-kit:quic-connection-closed-reason
              (lambda (target)
                (ensure-equal
                 (http-kit/http3::%http3-server-connection-quic-connection
                  connection)
                 target)
                "peer close reason"))
        (cons 'http-kit::%http-server-running-p
              (lambda (&rest arguments)
                (declare (ignore arguments))
                running-p))
        (cons 'http-kit::%http-server-broadcast
              (lambda (&rest arguments)
                (declare (ignore arguments))
                nil))
        (cons 'http-kit/http3::%http3-server-now (lambda () now))
        (cons 'http-kit/http3::%http3-server-report-error
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf error-count)))
        (cons 'http-kit/http3::%http3-server-stream-cancel
              (lambda (actual-server actual-connection stream error-code)
                (ensure-equal server actual-server)
                (ensure-equal connection actual-connection)
                (ensure-equal :stream stream)
                (ensure-equal #x103 error-code)
                (incf cancel-count)))
        (cons 'http-kit/http3::%http3-server-close-connection
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf close-count))))
       (lambda ()
         (setf (http-kit/http3::%http3-server-connection-http3-client connection)
               (http-kit/http3::%make-http3-client))
         (http-kit/http3::%http3-server-control-worker
          server listener connection :control-stream)
         (ensure-equal 12
                       (http-kit/http3::%http3-server-connection-peer-goaway-id
                        connection))
         (ensure-true
          (http-kit/http3::%http3-server-connection-peer-settings-received-p
           connection))
         (http-kit/http3::%http3-server-qpack-encoder-worker connection :stream)
         (http-kit/http3::%http3-server-qpack-decoder-worker connection :stream)
         (ensure-equal 2 encoder-reads)
         (ensure-equal 2 decoder-reads)
         (http-kit/http3::%http3-server-discard-stream server connection :stream)
         (ensure-equal 2 discard-reads)
         (dolist (stream-kind '(:control :qpack-encoder :qpack-decoder :unknown :push))
           (setf kind stream-kind)
           (http-kit/http3::%http3-server-unidirectional-worker
            server listener connection :stream)
           (incf dispatch-count))
         (ensure-equal 5 dispatch-count)
         (ensure-equal 1 close-count)
         (setf error-mode (simple-condition-format-control
                           (make-condition 'simple-error :format-control "bad stream")))
         (http-kit/http3::%http3-server-unidirectional-worker
            server listener connection :stream)
         (ensure-equal 1 error-count)
         (ensure-equal 2 close-count)
         (setf closed-p t)
         (http-kit/http3::%http3-server-unidirectional-worker
          server listener connection :stream)
         (ensure-equal 1 error-count)
         (setf closed-p nil running-p nil)
         (http-kit/http3::%http3-server-unidirectional-worker
          server listener connection :stream)
         (ensure-equal 1 error-count)
         (setf closed-p nil
               running-p t
               error-mode
                 (make-condition 'http-kit:http-timeout
                                 :message "unidirectional prefix timeout"))
         (http-kit/http3::%http3-server-unidirectional-worker
          server listener connection :stream)
         (ensure-equal 1 cancel-count)
         (ensure-equal 2 error-count)
         (ensure-equal 3 close-count)
         (setf running-p nil)
         (setf error-mode
               (http-kit::%http-server-callback-condition :on-event :control))
         (ensure-true
          (handler-case
              (progn
                (http-kit/http3::%http3-server-unidirectional-worker
                 server listener connection :stream)
                nil)
            (http-kit::http-server-callback-error () t)))
         (ensure-true
          (http-kit/http3::%http3-server-await-peer-settings
           server connection nil))
         (setf (http-kit/http3::%http3-server-connection-peer-settings-received-p
                connection)
               nil
               closed-p t
               now 2.0d0)
         (let ((condition
                 (handler-case
                     (progn
                       (http-kit/http3::%http3-server-await-peer-settings
                        server connection nil)
                       nil)
                   (http-kit/http3:http3-server-connection-closed
                     (condition)
                     condition))))
           (ensure-true
            (typep condition
                   'http-kit/http3:http3-server-connection-closed))
           (ensure-equal
            :remote
            (http-kit/http3:http3-server-connection-close-kind condition))
           (ensure-equal
            #x200
            (http-kit/http3:http3-server-connection-close-error-code
             condition))
           (ensure-equal
            "peer close reason"
            (http-kit/http3:http3-server-connection-close-reason condition)))
         (setf closed-p nil running-p nil)
         (ensure-true
          (handler-case
              (progn
                (http-kit/http3::%http3-server-await-peer-settings
                 server connection nil)
                nil)
            (http-kit/http3:http3-server-stopped () t)))
         (setf running-p t)
         (ensure-true
          (handler-case
              (progn
                (http-kit/http3::%http3-server-await-peer-settings
                 server connection 1.0d0)
                nil)
            (http-timeout () t)))
         (setf now 0.0d0)
         (%http3-listener-test-call-with-function-stubs
          (list
           (cons 'http-kit::%http-server-wait-for-listener-wakeup
                 (lambda (target deadline)
                   (declare (ignore target deadline))
                   (setf (http-kit/http3::%http3-server-connection-peer-settings-received-p
                          connection)
                         t))))
          (lambda ()
            (ensure-true
             (http-kit/http3::%http3-server-await-peer-settings
              server connection nil))))))))

  (deftest http3-listener-coverage-request-worker-errors
    (let* ((server (%http3-listener-test-unit-server))
           (connection (%http3-listener-test-unit-connection
                        :active-request-streams 1))
           (mode :pre-serving)
           (running-p t)
           (closed-p nil)
           (cancel-count 0)
           (report-count 0)
           (close-count 0)
           (wait-count 0)
           (callback-condition
             (http-kit::%http-server-callback-condition :handler :request)))
      (%http3-listener-test-call-with-function-stubs
       (list
        (cons 'http-kit/http3::%http3-server-stream-open
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (gensym "HTTP3-TEST-STREAM-")))
        (cons 'http-kit/http3::%http3-server-stream-write
              (lambda (&rest arguments)
                (declare (ignore arguments))
                t))
        (cons 'http-kit/http3::%http3-server-await-peer-settings
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (when (eq mode :pre-serving)
                  (error "peer SETTINGS wait failed"))))
        (cons 'http-kit/http3:serve-http3-request-stream
              (lambda (&rest arguments)
                (case mode
                  (:callback-error
                   (error callback-condition))
                  (:reported-error
                   (funcall (getf arguments :on-error)
                            (make-condition 'simple-error
                                             :format-control "body failed"
                                             :format-arguments nil)
                            nil)
                   (error "handler response failed"))
                  (otherwise (error "request stream failed")))))
        (cons 'http-kit/http3::%http3-server-stream-cancel-locked
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf cancel-count)))
        (cons 'http-kit/http3::%http3-server-report-error
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf report-count)))
        (cons 'http-kit::%http-server-running-p
              (lambda (&rest arguments)
                (declare (ignore arguments))
                running-p))
        (cons 'http-kit/http3::%http3-server-connection-closed-p
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (if closed-p '(:closed) nil)))
        (cons 'cl-quic-kit:quic-connection-close
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf close-count)
                (setf closed-p t)))
        (cons 'http-kit::%http-server-broadcast
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf wait-count))))
       (lambda ()
         (http-kit/http3::%http3-server-make-client server connection)
         (http-kit/http3::%http3-server-request-worker
          server connection :stream 0 nil)
         (ensure-equal 1 cancel-count)
         (ensure-equal 1 report-count)
         (ensure-equal 0
                       (http-kit/http3::%http3-server-connection-active-request-streams
                        connection))
         (setf mode :generic-error
               (http-kit/http3::%http3-server-connection-active-request-streams
                connection)
               1)
         (http-kit/http3::%http3-server-request-worker
          server connection :stream 0 nil)
         (ensure-equal 2 report-count)
         (setf mode :reported-error
               (http-kit/http3::%http3-server-connection-active-request-streams
                connection)
               1)
         (http-kit/http3::%http3-server-request-worker
          server connection :stream 0 nil)
         (ensure-equal 3 report-count)
         (setf mode :callback-error
               (http-kit/http3::%http3-server-connection-active-request-streams
                connection)
               1)
         (http-kit/http3::%http3-server-request-worker
          server connection :stream 0 nil)
         (ensure-equal :failed (http-kit::http-server-state server))
         (setf mode :generic-error
               running-p nil
               (http-kit::http-server-state server) :stopping
               (http-kit/http3::%http3-server-connection-active-request-streams
                connection)
               1)
         (http-kit/http3::%http3-server-request-worker
          server connection :stream 0 nil)
         (ensure-equal 3 report-count)
         (ensure-equal 1 close-count)
         (ensure-true (plusp wait-count))))))

  (deftest http3-listener-coverage-worker-start-and-request-start-errors
    (let* ((server (%http3-listener-test-unit-server))
           (connection (%http3-listener-test-unit-connection))
           (report-count 0)
           (failed-count 0)
           (coordinator-count 0)
           (closed-p nil)
           (running-p t)
           (live-supervisor-release (sb-thread:make-semaphore :count 0))
           (live-supervisor
             (sb-thread:make-thread
              (lambda () (sb-thread:wait-on-semaphore live-supervisor-release)))))
      (%http3-listener-test-call-with-function-stubs
       (list
        (cons 'http-kit::%http-server-report-error
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf report-count)))
        (cons 'http-kit::%http-server-mark-failed
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf failed-count)))
        (cons 'http-kit::%http-server-running-p
              (lambda (&rest arguments)
                (declare (ignore arguments))
                running-p))
        (cons 'http-kit/http3::%http3-server-connection-closed-p
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (if closed-p '(:closed) nil)))
        (cons 'http-kit::%http-server-ensure-shutdown-coordinator
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf coordinator-count))))
       (lambda ()
         (let ((worker
                 (http-kit/http3::%http3-server-start-worker
                  server (lambda () (error "worker failed")) "failure")))
           (sb-thread:join-thread worker))
         (ensure-equal 1 report-count)
         (setf running-p nil)
         (let ((worker
                 (http-kit/http3::%http3-server-start-worker
                  server (lambda () (error "stopped worker failed"))
                  "stopped-worker-failure")))
           (sb-thread:join-thread worker))
         (ensure-equal 1 report-count)
         (setf running-p t)
         (let ((condition
                 (handler-case
                     (%http3-listener-test-call-with-function-stubs
                      (list
                       (cons 'sb-thread:make-thread
                             (lambda (&rest arguments)
                               (declare (ignore arguments))
                               (error "thread creation failed"))))
                      (lambda ()
                        (http-kit/http3::%http3-server-start-worker
                         server (lambda () nil) "thread-start-failure")))
                   (http-server-error (condition) condition))))
           (ensure-equal :worker-start-failed
                         (http-server-error-reason-code condition)))
         (let ((worker
                 (http-kit/http3::%http3-server-start-worker
                  server
                  (lambda ()
                    (error
                     (http-kit::%http-server-callback-condition
                      :on-event :stream)))
                  "callback-failure")))
           (sb-thread:join-thread worker))
         (ensure-equal 1 failed-count)
         (let ((worker
                 (http-kit/http3::%http3-server-start-worker
                  server (lambda () (error "open stream failed"))
                  "open-stream-failure" connection)))
           (sb-thread:join-thread worker))
         (ensure-equal 2 report-count)
         (setf closed-p t)
         (let ((worker
                 (http-kit/http3::%http3-server-start-worker
                  server (lambda () (error "closed stream failed"))
                  "closed-stream-failure" connection)))
           (sb-thread:join-thread worker)
           (setf closed-p nil))
         (ensure-equal 2 report-count)
         (setf (http-kit::http-server-state server) :stopping
               (http-kit::http-server-shutdown-p server) t
               (http-kit::http-server-supervisor server) nil)
         (let ((worker
                 (http-kit/http3::%http3-server-start-worker
                  server (lambda () nil) "shutdown-no-supervisor")))
           (sb-thread:join-thread worker))
         (ensure-equal 1 coordinator-count)
         (setf (http-kit::http-server-supervisor server) live-supervisor)
         (let ((worker
                 (http-kit/http3::%http3-server-start-worker
                  server (lambda () nil) "shutdown-live-supervisor")))
           (sb-thread:join-thread worker))
         (ensure-equal 1 coordinator-count)
         (sb-thread:signal-semaphore live-supervisor-release)
         (sb-thread:join-thread live-supervisor)
         (setf (http-kit::http-server-supervisor server)
               live-supervisor)
         (let ((worker
                 (http-kit/http3::%http3-server-start-worker
                  server (lambda () nil) "shutdown-dead-supervisor")))
           (sb-thread:join-thread worker))
         (ensure-equal 2 coordinator-count)
         (setf (http-kit/http3::%http3-server-connection-active-request-streams
                connection)
               1)
         (%http3-listener-test-call-with-function-stubs
          (list
           (cons 'http-kit/http3::%http3-server-start-worker
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (error 'http-server-error
                          :reason-code :worker-start-failed
                          :phase :session))))
          (lambda ()
            (ensure-true
             (handler-case
                 (progn
                   (http-kit/http3::%http3-server-start-request-worker
                    server connection :stream 0 nil)
                   nil)
               (http-server-error () t)))
            (ensure-equal 0
                          (http-kit/http3::%http3-server-connection-active-request-streams
                           connection))))))
      (when (sb-thread:thread-alive-p live-supervisor)
        (sb-thread:signal-semaphore live-supervisor-release)
        (sb-thread:join-thread live-supervisor))))

  (deftest http3-listener-coverage-accept-alpn-and-setup-failures
    (let* ((server (%http3-listener-test-unit-server))
           (listener
             (http-kit/http3::%make-http3-server-listener
              :server server :quic-server :quic-server :socket :udp-socket))
           (reasons nil)
           (setup-error-p nil)
           (report-count 0)
           (close-count 0)
           (quic-connection (cl-quic-kit:make-quic-connection :role :server)))
      (%http3-listener-test-call-with-function-stubs
       (list
        (cons 'cl-quic-kit:quic-connection-close
              (lambda (connection &key error-code reason)
                (declare (ignore connection error-code))
                (push reason reasons)))
        (cons 'http-kit/http3::%http3-server-make-client
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (when setup-error-p (error "HTTP/3 setup failed"))))
        (cons 'http-kit/http3::%http3-server-report-error
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf report-count)))
        (cons 'http-kit/http3::%http3-server-close-connection
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf close-count)))
        (cons 'http-kit::%http-server-emit-event
              (lambda (&rest arguments)
                (declare (ignore arguments))
                nil)))
       (lambda ()
         (dolist (protocol '(nil "h2"))
           (setf (cl-quic-kit:quic-connection-alpn quic-connection)
                 protocol)
           (ensure-false
            (http-kit/http3::%http3-server-accept-quic-connection
             server listener quic-connection)))
         (ensure-equal 2 (length reasons))
         (ensure-true
          (every (lambda (reason) (equal reason "HTTP/3 requires ALPN h3"))
                 reasons))
         (setf (cl-quic-kit:quic-connection-alpn quic-connection) "h3"
               (http-kit::http-server-max-connections server) 0)
         (ensure-false
          (http-kit/http3::%http3-server-accept-quic-connection
           server listener quic-connection))
         (ensure-equal "HTTP/3 listener is at capacity" (first reasons))
         (setf (http-kit::http-server-max-connections server) 8
               setup-error-p t)
         (ensure-true
          (handler-case
              (progn
                (http-kit/http3::%http3-server-accept-quic-connection
                 server listener quic-connection)
                nil)
            (error () t)))
         (ensure-equal 1 report-count)
         (ensure-equal 1 close-count)))))

  (deftest http3-listener-raw-peer-rejects-malformed-and-slow-streams
    (dolist (action '("streams" "push" "malformed-control"
                      "malformed-request" "no-settings" "slow-request"))
      (let ((handler-calls 0)
            (peer-opened (sb-thread:make-semaphore :count 0))
            (connection-opened-at nil)
            (streams-ready-p nil)
            (streams-state nil))
        (%call-with-http3-listener
         (lambda (request)
           (declare (ignore request))
           (incf handler-calls)
           (make-http-response :status 200 :body (ascii "unexpected")))
         (lambda (server material observations)
           (multiple-value-bind (output errors status)
               (if (equal action "streams")
                   (let ((peer nil)
                         (setup-start (get-internal-real-time)))
                     (unwind-protect
                          (progn
                            (setf peer
                                  (%http3-listener-launch-peer
                                   material "http3-raw" (http-server-port server)
                                   "--raw-action" action "--timeout" "3s"))
                            (let ((deadline
                                    (+ setup-start
                                       (* 12 internal-time-units-per-second))))
                              (let ((phase :fixture-ready))
                                (handler-case
                                    (sb-ext:with-timeout 12
                                      (ensure-equal
                                       "RAW STREAMS READY"
                                       (%http3-listener-peer-line peer "RAW STREAMS READY"))
                                      (setf phase :connection-opened)
                                      (unless
                                          (sb-thread:wait-on-semaphore
                                           peer-opened
                                           :timeout
                                           (/ (max 0 (- deadline (get-internal-real-time)))
                                              internal-time-units-per-second))
                                        (error
                                         "The raw HTTP/3 peer connection-opened event was not observed after ~Dms; events ~S; listener errors ~S."
                                         (floor
                                          (* 1000
                                             (/ (- (get-internal-real-time)
                                                   setup-start)
                                                internal-time-units-per-second)))
                                         (mapcar (lambda (event)
                                                   (list (getf event :type)
                                                         (getf event :phase)
                                                         (getf event :protocol)))
                                                 (%http3-listener-events observations))
                                         (%http3-listener-error-summary observations))))
                                  (sb-ext:timeout ()
                                    (error
                                     "Raw HTTP/3 peer diagnostic timed out in phase ~S after ~Dms; events ~S; listener errors ~S."
                                     phase
                                     (floor
                                      (* 1000
                                         (/ (- (get-internal-real-time)
                                               setup-start)
                                            internal-time-units-per-second)))
                                     (mapcar (lambda (event)
                                               (list (getf event :type)
                                                     (getf event :phase)
                                                     (getf event :protocol)))
                                             (%http3-listener-events observations))
                                     (%http3-listener-error-summary observations))))))
                            (let* ((common-connection
                                     (first
                                      (http-kit/http3::%http3-server-connection-snapshot
                                       server)))
                                   (listener
                                     (http-kit::http-server-listener server))
                                   (captured-connection
                                     (and common-connection
                                          (gethash
                                           (http-kit::%http-server-connection-raw-stream
                                            common-connection)
                                           (http-kit/http3::%http3-server-listener-connections
                                            listener)))))
                              (unless captured-connection
                                (format
                                 *error-output*
                                 "RAW HTTP/3 peer SETTINGS setup elapsed=~Dms result=connection-not-registered.~%"
                                 (floor
                                  (* 1000
                                     (/ (- (get-internal-real-time)
                                           setup-start)
                                        internal-time-units-per-second))))
                                (let* ((status
                                         (handler-case
                                             (%http3-listener-peer-wait peer)
                                           (sb-ext:timeout ()
                                             (%http3-listener-peer-cleanup peer)
                                             :peer-wait-timeout)))
                                       (output
                                         (with-output-to-string (peer-output)
                                           (loop for line =
                                                   (read-line
                                                    (uiop:process-info-output peer)
                                                    nil nil)
                                                 while line
                                                 do (write-line line peer-output))))
                                       (errors
                                         (with-output-to-string (error-output)
                                           (loop for line =
                                                   (read-line
                                                    (uiop:process-info-error-output peer)
                                                    nil nil)
                                                 while line
                                                 do (write-line line error-output)))))
                                  (error
                                   "The raw HTTP/3 server connection was not registered; peer exit ~S; stdout ~S; stderr ~S; listener errors ~S; events ~S."
                                   status output errors
                                   (%http3-listener-error-summary observations)
                                   (%http3-listener-events observations))))
                              (handler-case
                                  (progn
                                    (http-kit/http3::%http3-server-await-peer-settings
                                     server captured-connection
                                     (+
                                      (http-kit/http3::%http3-server-now)
                                      (/ (max 0
                                              (- (+ setup-start
                                                    (* 12 internal-time-units-per-second))
                                                 (get-internal-real-time)))
                                         internal-time-units-per-second)))
                                    (format
                                     *error-output*
                                     "RAW HTTP/3 peer SETTINGS setup elapsed=~Dms result=received.~%"
                                     (floor
                                      (* 1000
                                         (/ (- (get-internal-real-time)
                                               setup-start)
                                            internal-time-units-per-second))))
                                    (format
                                     *error-output*
                                     "RAW HTTP/3 peer SETTINGS setup after connection-opened elapsed=~Dms result=received.~%"
                                     (floor
                                      (* 1000
                                         (/ (- (get-internal-real-time)
                                               connection-opened-at)
                                            internal-time-units-per-second)))))
                                (error (condition)
                                  (format
                                   *error-output*
                                   "RAW HTTP/3 peer SETTINGS setup elapsed=~Dms result=failed.~%"
                                   (floor
                                    (* 1000
                                       (/ (- (get-internal-real-time)
                                             setup-start)
                                          internal-time-units-per-second))))
                                  (format
                                   *error-output*
                                   "RAW HTTP/3 peer SETTINGS setup after connection-opened elapsed=~Dms result=failed.~%"
                                   (floor
                                    (* 1000
                                       (/ (- (get-internal-real-time)
                                             connection-opened-at)
                                          internal-time-units-per-second))))
                                  (error
                                   "Raw HTTP/3 SETTINGS wait failed with ~S: ~A; close info ~S; listener errors ~S; events ~S."
                                   (type-of condition)
                                   condition
                                   (and
                                    (typep condition
                                           'http-kit/http3:http3-server-connection-closed)
                                    (list
                                     :kind
                                     (http-kit/http3:http3-server-connection-close-kind
                                      condition)
                                     :error-code
                                     (http-kit/http3:http3-server-connection-close-error-code
                                      condition)
                                     :reason
                                     (http-kit/http3:http3-server-connection-close-reason
                                      condition)))
                                   (%http3-listener-error-summary observations)
                                   (mapcar
                                    (lambda (event)
                                      (list (getf event :type)
                                            (getf event :phase)
                                            (getf event :protocol)))
                                    (%http3-listener-events observations)))))
                            (ensure-equal 1 (%http3-listener-connection-count server))
                            (ensure-equal
                             nil
                             (%http3-listener-error-summary observations))
                            (let* ((common-connection
                                     (first
                                      (http-kit/http3::%http3-server-connection-snapshot
                                       server)))
                                   (listener
                                     (http-kit::http-server-listener server))
                                   (connection
                                     (and common-connection
                                          (gethash
                                           (http-kit::%http-server-connection-raw-stream
                                            common-connection)
                                           (http-kit/http3::%http3-server-listener-connections
                                            listener))))
                                   (client
                                     (and connection
                                          (http-kit/http3::%http3-server-connection-http3-client
                                           connection))))
                              (setf streams-ready-p
                                    (%http3-listener-wait-until
                                     (lambda ()
                                       (setf streams-state
                                             (and client
                                                  (http-kit/http3::%h3-with-scheduler-lock
                                                      (client)
                                                    (list
                                                     :encoder
                                                     (not
                                                      (null
                                                       (http-kit/http3::http3-client-peer-qpack-encoder-stream
                                                        client)))
                                                     :decoder
                                                     (not
                                                      (null
                                                       (http-kit/http3::http3-client-peer-qpack-decoder-stream
                                                        client)))
                                                     :discard
                                                     (mapcar
                                                      #'cdr
                                                      (http-kit/http3::http3-client-discard-peer-unidirectional-streams
                                                       client))
                                                     :stream-count
                                                     (http-kit/http3::http3-client-peer-unidirectional-stream-count
                                                      client)))))
                                       (and (getf streams-state :encoder)
                                            (getf streams-state :decoder)
                                            (= 4 (getf streams-state :stream-count))))
                                     2))
                              (http-kit/http3::%http3-server-close-connection
                               server connection :error-code :application-error
                               :reason "HTTP/3 listener test complete")))
                            (let ((status (%http3-listener-peer-wait peer))
                                  (output
                                    (with-output-to-string (peer-output)
                                      (loop for line =
                                              (read-line
                                               (uiop:process-info-output peer)
                                               nil nil)
                                            while line
                                            do (write-line line peer-output))))
                                  (errors
                                    (with-output-to-string (error-output)
                                      (loop for line =
                                              (read-line
                                               (uiop:process-info-error-output peer)
                                               nil nil)
                                            while line
                                            do (write-line line error-output)))))
                              (values output errors status)))
                       (%http3-listener-peer-cleanup peer)))
                   (%http3-listener-run-peer
                    material "http3-raw" (http-server-port server)
                    "--raw-action" action "--timeout" "3s"))
             (unless (zerop status)
               (error "Raw HTTP/3 action ~A failed: ~A; ~A; listener errors ~S; events ~S"
                      action output errors
                      (%http3-listener-error-summary observations)
                      (%http3-listener-events observations)))
             (ensure-equal 0 status)
             (when (equal action "streams")
               (unless streams-ready-p
                 (format *error-output* "RAW HTTP/3 peer stream state=~S.~%"
                         streams-state))
               (ensure-true streams-ready-p))
             (cond
               ((equal action "streams")
                (ensure-true
                 (search
                  "RAW CONNECTION CLOSED action=streams remote=true code=0x10 reason=\"HTTP/3 listener test complete\""
                  output)))
               ((member action '("push" "malformed-control") :test #'equal)
                (ensure-true
                 (search "RAW CONNECTION CLOSED" output)))
               (t
                (if (equal action "malformed-request")
                    (ensure-true
                     (search
                      "RAW CONNECTION CLOSED action=malformed-request remote=true code=0x200"
                      output))
                    (ensure-true (search "RAW STREAM RESET" output)))))
             (ensure-equal 0 handler-calls)
             (when (member action '("malformed-control"
                                    "malformed-request" "no-settings"
                                    "slow-request")
                           :test #'equal)
               (ensure-true
                (%http3-listener-wait-until
                 (lambda () (%http3-listener-errors observations)) 2)))
             (when (equal action "streams")
               (ensure-true
                (%http3-listener-wait-until
                 (lambda () (= 0 (%http3-listener-connection-count server))) 2))
               (ensure-equal
                nil
                (%http3-listener-error-summary observations)))))
         :server-error-callback
         (when (equal action "streams")
           (lambda (condition phase observations)
             (%http3-listener-record-error
              observations condition
              (list :phase phase :protocol :http3 :source :server-worker))))
         :event-callback
         (lambda (event)
           (when (eq (getf event :type) :connection-opened)
             (setf connection-opened-at (get-internal-real-time))
             (sb-thread:signal-semaphore peer-opened)))
         :options '(:request-timeout 0.25 :max-header-bytes 4096))))

  )

  (deftest http3-listener-survives-peer-reset-stream-and-stop-sending
    (let ((handler-calls 0))
      (%call-with-http3-listener
       (lambda (request)
         (declare (ignore request))
         (incf handler-calls)
         (make-http-response :status 200 :body (ascii "still running")))
       (lambda (server material observations)
         (declare (ignore observations))
         (multiple-value-bind (output errors status)
             (%http3-listener-run-peer
              material "http3-raw" (http-server-port server)
              "--raw-action" "peer-cancels-streams" "--timeout" "4s")
           (unless (zerop status)
             (error "Peer stream cancellation fixture failed: ~A; ~A"
                    output errors))
           (ensure-true (search "RAW PEER RESET_STREAM STOP_SENDING" output))
           (ensure-true (search "RAW FOLLOWUP RESPONSE" output))
           (ensure-equal :running (http-kit::http-server-state server))
           (ensure-equal 1 handler-calls)))
       :options '(:request-timeout 1 :max-header-bytes 4096))))

  (deftest http3-listener-error-callback-failure-fails-server
    (let ((handler-calls 0)
          (callback-count 0)
          (callback-lock (sb-thread:make-mutex :name "HTTP/3 error callback test")))
      (%call-with-http3-listener
       (lambda (request)
         (declare (ignore request))
         (incf handler-calls)
         (make-http-response :status 200))
       (lambda (server material observations)
         (declare (ignore observations))
         (multiple-value-bind (output errors status)
             (%http3-listener-run-peer
              material "http3-raw" (http-server-port server)
              "--raw-action" "malformed-control" "--timeout" "3s"
              "--expect-graceful-drain-close")
           (unless (zerop status)
             (error "Raw HTTP/3 callback-failure peer failed: ~A; ~A"
                    output errors))
           (unless
    (search
     (concatenate
      'string
      "RAW CONNECTION CLOSED action=malformed-control "
      "remote=true code=0x0 reason=\"HTTP/3 request drain complete\"")
     output)
  (error "Unexpected raw callback-failure output: ~A" output)))
         (ensure-true
          (%http3-listener-wait-until
           (lambda ()
             (eq :failed (http-kit::http-server-state server)))))
         (ensure-equal
          1
          (sb-thread:with-mutex (callback-lock) callback-count))
         (ensure-equal 0 handler-calls))
       :error-callback
       (lambda (condition context)
         (declare (ignore condition context))
         (sb-thread:with-mutex (callback-lock)
           (incf callback-count))
         (error "HTTP/3 error callback failed"))
       :options '(:graceful-timeout 0.25))))

  (deftest http3-listener-idle-timeout-closes-an-idle-peer
    (%call-with-http3-listener
     (lambda (request)
       (declare (ignore request))
       (make-http-response :status 200 :body (ascii "idle-ok")))
     (lambda (server material observations)
       (declare (ignore observations))
       (let ((peer
               (%http3-listener-launch-peer
                material "http3-client" (http-server-port server)
                "--hold-after-response")))
         (unwind-protect
              (progn
                (ensure-true
                 (search "HTTP3 HEADERS STATUS=200"
                         (%http3-listener-peer-line peer)))
                (ensure-true (search "HTTP3 STATUS=200"
                                     (%http3-listener-peer-line peer)))
                (ensure-true
                 (%http3-listener-wait-until
                  (lambda () (= 1 (%http3-listener-connection-count server)))))
                (ensure-true
                 (%http3-listener-wait-until
                  (lambda () (= 0 (%http3-listener-connection-count server))) 3))
                (%http3-listener-peer-release peer)
                (ensure-equal 0 (%http3-listener-peer-wait peer)))
           (%http3-listener-peer-cleanup peer))))
     :options '(:idle-timeout 0.15)))

  (progn
    (deftest http3-listener-raw-peer-ignores-greasing-streams
      (let ((handler-calls 0)
            (peer-opened (sb-thread:make-semaphore :count 0)))
        (%call-with-http3-listener
         (lambda (request)
           (declare (ignore request))
           (incf handler-calls)
           (make-http-response :status 200 :body (ascii "greasing-ok")))
         (lambda (server material observations)
           (let ((peer nil))
             (unwind-protect
                  (progn
                    (setf peer
                          (%http3-listener-launch-peer
                           material "http3-raw" (http-server-port server)
                           "--raw-action" "greasing-streams" "--timeout" "3s"))
                    (ensure-equal
                     "RAW GREASING STREAMS READY"
                     (%http3-listener-peer-line
                      peer "RAW GREASING STREAMS READY"))
                    (unless (sb-thread:wait-on-semaphore peer-opened :timeout 12)
                      (error "The greasing HTTP/3 peer connection was not accepted."))
                    (let* ((common-connection
                             (first
                              (http-kit/http3::%http3-server-connection-snapshot
                               server)))
                           (listener (http-kit::http-server-listener server))
                           (connection
                             (and common-connection
                                  (gethash
                                   (http-kit::%http-server-connection-raw-stream
                                    common-connection)
                                   (http-kit/http3::%http3-server-listener-connections
                                    listener)))))
                      (unless connection
                        (error "The greasing HTTP/3 server connection was not registered."))
                      (sb-ext:with-timeout 12
                        (http-kit/http3::%http3-server-await-peer-settings
                         server connection nil))
                      (ensure-equal
                       nil (%http3-listener-error-summary observations)))
                    (%http3-listener-peer-release peer)
                  (ensure-equal
                   "RAW GREASING STREAMS RESPONSE"
                   (%http3-listener-peer-line
                    peer "RAW GREASING STREAMS RESPONSE"))
                  (ensure-equal 1 (%http3-listener-connection-count server))
                  (ensure-equal :running (http-kit::http-server-state server))
                  (ensure-equal nil (%http3-listener-error-summary observations))
                  (%http3-listener-peer-release peer)
                    (let* ((sent-line
                             (%http3-listener-peer-line
                              peer "RAW GREASING STREAMS SENT"))
                           (status (%http3-listener-peer-wait peer)))
                      (unless (zerop status)
                        (error "Raw HTTP/3 greasing action failed after ~A (exit ~D)."
                               sent-line status))
                      (ensure-true
                       (search "RAW GREASING STREAMS SENT" sent-line))
                      (ensure-false (search "RAW CONNECTION CLOSED" sent-line)))
                    (ensure-equal 1 handler-calls)
                    (ensure-equal :running (http-kit::http-server-state server))
                    (ensure-equal
                     nil (%http3-listener-error-summary observations)))
               (%http3-listener-peer-cleanup peer))))
         :event-callback
         (lambda (event)
           (when (eq (getf event :type) :connection-opened)
             (sb-thread:signal-semaphore peer-opened)))
         :options '(:max-header-bytes 4096))))

    (deftest http3-listener-raw-peer-rejects-http2-reserved-frames
      (let ((results nil))
        (dolist (action
                 (list "http2-reserved-frame-02"
                       "http2-reserved-frame-06"
                       "http2-reserved-frame-08"
                       "http2-reserved-frame-09"
                       "http2-reserved-request-frame-02"
                       "http2-reserved-request-frame-06"
                       "http2-reserved-request-frame-08"
                       "http2-reserved-request-frame-09"))
          (let ((result
                  (handler-case
                      (let ((handler-calls 0))
                        (%call-with-http3-listener
                         (lambda (request)
                           (declare (ignore request))
                           (incf handler-calls)
                           (make-http-response :status 200))
                         (lambda (server material observations)
                           (multiple-value-bind (output errors status)
                               (%http3-listener-run-peer
                                material "http3-raw" (http-server-port server)
                                "--raw-action" action "--timeout" "3s")
                             (list :status status
                                   :output output
                                   :errors errors
                                   :handler-calls handler-calls
                                   :listener-errors
                                   (%http3-listener-error-summary observations))))
                         :options '(:max-header-bytes 4096)))
                    (error (condition)
                      (list :error (princ-to-string condition)))))
                (expected
                  (format nil
                          "RAW CONNECTION CLOSED action=~A remote=true code=0x105"
                          action)))
            (let ((passed
                    (and (null (getf result :error))
                         (eql 0 (getf result :status))
                         (search expected (getf result :output))
                         (eql 0 (getf result :handler-calls)))))
              (push (list :action action
                          :expected expected
                          :passed passed
                          :result result)
                    results))))
        (unless (every (lambda (result) (getf result :passed)) results)
          (error "HTTP/2 reserved-frame action results: ~S"
                 (nreverse results)))))
)
)
#+sbcl
(deftest http3-listener-partial-unidirectional-prefix-does-not-hold-qpack-lock
  (let ((peer-opened (sb-thread:make-semaphore :count 0))
        (read-entered (sb-thread:make-semaphore :count 0)))
    (%call-with-http3-listener
     (lambda (request)
       (declare (ignore request))
       (make-http-response :status 200))
     (lambda (server material observations)
       (declare (ignore observations))
       (let ((peer nil))
         (unwind-protect
              (%http3-listener-test-call-with-function-stubs
               (list
                (cons 'http-kit/http3::%http3-server-stream-read
                      (let ((original
                              (symbol-function
                               'http-kit/http3::%http3-server-stream-read)))
                        (lambda (server connection stream &rest arguments)
                          (sb-thread:signal-semaphore read-entered)
                          (apply original server connection stream arguments)))))
               (lambda ()
                 (setf peer
                       (%http3-listener-launch-peer
                        material "http3-raw" (http-server-port server)
                        "--raw-action" "partial-unidirectional-prefix"
                        "--timeout" "8s"))
                 (ensure-equal
                  "RAW PARTIAL UNI SENT"
                  (%http3-listener-peer-line peer "partial uni prefix"))
                 (unless (sb-thread:wait-on-semaphore peer-opened :timeout 2)
                   (error "The partial-prefix peer connection was not accepted."))
                 (unless (sb-thread:wait-on-semaphore read-entered :timeout 2)
                   (error "The server did not start reading the partial prefix."))
                 (let* ((listener (http-kit::http-server-listener server))
                        (connection
                          (loop for entry being the hash-values of
                                (http-kit/http3::%http3-server-listener-connections
                                 listener)
                                return entry)))
                   (unless connection
                     (error "The partial-prefix connection was not registered."))
                   (ensure-false
                    (sb-thread:mutex-owner
                     (http-kit/http3::%http3-server-connection-qpack-lock
                      connection)))
                   (%http3-listener-peer-release peer)
                   (ensure-equal
                    "RAW PARTIAL UNI CONTROL SENT"
                    (%http3-listener-peer-line peer "partial uni control stream"))
                   (ensure-true
                    (%http3-listener-wait-until
                     (lambda ()
                       (http-kit/http3::%http3-server-connection-peer-settings-received-p
                        connection))
                     2)))
                 (%http3-listener-peer-release peer)
                 (ensure-equal 0 (%http3-listener-peer-wait peer))))
           (%http3-listener-peer-cleanup peer))))
     :event-callback
     (lambda (event)
       (when (eq (getf event :type) :connection-opened)
         (sb-thread:signal-semaphore peer-opened)))
     :options '(:request-timeout 2))))

#+sbcl
  (deftest http3-listener-cancel-removes-engine-streams
      (let* ((stream-map (make-hash-table :test #'eql))
             (engine (cl-quic-kit::%make-quic-client :streams stream-map))
             (quic-connection
               (cl-quic-kit::%make-quic-connection
                :role :server :state :established :stream-engine engine))
             (connection
               (%http3-listener-test-unit-connection
                :quic-connection quic-connection))
             (original-close-stream
               (symbol-function 'cl-quic-kit:quic-connection-close-stream))
             (current-stream nil)
             (current-stream-id nil))
        (%http3-listener-test-call-with-function-stubs
         (list
          (cons 'cl-quic-kit:quic-connection-flush
                (lambda (target)
                  (ensure-equal quic-connection target)
                  (ensure-equal 1 (hash-table-count stream-map))
                  (ensure-true
                   (eq current-stream (gethash current-stream-id stream-map)))))
          (cons 'cl-quic-kit:quic-connection-close-stream
                (lambda (target stream)
                  (funcall original-close-stream target stream))))
         (lambda ()
           (dotimes (iteration 32)
             (setf current-stream-id (+ 1 (* 4 iteration))
                   current-stream
                   (cl-quic-kit:make-stream current-stream-id
                                            :local-initiator :server)
                   (gethash current-stream-id stream-map) current-stream)
             (http-kit/http3::%http3-server-stream-cancel-locked
              connection current-stream #x10c)
             (ensure-false (gethash current-stream-id stream-map))
             (ensure-equal 0 (hash-table-count stream-map)))))))

#+sbcl
  (deftest http3-listener-request-completion-removes-engine-stream
      (let* ((stream-map (make-hash-table :test #'eql))
             (engine (cl-quic-kit::%make-quic-client :streams stream-map))
             (quic-connection
               (cl-quic-kit::%make-quic-connection
                :role :server :state :established :stream-engine engine))
             (server (%http3-listener-test-unit-server))
             (stream (cl-quic-kit:make-stream 0 :local-initiator :server))
             (connection
               (%http3-listener-test-unit-connection
                :quic-connection quic-connection :active-request-streams 1))
             (original-close-stream
               (symbol-function 'cl-quic-kit:quic-connection-close-stream)))
        (setf (gethash 0 stream-map) stream)
        (%http3-listener-test-call-with-function-stubs
         (list
          (cons 'http-kit/http3::%http3-server-await-peer-settings
                (lambda (&rest arguments) (declare (ignore arguments)) nil))
          (cons 'http-kit/http3:serve-http3-request-stream
                (lambda (&rest arguments) (declare (ignore arguments)) :complete))
          (cons 'cl-quic-kit:quic-connection-close-stream
                (lambda (target target-stream)
                  (funcall original-close-stream target target-stream)))
          (cons 'http-kit::%http-server-broadcast
                (lambda (&rest arguments) (declare (ignore arguments)) nil)))
         (lambda ()
           (http-kit/http3::%http3-server-request-worker
            server connection stream 0 nil)))
        (ensure-false (gethash 0 stream-map))
        (ensure-equal 0 (hash-table-count stream-map))
        (ensure-equal
         0
         (http-kit/http3::%http3-server-connection-active-request-streams
          connection))))
