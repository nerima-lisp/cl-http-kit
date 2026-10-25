(in-package #:http-kit/test)

#+sbcl
(progn
  (defun %network-tls-contract-condition (thunk type operation)
    (let ((condition (handler-case (funcall thunk)
                       (error (condition) condition))))
      (ensure-true (typep condition type))
      (ensure-equal operation (http-error-operation condition))
      (ensure-true (plusp (length (http-error-message condition))))
      condition))

  (defun %network-tls-contract-protocol (thunk operation detail)
    (let ((condition (%network-tls-contract-condition
                      thunk 'http-protocol-error operation)))
      (ensure-equal detail (http-protocol-error-detail condition))
      condition))

  (defun %network-tls-contract-peer (listener)
    (open-http-tcp-stream
     (make-http-request
      :method "GET"
      :uri (format nil "http://127.0.0.1:~D/"
                   (http-network-listener-port listener)))
     :timeout 2))

  (defclass network-tls-contract-stream (binary-test-stream)
    ((closed-p :initform nil :accessor network-tls-contract-closed-p)
     (close-count :initform 0 :accessor network-tls-contract-close-count)
     (abort-p :initform nil :accessor network-tls-contract-abort-p)))

  (defmethod open-stream-p ((stream network-tls-contract-stream))
    (not (network-tls-contract-closed-p stream)))

  (defmethod close ((stream network-tls-contract-stream) &key abort)
    (incf (network-tls-contract-close-count stream))
    (setf (network-tls-contract-closed-p stream) t
          (network-tls-contract-abort-p stream) abort)
    t)

  (deftest network-tls-contract-listener-inputs
    (dolist (case '(((:host 7) 7)
                    ((:port -1) (:port -1))
                    ((:port 65536) (:port 65536))
                    ((:port 1/2) (:port 1/2))
                    ((:backlog 0) (:backlog 0))
                    ((:backlog -1) (:backlog -1))
                    ((:backlog "1") (:backlog "1"))
                    ((:clock-function nil) :clock-function)))
      (%network-tls-contract-protocol
       (lambda () (apply #'open-http-tcp-listener (first case)))
       :open (second case)))
    (%network-tls-contract-protocol
     (lambda () (accept-http-tcp-stream :not-listener)) :accept :not-listener)
    (%network-tls-contract-protocol
     (lambda () (close-http-tcp-listener :not-listener)) :close :not-listener)
    (let ((listener (open-http-tcp-listener :host "127.0.0.1")))
      (unwind-protect
           (progn
             (ensure-true (http-network-listener-p listener))
             (ensure-equal :ipv4 (http-network-listener-address-family listener))
             (ensure-true (plusp (http-network-listener-port listener)))
             (%network-tls-contract-protocol
              (lambda () (accept-http-tcp-stream listener :clock-function nil))
              :accept :clock-function)
             (ensure-equal nil (close-http-tcp-listener listener))
             (ensure-equal nil (close-http-tcp-listener listener))
             (%network-tls-contract-protocol
              (lambda () (accept-http-tcp-stream listener))
              :accept :closed-listener))
        (close-http-tcp-listener listener))))

  (deftest network-tls-contract-serve-inputs
    (%network-tls-contract-protocol
     (lambda () (serve-http1-listener :not-listener #'identity))
     :serve :not-listener)
    (let ((listener (open-http-tcp-listener :host "127.0.0.1")))
      (unwind-protect
           (progn
             (%network-tls-contract-protocol
              (lambda () (serve-http1-listener listener nil))
              :serve '(:handler nil))
             (dolist (options '((:max-connections -1) (:max-connections 1/2)
                                (:on-accept 1) (:on-error 1) (:stream-wrapper 1)
                                (:session-options (:key))
                                (:session-options (ordinary 1))
                                (:session-options (:key . 1))
                                (:clock-function 1)))
               (%network-tls-contract-protocol
                (lambda () (apply #'serve-http1-listener listener #'identity options))
                :serve options))
             (multiple-value-bind (count reason)
                 (serve-http1-listener listener #'identity :max-connections 0
                                      :session-options '(:close-stream nil))
               (ensure-equal 0 count)
               (ensure-equal :max-connections reason)))
        (close-http-tcp-listener listener))))

  (deftest network-tls-contract-cyclic-session-options
    (let ((listener (open-http-tcp-listener :host "127.0.0.1"))
          (calls 0))
      (unwind-protect
           (flet ((handler (&rest arguments)
                    (declare (ignore arguments))
                    (incf calls)))
             (dolist (size '(1 2 3 4))
               (let* ((tail (make-list size :initial-element :key))
                      (last (last tail))
                      (options (append (list :close-stream nil) tail)))
                 (unwind-protect
                      (progn
                        (setf (cdr last) tail)
                        (let ((condition
                                (%network-tls-contract-condition
                                 (lambda ()
                                   (serve-http1-listener
                                    listener #'handler :max-connections 0
                                    :session-options options))
                                 'http-protocol-error :serve)))
                          (ensure-equal :session-options
                                        (first (http-protocol-error-detail condition)))
                          (ensure-true
                           (eq options
                               (second (http-protocol-error-detail condition))))))
                   (setf (cdr last) nil))))
             (multiple-value-bind (count reason)
                 (serve-http1-listener listener #'handler :max-connections 0
                                      :session-options '(:close-stream nil))
               (ensure-equal 0 count)
               (ensure-equal :max-connections reason))
             (ensure-equal 0 calls)
             (ensure-true (plusp (http-network-listener-port listener))))
        (close-http-tcp-listener listener))))

  (deftest network-tls-contract-accept-timeout-reusable
    (let ((listener (open-http-tcp-listener :host "127.0.0.1"))
          (observed nil) (calls 0) (peer nil) (accepted nil))
      (unwind-protect
           (progn
             (multiple-value-bind (count reason)
                 (serve-http1-listener
                  listener #'identity :accept-deadline 0
                  :clock-function (lambda () 1)
                  :on-error (lambda (condition address port)
                              (incf calls)
                              (setf observed condition)
                              (ensure-equal nil address)
                              (ensure-equal nil port)))
               (ensure-equal 0 count)
               (ensure-equal :timeout reason))
             (ensure-equal 1 calls)
             (ensure-true (typep observed 'http-timeout))
             (ensure-equal :accept (http-error-operation observed))
             (ensure-equal :accept (http-timeout-kind observed))
             (setf peer (%network-tls-contract-peer listener)
                   accepted (accept-http-tcp-stream listener :timeout 2))
             (ensure-true (open-stream-p accepted)))
        (close-http-tcp-stream accepted)
        (close-http-tcp-stream peer)
        (close-http-tcp-listener listener))))

  (deftest network-tls-contract-callback-error-cleanup
    (dolist (failure-point '(:wrapper :on-accept))
      (let ((listener (open-http-tcp-listener :host "127.0.0.1"))
            (peer nil) (accepted nil) (observed nil) (calls 0) (handled 0)
            (expected (make-condition 'http-protocol-error
                                      :message "Callback rejected the connection."
                                      :operation :test-callback :detail failure-point)))
        (unwind-protect
             (progn
               (setf peer (%network-tls-contract-peer listener))
               (multiple-value-bind (count reason)
                   (serve-http1-listener
                    listener (lambda (request) (declare (ignore request)) (incf handled))
                    :max-connections 1 :accept-timeout 2
                    :stream-wrapper
                    (lambda (stream &key timeout deadline)
                      (declare (ignore timeout deadline))
                      (setf accepted stream)
                      (when (eq failure-point :wrapper) (error expected))
                      stream)
                    :on-accept (lambda (stream address port)
                                 (ensure-true (eq stream accepted))
                                 (ensure-equal "127.0.0.1" address)
                                 (ensure-true (plusp port))
                                 (error expected))
                    :on-error (lambda (condition address port)
                                (incf calls)
                                (setf observed condition)
                                (ensure-equal "127.0.0.1" address)
                                (ensure-true (plusp port))))
                 (ensure-equal 1 count)
                 (ensure-equal :max-connections reason))
               (ensure-equal 1 calls)
               (ensure-true (eq expected observed))
               (ensure-equal 0 handled)
               (ensure-true (streamp accepted))
               (ensure-false (open-stream-p accepted))
               (ensure-equal nil (sb-ext:with-timeout 2 (read-byte peer nil nil))))
          (close-http-tcp-stream accepted)
          (close-http-tcp-stream peer)
          (close-http-tcp-listener listener)))))

  (deftest network-tls-contract-callback-throw-cleanup
    (dolist (failure-point '(:wrapper :on-accept))
      (let ((listener (open-http-tcp-listener :host "127.0.0.1"))
            (peer nil) (accepted nil) (reused nil)
            (handled 0) (errors 0) (closes 0)
            (exit-value (list failure-point)))
        (unwind-protect
             (progn
               (setf peer (%network-tls-contract-peer listener))
               (ensure-true
                (eq exit-value
                    (%network-tls-contract-with-spy
                     'close-http-tcp-stream
                     (lambda (original)
                       (lambda (stream)
                         (when (eq stream accepted) (incf closes))
                         (funcall original stream)))
                     (lambda ()
                       (catch 'network-callback-exit
                         (serve-http1-listener
                          listener
                          (lambda (request)
                            (declare (ignore request)) (incf handled))
                          :max-connections 1 :accept-timeout 2
                          :stream-wrapper
                          (lambda (stream &key timeout deadline)
                            (ensure-equal 2 timeout)
                            (ensure-equal nil deadline)
                            (setf accepted stream)
                            (when (eq failure-point :wrapper)
                              (throw 'network-callback-exit exit-value))
                            stream)
                          :on-accept
                          (lambda (stream address port)
                            (ensure-true (eq stream accepted))
                            (ensure-equal "127.0.0.1" address)
                            (ensure-true (plusp port))
                            (throw 'network-callback-exit exit-value))
                          :on-error
                          (lambda (&rest arguments)
                            (declare (ignore arguments)) (incf errors)))
                         :returned)))))
               (ensure-equal 0 handled)
               (ensure-equal 0 errors)
               (ensure-true (streamp accepted))
               (ensure-false (open-stream-p accepted))
               (ensure-equal 1 closes)
               (ensure-equal nil (sb-ext:with-timeout 2 (read-byte peer nil nil)))
               (close-http-tcp-stream peer)
               (setf peer (%network-tls-contract-peer listener)
                     reused (accept-http-tcp-stream listener :timeout 2))
               (ensure-true (open-stream-p reused)))
          (close-http-tcp-stream reused)
          (close-http-tcp-stream accepted)
          (close-http-tcp-stream peer)
          (close-http-tcp-listener listener)))))

  (defun %network-listener-session-ownership (options handler check
                                            &optional
                                              (wire "GET /ownership HTTP/1.1|CRLF|Host: localhost|CRLF||CRLF|"))
    (let ((listener (open-http-tcp-listener :host "127.0.0.1"))
          (peer nil) (accepted nil) (closes 0) (errors nil)
          (first-condition nil) (result nil))
      (unwind-protect
           (progn
             (setf peer (%network-tls-contract-peer listener))
             (when wire
               (sb-ext:with-timeout 2
                 (write-sequence (ascii wire) peer)
                 (finish-output peer)))
             (setf result
                   (%network-tls-contract-with-spy
                    'close-http-tcp-stream
                    (lambda (original)
                      (lambda (stream)
                        (when (eq stream accepted) (incf closes))
                        (funcall original stream)))
                    (lambda ()
                      (catch 'network-session-exit
                        (multiple-value-list
                           (serve-http1-listener
                            listener
                            (lambda (request)
                              (handler-bind
                                  ((error (lambda (condition)
                                            (unless first-condition
                                              (setf first-condition condition)))))
                                (funcall handler request)))
                            :max-connections 1 :accept-timeout 2
                            :session-options options
                            :on-accept
                            (lambda (stream address port)
                              (setf accepted stream)
                              (ensure-equal "127.0.0.1" address)
                              (ensure-true (plusp port)))
                            :on-error
                            (lambda (condition address port)
                              (declare (ignore address port))
                              (push condition errors))))))))
             (funcall check accepted peer closes errors first-condition result))
        (close-http-tcp-stream accepted)
        (close-http-tcp-stream peer)
        (close-http-tcp-listener listener))))

  (deftest network-listener-session-error-once
    (dolist (custom-close-p '(nil t))
      (let ((expected (make-condition 'http-protocol-error :message "Handler failure."
                                     :operation :test-handler :detail :ownership))
            (handled 0) (session-errors 0) (close-hooks 0))
        (%network-listener-session-ownership
         (append
          (list :timeout 2 :on-error
                (lambda (condition request)
                  (incf session-errors)
                  (ensure-true (eq expected condition))
                  (ensure-equal "/ownership" (http-request-target request))))
          (when custom-close-p
            (list :close-stream (lambda (stream)
                                  (incf close-hooks)
                                  (close-http-tcp-stream stream)))))
         (lambda (request)
           (ensure-equal "GET" (http-request-method request))
           (incf handled)
           (error expected))
         (lambda (accepted peer closes errors first-condition result)
           (ensure-equal '(1 :max-connections) result)
           (ensure-equal 1 handled)
           (ensure-equal 1 session-errors)
           (ensure-equal (if custom-close-p 1 0) close-hooks)
           (ensure-equal 1 (length errors))
           (ensure-true (eq expected (first errors)))
           (ensure-true (eq expected first-condition))
           (ensure-equal 1 closes)
           (ensure-false (open-stream-p accepted))
           (ensure-equal :eof (sb-ext:with-timeout 2 (read-byte peer nil :eof))))))))

  (deftest network-listener-session-error-retained
    (let ((expected (make-condition 'http-protocol-error :message "Retained handler failure."
                                   :operation :test-handler :detail :retained)))
      (%network-listener-session-ownership
       (list :timeout 2 :close-stream nil)
       (lambda (request) (declare (ignore request)) (error expected))
       (lambda (accepted peer closes errors first-condition result)
         (ensure-equal '(1 :max-connections) result)
         (ensure-equal 1 (length errors))
         (ensure-true (eq expected (first errors)))
         (ensure-true (eq expected first-condition))
         (ensure-equal 0 closes)
         (ensure-true (open-stream-p accepted))
         (sb-ext:with-timeout 2
           (write-byte 123 accepted)
           (finish-output accepted)
           (ensure-equal 123 (read-byte peer)))))))

  (deftest network-listener-session-validation-cleanup
    (dolist (options '((:max-requests -1) (:max-requests -1 :close-stream nil)
                      (:on-error 7) (:on-upgrade 7) (:on-expect-continue 7)
                      (:close-stream 7) (:timeout -1) (:deadline :invalid)
                      (:timeout 1 :clock-function 7) (:unknown-session-option 7)))
      (let ((handled 0))
        (%network-listener-session-ownership
         options
         (lambda (request) (declare (ignore request)) (incf handled))
         (lambda (accepted peer closes errors first-condition result)
           (declare (ignore first-condition))
           (ensure-equal '(1 :max-connections) result)
           (ensure-equal 0 handled)
           (ensure-equal 1 (length errors))
           (ensure-true (typep (first errors) 'error))
           (ensure-equal 1 closes)
           (ensure-false (open-stream-p accepted))
           (ensure-equal :eof (sb-ext:with-timeout 2 (read-byte peer nil :eof))))
         nil))))

  (deftest network-listener-session-nonlocal-cleanup
    (dolist (phase '(:clock :nested-clock :handler :retained-handler :upgrade))
      (let ((exit-value (list phase)) (handled 0))
        (%network-listener-session-ownership
         (append
          (list :timeout 2)
          (when (member phase '(:clock :nested-clock))
            (list :clock-function
                  (lambda ()
                    (when (eq phase :nested-clock)
                      (with-open-stream
                          (nested (make-two-way-stream (make-string-input-stream "")
                                                       (make-string-output-stream)))
                        (multiple-value-bind (count reason)
                            (serve-http1-session nested
                                                 (lambda (request) (declare (ignore request)))
                                                 :max-requests 0 :close-stream nil)
                          (ensure-equal 0 count)
                          (ensure-equal :max-requests reason))))
                    (throw 'network-session-exit exit-value))))
          (when (eq phase :retained-handler) (list :close-stream nil))
          (when (eq phase :upgrade)
            (list :on-upgrade
                  (lambda (stream request response)
                    (ensure-true (open-stream-p stream))
                    (ensure-equal "/ownership" (http-request-target request))
                    (ensure-equal 101 (http-response-status response))
                    (throw 'network-session-exit exit-value)))))
         (lambda (request)
           (declare (ignore request))
           (incf handled)
           (if (eq phase :upgrade)
               (make-http-response
                :status 101 :headers (list (make-http-header "Connection" "Upgrade")
                                          (make-http-header "Upgrade" "websocket")))
               (throw 'network-session-exit exit-value)))
         (lambda (accepted peer closes errors first-condition result)
           (ensure-true (eq exit-value result))
           (ensure-equal (if (member phase '(:clock :nested-clock)) 0 1) handled)
           (ensure-equal nil errors)
           (ensure-equal nil first-condition)
           (ensure-equal (if (eq phase :retained-handler) 0 1) closes)
           (ensure-equal (eq phase :retained-handler) (open-stream-p accepted))
           (sb-ext:with-timeout 2
             (when (eq phase :upgrade)
               (ensure-equal 101 (http-response-status (parse-http-response peer :timeout 2))))
             (if (eq phase :retained-handler)
                 (progn (write-byte 123 accepted) (finish-output accepted)
                        (ensure-equal 123 (read-byte peer)))
                 (ensure-equal :eof (read-byte peer nil :eof)))))
         (unless (member phase '(:clock :nested-clock))
           "GET /ownership HTTP/1.1|CRLF|Host: localhost|CRLF|Connection: Upgrade|CRLF|Upgrade: websocket|CRLF||CRLF|")))))

  (deftest network-listener-session-upgrade-retained
    (let ((upgrades 0))
      (%network-listener-session-ownership
       (list :timeout 2 :on-upgrade
             (lambda (stream request response)
               (incf upgrades)
               (ensure-true (open-stream-p stream))
               (ensure-equal "/ownership" (http-request-target request))
               (ensure-equal 101 (http-response-status response))
               t))
       (lambda (request)
         (declare (ignore request))
         (make-http-response
          :status 101 :headers (list (make-http-header "Connection" "Upgrade")
                                    (make-http-header "Upgrade" "websocket"))))
       (lambda (accepted peer closes errors first-condition result)
         (ensure-equal '(1 :max-connections) result)
         (ensure-equal 1 upgrades)
         (ensure-equal nil errors)
         (ensure-equal nil first-condition)
         (ensure-equal 0 closes)
         (ensure-true (open-stream-p accepted))
         (sb-ext:with-timeout 2
           (ensure-equal 101 (http-response-status (parse-http-response peer :timeout 2)))
           (write-byte 123 accepted)
           (finish-output accepted)
           (ensure-equal 123 (read-byte peer))))
       "GET /ownership HTTP/1.1|CRLF|Host: localhost|CRLF|Connection: Upgrade|CRLF|Upgrade: websocket|CRLF||CRLF|")))

  (deftest network-tls-contract-nil-wrapper-cleanup
    (let ((listener (open-http-tcp-listener :host "127.0.0.1"))
          (peer nil) (raw nil) (reused nil) (observed nil)
          (accepts 0) (errors 0) (handled 0))
      (unwind-protect
           (progn
             (setf peer (%network-tls-contract-peer listener))
             (multiple-value-bind (count reason)
                 (serve-http1-listener
                  listener (lambda (request) (declare (ignore request)) (incf handled))
                  :max-connections 1 :accept-timeout 2
                  :stream-wrapper (lambda (stream &key timeout deadline)
                                    (ensure-equal 2 timeout)
                                    (ensure-equal nil deadline)
                                    (setf raw stream)
                                    nil)
                  :on-accept (lambda (&rest arguments)
                               (declare (ignore arguments)) (incf accepts))
                  :on-error (lambda (condition address port)
                              (incf errors) (setf observed condition)
                              (ensure-equal "127.0.0.1" address)
                              (ensure-true (plusp port))))
               (ensure-equal 1 count)
               (ensure-equal :max-connections reason))
             (ensure-equal 0 accepts)
             (ensure-equal 0 handled)
             (ensure-equal 1 errors)
             (ensure-true (typep observed 'http-protocol-error))
             (ensure-equal :serve (http-error-operation observed))
             (ensure-equal '(:stream-wrapper nil) (http-protocol-error-detail observed))
             (ensure-true (plusp (length (http-error-message observed))))
             (ensure-true (streamp raw))
             (ensure-false (open-stream-p raw))
             (ensure-equal nil (sb-ext:with-timeout 2 (read-byte peer nil nil)))
             (close-http-tcp-stream peer)
             (setf peer (%network-tls-contract-peer listener)
                   reused (accept-http-tcp-stream listener :timeout 2))
             (ensure-true (open-stream-p reused)))
        (close-http-tcp-stream reused)
        (close-http-tcp-stream raw)
        (close-http-tcp-stream peer)
        (close-http-tcp-listener listener))))

  (deftest network-tls-contract-error-callback-propagation
    (let ((listener (open-http-tcp-listener :host "127.0.0.1"))
          (peer nil) (raw nil) (calls 0)
          (primary (make-condition 'http-protocol-error :message "Connection rejected."
                                   :operation :test-primary :detail :primary))
          (callback (make-condition 'http-protocol-error :message "Error callback rejected."
                                    :operation :test-callback :detail :callback)))
      (unwind-protect
           (progn
             (setf peer (%network-tls-contract-peer listener))
             (let ((observed
                     (handler-case
                         (serve-http1-listener
                          listener #'identity :max-connections 1 :accept-timeout 2
                          :on-accept (lambda (stream address port)
                                       (declare (ignore address port))
                                       (setf raw stream) (error primary))
                          :on-error (lambda (condition address port)
                                      (incf calls)
                                      (ensure-true (eq primary condition))
                                      (ensure-equal "127.0.0.1" address)
                                      (ensure-true (plusp port))
                                      (error callback)))
                       (http-protocol-error (condition) condition))))
               (ensure-true (eq callback observed)))
             (ensure-equal 1 calls)
             (ensure-false (open-stream-p raw))
             (ensure-equal nil (sb-ext:with-timeout 2 (read-byte peer nil nil))))
        (close-http-tcp-stream raw)
        (close-http-tcp-stream peer)
        (close-http-tcp-listener listener))))

  (deftest network-tls-contract-accept-error-identity
    (let ((listener (open-http-tcp-listener :host "127.0.0.1"))
          (observed nil) (calls 0))
      (close-http-tcp-listener listener)
      (let ((condition
              (%network-tls-contract-protocol
               (lambda ()
                 (serve-http1-listener
                  listener #'identity :max-connections 1
                  :on-error (lambda (condition address port)
                              (incf calls) (setf observed condition)
                              (ensure-equal nil address) (ensure-equal nil port))))
               :accept :closed-listener)))
        (ensure-equal 1 calls)
        (ensure-true (eq condition observed)))))

  (deftest network-tls-contract-endpoint-inputs
    (%network-tls-contract-protocol
     (lambda () (make-http-network-stream-opener :clock-function nil))
     :network nil)
    (let ((request (make-http-request :method "GET" :uri "http://127.0.0.1/")))
      (dolist (plan '((:mode :unknown)
                      (:mode :forward :connect-host "" :connect-port 80)
                      (:mode :forward :connect-host "127.0.0.1" :connect-port 0)
                      (:mode :connect :proxy-host "127.0.0.1" :proxy-port 65536)
                      (:mode :socks5 :proxy-host 3 :proxy-port 80)))
        (let ((condition
                (%network-tls-contract-condition
                 (lambda () (funcall (make-http-network-stream-opener)
                                     request :proxy-plan plan))
                 'http-connection-error :endpoint)))
          (ensure-true (eq plan (http-connection-error-cause condition)))))))

  (deftest network-tls-contract-tls-factory-inputs
    (dolist (options '((:verify :invalid) (:alpn-protocols 1)
                       (:alpn-protocols ("")) (:alpn-protocols (1))
                       (:clock-function nil)))
      (%network-tls-contract-protocol
       (lambda () (apply #'make-http-tls-upgrader options)) :tls
       (case (first options)
         (:verify :invalid-verify) (:alpn-protocols :invalid-alpn)
         (:clock-function :invalid-clock-function))))
    (dolist (protocol (list (make-string 256 :initial-element #\a)
                            (string (code-char 0)) (string (code-char 128))))
      (%network-tls-contract-protocol
       (lambda () (make-http-tls-upgrader :alpn-protocols (list protocol)))
       :tls :invalid-alpn))
    (dolist (protocol (list "h" (make-string 255 :initial-element #\a)))
      (ensure-true (functionp (make-http-tls-upgrader :verify :optional
                                                     :trust-anchors (list t)
                                                     :alpn-protocols (list protocol)))))
    (let ((condition
            (%network-tls-contract-condition
             (lambda () (make-http-tls-upgrader :verify nil))
             'http-unsupported-feature :tls)))
      (ensure-equal :tls-certificate-verification-disable
                    (http-unsupported-feature-name condition))
      (ensure-equal :disabled (http-protocol-error-detail condition)))
    (dolist (options '((:certificate :certificate) (:key :key)))
      (%network-tls-contract-protocol
       (lambda () (apply #'make-http-tls-upgrader options))
       :tls '(:role "client" :required nil)))
    (dolist (case '(((:certificate :certificate :key :key) :tls-client-certificate-auth)
                    ((:password "test") :tls-client-certificate-auth)
                    ((:unwrap-stream-p t) :tls-stream-unwrapping)))
      (let ((condition
              (%network-tls-contract-condition
               (lambda () (apply #'make-http-tls-upgrader (first case)))
               'http-unsupported-feature :tls)))
        (ensure-equal (second case) (http-unsupported-feature-name condition)))))

  (deftest network-tls-contract-tls-stream-and-server-inputs
    (%network-tls-contract-protocol
     (lambda () (http-tls-selected-alpn-protocol :not-stream)) :tls :invalid-stream)
    (%network-tls-contract-protocol
     (lambda ()
       (funcall (make-http-tls-upgrader :verify :optional
                                        :trust-anchors (list t))
                :not-stream nil))
     :tls :not-stream)
    (dolist (options '(nil (:certificate-chain nil) (:private-key nil)))
      (%network-tls-contract-protocol
       (lambda () (apply #'make-http-tls-server-wrapper options))
       :tls '(:role "server" :required t)))
    (%network-tls-contract-protocol
     (lambda () (make-http-tls-server-wrapper :clock-function nil))
     :tls :invalid-clock-function)
    (let ((stream (make-instance 'network-tls-contract-stream :input (octets))))
      (unwind-protect
           (let ((condition
                   (%network-tls-contract-condition
                    (lambda () (http-tls-selected-alpn-protocol stream))
                    'http-unsupported-feature :tls)))
             (ensure-equal :tls-alpn-result (http-unsupported-feature-name condition))
             (ensure-true (open-stream-p stream))
             (ensure-equal 0 (network-tls-contract-close-count stream)))
        (close stream :abort t))))

  (deftest network-tls-contract-handshake-input-cleanup
    (dolist (case '((() :handshake-failed)
                    ((22 3) :truncated-record)
                    ((22 3 3 0 2 1) :truncated-record)
                    ((22 3 3 64 1) :record-overflow)))
      (let* ((wire (make-array (length (first case)) :element-type '(unsigned-byte 8)
                              :initial-contents (first case)))
             (stream (make-instance 'network-tls-contract-stream :input wire)))
        (unwind-protect
             (progn
               (%network-tls-contract-protocol
                (lambda ()
                  (funcall (make-http-tls-upgrader :verify :optional
                                   :trust-anchors (list t))
                           stream
                           (http-request-uri
                            (make-http-request :method "GET" :uri "https://127.0.0.1/"))
                           :timeout 2))
                :tls (second case))
               (ensure-false (open-stream-p stream))
               ;; The driver releases its transport before the upgrader aborts setup.
               (ensure-equal 2 (network-tls-contract-close-count stream))
               (ensure-true (network-tls-contract-abort-p stream))
               (ensure-equal (length wire)
                             (http-kit/test-core::binary-test-input-position stream))
               (ensure-true (plusp (length (binary-test-output stream)))))
          (when (open-stream-p stream) (close stream :abort t))))))

  (deftest network-tls-contract-handshake-expired-cleanup
    (let ((stream (make-instance 'network-tls-contract-stream :input (octets))))
      (unwind-protect
           (let ((condition
                   (%network-tls-contract-condition
                    (lambda ()
                      (funcall (make-http-tls-upgrader :verify :optional
                                                       :trust-anchors (list t)
                                                       :clock-function (lambda () 1))
                               stream (http-request-uri
                                       (make-http-request :method "GET"
                                                          :uri "https://127.0.0.1/"))
                               :deadline 0))
                    'http-timeout :tls)))
             (ensure-equal :tls (http-timeout-kind condition))
             (ensure-false (open-stream-p stream))
             (ensure-equal 1 (network-tls-contract-close-count stream))
             (ensure-true (network-tls-contract-abort-p stream))
             (ensure-equal 0 (length (binary-test-output stream)))
             (ensure-equal 0 (http-kit/test-core::binary-test-input-position stream)))
        (when (open-stream-p stream) (close stream :abort t))))))

#+sbcl
(progn
  (defun %network-tls-contract-with-spy (symbol wrapper thunk)
    (let ((original (fdefinition symbol)))
      (unwind-protect
           (progn
             (setf (fdefinition symbol) (funcall wrapper original))
             (funcall thunk))
        (setf (fdefinition symbol) original)
        (ensure-true (eq original (fdefinition symbol))))))

  (deftest network-tls-contract-wildcard-listeners
    (dolist (ipv6-p '(nil t))
      (let ((listener (open-http-tcp-listener :ipv6-p ipv6-p))
            (peer nil) (accepted nil))
        (unwind-protect
             (progn
               (ensure-equal (if ipv6-p :ipv6 :ipv4)
                             (http-network-listener-address-family listener))
               (ensure-true (plusp (http-network-listener-port listener)))
               (ensure-true
                (if ipv6-p
                    (find #\: (http-network-listener-address listener))
                    (string= "0.0.0.0" (http-network-listener-address listener))))
               (setf peer
                     (open-http-tcp-stream
                      (make-http-request
                       :method "GET"
                       :uri (format nil "http://~A:~D/"
                                    (if ipv6-p "[::1]" "127.0.0.1")
                                    (http-network-listener-port listener)))
                      :timeout 2))
               (multiple-value-bind (stream address port)
                   (accept-http-tcp-stream listener :timeout 2)
                 (setf accepted stream)
                 (ensure-true (streamp stream))
                 (ensure-true (plusp port))
                 (ensure-true (if ipv6-p (find #\: address)
                                 (string= "127.0.0.1" address))))
               (write-byte 19 peer)
               (finish-output peer)
               (ensure-equal 19 (read-byte accepted))
               (write-byte 73 accepted)
               (finish-output accepted)
               (ensure-equal 73 (read-byte peer)))
          (when accepted (close accepted :abort t))
          (when peer (close peer :abort t))
          (close-http-tcp-listener listener)))))

  (deftest network-tls-contract-bind-collision-cleanup
    (let ((listener (open-http-tcp-listener :host "127.0.0.1"
                                          :reuse-address nil))
          (closed nil) (peer nil) (accepted nil))
      (unwind-protect
           (progn
             (%network-tls-contract-with-spy
              'sb-bsd-sockets:socket-close
              (lambda (original)
                (lambda (socket)
                  (push socket closed)
                  (funcall original socket)))
              (lambda ()
                (let ((condition
                        (handler-case
                            (open-http-tcp-listener
                             :host "127.0.0.1" :reuse-address nil
                             :port (http-network-listener-port listener))
                          (error (condition) condition))))
                  (ensure-true (typep condition 'sb-bsd-sockets:address-in-use-error))
                  (ensure-equal 1 (length closed))
                  (ensure-true
                   (handler-case (progn (sb-bsd-sockets:socket-name (first closed)) nil)
                     (sb-bsd-sockets:socket-error () t))))))
             (setf peer (%network-tls-contract-peer listener)
                   accepted (accept-http-tcp-stream listener :timeout 2))
             (write-byte 31 peer)
             (finish-output peer)
             (ensure-equal 31 (read-byte accepted)))
        (when accepted (close accepted :abort t))
        (when peer (close peer :abort t))
        (close-http-tcp-listener listener))))

  (deftest network-tls-contract-resolver-error-identity
    (dolist (host '(nil "" 7))
      (let ((condition
              (%network-tls-contract-condition
               (lambda () (http-network-resolve-host host))
               'http-connection-error :resolve)))
        (ensure-equal nil (http-connection-error-cause condition))))
    (dolist (http-error-p '(nil t))
      (let ((sentinel (if http-error-p
                          (make-condition 'http-timeout :message "resolver deadline"
                                                        :operation :resolve :kind :resolve)
                          (make-condition 'simple-error :format-control "resolver failure")))
            (calls 0))
        (%network-tls-contract-with-spy
         'sb-bsd-sockets:get-host-by-name
         (lambda (original)
           (declare (ignore original))
           (lambda (host)
             (ensure-equal "127.0.0.1" host)
             (incf calls)
             (error sentinel)))
         (lambda ()
           (let ((condition
                   (%network-tls-contract-condition
                    (lambda () (http-network-resolve-host "127.0.0.1"))
                    (if http-error-p 'http-timeout 'http-connection-error) :resolve)))
             (ensure-equal 1 calls)
             (if http-error-p
                 (progn
                   (ensure-true (eq sentinel condition))
                   (ensure-equal :resolve (http-timeout-kind condition)))
                 (ensure-true (eq sentinel (http-connection-error-cause condition))))))))))

  (deftest network-tls-contract-resolver-completion-deadline
    (let ((tick 0) (calls 0))
      (%network-tls-contract-with-spy
       'sb-bsd-sockets:get-host-by-name
       (lambda (original)
         (lambda (host)
           (incf calls)
           (prog1 (funcall original host) (setf tick 2))))
       (lambda ()
         (let ((condition
                 (%network-tls-contract-condition
                  (lambda ()
                    (http-network-resolve-host "127.0.0.1" :deadline 1
                                               :clock-function (lambda () tick)))
                  'http-timeout :resolve)))
           (ensure-equal 1 calls)
           (ensure-equal :resolve (http-timeout-kind condition)))))))

  (deftest network-tls-contract-missing-trust-store
    (dolist (verify '(:required :optional))
      (let ((stream (make-instance 'network-tls-contract-stream :input (octets)))
            (calls 0))
        (unwind-protect
             (%network-tls-contract-with-spy
              'cl-tls-kit:default-trust-store-path
              (lambda (original)
                (declare (ignore original))
                (lambda () (incf calls) nil))
              (lambda ()
                (let ((condition
                        (%network-tls-contract-condition
                         (lambda ()
                           (funcall (make-http-tls-upgrader :verify verify) stream
                                    (parse-http-uri "https://127.0.0.1/") :timeout 2))
                         (if (eq verify :required)
                             'http-unsupported-feature 'http-protocol-error) :tls)))
                  (ensure-equal 1 calls)
                  (if (eq verify :required)
                      (progn
                        (ensure-equal :tls-trust-store (http-unsupported-feature-name condition))
                        (ensure-equal :not-found (http-protocol-error-detail condition))
                        (ensure-equal 0 (length (binary-test-output stream))))
                      (progn
                        (ensure-equal :handshake-failed
                                      (http-protocol-error-detail condition))
                        (ensure-true (plusp (length (binary-test-output stream)))))))
                (ensure-false (open-stream-p stream))
                (ensure-equal (if (eq verify :required) 1 2)
                              (network-tls-contract-close-count stream))
                (ensure-true (network-tls-contract-abort-p stream))))
          (when (open-stream-p stream) (close stream :abort t))))))

  (deftest network-tls-contract-trust-store-load-error
    (let ((stream (make-instance 'network-tls-contract-stream :input (octets)))
          (path-calls 0) (load-calls 0))
      (unwind-protect
           (%network-tls-contract-with-spy
            'cl-tls-kit:default-trust-store-path
            (lambda (original)
              (declare (ignore original))
              (lambda () (incf path-calls) "missing-ca.pem"))
            (lambda ()
              (%network-tls-contract-with-spy
               'cl-tls-kit:load-trust-store
               (lambda (original)
                 (declare (ignore original))
                 (lambda (&key ca-file)
                   (ensure-equal "missing-ca.pem" ca-file)
                   (incf load-calls)
                   (error "Trust store unavailable")))
               (lambda ()
                 (%network-tls-contract-protocol
                  (lambda ()
                    (funcall (make-http-tls-upgrader :verify :required) stream
                             (parse-http-uri "https://127.0.0.1/") :timeout 2))
                  :tls :invalid-trust-store)
                 (ensure-equal 1 path-calls)
                 (ensure-equal 1 load-calls)
                 (ensure-false (open-stream-p stream))
                 (ensure-equal 1 (network-tls-contract-close-count stream))
                 (ensure-true (network-tls-contract-abort-p stream))
                 (ensure-equal 0 (length (binary-test-output stream)))))))
        (when (open-stream-p stream) (close stream :abort t)))))

  (deftest network-tls-contract-connect-deadline-cleanup
    (dolist (native-timeout-p '(nil t))
      (let ((stream (make-instance 'network-tls-contract-stream :input (octets)))
            (tick 0) (calls 0))
        (unwind-protect
             (%network-tls-contract-with-spy
              'cl-tls-kit:tls13-client-driver-connect
              (lambda (original)
                (declare (ignore original))
                (lambda (driver)
                  (ensure-true (cl-tls-kit:tls13-client-driver-p driver))
                  (incf calls)
                  ;; Only deadline rejection is exercised, not a successful handshake.
                  (if native-timeout-p (sleep 0.1) (setf tick 2))))
              (lambda ()
                (let ((condition
                        (%network-tls-contract-condition
                         (lambda ()
                           (let ((upgrader
                                   (if native-timeout-p
                                       (make-http-tls-upgrader
                                        :verify :optional
                                        :trust-anchors (list t))
                                       (make-http-tls-upgrader
                                        :verify :optional
                                        :trust-anchors (list t)
                                        :clock-function (lambda () tick)))))
                             (if native-timeout-p
                                 (funcall upgrader stream (parse-http-uri "https://127.0.0.1/")
                                          :timeout 0.02)
                                 (funcall upgrader stream (parse-http-uri "https://127.0.0.1/")
                                          :deadline 1))))
                         'http-timeout :tls)))
                  (ensure-equal :tls (http-timeout-kind condition)))
                (ensure-equal 1 calls)
                (ensure-false (open-stream-p stream))
                (ensure-equal 1 (network-tls-contract-close-count stream))
                (ensure-true (network-tls-contract-abort-p stream))
                (ensure-equal 0 (length (binary-test-output stream)))
                (ensure-equal 0 (http-kit/test-core::binary-test-input-position stream))))
          (when (open-stream-p stream) (close stream :abort t))))))

  (deftest network-tls-contract-no-deadline-handshake
    (let ((stream (make-instance 'network-tls-contract-stream :input (octets))))
      (unwind-protect
           (progn
             (%network-tls-contract-protocol
              (lambda ()
                (funcall (make-http-tls-upgrader
                           :verify :optional
                           :trust-anchors (list t))
                         stream
                         (parse-http-uri "https://127.0.0.1/")))
              :tls :handshake-failed)
             (ensure-false (open-stream-p stream))
             (ensure-equal 2 (network-tls-contract-close-count stream))
             (ensure-true (network-tls-contract-abort-p stream))
             (ensure-true (plusp (length (binary-test-output stream)))))
        (when (open-stream-p stream) (close stream :abort t))))))
#+sbcl
(progn
  (deftest network-tls-contract-wake-listener
    (%network-tls-contract-protocol
     (lambda () (wake-http-tcp-listener :not-listener)) :wake :not-listener)
    (let ((listener (open-http-tcp-listener)))
      (unwind-protect
           (progn
             (ensure-equal nil (close-http-tcp-listener listener))
             (ensure-equal nil (wake-http-tcp-listener listener)))
        (close-http-tcp-listener listener)))
    (dolist (ipv6-p '(nil t))
      (let ((listener (open-http-tcp-listener :ipv6-p ipv6-p))
            (accepted nil))
        (unwind-protect
             (progn
               (ensure-equal (if ipv6-p :ipv6 :ipv4)
                             (http-network-listener-address-family listener))
               (ensure-equal
                (if ipv6-p
                    "0000:0000:0000:0000:0000:0000:0000:0000"
                    "0.0.0.0")
                (http-network-listener-address listener))
               (ensure-equal nil (wake-http-tcp-listener listener))
               (multiple-value-bind (stream address port)
                   (accept-http-tcp-stream listener :timeout 2)
                 (setf accepted stream)
                 (ensure-true (streamp stream))
                 (ensure-true
                  (if ipv6-p (find #\: address)
                      (string= "127.0.0.1" address)))
                 (ensure-true (plusp port))))
          (when accepted (close accepted :abort t))
          (close-http-tcp-listener listener))))))

#+sbcl
(progn
  (defun %network-tls-contract-with-spies (bindings thunk)
    (if bindings
        (%network-tls-contract-with-spy
         (caar bindings) (cadar bindings)
         (lambda () (%network-tls-contract-with-spies (cdr bindings) thunk)))
        (funcall thunk)))

  (deftest network-tls-contract-endpoint-routing-and-noninteger-port
    (dolist (case '(("http://127.0.0.1/" nil 80)
                    ("https://127.0.0.1/" nil 443)
                    ("https://127.0.0.1:8443/" (:mode :direct) 8443)
                    ("http://127.0.0.1/"
                     (:mode :forward :connect-host "127.0.0.1" :connect-port 8080) 8080)
                    ("https://127.0.0.1/"
                     (:mode :connect :proxy-host "127.0.0.1" :proxy-port 8081) 8081)
                    ("http://127.0.0.1/"
                     (:mode :socks5 :proxy-host "127.0.0.1" :proxy-port 1080) 1080)))
      (destructuring-bind (uri plan port) case
        (let ((calls 0)
              (expected (make-condition 'http-timeout :message "Connect boundary expired."
                                        :operation :connect :kind :connect)))
          (%network-tls-contract-with-spy
           'http-kit/network::%network-open-address
           (lambda (original)
             (declare (ignore original))
             (lambda (entry actual-port deadline clock)
               (incf calls)
               (ensure-equal 2 (first entry))
               (ensure-octets-equal #(127 0 0 1) (second entry))
               (ensure-equal port actual-port)
               (ensure-equal 12 deadline)
               (ensure-equal 10 (funcall clock))
               (error expected)))
           (lambda ()
             (ensure-true
              (eq expected
                  (%network-tls-contract-condition
                   (lambda ()
                     (open-http-tcp-stream (make-http-request :method "GET" :uri uri)
                                           :proxy-plan plan :timeout 2
                                           :clock-function (lambda () 10)))
                   'http-timeout :connect)))
             (ensure-equal 1 calls))))))
    (dolist (port '("80" 1/2 nil))
      (let* ((plan (list :mode :forward :connect-host "127.0.0.1" :connect-port port))
             (condition (%network-tls-contract-condition
                         (lambda ()
                           (open-http-tcp-stream
                            (make-http-request :method "GET" :uri "http://127.0.0.1/")
                            :proxy-plan plan))
                         'http-connection-error :endpoint)))
        (ensure-true (eq plan (http-connection-error-cause condition))))))

  (deftest network-tls-contract-empty-resolver-and-atomic-options
    (%network-tls-contract-with-spy
     'sb-bsd-sockets:get-host-by-name
     (lambda (original)
       (declare (ignore original))
       (lambda (host) (ensure-equal "127.0.0.1" host) (values nil nil)))
     (lambda ()
       (dolist (case (list (list :resolve (lambda () (http-network-resolve-host "127.0.0.1")))
                          (list :listen-resolve
                                (lambda () (open-http-tcp-listener :host "127.0.0.1")))
                          (list :connect
                                (lambda ()
                                  (open-http-tcp-stream
                                   (make-http-request :method "GET" :uri "http://127.0.0.1/"))))))
         (ensure-equal nil
                       (http-connection-error-cause
                        (%network-tls-contract-condition
                         (second case) 'http-connection-error (first case)))))))
    (%network-tls-contract-with-spy
     'sb-bsd-sockets:host-ent-address-type
     (lambda (original)
       (declare (ignore original))
       (lambda (entry) (ensure-true entry) 999))
     (lambda ()
       (ensure-equal nil
                     (http-connection-error-cause
                      (%network-tls-contract-condition
                       (lambda () (http-network-resolve-host "127.0.0.1"))
                       'http-connection-error :resolve)))))
    (let ((listener (open-http-tcp-listener :host "127.0.0.1")))
      (unwind-protect
           (%network-tls-contract-protocol
            (lambda () (serve-http1-listener listener #'identity :session-options :atom))
            :serve '(:session-options :atom))
        (close-http-tcp-listener listener))))

  (deftest network-tls-contract-address-retry-condition-identity
    (let* ((entries (list (list 2 #(127 0 0 1)) (list 2 #(127 0 0 2))))
           (first-error (make-condition 'http-protocol-error :message "First address rejected."
                                        :operation :connect :detail :first))
           (last-error (make-condition 'simple-error :format-control "Last address rejected."))
           (timeout (make-condition 'http-timeout :message "First address expired."
                                    :operation :connect :kind :connect)))
      (dolist (timeout-p '(nil t))
        (let ((attempts nil))
          (%network-tls-contract-with-spies
           (list
            (list 'http-kit/network::%network-resolve-entries
                  (lambda (original)
                    (declare (ignore original))
                    (lambda (host deadline clock)
                      (declare (ignore deadline clock))
                      (ensure-equal "127.0.0.1" host) entries)))
            (list 'http-kit/network::%network-open-address
                  (lambda (original)
                    (declare (ignore original))
                    (lambda (entry port deadline clock)
                      (declare (ignore deadline clock))
                      (ensure-equal 80 port)
                      (push entry attempts)
                      (error (if timeout-p timeout
                                 (if (eq entry (first entries)) first-error last-error)))))))
           (lambda ()
             (let ((condition (%network-tls-contract-condition
                               (lambda ()
                                 (open-http-tcp-stream
                                  (make-http-request :method "GET" :uri "http://127.0.0.1/")))
                               (if timeout-p 'http-timeout 'http-connection-error) :connect)))
               (if timeout-p
                   (ensure-true (eq timeout condition))
                   (ensure-true (eq last-error (http-connection-error-cause condition)))))
             (ensure-equal (if timeout-p (list (first entries)) entries)
                           (reverse attempts))))))))

  (deftest network-tls-contract-listener-family-preference-fallback
    (let ((v4 (list 2 #(127 0 0 1)))
          (v6 (list http-kit/network::+network-ipv6-address-type+
                    #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))))
      (dolist (case '(("127.0.0.1" t t :ipv6)
                      ("::1" nil t :ipv6)
                      ("::1" nil nil :ipv4)))
        (destructuring-bind (host ipv6-p include-v6-p family) case
          (let ((listener nil) (peer nil) (accepted nil))
            (unwind-protect
                 (progn
                   (%network-tls-contract-with-spy
                    'http-kit/network::%network-resolve-entries
                    (lambda (original)
                      (declare (ignore original))
                      (lambda (name deadline clock)
                        (declare (ignore deadline clock))
                        (ensure-equal host name)
                        (if include-v6-p (list v4 v6) (list v4))))
                    (lambda ()
                      (setf listener (open-http-tcp-listener :host host :ipv6-p ipv6-p))))
                   (ensure-equal family (http-network-listener-address-family listener))
                   (setf peer (open-http-tcp-stream
                               (make-http-request
                                :method "GET" :uri
                                (format nil "http://~A:~D/"
                                        (if (eq family :ipv6) "[::1]" "127.0.0.1")
                                        (http-network-listener-port listener))) :timeout 2)
                         accepted (accept-http-tcp-stream listener :timeout 2))
                   (write-byte 31 peer)
                   (finish-output peer)
                   (ensure-equal 31 (sb-ext:with-timeout 2 (read-byte accepted))))
              (close-http-tcp-stream accepted)
              (close-http-tcp-stream peer)
              (when listener (close-http-tcp-listener listener))))))))

  (deftest network-tls-contract-accept-stream-and-socket-unwind
    (dolist (failure-point '(:make-stream :peer-name))
      (let ((listener (open-http-tcp-listener :host "127.0.0.1"))
            (peer nil) (accepted nil) (socket-closes 0) (made 0)
            (expected (make-condition 'simple-error :format-control "Accept boundary rejected.")))
        (unwind-protect
             (progn
               (setf peer (%network-tls-contract-peer listener))
               (%network-tls-contract-with-spies
                (list
                 (list 'sb-bsd-sockets:socket-make-stream
                       (lambda (original)
                         (lambda (socket &rest args)
                           (incf made)
                           (when (eq failure-point :make-stream) (error expected))
                           (setf accepted (apply original socket args)))))
                 (list 'sb-bsd-sockets:socket-peername
                       (lambda (original)
                         (declare (ignore original))
                         (lambda (socket) (declare (ignore socket)) (error expected))))
                 (list 'sb-bsd-sockets:socket-close
                       (lambda (original)
                         (lambda (socket)
                           (incf socket-closes)
                           (when accepted (ensure-false (open-stream-p accepted)))
                           (funcall original socket)))))
                (lambda ()
                  (ensure-true
                   (eq expected (handler-case (accept-http-tcp-stream listener :timeout 2)
                                  (error (condition) condition))))
                  (ensure-equal 1 made)
                  (ensure-equal 1 socket-closes)
                  (ensure-equal nil (sb-ext:with-timeout 2 (read-byte peer nil nil)))))
               (when accepted (ensure-false (open-stream-p accepted)))
               (close-http-tcp-stream peer)
               (setf peer (%network-tls-contract-peer listener)
                     accepted (accept-http-tcp-stream listener :timeout 2))
               (ensure-true (open-stream-p accepted)))
          (close-http-tcp-stream accepted)
          (close-http-tcp-stream peer)
          (close-http-tcp-listener listener)))))

  (deftest network-tls-contract-native-connect-timeout-cleanup
    (let ((connects 0) (closes 0) (observed-socket nil))
      (%network-tls-contract-with-spies
       (list
        (list 'sb-bsd-sockets:socket-connect
              (lambda (original)
                (declare (ignore original))
                (lambda (socket &rest address)
                  (declare (ignore address))
                  (incf connects) (setf observed-socket socket)
                  (error 'sb-ext:timeout))))
        (list 'sb-bsd-sockets:socket-close
              (lambda (original)
                (lambda (socket)
                  (ensure-true (eq observed-socket socket))
                  (incf closes)
                  (funcall original socket)))))
       (lambda ()
         (let ((condition (%network-tls-contract-condition
                           (lambda ()
                             (open-http-tcp-stream
                              (make-http-request :method "GET" :uri "http://127.0.0.1/")
                              :timeout 2))
                           'http-timeout :connect)))
           (ensure-equal :connect (http-timeout-kind condition)))
         (ensure-equal 1 connects)
         (ensure-equal 1 closes)
         (ensure-equal -1 (sb-bsd-sockets:socket-file-descriptor observed-socket))))))

  (deftest network-tls-contract-connect-stream-ownership
    (dolist (outcome '(:error :throw :success))
      (let ((listener (open-http-tcp-listener :host "127.0.0.1"))
            (peer nil) (stream nil) (observed-socket nil)
            (made 0) (closes 0)
            (exit-value (list :stream-setup-exit))
            (expected (make-condition 'simple-error
                                      :format-control "Stream setup rejected.")))
        (unwind-protect
             (progn
               (%network-tls-contract-with-spies
                (list
                 (list 'sb-bsd-sockets:socket-make-stream
                       (lambda (original)
                         (lambda (socket &rest args)
                           (incf made)
                           (setf observed-socket socket)
                           (ecase outcome
                             (:error (error expected))
                             (:throw (throw 'stream-setup-exit exit-value))
                             (:success (apply original socket args))))))
                 (list 'sb-bsd-sockets:socket-close
                       (lambda (original)
                         (lambda (socket)
                           (ensure-true (eq observed-socket socket))
                           (incf closes)
                           (funcall original socket)
                           (error "Secondary socket cleanup failure.")))))
                (lambda ()
                  (ecase outcome
                    (:error
                     (let ((condition
                             (%network-tls-contract-condition
                              (lambda () (%network-tls-contract-peer listener))
                              'http-connection-error :connect)))
                       (ensure-true
                        (eq expected (http-connection-error-cause condition)))))
                    (:throw
                     (ensure-true
                      (eq exit-value
                          (catch 'stream-setup-exit
                            (%network-tls-contract-peer listener)
                            :returned))))
                    (:success
                     (setf stream (%network-tls-contract-peer listener))
                     (ensure-true (open-stream-p stream))))
                  (ensure-equal 1 made)
                  (ensure-equal (if (eq outcome :success) 0 1) closes)))
               (setf peer (accept-http-tcp-stream listener :timeout 2))
               (if (eq outcome :success)
                   (progn
                     (write-byte 71 stream)
                     (finish-output stream)
                     (ensure-equal 71 (sb-ext:with-timeout 2 (read-byte peer)))
                     (close-http-tcp-stream stream))
                   (ensure-equal
                    -1 (sb-bsd-sockets:socket-file-descriptor observed-socket)))
               (ensure-equal :eof
                             (sb-ext:with-timeout 2 (read-byte peer nil :eof))))
          (close-http-tcp-stream stream)
          (close-http-tcp-stream peer)
          (close-http-tcp-listener listener)))))

  (deftest network-tls-contract-proxy-stream-ownership
    (dolist (outcome '(:error :http-error :throw :success))
      (let* ((listener (open-http-tcp-listener :host "127.0.0.1"))
             (request
               (make-http-request
                :method "GET"
                :uri (format nil "https://127.0.0.1:~D/"
                             (http-network-listener-port listener))))
             (raw nil) (peer nil) (opened 0) (upgraded 0) (closes 0)
             (exit-value (list :proxy-setup-exit))
             (expected
               (if (eq outcome :http-error)
                   (make-condition 'http-timeout :message "Upgrade deadline."
                                   :operation :tls :kind :connect)
                   (make-condition 'simple-error
                                   :format-control "Upgrade rejected.")))
             (opener
               (make-http-proxy-stream-opener
                (lambda (request &key timeout deadline)
                  (incf opened)
                  (setf raw (open-http-tcp-stream request
                                                  :timeout timeout :deadline deadline)))
                (lambda (stream)
                  (ensure-true (eq raw stream))
                  (incf closes)
                  (close stream :abort t)
                  (error "Secondary proxy cleanup failure."))
                :tls-upgrade
                (lambda (stream target &key timeout deadline)
                  (ensure-true (eq raw stream))
                  (ensure-equal (http-request-uri request) target)
                  (ensure-equal 2 timeout)
                  (ensure-equal nil deadline)
                  (incf upgraded)
                  (setf peer (accept-http-tcp-stream listener :timeout 2))
                  (ecase outcome
                    ((:error :http-error) (error expected))
                    (:throw (throw 'proxy-setup-exit exit-value))
                    (:success stream))))))
        (unwind-protect
             (progn
               (ecase outcome
                 (:error
                  (let ((condition
                          (%network-tls-contract-condition
                           (lambda () (funcall opener request :timeout 2))
                           'http-proxy-error :proxy)))
                    (ensure-equal
                     expected (getf (http-client-error-detail condition) :cause))))
                 (:http-error
                  (ensure-true
                   (eq expected
                       (%network-tls-contract-condition
                        (lambda () (funcall opener request :timeout 2))
                        'http-timeout :tls))))
                 (:throw
                  (ensure-true
                   (eq exit-value
                       (catch 'proxy-setup-exit
                         (funcall opener request :timeout 2)
                         :returned))))
                 (:success
                  (let ((returned (funcall opener request :timeout 2)))
                    (ensure-true (eq raw returned)))
                  (ensure-true (open-stream-p raw))
                  (write-byte 73 raw)
                  (finish-output raw)
                  (ensure-equal 73 (sb-ext:with-timeout 2 (read-byte peer)))))
               (ensure-equal 1 opened)
               (ensure-equal 1 upgraded)
               (ensure-equal (if (eq outcome :success) 0 1) closes)
               (if (eq outcome :success)
                   (close-http-tcp-stream raw)
                   (ensure-false (open-stream-p raw)))
               (ensure-equal :eof
                             (sb-ext:with-timeout 2 (read-byte peer nil :eof))))
          (close-http-tcp-stream raw)
          (close-http-tcp-stream peer)
          (close-http-tcp-listener listener)))))

  (deftest network-tls-contract-trust-store-without-certificates
    (let ((raw (make-instance 'network-tls-contract-stream :input (octets)))
          (reads 0))
      (unwind-protect
           (%network-tls-contract-with-spies
            (list
             (list 'cl-tls-kit:default-trust-store-path
                   (lambda (original)
                     (declare (ignore original)) (lambda () "empty-trust-store.pem")))
             (list 'cl-tls-kit:load-trust-store
                   (lambda (original)
                     (declare (ignore original))
                     (lambda (&key ca-file)
                       (ensure-equal "empty-trust-store.pem" ca-file) ca-file)))
             (list 'http-kit/tls::%tls-read-text-file
                   (lambda (original)
                     (declare (ignore original))
                     (lambda (path)
                       (ensure-equal "empty-trust-store.pem" path) (incf reads) ""))))
            (lambda ()
              (%network-tls-contract-protocol
               (lambda ()
                 (funcall (make-http-tls-upgrader :verify :required) raw
                          (parse-http-uri "https://127.0.0.1/")))
               :tls :invalid-trust-store)
              (ensure-equal 1 reads)
              (ensure-equal 0 (length (binary-test-output raw)))
              (ensure-equal 0 (http-kit/test-core::binary-test-input-position raw))
              (ensure-false (open-stream-p raw))
              (ensure-equal 1 (network-tls-contract-close-count raw))
              (ensure-true (network-tls-contract-abort-p raw))))
        (when (open-stream-p raw) (close raw :abort t)))))

  (deftest network-tls-contract-key-exchange-groups-and-zero-secret
    (let* ((callbacks (http-kit/tls::%tls-key-exchange))
           (generate (getf callbacks :generate))
           (shared-secret (getf callbacks :shared-secret)))
      (dolist (group '(29 23))
        (multiple-value-bind (private-a public-a) (funcall generate group)
          (multiple-value-bind (private-b public-b) (funcall generate group)
            (let ((secret-a (funcall shared-secret group private-a public-b))
                  (secret-b (funcall shared-secret group private-b public-a)))
              (ensure-equal 32 (length secret-a))
              (ensure-octets-equal secret-a secret-b)
              (ensure-true (some #'plusp secret-a))))))
      (multiple-value-bind (private public) (funcall generate 29)
        (declare (ignore public))
        (%network-tls-contract-protocol
         (lambda () (funcall shared-secret 29 private
                             (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
         :tls :invalid-shared-secret))
      (dolist (thunk (list (lambda () (funcall generate 65535))
                          (lambda () (funcall shared-secret 65535 nil nil))))
        (let ((condition (%network-tls-contract-condition thunk 'http-unsupported-feature :tls)))
          (ensure-equal :tls-key-exchange (http-unsupported-feature-name condition))
          (ensure-equal '(:group 65535) (http-protocol-error-detail condition)))))))

#+sbcl
(progn
  (defclass network-tls-contract-ordered-stream (network-tls-contract-stream)
    ((events :initarg :events :reader network-tls-contract-events)
     (close-error :initarg :close-error :initform nil :reader network-tls-contract-close-error)))

  (defmethod close ((stream network-tls-contract-ordered-stream) &key abort)
    (vector-push-extend (list :raw-close abort) (network-tls-contract-events stream))
    (call-next-method)
    (when (network-tls-contract-close-error stream)
      (error (network-tls-contract-close-error stream)))
    t)

  (defun %network-tls-contract-record-stream (records &key write-error close-error)
    ;; Connected record-layer fixtures use genuine AEAD states, not a mocked handshake.
    (let* ((provider (cl-tls-kit:make-cl-crypto-kit-provider))
           (secret (make-array 32 :element-type '(unsigned-byte 8) :initial-element 7))
           (events (make-array 0 :adjustable t :fill-pointer 0))
           (written (make-array 0 :adjustable t :fill-pointer 0))
           (raw (make-instance 'network-tls-contract-ordered-stream :input (octets)
                               :events events :close-error close-error)))
      (flet ((state () (cl-tls-kit:make-tls13-traffic-state
                       provider :sha256 :aes-128-gcm secret 16 12)))
        (let* ((peer-write (state))
               (peer-read (state))
               (incoming
                 (mapcar (lambda (record)
                           (cl-tls-kit:encrypt-tls13-traffic-record
                            (cl-tls-kit:make-tls-plaintext (first record) (second record)) peer-write))
                         records))
               (driver
                 (cl-tls-kit:make-tls13-client-driver
                  :provider provider
                  :transport-read (lambda (driver)
                                    (declare (ignore driver))
                                    (vector-push-extend :read events) (pop incoming))
                  :transport-write (lambda (driver wire)
                                     (declare (ignore driver))
                                     (vector-push-extend :write events)
                                     (when write-error (error write-error))
                                     (vector-push-extend wire written))
                  :transport-close (lambda (driver)
                                     (declare (ignore driver))
                                     (vector-push-extend :driver-close events)))))
          (setf (cl-tls-kit::tls13-client-driver-state driver) :connected
                (cl-tls-kit::tls13-client-driver-application-read-state driver) (state)
                (cl-tls-kit::tls13-client-driver-application-write-state driver) (state))
          (values (http-kit/tls::%make-http-tls-stream driver raw)
                  driver raw events written peer-read)))))

  (deftest network-tls-contract-empty-record-partial-eof-and-close-order
    (multiple-value-bind (stream driver raw events written peer-read)
        (%network-tls-contract-record-stream
         (list (list 23 (octets)) (list 23 (octets 7 8)) (list 21 (octets 1 0))))
      (unwind-protect
           (let ((buffer (octets 99 99 99 99 99 99)))
             (ensure-equal 3 (sb-gray:stream-read-sequence stream buffer 1 5))
             (ensure-octets-equal #(99 7 8 99 99 99) buffer)
             (ensure-equal :eof (read-byte stream nil :eof))
             (ensure-equal 2 (read-sequence buffer stream :start 2 :end 2))
             (ensure-equal :eof
                           (cl-tls-kit:tls13-client-driver-read-application-data
                            driver))
             (ensure-equal '(:read :read :read :write :driver-close) (coerce events 'list))
             (ensure-equal 1 (length written))
             (let ((plaintext (cl-tls-kit:decrypt-tls13-traffic-record (aref written 0) peer-read)))
               (ensure-equal 21 (cl-tls-kit:tls-plaintext-content-type plaintext))
               (ensure-octets-equal #(1 0) (cl-tls-kit:tls-plaintext-fragment plaintext)))
             (ensure-true (open-stream-p raw))
             (ensure-true (search "TLS stream" (princ-to-string stream)))
             (ensure-true (close stream))
             (ensure-false (open-stream-p stream))
             (ensure-false (open-stream-p raw))
             (ensure-equal 1 (network-tls-contract-close-count raw))
             (ensure-equal '(:read :read :read :write :driver-close (:raw-close nil))
                           (coerce events 'list))
             (ensure-true (close stream))
             (ensure-equal 6 (length events)))
        (when (open-stream-p stream) (close stream :abort t)))))

  (deftest network-tls-contract-write-ranges-flush-and-normal-close
    (multiple-value-bind (stream driver raw events written peer-read)
        (%network-tls-contract-record-stream nil)
      (unwind-protect
           (let ((bytes (octets 10 11 12 13)))
             (ensure-equal 9 (write-byte 9 stream))
             (ensure-true (eq bytes (write-sequence bytes stream :start 1 :end 3)))
             (ensure-true (eq bytes (write-sequence bytes stream :start 2 :end 2)))
             (ensure-equal nil (finish-output stream))
             (ensure-equal nil (force-output stream))
             (ensure-equal 2 (length written))
             (ensure-equal '(:write :write) (coerce events 'list))
             (loop for wire across written for fragment in (list (octets 9) (octets 11 12))
                   for plaintext = (cl-tls-kit:decrypt-tls13-traffic-record wire peer-read)
                   do (ensure-equal 23 (cl-tls-kit:tls-plaintext-content-type plaintext))
                      (ensure-octets-equal fragment (cl-tls-kit:tls-plaintext-fragment plaintext)))
             (ensure-true (close stream))
             (ensure-equal 3 (length written))
             (let ((alert (cl-tls-kit:decrypt-tls13-traffic-record (aref written 2) peer-read)))
               (ensure-equal 21 (cl-tls-kit:tls-plaintext-content-type alert))
               (ensure-octets-equal #(1 0) (cl-tls-kit:tls-plaintext-fragment alert)))
             (ensure-false (open-stream-p raw))
             (ensure-false (network-tls-contract-abort-p raw))
             (ensure-equal '(:write :write :write :driver-close (:raw-close nil))
                           (coerce events 'list))
             (ensure-true (close stream :abort t))
             (ensure-equal 1 (network-tls-contract-close-count raw)))
        (when (open-stream-p stream) (close stream :abort t)))))

  (deftest network-tls-contract-peer-alert-condition-and-cleanup
    (multiple-value-bind (stream driver raw events written peer-read)
        (%network-tls-contract-record-stream (list (list 21 (octets 2 40))))
      (declare (ignore peer-read))
      (unwind-protect
           (progn
             (%network-tls-contract-protocol (lambda () (read-byte stream))
                                             :tls '(:peer-alert 2 40))
             (ensure-equal '(:read :driver-close) (coerce events 'list))
             (ensure-equal 0 (length written))
             (ensure-true (close stream :abort t))
             (ensure-equal '(:read :driver-close (:raw-close t)) (coerce events 'list))
             (ensure-false (open-stream-p raw))
             (ensure-equal 1 (network-tls-contract-close-count raw)))
        (when (open-stream-p stream) (close stream :abort t)))))

  (deftest network-tls-contract-close-failure-identity-and-abort-order
    (dolist (case '(:abort :already-closed :raw-error :write-error :both-errors))
      (let ((write-error (make-condition 'simple-error :format-control "TLS write rejected."))
            (raw-error (make-condition 'simple-error :format-control "Raw close rejected.")))
        (multiple-value-bind (stream driver raw events written peer-read)
            (%network-tls-contract-record-stream
             nil :write-error (when (member case '(:write-error :both-errors)) write-error)
             :close-error (when (member case '(:raw-error :both-errors)) raw-error))
          (declare (ignore peer-read))
          (when (eq case :already-closed) (close raw :abort t))
          (let ((condition (handler-case (close stream :abort (eq case :abort))
                             (error (condition) condition))))
            (if (member case '(:write-error :both-errors))
                (ensure-true (eq write-error condition))
                (if (eq case :raw-error)
                    (ensure-true (eq raw-error condition))
                    (ensure-equal t condition))))
          (ensure-false (open-stream-p stream))
          (ensure-false (open-stream-p raw))
          (ensure-equal 1 (network-tls-contract-close-count raw))
          (ensure-equal (not (null (member case '(:abort :already-closed :write-error :both-errors))))
                        (network-tls-contract-abort-p raw))
          (ensure-equal (case case
                          (:abort '((:raw-close t)))
                          (:already-closed '((:raw-close t) :write :driver-close))
                          ((:write-error :both-errors) '(:write :driver-close (:raw-close t)))
                          (otherwise '(:write :driver-close (:raw-close nil))))
                        (coerce events 'list))
          (ensure-equal (if (member case '(:abort :write-error :both-errors)) 0 1)
                        (length written))
          (let ((event-count (length events)))
            (ensure-true (close stream))
            (ensure-equal event-count (length events))))))))

#+sbcl
(progn
  (deftest network-tls-contract-real-lifecycle-tcp-ownership
    (let ((listener (open-http-tcp-listener :host "127.0.0.1"))
          (peer nil)
          (accepted nil)
          (next-peer nil)
          (next-accepted nil)
          (events nil)
          (accept-count 0)
          (error-count 0))
      (unwind-protect
           (progn
             (setf peer (%network-tls-contract-peer listener))
             (sb-ext:with-timeout 2
               (write-sequence
                (ascii "GET /ownership HTTP/1.1|CRLF|Host: localhost|CRLF|Connection: close|CRLF||CRLF|")
                peer)
               (finish-output peer)
               (multiple-value-bind (count reason)
                   (serve-http1-listener
                    listener
                    (lambda (request)
                      (ensure-equal '(:accept) events)
                      (ensure-true (open-stream-p accepted))
                      (ensure-equal "GET" (http-request-method request))
                      (ensure-equal "/ownership"
                                    (http-uri-path (http-request-uri request)))
                      (setf events (append events '(:handler)))
                      (make-http-response
                       :status 200
                       :headers (list (make-http-header "connection" "close"))
                       :body (ascii "ownership-ok")))
                    :max-connections 1 :accept-timeout 2
                    :on-accept
                    (lambda (stream address port)
                      (setf accepted stream)
                      (incf accept-count)
                      (ensure-true (open-stream-p stream))
                      (ensure-equal "127.0.0.1" address)
                      (ensure-true (and (integerp port) (< 0 port 65536)))
                      (setf events (append events '(:accept))))
                    :on-error
                    (lambda (condition address port)
                      (declare (ignore address port))
                      (incf error-count)
                      (error condition)))
                 (ensure-equal 1 count)
                 (ensure-equal :max-connections reason)))
             (ensure-equal 1 accept-count)
             (ensure-equal 0 error-count)
             (ensure-equal '(:accept :handler) events)
             (ensure-false (open-stream-p accepted))
             (ensure-true (open-stream-p peer))
             (sb-ext:with-timeout 2
               (let ((response (parse-http-response peer :timeout 2)))
                 (ensure-equal 200 (http-response-status response))
                 (ensure-equal "ownership-ok"
                               (octets-as-string (http-response-body response))))
               (ensure-equal :eof (read-byte peer nil :eof)))
             (ensure-equal nil (close-http-tcp-stream peer))
             ;; A new real accept establishes listener ownership, not a private flag.
             (setf next-peer (%network-tls-contract-peer listener))
             (sb-ext:with-timeout 2
               (multiple-value-bind (stream address port)
                   (accept-http-tcp-stream listener :timeout 2)
                 (setf next-accepted stream)
                 (ensure-true (open-stream-p stream))
                 (ensure-equal "127.0.0.1" address)
                 (ensure-true (and (integerp port) (< 0 port 65536))))))
        (dolist (stream (list accepted peer next-accepted next-peer))
          (when stream (close-http-tcp-stream stream)))
        (close-http-tcp-listener listener))
      (dolist (stream (list accepted peer next-accepted next-peer))
        (ensure-true (streamp stream))
        (ensure-false (open-stream-p stream)))
      (ensure-equal nil (close-http-tcp-listener listener))))

  (deftest network-tls-contract-real-lifecycle-tls-wrapper
    (let* ((directory
             (uiop:ensure-directory-pathname
              (string-trim '(#\Return #\Linefeed)
                           (uiop:run-program '("mktemp" "-d")
                                             :output :string :error-output :string))))
           (certificate (namestring (merge-pathnames "server.pem" directory)))
           (key (namestring (merge-pathnames "server.key" directory)))
           (openssl (or (uiop:getenv "OPENSSL") "openssl"))
           (listener nil)
           (port nil)
           (process nil)
           (pipes nil)
           (rejected-raw nil)
           (raw nil)
           (wrapper nil)
           (live-raw nil)
           (live-wrapper nil)
           (events nil)
           (close-count 0))
      (unwind-protect
           (progn
             (sb-ext:with-timeout 5
               (uiop:run-program
                (list openssl "req" "-x509" "-newkey" "rsa:2048" "-nodes"
                      "-days" "1" "-subj" "/CN=127.0.0.1"
                      "-addext" "subjectAltName=IP:127.0.0.1"
                      "-keyout" key "-out" certificate)
                :output :string :error-output :string))
             (setf listener (open-http-tcp-listener :host "127.0.0.1")
                   port (http-network-listener-port listener))
             (close-http-tcp-listener listener)
             (setf process
                   (uiop:launch-program
                    (list openssl "s_server" "-quiet" "-www"
                          "-accept" (princ-to-string port)
                          "-cert" certificate "-key" key "-tls1_3"
                          "-alpn" "http/1.1")
                    :input :stream :output :stream :error-output :stream
                    :element-type '(unsigned-byte 8) :wait nil)
                   pipes (list (uiop:process-info-input process)
                               (uiop:process-info-output process)
                               (uiop:process-info-error-output process)))
             (ensure-true (%e2e-wait-for-tcp "127.0.0.1" port :timeout 5))
             (let* ((anchor
                      (cl-tls-kit.x509:parse-certificate-der
                       (cl-tls-kit:pem-block-der
                        (first (cl-tls-kit:pem-decode
                                (uiop:read-file-string certificate))))))
                    (upgrade (make-http-tls-upgrader
                              :verify :required :trust-anchors (list anchor)
                              :alpn-protocols '("http/1.1")))
                    (opener (make-http-network-stream-opener)))
               (setf rejected-raw
                     (open-http-tcp-stream
                      (make-http-request :method "GET"
                                         :uri (format nil "http://127.0.0.1:~D/" port))
                      :timeout 5))
               (let ((condition
                       (sb-ext:with-timeout 5
                         (handler-case
                             (funcall upgrade rejected-raw
                                      (parse-http-uri
                                       (format nil "https://localhost:~D/" port))
                                      :timeout 5)
                           (http-protocol-error (condition) condition)))))
                 (ensure-true (typep condition 'http-protocol-error))
                 (ensure-equal :certificate-verification-failed
                               (first (http-protocol-error-detail condition)))
                 (ensure-true
                  (typep (getf (rest (http-protocol-error-detail condition)) :condition)
                         'cl-tls-kit:certificate-hostname-mismatch))
                 (ensure-false (open-stream-p rejected-raw)))
               (let ((client
                       (make-http-client
                        :automatic-decompression-p nil
                        :strict-transport-store nil :alternative-service-store nil
                        :open-stream
                        (lambda (request &rest options &key &allow-other-keys)
                          (setf raw (apply opener request options)))
                        :tls-upgrade
                        (lambda (stream uri &rest options &key &allow-other-keys)
                          (ensure-true (eq raw stream))
                          (ensure-true (open-stream-p raw))
                          (setf wrapper (apply upgrade stream uri options))
                          (ensure-true (streamp wrapper))
                          (ensure-false (eq raw wrapper))
                          (ensure-true (open-stream-p wrapper))
                          (ensure-true (open-stream-p raw))
                          (ensure-equal "http/1.1"
                                        (http-tls-selected-alpn-protocol wrapper))
                          (setf events (append events '(:upgrade)))
                          wrapper)
                        :close-stream
                        (lambda (stream)
                          (incf close-count)
                          (ensure-true (eq wrapper stream))
                          (ensure-equal '(:upgrade) events)
                          (ensure-true (open-stream-p wrapper))
                          ;; The peer's close_notify has already closed the transport.
                          (ensure-false (open-stream-p raw))
                          (setf events (append events '(:close)))
                          (ensure-true (close stream))
                          (ensure-false (open-stream-p wrapper))
                          (ensure-false (open-stream-p raw))
                          (ensure-true (close stream))
                          (setf events (append events '(:closed)))))))
                 (sb-ext:with-timeout 5
                   (multiple-value-bind (response request)
                       (http-client-send client "GET"
                                         (format nil "https://127.0.0.1:~D/" port)
                                         :timeout 5)
                     (ensure-equal "GET" (http-request-method request))
                     (ensure-equal 200 (http-response-status response))
                     (ensure-true
                      (search "s_server" (octets-as-string (http-response-body response))))))
                 (ensure-equal 1 close-count)
                 (ensure-equal '(:upgrade :close :closed) events)
                 (ensure-false (open-stream-p wrapper))
                 (ensure-false (open-stream-p raw)))
               ;; Close a verified live connection before the peer ends its response.
               (sb-ext:with-timeout 5
                 (setf live-raw
                       (open-http-tcp-stream
                        (make-http-request :method "GET"
                                           :uri (format nil "http://127.0.0.1:~D/" port))
                        :timeout 5)
                       live-wrapper
                       (funcall upgrade live-raw
                                (parse-http-uri
                                 (format nil "https://127.0.0.1:~D/" port))
                                :timeout 5))
                 (ensure-true (streamp live-wrapper))
                 (ensure-false (eq live-wrapper live-raw))
                 (ensure-true (open-stream-p live-wrapper))
                 (ensure-true (open-stream-p live-raw))
                 (ensure-equal "http/1.1"
                               (http-tls-selected-alpn-protocol live-wrapper))
                 (ensure-true (close live-wrapper))
                 (ensure-false (open-stream-p live-wrapper))
                 (ensure-false (open-stream-p live-raw))
                 (ensure-true (close live-wrapper)))))
        (dolist (stream (list live-wrapper live-raw wrapper raw rejected-raw))
          (when stream (close stream :abort t)))
        (when listener (close-http-tcp-listener listener))
        (when process
          (uiop:terminate-process process :urgent t)
          (sb-ext:with-timeout 5 (uiop:wait-process process)))
        (dolist (pipe pipes) (close pipe :abort t))
        (dolist (path (list certificate key))
          (when (probe-file path) (delete-file path)))
        (uiop:delete-empty-directory directory))
      (dolist (stream (list live-wrapper live-raw wrapper raw rejected-raw))
        (ensure-true (streamp stream))
        (ensure-false (open-stream-p stream)))
      (ensure-false (uiop:process-alive-p process))
      (ensure-true (integerp (uiop:wait-process process)))
      (dolist (pipe pipes) (ensure-false (open-stream-p pipe)))
      (ensure-false (probe-file directory)))))

#+sbcl
(deftest network-tls-contract-real-lifecycle-session-options
  (dolist (callback-p '(nil t))
    (let ((listener (open-http-tcp-listener :host "127.0.0.1"))
          (peer nil)
          (accepted nil)
          (events nil)
          (close-count 0)
          (error-count 0))
      (unwind-protect
           (progn
             (setf peer (%network-tls-contract-peer listener))
             (sb-ext:with-timeout 2
               (write-sequence
                (ascii "GET /session-options HTTP/1.1|CRLF|Host: localhost|CRLF||CRLF|")
                peer)
               (finish-output peer)
               (multiple-value-bind (count reason)
                   (serve-http1-listener
                    listener
                    (lambda (request)
                      (ensure-equal '(:accept) events)
                      (ensure-true (open-stream-p accepted))
                      (ensure-equal "GET" (http-request-method request))
                      (ensure-equal "/session-options"
                                    (http-uri-path (http-request-uri request)))
                      (setf events (append events '(:handler)))
                      (make-http-response :status 200 :body (ascii "retained-or-closed")))
                    :max-connections 1 :accept-timeout 2
                    :session-options
                    (list :max-requests 1 :timeout 2
                          :close-stream
                          (when callback-p
                            (lambda (stream)
                              (incf close-count)
                              (ensure-true (eq accepted stream))
                              (ensure-equal '(:accept :handler) events)
                              (ensure-true (open-stream-p stream))
                              (setf events (append events '(:close)))
                              (ensure-equal nil (close-http-tcp-stream stream))
                              (ensure-false (open-stream-p stream)))))
                    :on-accept
                    (lambda (stream address port)
                      (setf accepted stream)
                      (ensure-true (open-stream-p stream))
                      (ensure-equal "127.0.0.1" address)
                      (ensure-true (and (integerp port) (< 0 port 65536)))
                      (setf events (append events '(:accept))))
                    :on-error
                    (lambda (condition address port)
                      (declare (ignore address port))
                      (incf error-count)
                      (error condition)))
                 (ensure-equal 1 count)
                 (ensure-equal :max-connections reason)))
             (ensure-equal 0 error-count)
             (ensure-equal (if callback-p 1 0) close-count)
             (ensure-equal (if callback-p '(:accept :handler :close) '(:accept :handler))
                           events)
             (ensure-equal (not callback-p) (open-stream-p accepted))
             (sb-ext:with-timeout 2
               (let ((response (parse-http-response peer :timeout 2)))
                 (ensure-equal 200 (http-response-status response))
                 (ensure-equal "retained-or-closed"
                               (octets-as-string (http-response-body response))))
               (unless callback-p
                 ;; A retained stream must still deliver real bytes to its peer.
                 (write-byte 123 accepted)
                 (finish-output accepted)
                 (ensure-equal 123 (read-byte peer))
                 (ensure-equal nil (close-http-tcp-stream accepted)))
               (ensure-equal :eof (read-byte peer nil :eof))))
        (close-http-tcp-stream accepted)
        (close-http-tcp-stream peer)
        (close-http-tcp-listener listener))
      (dolist (stream (list accepted peer))
        (ensure-true (streamp stream))
        (ensure-false (open-stream-p stream)))
      (ensure-equal nil (close-http-tcp-listener listener)))))

#+sbcl
(progn
(deftest network-public-default-session-options-contract
  (let ((listener (open-http-tcp-listener :host "127.0.0.1"))
        (peer nil) (accepted nil) (events nil) (errors 0))
    (unwind-protect
         (progn
           (setf peer (%network-tls-contract-peer listener))
           (sb-ext:with-timeout 2
             (write-sequence
              (ascii "GET /default-session HTTP/1.1|CRLF|Host: localhost|CRLF|Connection: close|CRLF||CRLF|")
              peer)
             (finish-output peer)
             (multiple-value-bind (count reason)
                 (serve-http1-listener
                  listener
                  (lambda (request)
                    (ensure-equal '(:accept) events)
                    (ensure-true (open-stream-p accepted))
                    (ensure-equal "GET" (http-request-method request))
                    (ensure-equal "/default-session" (http-uri-path (http-request-uri request)))
                    (setf events (append events '(:handler)))
                    (make-http-response :status 200 :body (ascii "default-close")))
                  :max-connections 1 :accept-timeout 2
                  :on-accept
                  (lambda (stream address port)
                    (setf accepted stream)
                    (ensure-equal "127.0.0.1" address)
                    (ensure-true (< 0 port 65536))
                    (ensure-true (open-stream-p stream))
                    (setf events (append events '(:accept))))
                  :on-error (lambda (condition address port)
                              (declare (ignore address port))
                              (incf errors)
                              (error condition)))
               (ensure-equal 1 count)
               (ensure-equal :max-connections reason))
             (ensure-equal '(:accept :handler) events)
             (ensure-equal 0 errors)
             (ensure-false (open-stream-p accepted))
             (let ((response (parse-http-response peer :timeout 2)))
               (ensure-equal 200 (http-response-status response))
               (ensure-equal "default-close" (octets-as-string (http-response-body response))))
             (ensure-equal :eof (read-byte peer nil :eof)))
           (close-http-tcp-stream peer)
           (setf peer (%network-tls-contract-peer listener))
           (multiple-value-bind (stream address port)
               (accept-http-tcp-stream listener :timeout 2)
             (unwind-protect
                  (progn (ensure-true (open-stream-p stream))
                         (ensure-equal "127.0.0.1" address)
                         (ensure-true (< 0 port 65536)))
               (close-http-tcp-stream stream))))
      (close-http-tcp-stream accepted)
      (close-http-tcp-stream peer)
      (close-http-tcp-listener listener))
    (ensure-false (open-stream-p accepted))
    (ensure-false (open-stream-p peer))
    (ensure-equal nil (close-http-tcp-listener listener))))

(deftest network-tls-contract-missing-default-trust-store
  (%network-tls-contract-with-spy
   'cl-tls-kit:default-trust-store-path
   (lambda (original) (declare (ignore original)) (lambda () nil))
   (lambda ()
     (ensure-equal nil
                   (http-kit/tls::%tls-load-trust-anchors :optional nil))
     (let ((condition
             (%network-tls-contract-condition
              (lambda ()
                (http-kit/tls::%tls-load-trust-anchors :required nil))
              'http-unsupported-feature :tls)))
       (ensure-equal :tls-trust-store
                     (http-unsupported-feature-name condition))
       (ensure-equal :not-found
                     (http-protocol-error-detail condition))))))


(deftest network-tls-contract-server-cipher-and-alert-boundaries
  (dolist (mapping '((#x1301 :aes-128-gcm)
                     (#x1302 :aes-256-gcm)
                     (#x1303 :chacha20-poly1305)
                     (#x1304 :aes-128-ccm)))
    (ensure-equal (second mapping)
                  (http-kit/tls::%tls-server-cipher-algorithm
                   (first mapping))))
  (let ((state
          (http-kit/tls::%make-http-tls-server-state
           :driver nil :provider nil :transport-stream nil))
        (levels nil))
    (%network-tls-contract-with-spy
     'http-kit/tls::%tls-server-send-plaintext
     (lambda (original)
       (declare (ignore original))
       (lambda (&rest arguments)
         (push (second arguments) levels)))
     (lambda ()
       (setf (http-kit/tls::http-tls-server-state-handshake-complete-p state) t)
       (http-kit/tls::%tls-server-fatal-alert state 20)
       (setf (http-kit/tls::http-tls-server-state-handshake-complete-p state) nil
             (http-kit/tls::http-tls-server-state-write-state state) :write-state)
       (http-kit/tls::%tls-server-fatal-alert state 20)
       (setf (http-kit/tls::http-tls-server-state-write-state state) nil)
       (http-kit/tls::%tls-server-fatal-alert state 20)))
    (ensure-equal '(:initial :handshake :application) levels)))
)
#+sbcl
(progn
  (deftest network-tls-contract-server-event-output-boundaries
    (let* ((provider (cl-tls-kit:make-cl-crypto-kit-provider))
           (secret (make-array 32 :element-type '(unsigned-byte 8) :initial-element 7))
           (raw (make-instance 'network-tls-contract-stream :input (octets)))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider provider :transport-stream raw)))
      (ensure-true (search "TLS server state" (princ-to-string state)))
      (ensure-equal cl-tls-kit:+tls-content-type-application-data+
                    (http-kit/tls::%tls-server-content-type :application-data))
      (%network-tls-contract-protocol
       (lambda () (http-kit/tls::%tls-server-content-type :unsupported))
       :tls :unsupported-record-type)
      (%network-tls-contract-protocol
       (lambda () (http-kit/tls::%tls-server-cipher-algorithm #xffff))
       :tls :unsupported-cipher-suite)
      (%network-tls-contract-protocol
       (lambda () (http-kit/tls::%tls-server-traffic-state state secret))
       :tls :invalid-driver-state)
      (%network-tls-contract-protocol
       (lambda ()
         (http-kit/tls::%tls-server-send-plaintext
          state :renegotiation cl-tls-kit:+tls-content-type-application-data+ (octets 41)))
       :tls :invalid-driver-state)
      (setf (http-kit/tls::http-tls-server-state-write-state state) :not-a-traffic-state)
      (%network-tls-contract-protocol
       (lambda ()
         (http-kit/tls::%tls-server-send-plaintext
          state :application cl-tls-kit:+tls-content-type-application-data+ (octets 41)))
       :tls :record-protection-failed)
      (ensure-equal 0 (length (binary-test-output raw)))
      (setf (http-kit/tls::http-tls-server-state-write-state state) nil)
      (%network-tls-contract-protocol
       (lambda ()
         (http-kit/tls::%tls-server-handle-events
          state
          (list (list :type :cipher-suite :suite #x1301)
                (list :type :secret :direction :sideways :level :handshake
                      :secret secret))))
       :tls :invalid-driver-state)
      (ensure-equal #x1301 (http-kit/tls::http-tls-server-state-suite state))
      (ensure-equal :initial (http-kit/tls::http-tls-server-state-phase state))
      (ensure-false (http-kit/tls::http-tls-server-state-read-state state))
      (ensure-false (http-kit/tls::http-tls-server-state-write-state state))
      (ensure-equal 0 (length (binary-test-output raw)))
      (close raw :abort t)))

  (deftest network-tls-contract-server-record-stream-boundaries
    (let* ((raw (make-instance 'network-tls-contract-stream :input (octets)))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver :test-driver :provider nil :transport-stream raw))
           (hello (cl-tls-kit:make-tls13-client-hello
                   #x0303
                   (make-array 32 :element-type '(unsigned-byte 8)
                                  :initial-element 1)
                   (octets) #(#x1301) nil))
           (handshake-wire (cl-tls-kit:encode-handshake 1 hello))
           (handshake-length (length handshake-wire))
           (legacy-handshake-record
             (concatenate '(vector (unsigned-byte 8))
                          (vector 22 3 1
                                  (ldb (byte 8 8) handshake-length)
                                  (ldb (byte 8 0) handshake-length))
                          handshake-wire)))
      (%network-tls-contract-with-spy
       'cl-tls-kit:tls13-server-driver-step
       (lambda (original)
         (declare (ignore original))
         (lambda (driver wire &key level)
           (ensure-equal :test-driver driver)
           (ensure-octets-equal handshake-wire wire)
           (ensure-equal :initial level)
           (values driver nil)))
       (lambda ()
         (ensure-equal nil
                       (http-kit/tls::%tls-server-process-record
                        state legacy-handshake-record))))
      (ensure-true
       (http-kit/tls::http-tls-server-state-client-hello-received-p state))
      (%network-tls-contract-protocol
       (lambda ()
         (http-kit/tls::%tls-server-process-record state (octets 99 3 1 0 1 1)))
       :tls :unexpected-record)
      (setf (http-kit/tls::http-tls-server-state-early-data-offered-p state) t)
      (%network-tls-contract-protocol
       (lambda ()
         (http-kit/tls::%tls-server-process-record state (octets 23 3 3 0 1 41)))
       :tls :early-data-rejected)
      (ensure-octets-equal #(21 3 3 0 2 2 10) (binary-test-output raw))
      (setf (fill-pointer (binary-test-output raw)) 0
            (http-kit/tls::http-tls-server-state-early-data-offered-p state) nil
            (http-kit/tls::http-tls-server-state-read-state state) :read-keys)
      (%network-tls-contract-protocol
       (lambda ()
         (http-kit/tls::%tls-server-process-record state (octets 22 3 3 0 0)))
       :tls :unexpected-record)
      (setf (http-kit/tls::http-tls-server-state-read-state state) nil)
      (%network-tls-contract-protocol
       (lambda ()
         (http-kit/tls::%tls-server-dispatch-plaintext
          state (cl-tls-kit:make-tls-plaintext
                 cl-tls-kit:+tls-content-type-alert+ (octets 1 0))))
       :tls :handshake-failed)
      (close raw :abort t))
    (let* ((raw (make-instance 'network-tls-contract-stream :input (octets)))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider nil :transport-stream raw)))
      (%network-tls-contract-protocol
       (lambda () (http-kit/tls::%tls-server-handshake state))
       :tls :handshake-failed)
      (close raw :abort t))
    (let* ((raw (make-instance 'network-tls-contract-stream :input (octets)))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider nil :transport-stream raw))
           (stream (http-kit/tls::%make-http-tls-stream state raw)))
      (%network-tls-contract-protocol
       (lambda () (read-byte stream))
       :tls :missing-close-notify)
      (ensure-true (open-stream-p stream))
      (ensure-true (open-stream-p raw))
      (ensure-equal 0 (length (binary-test-output raw)))
      (close stream :abort t))
    (let* ((raw (make-instance 'network-tls-contract-stream :input (octets)))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider nil :transport-stream raw)))
      (ensure-true (http-kit/tls::%tls-server-send-close-notify state))
      (ensure-false (http-kit/tls::http-tls-server-state-close-sent-p state))
      (ensure-equal 0 (length (binary-test-output raw)))
      (close raw :abort t))
    (let* ((raw (make-instance 'network-tls-contract-stream :input (octets)))
           (state (http-kit/tls::%make-http-tls-server-state
                   :driver nil :provider nil :transport-stream raw))
           (stream (http-kit/tls::%make-http-tls-stream state raw)))
      (%network-tls-contract-protocol
       (lambda () (write-byte 42 stream))
       :tls :record-encryption-failed)
      (ensure-true (open-stream-p stream))
      (ensure-true (open-stream-p raw))
      (ensure-equal 0 (length (binary-test-output raw)))
      (close stream :abort t))))

