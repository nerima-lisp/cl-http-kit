(in-package #:http-kit/test)

#+sbcl
(progn
  (defun %e2e-auth-openssl-digest (algorithm value)
    (let* ((openssl (or (uiop:getenv "OPENSSL") "openssl"))
           (output
             (uiop:run-program
              (list openssl "dgst"
                    (ecase algorithm
                      (:md5 "-md5")
                      (:sha-256 "-sha256"))
                    "-r")
              :input (make-string-input-stream value)
              :output :string
              :error-output :string))
           (fields (uiop:split-string
                    output
                    :separator '(#\Space #\Tab #\Return #\Linefeed))))
      (or (first fields)
          (error "OpenSSL returned no digest for ~S." value))))

  (defun %e2e-auth-digest-response (algorithm username realm password method uri
                                    nonce nc cnonce)
    (let* ((ha1 (%e2e-auth-openssl-digest
                 algorithm (format nil "~A:~A:~A" username realm password)))
           (ha2 (%e2e-auth-openssl-digest
                 algorithm (format nil "~A:~A" method uri))))
      (%e2e-auth-openssl-digest
       algorithm
       (format nil "~A:~A:~A:~A:auth:~A"
               ha1 nonce nc cnonce ha2))))

  (defun %e2e-auth-verify-digest (request authorization challenge username password
                                  previous-nc)
    (let* ((received (first (http-parse-authentication-challenges authorization)))
           (algorithm-name
             (http-authentication-challenge-parameter received "algorithm"))
           (algorithm
             (cond ((string-equal algorithm-name "MD5") :md5)
                   ((string-equal algorithm-name "SHA-256") :sha-256)
                   (t (error "Unexpected Digest algorithm ~S." algorithm-name))))
           (realm (http-authentication-challenge-parameter challenge "realm"))
           (nonce (http-authentication-challenge-parameter challenge "nonce"))
           (received-nonce
             (http-authentication-challenge-parameter received "nonce"))
           (uri (http-authentication-challenge-parameter received "uri"))
           (nc (http-authentication-challenge-parameter received "nc"))
           (nc-number (parse-integer nc :radix 16))
           (cnonce (http-authentication-challenge-parameter received "cnonce"))
           (qop (http-authentication-challenge-parameter received "qop"))
           (response (http-authentication-challenge-parameter received "response"))
           (expected
             (%e2e-auth-digest-response
              algorithm username realm password
              (http-request-method request) uri nonce nc cnonce)))
      (ensure-equal "Digest" (http-authentication-challenge-scheme received))
      (ensure-equal nonce received-nonce)
      (ensure-equal (http-request-target request) uri)
      (ensure-equal "auth" qop)
      (ensure-equal previous-nc (1- nc-number))
      (ensure-equal (format nil "~8,'0x" nc-number) nc)
      (ensure-equal expected response)
      nc-number))

  (defun %e2e-auth-run-server (listener challenge request-count-limit verifier body)
    (let ((request-count 0)
          (server-error nil)
          (thread nil))
      (setf thread
            (sb-thread:make-thread
             (lambda ()
               (handler-case
                   (multiple-value-bind (served-count termination)
                       (serve-http1-listener
                        listener
                        (lambda (request)
                          (incf request-count)
                          (if (oddp request-count)
                              (make-http-response
                               :status 401
                               :headers
                               (list
                                (make-http-header "WWW-Authenticate" challenge)
                                (make-http-header "Connection" "close")))
                              (progn
                                (funcall verifier request)
                                (make-http-response
                                 :status 200
                                 :headers
                                 (list (make-http-header "Connection" "close"))
                                 :body (ascii body)))))
                        :max-connections request-count-limit
                        :session-options (list :max-requests 1))
                     (unless (and (= served-count request-count-limit)
                                  (eq termination :max-connections))
                       (error "The authentication server served ~D requests and stopped with ~S."
                              served-count termination)))
                 (error (condition)
                   (setf server-error condition))))))
      (values thread
              (lambda ()
                (sb-thread:join-thread thread)
                (values request-count server-error)))))

  (defun %e2e-auth-client (challenge-provider)
    (make-http-client
     :automatic-decompression-p nil
     :open-stream (make-http-network-stream-opener)
     :close-stream #'close-http-tcp-stream
     :challenge-auth-provider challenge-provider))

  (defun %e2e-auth-uri (listener)
    (format nil "http://127.0.0.1:~D/"
            (http-network-listener-port listener)))

  (deftest e2e-loopback-basic-auth-challenge
    (let* ((challenge "Basic realm=\"loopback-basic\"")
           (authorization "Basic YmFzaWMtdXNlcjpiYXNpYy1wYXNz")
           (client
             (%e2e-auth-client
              (lambda (request response challenges)
                (declare (ignore request response challenges))
                authorization)))
           (listener (open-http-tcp-listener :host "127.0.0.1" :port 0))
           (uri (%e2e-auth-uri listener)))
      (unwind-protect
           (multiple-value-bind (thread finish)
               (%e2e-auth-run-server
                listener challenge 2
                (lambda (request)
                  (ensure-equal authorization
                                (http-header-value
                                 (http-request-headers request) "Authorization")))
                "basic-ok")
             (declare (ignore thread))
             (multiple-value-bind (response request)
                 (http-client-send client "GET" uri :timeout 5)
               (ensure-equal 200 (http-response-status response))
               (ensure-equal "basic-ok"
                             (octets-as-string (http-response-body response)))
               (ensure-equal "/" (http-request-target request)))
             (multiple-value-bind (request-count server-error) (funcall finish)
               (ensure-equal 2 request-count)
               (ensure-true (null server-error))))
        (close-http-tcp-listener listener))))

  (deftest e2e-loopback-digest-auth-md5-and-sha256
    (dolist (algorithm '("MD5" "SHA-256"))
      (let* ((nonce (format nil "server-issued-~A-~36R"
                            algorithm (get-universal-time)))
             (challenge
               (format nil "Digest realm=\"loopback-digest\", nonce=\"~A\", qop=auth, algorithm=~A"
                       nonce algorithm))
             (parsed (first (http-parse-authentication-challenges challenge)))
             (nonce-count 0)
             (client
               (%e2e-auth-client
                (lambda (request response challenges)
                  (declare (ignore response))
                  (incf nonce-count)
                  (http-digest-authorization
                   (first challenges)
                   (http-request-method request)
                   (http-request-target request)
                   "digest-user" "digest-pass" "fixed-cnonce"
                   :nonce-count nonce-count
                   :qop "auth"))))
             (listener (open-http-tcp-listener :host "127.0.0.1" :port 0))
             (uri (%e2e-auth-uri listener))
             (verified-nc nil))
        (unwind-protect
             (multiple-value-bind (thread finish)
                 (%e2e-auth-run-server
                  listener challenge 4
                  (lambda (request)
                    (let ((authorization
                            (http-header-value
                             (http-request-headers request) "Authorization")))
                      (setf verified-nc
                            (%e2e-auth-verify-digest
                             request authorization parsed
                             "digest-user" "digest-pass" (or verified-nc 0)))))
                  "digest-ok")
               (declare (ignore thread))
               (dotimes (index 2)
                 (multiple-value-bind (response request)
                     (http-client-send client "GET" uri :timeout 5)
                   (ensure-equal 200 (http-response-status response))
                   (ensure-equal "digest-ok"
                                 (octets-as-string (http-response-body response)))
                   (ensure-equal "/" (http-request-target request))))
               (multiple-value-bind (request-count server-error) (funcall finish)
                 (ensure-equal 4 request-count)
                 (ensure-equal 2 nonce-count)
                 (ensure-equal 2 verified-nc)
                 (ensure-false server-error)))
          (close-http-tcp-listener listener))))))

#-sbcl
(deftest e2e-loopback-auth-requires-sbcl
  (signals http-unsupported-feature
    (error 'http-unsupported-feature
           :message "Loopback auth E2E requires the SBCL native network boundary."
           :operation :test
           :feature :native-tcp)))
