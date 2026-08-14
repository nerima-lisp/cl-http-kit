(in-package #:http-kit/test)

#+sbcl
(deftest client-http-forward-proxy-wire
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input (ascii "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok")))
         (opened 0)
         (closed 0)
         (proxy
           (make-http-proxy :scheme :http
                            :host "proxy.example"
                            :port 8080
                            :username "user"
                            :password "pass"))
         (client
           (make-http-client
            :cache nil
            :proxy proxy
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy))
              (ensure-equal :forward (getf proxy-plan :mode))
              (incf opened)
              stream)
            :close-stream
            (lambda (closed-stream)
              (ensure-equal stream closed-stream)
              (incf closed)))))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET"
                              "http://example.test/path?x=1"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (let ((wire (octets-as-string (binary-test-output stream))))
      (ensure-true (search "GET http://example.test/path?x=1 HTTP/1.1" wire))
      (ensure-true (search "Proxy-Authorization: Basic dXNlcjpwYXNz" wire)))
    (ensure-equal 1 opened)
    (ensure-equal 1 closed)))

#+sbcl
(deftest client-http-connect-proxy-wire
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (ascii
             "HTTP/1.1 200 Connection Established|CRLF||CRLF|HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok")))
         (upgrades 0)
         (proxy
           (make-http-proxy :scheme :http
                            :host "proxy.example"
                            :port 8080
                            :username "user"
                            :password "pass"))
         (client
           (make-http-client
            :cache nil
            :proxy proxy
            :tls-upgrade
            (lambda (received-stream uri &key timeout deadline)
              (declare (ignore timeout deadline))
              (ensure-equal "https" (http-uri-scheme uri))
              (incf upgrades)
              received-stream)
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy))
              (ensure-equal :connect (getf proxy-plan :mode))
              stream)
            :close-stream #'close)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "https://example.test/path"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (let* ((wire (octets-as-string (binary-test-output stream)))
           (authorization "Proxy-Authorization: Basic dXNlcjpwYXNz")
           (first-authorization (search authorization wire)))
      (ensure-true (search "CONNECT example.test:443 HTTP/1.1" wire))
      (ensure-true (search "GET /path HTTP/1.1" wire))
      (ensure-true first-authorization)
      (ensure-equal nil
                    (search authorization wire
                            :start2 (1+ first-authorization))))
    (ensure-equal 1 upgrades)))

#+sbcl
(deftest client-socks5-proxy-wire
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input
            (concatenate-octets
             (octets 5 0
                     5 0 0 1 0 0 0 0 0 0)
             (ascii
              "HTTP/1.1 200 OK|CRLF|Content-Length: 2|CRLF||CRLF|ok"))))
         (proxy
           (make-http-proxy :scheme :socks5
                            :host "proxy.example"
                            :port 1080))
         (client
           (make-http-client
            :cache nil
            :proxy proxy
            :open-stream
            (lambda (request &key timeout deadline proxy-plan proxy
                              &allow-other-keys)
              (declare (ignore request timeout deadline proxy))
              (ensure-equal :socks5 (getf proxy-plan :mode))
              stream)
            :close-stream #'close)))
    (multiple-value-bind (response effective)
        (http-client-send
         client
         (http-client-request client "GET" "http://192.0.2.1/path"))
      (declare (ignore effective))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "ok" (octets-as-string (http-response-body response))))
    (let ((output (binary-test-output stream)))
      (ensure-equal
       (octets 5 1 0
               5 1 0 1 192 0 2 1 0 80)
       (subseq output 0 13))
      (ensure-true
       (search "GET /path HTTP/1.1"
               (octets-as-string (subseq output 13)))))))
