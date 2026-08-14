(in-package #:http-kit/test)

(deftest client-redirects-and-hooks
  (let ((calls 0)
        (methods nil)
        (uris nil)
        (request-events 0)
        (response-events 0))
    (with-test-client (client
                       (lambda (request &key proxy-plan &allow-other-keys)
                         (declare (ignore proxy-plan))
                         (incf calls)
                         (push (http-request-method request) methods)
                         (push (http-uri-string (http-request-uri request)) uris)
                         (if (= calls 1)
                             (client-test-response
                              302
                              :headers (list (make-http-header
                                              "Location" "/final")))
                             (client-test-response 200 :body (ascii "done"))))
                       :cache nil
                       :on-request (lambda (request attempt)
                                     (declare (ignore request attempt))
                                     (incf request-events))
                       :on-response (lambda (response request attempt)
                                      (declare (ignore response request attempt))
                                      (incf response-events)))
      (let ((request (http-client-request client "POST"
                                          "http://example.test/start"
                                          :body (ascii "payload"))))
        (multiple-value-bind (response effective)
            (http-client-send client request)
          (ensure-equal 200 (http-response-status response))
          (ensure-equal "http://example.test/final"
                        (http-uri-string (http-request-uri effective)))
          (ensure-equal '(("POST" . "http://example.test/start")
                          ("GET" . "http://example.test/final"))
                        (mapcar #'cons (reverse methods) (reverse uris)))
          (ensure-equal 2 calls)
          (ensure-equal 2 request-events)
          (ensure-equal 2 response-events))))))

(deftest client-redirect-drops-trailers-when-method-changes
  (let ((calls 0)
        (seen-trailers nil))
    (let* ((client
             (make-http-client
              :cache nil
              :transport-function
              (lambda (request &key proxy-plan &allow-other-keys)
                (declare (ignore proxy-plan))
                (incf calls)
                (push (http-request-trailers request) seen-trailers)
                (if (= calls 1)
                    (client-test-response
                     302
                     :headers (list (make-http-header
                                     "Location" "/final")))
                    (client-test-response 200)))))
           (request (http-client-request
                     client "POST" "http://example.test/start"
                     :trailers (list (make-http-header "X-Checksum" "abc")))))
      (multiple-value-bind (response effective)
          (http-client-send client request)
        (declare (ignore response))
        (ensure-equal "GET" (http-request-method effective))
        (ensure-equal nil (http-request-trailers effective)))
      (ensure-equal 2 calls)
      (ensure-equal nil (first seen-trailers))
      (ensure-equal "abc"
                    (http-header-value (second seen-trailers) "X-Checksum")))))

(deftest client-retries
  (let ((calls 0)
        (sleeps nil))
    (let* ((client
             (make-http-client
              :cache nil
              :sleep-function (lambda (seconds)
                                (push seconds sleeps))
              :retry-policy (make-http-retry-policy
                             :max-attempts 2
                             :base-delay 0.25
                             :max-delay 0.25)
              :transport-function
              (lambda (request &key proxy-plan &allow-other-keys)
                (declare (ignore request proxy-plan))
                (incf calls)
                (if (= calls 1)
                    (client-test-response 503)
                    (client-test-response 200 :body (ascii "ok"))))))
           (request (http-client-request client "GET"
                                         "http://example.test/retry")))
      (multiple-value-bind (response effective)
          (http-client-send client request)
        (declare (ignore effective))
        (ensure-equal 200 (http-response-status response))
        (ensure-equal "ok"
                      (octets-as-string (http-response-body response))))
      (ensure-equal 2 calls)
      (ensure-equal '(0.25) sleeps)))
  (let ((calls 0)
        (condition nil))
    (let* ((client
             (make-http-client
              :cache nil
              :retry-policy (make-http-retry-policy :max-attempts 2)
              :transport-function
              (lambda (request &key proxy-plan &allow-other-keys)
                (declare (ignore request proxy-plan))
                (incf calls)
                (client-test-response 503))))
           (request (http-client-request client "GET"
                                         "http://example.test/exhausted")))
      (handler-case
          (http-client-send client request)
        (http-retry-exhausted (caught)
          (setf condition caught)))
      (ensure-true condition)
      (ensure-equal 2 calls)
      (ensure-equal 2 (http-retry-exhausted-attempts condition))
      (ensure-equal 503
                    (http-response-status
                     (http-retry-exhausted-last-response condition))))))

(deftest client-streaming-request-body-replay-factory
  (let ((attempts 0)
        (factory-calls 0)
        (bodies nil))
    (let ((client
            (make-http-client
             :cache nil
             :retry-policy (make-http-retry-policy
                            :max-attempts 2
                            :methods '("POST")
                            :base-delay 0
                            :max-delay 0)
             :transport-function
             (lambda (request &key request-body-function &allow-other-keys)
               (declare (ignore request))
               (incf attempts)
               (let ((chunks nil))
                 (loop for chunk = (funcall request-body-function 65536)
                       while chunk
                       do (push (octets-as-string chunk) chunks))
                 (push (nreverse chunks) bodies))
               (if (= attempts 1)
                   (client-test-response 503)
                   (client-test-response 200 :body (ascii "ok")))))))
      (multiple-value-bind (response effective)
          (http-client-send
           client
           (http-client-request client "POST" "http://example.test/retry-body")
           :request-body-factory
           (lambda ()
             (incf factory-calls)
             (let ((chunks (list (ascii "abc")
                                 (ascii "de"))))
               (lambda (maximum-size)
                 (ensure-equal 65536 maximum-size)
                 (pop chunks))))
           :request-body-length 5)
        (declare (ignore effective))
        (ensure-equal 200 (http-response-status response))
        (ensure-equal "ok" (octets-as-string (http-response-body response))))
      (ensure-equal 2 attempts)
      (ensure-equal 2 factory-calls)
      (ensure-equal '(("abc" "de") ("abc" "de"))
                    (reverse bodies)))))

(deftest client-streaming-request-body-does-not-retry-without-factory
  (let ((attempts 0)
        (bodies nil)
        (chunks (list (ascii "abc")
                      (ascii "de"))))
    (let ((client
            (make-http-client
             :cache nil
             :retry-policy (make-http-retry-policy :max-attempts 2)
             :transport-function
             (lambda (request &key request-body-function &allow-other-keys)
               (declare (ignore request))
               (incf attempts)
               (let ((received nil))
                 (loop for chunk = (funcall request-body-function 65536)
                       while chunk
                       do (push (octets-as-string chunk) received))
                 (push (nreverse received) bodies))
               (client-test-response 503)))))
      (multiple-value-bind (response effective)
          (http-client-send
           client
           (http-client-request client "POST" "http://example.test/no-replay")
           :request-body-function
           (lambda (maximum-size)
             (ensure-equal 65536 maximum-size)
             (pop chunks))
           :request-body-length 5)
        (declare (ignore effective))
        (ensure-equal 503 (http-response-status response)))
      (ensure-equal 1 attempts)
      (ensure-true (null chunks))
      (ensure-equal '(("abc" "de")) bodies))))

(deftest client-streaming-request-body-does-not-follow-same-method-redirect
  (let ((calls 0)
        (chunks (list (ascii "abc")
                      (ascii "de"))))
    (let ((client
            (make-http-client
             :cache nil
             :transport-function
             (lambda (request &key request-body-function &allow-other-keys)
               (declare (ignore request))
               (incf calls)
               (when request-body-function
                 (loop for chunk = (funcall request-body-function 65536)
                       while chunk))
               (client-test-response
                307
                :headers (list (make-http-header "Location" "/next")))))))
      (multiple-value-bind (response effective)
          (http-client-send
           client
           (http-client-request client "PUT" "http://example.test/start")
           :request-body-function
           (lambda (maximum-size)
             (ensure-equal 65536 maximum-size)
             (pop chunks))
           :request-body-length 5)
        (ensure-equal 307 (http-response-status response))
        (ensure-equal "http://example.test/start"
                      (http-uri-string (http-request-uri effective))))
      (ensure-equal 1 calls)
      (ensure-true (null chunks)))))
