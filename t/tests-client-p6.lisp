(in-package #:http-kit/test)

(deftest client-p6-convenience-url-method
  (let* ((seen-request nil)
         (client
           (make-http-client
            :transport-function
            (lambda (request &rest arguments)
              (declare (ignore arguments))
              (setf seen-request request)
              (make-http-response
               :status 204
               :headers nil
               :body (make-array 0 :element-type '(unsigned-byte 8)))))))
    (multiple-value-bind (response request)
        (http-client-send client "GET" "http://example.test/resource")
      (ensure-equal 204 (http-response-status response))
      (ensure-equal "GET" (http-request-method request))
      (ensure-equal request seen-request))))

(deftest client-p6-proxy-environment-resolution
  (let ((values '(("https_proxy" . "http://user:secret@proxy.test:8080")
                  ("NO_PROXY" . "example.test"))))
    (let ((http-kit/client::*proxy-environment-function*
            (lambda (name)
              (cdr (assoc name values :test #'string=)))))
      (let ((proxy (http-proxy-for-uri nil "https://example.test/resource")))
        (ensure-equal :http (http-proxy-scheme proxy))
        (ensure-equal "proxy.test" (http-proxy-host proxy))
        (ensure-true (http-proxy-no-proxy-p proxy "https://example.test/resource"))
        (ensure-false (http-proxy-no-proxy-p proxy "https://other.test/resource"))))))

#+sbcl
(deftest client-p6-cookie-jar-concurrent-updates
  (let* ((jar (make-http-cookie-jar))
         (threads
           (loop for thread-index below 8
                 collect
                 (let ((name (format nil "thread-~D" thread-index)))
                   (sb-thread:make-thread
                    (lambda ()
                      (dotimes (iteration 20)
                        (declare (ignore iteration))
                        (http-cookie-jar-accept-response
                         jar
                         "https://example.test/"
                         (make-http-response
                          :status 200
                          :headers
                          (list
                           (make-http-header
                            "Set-Cookie"
                            (format nil "~A=value; Path=/" name)))
                          :body nil)))))))))
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    (ensure-equal 8 (length (http-cookie-jar-cookies jar)))))

#+sbcl
(deftest client-p6-pool-concurrent-observation
  (let ((pool
          (make-http-connection-pool
           :max-idle 4
           :open-stream (lambda (&rest arguments)
                          (declare (ignore arguments))
                          (error "The observation test must not open a stream.")))))
    (let ((threads
            (loop repeat 8
                  collect
                  (sb-thread:make-thread
                   (lambda ()
                     (dotimes (iteration 20)
                       (declare (ignore iteration))
                       (ensure-equal 0
                                     (getf (http-connection-pool-stats pool)
                                           :idle-count))))))))
      (dolist (thread threads)
        (sb-thread:join-thread thread)))))
