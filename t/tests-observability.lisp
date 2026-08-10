(in-package #:http-kit/test)

(defun %metric-value (metric labels)
  (let ((sample
          (find labels
                (metric-snapshot-samples (metric-snapshot metric))
                :key #'metric-sample-labels
                :test #'equal)))
    (and sample (metric-sample-value sample))))

(defun %observed-request ()
  (make-http-request :method "GET"
                     :uri "http://127.0.0.1/"))

(deftest observability-construction-boundaries
  (let ((registry (make-metric-registry)))
    (ensure-true
     (eq registry
         (http-metrics-registry (make-http-metrics registry)))))
  (signals type-error
    (make-http-metrics nil))
  (signals type-error
    (call-with-http-observability/cps
     (make-http-metrics)
     (%observed-request)
     (lambda (on-success on-error)
       (declare (ignore on-success on-error)))
     (lambda (response)
       (declare (ignore response)))
     :on-error t)))

(deftest observability-success
  (let* ((metrics (make-http-metrics))
         (request (%observed-request))
         (response (make-http-response :status 204)))
    (ensure-equal :ok
                  (call-with-http-observability/cps
                   metrics request
                   (lambda (on-success on-error)
                     (declare (ignore on-error))
                     (funcall on-success response))
                   (lambda (received-response)
                     (ensure-equal 204 (http-response-status received-response))
                     :ok)))
    (ensure-equal 1
                  (%metric-value
                   (http-metrics-request-counter metrics)
                   '(("method" . "GET") ("outcome" . "success"))))))

(deftest observability-error-kinds
  (let ((metrics (make-http-metrics))
        (request (%observed-request)))
    (dolist (condition (list (make-condition 'http-timeout :message "timeout")
                             (make-condition 'http-protocol-error :message "protocol")
                             (make-condition 'http-connection-error :message "connection")
                             (make-condition 'simple-error
                                             :format-control "generic"
                                             :format-arguments nil)))
      (ensure-equal :handled
                    (call-with-http-observability/cps
                     metrics request
                     (lambda (on-success on-error)
                       (declare (ignore on-success))
                       (funcall on-error condition))
                     (lambda (response)
                       (declare (ignore response))
                       :unexpected)
                     :on-error (lambda (received-condition)
                                 (ensure-equal condition received-condition)
                                 :handled))))
    (ensure-equal 4
                  (%metric-value
                   (http-metrics-request-counter metrics)
                   '(("method" . "GET") ("outcome" . "error"))))
    (dolist (kind '("timeout" "protocol" "connection" "error"))
      (ensure-equal 1
                     (%metric-value
                     (http-metrics-error-counter metrics)
                     (list (cons "kind" kind)
                           (cons "method" "GET")))))))

(deftest observability-continuation-boundaries
  (let* ((metrics (make-http-metrics))
         (request (%observed-request))
         (response (make-http-response :status 200)))
    (signals http-protocol-error
      (call-with-http-observability/cps
       metrics request
       (lambda (on-success on-error)
         (declare (ignore on-error))
         (funcall on-success response)
         (funcall on-success response))
       (lambda (received-response)
         (declare (ignore received-response)))))
    (ensure-equal 1
                  (%metric-value
                   (http-metrics-request-counter metrics)
                   '(("method" . "GET") ("outcome" . "success"))))
    (signals http-timeout
      (call-with-http-observability/cps
       metrics request
       (lambda (on-success on-error)
         (declare (ignore on-success))
         (funcall on-error (make-condition 'http-timeout :message "timeout")))
       (lambda (received-response)
         (declare (ignore received-response)))
       :on-error nil))
    (signals error
      (call-with-http-observability/cps
       metrics request
       (lambda (on-success on-error)
         (declare (ignore on-error))
         (funcall on-success response))
       (lambda (received-response)
         (declare (ignore received-response))
         (error "callback failure"))))
    (signals type-error
      (call-with-http-observability/cps
       metrics request nil
        (lambda (received-response)
          (declare (ignore received-response)))))))

(deftest observability-failure-boundaries
  (let ((metrics (make-http-metrics))
        (request (%observed-request)))
    (signals http-protocol-error
      (call-with-http-observability/cps
       metrics request
       (lambda (on-success on-error)
         (declare (ignore on-success))
         (funcall on-error (make-condition 'http-timeout :message "first"))
         (funcall on-error (make-condition 'http-timeout :message "second")))
       (lambda (response)
         (declare (ignore response)))
       :on-error (lambda (condition)
                   (declare (ignore condition)))))
    (signals type-error
      (call-with-http-observability/cps
       metrics request
       (lambda (on-success on-error)
         (declare (ignore on-success))
         (funcall on-error :not-a-condition))
       (lambda (response)
         (declare (ignore response)))
       :on-error (lambda (condition)
                   (declare (ignore condition)))))))

(deftest observability-operation-errors
  (let ((metrics (make-http-metrics))
        (request (%observed-request)))
    (signals simple-error
      (call-with-http-observability/cps
       metrics request
       (lambda (on-success on-error)
         (declare (ignore on-success on-error))
         (error "operation failure"))
       (lambda (response)
         (declare (ignore response))))))
  (let ((metrics (make-http-metrics))
        (request (%observed-request)))
    (signals simple-error
      (call-with-http-observability/cps
       metrics request
       (lambda (on-success on-error)
         (declare (ignore on-success))
         (funcall on-error (make-condition 'simple-error
                                           :format-control "transport failure"
                                           :format-arguments nil)))
       (lambda (response)
         (declare (ignore response)))
       :on-error (lambda (condition)
                   (declare (ignore condition))
                   (error "error callback failure"))))))
