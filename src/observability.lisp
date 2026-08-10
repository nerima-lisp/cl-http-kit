(in-package #:http-kit/observability)

(defun make-http-metrics (&optional (registry (make-metric-registry)))
  (check-type registry metric-registry)
  (%make-http-metrics
   registry
   (define-counter registry http_client_requests_total
     :help "Completed HTTP requests by method and outcome."
     :label-names '("method" "outcome")
     :cardinality-limit 16)
   (define-counter registry http_client_errors_total
     :help "HTTP request failures by method and condition kind."
     :label-names '("method" "kind")
     :cardinality-limit 32)))

(defun %condition-kind (condition)
  (cond ((typep condition 'http-timeout) :timeout)
        ((typep condition 'http-protocol-error) :protocol)
        ((typep condition 'http-connection-error) :connection)
        (t :error)))

(defun %request-labels (request outcome)
  (list (cons "method" (http-request-method request))
        (cons "outcome" outcome)))

(defun %error-labels (request condition)
  (list (cons "method" (http-request-method request))
        (cons "kind" (string-downcase (symbol-name (%condition-kind condition))))))

(defun %record-success (metrics request)
  (metric-inc (http-metrics-request-counter metrics)
              :labels (%request-labels request "success")))

(defun %record-error (metrics request condition)
  (metric-inc (http-metrics-request-counter metrics)
              :labels (%request-labels request "error"))
  (metric-inc (http-metrics-error-counter metrics)
              :labels (%error-labels request condition)))

(defun call-with-http-observability/cps
    (metrics request operation on-success &key on-error)
  "Run OPERATION with metrics-aware success and error continuations.

OPERATION receives two continuations.  Each continuation records one outcome
before invoking the corresponding caller continuation.  A callback exception
is re-signaled without being counted as a second transport outcome."
  (check-type metrics http-metrics)
  (check-type request http-request)
  (check-type operation function)
  (check-type on-success function)
  (when on-error
    (check-type on-error function))
  (let ((state :pending))
    (labels ((success (response)
               (if (eq state :pending)
                   (progn
                     (setf state :success)
                     (%record-success metrics request)
                     (funcall on-success response))
                   (error 'http-protocol-error
                          :message "The HTTP operation invoked a continuation twice."
                          :operation :observability
                          :detail state)))
             (failure (condition)
               (unless (typep condition 'condition)
                 (setf state :callback-error)
                 (error 'type-error
                        :datum condition
                        :expected-type 'condition))
               (if (eq state :pending)
                   (progn
                     (setf state :failure)
                     (%record-error metrics request condition)
                     (if on-error
                         (funcall on-error condition)
                         (error condition)))
                   (error 'http-protocol-error
                          :message "The HTTP operation invoked a continuation twice."
                          :operation :observability
                          :detail state))))
      (handler-case
          (funcall operation #'success #'failure)
        (error (condition)
          (if (eq state :pending)
              (failure condition)
              (error condition)))))))
