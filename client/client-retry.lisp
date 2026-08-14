(in-package #:http-kit/client)

(defun %client-request-with-proxy-authorization (request proxy-plan)
  (let ((authorization
          (and proxy-plan
               (eq (getf proxy-plan :mode) :forward)
               (getf proxy-plan :proxy-authorization))))
    (if (and authorization
             (not (http-header-present-p
                   (http-request-headers request)
                   "Proxy-Authorization")))
        (%client-request-with
         request
         :headers (%client-header-set
                   (http-request-headers request)
                   "Proxy-Authorization"
                   authorization))
        request)))

(defun %client-retry-method-p (policy request)
  (member (string-upcase (http-request-method request))
          (http-retry-policy-methods policy)
          :test #'string-equal))

(defun %client-retry-status-p (policy response)
  (member (http-response-status response)
          (http-retry-policy-statuses policy)))

(defun %client-retryable-condition-p (policy condition)
  (or (and (typep condition 'http-timeout)
           (http-retry-policy-retry-on-timeout-p policy))
      (and (typep condition 'http-connection-error)
           (http-retry-policy-retry-on-connection-error-p policy))))

(defun %client-retry-delay (client policy response attempt)
  (let* ((retry-after (and (http-retry-policy-respect-retry-after-p policy)
                           response
                           (%retry-after-seconds
                            (http-response-headers response)
                            (funcall (http-client-clock-function client)))))
         (exponential (* (float (http-retry-policy-base-delay policy))
                         (expt 2 (1- attempt)))))
    (min (float (http-retry-policy-max-delay policy))
         (float (or retry-after exponential)))))

(defun %client-sleep-before-retry (client policy response attempt)
  (let ((delay (%client-retry-delay client policy response attempt)))
    (when (plusp delay)
      (funcall (http-client-sleep-function client) delay))))

(defun %client-call-transport
    (client request proxy-plan &key timeout deadline request-body-function
                                      request-body-length on-body-chunk
                                      on-information
                                      (collect-body-p t))
  (let* ((arguments
           (list :timeout timeout
                 :deadline deadline
                 :max-header-bytes (http-client-max-header-bytes client)
                 :max-body-bytes (http-client-max-body-bytes client)
                 :proxy (http-client-proxy client)
                 :proxy-plan proxy-plan
                 :request-body-function request-body-function
                 :request-body-length request-body-length
                 :on-body-chunk on-body-chunk
                 :on-information on-information
                 :collect-body-p collect-body-p))
         (effective-request
           (%client-request-with-proxy-authorization request proxy-plan)))
    (let ((response
            (apply (http-client-transport-function client)
                   effective-request
                   arguments)))
      (unless (http-response-p response)
        (%client-protocol-error
         "The client transport must return an HTTP-RESPONSE."
         response))
      response)))

(defun %client-attempt
    (client request proxy-plan policy &key timeout deadline request-body-function
                                                request-body-factory request-body-length
                                                on-body-chunk
                                                on-information
                                                (collect-body-p t))
  (let* ((attempt 1)
         (max-attempts (http-retry-policy-max-attempts policy))
         (last-response nil)
         (retryable-request-p
           (and (%client-retry-method-p policy request)
                (or (null request-body-function)
                    request-body-factory))))
    (loop
      (when (http-client-on-request client)
        (funcall (http-client-on-request client) request attempt))
      (let ((attempt-body-function
              (%client-request-body-for-attempt
               request-body-function request-body-factory)))
        (handler-case
            (let ((response (%client-call-transport
                             client request proxy-plan
                             :timeout timeout
                             :deadline deadline
                             :request-body-function attempt-body-function
                             :request-body-length request-body-length
                             :on-body-chunk on-body-chunk
                             :on-information on-information
                             :collect-body-p collect-body-p)))
              (setf last-response response)
              (http-cookie-jar-accept-response
               (http-client-cookie-jar client)
               (http-request-uri request)
               response
               :partition-key (http-client-cookie-partition-key client))
              (if (and (< attempt max-attempts)
                       retryable-request-p
                       (%client-retry-status-p policy response))
                  (progn
                    (%client-sleep-before-retry client policy response attempt)
                    (incf attempt))
                  (progn
                    (when (and (> max-attempts 1)
                               retryable-request-p
                               (%client-retry-status-p policy response))
                      (%client-retry-exhausted-error
                       attempt
                       :request request
                       :last-response response))
                    (when (http-client-on-response client)
                      (funcall (http-client-on-response client) response request attempt))
                    (return (values response attempt)))))
          (http-error (condition)
            (if (and (< attempt max-attempts)
                     retryable-request-p
                     (%client-retryable-condition-p policy condition))
                (progn
                  (%client-sleep-before-retry client policy last-response attempt)
                  (incf attempt))
                (if (and (> max-attempts 1)
                         retryable-request-p
                         (%client-retryable-condition-p policy condition))
                    (%client-retry-exhausted-error
                     attempt
                     :request request
                     :last-condition condition
                     :last-response last-response)
                    (error condition)))))))))
