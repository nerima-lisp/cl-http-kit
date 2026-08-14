(in-package #:http-kit/client)

(defun %client-validate-collect-body-p (value)
  (unless (member value '(nil t))
    (%client-protocol-error
     "The body collection flag must be NIL or T."
     value))
  value)

(defun %client-deliver-body-chunk (response on-body-chunk)
  (let ((body (http-response-body response)))
    (when (and on-body-chunk (consp body))
      (funcall on-body-chunk body)))
  response)

(defun %client-validate-send-options
    (client request request-body-function request-body-factory
            request-body-length on-body-chunk on-information collect-body-p)
  "Validate the boundary arguments accepted by HTTP-CLIENT-SEND."
  (unless (http-client-p client)
    (%client-protocol-error "The client must be an HTTP-CLIENT." client))
  (unless (http-request-p request)
    (%client-protocol-error "The request must be an HTTP-REQUEST." request))
  (%client-validate-request-body-stream
   request request-body-function request-body-factory request-body-length)
  (when on-body-chunk
    (%ensure-function on-body-chunk
                      "The response body callback must be a function."))
  (when on-information
    (%ensure-function on-information
                      "The informational response callback must be a function."))
  (%client-validate-collect-body-p collect-body-p))

(defun http-client-send
    (client request &key timeout deadline redirect-policy retry-policy
                         request-body-function request-body-factory
                         request-body-length
                         on-body-chunk on-information (collect-body-p t))
  "Execute REQUEST with redirects, retries, cookies, cache, auth, and proxy policy.

Returns the final HTTP-RESPONSE as the primary value and the effective
HTTP-REQUEST as a secondary value.  TIMEOUT and DEADLINE are passed through to
the configured transport boundary.  REQUEST-BODY-FACTORY, when supplied,
must return a fresh producer function on every invocation.  Streaming request
bodies without a factory are sent once and are not automatically retried or
resent across same-method redirects."
  (%client-validate-send-options
   client request request-body-function request-body-factory
   request-body-length on-body-chunk on-information collect-body-p)
  (let* ((redirect-policy (or redirect-policy
                              (http-client-redirect-policy client)))
         (retry-policy (or retry-policy
                           (http-client-retry-policy client)))
         (initial-uri (http-request-uri request))
         (current-request request)
         (redirect-count 0)
         (stale-entry nil))
    (unless (http-redirect-policy-p redirect-policy)
      (%client-protocol-error "The redirect policy must be an HTTP-REDIRECT-POLICY."
                              redirect-policy))
    (unless (http-retry-policy-p retry-policy)
      (%client-protocol-error "The retry policy must be an HTTP-RETRY-POLICY."
                              retry-policy))
    (when (and (http-client-cache client)
               (%client-cacheable-method-p current-request))
      (multiple-value-bind (response state entry)
          (http-cache-lookup (http-client-cache client) current-request)
        (when (eq state :fresh)
          (%client-deliver-body-chunk response on-body-chunk)
          (when (http-client-on-response client)
            (funcall (http-client-on-response client) response current-request 0))
          (return-from http-client-send (values response current-request)))
        (when (eq state :stale)
          (setf stale-entry entry
                current-request (%client-conditional-request
                                 current-request entry)))))
    (loop
      (let* ((redirect-p (plusp redirect-count))
             (prepared (%client-prepare-request
                        client current-request initial-uri :redirect-p redirect-p))
             (proxy-plan (http-proxy-plan
                          (http-client-proxy client)
                          (http-request-uri prepared))))
        (let ((response
                (%client-attempt client prepared proxy-plan retry-policy
                                 :timeout timeout :deadline deadline
                                 :request-body-function request-body-function
                                 :request-body-factory request-body-factory
                                 :request-body-length request-body-length
                                 :on-body-chunk on-body-chunk
                                 :on-information on-information
                                 :collect-body-p collect-body-p)))
          (when (and stale-entry (= (http-response-status response) 304))
            (setf response (%client-response-merge-304
                            (http-cache-entry-response stale-entry)
                            response))
            (%client-deliver-body-chunk response on-body-chunk))
          (let* ((redirect-request
                   (and (member (http-response-status response)
                                (http-redirect-policy-statuses redirect-policy))
                        (%client-header-value
                         (http-response-headers response) "Location")))
                 (next-request
                   (and redirect-request
                        (%client-redirect-request
                         prepared response redirect-policy initial-uri
                         redirect-count)))
                 (follow-redirect-p
                   (%client-follow-redirect-p
                    prepared next-request request-body-function
                    request-body-factory)))
            (if follow-redirect-p
                (if (>= redirect-count
                        (http-redirect-policy-max-redirects redirect-policy))
                    (%client-redirect-limit-error
                     (http-request-uri prepared)
                     redirect-count)
                    (progn
                      (unless (string-equal
                               (http-request-method next-request)
                               (http-request-method prepared))
                        (setf request-body-function nil
                              request-body-factory nil
                              request-body-length nil))
                      (setf current-request next-request)
                      (setf redirect-count (1+ redirect-count)
                            stale-entry nil)
                      (when (and (http-client-cache client)
                                 (%client-mutating-method-p current-request))
                        (http-cache-clear (http-client-cache client)))))
                (progn
                  (%client-store-response client prepared response)
                  (return (values response prepared))))))))))
