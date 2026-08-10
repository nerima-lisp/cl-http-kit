(in-package #:http-kit)

(defun make-recording-session (&key responses response-function)
  "Create deterministic in-memory response state for tests and examples.

RESPONSES is a list consumed in order.  Alternatively RESPONSE-FUNCTION is
called as (REQUEST &KEY TIMEOUT DEADLINE) and must return an HTTP-RESPONSE.
The session records immutable request and response values for inspection."
  (unless (and (or (null responses) (listp responses))
               (or (null response-function) (functionp response-function)))
    (error 'http-protocol-error
           :message "Recorded responses must be a list or function."
           :operation :recording
           :detail (list responses response-function)))
  (%make-recording-session
   :pending-responses (copy-list responses)
   :response-function response-function))

(defun %copy-request-value (request)
  (make-http-request
   :method (http-request-method request)
   :protocol-version (http-request-protocol-version request)
   :uri (http-request-uri request)
   :request-target (and (%request-target request)
                        (copy-seq (%request-target request)))
   :headers (mapcar (lambda (header)
                      (make-http-header (http-header-name header)
                                        (http-header-content header)))
                    (http-request-headers request))
   :trailers (mapcar (lambda (header)
                       (make-http-header (http-header-name header)
                                         (http-header-content header)))
                     (http-request-trailers request))
   :body (http-request-body request)))

(defun %copy-response-value (response)
  (make-http-response
   :protocol-version (http-response-protocol-version response)
   :status (http-response-status response)
   :reason (http-response-reason response)
   :headers (mapcar (lambda (header)
                      (make-http-header (http-header-name header)
                                        (http-header-content header)))
                    (http-response-headers response))
   :trailers (mapcar (lambda (header)
                       (make-http-header (http-header-name header)
                                         (http-header-content header)))
                     (http-response-trailers response))
   :body (http-response-body response)))

(defun recording-session-requests (session)
  (reverse (%recording-requests session)))

(defun recording-session-responses (session)
  (reverse (%recording-responses session)))

(defun %recording-header-bytes (response)
  "Conservatively estimate the wire bytes covered by a response header limit."
  (+ 15
     (length (http-response-reason response))
     2
     (reduce #'+ (append (http-response-headers response)
                         (http-response-trailers response))
             :initial-value 0
             :key (lambda (header)
                    (+ (length (http-header-name header))
                       2
                       (length (http-header-content header))
                       2)))
     2))

(defun send-recorded-http-request
    (session request &key timeout deadline max-header-bytes max-body-bytes
                      clock-function)
  "Return the next recorded response for REQUEST and update SESSION history."
  (%check-http-request request)
  (unless (recording-session-p session)
    (error 'http-protocol-error
           :message "A recorded request requires a RECORDING-SESSION."
           :operation :recording
           :detail (type-of session)))
  (let* ((clock-function (or clock-function #'%monotonic-time))
         (header-limit (or max-header-bytes *default-max-header-bytes*))
         (body-limit (or max-body-bytes *default-max-body-bytes*)))
    (with-http-deadline (absolute-deadline timeout
                          :inherited deadline
                          :clock-function clock-function
                          :kind :recording)
      (unless (and (integerp header-limit) (plusp header-limit))
        (error 'http-protocol-error
               :message "The maximum response header size must be a positive integer."
               :operation :limit
               :detail header-limit))
      (unless (and (integerp body-limit) (>= body-limit 0))
        (error 'http-protocol-error
               :message "The maximum response body size must be a non-negative integer."
               :operation :limit
               :detail body-limit))
      (push (%copy-request-value request) (%recording-requests session))
      (let ((response
              (%with-http-error-translation
                  ("The recorded response function failed." :recording)
                (if (%recording-response-function session)
                    (funcall (%recording-response-function session) request
                             :timeout timeout
                             :deadline absolute-deadline)
                    (let ((pending (%recording-pending-responses session)))
                      (if pending
                          (prog1 (first pending)
                            (setf (%recording-pending-responses session)
                                  (rest pending)))
                          (error 'http-connection-error
                                 :message "The recording session has no response left."
                                 :operation :recording
                                 :cause :no-recorded-response)))))))
        (unless (http-response-p response)
          (error 'http-protocol-error
                 :message "A recorded response must be an HTTP-RESPONSE."
                 :operation :recording
                 :detail (type-of response)))
        (%check-limit :headers (%recording-header-bytes response) header-limit)
        (%check-limit :body (length (http-response-body response)) body-limit)
        (push (%copy-response-value response) (%recording-responses session))
        (%check-deadline absolute-deadline clock-function :recording)
        (%copy-response-value response)))))

(defun send-recorded-http-request/cps
    (session request on-success
     &key on-error timeout deadline max-header-bytes max-body-bytes
       clock-function)
  "Send a request through a recording session and dispatch its result."
  (%call-http-operation/cps
   (lambda ()
     (send-recorded-http-request
      session request
      :timeout timeout
      :deadline deadline
      :max-header-bytes max-header-bytes
      :max-body-bytes max-body-bytes
      :clock-function clock-function))
   on-success
   :on-error on-error))
