(defpackage #:http-kit
  (:use #:cl)
  (:export
   ;; URI values
   #:http-uri
   #:http-uri-p
   #:make-http-uri
   #:parse-http-uri
   #:http-uri-scheme
   #:http-uri-authority
   #:http-uri-host
   #:http-uri-port
   #:http-uri-path
   #:http-uri-query
   #:http-uri-string
   ;; Header values
   #:http-header
   #:http-header-p
   #:make-http-header
   #:http-header-name
   #:http-header-content
   #:http-header-values
   #:http-header-value
   #:http-header-present-p
   ;; Message values
   #:http-request
   #:http-request-p
   #:make-http-request
   #:http-request-protocol-version
   #:http-request-method
   #:http-request-uri
   #:http-request-target
   #:http-request-headers
   #:http-request-trailers
   #:http-request-body
   #:http-request-authority
   #:http-request-path
   #:http-request-query
   #:http-request-summary
   #:http-response
   #:http-response-p
   #:make-http-response
   #:http-response-protocol-version
   #:http-response-status
   #:http-response-reason
   #:http-response-headers
   #:http-response-trailers
   #:http-response-body
   #:http-response-summary
   #:http-response-stream
   #:http-response-stream-p
   #:make-http-response-stream
   #:http-response-stream-protocol-version
   #:http-response-stream-status
   #:http-response-stream-reason
   #:http-response-stream-headers
   #:http-response-stream-trailers
   #:http-response-stream-body-function
   #:http-response-stream-body-length
   ;; HTTP/1.1 wire API
   #:serialize-http-request
   #:serialize-http-response
   #:parse-http-request
   #:parse-http-response
   #:serve-http1-session
   ;; Direct I/O boundaries
   #:send-http-request-over-open-stream
   #:send-http-request-over-stream
   #:send-http-request-over-stream/cps
   #:http-response-reusable-p
   ;; Deterministic test boundary
   #:recording-session
   #:recording-session-p
   #:make-recording-session
   #:recording-session-requests
   #:recording-session-responses
   #:send-recorded-http-request
   #:send-recorded-http-request/cps
   ;; Limits and deadlines
   #:http-deadline
   #:with-http-deadline
   #:http-size-limit-exceeded
   #:http-size-limit-exceeded-limit
   #:http-size-limit-exceeded-observed
   #:http-size-limit-exceeded-kind
   ;; Structured conditions
   #:http-error
   #:http-error-message
   #:http-error-operation
   #:http-protocol-error
   #:http-protocol-error-detail
   #:http-invalid-uri
   #:http-invalid-uri-input
   #:http-invalid-header
   #:http-invalid-header-name
   #:http-invalid-header-reason
   #:http-invalid-status
   #:http-invalid-status-line
   #:http-invalid-status-code
   #:http-connection-error
   #:http-connection-error-cause
   #:http-timeout
   #:http-timeout-kind
   #:http-unsupported-feature
   #:http-unsupported-feature-name))
