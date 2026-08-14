(in-package #:http-kit)

(defun %http1-session-header-token-p (headers name token)
  (%http1-header-token-p headers name token :operation :session))

(defun %http1-session-response-reusable-p (request response)
  (let* ((request-headers (http-request-headers request))
         (response-headers (http-response-headers response))
         (method (http-request-method request))
         (status (http-response-status response))
         (protocol-version (http-request-protocol-version request))
         (http10-p (string= protocol-version "HTTP/1.0"))
         (http11-p (string= protocol-version "HTTP/1.1")))
    (and (or http10-p http11-p)
         (not (%http1-session-header-token-p request-headers
                                             "Connection"
                                             "close"))
         (not (%http1-session-header-token-p response-headers
                                             "Connection"
                                             "close"))
         (or http11-p
             (%http1-session-header-token-p response-headers
                                           "Connection"
                                           "keep-alive"))
         (not (= status 101))
         (not (and (string-equal method "CONNECT")
                   (and (>= status 200) (< status 300)))))))

(defun %http1-session-response-for-request (request response)
  (let ((protocol-version (http-request-protocol-version request)))
    (if (string= protocol-version
                 (http-response-protocol-version response))
        response
        (make-http-response
         :protocol-version protocol-version
         :status (http-response-status response)
         :reason (http-response-reason response)
         :headers (http-response-headers response)
         :trailers (http-response-trailers response)
         :body (http-response-body response)))))

(defun %http1-session-response-stream-for-request (request response)
  (let ((protocol-version (http-request-protocol-version request)))
    (if (string= protocol-version
                 (http-response-stream-protocol-version response))
        response
        (make-http-response-stream
         :protocol-version protocol-version
         :status (http-response-stream-status response)
         :reason (http-response-stream-reason response)
         :headers (http-response-stream-headers response)
         :trailers (http-response-stream-trailers response)
         :body-function (http-response-stream-body-function response)
         :body-length (http-response-stream-body-length response)))))
