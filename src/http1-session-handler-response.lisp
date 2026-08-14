(in-package #:http-kit)

(defun %http1-session-handler-response-status (response stream-response-p)
  (if stream-response-p
      (http-response-stream-status response)
      (http-response-status response)))

(defun %http1-session-validate-handler-response (response)
  (let ((stream-response-p (http-response-stream-p response)))
    (unless (or (http-response-p response)
                stream-response-p)
      (%http1-session-error
       "HTTP/1 session handler must return an HTTP response or response stream."
       (type-of response)))
    (let ((status (%http1-session-handler-response-status response
                                                         stream-response-p)))
      (when (and (>= status 100)
                 (< status 200)
                 (/= status 101))
        (%http1-session-error
         "HTTP/1 session handlers cannot return interim responses."
         status)))
    stream-response-p))

(defun %http1-session-upgrade-response-p (request wire-response)
  (or (= (http-response-status wire-response) 101)
      (and (string-equal (http-request-method request) "CONNECT")
           (>= (http-response-status wire-response) 200)
           (< (http-response-status wire-response) 300))))

(defun %http1-session-notify-upgrade (stream request wire-response on-upgrade)
  (when on-upgrade
    (not (null (funcall on-upgrade
                        stream
                        request
                        wire-response)))))

(defun %http1-session-write-static-response (stream request response)
  (let* ((wire-response
           (%http1-session-response-for-request request response))
         (wire
           (serialize-http-response
            wire-response
            :request-method
            (http-request-method request))))
    (write-sequence wire stream)
    (finish-output stream)
    (values wire-response
            (%http1-session-response-reusable-p request wire-response))))
