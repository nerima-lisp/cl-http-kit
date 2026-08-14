(in-package #:http-kit)

(defun %http1-session-error (message detail)
  (error 'http-protocol-error
         :message message
         :operation :session
         :detail detail))

(defun %http1-session-write-continue (stream request)
  (write-sequence
   (serialize-http-response
    (make-http-response
     :protocol-version (http-request-protocol-version request)
     :status 100)
    :request-method (http-request-method request))
   stream)
  (finish-output stream))
