(in-package #:http-kit)

(defun %http1-session-write-handler-response
    (stream request response &key on-upgrade)
  (let ((stream-response-p
          (%http1-session-validate-handler-response response)))
    (multiple-value-bind (wire-response reusable-p)
        (if stream-response-p
            (%write-http1-response-stream
             stream
             request
             (%http1-session-response-stream-for-request request response))
            (%http1-session-write-static-response stream request response))
      (when stream-response-p
        (finish-output stream))
      (cond
        ((%http1-session-upgrade-response-p request wire-response)
         (values :upgrade
                 (%http1-session-notify-upgrade
                  stream
                  request
                  wire-response
                  on-upgrade)))
        (reusable-p
         (values :running nil))
        (t
         (values :close nil))))))
