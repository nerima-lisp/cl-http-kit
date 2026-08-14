(in-package #:http-kit)

(defun %write-http1-response-stream (stream request response)
  (let* ((protocol-version (http-response-stream-protocol-version response))
         (status (http-response-stream-status response))
         (reason (http-response-stream-reason response))
         (headers (http-response-stream-headers response))
         (trailers (http-response-stream-trailers response))
         (body-function (http-response-stream-body-function response))
         (body-length (http-response-stream-body-length response))
         (request-method (http-request-method request))
         (head-response-p (string-equal request-method "HEAD"))
         (connect-response-p (string-equal request-method "CONNECT"))
         (status-bodyless-p (%response-bodyless-status-p status))
         (connect-bodyless-p (and connect-response-p
                                  (<= 200 status 299)))
         (content-length (%serialize-response-content-length headers))
         (transfer-mode (%serialize-response-transfer-mode headers)))
    (multiple-value-setq (head-response-p status-bodyless-p connect-bodyless-p)
      (%validate-response-stream-serialization
       protocol-version status headers trailers body-length content-length
       transfer-mode request-method))
    (multiple-value-setq (headers transfer-mode content-length)
      (%prepare-response-stream-headers
       protocol-version status headers trailers body-length content-length
       transfer-mode head-response-p status-bodyless-p connect-bodyless-p))
    (let* ((wire-response
             (make-http-response
              :protocol-version protocol-version
              :status status
              :reason reason
              :headers headers
              :trailers trailers))
           (bodyless-p (or head-response-p
                           status-bodyless-p
                           connect-bodyless-p))
           (framed-p (or bodyless-p content-length transfer-mode)))
      (%write-http1-response-stream-head
       stream protocol-version status reason headers)
      (unless bodyless-p
        (%write-http1-response-stream-body
         stream body-function body-length content-length transfer-mode trailers))
      (values wire-response
              (and framed-p
                   (%http1-session-response-reusable-p request wire-response))))))
