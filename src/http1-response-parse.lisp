(in-package #:http-kit)

(defun %response-parse-error (message &optional detail)
  (error 'http-protocol-error
         :message message
         :operation :response-parse
         :detail detail))

(defun %response-header-error (message name reason &optional detail)
  (error 'http-invalid-header
         :message message
         :operation :response-parse
         :name name
         :reason reason
         :detail detail))

(defun %validate-response-parse-arguments
    (header-limit body-limit on-body-chunk on-information collect-body-p)
  (unless (and (integerp header-limit) (plusp header-limit))
    (%response-parse-error
     "The response header limit must be a positive integer."
     header-limit))
  (unless (and (integerp body-limit) (>= body-limit 0))
    (%response-parse-error
     "The response body limit must be a non-negative integer."
     body-limit))
  (when (and on-body-chunk (not (functionp on-body-chunk)))
    (%response-parse-error
     "ON-BODY-CHUNK must be a function or NIL."
     on-body-chunk))
  (when (and on-information (not (functionp on-information)))
    (%response-parse-error
     "ON-INFORMATION must be a function or NIL."
     on-information))
  (unless (member collect-body-p '(nil t))
    (%response-parse-error
     "COLLECT-BODY-P must be NIL or T."
     collect-body-p)))

(defun %validate-http-response-framing
    (protocol-version status transfer-encoding content-length)
  (when (and transfer-encoding content-length)
    (%response-header-error
     "Transfer-Encoding and Content-Length must not be combined."
     "content-length" :framing-conflict))
  (when (and (string= protocol-version "HTTP/1.0")
             transfer-encoding)
    (error 'http-unsupported-feature
           :message "HTTP/1.0 transfer codings are unsupported."
           :operation :response-parse
           :feature :http1-transfer-encoding
           :detail transfer-encoding))
  (when (= status 101)
    (when (and content-length (plusp content-length))
      (%response-header-error
       "A 101 Switching Protocols response cannot declare a non-zero Content-Length."
       "content-length" :forbidden))
    (when transfer-encoding
      (%response-header-error
       "A 101 Switching Protocols response cannot declare Transfer-Encoding."
       "transfer-encoding" :forbidden))))

(defun %make-header-only-http-response (protocol-version status reason headers)
  (make-http-response :protocol-version protocol-version
                      :status status :reason reason
                      :headers headers :trailers '()
                      :body (%empty-octets)))

(defun %http-response-bodyless-p
    (status head-response-p connect-response-p content-length transfer-encoding)
  (when (or head-response-p
            (and connect-response-p
                 (<= 200 status 299))
            (= status 204)
            (= status 205)
            (= status 304))
    (when (and (or (not head-response-p)
                   connect-response-p)
               content-length
               (plusp content-length))
      (%response-header-error
       "A bodyless HTTP/1.1 response cannot declare a non-zero Content-Length."
       "content-length" :forbidden))
    (when (and connect-response-p transfer-encoding)
      (%response-header-error
       "A successful CONNECT response cannot declare Transfer-Encoding."
       "transfer-encoding" :forbidden))
    t))

(defun %read-final-http-response
    (source protocol-version status reason headers absolute-deadline
     clock-function header-limit body-limit header-used
     head-response-p connect-response-p transfer-encoding content-length
     on-body-chunk collect-body-p)
  (let ((body (%empty-octets))
        (trailers '()))
    (cond
      ((%http-response-bodyless-p
        status head-response-p connect-response-p content-length transfer-encoding)
       (setf body (%empty-octets)))
      (transfer-encoding
       (multiple-value-setq (body trailers)
         (%read-chunked-body source absolute-deadline clock-function
                             header-limit body-limit header-used
                             :on-body-chunk on-body-chunk
                             :collect-body-p collect-body-p)))
      (content-length
       (setf body (%read-exact-body source content-length absolute-deadline
                                    clock-function body-limit
                                    :on-body-chunk on-body-chunk
                                    :collect-body-p collect-body-p)))
      (t
       (setf body (%read-close-body source absolute-deadline clock-function
                                    body-limit
                                    :on-body-chunk on-body-chunk
                                    :collect-body-p collect-body-p))))
    (make-http-response :protocol-version protocol-version
                        :status status :reason reason
                        :headers headers :trailers trailers
                        :body body)))
