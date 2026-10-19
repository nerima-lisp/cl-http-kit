(in-package #:http-kit)

(defun %response-header-error (message name reason &optional detail)
  (error 'http-invalid-header
         :message message
         :operation :response-parse
         :name name
         :reason reason
         :detail detail))

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

