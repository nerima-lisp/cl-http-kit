(in-package #:http-kit)

(defun parse-http-response (input &key timeout deadline
                                      max-header-bytes max-body-bytes
                                      request-method
                                      on-body-chunk
                                      on-information
                                      (collect-body-p t)
                                      (clock-function #'%monotonic-time))
  "Parse one binary HTTP/1.x response from INPUT.

INPUT may be an octet vector or a binary stream.  A response with
Transfer-Encoding: chunked is decoded, Content-Length is enforced, and an
otherwise unframed response is read until connection close.  Supply
REQUEST-METHOD as \"HEAD\" for the bodyless response semantics of HEAD."
  (let* ((source (%make-byte-source-for input))
         (absolute-deadline (http-deadline timeout :deadline deadline
                                           :clock-function clock-function))
         (header-limit (or max-header-bytes *default-max-header-bytes*))
         (body-limit (or max-body-bytes *default-max-body-bytes*))
         (head-response-p (and (stringp request-method)
                               (string-equal request-method "HEAD")))
         (connect-response-p (and (stringp request-method)
                                  (string-equal request-method "CONNECT")))
         (header-used 0))
    (unless (and (integerp header-limit) (plusp header-limit))
      (error 'http-protocol-error
             :message "The response header limit must be a positive integer."
             :operation :response-parse
             :detail header-limit))
    (unless (and (integerp body-limit) (>= body-limit 0))
      (error 'http-protocol-error
             :message "The response body limit must be a non-negative integer."
             :operation :response-parse
             :detail body-limit))
    (when (and on-body-chunk (not (functionp on-body-chunk)))
      (error 'http-protocol-error
             :message "ON-BODY-CHUNK must be a function or NIL."
             :operation :response-parse
             :detail on-body-chunk))
    (when (and on-information (not (functionp on-information)))
      (error 'http-protocol-error
             :message "ON-INFORMATION must be a function or NIL."
             :operation :response-parse
             :detail on-information))
    (unless (member collect-body-p '(nil t))
      (error 'http-protocol-error
             :message "COLLECT-BODY-P must be NIL or T."
             :operation :response-parse
             :detail collect-body-p))
    (loop
      (multiple-value-bind (status-line updated-bytes)
          (%read-crlf-line source absolute-deadline clock-function header-limit header-used)
        (setf header-used updated-bytes)
        (multiple-value-bind (protocol-version status reason)
            (%parse-status-line status-line)
          (multiple-value-bind (headers final-header-bytes)
              (%read-response-headers source absolute-deadline clock-function
                                      header-limit header-used)
            (setf header-used final-header-bytes)
            (let ((transfer-encoding (%response-transfer-encoding headers))
                  (content-length (%response-content-length headers)))
              (when (and transfer-encoding content-length)
                (error 'http-invalid-header
                       :message "Transfer-Encoding and Content-Length must not be combined."
                       :operation :response-parse
                       :name "content-length"
                       :reason :framing-conflict))
              (when (and (string= protocol-version "HTTP/1.0")
                         transfer-encoding)
                (error 'http-unsupported-feature
                       :message "HTTP/1.0 transfer codings are unsupported."
                       :operation :response-parse
                       :feature :http1-transfer-encoding
                       :detail transfer-encoding))
              (when (= status 101)
                (when (and content-length (plusp content-length))
                  (error 'http-invalid-header
                         :message "A 101 Switching Protocols response cannot declare a non-zero Content-Length."
                         :operation :response-parse
                         :name "content-length"
                         :reason :forbidden))
                (when transfer-encoding
                  (error 'http-invalid-header
                         :message "A 101 Switching Protocols response cannot declare Transfer-Encoding."
                         :operation :response-parse
                         :name "transfer-encoding"
                         :reason :forbidden))
                (return
                  (make-http-response :protocol-version protocol-version
                                      :status status :reason reason
                                      :headers headers :trailers '()
                                      :body (%empty-octets))))
              ;; Informational responses do not carry a response body;
              ;; notify the caller and continue until the final response.
              ;; The 101 case above is a protocol switch rather than an
              ;; interim response.
              (if (< status 200)
                  (when on-information
                    (funcall on-information
                             (make-http-response
                              :protocol-version protocol-version
                              :status status :reason reason
                              :headers headers :trailers '()
                              :body (%empty-octets))))
                  (let ((body (%empty-octets))
                        (trailers '()))
                    (cond
                      ((or head-response-p
                           (and connect-response-p
                                (<= 200 status 299))
                           (= status 204)
                           (= status 205)
                           (= status 304))
                       (when (and (or (not head-response-p)
                                      connect-response-p)
                                  content-length
                                  (plusp content-length))
                         (error 'http-invalid-header
                                :message "A bodyless HTTP/1.1 response cannot declare a non-zero Content-Length."
                                :operation :response-parse
                                :name "content-length"
                                :reason :forbidden))
                       (when (and connect-response-p transfer-encoding)
                         (error 'http-invalid-header
                                :message "A successful CONNECT response cannot declare Transfer-Encoding."
                                :operation :response-parse
                                :name "transfer-encoding"
                                :reason :forbidden))
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
                       ;; HTTP/1.1 close-delimited responses are valid framing.
                       ;; The transport owns closing the stream, so this parser
                       ;; consumes until EOF even when Connection: close is omitted.
                       (setf body (%read-close-body source absolute-deadline clock-function
                                                    body-limit
                                                    :on-body-chunk on-body-chunk
                                                    :collect-body-p collect-body-p))))
                    (return
                      (make-http-response :protocol-version protocol-version
                                          :status status :reason reason
                                          :headers headers :trailers trailers
                                          :body body)))))))))))
