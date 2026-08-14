(in-package #:http-kit)

(defun %request-parse-error (message &optional detail)
  (error 'http-protocol-error
         :message message
         :operation :request-parse
         :detail detail))

(defun %request-header-error (message name reason &optional detail)
  (error 'http-invalid-header
         :message message
         :operation :request-parse
         :name name
         :reason reason
         :detail detail))

(defun parse-http-request
    (input &key timeout deadline max-header-bytes max-body-bytes
            default-authority on-body-chunk (collect-body-p t)
            on-expect-continue (clock-function #'%monotonic-time)
            allow-eof-p)
  "Parse one HTTP/1.0 or HTTP/1.1 request from INPUT.

INPUT may be an octet vector or a binary stream.  Request bodies are
framed by Content-Length or HTTP/1.1 chunked transfer coding; an
unframed request is treated as having an empty body because requests do
not use close-delimited framing.  When ON-EXPECT-CONTINUE is a function,
it receives a metadata-only request before an expected request body is
read."
  (let* ((source (%make-byte-source-for input :operation :request-parse))
         (absolute-deadline (http-deadline timeout :deadline deadline
                                           :clock-function clock-function))
         (header-limit (or max-header-bytes *default-max-header-bytes*))
         (body-limit (or max-body-bytes *default-max-body-bytes*)))
    (unless (and (integerp header-limit) (plusp header-limit))
      (%request-parse-error
       "The request header limit must be a positive integer."
       header-limit))
    (unless (and (integerp body-limit) (>= body-limit 0))
      (%request-parse-error
       "The request body limit must be a non-negative integer."
       body-limit))
    (when (and on-body-chunk (not (functionp on-body-chunk)))
      (%request-parse-error
       "ON-BODY-CHUNK must be a function or NIL."
       on-body-chunk))
    (when (and on-expect-continue (not (functionp on-expect-continue)))
      (%request-parse-error
       "ON-EXPECT-CONTINUE must be a function or NIL."
       on-expect-continue))
    (unless (member collect-body-p '(nil t))
      (%request-parse-error
       "COLLECT-BODY-P must be NIL or T."
       collect-body-p))
    (unless (member allow-eof-p '(nil t))
      (%request-parse-error
       "ALLOW-EOF-P must be NIL or T."
       allow-eof-p))
    (multiple-value-bind (request-line header-used)
        (%read-crlf-line source absolute-deadline clock-function header-limit 0
                         :operation :request-parse
                         :allow-eof-p allow-eof-p)
      (if (eq request-line :eof)
          nil
          (multiple-value-bind (method target version)
              (%parse-request-line request-line)
            (multiple-value-bind (headers final-header-bytes)
                (%read-request-headers source absolute-deadline clock-function
                                       header-limit header-used)
              (let* ((host-values (http-header-values headers "host"))
                     (uri (%request-target-uri method version target host-values
                                               default-authority))
                     (transfer-mode (%request-transfer-mode headers))
                     (content-length (%request-content-length headers))
                     (expect-continue-p (%request-expectation headers version)))
                (when (and transfer-mode content-length)
                  (%request-header-error
                   "Transfer-Encoding and Content-Length must not be combined."
                   "content-length" :framing-conflict))
                (when (and (string= version "HTTP/1.0") transfer-mode)
                  (error 'http-unsupported-feature
                         :message "HTTP/1.0 transfer codings are unsupported."
                         :operation :request-parse
                         :feature :http1-request-transfer-encoding
                         :detail transfer-mode))
                (when (and expect-continue-p
                           on-expect-continue
                           (or transfer-mode
                               (and content-length (plusp content-length))))
                  (funcall on-expect-continue
                           (make-http-request
                            :method method
                            :protocol-version version
                            :uri uri
                            :request-target target
                            :headers headers
                            :trailers '()
                            :body (%empty-octets))))
                (let (body trailers)
                  (cond
                    ((eq transfer-mode :chunked)
                     (multiple-value-setq (body trailers)
                       (%read-request-chunked-body
                        source absolute-deadline clock-function header-limit body-limit
                        final-header-bytes
                        :on-body-chunk on-body-chunk
                        :collect-body-p collect-body-p)))
                    (content-length
                     (setf body (%read-request-exact-body
                                 source content-length absolute-deadline clock-function
                                 body-limit :on-body-chunk on-body-chunk
                                 :collect-body-p collect-body-p)
                           trailers '()))
                    (t
                     (setf body (%empty-octets)
                           trailers '())))
                  (make-http-request :method method
                                     :protocol-version version
                                     :uri uri
                                     :request-target target
                                     :headers headers
                                     :trailers trailers
                                     :body body)))))))))
