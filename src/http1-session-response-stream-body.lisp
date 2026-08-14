(in-package #:http-kit)

(defun %write-http1-response-stream-crlf (stream)
  (write-byte 13 stream)
  (write-byte 10 stream))

(defun %write-http1-response-stream-header (stream header)
  (write-sequence (%string-octets (http-header-name header)
                                  :context :header-name)
                  stream)
  (write-byte 58 stream)
  (write-byte 32 stream)
  (write-sequence (%string-octets (http-header-content header)
                                  :context :header-value)
                  stream)
  (%write-http1-response-stream-crlf stream))

(defun %write-http1-response-stream-head
    (stream protocol-version status reason headers)
  (write-sequence (%string-octets protocol-version
                                  :context :protocol-version)
                  stream)
  (write-byte 32 stream)
  (write-sequence (%string-octets (princ-to-string status)
                                  :context :status)
                  stream)
  (write-byte 32 stream)
  (write-sequence (%string-octets reason :context :reason) stream)
  (%write-http1-response-stream-crlf stream)
  (dolist (header headers)
    (%write-http1-response-stream-header stream header))
  (%write-http1-response-stream-crlf stream))

(defun %write-http1-response-stream-chunk
    (stream chunk transfer-mode)
  (unless (zerop (array-total-size chunk))
    (when (eq transfer-mode :chunked)
      (write-sequence (%string-octets (format nil "~X" (length chunk)))
                      stream)
      (%write-http1-response-stream-crlf stream))
    (write-sequence chunk stream)
    (when (eq transfer-mode :chunked)
      (%write-http1-response-stream-crlf stream))))

(defun %write-http1-response-stream-final-chunk (stream trailers)
  (write-byte 48 stream)
  (%write-http1-response-stream-crlf stream)
  (dolist (trailer trailers)
    (%write-http1-response-stream-header stream trailer))
  (%write-http1-response-stream-crlf stream))

(defun %response-stream-bodyless-p
    (status-bodyless-p connect-bodyless-p)
  (or status-bodyless-p connect-bodyless-p))

(defun %response-stream-trailers-requested-p (headers trailers)
  (or trailers
      (http-header-values headers "trailer")))

(defun %response-stream-append-header (headers name content)
  (append headers (list (make-http-header name content))))

(defun %response-stream-add-content-length (headers length)
  (values (%response-stream-append-header
           headers
           "Content-Length"
           (princ-to-string length))
          length))

(defun %response-stream-enable-chunked (headers)
  (values (%response-stream-append-header
           headers
           "Transfer-Encoding"
           "chunked")
          :chunked))

(defun %validate-response-stream-version (protocol-version)
  (unless (member protocol-version '("HTTP/1.0" "HTTP/1.1")
                  :test #'string=)
    (error 'http-unsupported-feature
           :message "Only HTTP/1.0 and HTTP/1.1 response streams can be sent on this boundary."
           :operation :serialization
           :feature :http1-response-version
           :detail protocol-version)))

(defun %validate-response-stream-framing
    (headers trailers body-length content-length transfer-mode
     status head-response-p status-bodyless-p connect-bodyless-p)
  (let ((bodyless-p
          (%response-stream-bodyless-p
           status-bodyless-p connect-bodyless-p)))
    (when (and content-length transfer-mode)
      (error 'http-invalid-header
             :message "Transfer-Encoding and Content-Length must not be combined."
             :operation :serialization
             :name "content-length"
             :reason :ambiguous-framing))
    (when (and transfer-mode bodyless-p)
      (error 'http-invalid-header
             :message "A bodyless HTTP response cannot declare Transfer-Encoding."
             :operation :serialization
             :name "transfer-encoding"
             :reason :forbidden))
    (when (and bodyless-p body-length (plusp body-length))
      (error 'http-protocol-error
             :message "A bodyless HTTP response cannot declare a non-empty streamed body."
             :operation :serialization
             :detail body-length))
    (when (and (%response-stream-trailers-requested-p headers trailers)
               (or head-response-p bodyless-p))
      (error 'http-protocol-error
             :message "HTTP trailers cannot be sent without a response body."
             :operation :serialization
             :detail status)))
  (%validate-http1-trailers trailers :serialization))

(defun %validate-response-stream-content-length
    (status body-length content-length status-bodyless-p connect-bodyless-p)
  (if (%response-stream-bodyless-p status-bodyless-p connect-bodyless-p)
      (when (and content-length (plusp content-length))
        (error 'http-invalid-header
               :message "This bodyless HTTP response may only declare Content-Length: 0."
               :operation :serialization
               :name "content-length"
               :reason :forbidden))
      (when (and content-length body-length (/= status 304)
                 (/= content-length body-length))
        (error 'http-invalid-header
               :message "Content-Length does not match the streamed response length."
               :operation :serialization
               :name "content-length"
               :reason :mismatch))))

(defun %validate-response-stream-serialization
    (protocol-version status headers trailers body-length content-length
     transfer-mode request-method)
  (multiple-value-bind (head-response-p status-bodyless-p connect-bodyless-p)
      (%response-head-flags status request-method nil)
    (%validate-response-stream-version protocol-version)
    (%validate-response-stream-framing
     headers trailers body-length content-length transfer-mode
     status head-response-p status-bodyless-p connect-bodyless-p)
    (%validate-response-stream-content-length
     status body-length content-length status-bodyless-p connect-bodyless-p)
    (values head-response-p status-bodyless-p connect-bodyless-p)))

(defun %prepare-response-stream-trailers
    (headers trailers content-length transfer-mode)
  (when (and (%response-stream-trailers-requested-p headers trailers)
             content-length)
    (error 'http-invalid-header
           :message "Response trailers require chunked transfer encoding, not Content-Length."
           :operation :serialization
           :name "content-length"
           :reason :framing))
  (if (and (null transfer-mode)
           (%response-stream-trailers-requested-p headers trailers))
      (%response-stream-enable-chunked headers)
      (values headers transfer-mode)))

(defun %ensure-response-stream-transfer-support
    (protocol-version transfer-mode)
  (when (and (string= protocol-version "HTTP/1.0") transfer-mode)
    (error 'http-unsupported-feature
           :message "HTTP/1.0 transfer codings and trailers are unsupported."
           :operation :serialization
           :feature :http1-response-transfer-encoding
           :detail transfer-mode)))

(defun %default-response-stream-framing
    (protocol-version status headers body-length content-length transfer-mode
     head-response-p status-bodyless-p connect-bodyless-p)
  (cond
    (transfer-mode
     (values headers transfer-mode content-length))
    (content-length
     (values headers transfer-mode content-length))
    ((= status 205)
     (multiple-value-bind (new-headers new-content-length)
         (%response-stream-add-content-length headers 0)
       (values new-headers transfer-mode new-content-length)))
    ((%response-stream-bodyless-p status-bodyless-p connect-bodyless-p)
     (values headers transfer-mode content-length))
    (body-length
     (multiple-value-bind (new-headers new-content-length)
         (%response-stream-add-content-length headers body-length)
       (values new-headers transfer-mode new-content-length)))
    ((and (string= protocol-version "HTTP/1.1")
          (not head-response-p))
     (multiple-value-bind (new-headers new-transfer-mode)
         (%response-stream-enable-chunked headers)
       (values new-headers new-transfer-mode content-length)))
    (t
     (values headers transfer-mode content-length))))

(defun %prepare-response-stream-headers
    (protocol-version status headers trailers body-length content-length
     transfer-mode head-response-p status-bodyless-p connect-bodyless-p)
  (multiple-value-setq (headers transfer-mode)
    (%prepare-response-stream-trailers
     headers trailers content-length transfer-mode))
  (%ensure-response-stream-transfer-support protocol-version transfer-mode)
  (setf headers
        (%http1-ensure-trailer-declaration
         headers trailers transfer-mode :serialization))
  (multiple-value-setq (headers transfer-mode content-length)
    (%default-response-stream-framing
     protocol-version status headers body-length content-length transfer-mode
     head-response-p status-bodyless-p connect-bodyless-p))
  (values headers transfer-mode content-length))

(defun %response-stream-check-produced-length
    (produced expected condition message &rest initargs)
  (when (and expected (> produced expected))
    (apply #'error condition
           :message message
           :operation :serialization
           initargs)))

(defun %response-stream-finalize-produced-length
    (produced expected condition message &rest initargs)
  (when (and expected (/= produced expected))
    (apply #'error condition
           :message message
           :operation :serialization
           initargs)))

(defun %write-http1-response-stream-body-chunk
    (stream chunk transfer-mode produced body-length content-length)
  (let ((octets (%copy-octets chunk :allow-list nil)))
    (incf produced (length octets))
    (%response-stream-check-produced-length
     produced body-length 'http-protocol-error
     "The streamed response body exceeded body-length."
     :detail body-length)
    (%response-stream-check-produced-length
     produced content-length 'http-invalid-header
     "The streamed response body exceeded Content-Length."
     :name "content-length"
     :reason :mismatch)
    (%write-http1-response-stream-chunk stream octets transfer-mode)
    produced))

(defun %write-http1-response-stream-body
    (stream body-function body-length content-length transfer-mode trailers)
  (let ((produced 0))
    (loop
      for chunk = (funcall body-function)
      do (if (null chunk)
             (return)
             (setf produced
                   (%write-http1-response-stream-body-chunk
                    stream chunk transfer-mode produced
                    body-length content-length))))
    (%response-stream-finalize-produced-length
     produced body-length 'http-protocol-error
     "The streamed response body ended before body-length."
     :detail (list :expected body-length :observed produced))
    (%response-stream-finalize-produced-length
     produced content-length 'http-invalid-header
     "Content-Length does not match the streamed response body."
     :name "content-length"
     :reason :mismatch)
    (when (eq transfer-mode :chunked)
      (%write-http1-response-stream-final-chunk stream trailers))))
