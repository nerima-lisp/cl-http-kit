(in-package #:http-kit)

(defun %response-head-flags (status request-method head-p)
  (let* ((head-response-p (or head-p
                              (and request-method
                                   (string-equal request-method "HEAD"))))
         (connect-response-p (and request-method
                                  (string-equal request-method "CONNECT")))
         (status-bodyless-p (%response-bodyless-status-p status))
         (connect-bodyless-p (and connect-response-p
                                  (<= 200 status 299))))
    (values head-response-p status-bodyless-p connect-bodyless-p)))

(defun %validate-response-serialization
    (protocol-version status headers trailers body content-length transfer-mode
     request-method head-p)
  (multiple-value-bind (head-response-p status-bodyless-p connect-bodyless-p)
      (%response-head-flags status request-method head-p)
    (unless (member protocol-version '("HTTP/1.0" "HTTP/1.1")
                    :test #'string=)
      (error 'http-unsupported-feature
             :message "Only HTTP/1.0 and HTTP/1.1 responses can be serialized on this boundary."
             :operation :serialization
             :feature :http1-response-version
             :detail protocol-version))
    (when (and content-length transfer-mode)
      (error 'http-invalid-header
             :message "Transfer-Encoding and Content-Length must not be combined."
             :operation :serialization
             :name "content-length"
             :reason :ambiguous-framing))
    (when (and transfer-mode
               (or status-bodyless-p connect-bodyless-p))
      (error 'http-invalid-header
             :message "A bodyless HTTP response cannot declare Transfer-Encoding."
             :operation :serialization
             :name "transfer-encoding"
             :reason :forbidden))
    (when (and (or status-bodyless-p connect-bodyless-p)
               (plusp (array-total-size body)))
      (error 'http-protocol-error
             :message "A bodyless HTTP response cannot carry a response body."
             :operation :serialization
             :detail status))
    (when (and (or trailers
                   (http-header-values headers "trailer"))
               (or head-response-p status-bodyless-p connect-bodyless-p))
      (error 'http-protocol-error
             :message "HTTP trailers cannot be sent without a response body."
             :operation :serialization
             :detail status))
    (%validate-http1-trailers trailers :serialization)
    (cond
      ((and content-length (= status 304))
       nil)
      ((or status-bodyless-p connect-bodyless-p)
       (when (and content-length (plusp content-length))
         (error 'http-invalid-header
                :message "This bodyless HTTP response may only declare Content-Length: 0."
                :operation :serialization
                :name "content-length"
                :reason :forbidden)))
      ((and content-length (/= content-length (length body)))
       (error 'http-invalid-header
              :message "Content-Length does not match the response representation body."
              :operation :serialization
              :name "content-length"
              :reason :mismatch)))
    (values head-response-p status-bodyless-p connect-bodyless-p)))

(defun %prepare-response-serialization-headers
    (protocol-version status headers trailers body content-length transfer-mode
     status-bodyless-p connect-bodyless-p)
  (when (and (or trailers
                 (http-header-values headers "trailer"))
             content-length)
    (error 'http-invalid-header
           :message "Response trailers require chunked transfer encoding, not Content-Length."
           :operation :serialization
           :name "content-length"
           :reason :framing))
  (when (and (null transfer-mode)
             (or trailers
                 (http-header-values headers "trailer")))
    (setf transfer-mode :chunked
          headers (append headers
                          (list (make-http-header
                                 "Transfer-Encoding" "chunked")))))
  (when (and (string= protocol-version "HTTP/1.0") transfer-mode)
    (error 'http-unsupported-feature
           :message "HTTP/1.0 transfer codings and trailers are unsupported."
           :operation :serialization
           :feature :http1-response-transfer-encoding
           :detail transfer-mode))
  (setf headers
        (%http1-ensure-trailer-declaration
         headers trailers transfer-mode :serialization))
  (when (and (null transfer-mode)
             (null content-length)
             (null (http-header-values headers "trailer"))
             (or (= status 205)
                 (not (or status-bodyless-p connect-bodyless-p))))
    (let ((wire-length (if (= status 205) 0 (length body))))
      (setf content-length wire-length
            headers (append headers
                            (list (make-http-header
                                   "Content-Length"
                                   (princ-to-string wire-length)))))))
  (values headers transfer-mode content-length))

(defun %write-http1-response-body (builder body transfer-mode trailers)
  (if (eq transfer-mode :chunked)
      (progn
        (unless (zerop (array-total-size body))
          (%builder-write-string builder (format nil "~X" (length body)))
          (%builder-crlf builder)
          (%builder-write-octets builder body)
          (%builder-crlf builder))
        (%builder-write-string builder "0")
        (%builder-crlf builder)
        (%write-http1-trailers builder trailers)
        (%builder-crlf builder))
      (%builder-write-octets builder body)))
