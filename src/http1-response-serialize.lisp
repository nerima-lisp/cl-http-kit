(in-package #:http-kit)

(defun %serialize-response-content-length (headers)
  (let ((values (http-header-values headers "content-length")))
    (cond
      ((null values) nil)
      ((not (every #'%decimal-string-p values))
       (error 'http-invalid-header
              :message "Content-Length must be an ASCII decimal integer."
              :operation :serialization
              :name "content-length"
              :reason :value))
      ((not (every (lambda (value)
                     (= (%parse-decimal value)
                        (%parse-decimal (first values))))
                   values))
       (error 'http-invalid-header
              :message "Duplicate Content-Length values must agree."
              :operation :serialization
              :name "content-length"
              :reason :duplicate))
      (t (%parse-decimal (first values))))))

(defun %serialize-response-transfer-mode (headers)
  (%http1-chunked-transfer-mode
   (%parse-http1-transfer-codings
    (http-header-values headers "transfer-encoding")
    :serialization
    "transfer-encoding")
   :serialization
   :http1-response-transfer-encoding))

(defun %response-bodyless-status-p (status)
  (or (< status 200)
      (= status 204)
      (= status 205)
      (= status 304)))

(defun serialize-http-response (response &key request-method head-p)
  "Serialize one HTTP/1.0 or HTTP/1.1 response into an octet vector.

REQUEST-METHOD controls HEAD and successful CONNECT body semantics.
HEAD responses may carry a representation body in RESPONSE, but that
body is never written to the wire."
  (check-type response http-response)
  (unless (member head-p '(nil t))
    (error 'http-protocol-error
           :message "HEAD-P must be NIL or T."
           :operation :serialization
           :detail head-p))
  (when (and request-method (not (stringp request-method)))
    (error 'http-protocol-error
           :message "REQUEST-METHOD must be a string or NIL."
           :operation :serialization
           :detail request-method))
  (let* ((protocol-version (http-response-protocol-version response))
         (status (http-response-status response))
         (reason (http-response-reason response))
         (headers (http-response-headers response))
         (trailers (http-response-trailers response))
         (body (http-response-body response))
         (content-length (%serialize-response-content-length headers))
         (transfer-mode (%serialize-response-transfer-mode headers)))
    (multiple-value-bind (head-response-p status-bodyless-p connect-bodyless-p)
        (%validate-response-serialization
         protocol-version status headers trailers body content-length transfer-mode
         request-method head-p)
      (multiple-value-setq (headers transfer-mode content-length)
        (%prepare-response-serialization-headers
         protocol-version status headers trailers body content-length transfer-mode
         status-bodyless-p connect-bodyless-p))
      (let ((builder (%make-byte-builder)))
        (%builder-write-string builder protocol-version :context :protocol-version)
        (%builder-write-string builder " ")
        (%builder-write-string builder (princ-to-string status) :context :status)
        (%builder-write-string builder " ")
        (%builder-write-string builder reason :context :reason)
        (%builder-crlf builder)
        (%write-http1-headers builder headers)
        (%builder-crlf builder)
        (unless (or head-response-p status-bodyless-p connect-bodyless-p)
          (%write-http1-response-body builder body transfer-mode trailers))
        (let ((result (make-array (length builder)
                                  :element-type '(unsigned-byte 8))))
          (replace result builder)
          result)))))
