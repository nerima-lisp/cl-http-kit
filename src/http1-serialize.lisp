(in-package #:http-kit)

(defun %request-transfer-encoding (values)
  (%http1-chunked-transfer-mode
   (%parse-http1-transfer-codings values :serialization "transfer-encoding")
   :serialization
   :http1-request-transfer-encoding))

(defun %http-request-target (request request-target)
  (let ((target
          (or request-target
              (http-request-target request))))
    (unless (and (stringp target)
                 (not (string= target ""))
                 (loop for character across target
                       for code = (char-code character)
                       always (and (>= code #x21)
                                   (/= code #x7f))))
      (error 'http-protocol-error
             :message "HTTP request-target must be a non-empty token without controls or spaces."
             :operation :serialization
             :detail target))
    target))

(defun %request-head-wire
    (request &key request-target body-length (body-length-known-p t))
  (check-type request http-request)
  (unless (or (not body-length-known-p)
              (and (integerp body-length) (>= body-length 0)))
    (error 'http-protocol-error
           :message "A known HTTP request body length must be a non-negative integer."
           :operation :serialization
           :detail body-length))
  (let* ((uri (http-request-uri request))
         (protocol-version (http-request-protocol-version request))
         (headers (http-request-headers request))
         (trailers (http-request-trailers request))
         (transfer-encoding (http-header-values headers "transfer-encoding"))
         (transfer-mode (%request-transfer-encoding transfer-encoding))
         (host-values (http-header-values headers "host"))
         (host-count (length host-values)))
    (unless (member protocol-version '("HTTP/1.0" "HTTP/1.1")
                    :test #'string=)
      (error 'http-unsupported-feature
             :message "Only HTTP/1.0 and HTTP/1.1 requests can be serialized on this boundary."
             :operation :serialization
             :feature :http1-request-version
             :detail protocol-version))
    (when (and (string= protocol-version "HTTP/1.0") transfer-mode)
      (error 'http-unsupported-feature
             :message "HTTP/1.0 transfer codings and trailers are unsupported."
             :operation :serialization
             :feature :http1-request-transfer-encoding
             :detail transfer-mode))
    (when (> host-count 1)
      (error 'http-invalid-header
             :message "An HTTP request may contain only one Host field."
             :operation :serialization
             :name "host"
             :reason :duplicate))
    (when (and (= host-count 1)
               (not (string-equal (first host-values)
                                  (http-uri-authority uri))))
      (error 'http-invalid-header
             :message "The HTTP Host field must agree with the URI authority."
             :operation :serialization
             :name "host"
             :reason :host-authority-mismatch))
    (let* ((effective-headers (if (zerop host-count)
                                  (append headers
                                          (list (make-http-header
                                                 "Host"
                                                 (http-uri-authority uri))))
                                  headers))
           (content-length-values (http-header-values effective-headers
                                                      "content-length"))
           (content-length
             (unless transfer-mode
               (%validated-content-length
                effective-headers body-length
                :body-length-known-p body-length-known-p)))
           (target (%http-request-target request request-target))
           (builder (%make-byte-builder)))
      (when (and transfer-mode content-length-values)
        (error 'http-invalid-header
               :message "Transfer-Encoding and Content-Length must not be combined."
               :operation :serialization
               :name "content-length"
               :reason :ambiguous-framing))
      (when (and (null transfer-mode) (null content-length-values))
        (if (and body-length-known-p
                 (null trailers)
                 (null (http-header-values effective-headers "trailer")))
            (progn
              (setf effective-headers
                    (append effective-headers
                            (list (make-http-header
                                   "Content-Length"
                                   (princ-to-string body-length))))
                    content-length-values
                    (list (princ-to-string body-length)))
              (setf content-length body-length))
            (progn
              (setf effective-headers
                    (append effective-headers
                            (list (make-http-header
                                   "Transfer-Encoding"
                                   "chunked")))
                    transfer-mode :chunked))))
      (setf effective-headers
            (%http1-ensure-trailer-declaration
             effective-headers trailers transfer-mode :serialization))
      (%builder-write-string builder (http-request-method request)
                             :context :method)
      (%builder-write-string builder " ")
      (%builder-write-string builder target :context :request-target)
      (%builder-write-string builder " ")
      (%builder-write-string builder protocol-version :context :protocol-version)
      (%builder-crlf builder)
      (%write-http1-headers builder effective-headers)
      (%builder-crlf builder)
      (let ((result (make-array (length builder)
                                :element-type '(unsigned-byte 8))))
        (replace result builder)
        (values result transfer-mode
                (if body-length-known-p body-length content-length))))))

(defun serialize-http-request (request &key request-target)
  (check-type request http-request)
  (let ((body (http-request-body request)))
    (multiple-value-bind (head transfer-mode expected-body-length)
        (%request-head-wire request
                            :request-target request-target
                            :body-length (length body))
      (declare (ignore expected-body-length))
      (let ((builder (%make-byte-builder)))
        (%builder-write-octets builder head)
        (if transfer-mode
            (progn
              (unless (zerop (array-total-size body))
                (%builder-write-string builder (format nil "~X" (length body)))
                (%builder-crlf builder)
                (%builder-write-octets builder body)
                (%builder-crlf builder))
            (%builder-write-string builder "0")
            (%builder-crlf builder)
              (%write-http1-trailers builder (http-request-trailers request))
              (%builder-crlf builder))
            (%builder-write-octets builder body))
        (let ((result (make-array (length builder)
                                  :element-type '(unsigned-byte 8))))
          (replace result builder)
          result)))))
