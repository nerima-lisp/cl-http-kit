(in-package #:http-kit)

(defun %make-byte-builder ()
  (make-array 256 :element-type '(unsigned-byte 8)
              :adjustable t :fill-pointer 0))

(defun %builder-write-octets (builder octets)
  (loop for octet across octets do (vector-push-extend octet builder))
  builder)

(defun %builder-write-string (builder string &key (context :serialization))
  (%builder-write-octets builder (%string-octets string :context context)))

(defun %builder-crlf (builder)
  (vector-push-extend 13 builder)
  (vector-push-extend 10 builder)
  builder)

(defun %validated-content-length
    (headers body-length &key (body-length-known-p t))
  (let ((values (http-header-values headers "content-length")))
    (cond
      ((null values) (and body-length-known-p body-length))
      ((not (every #'%decimal-string-p values))
       (error 'http-invalid-header
              :message "Content-Length must be an ASCII decimal integer."
              :operation :serialization
              :name "content-length"
              :reason :value))
      ((not (every (lambda (value)
                     (= (%parse-decimal value) (%parse-decimal (first values))))
                   values))
       (error 'http-invalid-header
              :message "Duplicate Content-Length values must agree."
              :operation :serialization
              :name "content-length"
              :reason :duplicate))
      ((and body-length-known-p
            (/= (%parse-decimal (first values)) body-length))
       (error 'http-invalid-header
              :message "Content-Length does not match the request body."
              :operation :serialization
              :name "content-length"
              :reason :mismatch))
      (t (%parse-decimal (first values))))))

(defun %http1-forbidden-trailer-name-p (name)
  (member (string-downcase name)
          '("connection" "content-length" "host" "keep-alive"
            "proxy-authenticate" "proxy-authorization" "proxy-connection"
            "te" "trailer" "transfer-encoding" "upgrade")
          :test #'string=))

(defun %validate-http1-trailers (trailers operation)
  (dolist (trailer trailers)
    (when (%http1-forbidden-trailer-name-p (http-header-name trailer))
      (error 'http-invalid-header
             :message "A trailer field is forbidden by HTTP/1 framing rules."
             :operation operation
             :name (http-header-name trailer)
             :reason :forbidden-trailer))))

(defun %http1-trailer-names (trailers)
  (let ((names '()))
    (dolist (trailer trailers (nreverse names))
      (let ((name (http-header-name trailer)))
        (unless (member name names :test #'string-equal)
          (push name names))))))

(defun %parse-http1-trailer-declaration (values operation)
  (let ((names '()))
    (dolist (value values (nreverse names))
      (let ((start 0))
        (loop
          for comma = (position #\, value :start start)
          for end = (or comma (length value))
          for name = (%trim-ows (subseq value start end))
          do (when (zerop (length name))
               (error 'http-invalid-header
                      :message "A comma-separated Trailer field contains an empty item."
                      :operation operation
                      :name "trailer"
                      :reason :empty-item))
             (unless (%header-name-p name)
               (error 'http-invalid-header
                      :message "Trailer field names must be ASCII tokens."
                      :operation operation
                      :name "trailer"
                      :reason :value))
             (when (%http1-forbidden-trailer-name-p name)
               (error 'http-invalid-header
                      :message "A Trailer declaration contains a forbidden field name."
                      :operation operation
                      :name name
                      :reason :forbidden-trailer))
             (unless (member name names :test #'string-equal)
               (push name names))
             (if comma
                 (setf start (1+ comma))
                 (return)))))))

(defun %http1-ensure-trailer-declaration
    (headers trailers transfer-mode operation)
  (let* ((declared-values (http-header-values headers "trailer"))
         (declared-names (and declared-values
                              (%parse-http1-trailer-declaration
                               declared-values operation)))
         (actual-names (%http1-trailer-names trailers)))
    (when (and (null transfer-mode)
               (or declared-values actual-names))
      (error 'http-invalid-header
             :message "HTTP trailers require chunked transfer encoding."
             :operation operation
             :name "trailer"
             :reason :framing))
    (when (and declared-names
               (not (every (lambda (name)
                             (member name declared-names :test #'string-equal))
                           actual-names)))
      (error 'http-invalid-header
             :message "Every emitted HTTP trailer must be declared by Trailer."
             :operation operation
             :name "trailer"
             :reason :undeclared-trailer
             :detail (list :declared declared-names :actual actual-names)))
    (if (or declared-values (null actual-names))
        headers
        (append headers
                (list (make-http-header
                       "Trailer"
                       (format nil "~{~A~^, ~}" actual-names)))))))

(defun %request-transfer-encoding (values)
  (when values
    (let ((codings '()))
      (dolist (value values)
        (let ((start 0))
          (loop
            for comma = (position #\, value :start start)
            for end = (or comma (length value))
            for coding = (%trim-ows (subseq value start end))
            do (when (zerop (length coding))
                 (error 'http-invalid-header
                        :message "A comma-separated Transfer-Encoding contains an empty item."
                        :operation :serialization
                        :name "transfer-encoding"
                        :reason :empty-item))
               (push (string-downcase coding) codings)
               (if comma
                   (setf start (1+ comma))
                   (return)))))
      (setf codings (nreverse codings))
      (unless (and (consp codings)
                   (null (cdr codings))
                   (string= (first codings) "chunked"))
        (error 'http-unsupported-feature
               :message "Only a single HTTP/1.1 chunked transfer coding is supported."
               :operation :serialization
               :detail codings
               :feature :http1-request-transfer-encoding))
      :chunked)))

(defun %http-request-target (request request-target)
  (let ((target
          (or request-target
              (http-request-target request))))
    (unless (and (stringp target)
                 (plusp (length target))
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
      (dolist (header effective-headers)
        (%builder-write-string builder (http-header-name header)
                               :context :header-name)
        (%builder-write-string builder ": ")
        (%builder-write-string builder (http-header-content header)
                               :context :header-value)
        (%builder-crlf builder))
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
              (unless (zerop (length body))
                (%builder-write-string builder (format nil "~X" (length body)))
                (%builder-crlf builder)
                (%builder-write-octets builder body)
                (%builder-crlf builder))
            (%builder-write-string builder "0")
            (%builder-crlf builder)
              (dolist (trailer (http-request-trailers request))
                (%builder-write-string builder (http-header-name trailer)
                                       :context :header-name)
                (%builder-write-string builder ": ")
                (%builder-write-string builder (http-header-content trailer)
                                       :context :header-value)
                (%builder-crlf builder))
              (%builder-crlf builder))
            (%builder-write-octets builder body))
        (let ((result (make-array (length builder)
                                  :element-type '(unsigned-byte 8))))
          (replace result builder)
          result)))))
