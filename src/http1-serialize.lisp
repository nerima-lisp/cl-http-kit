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

(defun %validate-http1-trailers (trailers operation)
  (dolist (trailer trailers)
    (when (%forbidden-trailer-field-name-p (http-header-name trailer))
      (error 'http-invalid-header
             :message "The field definition does not permit this HTTP trailer."
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
             (when (%forbidden-trailer-field-name-p name)
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
  (%http1-chunked-transfer-mode
   (%parse-http1-transfer-codings values :serialization "transfer-encoding")
   :serialization
   :http1-request-transfer-encoding))

(defun %http-request-target (request request-target)
  (let ((target
          (or request-target
              (http-request-target request)))
        (method (http-request-method request)))
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
    (when (and (string= target "*")
               (not (string= method "OPTIONS")))
      (error 'http-protocol-error
             :message "The asterisk-form request-target is valid only for OPTIONS."
             :operation :serialization
             :detail target))
    target))

(defun %serialization-effective-port (uri)
  (or (http-uri-port uri)
      (if (string= (http-uri-scheme uri) "https") 443 80)))

(defun %serialization-authorities-agree-p (left right)
  (and (string= (http-uri-host left) (http-uri-host right))
       (= (%serialization-effective-port left)
          (%serialization-effective-port right))))

(defun %absolute-http-request-target-p (target)
  (or (and (>= (length target) 7)
           (string-equal target "http://" :end1 7 :end2 7))
      (and (>= (length target) 8)
           (string-equal target "https://" :end1 8 :end2 8))))

(defun %validate-request-target-form (method target effective-authority)
  (cond
    ((string= method "CONNECT")
     (when (or (char= (char target 0) #\/)
               (string= target "*")
               (search "://" target))
       (error 'http-protocol-error
              :message "CONNECT request-targets must use authority-form."
              :operation :serialization
              :detail target))
     (let ((target-uri
             (make-http-uri :scheme (http-uri-scheme effective-authority)
                            :authority target
                            :path "/")))
       (unless (http-uri-port target-uri)
         (error 'http-protocol-error
                :message "A CONNECT authority-form target must include a port."
                :operation :serialization
                :detail target))
       (unless (%serialization-authorities-agree-p effective-authority
                                                   target-uri)
         (error 'http-invalid-header
                :message "The CONNECT authority must agree with the effective Host."
                :operation :serialization
                :name "host"
                :reason :host-authority-mismatch))))
    ((or (char= (char target 0) #\/)
         (string= target "*"))
     nil)
    ((%absolute-http-request-target-p target)
     (let ((target-uri (parse-http-uri target)))
       (unless (%serialization-authorities-agree-p effective-authority
                                                   target-uri)
         (error 'http-invalid-header
                :message "The absolute request-target authority must agree with the effective Host."
                :operation :serialization
                :name "host"
                :reason :host-authority-mismatch))))
    (t
     (error 'http-protocol-error
            :message "A non-CONNECT request-target must use origin-, absolute-, or asterisk-form."
            :operation :serialization
            :detail target))))

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
    (let* ((host-uri (and (= host-count 1)
                          (make-http-uri :scheme (http-uri-scheme uri)
                                         :authority (first host-values)
                                         :path "/")))
           (effective-authority (or host-uri uri)))
      (when (and host-uri
                 (not (%serialization-authorities-agree-p host-uri uri)))
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
        (%validate-request-target-form (http-request-method request)
                                       target effective-authority)
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
                  (if body-length-known-p body-length content-length)))))))

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
