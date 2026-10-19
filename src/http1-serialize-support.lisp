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

(defun %write-http1-headers (builder headers)
  (dolist (header headers)
    (%builder-write-string builder (http-header-name header)
                           :context :header-name)
    (%builder-write-string builder ": ")
    (%builder-write-string builder (http-header-content header)
                           :context :header-value)
    (%builder-crlf builder)))

(defun %write-http1-trailers (builder trailers)
  (dolist (trailer trailers)
    (%builder-write-string builder (http-header-name trailer)
                           :context :header-name)
    (%builder-write-string builder ": ")
    (%builder-write-string builder (http-header-content trailer)
                           :context :header-value)
    (%builder-crlf builder)))

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

(defun %do-http1-comma-separated-items (values operation field-name receiver)
  (dolist (value values)
    (loop with start = 0
          for index from 0 to (length value)
          when (or (= index (length value))
                   (char= (char value index) #\,))
            do (let ((item (%trim-ows (subseq value start index))))
                 (when (string= item "")
                   (error 'http-invalid-header
                          :message
                          (format nil "A comma-separated ~A contains an empty item."
                                  field-name)
                          :operation operation
                          :name field-name
                          :reason :empty-item))
                 (funcall receiver item)
                 (setf start (1+ index))))))

(defun %parse-http1-transfer-codings (values operation field-name)
  (when values
    (let ((codings '()))
      (%do-http1-comma-separated-items
       values operation field-name
       (lambda (coding)
         (push (string-downcase coding) codings)))
      (nreverse codings))))

(defun %http1-chunked-transfer-mode (codings operation feature)
  (when codings
    (unless (and (consp codings)
                 (null (cdr codings))
                 (string= (first codings) "chunked"))
      (error 'http-unsupported-feature
             :message "Only a single HTTP/1.1 chunked transfer coding is supported."
             :operation operation
             :detail codings
             :feature feature))
    :chunked))

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
  (%validate-http1-trailers trailers operation)
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
