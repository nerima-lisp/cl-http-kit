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

(defparameter +http1-forbidden-trailer-name-set+
  (let ((table (make-hash-table :test #'equal)))
    (dolist (name '("connection" "content-length" "host" "keep-alive"
                    "proxy-authenticate" "proxy-authorization"
                    "proxy-connection" "te" "trailer"
                    "transfer-encoding" "upgrade")
             table)
      (setf (gethash name table) t))))

(defun %http1-normalize-name (name)
  (string-downcase name))

(defun %make-http1-name-set ()
  (make-hash-table :test #'equal))

(defun %http1-name-set-add (name names)
  (setf (gethash (%http1-normalize-name name) names) t))

(defun %http1-name-set-contains-p (name names)
  (nth-value 1 (gethash (%http1-normalize-name name) names)))

(defun %http1-unique-names (names)
  (let ((seen (%make-http1-name-set))
        (unique '()))
    (dolist (name names (nreverse unique))
      (unless (%http1-name-set-contains-p name seen)
        (%http1-name-set-add name seen)
        (push name unique)))))

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

(defun %http1-header-token-p (headers name token &key operation)
  (let ((matched nil))
    (%do-http1-comma-separated-items
     (http-header-values headers name)
     operation
     name
     (lambda (item)
       (when (string-equal item token)
         (setf matched t))))
    matched))

(defun %http1-forbidden-trailer-name-p (name)
  (%http1-name-set-contains-p name +http1-forbidden-trailer-name-set+))

(defun %validate-http1-trailers (trailers operation)
  (dolist (trailer trailers)
    (when (%http1-forbidden-trailer-name-p (http-header-name trailer))
      (error 'http-invalid-header
             :message "A trailer field is forbidden by HTTP/1 framing rules."
             :operation operation
             :name (http-header-name trailer)
             :reason :forbidden-trailer))))

(defun %http1-trailer-names (trailers)
  (%http1-unique-names (mapcar #'http-header-name trailers)))

(defun %parse-http1-trailer-declaration (values operation)
  (let ((names '())
        (seen (%make-http1-name-set)))
    (%do-http1-comma-separated-items
     values operation "trailer"
     (lambda (name)
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
       (unless (%http1-name-set-contains-p name seen)
         (%http1-name-set-add name seen)
         (push name names))))
    (nreverse names)))

(defun %http1-ensure-trailer-declaration
    (headers trailers transfer-mode operation)
  (let* ((declared-values (http-header-values headers "trailer"))
         (declared-names (and declared-values
                              (%parse-http1-trailer-declaration
                               declared-values operation)))
         (declared-name-set (and declared-names
                                 (%make-http1-name-set)))
         (actual-names (%http1-trailer-names trailers)))
    (dolist (name declared-names)
      (%http1-name-set-add name declared-name-set))
    (when (and (null transfer-mode)
               (or declared-values actual-names))
      (error 'http-invalid-header
             :message "HTTP trailers require chunked transfer encoding."
             :operation operation
             :name "trailer"
             :reason :framing))
    (when (and declared-names
               (not (every (lambda (name)
                             (%http1-name-set-contains-p name
                                                         declared-name-set))
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
