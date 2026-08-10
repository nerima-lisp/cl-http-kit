(in-package #:http-kit/http2)

(defun %h2-comma-items (values name)
  (let ((result '()))
    (dolist (value values (nreverse result))
      (let ((start 0))
        (loop
          for comma = (position #\, value :start start)
          for piece = (http-kit::%trim-ows
                       (subseq value start comma))
          do (when (zerop (length piece))
               (error 'http-kit:http-invalid-header
                      :message "An HTTP/2 comma-separated header contains an empty item."
                      :operation :http2-headers
                      :name name
                      :reason :empty-item))
             (push (string-downcase piece) result)
             (if comma
                 (setf start (1+ comma))
                 (return)))))))

(defun %h2-connection-specific-header-p (name)
  (member name *h2-connection-specific-header-names* :test #'string=))

(defun %h2-content-length (headers body-length &key (body-length-known-p t))
  (let ((values (http-kit:http-header-values headers "content-length")))
    (when values
      (unless (every #'http-kit::%decimal-string-p values)
        (error 'http-kit:http-invalid-header
               :message "HTTP/2 Content-Length must be an ASCII decimal integer."
               :operation :http2-headers
               :name "content-length"
               :reason :value))
      (let ((length (http-kit::%parse-decimal (first values))))
        (unless (every (lambda (value)
                         (= length (http-kit::%parse-decimal value)))
                       values)
          (error 'http-kit:http-invalid-header
                 :message "Duplicate HTTP/2 Content-Length values must agree."
                 :operation :http2-headers
                 :name "content-length"
                 :reason :duplicate))
        (unless (or (not body-length-known-p)
                    (= length body-length))
          (error 'http-kit:http-invalid-header
                 :message "HTTP/2 Content-Length does not match the request body."
                 :operation :http2-headers
                 :name "content-length"
                 :reason :mismatch))
        length))))

(defun %h2-classify-request-headers (headers)
  (let ((regular '())
        (host-values '()))
    (dolist (header headers)
      (let ((name (string-downcase (http-kit:http-header-name header)))
            (value (http-kit:http-header-content header)))
        (cond
          ((string= name "host")
           (push value host-values))
          ((%h2-connection-specific-header-p name)
           (error 'http-kit:http-invalid-header
                  :message "Connection-specific headers are forbidden in HTTP/2."
                  :operation :http2-headers
                  :name name
                  :reason :connection-specific))
          ((string= name "te")
           (unless (every (lambda (item) (string= item "trailers"))
                          (%h2-comma-items (list value) name))
             (error 'http-kit:http-unsupported-feature
                    :message "HTTP/2 only permits TE: trailers."
                    :operation :http2-headers
                    :feature :http2-te))
           (push (cons name value) regular))
          (t
           (push (cons name value) regular)))))
    (values (nreverse regular) host-values)))

(defun %h2-validate-host-values (host-values authority)
  (when (> (length host-values) 1)
    (error 'http-kit:http-invalid-header
           :message "An HTTP/2 request may contain only one Host field."
           :operation :http2-headers
           :name "host"
           :reason :duplicate))
  (when (some (lambda (value) (not (string-equal value authority))) host-values)
    (error 'http-kit:http-invalid-header
           :message "The HTTP/2 Host field must agree with :authority."
           :operation :http2-headers
           :name "host"
           :reason :authority-mismatch)))

(defun %h2-request-fields (request &key body-length (body-length-known-p t))
  (let* ((uri (http-kit:http-request-uri request))
         (method (http-kit:http-request-method request))
         (headers (http-kit:http-request-headers request))
         (body (http-kit:http-request-body request))
         (effective-body-length
           (and body-length-known-p
                (if (null body-length)
                    (length body)
                    body-length)))
         (authority (http-kit:http-uri-authority uri))
         (path (http-kit:http-uri-path uri))
         (query (http-kit:http-uri-query uri))
         (path-and-query (if query
                            (format nil "~A?~A" path query)
                            path)))
    (multiple-value-bind (regular host-values)
        (%h2-classify-request-headers headers)
      (%h2-validate-host-values host-values authority)
      (%h2-content-length headers effective-body-length
                          :body-length-known-p body-length-known-p)
      (unless (http-kit:http-header-present-p headers "content-length")
        (when (and effective-body-length
                   (plusp effective-body-length))
          (setf regular
                (append regular
                        (list (cons "content-length"
                                    (princ-to-string effective-body-length)))))))
      (if (string-equal method "CONNECT")
          ;; RFC 7540 section 8.3.1: a regular CONNECT request carries
          ;; only :method and :authority.  The URI still supplies the
          ;; authority and Host validation above keeps both spellings in
          ;; agreement.
          (append (list (cons ":method" method)
                        (cons ":authority" authority))
                  regular)
          (append (list (cons ":method" method)
                        (cons ":scheme" (http-kit:http-uri-scheme uri))
                        (cons ":authority" authority)
                        (cons ":path" path-and-query))
                  regular)))))

(defun %h2-header-list-size (fields)
  (reduce #'+ fields
          :key (lambda (field)
                 (+ 32 (length (car field)) (length (cdr field))))
          :initial-value 0))

(defun %h2-header-frames (block end-stream max-frame-size &optional (stream-id 1))
  (let ((frames '())
        (position 0)
        (length (length block))
        (first t))
    (loop while (< position length)
          do (let* ((size (min max-frame-size (- length position)))
                    (last (= (+ position size) length))
                    (payload (subseq block position (+ position size)))
                    (flags 0))
               (when (and first end-stream)
                 (setf flags (logior flags +http2-end-stream-flag+)))
               (when last
                 (setf flags (logior flags +http2-end-headers-flag+)))
               (push (%h2-frame-wire (if first +http2-headers-type+
                                         +http2-continuation-type+)
                                     flags stream-id payload)
                     frames)
               (incf position size)
               (setf first nil)))
    (nreverse frames)))
