(in-package #:http-kit)

(defun %parse-status-line (line)
  (let ((version (and (>= (length line) 8)
                      (subseq line 0 8))))
    (unless (and version
                 (or (string= version "HTTP/1.0")
                     (string= version "HTTP/1.1"))
                 (>= (length line) 12)
                 (char= (char line 8) #\Space)
                 (%decimal-string-p (subseq line 9 12))
                 (or (= (length line) 12)
                     (char= (char line 12) #\Space)))
      (error 'http-invalid-status
             :message "The HTTP status line is malformed."
             :operation :response-parse
             :line line))
    (let ((code (parse-integer line :start 9 :end 12))
          (reason (if (> (length line) 13)
                      (subseq line 13)
                      "")))
      (unless (<= 100 code 599)
        (error 'http-invalid-status
               :message "The HTTP status code is outside the supported range."
               :operation :response-parse
               :line line
               :code code))
      (values version code reason))))

(defun %parse-response-header-line (line)
  (when (and (not (string= line ""))
             (find (char line 0) '(#\Space #\Tab)))
    (error 'http-invalid-header
           :message "Obsolete folded response headers are not accepted."
           :operation :response-parse
           :reason :obs-fold))
  (let ((colon (position #\: line)))
    (unless colon
      (error 'http-invalid-header
             :message "A response header line must contain a colon."
             :operation :response-parse
             :reason :missing-colon))
    (make-http-header (subseq line 0 colon)
                      (subseq line (1+ colon)))))

(defun %read-response-headers
    (source deadline clock-function max-header-bytes max-fields header-used)
  (let ((headers '())
        (bytes header-used)
        (field-count 0))
    (loop
      (multiple-value-bind (line updated-bytes)
          (%read-crlf-line source deadline clock-function max-header-bytes bytes)
        (setf bytes updated-bytes)
        (if (string= line "")
            (return (values (nreverse headers) bytes))
            (progn
              (incf field-count)
              (%check-limit :fields field-count max-fields
                            :operation :response-parse)
              (push (%parse-response-header-line line) headers)))))))

(defun %split-comma-values (values)
  (%parse-http1-transfer-codings values :response-parse "transfer-encoding"))

(defun %response-content-length (headers max-body-bytes)
  (let ((values (http-header-values headers "content-length")))
    (cond
      ((null values) nil)
      ((not (every #'%decimal-string-p values))
       (error 'http-invalid-header
              :message "Content-Length must be an ASCII decimal integer."
              :operation :response-parse
              :name "content-length"
              :reason :value))
      ((not (every (lambda (value)
                     (= (%parse-decimal-limited value max-body-bytes
                                                :operation :response-parse)
                        (%parse-decimal-limited (first values) max-body-bytes
                                                :operation :response-parse)))
                   values))
       (error 'http-invalid-header
              :message "Duplicate Content-Length values must agree."
              :operation :response-parse
              :name "content-length"
              :reason :duplicate))
      (t (%parse-decimal-limited (first values) max-body-bytes
                                 :operation :response-parse)))))

(defun %response-transfer-encoding (headers)
  (let ((values (http-header-values headers "transfer-encoding")))
    (when values
      (let ((codings (%split-comma-values values)))
        (unless (and (consp codings)
                     (null (cdr codings))
                     (string= (first codings) "chunked"))
          (error 'http-unsupported-feature
                 :message "Only a single HTTP/1.1 chunked transfer coding is supported."
                 :operation :response-parse
                 :feature :http1-transfer-encoding
                 :detail codings))
        :chunked))))
