(in-package #:http-kit)

(defun %parse-request-line (line)
  (let* ((first-space (position #\Space line))
         (second-space (and first-space
                            (position #\Space line :start (1+ first-space)))))
    (unless (and first-space
                 second-space
                 (null (position #\Space line :start (1+ second-space))))
      (%request-parse-error
       "An HTTP request-line must contain METHOD, request-target, and version separated by single spaces."
       line))
    (let ((method (subseq line 0 first-space))
          (target (subseq line (1+ first-space) second-space))
          (version (subseq line (1+ second-space))))
      (unless (%token-p method)
        (%request-parse-error
         "An HTTP request method must be a non-empty token."
         method))
      (unless (%request-target-value-p target)
        (%request-parse-error
         "An HTTP request-target must be non-empty and contain no controls or spaces."
         target))
      (unless (member version '("HTTP/1.0" "HTTP/1.1") :test #'string=)
        (%request-parse-error
         "Only HTTP/1.0 and HTTP/1.1 request versions are supported."
         version))
      (values (string-upcase method) target version))))

(defun %parse-request-header-line (line)
  (when (and (not (string= line ""))
             (find (char line 0) '(#\Space #\Tab)))
    (%request-parse-error
     "Obsolete folded request headers are not accepted."
     line))
  (let ((colon (position #\: line)))
    (unless colon
      (%request-header-error
       "An HTTP request header must contain a colon."
       nil :missing-colon line))
    (let ((name (subseq line 0 colon))
          (content (%trim-ows (subseq line (1+ colon)))))
      (handler-case
          (make-http-header name content)
        (http-invalid-header (condition)
          (error 'http-invalid-header
                 :message (http-error-message condition)
                 :operation :request-parse
                 :name (or (http-invalid-header-name condition) name)
                 :reason (http-invalid-header-reason condition)))))))

(defun %read-request-headers
    (source deadline clock-function max-header-bytes header-used)
  (let ((headers '())
        (bytes header-used))
    (loop
      (multiple-value-bind (line updated-bytes)
          (%read-crlf-line source deadline clock-function max-header-bytes bytes
                           :operation :request-parse)
        (setf bytes updated-bytes)
        (if (string= line "")
            (return (values (nreverse headers) bytes))
            (push (%parse-request-header-line line) headers))))))

(defun %request-content-length (headers)
  (let ((values (http-header-values headers "content-length")))
    (cond
      ((null values) nil)
      ((not (every #'%decimal-string-p values))
       (%request-header-error
        "Content-Length must be an ASCII decimal integer."
        "content-length" :value values))
      ((not (every (lambda (value)
                     (= (%parse-decimal value)
                        (%parse-decimal (first values))))
                   values))
       (%request-header-error
        "Duplicate Content-Length values must agree."
        "content-length" :duplicate values))
      (t (%parse-decimal (first values))))))

(defun %request-transfer-mode (headers)
  (%http1-chunked-transfer-mode
   (%parse-http1-transfer-codings
    (http-header-values headers "transfer-encoding")
    :request-parse
    "transfer-encoding")
   :request-parse
   :http1-request-transfer-encoding))

(defun %request-expectation (headers protocol-version)
  (let ((values (http-header-values headers "expect")))
    (when values
      (let ((expectations '()))
        (%do-http1-comma-separated-items
         values :request-parse "expect"
         (lambda (expectation)
           (push (string-downcase expectation) expectations)))
        (setf expectations (nreverse expectations))
        (unless (and (string= protocol-version "HTTP/1.1")
                     (every (lambda (expectation)
                              (string= expectation "100-continue"))
                            expectations))
          (error 'http-unsupported-feature
                 :message "Only HTTP/1.1 Expect: 100-continue is supported."
                 :operation :request-parse
                 :feature :http1-expectation
                 :detail expectations))
        t))))
