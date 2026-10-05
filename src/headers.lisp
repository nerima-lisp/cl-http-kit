(in-package #:http-kit)

(defstruct (http-header (:constructor %make-http-header))
  name
  content)

(defun %sensitive-header-p (name)
  (member (string-downcase name) *redacted-header-names* :test #'string=))

(defun make-http-header (name content)
  (unless (and (stringp name) (%header-name-p name))
    (error 'http-invalid-header
           :message "Header names must be non-empty ASCII tokens."
           :operation :header
           :name name
           :reason :name))
  (unless (stringp content)
    (error 'http-invalid-header
           :message "Header values must be strings."
           :operation :header
           :name name
           :reason :value-type))
  (unless (%header-value-p content)
    (error 'http-invalid-header
           :message "Header values cannot contain controls or CRLF."
           :operation :header
           :name name
           :reason :control))
  (%make-http-header :name name :content (%trim-ows content)))

(defun %copy-http-header (header)
  (make-http-header (http-header-name header)
                    (http-header-content header)))

(define-http-diagnostic-printer (http-header header stream)
  (:string (http-header-name header))
  (:string ": ")
  (:string (if (%sensitive-header-p (http-header-name header))
               "<redacted>"
               (%bounded-diagnostic (http-header-content header)))))

(defun %normalize-headers (headers)
  (mapcar (lambda (header)
            (cond ((http-header-p header) (%copy-http-header header))
                  ((and (consp header) (stringp (car header)))
                   (let ((tail (cdr header)))
                     (cond ((stringp tail)
                            (make-http-header (car header) tail))
                           ((and (consp tail)
                                 (stringp (car tail))
                                 (null (cdr tail)))
                            (make-http-header (car header) (car tail)))
                           (t
                            (error 'http-invalid-header
                                   :message "Headers must be HTTP-HEADER values or name/value pairs."
                                   :operation :header
                                   :reason :type)))))
                  (t
                   (error 'http-invalid-header
                          :message "Headers must be HTTP-HEADER values or name/value pairs."
                          :operation :header
                          :reason :type))))
          (or headers '())))

(defun %header-name-equal-p (left right)
  (string-equal left right))

(defun http-header-values (headers name)
  (unless (and (stringp name) (%header-name-p name))
    (error 'http-invalid-header
           :message "Header lookup requires an ASCII token name."
           :operation :header-lookup
           :name name
           :reason :name))
  (loop for header in headers
        when (%header-name-equal-p name (http-header-name header))
          collect (http-header-content header)))

(defun http-header-value (headers name &optional default)
  (let ((values (http-header-values headers name)))
    (if values
        (first values)
        default)))

(defun http-header-present-p (headers name)
  (not (null (http-header-values headers name))))
