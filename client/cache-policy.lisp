(in-package #:http-kit/client)

(defun %cache-trim (value)
  (string-trim '(#\Space #\Tab) value))

(defun %cache-split-comma (value)
  (let ((parts nil)
        (start 0))
    (loop for separator = (position #\, value :start start)
          do (push (%cache-trim
                    (subseq value start (or separator (length value))))
                   parts)
          if separator
            do (setf start (1+ separator))
          else
            do (return (nreverse parts)))))

(defun %cache-split-assignment (value)
  (let ((position (position #\= value)))
    (if position
        (values (%cache-trim (subseq value 0 position))
                (%cache-trim (subseq value (1+ position))))
        (values (%cache-trim value) nil))))

(defun %cache-unquote (value)
  (if (and (>= (length value) 2)
           (char= (char value 0) #\")
           (char= (char value (1- (length value))) #\"))
      (subseq value 1 (1- (length value)))
      value))

(defun %cache-control-directives (headers)
  (let ((directives nil))
    (dolist (header (http-header-values headers "Cache-Control") directives)
      (dolist (directive (%cache-split-comma header))
        (multiple-value-bind (name value)
            (%cache-split-assignment directive)
          (push (cons (string-downcase name)
                      (and value (%cache-unquote value)))
                directives))))))

(defun %cache-directive (directives name)
  (cdr (assoc name directives :test #'string=)))

(defun %cache-valid-status-p (status)
  (member status '(200 203 204 206 300 301 404 410) :test #'=))

(defun %cache-vary-fields (headers)
  (let ((fields nil))
    (dolist (value (http-header-values headers "Vary") fields)
      (dolist (field (%cache-split-comma value))
        (when (string= field "")
          (return-from %cache-vary-fields nil))
        (if (string= field "*")
            (return-from %cache-vary-fields :star)
            (push (string-downcase field) fields))))
    (remove-duplicates (nreverse fields) :test #'string=)))

(defun %cache-request-vary-values (request fields)
  (when fields
    (mapcar (lambda (field)
              (cons field
                    (if (http-request-p request)
                        (http-header-values (http-request-headers request)
                                            field)
                        nil)))
            fields)))

(defun %cache-vary-match-p (entry request)
  (every (lambda (field-and-values)
           (equal (cdr field-and-values)
                  (http-header-values (http-request-headers request)
                                      (car field-and-values))))
         (http-cache-entry-request-vary-values entry)))

(defun %cache-freshness
    (response now)
  (let* ((headers (http-response-headers response))
         (directives (%cache-control-directives headers))
         (max-age (%cache-directive directives "max-age"))
         (max-age (and max-age
                       (%client-parse-integer max-age)))
         (expires (and (%cache-directive directives "no-store") nil))
         (expires (or expires
                      (let ((value (http-header-value headers "Expires")))
                        (and value (http-parse-date value)))))
         (date (let ((value (http-header-value headers "Date")))
                 (and value (http-parse-date value))))
         (last-modified (let ((value (http-header-value headers "Last-Modified")))
                          (and value (http-parse-date value))))
         (age (let ((value (http-header-value headers "Age")))
                (and value
                     (%client-parse-integer value)))))
    (cond
      ((%cache-directive directives "no-store")
       (values nil nil))
      ((or (and max-age (< max-age 0))
           (and (%cache-directive directives "s-maxage")
                (null max-age)))
       (values nil nil))
      (max-age
       (values t (+ now (max 0 (- max-age (or age 0))))))
      (expires
       (values t (max now (+ expires (- now (or date now))))))
      (last-modified
       (values t (+ now (min 86400
                             (max 0 (floor (- (or date now) last-modified)
                                           10))))))
      (t
       (values t now)))))
