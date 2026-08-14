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

(defun %cache-directive-present-p (directives name)
  (not (null (assoc name directives :test #'string=))))

(defun %cache-nonnegative-seconds (value)
  (let ((number (%client-parse-integer value :allow-sign-p nil)))
    (and number (>= number 0) number)))

(defun %cache-request-directives (request)
  (%cache-control-directives (http-request-headers request)))

(defun %cache-request-no-store-p (request)
  (%cache-directive-present-p (%cache-request-directives request) "no-store"))

(defun %cache-request-no-cache-p (request)
  (let* ((directives (%cache-request-directives request))
         (max-age (%cache-nonnegative-seconds
                   (%cache-directive directives "max-age"))))
    (or (%cache-directive-present-p directives "no-cache")
        (and max-age (zerop max-age))
        (some (lambda (value)
                (member "no-cache"
                        (%cache-split-comma value)
                        :test #'string-equal))
              (http-header-values (http-request-headers request) "Pragma")))))

(defun %cache-request-min-fresh (request)
  (%cache-nonnegative-seconds
   (%cache-directive (%cache-request-directives request) "min-fresh")))

(defun %cache-request-max-stale (request)
  (let* ((directives (%cache-request-directives request))
         (name "max-stale"))
    (when (%cache-directive-present-p directives name)
      (let ((value (%cache-directive directives name)))
        (if (null value)
            :unbounded
            (%cache-nonnegative-seconds value))))))

(defun %cache-response-must-revalidate-p (headers)
  (let ((directives (%cache-control-directives headers)))
    (or (%cache-directive-present-p directives "must-revalidate")
        (%cache-directive-present-p directives "proxy-revalidate"))))

(defun %cache-valid-status-p (status)
  (member status '(200 203 204 206 300 301 302 303 307 308
                   404 405 410 414 501)
          :test #'=))

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
         (max-age (%cache-nonnegative-seconds
                   (%cache-directive directives "max-age")))
         (no-store (%cache-directive-present-p directives "no-store"))
         (no-cache (%cache-directive-present-p directives "no-cache"))
         (expires (let ((value (http-header-value headers "Expires")))
                    (and value (http-parse-date value))))
         (date (let ((value (http-header-value headers "Date")))
                 (and value (http-parse-date value))))
         (last-modified (let ((value (http-header-value headers "Last-Modified")))
                          (and value (http-parse-date value))))
         (age (%cache-nonnegative-seconds
               (http-header-value headers "Age")))
         (current-age (max (or age 0)
                           (max 0 (- now (or date now))))))
    (cond
      (no-store
       (values nil nil))
      (no-cache
       (values t now))
      (max-age
       (values t (+ now (max 0 (- max-age current-age)))))
      (expires
       (let ((lifetime (max 0 (- expires (or date now)))))
         (values t (+ now (max 0 (- lifetime current-age))))))
      ((and last-modified date (< last-modified date))
       (values t (+ now
                    (max 0
                         (- (min 86400
                                  (floor (- date last-modified) 10))
                            current-age)))))
      (t
       (values t now)))))
