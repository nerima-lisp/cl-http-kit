(in-package #:http-kit)

(defun %request-authority-uri (authority name &optional (scheme "http"))
  (handler-case
      (make-http-uri :scheme scheme :authority authority :path "/")
    (http-invalid-uri (condition)
      (%request-header-error
       "An HTTP authority is not valid."
       name :value
       (list authority (http-error-message condition))))))

(defun %request-effective-port (uri)
  (or (http-uri-port uri)
      (if (string= (http-uri-scheme uri) "https") 443 80)))

(defun %request-authorities-agree-p (left right)
  (and (string= (http-uri-host left) (http-uri-host right))
       (= (%request-effective-port left)
          (%request-effective-port right))))

(defun %request-target-uri
    (method version target host-values default-authority)
  (when (> (length host-values) 1)
    (%request-header-error
     "An HTTP request may contain only one Host field."
     "host" :duplicate host-values))
  (when (and default-authority (not (stringp default-authority)))
    (%request-parse-error
     "DEFAULT-AUTHORITY must be a string or NIL."
     default-authority))
  (let* ((host-uri (and host-values
                        (%request-authority-uri (first host-values) "host")))
         (default-uri (and default-authority
                           (%request-authority-uri default-authority
                                                   "default-authority"))))
    (when (and (string= version "HTTP/1.1") (null host-uri))
      (%request-header-error
       "HTTP/1.1 requests must contain a Host field."
       "host" :missing))
    (cond
      ((string= method "CONNECT")
       (when (or (char= (char target 0) #\/) (string= target "*")
                 (search "://" target))
         (%request-parse-error
          "CONNECT request-targets must use authority-form."
          target))
       (let ((target-uri (%request-authority-uri target "request-target")))
         (unless (http-uri-port target-uri)
           (%request-parse-error
            "A CONNECT authority-form target must include a port."
            target))
         (when (and host-uri
                    (not (%request-authorities-agree-p host-uri target-uri)))
           (%request-header-error
            "The HTTP Host field must agree with the CONNECT authority."
            "host" :host-authority-mismatch target))
         target-uri))
      ((char= (char target 0) #\/)
       (let ((authority-uri (or host-uri default-uri)))
         (unless authority-uri
           (%request-parse-error
            "An origin-form request requires Host or DEFAULT-AUTHORITY."
            target))
         (let* ((query-position (position #\? target))
                (path (if query-position
                          (subseq target 0 query-position)
                          target))
                (query (and query-position
                            (subseq target (1+ query-position)))))
           (make-http-uri :scheme "http"
                          :authority (http-uri-authority authority-uri)
                          :path path
                          :query query))))
      ((string= target "*")
       (unless (string= method "OPTIONS")
         (%request-parse-error
          "The asterisk-form request-target is only valid for OPTIONS."
          method))
       (let ((authority-uri (or host-uri default-uri)))
         (unless authority-uri
           (%request-parse-error
            "An asterisk-form request requires Host or DEFAULT-AUTHORITY."
            target))
         (make-http-uri :scheme "http"
                        :authority (http-uri-authority authority-uri)
                        :path "/")))
      ((search "://" target)
       (let ((target-uri (parse-http-uri target)))
         (when (and host-uri
                    (not (%request-authorities-agree-p host-uri target-uri)))
           (setf host-uri
                 (%request-authority-uri
                  (first host-values) "host"
                  (http-uri-scheme target-uri)))
           (unless (%request-authorities-agree-p host-uri target-uri)
             (%request-header-error
              "The HTTP Host field must agree with the absolute request-target authority."
              "host" :host-authority-mismatch target)))
         target-uri))
      (t
       (%request-parse-error
        "An HTTP request-target must use origin-, absolute-, authority-, or asterisk-form."
        target)))))
