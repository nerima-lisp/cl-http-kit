(in-package #:http-kit)

(defun %copy-http-uri (uri)
  (make-http-uri :scheme (http-uri-scheme uri)
                 :authority (http-uri-authority uri)
                 :path (http-uri-path uri)
                 :query (http-uri-query uri)))

(defun %coerce-uri (uri)
  (cond ((http-uri-p uri) (%copy-http-uri uri))
        ((stringp uri) (parse-http-uri uri))
        (t (error 'http-invalid-uri
                  :message "An HTTP request URI must be an HTTP-URI or string."
                  :operation :request
                  :input (type-of uri)))))

(defun %request-target-value-p (target)
  (and (stringp target)
       (string/= target "")
       (loop for character across target
             for code = (char-code character)
             always (and (>= code #x21)
                         (/= code #x7f)))))

(defun make-http-request
    (&key method uri request-target headers trailers body
          (protocol-version "HTTP/1.1"))
  (unless (and (stringp method) (%token-p method))
    (error 'http-protocol-error
           :message "HTTP request methods must be non-empty tokens."
           :operation :request
           :detail method))
  (when (and request-target
             (not (%request-target-value-p request-target)))
    (error 'http-protocol-error
           :message "HTTP request-target must be a non-empty string without controls or spaces."
           :operation :request
           :detail request-target))
  (unless (and (stringp protocol-version)
               (member protocol-version '("HTTP/1.0" "HTTP/1.1" "HTTP/2" "HTTP/3")
                       :test #'string=))
    (error 'http-protocol-error
           :message "HTTP request protocol version is unsupported."
           :operation :request
           :detail protocol-version))
  (%make-http-request :protocol-version protocol-version
                      :method (string-upcase method)
                      :uri (%coerce-uri uri)
                      :target (and request-target (copy-seq request-target))
                      :headers (%normalize-headers headers)
                      :trailers (%normalize-headers trailers)
                      :body (if body (%copy-octets body) (%empty-octets))))

(defun http-request-protocol-version (request)
  (%request-protocol-version request))

(defun http-request-method (request)
  (%request-method request))

(defun http-request-uri (request)
  (%copy-http-uri (%request-uri request)))

(defun http-request-headers (request)
  (mapcar #'%copy-http-header (%request-headers request)))

(defun http-request-body (request)
  (%copy-octets (%request-body request)))

(defun http-request-target (request)
  (or (and (%request-target request)
           (copy-seq (%request-target request)))
      (if (http-uri-query (%request-uri request))
          (concatenate 'string
                       (http-uri-path (%request-uri request))
                       "?"
                       (http-uri-query (%request-uri request)))
          (http-uri-path (%request-uri request)))))

(defun http-request-trailers (request)
  (mapcar #'%copy-http-header (%request-trailers request)))

(defun http-request-authority (request)
  (http-uri-authority (%request-uri request)))

(defun http-request-path (request)
  (http-uri-path (%request-uri request)))

(defun http-request-query (request)
  (http-uri-query (%request-uri request)))
