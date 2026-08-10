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
       (plusp (length target))
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

(defun %default-reason (status)
  (or (cdr (assoc status *http-status-reasons*)) ""))

(defun make-http-response
    (&key status reason headers trailers body (protocol-version "HTTP/1.1"))
  (unless (and (integerp status) (<= 100 status 599))
    (error 'http-invalid-status
           :message "HTTP status must be an integer from 100 through 599."
           :operation :response
           :line status
           :code status))
  (when (and reason (not (stringp reason)))
    (error 'http-invalid-status
           :message "HTTP response reason must be a string."
           :operation :response
           :line reason
           :code status))
  (when (and reason (not (%header-value-p reason)))
    (error 'http-invalid-status
           :message "HTTP response reason cannot contain control characters."
           :operation :response
           :line reason
           :code status))
  (unless (and (stringp protocol-version)
               (member protocol-version '("HTTP/1.0" "HTTP/1.1" "HTTP/2" "HTTP/3")
                       :test #'string=))
    (error 'http-protocol-error
           :message "HTTP response protocol version is unsupported."
           :operation :response
           :detail protocol-version))
  (%make-http-response :protocol-version protocol-version
                       :status status
                       :reason (or reason (%default-reason status))
                       :headers (%normalize-headers headers)
                       :trailers (%normalize-headers trailers)
                       :body (if body (%copy-octets body) (%empty-octets))))

(defun http-response-protocol-version (response)
  (%response-protocol-version response))

(defun http-response-status (response)
  (%response-status response))

(defun http-response-reason (response)
  (%response-reason response))

(defun http-response-headers (response)
  (mapcar #'%copy-http-header (%response-headers response)))

(defun http-response-trailers (response)
  (mapcar #'%copy-http-header (%response-trailers response)))

(defun http-response-body (response)
  (%copy-octets (%response-body response)))

(defun make-http-response-stream
    (&key status reason headers trailers body-function body-length
          (protocol-version "HTTP/1.1"))
  "Create a response whose body is produced incrementally by BODY-FUNCTION.

BODY-FUNCTION is called with no arguments until it returns NIL.  Each
non-NIL value must be a one-dimensional octet vector.  BODY-LENGTH, when
supplied, is the exact representation length and allows the HTTP/1 server to
use Content-Length; otherwise HTTP/1.1 uses chunked transfer coding."
  (unless (functionp body-function)
    (error 'http-protocol-error
           :message "HTTP response stream body-function must be a function."
           :operation :response
           :detail (type-of body-function)))
  (unless (or (null body-length)
              (and (integerp body-length) (>= body-length 0)))
    (error 'http-protocol-error
           :message "HTTP response stream body-length must be a non-negative integer or NIL."
           :operation :response
           :detail body-length))
  (let ((response (make-http-response
                   :protocol-version protocol-version
                   :status status
                   :reason reason
                   :headers headers
                   :trailers trailers)))
    (%make-http-response-stream
     :protocol-version (http-response-protocol-version response)
     :status (http-response-status response)
     :reason (http-response-reason response)
     :headers (http-response-headers response)
     :trailers (http-response-trailers response)
     :body-function body-function
     :body-length body-length)))

(defun http-response-stream-protocol-version (response)
  (%response-stream-protocol-version response))

(defun http-response-stream-status (response)
  (%response-stream-status response))

(defun http-response-stream-reason (response)
  (%response-stream-reason response))

(defun http-response-stream-headers (response)
  (mapcar #'%copy-http-header (%response-stream-headers response)))

(defun http-response-stream-trailers (response)
  (mapcar #'%copy-http-header (%response-stream-trailers response)))

(defun http-response-stream-body-function (response)
  (%response-stream-body-function response))

(defun http-response-stream-body-length (response)
  (%response-stream-body-length response))

(defmethod print-object ((request http-request) stream)
  (print-unreadable-object (request stream :type t)
    (let ((uri (%request-uri request)))
      (format stream "~A ~A://~A~A~A headers=~D body-bytes=~D"
            (%request-method request)
            (http-uri-scheme uri)
            (http-uri-authority uri)
            (http-uri-path uri)
            (if (http-uri-query uri) "?<redacted>" "")
            (length (%request-headers request))
            (length (%request-body request))))))

(defmethod print-object ((response http-response) stream)
  (print-unreadable-object (response stream :type t)
    (format stream "~A status=~D headers=~D trailers=~D body-bytes=~D"
            (%response-protocol-version response)
            (%response-status response)
            (length (%response-headers response))
            (length (%response-trailers response))
            (length (%response-body response)))))
