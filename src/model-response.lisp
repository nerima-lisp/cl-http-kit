(in-package #:http-kit)

(defun %default-reason (status)
  (or (cdr (assoc status *http-status-reasons*)) ""))

(defun %validate-http-response-status (status)
  (unless (and (integerp status) (<= 100 status 599))
    (error 'http-invalid-status
           :message "HTTP status must be an integer from 100 through 599."
           :operation :response
           :line status
           :code status)))

(defun %validate-http-response-reason (reason status)
  (when reason
    (unless (stringp reason)
      (error 'http-invalid-status
             :message "HTTP response reason must be a string."
             :operation :response
             :line reason
             :code status))
    (unless (%header-value-p reason)
      (error 'http-invalid-status
             :message "HTTP response reason cannot contain control characters."
             :operation :response
             :line reason
             :code status))))

(defun %validate-http-response-protocol-version (protocol-version)
  (unless (and (stringp protocol-version)
               (member protocol-version '("HTTP/1.0" "HTTP/1.1" "HTTP/2" "HTTP/3")
                       :test #'string=))
    (error 'http-protocol-error
           :message "HTTP response protocol version is unsupported."
           :operation :response
           :detail protocol-version)))

(defun make-http-response
    (&key status reason headers trailers body (protocol-version "HTTP/1.1"))
  (%validate-http-response-status status)
  (%validate-http-response-reason reason status)
  (%validate-http-response-protocol-version protocol-version)
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
