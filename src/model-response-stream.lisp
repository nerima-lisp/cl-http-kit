(in-package #:http-kit)

(defun %ensure-http-response-stream-body-function (body-function)
  (unless (functionp body-function)
    (error 'http-protocol-error
           :message "HTTP response stream body-function must be a function."
           :operation :response
           :detail (type-of body-function)))
  body-function)

(defun %ensure-http-response-stream-body-length (body-length)
  (unless (or (null body-length)
              (and (integerp body-length) (>= body-length 0)))
    (error 'http-protocol-error
           :message "HTTP response stream body-length must be a non-negative integer or NIL."
           :operation :response
           :detail body-length))
  body-length)

(defun make-http-response-stream (&rest initargs)
  (let* ((status (getf initargs :status))
         (reason (getf initargs :reason))
         (headers (getf initargs :headers))
         (trailers (getf initargs :trailers))
         (body-function (getf initargs :body-function))
         (body-length (getf initargs :body-length))
         (protocol-version
           (if (member :protocol-version initargs)
               (getf initargs :protocol-version)
               "HTTP/1.1")))
    (%make-http-response-stream
     :protocol-version protocol-version
     :status status
     :reason reason
     :headers headers
     :trailers trailers
     :body-function (%ensure-http-response-stream-body-function
                     body-function)
     :body-length (%ensure-http-response-stream-body-length
                   body-length))))

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

(defun %response-stream-body-length-value (response)
  (%response-stream-body-length response))

(defun http-response-stream-body-length (response)
  (%response-stream-body-length-value response))
