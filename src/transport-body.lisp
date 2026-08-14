(in-package #:http-kit)

(defconstant +http1-request-body-chunk-size+ 65536)

(defun %validate-request-body-stream-options
    (request request-body-function request-body-length)
  (when (and request-body-function (not (functionp request-body-function)))
    (error 'http-protocol-error
           :message "The request body producer must be a function or NIL."
           :operation :body
           :detail request-body-function))
  (when (and request-body-length
             (or (not (integerp request-body-length))
                 (minusp request-body-length)))
    (error 'http-protocol-error
           :message "The request body length must be a non-negative integer or NIL."
           :operation :body
           :detail request-body-length))
  (when (and (null request-body-function) request-body-length)
    (error 'http-protocol-error
           :message "A request body length requires a request body producer."
           :operation :body
           :detail request-body-length))
  (when request-body-function
    (unless (zerop (array-total-size (http-request-body request)))
      (error 'http-protocol-error
             :message "A streaming request must not also carry an in-memory body."
             :operation :body
             :detail (length (http-request-body request)))))
  (values request-body-function request-body-length))

(defun %check-request-body-chunk (chunk)
  (unless (and (arrayp chunk)
               (= (array-rank chunk) 1)
               (not (stringp chunk))
               (loop for octet across chunk
                     always (and (integerp octet) (<= 0 octet #xff))))
    (error 'http-protocol-error
           :message "A request body producer must return a one-dimensional octet vector or NIL."
           :operation :body
           :detail (type-of chunk)))
  (when (zerop (array-total-size chunk))
    (error 'http-protocol-error
           :message "A request body producer must not return an empty chunk."
           :operation :body
           :detail chunk))
  (when (> (length chunk) +http1-request-body-chunk-size+)
    (error 'http-protocol-error
           :message "A request body producer returned more than the advertised maximum chunk size."
           :operation :body
           :detail (length chunk)))
  chunk)
