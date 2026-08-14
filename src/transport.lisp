(in-package #:http-kit)

(defun %check-http-request (request)
  (unless (http-request-p request)
    (error 'http-protocol-error
           :message "A stream request must be an HTTP-REQUEST."
           :operation :transport
           :detail (type-of request)))
  request)

(defun %call-http-operation/cps (operation on-success &key on-error)
  (unless (functionp on-success)
    (error 'http-protocol-error
           :message "The success continuation must be a function."
           :operation :transport
           :detail on-success))
  (when (and on-error (not (functionp on-error)))
    (error 'http-protocol-error
           :message "The error continuation must be a function or NIL."
           :operation :transport
           :detail on-error))
  (let ((response
          (handler-case
              (funcall operation)
            (http-error (condition)
              (if on-error
                  (return-from %call-http-operation/cps
                    (funcall on-error condition))
                  (error condition))))))
    (funcall on-success response)))

(defun %validate-stream-boundary (open-stream close-stream)
  (unless (functionp open-stream)
    (error 'http-protocol-error
           :message "A stream request requires an :OPEN-STREAM function."
           :operation :transport
           :detail open-stream))
  (unless (or (null close-stream) (functionp close-stream))
    (error 'http-protocol-error
           :message "A stream request :CLOSE-STREAM must be a function."
           :operation :transport
           :detail close-stream))
  (values open-stream (or close-stream #'close)))
