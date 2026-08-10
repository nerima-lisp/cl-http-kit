(in-package #:http-kit)

(defmacro with-http-deadline
    ((deadline timeout &key inherited
                           (clock-function #'%monotonic-time)
                           (kind :connect))
     &body body)
  `(let ((,deadline
           (http-deadline ,timeout
                          :deadline ,inherited
                          :clock-function ,clock-function)))
     (%check-deadline ,deadline ,clock-function ,kind)
     ,@body))

(defmacro %with-http-error-translation ((message operation) &body body)
  `(handler-case
       (progn ,@body)
     (http-error (condition)
       (error condition))
     (error (condition)
       (error 'http-connection-error
              :message ,message
              :operation ,operation
              :cause condition))))
