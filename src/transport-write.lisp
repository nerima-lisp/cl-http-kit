(in-package #:http-kit)

(defun %write-http-stream-string (stream string)
  (write-sequence (%string-octets string :context :serialization) stream))

(defun %write-http-stream-crlf (stream)
  (write-byte 13 stream)
  (write-byte 10 stream))

(defun %write-request-body-stream
    (stream request-body-function expected-body-length transfer-mode
            deadline clock-function)
  (let ((written 0))
    (loop
      (%check-deadline deadline clock-function :write)
      (let ((chunk (funcall request-body-function
                            +http1-request-body-chunk-size+)))
        (if (null chunk)
            (progn
              (when (and expected-body-length
                         (/= written expected-body-length))
                (error 'http-protocol-error
                       :message "The request body producer ended before its declared length."
                       :operation :body
                       :detail (list :expected expected-body-length
                                     :observed written)))
              (when (eq transfer-mode :chunked)
                (%write-http-stream-string stream "0")
                (%write-http-stream-crlf stream)
                (%write-http-stream-crlf stream))
              (return written))
            (let* ((validated-chunk (%check-request-body-chunk chunk))
                   (chunk-length (length validated-chunk))
                   (new-written (+ written chunk-length)))
              (when (and expected-body-length
                         (> new-written expected-body-length))
                (error 'http-protocol-error
                       :message "The request body producer exceeded its declared length."
                       :operation :body
                       :detail (list :expected expected-body-length
                                     :observed new-written)))
              (when (eq transfer-mode :chunked)
                (%write-http-stream-string stream
                                           (format nil "~X" chunk-length))
                (%write-http-stream-crlf stream)
                (write-sequence validated-chunk stream)
                (%write-http-stream-crlf stream))
              (unless (eq transfer-mode :chunked)
                (write-sequence validated-chunk stream))
              (setf written new-written)))))))

(defun %write-request-body-octets
    (stream request transfer-mode expected-body-length deadline clock-function)
  (let ((body (http-request-body request))
        (body-length (length (http-request-body request))))
    (when (and expected-body-length (/= body-length expected-body-length))
      (error 'http-protocol-error
             :message "The in-memory request body length changed after serialization."
             :operation :body
             :detail (list :expected expected-body-length
                           :observed body-length)))
    (%check-deadline deadline clock-function :write)
    (if (eq transfer-mode :chunked)
        (progn
          (unless (zerop body-length)
            (%write-http-stream-string stream (format nil "~X" body-length))
            (%write-http-stream-crlf stream)
            (write-sequence body stream)
            (%write-http-stream-crlf stream))
          (%write-http-stream-string stream "0")
          (%write-http-stream-crlf stream)
          (dolist (trailer (http-request-trailers request))
            (%write-http-stream-string stream (http-header-name trailer))
            (%write-http-stream-string stream ": ")
            (%write-http-stream-string stream (http-header-content trailer))
            (%write-http-stream-crlf stream))
          (%write-http-stream-crlf stream))
        (write-sequence body stream))
    body-length))

(defun %write-request-head
    (stream request request-target request-body-function request-body-length
            deadline clock-function)
  "Write REQUEST's head and return its transfer mode and expected length.
The request body is deliberately left unwritten."
  (let ((body-length (if request-body-function
                        request-body-length
                        (length (http-request-body request)))))
    (multiple-value-bind (head transfer-mode expected-body-length)
        (%request-head-wire
         request
         :request-target request-target
         :body-length body-length
         :body-length-known-p (if request-body-function
                                  (not (null request-body-length))
                                  t))
      (%check-deadline deadline clock-function :write)
      (write-sequence head stream)
      (finish-output stream)
      (values transfer-mode expected-body-length))))
