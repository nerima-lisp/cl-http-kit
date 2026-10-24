(in-package #:http-kit/test-core)

(defun http1-request-wire-for-test (request-line &rest fields)
  (apply #'concatenate-octets
         (ascii request-line)
         (mapcar #'ascii fields)))

(defmacro with-http1-request ((request wire &rest arguments) &body body)
  `(let ((,request (parse-http-request ,wire ,@arguments)))
     ,@body))

(defmacro with-http1-response-roundtrip ((response parsed &rest arguments) &body body)
  `(let ((,parsed (parse-http-response
                   (serialize-http-response ,response ,@arguments))))
     ,@body))

(defun %http1-session-output-octets (path)
  (with-open-file (stream path
                          :element-type '(unsigned-byte 8))
    (let ((result (make-array 0
                              :element-type '(unsigned-byte 8)
                              :adjustable t
                              :fill-pointer 0)))
      (loop for byte = (read-byte stream nil :eof)
            until (eq byte :eof)
            do (vector-push-extend byte result))
      result)))

(defun %run-http1-session-from-file (request-bytes handler &rest arguments)
  (let* ((base-name (format nil "cl-http-kit-http1-session-~A" (gensym)))
         (input-path (merge-pathnames
                      (make-pathname :name base-name :type "input")
                      (uiop:temporary-directory)))
         (output-path (merge-pathnames
                       (make-pathname :name base-name :type "output")
                       (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (stream input-path
                                   :direction :output
                                   :element-type '(unsigned-byte 8)
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-sequence request-bytes stream))
           (let ((result
                   (with-open-file (input input-path
                                          :element-type '(unsigned-byte 8))
                     (with-open-file (output output-path
                                            :direction :output
                                            :element-type '(unsigned-byte 8)
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
                       (let ((session-stream (make-two-way-stream input output)))
                         (multiple-value-list
                          (apply #'serve-http1-session
                                 session-stream
                                 handler
                                 arguments)))))))
             (values (first result)
                     (second result)
                     (%http1-session-output-octets output-path))))
      (http-kit::%with-http-cleanup (delete-file input-path))
      (http-kit::%with-http-cleanup (delete-file output-path)))))

(defun ensure-http1-session-run (request-bytes
                                 handler
                                 expected-count
                                 expected-reason
                                 &rest arguments)
  (multiple-value-bind (count reason wire)
      (apply #'%run-http1-session-from-file request-bytes handler arguments)
    (ensure-equal expected-count count)
    (ensure-equal expected-reason reason)
    wire))
(deftest core-coverage-http1-session-deadline-eof-and-shutdown
  (let* ((request-bytes
           (ascii "GET /deadline HTTP/1.1|CRLF|Host: example.test|CRLF||CRLF|"))
         (response-wire (ascii "HTTP/1.1 204 No Content|CRLF||CRLF|"))
         (handler-calls 0)
         (handler (lambda (request)
                    (declare (ignore request))
                    (incf handler-calls)
                    (make-http-response :status 204))))
    (multiple-value-bind (count reason wire)
        (%run-http1-session-from-file
         request-bytes handler
         :timeout 10 :request-timeout 1
         :clock-function (lambda () 0)
         :close-stream nil)
      (ensure-equal 1 count)
      (ensure-equal :eof reason)
      (ensure-equal 1 handler-calls)
      (ensure-octets-equal response-wire wire))
    (setf handler-calls 0)
    (multiple-value-bind (count reason wire)
        (%run-http1-session-from-file
         request-bytes handler
         :timeout 10 :idle-timeout 1
         :clock-function (lambda () 0)
         :close-stream nil)
      (ensure-equal 1 count)
      (ensure-equal :eof reason)
      (ensure-equal 1 handler-calls)
      (ensure-octets-equal response-wire wire))
    (setf handler-calls 0)
    (multiple-value-bind (count reason wire)
        (%run-http1-session-from-file
         request-bytes handler
         :shutdown-p (constantly t)
         :close-stream nil)
      (ensure-equal 0 count)
      (ensure-equal :close reason)
      (ensure-equal 0 handler-calls)
      (ensure-octets-equal (octets) wire))))
