(in-package #:http-kit)

(defun serve-http1-session
    (stream handler &key timeout deadline max-header-bytes max-body-bytes
             default-authority on-body-chunk (collect-body-p t)
             (on-expect-continue :automatic)
             max-requests on-error on-upgrade (close-stream #'close)
             (clock-function #'%monotonic-time))
  "Serve HTTP/1 requests from STREAM until EOF, close, or a request limit.

  HANDLER receives each parsed request and must return an HTTP response or
  HTTP response stream.  A response stream calls its body function until NIL;
  known-length streams use Content-Length and unknown-length HTTP/1.1 streams
  use chunked transfer coding.  The return values are the number of responses written and a termination reason,
one of :EOF, :CLOSE, :UPGRADE, or :MAX-REQUESTS.  A supplied ON-ERROR
function receives the condition and the current request before the condition
is re-signaled.  When a response switches protocols, ON-UPGRADE receives the
stream, request, and wire response.  A non-NIL return value transfers stream
ownership to the callback and prevents CLOSE-STREAM from being called.
ON-EXPECT-CONTINUE defaults to :AUTOMATIC and writes a 100 Continue response
before an expected request body is read.  NIL disables that response; a
function receives the metadata-only request and can write its own interim
response."
  (%http1-session-validate-serve-arguments
   stream
   handler
   max-requests
   on-error
   on-upgrade
   on-expect-continue
   close-stream)
  (let* ((expect-continue-callback
           (%http1-session-make-expect-continue-callback
            stream
            on-expect-continue))
         (absolute-deadline
           (http-deadline timeout
                          :deadline deadline
                          :clock-function clock-function))
         (count 0)
         (termination :running)
         (handed-off-p nil))
    (unwind-protect
         (progn
           (loop while (eq termination :running)
                 do (if (and max-requests
                             (>= count max-requests))
                        (setf termination :max-requests)
                        (let ((request nil))
                          (handler-case
                              (let ((parsed
                                      (%http1-session-read-request
                                       stream
                                       absolute-deadline
                                       max-header-bytes
                                       max-body-bytes
                                       default-authority
                                       on-body-chunk
                                       collect-body-p
                                       expect-continue-callback
                                       clock-function)))
                                (if (null parsed)
                                    (setf termination :eof)
                                    (progn
                                      (setf request parsed)
                                      (multiple-value-bind (next-termination
                                                            next-handed-off-p)
                                          (%http1-session-dispatch-request
                                           stream
                                           handler
                                           request
                                           on-upgrade)
                                        (incf count)
                                        (setf termination next-termination)
                                        (when next-handed-off-p
                                          (setf handed-off-p t))))))
                            (error (caught-condition)
                              (when on-error
                                (funcall on-error caught-condition request))
                              (error caught-condition))))))
           (values count termination))
      (when (and close-stream (not handed-off-p))
        (funcall close-stream stream)))))
