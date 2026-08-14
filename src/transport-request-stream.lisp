(in-package #:http-kit)

(defun send-http-request-over-stream
    (request &key open-stream close-stream timeout deadline
                   max-header-bytes max-body-bytes
                   request-target on-body-chunk on-information
                   request-body-function request-body-length
                   (collect-body-p t)
                   (clock-function #'%monotonic-time))
  "Send REQUEST through an injected binary stream boundary.

OPEN-STREAM is called as (REQUEST &KEY TIMEOUT DEADLINE) and must return a
stream.  CLOSE-STREAM is called with that stream after the exchange.  Socket
creation, DNS, proxy policy, TLS, and pooling remain application policies
outside this direct function boundary."
  (%check-http-request request)
  (%validate-request-body-stream-options
   request request-body-function request-body-length)
  (multiple-value-bind (open close)
      (%validate-stream-boundary open-stream close-stream)
    (let ((stream nil))
      (with-http-deadline (absolute-deadline timeout
                            :inherited deadline
                            :clock-function clock-function)
        (%with-http-error-translation
            ("The HTTP stream request failed." :transport)
          (unwind-protect
               (progn
                 (setf stream
                       (funcall open request
                                :timeout timeout
                                :deadline absolute-deadline))
                 (unless (streamp stream)
                   (error 'http-connection-error
                          :message "The stream factory did not return a stream."
                          :operation :connect
                          :cause (type-of stream)))
                 (send-http-request-over-open-stream
                  request stream
                  :deadline absolute-deadline
                  :max-header-bytes max-header-bytes
                  :max-body-bytes max-body-bytes
                  :request-target request-target
                  :on-body-chunk on-body-chunk
                  :on-information on-information
                  :request-body-function request-body-function
                  :request-body-length request-body-length
                  :collect-body-p collect-body-p
                  :clock-function clock-function))
            (when stream
              (funcall close stream))))))))
