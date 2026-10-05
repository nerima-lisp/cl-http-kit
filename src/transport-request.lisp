(in-package #:http-kit)

(defun send-http-request-over-open-stream
    (request stream &key timeout deadline
                      max-header-bytes max-body-bytes
                      request-target on-body-chunk on-information
                      request-body-function request-body-length
                      (collect-body-p t)
                      (clock-function #'%monotonic-time))
  "Send one REQUEST over an already-open binary STREAM.

The stream remains open.  The primary value is the response and the second
value is the conservative result of HTTP-RESPONSE-REUSABLE-P.  Callers must
close the stream when the second value is NIL or when this function signals.
This boundary makes connection reuse possible without imposing socket, DNS,
TLS, proxy, or concurrency dependencies on the core system."
  (%check-http-request request)
  (%validate-request-body-stream-options
   request request-body-function request-body-length)
  (unless (streamp stream)
    (error 'http-connection-error
           :message "The stream boundary requires an open stream."
           :operation :connect
           :cause (type-of stream)))
  (with-http-deadline (absolute-deadline timeout
                        :inherited deadline
                        :clock-function clock-function)
    (%with-http-error-translation
        ("The HTTP request over an open stream failed." :transport)
      (let ((body-sent-p nil)
            (expect-continue-p
              (and (%request-expect-continue-p request)
                   (%request-body-present-p request request-body-function)))
            (transfer-mode nil)
            (expected-body-length nil))
        (labels ((send-body ()
                   (unless body-sent-p
                     (if request-body-function
                         (%write-request-body-stream
                          stream request-body-function expected-body-length
                          transfer-mode absolute-deadline clock-function)
                         (%write-request-body-octets
                          stream request transfer-mode expected-body-length
                          absolute-deadline clock-function))
                     (finish-output stream)
                     (setf body-sent-p t)))
                 (handle-information (information)
                   (when on-information
                     (funcall on-information information))
                   (when (and expect-continue-p
                              (= (http-response-status information) 100))
                     (send-body))))
          (if expect-continue-p
              (multiple-value-bind (mode body-length)
                  (%write-request-head
                   stream request request-target request-body-function
                   request-body-length absolute-deadline clock-function)
                (setf transfer-mode mode
                      expected-body-length body-length))
              (if request-body-function
                  (multiple-value-bind (mode body-length)
                      (%write-request-head
                       stream request request-target request-body-function
                       request-body-length absolute-deadline clock-function)
                    (setf transfer-mode mode
                          expected-body-length body-length)
                    (send-body))
                  (let ((wire (serialize-http-request
                               request :request-target request-target)))
                    (write-sequence wire stream)
                    (finish-output stream)
                    (setf body-sent-p t))))
          (%check-deadline absolute-deadline clock-function :read)
          (let ((response
                  (parse-http-response stream
                                       :deadline absolute-deadline
                                       :max-header-bytes max-header-bytes
                                       :max-body-bytes max-body-bytes
                                       :request-method (http-request-method request)
                                       :on-body-chunk on-body-chunk
                                       :on-information
                                       (and (or on-information expect-continue-p)
                                            #'handle-information)
                                       :collect-body-p collect-body-p
                                       :clock-function clock-function)))
            (values response
                    (and (or (not expect-continue-p) body-sent-p)
                          (http-response-reusable-p request response)))))))))
