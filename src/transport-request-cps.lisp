(in-package #:http-kit)

(defun send-http-request-over-stream/cps
    (request on-success
     &key on-error open-stream close-stream timeout deadline
       max-header-bytes max-body-bytes
       request-target on-body-chunk on-information
       request-body-function request-body-length
       (collect-body-p t)
       (clock-function #'%monotonic-time))
  "Send a stream request and dispatch the result to CPS continuations."
  (%call-http-operation/cps
   (lambda ()
     (send-http-request-over-stream
      request
      :open-stream open-stream
      :close-stream close-stream
      :timeout timeout
      :deadline deadline
      :max-header-bytes max-header-bytes
      :max-body-bytes max-body-bytes
      :request-target request-target
      :on-body-chunk on-body-chunk
      :on-information on-information
      :request-body-function request-body-function
      :request-body-length request-body-length
      :collect-body-p collect-body-p
      :clock-function clock-function))
   on-success
   :on-error on-error))
