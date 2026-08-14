(in-package #:http-kit/client)

(defun %client-monotonic-time ()
  (/ (float (get-internal-real-time))
     internal-time-units-per-second))

(defun %client-validate-limit (value message)
  (when value
    (%ensure-nonnegative-integer value message))
  value)

(defun %client-transport-from-stream-boundary (open-stream close-stream)
  (lambda (request &key timeout deadline max-header-bytes max-body-bytes
                         request-target request-body-function request-body-length
                         on-body-chunk on-information (collect-body-p t)
                         proxy-plan proxy)
    (send-http-request-over-stream
     request
     :open-stream
     (lambda (stream-request &key timeout deadline)
       (funcall open-stream
                stream-request
                :timeout timeout
                :deadline deadline
                :proxy-plan proxy-plan
                :proxy proxy))
     :close-stream close-stream
     :timeout timeout
     :deadline deadline
     :max-header-bytes max-header-bytes
     :max-body-bytes max-body-bytes
     :request-target
     (or request-target
         (and (eq (getf proxy-plan :mode) :forward)
              (getf proxy-plan :request-target)))
     :request-body-function request-body-function
     :request-body-length request-body-length
     :on-body-chunk on-body-chunk
     :on-information on-information
     :collect-body-p collect-body-p
     :clock-function #'%client-monotonic-time)))

(defun %client-connection-key (request proxy-plan)
  (let ((proxy (and proxy-plan (getf proxy-plan :proxy))))
    (list (http-uri-origin (http-request-uri request))
          (and proxy-plan (getf proxy-plan :mode))
          (and proxy-plan (getf proxy-plan :connect-host))
          (and proxy-plan (getf proxy-plan :connect-port))
          (and proxy-plan (getf proxy-plan :proxy-host))
          (and proxy-plan (getf proxy-plan :proxy-port))
          (and proxy-plan (getf proxy-plan :proxy-authorization))
          (and proxy
               (list (http-proxy-scheme proxy)
                     (http-proxy-host proxy)
                     (http-proxy-port proxy))))))

(defun %client-transport-from-connection-pool (connection-pool)
  (lambda (request &key timeout deadline max-header-bytes max-body-bytes
                         request-target request-body-function request-body-length
                         on-body-chunk on-information (collect-body-p t)
                         proxy-plan proxy)
    (http-connection-pool-send
     connection-pool
     request
     :key (%client-connection-key request proxy-plan)
     :timeout timeout
     :deadline deadline
     :max-header-bytes max-header-bytes
     :max-body-bytes max-body-bytes
     :proxy-plan proxy-plan
     :proxy proxy
     :request-target request-target
     :request-body-function request-body-function
     :request-body-length request-body-length
     :on-body-chunk on-body-chunk
     :on-information on-information
     :collect-body-p collect-body-p)))
