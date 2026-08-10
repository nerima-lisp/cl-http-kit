(in-package #:http-kit/client)

(define-condition http-client-error (http-error)
  ((detail :initarg :detail :initform nil :reader http-client-error-detail)))

(define-condition http-redirect-limit-exceeded (http-client-error)
  ((uri :initarg :uri :reader http-redirect-limit-exceeded-uri)
   (redirects :initarg :redirects :reader http-redirect-limit-exceeded-redirects)))

(define-condition http-retry-exhausted (http-client-error)
  ((attempts :initarg :attempts :reader http-retry-exhausted-attempts)
   (last-condition :initarg :last-condition :initform nil
                   :reader http-retry-exhausted-last-condition)
   (last-response :initarg :last-response :initform nil
                  :reader http-retry-exhausted-last-response)))

(define-condition http-cookie-error (http-client-error) ())
(define-condition http-cache-error (http-client-error) ())
(define-condition http-proxy-error (http-client-error) ())
