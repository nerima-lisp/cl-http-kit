(in-package #:http-kit)

(defparameter *redacted-header-names*
  '("authorization" "proxy-authorization" "cookie" "set-cookie"
    "x-api-key" "x-auth-token"))
