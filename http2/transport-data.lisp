(in-package #:http-kit/http2)

(defparameter *h2-connection-specific-header-names*
  '("connection" "keep-alive" "proxy-connection"
    "transfer-encoding" "upgrade" "http2-settings"))
