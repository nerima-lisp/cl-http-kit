(in-package #:http-kit/test)

(defun client-test-response (status &key headers body)
  (make-http-response :status status
                      :headers headers
                      :body (or body (octets))))
