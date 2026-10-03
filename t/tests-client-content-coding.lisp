(in-package #:http-kit/test)

(defun %fr014-octets (string)
  (map '(vector (unsigned-byte 8)) #'char-code string))

(defun %fr014-response (body content-encoding)
  (make-http-response
   :status 200
   :headers (list (make-http-header "Content-Encoding" content-encoding)
                  (make-http-header "Content-Length"
                                    (princ-to-string (length body))))
   :body body))

(deftest client-content-coding-defaults-and-gzip
  (let* ((payload (%fr014-octets "content-coding payload"))
         (seen-request nil)
         (client
           (make-http-client
            :transport-function
            (lambda (request &rest arguments)
              (declare (ignore arguments))
              (setf seen-request request)
              (%fr014-response
               (deflate-kit:gzip-compress payload)
               "gzip")))))
    (multiple-value-bind (response effective-request)
        (http-client-send
         client
         (http-client-request client "GET" "http://example.test/"))
      (ensure-equal "gzip, deflate"
                    (http-header-value
                     (http-request-headers effective-request)
                     "Accept-Encoding"))
      (ensure-equal "gzip, deflate"
                    (http-header-value
                     (http-request-headers seen-request)
                     "Accept-Encoding"))
      (ensure-equal payload (http-response-body response))
      (ensure-false (http-header-present-p
                     (http-response-headers response) "Content-Encoding"))
      (ensure-false (http-header-present-p
                     (http-response-headers response) "Content-Length")))))

(deftest client-content-coding-decodes-zlib-and-raw-deflate
  (let ((payload (%fr014-octets "zlib and raw deflate")))
    (dolist (encoded
             (list (deflate-kit:zlib-compress payload)
                   (deflate-kit:deflate payload)))
      (let ((response
              (decode-http-response-content
               (%fr014-response encoded "deflate"))))
        (ensure-equal payload (http-response-body response))
        (ensure-false (http-header-present-p
                       (http-response-headers response) "Content-Encoding"))
        (ensure-false (http-header-present-p
                       (http-response-headers response) "Content-Length"))))))

(deftest client-content-coding-enforces-expanded-body-limit
  (let* ((payload (%fr014-octets (make-string 4096 :initial-element #\A)))
         (encoded (deflate-kit:gzip-compress payload)))
    (ensure-true (< (length encoded) (length payload)))
    (ensure-equal payload
                  (http-response-body
                   (decode-http-response-content
                    (%fr014-response encoded "gzip")
                    :max-body-bytes (length payload))))
    (let ((caught nil))
      (handler-case
          (decode-http-response-content
           (%fr014-response encoded "gzip")
           :max-body-bytes (1- (length payload)))
        (http-size-limit-exceeded (condition)
          (setf caught condition)))
      (ensure-true caught)
      (ensure-equal (1- (length payload))
                    (http-size-limit-exceeded-limit caught))
      (ensure-true (> (http-size-limit-exceeded-observed caught)
                      (http-size-limit-exceeded-limit caught)))
      (ensure-equal :body (http-size-limit-exceeded-kind caught)))))

(deftest client-content-coding-reports-malformed-deflate
  (handler-case
      (decode-http-response-content
       (%fr014-response (octets 1 2 3) "gzip"))
    (http-protocol-error (condition)
      (ensure-equal :client (http-error-operation condition)))
    (condition (condition)
      (error "Expected HTTP-PROTOCOL-ERROR, got ~S." (type-of condition)))))
