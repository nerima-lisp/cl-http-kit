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
      (ensure-equal "gzip, deflate, br, zstd"
                    (http-header-value
                     (http-request-headers effective-request)
                     "Accept-Encoding"))
      (ensure-equal "gzip, deflate, br, zstd"
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
(deftest client-content-coding-decodes-brotli-and-zstd-through-client
  (let ((cases
          (list
           (list "br"
                 (octets 161 88 1 192 111 156 28 107 183 227 86 41
                         8 233 13 175 215 113 121 15 38 41 81 120 164
                         44 193 52 84 100 248 115 167 47 131 96 218
                         136 242 100 165 252 217 50)
                 (%fr014-octets
                  (format nil "Brotli decoding returns the original bytes.~%")))
           (list "zstd"
                 (octets 40 181 47 253 36 49 137 1 0 80 108 97 105 110
                         32 90 115 116 97 110 100 97 114 100 32 102 105
                         120 116 117 114 101 32 102 114 111 109 32 116
                         104 101 32 112 105 110 110 101 100 32 78 105
                         120 32 67 76 73 46 10 17 45 225 176)
                 (%fr014-octets
                  (format nil "Plain Zstandard fixture from the pinned Nix CLI.~%"))))))
    (dolist (entry cases)
      (destructuring-bind (coding encoded payload) entry
        (let* ((seen-request nil)
               (client
                 (make-http-client
                  :transport-function
                  (lambda (request &rest arguments)
                    (declare (ignore request arguments))
                    (setf seen-request request)
                    (%fr014-response encoded coding)))))
          (multiple-value-bind (response effective-request)
              (http-client-send
               client
               (http-client-request client "GET" "http://example.test/"))
            (ensure-equal "gzip, deflate, br, zstd"
                          (http-header-value
                           (http-request-headers effective-request)
                           "Accept-Encoding"))
            (ensure-equal "gzip, deflate, br, zstd"
                          (http-header-value
                           (http-request-headers seen-request)
                           "Accept-Encoding"))
            (ensure-equal payload (http-response-body response))
            (ensure-false (http-header-present-p
                           (http-response-headers response) "Content-Encoding"))
            (ensure-false (http-header-present-p
                           (http-response-headers response) "Content-Length"))))))
    (dolist (entry cases)
      (destructuring-bind (coding encoded payload) entry
        (let* ((limit (1- (length payload)))
               (caught nil)
               (client
                 (make-http-client
                  :max-body-bytes limit
                  :transport-function
                  (lambda (request &rest arguments)
                    (declare (ignore request arguments))
                    (%fr014-response encoded coding)))))
          (handler-case
              (http-client-send
               client
               (http-client-request client "GET" "http://example.test/"))
            (http-size-limit-exceeded (condition)
              (setf caught condition)))
          (ensure-true caught)
          (ensure-equal limit (http-size-limit-exceeded-limit caught))
          (ensure-true (> (http-size-limit-exceeded-observed caught) limit))
          (ensure-equal :body (http-size-limit-exceeded-kind caught)))))
    (let ((window-frame (octets 40 181 47 253 0 105 9 0 0 65)))
      (ensure-equal (%fr014-octets "A")
                    (deflate-kit:zstd-decode
                     window-frame :max-window-size-bytes (* 16 1024 1024)))
      (let* ((caught nil)
             (client
               (make-http-client
                :transport-function
                (lambda (request &rest arguments)
                  (declare (ignore request arguments))
                  (%fr014-response window-frame "zstd")))))
        (handler-case
            (http-client-send
             client
             (http-client-request client "GET" "http://example.test/"))
          (http-protocol-error (condition)
            (setf caught condition)))
        (ensure-true caught)
        (ensure-equal :client (http-error-operation caught))))))

