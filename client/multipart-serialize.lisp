(in-package #:http-kit/client)

(defun %multipart-header-safe-p (string)
  (and (stringp string)
       (not (find-if (lambda (character)
                       (or (char= character #\Return)
                           (char= character #\Linefeed)
                           (char= character #\")))
                     string))))

(defun %multipart-quoted-value (string)
  (with-output-to-string (stream)
    (loop for character across string
          do (when (or (char= character #\")
                       (char= character #\\))
               (write-char #\\ stream))
             (write-char character stream))))

(defun %generated-boundary ()
  (format nil "------------------------cl-http-kit-~36R-~36R"
          (get-universal-time)
          (random (expt 36 8))))

(defun %client-crlf ()
  (coerce (list #\Return #\Linefeed) 'string))

(defun make-http-multipart-body (parts &key boundary)
  "Return two values: multipart octets and its Content-Type value.

Each item in PARTS should be an HTTP-MULTIPART-PART.  The boundary can be
provided for deterministic tests; an implementation-generated boundary is
otherwise used."
  (unless (listp parts)
    (%client-protocol-error "Multipart parts must be a list." parts))
  (let ((boundary (or boundary (%generated-boundary))))
    (unless (and (stringp boundary)
                 (not (string= boundary ""))
                 (not (find-if (lambda (character)
                                 (or (char= character #\Return)
                                     (char= character #\Linefeed)
                                     (char= character #\Space)))
                               boundary)))
      (%client-protocol-error "A multipart boundary contains invalid characters."
                              boundary))
    (let ((crlf (%client-crlf))
          (result (make-array 0
                              :element-type '(unsigned-byte 8)
                              :adjustable t
                              :fill-pointer 0)))
      (dolist (part parts)
        (unless (http-multipart-part-p part)
          (%client-protocol-error "Multipart parts must be HTTP-MULTIPART-PART values."
                                  part))
        (let ((name (http-multipart-part-name part))
              (filename (http-multipart-part-filename part))
              (content-type (http-multipart-part-content-type part))
              (value (http-multipart-part-value part)))
          (unless (%multipart-header-safe-p name)
            (%client-protocol-error "A multipart name contains invalid characters."
                                    name))
          (when (and filename (not (%multipart-header-safe-p filename)))
            (%client-protocol-error
             "A multipart filename contains invalid characters."
             filename))
          (when (and content-type (not (%multipart-header-safe-p content-type)))
            (%client-protocol-error
             "A multipart content type contains invalid characters."
             content-type))
          (%multipart-append-string result (format nil "--~A~A" boundary crlf))
          (%multipart-append-string
           result
           (format nil "Content-Disposition: form-data; name=\"~A\"~A~A"
                   (%multipart-quoted-value name)
                   (if filename
                       (format nil "; filename=\"~A\""
                               (%multipart-quoted-value filename))
                       "")
                   crlf))
          (when content-type
            (%multipart-append-string result
                                      (format nil "Content-Type: ~A~A"
                                              content-type crlf)))
          (%multipart-append-string result crlf)
          (if (stringp value)
              (%multipart-append-string result value)
              (let ((octets (%copy-client-octets value)))
                (loop for byte across octets
                      do (vector-push-extend byte result))))
          (%multipart-append-string result crlf)))
      (%multipart-append-string result (format nil "--~A--~A" boundary crlf))
      (let ((copy (make-array (length result)
                              :element-type '(unsigned-byte 8))))
        (replace copy result)
        (values copy (format nil "multipart/form-data; boundary=~A" boundary))))))
