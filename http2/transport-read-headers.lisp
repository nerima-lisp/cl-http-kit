(in-package #:http-kit/http2)

(defun %h2-header-fragment (frame &key first-p)
  "Return the header-block fragment after removing HEADERS padding/priority."
  (let* ((payload (%h2-frame-payload frame))
         (flags (%h2-frame-flags frame))
         (start 0)
         (end (length payload)))
    (when first-p
      (when (/= 0 (logand flags +http2-padded-flag+))
        (when (zerop end)
          (error 'http-kit:http-protocol-error
                 :message "A padded HTTP/2 HEADERS frame has no pad length."
                 :operation :http2-headers
                 :detail :missing-pad-length))
        (let ((padding-length (aref payload 0)))
          (incf start)
          (when (> padding-length (- end start))
            (error 'http-kit:http-protocol-error
                   :message "HTTP/2 HEADERS padding exceeds the payload."
                   :operation :http2-headers
                   :detail padding-length))
          (decf end padding-length)))
      (when (/= 0 (logand flags +http2-priority-flag+))
        (when (< (- end start) 5)
          (error 'http-kit:http-protocol-error
                 :message "A priority-bearing HTTP/2 HEADERS frame lacks priority data."
                 :operation :http2-headers
                 :detail (- end start)))
        (let ((dependency (%h2-u32 payload start)))
          (when (= (logand dependency #x7fffffff)
                   (%h2-frame-stream-id frame))
            (error 'http-kit:http-protocol-error
                   :message "An HTTP/2 stream cannot depend on itself."
                   :operation :http2-headers
                   :detail (%h2-frame-stream-id frame))))
        (incf start 5)))
    (subseq payload start end)))

(defun %h2-read-header-block (first-frame reader max-frame-size deadline
                                            clock-function max-header-bytes
                                            &optional (expected-stream-id 1)
                                              next-frame-function)
  (let ((stream-id (%h2-frame-stream-id first-frame)))
    (unless (and (= (%h2-frame-type first-frame) +http2-headers-type+)
                 (= stream-id expected-stream-id))
      (error 'http-kit:http-protocol-error
             :message "The HTTP/2 client transport received response HEADERS on an unexpected stream."
             :operation :http2-headers
             :detail (list :expected-stream-id expected-stream-id
                           :frame-type (%h2-frame-type first-frame)
                           :stream-id stream-id)))
    (let ((first-fragment (%h2-header-fragment first-frame :first-p t)))
      (let ((parts (list first-fragment))
            (header-block-bytes (length first-fragment))
            (frame first-frame))
        (http-kit::%check-limit :headers header-block-bytes max-header-bytes)
        (loop until (/= 0 (logand (%h2-frame-flags frame)
                                  +http2-end-headers-flag+))
              do (setf frame
                        (if next-frame-function
                            (funcall next-frame-function)
                            (%h2-read-frame reader max-frame-size deadline
                                            clock-function)))
                 (when (eq frame :eof)
                   (error 'http-kit:http-protocol-error
                          :message "HTTP/2 HEADERS ended before CONTINUATION END_HEADERS."
                          :operation :http2-headers
                          :detail :eof))
                 (unless (and (= (%h2-frame-type frame) +http2-continuation-type+)
                              (= (%h2-frame-stream-id frame) stream-id))
                   (error 'http-kit:http-protocol-error
                          :message "HTTP/2 HEADERS must be followed by same-stream CONTINUATION frames."
                          :operation :http2-headers
                          :detail (list (%h2-frame-type frame)
                                        (%h2-frame-stream-id frame))))
                 (when (/= 0 (logand (%h2-frame-flags frame)
                                     (lognot +http2-end-headers-flag+)))
                   (error 'http-kit:http-protocol-error
                          :message "An HTTP/2 CONTINUATION has an invalid flag."
                          :operation :http2-headers
                          :detail (%h2-frame-flags frame)))
                 (push (%h2-frame-payload frame) parts)
                 (incf header-block-bytes (length (%h2-frame-payload frame)))
                 (http-kit::%check-limit :headers header-block-bytes
                                         max-header-bytes))
        (values (%h2-concat (nreverse parts))
                (/= 0 (logand (%h2-frame-flags first-frame)
                              +http2-end-stream-flag+)))))))

(defun %h2-regular-header-valid-p (name value)
  (unless (and (not (string= name ""))
               (string= name (string-downcase name)))
    (error 'http-kit:http-invalid-header
           :message "HTTP/2 header field names must be lowercase."
           :operation :http2-headers
           :name name
           :reason :name))
  (when (%h2-connection-specific-header-p name)
    (error 'http-kit:http-invalid-header
           :message "Connection-specific headers are forbidden in HTTP/2 responses."
           :operation :http2-headers
           :name name
           :reason :connection-specific))
  (when (string= name "te")
    (unless (every (lambda (item) (string= item "trailers"))
                   (%h2-comma-items (list value) name))
      (error 'http-kit:http-invalid-header
             :message "HTTP/2 TE is only permitted with the trailers value."
             :operation :http2-headers
             :name name
             :reason :value)))
  t)

(defun %h2-status-and-headers (fields)
  (let ((status nil)
        (regular '())
        (regular-seen nil))
    (dolist (field fields)
      (let ((name (car field))
            (value (cdr field)))
        (unless (string/= name "")
          (error 'http-kit:http-invalid-header
                 :message "HTTP/2 header field names cannot be empty."
                 :operation :http2-headers
                 :name name
                 :reason :name))
        (if (char= (char name 0) #\:)
            (progn
              (when regular-seen
                (error 'http-kit:http-protocol-error
                       :message "HTTP/2 pseudo-headers must precede regular headers."
                       :operation :http2-headers
                       :detail name))
              (unless (string= name ":status")
                (error 'http-kit:http-invalid-header
                       :message "An HTTP/2 response contains an unknown pseudo-header."
                       :operation :http2-headers
                       :name name
                       :reason :pseudo-header))
              (when status
                (error 'http-kit:http-invalid-status
                       :message "An HTTP/2 response contains duplicate :status fields."
                       :operation :http2-headers
                       :line value))
              (unless (and (= (length value) 3)
                           (http-kit::%decimal-string-p value))
                (error 'http-kit:http-invalid-status
                       :message "HTTP/2 :status must be a three-digit decimal code."
                       :operation :http2-headers
                       :line value))
              (setf status (http-kit::%parse-decimal value)))
            (progn
              (setf regular-seen t)
              (%h2-regular-header-valid-p name value)
              (push (http-kit:make-http-header name value) regular)))))
    (unless (and status (<= 100 status 599))
      (error 'http-kit:http-invalid-status
             :message "An HTTP/2 response must contain a valid :status field."
             :operation :http2-headers
             :line fields
             :code status))
    (when (= status 101)
      (error 'http-kit:http-unsupported-feature
             :message "HTTP/2 does not support the HTTP/1.1 101 status transition."
             :operation :http2-headers
             :feature :http2-switching-protocols))
    (values status (nreverse regular))))

(defun %h2-trailers (fields)
  (mapcar (lambda (field)
            (let ((name (car field))
                  (value (cdr field)))
              (unless (string/= name "")
                (error 'http-kit:http-invalid-header
                       :message "HTTP/2 trailer field names cannot be empty."
                       :operation :http2-trailers
                       :name name
                       :reason :name))
              (when (char= (char name 0) #\:)
                (error 'http-kit:http-invalid-header
                       :message "HTTP/2 trailers cannot contain pseudo-headers."
                       :operation :http2-trailers
                       :name name
                       :reason :pseudo-header))
              (when (string= name "content-length")
                (error 'http-kit:http-invalid-header
                       :message "HTTP/2 trailers cannot contain Content-Length."
                       :operation :http2-trailers
                       :name name
                       :reason :forbidden))
              (%h2-regular-header-valid-p name value)
              (http-kit:make-http-header name value)))
          fields))

(defun %h2-finish-response (status headers trailers body
                            &key no-body (body-length (length body)))
  (let ((content-lengths (http-kit:http-header-values headers "content-length")))
    (when content-lengths
      (unless (every #'http-kit::%decimal-string-p content-lengths)
        (error 'http-kit:http-invalid-header
               :message "HTTP/2 response Content-Length is invalid."
               :operation :http2-response
               :name "content-length"
               :reason :value))
      (let ((expected (http-kit::%parse-decimal (first content-lengths))))
        (unless (every (lambda (value)
                         (= expected (http-kit::%parse-decimal value)))
                       content-lengths)
          (error 'http-kit:http-invalid-header
                 :message "Duplicate HTTP/2 response Content-Length values must agree."
                 :operation :http2-response
                 :name "content-length"
                 :reason :duplicate))
        (when (and (member status '(204 205) :test #'=)
                   (plusp expected))
          (error 'http-kit:http-invalid-header
                 :message "HTTP/2 204 and 205 responses cannot have a positive Content-Length."
                 :operation :http2-response
                 :name "content-length"
                 :reason :forbidden))
        (unless (or no-body (= expected body-length))
          (error 'http-kit:http-invalid-header
                 :message "HTTP/2 response Content-Length does not match DATA."
                 :operation :http2-response
                 :name "content-length"
                 :reason :mismatch))))
    (http-kit:make-http-response :protocol-version "HTTP/2"
                                 :status status
                                 :headers headers
                                 :trailers trailers
                                 :body body)))
