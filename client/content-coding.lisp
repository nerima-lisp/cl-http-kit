(in-package #:http-kit/client)

(defun %content-coding-tokens (headers)
  (let ((values (http-header-values headers "Content-Encoding")))
    (when values
      (let ((value (format nil "~{~A~^,~}" values)))
        (loop with start = 0
              for comma = (position #\, value :start start)
              for token = (string-downcase
                           (string-trim '(#\Space #\Tab)
                                        (subseq value start comma)))
              unless (zerop (length token)) collect token
              while comma
              do (setf start (1+ comma)))))))

(defun %decompress-content-with-limit (coding-format body max-body-bytes)
  (if (null max-body-bytes)
      (chipz:decompress nil coding-format body)
      (let* ((capacity (1+ max-body-bytes))
             (output (make-array capacity :element-type '(unsigned-byte 8)))
             (state (chipz:make-dstate coding-format)))
        (multiple-value-bind (consumed produced)
            (chipz:decompress output state body)
          (declare (ignore consumed))
          (when (= produced capacity)
            (error 'http-size-limit-exceeded
                   :message "The decoded HTTP body limit was exceeded."
                   :operation :content-decoding
                   :limit max-body-bytes
                   :observed produced
                   :kind :body))
          (chipz:finish-dstate state)
          (subseq output 0 produced)))))

(defun %chipz-content-decoder (format)
  (lambda (body max-body-bytes)
    (%decompress-content-with-limit format body max-body-bytes)))

(defun make-http-content-decoders (&rest decoders)
  "Return a validated Content-Encoding decoder association list."
  (let ((result
          (list (cons "gzip" (%chipz-content-decoder 'chipz:gzip))
                (cons "deflate" (%chipz-content-decoder 'chipz:zlib)))))
    (dolist (decoder decoders)
      (unless (and (consp decoder)
                   (stringp (car decoder))
                   (http-kit::%token-p (car decoder))
                   (functionp (cdr decoder)))
        (%client-protocol-error
         "A content decoder must be a coding token paired with a function."
         decoder))
      (let ((name (string-downcase (car decoder))))
        (setf result (delete name result :key #'car :test #'string=))
        (setf result (append result (list (cons name (cdr decoder)))))))
    result))

(defun %content-decoder (coding decoders)
  (cond
    ((string= coding "identity")
     (lambda (body max-body-bytes)
       (declare (ignore max-body-bytes))
       body))
    (t (cdr (assoc coding decoders :test #'string-equal)))))

(defun decode-http-response-content
    (response &key max-body-bytes (content-decoders (make-http-content-decoders)))
  "Decode supported Content-Encoding values in RESPONSE's collected body."
  (unless (http-response-p response)
    (%client-protocol-error "The response to decode must be an HTTP-RESPONSE."
                            response))
  (let* ((headers (http-response-headers response))
         (codings (%content-coding-tokens headers))
         (decoders (mapcar (lambda (coding)
                             (%content-decoder coding content-decoders))
                           codings)))
    (if (or (null codings) (member nil decoders))
        response
        (handler-case
            (let ((body (http-response-body response)))
              (dolist (decoder (reverse decoders))
                (setf body (funcall decoder body max-body-bytes)))
              (make-http-response
               :protocol-version (http-response-protocol-version response)
               :status (http-response-status response)
               :reason (http-response-reason response)
               :headers (remove-if
                         (lambda (header)
                           (member (http-header-name header)
                                   '("Content-Encoding" "Content-Length")
                                   :test #'string-equal))
                         headers)
               :trailers (http-response-trailers response)
               :body body))
          (chipz:chipz-error (condition)
            (%client-protocol-error
             "The encoded HTTP response body could not be decompressed."
             condition))))))
