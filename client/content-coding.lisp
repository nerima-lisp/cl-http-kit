(in-package #:http-kit/client)

(defstruct (http-content-coding
             (:constructor %make-http-content-coding))
  name
  encoder
  decoder)

(defun make-http-content-coding (&key name encoder decoder)
  (unless (and (stringp name) (http-kit::%token-p name))
    (%client-protocol-error "A content coding name must be a token." name))
  (unless (or (null encoder) (functionp encoder))
    (%client-protocol-error "A content coding encoder must be a function or NIL."
                            encoder))
  (unless (or (null decoder) (functionp decoder))
    (%client-protocol-error "A content coding decoder must be a function or NIL."
                            decoder))
  (%make-http-content-coding :name (string-downcase name)
                             :encoder encoder
                             :decoder decoder))

(defun %content-coding-split (value separator)
  (loop with start = 0
        with parts = nil
        for position = (position separator value :start start)
        do (push (subseq value start (or position (length value))) parts)
        if position do (setf start (1+ position))
        else do (return (nreverse parts))))

(defun %content-coding-quality (value)
  (let ((dot (position #\. value)))
    (if dot
        (float (+ (%client-parse-integer (subseq value 0 dot))
                  (/ (%client-parse-integer (subseq value (1+ dot)))
                     (expt 10 (- (length value) dot 1))))
               1.0)
        (%client-parse-integer value))))

(defun parse-http-accept-encoding (value)
  (unless (stringp value)
    (%client-protocol-error "Accept-Encoding must be a string." value))
  (loop for item in (%content-coding-split value #\,)
        for parts = (%content-coding-split item #\;)
        for name = (string-downcase (string-trim '(#\Space #\Tab) (first parts)))
        unless (zerop (length name))
          collect (cons name
                        (let ((q (find-if (lambda (part)
                                            (search "q=" part :test #'char-equal))
                                          (rest parts))))
                          (if q
                              (%content-coding-quality
                               (string-trim '(#\Space #\Tab)
                                            (subseq q (1+ (position #\= q)))))
                              1.0)))))

(defun http-select-content-coding (accept-encoding available)
  (let ((accepted (parse-http-accept-encoding accept-encoding)))
    (or (loop with selected = nil
              with selected-quality = 0
              for coding in available
              for name = (if (stringp coding) coding
                             (http-content-coding-name coding))
              for quality = (or (cdr (assoc (string-downcase name) accepted
                                            :test #'string=))
                                (cdr (assoc "*" accepted :test #'string=)))
              when (and quality (> quality selected-quality))
                do (setf selected coding
                         selected-quality quality)
              finally (return selected))
        (and (or (null accepted)
                 (not (equal 0 (cdr (assoc "identity" accepted
                                           :test #'string=)))))
             "identity"))))

(defun http-content-coding-encode (coding body)
  (if (http-content-coding-encoder coding)
      (funcall (http-content-coding-encoder coding) body)
      body))

(defun http-content-coding-decode (coding body)
  (if (http-content-coding-decoder coding)
      (funcall (http-content-coding-decoder coding) body)
      body))

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

(defun %deflate-kit-call (function body keyword max-body-bytes)
  (if (null max-body-bytes)
      (funcall function body)
      (funcall function body keyword max-body-bytes)))

(defun %deflate-kit-raw-or-zlib (body max-body-bytes)
  (handler-case
      (%deflate-kit-call #'deflate-kit:zlib-decompress
                         body :max-output max-body-bytes)
    (deflate-kit:invalid-container-error ()
      (%deflate-kit-call #'deflate-kit:inflate
                         body :max-output-bytes max-body-bytes))))

(defun %deflate-kit-content-decoder (format)
  (lambda (body max-body-bytes)
    (ecase format
      (:gzip
       (%deflate-kit-call #'deflate-kit:gzip-decompress
                          body :max-output max-body-bytes))
      (:deflate
       (%deflate-kit-raw-or-zlib body max-body-bytes)))))

(defun make-http-content-decoders (&rest decoders)
  "Return a validated Content-Encoding decoder association list."
  (let ((result
          (list (cons "gzip" (%deflate-kit-content-decoder :gzip))
                (cons "deflate" (%deflate-kit-content-decoder :deflate)))))
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
  (%client-validate-limit
   max-body-bytes
   "The decoded HTTP body size limit must be a non-negative integer or NIL.")
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
          (deflate-kit:inflate-size-limit-exceeded (condition)
            (error 'http-size-limit-exceeded
                   :message "The decoded HTTP body limit was exceeded."
                   :operation :content-decoding
                   :limit (deflate-kit:deflate-output-limit-limit condition)
                   :observed (deflate-kit:inflate-error-observed condition)
                   :kind :body))
          (deflate-kit:deflate-error (condition)
            (%client-protocol-error
             "The encoded HTTP response body could not be decompressed."
             condition))))))
