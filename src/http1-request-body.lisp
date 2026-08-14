(in-package #:http-kit)

(defun %read-request-body-segment
    (source length deadline clock-function detail collector on-body-chunk)
  (loop with remaining = length
        while (plusp remaining)
        do (let* ((size (min remaining *http-body-read-chunk-size*))
                  (chunk (make-array size :element-type '(unsigned-byte 8))))
             (loop for index below size
                   do (setf (aref chunk index)
                            (%read-required-byte source deadline clock-function detail
                                                 :operation :request-parse)))
             (%append-body-chunk collector chunk)
             (when on-body-chunk
               (funcall on-body-chunk chunk))
             (decf remaining size)))
  collector)

(defun %read-request-exact-body
    (source length deadline clock-function max-body-bytes
     &key on-body-chunk (collect-body-p t))
  (%check-limit :body length max-body-bytes :operation :request-parse)
  (%finish-body-collector
   (%read-request-body-segment source length deadline clock-function :body
                               (%make-body-collector collect-body-p)
                               on-body-chunk)))

(defun %read-request-chunked-body
    (source deadline clock-function max-header-bytes max-body-bytes header-used
     &key on-body-chunk (collect-body-p t))
  (let ((body (%make-body-collector collect-body-p))
        (trailers '())
        (bytes header-used)
        (body-length 0))
    (loop
      (multiple-value-bind (line updated-bytes)
          (%read-crlf-line source deadline clock-function max-header-bytes bytes
                           :operation :request-parse)
        (setf bytes updated-bytes)
        (let* ((separator (position #\; line))
               (size-text (%trim-ows (if separator
                                         (subseq line 0 separator)
                                         line))))
          (unless (and (not (string= size-text ""))
                       (every #'%hex-character-p size-text))
            (%request-parse-error
             "A chunk size is not a valid hexadecimal integer."
             line))
          (let ((size (parse-integer size-text :radix 16)))
            (if (zerop size)
                (progn
                  (multiple-value-bind (parsed-trailers trailer-bytes)
                      (%read-request-headers source deadline clock-function
                                             max-header-bytes bytes)
                    (setf trailers parsed-trailers
                          bytes trailer-bytes))
                  (%validate-http1-trailers trailers :request-parse)
                  (return))
                (progn
                  (incf body-length size)
                  (%check-limit :body body-length max-body-bytes
                                :operation :request-parse)
                  (%read-request-body-segment source size deadline clock-function
                                              :chunk-data body on-body-chunk)
                  (%read-framing-crlf source deadline clock-function
                                      :operation :request-parse)
                  (incf bytes 2)
                  (%check-limit :headers bytes max-header-bytes
                                :operation :request-parse)))))))
    (values (%finish-body-collector body) trailers)))
