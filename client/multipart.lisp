(in-package #:http-kit/client)

(defun %client-octet-vector-p (value)
  (and (arrayp value)
       (= (array-rank value) 1)
       (not (stringp value))
       (loop for octet across value
             always (and (integerp octet) (<= 0 octet #xff)))))

(defun %copy-client-octets (value)
  (unless (%client-octet-vector-p value)
    (%client-protocol-error
     "Expected a one-dimensional vector containing octets."
     value))
  (let ((copy (make-array (length value)
                          :element-type '(unsigned-byte 8))))
    (replace copy value)
    copy))

(defun %multipart-append-string (result string)
  (loop for byte across (cl-codec-kit:string-to-octets string :encoding :utf-8)
        do (vector-push-extend byte result))
  result)

(defun %multipart-octets-string (octets start end)
  (map 'string #'code-char (subseq octets start end)))

(defun %multipart-find-sequence (octets needle start &key end)
  (let* ((end (or end (length octets)))
         (needle-length (length needle))
         (last-start (- end needle-length)))
    (when (and (<= 0 start) (<= start last-start))
      (loop for position from start to last-start
            when (loop for offset below needle-length
                       always (= (aref octets (+ position offset))
                                 (aref needle offset)))
              return position))))
