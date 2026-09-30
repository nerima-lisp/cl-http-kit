(in-package #:http-kit/http2)

(defun %hpack-read-integer (octets position prefix-bits)
  (unless (and (<= 1 prefix-bits 8) (< position (length octets)))
    (error 'http-protocol-error
           :message "An HPACK integer is truncated."
           :operation :hpack
           :detail position))
  (let* ((mask (1- (ash 1 prefix-bits)))
         (first (aref octets position))
         (value (logand first mask))
         (position (1+ position)))
    (if (< value mask)
        (values value position)
        (let ((shift 0))
          (loop
            (when (>= position (length octets))
              (error 'http-protocol-error
                     :message "An HPACK integer continuation is truncated."
                     :operation :hpack
                     :detail position))
            (let ((octet (aref octets position)))
              (incf position)
              (let ((increment (ash (logand octet #x7f) shift)))
                (when (> increment #xffffffff)
                  (error 'http-protocol-error
                         :message "An HPACK integer is too large."
                         :operation :hpack
                         :detail value))
                (incf value increment))
              (when (> value #xffffffff)
                (error 'http-protocol-error
                       :message "An HPACK integer is too large."
                       :operation :hpack
                       :detail value))
              (if (zerop (logand octet #x80))
                  (return (values value position))
                  (progn
                    (incf shift 7)
                    (when (> shift 28)
                      (error 'http-protocol-error
                             :message "An HPACK integer is too large."
                             :operation :hpack
                             :detail value))))))))))

(defun %hpack-encode-integer (value prefix-bits prefix)
  (unless (and (integerp value) (>= value 0)
               (<= 1 prefix-bits 8)
               (integerp prefix) (<= 0 prefix #xff))
    (error 'http-protocol-error
           :message "Cannot encode an invalid HPACK integer."
           :operation :hpack
           :detail value))
  (let* ((mask (1- (ash 1 prefix-bits)))
         (prefix (logand prefix #xff)))
    (if (< value mask)
        (let ((result (make-array 1 :element-type '(unsigned-byte 8))))
          (setf (aref result 0) (logior prefix value))
          result)
        (let ((result (make-array 1 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 1)))
          (setf (aref result 0) (logior prefix mask))
          (decf value mask)
          (loop while (>= value 128)
                do (vector-push-extend (logior #x80 (logand value #x7f)) result)
                   (setf value (ash value -7)))
          (vector-push-extend value result)
          (let ((copy (make-array (length result)
                                  :element-type '(unsigned-byte 8))))
            (replace copy result)
            copy)))))

(defun %hpack-decode-string (octets position)
  (when (>= position (length octets))
    (error 'http-protocol-error
           :message "An HPACK string is truncated."
           :operation :hpack
           :detail position))
  (let ((huffman (/= 0 (logand (aref octets position) #x80))))
    (multiple-value-bind (length next-position)
        (%hpack-read-integer octets position 7)
      (let ((end (+ next-position length)))
        (when (> end (length octets))
          (error 'http-protocol-error
                 :message "An HPACK string is truncated."
                 :operation :hpack
                 :detail length))
        (values (http-kit::%octets-string
                 (if huffman
                     (%hpack-huffman-decode (subseq octets next-position end))
                     (subseq octets next-position end)))
                end)))))

(defun %hpack-encode-string (string &key (huffman-p nil))
  (let* ((octets (http-kit::%string-octets string :context :hpack))
         (encoded-octets (if huffman-p
                             (%hpack-huffman-encode octets)
                             octets))
         (prefix (%hpack-encode-integer
                  (length encoded-octets)
                  7
                  (if huffman-p #x80 0))))
    (http-kit::%join-octets prefix encoded-octets)))

(defun %hpack-static-index (name &optional value)
  (loop for index from 1 below (1+ (length *hpack-static-table*))
        for field = (aref *hpack-static-table* (1- index))
        when (and (string= name (car field))
                  (or (null value) (string= value (cdr field))))
          do (return index)))

(defun %hpack-indexed-field (context index)
  (unless (and (integerp index) (plusp index))
    (error 'http-protocol-error
           :message "An HPACK table index must be positive."
           :operation :hpack
           :detail index))
  (if (<= index (length *hpack-static-table*))
      (aref *hpack-static-table* (1- index))
      (let* ((dynamic-index (- index (length *hpack-static-table*)))
             (entry (nth (1- dynamic-index) (%hpack-context-entries context))))
        (unless entry
          (error 'http-protocol-error
                 :message "An HPACK dynamic table index is out of range."
                 :operation :hpack
                 :detail index))
        (cons (%hpack-entry-name entry) (%hpack-entry-value entry)))))

(defun %hpack-indexed-name (context index)
  (unless (zerop index)
    (car (%hpack-indexed-field context index))))

(defun %hpack-literal-name (octets position context prefix-bits)
  (multiple-value-bind (index next-position)
      (%hpack-read-integer octets position prefix-bits)
    (if (zerop index)
        (multiple-value-bind (name end) (%hpack-decode-string octets next-position)
          (values name end))
        (values (%hpack-indexed-name context index) next-position))))

(defun %hpack-name-p (name)
  (and (stringp name)
       (not (string= name ""))
       (if (char= (char name 0) #\:)
           (and (> (length name) 1)
                (http-kit::%header-name-p (subseq name 1)))
           (http-kit::%header-name-p name))))

(defun %hpack-validate-field (name value)
  (unless (and (stringp name)
               (not (string= name ""))
               (string= name (string-downcase name))
               (%hpack-name-p name))
    (error 'http-invalid-header
           :message "HTTP/2 header names must be lowercase ASCII tokens."
           :operation :hpack
           :name name
           :reason :name))
  (unless (and (stringp value) (http-kit::%header-value-p value))
    (error 'http-invalid-header
           :message "HTTP/2 header values contain an invalid character."
           :operation :hpack
           :name name
           :reason :value))
  (cons name value))

(defun %hpack-decode-block (octets context &key max-header-bytes max-fields)
  (let ((position 0)
        (fields '())
        (header-bytes 0)
        (field-count 0)
        (seen-header nil))
    (unless (or (null max-fields)
                (and (integerp max-fields) (plusp max-fields)))
      (error 'http-protocol-error
             :message "HPACK field-count limits must be positive integers."
             :operation :hpack
             :detail max-fields))
    (labels ((check-field-count ()
               (incf field-count)
               (when (and max-fields (> field-count max-fields))
                 (error 'http-kit:http-size-limit-exceeded
                        :message "HPACK field count exceeds the configured limit."
                        :operation :hpack
                        :limit max-fields
                        :observed field-count
                        :kind :fields))))
    (loop while (< position (length octets))
          do (let ((first (aref octets position)))
               (cond
                 ((/= 0 (logand first #x80))
                  (multiple-value-bind (index next-position)
                      (%hpack-read-integer octets position 7)
                    (let ((field (%hpack-indexed-field context index)))
                      (check-field-count)
                      (push (%hpack-validate-field (car field) (cdr field)) fields)
                      (incf header-bytes (+ (length (car field))
                                            (length (cdr field)) 32))
                      (http-kit::%check-limit :headers header-bytes max-header-bytes)
                      (setf seen-header t
                            position next-position))))
                 ((/= 0 (logand first #x40))
                  (setf seen-header t)
                  (multiple-value-bind (name next-position)
                      (%hpack-literal-name octets position context 6)
                    (multiple-value-bind (value end)
                        (%hpack-decode-string octets next-position)
                      (let ((field (%hpack-validate-field name value)))
                        (check-field-count)
                        (push field fields)
                        (incf header-bytes (+ (length name) (length value) 32))
                        (http-kit::%check-limit :headers header-bytes max-header-bytes)
                        (%hpack-add context name value)
                        (setf position end)))))
                 ((/= 0 (logand first #x20))
                  (when seen-header
                    (error 'http-protocol-error
                           :message "An HPACK table size update must precede header fields."
                           :operation :hpack
                           :detail :table-size-update))
                  (multiple-value-bind (size next-position)
                      (%hpack-read-integer octets position 5)
                    (%hpack-set-max-size context size)
                    (setf position next-position)))
                 (t
                  (setf seen-header t)
                  (multiple-value-bind (name next-position)
                      (%hpack-literal-name octets position context 4)
                    (multiple-value-bind (value end)
                        (%hpack-decode-string octets next-position)
                      (let ((field (%hpack-validate-field name value)))
                        (check-field-count)
                        (push field fields)
                        (incf header-bytes (+ (length name) (length value) 32))
                        (http-kit::%check-limit :headers header-bytes max-header-bytes)
                        (setf position end))))))))
    (nreverse fields))))

(defun %hpack-encode-name-value (name value &key (huffman-p nil))
  (let ((exact (%hpack-static-index name value))
        (name-index (%hpack-static-index name)))
    (cond
      (exact (%hpack-encode-integer exact 7 #x80))
      (name-index
       (http-kit::%join-octets
        (%hpack-encode-integer name-index 4 0)
        (%hpack-encode-string value :huffman-p huffman-p)))
      (t
       (http-kit::%join-octets
        (%hpack-encode-integer 0 4 0)
        (%hpack-encode-string name :huffman-p huffman-p)
        (%hpack-encode-string value :huffman-p huffman-p))))))

(defun %hpack-encode-block (fields &key (huffman-p nil))
  (if fields
      (apply #'http-kit::%join-octets
             (mapcar (lambda (field)
                       (%hpack-encode-name-value
                        (car field) (cdr field)
                        :huffman-p huffman-p))
                     fields))
      (http-kit::%empty-octets)))
