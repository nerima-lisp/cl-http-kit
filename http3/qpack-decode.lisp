(in-package #:http-kit/http3)

(defun %qpack-decode-string (octets position prefix-bits huffman-mask)
  (when (>= position (length octets))
    (%qpack-error "A QPACK string is truncated." position))
  (let ((huffman (/= 0 (logand (aref octets position) huffman-mask))))
    (multiple-value-bind (length next-position)
        (http-kit/http2::%hpack-read-integer octets position prefix-bits)
      (let ((end (+ next-position length)))
        (when (> end (length octets))
          (%qpack-error "A QPACK string is truncated." length))
        (values
         (http-kit::%octets-string
          (if huffman
              (http-kit/http2::%hpack-huffman-decode
               (subseq octets next-position end))
              (subseq octets next-position end)))
         end)))))

(defun %qpack-decode-required-insert-count
    (encoded-count dynamic-table max-capacity)
  (when (zerop encoded-count)
    (return-from %qpack-decode-required-insert-count 0))
  (let* ((max-entries (%qpack-max-entries max-capacity))
         (full-range (* 2 max-entries))
         (total-insert-count (if dynamic-table
                                 (qpack-dynamic-table-insert-count dynamic-table)
                                 0)))
    (when (zerop full-range)
      (%qpack-error
       "A non-zero QPACK required insert count needs dynamic-table capacity."
       encoded-count))
    (when (> encoded-count full-range)
      (%qpack-error "QPACK encoded required insert count is out of range."
                    encoded-count))
    (let* ((max-value (+ total-insert-count max-entries))
           (max-wrapped (* (floor max-value full-range) full-range))
           (required (+ max-wrapped encoded-count -1)))
      (when (> required max-value)
        (decf required full-range))
      (when (<= required 0)
        (%qpack-error "QPACK encoded required insert count resolves to zero."
                      encoded-count))
      (when (or (null dynamic-table)
                (> required total-insert-count))
        (%qpack-error
         "QPACK field section references dynamic entries not yet available."
         required))
      required)))

(defun %qpack-field-entry (dynamic-table static-p index base required post-base-p)
  (if static-p
      (or (%qpack-static-entry index)
          (%qpack-error "QPACK static-table index is out of range." index))
      (let ((absolute-index
              (if post-base-p
                  (+ base index)
                  (- base index 1))))
        (when (< absolute-index 0)
          (%qpack-error "QPACK dynamic-table reference has a negative index."
                        absolute-index))
        (when (and (not post-base-p)
                   (>= absolute-index required))
          (%qpack-error
           "QPACK field section references an entry at or after its required count."
           absolute-index))
        (let ((entry (%qpack-dynamic-entry-at-absolute
                      dynamic-table absolute-index)))
          (unless (< absolute-index
                     (qpack-dynamic-table-insert-count dynamic-table))
            (%qpack-error "QPACK dynamic-table entry is not yet inserted."
                          absolute-index))
          entry))))

(defun qpack-decode-field-section
    (octets &key dynamic-table max-table-capacity
            (max-header-bytes 65536) (max-fields 256))
  "Decode a QPACK field section with optional dynamic-table references.

The decoder is deliberately synchronous: a section that references an entry
not yet present in DYNAMIC-TABLE signals a protocol error instead of blocking
the caller.  A transport that supports blocked streams can defer this call
until its encoder stream has advanced the table."
  (unless (%http3-octet-vector-p octets)
    (%qpack-error "QPACK field sections must be octet vectors." (type-of octets)))
  (when (and dynamic-table (not (qpack-dynamic-table-p dynamic-table)))
    (%qpack-error "DYNAMIC-TABLE must be a QPACK dynamic table object."
                  dynamic-table))
  (unless (and (integerp max-header-bytes) (>= max-header-bytes 0))
    (%qpack-error "QPACK header-size limits must be non-negative integers."
                  max-header-bytes))
  (unless (and (integerp max-fields) (plusp max-fields))
    (%qpack-error "QPACK field-count limits must be positive integers."
                  max-fields))
  (let* ((effective-max-capacity
           (%qpack-effective-max-capacity dynamic-table max-table-capacity))
         (position 0)
         (encoded-required-insert-count nil)
         (required-insert-count nil)
         (base nil)
         (fields '())
         (regular-seen-p nil)
         (total-bytes 0))
    (multiple-value-setq (encoded-required-insert-count position)
      (http-kit/http2::%hpack-read-integer octets position 8))
    (setf required-insert-count
          (%qpack-decode-required-insert-count
           encoded-required-insert-count dynamic-table effective-max-capacity))
    (when (>= position (length octets))
      (%qpack-error "A QPACK field section is missing its base."))
    (let ((base-sign (/= 0 (logand (aref octets position) #x80))))
      (multiple-value-bind (delta next-position)
          (http-kit/http2::%hpack-read-integer octets position 7)
        (when (and base-sign (<= required-insert-count delta))
          (%qpack-error "QPACK signed base delta would produce a negative base."
                        delta))
        (setf base (if base-sign
                       (- required-insert-count delta 1)
                       (+ required-insert-count delta))
              position next-position)))
    (labels ((append-field (name value)
               (let* ((normalized-name (%qpack-normalize-name name))
                      (normalized-value (%qpack-normalize-value value)))
                 (unless (string= normalized-name name)
                   (%qpack-error
                    "QPACK decoded field names must already be lowercase."
                    name))
                 (if (char= (char normalized-name 0) #\:)
                     (when regular-seen-p
                       (%qpack-error
                        "QPACK pseudo-fields must precede regular fields."
                        normalized-name))
                     (setf regular-seen-p t))
                 (push (cons normalized-name normalized-value) fields)
                 (incf total-bytes
                       (+ (length (http-kit::%string-octets normalized-name))
                          (length (http-kit::%string-octets normalized-value))))))
             (append-entry (entry)
               (append-field (if (qpack-dynamic-entry-p entry)
                                 (qpack-dynamic-entry-name entry)
                                 (first entry))
                             (if (qpack-dynamic-entry-p entry)
                                 (qpack-dynamic-entry-value entry)
                                 (second entry))))
             (check-limits ()
               (when (> (length fields) max-fields)
                 (error 'http-size-limit-exceeded
                        :message "QPACK field count exceeds the configured limit."
                        :operation :qpack
                        :limit max-fields
                        :observed (length fields)
                        :kind :fields))
               (when (> total-bytes max-header-bytes)
                 (error 'http-size-limit-exceeded
                        :message "QPACK field section exceeds the configured header-size limit."
                        :operation :qpack
                        :limit max-header-bytes
                        :observed total-bytes
                        :kind :headers))))
      (loop while (< position (length octets))
            do (let ((first (aref octets position)))
                 (cond
                   ((/= 0 (logand first #x80))
                    (multiple-value-bind (index next)
                        (http-kit/http2::%hpack-read-integer octets position 6)
                      (append-entry
                       (%qpack-field-entry
                        dynamic-table (/= 0 (logand first #x40)) index
                        base required-insert-count nil))
                      (setf position next)))
                   ((= (logand first #xc0) #x40)
                    (multiple-value-bind (index next)
                        (http-kit/http2::%hpack-read-integer octets position 4)
                      (let ((entry
                              (%qpack-field-entry
                               dynamic-table (/= 0 (logand first #x10)) index
                               base required-insert-count nil)))
                        (multiple-value-bind (value after-value)
                            (%qpack-decode-string octets next 7 #x80)
                          (append-field
                           (if (qpack-dynamic-entry-p entry)
                               (qpack-dynamic-entry-name entry)
                               (first entry))
                           value)
                          (setf position after-value)))))
                   ((= (logand first #xe0) #x20)
                    (multiple-value-bind (name next)
                        (%qpack-decode-string octets position 3 #x08)
                      (multiple-value-bind (value after-value)
                          (%qpack-decode-string octets next 7 #x80)
                        (append-field name value)
                        (setf position after-value))))
                   ((= (logand first #xf0) #x10)
                    (multiple-value-bind (index next)
                        (http-kit/http2::%hpack-read-integer octets position 4)
                      (append-entry
                       (%qpack-field-entry
                        dynamic-table nil index base required-insert-count t))
                      (setf position next)))
                   ((= (logand first #xf0) 0)
                    (multiple-value-bind (index next)
                        (http-kit/http2::%hpack-read-integer octets position 3)
                      (let ((entry
                              (%qpack-field-entry
                               dynamic-table nil index base required-insert-count t)))
                        (multiple-value-bind (value after-value)
                            (%qpack-decode-string octets next 7 #x80)
                          (append-field (qpack-dynamic-entry-name entry) value)
                          (setf position after-value)))))
                   (t
                    (%qpack-error
                     "QPACK field representation has an invalid prefix."
                     first)))
                 (check-limits)))
    (nreverse fields))))
