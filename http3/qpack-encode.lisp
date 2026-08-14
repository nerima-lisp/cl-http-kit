(in-package #:http-kit/http3)

(defun %qpack-encode-string
    (string &key (prefix-bits 7) (prefix 0) (huffman-p nil))
  (let* ((octets (http-kit::%string-octets string))
         (encoded-octets
           (if huffman-p
               (http-kit/http2::%hpack-huffman-encode octets)
               octets)))
    (%qpack-concat
     (http-kit/http2::%hpack-encode-integer
      (length encoded-octets)
      prefix-bits
      (if huffman-p
          (logior prefix (ash 1 prefix-bits))
          prefix))
     encoded-octets)))

(defun qpack-encode-set-dynamic-table-capacity (capacity)
  "Encode a QPACK encoder-stream table-capacity instruction."
  (%qpack-validate-index capacity "QPACK table capacity")
  (%qpack-concat
   (http-kit/http2::%hpack-encode-integer capacity 5 #x20)))

(defun qpack-encode-insert-with-name-reference
    (name-index value &key (static-p t) (huffman-p nil))
  "Encode an encoder-stream insertion using a static or relative name index."
  (%qpack-validate-index name-index "QPACK name index")
  (%qpack-concat
   (http-kit/http2::%hpack-encode-integer
    name-index 6 (if static-p #xc0 #x80))
   (%qpack-encode-string value :huffman-p huffman-p)))

(defun qpack-encode-insert-with-literal-name (name value &key (huffman-p nil))
  "Encode an encoder-stream insertion carrying a literal field name.

When HUFFMAN-P is true, both the literal name and value use HPACK Huffman
coding."
  (let* ((normalized-name (%qpack-normalize-name name))
         (name-octets (http-kit::%string-octets normalized-name))
         (encoded-name-octets
           (if huffman-p
               (http-kit/http2::%hpack-huffman-encode name-octets)
               name-octets)))
    (%qpack-concat
     (http-kit/http2::%hpack-encode-integer
      (length encoded-name-octets) 5
      (if huffman-p #x60 #x40))
     encoded-name-octets
     (%qpack-encode-string (%qpack-normalize-value value)
                           :huffman-p huffman-p))))

(defun qpack-encode-duplicate (relative-index)
  "Encode an encoder-stream duplicate instruction."
  (%qpack-validate-index relative-index "QPACK duplicate index")
  (%qpack-concat
   (http-kit/http2::%hpack-encode-integer relative-index 5 0)))

(defun qpack-encode-section-acknowledgment (stream-id)
  "Encode a decoder-stream field-section acknowledgment."
  (%qpack-validate-index stream-id "QPACK stream ID")
  (%qpack-concat
   (http-kit/http2::%hpack-encode-integer stream-id 7 #x80)))

(defun qpack-encode-stream-cancellation (stream-id)
  "Encode a decoder-stream stream-cancellation instruction."
  (%qpack-validate-index stream-id "QPACK stream ID")
  (%qpack-concat
   (http-kit/http2::%hpack-encode-integer stream-id 6 #x40)))

(defun qpack-encode-insert-count-increment (increment)
  "Encode a decoder-stream insert-count increment instruction."
  (unless (and (integerp increment) (plusp increment))
    (%qpack-error "QPACK insert-count increments must be positive integers."
                  increment))
  (%qpack-concat
   (http-kit/http2::%hpack-encode-integer increment 6 0)))

(defun %qpack-field-dynamic-entry (table name value)
  (when table
    (let ((static-index (%qpack-static-index name value))
          (static-name-index (%qpack-static-name-index name)))
      (cond
        ((and (null static-index)
              (not (%qpack-sensitive-name-p name)))
         (%qpack-dynamic-exact-entry table name value))
        ((null static-name-index)
         (%qpack-dynamic-name-entry table name))
        (t nil)))))

(defun %qpack-encode-field (name value dynamic-table base huffman-p)
  (let* ((indexed (%qpack-static-index name value))
         (name-index (%qpack-static-name-index name))
         (dynamic-entry (%qpack-field-dynamic-entry
                         dynamic-table name value))
         (sensitive (if (%qpack-sensitive-name-p name) #x20 0)))
    (cond
      (indexed
       (%qpack-concat
        (http-kit/http2::%hpack-encode-integer indexed 6 #xc0)))
      ((and dynamic-entry
            (string= value (qpack-dynamic-entry-value dynamic-entry))
            (not (%qpack-sensitive-name-p name)))
       (let ((relative-index
               (- base (qpack-dynamic-entry-absolute-index dynamic-entry) 1)))
         (%qpack-validate-index relative-index
                                "QPACK dynamic reference index")
         (%qpack-concat
          (http-kit/http2::%hpack-encode-integer relative-index 6 #x80))))
      (name-index
       (%qpack-concat
        (http-kit/http2::%hpack-encode-integer name-index 4
                                               (logior #x50 sensitive))
        (%qpack-encode-string value :huffman-p huffman-p)))
      (dynamic-entry
       (let ((relative-index
               (- base (qpack-dynamic-entry-absolute-index dynamic-entry) 1)))
         (%qpack-validate-index relative-index
                                "QPACK dynamic name reference index")
         (%qpack-concat
          (http-kit/http2::%hpack-encode-integer
           relative-index 4 (logior #x40 sensitive))
          (%qpack-encode-string value :huffman-p huffman-p))))
      (t
       (let* ((name-octets (http-kit::%string-octets name))
              (encoded-name-octets
                (if huffman-p
                    (http-kit/http2::%hpack-huffman-encode name-octets)
                    name-octets)))
         (%qpack-concat
          (http-kit/http2::%hpack-encode-integer
           (length encoded-name-octets) 3
           (logior #x20
                   (if sensitive #x10 0)
                   (if huffman-p #x08 0)))
          encoded-name-octets
          (%qpack-encode-string value :huffman-p huffman-p)))))))

(defun %qpack-encode-required-insert-count (required-insert-count
                                            max-capacity)
  (unless (and (integerp required-insert-count)
               (>= required-insert-count 0))
    (%qpack-error "QPACK required insert count must be non-negative."
                  required-insert-count))
  (if (zerop required-insert-count)
      0
      (let ((full-range (* 2 (%qpack-max-entries max-capacity))))
        (when (zerop full-range)
          (%qpack-error
           "A non-zero QPACK required insert count needs dynamic-table capacity."
           max-capacity))
        (1+ (mod required-insert-count full-range)))))

(defun %qpack-encode-base (required-insert-count base)
  (if (>= base required-insert-count)
      (http-kit/http2::%hpack-encode-integer
       (- base required-insert-count) 7 0)
      (http-kit/http2::%hpack-encode-integer
       (- required-insert-count base 1) 7 #x80)))

(defun qpack-encode-field-section (fields &key dynamic-table (huffman-p nil))
  "Encode FIELDS using static and, when supplied, dynamic QPACK entries.

DYNAMIC-TABLE is the encoder's current table.  The function only references
entries already inserted in that table; callers must send the corresponding
encoder-stream instructions before a peer can decode a non-zero required
insert count.  When HUFFMAN-P is true, string literals use HPACK Huffman
coding."
  (when (and dynamic-table (not (qpack-dynamic-table-p dynamic-table)))
    (%qpack-error "DYNAMIC-TABLE must be a QPACK dynamic table object."
                  dynamic-table))
  (let* ((normalized (%qpack-fields fields))
         (required-insert-count
           (loop for field in normalized
                 for entry = (%qpack-field-dynamic-entry
                              dynamic-table (car field) (cdr field))
                 maximize (if entry
                              (1+ (qpack-dynamic-entry-absolute-index entry))
                              0)))
         (base (if (plusp required-insert-count)
                   (qpack-dynamic-table-insert-count dynamic-table)
                   0))
         (max-capacity (if dynamic-table
                           (qpack-dynamic-table-max-capacity dynamic-table)
                           0))
         (section (%qpack-concat
                   (http-kit/http2::%hpack-encode-integer
                    (%qpack-encode-required-insert-count
                     required-insert-count max-capacity)
                    8 0)
                   (%qpack-encode-base required-insert-count base))))
    (dolist (field normalized section)
      (setf section (%qpack-concat
                     section
                     (%qpack-encode-field (car field)
                                          (cdr field)
                                          dynamic-table
                                          base
                                          huffman-p))))))
