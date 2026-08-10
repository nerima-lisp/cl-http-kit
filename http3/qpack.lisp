(in-package #:http-kit/http3)

(defparameter +qpack-static-table+
  (vector
   (list ":authority" "")
   (list ":path" "/")
   (list "age" "0")
   (list "content-disposition" "")
   (list "content-length" "0")
   (list "cookie" "")
   (list "date" "")
   (list "etag" "")
   (list "if-modified-since" "")
   (list "if-none-match" "")
   (list "last-modified" "")
   (list "link" "")
   (list "location" "")
   (list "referer" "")
   (list "set-cookie" "")
   (list ":method" "CONNECT")
   (list ":method" "DELETE")
   (list ":method" "GET")
   (list ":method" "HEAD")
   (list ":method" "OPTIONS")
   (list ":method" "POST")
   (list ":method" "PUT")
   (list ":scheme" "http")
   (list ":scheme" "https")
   (list ":status" "103")
   (list ":status" "200")
   (list ":status" "304")
   (list ":status" "404")
   (list ":status" "503")
   (list "accept" "*/*")
   (list "accept" "application/dns-message")
   (list "accept-encoding" "gzip, deflate, br")
   (list "accept-ranges" "bytes")
   (list "access-control-allow-headers" "cache-control")
   (list "access-control-allow-headers" "content-type")
   (list "access-control-allow-origin" "*")
   (list "cache-control" "max-age=0")
   (list "cache-control" "max-age=2592000")
   (list "cache-control" "max-age=604800")
   (list "cache-control" "no-cache")
   (list "cache-control" "no-store")
   (list "cache-control" "public, max-age=31536000")
   (list "content-encoding" "br")
   (list "content-encoding" "gzip")
   (list "content-type" "application/dns-message")
   (list "content-type" "application/javascript")
   (list "content-type" "application/json")
   (list "content-type" "application/x-www-form-urlencoded")
   (list "content-type" "image/gif")
   (list "content-type" "image/jpeg")
   (list "content-type" "image/png")
   (list "content-type" "text/css")
   (list "content-type" "text/html; charset=utf-8")
   (list "content-type" "text/plain")
   (list "content-type" "text/plain;charset=utf-8")
   (list "range" "bytes=0-")
   (list "strict-transport-security" "max-age=31536000")
   (list "strict-transport-security" "max-age=31536000; includesubdomains")
   (list "strict-transport-security"
         "max-age=31536000;includesubdomains; preload")
   (list "vary" "accept-encoding")
   (list "vary" "origin")
   (list "x-content-type-options" "nosniff")
   (list "x-xss-protection" "1; mode=block")
   (list ":status" "100")
   (list ":status" "204")
   (list ":status" "206")
   (list ":status" "302")
   (list ":status" "400")
   (list ":status" "403")
   (list ":status" "421")
   (list ":status" "425")
   (list ":status" "500")
   (list "accept-language" "")
   (list "access-control-allow-credentials" "FALSE")
   (list "access-control-allow-credentials" "TRUE")
   (list "access-control-allow-headers" "*")
   (list "access-control-allow-methods" "get")
   (list "access-control-allow-methods" "get, post, options")
   (list "access-control-allow-methods" "options")
   (list "access-control-expose-headers" "content-length")
   (list "access-control-request-headers" "content-type")
   (list "access-control-request-method" "get")
   (list "access-control-request-method" "post")
   (list "alt-svc" "clear")
   (list "authorization" "")
   (list "content-security-policy"
         "script-src 'none'; object-src 'none'; base-uri 'none'")
   (list "early-data" "1")
   (list "expect-ct" "")
   (list "forwarded" "")
   (list "if-range" "")
   (list "origin" "")
   (list "purpose" "prefetch")
   (list "server" "")
   (list "timing-allow-origin" "*")
   (list "upgrade-insecure-requests" "1")
   (list "user-agent" "")
   (list "x-forwarded-for" "")
   (list "x-frame-options" "deny")
   (list "x-frame-options" "sameorigin")))

(defun %qpack-error (message &optional detail)
  (error 'http-protocol-error
         :message message
         :operation :qpack
         :detail detail))

(defstruct (qpack-dynamic-entry
             (:constructor %make-qpack-dynamic-entry))
  "One QPACK dynamic-table entry.

ABSOLUTE-INDEX is zero based, while the QPACK insert count is one based.  The
entries in a QPACK-DYNAMIC-TABLE are kept newest first, which makes relative
references directly correspond to list positions."
  (absolute-index 0 :type integer)
  (name "" :type string)
  (value "" :type string)
  (size 0 :type integer))

(defstruct (qpack-dynamic-table
             (:constructor %make-qpack-dynamic-table))
  "State for the QPACK dynamic table.

This table implements the insertion, eviction, and index calculations needed
by both the encoder and decoder.  Stream blocking and acknowledgement policy
remain transport concerns; a field section may only reference entries already
present in this table."
  (max-capacity 0 :type integer)
  (capacity 0 :type integer)
  (entries '() :type list)
  (size 0 :type integer)
  (insert-count 0 :type integer))

(defun make-qpack-dynamic-table (&key (max-capacity 0) (capacity max-capacity))
  "Create a QPACK dynamic table with MAX-CAPACITY and current CAPACITY."
  (unless (and (integerp max-capacity) (>= max-capacity 0))
    (%qpack-error "QPACK maximum table capacity must be a non-negative integer."
                  max-capacity))
  (unless (and (integerp capacity)
               (>= capacity 0)
               (<= capacity max-capacity))
    (%qpack-error "QPACK table capacity must be between zero and its maximum."
                  capacity))
  (%make-qpack-dynamic-table :max-capacity max-capacity
                             :capacity capacity))

(defun qpack-dynamic-table-set-capacity (table capacity)
  "Set TABLE's capacity, evicting its oldest entries as required."
  (unless (qpack-dynamic-table-p table)
    (%qpack-error "A QPACK dynamic table object is required." table))
  (unless (and (integerp capacity)
               (>= capacity 0)
               (<= capacity (qpack-dynamic-table-max-capacity table)))
    (%qpack-error "QPACK table capacity is outside the advertised maximum."
                  capacity))
  (setf (qpack-dynamic-table-capacity table) capacity)
  (loop while (> (qpack-dynamic-table-size table) capacity)
        do (let* ((entries (qpack-dynamic-table-entries table))
                  (oldest (car (last entries))))
             (unless oldest
               (return))
             (setf (qpack-dynamic-table-entries table)
                   (butlast entries)
                   (qpack-dynamic-table-size table)
                   (- (qpack-dynamic-table-size table)
                      (qpack-dynamic-entry-size oldest)))))
  table)

(defun %qpack-entry-size (name value)
  (+ 32
     (length (http-kit::%string-octets name))
     (length (http-kit::%string-octets value))))

(defun qpack-dynamic-table-insert (table name value)
  "Insert NAME and VALUE, evicting oldest entries when necessary.

The operation signals an HTTP protocol error when the entry cannot fit in the
current capacity, as required by RFC 9204."
  (unless (qpack-dynamic-table-p table)
    (%qpack-error "A QPACK dynamic table object is required." table))
  (let* ((normalized-name (%qpack-normalize-name name))
         (normalized-value (%qpack-normalize-value value))
         (entry-size (%qpack-entry-size normalized-name normalized-value))
         (capacity (qpack-dynamic-table-capacity table)))
    (when (> entry-size capacity)
      (%qpack-error "A QPACK dynamic-table entry exceeds the table capacity."
                    entry-size))
    (loop while (> (+ (qpack-dynamic-table-size table) entry-size)
                   capacity)
          do (let* ((entries (qpack-dynamic-table-entries table))
                    (oldest (car (last entries))))
               (unless oldest
                 (%qpack-error "QPACK dynamic-table eviction state is invalid."))
               (setf (qpack-dynamic-table-entries table)
                     (butlast entries)
                     (qpack-dynamic-table-size table)
                     (- (qpack-dynamic-table-size table)
                        (qpack-dynamic-entry-size oldest)))))
    (let ((entry (%make-qpack-dynamic-entry
                  :absolute-index (qpack-dynamic-table-insert-count table)
                  :name normalized-name
                  :value normalized-value
                  :size entry-size)))
      (push entry (qpack-dynamic-table-entries table))
      (incf (qpack-dynamic-table-size table) entry-size)
      (incf (qpack-dynamic-table-insert-count table))
      entry)))

(defun %qpack-concat (&rest parts)
  (let ((result (make-array (reduce #'+ parts :key #'length :initial-value 0)
                            :element-type '(unsigned-byte 8)))
        (position 0))
    (dolist (part parts result)
      (replace result part :start1 position)
      (incf position (length part)))))

(defun %qpack-field-pair (field)
  (cond
    ((http-kit:http-header-p field)
     (cons (http-kit:http-header-name field)
           (http-kit:http-header-content field)))
    ((and (consp field) (stringp (car field)) (stringp (cdr field))) field)
    ((and (consp field)
          (stringp (car field))
          (consp (cdr field))
          (stringp (cadr field))
          (null (cddr field)))
     (cons (car field) (cadr field)))
    (t
     (%qpack-error "QPACK fields must be HTTP-HEADER values or name/value pairs."
                   field))))

(defun %qpack-valid-name-p (name)
  (and (stringp name)
       (plusp (length name))
       (loop for character across name
             for code = (char-code character)
             for valid = (or (and (>= code (char-code #\a))
                                  (<= code (char-code #\z)))
                             (and (>= code (char-code #\0))
                                  (<= code (char-code #\9)))
                             (member character '(#\! #\# #\$ #\% #\& #\' #\* #\+
                                                   #\- #\. #\^ #\_ #\` #\| #\~)
                                     :test #'char=))
             always valid)))

(defun %qpack-normalize-name (name)
  (unless (stringp name)
    (%qpack-error "QPACK field names must be strings." name))
  (let ((normalized (string-downcase name)))
    (unless (and (plusp (length normalized))
                 (if (char= (char normalized 0) #\:)
                     (and (> (length normalized) 1)
                          (%qpack-valid-name-p (subseq normalized 1)))
                     (%qpack-valid-name-p normalized)))
      (%qpack-error "QPACK field names must be lowercase HTTP names or pseudo-fields."
                    name))
    normalized))

(defun %qpack-normalize-value (value)
  (unless (and (stringp value) (http-kit::%header-value-p value))
    (%qpack-error "QPACK field values must be strings without controls or CRLF."
                  value))
  value)

(defun %qpack-fields (fields)
  (let ((regular-seen-p nil)
        (result '()))
    (dolist (field fields (nreverse result))
      (let* ((pair (%qpack-field-pair field))
             (name (%qpack-normalize-name (car pair)))
             (value (%qpack-normalize-value (cdr pair))))
        (if (char= (char name 0) #\:)
            (when regular-seen-p
              (%qpack-error "QPACK pseudo-fields must precede regular fields."
                            name))
            (setf regular-seen-p t))
        (push (cons name value) result)))))

(defun %qpack-static-entry (index)
  (when (and (integerp index)
             (<= 0 index)
             (< index (length +qpack-static-table+)))
    (aref +qpack-static-table+ index)))

(defun %qpack-static-index (name value)
  (loop for index below (length +qpack-static-table+)
        for entry = (aref +qpack-static-table+ index)
        when (and (string= name (first entry))
                  (string= value (second entry)))
          do (return index)))

(defun %qpack-static-name-index (name)
  (loop for index below (length +qpack-static-table+)
        for entry = (aref +qpack-static-table+ index)
        when (string= name (first entry))
          do (return index)))

(defun %qpack-sensitive-name-p (name)
  (member name '("authorization" "cookie" "proxy-authorization" "set-cookie")
          :test #'string=))

(defun %qpack-dynamic-entry-at-relative (table index)
  (unless (and (qpack-dynamic-table-p table)
               (integerp index)
               (>= index 0))
    (%qpack-error "QPACK dynamic-table relative indexes must be non-negative integers."
                  index))
  (or (nth index (qpack-dynamic-table-entries table))
      (%qpack-error "QPACK dynamic-table relative index is unavailable." index)))

(defun %qpack-dynamic-entry-at-absolute (table index)
  (unless (and (qpack-dynamic-table-p table)
               (integerp index)
               (>= index 0))
    (%qpack-error "QPACK dynamic-table absolute indexes must be non-negative integers."
                  index))
  (or (find index (qpack-dynamic-table-entries table)
            :key #'qpack-dynamic-entry-absolute-index
            :test #'=)
      (%qpack-error "QPACK dynamic-table absolute index is unavailable." index)))

(defun %qpack-dynamic-name-entry (table name)
  (loop for entry in (qpack-dynamic-table-entries table)
        when (string= name (qpack-dynamic-entry-name entry))
          do (return entry)))

(defun %qpack-dynamic-exact-entry (table name value)
  (loop for entry in (qpack-dynamic-table-entries table)
        when (and (string= name (qpack-dynamic-entry-name entry))
                  (string= value (qpack-dynamic-entry-value entry)))
          do (return entry)))

(defun %qpack-validate-index (index context)
  (unless (and (integerp index) (>= index 0))
    (%qpack-error (format nil "~A must be a non-negative integer." context)
                  index))
  index)

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

(defun %qpack-max-entries (max-capacity)
  (floor max-capacity 32))

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

(defun %qpack-effective-max-capacity (dynamic-table max-table-capacity)
  (let ((capacity (if max-table-capacity
                      max-table-capacity
                      (if dynamic-table
                          (qpack-dynamic-table-max-capacity dynamic-table)
                          0))))
    (unless (and (integerp capacity) (>= capacity 0))
      (%qpack-error
       "QPACK maximum table capacity must be a non-negative integer."
       capacity))
    (when (and dynamic-table
               (> capacity (qpack-dynamic-table-max-capacity dynamic-table)))
      (%qpack-error
       "QPACK field-section capacity exceeds the decoder table maximum."
       capacity))
    capacity))

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

(defun qpack-process-encoder-stream (table octets)
  "Apply complete QPACK encoder-stream instructions to TABLE.

Returns two values: an event list and the consumed position.  The parser is
intentionally strict and signals when OCTETS ends in the middle of an
instruction; stream buffering belongs to the HTTP/3 transport."
  (unless (qpack-dynamic-table-p table)
    (%qpack-error "A QPACK dynamic table object is required." table))
  (unless (%http3-octet-vector-p octets)
    (%qpack-error "QPACK encoder streams must be octet vectors." (type-of octets)))
  (let ((position 0)
        (events '()))
    (loop while (< position (length octets))
          do (let ((first (aref octets position)))
               (cond
                 ((= (logand first #xe0) #x20)
                  (multiple-value-bind (capacity next)
                      (http-kit/http2::%hpack-read-integer octets position 5)
                    (qpack-dynamic-table-set-capacity table capacity)
                    (push (list :set-capacity capacity) events)
                    (setf position next)))
                 ((/= 0 (logand first #x80))
                  (let ((static-p (/= 0 (logand first #x40))))
                    (multiple-value-bind (name-index next)
                        (http-kit/http2::%hpack-read-integer octets position 6)
                      (let ((entry
                              (if static-p
                                  (or (%qpack-static-entry name-index)
                                      (%qpack-error
                                       "QPACK static-table name index is out of range."
                                       name-index))
                                  (%qpack-dynamic-entry-at-relative
                                   table name-index))))
                        (multiple-value-bind (value after-value)
                            (%qpack-decode-string octets next 7 #x80)
                          (let ((inserted
                                  (qpack-dynamic-table-insert
                                   table
                                   (if (qpack-dynamic-entry-p entry)
                                       (qpack-dynamic-entry-name entry)
                                       (first entry))
                                   value)))
                            (push (list :insert inserted) events)
                            (setf position after-value)))))))
                 ((= (logand first #xc0) #x40)
                  (multiple-value-bind (name next)
                      (%qpack-decode-string octets position 5 #x20)
                    (multiple-value-bind (value after-value)
                        (%qpack-decode-string octets next 7 #x80)
                      (let ((inserted (qpack-dynamic-table-insert
                                       table name value)))
                        (push (list :insert inserted) events)
                        (setf position after-value)))))
                 (t
                  (multiple-value-bind (relative-index next)
                      (http-kit/http2::%hpack-read-integer octets position 5)
                    (let* ((source (%qpack-dynamic-entry-at-relative
                                    table relative-index))
                           (inserted
                             (qpack-dynamic-table-insert
                              table
                              (qpack-dynamic-entry-name source)
                              (qpack-dynamic-entry-value source))))
                      (push (list :duplicate inserted) events)
                      (setf position next)))))))
    (values (nreverse events) position)))

(defun qpack-process-decoder-stream
    (octets &key on-section-acknowledgment on-stream-cancellation
            on-insert-count-increment)
  "Decode QPACK decoder-stream instructions and return event records."
  (unless (%http3-octet-vector-p octets)
    (%qpack-error "QPACK decoder streams must be octet vectors." (type-of octets)))
  (dolist (callback (list on-section-acknowledgment
                          on-stream-cancellation
                          on-insert-count-increment))
    (when (and callback (not (functionp callback)))
      (%qpack-error "QPACK decoder-stream callbacks must be functions." callback)))
  (let ((position 0)
        (events '()))
    (loop while (< position (length octets))
          do (let ((first (aref octets position)))
               (cond
                 ((/= 0 (logand first #x80))
                  (multiple-value-bind (stream-id next)
                      (http-kit/http2::%hpack-read-integer octets position 7)
                    (when on-section-acknowledgment
                      (funcall on-section-acknowledgment stream-id))
                    (push (list :section-acknowledgment stream-id) events)
                    (setf position next)))
                 ((= (logand first #xc0) #x40)
                  (multiple-value-bind (stream-id next)
                      (http-kit/http2::%hpack-read-integer octets position 6)
                    (when on-stream-cancellation
                      (funcall on-stream-cancellation stream-id))
                    (push (list :stream-cancellation stream-id) events)
                    (setf position next)))
                 (t
                  (multiple-value-bind (increment next)
                      (http-kit/http2::%hpack-read-integer octets position 6)
                    (unless (plusp increment)
                      (%qpack-error
                       "QPACK insert-count increments must be positive."
                       increment))
                    (when on-insert-count-increment
                      (funcall on-insert-count-increment increment))
                    (push (list :insert-count-increment increment) events)
                    (setf position next))))))
    (values (nreverse events) position)))
