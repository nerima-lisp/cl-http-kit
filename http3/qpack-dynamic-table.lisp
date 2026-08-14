(in-package #:http-kit/http3)

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
       (not (string= name ""))
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
    (unless (and (not (string= normalized ""))
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

(defun %qpack-max-entries (max-capacity)
  (floor max-capacity 32))

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
