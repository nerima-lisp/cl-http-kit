(in-package #:http-kit/test)

(deftest http3-defensive-qpack-section-ownership
  (let* ((table (http-kit/http3:make-qpack-dynamic-table
                 :max-capacity 34 :capacity 34))
         (state (http-kit/http3:make-qpack-decoder-stream-state table)))
    (http-kit/http3:qpack-dynamic-table-insert table "a" "1")
    (http-kit/http3:qpack-decoder-stream-state-note-insertions-sent state 1)
    ;; Equal sections are distinct owners, even on the same stream.
    (dolist (id '(4 4 8))
      (http-kit/http3:qpack-decoder-stream-state-register-section
       state id 1 '(0 0)))
    (flet ((receive (wire expected)
             (multiple-value-bind (events consumed)
                 (http-kit/http3:qpack-process-decoder-stream wire :state state)
               (ensure-equal (list expected) events)
               (ensure-equal (length wire) consumed)))
           (protected ()
             (http3-codec-check-protected-eviction
              (lambda ()
                (http-kit/http3:qpack-dynamic-table-set-capacity table 0)) 0)
             (ensure-equal 34 (http-kit/http3:qpack-dynamic-table-capacity table))
             (ensure-equal 34 (http-kit/http3:qpack-dynamic-table-size table))))
      (receive (http-kit/http3:qpack-encode-section-acknowledgment 4)
               '(:section-acknowledgment 4))
      (protected)
      (receive (http-kit/http3:qpack-encode-stream-cancellation 4)
               '(:stream-cancellation 4))
      (protected)
      (dolist (wire (list (http-kit/http3:qpack-encode-section-acknowledgment 4)
                         (http-kit/http3:qpack-encode-stream-cancellation 4)))
        (let ((notifications 0)
              (failure nil))
          (setf failure
                (handler-case
                    (http-kit/http3:qpack-process-decoder-stream
                     wire :state state
                     :on-section-acknowledgment
                     (lambda (id) (declare (ignore id)) (incf notifications))
                     :on-stream-cancellation
                     (lambda (id) (declare (ignore id)) (incf notifications)))
                  (http-protocol-error (condition) condition)))
          (ensure-true (typep failure 'http-protocol-error))
          (ensure-equal :qpack (http-error-operation failure))
          (ensure-equal 4 (http-protocol-error-detail failure))
          (ensure-equal 0 notifications))
        (protected))
      (receive (http-kit/http3:qpack-encode-section-acknowledgment 8)
               '(:section-acknowledgment 8)))
    (http-kit/http3:qpack-dynamic-table-set-capacity table 0)
    (ensure-equal 0 (http-kit/http3:qpack-dynamic-table-size table))
    (ensure-equal nil (http-kit/http3:qpack-dynamic-table-entries table))
    (ensure-equal 1 (http-kit/http3:qpack-dynamic-table-insert-count table))))

(deftest http3-codec-push-priority-requires-max-push-id
  (let* ((state (http-kit/http3:make-http3-control-state
                 :peer-role :client :promised-push-ids '(0)))
         (frame (http-kit/http3:make-http3-priority-update-frame
                 :element-id 0 :priority-field-value "u=1" :kind :push)))
    (ensure-equal :settings
                  (http-kit/http3:process-http3-control-frame
                   state (http-kit/http3:make-http3-settings-frame)))
    (let ((failure
            (handler-case
                (http-kit/http3:process-http3-control-frame state frame)
              (http-protocol-error (condition) condition))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :http3-control-stream (http-error-operation failure))
      (ensure-equal :h3-id-error (http-protocol-error-detail failure)))
    (ensure-equal nil (http-kit/http3:http3-control-state-max-push-id state))
    (ensure-equal nil (http-kit/http3:http3-control-state-priority-updates state))
    (ensure-equal '(0) (http-kit/http3:http3-control-state-promised-push-ids state))
    (ensure-equal :max-push-id
                  (http-kit/http3:process-http3-control-frame
                   state (http-kit/http3:make-http3-frame
                          :type http-kit/http3:+http3-max-push-id-type+
                          :payload (http-kit/http3:http3-varint-encode 0))))
    (ensure-equal :priority-update
                  (http-kit/http3:process-http3-control-frame state frame))
    (ensure-equal '(((:push . 0) . "u=1"))
                  (http-kit/http3:http3-control-state-priority-updates state))
    (ensure-equal :priority-update
                  (http-kit/http3:process-http3-control-frame
                   state (http-kit/http3:make-http3-priority-update-frame
                          :element-id 0 :priority-field-value "u=7" :kind :push)))
    (ensure-equal '(((:push . 0) . "u=7"))
                  (http-kit/http3:http3-control-state-priority-updates state))))

(deftest http3-codec-qpack-digit-names-and-empty-pseudo-name
  (dolist (huffman '(nil t))
    (let* ((fields '(("9X-0123456789" . "value")))
           (wire (http-kit/http3:qpack-encode-field-section
                  fields :huffman-p huffman)))
      (ensure-true (plusp (length wire)))
      (ensure-equal '(("9x-0123456789" . "value"))
                    (http-kit/http3:qpack-decode-field-section wire))
      (ensure-equal '(("9X-0123456789" . "value")) fields)))
  (let ((failure
          (handler-case
              (http-kit/http3:qpack-encode-field-section '((":" . "value")))
            (http-protocol-error (condition) condition))))
    (ensure-true (typep failure 'http-protocol-error))
    (ensure-equal :qpack (http-error-operation failure))
    (ensure-equal ":" (http-protocol-error-detail failure))))

(deftest http3-codec-qpack-nonstring-list-name-is-rejected
  (dolist (name '(nil 7 #\x))
    (let* ((field (list name "value"))
           (fields (list field))
           (before (copy-tree fields))
           (failure
             (handler-case
                 (http-kit/http3:qpack-encode-field-section fields)
               (http-protocol-error (condition) condition))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :qpack (http-error-operation failure))
      (ensure-equal field (http-protocol-error-detail failure))
      (ensure-equal before fields)))
  (let* ((fields '(("x-name" "value")))
         (wire (http-kit/http3:qpack-encode-field-section fields)))
    (ensure-equal '(("x-name" . "value"))
                  (http-kit/http3:qpack-decode-field-section wire))
    (ensure-equal '(("x-name" "value")) fields)))

(deftest http3-codec-qpack-sensitive-equal-value-is-never-indexed
  (dolist (huffman '(nil t))
    (let* ((fields '(("proxy-authorization" . "same")))
           (table (http-kit/http3:make-qpack-dynamic-table
                   :max-capacity 128 :capacity 128))
           (entry (http-kit/http3:qpack-dynamic-table-insert
                   table "proxy-authorization" "same"))
           (state (http-kit/http3:make-qpack-decoder-stream-state
                   table :blocked-stream-limit 1)))
      (http-kit/http3:qpack-decoder-stream-state-note-insertions-sent state 1)
      (let ((wire (http-kit/http3:qpack-encode-field-section
                   fields :dynamic-table table :decoder-stream-state state
                   :stream-id 4 :huffman-p huffman)))
        (ensure-equal 2 (aref wire 0))
        (ensure-equal 0 (aref wire 1))
        (ensure-equal #x60 (logand #xf0 (aref wire 2)))
        (ensure-true (> (length wire) 3))
        (ensure-equal fields
                      (http-kit/http3:qpack-decode-field-section
                       wire :dynamic-table table))
        (ensure-equal 1 (length (gethash
                                4 (http-kit/http3:qpack-decoder-stream-state-outstanding-sections
                                   state))))
        (ensure-equal "same" (http-kit/http3:qpack-dynamic-entry-value entry))
        (ensure-equal 1 (http-kit/http3:qpack-dynamic-table-insert-count table)))
      (http-kit/http3:qpack-process-decoder-stream
       (http-kit/http3:qpack-encode-section-acknowledgment 4) :state state)
      (ensure-equal 1 (http-kit/http3:qpack-decoder-stream-state-known-received-count state))
      (ensure-equal 0 (hash-table-count
                       (http-kit/http3:qpack-decoder-stream-state-outstanding-sections state)))
      (ensure-equal 0 (hash-table-count
                       (http-kit/http3::qpack-dynamic-table-protected-indices table)))
      (ensure-equal fields '(("proxy-authorization" . "same"))))))

(deftest http3-codec-qpack-mixed-sections-retain-blocked-stream-budget
  (let* ((table (http-kit/http3:make-qpack-dynamic-table
                 :max-capacity 128 :capacity 128))
         (empty-table (http-kit/http3:make-qpack-dynamic-table
                       :max-capacity 128 :capacity 128))
         (state (http-kit/http3:make-qpack-decoder-stream-state
                 table :blocked-stream-limit 1)))
    (http-kit/http3:qpack-dynamic-table-insert table "x-a" "one")
    (http-kit/http3:qpack-dynamic-table-insert table "x-b" "two")
    (http-kit/http3:qpack-decoder-stream-state-note-insertions-sent state 2)
    (dolist (fields '((("x-a" . "one")) (("x-b" . "two"))))
      (let ((wire (http-kit/http3:qpack-encode-field-section
                   fields :dynamic-table table :decoder-stream-state state :stream-id 4)))
        (ensure-equal fields
                      (http-kit/http3:qpack-decode-field-section wire :dynamic-table table))))
    (http-kit/http3:qpack-process-decoder-stream
     (http-kit/http3:qpack-encode-insert-count-increment 1) :state state)
    (ensure-equal 1 (http-kit/http3:qpack-decoder-stream-state-known-received-count state))
    (ensure-equal '(1 2)
                  (mapcar #'car (gethash
                                 4 (http-kit/http3:qpack-decoder-stream-state-outstanding-sections state))))
    (let ((wire (http-kit/http3:qpack-encode-field-section
                 '(("x-b" . "two")) :dynamic-table table
                 :decoder-stream-state state :stream-id 8)))
      (ensure-equal 0 (aref wire 0))
      (ensure-equal '(("x-b" . "two"))
                    (http-kit/http3:qpack-decode-field-section wire :dynamic-table empty-table))
      (ensure-equal nil (gethash
                        8 (http-kit/http3:qpack-decoder-stream-state-outstanding-sections state))))
    (let ((wire (http-kit/http3:qpack-encode-field-section
                 '(("x-b" . "two")) :dynamic-table table
                 :decoder-stream-state state :stream-id 4)))
      (ensure-equal 3 (aref wire 0))
      (ensure-equal '(("x-b" . "two"))
                    (http-kit/http3:qpack-decode-field-section wire :dynamic-table table)))
    (ensure-equal '(1 2 2)
                  (mapcar #'car (gethash
                                 4 (http-kit/http3:qpack-decoder-stream-state-outstanding-sections state))))
    (dotimes (index 3)
      (declare (ignore index))
      (http-kit/http3:qpack-process-decoder-stream
       (http-kit/http3:qpack-encode-section-acknowledgment 4) :state state))
    (ensure-equal 2 (http-kit/http3:qpack-decoder-stream-state-known-received-count state))
    (ensure-equal 0 (hash-table-count
                     (http-kit/http3:qpack-decoder-stream-state-outstanding-sections state)))
    (let ((wire (http-kit/http3:qpack-encode-field-section
                 '(("x-b" . "two")) :dynamic-table table
                 :decoder-stream-state state :stream-id 8)))
      (ensure-equal 3 (aref wire 0))
      (ensure-equal '(("x-b" . "two"))
                    (http-kit/http3:qpack-decode-field-section wire :dynamic-table table)))
    (http-kit/http3:qpack-process-decoder-stream
     (http-kit/http3:qpack-encode-section-acknowledgment 8) :state state)
    (ensure-equal 0 (hash-table-count
                     (http-kit/http3:qpack-decoder-stream-state-outstanding-sections state)))
    (ensure-equal 0 (hash-table-count
                     (http-kit/http3::qpack-dynamic-table-protected-indices table)))))

(deftest http3-codec-qpack-blocked-condition-describes-missing-inserts
  (let* ((encoder-table (http-kit/http3:make-qpack-dynamic-table
                         :max-capacity 128 :capacity 128))
         (decoder-table (http-kit/http3:make-qpack-dynamic-table
                         :max-capacity 128 :capacity 128)))
    (http-kit/http3:qpack-dynamic-table-insert encoder-table "x-name" "value")
    (let* ((wire (http-kit/http3:qpack-encode-field-section
                  '(("x-name" . "value")) :dynamic-table encoder-table))
           (failure
             (handler-case
                 (http-kit/http3:qpack-decode-field-section wire :dynamic-table decoder-table)
               (http-kit/http3:qpack-blocked-field-section (condition) condition))))
      (ensure-true (typep failure 'http-kit/http3:qpack-blocked-field-section))
      (ensure-equal 1 (http-kit/http3:qpack-blocked-field-section-required-insert-count failure))
      (ensure-equal 0 (http-kit/http3:qpack-blocked-field-section-current-insert-count failure))
      (let ((report (format nil "~A" failure)))
        (ensure-true (not (null (search "requires insert count 1" report))))
        (ensure-true (not (null (search "only 0 is available" report)))))
      (ensure-equal 0 (http-kit/http3:qpack-dynamic-table-insert-count decoder-table))
      (let ((instruction
              (http-kit/http3:qpack-encode-insert-with-literal-name "x-name" "value")))
        (multiple-value-bind (events consumed)
            (http-kit/http3:qpack-process-encoder-stream decoder-table instruction)
          (ensure-equal '(:insert) (mapcar #'first events))
          (ensure-equal (length instruction) consumed)))
      (ensure-equal '(("x-name" . "value"))
                    (http-kit/http3:qpack-decode-field-section wire :dynamic-table decoder-table)))))

(defun http3-codec-check-protected-eviction (thunk index)
  (let ((failure (handler-case (funcall thunk)
                   (http-protocol-error (condition) condition))))
    (ensure-true (typep failure 'http-protocol-error))
    (ensure-equal :qpack (http-error-operation failure))
    (ensure-equal "QPACK cannot evict an entry referenced by a field section."
                  (http-error-message failure))
    (ensure-equal index (http-protocol-error-detail failure))))

(deftest http3-codec-qpack-protected-eviction-is-atomic
  (let* ((table (http-kit/http3:make-qpack-dynamic-table :max-capacity 68 :capacity 68))
         (oldest (http-kit/http3:qpack-dynamic-table-insert table "a" "1"))
         (newest (http-kit/http3:qpack-dynamic-table-insert table "b" "2"))
         (state (http-kit/http3:make-qpack-decoder-stream-state table :blocked-stream-limit 2)))
    (http-kit/http3:qpack-decoder-stream-state-note-insertions-sent state 2)
    (http-kit/http3:qpack-decoder-stream-state-register-section state 4 2 '(1))
    (dolist (operation (list (lambda () (http-kit/http3:qpack-dynamic-table-set-capacity table 0))
                            (lambda () (http-kit/http3:qpack-dynamic-table-insert table "c" "33"))))
      (http3-codec-check-protected-eviction operation 1)
      (ensure-equal 68 (http-kit/http3:qpack-dynamic-table-capacity table))
      (ensure-equal 68 (http-kit/http3:qpack-dynamic-table-size table))
      (ensure-equal 2 (http-kit/http3:qpack-dynamic-table-insert-count table))
      (ensure-true (eq newest (first (http-kit/http3:qpack-dynamic-table-entries table))))
      (ensure-true (eq oldest (second (http-kit/http3:qpack-dynamic-table-entries table))))
      (ensure-equal '((2 1))
                    (gethash 4 (http-kit/http3:qpack-decoder-stream-state-outstanding-sections state))))
    (let ((wire (http-kit/http3:qpack-encode-section-acknowledgment 4)))
      (multiple-value-bind (events consumed)
          (http-kit/http3:qpack-process-decoder-stream wire :state state)
        (ensure-equal '((:section-acknowledgment 4)) events)
        (ensure-equal (length wire) consumed)))
    (ensure-equal 2 (http-kit/http3:qpack-decoder-stream-state-known-received-count state))
    (ensure-equal 0 (hash-table-count (http-kit/http3:qpack-decoder-stream-state-outstanding-sections state)))
    (let ((entry (http-kit/http3:qpack-dynamic-table-insert table "c" "33")))
      (ensure-equal 2 (http-kit/http3:qpack-dynamic-entry-absolute-index entry))
      (ensure-equal (list entry) (http-kit/http3:qpack-dynamic-table-entries table)))
    (ensure-equal 35 (http-kit/http3:qpack-dynamic-table-size table))
    (ensure-equal 3 (http-kit/http3:qpack-dynamic-table-insert-count table))
    (ensure-equal '(("c" . "33"))
                  (http-kit/http3:qpack-decode-field-section
                   (http-kit/http3:qpack-encode-field-section '(("c" . "33")) :dynamic-table table)
                   :dynamic-table table))
    (http-kit/http3:qpack-dynamic-table-set-capacity table 0)
    (ensure-equal 0 (http-kit/http3:qpack-dynamic-table-size table))
    (ensure-equal nil (http-kit/http3:qpack-dynamic-table-entries table))
    (ensure-equal 3 (http-kit/http3:qpack-dynamic-table-insert-count table))))

(deftest http3-codec-qpack-shared-reference-survives-one-ack
  (let* ((table (http-kit/http3:make-qpack-dynamic-table :max-capacity 34 :capacity 34))
         (entry (http-kit/http3:qpack-dynamic-table-insert table "a" "1"))
         (state (http-kit/http3:make-qpack-decoder-stream-state table :blocked-stream-limit 2)))
    (http-kit/http3:qpack-decoder-stream-state-note-insertions-sent state 1)
    (dolist (id '(4 8))
      (http-kit/http3:qpack-decoder-stream-state-register-section state id 1 '(0)))
    (let ((wire (http-kit/http3:qpack-encode-section-acknowledgment 4)))
      (multiple-value-bind (events consumed)
          (http-kit/http3:qpack-process-decoder-stream wire :state state)
        (ensure-equal '((:section-acknowledgment 4)) events)
        (ensure-equal (length wire) consumed)))
    (ensure-equal nil (gethash 4 (http-kit/http3:qpack-decoder-stream-state-outstanding-sections state)))
    (ensure-equal '((1 0)) (gethash 8 (http-kit/http3:qpack-decoder-stream-state-outstanding-sections state)))
    (http3-codec-check-protected-eviction
     (lambda () (http-kit/http3:qpack-dynamic-table-set-capacity table 0)) 0)
    (ensure-equal 34 (http-kit/http3:qpack-dynamic-table-capacity table))
    (ensure-equal (list entry) (http-kit/http3:qpack-dynamic-table-entries table))
    (ensure-equal 1 (http-kit/http3:qpack-decoder-stream-state-known-received-count state))
    (let ((wire (http-kit/http3:qpack-encode-stream-cancellation 8)))
      (multiple-value-bind (events consumed)
          (http-kit/http3:qpack-process-decoder-stream wire :state state)
        (ensure-equal '((:stream-cancellation 8)) events)
        (ensure-equal (length wire) consumed)))
    (ensure-equal 0 (hash-table-count (http-kit/http3:qpack-decoder-stream-state-outstanding-sections state)))
    (http-kit/http3:qpack-dynamic-table-set-capacity table 0)
    (ensure-equal 0 (http-kit/http3:qpack-dynamic-table-size table))
    (ensure-equal nil (http-kit/http3:qpack-dynamic-table-entries table))
    (ensure-equal 1 (http-kit/http3:qpack-dynamic-table-insert-count table))))

(deftest http3-codec-qpack-sensitive-name-search-skips-unrelated-entries
  (dolist (huffman '(nil t))
    (dolist (matching-name '(nil t))
      (let* ((fields '(("proxy-authorization" . "replacement")))
             (table (http-kit/http3:make-qpack-dynamic-table
                     :max-capacity 256 :capacity 256)))
        (when matching-name
          (http-kit/http3:qpack-dynamic-table-insert
           table "proxy-authorization" "previous"))
        (http-kit/http3:qpack-dynamic-table-insert table "x-unrelated" "value")
        (let* ((before (copy-list (http-kit/http3:qpack-dynamic-table-entries table)))
               (wire (http-kit/http3:qpack-encode-field-section
                      fields :dynamic-table table :huffman-p huffman)))
          (ensure-equal (if matching-name 2 0) (aref wire 0))
          (ensure-equal (if matching-name 1 0) (aref wire 1))
          (ensure-equal (if matching-name #x60 #x30)
                        (logand #xf0 (aref wire 2)))
          (ensure-equal fields (http-kit/http3:qpack-decode-field-section
                                wire :dynamic-table table))
          (ensure-equal before (http-kit/http3:qpack-dynamic-table-entries table))
          (ensure-equal (if matching-name 2 1)
                        (http-kit/http3:qpack-dynamic-table-insert-count table))
          (ensure-equal '(("proxy-authorization" . "replacement")) fields))))))

(deftest http3-codec-qpack-public-blocked-nil-stream-id
  (let* ((encoder (http-kit/http3:make-qpack-dynamic-table
                   :max-capacity 128 :capacity 128))
         (decoder (http-kit/http3:make-qpack-dynamic-table
                   :max-capacity 128 :capacity 128))
         (context (http-kit/http3:make-http3-qpack-decoder-context
                   decoder :blocked-stream-limit 1))
         (callbacks 0)
         (fields '(("x" . "y"))))
    (http-kit/http3:qpack-dynamic-table-insert encoder "x" "y")
    (let* ((wire (http-kit/http3:qpack-encode-field-section
                  fields :dynamic-table encoder))
           (callback (lambda (decoded) (incf callbacks) decoded))
           (failure
             (handler-case
                 (http-kit/http3:decode-http3-qpack-field-section
                  wire context nil :on-decoded callback)
               (http-protocol-error (condition) condition))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :http3-transport (http-error-operation failure))
      (ensure-equal "QPACK field section decompression failed."
                    (http-error-message failure))
      (let* ((detail (http-protocol-error-detail failure))
             (inner (second detail)))
        (ensure-equal 2 (length detail))
        (ensure-equal :qpack-decompression-failed (first detail))
        (ensure-true (typep inner 'http-protocol-error))
        (ensure-equal :qpack (http-error-operation inner))
        (ensure-equal "QPACK stream IDs must be non-negative integers."
                      (http-error-message inner))
        (ensure-equal nil (http-protocol-error-detail inner)))
      (ensure-equal 0 callbacks)
      (let ((blocked (http-kit/http3:decode-http3-qpack-field-section
                      wire context 4 :on-decoded callback)))
        (ensure-true (http-kit/http3:http3-qpack-blocked-stream-p blocked))
        (ensure-equal 4 (http-kit/http3:http3-qpack-blocked-stream-stream-id blocked))
        (ensure-equal 1 (http-kit/http3:http3-qpack-blocked-stream-required-insert-count blocked))
        (ensure-equal 0 callbacks))
      (http-kit/http3:qpack-dynamic-table-insert decoder "x" "y")
      (ensure-equal fields (http-kit/http3:decode-http3-qpack-field-section
                            wire context 4 :on-decoded callback))
      (ensure-equal 1 callbacks))))

(deftest http3-codec-qpack-public-post-base-without-table
  (let ((failure
          (handler-case
              (http-kit/http3:qpack-decode-field-section (octets 0 0 #x10))
            (http-protocol-error (condition) condition))))
    (ensure-true (typep failure 'http-protocol-error))
    (ensure-equal :qpack (http-error-operation failure))
    (ensure-equal "QPACK dynamic-table absolute indexes must be non-negative integers."
                  (http-error-message failure))
    (ensure-equal 0 (http-protocol-error-detail failure)))
  (ensure-equal nil (http-kit/http3:qpack-decode-field-section (octets 0 0)))
  (let ((table (http-kit/http3:make-qpack-dynamic-table
                :max-capacity 64 :capacity 64)))
    (http-kit/http3:qpack-dynamic-table-insert table "x" "1")
    (ensure-equal '(("x" . "1"))
                  (http-kit/http3:qpack-decode-field-section
                   (octets 2 #x80 #x10) :dynamic-table table))))

(deftest http3-codec-qpack-public-known-unacked-blocked-budget
  (dolist (increment '(nil t))
    (let* ((table (http-kit/http3:make-qpack-dynamic-table
                   :max-capacity 128 :capacity 128))
           (state (http-kit/http3:make-qpack-decoder-stream-state
                   table :blocked-stream-limit 1))
           (sections (http-kit/http3:qpack-decoder-stream-state-outstanding-sections state)))
      (http-kit/http3:qpack-dynamic-table-insert table "x-a" "one")
      (http-kit/http3:qpack-dynamic-table-insert table "x-b" "two")
      (http-kit/http3:qpack-decoder-stream-state-note-insertions-sent state 2)
      (let ((wire (http-kit/http3:qpack-encode-field-section
                   '(("x-a" . "one")) :dynamic-table table
                   :decoder-stream-state state :stream-id 4)))
        (ensure-equal (octets 2 1) (subseq wire 0 2))
        (ensure-equal '(("x-a" . "one"))
                      (http-kit/http3:qpack-decode-field-section wire :dynamic-table table)))
      (when increment
        (http-kit/http3:qpack-process-decoder-stream
         (http-kit/http3:qpack-encode-insert-count-increment 1) :state state))
      (ensure-equal (if increment 1 0)
                    (http-kit/http3:qpack-decoder-stream-state-known-received-count state))
      (ensure-true (gethash 4 sections))
      (http3-codec-check-protected-eviction
       (lambda () (http-kit/http3:qpack-dynamic-table-set-capacity table 0)) 0)
      (let ((wire (http-kit/http3:qpack-encode-field-section
                   '(("x-b" . "two")) :dynamic-table table
                   :decoder-stream-state state :stream-id 8)))
        (ensure-equal (if increment (octets 3 0) (octets 0 0))
                      (subseq wire 0 2))
        (ensure-equal '(("x-b" . "two"))
                      (http-kit/http3:qpack-decode-field-section
                       wire :dynamic-table (and increment table)))
        (ensure-equal (if increment 2 1) (hash-table-count sections)))
      (http-kit/http3:qpack-process-decoder-stream
       (http-kit/http3:qpack-encode-section-acknowledgment 4) :state state)
      (ensure-equal nil (gethash 4 sections))
      (when increment
        (ensure-true (gethash 8 sections))
        (http3-codec-check-protected-eviction
         (lambda () (http-kit/http3:qpack-dynamic-table-set-capacity table 0)) 1)
        (http-kit/http3:qpack-process-decoder-stream
         (http-kit/http3:qpack-encode-section-acknowledgment 8) :state state))
      (ensure-equal 0 (hash-table-count sections))
      (ensure-equal (if increment 2 1)
                    (http-kit/http3:qpack-decoder-stream-state-known-received-count state))
      (http-kit/http3:qpack-dynamic-table-set-capacity table 0)
      (ensure-equal 0 (http-kit/http3:qpack-dynamic-table-size table))
      (ensure-equal nil (http-kit/http3:qpack-dynamic-table-entries table)))))

(defun http3-codec-response-frame-check (wire forbidden-type)
  (let* ((before (copy-seq wire))
         (fixture (http3-qpack-contract-fixture wire 0))
         (client (http3-qpack-contract-fixture-client fixture))
         (request (make-http-request :method "GET" :uri "https://example.test/"))
         (request-before (http3-qpack-contract-request-source request))
         (failure nil)
         (response nil))
    (unwind-protect
         (progn
           (handler-case
               (setf response (http-kit/http3:send-http3-request client request))
             (http-protocol-error (condition) (setf failure condition)))
           (if forbidden-type
               (progn
                 (http3-public-error-check
                  failure "HTTP/3 control frames are not valid on a request stream."
                  forbidden-type)
                 (ensure-true (eq nil response)))
               (progn
                 (ensure-true (eq nil failure))
                 (ensure-equal 200 (http-response-status response))
                 (ensure-octets-equal (ascii "ok") (http-response-body response))
                 (ensure-equal '(("x-end" . "yes"))
                               (mapcar (lambda (header)
                                         (cons (http-header-name header)
                                               (http-header-content header)))
                                       (http-response-trailers response)))))
           (let* ((events (reverse (http3-qpack-contract-fixture-events fixture)))
                  (stream (second (first events)))
                  (last-event (car (last events))))
             (ensure-equal '(:open :write :write :read :close) (mapcar #'first events))
             (ensure-true (eq request (third (first events))))
             (ensure-equal :request (http3-test-stream-kind stream))
             (dolist (event events) (ensure-true (eq stream (second event))))
             (ensure-equal '(nil t) (mapcar #'second (reverse (http3-test-stream-writes stream))))
             (ensure-equal nil (http3-test-stream-reads stream))
             (ensure-true (http3-test-stream-closed-p stream))
             (ensure-true (eq failure (third last-event))))
           (ensure-true (http-kit/http3:http3-client-open-p client))
           (ensure-equal request-before (http3-qpack-contract-request-source request))
           (ensure-octets-equal before wire))
      (http3-qpack-contract-cleanup fixture))))

(deftest http3-codec-response-frame-extensions-preserve-response
  (dolist (type '(33 57005))
    (let ((extension (http-kit/http3:encode-http3-frame
                      (http-kit/http3:make-http3-frame :type type :payload (octets 0 255 1)))))
      (http3-codec-response-frame-check
       (http3-test-concat-octets
        extension (http3-boundary-headers '((":status" . "200")))
        extension (http3-boundary-data "ok")
        extension (http3-boundary-headers '(("x-end" . "yes"))) extension)
       nil))))

(deftest http3-codec-response-frame-controls-are-rejected
  (dolist (type (list http-kit/http3:+http3-settings-type+
                     http-kit/http3:+http3-cancel-push-type+
                     http-kit/http3:+http3-goaway-type+
                     http-kit/http3:+http3-max-push-id-type+
                     http-kit/http3:+http3-priority-update-request-type+
                     http-kit/http3:+http3-priority-update-push-type+))
    (dolist (after-headers '(nil t))
      (let ((control (http-kit/http3:encode-http3-frame
                      (http-kit/http3:make-http3-frame :type type :payload (octets 0)))))
        (http3-codec-response-frame-check
         (if after-headers
             (http3-test-concat-octets (http3-boundary-headers '((":status" . "200"))) control)
             control)
         type)))))

(defun http3-codec-server-serializer-check
    (trailers-p replacement error-kind &optional reason)
  (let* ((fields '((":method" . "GET") (":scheme" . "https")
                  (":authority" . "example.test") (":path" . "/")
                  ("x-before" . "kept")
                  ("x-check" . "ok")))
         (wire (if trailers-p
                   (http3-test-concat-octets
                    (http3-boundary-headers fields)
                    (http3-boundary-headers '(("x-check" . "ok"))))
                   (http3-boundary-headers fields)))
         (before (copy-seq wire))
         (table (http-kit/http3:make-qpack-dynamic-table :max-capacity 0))
         (context (http-kit/http3:make-http3-qpack-decoder-context table))
         (stream (make-http3-test-stream :kind :request))
         (reads 0) (serializations 0) (calls 0) (writes nil)
         (closed nil) (errors nil) (response nil) (failure nil))
    (handler-case
        (setf response
              (http-kit/http3:serve-http3-request-stream
               stream
               (lambda (request)
                 (incf calls)
                 (ensure-equal "GET" (http-request-method request))
                 (ensure-octets-equal (octets) (http-request-body request))
                 (ensure-equal (if trailers-p '(("x-check" . "ok")) nil)
                               (mapcar (lambda (header)
                                         (cons (http-header-name header)
                                               (http-header-content header)))
                                       (http-request-trailers request)))
                 (make-http-response :status 204))
               :qpack-decoder-context context :stream-id 4
               :await-qpack (lambda (token)
                              (declare (ignore token))
                              (error "Static fields must not block."))
               :qpack-serialize
               (lambda (thunk)
                 (incf serializations)
                 (let ((decoded (funcall thunk)))
                   (ensure-equal '("x-check" . "ok") (car (last decoded)))
                   (when (and error-kind
                              (= serializations (if trailers-p 2 1)))
                     (setf (car (last decoded)) (copy-tree replacement)))
                   decoded))
               :read-stream
               (lambda (actual &key timeout deadline)
                 (declare (ignore timeout deadline))
                 (ensure-true (eq stream actual))
                 (incf reads)
                 (ensure-equal 1 reads)
                 (values wire t))
               :write-stream
               (lambda (actual bytes &key fin-p timeout deadline)
                 (declare (ignore timeout deadline))
                 (ensure-true (eq stream actual))
                 (push (list (copy-seq bytes) fin-p) writes))
               :close-stream
               (lambda (actual &key condition)
                 (ensure-true (eq stream actual))
                 (push condition closed))
               :on-error (lambda (condition request)
                           (push (list condition request) errors))))
      (http-protocol-error (condition) (setf failure condition)))
    (ensure-equal 1 reads)
    (ensure-equal (if trailers-p 2 1) serializations)
    (ensure-equal 0 (http-kit/http3:qpack-dynamic-table-insert-count table))
    (ensure-octets-equal before wire)
    (ensure-equal 1 (length closed))
    (ensure-true (eq failure (first closed)))
    (if error-kind
        (progn
          (ensure-equal 0 calls)
          (ensure-equal nil response)
          (ensure-equal nil writes)
          (ensure-equal 1 (length errors))
          (ensure-true (eq failure (first (first errors))))
          (ensure-equal nil (second (first errors)))
          (if (eq error-kind :header)
              (progn
                (ensure-true (typep failure 'http-invalid-header))
                (ensure-equal :http3-header (http-error-operation failure))
                (ensure-equal (car replacement) (http-invalid-header-name failure))
                (ensure-equal reason (http-invalid-header-reason failure)))
              (http3-public-error-check failure reason
                                        (if (eq error-kind :pair)
                                            replacement
                                            (car replacement)))))
        (progn
          (ensure-equal nil failure)
          (ensure-equal nil errors)
          (ensure-equal 1 calls)
          (ensure-equal 204 (http-response-status response))
          (ensure-equal '(t) (mapcar #'second writes))
          (let ((frames (http3-server-coverage-frames (nreverse writes))))
            (ensure-equal '(1) (mapcar #'http-kit/http3:http3-frame-type frames))
            (ensure-equal '((":status" . "204"))
                          (http-kit/http3:qpack-decode-field-section
                           (http-kit/http3:http3-frame-payload (first frames)))))))))

(deftest http3-codec-server-serializer-rejects-malformed-pairs
  (dolist (trailers-p '(nil t))
    (http3-codec-server-serializer-check trailers-p nil nil)
    (dolist (field '(nil (7 . "ok") ("x-check" . 7)))
      (http3-codec-server-serializer-check
       trailers-p field :pair
       (if trailers-p
           "HTTP/3 trailer fields must be name/value pairs."
           "HTTP/3 request fields must be name/value pairs.")))))

(deftest http3-codec-server-serializer-revalidates-fields
  (dolist (trailers-p '(nil t))
    (http3-codec-server-serializer-check trailers-p nil nil)
    (dolist (name '("" "X-check" "x check"))
      (http3-codec-server-serializer-check
       trailers-p (cons name "ok") :header
       "HTTP/3 field names must be lowercase ASCII tokens.")))
  (http3-codec-server-serializer-check
   nil (cons "x-check" (string #\Newline)) :header
   "field values cannot contain controls.")
  (http3-codec-server-serializer-check
   nil '(":method" . "POST") :order
   "HTTP/3 pseudo-fields must precede regular fields."))

(defun http3-codec-client-serializer-check (replacement error-kind)
  (let* ((fields '((":status" . "204") ("x-before" . "kept")
                   ("x-check" . "ok")))
         (wire (http3-boundary-headers fields))
         (before (copy-seq wire))
         (table (http-kit/http3:make-qpack-dynamic-table :max-capacity 0))
         (context (http-kit/http3:make-http3-qpack-decoder-context table))
         (request (make-http-request :method "GET" :uri "https://example.test/"))
         (source (http3-qpack-contract-request-source request))
         (request-stream nil) (reads 0) (decoded-count 0) (closes nil)
         (response nil) (failure nil)
         (client
           (http-kit/http3:make-http3-client
            :serialize
            (lambda (thunk)
              (let ((decoded (funcall thunk)))
                (when (equal decoded fields)
                  (incf decoded-count)
                  (when error-kind
                    (setf (car (last decoded)) (copy-tree replacement))))
                decoded))
            :open-stream
            (lambda (actual &key stream-type timeout deadline)
              (declare (ignore timeout deadline))
              (let ((stream (make-http3-test-stream :kind stream-type)))
                (when (eq stream-type :request)
                  (ensure-true (eq request actual))
                  (setf request-stream stream))
                (values stream 0)))
            :write-stream
            (lambda (stream bytes &key fin-p timeout deadline)
              (declare (ignore timeout deadline))
              (push (list (copy-seq bytes) fin-p)
                    (http3-test-stream-writes stream)))
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore timeout deadline))
              (ensure-true (eq request-stream stream))
              (incf reads)
              (ensure-equal 1 reads)
              (values wire t))
            :close-stream
            (lambda (stream &key condition)
              (setf (http3-test-stream-closed-p stream) t)
              (push (cons stream condition) closes)))))
    (unwind-protect
         (progn
           (handler-case
               (setf response
                     (http-kit/http3:send-http3-request
                      client request :qpack-decoder-context context
                      :await-qpack (lambda (token)
                                     (declare (ignore token))
                                     (error "Static fields must not block."))))
             (http-protocol-error (condition) (setf failure condition)))
           (ensure-equal 1 reads)
           (ensure-equal 1 decoded-count)
           (ensure-equal 0 (http-kit/http3:qpack-dynamic-table-insert-count table))
           (ensure-octets-equal before wire)
           (ensure-equal source (http3-qpack-contract-request-source request))
           (ensure-equal 1 (length closes))
           (ensure-true (eq request-stream (caar closes)))
           (ensure-true (eq failure (cdar closes)))
           (ensure-true (http3-test-stream-closed-p request-stream))
           (ensure-equal '(nil t)
                         (mapcar #'second
                                 (reverse (http3-test-stream-writes request-stream))))
           (ensure-true (http-kit/http3:http3-client-open-p client))
           (cond ((eq error-kind :order)
                  (ensure-equal nil response)
                  (http3-public-error-check
                   failure "HTTP/3 pseudo-fields must precede regular fields."
                   ":status"))
                 ((eq error-kind :empty)
                  (ensure-equal nil response)
                  (ensure-true (typep failure 'http-invalid-header))
                  (ensure-equal "" (http-invalid-header-name failure)))
                 (t
                  (ensure-equal nil failure)
                  (ensure-equal 204 (http-response-status response))
                  (ensure-octets-equal (octets) (http-response-body response))
                  (ensure-equal '(("x-before" . "kept") ("x-check" . "ok"))
                                (mapcar (lambda (header)
                                          (cons (http-header-name header)
                                                (http-header-content header)))
                                        (http-response-headers response))))))
      (http-kit/http3:close-http3-client client))))

(deftest http3-codec-client-serializer-revalidates-response-fields
  (http3-codec-client-serializer-check nil nil)
  (http3-codec-client-serializer-check '("" . "ok") :empty)
  (http3-codec-client-serializer-check '(":status" . "204") :order))

(deftest http3-codec-server-response-header-copies
  (dolist (stream-p '(nil t))
    (dolist (information-p '(nil t))
      (let* ((headers (list (make-http-header "x-copy" "kept")))
             (trailers (unless information-p
                         (list (make-http-header "x-trailer" "kept"))))
             (body-calls 0)
             (response
               (if stream-p
                   (make-http-response-stream
                    :status (if information-p 100 200)
                    :headers headers :trailers trailers
                    :body-function (lambda () (incf body-calls) nil))
                   (make-http-response
                    :status (if information-p 100 200)
                    :headers headers :trailers trailers)))
             (copies (if stream-p
                         (http-response-stream-headers response)
                         (http-response-headers response)))
             (trailer-copies (if stream-p
                                 (http-response-stream-trailers response)
                                 (http-response-trailers response)))
             (final (if information-p
                        (make-http-response :status 204)
                        response)))
        (ensure-true (not (eq headers copies)))
        (ensure-true (not (eq (first headers) (first copies))))
        (dolist (list (list headers copies trailers trailer-copies))
          (when list
            (setf (http-header-name (first list)) 7
                  (http-header-content (first list)) 7
                  (first list) 7)))
        (multiple-value-bind (failure actual writes)
            (http3-server-coverage-serve
             (lambda (request)
               (declare (ignore request))
               (values final (when information-p (list response)))))
          (ensure-equal nil failure)
          (ensure-true (eq final actual))
          (ensure-equal (if stream-p 1 0) body-calls)
          (ensure-equal '(nil t) (mapcar #'second writes))
          (let ((frames (http3-server-coverage-frames writes)))
            (ensure-equal '(1 1) (mapcar #'http-kit/http3:http3-frame-type frames))
            (ensure-equal
             (list (list (cons ":status" (if information-p "100" "200"))
                         '("x-copy" . "kept"))
                   (if information-p
                       '((":status" . "204"))
                       '(("x-trailer" . "kept"))))
             (mapcar (lambda (frame)
                       (http-kit/http3:qpack-decode-field-section
                        (http-kit/http3:http3-frame-payload frame)))
                     frames))))))))

(deftest http3-codec-server-response-revalidates-aliased-header-names
  (dolist (stream-p '(nil t))
    (dolist (information-p '(nil t))
      (let* ((name (copy-seq "x-copy"))
             (header (make-http-header name "kept"))
             (response
               (if stream-p
                   (make-http-response-stream
                    :status (if information-p 100 200) :headers (list header)
                    :body-function (lambda () nil))
                   (make-http-response
                    :status (if information-p 100 200) :headers (list header))))
             (final (if information-p
                        (make-http-response :status 204)
                        response)))
        (setf (char name 0) #\Space)
        (multiple-value-bind (failure actual writes)
            (http3-server-coverage-serve
             (lambda (request)
               (declare (ignore request))
               (values final (when information-p (list response)))))
          (ensure-true (typep failure 'http-invalid-header))
          (ensure-equal name (http-invalid-header-name failure))
          (ensure-equal :name (http-invalid-header-reason failure))
          (ensure-equal nil actual)
          (ensure-equal nil writes))))))

(deftest http3-codec-qpack-required-count-prefix-boundaries
  (dolist (insert-count '(nil -1 1/2 "1"))
    (let ((failure
            (handler-case
                (http-kit/http3::%qpack-encode-required-insert-count insert-count 128)
              (http-protocol-error (condition) condition))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :qpack (http-error-operation failure))
      (ensure-equal insert-count (http-protocol-error-detail failure))))
  (dolist (capacity '(0 1 31))
    (ensure-equal 0
                  (http-kit/http3::%qpack-encode-required-insert-count 0 capacity))
    (let ((failure
            (handler-case
                (http-kit/http3::%qpack-encode-required-insert-count 1 capacity)
              (http-protocol-error (condition) condition))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :qpack (http-error-operation failure))
      (ensure-equal capacity (http-protocol-error-detail failure))))
  (dolist (case '((1 128 2) (7 128 8) (8 128 1) (9 128 2)
                 (1 32 2) (2 32 1)))
    (destructuring-bind (count capacity expected) case
      (ensure-equal expected
                    (http-kit/http3::%qpack-encode-required-insert-count
                     count capacity)))))

(deftest http3-codec-qpack-base-prefix-sign-and-integer-boundaries
  (dolist (case '((4 4 (0)) (4 6 (2)) (4 3 (128)) (4 0 (131))
                 (4 133 (127 2)) (130 0 (255 2))))
    (destructuring-bind (required base expected) case
      (ensure-equal expected
                    (coerce (http-kit/http3::%qpack-encode-base required base)
                            'list))))
  (let ((table (http-kit/http3:make-qpack-dynamic-table :max-capacity 128)))
    (http-kit/http3:qpack-dynamic-table-insert table "x-a" "one")
    (dolist (case '((0 16) (1 128)))
      (destructuring-bind (base reference) case
        (let* ((prefix (http-kit/http3::%qpack-encode-base 1 base))
               (wire (make-array (+ 2 (length prefix))
                                 :element-type '(unsigned-byte 8))))
          (setf (aref wire 0)
                (http-kit/http3::%qpack-encode-required-insert-count 1 128))
          (replace wire prefix :start1 1)
          (setf (aref wire (1- (length wire))) reference)
          (ensure-equal '(("x-a" . "one"))
                        (http-kit/http3:qpack-decode-field-section
                         wire :dynamic-table table)))))))

(deftest http3-codec-qpack-empty-table-eviction-boundaries
  (let ((table (http-kit/http3:make-qpack-dynamic-table :max-capacity 128)))
    (http-kit/http3::%qpack-ensure-evictable-size table 0)
    (dolist (operation
             (list (lambda () (http-kit/http3::%qpack-evict-oldest table))
                   (lambda () (http-kit/http3::%qpack-ensure-evictable-size table -1))))
      (let ((failure (handler-case (funcall operation)
                       (http-protocol-error (condition) condition))))
        (ensure-true (typep failure 'http-protocol-error))
        (ensure-equal :qpack (http-error-operation failure))
        (ensure-equal "QPACK dynamic-table eviction state is invalid."
                      (http-error-message failure))
        (ensure-equal nil (http-kit/http3:qpack-dynamic-table-entries table))
        (ensure-equal 0 (http-kit/http3:qpack-dynamic-table-size table))
        (ensure-equal 0 (http-kit/http3:qpack-dynamic-table-insert-count table))
        (ensure-equal 128 (http-kit/http3:qpack-dynamic-table-capacity table))))))

(deftest http3-codec-qpack-direct-eviction-preserves-protected-entry
  (let* ((table (http-kit/http3:make-qpack-dynamic-table :max-capacity 34))
         (entry (http-kit/http3:qpack-dynamic-table-insert table "a" "1"))
         (state (http-kit/http3:make-qpack-decoder-stream-state table)))
    (http-kit/http3:qpack-decoder-stream-state-note-insertions-sent state 1)
    (http-kit/http3:qpack-decoder-stream-state-register-section state 4 1 '(0))
    (http3-codec-check-protected-eviction
     (lambda () (http-kit/http3::%qpack-evict-oldest table)) 0)
    (ensure-true (eq entry (first (http-kit/http3:qpack-dynamic-table-entries table))))
    (ensure-equal 34 (http-kit/http3:qpack-dynamic-table-size table))
    (ensure-equal '((1 0))
                  (gethash 4 (http-kit/http3:qpack-decoder-stream-state-outstanding-sections state)))
    (let ((wire (http-kit/http3:qpack-encode-section-acknowledgment 4)))
      (multiple-value-bind (events consumed)
          (http-kit/http3:qpack-process-decoder-stream wire :state state)
        (ensure-equal '((:section-acknowledgment 4)) events)
        (ensure-equal (length wire) consumed)))
    (http-kit/http3::%qpack-evict-oldest table)
    (ensure-equal nil (http-kit/http3:qpack-dynamic-table-entries table))
    (ensure-equal 0 (http-kit/http3:qpack-dynamic-table-size table))
    (ensure-equal 1 (http-kit/http3:qpack-dynamic-table-insert-count table))
    (ensure-equal 34 (http-kit/http3:qpack-dynamic-table-capacity table))))

(deftest http3-codec-qpack-rejects-unavailable-dynamic-entries
  (let ((table (http-kit/http3:make-qpack-dynamic-table :max-capacity 34)))
    (http-kit/http3:qpack-dynamic-table-insert table "a" "1")
    (ensure-equal '(("a" . "1"))
                  (http-kit/http3:qpack-decode-field-section
                   (octets #x02 #x00 #x80) :dynamic-table table))
    (let ((failure
            (handler-case
                (http-kit/http3:qpack-decode-field-section
                 (octets #x02 #x00 #x10) :dynamic-table table)
              (http-protocol-error (condition) condition))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :qpack (http-error-operation failure))
      (ensure-equal "QPACK dynamic-table entry is not yet inserted."
                    (http-error-message failure))
      (ensure-equal 1 (http-protocol-error-detail failure)))
    (http-kit/http3:qpack-dynamic-table-insert table "b" "2")
    (ensure-equal '(("b" . "2"))
                  (http-kit/http3:qpack-decode-field-section
                   (octets #x01 #x00 #x80) :dynamic-table table))
    (let ((failure
            (handler-case
                (http-kit/http3:qpack-decode-field-section
                 (octets #x01 #x00 #x81) :dynamic-table table)
              (http-protocol-error (condition) condition))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :qpack (http-error-operation failure))
      (ensure-equal "QPACK dynamic-table absolute index is unavailable."
                    (http-error-message failure))
      (ensure-equal 0 (http-protocol-error-detail failure)))))
