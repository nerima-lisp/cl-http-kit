(in-package #:http-kit/test)

(deftest hpack-public-api-rejects-invalid-table-and-index-inputs
  (let ((context
          (http-message-kit:make-hpack-context
           :max-table-size 4096 :huffman nil)))
    (signals http-message-kit:http-protocol-error
      (http-message-kit:hpack-context-set-max-table-size context -1))
    (signals http-message-kit:http-protocol-error
      (http-message-kit:hpack-context-set-max-table-size context "invalid"))
    (ensure-equal 4096
                  (http-message-kit:hpack-context-max-table-size context))
    (signals http-message-kit:http-protocol-error
      (http-message-kit:hpack-decode-header-block
       (octets #x80) :context context))
    (signals http-message-kit:http-protocol-error
      (http-message-kit:hpack-decode-header-block
       (http-message-kit:hpack-encode-integer 62 7 #x80)
       :context context))
    (signals http-message-kit:http-protocol-error
      (http-message-kit:hpack-decode-header-block
       (http-message-kit:hpack-encode-integer 4097 5 #x20)
       :context context))))

(deftest hpack-public-api-decodes-empty-huffman-string
  (multiple-value-bind (value next)
      (http-message-kit:hpack-decode-string (octets #x80))
    (ensure-equal "" value)
    (ensure-equal 1 next)))

(deftest hpack-public-api-preserves-zero-table-size-and-empty-block
  (let ((context
          (http-message-kit:make-hpack-context
           :max-table-size 64 :huffman nil)))
    (http-message-kit:hpack-encode-header-block
     (list (http-message-kit:make-http-header "x-before-zero" "value"))
     :context context :indexing :incremental)
    (ensure-equal 1
                  (http-message-kit:hpack-context-dynamic-entry-count
                   context))
    (http-message-kit:hpack-context-set-max-table-size context 0)
    (ensure-equal 0 (http-message-kit:hpack-context-table-size context))
    (ensure-equal 0
                  (http-message-kit:hpack-context-dynamic-entry-count
                   context))
    (http-message-kit:hpack-encode-header-block
     (list (http-message-kit:make-http-header "x-after-zero" "value"))
     :context context :indexing :incremental)
    (ensure-equal 0
                  (http-message-kit:hpack-context-dynamic-entry-count
                   context)))
  (let ((context
          (http-message-kit:make-hpack-context
           :max-table-size 0 :huffman nil)))
    (ensure-equal (octets)
                  (http-message-kit:hpack-encode-header-block
                   (list) :context context))
    (ensure-equal 0 (http-message-kit:hpack-context-table-size context))
    (multiple-value-bind (headers used)
        (http-message-kit:hpack-decode-header-block
         (octets) :context context)
      (ensure-equal nil headers)
      (ensure-equal 0 used))))

(deftest hpack-public-api-reuses-decoder-after-update-order-error
  (let* ((headers
           (list
            (http-message-kit:make-http-header
             ":method" "GET" :pseudo-p t)))
         (header-block
           (http-message-kit:hpack-encode-header-block
            headers :static-only-p t))
         (table-size-update
           (http-message-kit:hpack-encode-integer 0 5 #x20))
         (late-update
           (concatenate-octets header-block table-size-update))
         (valid-block
           (concatenate-octets table-size-update header-block))
         (context
           (http-message-kit:make-hpack-context :huffman nil)))
    (signals http-message-kit:http-protocol-error
      (http-message-kit:hpack-decode-header-block
       late-update :context context))
    (multiple-value-bind (decoded used)
        (http-message-kit:hpack-decode-header-block
         valid-block :context context)
      (ensure-equal 1 (length decoded))
      (ensure-equal ":method"
                    (http-message-kit:http-header-name (first decoded)))
      (ensure-equal "GET"
                    (http-message-kit:http-header-content (first decoded)))
      (ensure-equal (length valid-block) used))))

(deftest hpack-public-api-rejects-nil-header-name-and-value
  (let ((invalid-name
          (http-message-kit:make-http-header "x-header" "value"))
        (invalid-value
          (http-message-kit:make-http-header "x-header" "value")))
    (setf (http-message-kit:http-header-name invalid-name) nil
          (http-message-kit:http-header-content invalid-value) nil)
    (signals http-message-kit:http-invalid-header
      (http-message-kit:hpack-encode-header-block
       (list invalid-name)))
    (signals http-message-kit:http-invalid-header
      (http-message-kit:hpack-encode-header-block
       (list invalid-value)))))
(deftest hpack-public-api-rejects-non-string-header-name
  (let ((condition
          (handler-case
              (http-message-kit:make-http-header 42 "value")
            (http-message-kit:http-invalid-header (condition) condition))))
    (ensure-true
     (typep condition 'http-message-kit:http-invalid-header))
    (ensure-equal :header
                  (http-message-kit:http-error-operation condition))
    (ensure-equal 42
                  (http-message-kit:http-invalid-header-name condition))
    (ensure-equal :name
                  (http-message-kit:http-invalid-header-reason condition))))

(deftest hpack-public-api-validates-max-fields-before-table-mutation
  (let* ((wire
           (http-message-kit:hpack-encode-header-block
            (list (http-message-kit:make-http-header "x-limit" "value"))
            :indexing :incremental))
         (decoder (http-message-kit:make-hpack-context :huffman nil)))
    (dolist (max-fields (list 0 -1 1/2 "invalid" :invalid))
      (signals http-message-kit:http-protocol-error
        (http-message-kit:hpack-decode-header-block
         wire :context decoder :max-fields max-fields))
      (ensure-equal 0
                    (http-message-kit:hpack-context-table-size decoder))
      (ensure-equal 0
                    (http-message-kit:hpack-context-dynamic-entry-count
                     decoder)))
    (let ((decoded
            (http-message-kit:hpack-decode-header-block
             wire :context decoder)))
      (ensure-equal 1 (length decoded))
      (ensure-equal "value"
                    (http-message-kit:http-header-content (first decoded)))
      (ensure-equal 1
                    (http-message-kit:hpack-context-dynamic-entry-count
                     decoder)))))
(deftest hpack-integer-codec-boundaries
  (labels ((capture (thunk)
             (handler-case
                 (funcall thunk)
               (hpack-codec:hpack-codec-error (condition) condition)))
           (assert-error (condition message detail)
             (ensure-true (typep condition 'hpack-codec:hpack-codec-error))
             (ensure-equal :hpack
                           (hpack-codec:hpack-codec-error-operation condition))
             (ensure-equal message
                           (hpack-codec:hpack-codec-error-message condition))
             (ensure-equal detail
                           (hpack-codec:hpack-codec-error-detail condition))))
    (dolist (case
             (list
              (list (octets) 0 5
                    "An HPACK integer is truncated or has an invalid prefix." 0)
              (list (octets #x00) 0 0
                    "An HPACK integer is truncated or has an invalid prefix." 0)
              (list (octets #x1f) 0 5
                    "An HPACK integer continuation is truncated." 1)
              (list (octets #x1f #xff #xff #xff #xff #x10) 0 5
                    "An HPACK integer is too large." #x1000001e)
              (list (octets #x1f #xff #xff #xff #xff #x0f) 0 5
                    "An HPACK integer is too large." #x1000001e)))
      (destructuring-bind (wire position prefix-bits message detail) case
        (assert-error
         (capture
          (lambda ()
            (hpack-codec:hpack-read-integer wire position prefix-bits)))
         message detail)))
    (dolist (arguments
             (list (list nil 5 0)
                   (list -1 5 0)
                   (list 1 0 0)
                   (list 1 9 0)
                   (list 1 5 :invalid)
                   (list 1 5 -1)
                   (list 1 5 256)))
      (assert-error
       (capture
        (lambda ()
          (apply #'hpack-codec:hpack-encode-integer arguments)))
       "Cannot encode an invalid HPACK integer."
       (first arguments)))))
(deftest hpack-huffman-codec-rejects-eos-and-invalid-padding
  (labels ((decode-error (wire)
             (handler-case
                 (hpack-codec:hpack-huffman-decode wire)
               (hpack-codec:hpack-codec-error (condition) condition)))
           (assert-error (condition message detail)
             (ensure-true (typep condition 'hpack-codec:hpack-codec-error))
             (ensure-equal :hpack
                           (hpack-codec:hpack-codec-error-operation condition))
             (ensure-equal message
                           (hpack-codec:hpack-codec-error-message condition))
             (ensure-equal detail
                           (hpack-codec:hpack-codec-error-detail condition))))
    (dolist (case
             (list
              (list (octets #xff #xff #xff #xfc)
                    "An HPACK Huffman string contains EOS."
                    :huffman-eos)
              (list (octets #xff)
                    "An HPACK Huffman string has excessive padding."
                    8)
              (list (octets #x1e)
                    "An HPACK Huffman string has invalid padding."
                    6)))
      (destructuring-bind (wire message detail) case
        (assert-error (decode-error wire) message detail)))))
