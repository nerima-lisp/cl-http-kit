(in-package #:http-kit/test)

(deftest http3-qpack-static-field-sections
  (let* ((fields (list (cons ":method" "GET")
                       (cons ":scheme" "https")
                       (cons ":authority" "example.test")
                       (cons ":path" "/resource")
                       (cons "accept" "text/plain")))
         (encoded (http-kit/http3:qpack-encode-field-section fields))
         (decoded (http-kit/http3:qpack-decode-field-section encoded)))
    (ensure-equal '(unsigned-byte 8)
                  (array-element-type encoded)
                  "QPACK sections are binary octet vectors")
    (ensure-equal fields decoded))
  (signals http-protocol-error
    (http-kit/http3:qpack-decode-field-section
     (octets #x01 #x00)))
  (signals http-size-limit-exceeded
    (http-kit/http3:qpack-decode-field-section
     (http-kit/http3:qpack-encode-field-section
      (list (cons "x-test" "value")))
     :max-header-bytes 1)))

(deftest http3-qpack-dynamic-table-and-streams
  (let* ((encoder-table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256
            :capacity 0))
         (decoder-table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256
            :capacity 0))
         (encoder-stream
           (http3-test-concat-octets
            (http-kit/http3:qpack-encode-set-dynamic-table-capacity 256)
            (http-kit/http3:qpack-encode-insert-with-literal-name
             "x-request-id" "one")
            (http-kit/http3:qpack-encode-insert-with-name-reference
             0 "two" :static-p nil)
            (http-kit/http3:qpack-encode-duplicate 0)
            (http-kit/http3:qpack-encode-insert-with-name-reference
             17 "PATCH")))
         (fields (list (cons ":method" "PATCH")
                       (cons "x-request-id" "two"))))
    (multiple-value-bind (events consumed)
        (http-kit/http3:qpack-process-encoder-stream
         encoder-table encoder-stream)
      (ensure-equal 5 (length events))
      (ensure-equal (length encoder-stream) consumed))
    (multiple-value-bind (events consumed)
        (http-kit/http3:qpack-process-encoder-stream
         decoder-table encoder-stream)
      (ensure-equal 5 (length events))
      (ensure-equal (length encoder-stream) consumed))
    (ensure-equal 4
                  (http-kit/http3:qpack-dynamic-table-insert-count
                   encoder-table))
    (ensure-equal 4
                  (length (http-kit/http3:qpack-dynamic-table-entries
                           encoder-table)))
    (let ((encoded
            (http-kit/http3:qpack-encode-field-section
             fields :dynamic-table encoder-table)))
      (ensure-equal fields
                    (http-kit/http3:qpack-decode-field-section
                     encoded :dynamic-table decoder-table))
      (signals http-protocol-error
        (http-kit/http3:qpack-decode-field-section
         encoded
         :dynamic-table
         (http-kit/http3:make-qpack-dynamic-table
          :max-capacity 256
          :capacity 256)))))
  (let ((table
          (http-kit/http3:make-qpack-dynamic-table
           :max-capacity 64)))
    (http-kit/http3:qpack-dynamic-table-insert table "a" "b")
    (http-kit/http3:qpack-dynamic-table-insert table "c" "d")
    (ensure-equal 1
                  (length (http-kit/http3:qpack-dynamic-table-entries table)))
    (ensure-equal "c"
                  (http-kit/http3:qpack-dynamic-entry-name
                   (first (http-kit/http3:qpack-dynamic-table-entries table))))
    (signals http-protocol-error
      (http-kit/http3:qpack-dynamic-table-insert table
                                                 "this-name-is-definitely-too-large-for-the-table"
                                                 "value")))
  (let ((acknowledged nil)
        (cancelled nil)
        (increments nil))
    (multiple-value-bind (events consumed)
        (http-kit/http3:qpack-process-decoder-stream
         (http3-test-concat-octets
          (http-kit/http3:qpack-encode-section-acknowledgment 7)
          (http-kit/http3:qpack-encode-stream-cancellation 9)
          (http-kit/http3:qpack-encode-insert-count-increment 3))
         :on-section-acknowledgment
         (lambda (stream-id) (push stream-id acknowledged))
         :on-stream-cancellation
         (lambda (stream-id) (push stream-id cancelled))
         :on-insert-count-increment
         (lambda (increment) (push increment increments)))
      (ensure-equal '((:section-acknowledgment 7)
                      (:stream-cancellation 9)
                      (:insert-count-increment 3))
                    events)
      (ensure-equal 3 consumed))
    (ensure-equal '(7) acknowledged)
    (ensure-equal '(9) cancelled)
    (ensure-equal '(3) increments)))

(deftest http3-qpack-huffman-encoding
  (let* ((input (octets 119 119 119 46 101 120 97 109 112 108 101 46 99 111 109))
         (encoded (http-kit/http2::%hpack-huffman-encode input)))
    (ensure-equal (octets #xf1 #xe3 #xc2 #xe5 #xf2 #x3a
                          #x6b #xa0 #xab #x90 #xf4 #xff)
                  encoded)
    (ensure-equal input
                  (http-kit/http2::%hpack-huffman-decode encoded)))
  (let* ((table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256
            :capacity 256))
         (encoder-stream
           (http-kit/http3:qpack-encode-insert-with-literal-name
            "x-huffman" "www.example.com" :huffman-p t)))
    (multiple-value-bind (events consumed)
        (http-kit/http3:qpack-process-encoder-stream table encoder-stream)
      (ensure-equal 1 (length events))
      (ensure-equal (length encoder-stream) consumed))
    (let ((entry (first (http-kit/http3:qpack-dynamic-table-entries table))))
      (ensure-equal "x-huffman"
                    (http-kit/http3:qpack-dynamic-entry-name entry))
      (ensure-equal "www.example.com"
                    (http-kit/http3:qpack-dynamic-entry-value entry))))
  (let* ((fields (list (cons "x-huffman" "www.example.com")
                       (cons "accept" "text/plain")))
         (encoded (http-kit/http3:qpack-encode-field-section
                   fields :huffman-p t)))
    (ensure-equal fields
                  (http-kit/http3:qpack-decode-field-section encoded))))
