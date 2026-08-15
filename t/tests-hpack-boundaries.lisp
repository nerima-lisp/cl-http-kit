(in-package #:http-kit/test)

(deftest hpack-codec-boundaries
  (ensure-true
   (http-kit/http2::%hpack-build-huffman-tree
    (vector 0 1) (vector 1 1)))
  (signals error
    (http-kit/http2::%hpack-build-huffman-tree
     (vector 0 0) (vector 1 2)))
  (signals error
    (http-kit/http2::%hpack-build-huffman-tree
     (vector 0 0) (vector 1 1)))
  (let ((context (http-kit/http2::%make-hpack-context)))
    (ensure-equal 2
                  (http-kit/http2::%hpack-static-index ":method" "GET"))
    (ensure-equal 2
                  (http-kit/http2::%hpack-static-index ":method"))
    (ensure-equal nil
                  (http-kit/http2::%hpack-static-index "x-missing"))
    (ensure-equal nil
                  (http-kit/http2::%hpack-indexed-name context 0))
    (signals http-protocol-error
      (http-kit/http2::%hpack-indexed-field context 0))
    (signals http-protocol-error
      (http-kit/http2::%hpack-indexed-field context :invalid))
    (signals http-protocol-error
      (http-kit/http2::%hpack-indexed-field
       context (1+ (length http-kit/http2::*hpack-static-table*))))
    (signals http-protocol-error
      (http-kit/http2::%hpack-set-maximum-size context "invalid"))
    (signals http-protocol-error
      (http-kit/http2::%hpack-set-max-size context "invalid"))
    (signals http-protocol-error
      (http-kit/http2::%hpack-set-maximum-size context -1))
    (signals http-protocol-error
      (http-kit/http2::%hpack-set-max-size context -1))
    (signals http-protocol-error
      (http-kit/http2::%hpack-set-max-size context
       (1+ (http-kit/http2::%hpack-context-maximum-size context))))
    (signals http-protocol-error
      (http-kit/http2::%hpack-read-integer (octets) 0 7))
    (signals http-protocol-error
      (http-kit/http2::%hpack-read-integer (octets 0) 0 0))
    (signals http-protocol-error
      (http-kit/http2::%hpack-read-integer (octets #xff) 0 8))
    (signals http-protocol-error
      (http-kit/http2::%hpack-read-integer
       (octets #xff #xff #xff #xff #xff #xff) 0 8))
    (signals http-protocol-error
      (http-kit/http2::%hpack-read-integer
       (octets #xff #xff #xff #xff #xff #x0f) 0 8))
    (signals http-protocol-error
      (http-kit/http2::%hpack-read-integer
       (octets #xff #x80 #x80 #x80 #x80 #x80) 0 8))
    (signals http-protocol-error
      (http-kit/http2::%hpack-encode-integer -1 7 0))
    (signals http-protocol-error
      (http-kit/http2::%hpack-encode-integer 0 0 0))
    (signals http-protocol-error
      (http-kit/http2::%hpack-decode-string (octets) 0))
    (signals http-protocol-error
      (http-kit/http2::%hpack-decode-string (octets 1) 0))
    (signals http-protocol-error
      (http-kit/http2::%hpack-huffman-decode (octets #xff)))
    (signals http-protocol-error
      (http-kit/http2::%hpack-huffman-decode (octets #xff #xfc)))
    (signals http-protocol-error
      (http-kit/http2::%hpack-huffman-decode
       (octets #xff #xff #xff #xfc)))
    (signals http-protocol-error
      (http-kit/http2::%hpack-huffman-decode
       (octets #xff #xff #xea)))
    (ensure-equal (octets)
                  (http-kit/http2::%hpack-huffman-decode (octets)))
    (signals http-invalid-header
      (http-kit/http2::%hpack-validate-field "X-Header" "value"))
    (signals http-invalid-header
      (http-kit/http2::%hpack-validate-field "x-header" nil))
    (signals http-invalid-header
      (http-kit/http2::%hpack-validate-field nil "value"))
    (signals http-invalid-header
      (http-kit/http2::%hpack-validate-field ":" "value"))
    (signals http-invalid-header
      (http-kit/http2::%hpack-validate-field "" "value"))
    (ensure-equal 8192
                  (progn
                    (http-kit/http2::%hpack-set-maximum-size context 8192)
                    (http-kit/http2::%hpack-context-maximum-size context)))
    (http-kit/http2::%hpack-add context "x-name" "x-value")
    (http-kit/http2::%hpack-set-maximum-size context 0)
    (ensure-equal 0 (http-kit/http2::%hpack-context-size context))
    (ensure-equal '() (http-kit/http2::%hpack-context-entries context))
    (http-kit/http2::%hpack-set-max-size context 0)
    (http-kit/http2::%hpack-add context "large-name" "large-value")
    (ensure-equal 0 (http-kit/http2::%hpack-context-size context))
    (ensure-equal '() (http-kit/http2::%hpack-context-entries context))
    (ensure-equal (octets)
                  (http-kit/http2::%hpack-encode-block '())))
  (let ((empty-context (http-kit/http2::%make-hpack-context)))
    (setf (http-kit/http2::%hpack-context-size empty-context) 1
          (http-kit/http2::%hpack-context-max-size empty-context) 0)
    (http-kit/http2::%hpack-evict empty-context)
    (ensure-equal 1 (http-kit/http2::%hpack-context-size empty-context)))
  (let ((context (http-kit/http2::%make-hpack-context)))
    (signals http-protocol-error
      (http-kit/http2::%hpack-decode-block
       (concatenate-octets
        (h2-header-block (cons ":method" "GET"))
        (http-kit/http2::%hpack-encode-integer 0 5 #x20))
       context))
    (ensure-equal '("GET")
                  (list
                   (cdr
                    (first
                   (http-kit/http2::%hpack-decode-block
                      (concatenate-octets
                       (http-kit/http2::%hpack-encode-integer 0 5 #x20)
                       (h2-header-block (cons ":method" "GET")))
                      context)))))))

(deftest hpack-name-p-boundaries
  (ensure-true (http-kit/http2::%hpack-name-p "x-header"))
  (ensure-true (not (http-kit/http2::%hpack-name-p 42))))

(deftest hpack-field-count-limit
  (let ((block
          (http-kit/http2::%hpack-encode-block
           (list (cons ":status" "200")
                 (cons "content-type" "text/plain")))))
    (ensure-equal
     2
     (length
      (http-kit/http2::%hpack-decode-block
       block (http-kit/http2::%make-hpack-context) :max-fields 2)))
    (signals http-size-limit-exceeded
      (http-kit/http2::%hpack-decode-block
       block (http-kit/http2::%make-hpack-context) :max-fields 1))
    (signals http-protocol-error
      (http-kit/http2::%hpack-decode-block
       block (http-kit/http2::%make-hpack-context) :max-fields 0))))

(deftest hpack-huffman-round-trips-every-octet
  ;; Symbol 9 (TAB) was carried as #xfffea, four bits short of its declared
  ;; 24-bit width, so encoding a tab produced a stream no decoder could read
  ;; back. Coverage above exercises decode error paths and tree construction
  ;; but never encodes a symbol and reads it back, which is how one missing
  ;; hex digit survived: tab is the only code in that region shorter than its
  ;; neighbours', so it reads as a plausible entry.
  (let ((tab (octets 9)))
    (ensure-equal tab
                  (http-kit/http2::%hpack-huffman-decode
                   (http-kit/http2::%hpack-huffman-encode tab))))
  (let ((every-octet
          (map '(vector (unsigned-byte 8)) #'identity
               (loop for code below 256 collect code))))
    (ensure-equal every-octet
                  (http-kit/http2::%hpack-huffman-decode
                   (http-kit/http2::%hpack-huffman-encode every-octet))))
  ;; Every code must fit inside the width the length table declares for it; a
  ;; value narrower than its declared width encodes leading zeros that shift
  ;; the whole stream.
  (ensure-true
   (loop for symbol below (length http-kit/http2::+hpack-huffman-codes+)
         always (< (aref http-kit/http2::+hpack-huffman-codes+ symbol)
                   (ash 1 (aref http-kit/http2::+hpack-huffman-lengths+ symbol))))))
