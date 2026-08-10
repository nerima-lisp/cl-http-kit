(in-package #:http-kit/http2)

(defstruct (%hpack-huffman-node
            (:constructor %make-hpack-huffman-node (&optional zero one symbol)))
  zero
  one
  symbol)

(defun %hpack-build-huffman-tree
    (&optional (codes +hpack-huffman-codes+)
               (lengths +hpack-huffman-lengths+))
  (let ((root (%make-hpack-huffman-node)))
    (loop for symbol below (length codes)
          for code = (aref codes symbol)
          for bits = (aref lengths symbol)
          do (let ((node root))
               (loop for bit-position downfrom (1- bits) to 0
                     do (when (%hpack-huffman-node-symbol node)
                          (error "HPACK Huffman table contains a prefix collision."))
                        (let* ((bit (if (logbitp bit-position code) 1 0))
                               (next (if (zerop bit)
                                         (%hpack-huffman-node-zero node)
                                         (%hpack-huffman-node-one node))))
                          (unless next
                            (setf next (%make-hpack-huffman-node))
                            (if (zerop bit)
                                (setf (%hpack-huffman-node-zero node) next)
                                (setf (%hpack-huffman-node-one node) next)))
                          (setf node next)))
               (when (%hpack-huffman-node-symbol node)
                 (error "HPACK Huffman table contains a duplicate code."))
               (setf (%hpack-huffman-node-symbol node) symbol)))
    root))

(defun %hpack-huffman-decode (octets)
  (let ((node *hpack-huffman-tree*)
        (result (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0))
        (padding-bits 0)
        (padding-value 0))
    (loop for octet across octets
          do (loop for bit-position downfrom 7 to 0
                   for bit = (if (logbitp bit-position octet) 1 0)
                   do (let ((next (if (zerop bit)
                                      (%hpack-huffman-node-zero node)
                                      (%hpack-huffman-node-one node))))
                        (unless next
                          (error 'http-protocol-error
                                 :message "An HPACK Huffman string has an invalid code."
                                 :operation :hpack
                                 :detail :huffman-code))
                        (setf node next)
                        (incf padding-bits)
                        (setf padding-value
                              (logior (ash padding-value 1) bit))
                        (let ((symbol (%hpack-huffman-node-symbol node)))
                          (when symbol
                            (when (= symbol 256)
                              (error 'http-protocol-error
                                     :message "An HPACK Huffman string contains EOS."
                                     :operation :hpack
                                     :detail :huffman-eos))
                            (vector-push-extend symbol result)
                            (setf node *hpack-huffman-tree*
                                  padding-bits 0
                                  padding-value 0))))))
    (when (> padding-bits 7)
      (error 'http-protocol-error
             :message "An HPACK Huffman string has excessive padding."
             :operation :hpack
             :detail padding-bits))
    (when (and (plusp padding-bits)
               (/= padding-value (1- (ash 1 padding-bits))))
      (error 'http-protocol-error
             :message "An HPACK Huffman string has invalid padding."
             :operation :hpack
             :detail padding-value))
    (let ((copy (make-array (length result)
                            :element-type '(unsigned-byte 8))))
      (replace copy result)
      copy)))

(defun %hpack-huffman-encode (octets)
  "Encode OCTETS with the HPACK Huffman code table.

The accumulator is kept MSB-first and the final partial byte is padded with
the prefix of the EOS code, as required by RFC 7541."
  (let ((result (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0))
        (accumulator 0)
        (bits 0))
    (loop for octet across octets
          for code = (aref +hpack-huffman-codes+ octet)
          for code-bits = (aref +hpack-huffman-lengths+ octet)
          do (setf accumulator
                   (logior (ash accumulator code-bits) code)
                   bits (+ bits code-bits))
             (loop while (>= bits 8)
                   do (let ((shift (- bits 8)))
                        (vector-push-extend
                         (logand (ash accumulator (- shift)) #xff)
                         result)
                        (setf accumulator
                              (if (plusp shift)
                                  (logand accumulator
                                          (1- (ash 1 shift)))
                                  0)
                              bits shift))))
    (when (plusp bits)
      (vector-push-extend
       (logior (ash accumulator (- 8 bits))
               (1- (ash 1 (- 8 bits))))
       result))
    (let ((copy (make-array (length result)
                           :element-type '(unsigned-byte 8))))
      (replace copy result)
      copy)))
