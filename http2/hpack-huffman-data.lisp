(in-package #:http-kit/http2)

(defparameter *hpack-huffman-tree* (%hpack-build-huffman-tree))
