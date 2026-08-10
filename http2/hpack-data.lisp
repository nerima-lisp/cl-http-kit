(in-package #:http-kit/http2)

;; RFC 7541, Appendix A.  The table is kept as an immutable vector so that
;; the decoder can use the same one-based indexes as the wire format.
(defparameter *hpack-static-table*
  (vector
   '(":authority" . "")
   '(":method" . "GET")
   '(":method" . "POST")
   '(":path" . "/")
   '(":path" . "/index.html")
   '(":scheme" . "http")
   '(":scheme" . "https")
   '(":status" . "200")
   '(":status" . "204")
   '(":status" . "206")
   '(":status" . "304")
   '(":status" . "400")
   '(":status" . "404")
   '(":status" . "500")
   '("accept-charset" . "")
   '("accept-encoding" . "gzip, deflate")
   '("accept-language" . "")
   '("accept-ranges" . "")
   '("accept" . "")
   '("access-control-allow-origin" . "")
   '("age" . "")
   '("allow" . "")
   '("authorization" . "")
   '("cache-control" . "")
   '("content-disposition" . "")
   '("content-encoding" . "")
   '("content-language" . "")
   '("content-length" . "")
   '("content-location" . "")
   '("content-range" . "")
   '("content-type" . "")
   '("cookie" . "")
   '("date" . "")
   '("etag" . "")
   '("expect" . "")
   '("expires" . "")
   '("from" . "")
   '("host" . "")
   '("if-match" . "")
   '("if-modified-since" . "")
   '("if-none-match" . "")
   '("if-range" . "")
   '("if-unmodified-since" . "")
   '("last-modified" . "")
   '("link" . "")
   '("location" . "")
   '("max-forwards" . "")
   '("proxy-authenticate" . "")
   '("proxy-authorization" . "")
   '("range" . "")
   '("referer" . "")
   '("refresh" . "")
   '("retry-after" . "")
   '("server" . "")
   '("set-cookie" . "")
   '("strict-transport-security" . "")
   '("transfer-encoding" . "")
   '("user-agent" . "")
   '("vary" . "")
   '("via" . "")
   '("www-authenticate" . "")))

;; RFC 7541, Appendix B.  Encoding is deliberately kept simple and emits
;; literal octets, but decoding Huffman strings is required for
;; interoperability with ordinary HTTP/2 peers.
(defparameter +hpack-huffman-codes+
  #(#x1ff8 #x7fffd8 #xfffffe2 #xfffffe3 #xfffffe4 #xfffffe5 #xfffffe6
    #xfffffe7 #xfffffe8 #xfffea #x3ffffffc #xfffffe9 #xfffffea #x3ffffffd
    #xfffffeb #xfffffec #xfffffed #xfffffee #xfffffef #xffffff0 #xffffff1
    #xffffff2 #x3ffffffe #xffffff3 #xffffff4 #xffffff5 #xffffff6 #xffffff7
    #xffffff8 #xffffff9 #xffffffa #xffffffb
    #x14 #x3f8 #x3f9 #xffa #x1ff9 #x15 #xf8 #x7fa #x3fa #x3fb #xf9
    #x7fb #xfa #x16 #x17 #x18 #x0 #x1 #x2 #x19 #x1a #x1b #x1c #x1d
    #x1e #x1f #x5c #xfb #x7ffc #x20 #xffb #x3fc
    #x1ffa #x21 #x5d #x5e #x5f #x60 #x61 #x62 #x63 #x64 #x65 #x66 #x67
    #x68 #x69 #x6a #x6b #x6c #x6d #x6e #x6f #x70 #x71 #x72 #xfc #x73
    #xfd #x1ffb #x7fff0 #x1ffc #x3ffc #x22
    #x7ffd #x3 #x23 #x4 #x24 #x5 #x25 #x26 #x27 #x6 #x74 #x75 #x28
    #x29 #x2a #x7 #x2b #x76 #x2c #x8 #x9 #x2d #x77 #x78 #x79 #x7a
    #x7b #x7ffe #x7fc #x3ffd #x1ffd #xffffffc
    #xfffe6 #x3fffd2 #xfffe7 #xfffe8 #x3fffd3 #x3fffd4 #x3fffd5 #x7fffd9
    #x3fffd6 #x7fffda #x7fffdb #x7fffdc #x7fffdd #x7fffde #xffffeb #x7fffdf
    #xffffec #xffffed #x3fffd7 #x7fffe0 #xffffee #x7fffe1 #x7fffe2 #x7fffe3
    #x7fffe4 #x1fffdc #x3fffd8 #x7fffe5 #x3fffd9 #x7fffe6 #x7fffe7 #xffffef
    #x3fffda #x1fffdd #xfffe9 #x3fffdb #x3fffdc #x7fffe8 #x7fffe9 #x1fffde
    #x7fffea #x3fffdd #x3fffde #xfffff0 #x1fffdf #x3fffdf #x7fffeb #x7fffec
    #x1fffe0 #x1fffe1 #x3fffe0 #x1fffe2 #x7fffed #x3fffe1 #x7fffee #x7fffef
    #xfffea #x3fffe2 #x3fffe3 #x3fffe4 #x7ffff0 #x3fffe5 #x3fffe6 #x7ffff1
    #x3ffffe0 #x3ffffe1 #xfffeb #x7fff1 #x3fffe7 #x7ffff2 #x3fffe8 #x1ffffec
    #x3ffffe2 #x3ffffe3 #x3ffffe4 #x7ffffde #x7ffffdf #x3ffffe5 #xfffff1
    #x1ffffed #x7fff2 #x1fffe3 #x3ffffe6 #x7ffffe0 #x7ffffe1 #x3ffffe7
    #x7ffffe2 #xfffff2 #x1fffe4 #x1fffe5 #x3ffffe8 #x3ffffe9 #xffffffd
    #x7ffffe3 #x7ffffe4 #x7ffffe5
    #xfffec #xfffff3 #xfffed #x1fffe6 #x3fffe9 #x1fffe7 #x1fffe8 #x7ffff3
    #x3fffea #x3fffeb #x1ffffee #x1ffffef #xfffff4 #xfffff5 #x3ffffea #x7ffff4
    #x3ffffeb #x7ffffe6 #x3ffffec #x3ffffed #x7ffffe7 #x7ffffe8 #x7ffffe9
    #x7ffffea #x7ffffeb #xffffffe #x7ffffec #x7ffffed #x7ffffee #x7ffffef
    #x7fffff0 #x3ffffee
    #x3fffffff))

(defparameter +hpack-huffman-lengths+
  #(13 23 28 28 28 28 28 28 28 24 30 28 28 30 28 28 28 28 28 28 28 28 30
    28 28 28 28 28 28 28 28 28
    6 10 10 12 13 6 8 11 10 10 8 11 8 6 6 6 5 5 5 6 6 6 6 6 6 6 7 8 15 6
    12 10
    13 6 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 8 7 8 13 19 13 14 6
    15 5 6 5 6 5 6 6 6 5 7 7 6 6 6 5 6 7 6 5 5 6 7 7 7 7 7 15 11 14 13 28
    20 22 20 20 22 22 22 23 22 23 23 23 23 23 24 23 24 24 22 23 24 23 23 23
    23 21 22 23 22 23 23 24
    22 21 20 22 22 23 23 21 23 22 22 24 21 22 23 23 21 21 22 21 23 22 23 23
    20 22 22 22 23 22 22 23
    26 26 20 19 22 23 22 25 26 26 26 27 27 26 24 25 19 21 26 27 27 26 27 24
    21 21 26 26 28 27 27 27
    20 24 20 21 22 21 21 23 22 22 25 25 24 24 26 23 26 27 26 26 27 27 27 27
    27 28 27 27 27 27 27 26
    30))

(defconstant +hpack-default-table-size+ 4096)

(defstruct (%hpack-context (:constructor %make-hpack-context))
  (max-size +hpack-default-table-size+)
  (maximum-size +hpack-default-table-size+)
  (entries '())
  (size 0))
