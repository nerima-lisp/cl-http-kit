(in-package #:http-kit/test-core)

(describe "wire-safe utility properties" ; paredit:ignore leftover-inspect-call -- cl-weave DESCRIBE is the native test DSL, not an interactive debugger.
  (it-property "copy-octets preserves every byte value"
    ((value (gen-integer :min 0 :max #xff)))
    (expect (http-kit::%copy-octets (list value))
            :to-equalp (octets value)))

  (it-property "single-byte octet/string conversion round-trips every byte value"
    ((value (gen-integer :min 0 :max #xff)))
    (let* ((octets (octets value))
           (string (http-kit::%octets-string octets))
           (roundtrip (http-kit::%string-octets string)))
      (expect roundtrip :to-equalp octets))))
