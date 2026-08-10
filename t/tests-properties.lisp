(in-package #:http-kit/test)

(describe "wire-safe utility properties" ; paredit:ignore leftover-inspect-call -- cl-weave DESCRIBE is the native test DSL, not an interactive debugger.
  (it-property "copy-octets preserves every byte value"
    ((value (gen-integer :min 0 :max #xff)))
    (expect (http-kit::%copy-octets (list value))
            :to-equalp (octets value))))
