(defpackage #:http-kit/test-core
  (:use #:cl #:http-kit)
  (:shadowing-import-from #:cl-weave
                          #:describe)
  (:import-from #:cl-weave
                #:expect
                #:expect-not
                #:gen-integer
                #:it
                #:it-property
                #:run-all
                #:signals)
  (:export #:ascii
           #:binary-test-output
           #:binary-test-stream
           #:concatenate-octets
           #:deftest
           #:ensure-conversion-cases
           #:ensure-equal
           #:ensure-printed-contains
           #:ensure-printed=
           #:ensure-serialization-cases
           #:ensure-signals-cases
           #:ensure-summary-contains
           #:ensure-summary=
           #:ensure-true
           #:octets
           #:octets-as-string
           #:run-tests))
