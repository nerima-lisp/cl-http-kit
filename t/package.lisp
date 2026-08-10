(defpackage #:http-kit/test
  (:use #:cl #:http-kit #:http-kit/client #:http-kit/websocket
        #:http-kit/network #:http-kit/observability)
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
  (:import-from #:observability-kit
                #:make-metric-registry
                #:metric-sample-labels
                #:metric-sample-value
                #:metric-snapshot
                #:metric-snapshot-samples)
  (:export #:run-tests))
