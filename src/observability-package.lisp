(defpackage #:http-kit/observability
  (:use #:cl #:http-kit)
  (:import-from #:observability-kit
                #:define-counter
                #:make-metric-registry
                #:metric-inc
                #:metric-registry)
  (:export
   #:http-metrics
   #:http-metrics-p
   #:make-http-metrics
   #:http-metrics-registry
   #:http-metrics-request-counter
   #:http-metrics-error-counter
   #:call-with-http-observability/cps))
