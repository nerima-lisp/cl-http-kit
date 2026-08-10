(in-package #:http-kit/observability)

(defstruct (http-metrics
            (:constructor %make-http-metrics
                (registry request-counter error-counter)))
  registry
  request-counter
  error-counter)
