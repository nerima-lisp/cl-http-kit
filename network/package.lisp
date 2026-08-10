(defpackage #:http-kit/network
  (:use #:cl)
  (:import-from #:http-kit
                #:http-connection-error
                #:http-deadline
                #:http-error
                #:http-protocol-error
                #:http-request-uri
                #:serve-http1-session
                #:http-timeout
                #:http-unsupported-feature
                #:http-uri-host
                #:http-uri-port
                #:http-uri-scheme)
  (:export #:open-http-tcp-stream
           #:close-http-tcp-stream
           #:http-network-resolve-host
           #:make-http-network-stream-opener
           #:http-network-listener-p
           #:http-network-listener-address
           #:http-network-listener-port
           #:http-network-listener-address-family
           #:open-http-tcp-listener
           #:accept-http-tcp-stream
           #:close-http-tcp-listener
           #:serve-http1-listener))
