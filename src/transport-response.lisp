(in-package #:http-kit)

(defun %http-header-token-p (headers name token)
  (%http1-header-token-p headers name token :operation :transport))

(defun %request-expect-continue-p (request)
  (and (string= (http-request-protocol-version request) "HTTP/1.1")
       (%http-header-token-p (http-request-headers request)
                             "Expect"
                             "100-continue")))

(defun %request-body-present-p (request request-body-function)
  (or request-body-function
      (plusp (array-total-size (http-request-body request)))
      (not (null (http-request-trailers request)))))

(defun http-response-reusable-p (request response)
  "Return true when RESPONSE can remain on REQUEST's HTTP/1.x stream.

This predicate deliberately reports false for close-delimited responses and
successful CONNECT responses.  It is a conservative boundary for connection
pools: a false result asks the caller to close the stream, while a true result
means that the response framing was self-delimiting and neither side asked
for connection close."
  (unless (http-request-p request)
    (error 'http-protocol-error
           :message "The request must be an HTTP-REQUEST."
           :operation :transport
           :detail request))
  (unless (http-response-p response)
    (error 'http-protocol-error
           :message "The response must be an HTTP-RESPONSE."
           :operation :transport
           :detail response))
  (let* ((request-headers (http-request-headers request))
         (response-headers (http-response-headers response))
         (method (http-request-method request))
         (status (http-response-status response))
         (protocol-version (http-response-protocol-version response))
         (http10-p (string= protocol-version "HTTP/1.0"))
         (http11-p (string= protocol-version "HTTP/1.1"))
         (bodyless-p (or (string-equal method "HEAD")
                         (= status 204)
                         (= status 205)
                         (= status 304)
                         (= status 101)))
         (self-delimited-p
           (or bodyless-p
               (http-header-present-p response-headers "Content-Length")
               (http-header-present-p response-headers "Transfer-Encoding"))))
    (and (or http10-p http11-p)
         (not (%http-header-token-p request-headers "Connection" "close"))
         (not (%http-header-token-p response-headers "Connection" "close"))
         (or http11-p
             (%http-header-token-p response-headers "Connection" "keep-alive"))
         (not (= status 101))
         (not (and (string-equal method "CONNECT")
                   (<= 200 status 299)))
         self-delimited-p)))
