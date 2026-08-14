(in-package #:http-kit/client)

(defun %client-authorization-value (value)
  (cond
    ((null value) nil)
    ((stringp value) value)
    ((http-header-p value) (http-header-content value))
    (t
     (%client-protocol-error
      "An authentication provider must return a string, HTTP-HEADER, or NIL."
      value))))

(defun %client-prepare-request (client request initial-uri &key redirect-p)
  (let ((headers (http-request-headers request)))
    (when redirect-p
      (setf headers (%client-remove-headers headers '("Cookie"))))
    (let ((cookie-value
            (http-cookie-jar-cookie-header
             (http-client-cookie-jar client)
             (http-request-uri request)
             :partition-key (http-client-cookie-partition-key client)
             :same-site-context (http-client-cookie-same-site-context client)
             :method (http-request-method request))))
      (when cookie-value
        (setf headers (%client-header-set headers "Cookie" cookie-value))))
    (when (and (http-client-auth-provider client)
               (http-same-origin-p initial-uri (http-request-uri request))
               (not (http-header-present-p headers "Authorization")))
      (let ((value (%client-authorization-value
                    (funcall (http-client-auth-provider client) request))))
        (when value
          (setf headers (%client-header-set headers "Authorization" value)))))
    (%client-request-with request :headers headers)))

(defun %client-store-response (client request response)
  "Update CLIENT's cache for the completed REQUEST and RESPONSE."
  (when (http-client-cache client)
    (cond
      ((= (http-response-status response) 304)
       (http-cache-store (http-client-cache client) request response))
      ((%client-cacheable-method-p request)
       (http-cache-store (http-client-cache client) request response))
      ((%client-mutating-method-p request)
       (http-cache-invalidate (http-client-cache client) request)))))

(defun %client-follow-redirect-p
    (prepared next-request request-body-function request-body-factory)
  "Whether NEXT-REQUEST can safely reuse the request body producer."
  (and next-request
       (or (null request-body-function)
           request-body-factory
           (not (string-equal
                 (http-request-method next-request)
                 (http-request-method prepared))))))

(defun %client-response-merge-304 (cached response)
  (let* ((new-headers (http-response-headers response))
         (names (remove-duplicates (mapcar #'http-header-name new-headers)
                                   :test #'string-equal))
         (headers (append (%client-remove-headers
                           (http-response-headers cached) names)
                          (%client-copy-header-list new-headers))))
    (make-http-response
     :protocol-version (http-response-protocol-version cached)
     :status (http-response-status cached)
     :reason (http-response-reason cached)
     :headers headers
     :trailers (http-response-trailers response)
     :body (http-response-body cached))))

(defun %client-redirect-method (method status)
  (cond
    ((= status 303)
     (if (string-equal method "HEAD") "HEAD" "GET"))
    ((and (member status '(301 302))
          (string-equal method "POST"))
     "GET")
    (t method)))

(defun %client-redirect-request
    (request response policy initial-uri redirect-count)
  (declare (ignore redirect-count))
  (let ((location (%client-header-value
                    (http-response-headers response) "Location")))
    (unless location
      (return-from %client-redirect-request))
    (let* ((target (resolve-http-uri (http-request-uri request) location))
           (old-scheme (http-uri-scheme (http-request-uri request)))
           (new-scheme (http-uri-scheme target))
           (downgrade (and (string-equal old-scheme "https")
                           (string-equal new-scheme "http")))
           (new-method (%client-redirect-method
                        (http-request-method request)
                        (http-response-status response)))
           (method-changed (not (string-equal new-method
                                              (http-request-method request))))
           (headers (%client-remove-headers
                     (http-request-headers request)
                     '("Cookie"))))
      (when (and downgrade
                 (not (http-redirect-policy-allow-downgrade-p policy)))
        (%client-error
         'http-client-error
         "The redirect policy rejected an HTTPS to HTTP downgrade."
         :detail target
         :operation :redirect))
      (unless (or (http-redirect-policy-preserve-authorization-p policy)
                  (http-same-origin-p initial-uri target))
        (setf headers (%client-remove-headers headers '("Authorization"))))
      (when method-changed
        (setf headers (%client-remove-headers
                       headers '("Content-Length" "Content-Type"
                                 "Transfer-Encoding"))))
      (%client-request-rebuild
       request
       :method new-method
       :uri target
       :headers headers
       :trailers (unless method-changed
                   (http-kit:http-request-trailers request))
       :body (if method-changed
                 (%client-empty-octets)
                 (http-request-body request))))))

(defun %client-cacheable-method-p (request)
  (member (string-upcase (http-request-method request)) '("GET" "HEAD")
          :test #'string=))

(defun %client-mutating-method-p (request)
  (not (%client-cacheable-method-p request)))

(defun %client-conditional-request (request entry)
  (let ((headers (http-request-headers request)))
    (when (http-cache-entry-etag entry)
      (setf headers (%client-header-set
                     headers "If-None-Match" (http-cache-entry-etag entry))))
    (when (and (http-cache-entry-last-modified entry)
               (not (http-cache-entry-etag entry)))
      (setf headers (%client-header-set
                     headers "If-Modified-Since"
                     (http-cache-entry-last-modified entry))))
    (%client-request-with request :headers headers)))
