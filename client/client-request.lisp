(in-package #:http-kit/client)

(defun %client-empty-octets ()
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun %client-body-octets (body)
  (cond
    ((null body)
     (%client-empty-octets))
    ((stringp body)
     (cl-codec-kit:string-to-octets body :encoding :utf-8))
    ((and (arrayp body) (= (array-rank body) 1))
     (let ((octets (make-array (array-total-size body)
                               :element-type '(unsigned-byte 8))))
       (dotimes (index (length octets) octets)
         (let ((value (row-major-aref body index)))
           (unless (and (integerp value) (<= 0 value 255))
             (%client-protocol-error
              "Request bodies must contain unsigned octets."
              value))
           (setf (aref octets index) value)))))
    (t
     (%client-protocol-error
      "Request bodies must be strings, one-dimensional arrays, or NIL."
      body))))

(defun %client-copy-header-list (headers)
  (mapcar (lambda (header)
            (make-http-header (http-header-name header)
                              (http-header-content header)))
          headers))

(defun %client-normalize-headers (headers)
  ;; Let the core request constructor retain its single header validation path.
  (http-request-headers
   (make-http-request :method "GET"
                      :uri "http://example.com/"
                      :headers headers
                      :body (%client-empty-octets))))

(defun %client-header-named-p (header names)
  (member (http-header-name header) names :test #'string-equal))

(defun %client-remove-headers (headers names)
  (remove-if (lambda (header)
               (%client-header-named-p header names))
             headers))

(defun %client-header-value (headers name)
  (http-header-value headers name nil))

(defun %client-header-set (headers name value)
  (append (%client-remove-headers headers (list name))
          (list (make-http-header name value))))

(defun %client-request-rebuild
    (request &key method uri headers body (trailers nil trailers-supplied-p))
  (make-http-request :method (or method (http-request-method request))
                     :protocol-version (http-request-protocol-version request)
                     :uri (or uri (http-request-uri request))
                     :headers (or headers (http-request-headers request))
                     :trailers (if trailers-supplied-p
                                   trailers
                                   (http-kit:http-request-trailers request))
                     :body (or body (%client-empty-octets))))

(defun %client-validate-request-body-stream
    (request request-body-function request-body-factory request-body-length)
  (when request-body-function
    (%ensure-function request-body-function
                      "The request body producer must be a function."))
  (when request-body-factory
    (%ensure-function request-body-factory
                      "The request body factory must be a function."))
  (when (and request-body-function request-body-factory)
    (%client-protocol-error
     "A request body producer cannot be combined with a request body factory."
     request))
  (%client-validate-limit
   request-body-length
   "The request body length must be a non-negative integer or NIL.")
  (when (and request-body-length
             (null (or request-body-function request-body-factory)))
    (%client-protocol-error
     "A request body length requires a request body producer or factory."
     request-body-length))
  (when (and (or request-body-function request-body-factory)
             (plusp (array-total-size (http-request-body request))))
    (%client-protocol-error
     "A request body producer or factory cannot be combined with an in-memory request body."
     request))
  (values request-body-function request-body-factory request-body-length))

(defun %client-request-body-for-attempt
    (request-body-function request-body-factory)
  (if request-body-factory
      (%ensure-function
       (funcall request-body-factory)
       "A request body factory must return a fresh producer function.")
      request-body-function))

(defun %client-request-with
    (request &key headers body method uri (trailers nil trailers-supplied-p))
  (%client-request-rebuild
   request
   :method method
   :uri uri
   :headers headers
   :trailers (if trailers-supplied-p
                 trailers
                 (http-kit:http-request-trailers request))
   :body (if body
             (%client-body-octets body)
             (http-request-body request))))
