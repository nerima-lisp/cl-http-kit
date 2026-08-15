(in-package #:http-kit)

(defun %check-http-request (request)
  (unless (http-request-p request)
    (error 'http-protocol-error
           :message "A stream request must be an HTTP-REQUEST."
           :operation :transport
           :detail (type-of request)))
  request)

(defun %call-http-operation/cps (operation on-success &key on-error)
  (unless (functionp on-success)
    (error 'http-protocol-error
           :message "The success continuation must be a function."
           :operation :transport
           :detail on-success))
  (when (and on-error (not (functionp on-error)))
    (error 'http-protocol-error
           :message "The error continuation must be a function or NIL."
           :operation :transport
           :detail on-error))
  (let ((response
          (handler-case
              (funcall operation)
            (http-error (condition)
              (if on-error
                  (return-from %call-http-operation/cps
                    (funcall on-error condition))
                  (error condition))))))
    (funcall on-success response)))

(defun %validate-stream-boundary (open-stream close-stream)
  (unless (functionp open-stream)
    (error 'http-protocol-error
           :message "A stream request requires an :OPEN-STREAM function."
           :operation :transport
           :detail open-stream))
  (unless (or (null close-stream) (functionp close-stream))
    (error 'http-protocol-error
           :message "A stream request :CLOSE-STREAM must be a function."
           :operation :transport
           :detail close-stream))
  (values open-stream (or close-stream #'close)))

(defun %http-header-token-p (headers name token)
  (labels ((value-has-token-p (value)
             (loop with start = 0
                   do (let* ((comma (position #\, value :start start))
                             (end (or comma (length value)))
                             (item (%trim-ows (subseq value start end))))
                        (when (string-equal item token)
                          (return t))
                        (unless comma
                          (return nil))
                        (setf start (1+ comma))))))
    (some #'value-has-token-p (http-header-values headers name))))

(defun %request-expect-continue-p (request)
  (and (string= (http-request-protocol-version request) "HTTP/1.1")
       (%http-header-token-p (http-request-headers request)
                             "Expect"
                             "100-continue")))

(defun %request-body-present-p (request request-body-function)
  (or request-body-function
      (plusp (length (http-request-body request)))
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
         (bodyless-p (or (string= method "HEAD")
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
         (not (and (string= method "CONNECT")
                   (<= 200 status 299)))
         self-delimited-p)))

(defconstant +http1-request-body-chunk-size+ 65536)

(defun %validate-request-body-stream-options
    (request request-body-function request-body-length)
  (when (and request-body-function (not (functionp request-body-function)))
    (error 'http-protocol-error
           :message "The request body producer must be a function or NIL."
           :operation :body
           :detail request-body-function))
  (when (and request-body-length
             (or (not (integerp request-body-length))
                 (minusp request-body-length)))
    (error 'http-protocol-error
           :message "The request body length must be a non-negative integer or NIL."
           :operation :body
           :detail request-body-length))
  (when (and (null request-body-function) request-body-length)
    (error 'http-protocol-error
           :message "A request body length requires a request body producer."
           :operation :body
           :detail request-body-length))
  (when (and request-body-function
             (string= (http-request-method request) "TRACE"))
    (error 'http-protocol-error
           :message "TRACE requests must not contain content."
           :operation :body
           :detail :trace-content))
  (when request-body-function
    (unless (zerop (length (http-request-body request)))
      (error 'http-protocol-error
             :message "A streaming request must not also carry an in-memory body."
             :operation :body
             :detail (length (http-request-body request)))))
  (values request-body-function request-body-length))

(defun %check-request-body-chunk (chunk)
  (unless (and (arrayp chunk)
               (= (array-rank chunk) 1)
               (not (stringp chunk))
               (loop for octet across chunk
                     always (and (integerp octet) (<= 0 octet #xff))))
    (error 'http-protocol-error
           :message "A request body producer must return a one-dimensional octet vector or NIL."
           :operation :body
           :detail (type-of chunk)))
  (when (zerop (length chunk))
    (error 'http-protocol-error
           :message "A request body producer must not return an empty chunk."
           :operation :body
           :detail chunk))
  (when (> (length chunk) +http1-request-body-chunk-size+)
    (error 'http-protocol-error
           :message "A request body producer returned more than the advertised maximum chunk size."
           :operation :body
           :detail (length chunk)))
  chunk)

(defun %write-http-stream-string (stream string)
  (write-sequence (%string-octets string :context :serialization) stream))

(defun %write-http-stream-crlf (stream)
  (write-byte 13 stream)
  (write-byte 10 stream))

(defun %write-request-body-stream
    (stream request-body-function expected-body-length transfer-mode
            deadline clock-function)
  (let ((written 0))
    (loop
      (%check-deadline deadline clock-function :write)
      (let ((chunk (funcall request-body-function
                            +http1-request-body-chunk-size+)))
        (if (null chunk)
            (progn
              (when (and expected-body-length
                         (/= written expected-body-length))
                (error 'http-protocol-error
                       :message "The request body producer ended before its declared length."
                       :operation :body
                       :detail (list :expected expected-body-length
                                     :observed written)))
              (when (eq transfer-mode :chunked)
                (%write-http-stream-string stream "0")
                (%write-http-stream-crlf stream)
                (%write-http-stream-crlf stream))
              (return written))
            (let* ((validated-chunk (%check-request-body-chunk chunk))
                   (chunk-length (length validated-chunk))
                   (new-written (+ written chunk-length)))
              (when (and expected-body-length
                         (> new-written expected-body-length))
                (error 'http-protocol-error
                       :message "The request body producer exceeded its declared length."
                       :operation :body
                       :detail (list :expected expected-body-length
                                     :observed new-written)))
              (when (eq transfer-mode :chunked)
                (%write-http-stream-string stream
                                           (format nil "~X" chunk-length))
                (%write-http-stream-crlf stream)
                (write-sequence validated-chunk stream)
                (%write-http-stream-crlf stream))
              (unless (eq transfer-mode :chunked)
                (write-sequence validated-chunk stream))
              (setf written new-written)))))))

(defun %write-request-body-octets
    (stream request transfer-mode expected-body-length deadline clock-function)
  (let ((body (http-request-body request))
        (body-length (length (http-request-body request))))
    (when (and expected-body-length (/= body-length expected-body-length))
      (error 'http-protocol-error
             :message "The in-memory request body length changed after serialization."
             :operation :body
             :detail (list :expected expected-body-length
                           :observed body-length)))
    (%check-deadline deadline clock-function :write)
    (if (eq transfer-mode :chunked)
        (progn
          (unless (zerop body-length)
            (%write-http-stream-string stream (format nil "~X" body-length))
            (%write-http-stream-crlf stream)
            (write-sequence body stream)
            (%write-http-stream-crlf stream))
          (%write-http-stream-string stream "0")
          (%write-http-stream-crlf stream)
          (dolist (trailer (http-request-trailers request))
            (%write-http-stream-string stream (http-header-name trailer))
            (%write-http-stream-string stream ": ")
            (%write-http-stream-string stream (http-header-content trailer))
            (%write-http-stream-crlf stream))
          (%write-http-stream-crlf stream))
        (write-sequence body stream))
    body-length))

(defun send-http-request-over-open-stream
    (request stream &key timeout deadline
                      max-header-bytes max-fields max-body-bytes
                      request-target on-body-chunk on-information
                      request-body-function request-body-length
                      (collect-body-p t)
                      (clock-function #'%monotonic-time))
  "Send one REQUEST over an already-open binary STREAM.

The stream remains open.  The primary value is the response and the second
value is the conservative result of HTTP-RESPONSE-REUSABLE-P.  Callers must
close the stream when the second value is NIL or when this function signals.
This boundary makes connection reuse possible without imposing socket, DNS,
TLS, proxy, or concurrency dependencies on the core system."
  (%check-http-request request)
  (%validate-request-body-stream-options
   request request-body-function request-body-length)
  (unless (streamp stream)
    (error 'http-connection-error
           :message "The stream boundary requires an open stream."
           :operation :connect
           :cause (type-of stream)))
  (with-http-deadline (absolute-deadline timeout
                        :inherited deadline
                        :clock-function clock-function)
    (%with-http-error-translation
        ("The HTTP request over an open stream failed." :transport)
      (let ((body-sent-p nil)
            (expect-continue-p
              (and (%request-expect-continue-p request)
                   (%request-body-present-p request request-body-function)))
            (transfer-mode nil)
            (expected-body-length nil))
        (labels ((send-body ()
                   (unless body-sent-p
                     (if request-body-function
                         (%write-request-body-stream
                          stream request-body-function expected-body-length
                          transfer-mode absolute-deadline clock-function)
                         (%write-request-body-octets
                          stream request transfer-mode expected-body-length
                          absolute-deadline clock-function))
                     (finish-output stream)
                     (setf body-sent-p t)))
                 (handle-information (information)
                   (when on-information
                     (funcall on-information information))
                   (when (and expect-continue-p
                              (= (http-response-status information) 100))
                     (send-body))))
          (%check-deadline absolute-deadline clock-function :write)
          (if expect-continue-p
              (multiple-value-bind (head mode expected)
                  (%request-head-wire
                   request
                   :request-target request-target
                   :body-length (if request-body-function
                                    request-body-length
                                    (length (http-request-body request)))
                   :body-length-known-p (if request-body-function
                                            (not (null request-body-length))
                                            t))
                (setf transfer-mode mode
                      expected-body-length expected)
                (write-sequence head stream)
                (finish-output stream))
              (if request-body-function
                  (multiple-value-bind (head mode expected)
                      (%request-head-wire
                       request
                       :request-target request-target
                       :body-length request-body-length
                       :body-length-known-p (not (null request-body-length)))
                    (setf transfer-mode mode
                          expected-body-length expected)
                    (write-sequence head stream)
                    (send-body))
                  (let ((wire (serialize-http-request
                               request :request-target request-target)))
                    (write-sequence wire stream)
                    (finish-output stream)
                    (setf body-sent-p t))))
          (%check-deadline absolute-deadline clock-function :read)
          (let ((response
                  (parse-http-response stream
                                       :deadline absolute-deadline
                                       :max-header-bytes max-header-bytes
                                       :max-fields max-fields
                                       :max-body-bytes max-body-bytes
                                       :request-method (http-request-method request)
                                       :on-body-chunk on-body-chunk
                                       :on-information
                                       (and (or on-information expect-continue-p)
                                            #'handle-information)
                                       :collect-body-p collect-body-p
                                       :clock-function clock-function)))
            (values response
                    (and (or (not expect-continue-p) body-sent-p)
                          (http-response-reusable-p request response)))))))))

(defun send-http-request-over-stream
    (request &key open-stream close-stream timeout deadline
                   max-header-bytes max-fields max-body-bytes
                   request-target on-body-chunk on-information
                   request-body-function request-body-length
                   (collect-body-p t)
                   (clock-function #'%monotonic-time))
  "Send REQUEST through an injected binary stream boundary.

OPEN-STREAM is called as (REQUEST &KEY TIMEOUT DEADLINE) and must return a
stream.  CLOSE-STREAM is called with that stream after the exchange.  Socket
creation, DNS, proxy policy, TLS, and pooling remain application policies
outside this direct function boundary."
  (%check-http-request request)
  (%validate-request-body-stream-options
   request request-body-function request-body-length)
  (multiple-value-bind (open close)
      (%validate-stream-boundary open-stream close-stream)
    (let ((stream nil))
      (with-http-deadline (absolute-deadline timeout
                            :inherited deadline
                            :clock-function clock-function)
        (%with-http-error-translation
            ("The HTTP stream request failed." :transport)
          (unwind-protect
               (progn
                 (setf stream
                       (funcall open request
                                :timeout timeout
                                :deadline absolute-deadline))
                 (unless (streamp stream)
                   (error 'http-connection-error
                          :message "The stream factory did not return a stream."
                          :operation :connect
                          :cause (type-of stream)))
                 (send-http-request-over-open-stream
                  request stream
                  :deadline absolute-deadline
                  :max-header-bytes max-header-bytes
                  :max-fields max-fields
                  :max-body-bytes max-body-bytes
                  :request-target request-target
                  :on-body-chunk on-body-chunk
                  :on-information on-information
                  :request-body-function request-body-function
                  :request-body-length request-body-length
                  :collect-body-p collect-body-p
                  :clock-function clock-function))
            (when stream
              (funcall close stream))))))))

(defun send-http-request-over-stream/cps
    (request on-success
     &key on-error open-stream close-stream timeout deadline
       max-header-bytes max-fields max-body-bytes
       request-target on-body-chunk on-information
       request-body-function request-body-length
       (collect-body-p t)
       (clock-function #'%monotonic-time))
  "Send a stream request and dispatch the result to CPS continuations."
  (%call-http-operation/cps
   (lambda ()
     (send-http-request-over-stream
      request
      :open-stream open-stream
      :close-stream close-stream
      :timeout timeout
      :deadline deadline
      :max-header-bytes max-header-bytes
      :max-fields max-fields
      :max-body-bytes max-body-bytes
      :request-target request-target
      :on-body-chunk on-body-chunk
      :on-information on-information
      :request-body-function request-body-function
      :request-body-length request-body-length
      :collect-body-p collect-body-p
      :clock-function clock-function))
   on-success
   :on-error on-error))
