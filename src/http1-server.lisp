(in-package #:http-kit)

(defun %request-parse-error (message &optional detail)
  (error 'http-protocol-error
         :message message
         :operation :request-parse
         :detail detail))

(defun %request-header-error (message name reason &optional detail)
  (error 'http-invalid-header
         :message message
         :operation :request-parse
         :name name
         :reason reason
         :detail detail))

(defun %parse-request-line (line)
  (let* ((first-space (position #\Space line))
         (second-space (and first-space
                           (position #\Space line :start (1+ first-space)))))
    (unless (and first-space
                 second-space
                 (null (position #\Space line :start (1+ second-space))))
      (%request-parse-error
       "An HTTP request-line must contain METHOD, request-target, and version separated by single spaces."
       line))
    (let ((method (subseq line 0 first-space))
          (target (subseq line (1+ first-space) second-space))
          (version (subseq line (1+ second-space))))
      (unless (%token-p method)
        (%request-parse-error
         "An HTTP request method must be a non-empty token."
         method))
      (unless (%request-target-value-p target)
        (%request-parse-error
         "An HTTP request-target must be non-empty and contain no controls or spaces."
         target))
      (unless (member version '("HTTP/1.0" "HTTP/1.1") :test #'string=)
        (%request-parse-error
         "Only HTTP/1.0 and HTTP/1.1 request versions are supported."
         version))
      (values (string-upcase method) target version))))

(defun %parse-request-header-line (line)
  (when (and (plusp (length line))
             (find (char line 0) '(#\Space #\Tab)))
    (%request-parse-error
     "Obsolete folded request headers are not accepted."
     line))
  (let ((colon (position #\: line)))
    (unless colon
      (%request-header-error
       "An HTTP request header must contain a colon."
       nil :missing-colon line))
    (let ((name (subseq line 0 colon))
          (content (%trim-ows (subseq line (1+ colon)))))
      (handler-case
          (make-http-header name content)
        (http-invalid-header (condition)
          (error 'http-invalid-header
                 :message (http-error-message condition)
                 :operation :request-parse
                 :name (or (http-invalid-header-name condition) name)
                 :reason (http-invalid-header-reason condition)))))))

(defun %read-request-headers
    (source deadline clock-function max-header-bytes header-used)
  (let ((headers '())
        (bytes header-used))
    (loop
      (multiple-value-bind (line updated-bytes)
          (%read-crlf-line source deadline clock-function max-header-bytes bytes
                           :operation :request-parse)
        (setf bytes updated-bytes)
        (if (zerop (length line))
            (return (values (nreverse headers) bytes))
            (push (%parse-request-header-line line) headers))))))

(defun %request-content-length (headers)
  (let ((values (http-header-values headers "content-length")))
    (cond
      ((null values) nil)
      ((not (every #'%decimal-string-p values))
       (%request-header-error
        "Content-Length must be an ASCII decimal integer."
        "content-length" :value values))
      ((not (every (lambda (value)
                     (= (%parse-decimal value)
                        (%parse-decimal (first values))))
                   values))
       (%request-header-error
        "Duplicate Content-Length values must agree."
        "content-length" :duplicate values))
      (t (%parse-decimal (first values))))))

(defun %request-transfer-mode (headers)
  (let ((values (http-header-values headers "transfer-encoding")))
    (when values
      (let ((codings '()))
        (dolist (value values)
          (let ((start 0))
            (loop
              for comma = (position #\, value :start start)
              for end = (or comma (length value))
              for coding = (%trim-ows (subseq value start end))
              do (when (zerop (length coding))
                   (%request-header-error
                    "A comma-separated Transfer-Encoding contains an empty item."
                    "transfer-encoding" :empty-item value))
                 (push (string-downcase coding) codings)
                 (if comma
                     (setf start (1+ comma))
                     (return)))))
        (setf codings (nreverse codings))
        (unless (and (consp codings)
                     (null (cdr codings))
                     (string= (first codings) "chunked"))
          (error 'http-unsupported-feature
                 :message "Only a single HTTP/1.1 chunked transfer coding is supported."
                 :operation :request-parse
                 :feature :http1-request-transfer-encoding
                 :detail codings))
        :chunked))))

(defun %request-expectation (headers protocol-version)
  (let ((values (http-header-values headers "expect")))
    (when values
      (let ((expectations '()))
        (dolist (value values)
          (let ((start 0))
            (loop
              for comma = (position #\, value :start start)
              for end = (or comma (length value))
              for expectation = (%trim-ows (subseq value start end))
              do (when (zerop (length expectation))
                   (%request-header-error
                    "A comma-separated Expect header contains an empty item."
                    "expect" :empty-item value))
                 (push (string-downcase expectation) expectations)
                 (if comma
                     (setf start (1+ comma))
                     (return)))))
        (setf expectations (nreverse expectations))
        (unless (and (string= protocol-version "HTTP/1.1")
                     (every (lambda (expectation)
                              (string= expectation "100-continue"))
                            expectations))
          (error 'http-unsupported-feature
                 :message "Only HTTP/1.1 Expect: 100-continue is supported."
                 :operation :request-parse
                 :feature :http1-expectation
                 :detail expectations))
        t))))

(defun %request-authority-uri (authority name &optional (scheme "http"))
  (handler-case
      (make-http-uri :scheme scheme :authority authority :path "/")
    (http-invalid-uri (condition)
      (%request-header-error
       "An HTTP authority is not valid."
       name :value
       (list authority (http-error-message condition))))))

(defun %request-effective-port (uri)
  (or (http-uri-port uri)
      (if (string= (http-uri-scheme uri) "https") 443 80)))

(defun %request-authorities-agree-p (left right)
  (and (string= (http-uri-host left) (http-uri-host right))
       (= (%request-effective-port left)
          (%request-effective-port right))))

(defun %request-target-uri
    (method version target host-values default-authority)
  (when (> (length host-values) 1)
    (%request-header-error
     "An HTTP request may contain only one Host field."
     "host" :duplicate host-values))
  (when (and default-authority (not (stringp default-authority)))
    (%request-parse-error
     "DEFAULT-AUTHORITY must be a string or NIL."
     default-authority))
  (let* ((host-uri (and host-values
                        (%request-authority-uri (first host-values) "host")))
         (default-uri (and default-authority
                           (%request-authority-uri default-authority
                                                   "default-authority"))))
    (when (and (string= version "HTTP/1.1") (null host-uri))
      (%request-header-error
       "HTTP/1.1 requests must contain a Host field."
       "host" :missing))
    (cond
      ((string= method "CONNECT")
       (when (or (char= (char target 0) #\/) (string= target "*")
                 (search "://" target))
         (%request-parse-error
          "CONNECT request-targets must use authority-form."
          target))
       (let ((target-uri (%request-authority-uri target "request-target")))
         (unless (http-uri-port target-uri)
           (%request-parse-error
            "A CONNECT authority-form target must include a port."
            target))
         (when (and host-uri
                    (not (%request-authorities-agree-p host-uri target-uri)))
           (%request-header-error
            "The HTTP Host field must agree with the CONNECT authority."
            "host" :host-authority-mismatch target))
         target-uri))
      ((char= (char target 0) #\/)
       (let ((authority-uri (or host-uri default-uri)))
         (unless authority-uri
           (%request-parse-error
            "An origin-form request requires Host or DEFAULT-AUTHORITY."
            target))
         (let* ((query-position (position #\? target))
                (path (if query-position
                          (subseq target 0 query-position)
                          target))
                (query (and query-position
                            (subseq target (1+ query-position)))))
           (make-http-uri :scheme "http"
                          :authority (http-uri-authority authority-uri)
                          :path path
                          :query query))))
      ((string= target "*")
       (unless (string= method "OPTIONS")
         (%request-parse-error
          "The asterisk-form request-target is only valid for OPTIONS."
          method))
       (let ((authority-uri (or host-uri default-uri)))
         (unless authority-uri
           (%request-parse-error
            "An asterisk-form request requires Host or DEFAULT-AUTHORITY."
            target))
         (make-http-uri :scheme "http"
                        :authority (http-uri-authority authority-uri)
                        :path "/")))
      ((search "://" target)
       (let ((target-uri (parse-http-uri target)))
         (when (and host-uri
                    (not (%request-authorities-agree-p host-uri target-uri)))
           (setf host-uri
                 (%request-authority-uri
                  (first host-values) "host"
                  (http-uri-scheme target-uri)))
           (unless (%request-authorities-agree-p host-uri target-uri)
           (%request-header-error
            "The HTTP Host field must agree with the absolute request-target authority."
            "host" :host-authority-mismatch target)))
         target-uri))
      (t
       (%request-parse-error
        "An HTTP request-target must use origin-, absolute-, authority-, or asterisk-form."
        target)))))

(defun %read-request-body-segment
    (source length deadline clock-function detail collector on-body-chunk)
  (loop with remaining = length
        while (plusp remaining)
        do (let* ((size (min remaining *http-body-read-chunk-size*))
                  (chunk (make-array size :element-type '(unsigned-byte 8))))
             (loop for index below size
                   do (setf (aref chunk index)
                            (%read-required-byte source deadline clock-function detail
                                                 :operation :request-parse)))
             (%append-body-chunk collector chunk)
             (when on-body-chunk
               (funcall on-body-chunk chunk))
             (decf remaining size)))
  collector)

(defun %read-request-exact-body
    (source length deadline clock-function max-body-bytes
     &key on-body-chunk (collect-body-p t))
  (%check-limit :body length max-body-bytes :operation :request-parse)
  (%finish-body-collector
   (%read-request-body-segment source length deadline clock-function :body
                               (%make-body-collector collect-body-p)
                               on-body-chunk)))

(defun %read-request-chunked-body
    (source deadline clock-function max-header-bytes max-body-bytes header-used
     &key on-body-chunk (collect-body-p t))
  (let ((body (%make-body-collector collect-body-p))
        (trailers '())
        (bytes header-used)
        (body-length 0))
    (loop
      (multiple-value-bind (line updated-bytes)
          (%read-crlf-line source deadline clock-function max-header-bytes bytes
                           :operation :request-parse)
        (setf bytes updated-bytes)
        (let* ((separator (position #\; line))
               (size-text (%trim-ows (if separator
                                        (subseq line 0 separator)
                                        line))))
          (unless (and (plusp (length size-text))
                       (every #'%hex-character-p size-text))
            (%request-parse-error
             "A chunk size is not a valid hexadecimal integer."
             line))
          (let ((size (parse-integer size-text :radix 16)))
            (if (zerop size)
                (progn
                  (multiple-value-bind (parsed-trailers trailer-bytes)
                      (%read-request-headers source deadline clock-function
                                              max-header-bytes bytes)
                    (setf trailers parsed-trailers
                          bytes trailer-bytes))
                  (%validate-http1-trailers trailers :request-parse)
                  (return))
                (progn
                  (incf body-length size)
                  (%check-limit :body body-length max-body-bytes
                                :operation :request-parse)
                  (%read-request-body-segment source size deadline clock-function
                                              :chunk-data body on-body-chunk)
                  (%read-framing-crlf source deadline clock-function
                                      :operation :request-parse)
                  ;; Chunk data itself is body bytes, but the CRLF framing is
                  ;; still part of the bounded request envelope.
                  (incf bytes 2)
                  (%check-limit :headers bytes max-header-bytes
                                :operation :request-parse)))))))
    (values (%finish-body-collector body) trailers)))

(defun parse-http-request
    (input &key timeout deadline max-header-bytes max-body-bytes
             default-authority on-body-chunk (collect-body-p t)
             on-expect-continue (clock-function #'%monotonic-time)
             allow-eof-p)
  "Parse one HTTP/1.0 or HTTP/1.1 request from INPUT.

INPUT may be an octet vector or a binary stream.  Request bodies are
framed by Content-Length or HTTP/1.1 chunked transfer coding; an
unframed request is treated as having an empty body because requests do
not use close-delimited framing.  When ON-EXPECT-CONTINUE is a function,
it receives a metadata-only request before an expected request body is
read."
  (let* ((source (%make-byte-source-for input :operation :request-parse))
         (absolute-deadline (http-deadline timeout :deadline deadline
                                           :clock-function clock-function))
         (header-limit (or max-header-bytes *default-max-header-bytes*))
         (body-limit (or max-body-bytes *default-max-body-bytes*)))
    (unless (and (integerp header-limit) (plusp header-limit))
      (%request-parse-error
       "The request header limit must be a positive integer."
       header-limit))
    (unless (and (integerp body-limit) (>= body-limit 0))
      (%request-parse-error
       "The request body limit must be a non-negative integer."
       body-limit))
    (when (and on-body-chunk (not (functionp on-body-chunk)))
      (%request-parse-error
       "ON-BODY-CHUNK must be a function or NIL."
       on-body-chunk))
    (when (and on-expect-continue (not (functionp on-expect-continue)))
      (%request-parse-error
       "ON-EXPECT-CONTINUE must be a function or NIL."
       on-expect-continue))
    (unless (member collect-body-p '(nil t))
      (%request-parse-error
       "COLLECT-BODY-P must be NIL or T."
       collect-body-p))
    (unless (member allow-eof-p '(nil t))
      (%request-parse-error
       "ALLOW-EOF-P must be NIL or T."
       allow-eof-p))
    (multiple-value-bind (request-line header-used)
        (%read-crlf-line source absolute-deadline clock-function header-limit 0
                         :operation :request-parse
                         :allow-eof-p allow-eof-p)
      (if (eq request-line :eof)
          nil
          (multiple-value-bind (method target version)
              (%parse-request-line request-line)
            (multiple-value-bind (headers final-header-bytes)
                (%read-request-headers source absolute-deadline clock-function
                                       header-limit header-used)
              (let* ((host-values (http-header-values headers "host"))
                     (uri (%request-target-uri method version target host-values
                                               default-authority))
                     (transfer-mode (%request-transfer-mode headers))
                     (content-length (%request-content-length headers))
                     (expect-continue-p (%request-expectation headers version)))
                (when (and transfer-mode content-length)
                  (%request-header-error
                   "Transfer-Encoding and Content-Length must not be combined."
                   "content-length" :framing-conflict))
                (when (and (string= version "HTTP/1.0") transfer-mode)
                  (error 'http-unsupported-feature
                         :message "HTTP/1.0 transfer codings are unsupported."
                         :operation :request-parse
                         :feature :http1-request-transfer-encoding
                         :detail transfer-mode))
                (when (and expect-continue-p
                           on-expect-continue
                           (or transfer-mode
                               (and content-length (plusp content-length))))
                  (funcall on-expect-continue
                           (make-http-request
                            :method method
                            :protocol-version version
                            :uri uri
                            :request-target target
                            :headers headers
                            :trailers '()
                            :body (%empty-octets))))
                (let (body trailers)
                  (cond
                    ((eq transfer-mode :chunked)
                     (multiple-value-setq (body trailers)
                       (%read-request-chunked-body
                        source absolute-deadline clock-function header-limit body-limit
                        final-header-bytes
                        :on-body-chunk on-body-chunk
                        :collect-body-p collect-body-p)))
                    (content-length
                     (setf body (%read-request-exact-body
                                 source content-length absolute-deadline clock-function
                                 body-limit :on-body-chunk on-body-chunk
                                 :collect-body-p collect-body-p)
                           trailers '()))
                    (t
                     (setf body (%empty-octets)
                           trailers '())))
                  (make-http-request :method method
                                     :protocol-version version
                                     :uri uri
                                     :request-target target
                                     :headers headers
                                     :trailers trailers
                                     :body body)))))))))

(defun %serialize-response-content-length (headers)
  (let ((values (http-header-values headers "content-length")))
    (cond
      ((null values) nil)
      ((not (every #'%decimal-string-p values))
       (error 'http-invalid-header
              :message "Content-Length must be an ASCII decimal integer."
              :operation :serialization
              :name "content-length"
              :reason :value))
      ((not (every (lambda (value)
                     (= (%parse-decimal value)
                        (%parse-decimal (first values))))
                   values))
       (error 'http-invalid-header
              :message "Duplicate Content-Length values must agree."
              :operation :serialization
              :name "content-length"
              :reason :duplicate))
      (t (%parse-decimal (first values))))))

(defun %serialize-response-transfer-mode (headers)
  (let ((values (http-header-values headers "transfer-encoding")))
    (when values
      (let ((codings '()))
        (dolist (value values)
          (let ((start 0))
            (loop
              for comma = (position #\, value :start start)
              for end = (or comma (length value))
              for coding = (%trim-ows (subseq value start end))
              do (when (zerop (length coding))
                   (error 'http-invalid-header
                          :message "A comma-separated Transfer-Encoding contains an empty item."
                          :operation :serialization
                          :name "transfer-encoding"
                          :reason :empty-item))
                 (push (string-downcase coding) codings)
                 (if comma
                     (setf start (1+ comma))
                     (return)))))
        (setf codings (nreverse codings))
        (unless (and (consp codings)
                     (null (cdr codings))
                     (string= (first codings) "chunked"))
          (error 'http-unsupported-feature
                 :message "Only a single HTTP/1.1 chunked transfer coding is supported."
                 :operation :serialization
                 :feature :http1-response-transfer-encoding
                 :detail codings))
        :chunked))))

(defun %response-bodyless-status-p (status)
  (or (< status 200)
      (= status 204)
      (= status 205)
      (= status 304)))

(defun %write-http1-headers (builder headers)
  (dolist (header headers)
    (%builder-write-string builder (http-header-name header)
                           :context :header-name)
    (%builder-write-string builder ": ")
    (%builder-write-string builder (http-header-content header)
                           :context :header-value)
    (%builder-crlf builder)))

(defun %write-http1-trailers (builder trailers)
  (dolist (trailer trailers)
    (%builder-write-string builder (http-header-name trailer)
                           :context :header-name)
    (%builder-write-string builder ": ")
    (%builder-write-string builder (http-header-content trailer)
                           :context :header-value)
    (%builder-crlf builder)))

(defun serialize-http-response (response &key request-method head-p)
  "Serialize one HTTP/1.0 or HTTP/1.1 response into an octet vector.

REQUEST-METHOD controls HEAD and successful CONNECT body semantics.
HEAD responses may carry a representation body in RESPONSE, but that
body is never written to the wire."
  (check-type response http-response)
  (unless (member head-p '(nil t))
    (error 'http-protocol-error
           :message "HEAD-P must be NIL or T."
           :operation :serialization
           :detail head-p))
  (when (and request-method (not (stringp request-method)))
    (error 'http-protocol-error
           :message "REQUEST-METHOD must be a string or NIL."
           :operation :serialization
           :detail request-method))
  (let* ((protocol-version (http-response-protocol-version response))
         (status (http-response-status response))
         (reason (http-response-reason response))
         (headers (http-response-headers response))
         (trailers (http-response-trailers response))
         (body (http-response-body response))
         (head-response-p (or head-p
                              (and request-method
                                   (string-equal request-method "HEAD"))))
         (connect-response-p (and request-method
                                  (string-equal request-method "CONNECT")))
         (status-bodyless-p (%response-bodyless-status-p status))
         (connect-bodyless-p (and connect-response-p
                                  (<= 200 status 299)))
         (content-length (%serialize-response-content-length headers))
         (transfer-mode (%serialize-response-transfer-mode headers)))
    (unless (member protocol-version '("HTTP/1.0" "HTTP/1.1")
                    :test #'string=)
      (error 'http-unsupported-feature
             :message "Only HTTP/1.0 and HTTP/1.1 responses can be serialized on this boundary."
             :operation :serialization
             :feature :http1-response-version
             :detail protocol-version))
    (when (and content-length transfer-mode)
      (error 'http-invalid-header
             :message "Transfer-Encoding and Content-Length must not be combined."
             :operation :serialization
             :name "content-length"
             :reason :ambiguous-framing))
    (when (and transfer-mode
               (or status-bodyless-p connect-bodyless-p))
      (error 'http-invalid-header
             :message "A bodyless HTTP response cannot declare Transfer-Encoding."
             :operation :serialization
             :name "transfer-encoding"
             :reason :forbidden))
    (when (and (or status-bodyless-p connect-bodyless-p)
               (plusp (length body)))
      (error 'http-protocol-error
             :message "A bodyless HTTP response cannot carry a response body."
             :operation :serialization
             :detail status))
    (when (and (or trailers
                   (http-header-values headers "trailer"))
               (or head-response-p status-bodyless-p connect-bodyless-p))
      (error 'http-protocol-error
             :message "HTTP trailers cannot be sent without a response body."
             :operation :serialization
             :detail status))
    (%validate-http1-trailers trailers :serialization)
    (cond
      ((and content-length (= status 304))
       ;; A 304 Content-Length describes the selected representation, not
       ;; bytes in this response, so it need not equal BODY's length.
       nil)
      ((or status-bodyless-p connect-bodyless-p)
       (when (and content-length (plusp content-length))
         (error 'http-invalid-header
                :message "This bodyless HTTP response may only declare Content-Length: 0."
                :operation :serialization
                :name "content-length"
                :reason :forbidden)))
      ((and content-length (/= content-length (length body)))
       (error 'http-invalid-header
              :message "Content-Length does not match the response representation body."
              :operation :serialization
              :name "content-length"
              :reason :mismatch)))
    (when (and (or trailers
                   (http-header-values headers "trailer"))
               content-length)
      (error 'http-invalid-header
             :message "Response trailers require chunked transfer encoding, not Content-Length."
             :operation :serialization
             :name "content-length"
             :reason :framing))
    (when (and (null transfer-mode)
               (or trailers
                   (http-header-values headers "trailer")))
      (setf transfer-mode :chunked
            headers (append headers
                            (list (make-http-header
                                   "Transfer-Encoding" "chunked")))))
    (when (and (string= protocol-version "HTTP/1.0") transfer-mode)
      (error 'http-unsupported-feature
             :message "HTTP/1.0 transfer codings and trailers are unsupported."
             :operation :serialization
             :feature :http1-response-transfer-encoding
             :detail transfer-mode))
    (setf headers
          (%http1-ensure-trailer-declaration
           headers trailers transfer-mode :serialization))
    (when (and (null transfer-mode)
               (null content-length)
               (null (http-header-values headers "trailer"))
               (or (= status 205)
                   (not (or status-bodyless-p connect-bodyless-p))))
      (let ((wire-length (if (= status 205) 0 (length body))))
        (setf content-length wire-length
              headers (append headers
                              (list (make-http-header
                                     "Content-Length"
                                     (princ-to-string wire-length)))))))
    (let ((builder (%make-byte-builder)))
      (%builder-write-string builder protocol-version :context :protocol-version)
      (%builder-write-string builder " ")
      (%builder-write-string builder (princ-to-string status) :context :status)
      (%builder-write-string builder " ")
      (%builder-write-string builder reason :context :reason)
      (%builder-crlf builder)
      (%write-http1-headers builder headers)
      (%builder-crlf builder)
      (unless (or head-response-p status-bodyless-p connect-bodyless-p)
        (if (eq transfer-mode :chunked)
            (progn
              (unless (zerop (length body))
                (%builder-write-string builder (format nil "~X" (length body)))
                (%builder-crlf builder)
                (%builder-write-octets builder body)
                (%builder-crlf builder))
              (%builder-write-string builder "0")
              (%builder-crlf builder)
              (%write-http1-trailers builder trailers)
              (%builder-crlf builder))
            (%builder-write-octets builder body)))
      (let ((result (make-array (length builder)
                                :element-type '(unsigned-byte 8))))
        (replace result builder)
        result))))

(defun %http1-session-error (message detail)
  (error 'http-protocol-error
         :message message
         :operation :session
         :detail detail))

(defun %http1-session-write-continue (stream request)
  (write-sequence
   (serialize-http-response
    (make-http-response
     :protocol-version (http-request-protocol-version request)
     :status 100)
    :request-method (http-request-method request))
   stream)
  (finish-output stream))

(defun %http1-session-header-token-p (headers name token)
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

(defun %http1-session-response-reusable-p (request response)
  (let* ((request-headers (http-request-headers request))
         (response-headers (http-response-headers response))
         (method (http-request-method request))
         (status (http-response-status response))
         (protocol-version (http-request-protocol-version request))
         (http10-p (string= protocol-version "HTTP/1.0"))
         (http11-p (string= protocol-version "HTTP/1.1")))
    (and (or http10-p http11-p)
         (not (%http1-session-header-token-p request-headers
                                             "Connection"
                                             "close"))
         (not (%http1-session-header-token-p response-headers
                                             "Connection"
                                             "close"))
         (or http11-p
             (%http1-session-header-token-p response-headers
                                             "Connection"
                                             "keep-alive"))
         (not (= status 101))
         (not (and (string-equal method "CONNECT")
                   (and (>= status 200) (< status 300)))))))

(defun %http1-session-response-for-request (request response)
  (let ((protocol-version (http-request-protocol-version request)))
    (if (string= protocol-version
                 (http-response-protocol-version response))
        response
        (make-http-response
         :protocol-version protocol-version
         :status (http-response-status response)
         :reason (http-response-reason response)
         :headers (http-response-headers response)
         :trailers (http-response-trailers response)
         :body (http-response-body response)))))

(defun %http1-session-response-stream-for-request (request response)
  (let ((protocol-version (http-request-protocol-version request)))
    (if (string= protocol-version
                 (http-response-stream-protocol-version response))
        response
        (make-http-response-stream
         :protocol-version protocol-version
         :status (http-response-stream-status response)
         :reason (http-response-stream-reason response)
         :headers (http-response-stream-headers response)
         :trailers (http-response-stream-trailers response)
         :body-function (http-response-stream-body-function response)
         :body-length (http-response-stream-body-length response)))))

(defun %write-http1-response-stream-crlf (stream)
  (write-byte 13 stream)
  (write-byte 10 stream))

(defun %write-http1-response-stream-header (stream header)
  (write-sequence (%string-octets (http-header-name header)
                                  :context :header-name)
                  stream)
  (write-byte 58 stream)
  (write-byte 32 stream)
  (write-sequence (%string-octets (http-header-content header)
                                  :context :header-value)
                  stream)
  (%write-http1-response-stream-crlf stream))

(defun %write-http1-response-stream-head
    (stream protocol-version status reason headers)
  (write-sequence (%string-octets protocol-version
                                  :context :protocol-version)
                  stream)
  (write-byte 32 stream)
  (write-sequence (%string-octets (princ-to-string status)
                                  :context :status)
                  stream)
  (write-byte 32 stream)
  (write-sequence (%string-octets reason :context :reason) stream)
  (%write-http1-response-stream-crlf stream)
  (dolist (header headers)
    (%write-http1-response-stream-header stream header))
  (%write-http1-response-stream-crlf stream))

(defun %write-http1-response-stream-chunk
    (stream chunk transfer-mode)
  (unless (zerop (length chunk))
    (when (eq transfer-mode :chunked)
      (write-sequence (%string-octets (format nil "~X" (length chunk)))
                      stream)
      (%write-http1-response-stream-crlf stream))
    (write-sequence chunk stream)
    (when (eq transfer-mode :chunked)
      (%write-http1-response-stream-crlf stream))))

(defun %write-http1-response-stream-final-chunk (stream trailers)
  (write-byte 48 stream)
  (%write-http1-response-stream-crlf stream)
  (dolist (trailer trailers)
    (%write-http1-response-stream-header stream trailer))
  (%write-http1-response-stream-crlf stream))

(defun %write-http1-response-stream (stream request response)
  (let* ((protocol-version (http-response-stream-protocol-version response))
         (status (http-response-stream-status response))
         (reason (http-response-stream-reason response))
         (headers (http-response-stream-headers response))
         (trailers (http-response-stream-trailers response))
         (body-function (http-response-stream-body-function response))
         (body-length (http-response-stream-body-length response))
         (request-method (http-request-method request))
         (head-response-p (string-equal request-method "HEAD"))
         (connect-response-p (string-equal request-method "CONNECT"))
         (status-bodyless-p (%response-bodyless-status-p status))
         (connect-bodyless-p (and connect-response-p
                                  (<= 200 status 299)))
         (content-length (%serialize-response-content-length headers))
         (transfer-mode (%serialize-response-transfer-mode headers)))
    (unless (member protocol-version '("HTTP/1.0" "HTTP/1.1")
                    :test #'string=)
      (error 'http-unsupported-feature
             :message "Only HTTP/1.0 and HTTP/1.1 response streams can be sent on this boundary."
             :operation :serialization
             :feature :http1-response-version
             :detail protocol-version))
    (when (and content-length transfer-mode)
      (error 'http-invalid-header
             :message "Transfer-Encoding and Content-Length must not be combined."
             :operation :serialization
             :name "content-length"
             :reason :ambiguous-framing))
    (when (and transfer-mode
               (or status-bodyless-p connect-bodyless-p))
      (error 'http-invalid-header
             :message "A bodyless HTTP response cannot declare Transfer-Encoding."
             :operation :serialization
             :name "transfer-encoding"
             :reason :forbidden))
    (when (and (or status-bodyless-p connect-bodyless-p)
               body-length
               (plusp body-length))
      (error 'http-protocol-error
             :message "A bodyless HTTP response cannot declare a non-empty streamed body."
             :operation :serialization
             :detail status))
    (when (and (or trailers
                   (http-header-values headers "trailer"))
               (or head-response-p status-bodyless-p connect-bodyless-p))
      (error 'http-protocol-error
             :message "HTTP trailers cannot be sent without a response body."
             :operation :serialization
             :detail status))
    (%validate-http1-trailers trailers :serialization)
    (cond
      ((or status-bodyless-p connect-bodyless-p)
       (when (and content-length (plusp content-length))
         (error 'http-invalid-header
                :message "This bodyless HTTP response may only declare Content-Length: 0."
                :operation :serialization
                :name "content-length"
                :reason :forbidden)))
      ((and content-length body-length (/= status 304)
            (/= content-length body-length))
       (error 'http-invalid-header
              :message "Content-Length does not match the streamed response length."
              :operation :serialization
              :name "content-length"
              :reason :mismatch)))
    (when (and (or trailers
                   (http-header-values headers "trailer"))
               content-length)
      (error 'http-invalid-header
             :message "Response trailers require chunked transfer encoding, not Content-Length."
             :operation :serialization
             :name "content-length"
             :reason :framing))
    (when (and (null transfer-mode)
               (or trailers
                   (http-header-values headers "trailer")))
      (setf transfer-mode :chunked
            headers (append headers
                            (list (make-http-header
                                   "Transfer-Encoding" "chunked")))))
    (when (and (string= protocol-version "HTTP/1.0") transfer-mode)
      (error 'http-unsupported-feature
             :message "HTTP/1.0 transfer codings and trailers are unsupported."
             :operation :serialization
             :feature :http1-response-transfer-encoding
             :detail transfer-mode))
    (setf headers
          (%http1-ensure-trailer-declaration
           headers trailers transfer-mode :serialization))
    (when (and (null transfer-mode)
               (null content-length))
      (cond
        ((= status 205)
         (setf content-length 0
               headers (append headers
                               (list (make-http-header
                                      "Content-Length" "0")))))
        ((or status-bodyless-p connect-bodyless-p)
         nil)
        (body-length
         (setf content-length body-length
               headers (append headers
                               (list (make-http-header
                                      "Content-Length"
                                      (princ-to-string body-length))))))
        ((and (string= protocol-version "HTTP/1.1")
              (not head-response-p))
         (setf transfer-mode :chunked
               headers (append headers
                               (list (make-http-header
                                      "Transfer-Encoding" "chunked")))))))
    (let* ((wire-response
             (make-http-response
              :protocol-version protocol-version
              :status status
              :reason reason
              :headers headers
              :trailers trailers))
           (bodyless-p (or head-response-p
                           status-bodyless-p
                           connect-bodyless-p))
           (framed-p (or bodyless-p content-length transfer-mode))
           (produced 0))
      (%write-http1-response-stream-head
       stream protocol-version status reason headers)
      (unless bodyless-p
        (loop
          for chunk = (funcall body-function)
          do (if (null chunk)
                 (return)
                 (let ((octets (%copy-octets chunk :allow-list nil)))
                   (incf produced (length octets))
                   (when (and body-length (> produced body-length))
                     (error 'http-protocol-error
                            :message "The streamed response body exceeded body-length."
                            :operation :serialization
                            :detail body-length))
                   (when (and content-length (> produced content-length))
                     (error 'http-invalid-header
                            :message "The streamed response body exceeded Content-Length."
                            :operation :serialization
                            :name "content-length"
                            :reason :mismatch))
                   (%write-http1-response-stream-chunk
                    stream octets transfer-mode))))
        (when (and body-length (/= produced body-length))
          (error 'http-protocol-error
                 :message "The streamed response body ended before body-length."
                 :operation :serialization
                 :detail (list :expected body-length :observed produced)))
        (when (and content-length (/= produced content-length))
          (error 'http-invalid-header
                 :message "Content-Length does not match the streamed response body."
                 :operation :serialization
                 :name "content-length"
                 :reason :mismatch))
        (when (eq transfer-mode :chunked)
          (%write-http1-response-stream-final-chunk stream trailers)))
      (values wire-response
              (and framed-p
                   (%http1-session-response-reusable-p request wire-response))))))

(defun serve-http1-session
    (stream handler &key timeout deadline max-header-bytes max-body-bytes
             default-authority on-body-chunk (collect-body-p t)
             (on-expect-continue :automatic)
             max-requests on-error on-upgrade (close-stream #'close)
             (clock-function #'%monotonic-time))
  "Serve HTTP/1 requests from STREAM until EOF, close, or a request limit.

  HANDLER receives each parsed request and must return an HTTP response or
  HTTP response stream.  A response stream calls its body function until NIL;
  known-length streams use Content-Length and unknown-length HTTP/1.1 streams
  use chunked transfer coding.  The return values are the number of responses written and a termination reason,
one of :EOF, :CLOSE, :UPGRADE, or :MAX-REQUESTS.  A supplied ON-ERROR
function receives the condition and the current request before the condition
is re-signaled.  When a response switches protocols, ON-UPGRADE receives the
stream, request, and wire response.  A non-NIL return value transfers stream
ownership to the callback and prevents CLOSE-STREAM from being called.
ON-EXPECT-CONTINUE defaults to :AUTOMATIC and writes a 100 Continue response
before an expected request body is read.  NIL disables that response; a
function receives the metadata-only request and can write its own interim
response."
  (unless (streamp stream)
    (%http1-session-error "HTTP/1 session stream must be a stream."
                          (type-of stream)))
  (unless (functionp handler)
    (%http1-session-error "HTTP/1 session handler must be a function."
                          (type-of handler)))
  (unless (or (null max-requests)
              (and (integerp max-requests)
                   (>= max-requests 0)))
    (%http1-session-error "HTTP/1 session max-requests must be a non-negative integer or NIL."
                          max-requests))
  (unless (or (null on-error) (functionp on-error))
    (%http1-session-error "HTTP/1 session on-error must be a function or NIL."
                          (type-of on-error)))
  (unless (or (null on-upgrade) (functionp on-upgrade))
    (%http1-session-error "HTTP/1 session on-upgrade must be a function or NIL."
                          (type-of on-upgrade)))
  (unless (or (eq on-expect-continue :automatic)
              (null on-expect-continue)
              (functionp on-expect-continue))
    (%http1-session-error
     "HTTP/1 session on-expect-continue must be :AUTOMATIC, a function, or NIL."
     (type-of on-expect-continue)))
  (unless (or (null close-stream) (functionp close-stream))
    (%http1-session-error "HTTP/1 session close-stream must be a function or NIL."
                          (type-of close-stream)))
  (let* ((expect-continue-callback
           (cond
             ((eq on-expect-continue :automatic)
              (lambda (request)
                (%http1-session-write-continue stream request)))
             ((null on-expect-continue) nil)
             (t on-expect-continue)))
         (absolute-deadline
           (http-deadline timeout
                          :deadline deadline
                          :clock-function clock-function))
         (count 0)
         (termination :running)
         (handed-off-p nil))
    (unwind-protect
         (progn
           (loop while (eq termination :running)
                 do (if (and max-requests
                             (>= count max-requests))
                        (setf termination :max-requests)
                        (let ((request nil))
                          (handler-case
                              (let ((parsed
                                      (parse-http-request
                                       stream
                                       :deadline absolute-deadline
                                       :max-header-bytes max-header-bytes
                                       :max-body-bytes max-body-bytes
                                       :default-authority default-authority
                                       :on-body-chunk on-body-chunk
                                       :collect-body-p collect-body-p
                                       :on-expect-continue expect-continue-callback
                                       :clock-function clock-function
                                       :allow-eof-p t)))
                                (if (null parsed)
                                    (setf termination :eof)
                                    (progn
                                      (setf request parsed)
                                      (let* ((response (funcall handler request))
                                             (stream-response-p
                                               (http-response-stream-p response)))
                                        (unless (or (http-response-p response)
                                                    stream-response-p)
                                          (%http1-session-error
                                           "HTTP/1 session handler must return an HTTP response or response stream."
                                           (type-of response)))
                                        (let ((status (if stream-response-p
                                                          (http-response-stream-status response)
                                                          (http-response-status response))))
                                          (when (and (>= status 100)
                                                     (< status 200)
                                                     (/= status 101))
                                            (%http1-session-error
                                             "HTTP/1 session handlers cannot return interim responses."
                                             status))
                                          (if stream-response-p
                                              (multiple-value-bind (wire-response reusable-p)
                                                  (%write-http1-response-stream
                                                   stream
                                                   request
                                                   (%http1-session-response-stream-for-request
                                                    request response))
                                                (finish-output stream)
                                                (incf count)
                                                (if (or (= (http-response-status wire-response)
                                                           101)
                                                        (and (string-equal
                                                              (http-request-method request)
                                                              "CONNECT")
                                                             (>= (http-response-status
                                                                  wire-response)
                                                                 200)
                                                             (< (http-response-status
                                                                 wire-response)
                                                                300)))
                                                    (progn
                                                      (when on-upgrade
                                                        (setf handed-off-p
                                                              (not (null
                                                                    (funcall on-upgrade
                                                                             stream
                                                                             request
                                                                             wire-response)))))
                                                      (setf termination :upgrade))
                                                    (unless reusable-p
                                                      (setf termination :close))))
                                              (let* ((wire-response
                                                       (%http1-session-response-for-request
                                                        request response))
                                                     (wire
                                                       (serialize-http-response
                                                        wire-response
                                                        :request-method
                                                        (http-request-method request))))
                                                (write-sequence wire stream)
                                                (finish-output stream)
                                                (incf count)
                                                (if (or (= (http-response-status wire-response)
                                                           101)
                                                        (and (string-equal
                                                              (http-request-method request)
                                                              "CONNECT")
                                                             (>= (http-response-status
                                                                  wire-response)
                                                                 200)
                                                             (< (http-response-status
                                                                 wire-response)
                                                                300)))
                                                    (progn
                                                      (when on-upgrade
                                                        (setf handed-off-p
                                                              (not (null
                                                                    (funcall on-upgrade
                                                                             stream
                                                                             request
                                                                             wire-response)))))
                                                      (setf termination :upgrade))
                                                    (unless (%http1-session-response-reusable-p
                                                             request wire-response)
                                                      (setf termination :close))))))))))
                            (error (caught-condition)
                              (when on-error
                                (funcall on-error caught-condition request))
                              (error caught-condition))))))
           (values count termination))
      (when (and close-stream (not handed-off-p))
        (funcall close-stream stream)))))
