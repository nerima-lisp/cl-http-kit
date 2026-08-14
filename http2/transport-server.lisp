(in-package #:http-kit/http2)

;;;; HTTP/2 server session
;;;;
;;;; This is deliberately a stream-oriented event loop.  The network
;;;; transport is supplied by the caller, while this file owns the HTTP/2
;;;; connection preface, stream state, HPACK, flow control, and response
;;;; framing.

(defstruct (%h2-server-stream
            (:constructor %make-h2-server-stream
                (id &key request method scheme authority target headers
                    (body (make-array 0 :element-type '(unsigned-byte 8)
                                       :adjustable t :fill-pointer 0))
                    expected-body-length (body-length-seen 0) trailers
                    headers-complete-p end-stream-p reset-p responded-p
                    (send-window +http2-default-window-size+)
                    (receive-window +http2-default-window-size+))))
  id
  request
  method
  scheme
  authority
  target
  headers
  body
  expected-body-length
  body-length-seen
  trailers
  headers-complete-p
  end-stream-p
  reset-p
  responded-p
  send-window
  receive-window)

(defun %h2-server-error (message detail)
  (error 'http-kit:http-protocol-error
         :message message
         :operation :http2-server
         :detail detail))

(defun %h2-server-invalid-header (name reason)
  (error 'http-kit:http-invalid-header
         :name name
         :reason reason
         :operation :http2-server
         :message "Invalid HTTP/2 header"))

(defun %h2-server-invalid-stream-id (stream-id)
  (unless (and (integerp stream-id)
               (plusp stream-id)
               (oddp stream-id))
    (%h2-server-error "HTTP/2 request stream must be an odd positive ID"
                      stream-id)))

(defun %h2-server-append-body (state octets collect-body-p)
  (when collect-body-p
    (loop for octet across octets
          do (vector-push-extend octet (%h2-server-stream-body state))))
  state)

(defun %h2-server-octets (value)
  (unless (and (arrayp value)
               (= (array-rank value) 1)
               (not (stringp value)))
    (error 'http-kit:http-protocol-error
           :message "HTTP/2 body chunks must be one-dimensional octet arrays"
           :operation :http2-server
           :detail value))
  (handler-case
      (http-kit::%copy-octets value)
    (error ()
      (error 'http-kit:http-protocol-error
             :message "HTTP/2 body chunks must contain octets"
             :operation :http2-server
             :detail value))))

(defun %h2-server-header-fields (fields default-authority)
  "Parse a decoded request header block.

Returns METHOD, SCHEME, AUTHORITY, TARGET, and regular HEADERS.  Request
pseudo-fields are kept separate because they are not ordinary HTTP headers.
"
  (let ((method nil)
        (scheme nil)
        (authority nil)
        (path nil)
        (regular '())
        (seen-pseudo (make-hash-table :test #'equal))
        (regular-seen-p nil))
    (dolist (field fields)
      (unless (and (consp field)
                   (stringp (car field))
                   (stringp (cdr field)))
        (%h2-server-error "Decoded HPACK field is not a name/value pair"
                          field))
      (let ((name (car field))
            (value (cdr field)))
        (if (and (string/= name "")
                 (char= (char name 0) #\:))
            (progn
              (when regular-seen-p
                (%h2-server-invalid-header name
                                           "Pseudo-fields must precede regular fields"))
              (when (gethash name seen-pseudo)
                (%h2-server-invalid-header name "Duplicate pseudo-field"))
              (setf (gethash name seen-pseudo) t)
              (cond
                ((string= name ":method") (setf method value))
                ((string= name ":scheme") (setf scheme value))
                ((string= name ":authority") (setf authority value))
                ((string= name ":path") (setf path value))
                ((string= name ":protocol")
                 (error 'http-kit:http-unsupported-feature
                        :feature :http2-extended-connect
                        :operation :http2-server
                        :message "Extended CONNECT is not implemented"))
                (t (%h2-server-invalid-header name "Unknown request pseudo-field"))))
            (progn
              (setf regular-seen-p t)
              (unless (%h2-regular-header-valid-p name value)
                (%h2-server-invalid-header name "Forbidden HTTP/2 header field"))
              (push (http-kit:make-http-header name value) regular)))))
    (setf regular (nreverse regular))
    (unless (and method (string/= method "")
                 (http-kit::%token-p method))
      (%h2-server-invalid-header ":method" "Missing or invalid method"))
    (setf authority (or authority default-authority))
    (unless (and (stringp authority) (string/= authority ""))
      (%h2-server-invalid-header ":authority" "Missing authority"))
    (let* ((connect-p (string-equal method "CONNECT"))
           (target
             (cond
               (connect-p
                (when (or scheme path)
                  (%h2-server-invalid-header ":path"
                                             "CONNECT must use authority-form"))
                authority)
               (t
                (unless (and scheme (string/= scheme ""))
                  (%h2-server-invalid-header ":scheme" "Missing scheme"))
                (unless (and path (string/= path ""))
                  (%h2-server-invalid-header ":path" "Missing path"))
                (unless (or (string= path "*")
                            (char= (char path 0) #\/))
                  (%h2-server-invalid-header ":path" "Path must be origin-form"))
                path))))
      (handler-case
          (http-kit::%authority-parts authority authority)
        (http-kit:http-invalid-uri ()
          (%h2-server-invalid-header ":authority" "Invalid authority")))
      (when (and (not connect-p)
                 (not (member (string-downcase scheme)
                              '("http" "https") :test #'string=)))
        (error 'http-kit:http-unsupported-feature
               :feature :http2-scheme
               :operation :http2-server
               :message "Only HTTP and HTTPS URI schemes are supported"))
      (let ((host-values (http-kit:http-header-values regular "host")))
        (when host-values
          (%h2-validate-host-values host-values authority)))
      (values method
              (if connect-p "http" (string-downcase scheme))
              authority
              target
              regular))))

(defun %h2-server-make-request (state collect-body-p)
  (let* ((target (%h2-server-stream-target state))
         (connect-p (string-equal (%h2-server-stream-method state) "CONNECT"))
         (path (if connect-p "/" target))
         (query-start (and (not connect-p) (position #\? path)))
         (uri-path (if query-start (subseq path 0 query-start) path))
         (query (and query-start (subseq path (1+ query-start))))
         (uri (http-kit:make-http-uri
               :scheme (%h2-server-stream-scheme state)
               :authority (%h2-server-stream-authority state)
               :path (if (string= uri-path "*") "/" uri-path)
               :query query)))
    (http-kit:make-http-request
     :method (%h2-server-stream-method state)
     :uri uri
     :request-target target
     :protocol-version "HTTP/2"
     :headers (%h2-server-stream-headers state)
     :trailers (%h2-server-stream-trailers state)
     :body (if collect-body-p
               (http-kit::%copy-octets (%h2-server-stream-body state))
               (http-kit::%empty-octets)))))

(defun %h2-server-window-add (current increment kind)
  (unless (and (integerp increment) (plusp increment))
    (%h2-server-error "HTTP/2 WINDOW_UPDATE increment must be positive"
                      (list kind increment)))
  (let ((next (+ current increment)))
    (when (> next #x7fffffff)
      (%h2-server-error "HTTP/2 flow-control window overflow"
                        (list kind current increment)))
    next))

(defun %h2-server-frame-flags-valid-p (frame mask)
  (= (logand (%h2-frame-flags frame) (lognot mask)) 0))

(defun %h2-server-materialize-response (response request)
  (let ((status nil)
        (headers nil)
        (trailers nil)
        (body (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0))
        (body-length nil))
    (cond
      ((http-kit:http-response-p response)
       (setf status (http-kit:http-response-status response)
             headers (http-kit:http-response-headers response)
             trailers (http-kit:http-response-trailers response))
       (let ((value (%h2-server-octets (http-kit:http-response-body response))))
         (setf body-length (length value))
         (loop for octet across value do (vector-push-extend octet body))))
      ((http-kit:http-response-stream-p response)
        (setf status (http-kit:http-response-stream-status response)
             headers (http-kit:http-response-stream-headers response)
             trailers (http-kit:http-response-stream-trailers response)
             body-length (http-kit:http-response-stream-body-length response))
       (let ((body-function (http-kit:http-response-stream-body-function response)))
         (loop for chunk = (funcall body-function)
               while chunk
               do (let ((value (%h2-server-octets chunk)))
                    (loop for octet across value do (vector-push-extend octet body))))))
      (t
       (error 'http-kit:http-protocol-error
              :message "HTTP/2 handler must return an HTTP response"
              :operation :http2-server
              :detail response)))
    (unless (and (integerp status) (<= 100 status 999))
      (error 'http-kit:http-invalid-status
             :code status
             :operation :http2-server
             :message "Invalid HTTP/2 response status"))
    (when (or (< status 200) (= status 101))
      (error 'http-kit:http-unsupported-feature
             :feature :http2-informational-response
             :operation :http2-server
             :message "A single HTTP/2 response must be final"))
    (let* ((actual-body-length (length body))
           (method (http-kit:http-request-method request))
           (no-body (or (string-equal method "HEAD")
                        (= status 204)
                        (= status 205)
                        (= status 304)
                        (<= 100 status 199))))
      (multiple-value-bind (validated)
          (%h2-finish-response status headers trailers body
                               :no-body no-body
                               :body-length actual-body-length)
        (declare (ignore validated)))
      (when (and body-length (/= body-length actual-body-length))
        (%h2-server-error "HTTP/2 response stream length did not match its body"
                          (list body-length actual-body-length)))
      (values status headers trailers body actual-body-length no-body))))
