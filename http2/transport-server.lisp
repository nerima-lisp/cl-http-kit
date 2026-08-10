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
        (if (and (plusp (length name))
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
    (unless (and method (plusp (length method))
                 (http-kit::%token-p method))
      (%h2-server-invalid-header ":method" "Missing or invalid method"))
    (setf authority (or authority default-authority))
    (unless (and (stringp authority) (plusp (length authority)))
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
                (unless (and scheme (plusp (length scheme)))
                  (%h2-server-invalid-header ":scheme" "Missing scheme"))
                (unless (and path (plusp (length path)))
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

(defun serve-http2-session
    (stream handler &key timeout deadline
             (max-frame-size +http2-default-max-frame-size+)
             (max-header-bytes http-kit::*default-max-header-bytes*)
             (max-body-bytes http-kit::*default-max-body-bytes*)
             default-authority
             (collect-body-p t)
             on-body-chunk
             max-requests
             on-error
             (close-stream #'close)
             (clock-function #'http-kit::%monotonic-time)
             (huffman-p nil))
  "Serve one HTTP/2 connection on STREAM.

The caller owns the network transport and supplies HANDLER.  This function
owns the HTTP/2 connection preface, SETTINGS exchange, stream state, HPACK,
flow control, request body collection, HPACK encoding, and response framing.
It returns the number of completed requests and the termination reason as two
values.
"
  (unless (streamp stream)
    (error 'http-kit:http-protocol-error
           :message "HTTP/2 server stream must be a Common Lisp stream"
           :operation :http2-server
           :detail stream))
  (unless (functionp handler)
    (error 'http-kit:http-protocol-error
           :message "HTTP/2 server handler must be a function"
           :operation :http2-server
           :detail handler))
  (unless (or (null close-stream) (functionp close-stream))
    (error 'http-kit:http-protocol-error
           :message "HTTP/2 close-stream must be NIL or a function"
           :operation :http2-server
           :detail close-stream))
  (unless (or (null on-error) (functionp on-error))
    (error 'http-kit:http-protocol-error
           :message "HTTP/2 on-error must be NIL or a function"
           :operation :http2-server
           :detail on-error))
  (unless (or (null on-body-chunk) (functionp on-body-chunk))
    (error 'http-kit:http-protocol-error
           :message "HTTP/2 on-body-chunk must be NIL or a function"
           :operation :http2-server
           :detail on-body-chunk))
  (unless (or (null default-authority) (stringp default-authority))
    (error 'http-kit:http-protocol-error
           :message "HTTP/2 default-authority must be NIL or a string"
           :operation :http2-server
           :detail default-authority))
  (unless (or (null max-requests)
              (and (integerp max-requests) (not (minusp max-requests))))
    (error 'http-kit:http-protocol-error
           :message "HTTP/2 max-requests must be NIL or a non-negative integer"
           :operation :http2-server
           :detail max-requests))
  (unless (member collect-body-p '(nil t))
    (error 'http-kit:http-protocol-error
           :message "HTTP/2 collect-body-p must be boolean"
           :operation :http2-server
           :detail collect-body-p))
  (%h2-validate-frame-size max-frame-size)
  (%h2-validate-limit :max-header-bytes max-header-bytes)
  (%h2-validate-limit :max-body-bytes max-body-bytes :allow-zero t)
  (let ((request-count 0)
        (termination :running)
        (current-request nil))
    (unwind-protect
         (handler-case
             (http-kit:with-http-deadline
                 (absolute-deadline timeout
                                    :inherited deadline
                                    :clock-function clock-function
                                    :kind :http2-server)
               (let* ((reader (%h2-reader-for stream))
                      (writer
                        (lambda (wire)
                          (%h2-write-wire stream wire absolute-deadline
                                          clock-function)))
                      (streams (make-hash-table :test #'eql))
                      (pending-frames '())
                      (last-client-stream-id 0)
                      (goaway-sent-p nil)
                      (peer-max-frame-size +http2-default-max-frame-size+)
                      (peer-initial-window-size +http2-default-window-size+)
                      (peer-connection-window +http2-default-window-size+)
                      (receive-connection-window +http2-default-window-size+)
                      (decoder-context
                        (%make-hpack-context
                         :max-size +hpack-default-table-size+
                         :maximum-size +hpack-default-table-size+)))
                 (let ((preface
                         (%h2-reader-read reader
                                         (length +http2-connection-preface+)
                                         absolute-deadline
                                         clock-function
                                         :allow-eof t)))
                   (cond
                     ((eq preface :eof)
                      (setf termination :eof)
                      (return-from serve-http2-session
                        (values request-count termination)))
                     ((not (equalp preface +http2-connection-preface+))
                      (%h2-server-error
                       "Invalid HTTP/2 client connection preface"
                       preface))))
                 (let ((first-frame
                         (%h2-read-frame reader max-frame-size
                                        absolute-deadline clock-function)))
                   (when (eq first-frame :eof)
                     (%h2-server-error
                      "HTTP/2 peer closed before sending SETTINGS" nil))
                   (unless (and (= (%h2-frame-type first-frame)
                                   +http2-settings-type+)
                                (zerop (%h2-frame-stream-id first-frame))
                                (zerop (%h2-frame-flags first-frame)))
                     (%h2-server-error
                      "HTTP/2 first frame must be a non-ACK SETTINGS frame"
                      first-frame))
                   (multiple-value-bind
                         (new-max-frame-size new-table-size
                          new-initial-window-size)
                       (%h2-settings (%h2-frame-payload first-frame))
                     (when new-max-frame-size
                       (setf peer-max-frame-size new-max-frame-size))
                     (when new-table-size
                       (%hpack-set-maximum-size decoder-context new-table-size))
                     (when new-initial-window-size
                       (setf peer-initial-window-size new-initial-window-size))))
                 (%h2-send-control writer
                                   +http2-settings-type+
                                   0
                                   0
                                   (%h2-settings-wire max-frame-size))
                 (%h2-send-control writer
                                   +http2-settings-type+
                                   +http2-ack-flag+
                                   0
                                   (http-kit::%empty-octets))
                 (labels
                     ((queue-frame (frame)
                        (setf pending-frames
                              (nconc pending-frames (list frame))))
                      (next-frame ()
                        (if pending-frames
                            (pop pending-frames)
                            (%h2-read-frame reader max-frame-size
                                            absolute-deadline clock-function)))
                     (apply-peer-settings (payload)
                          (multiple-value-bind
                                (new-max-frame-size new-table-size
                                 new-initial-window-size)
                            (%h2-settings payload)
                          (when new-max-frame-size
                            (setf peer-max-frame-size new-max-frame-size))
                          (when new-table-size
                            (%hpack-set-maximum-size decoder-context
                                                     new-table-size))
                          (when new-initial-window-size
                            (let ((delta
                                    (- new-initial-window-size
                                       peer-initial-window-size)))
                              (maphash
                               (lambda (stream-id state)
                                 (declare (ignore stream-id))
                                 (let ((next
                                         (+ (%h2-server-stream-send-window state)
                                            delta)))
                                   (unless (<= (- #x80000000) next
                                               #x7fffffff)
                                     (%h2-server-error
                                      "HTTP/2 stream send window overflow"
                                      next))
                                   (setf (%h2-server-stream-send-window state)
                                         next)))
                               streams)
                              (setf peer-initial-window-size
                                    new-initial-window-size)))))
                      (send-goaway ()
                        (unless goaway-sent-p
                          (let ((payload (make-array 8
                                                     :element-type
                                                     '(unsigned-byte 8))))
                            ;; A graceful server shutdown uses NO_ERROR and
                            ;; accepts no new streams after the last request
                            ;; already observed from the client.
                            (%h2-put-u32 payload 0 last-client-stream-id)
                            (%h2-put-u32 payload 4 0)
                            (%h2-send-control writer
                                              +http2-goaway-type+
                                              0
                                              0
                                              payload)
                            (setf goaway-sent-p t))))
                      (process-control-frame (frame &optional interested-state)
                        (block process-control-frame
                          (let ((type (%h2-frame-type frame))
                                (flags (%h2-frame-flags frame))
                                (stream-id (%h2-frame-stream-id frame))
                                (payload (%h2-frame-payload frame)))
                            (cond
                              ((= type +http2-settings-type+)
                               (unless (zerop stream-id)
                                 (%h2-server-error
                                  "HTTP/2 SETTINGS must use stream zero"
                                  stream-id))
                               (unless (%h2-server-frame-flags-valid-p
                                        frame +http2-ack-flag+)
                                 (%h2-server-error
                                  "Invalid HTTP/2 SETTINGS flags" flags))
                               (if (plusp (logand flags +http2-ack-flag+))
                                   (unless (zerop (length payload))
                                     (%h2-server-error
                                      "HTTP/2 SETTINGS ACK must have an empty payload"
                                      (length payload)))
                                   (progn
                                     (apply-peer-settings payload)
                                     (%h2-send-control writer
                                                       +http2-settings-type+
                                                       +http2-ack-flag+
                                                       0
                                                       (http-kit::%empty-octets))))
                               nil)
                              ((= type +http2-ping-type+)
                               (unless (zerop stream-id)
                                 (%h2-server-error
                                  "HTTP/2 PING must use stream zero"
                                  stream-id))
                               (unless (%h2-server-frame-flags-valid-p
                                        frame +http2-ack-flag+)
                                 (%h2-server-error
                                  "Invalid HTTP/2 PING flags" flags))
                               (unless (= (length payload) 8)
                                 (%h2-server-error
                                  "HTTP/2 PING payload must be eight octets"
                                  (length payload)))
                               (when (zerop (logand flags +http2-ack-flag+))
                                 (%h2-send-control writer
                                                   +http2-ping-type+
                                                   +http2-ack-flag+
                                                   0 payload))
                               nil)
                              ((= type +http2-window-update-type+)
                               (unless (and (zerop flags) (= (length payload) 4))
                                 (%h2-server-error
                                  "Invalid HTTP/2 WINDOW_UPDATE frame" frame))
                               (let ((increment
                                       (logand (%h2-u32 payload 0)
                                               #x7fffffff)))
                                 (when (zerop increment)
                                   (%h2-server-error
                                    "HTTP/2 WINDOW_UPDATE increment cannot be zero"
                                    stream-id))
                                 (if (zerop stream-id)
                                     (setf peer-connection-window
                                           (%h2-server-window-add
                                            peer-connection-window increment
                                            :connection))
                                     (let ((state (gethash stream-id streams)))
                                       (when state
                                         (setf (%h2-server-stream-send-window state)
                                               (%h2-server-window-add
                                                (%h2-server-stream-send-window state)
                                                increment
                                                :stream))
                                         (when (and interested-state
                                                    (eq state interested-state)
                                                    (%h2-server-stream-reset-p state))
                                           (return-from process-control-frame
                                             :reset)))))
                               nil))
                              ((= type +http2-rst-stream-type+)
                               (unless (and (plusp stream-id)
                                            (zerop flags)
                                            (= (length payload) 4))
                                 (%h2-server-error
                                  "Invalid HTTP/2 RST_STREAM frame" frame))
                               (let ((state (gethash stream-id streams)))
                                 (when state
                                   (setf (%h2-server-stream-reset-p state) t)
                                   (when (and interested-state
                                              (eq state interested-state))
                                     (return-from process-control-frame :reset))))
                               nil)
                              ((= type +http2-goaway-type+)
                               (unless (and (zerop stream-id)
                                            (zerop flags)
                                            (>= (length payload) 8))
                                 (%h2-server-error
                                  "Invalid HTTP/2 GOAWAY frame" frame))
                               (when (logbitp 31 (%h2-u32 payload 0))
                                 (%h2-server-error
                                  "HTTP/2 GOAWAY reserved bit is set" frame))
                               (setf termination :goaway)
                               :goaway)
                              ((= type +http2-priority-type+)
                               (unless (and (plusp stream-id)
                                            (zerop flags)
                                            (= (length payload) 5))
                                 (%h2-server-error
                                  "Invalid HTTP/2 PRIORITY frame" frame))
                               (when (logbitp 31 (%h2-u32 payload 0))
                                 (%h2-server-error
                                  "HTTP/2 PRIORITY dependency reserved bit is set"
                                  frame))
                               (when (= stream-id
                                        (logand (%h2-u32 payload 0)
                                                #x7fffffff))
                                 (%h2-server-error
                                  "HTTP/2 stream cannot depend on itself"
                                  stream-id))
                               nil)
                              (t nil)))))
                      (await-send-window (state)
                        (loop
                          (when (%h2-server-stream-reset-p state)
                            (return :reset))
                          (when (and (plusp peer-connection-window)
                                     (plusp (%h2-server-stream-send-window state)))
                            (return :ready))
                          (let ((frame (next-frame)))
                            (when (eq frame :eof)
                              (return :eof))
                            (if (member (%h2-frame-type frame)
                                        (list +http2-settings-type+
                                              +http2-ping-type+
                                              +http2-window-update-type+
                                              +http2-rst-stream-type+
                                              +http2-goaway-type+
                                              +http2-priority-type+))
                                (let ((result
                                        (process-control-frame frame state)))
                                  (when (member result '(:goaway :reset))
                                    (return result)))
                                (queue-frame frame)))))
                      (response-fields (status headers)
                        (let ((fields (list (cons ":status"
                                                  (princ-to-string status)))))
                          (dolist (header headers)
                            (let ((name (http-kit:http-header-name header))
                                  (value (http-kit:http-header-content header)))
                              (when (and (plusp (length name))
                                         (char= (char name 0) #\:))
                                (%h2-server-invalid-header
                                 name "Response pseudo-fields are forbidden"))
                              (unless (%h2-regular-header-valid-p name value)
                                (%h2-server-invalid-header
                                 name "Forbidden HTTP/2 response header field"))
                              (setf fields
                                    (nconc fields (list (cons name value))))))
                          fields))
                      (trailer-fields (trailers)
                        (let ((fields '()))
                          (dolist (header trailers)
                            (let ((name (http-kit:http-header-name header))
                                  (value (http-kit:http-header-content header)))
                              (unless (%h2-trailers (list (cons name value)))
                                (%h2-server-invalid-header
                                 name "Forbidden HTTP/2 trailer field"))
                              (setf fields
                                    (nconc fields (list (cons name value))))))
                          fields))
                      (send-response (state response)
                        (block send-response
                          (multiple-value-bind
                                (status headers trailers body body-length no-body)
                              (%h2-server-materialize-response response
                                                                current-request)
                            (let* ((response-header-fields
                                     (response-fields status headers))
                                 (response-trailer-fields
                                   (trailer-fields trailers))
                                 (has-trailers
                                   (not (null response-trailer-fields)))
                                 (header-block
                                   (%hpack-encode-block
                                    response-header-fields
                                    :huffman-p huffman-p))
                                 (header-frames
                                   (%h2-header-frames
                                    header-block
                                    (or no-body
                                        (and (zerop body-length)
                                             (not has-trailers)))
                                    peer-max-frame-size
                                    (%h2-server-stream-id state))))
                            (dolist (wire header-frames)
                              (funcall writer wire))
                            (unless no-body
                              (let ((offset 0))
                                (loop while (< offset body-length)
                                      do (let ((window-result
                                                 (await-send-window state)))
                                           (case window-result
                                             (:ready
                                              (let ((chunk-length
                                                      (min (- body-length offset)
                                                           peer-max-frame-size
                                                           peer-connection-window
                                                           (%h2-server-stream-send-window
                                                            state))))
                                                (when (zerop chunk-length)
                                                  (%h2-server-error
                                                   "HTTP/2 response body made no flow-control progress"
                                                   state))
                                                (let ((chunk
                                                        (make-array chunk-length
                                                                    :element-type
                                                                    '(unsigned-byte 8))))
                                                  (replace chunk body
                                                           :start2 offset
                                                           :end2 (+ offset
                                                                    chunk-length))
                                                  (funcall
                                                   writer
                                                   (%h2-frame-wire
                                                    +http2-data-type+
                                                    (if (= (+ offset chunk-length)
                                                           body-length)
                                                        (if has-trailers
                                                            0
                                                            +http2-end-stream-flag+)
                                                        0)
                                                    (%h2-server-stream-id state)
                                                    chunk))
                                                  (incf offset chunk-length)
                                                  (decf peer-connection-window
                                                        chunk-length)
                                                  (decf
                                                   (%h2-server-stream-send-window
                                                    state)
                                                   chunk-length))))
                                             (:reset
                                              (return-from send-response :reset))
                                             (:goaway
                                              (return-from send-response :goaway))
                                             (:eof
                                              (%h2-server-error
                                               "HTTP/2 peer closed while response was flow-control blocked"
                                               state))))))
                            (when has-trailers
                              (dolist (wire
                                       (%h2-header-frames
                                        (%hpack-encode-block
                                         response-trailer-fields
                                         :huffman-p huffman-p)
                                        t
                                        peer-max-frame-size
                                        (%h2-server-stream-id state)))
                                  (funcall writer wire))))))))
                      (finish-request (state)
                        (block finish-request
                          (when (or (%h2-server-stream-reset-p state)
                                    (%h2-server-stream-responded-p state))
                            (return-from finish-request nil))
                          (unless (%h2-server-stream-end-stream-p state)
                            (%h2-server-error
                             "HTTP/2 request was finalized before END_STREAM"
                             state))
                          (%h2-content-length
                           (%h2-server-stream-headers state)
                           (%h2-server-stream-body-length-seen state)
                           :body-length-known-p t)
                          (let ((request
                                  (%h2-server-make-request state collect-body-p)))
                            (setf (%h2-server-stream-request state) request
                                  current-request request)
                            (let ((response (funcall handler request)))
                              (let ((send-result (send-response state response)))
                                (when (eq send-result :goaway)
                                  (return-from finish-request :goaway))
                                (when (eq send-result :reset)
                                  (return-from finish-request :reset)))
                              (setf (%h2-server-stream-responded-p state) t)
                              (incf request-count)
                              (when (and max-requests
                                         (>= request-count max-requests))
                                (send-goaway)
                                (return-from finish-request :max-requests))))))
                      (handle-headers (frame)
                        (let ((stream-id (%h2-frame-stream-id frame))
                              (flags (%h2-frame-flags frame)))
                          (%h2-server-invalid-stream-id stream-id)
                          (unless (%h2-server-frame-flags-valid-p
                                   frame
                                   (logior +http2-end-stream-flag+
                                           +http2-end-headers-flag+
                                           +http2-padded-flag+
                                           +http2-priority-flag+))
                            (%h2-server-error
                             "Invalid HTTP/2 HEADERS flags" flags))
                          (let ((state (gethash stream-id streams)))
                            (if state
                                (progn
                                  (when (or (not (%h2-server-stream-headers-complete-p
                                                 state))
                                            (%h2-server-stream-responded-p state)
                                            (%h2-server-stream-reset-p state))
                                    (%h2-server-error
                                     "Unexpected HTTP/2 request header block"
                                     stream-id))
                                  (when (zerop (logand flags
                                                      +http2-end-stream-flag+))
                                    (%h2-server-error
                                     "HTTP/2 request trailers must end the stream"
                                     stream-id))
                                  (multiple-value-bind (block end-stream-p)
                                      (%h2-read-header-block
                                      frame reader max-frame-size
                                      absolute-deadline clock-function
                                       max-header-bytes stream-id
                                       #'next-frame)
                                    (unless end-stream-p
                                      (%h2-server-error
                                       "HTTP/2 trailer block must have END_STREAM"
                                       stream-id))
                                    (let ((fields
                                            (%hpack-decode-block
                                             block decoder-context
                                             :max-header-bytes max-header-bytes)))
                                      (%h2-trailers fields)
                                      (setf (%h2-server-stream-trailers state)
                                            (mapcar
                                             (lambda (field)
                                               (http-kit:make-http-header
                                                (car field) (cdr field)))
                                             fields)
                                            (%h2-server-stream-end-stream-p state)
                                            t)
                                      (finish-request state))))
                                (progn
                                  (when (<= stream-id last-client-stream-id)
                                    (%h2-server-error
                                     "HTTP/2 request stream IDs must increase"
                                     stream-id))
                                  (setf last-client-stream-id stream-id)
                                  (multiple-value-bind (block end-stream-p)
                                      (%h2-read-header-block
                                      frame reader max-frame-size
                                      absolute-deadline clock-function
                                       max-header-bytes stream-id
                                       #'next-frame)
                                    (let ((fields
                                            (%hpack-decode-block
                                             block decoder-context
                                             :max-header-bytes max-header-bytes)))
                                      (multiple-value-bind
                                            (method scheme authority target headers)
                                          (%h2-server-header-fields
                                           fields default-authority)
                                        (let ((state
                                                (%make-h2-server-stream
                                                 stream-id
                                                 :method method
                                                 :scheme scheme
                                                 :authority authority
                                                 :target target
                                                 :headers headers
                                                 :expected-body-length
                                                 (%h2-content-length
                                                  headers nil
                                                  :body-length-known-p nil)
                                                 :headers-complete-p t
                                                 :end-stream-p end-stream-p)))
                                          (setf (gethash stream-id streams) state)
                                          (when end-stream-p
                                            (finish-request state)))))))))))
                      (handle-data (frame)
                        (let* ((stream-id (%h2-frame-stream-id frame))
                               (flags (%h2-frame-flags frame))
                               (payload (%h2-frame-payload frame))
                               (state (gethash stream-id streams)))
                          (%h2-server-invalid-stream-id stream-id)
                          (unless (%h2-server-frame-flags-valid-p
                                   frame
                                   (logior +http2-end-stream-flag+
                                           +http2-padded-flag+))
                            (%h2-server-error
                             "Invalid HTTP/2 DATA flags" flags))
                          (unless state
                            (%h2-server-error
                             "HTTP/2 DATA frame references an unknown stream"
                             stream-id))
                          (when (or (not (%h2-server-stream-headers-complete-p
                                          state))
                                    (%h2-server-stream-end-stream-p state)
                                    (%h2-server-stream-responded-p state)
                                    (%h2-server-stream-reset-p state))
                            (%h2-server-error
                             "HTTP/2 DATA frame is not valid for this stream"
                             stream-id))
                          (let* ((frame-length (length payload))
                                 (body (%h2-data-payload frame)))
                            (when (> frame-length receive-connection-window)
                              (%h2-server-error
                               "HTTP/2 connection receive window was exceeded"
                               frame-length))
                            (when (> frame-length
                                     (%h2-server-stream-receive-window state))
                              (%h2-server-error
                               "HTTP/2 stream receive window was exceeded"
                               frame-length))
                            (let ((next-body-length
                                    (+ (%h2-server-stream-body-length-seen state)
                                       (length body))))
                              (http-kit::%check-limit
                               :body next-body-length max-body-bytes
                               :operation :http2-server)
                              (decf receive-connection-window frame-length)
                              (decf (%h2-server-stream-receive-window state)
                                    frame-length)
                              (%h2-server-append-body state body collect-body-p)
                              (incf (%h2-server-stream-body-length-seen state)
                                    (length body))
                              (when (and on-body-chunk (plusp (length body)))
                                (funcall on-body-chunk state body))
                              (%h2-send-window-update writer 0 frame-length)
                              (%h2-send-window-update writer stream-id
                                                       frame-length)
                              (incf receive-connection-window frame-length)
                              (incf (%h2-server-stream-receive-window state)
                                    frame-length)
                              (when (plusp (logand flags
                                                  +http2-end-stream-flag+))
                                (setf (%h2-server-stream-end-stream-p state) t)
                                (finish-request state))))))
                      (process-frame (frame)
                        (if (eq frame :eof)
                            :eof
                            (case (%h2-frame-type frame)
                              ((#.+http2-settings-type+
                                #.+http2-ping-type+
                                #.+http2-window-update-type+
                                #.+http2-rst-stream-type+
                                #.+http2-goaway-type+
                                #.+http2-priority-type+)
                               (process-control-frame frame))
                              (#.+http2-headers-type+
                               (handle-headers frame))
                              (#.+http2-data-type+
                               (handle-data frame))
                              (#.+http2-continuation-type+
                               (%h2-server-error
                                "Unexpected HTTP/2 CONTINUATION frame" frame))
                              (#.+http2-push-promise-type+
                               (%h2-server-error
                                "HTTP/2 PUSH_PROMISE is invalid from a client"
                                frame))
                              (otherwise nil)))))
                   (loop
                     (when (and max-requests
                                (>= request-count max-requests))
                       (send-goaway)
                       (setf termination :max-requests)
                       (return))
                     (let ((result (process-frame (next-frame))))
                       (case result
                         (:eof
                          (setf termination :eof)
                          (return))
                         (:goaway
                          (setf termination :goaway)
                          (return))
                         (:max-requests
                          (setf termination :max-requests)
                          (return))))))))
           (condition (caught-condition)
             (when on-error
               (funcall on-error caught-condition current-request))
             (error caught-condition)))
      (when close-stream
        (funcall close-stream stream)))
    (values request-count termination)))
