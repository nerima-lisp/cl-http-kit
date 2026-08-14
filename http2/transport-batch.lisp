(in-package #:http-kit/http2)

(defun %h2-batch-entry-for-stream (entries stream-id)
  (find stream-id entries
        :key #'%h2-batch-entry-stream-id
        :test #'=))

(defun %h2-batch-read-header-block
    (first-frame next-frame max-frame-size max-header-bytes)
  "Read one complete response header block through NEXT-FRAME.

NEXT-FRAME is used instead of the ordinary frame reader because a flow-control
stall can make the batch uploader read response frames into a pending queue.
CONTINUATION frames must still be consumed contiguously from that queue."
  (declare (ignore max-frame-size))
  (let ((stream-id (%h2-frame-stream-id first-frame)))
    (unless (and (= (%h2-frame-type first-frame) +http2-headers-type+)
                 (plusp stream-id))
      (error 'http-kit:http-protocol-error
             :message "The HTTP/2 batch received HEADERS on an invalid stream."
             :operation :http2-headers
             :detail (list (%h2-frame-type first-frame) stream-id)))
    (let ((first-fragment (%h2-header-fragment first-frame :first-p t))
          (parts nil)
          (header-block-bytes 0)
          (frame first-frame))
      (push first-fragment parts)
      (incf header-block-bytes (length first-fragment))
      (http-kit::%check-limit :headers header-block-bytes max-header-bytes)
      (loop until (/= 0 (logand (%h2-frame-flags frame)
                                +http2-end-headers-flag+))
            do (setf frame (funcall next-frame))
               (when (eq frame :eof)
                 (error 'http-kit:http-protocol-error
                        :message "HTTP/2 HEADERS ended before CONTINUATION END_HEADERS."
                        :operation :http2-headers
                        :detail :eof))
               (unless (and (= (%h2-frame-type frame)
                               +http2-continuation-type+)
                            (= (%h2-frame-stream-id frame) stream-id))
                 (error 'http-kit:http-protocol-error
                        :message "HTTP/2 HEADERS must be followed by same-stream CONTINUATION frames."
                        :operation :http2-headers
                        :detail (list (%h2-frame-type frame)
                                      (%h2-frame-stream-id frame))))
               (when (/= 0 (logand (%h2-frame-flags frame)
                                   (lognot +http2-end-headers-flag+)))
                 (error 'http-kit:http-protocol-error
                        :message "An HTTP/2 CONTINUATION has an invalid flag."
                        :operation :http2-headers
                        :detail (%h2-frame-flags frame)))
               (push (%h2-frame-payload frame) parts)
               (incf header-block-bytes
                     (length (%h2-frame-payload frame)))
               (http-kit::%check-limit :headers header-block-bytes
                                       max-header-bytes))
      (values (%h2-concat (nreverse parts))
              (/= 0 (logand (%h2-frame-flags first-frame)
                            +http2-end-stream-flag+))))))

(defun %h2-batch-process-headers-frame
    (entry frame next-frame max-frame-size max-header-bytes context
           request-method)
  (multiple-value-bind (block end-stream)
      (%h2-batch-read-header-block frame next-frame max-frame-size
                                   max-header-bytes)
    (let* ((fields (%hpack-decode-block block context
                                        :max-header-bytes max-header-bytes))
           (status (%h2-batch-entry-status entry))
           (headers (%h2-batch-entry-headers entry))
           (body (%h2-batch-entry-body-vector entry))
           (body-length (%h2-batch-entry-body-length entry)))
      (if status
          (progn
            (unless end-stream
              (error 'http-kit:http-protocol-error
                     :message "HTTP/2 trailing HEADERS must end the stream."
                     :operation :http2-trailers
                     :detail :missing-end-stream))
            (values status headers
                    (%h2-finish-request-response
                     status headers (%h2-trailers fields) body request-method
                     :body-length body-length)))
          (multiple-value-bind (candidate-status candidate-headers)
              (%h2-status-and-headers fields)
            (if (< candidate-status 200)
                (progn
                  (when end-stream
                    (error 'http-kit:http-protocol-error
                           :message "An informational HTTP/2 response cannot end the stream."
                           :operation :http2-read
                           :detail candidate-status))
                  (values nil nil nil))
                (values candidate-status candidate-headers
                        (when end-stream
                          (%h2-finish-request-response
                           candidate-status candidate-headers nil body
                           request-method :body-length body-length)))))))))

(defun %h2-batch-option-list (name value count)
  (cond
    ((null value)
     (make-list count :initial-element nil))
    ((not (listp value))
     (error 'http-kit:http-protocol-error
            :message "An HTTP/2 batch option must be a proper list."
            :operation :http2-client
            :detail (list name value)))
    ((/= (length value) count)
     (error 'http-kit:http-protocol-error
            :message "An HTTP/2 batch option must be parallel to REQUESTS."
            :operation :http2-client
            :detail (list name (length value) count)))
    (t value)))

(defun send-http2-requests-over-connection
    (connection requests
     &key timeout deadline max-header-bytes max-body-bytes clock-function
       request-body-functions request-body-lengths
       on-body-chunk (collect-body-p t) (huffman-p nil))
  "Send REQUESTS as multiplexed HTTP/2 streams.

The request headers are emitted for every stream before request bodies are
uploaded.  REQUEST-BODY-FUNCTIONS and REQUEST-BODY-LENGTHS, when supplied,
are proper lists parallel to REQUESTS; a non-NIL function produces octet
chunks and its matching length may be NIL for an unknown length.  NIL entries
use each request's in-memory body.  Responses may arrive in any order and are
returned in the same order as REQUESTS.  ON-BODY-CHUNK, when supplied,
receives a payload and the corresponding request.  This API is cooperative
rather than thread-safe: one caller owns a connection while this operation is
active."
  (unless (http2-connection-p connection)
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 batch request requires an HTTP2-CONNECTION."
           :operation :http2-client
           :detail (type-of connection)))
  (unless (http2-connection-open-p connection)
    (error 'http-kit:http-connection-error
           :message "The HTTP/2 connection is closed."
           :operation :http2-client
           :cause :closed))
  (unless (and (listp requests) requests)
    (error 'http-kit:http-protocol-error
           :message "HTTP/2 batch requests must be a non-empty proper list."
           :operation :http2-client
           :detail requests))
  (let* ((body-functions
           (%h2-batch-option-list :request-body-functions
                                  request-body-functions
                                  (length requests)))
         (body-lengths
           (%h2-batch-option-list :request-body-lengths
                                  request-body-lengths
                                  (length requests))))
    (loop for request in requests
          for body-function in body-functions
          for body-length in body-lengths
          do (http-kit::%check-http-request request)
             (%h2-validate-request-body-options request
                                                body-function body-length))
    (when (and on-body-chunk (not (functionp on-body-chunk)))
      (error 'http-kit:http-protocol-error
             :message "ON-BODY-CHUNK must be a function or NIL."
             :operation :http2-client
             :detail on-body-chunk))
    (unless (member collect-body-p '(nil t))
      (error 'http-kit:http-protocol-error
             :message "COLLECT-BODY-P must be NIL or T."
             :operation :http2-client
             :detail collect-body-p))
    (let* ((clock (or clock-function
                      (%http2-connection-clock-function connection)))
           (header-limit (or max-header-bytes
                             (%http2-connection-max-header-bytes connection)))
           (body-limit (or max-body-bytes
                           (%http2-connection-max-body-bytes connection))))
      (http-kit:with-http-deadline (absolute-deadline timeout
                                     :inherited deadline
                                     :clock-function clock)
        (%h2-validate-limit :max-header-bytes header-limit)
        (%h2-validate-limit :max-body-bytes body-limit :allow-zero t)
        (http-kit::%with-http-error-translation
            ("The HTTP/2 batch request failed." :http2-connection)
          (handler-case
            (let* ((session-started-p
                     (%http2-connection-session-started-p connection))
                   (reader (%h2-reader-for
                            (%http2-connection-stream connection)))
                   (writer (%h2-writer (%http2-connection-stream connection)
                                       absolute-deadline clock))
                   (entries nil)
                   (header-writes nil))
              ;; Build every header block before writing any bytes.  A bad
              ;; request therefore cannot leave a half-emitted batch on the
              ;; connection.
              (loop for request in requests
                    for body-function in body-functions
                    for body-length in body-lengths
                    do (let ((stream-id
                               (%h2-connection-next-stream-id connection)))
                         (multiple-value-bind
                               (header-wire body outgoing-frame-size
                                expected-body-length trailer-fields)
                             (%h2-request-header-wire
                              request
                              (%http2-connection-max-frame-size connection)
                              header-limit body-limit
                              :stream-id stream-id
                              :include-session-p
                              (and (not session-started-p)
                                   (null entries))
                              :peer-max-frame-size
                              (%http2-connection-peer-max-frame-size
                               connection)
                              :request-body-function body-function
                              :request-body-length body-length
                              :huffman-p huffman-p)
                           (push (%make-h2-batch-entry
                                  :request request
                                  :stream-id stream-id
                                  :body body
                                  :request-body-function body-function
                                  :expected-body-length expected-body-length
                                  :outgoing-frame-size outgoing-frame-size
                                  :trailer-fields trailer-fields)
                                 entries)
                          (push (list header-wire outgoing-frame-size stream-id)
                                 header-writes)
                           (incf (%http2-connection-next-stream-id connection)
                                 2))))
              (setf entries (nreverse entries)
                    header-writes (nreverse header-writes))
              (dolist (write header-writes)
                (%h2-write-wire (%http2-connection-stream connection)
                                (first write) absolute-deadline clock))
              (let ((pending-head nil)
                    (pending-tail nil)
                    (first-settings-p (not session-started-p))
                    (remaining (length entries)))
                (labels
                    ((enqueue (frame)
                       (let ((cell (list frame)))
                         (if pending-tail
                             (setf (cdr pending-tail) cell
                                   pending-tail cell)
                             (setf pending-head cell
                                   pending-tail cell))))
                     (dequeue ()
                       (when pending-head
                         (let ((frame (car pending-head)))
                           (setf pending-head (cdr pending-head))
                           (unless pending-head
                             (setf pending-tail nil))
                           frame)))
                     (read-network-frame ()
                       (let ((frame (%h2-read-frame
                                     reader
                                     (%http2-connection-max-frame-size connection)
                                     absolute-deadline clock)))
                         (when (eq frame :eof)
                           (error 'http-kit:http-connection-error
                                  :message "The HTTP/2 peer closed during a batch request."
                                  :operation :http2-read
                                  :cause :eof))
                         (when first-settings-p
                           (unless (and (= (%h2-frame-type frame)
                                           +http2-settings-type+)
                                        (zerop (%h2-frame-stream-id frame))
                                        (zerop (logand (%h2-frame-flags frame)
                                                       +http2-ack-flag+)))
                             (error 'http-kit:http-protocol-error
                                    :message "The first HTTP/2 peer frame must be a non-ACK SETTINGS frame."
                                    :operation :http2-read
                                    :detail (list (%h2-frame-type frame)
                                                  (%h2-frame-stream-id frame)
                                                  (%h2-frame-flags frame))))
                           (setf first-settings-p nil))
                         frame))
                     (next-frame ()
                       (or (dequeue) (read-network-frame)))
                     (control-expected-stream-id (stream-id)
                       (if (plusp stream-id)
                           stream-id
                           (or (and entries
                                    (%h2-batch-entry-stream-id
                                     (first entries)))
                               1)))
                     (handle-control (frame)
                       (let ((type (%h2-frame-type frame))
                             (stream-id (%h2-frame-stream-id frame))
                             (payload (%h2-frame-payload frame)))
                         (cond
                           ((= type +http2-settings-type+)
                            (%h2-connection-note-settings connection frame writer))
                           ((= type +http2-window-update-type+)
                            (%h2-connection-note-window-update
                             connection frame
                             (control-expected-stream-id stream-id))
                            (%h2-handle-control-frame
                             frame writer
                             (control-expected-stream-id stream-id)))
                           ((= type +http2-goaway-type+)
                            (unless (and (zerop stream-id)
                                         (>= (length payload) 8))
                              (error 'http-kit:http-protocol-error
                                     :message "An HTTP/2 GOAWAY frame is invalid."
                                     :operation :http2-control
                                     :detail (list stream-id (length payload))))
                            (let ((last-stream-id (%h2-u32 payload 0)))
                              (when (/= 0 (logand last-stream-id #x80000000))
                                (error 'http-kit:http-protocol-error
                                       :message "An HTTP/2 GOAWAY last-stream identifier is invalid."
                                       :operation :http2-control
                                       :detail last-stream-id))
                              (setf (%http2-connection-goaway-last-stream-id
                                     connection)
                                    last-stream-id)
                              (when (some
                                     (lambda (entry)
                                       (> (%h2-batch-entry-stream-id entry)
                                          last-stream-id))
                                     entries)
                                (error 'http-kit:http-connection-error
                                       :message "The HTTP/2 peer rejected a batch stream with GOAWAY."
                                       :operation :http2-control
                                       :cause (list :goaway last-stream-id
                                                    (%h2-u32 payload 4))))))
                           ((= type +http2-rst-stream-type+)
                            (unless (and (plusp stream-id)
                                         (= (length payload) 4))
                              (error 'http-kit:http-protocol-error
                                     :message "An HTTP/2 RST_STREAM frame is invalid."
                                     :operation :http2-control
                                     :detail (list stream-id (length payload))))
                            (if (%h2-batch-entry-for-stream entries stream-id)
                                (error 'http-kit:http-connection-error
                                       :message "The HTTP/2 peer reset a batch response stream."
                                       :operation :http2-control
                                       :cause (list :rst-stream stream-id
                                                    (%h2-u32 payload 0)))
                                (%h2-handle-control-frame
                                 frame writer
                                 (control-expected-stream-id stream-id))))
                           (t
                            (%h2-handle-control-frame
                             frame writer
                             (control-expected-stream-id stream-id))))))
                     (send-entry-body-with-producer (entry)
                       (multiple-value-bind (pending-frames new-first-settings-p)
                           (%h2-connection-send-request-body
                            connection reader writer
                            (%h2-batch-entry-stream-id entry)
                            (%h2-batch-entry-body entry)
                            (%h2-batch-entry-request-body-function entry)
                            (%h2-batch-entry-expected-body-length entry)
                            body-limit
                            (%h2-batch-entry-outgoing-frame-size entry)
                            (%h2-batch-entry-trailer-fields entry)
                            absolute-deadline clock first-settings-p
                            huffman-p)
                         (dolist (frame pending-frames)
                           (enqueue frame))
                         (setf first-settings-p new-first-settings-p)))
                     (mark-complete (entry response)
                       (unless (%h2-batch-entry-response entry)
                         (setf (%h2-batch-entry-response entry) response)
                         (decf remaining)))
                  )
                  (dolist (entry entries)
                    (send-entry-body-with-producer entry))
                  (loop while (plusp remaining)
                        do (let ((frame (next-frame)))
                             (cond
                               ((%h2-control-frame-p (%h2-frame-type frame))
                                (handle-control frame))
                               ((= (%h2-frame-type frame)
                                   +http2-headers-type+)
                                (let ((entry
                                        (%h2-batch-entry-for-stream
                                         entries (%h2-frame-stream-id frame))))
                                  (unless entry
                                    (error 'http-kit:http-protocol-error
                                           :message (format nil
                                                            "The HTTP/2 batch received HEADERS for unknown stream ~D."
                                                            (%h2-frame-stream-id frame))
                                           :operation :http2-read
                                           :detail (%h2-frame-stream-id frame)))
                                  (when (%h2-batch-entry-response entry)
                                    (error 'http-kit:http-protocol-error
                                           :message "The HTTP/2 batch received HEADERS after stream completion."
                                           :operation :http2-read
                                           :detail (%h2-frame-stream-id frame)))
                                  (multiple-value-bind
                                        (status headers response)
                                      (%h2-batch-process-headers-frame
                                       entry frame
                                       (lambda ()
                                         (next-frame))
                                       (%http2-connection-max-frame-size
                                        connection)
                                       header-limit
                                       (%http2-connection-hpack-context
                                        connection)
                                       (http-kit:http-request-method
                                        (%h2-batch-entry-request entry)))
                                    (when status
                                      (setf (%h2-batch-entry-status entry)
                                            status
                                            (%h2-batch-entry-headers entry)
                                            headers))
                                    (when response
                                      (mark-complete entry response)))))
                               ((= (%h2-frame-type frame)
                                   +http2-data-type+)
                                (let* ((stream-id (%h2-frame-stream-id frame))
                                       (entry (%h2-batch-entry-for-stream
                                               entries stream-id)))
                                  (unless entry
                                    (error 'http-kit:http-protocol-error
                                           :message (format nil
                                                            "The HTTP/2 batch received DATA for unknown stream ~D."
                                                            stream-id)
                                           :operation :http2-read
                                           :detail stream-id))
                                  (when (%h2-batch-entry-response entry)
                                    (error 'http-kit:http-protocol-error
                                           :message "The HTTP/2 batch received DATA after stream completion."
                                           :operation :http2-read
                                           :detail stream-id))
                                  (multiple-value-bind (end-stream-p new-body-length
                                                        payload-length)
                                      (%h2-append-data-frame
                                       frame
                                       (%h2-batch-entry-status entry)
                                       (%h2-batch-entry-body-vector entry)
                                       (%h2-batch-entry-body-length entry)
                                       (http-kit:http-request-method
                                        (%h2-batch-entry-request entry))
                                       body-limit
                                       (and on-body-chunk
                                            (lambda (payload)
                                              (funcall
                                               on-body-chunk payload
                                               (%h2-batch-entry-request entry))))
                                       collect-body-p stream-id)
                                    (setf (%h2-batch-entry-body-length entry)
                                          new-body-length)
                                    (%h2-send-window-update writer stream-id
                                                            payload-length)
                                    (%h2-send-window-update writer 0
                                                            payload-length)
                                    (when end-stream-p
                                      (mark-complete
                                       entry
                                       (%h2-finish-request-response
                                        (%h2-batch-entry-status entry)
                                        (%h2-batch-entry-headers entry)
                                        nil
                                        (%h2-batch-entry-body-vector entry)
                                        (http-kit:http-request-method
                                         (%h2-batch-entry-request entry))
                                        :body-length new-body-length))))))
                               ((= (%h2-frame-type frame)
                                   +http2-continuation-type+)
                                (error 'http-kit:http-protocol-error
                                       :message "An HTTP/2 CONTINUATION arrived without HEADERS."
                                       :operation :http2-headers
                                       :detail (%h2-frame-stream-id frame)))
                               ((= (%h2-frame-type frame)
                                   +http2-push-promise-type+)
                                (error 'http-kit:http-unsupported-feature
                                       :message "HTTP/2 server push is not supported by the batch client."
                                       :operation :http2-read
                                       :feature :http2-server-push))
                               (t nil)))))
                  (setf (%http2-connection-session-started-p connection) t)
                  (mapcar #'%h2-batch-entry-response entries)))
            (error (condition)
            (close-http2-connection connection)
            (error condition))))))))

(defun send-http2-requests-over-connection/cps
    (connection requests on-success
     &key on-error timeout deadline max-header-bytes max-body-bytes
       clock-function request-body-functions request-body-lengths
       on-body-chunk (collect-body-p t) (huffman-p nil))
  "Send a concurrent HTTP/2 batch using CPS continuations."
  (http-kit::%call-http-operation/cps
   (lambda ()
     (send-http2-requests-over-connection
      connection requests
      :timeout timeout
      :deadline deadline
      :max-header-bytes max-header-bytes
      :max-body-bytes max-body-bytes
      :clock-function clock-function
      :request-body-functions request-body-functions
      :request-body-lengths request-body-lengths
      :huffman-p huffman-p
      :on-body-chunk on-body-chunk
      :collect-body-p collect-body-p))
   on-success
   :on-error on-error))
