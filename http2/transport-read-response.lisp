(in-package #:http-kit/http2)

(defun %h2-read-response (reader writer max-frame-size deadline clock-function
                                  max-header-bytes max-body-bytes request-method
                                  &rest options)
  ;; Keep the historical positional callback/collection arguments while
  ;; accepting the connection-specific keyword options without mixing
  ;; &OPTIONAL and &KEY in the lambda list (which some implementations warn
  ;; about).  A keyword immediately after the callback means that collection
  ;; keeps its default value.
  (let ((on-body-chunk nil)
        (collect-body-p t)
        (expected-stream-id 1)
        (hpack-context nil)
        (read-initial-settings-p t)
        (peer-max-frame-size nil)
        (initial-frames nil)
        (on-peer-settings nil)
        (control-handler nil)
        (keyword-options options)
        (missing-value (gensym "MISSING-VALUE-")))
    (declare (ignorable peer-max-frame-size))
    (unless (or (null keyword-options)
                (keywordp (car keyword-options)))
      (setf on-body-chunk (pop keyword-options))
      (when (and keyword-options
                 (not (keywordp (car keyword-options))))
        (setf collect-body-p (pop keyword-options))))
    (loop while keyword-options
          for key = (pop keyword-options)
          for value = (if keyword-options
                          (pop keyword-options)
                          missing-value)
          do (when (eq value missing-value)
               (error 'http-kit:http-protocol-error
                      :message "An HTTP/2 response option is missing its value."
                      :operation :http2-read
                      :detail key))
             (case key
               (:expected-stream-id (setf expected-stream-id value))
               (:hpack-context (setf hpack-context value))
               (:read-initial-settings-p
                (setf read-initial-settings-p value))
               (:peer-max-frame-size (setf peer-max-frame-size value))
               (:initial-frames (setf initial-frames value))
               (:on-peer-settings (setf on-peer-settings value))
               (:control-handler (setf control-handler value))
               (otherwise
                (error 'http-kit:http-protocol-error
                       :message "An unknown HTTP/2 response option was supplied."
                       :operation :http2-read
                       :detail key))))
    (let ((context (or hpack-context
                       (%make-hpack-context
                        :max-size +hpack-default-table-size+
                        :maximum-size +hpack-default-table-size+)))
        (status nil)
        (headers nil)
        (body (if collect-body-p
                  (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)
                  (http-kit::%empty-octets)))
        (body-length 0))
    (when read-initial-settings-p
      (let ((first-frame (%h2-read-frame reader max-frame-size deadline
                                         clock-function)))
        (when (eq first-frame :eof)
          (error 'http-kit:http-protocol-error
                 :message "The HTTP/2 peer sent no initial SETTINGS frame."
                 :operation :http2-read
                 :detail :eof))
        (unless (and (= (%h2-frame-type first-frame) +http2-settings-type+)
                     (zerop (%h2-frame-stream-id first-frame))
                     (zerop (logand (%h2-frame-flags first-frame)
                                    +http2-ack-flag+)))
          (error 'http-kit:http-protocol-error
                 :message "The first HTTP/2 peer frame must be a non-ACK SETTINGS frame."
                 :operation :http2-read
                 :detail (list (%h2-frame-type first-frame)
                               (%h2-frame-stream-id first-frame)
                               (%h2-frame-flags first-frame))))
        (multiple-value-bind (peer-frame-size peer-table-size peer-window-size)
            (%h2-settings (%h2-frame-payload first-frame))
          (when on-peer-settings
            (funcall on-peer-settings peer-frame-size peer-table-size
                     peer-window-size)))
        (%h2-validate-settings-frame first-frame writer)))
    (loop
      for frame = (if initial-frames
                      (pop initial-frames)
                      (%h2-read-frame reader max-frame-size deadline clock-function))
      do (when (eq frame :eof)
           (error 'http-kit:http-protocol-error
                  :message "The HTTP/2 response ended before END_STREAM."
                  :operation :http2-read
                  :detail :eof))
         (let ((type (%h2-frame-type frame))
               (stream-id (%h2-frame-stream-id frame)))
           (cond
             ((%h2-control-frame-p type)
              (if control-handler
                  (funcall control-handler frame writer expected-stream-id)
                  (%h2-handle-control-frame frame writer expected-stream-id)))
             ((= type +http2-push-promise-type+)
              (error 'http-kit:http-unsupported-feature
                     :message "HTTP/2 server push is not supported by this client."
                     :operation :http2-read
                     :feature :http2-server-push))
             ((= type +http2-headers-type+)
              (multiple-value-bind (new-status new-headers response)
                  (%h2-process-headers-frame
                   frame reader max-frame-size deadline clock-function
                   max-header-bytes context status headers body body-length
                   request-method expected-stream-id)
                (setf status new-status
                      headers new-headers)
                (when response
                  (return-from %h2-read-response response))))
             ((= type +http2-data-type+)
              (multiple-value-bind (end-stream new-body-length data-length)
                  (%h2-append-data-frame*
                   frame status body body-length request-method max-body-bytes
                   on-body-chunk collect-body-p expected-stream-id)
                (setf body-length new-body-length)
                ;; DATA consumes both the per-stream and connection-level
                ;; receive windows.  Restore them after processing the
                ;; payload so a stream transport can receive bodies larger
                ;; than the initial 65,535-byte window.
                (%h2-send-window-update writer stream-id data-length)
                (%h2-send-window-update writer 0 data-length)
                (when end-stream
                  (return-from %h2-read-response
                    (%h2-finish-request-response
                     status headers nil body request-method
                     :body-length body-length)))))
             ((= type +http2-continuation-type+)
              (error 'http-kit:http-protocol-error
                     :message "An unexpected HTTP/2 CONTINUATION frame was received."
                     :operation :http2-read
                     :detail stream-id))
             (t
              ;; Unknown extension frames are ignored as required by HTTP/2.
              nil)))))))

(defun send-http2-request
    (client request
     &key timeout deadline max-header-bytes max-body-bytes clock-function
       request-body-function request-body-length
       on-body-chunk (collect-body-p t) (huffman-p nil))
  (unless (http2-client-p client)
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 request requires an HTTP2-CLIENT."
           :operation :http2-client
           :detail (type-of client)))
  (http-kit::%check-http-request request)
  (%h2-validate-request-body-options
   request request-body-function request-body-length)
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
  (let* ((clock (or clock-function (%http2-clock-function client)))
         (header-limit (or max-header-bytes (%http2-max-header-bytes client)))
         (body-limit (or max-body-bytes (%http2-max-body-bytes client)))
         (wire nil)
         (stream nil)
         (temporary-connection nil))
    (http-kit:with-http-deadline (absolute-deadline timeout
                                   :inherited deadline
                                   :clock-function clock)
      (%h2-validate-limit :max-header-bytes header-limit)
      (%h2-validate-limit :max-body-bytes body-limit :allow-zero t)
        (http-kit::%with-http-error-translation
          ("The HTTP/2 client request failed." :http2-client)
        (unwind-protect
             (cond
               ((%http2-connection client)
                (send-http2-request-over-connection
                 (%http2-connection client) request
                 :timeout timeout
                 :deadline absolute-deadline
                 :max-header-bytes header-limit
                 :max-body-bytes body-limit
                 :clock-function clock
                 :request-body-function request-body-function
                 :request-body-length request-body-length
                 :huffman-p huffman-p
                 :on-body-chunk on-body-chunk
                 :collect-body-p collect-body-p))
               ((%http2-exchange client)
                (when request-body-function
                  (error 'http-kit:http-unsupported-feature
                         :message "One-shot HTTP/2 exchanges do not support streaming request bodies."
                         :operation :http2-write
                         :feature :http2-streaming-request-body))
                (setf wire (%h2-request-wire request
                                             (%http2-max-frame-size client)
                                             header-limit body-limit
                                             :huffman-p huffman-p))
                (let ((reply (funcall (%http2-exchange client)
                                      request wire
                                      :timeout timeout
                                      :deadline absolute-deadline)))
                  (http-kit::%check-deadline absolute-deadline clock :read)
                  (%h2-read-response (%h2-reader-for reply) nil
                                     (%http2-max-frame-size client)
                                     absolute-deadline clock
                                     header-limit body-limit
                                     (http-kit:http-request-method request)
                                     on-body-chunk collect-body-p)))
               (t
                (setf stream
                      (funcall (%http2-open-stream client)
                               request
                               :timeout timeout
                               :deadline absolute-deadline))
                (unless (streamp stream)
                  (error 'http-kit:http-connection-error
                         :message "The HTTP/2 stream factory did not return a stream."
                         :operation :connect
                         :cause (type-of stream)))
                ;; A public OPEN-STREAM callback provides the negotiated byte
                ;; stream, so use the same state machine as a reusable
                ;; connection.  The old wire-at-once path could not consume
                ;; peer SETTINGS/WINDOW_UPDATE frames while an upload was
                ;; blocked at the initial flow-control window.
                (setf temporary-connection
                      (make-http2-connection
                       :stream stream
                       :close-stream (%http2-close-stream client)
                       :max-frame-size (%http2-max-frame-size client)
                       :max-header-bytes header-limit
                       :max-body-bytes body-limit
                       :clock-function clock))
                (send-http2-request-over-connection
                 temporary-connection request
                 :timeout timeout
                 :deadline absolute-deadline
                 :max-header-bytes header-limit
                 :max-body-bytes body-limit
                 :clock-function clock
                 :request-body-function request-body-function
                 :request-body-length request-body-length
                 :huffman-p huffman-p
                 :on-body-chunk on-body-chunk
                 :collect-body-p collect-body-p)))
          (when temporary-connection
            (close-http2-connection temporary-connection))
          (when (and stream (null temporary-connection))
            (funcall (%http2-close-stream client) stream)))))))

(defun send-http2-request/cps
    (client request on-success
     &key on-error timeout deadline max-header-bytes max-body-bytes
       clock-function request-body-function request-body-length
       on-body-chunk (collect-body-p t) (huffman-p nil))
  "Send an HTTP/2 request and dispatch its result to CPS continuations."
  (http-kit::%call-http-operation/cps
   (lambda ()
     (send-http2-request
      client request
      :timeout timeout
      :deadline deadline
      :max-header-bytes max-header-bytes
      :max-body-bytes max-body-bytes
      :clock-function clock-function
      :request-body-function request-body-function
      :request-body-length request-body-length
      :huffman-p huffman-p
      :on-body-chunk on-body-chunk
      :collect-body-p collect-body-p))
   on-success
   :on-error on-error))
