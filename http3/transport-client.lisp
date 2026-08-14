(in-package #:http-kit/http3)

(defun %h3-read-response
    (client stream &key on-body-chunk collect-body-p max-body-bytes
            qpack-decoder-table timeout deadline)
  (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)))
        (status nil)
        (headers nil)
        (trailers nil)
        (body (make-array 0 :element-type '(unsigned-byte 8)))
        (body-length 0)
        (final-response-p nil)
        (trailers-seen-p nil))
    (labels
        ((process-frame (frame)
           (let ((type (http3-frame-type frame))
                 (payload (http3-frame-payload frame)))
             (cond
               ((= type +http3-headers-type+)
                (let ((fields
                        (qpack-decode-field-section
                         payload
                         :max-header-bytes
                         (http3-client-max-header-bytes client)
                         :dynamic-table qpack-decoder-table)))
                  (multiple-value-bind (new-status new-headers)
                      (%h3-response-fields fields :trailers-p final-response-p)
                    (if final-response-p
                        (progn
                          (when trailers-seen-p
                            (%h3-transport-error
                             "An HTTP/3 response cannot contain multiple trailer blocks."))
                          (setf trailers new-headers
                                trailers-seen-p t))
                        (progn
                          (when (= new-status 101)
                            (%h3-transport-error
                             "HTTP/3 does not permit a 101 Switching Protocols response."))
                          (if (>= new-status 200)
                              (progn
                                (setf status new-status
                                      headers new-headers
                                      final-response-p t))
                              ;; Informational response fields are valid but are
                              ;; intentionally not retained by this one-shot API.
                              nil))))))
               ((= type +http3-data-type+)
                (unless final-response-p
                  (%h3-transport-error
                   "An HTTP/3 DATA frame arrived before final response HEADERS."))
                (when trailers-seen-p
                  (%h3-transport-error
                   "An HTTP/3 DATA frame arrived after response trailers."))
                (let ((new-length (+ body-length (length payload))))
                  (when (and max-body-bytes (> new-length max-body-bytes))
                    (%h3-size-error :body max-body-bytes new-length))
                  (setf body-length new-length)
                  (when collect-body-p
                    (setf body (%h3-append-body body payload)))
                  (when on-body-chunk
                    (funcall on-body-chunk (%http3-copy-octets payload)))))
               ((= type +http3-push-promise-type+)
                (error 'http-unsupported-feature
                       :message "HTTP/3 server push is not implemented by this client."
                       :operation :http3-response
                       :feature :http3-server-push))
               ((member type (list +http3-settings-type+
                                   +http3-cancel-push-type+
                                   +http3-goaway-type+
                                   +http3-max-push-id-type+)
                        :test #'=)
                (%h3-transport-error
                 "HTTP/3 control frames are not valid on a request stream." type))
               (t
                ;; Extension frame types are ignored by HTTP/3 endpoints.
                nil))))
         (finish-response ()
           (unless final-response-p
             (%h3-transport-error "The HTTP/3 stream ended before final response HEADERS."))
           (%h3-response-content-length headers body-length)
           (http-kit:make-http-response
            :protocol-version "HTTP/3"
            :status status
            :headers headers
            :trailers trailers
            :body (if collect-body-p body nil))))
      (loop
        (multiple-value-bind (chunk fin-p)
            (funcall (http3-client-read-stream client)
                     stream :timeout timeout :deadline deadline)
          (when chunk
            (unless (%http3-octet-vector-p chunk)
              (%h3-transport-error
               "HTTP/3 read-stream must return an octet vector or NIL."
               (type-of chunk)))
            (when (plusp (array-total-size chunk))
              (setf buffer (%http3-concatenate-octets buffer chunk))
              (multiple-value-bind (frames remainder)
                  (decode-http3-frames
                   buffer :allow-incomplete-p t
                   :max-frame-size (http3-client-max-frame-size client))
                (setf buffer remainder)
                (dolist (frame frames)
                  (process-frame frame)))))
          (when (or fin-p (null chunk))
            (when (plusp (array-total-size buffer))
              (%h3-transport-error
               "The HTTP/3 stream ended with a truncated frame."))
            (return (finish-response))))))))

(defun send-http3-request
    (client request &key on-body-chunk (collect-body-p t) max-body-bytes
            qpack-encoder-table qpack-decoder-table (huffman-p nil)
            timeout deadline)
  "Send REQUEST on a new HTTP/3 bidirectional stream and read its response.

The request and response bodies are represented as octet vectors.  When
ON-BODY-CHUNK is supplied it receives each response DATA payload; the payload
is still collected when COLLECT-BODY-P is true.  QPACK-ENCODER-TABLE and
QPACK-DECODER-TABLE, when supplied, are caller-owned dynamic tables used for
request and response field sections.  HUFFMAN-P enables Huffman encoding for
newly emitted field values and names."
  (unless (http3-client-p client)
    (%h3-transport-error "SEND-HTTP3-REQUEST requires an HTTP/3 client."
                         (type-of client)))
  (unless (http3-client-open-p client)
    (%h3-transport-error "The HTTP/3 client is already closed."))
  (unless (http-kit:http-request-p request)
    (%h3-transport-error "SEND-HTTP3-REQUEST requires an HTTP request."
                         (type-of request)))
  (unless (or (null max-body-bytes) (%h3-non-negative-limit-p max-body-bytes))
    (%h3-transport-error "max-body-bytes must be NIL or a non-negative integer."
                         max-body-bytes))
  (let* ((body (http-kit:http-request-body request))
         (request-fields (%h3-request-fields request))
         (trailer-fields (%h3-trailer-fields request))
         (header-block
           (qpack-encode-field-section
            request-fields
            :dynamic-table qpack-encoder-table
            :huffman-p huffman-p))
         (stream nil)
         (failure nil))
    (setf stream
          (funcall (http3-client-open-stream client)
                   request :stream-type :request :timeout timeout :deadline deadline))
    (unless stream
      (%h3-transport-error "The HTTP/3 open-stream callback returned NIL."))
    (unwind-protect
         (handler-case
             (progn
                (%h3-write-frame
                 client stream
                 (make-http3-frame :type +http3-headers-type+
                                   :payload header-block)
                :fin-p (and (zerop (array-total-size body)) (null trailer-fields))
                :timeout timeout :deadline deadline)
               (unless (zerop (array-total-size body))
                 (loop with position = 0
                       while (< position (length body))
                       for end = (min (length body)
                                      (+ position (http3-client-max-frame-size client)))
                       for last-p = (= end (length body))
                       do (%h3-write-frame
                           client stream
                            (make-http3-frame
                             :type +http3-data-type+
                             :payload (subseq body position end))
                           :fin-p (and last-p (null trailer-fields))
                           :timeout timeout :deadline deadline)
                          (setf position end)))
               (when trailer-fields
                  (%h3-write-frame
                   client stream
                   (make-http3-frame
                    :type +http3-headers-type+
                    :payload
                    (qpack-encode-field-section
                     trailer-fields
                     :dynamic-table qpack-encoder-table
                     :huffman-p huffman-p))
                  :fin-p t :timeout timeout :deadline deadline))
               (%h3-read-response
                client stream
                :on-body-chunk on-body-chunk
                :collect-body-p collect-body-p
                :max-body-bytes max-body-bytes
                :qpack-decoder-table qpack-decoder-table
                :timeout timeout
                :deadline deadline))
           (error (condition)
             (setf failure condition)
             (error condition)))
      (funcall (http3-client-close-stream client)
               stream :condition failure))))

(defun send-http3-request/cps
    (client request on-success
     &key on-error on-body-chunk (collect-body-p t) max-body-bytes
          qpack-encoder-table qpack-decoder-table (huffman-p nil)
          timeout deadline)
  "Send REQUEST and deliver its response to ON-SUCCESS.
ON-ERROR receives HTTP-ERROR conditions when supplied."
  (http-kit::%call-http-operation/cps
   (lambda ()
     (send-http3-request
      client request
      :on-body-chunk on-body-chunk
      :collect-body-p collect-body-p
      :max-body-bytes max-body-bytes
      :qpack-encoder-table qpack-encoder-table
      :qpack-decoder-table qpack-decoder-table
      :huffman-p huffman-p
      :timeout timeout
      :deadline deadline))
   on-success
   :on-error on-error))
