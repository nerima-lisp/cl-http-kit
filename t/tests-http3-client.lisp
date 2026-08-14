(in-package #:http-kit/test)

(deftest http3-injected-quic-request-response
  (signals http-protocol-error
    (http-kit/http3:send-http3-request/cps nil nil nil))
  (let* ((request-table
           (let ((table
                   (http-kit/http3:make-qpack-dynamic-table
                    :max-capacity 256
                    :capacity 256)))
             (http-kit/http3:qpack-dynamic-table-insert
              table "x-test" "yes")
             table))
         (response-table
           (let ((table
                   (http-kit/http3:make-qpack-dynamic-table
                    :max-capacity 256
                    :capacity 256)))
             (http-kit/http3:qpack-dynamic-table-insert
              table "content-type" "text/plain")
             table))
         (response-fields (list (cons ":status" "200")
                                (cons "content-type" "text/plain")
                                (cons "content-length" "5")))
         (response-headers
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-headers-type+
             :payload
             (http-kit/http3:qpack-encode-field-section
              response-fields
              :dynamic-table response-table
              :huffman-p t))))
         (response-data
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-data-type+
             :payload (octets 104 101 108 108 111))))
         (response-wire (http3-test-concat-octets response-headers response-data))
         (response-chunks
           (list (list (subseq response-wire 0 1) nil)
                 (list (subseq response-wire 1 4) nil)
                 (list (subseq response-wire 4) t)))
         (opened-streams nil)
         (request-stream nil)
         (body-chunks nil))
    (labels ((open-stream (request &key stream-type timeout deadline)
               (declare (ignore timeout deadline))
               (let ((stream
                       (make-http3-test-stream
                        :kind stream-type
                        :reads (if (eq stream-type :request)
                                   response-chunks
                                   nil))))
                 (when (eq stream-type :request)
                   (ensure-true (http-request-p request))
                   (setf request-stream stream))
                 (push stream opened-streams)
                 stream))
             (write-stream (stream octets &key fin-p timeout deadline)
               (declare (ignore timeout deadline))
               (push (list (subseq octets 0) fin-p)
                     (http3-test-stream-writes stream)))
             (read-stream (stream &key timeout deadline)
               (declare (ignore timeout deadline))
               (let ((entry (pop (http3-test-stream-reads stream))))
                 (if entry
                     (values (first entry) (second entry))
                     (values nil t))))
             (close-stream (stream &key condition)
               (declare (ignore condition))
               (setf (http3-test-stream-closed-p stream) t)))
      (let ((client
              (http-kit/http3:make-http3-client
               :open-stream #'open-stream
               :write-stream #'write-stream
               :read-stream #'read-stream
               :close-stream #'close-stream)))
        (ensure-true (http-kit/http3:http3-client-open-p client))
        (let ((response nil))
          (http-kit/http3:send-http3-request/cps
           client
           (make-http-request
            :method "POST"
            :uri "https://example.test/resource?q=1"
            :headers (list (make-http-header "x-test" "yes"))
            :body (octets 65 66))
           (lambda (value)
             (setf response value))
           :collect-body-p nil
           :qpack-encoder-table request-table
           :qpack-decoder-table response-table
           :huffman-p t
           :on-body-chunk
           (lambda (chunk)
             (push (subseq chunk 0) body-chunks)))
          (ensure-equal "HTTP/3" (http-response-protocol-version response))
          (ensure-equal 200 (http-response-status response))
          (ensure-equal 0 (length (http-response-body response)))
          (ensure-equal "5"
                        (http-header-value
                         (http-response-headers response)
                         "content-length"))
          (ensure-equal (octets 104 101 108 108 111)
                        (apply #'http3-test-concat-octets
                               (nreverse body-chunks))))
        (let* ((control-stream
                 (find :control opened-streams
                       :key #'http3-test-stream-kind))
               (control-wire
                 (first (http3-test-stream-writes control-stream))))
          (multiple-value-bind (stream-type position)
              (http-kit/http3:http3-varint-decode (first control-wire))
            (ensure-equal http-kit/http3:+http3-control-stream-type+
                          stream-type)
            (multiple-value-bind (frames remainder)
                (http-kit/http3:decode-http3-frames
                 (subseq (first control-wire) position))
              (ensure-equal 0 (length remainder))
              (ensure-equal 1 (length frames))
              (ensure-equal http-kit/http3:+http3-settings-type+
                            (http-kit/http3:http3-frame-type (first frames))))))
        (let* ((request-writes
                 (nreverse (http3-test-stream-writes request-stream)))
               (header-wire (first (first request-writes)))
               (data-wire (first (second request-writes))))
          (multiple-value-bind (frames remainder)
              (http-kit/http3:decode-http3-frames header-wire)
            (ensure-equal 0 (length remainder))
            (ensure-equal http-kit/http3:+http3-headers-type+
                          (http-kit/http3:http3-frame-type (first frames)))
            (ensure-equal
             (list (cons ":method" "POST")
                   (cons ":scheme" "https")
                   (cons ":authority" "example.test")
                   (cons ":path" "/resource?q=1")
                   (cons "content-length" "2")
                   (cons "x-test" "yes"))
             (http-kit/http3:qpack-decode-field-section
              (http-kit/http3:http3-frame-payload (first frames))
              :dynamic-table request-table)))
          (multiple-value-bind (frames remainder)
              (http-kit/http3:decode-http3-frames data-wire)
            (ensure-equal 0 (length remainder))
            (ensure-equal http-kit/http3:+http3-data-type+
                          (http-kit/http3:http3-frame-type (first frames)))))
        (http-kit/http3:close-http3-client client)
        (ensure-true (every #'http3-test-stream-closed-p opened-streams))
        (ensure-true (not (http-kit/http3:http3-client-open-p client)))))))
