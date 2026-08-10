(in-package #:http-kit/test)

(defstruct http3-test-stream
  kind
  writes
  reads
  closed-p)

(defun http3-test-concat-octets (&rest parts)
  (let ((result (make-array (reduce #'+ parts :key #'length :initial-value 0)
                            :element-type '(unsigned-byte 8))))
    (loop with position = 0
          for part in parts
          do (replace result part :start1 position)
             (incf position (length part)))
    result))

(deftest http3-varint-and-frame-boundaries
  (dolist (value '(0 63 64 16383 16384 #x3fffffff #x40000000
                   #x3fffffffffffffff))
    (let ((encoded (http-kit/http3:http3-varint-encode value)))
      (ensure-equal '(unsigned-byte 8)
                    (array-element-type encoded)
                    "HTTP/3 varints are binary octet vectors")
      (multiple-value-bind (decoded position)
          (http-kit/http3:http3-varint-decode encoded)
        (ensure-equal value decoded)
        (ensure-equal (length encoded) position))))
  (multiple-value-bind (decoded position)
      (http-kit/http3:http3-varint-decode
       (make-array 0 :element-type '(unsigned-byte 8))
       :allow-incomplete-p t)
    (ensure-equal nil decoded)
    (ensure-equal 0 position))
  (signals http-protocol-error
    (http-kit/http3:http3-varint-decode
     (make-array 0 :element-type '(unsigned-byte 8))))
  (let* ((settings-frame
           (http-kit/http3:make-http3-settings-frame
            :max-field-section-size 4096
            :enable-connect 1
            :extra-settings (list (cons #x21 7))))
         (wire (http-kit/http3:encode-http3-frame settings-frame)))
    (multiple-value-bind (frames remainder)
        (http-kit/http3:decode-http3-frames wire)
      (ensure-equal 1 (length frames))
      (ensure-equal 0 (length remainder))
      (let ((settings
              (http-kit/http3:decode-http3-settings
               (http-kit/http3:http3-frame-payload (first frames)))))
      (ensure-equal 4096 (cdr (assoc #x6 settings)))
      (ensure-equal 1 (cdr (assoc #x8 settings)))
      (ensure-equal 7 (cdr (assoc #x21 settings)))))))
  (signals http-protocol-error
    (http-kit/http3:make-http3-settings-frame
     :extra-settings (list (cons http-kit/http3:+http3-setting-enable-push+ 0))))
(signals http-protocol-error
    (http-kit/http3:decode-http3-settings
     (http3-test-concat-octets
      (http-kit/http3:http3-varint-encode
       http-kit/http3:+http3-setting-enable-push+)
      (http-kit/http3:http3-varint-encode 0))))

(deftest http3-control-frame-state-boundaries
  (labels ((frame (type &optional (payload (octets)))
             (http-kit/http3:make-http3-frame :type type :payload payload)))
    (let ((state (http-kit/http3:make-http3-control-state)))
      (signals http-protocol-error
        (http-kit/http3:process-http3-control-frame
         state
         (frame http-kit/http3:+http3-goaway-type+
                (http-kit/http3:http3-varint-encode 7))))
      (ensure-equal :settings
                    (http-kit/http3:process-http3-control-frame
                     state
                     (frame http-kit/http3:+http3-settings-type+)))
      (ensure-true
       (http-kit/http3:http3-control-state-settings-received-p state))
      (ensure-equal nil
                    (http-kit/http3:http3-control-state-settings state))
      (signals http-protocol-error
        (http-kit/http3:process-http3-control-frame
         state
         (frame http-kit/http3:+http3-settings-type+)))
      (ensure-equal :goaway
                    (http-kit/http3:process-http3-control-frame
                     state
                     (frame http-kit/http3:+http3-goaway-type+
                            (http-kit/http3:http3-varint-encode 7))))
      (ensure-equal 7 (http-kit/http3:http3-control-state-goaway-id state))
      (ensure-equal :goaway
                    (http-kit/http3:process-http3-control-frame
                     state
                     (frame http-kit/http3:+http3-goaway-type+
                            (http-kit/http3:http3-varint-encode 3))))
      (signals http-protocol-error
        (http-kit/http3:process-http3-control-frame
         state
         (frame http-kit/http3:+http3-goaway-type+
                (http-kit/http3:http3-varint-encode 4))))
      (ensure-equal :max-push-id
                    (http-kit/http3:process-http3-control-frame
                     state
                     (frame http-kit/http3:+http3-max-push-id-type+
                            (http-kit/http3:http3-varint-encode 2))))
      (ensure-equal :max-push-id
                    (http-kit/http3:process-http3-control-frame
                     state
                     (frame http-kit/http3:+http3-max-push-id-type+
                            (http-kit/http3:http3-varint-encode 3))))
      (signals http-protocol-error
        (http-kit/http3:process-http3-control-frame
         state
         (frame http-kit/http3:+http3-max-push-id-type+
                (http-kit/http3:http3-varint-encode 1))))
      (ensure-equal :cancel-push
                    (http-kit/http3:process-http3-control-frame
                     state
                     (frame http-kit/http3:+http3-cancel-push-type+
                            (http-kit/http3:http3-varint-encode 9))))
      (ensure-equal :cancel-push
                    (http-kit/http3:process-http3-control-frame
                     state
                     (frame http-kit/http3:+http3-cancel-push-type+
                            (http-kit/http3:http3-varint-encode 9))))
      (ensure-equal '(9)
                    (http-kit/http3:http3-control-state-cancelled-push-ids
                     state))
      (signals http-protocol-error
        (http-kit/http3:process-http3-control-frame
         state
         (frame http-kit/http3:+http3-data-type+ (octets 1))))
      (signals http-protocol-error
        (http-kit/http3:process-http3-control-frame
         state
         (frame http-kit/http3:+http3-goaway-type+
                (http3-test-concat-octets
                 (http-kit/http3:http3-varint-encode 2)
                 (octets 0)))))
      (ensure-equal :extension
                    (http-kit/http3:process-http3-control-frame
                     state
                     (frame #x2a (octets 1 2)))))))

(deftest http3-control-stream-service-and-client-reader
  (let* ((settings-wire
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-settings-frame
             :max-field-section-size 4096
             :enable-connect 1
             :extra-settings (list (cons #x21 7)))))
         (goaway-wire
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-goaway-type+
             :payload (http-kit/http3:http3-varint-encode 7))))
         (max-push-id-wire
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-max-push-id-type+
             :payload (http-kit/http3:http3-varint-encode 3))))
         (cancel-push-wire
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-cancel-push-type+
             :payload (http-kit/http3:http3-varint-encode 9))))
         (wire
           (http3-test-concat-octets
            (http-kit/http3:http3-control-stream-prefix)
            settings-wire goaway-wire max-push-id-wire cancel-push-wire)))
    (labels ((chunks ()
               (list (list (subseq wire 0 2) nil)
                     (list (subseq wire 2) t))))
      (let ((stream (make-http3-test-stream
                     :kind :control
                     :reads (chunks)))
            (settings-seen nil)
            (goaways nil)
            (max-push-ids nil)
            (cancelled-push-ids nil)
            (closed-condition :unset))
        (labels ((read-stream (ignored-stream &key timeout deadline)
                   (declare (ignore ignored-stream timeout deadline))
                   (let ((entry (pop (http3-test-stream-reads stream))))
                     (if entry
                         (values (first entry) (second entry))
                         (values nil t))))
                 (close-stream (ignored-stream &key condition)
                   (declare (ignore ignored-stream))
                   (setf closed-condition condition
                         (http3-test-stream-closed-p stream) t)))
          (multiple-value-bind (state ended-p)
              (http-kit/http3:serve-http3-control-stream
               stream
               :read-stream #'read-stream
               :close-stream #'close-stream
               :on-settings (lambda (settings state)
                              (declare (ignore state))
                              (push settings settings-seen))
               :on-goaway (lambda (identifier state)
                            (declare (ignore state))
                            (push identifier goaways))
               :on-max-push-id (lambda (identifier state)
                                 (declare (ignore state))
                                 (push identifier max-push-ids))
               :on-cancel-push (lambda (identifier state)
                                 (declare (ignore state))
                                 (push identifier cancelled-push-ids)))
            (ensure-true ended-p)
            (ensure-true (http-kit/http3:http3-control-state-p state))
            (let ((settings (first settings-seen)))
              (ensure-equal 4096 (cdr (assoc #x6 settings)))
              (ensure-equal 1 (cdr (assoc #x8 settings)))
              (ensure-equal 7 (cdr (assoc #x21 settings))))
            (ensure-equal '(7) (nreverse goaways))
            (ensure-equal '(3) (nreverse max-push-ids))
            (ensure-equal '(9) (nreverse cancelled-push-ids))
            (ensure-equal 7
                          (http-kit/http3:http3-control-state-goaway-id state))
            (ensure-equal 3
                          (http-kit/http3:http3-control-state-max-push-id state))
            (ensure-true (http-kit/http3:http3-control-state-settings-received-p
                          state))
            (ensure-true (http-kit/http3:http3-control-state-cancelled-push-ids
                          state))
            (ensure-equal nil closed-condition)
            (ensure-true (http3-test-stream-closed-p stream)))))
      (let* ((peer-stream (make-http3-test-stream
                           :kind :peer-control
                           :reads (chunks)))
             (opened-streams nil)
             (closed-streams nil))
        (labels ((open-stream (ignored-request &key stream-type timeout deadline)
                   (declare (ignore ignored-request timeout deadline))
                   (ensure-equal :control stream-type)
                   (let ((stream (make-http3-test-stream :kind stream-type)))
                     (push stream opened-streams)
                     stream))
                 (write-stream (stream octets &key fin-p timeout deadline)
                   (declare (ignore fin-p timeout deadline))
                   (push (subseq octets 0)
                         (http3-test-stream-writes stream)))
                 (read-stream (stream &key timeout deadline)
                   (declare (ignore timeout deadline))
                   (ensure-equal peer-stream stream)
                   (let ((entry (pop (http3-test-stream-reads stream))))
                     (if entry
                         (values (first entry) (second entry))
                         (values nil t))))
                 (close-stream (stream &key condition)
                   (declare (ignore condition))
                   (push stream closed-streams)
                   (setf (http3-test-stream-closed-p stream) t)))
          (let ((client
                  (http-kit/http3:make-http3-client
                   :open-stream #'open-stream
                   :write-stream #'write-stream
                   :read-stream #'read-stream
                   :close-stream #'close-stream)))
            (ensure-equal client
                          (http-kit/http3:attach-http3-peer-control-stream
                           client peer-stream))
            (signals http-protocol-error
              (http-kit/http3:attach-http3-peer-control-stream
               client
               (make-http3-test-stream :kind :duplicate-peer-control)))
            (multiple-value-bind (events ended-p)
                (http-kit/http3:read-http3-control-stream client)
              (ensure-equal '() events)
              (ensure-equal nil ended-p))
            (multiple-value-bind (events ended-p)
                (http-kit/http3:read-http3-control-stream client)
              (ensure-equal '(:settings :goaway :max-push-id :cancel-push)
                            events)
              (ensure-true ended-p))
            (let ((state (http-kit/http3:http3-client-peer-control-state client)))
              (ensure-equal 7
                            (http-kit/http3:http3-control-state-goaway-id state))
              (ensure-equal 3
                            (http-kit/http3:http3-control-state-max-push-id state))
              (ensure-true
               (http-kit/http3:http3-control-state-settings-received-p state)))
            (ensure-true (http-kit/http3:http3-client-peer-control-fin-p client))
            (ensure-true (http-kit/http3:close-http3-client client))
            (ensure-true (every #'http3-test-stream-closed-p
                                (append opened-streams (list peer-stream))))))))))

(deftest http3-qpack-static-field-sections
  (let* ((fields (list (cons ":method" "GET")
                       (cons ":scheme" "https")
                       (cons ":authority" "example.test")
                       (cons ":path" "/resource")
                       (cons "accept" "text/plain")))
         (encoded (http-kit/http3:qpack-encode-field-section fields))
         (decoded (http-kit/http3:qpack-decode-field-section encoded)))
    (ensure-equal '(unsigned-byte 8)
                  (array-element-type encoded)
                  "QPACK sections are binary octet vectors")
    (ensure-equal fields decoded))
  (signals http-protocol-error
    (http-kit/http3:qpack-decode-field-section
     (octets #x01 #x00)))
  (signals http-size-limit-exceeded
    (http-kit/http3:qpack-decode-field-section
     (http-kit/http3:qpack-encode-field-section
      (list (cons "x-test" "value")))
     :max-header-bytes 1)))

(deftest http3-qpack-dynamic-table-and-streams
  (let* ((encoder-table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256
            :capacity 0))
         (decoder-table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256
            :capacity 0))
         (encoder-stream
           (http3-test-concat-octets
            (http-kit/http3:qpack-encode-set-dynamic-table-capacity 256)
            (http-kit/http3:qpack-encode-insert-with-literal-name
             "x-request-id" "one")
            (http-kit/http3:qpack-encode-insert-with-name-reference
             0 "two" :static-p nil)
            (http-kit/http3:qpack-encode-duplicate 0)
            (http-kit/http3:qpack-encode-insert-with-name-reference
             17 "PATCH")))
         (fields (list (cons ":method" "PATCH")
                       (cons "x-request-id" "two"))))
    (multiple-value-bind (events consumed)
        (http-kit/http3:qpack-process-encoder-stream
         encoder-table encoder-stream)
      (ensure-equal 5 (length events))
      (ensure-equal (length encoder-stream) consumed))
    (multiple-value-bind (events consumed)
        (http-kit/http3:qpack-process-encoder-stream
         decoder-table encoder-stream)
      (ensure-equal 5 (length events))
      (ensure-equal (length encoder-stream) consumed))
    (ensure-equal 4
                  (http-kit/http3:qpack-dynamic-table-insert-count
                   encoder-table))
    (ensure-equal 4
                  (length (http-kit/http3:qpack-dynamic-table-entries
                           encoder-table)))
    (let ((encoded
            (http-kit/http3:qpack-encode-field-section
             fields :dynamic-table encoder-table)))
      (ensure-equal fields
                    (http-kit/http3:qpack-decode-field-section
                     encoded :dynamic-table decoder-table))
      (signals http-protocol-error
        (http-kit/http3:qpack-decode-field-section
         encoded
         :dynamic-table
         (http-kit/http3:make-qpack-dynamic-table
          :max-capacity 256
          :capacity 256)))))
  (let ((table
          (http-kit/http3:make-qpack-dynamic-table
           :max-capacity 64)))
    (http-kit/http3:qpack-dynamic-table-insert table "a" "b")
    (http-kit/http3:qpack-dynamic-table-insert table "c" "d")
    (ensure-equal 1
                  (length (http-kit/http3:qpack-dynamic-table-entries table)))
    (ensure-equal "c"
                  (http-kit/http3:qpack-dynamic-entry-name
                   (first (http-kit/http3:qpack-dynamic-table-entries table))))
    (signals http-protocol-error
      (http-kit/http3:qpack-dynamic-table-insert table
                                                  "this-name-is-definitely-too-large-for-the-table"
                                                  "value")))
  (let ((acknowledged nil)
        (cancelled nil)
        (increments nil))
    (multiple-value-bind (events consumed)
        (http-kit/http3:qpack-process-decoder-stream
         (http3-test-concat-octets
          (http-kit/http3:qpack-encode-section-acknowledgment 7)
          (http-kit/http3:qpack-encode-stream-cancellation 9)
          (http-kit/http3:qpack-encode-insert-count-increment 3))
         :on-section-acknowledgment
         (lambda (stream-id) (push stream-id acknowledged))
         :on-stream-cancellation
         (lambda (stream-id) (push stream-id cancelled))
         :on-insert-count-increment
         (lambda (increment) (push increment increments)))
      (ensure-equal '((:section-acknowledgment 7)
                      (:stream-cancellation 9)
                      (:insert-count-increment 3))
                    events)
      (ensure-equal 3 consumed))
    (ensure-equal '(7) acknowledged)
    (ensure-equal '(9) cancelled)
    (ensure-equal '(3) increments)))

(deftest http3-qpack-huffman-encoding
  (let* ((input (octets 119 119 119 46 101 120 97 109 112 108 101 46 99 111 109))
         (encoded (http-kit/http2::%hpack-huffman-encode input)))
    (ensure-equal (octets #xf1 #xe3 #xc2 #xe5 #xf2 #x3a
                          #x6b #xa0 #xab #x90 #xf4 #xff)
                  encoded)
    (ensure-equal input
                  (http-kit/http2::%hpack-huffman-decode encoded)))
  (let* ((table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256
            :capacity 256))
         (encoder-stream
           (http-kit/http3:qpack-encode-insert-with-literal-name
            "x-huffman" "www.example.com" :huffman-p t)))
    (multiple-value-bind (events consumed)
        (http-kit/http3:qpack-process-encoder-stream table encoder-stream)
      (ensure-equal 1 (length events))
      (ensure-equal (length encoder-stream) consumed))
    (let ((entry (first (http-kit/http3:qpack-dynamic-table-entries table))))
      (ensure-equal "x-huffman"
                    (http-kit/http3:qpack-dynamic-entry-name entry))
      (ensure-equal "www.example.com"
                    (http-kit/http3:qpack-dynamic-entry-value entry))))
  (let* ((fields (list (cons "x-huffman" "www.example.com")
                       (cons "accept" "text/plain")))
         (encoded (http-kit/http3:qpack-encode-field-section
                   fields :huffman-p t)))
    (ensure-equal fields
                  (http-kit/http3:qpack-decode-field-section encoded))))

(deftest http3-injected-quic-request-response
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
        (let ((response
                (http-kit/http3:send-http3-request
                 client
                 (make-http-request
                  :method "POST"
                  :uri "https://example.test/resource?q=1"
                  :headers (list (make-http-header "x-test" "yes"))
                  :body (octets 65 66))
                 :collect-body-p nil
                 :qpack-encoder-table request-table
                 :qpack-decoder-table response-table
                 :huffman-p t
                 :on-body-chunk
                 (lambda (chunk)
                   (push (subseq chunk 0) body-chunks)))))
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

(deftest http3-server-request-response
  (let* ((request-fields
           (list (cons ":method" "POST")
                 (cons ":scheme" "https")
                 (cons ":authority" "example.test")
                 (cons ":path" "/upload?q=1")
                 (cons "content-length" "5")
                 (cons "x-test" "yes")))
         (request-headers
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-headers-type+
             :payload
             (http-kit/http3:qpack-encode-field-section
              request-fields
              :huffman-p t))))
         (request-data
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-data-type+
             :payload (octets 104 101 108 108 111))))
         (request-trailers
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-headers-type+
             :payload
             (http-kit/http3:qpack-encode-field-section
              (list (cons "x-request-trailer" "done"))
              :huffman-p t))))
         (wire (http3-test-concat-octets request-headers
                                         request-data
                                         request-trailers))
         (reads
           (loop for start from 0 below (length wire) by 3
                 for end = (min (length wire) (+ start 3))
                 collect (list (subseq wire start end)
                               (= end (length wire)))))
         (stream (make-http3-test-stream :kind :request :reads reads))
         (writes nil)
         (body-chunks nil)
         (seen-request nil)
         (closed-condition :unset))
    (labels ((read-stream (ignored-stream &key timeout deadline)
               (declare (ignore ignored-stream timeout deadline))
               (let ((entry (pop (http3-test-stream-reads stream))))
                 (if entry
                     (values (first entry) (second entry))
                     (values nil t))))
             (write-stream (ignored-stream octets &key fin-p timeout deadline)
               (declare (ignore ignored-stream timeout deadline))
               (push (list (subseq octets 0) fin-p) writes))
             (close-stream (ignored-stream &key condition)
               (declare (ignore ignored-stream))
               (setf closed-condition condition)))
      (let ((response
              (http-kit/http3:serve-http3-request-stream
               stream
               (lambda (request)
                 (setf seen-request request)
                 (http-kit:make-http-response
                  :status 201
                  :headers (list (http-kit:make-http-header
                                  "content-type" "text/plain")
                                 (http-kit:make-http-header
                                  "content-length" "2"))
                  :trailers (list (http-kit:make-http-header
                                   "x-response-trailer" "done") )
                  :body (octets 111 107)
                  :protocol-version "HTTP/3"))
               :read-stream #'read-stream
               :write-stream #'write-stream
               :close-stream #'close-stream
               :huffman-p t
               :on-body-chunk
               (lambda (chunk)
                 (push (subseq chunk 0) body-chunks)))))
        (ensure-true (http-response-p response))
        (ensure-equal 201 (http-response-status response)))
      (ensure-true (http-request-p seen-request))
      (ensure-equal "POST" (http-request-method seen-request))
      (ensure-equal "HTTP/3" (http-request-protocol-version seen-request))
      (ensure-equal "https" (http-uri-scheme
                              (http-request-uri seen-request)))
      (ensure-equal "example.test" (http-request-authority seen-request))
      (ensure-equal "/upload?q=1" (http-request-target seen-request))
      (ensure-equal (octets 104 101 108 108 111)
                    (http-request-body seen-request))
      (ensure-equal "yes"
                    (http-header-value (http-request-headers seen-request)
                                       "x-test"))
      (ensure-equal "done"
                    (http-header-value (http-request-trailers seen-request)
                                       "x-request-trailer"))
      (ensure-equal (octets 104 101 108 108 111)
                    (apply #'http3-test-concat-octets
                           (nreverse body-chunks)))
      (ensure-equal nil closed-condition)
      (let* ((response-writes (nreverse writes))
             (response-wire
               (apply #'http3-test-concat-octets
                      (mapcar #'first response-writes))))
        (ensure-equal '(nil nil t)
                      (mapcar #'second response-writes))
        (multiple-value-bind (frames remainder)
            (http-kit/http3:decode-http3-frames response-wire)
          (ensure-equal 0 (length remainder))
          (ensure-equal 3 (length frames))
          (ensure-equal
           (list (cons ":status" "201")
                 (cons "content-type" "text/plain")
                 (cons "content-length" "2"))
           (http-kit/http3:qpack-decode-field-section
            (http-kit/http3:http3-frame-payload (first frames))))
          (ensure-equal (octets 111 107)
                        (http-kit/http3:http3-frame-payload
                         (second frames)))
          (ensure-equal
           (list (cons "x-response-trailer" "done"))
           (http-kit/http3:qpack-decode-field-section
            (http-kit/http3:http3-frame-payload (third frames)))))))))

(deftest http3-server-body-limit-closes-stream
  (let* ((request-fields
           (list (cons ":method" "POST")
                 (cons ":scheme" "https")
                 (cons ":authority" "example.test")
                 (cons ":path" "/")
                 (cons "content-length" "5")))
         (wire
           (http3-test-concat-octets
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-headers-type+
              :payload
              (http-kit/http3:qpack-encode-field-section request-fields)))
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-data-type+
              :payload (octets 104 101 108 108 111)))))
         (stream (make-http3-test-stream
                  :kind :request
                  :reads (list (list wire t))))
         (seen-error nil)
         (closed-condition :unset))
    (labels ((read-stream (ignored-stream &key timeout deadline)
               (declare (ignore ignored-stream timeout deadline))
               (let ((entry (pop (http3-test-stream-reads stream))))
                 (if entry
                     (values (first entry) (second entry))
                     (values nil t))))
             (write-stream (ignored-stream octets &key fin-p timeout deadline)
               (declare (ignore ignored-stream octets fin-p timeout deadline))
               (error "The handler must not write after a request-body limit error."))
             (close-stream (ignored-stream &key condition)
               (declare (ignore ignored-stream))
               (setf closed-condition condition)))
      (signals http-size-limit-exceeded
        (http-kit/http3:serve-http3-request-stream
         stream
         (lambda (request)
           (declare (ignore request))
           (error "The handler must not run after a request-body limit error."))
         :read-stream #'read-stream
         :write-stream #'write-stream
         :close-stream #'close-stream
         :max-body-bytes 4
         :on-error (lambda (condition request)
                     (setf seen-error (list condition request)))))
      (ensure-true (consp seen-error))
      (ensure-true (typep (first seen-error) 'http-size-limit-exceeded))
      (ensure-equal nil (second seen-error))
      (ensure-true (typep closed-condition 'http-size-limit-exceeded)))))
