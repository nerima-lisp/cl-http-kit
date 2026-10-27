(in-package #:http-kit/test)

(deftest http3-defensive-qpack-section-ownership
  (let* ((context (http-message-kit:make-qpack-context
                   :max-table-capacity 34 :table-capacity 34
                   :max-blocked-streams 1 :huffman nil))
         (headers (list (http-message-kit:make-http-header "a" "1"))))
    (http-message-kit:qpack-encode-insert-with-literal-name
     context "a" "1" :huffman nil)
    (http-message-kit:qpack-encode-header-block
     headers :context context :use-dynamic-p t :stream-id 4 :huffman nil)
    (let ((failure
            (handler-case
                (http-message-kit:qpack-context-set-capacity context 0)
              (http-protocol-error (condition) condition))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :qpack (http-error-operation failure))
      (ensure-equal "QPACK cannot evict an entry referenced by a field section."
                    (http-error-message failure))
      (ensure-equal 0 (http-protocol-error-detail failure))
      (ensure-equal 34 (http-message-kit:qpack-context-table-capacity context))
      (ensure-equal 34 (http-message-kit:qpack-context-table-size context)))
    (let* ((wire (http-message-kit:qpack-encode-section-ack 4))
           (events nil)
           (used nil))
      (multiple-value-setq
          (events used)
        (http-message-kit:qpack-decode-decoder-stream context wire))
      (ensure-equal '((:section-ack 4)) events)
      (ensure-equal (length wire) used))
    (let* ((wire (http-message-kit:qpack-encode-section-ack 4))
           (events nil)
           (used nil)
           (known-received-count
             (http-message-kit:qpack-context-known-received-count context))
           (failure nil))
      (handler-case
          (multiple-value-setq
              (events used)
            (http-message-kit:qpack-decode-decoder-stream context wire))
        (http-protocol-error (condition)
          (setf failure condition)))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :qpack (http-error-operation failure))
      (ensure-equal
       "QPACK decoder stream section acknowledgment has no outstanding field section."
       (http-error-message failure))
      (ensure-equal
       '(:qpack-decoder-stream-error :section-acknowledgment 4)
       (http-protocol-error-detail failure))
      (ensure-equal nil events)
      (ensure-equal nil used)
      (ensure-equal known-received-count
                    (http-message-kit:qpack-context-known-received-count context)))
    (let ((wire (http-message-kit:qpack-encode-stream-cancellation 4)))
      (multiple-value-bind (events used)
          (http-message-kit:qpack-decode-decoder-stream context wire)
        (ensure-equal '((:stream-cancellation 4)) events)
        (ensure-equal (length wire) used)))
    (ensure-equal 1 (http-message-kit:qpack-context-insert-count context))
    (ensure-equal 1 (http-message-kit:qpack-context-known-received-count context))
    (http-message-kit:qpack-context-set-capacity context 0)
    (ensure-equal 0 (http-message-kit:qpack-context-table-size context))
    (ensure-equal 0 (http-message-kit:qpack-context-dynamic-entry-count context))))

(deftest http3-codec-push-priority-requires-max-push-id
  (let* ((state (http-kit/http3:make-http3-control-state
                 :peer-role :client :promised-push-ids '(0)))
         (frame (http-kit/http3:make-http3-priority-update-frame
                 :element-id 0 :priority-field-value "u=1" :kind :push)))
    (ensure-equal :settings
                  (http-kit/http3:process-http3-control-frame
                   state (http-kit/http3:make-http3-settings-frame)))
    (let ((failure
            (handler-case
                (http-kit/http3:process-http3-control-frame state frame)
              (http-protocol-error (condition) condition))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :http3-control-stream (http-error-operation failure))
      (ensure-equal :h3-id-error (http-protocol-error-detail failure)))
    (ensure-equal nil (http-kit/http3:http3-control-state-max-push-id state))
    (ensure-equal nil (http-kit/http3:http3-control-state-priority-updates state))
    (ensure-equal '(0) (http-kit/http3:http3-control-state-promised-push-ids state))
    (ensure-equal :max-push-id
                  (http-kit/http3:process-http3-control-frame
                   state (http-kit/http3:make-http3-frame
                          :type http-kit/http3:+http3-max-push-id-type+
                          :payload (http-kit/http3:http3-varint-encode 0))))
    (ensure-equal :priority-update
                  (http-kit/http3:process-http3-control-frame state frame))
    (ensure-equal '(((:push . 0) . "u=1"))
                  (http-kit/http3:http3-control-state-priority-updates state))
    (ensure-equal :priority-update
                  (http-kit/http3:process-http3-control-frame
                   state (http-kit/http3:make-http3-priority-update-frame
                          :element-id 0 :priority-field-value "u=7" :kind :push)))
    (ensure-equal '(((:push . 0) . "u=7"))
                  (http-kit/http3:http3-control-state-priority-updates state))))

(deftest http3-codec-qpack-nonstring-list-name-is-rejected
  (dolist (name '(nil 7 #\x))
    (let* ((field (list name "value"))
           (fields (list field))
           (before (copy-tree fields))
           (failure
             (handler-case
                 (http-message-kit:qpack-encode-header-block fields)
               (http-protocol-error (condition) condition))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :qpack (http-error-operation failure))
      (ensure-equal
       "QPACK fields must be HTTP-HEADER values or name/value pairs."
       (http-error-message failure))
      (ensure-equal field (http-protocol-error-detail failure))
      (ensure-equal before fields)))
  (let* ((fields '(("x-name" "value")))
         (wire (http-message-kit:qpack-encode-header-block fields)))
    (ensure-equal '(("x-name" . "value"))
                  (mapcar (lambda (header)
                            (cons (http-message-kit:http-header-name header)
                                  (http-message-kit:http-header-content header)))
                          (http-message-kit:qpack-decode-header-block wire)))
    (ensure-equal '(("x-name" "value")) fields)))

(deftest http3-codec-qpack-public-blocked-nil-stream-id
  (let* ((context (http-message-kit:make-qpack-context
                   :max-table-capacity 0 :table-capacity 0
                   :max-blocked-streams 1 :huffman nil))
         (adapter (http-message-kit:qpack-make-http3-decoder-context
                   context :max-blocked-streams 1))
         (serializations 0)
         (acknowledgments 0)
         (failure
           (handler-case
               (http-message-kit:qpack-decode-http3-field-section
                (octets 0 0) adapter nil :role :trailers
                :serialize
                (lambda (thunk)
                  (incf serializations)
                  (funcall thunk))
                :on-section-acknowledgment
                (lambda (stream-id)
                  (declare (ignore stream-id))
                  (incf acknowledgments)))
             (http-protocol-error (condition) condition))))
    (ensure-true (typep failure 'http-protocol-error))
    (ensure-equal :qpack (http-error-operation failure))
    (ensure-equal "QPACK stream IDs must be between zero and 2^62-1."
                  (http-error-message failure))
    (ensure-equal nil (http-protocol-error-detail failure))
    (ensure-equal 0 serializations)
    (ensure-equal 0 acknowledgments)))

(deftest http3-codec-qpack-public-post-base-without-table
  (let ((failure
          (handler-case
              (http-message-kit:qpack-decode-header-block
               (octets 0 0 #x10))
            (http-protocol-error (condition) condition))))
    (ensure-true (typep failure 'http-protocol-error))
    (ensure-equal :qpack (http-error-operation failure))
    (ensure-equal
     "QPACK field section references an entry at or after its required count."
     (http-error-message failure))
    (ensure-equal 0 (http-protocol-error-detail failure))))

(deftest http3-codec-qpack-rejects-unavailable-dynamic-entries
  (let ((failure
          (handler-case
              (http-message-kit:qpack-decode-header-block
               (octets #x02 0 #x80))
            (http-protocol-error (condition) condition))))
    (ensure-true (typep failure 'http-protocol-error))
    (ensure-equal :qpack (http-error-operation failure))
    (ensure-equal
     "A non-zero QPACK required insert count needs dynamic-table capacity."
     (http-error-message failure))
    (ensure-equal 2 (http-protocol-error-detail failure)))
  (let* ((context (http-message-kit:make-qpack-context
                   :max-table-capacity 34 :table-capacity 0 :huffman nil))
         (blocked
           (handler-case
               (http-message-kit:qpack-decode-header-block
                (octets #x02 0 #x80) :context context)
             (http-message-kit:http-qpack-blocked (condition) condition))))
    (ensure-true (typep blocked 'http-message-kit:http-qpack-blocked))
    (ensure-equal :qpack (http-error-operation blocked))
    (ensure-equal
     "QPACK field section is blocked on dynamic-table insertions."
     (http-error-message blocked))
    (ensure-equal 1
                  (http-message-kit:http-qpack-blocked-required-insert-count
                   blocked))
    (ensure-equal 0
                  (http-message-kit:http-qpack-blocked-available-insert-count
                   blocked)))
  (let ((context (http-message-kit:make-qpack-context
                  :max-table-capacity 34 :table-capacity 34 :huffman nil)))
    (http-message-kit:qpack-encode-insert-with-literal-name
     context "a" "1" :huffman nil)
    (ensure-equal '(("a" . "1"))
                  (mapcar (lambda (header)
                            (cons (http-message-kit:http-header-name header)
                                  (http-message-kit:http-header-content header)))
                          (http-message-kit:qpack-decode-header-block
                           (octets #x02 0 #x80) :context context)))
    (http-message-kit:qpack-encode-insert-with-literal-name
     context "b" "2" :huffman nil)
    (ensure-equal '(("b" . "2"))
                  (mapcar (lambda (header)
                            (cons (http-message-kit:http-header-name header)
                                  (http-message-kit:http-header-content header)))
                          (http-message-kit:qpack-decode-header-block
                           (octets #x01 0 #x80) :context context)))
    (let ((failure
            (handler-case
                (http-message-kit:qpack-decode-header-block
                 (octets #x01 0 #x81) :context context)
              (http-protocol-error (condition) condition))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :qpack (http-error-operation failure))
      (ensure-equal
       "QPACK dynamic-table absolute index is unavailable."
       (http-error-message failure))
      (ensure-equal 0 (http-protocol-error-detail failure))
      (ensure-equal 2 (http-message-kit:qpack-context-insert-count context))
      (ensure-equal 1 (http-message-kit:qpack-context-dynamic-entry-count context))
      (ensure-equal 34 (http-message-kit:qpack-context-table-size context)))))

(deftest http3-codec-qpack-sensitive-name-search-skips-unrelated-entries
  (dolist (huffman '(nil t))
    (dolist (matching-name '(nil t))
      (let* ((fields (list (make-http-header
                            "proxy-authorization" "replacement")))
             (context (http-message-kit:make-qpack-context
                       :max-table-capacity 256 :table-capacity 256
                       :max-blocked-streams 1 :huffman huffman)))
        (when matching-name
          (http-message-kit:qpack-encode-insert-with-literal-name
           context "proxy-authorization" "previous" :huffman nil))
        (http-message-kit:qpack-encode-insert-with-literal-name
         context "x-unrelated" "value" :huffman nil)
        (let* ((before-size (http-message-kit:qpack-context-table-size context))
               (before-count (http-message-kit:qpack-context-insert-count context))
               (wire (http-message-kit:qpack-encode-header-block
                      fields :context context :use-dynamic-p t :stream-id 4
                      :huffman huffman))
               (field-line (aref wire 2)))
          (ensure-equal (if matching-name 2 0) (aref wire 0))
          (ensure-equal (if matching-name 1 0) (aref wire 1))
          (ensure-equal (if matching-name #x60 #x30)
                        (logand field-line #xf0))
          (ensure-equal
           (list (cons "proxy-authorization" "replacement"))
           (mapcar (lambda (header)
                     (cons (http-message-kit:http-header-name header)
                           (http-message-kit:http-header-content header)))
                   (http-message-kit:qpack-decode-header-block
                    wire :context context)))
          (ensure-equal before-size
                        (http-message-kit:qpack-context-table-size context))
          (ensure-equal before-count
                        (http-message-kit:qpack-context-insert-count context))
          (ensure-equal (if matching-name 2 1)
                        (http-message-kit:qpack-context-dynamic-entry-count
                         context))
          (ensure-equal "proxy-authorization"
                        (http-message-kit:http-header-name (first fields)))
          (ensure-equal "replacement"
                        (http-message-kit:http-header-content (first fields))))))))

(deftest http3-codec-qpack-required-count-prefix-boundaries
  (dolist (capacity '(0 1 31))
    (let* ((context (http-message-kit:make-qpack-context
                     :max-table-capacity capacity :table-capacity capacity
                     :max-blocked-streams 1 :huffman nil))
           (wire (http-message-kit:qpack-encode-header-block
                  (list (http-message-kit:make-http-header "x" "static"))
                  :context context :use-dynamic-p t :stream-id 4 :huffman nil)))
      (ensure-equal 0 (aref wire 0))
      (ensure-equal 0 (http-message-kit:qpack-context-insert-count context))))
  (dolist (case '((1 128 2) (7 128 8) (8 128 1) (9 128 2)))
    (destructuring-bind (count capacity expected) case
      (let ((context (http-message-kit:make-qpack-context
                      :max-table-capacity capacity :table-capacity capacity
                      :max-blocked-streams 1 :huffman nil)))
        (loop for index from 1 to count
              do (http-message-kit:qpack-encode-insert-with-literal-name
                  context "x" (format nil "v~D" index) :huffman nil))
        (let* ((headers (list (http-message-kit:make-http-header
                               "x" (format nil "v~D" count))))
               (wire (http-message-kit:qpack-encode-header-block
                      headers :context context :use-dynamic-p t
                      :stream-id 4 :huffman nil)))
          (ensure-equal expected (aref wire 0))
          (multiple-value-bind (decoded used required base)
              (http-message-kit:qpack-decode-header-block
               wire :context context)
            (ensure-equal headers decoded)
            (ensure-equal (length wire) used)
            (ensure-equal count required)
            (ensure-equal count base))
          (ensure-equal count (http-message-kit:qpack-context-insert-count
                               context)))))))

(defun http3-codec-response-frame-check (wire forbidden-type)
  (let* ((before (copy-seq wire))
         (fixture (http3-qpack-contract-fixture wire 0))
         (client (http3-qpack-contract-fixture-client fixture))
         (request (make-http-request :method "GET" :uri "https://example.test/"))
         (request-before (http3-qpack-contract-request-source request))
         (failure nil)
         (response nil))
    (unwind-protect
         (progn
           (handler-case
               (setf response (http-kit/http3:send-http3-request client request))
             (http-protocol-error (condition) (setf failure condition)))
           (if forbidden-type
               (progn
                 (http3-public-error-check
                  failure "HTTP/3 control frames are not valid on a request stream."
                  forbidden-type)
                 (ensure-true (eq nil response)))
               (progn
                 (ensure-true (eq nil failure))
                 (ensure-equal 200 (http-response-status response))
                 (ensure-octets-equal (ascii "ok") (http-response-body response))
                 (ensure-equal '(("x-end" . "yes"))
                               (mapcar (lambda (header)
                                         (cons (http-header-name header)
                                               (http-header-content header)))
                                       (http-response-trailers response)))))
           (let* ((events (reverse (http3-qpack-contract-fixture-events fixture)))
                  (stream (second (first events)))
                  (last-event (car (last events))))
             (ensure-equal '(:open :write :write :read :close) (mapcar #'first events))
             (ensure-true (eq request (third (first events))))
             (ensure-equal :request (http3-test-stream-kind stream))
             (dolist (event events) (ensure-true (eq stream (second event))))
             (ensure-equal '(nil t) (mapcar #'second (reverse (http3-test-stream-writes stream))))
             (ensure-equal nil (http3-test-stream-reads stream))
             (ensure-true (http3-test-stream-closed-p stream))
             (ensure-true (eq failure (third last-event))))
           (ensure-true (http-kit/http3:http3-client-open-p client))
           (ensure-equal request-before (http3-qpack-contract-request-source request))
           (ensure-octets-equal before wire))
      (http3-qpack-contract-cleanup fixture))))

(deftest http3-codec-response-frame-extensions-preserve-response
  (dolist (type '(33 57005))
    (let ((extension (http-kit/http3:encode-http3-frame
                      (http-kit/http3:make-http3-frame :type type :payload (octets 0 255 1)))))
      (http3-codec-response-frame-check
       (http3-test-concat-octets
        extension (http3-boundary-headers '((":status" . "200")))
        extension (http3-boundary-data "ok")
        extension (http3-boundary-headers '(("x-end" . "yes"))) extension)
       nil))))

(deftest http3-codec-response-frame-controls-are-rejected
  (dolist (type (list http-kit/http3:+http3-settings-type+
                     http-kit/http3:+http3-cancel-push-type+
                     http-kit/http3:+http3-goaway-type+
                     http-kit/http3:+http3-max-push-id-type+
                     http-kit/http3:+http3-priority-update-request-type+
                     http-kit/http3:+http3-priority-update-push-type+))
    (dolist (after-headers '(nil t))
      (let ((control (http-kit/http3:encode-http3-frame
                      (http-kit/http3:make-http3-frame :type type :payload (octets 0)))))
        (http3-codec-response-frame-check
         (if after-headers
             (http3-test-concat-octets (http3-boundary-headers '((":status" . "200"))) control)
             control)
         type)))))

(defun http3-codec-server-serializer-check
    (trailers-p replacement error-kind &optional reason expected-operation)
  (let* ((fields '((":method" . "GET") (":scheme" . "https")
                  (":authority" . "example.test") (":path" . "/")
                  ("x-before" . "kept")
                  ("x-check" . "ok")))
         (wire (if trailers-p
                   (http3-test-concat-octets
                    (http3-boundary-headers fields)
                    (http3-boundary-headers '(("x-check" . "ok"))))
                   (http3-boundary-headers fields)))
         (before (copy-seq wire))
         (context (http-message-kit:make-qpack-context
                   :max-table-capacity 0 :table-capacity 0
                   :max-blocked-streams 0 :huffman nil))
         (adapter (http-message-kit:qpack-make-http3-decoder-context
                   context :max-blocked-streams 0))
         (stream (make-http3-test-stream :kind :request))
         (reads 0) (serializations 0) (acknowledgments 0)
         (calls 0) (writes nil)
         (closed nil) (errors nil) (response nil) (failure nil))
    (handler-case
        (setf response
              (http-kit/http3:serve-http3-request-stream
               stream
               (lambda (request)
                 (incf calls)
                 (ensure-equal "GET" (http-request-method request))
                 (ensure-octets-equal (octets) (http-request-body request))
                 (ensure-equal (if trailers-p '(("x-check" . "ok")) nil)
                               (mapcar (lambda (header)
                                         (cons (http-header-name header)
                                               (http-header-content header)))
                                       (http-request-trailers request)))
                 (make-http-response :status 204))
               :qpack-decoder-context context
               :qpack-http3-decoder-context adapter
               :on-qpack-section-acknowledgment
               (lambda (stream-id)
                 (declare (ignore stream-id))
                 (incf acknowledgments))
               :stream-id 4
               :await-qpack (lambda (token)
                              (declare (ignore token))
                              (error "Static fields must not block."))
               :qpack-serialize
               (lambda (thunk)
                 (incf serializations)
                 (let ((decoded (funcall thunk)))
                   (ensure-equal '("x-check" . "ok") (car (last decoded)))
                   (when (and error-kind
                              (= serializations (if trailers-p 2 1)))
                     (setf (car (last decoded)) (copy-tree replacement)))
                   decoded))
               :read-stream
               (lambda (actual &key timeout deadline)
                 (declare (ignore timeout deadline))
                 (ensure-true (eq stream actual))
                 (incf reads)
                 (ensure-equal 1 reads)
                 (values wire t))
               :write-stream
               (lambda (actual bytes &key fin-p timeout deadline)
                 (declare (ignore timeout deadline))
                 (ensure-true (eq stream actual))
                 (push (list (copy-seq bytes) fin-p) writes))
               :close-stream
               (lambda (actual &key condition)
                 (ensure-true (eq stream actual))
                 (push condition closed))
               :on-error (lambda (condition request)
                           (push (list condition request) errors))))
      (http-protocol-error (condition) (setf failure condition)))
    (ensure-equal 1 reads)
    (ensure-equal (if trailers-p 2 1) serializations)
    (ensure-equal 0 acknowledgments)
    (ensure-equal 0 (http-message-kit:qpack-context-insert-count context))
    (ensure-octets-equal before wire)
    (ensure-equal 1 (length closed))
    (ensure-true (eq failure (first closed)))
    (if error-kind
        (progn
          (ensure-equal 0 calls)
          (ensure-equal nil response)
          (ensure-equal nil writes)
          (ensure-equal 1 (length errors))
          (ensure-true (eq failure (first (first errors))))
          (ensure-equal nil (second (first errors)))
          (cond
           ((eq error-kind :header)
            (progn
              (ensure-true (typep failure 'http-invalid-header))
              (ensure-equal expected-operation (http-error-operation failure))
              (ensure-equal (car replacement) (http-invalid-header-name failure))
              (ensure-equal reason (http-invalid-header-reason failure))))
           ((eq error-kind :pair)
            (progn
              (ensure-true (typep failure 'http-invalid-header))
              (ensure-equal :header (http-error-operation failure))
              (ensure-equal :type (http-invalid-header-reason failure))))
           ((eq error-kind :order)
            (progn
              (ensure-true (typep failure 'http-invalid-header))
              (ensure-equal expected-operation (http-error-operation failure))
              (ensure-equal (car replacement) (http-invalid-header-name failure))
              (ensure-equal reason (http-invalid-header-reason failure))))
           (t (http3-public-error-check failure reason (car replacement)))))
        (progn
          (ensure-equal nil failure)
          (ensure-equal nil errors)
          (ensure-equal 1 calls)
          (ensure-equal 204 (http-response-status response))
          (ensure-equal '(t) (mapcar #'second writes))
          (let* ((frames (http3-server-coverage-frames (nreverse writes)))
                 (headers
                   (http-message-kit:qpack-decode-header-block
                    (http-kit/http3:http3-frame-payload (first frames)))))
            (ensure-equal '(1) (mapcar #'http-kit/http3:http3-frame-type frames))
            (ensure-equal '((":status" . "204"))
                          (mapcar (lambda (header)
                                    (cons (http-message-kit:http-header-name header)
                                          (http-message-kit:http-header-content header)))
                                  headers)))))))

(deftest http3-codec-server-serializer-rejects-malformed-pairs
  (dolist (trailers-p '(nil t))
    (http3-codec-server-serializer-check trailers-p nil nil)
    (dolist (field '(nil (7 . "ok") ("x-check" . 7)))
      (http3-codec-server-serializer-check
       trailers-p field :pair
       (if trailers-p
           "HTTP/3 trailer fields must be name/value pairs."
           "HTTP/3 request fields must be name/value pairs.")))))

(deftest http3-codec-server-serializer-revalidates-fields
  (dolist (trailers-p '(nil t))
    (http3-codec-server-serializer-check trailers-p nil nil)
    (dolist (row '(("" :name :header)
                   ("X-check" :uppercase-name :http/3)
                   ("x check" :name :header)))
      (destructuring-bind (name expected-reason expected-operation) row
        (http3-codec-server-serializer-check
         trailers-p (cons name "ok") :header
         expected-reason expected-operation))))
  (http3-codec-server-serializer-check
   nil (cons "x-check" (string #\Newline)) :header :control :header)
  (http3-codec-server-serializer-check
   nil '(":method" . "POST") :order
   :pseudo-after-regular :http/3))

(defun http3-codec-client-serializer-check (replacement error-kind)
  (let* ((fields '((":status" . "204") ("x-before" . "kept")
                   ("x-check" . "ok")))
         (wire (http3-boundary-headers fields))
         (before (copy-seq wire))
         (request (make-http-request :method "GET" :uri "https://example.test/"))
         (source (http3-qpack-contract-request-source request))
         (request-stream nil) (reads 0) (decoded-count 0) (closes nil)
         (response nil) (failure nil)
         (client
           (http-kit/http3:make-http3-client
            :serialize
            (lambda (thunk)
              (let ((decoded (funcall thunk)))
                (when (equal decoded fields)
                  (incf decoded-count)
                  (when error-kind
                    (setf (car (last decoded)) (copy-tree replacement))))
                decoded))
            :open-stream
            (lambda (actual &key stream-type timeout deadline)
              (declare (ignore timeout deadline))
              (let ((stream (make-http3-test-stream :kind stream-type)))
                (when (eq stream-type :request)
                  (ensure-true (eq request actual))
                  (setf request-stream stream))
                (values stream 0)))
            :write-stream
            (lambda (stream bytes &key fin-p timeout deadline)
              (declare (ignore timeout deadline))
              (push (list (copy-seq bytes) fin-p)
                    (http3-test-stream-writes stream)))
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore timeout deadline))
              (ensure-true (eq request-stream stream))
              (incf reads)
              (ensure-equal 1 reads)
              (values wire t))
            :close-stream
            (lambda (stream &key condition)
              (setf (http3-test-stream-closed-p stream) t)
              (push (cons stream condition) closes)))))
    (unwind-protect
         (progn
           (handler-case
               (setf response
                     (http-kit/http3:send-http3-request
                        client request
                        :qpack-decoder-context
                        (http-kit/http3::http3-client-qpack-decoder-context
                         client)
                        :qpack-http3-decoder-context
                        (http-kit/http3::http3-client-qpack-http3-decoder-context
                         client)
                        :await-qpack (lambda (token)
                                       (declare (ignore token))
                                       (error "Static fields must not block."))))
             (http-protocol-error (condition) (setf failure condition)))
           (ensure-equal 1 reads)
           (ensure-equal 1 decoded-count)
           (ensure-equal 0
                         (http-message-kit:qpack-context-insert-count
                          (http-kit/http3::http3-client-qpack-decoder-context
                           client)))
           (ensure-octets-equal before wire)
           (ensure-equal source (http3-qpack-contract-request-source request))
           (ensure-equal 1 (length closes))
           (ensure-true (eq request-stream (caar closes)))
           (ensure-true (eq failure (cdar closes)))
           (ensure-true (http3-test-stream-closed-p request-stream))
           (ensure-equal '(nil t)
                         (mapcar #'second
                                 (reverse (http3-test-stream-writes request-stream))))
           (ensure-true (http-kit/http3:http3-client-open-p client))
           (cond ((eq error-kind :order)
                  (ensure-equal nil response)
                  (http3-public-error-check
                   failure "HTTP/3 pseudo-fields must precede regular fields."
                   ":status"))
                 ((eq error-kind :empty)
                  (ensure-equal nil response)
                  (ensure-true (typep failure 'http-invalid-header))
                  (ensure-equal :header (http-error-operation failure))
                  (ensure-equal "" (http-invalid-header-name failure))
                  (ensure-equal :name (http-invalid-header-reason failure)))
                 (t
                  (ensure-equal nil failure)
                  (ensure-equal 204 (http-response-status response))
                  (ensure-octets-equal (octets) (http-response-body response))
                  (ensure-equal '(("x-before" . "kept") ("x-check" . "ok"))
                                (mapcar (lambda (header)
                                          (cons (http-header-name header)
                                                (http-header-content header)))
                                        (http-response-headers response))))))
      (http-kit/http3:close-http3-client client))))

(deftest http3-codec-client-serializer-revalidates-response-fields
  (http3-codec-client-serializer-check nil nil)
  (http3-codec-client-serializer-check '("" . "ok") :empty)
  (http3-codec-client-serializer-check '(":status" . "204") :order))

(deftest http3-codec-server-response-header-copies
  (dolist (stream-p '(nil t))
    (dolist (information-p '(nil t))
      (let* ((headers (list (make-http-header "x-copy" "kept")))
             (trailers (unless information-p
                         (list (make-http-header "x-trailer" "kept"))))
             (body-calls 0)
             (response
               (if stream-p
                   (make-http-response-stream
                    :status (if information-p 100 200)
                    :headers headers :trailers trailers
                    :body-function (lambda () (incf body-calls) nil))
                   (make-http-response
                    :status (if information-p 100 200)
                    :headers headers :trailers trailers)))
             (copies (if stream-p
                         (http-response-stream-headers response)
                         (http-response-headers response)))
             (trailer-copies (if stream-p
                                 (http-response-stream-trailers response)
                                 (http-response-trailers response)))
             (final (if information-p
                        (make-http-response :status 204)
                        response)))
        (ensure-true (not (eq headers copies)))
        (ensure-true (not (eq (first headers) (first copies))))
        (dolist (list (list headers copies trailers trailer-copies))
          (when list
            (setf (http-header-name (first list)) 7
                  (http-header-content (first list)) 7
                  (first list) 7)))
        (multiple-value-bind (failure actual writes)
            (http3-server-coverage-serve
             (lambda (request)
               (declare (ignore request))
               (values final (when information-p (list response)))))
          (ensure-equal nil failure)
          (ensure-true (eq final actual))
          (ensure-equal (if stream-p 1 0) body-calls)
          (ensure-equal '(nil t) (mapcar #'second writes))
          (let ((frames (http3-server-coverage-frames writes)))
            (ensure-equal '(1 1) (mapcar #'http-kit/http3:http3-frame-type frames))
            (ensure-equal
             (list (list (cons ":status" (if information-p "100" "200"))
                         '("x-copy" . "kept"))
                   (if information-p
                       '((":status" . "204"))
                       '(("x-trailer" . "kept"))))
             (mapcar (lambda (frame)
                       (mapcar (lambda (header)
                                 (cons (http-message-kit:http-header-name header)
                                       (http-message-kit:http-header-content header)))
                               (http-message-kit:qpack-decode-header-block
                                (http-kit/http3:http3-frame-payload frame))))
                     frames))))))))

(deftest http3-codec-server-response-revalidates-aliased-header-names
  (dolist (stream-p '(nil t))
    (dolist (information-p '(nil t))
      (let* ((name (copy-seq "x-copy"))
             (header (make-http-header name "kept"))
             (response
               (if stream-p
                   (make-http-response-stream
                    :status (if information-p 100 200) :headers (list header)
                    :body-function (lambda () nil))
                   (make-http-response
                    :status (if information-p 100 200) :headers (list header))))
             (final (if information-p
                        (make-http-response :status 204)
                        response)))
        (setf (char name 0) #\Space)
        (multiple-value-bind (failure actual writes)
            (http3-server-coverage-serve
             (lambda (request)
               (declare (ignore request))
               (values final (when information-p (list response)))))
          (ensure-true (typep failure 'http-invalid-header))
          (ensure-equal name (http-invalid-header-name failure))
          (ensure-equal :name (http-invalid-header-reason failure))
          (ensure-equal nil actual)
          (ensure-equal nil writes))))))
(deftest http3-codec-qpack-reregistered-stream-rejects-stale-token
  (multiple-value-bind (fields decoder-context section instruction)
      (http3-test-qpack-blocked-fixture)
    (let* ((adapter
             (http-message-kit:qpack-make-http3-decoder-context
              decoder-context :max-blocked-streams 1))
           (first-token
             (http-message-kit:qpack-decode-http3-field-section
              section adapter 0 :role :trailers))
           (second-token
             (http-message-kit:qpack-decode-http3-field-section
              section adapter 0 :role :trailers))
           (events nil)
           (consumed nil))
      (ensure-true (http-message-kit:qpack-blocked-stream-p first-token))
      (ensure-true (http-message-kit:qpack-blocked-stream-p second-token))
      (ensure-true (not (eq first-token second-token)))
      (multiple-value-setq (events consumed)
        (http-message-kit:qpack-decode-encoder-stream
         decoder-context instruction))
      (ensure-equal '((:set-capacity 256)
                      (:insert "x-blocked" "ready" 0 :huffman nil))
                    events)
      (ensure-equal (length instruction) consumed)
      (let ((ready (http-message-kit:qpack-take-ready-streams adapter)))
        (ensure-equal 1 (length ready))
        (ensure-true (eq second-token (first ready)))
        (let ((failure
                (handler-case
                    (progn
                      (http-message-kit:qpack-resume-blocked-stream first-token)
                      nil)
                  (http-message-kit:http-protocol-error (condition)
                    condition))))
          (ensure-true
           (typep failure 'http-message-kit:http-protocol-error))
          (ensure-equal
           :qpack (http-message-kit:http-error-operation failure))
          (ensure-equal
           "QPACK blocked-stream token is not resumable."
           (http-message-kit:http-error-message failure)))
        (ensure-equal
         fields
         (http-message-kit:qpack-resume-blocked-stream (first ready)))))))
(deftest http3-codec-qpack-failed-shrink-preserves-entry-identity-and-value
  (let ((context
          (http-message-kit:make-qpack-context
           :max-table-capacity 34
           :table-capacity 34
           :max-blocked-streams 1
           :huffman nil)))
    (http-message-kit:qpack-encode-insert-with-literal-name
     context "x" "y" :huffman nil)
    (let* ((section
             (http-message-kit:qpack-encode-header-block
              (list (http-message-kit:make-http-header "x" "y"))
              :context context :use-dynamic-p t :stream-id 4 :huffman nil))
           (same-first-entry-p
             (http-message-kit/test-support:make-qpack-first-dynamic-entry-identity-probe
              context))
           (failure
             (handler-case
                 (progn
                   (http-message-kit:qpack-context-set-capacity context 0)
                   nil)
               (http-protocol-error (condition) condition))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal
       "QPACK cannot evict an entry referenced by a field section."
       (http-error-message failure))
      (ensure-equal 34 (http-message-kit:qpack-context-table-capacity context))
      (ensure-equal 34 (http-message-kit:qpack-context-table-size context))
      (ensure-equal 1 (http-message-kit:qpack-context-dynamic-entry-count context))
      (ensure-true (funcall same-first-entry-p))
      (ensure-equal
       '(("x" . "y"))
       (mapcar
        (lambda (header)
          (cons (http-message-kit:http-header-name header)
                (http-message-kit:http-header-content header)))
        (http-message-kit:qpack-decode-header-block section :context context))))))
(deftest http3-codec-qpack-malformed-section-preserves-blocked-registration
  (multiple-value-bind (fields decoder-context section instruction)
      (http3-test-qpack-blocked-fixture)
    (let* ((adapter
             (http-message-kit:qpack-make-http3-decoder-context
              decoder-context :max-blocked-streams 1))
           (token
             (http-message-kit:qpack-decode-http3-field-section
              section adapter 0 :role :trailers))
           (failure
             (handler-case
                 (progn
                   (http-message-kit:qpack-decode-http3-field-section
                    (octets) adapter 0 :role :trailers)
                   nil)
               (http-message-kit:http-connection-error (condition)
                 condition))))
      (ensure-true (http-message-kit:qpack-blocked-stream-p token))
      (ensure-true
       (typep failure 'http-message-kit:http-connection-error))
      (ensure-equal :http3-transport (http-error-operation failure))
      (ensure-equal "HTTP/3 QPACK decompression failed."
                    (http-error-message failure))
      (ensure-equal nil (http-message-kit:qpack-take-ready-streams adapter))
      (ensure-equal
       fields
       (http3-test-decode-inserts-and-resume
        adapter decoder-context instruction token)))))
(deftest http3-codec-qpack-empty-table-rejects-negative-capacity
  (let ((context
          (http-message-kit:make-qpack-context
           :max-table-capacity 64 :table-capacity 0 :huffman nil)))
    (ensure-equal 0 (http-message-kit:qpack-context-table-size context))
    (ensure-equal 0 (http-message-kit:qpack-context-dynamic-entry-count context))
    (let ((failure
            (handler-case
                (progn
                  (http-message-kit:qpack-context-set-capacity context -1)
                  nil)
              (http-protocol-error (condition) condition))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :qpack (http-error-operation failure))
      (ensure-equal -1 (http-protocol-error-detail failure))
      (ensure-equal 0 (http-message-kit:qpack-context-table-capacity context))
      (ensure-equal 0 (http-message-kit:qpack-context-table-size context))
      (ensure-equal 0 (http-message-kit:qpack-context-insert-count context))
      (ensure-equal 0 (http-message-kit:qpack-context-dynamic-entry-count context)))))
(deftest http3-codec-client-rejects-non-numeric-stream-id-before-response-read
  (dolist (stream-id '(nil :not-numeric))
    (let* ((response-wire (http3-boundary-headers '((":status" . "204"))))
           (request (make-http-request :method "GET" :uri "https://example.test/"))
           (request-stream nil)
           (request-opens 0)
           (request-writes 0)
           (reads 0)
           (on-open-calls 0)
           (request-close-condition nil)
           (request-closes 0)
           (client
             (http-kit/http3:make-http3-client
              :qpack-settings
              (list
               (cons http-kit/http3:+http3-setting-qpack-max-table-capacity+ 256)
               (cons http-kit/http3:+http3-setting-qpack-blocked-streams+ 1))
              :open-stream
              (lambda (actual &key stream-type timeout deadline)
                (declare (ignore timeout deadline))
                (let ((stream (make-http3-test-stream :kind stream-type)))
                  (when (eq stream-type :request)
                    (ensure-true (eq request actual))
                    (incf request-opens)
                    (setf request-stream stream))
                  (values stream (when (eq stream-type :request) stream-id))))
              :write-stream
              (lambda (stream bytes &key fin-p timeout deadline)
                (declare (ignore bytes fin-p timeout deadline))
                (when (eq stream request-stream)
                  (incf request-writes)))
              :read-stream
              (lambda (stream &key timeout deadline)
                (declare (ignore stream timeout deadline))
                (incf reads)
                (values response-wire t))
              :close-stream
              (lambda (stream &key condition)
                (when (eq stream request-stream)
                  (incf request-closes)
                  (setf request-close-condition condition)
                  (setf (http3-test-stream-closed-p stream) t)))))
           (decoder-context
             (http-kit/http3::http3-client-qpack-decoder-context client))
           (decoder-adapter
             (http-kit/http3::http3-client-qpack-http3-decoder-context client))
           (failure nil))
      (unwind-protect
           (progn
             (setf failure
                   (handler-case
                       (http-kit/http3:send-http3-request
                        client request
                        :qpack-decoder-context decoder-context
                        :qpack-http3-decoder-context decoder-adapter
                        :on-stream-open
                        (lambda (stream stream-id cancel)
                          (declare (ignore stream stream-id cancel))
                          (incf on-open-calls)))
                     (http-protocol-error (condition) condition)))
             (http3-public-error-check
              failure
              "Dynamic QPACK response decoding requires a numeric stream ID."
              stream-id)
             (ensure-equal 1 request-opens)
             (ensure-equal 0 request-writes)
             (ensure-equal 0 reads)
             (ensure-equal 0 on-open-calls)
             (ensure-equal 1 request-closes)
             (ensure-true (eq failure request-close-condition))
             (ensure-true (http3-test-stream-closed-p request-stream))
             (ensure-equal 0
                           (http-kit/http3::http3-client-request-stream-count
                            client))
             (ensure-equal 0
                           (http-message-kit:qpack-context-insert-count
                            decoder-context))
             (ensure-equal 0
                           (http-message-kit:qpack-context-table-size
                            decoder-context))
             (ensure-true (http-kit/http3:http3-client-open-p client)))
        (http-kit/http3:close-http3-client client))))
)
