(in-package #:http-kit/test)

(deftest http3-defensive-control-attachment-lifetime
  (let* ((fixture (http3-boundary-fixture nil))
         (client (http3-boundary-fixture-client fixture))
         (peer (make-http3-test-stream
                :kind :peer-control
                :reads (list (list (octets) nil)
                             (list (octets 0) nil)
                             (list (http-kit/http3:encode-http3-frame
                                    (http-kit/http3:make-http3-settings-frame)) nil))))
         (replacement (make-http3-test-stream :kind :peer-control)))
    (push peer (http3-boundary-fixture-streams fixture))
    (unwind-protect
         (flet ((reject-replacement ()
                  (let ((reads (http3-boundary-fixture-reads fixture))
                        (closes (length (http3-boundary-fixture-closes fixture))))
                    (http3-public-error-check
                     (http3-boundary-error
                      (lambda ()
                        (http-kit/http3:attach-http3-peer-control-stream
                         client replacement)))
                     "An HTTP/3 client can have only one peer control stream." nil)
                    (ensure-equal reads (http3-boundary-fixture-reads fixture))
                    (ensure-equal closes
                                  (length (http3-boundary-fixture-closes fixture))))))
           (ensure-true (eq client (http-kit/http3:attach-http3-peer-control-stream
                                   client peer)))
           (reject-replacement)
           (dolist (expected '(nil nil (:settings)))
             (multiple-value-bind (effects fin-p)
                 (http-kit/http3:read-http3-control-stream client)
               (ensure-equal expected effects)
               (ensure-equal nil fin-p))
             (reject-replacement))
           (ensure-equal 3 (http3-boundary-fixture-reads fixture))
           (http-kit/http3:close-http3-client client)
           (reject-replacement)
           (ensure-equal 1 (count peer (http3-boundary-fixture-closes fixture)
                                  :key #'car :test #'eq))
           (ensure-equal 0 (count replacement (http3-boundary-fixture-closes fixture)
                                  :key #'car :test #'eq)))
      (http-kit/http3:close-http3-client client))))

(deftest http3-defensive-control-attachment-after-close
  (cl-weave:expect-assertions 16)
  (let* ((fixture (http3-boundary-fixture nil))
         (client (http3-boundary-fixture-client fixture))
         (peer (make-http3-test-stream :kind :peer-control)))
    (unwind-protect
         (progn
           (ensure-true (http-kit/http3:http3-client-open-p client))
           (ensure-equal t (http-kit/http3:close-http3-client client))
           (ensure-equal nil (http-kit/http3:http3-client-open-p client))
           (ensure-equal nil (http-kit/http3:http3-client-peer-control-stream client))
           (ensure-equal 3 (length (http3-boundary-fixture-closes fixture)))
           (let* ((closes (length (http3-boundary-fixture-closes fixture)))
                  (failure (http3-boundary-error
                            (lambda ()
                              (http-kit/http3:attach-http3-peer-control-stream
                               client peer)))))
             (ensure-equal t (http-kit/http3:close-http3-client client))
             (http3-public-error-check
              failure "Cannot attach an HTTP/3 control stream after client close." nil)
             (ensure-equal nil (http-kit/http3:http3-client-open-p client))
             (ensure-equal nil (http-kit/http3:http3-client-peer-control-stream client))
             (ensure-equal closes (length (http3-boundary-fixture-closes fixture)))
             (ensure-equal nil (http3-test-stream-closed-p peer))
             (ensure-equal 0 (count peer (http3-boundary-fixture-closes fixture)
                                    :key #'car :test #'eq))
             (ensure-equal 0 (http3-boundary-fixture-reads fixture))))
      (http-kit/http3:close-http3-client client))))

(deftest http3-defensive-qpack-public-callback-once
  (multiple-value-bind (unused-table section instruction)
      (http3-test-qpack-blocked-section '(("x-blocked" . "ready")))
    (declare (ignore unused-table))
    (let ((encoder-wire
            (http3-test-concat-octets
             (http-kit/http3:http3-varint-encode
              http-kit/http3:+http3-qpack-encoder-stream-type+)
             (http-kit/http3:qpack-encode-set-dynamic-table-capacity 256)
             instruction)))
      (multiple-value-bind (fixture peer)
          (http3-public-qpack-fixture nil (list (list encoder-wire nil)))
        (declare (ignore peer))
        (let* ((client (http3-boundary-fixture-client fixture))
               (context (http-kit/http3:http3-client-qpack-decoder-context client))
               (calls 0)
               (token (http-kit/http3:decode-http3-qpack-field-section
                       section context 0
                       :on-decoded (lambda (fields)
                                     (incf calls)
                                     (ensure-equal '(("x-blocked" . "ready")) fields)
                                     fields))))
          (unwind-protect
               (progn
                 (ensure-true (http-kit/http3:http3-qpack-blocked-stream-p token))
                 (ensure-equal 0 calls)
                 (signals http-protocol-error
                   (http-kit/http3:resume-http3-qpack-blocked-stream token))
                 (ensure-equal 0 calls)
                 (multiple-value-bind (events fin-p)
                     (http-kit/http3:read-http3-qpack-encoder-stream client)
                   (ensure-equal nil fin-p)
                   (let ((ready (remove-if-not
                                 (lambda (event)
                                   (eq :qpack-stream-unblocked (first event))) events)))
                     (ensure-equal 1 (length ready))
                     (ensure-true (eq token (second (first ready))))))
                 (ensure-equal '(("x-blocked" . "ready"))
                               (http-kit/http3:resume-http3-qpack-blocked-stream token))
                 (ensure-equal 1 calls)
                 (signals http-protocol-error
                   (http-kit/http3:resume-http3-qpack-blocked-stream token))
                 (ensure-equal 1 calls)
                 ;; An empty field list is still a completed decode.
                 (ensure-equal nil
                               (http-kit/http3:decode-http3-qpack-field-section
                                (http-kit/http3:qpack-encode-field-section nil)
                                context 4
                                :on-decoded (lambda (fields)
                                              (incf calls)
                                              (ensure-equal nil fields)
                                              fields)))
                 (ensure-equal 2 calls))
            (http-kit/http3:close-http3-client client)))))))

(deftest http3-public-server-producer-eof-and-empty-chunks
  (dolist (chunks (list (list nil)
                       (list (octets) nil)
                       (list (octets) (ascii "a") (octets) (ascii "bc") nil)))
    (dolist (trailers-p '(nil t))
      (let* ((remaining (copy-list chunks))
             (pulls 0)
             (expected-body (apply #'http3-test-concat-octets (remove nil chunks)))
             (final
               (make-http-response-stream
                :status 200 :body-length (length expected-body)
                :trailers (when trailers-p '(("x-checksum" . "ok")))
                :body-function
                (lambda ()
                  (incf pulls)
                  (ensure-true (consp remaining))
                  (pop remaining)))))
        (multiple-value-bind (failure response writes)
            (http3-server-coverage-serve
             (lambda (request) (declare (ignore request)) final))
          (ensure-equal nil failure)
          (ensure-true (eq final response))
          (ensure-equal (length chunks) pulls)
          (ensure-equal nil remaining)
          (let* ((frames (http3-server-coverage-frames writes))
                 (data (remove-if-not
                        (lambda (frame) (zerop (http-kit/http3:http3-frame-type frame)))
                        frames)))
            (ensure-equal expected-body
                          (apply #'http3-test-concat-octets
                                 (mapcar #'http-kit/http3:http3-frame-payload data)))
            (ensure-equal (count-if (lambda (chunk) (and chunk (plusp (length chunk)))) chunks)
                          (length data))
            (ensure-equal 1 (count t writes :key #'second))
            (ensure-equal t (second (car (last writes))))
            (ensure-equal (if trailers-p 2 1)
                          (count 1 frames :key #'http-kit/http3:http3-frame-type))
            (when trailers-p
              (ensure-equal '(("x-checksum" . "ok"))
                            (http-kit/http3:qpack-decode-field-section
                             (http-kit/http3:http3-frame-payload (car (last frames)))))))))))
  (let* ((pulls 0)
         (final (make-http-response-stream
                 :status 200 :body-function (lambda () (incf pulls) 17))))
    (multiple-value-bind (failure response writes)
        (http3-server-coverage-serve
         (lambda (request) (declare (ignore request)) final))
      (http3-public-error-check
       failure "HTTP/3 response body functions must return octet vectors or NIL." (type-of 17))
      (ensure-equal nil response)
      (ensure-equal nil writes)
      (ensure-equal 1 pulls))))

(deftest http3-public-control-callback-options-before-io
  (dolist (row '((:read-stream 17 "HTTP/3 control read-stream callback is required." :type)
                 (:close-stream 17 "HTTP/3 control close-stream must be a function or NIL." :type)
                 (:on-settings 17 "HTTP/3 control callbacks must be functions or NIL." :type)
                 (:on-goaway 17 "HTTP/3 control callbacks must be functions or NIL." :type)
                 (:on-max-push-id 17 "HTTP/3 control callbacks must be functions or NIL." :type)
                 (:on-cancel-push 17 "HTTP/3 control callbacks must be functions or NIL." :type)
                 (:on-error 17 "HTTP/3 control callbacks must be functions or NIL." :type)
                 (:max-frame-size 0 "HTTP/3 control max-frame-size must be a positive QUIC varint." 0)
                 (:max-frame-size "1" "HTTP/3 control max-frame-size must be a positive QUIC varint." "1")
                 (:peer-role nil "HTTP/3 control peer-role must be :CLIENT or :SERVER." nil)))
    (destructuring-bind (key value message detail) row
      (let ((reads 0) (closes 0))
        (http3-public-error-check
         (http3-boundary-error
          (lambda ()
            (apply #'http-kit/http3:serve-http3-control-stream :control
                   (append (list key value)
                           (list :peer-role :client
                                 :read-stream
                                 (lambda (stream &key timeout deadline)
                                   (declare (ignore stream timeout deadline))
                                   (incf reads)
                                   (values nil t))
                                 :close-stream
                                 (lambda (stream &key condition)
                                   (declare (ignore stream condition))
                                   (incf closes)))))))
         message (if (eq detail :type) (type-of 17) detail))
        (ensure-equal 0 reads)
        (ensure-equal 0 closes)))))

(deftest http3-public-control-fin-and-wire-contracts
  (dolist (row (list (list (octets) :http3-control-stream
                          "The HTTP/3 control stream ended before its stream-type prefix." nil)
                    (list (octets 0) :http3-control-stream
                          "The HTTP/3 control stream ended before SETTINGS." nil)
                    (list (octets 0 4 1) :http3-control-stream
                          "The HTTP/3 control stream ended with a truncated frame." nil)
                    (list (octets 0 4 0) :http3-control-stream
                          "The peer closed the HTTP/3 control stream." :h3-closed-critical-stream)
                    (list (octets 2) :http3-transport
                          "The peer HTTP/3 unidirectional stream is not a control stream." 2)
                    (list "not octets" :http3-transport
                          "HTTP/3 control read-stream must return an octet vector or NIL."
                          (type-of "not octets"))))
    (destructuring-bind (wire operation message detail) row
      (let ((reads 0) (closes 0) (errors 0) (observed nil) (closed nil))
        (let ((failure
                (http3-boundary-error
                 (lambda ()
                   (http-kit/http3:serve-http3-control-stream
                    :control :peer-role :server :timeout 2 :deadline 5
                    :read-stream
                    (lambda (stream &key timeout deadline)
                      (ensure-equal :control stream)
                      (ensure-equal 2 timeout)
                      (ensure-equal 5 deadline)
                      (incf reads)
                      (values wire t))
                    :close-stream
                    (lambda (stream &key condition)
                      (ensure-equal :control stream)
                      (incf closes)
                      (setf closed condition))
                    :on-error
                    (lambda (condition state)
                      (incf errors)
                      (setf observed condition)
                      (ensure-equal :server (http-kit/http3:http3-control-state-peer-role state))))))))
          (ensure-true (typep failure 'http-protocol-error))
          (ensure-equal operation (http-error-operation failure))
          (ensure-equal message (http-error-message failure))
          (ensure-equal detail (http-protocol-error-detail failure))
          (ensure-true (eq failure observed))
          (ensure-true (eq failure closed))
          (ensure-equal '(1 1 1) (list reads closes errors))))))
  (let ((reads 0))
    (let ((failure
            (http3-boundary-error
             (lambda ()
               (http-kit/http3:serve-http3-control-stream
                :control :peer-role :client
                :read-stream
                (lambda (stream &key timeout deadline)
                  (declare (ignore stream timeout deadline))
                  (incf reads)
                  (values (octets 0 4 0) t)))))))
      (ensure-equal :http3-control-stream (http-error-operation failure))
      (ensure-equal :h3-closed-critical-stream (http-protocol-error-detail failure))
      (ensure-equal 1 reads))))

(deftest http3-public-control-optional-notifications
  (let* ((wire (http3-test-concat-octets
                (http-kit/http3:http3-control-stream-prefix)
                (http-kit/http3:encode-http3-frame
                 (http-kit/http3:make-http3-frame :type 4 :payload (octets)))
                (http-kit/http3:encode-http3-frame
                 (http-kit/http3:make-http3-frame :type 33 :payload (octets 42)))
                (http-kit/http3:encode-http3-frame
                 (http-kit/http3:make-http3-priority-update-frame
                  :element-id 0 :priority-field-value "u=1"))
                (http-kit/http3:encode-http3-frame
                 (http-kit/http3:make-http3-frame :type 7 :payload (octets 0)))
                (http-kit/http3:encode-http3-frame
                 (http-kit/http3:make-http3-frame :type 13 :payload (octets 3)))
                (http-kit/http3:encode-http3-frame
                 (http-kit/http3:make-http3-frame :type 3 :payload (octets 2)))))
         (promised (list 2)))
    (dolist (notify-p '(nil t))
      (let ((seen nil) (closed nil) (state nil))
        (let ((failure
                (http3-boundary-error
                 (lambda ()
                   (http-kit/http3:serve-http3-control-stream
                    :control :peer-role :client :promised-push-ids promised
                    :read-stream
                    (lambda (stream &key timeout deadline)
                      (declare (ignore stream timeout deadline))
                      (values wire t))
                    :close-stream
                    (lambda (stream &key condition)
                      (ensure-equal :control stream)
                      (push condition closed))
                    :on-settings (when notify-p
                                   (lambda (settings current)
                                     (ensure-equal nil settings)
                                     (push :settings seen)
                                     (setf state current)))
                    :on-goaway (when notify-p
                                 (lambda (identifier current)
                                   (ensure-equal 0 identifier)
                                   (ensure-true (eq state current))
                                   (push :goaway seen)))
                    :on-max-push-id (when notify-p
                                     (lambda (identifier current)
                                       (ensure-equal 3 identifier)
                                       (ensure-true (eq state current))
                                       (push :max-push-id seen)))
                    :on-cancel-push (when notify-p
                                     (lambda (identifier current)
                                       (ensure-equal 2 identifier)
                                       (ensure-true (eq state current))
                                       (push :cancel-push seen)))
                    :on-error
                    (lambda (condition current)
                      (ensure-equal :h3-closed-critical-stream
                                    (http-protocol-error-detail condition))
                      (setf state current)))))))
          (when (typep failure 'cl-weave:assertion-failure)
            (error failure))
          (ensure-equal :http3-control-stream (http-error-operation failure))
          (ensure-equal (list failure) closed)
          (ensure-equal (when notify-p '(:settings :goaway :max-push-id :cancel-push))
                        (reverse seen))
          (ensure-true (http-kit/http3:http3-control-state-settings-received-p state))
          (ensure-equal 0 (http-kit/http3:http3-control-state-goaway-id state))
          (ensure-equal 3 (http-kit/http3:http3-control-state-max-push-id state))
          (ensure-equal '(2) (http-kit/http3:http3-control-state-cancelled-push-ids state))
          (ensure-equal "u=1" (cdr (assoc '(:request . 0)
                                       (http-kit/http3:http3-control-state-priority-updates state)
                                       :test #'equal)))
          (ensure-true (not (eq promised (http-kit/http3:http3-control-state-promised-push-ids state))))
          (ensure-equal '(2) promised))))))

(deftest http3-public-response-pseudo-field-contracts
  (dolist (row '((nil "An HTTP/3 response HEADERS block lacks :status." nil)
                 (((":method" . "GET")) "Unknown HTTP/3 response pseudo-field." ":method")
                 (((":status" . "200") (":status" . "201"))
                  "An HTTP/3 response has duplicate :status fields." nil)
                 (((":status" . "20"))
                  "HTTP/3 :status must be a three-digit ASCII status code." "20")
                 (((":status" . "2a0"))
                  "HTTP/3 :status must be a three-digit ASCII status code." "2a0")
                 (((":status" . "099"))
                  "HTTP/3 :status is outside the HTTP status range." 99)
                 (((":status" . "600"))
                  "HTTP/3 :status is outside the HTTP status range." 600)))
    (destructuring-bind (fields message detail) row
      (let* ((fixture (http3-boundary-fixture
                       (list (list (http3-boundary-headers fields) t))))
             (client (http3-boundary-fixture-client fixture)))
        (unwind-protect
             (let ((failure
                     (http3-boundary-error
                      (lambda ()
                        (http-kit/http3:send-http3-request
                         client (make-http-request :method "GET" :uri "https://example.test/"))))))
               (http3-public-error-check failure message detail)
               (ensure-equal 1 (http3-boundary-fixture-reads fixture))
               (ensure-equal 1 (length (http3-boundary-fixture-closes fixture)))
               (http3-boundary-check-release fixture failure)
               (ensure-true (http-kit/http3:http3-client-open-p client)))
          (http-kit/http3:close-http3-client client)))))
  (let* ((regular (http-kit/http3:qpack-encode-field-section '(("x-before" . "yes"))))
         (status (http-kit/http3:qpack-encode-field-section '((":status" . "200"))))
         ;; Keep the section prefix once, then place the pseudo-field after a regular field.
         (payload (http3-test-concat-octets regular (subseq status 2)))
         (wire (http-kit/http3:encode-http3-frame
                (http-kit/http3:make-http3-frame :type 1 :payload payload)))
         (fixture (http3-boundary-fixture (list (list wire t))))
         (client (http3-boundary-fixture-client fixture)))
    (ensure-equal '(0 0) (coerce (subseq regular 0 2) 'list))
    (ensure-equal '(0 0) (coerce (subseq status 0 2) 'list))
    (ensure-equal '(("x-before" . "yes"))
                  (http-kit/http3:qpack-decode-field-section regular))
    (ensure-equal '((":status" . "200"))
                  (http-kit/http3:qpack-decode-field-section status))
    (unwind-protect
         (let ((failure
                 (http3-boundary-error
                  (lambda ()
                    (http-kit/http3:send-http3-request
                     client (make-http-request :method "GET" :uri "https://example.test/"))))))
           (ensure-true (typep failure 'http-protocol-error))
           (ensure-equal :qpack (http-error-operation failure))
           (ensure-equal "QPACK pseudo-fields must precede regular fields."
                         (http-error-message failure))
           (ensure-equal ":status" (http-protocol-error-detail failure))
           (ensure-equal 1 (http3-boundary-fixture-reads fixture))
           (ensure-equal 1 (length (http3-boundary-fixture-closes fixture)))
           (http3-boundary-check-release fixture failure)
           (ensure-true (http-kit/http3:http3-client-open-p client)))
      (http-kit/http3:close-http3-client client)))
  (dolist (name '(":status" ":method"))
    (let* ((wire (http3-test-concat-octets
                  (http3-boundary-headers '((":status" . "200")))
                  (http3-boundary-headers (list (cons name "200")))))
           (fixture (http3-boundary-fixture (list (list wire t))))
           (client (http3-boundary-fixture-client fixture)))
      (unwind-protect
           (let ((failure
                   (http3-boundary-error
                    (lambda ()
                      (http-kit/http3:send-http3-request
                       client (make-http-request :method "GET" :uri "https://example.test/"))))))
             (http3-public-error-check failure "HTTP/3 trailers cannot contain pseudo-fields." name)
             (ensure-equal 1 (length (http3-boundary-fixture-closes fixture)))
             (http3-boundary-check-release fixture failure))
        (http-kit/http3:close-http3-client client))))
  (let* ((wire (http3-test-concat-octets
                (http3-boundary-headers '((":status" . "200")))
                (http3-boundary-headers '(("x-checksum" . "empty")))))
         (fixture (http3-boundary-fixture (list (list wire t))))
         (client (http3-boundary-fixture-client fixture)))
    (unwind-protect
         (let ((response (http-kit/http3:send-http3-request
                          client (make-http-request :method "GET" :uri "https://example.test/"))))
           (ensure-equal 200 (http-response-status response))
           (ensure-equal '("empty") (http-header-values (http-response-trailers response) "x-checksum"))
           (ensure-equal 0 (length (http-response-body response)))
           (http3-boundary-check-release fixture nil))
      (http-kit/http3:close-http3-client client))))

(deftest http3-public-trace-body-producer-before-io
  (let* ((fixture (http3-boundary-fixture nil))
         (client (http3-boundary-fixture-client fixture))
         (calls 0))
    (unwind-protect
         (let ((failure
                 (http3-boundary-error
                  (lambda ()
                    (http-kit/http3:send-http3-request
                     client (make-http-request :method "TRACE" :uri "https://example.test/")
                     :request-body-function
                     (lambda (limit) (declare (ignore limit)) (incf calls) nil))))))
           (http3-public-error-check failure "TRACE requests must not contain content." :trace-content)
           (ensure-equal 0 calls)
           (ensure-equal nil (http3-boundary-fixture-streams fixture))
           (ensure-equal 0 (http3-boundary-fixture-reads fixture))
           (ensure-equal nil (http3-boundary-fixture-closes fixture))
           (ensure-true (http-kit/http3:http3-client-open-p client)))
      (http-kit/http3:close-http3-client client))))

(deftest http3-public-send-options-before-stream-allocation
  (dolist (row '((:on-stream-open 17 "ON-STREAM-OPEN must be a function or NIL.")
                 (:on-push-promise 17 "ON-PUSH-PROMISE must be a function or NIL.")
                 (:on-information 17 "ON-INFORMATION must be a function or NIL.")
                 (:max-header-bytes 0 "MAX-HEADER-BYTES must be NIL or a positive integer.")
                 (:max-header-bytes "1" "MAX-HEADER-BYTES must be NIL or a positive integer.")
                 (:max-fields 0 "MAX-FIELDS must be NIL or a positive integer.")
                 (:max-fields "1" "MAX-FIELDS must be NIL or a positive integer.")
                 (:request-body-function 17 "The HTTP/3 request body producer must be a function.")
                 (:request-body-length -1 "The HTTP/3 request body length must be a non-negative integer or NIL.")
                 (:request-body-length "1" "The HTTP/3 request body length must be a non-negative integer or NIL.")
                 (:request-body-length 0 "A request body length requires a request body producer.")
                 (:max-body-bytes -1 "max-body-bytes must be NIL or a non-negative integer.")
                 (:max-body-bytes "1" "max-body-bytes must be NIL or a non-negative integer.")))
    (destructuring-bind (key value message) row
      (let* ((fixture (http3-boundary-fixture nil))
             (client (http3-boundary-fixture-client fixture)))
        (unwind-protect
             (let ((failure
                     (http3-boundary-error
                      (lambda ()
                        (apply #'http-kit/http3:send-http3-request client
                               (make-http-request :method "GET" :uri "https://example.test/")
                               (list key value))))))
               (http3-public-error-check failure message value)
               (ensure-equal nil (http3-boundary-fixture-streams fixture))
               (ensure-equal 0 (http3-boundary-fixture-reads fixture))
               (ensure-equal nil (http3-boundary-fixture-closes fixture))
               (ensure-true (http-kit/http3:http3-client-open-p client)))
          (http-kit/http3:close-http3-client client)))))
  (let* ((fixture (http3-boundary-fixture
                   (list (list (http3-boundary-headers '((":status" . "204"))) t))))
         (client (http3-boundary-fixture-client fixture)))
    (unwind-protect
         (let ((response
                 (http-kit/http3:send-http3-request
                  client (make-http-request :method "GET" :uri "https://example.test/")
                  :on-stream-open nil :on-push-promise nil :on-information nil
                  :max-header-bytes 42 :max-fields 1 :max-body-bytes 0)))
           (ensure-equal 204 (http-response-status response))
           (ensure-equal 0 (length (http-response-body response)))
           (ensure-equal 1 (http3-boundary-fixture-reads fixture))
           (http3-boundary-check-release fixture nil))
      (http-kit/http3:close-http3-client client))))

(deftest http3-public-server-callback-contracts
  (dolist (row
            '(((:handler 17)
               "HTTP/3 server handler, read-stream, and write-stream callbacks are required." :type)
              ((:read-stream 17)
               "HTTP/3 server handler, read-stream, and write-stream callbacks are required." :type)
              ((:write-stream 17)
               "HTTP/3 server handler, read-stream, and write-stream callbacks are required." :type)
              ((:qpack-decoder-context t :stream-id nil)
               "Dynamic QPACK request decoding requires a numeric stream ID." nil)
              ((:qpack-decoder-context t :stream-id 0 :await-qpack nil)
               "Dynamic QPACK request decoding requires an await-qpack callback." nil)
              ((:qpack-decoder-context t :stream-id 0 :await-qpack :valid :qpack-serialize 17)
               "Dynamic QPACK request decoding requires a serializer." 17)))
    (destructuring-bind (options message detail) row
      (let* ((calls (vector 0 0 0 0))
             (handler (lambda (request)
                        (declare (ignore request))
                        (incf (aref calls 0))
                        (make-http-response :status 204)))
             (arguments (copy-list options)))
        (when (eq :valid (getf arguments :await-qpack))
          (setf (getf arguments :await-qpack) (lambda (token) (declare (ignore token)))))
        (let ((failure
                (http3-boundary-error
                 (lambda ()
                   (apply #'http-kit/http3:serve-http3-request-stream :stream
                          (if (member :handler arguments) (getf arguments :handler) handler)
                          (append (loop for (key value) on arguments by #'cddr
                                        unless (eq key :handler) append (list key value))
                                  (list :read-stream
                                        (lambda (stream &key timeout deadline)
                                          (declare (ignore stream timeout deadline))
                                          (incf (aref calls 1))
                                          (values nil t))
                                        :write-stream
                                        (lambda (stream bytes &key fin-p timeout deadline)
                                          (declare (ignore stream bytes fin-p timeout deadline))
                                          (incf (aref calls 2)))
                                        :close-stream
                                        (lambda (stream &key condition)
                                          (declare (ignore stream condition))
                                          (incf (aref calls 3))))))))))
          (http3-public-error-check failure message (if (eq detail :type) (type-of 17) detail))
          (ensure-equal '(0 0 0 0) (coerce calls 'list)))))))

(deftest http3-public-server-wire-and-handler-contracts
  (let ((headers (http3-boundary-headers
                  '((":method" . "GET") (":scheme" . "https")
                    (":authority" . "example.test") (":path" . "/")))))
    (dolist (row
              (append
               (list (list "not octets"
                           "HTTP/3 read-stream must return an octet vector or NIL."
                           (type-of "not octets") nil))
               (loop for type in '(3 4 5 7 13 984832 984833)
                     collect (list (http-kit/http3:encode-http3-frame
                                    (http-kit/http3:make-http3-frame :type type :payload (octets)))
                                   "HTTP/3 control or push frames are invalid on a request stream."
                                   type nil))
               (list (list headers
                           "HTTP/3 informational responses must be supplied as a list."
                           (type-of 17) t))))
      (destructuring-bind (wire message detail handler-p) row
        (let ((reads 0))
          (multiple-value-bind (failure response handlers writes closes observed request error-request)
              (http3-public-error-serve
               (list (list wire t))
               (lambda (request)
                 (declare (ignore request))
                 (values (make-http-response :status 204) 17))
               :before-read (lambda (handlers writes)
                              (declare (ignore handlers writes))
                              (incf reads)))
            (http3-public-error-check failure message detail)
            (ensure-equal nil response)
            (ensure-equal 1 reads)
            (ensure-equal (if handler-p 1 0) handlers)
            (ensure-equal nil writes)
            (ensure-equal 1 (length closes))
            (ensure-true (eq failure (first closes)))
            (ensure-true (eq failure observed))
            (ensure-equal handler-p (not (null request)))
            (ensure-true (eq request error-request))))))
    (multiple-value-bind (failure response handlers writes closes observed request error-request)
        (http3-public-error-serve
         (list (list (http3-test-concat-octets
                      (http-kit/http3:encode-http3-frame
                       (http-kit/http3:make-http3-frame :type 33 :payload (octets 1 2 3))) headers) t))
         (lambda (request)
           (ensure-equal "GET" (http-request-method request))
           (make-http-response :status 204)))
      (ensure-equal nil failure)
      (ensure-equal 204 (http-response-status response))
      (ensure-equal 1 handlers)
      (ensure-equal 1 (length writes))
      (ensure-equal '(nil) closes)
      (ensure-equal nil observed)
      (ensure-true (http-request-p request))
      (ensure-equal nil error-request))))

(deftest http3-public-server-options-before-io
  (dolist (row '((:close-stream 17 "HTTP/3 server close-stream must be a function or NIL." :type)
                 (:max-frame-size 0 "HTTP/3 server max-frame-size must be a positive QUIC varint." :value)
                 (:max-header-bytes 0 "HTTP/3 server max-header-bytes must be a positive integer." :value)
                 (:max-fields 0 "HTTP/3 server max-fields must be a positive integer." :value)
                 (:max-fields "1" "HTTP/3 server max-fields must be a positive integer." :value)
                 (:max-body-bytes -1 "HTTP/3 server max-body-bytes must be NIL or a non-negative integer." :value)
                 (:collect-body-p 17 "HTTP/3 server collect-body-p must be boolean." :value)
                 (:on-body-chunk 17 "HTTP/3 server on-body-chunk must be a function or NIL." :type)
                 (:on-error 17 "HTTP/3 server on-error must be a function or NIL." :type)))
    (destructuring-bind (key value message detail-kind) row
      (let ((reads 0) (writes 0) (handlers 0) (closes 0))
        (let ((failure
                (http3-boundary-error
                 (lambda ()
                   (apply #'http-kit/http3:serve-http3-request-stream :stream
                          (lambda (request)
                            (declare (ignore request))
                            (incf handlers)
                            (make-http-response :status 204))
                          (append (list key value)
                                  (list :read-stream
                                        (lambda (stream &key timeout deadline)
                                          (declare (ignore stream timeout deadline))
                                          (incf reads)
                                          (values nil t))
                                        :write-stream
                                        (lambda (stream bytes &key fin-p timeout deadline)
                                          (declare (ignore stream bytes fin-p timeout deadline))
                                          (incf writes))
                                        :close-stream
                                        (lambda (stream &key condition)
                                          (declare (ignore stream condition))
                                          (incf closes)))))))))
          (http3-public-error-check failure message
                                   (if (eq detail-kind :type) (type-of value) value))
          (ensure-equal 0 reads)
          (ensure-equal 0 writes)
          (ensure-equal 0 handlers)
          (ensure-equal 0 closes)))))
  (multiple-value-bind (failure response handlers writes closes observed request error-request)
      (http3-public-error-serve
       (list (list (http3-boundary-headers '((":method" . "GET") (":scheme" . "https")
                                            (":authority" . "example.test") (":path" . "/"))) t))
       (lambda (request)
         (ensure-equal "GET" (http-request-method request))
         (make-http-response :status 204)))
    (ensure-equal nil failure)
    (ensure-equal 204 (http-response-status response))
    (ensure-equal 1 handlers)
    (ensure-equal 1 (length writes))
    (ensure-equal '(nil) closes)
    (ensure-equal nil observed)
    (ensure-true (http-request-p request))
    (ensure-equal nil error-request)))

(deftest http3-public-manager-validation-and-accessors
  (dolist (accessor (list #'http-kit/http3:http3-connection-manager-open-p
                        #'http-kit/http3:http3-connection-manager-max-connections
                        #'http-kit/http3:http3-connection-manager-idle-timeout
                        #'http-kit/http3:http3-connection-manager-max-connection-age
                        #'http-kit/http3:http3-connection-manager-clock-function
                        #'http-kit/http3:http3-connection-manager-connection-count))
    (ensure-equal nil (funcall accessor nil)))
  (dolist (operation (list #'http-kit/http3:http3-connection-manager-clients
                         #'http-kit/http3:make-http3-connection-manager-transport
                         #'http-kit/http3:close-http3-connection-manager))
    (let ((failure (http3-boundary-error (lambda () (funcall operation 17)))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :http3-transport (http-error-operation failure))
      (ensure-equal (type-of 17) (http-protocol-error-detail failure))))
  (dolist (row '((:open-client 17 "MAKE-HTTP3-CONNECTION-MANAGER requires an open-client callback.")
                 (:max-connections 0 "MAX-CONNECTIONS must be a positive integer.")
                 (:max-connections "1" "MAX-CONNECTIONS must be a positive integer.")
                 (:idle-timeout -1 "IDLE-TIMEOUT must be a non-negative real or NIL.")
                 (:idle-timeout "1" "IDLE-TIMEOUT must be a non-negative real or NIL.")
                 (:max-connection-age -1 "MAX-CONNECTION-AGE must be a non-negative real or NIL.")
                 (:max-connection-age "1" "MAX-CONNECTION-AGE must be a non-negative real or NIL.")
                 (:clock-function 17 "CLOCK-FUNCTION must be a function.")))
    (destructuring-bind (key value message) row
      (let ((opened 0))
        (http3-public-error-check
         (http3-boundary-error
          (lambda ()
            (apply #'http-kit/http3:make-http3-connection-manager
                   (append (list key value)
                           (list :open-client
                                 (lambda (request &key timeout deadline)
                                   (declare (ignore request timeout deadline))
                                   (incf opened)))))))
         message value)
        (ensure-equal 0 opened))))
  (let* ((clock (lambda () 0))
         (manager
           (http-kit/http3:make-http3-connection-manager
            :open-client (lambda (request &key timeout deadline)
                           (declare (ignore request timeout deadline)))
            :max-connections 1 :idle-timeout 0 :max-connection-age 0
            :clock-function clock)))
    (ensure-true (http-kit/http3:http3-connection-manager-open-p manager))
    (ensure-equal 1 (http-kit/http3:http3-connection-manager-max-connections manager))
    (ensure-equal 0 (http-kit/http3:http3-connection-manager-idle-timeout manager))
    (ensure-equal 0 (http-kit/http3:http3-connection-manager-max-connection-age manager))
    (ensure-true (eq clock (http-kit/http3:http3-connection-manager-clock-function manager)))
    (ensure-equal 0 (http-kit/http3:http3-connection-manager-connection-count manager))
    (ensure-equal nil (http-kit/http3:http3-connection-manager-clients manager))
    (ensure-true (eq manager (http-kit/http3:close-http3-connection-manager manager)))
    (ensure-equal nil (http-kit/http3:http3-connection-manager-open-p manager))
    (ensure-true (eq manager (http-kit/http3:close-http3-connection-manager manager)))))

(defun http3-public-counted-client ()
  (let ((counts (vector 0 0 0)))
    (values
     (http3-transport-boundary-client
      :write-stream (lambda (stream bytes &key fin-p timeout deadline)
                      (declare (ignore stream bytes fin-p timeout deadline))
                      (incf (aref counts 0)))
      :read-stream (lambda (stream &key timeout deadline)
                     (declare (ignore stream timeout deadline))
                     (incf (aref counts 1))
                     (values nil t))
      :close-stream (lambda (stream &key condition)
                      (declare (ignore stream condition))
                      (incf (aref counts 2))))
     counts)))

(deftest http3-public-client-lifecycle-guards
  (dolist (row
            (list
             (list (lambda (client) (http-kit/http3:cancel-http3-stream client :stream))
                   "CANCEL-HTTP3-STREAM requires an HTTP/3 client." "The HTTP/3 client is already closed." t)
             (list (lambda (client) (http-kit/http3:send-http3-priority-update client 0))
                   "SEND-HTTP3-PRIORITY-UPDATE requires an HTTP/3 client." "The HTTP/3 client is already closed." t)
             (list (lambda (client) (http-kit/http3:advertise-http3-max-push-id client 0))
                   "ADVERTISE-HTTP3-MAX-PUSH-ID requires an HTTP/3 client." "The HTTP/3 client is already closed." t)
             (list (lambda (client) (http-kit/http3:cancel-http3-push client 0))
                   "CANCEL-HTTP3-PUSH requires an HTTP/3 client." "The HTTP/3 client is already closed." t)
             (list (lambda (client) (http-kit/http3:read-http3-control-stream client))
                   "READ-HTTP3-CONTROL-STREAM requires an HTTP/3 client."
                   "Cannot read an HTTP/3 control stream after client close." t)
             (list (lambda (client) (http-kit/http3:accept-http3-peer-unidirectional-stream client :stream))
                   "A live HTTP/3 client is required." "A live HTTP/3 client is required." nil)
             (list (lambda (client) (http-kit/http3:http3-client-set-qpack-capacity client 0))
                   "A live HTTP/3 client is required." "A live HTTP/3 client is required." nil)
             (list (lambda (client) (http-kit/http3:http3-client-insert-qpack-field client "x-test" "value"))
                   "A live HTTP/3 client is required." "A live HTTP/3 client is required." nil)))
    (destructuring-bind (operation wrong-message closed-message type-detail-p) row
      (http3-public-error-check
       (http3-boundary-error (lambda () (funcall operation 17)))
       wrong-message (and type-detail-p (type-of 17)))
      (multiple-value-bind (client counts) (http3-public-counted-client)
        (http-kit/http3:close-http3-client client)
        (let ((writes (aref counts 0))
              (closes (aref counts 2)))
          (http3-public-error-check
           (http3-boundary-error (lambda () (funcall operation client)))
           closed-message nil)
          (ensure-equal writes (aref counts 0))
          (ensure-equal closes (aref counts 2))
          (ensure-equal 0 (aref counts 1))))))
  (multiple-value-bind (client counts) (http3-public-counted-client)
    (unwind-protect
         (progn
           (ensure-true (eq client (http-kit/http3:send-http3-priority-update client 0)))
           (ensure-true (eq client (http-kit/http3:advertise-http3-max-push-id client 0)))
           (ensure-equal 0 (aref counts 1))
           (ensure-true (http-kit/http3:http3-client-open-p client)))
      (http-kit/http3:close-http3-client client))))

(deftest http3-public-stream-and-id-validation
  (dolist (row
            (list
             (list (lambda (client) (http-kit/http3:cancel-http3-stream client nil))
                   "CANCEL-HTTP3-STREAM requires a stream." nil)
             (list (lambda (client) (http-kit/http3:cancel-http3-stream client :stream :error-code -1))
                   "HTTP/3 cancellation error-code must be a QUIC varint." -1)
             (list (lambda (client) (http-kit/http3:cancel-http3-stream client :stream :error-code "0"))
                   "HTTP/3 cancellation error-code must be a QUIC varint." "0")
             (list (lambda (client) (http-kit/http3:cancel-http3-stream client :stream))
                   "The HTTP/3 QUIC backend does not provide stream cancellation." :unsupported)
             (list (lambda (client) (http-kit/http3:send-http3-priority-update client -1))
                   "An HTTP/3 priority element ID must be a QUIC varint." -1)
             (list (lambda (client) (http-kit/http3:send-http3-priority-update client "0"))
                   "An HTTP/3 priority element ID must be a QUIC varint." "0")
             (list (lambda (client) (http-kit/http3:send-http3-priority-update client 0 :kind :push))
                   "Push PRIORITY_UPDATE requires HTTP/3 server-push support." :unsupported)
             (list (lambda (client) (http-kit/http3:send-http3-priority-update client 1))
                   "An HTTP/3 request priority target must be a client-initiated bidirectional stream." 1)
             (list (lambda (client) (http-kit/http3:advertise-http3-max-push-id client -1))
                   "An HTTP/3 maximum push ID must be a QUIC varint." -1)
             (list (lambda (client) (http-kit/http3:advertise-http3-max-push-id client "0"))
                   "An HTTP/3 maximum push ID must be a QUIC varint." "0")
             (list (lambda (client) (http-kit/http3:cancel-http3-push client -1))
                   "An HTTP/3 push ID must be a QUIC varint." -1)
             (list (lambda (client) (http-kit/http3:cancel-http3-push client "0"))
                   "An HTTP/3 push ID must be a QUIC varint." "0")
             (list (lambda (client) (http-kit/http3:cancel-http3-push client 0))
                   "CANCEL_PUSH must identify a promised HTTP/3 server push." :h3-id-error)
             (list (lambda (client) (http-kit/http3:accept-http3-peer-unidirectional-stream client nil))
                   "ACCEPT-HTTP3-PEER-UNIDIRECTIONAL-STREAM requires a stream." nil)
             (list (lambda (client) (http-kit/http3:attach-http3-peer-control-stream client nil))
                   "ATTACH-HTTP3-PEER-CONTROL-STREAM requires a stream." nil)
             (list (lambda (client) (http-kit/http3:read-http3-control-stream client))
                   "READ-HTTP3-CONTROL-STREAM requires an attached peer control stream." nil)
             (list (lambda (client) (http-kit/http3:attach-http3-peer-qpack-encoder-stream client nil))
                   "ATTACH-HTTP3-PEER-QPACK-ENCODER-STREAM requires a client and stream." nil)
             (list (lambda (client) (http-kit/http3:attach-http3-peer-qpack-decoder-stream client nil))
                   "ATTACH-HTTP3-PEER-QPACK-DECODER-STREAM requires a client and stream." nil)))
    (destructuring-bind (operation message detail) row
      (multiple-value-bind (client counts) (http3-public-counted-client)
        (let ((writes (aref counts 0)))
        (unwind-protect
             (progn
               (http3-public-error-check
                (http3-boundary-error (lambda () (funcall operation client))) message detail)
               (ensure-equal writes (aref counts 0))
               (ensure-equal 0 (aref counts 1))
               (ensure-equal 0 (aref counts 2))
               (ensure-true (http-kit/http3:http3-client-open-p client)))
          (http-kit/http3:close-http3-client client))))))
  (multiple-value-bind (client counts) (http3-public-counted-client)
    (unwind-protect
         (progn
           (ensure-true (eq client (http-kit/http3:attach-http3-peer-control-stream client :control)))
           (ensure-true (eq client (http-kit/http3:attach-http3-peer-qpack-encoder-stream client :encoder)))
           (ensure-true (eq client (http-kit/http3:attach-http3-peer-qpack-decoder-stream client :decoder)))
           (ensure-true (eq client (http-kit/http3:advertise-http3-max-push-id client 0)))
           (let ((writes (aref counts 0)))
             (http3-public-error-check
              (http3-boundary-error (lambda () (http-kit/http3:advertise-http3-max-push-id client 0)))
              "MAX_PUSH_ID must increase the previously advertised push ID." :h3-id-error)
             (ensure-equal writes (aref counts 0)))
           (ensure-equal 0 (aref counts 1)))
      (http-kit/http3:close-http3-client client))))

(defun check-http3-public-request-target
    (fields &key enable-connect-p message detail method protocol scheme path query target)
  (let* ((wire (http3-boundary-headers fields))
         (stream (make-http3-test-stream :kind :request))
         (reads 0) (handlers 0) (body-calls 0) (closes 0)
         (writes nil) (errors nil) (closed-condition nil)
         (request nil) (handler-response nil) (response nil))
    (ensure-true (plusp (length wire)))
    (let ((failure
            (http3-boundary-error
             (lambda ()
               (setf response
                     (http-kit/http3:serve-http3-request-stream
                      stream
                      (lambda (incoming)
                        (incf handlers)
                        (setf request incoming
                              handler-response (make-http-response :status 204)))
                      :enable-connect-p enable-connect-p
                      :read-stream
                      (lambda (seen &key timeout deadline)
                        (declare (ignore timeout deadline))
                        (ensure-true (eq stream seen))
                        (incf reads)
                        (ensure-equal 1 reads)
                        (values wire t))
                      :write-stream
                      (lambda (seen bytes &key fin-p timeout deadline)
                        (declare (ignore timeout deadline))
                        (ensure-true (eq stream seen))
                        (push (list (copy-seq bytes) fin-p) writes))
                      :close-stream
                      (lambda (seen &key condition)
                        (ensure-true (eq stream seen))
                        (incf closes)
                        (setf closed-condition condition))
                      :on-body-chunk (lambda (&rest args)
                                       (declare (ignore args))
                                       (incf body-calls))
                      :on-error (lambda (condition incoming)
                                  (push (list condition incoming) errors))))))))
      (ensure-equal 1 reads)
      (ensure-equal 0 body-calls)
      (ensure-equal 1 closes)
      (ensure-true (eq failure closed-condition))
      (if message
          (progn
            (http3-public-error-check failure message detail)
            (ensure-equal 0 handlers)
            (ensure-false response)
            (ensure-false writes)
            (ensure-equal 1 (length errors))
            (ensure-true (eq failure (first (first errors))))
            (ensure-false (second (first errors))))
          (progn
            (ensure-false failure)
            (ensure-equal 1 handlers)
            (ensure-false errors)
            (ensure-true (eq handler-response response))
            (ensure-equal method (http-request-method request))
            (ensure-equal protocol (http-request-protocol request))
            (let ((uri (http-request-uri request)))
              (ensure-equal scheme (http-uri-scheme uri))
              (ensure-equal "example.test" (http-uri-authority uri))
              (ensure-equal path (http-uri-path uri))
              (ensure-equal query (http-uri-query uri)))
            (ensure-equal target (http-request-target request))
            (ensure-equal (octets) (http-request-body request))
            (ensure-equal '(t) (mapcar #'second writes))
            (let ((frames (http3-server-coverage-frames (nreverse writes))))
              (ensure-equal '(1) (mapcar #'http-kit/http3:http3-frame-type frames))
              (ensure-equal "204"
                            (cdr (assoc ":status"
                                        (http-kit/http3:qpack-decode-field-section
                                         (http-kit/http3:http3-frame-payload (first frames)))
                                        :test #'string=)))))))))

(deftest http3-coverage-server-request-target-validation
  (dolist (row '((nil "/" "An HTTP/3 request requires an http or https :scheme.")
                 ("ftp" "/" "An HTTP/3 request requires an http or https :scheme.")
                 ("https" nil "An HTTP/3 request requires a :path pseudo-field.")
                 ("https" "" "An HTTP/3 :path must be an origin-form path or *.")
                 ("https" "relative" "An HTTP/3 :path must be an origin-form path or *.")
                 ("https" "*" "The asterisk-form HTTP/3 target is valid only for OPTIONS.")))
    (destructuring-bind (scheme path message) row
      (check-http3-public-request-target
       (append (list (cons ":method" "GET"))
               (when scheme (list (cons ":scheme" scheme)))
               (list (cons ":authority" "example.test"))
               (when path (list (cons ":path" path))))
       :message message)))
  (check-http3-public-request-target
   '((":method" . "GET") (":scheme" . "https")
     (":authority" . "example.test") (":path" . "/items?q=1"))
   :method "GET" :scheme "https" :path "/items" :query "q=1" :target "/items?q=1")
  (check-http3-public-request-target
   '((":method" . "OPTIONS") (":scheme" . "http")
     (":authority" . "example.test") (":path" . "*"))
   :method "OPTIONS" :scheme "http" :path "/" :target "*"))

(deftest http3-coverage-server-extended-connect-target-validation
  (dolist (row '((nil "/chat" "An extended CONNECT request requires an http or https :scheme.")
                 ("ftp" "/chat" "An extended CONNECT request requires an http or https :scheme.")
                 ("https" nil "An extended CONNECT request requires a :path pseudo-field.")
                 ("https" "" "An extended CONNECT :path must be an origin-form path.")
                 ("https" "relative" "An extended CONNECT :path must be an origin-form path.")
                 ("https" "*" "An extended CONNECT :path must be an origin-form path.")))
    (destructuring-bind (scheme path message) row
      (check-http3-public-request-target
       (append '((":method" . "CONNECT") (":protocol" . "websocket"))
               (when scheme (list (cons ":scheme" scheme)))
               '((":authority" . "example.test"))
               (when path (list (cons ":path" path))))
       :enable-connect-p t :message message)))
  (let ((fields '((":method" . "CONNECT") (":protocol" . "websocket")
                  (":scheme" . "https") (":authority" . "example.test")
                  (":path" . "/chat?room=1"))))
    (check-http3-public-request-target
     fields :message "Extended CONNECT was not enabled by local SETTINGS."
     :detail :h3-message-error)
    (check-http3-public-request-target
     fields :enable-connect-p t :method "CONNECT" :protocol "websocket"
     :scheme "https" :path "/chat" :query "room=1" :target "/chat?room=1")))

(deftest http3-public-request-and-push-lifecycle-preflight
  (let ((request (make-http-request :method "GET" :uri "https://example.test/")))
    (dolist (row
              (list (list (lambda (client)
                            (http-kit/http3:send-http3-request client request))
                          "SEND-HTTP3-REQUEST requires an HTTP/3 client.")
                    (list (lambda (client)
                            (http-kit/http3:receive-http3-push client :stream))
                          "RECEIVE-HTTP3-PUSH requires an HTTP/3 client.")))
      (destructuring-bind (operation message) row
        (http3-public-error-check
         (handler-case (funcall operation :not-a-client)
           (http-protocol-error (condition) condition))
         message (type-of :not-a-client))
        (multiple-value-bind (client counts) (http3-public-counted-client)
          (http-kit/http3:close-http3-client client)
          (let ((before (copy-seq counts)))
            (http3-public-error-check
             (handler-case (funcall operation client)
               (http-protocol-error (condition) condition))
             "The HTTP/3 client is already closed." nil)
            (ensure-equal before counts)
            (ensure-equal nil (http-kit/http3:http3-client-open-p client))))))
    (multiple-value-bind (client counts) (http3-public-counted-client)
      (unwind-protect
           (let ((before (copy-seq counts)))
             (http3-public-error-check
              (handler-case (http-kit/http3:send-http3-request client :not-a-request)
                (http-protocol-error (condition) condition))
              "SEND-HTTP3-REQUEST requires an HTTP request." (type-of :not-a-request))
             (ensure-equal before counts)
             (ensure-true (http-kit/http3:http3-client-open-p client)))
        (http-kit/http3:close-http3-client client)))))

(deftest http3-public-push-receive-options-before-io
  (multiple-value-bind (client counts) (http3-public-counted-client)
    (unwind-protect
         (let ((before (copy-seq counts)))
           (http3-public-error-check
            (handler-case (http-kit/http3:receive-http3-push client nil)
              (http-protocol-error (condition) condition))
            "RECEIVE-HTTP3-PUSH requires a QUIC stream." nil)
           (ensure-equal before counts)
           (dolist (row
                     '((:on-body-chunk :not-a-function "ON-BODY-CHUNK must be a function or NIL.")
                       (:await-push-promise :not-a-function "AWAIT-PUSH-PROMISE must be a function or NIL.")
                       (:max-header-bytes 0 "max-header-bytes must be NIL or a positive integer.")
                       (:max-header-bytes -1 "max-header-bytes must be NIL or a positive integer.")
                       (:max-header-bytes "1" "max-header-bytes must be NIL or a positive integer.")
                       (:max-header-bytes 1/2 "max-header-bytes must be NIL or a positive integer.")
                       (:max-fields 0 "max-fields must be NIL or a positive integer.")
                       (:max-fields -1 "max-fields must be NIL or a positive integer.")
                       (:max-fields "1" "max-fields must be NIL or a positive integer.")
                       (:max-fields 1/2 "max-fields must be NIL or a positive integer.")
                       (:max-body-bytes -1 "max-body-bytes must be NIL or a non-negative integer.")
                       (:max-body-bytes "1" "max-body-bytes must be NIL or a non-negative integer.")
                       (:max-body-bytes 1/2 "max-body-bytes must be NIL or a non-negative integer.")))
             (destructuring-bind (keyword value message) row
               (http3-public-error-check
                (handler-case
                    (apply #'http-kit/http3:receive-http3-push client :stream
                           (list keyword value))
                  (http-protocol-error (condition) condition))
                message value)
               (ensure-equal before counts)
               (ensure-true (http-kit/http3:http3-client-open-p client)))))
      (http-kit/http3:close-http3-client client))))

(deftest http3-public-extended-connect-needs-peer-setting
  (let ((request (make-http-request :method "CONNECT" :protocol "websocket"
                                    :uri "https://example.test/chat")))
    (dolist (settings '(nil :default :disabled))
      (multiple-value-bind (client counts) (http3-public-counted-client)
        (unwind-protect
             (progn
               (when settings
                 (ensure-equal :settings
                               (http-kit/http3:process-http3-control-frame
                                (http-kit/http3:http3-client-peer-control-state client)
                                (if (eq settings :disabled)
                                    (http-kit/http3:make-http3-settings-frame :enable-connect 0)
                                    (http-kit/http3:make-http3-settings-frame)))))
               (let ((before (copy-seq counts)))
                 (http3-public-error-check
                  (handler-case (http-kit/http3:send-http3-request client request)
                    (http-protocol-error (condition) condition))
                  "Extended CONNECT requires peer SETTINGS_ENABLE_CONNECT_PROTOCOL=1."
                  :h3-settings-error)
                 (ensure-equal before counts)
                 (ensure-true (http-kit/http3:http3-client-open-p client))))
          (http-kit/http3:close-http3-client client))))))

(deftest http3-reachable-classic-connect-request-contract
  (check-http3-public-request-target
   '((":method" . "CONNECT") (":authority" . "example.test"))
   :method "CONNECT" :scheme "http" :path "/" :target "example.test")
  (dolist (extra '((":scheme" . "https") (":path" . "/")))
    (check-http3-public-request-target
     (list '(":method" . "CONNECT") '(":authority" . "example.test") extra)
     :message "A CONNECT request must not contain :scheme or :path.")))

(deftest http3-reachable-method-pseudo-field-contract
  (dolist (method '(nil "" "bad method"))
    (check-http3-public-request-target
     (append (when method (list (cons ":method" method)))
             '((":scheme" . "https") (":authority" . "example.test")
               (":path" . "/")))
     :message "An HTTP/3 request must contain a valid :method pseudo-field."))
  (dolist (duplicate '((":method" . "GET") (":scheme" . "https")
                      (":authority" . "example.test") (":path" . "/")))
    (check-http3-public-request-target
     (append '((":method" . "GET") (":scheme" . "https")
               (":authority" . "example.test") (":path" . "/"))
             (list duplicate))
     :message "HTTP/3 request pseudo-fields must be unique."
     :detail (car duplicate))))

(defun http3-reachable-check-header-error (failure name reason)
  (ensure-true (typep failure 'http-invalid-header))
  (ensure-equal :http3-header (http-error-operation failure))
  (ensure-equal name (http-invalid-header-name failure))
  (ensure-equal reason (http-invalid-header-reason failure))
  (ensure-equal (format nil "Invalid HTTP/3 header ~S: ~A." name reason)
                (http-error-message failure)))

(deftest http3-reachable-client-header-preflight-contract
  (dolist (row '(((("connection" . "close")) "connection"
                "connection-specific fields are forbidden.")
               ((("keep-alive" . "timeout=5")) "keep-alive"
                "connection-specific fields are forbidden.")
               ((("proxy-connection" . "close")) "proxy-connection"
                "connection-specific fields are forbidden.")
               ((("transfer-encoding" . "chunked")) "transfer-encoding"
                "connection-specific fields are forbidden.")
               ((("upgrade" . "websocket")) "upgrade"
                "connection-specific fields are forbidden.")
               ((("te" . "gzip")) "te" "HTTP/3 permits only the trailers value.")
               ((("host" . "other.test")) "host"
                "host must match the request URI authority.")
               ((("host" . "example.test") ("host" . "example.test")) "host"
                "host must match the request URI authority.")))
    (destructuring-bind (headers name reason) row
      (let* ((events nil)
             (client
               (http-kit/http3:make-http3-client
                :open-stream (lambda (&rest args) (declare (ignore args))
                               (push :open events))
                :read-stream (lambda (&rest args) (declare (ignore args))
                               (push :read events))
                :write-stream (lambda (&rest args) (declare (ignore args))
                                (push :write events))
                :close-stream (lambda (&rest args) (declare (ignore args))
                                (push :close events))))
             (startup-events (copy-list events)))
        (unwind-protect
             (progn
               (http3-reachable-check-header-error
                (http3-boundary-error
                 (lambda ()
                   (http-kit/http3:send-http3-request
                    client (make-http-request :method "GET"
                                              :uri "https://example.test/"
                                              :headers headers))))
                name reason)
               (ensure-equal startup-events events)
               (ensure-true (http-kit/http3:http3-client-open-p client)))
          (http-kit/http3:close-http3-client client)))))
  (let* ((fixture (http3-boundary-fixture
                   (list (list (http3-boundary-headers '((":status" . "204"))) t))))
         (client (http3-boundary-fixture-client fixture)))
    (unwind-protect
         (let ((response (http-kit/http3:send-http3-request
                          client (make-http-request
                                  :method "GET" :uri "https://example.test/"
                                  :headers '(("host" . "EXAMPLE.TEST")
                                             ("te" . "trailers, trailers"))))))
           (ensure-equal 204 (http-response-status response))
           (http3-boundary-check-release fixture nil)
           (let* ((stream (first (http3-boundary-fixture-streams fixture)))
                  (frames (http3-server-coverage-frames
                           (reverse (http3-test-stream-writes stream))))
                  (fields (http-kit/http3:qpack-decode-field-section
                           (http-kit/http3:http3-frame-payload (first frames)))))
             (ensure-equal 1 (http-kit/http3:http3-frame-type (first frames)))
             (ensure-equal "trailers, trailers" (cdr (assoc "te" fields :test #'string=)))
             (ensure-equal nil (assoc "host" fields :test #'string=))
             (ensure-equal "example.test" (cdr (assoc ":authority" fields :test #'string=)))))
      (http-kit/http3:close-http3-client client))))

(deftest http3-reachable-client-classic-connect-wire-contract
  (let* ((fixture (http3-boundary-fixture
                   (list (list (http3-boundary-headers '((":status" . "200"))) t))))
         (client (http3-boundary-fixture-client fixture)))
    (unwind-protect
         (let ((response (http-kit/http3:send-http3-request
                          client (make-http-request :method "CONNECT"
                                                    :uri "https://example.test/ignored"))))
           (ensure-equal 200 (http-response-status response))
           (http3-boundary-check-release fixture nil)
           (let* ((stream (first (http3-boundary-fixture-streams fixture)))
                  (frames (http3-server-coverage-frames
                           (reverse (http3-test-stream-writes stream)))))
             (ensure-equal 1 (http-kit/http3:http3-frame-type (first frames)))
             (ensure-equal '((":method" . "CONNECT") (":authority" . "example.test"))
                           (http-kit/http3:qpack-decode-field-section
                            (http-kit/http3:http3-frame-payload (first frames))))))
      (http-kit/http3:close-http3-client client))))

(defstruct http3-qpack-contract-fixture client streams events)

(defun http3-qpack-contract-fixture (wire stream-id &key max-push-id)
  (let ((fixture (make-http3-qpack-contract-fixture)))
    (setf (http3-qpack-contract-fixture-client fixture)
          (http-kit/http3:make-http3-client
           :max-push-id max-push-id
           :open-stream
           (lambda (request &key stream-type timeout deadline)
             (declare (ignore timeout deadline))
             (let ((stream (make-http3-test-stream
                            :kind stream-type
                            :reads (when (eq stream-type :request)
                                     (list (list wire t))))))
               (push stream (http3-qpack-contract-fixture-streams fixture))
               (push (list :open stream request)
                     (http3-qpack-contract-fixture-events fixture))
               (values stream (when (eq stream-type :request) stream-id))))
           :write-stream
           (lambda (stream bytes &key fin-p timeout deadline)
             (declare (ignore timeout deadline))
             (push (list (copy-seq bytes) fin-p) (http3-test-stream-writes stream))
             (push (list :write stream fin-p)
                   (http3-qpack-contract-fixture-events fixture)))
           :read-stream
           (lambda (stream &key timeout deadline)
             (declare (ignore timeout deadline))
             (push (list :read stream) (http3-qpack-contract-fixture-events fixture))
             (ensure-true (consp (http3-test-stream-reads stream)))
             (let ((entry (pop (http3-test-stream-reads stream))))
               (values (first entry) (second entry))))
           :close-stream
           (lambda (stream &key condition)
             (ensure-false (http3-test-stream-closed-p stream))
             (setf (http3-test-stream-closed-p stream) t)
             (push (list :close stream condition)
                   (http3-qpack-contract-fixture-events fixture)))))
    (ensure-equal 3 (length (http3-qpack-contract-fixture-streams fixture)))
    (setf (http3-qpack-contract-fixture-events fixture) nil)
    fixture))

(defun http3-qpack-contract-settings (fixture &rest settings)
  (let* ((client (http3-qpack-contract-fixture-client fixture))
         (wire (http3-test-concat-octets
                (http-kit/http3:http3-varint-encode http-kit/http3:+http3-control-stream-type+)
                (http-kit/http3:encode-http3-frame
                 (apply #'http-kit/http3:make-http3-settings-frame settings))))
         (before (copy-seq wire))
         (stream (make-http3-test-stream :kind :peer-control :reads (list (list wire nil)))))
    (push stream (http3-qpack-contract-fixture-streams fixture))
    (ensure-true (eq client (http-kit/http3:attach-http3-peer-control-stream client stream)))
    (multiple-value-bind (effects fin-p) (http-kit/http3:read-http3-control-stream client)
      (ensure-equal '(:settings) effects)
      (ensure-false fin-p))
    (ensure-octets-equal before wire)))

(defun http3-qpack-contract-cleanup (fixture)
  (let ((client (http3-qpack-contract-fixture-client fixture)))
    (setf (http3-qpack-contract-fixture-events fixture) nil)
    (ensure-true (http-kit/http3:close-http3-client client))
    (ensure-false (http-kit/http3:http3-client-open-p client))
    (let* ((events (reverse (http3-qpack-contract-fixture-events fixture)))
           (streams (remove-if (lambda (stream)
                                 (eq :request (http3-test-stream-kind stream)))
                               (http3-qpack-contract-fixture-streams fixture))))
      (ensure-equal (length streams) (length events))
      (dolist (stream streams)
        (let ((matches (remove stream events :key #'second :test-not #'eq)))
          (ensure-equal 1 (length matches))
          (ensure-equal :close (first (first matches)))
          (ensure-true (eq nil (third (first matches))))))
      (dolist (stream (http3-qpack-contract-fixture-streams fixture))
        (ensure-true (http3-test-stream-closed-p stream)))
      (ensure-true (http-kit/http3:close-http3-client client))
      (ensure-equal events (reverse (http3-qpack-contract-fixture-events fixture))))))

(defun http3-qpack-contract-request-source (request)
  (let ((uri (http-request-uri request)))
    (prin1-to-string
     (list (http-request-method request)
           (list (http-uri-scheme uri) (http-uri-authority uri)
                 (http-uri-host uri) (http-uri-port uri) (http-uri-path uri)
                 (http-uri-query uri) (http-uri-string uri))
           (http-request-protocol request) (http-request-headers request)
           (http-request-body request)))))

(defun http3-qpack-contract-send-case
    (stream-id options-builder expected-operations &key setup message detail request)
  (let* ((wire (http3-boundary-headers '((":status" . "204"))))
         (before (copy-seq wire))
         (fixture (http3-qpack-contract-fixture wire stream-id))
         (client (http3-qpack-contract-fixture-client fixture))
         (request (or request (make-http-request :method "GET" :uri "https://example.test/")))
         (source (http3-qpack-contract-request-source request))
         (await-count 0)
         (opened-count 0))
    (unwind-protect
         (progn
           (when setup (funcall setup fixture))
           (setf (http3-qpack-contract-fixture-events fixture) nil)
           (let* ((options (funcall options-builder client
                                   (lambda (&rest args)
                                     (declare (ignore args))
                                     (incf await-count))))
                  (failure nil)
                  (response
                    (handler-case
                        (apply #'http-kit/http3:send-http3-request client request
                               :on-stream-open
                               (lambda (stream id cancel)
                                 (incf opened-count)
                                 (ensure-true (eq stream (first (http3-qpack-contract-fixture-streams fixture))))
                                 (ensure-equal stream-id id)
                                 (ensure-true (functionp cancel))
                                 (push (list :on-open stream)
                                       (http3-qpack-contract-fixture-events fixture)))
                               options)
                      (http-protocol-error (condition) (setf failure condition) nil))))
             (if message
                 (progn
                   (http3-public-error-check failure message detail)
                   (ensure-equal nil response))
                 (progn
                   (ensure-equal nil failure)
                   (ensure-equal 204 (http-response-status response))))
             (let* ((events (reverse (http3-qpack-contract-fixture-events fixture)))
                    (stream (find :request (http3-qpack-contract-fixture-streams fixture)
                                  :key #'http3-test-stream-kind)))
               (ensure-equal expected-operations (mapcar #'first events))
               (ensure-equal (count :on-open expected-operations) opened-count)
               (ensure-equal 0 await-count)
               (if (member :open expected-operations)
                   (progn
                     (ensure-true (eq request (third (first events))))
                     (dolist (event events) (ensure-true (eq stream (second event))))
                     (ensure-true (eq failure (third (car (last events)))))
                     (ensure-true (http3-test-stream-closed-p stream))
                     (ensure-equal (count :write expected-operations)
                                   (length (http3-test-stream-writes stream)))
                     (when (null message)
                       (ensure-equal '(nil t) (mapcar #'second
                                                     (reverse (http3-test-stream-writes stream))))))
                   (ensure-equal nil stream)))
             (ensure-true (http-kit/http3:http3-client-open-p client))
             (ensure-octets-equal before wire)
             (ensure-equal source (http3-qpack-contract-request-source request))))
      (http3-qpack-contract-cleanup fixture))))

(deftest http3-qpack-public-wire-extended-connect-contract
  (dolist (enabled '(0 1))
    (http3-qpack-contract-send-case
     0 (lambda (client await) (declare (ignore client await)) nil)
     (if (= enabled 1) '(:open :on-open :write :write :read :close) nil)
     :request (make-http-request :method "CONNECT" :protocol "websocket"
                                 :uri "https://example.test/chat")
     :setup (lambda (fixture)
              (http3-qpack-contract-settings fixture :enable-connect enabled))
     :message (when (= enabled 0)
                "Extended CONNECT requires peer SETTINGS_ENABLE_CONNECT_PROTOCOL=1.")
     :detail (when (= enabled 0) :h3-settings-error))))

(deftest http3-qpack-public-owned-decoder-auto-context-contract
  (dolist (mode '(:owned-no-id :custom-no-id :owned-id))
    (http3-qpack-contract-send-case
     (when (eq mode :owned-id) 0)
     (lambda (client await)
       (declare (ignore client))
       (append (list :await-qpack await)
               (when (eq mode :custom-no-id)
                 (list :qpack-decoder-table (http-kit/http3:make-qpack-dynamic-table)))))
     (if (eq mode :owned-no-id) '(:open :close)
         '(:open :on-open :write :write :read :close))
     :message (when (eq mode :owned-no-id)
                "Dynamic QPACK response decoding requires a numeric stream ID."))))

(deftest http3-qpack-public-explicit-context-await-contract
  (dolist (value '(nil :not-a-callback :function))
    (http3-qpack-contract-send-case
     0 (lambda (client await)
         (list :qpack-decoder-context (http-kit/http3:http3-client-qpack-decoder-context client)
               :await-qpack (if (eq value :function) await value)))
     (if (eq value :function) '(:open :on-open :write :write :read :close) '(:open :close))
     :message (unless (eq value :function)
                "Dynamic QPACK response decoding requires an await-qpack callback.")
     :detail (unless (eq value :function) value))))

(deftest http3-qpack-public-inserted-encoder-stream-id-contract
  (dolist (stream-id '(nil 0))
    (http3-qpack-contract-send-case
     stream-id (lambda (client await) (declare (ignore client await)) nil)
     (if stream-id '(:open :on-open :write :write :read :close) '(:open :on-open :close))
     :setup (lambda (fixture)
              (let ((client (http3-qpack-contract-fixture-client fixture))
                    (name (copy-seq "x-local")) (value (copy-seq "yes")))
                (http3-qpack-contract-settings fixture :qpack-max-table-capacity 256)
                (ensure-true (eq client (http-kit/http3:http3-client-set-qpack-capacity client 256)))
                (ensure-true (http-kit/http3:http3-client-insert-qpack-field client name value))
                (ensure-equal "x-local" name)
                (ensure-equal "yes" value)))
     :message (unless stream-id
                "OPEN-STREAM must return a numeric stream ID when dynamic QPACK entries exist."))))

(deftest http3-qpack-public-push-auto-context-contract
  (dolist (mode '(:owned-no-id :custom-no-id :owned-id))
    (let* ((fields '((":method" . "GET") (":scheme" . "https")
                     (":authority" . "example.test") (":path" . "/asset.css")))
           (fields-before (prin1-to-string fields))
           (wire (http3-test-concat-octets
                  (http-kit/http3:encode-http3-frame
                   (http-kit/http3:make-http3-frame
                    :type http-kit/http3:+http3-push-promise-type+
                    :payload (http3-test-concat-octets
                              (http-kit/http3:http3-varint-encode 2)
                              (http-kit/http3:qpack-encode-field-section fields))))
                  (http3-boundary-headers '((":status" . "204")))))
           (push-wire (http3-test-concat-octets
                       (http-kit/http3:http3-varint-encode http-kit/http3:+http3-push-stream-type+)
                       (http-kit/http3:http3-varint-encode 2)
                       (http3-boundary-headers '((":status" . "200")))))
           (wire-before (copy-seq wire))
           (push-before (copy-seq push-wire))
           (fixture (http3-qpack-contract-fixture wire 0 :max-push-id 2))
           (client (http3-qpack-contract-fixture-client fixture))
           (stream (make-http3-test-stream :kind :push :reads (list (list push-wire t))))
           (await-count 0)
           (promises nil))
      (unwind-protect
           (progn
             (ensure-equal 204
                           (http-response-status
                            (http-kit/http3:send-http3-request
                             client (make-http-request :method "GET" :uri "https://example.test/")
                             :on-push-promise (lambda (id promised)
                                                (push (list id promised) promises)))))
             (ensure-equal (list (list 2 fields)) promises)
             (ensure-equal '(2) (http-kit/http3:http3-client-promised-push-ids client))
             (let ((events (reverse (http3-qpack-contract-fixture-events fixture))))
               (ensure-equal '(:open :write :write :read :close) (mapcar #'first events))
               (ensure-true (eq nil (third (car (last events))))))
             (setf (http3-qpack-contract-fixture-events fixture) nil)
             (let* ((failure nil)
                    (result
                      (handler-case
                          (multiple-value-list
                           (apply #'http-kit/http3:receive-http3-push client stream
                                  :await-qpack (lambda (&rest args)
                                                 (declare (ignore args)) (incf await-count))
                                  (case mode
                                    (:custom-no-id
                                     (list :qpack-decoder-table (http-kit/http3:make-qpack-dynamic-table)))
                                    (:owned-id (list :stream-id 3)))))
                        (http-protocol-error (condition) (setf failure condition) nil))))
               (if (eq mode :owned-no-id)
                   (progn
                     (http3-public-error-check
                      failure "Dynamic QPACK response decoding requires a numeric stream ID." nil)
                     (ensure-equal nil result))
                   (progn
                     (ensure-equal nil failure)
                     (ensure-equal 2 (first result))
                     (ensure-equal 200 (http-response-status (second result)))
                     (ensure-octets-equal (octets) (http-response-body (second result)))))
               (let ((events (reverse (http3-qpack-contract-fixture-events fixture))))
                 (ensure-equal '(:read :close) (mapcar #'first events))
                 (dolist (event events) (ensure-true (eq stream (second event))))
                 (ensure-true (eq failure (third (second events)))))
               (ensure-equal 0 await-count)
               (ensure-equal nil (http3-test-stream-writes stream))
               (ensure-true (http3-test-stream-closed-p stream))
               (ensure-true (http-kit/http3:http3-client-open-p client))
               (ensure-octets-equal wire-before wire)
               (ensure-octets-equal push-before push-wire)
               (ensure-equal fields-before (prin1-to-string fields))))
        (http3-qpack-contract-cleanup fixture)))))

(defun http3-five-manager-case (options &key invalid-manager invalid-request message detail)
  (let* ((request (make-http-request :method "GET" :uri "https://example.test/"))
         (source (http3-qpack-contract-request-source request))
         (options-source (prin1-to-string options))
         (fixture nil)
         (opens 0)
         (callbacks 0)
         (manager
           (http-kit/http3:make-http3-connection-manager
            :open-client
            (lambda (opened-request &key timeout deadline)
              (declare (ignore timeout deadline))
              (incf opens)
              (ensure-true (eq request opened-request))
              (setf fixture (http3-qpack-contract-fixture
                             (http3-boundary-headers '((":status" . "204"))) 0))
              (http3-qpack-contract-fixture-client fixture)))))
    (unwind-protect
         (let ((failure nil)
               (response nil))
           (handler-case
               (setf response
                     (apply #'http-kit/http3:send-http3-request-over-connection-manager
                            (if invalid-manager nil manager)
                            (if invalid-request 17 request)
                            :on-stream-open
                            (lambda (stream id cancel)
                              (incf callbacks)
                              (ensure-equal 0 id)
                              (ensure-true (functionp cancel))
                              (push (list :callback stream)
                                    (http3-qpack-contract-fixture-events fixture)))
                            options))
             (http-protocol-error (condition) (setf failure condition)))
           (if message
               (progn
                 (http3-public-error-check failure message detail)
                 (if invalid-request
                     (ensure-equal (type-of 17) (http-protocol-error-detail failure))
                     (ensure-true (eq detail (http-protocol-error-detail failure))))
                 (ensure-equal nil response)
                 (ensure-equal 0 opens)
                 (ensure-equal 0 callbacks)
                 (ensure-equal nil fixture)
                 (ensure-equal 0 (http-kit/http3:http3-connection-manager-connection-count manager)))
               (let* ((events (reverse (http3-qpack-contract-fixture-events fixture)))
                      (stream (find :request (http3-qpack-contract-fixture-streams fixture)
                                    :key #'http3-test-stream-kind)))
                 (ensure-equal nil failure)
                 (ensure-equal 204 (http-response-status response))
                 (ensure-equal 1 opens)
                 (ensure-equal 1 callbacks)
                 (ensure-equal '(:open :callback :write :write :read :close)
                               (mapcar #'first events))
                 (dolist (event events) (ensure-true (eq stream (second event))))
                 (ensure-true (eq nil (third (car (last events)))))
                 (ensure-equal 2 (length (http3-test-stream-writes stream)))
                 (ensure-equal 1 (http-kit/http3:http3-connection-manager-connection-count manager))))
           (ensure-true (http-kit/http3:http3-connection-manager-open-p manager))
           (ensure-equal source (http3-qpack-contract-request-source request))
           (ensure-equal options-source (prin1-to-string options)))
      (http-kit/http3:close-http3-connection-manager manager)
      (ensure-false (http-kit/http3:http3-connection-manager-open-p manager))
      (ensure-equal 0 (http-kit/http3:http3-connection-manager-connection-count manager))
      (when fixture
        (ensure-false (http-kit/http3:http3-client-open-p
                       (http3-qpack-contract-fixture-client fixture)))
        (let ((events (http3-qpack-contract-fixture-events fixture)))
          (dolist (stream (http3-qpack-contract-fixture-streams fixture))
            (ensure-true (http3-test-stream-closed-p stream))
            (ensure-equal 1 (count-if (lambda (event)
                                      (and (eq :close (first event))
                                           (eq stream (second event)))) events)))))
      (ensure-true (http-kit/http3:close-http3-connection-manager manager)))))

(deftest http3-five-public-proxy-plan-shapes
  (dolist (plan (list 17 (cons :mode :direct) (list "mode" :direct)))
    (http3-five-manager-case
     (list :proxy-plan plan)
     :message "HTTP/3 proxy routing is not supported by this transport." :detail plan))
  (http3-five-manager-case (list :proxy-plan :direct))
  (http3-five-manager-case
   (list :proxy-plan (http-proxy-plan nil "https://example.test/"))))

(deftest http3-five-public-manager-preflight
  (http3-five-manager-case nil :invalid-manager t
                           :message "An HTTP/3 request requires a connection manager."
                           :detail (type-of nil))
  (http3-five-manager-case nil :invalid-request t
                           :message "The HTTP/3 manager requires an HTTP request."
                           :detail (type-of 17))
  (dolist (limit (list 0 -1 "1"))
    (http3-five-manager-case
     (list :max-header-bytes limit)
     :message "MAX-HEADER-BYTES must be NIL or a positive integer." :detail limit))
  (http3-five-manager-case (list :max-header-bytes nil))
  (http3-five-manager-case (list :max-header-bytes 4096)))

(defun http3-five-response-case (wire &key header-reason message detail body)
  (let* ((before (if (vectorp wire) (copy-seq wire) wire))
         (fixture (http3-qpack-contract-fixture wire 0))
         (client (http3-qpack-contract-fixture-client fixture))
         (request (make-http-request :method "GET" :uri "https://example.test/"))
         (source (http3-qpack-contract-request-source request))
         (chunks nil)
         (response nil)
         (failure nil))
    (unwind-protect
         (progn
           (handler-case
               (setf response
                     (http-kit/http3:send-http3-request
                      client request
                      :on-body-chunk
                      (lambda (chunk)
                        (push (copy-seq chunk) chunks)
                        (push (list :body (find :request
                                                (http3-qpack-contract-fixture-streams fixture)
                                                :key #'http3-test-stream-kind))
                              (http3-qpack-contract-fixture-events fixture)))))
             (http-protocol-error (condition) (setf failure condition)))
           (cond (header-reason
                  (http3-reachable-check-header-error failure "content-length" header-reason)
                  (ensure-equal nil response))
                 (message
                  (http3-public-error-check failure message detail)
                  (ensure-equal nil response))
                 (t
                  (ensure-equal nil failure)
                  (ensure-equal (if body 200 204) (http-response-status response))
                  (when body (ensure-octets-equal body (http-response-body response)))))
           (let* ((events (reverse (http3-qpack-contract-fixture-events fixture)))
                  (stream (find :request (http3-qpack-contract-fixture-streams fixture)
                                :key #'http3-test-stream-kind)))
             (ensure-equal (if body '(:open :write :write :read :body :close)
                              '(:open :write :write :read :close))
                           (mapcar #'first events))
             (ensure-true (eq request (third (first events))))
             (dolist (event events) (ensure-true (eq stream (second event))))
             (ensure-true (eq failure (third (car (last events)))))
             (ensure-equal 2 (length (http3-test-stream-writes stream)))
             (ensure-true (http3-test-stream-closed-p stream)))
           (ensure-equal (if body 1 0) (length chunks))
           (when body (ensure-octets-equal body (first chunks)))
           (ensure-true (http-kit/http3:http3-client-open-p client))
           (if (vectorp wire) (ensure-octets-equal before wire) (ensure-equal before wire))
           (ensure-equal source (http3-qpack-contract-request-source request)))
      (http3-qpack-contract-cleanup fixture))))

(deftest http3-five-public-response-content-length
  (dolist (fields '(((":status" . "200") ("content-length" . "3x"))
                    ((":status" . "200") ("content-length" . "0")
                     ("content-length" . "0"))))
    (let ((before (copy-tree fields)))
      (http3-five-response-case
       (http3-boundary-headers fields)
       :header-reason "content-length is invalid or duplicated.")
      (ensure-equal before fields)))
  (let* ((body (octets 97 98 99))
         (before (copy-seq body))
         (data (http-kit/http3:encode-http3-frame
                (http-kit/http3:make-http3-frame
                 :type http-kit/http3:+http3-data-type+ :payload body))))
    (dolist (length '("2" "3"))
      (http3-five-response-case
       (http3-test-concat-octets
        (http3-boundary-headers (list (cons ":status" "200")
                                      (cons "content-length" length))) data)
       :body body
       :header-reason (when (string= length "2")
                        "content-length does not match the response body.")))
    (ensure-octets-equal before body))
  (http3-five-response-case (http3-boundary-headers '((":status" . "204")))))

(defun http3-five-promise-wire (fields)
  (http3-test-concat-octets
   (http-kit/http3:encode-http3-frame
    (http-kit/http3:make-http3-frame
     :type http-kit/http3:+http3-push-promise-type+
     :payload (http3-test-concat-octets
               (http-kit/http3:http3-varint-encode 2)
               (http-kit/http3:qpack-encode-field-section fields))))
   (http3-boundary-headers '((":status" . "204")))))

(deftest http3-five-public-repeated-push-promise
  (let ((fields '((":method" . "GET") (":scheme" . "https")
                  (":authority" . "example.test") (":path" . "/p")
                  ("x-a" . "same"))))
    (dolist (repeat (list (append fields '(("x-b" . "extra")))
                         (append (butlast fields) '(("x-b" . "same")))
                         (copy-tree fields)))
      (let* ((fields-before (copy-tree fields))
             (repeat-before (copy-tree repeat))
             (wire (http3-five-promise-wire fields))
             (wire-before (copy-seq wire))
             (repeat-wire (http3-five-promise-wire repeat))
             (repeat-wire-before (copy-seq repeat-wire))
             (fixture (http3-qpack-contract-fixture wire 0 :max-push-id 2))
             (client (http3-qpack-contract-fixture-client fixture))
             (request (make-http-request :method "GET" :uri "https://example.test/"))
             (source (http3-qpack-contract-request-source request))
             (calls 0))
        (unwind-protect
             (progn
              (dotimes (iteration 2)
               (setf (http3-qpack-contract-fixture-events fixture) nil)
               (let ((failure nil)
                     (response nil)
                     (calls-before calls))
                 (handler-case
                     (setf response
                           (http-kit/http3:send-http3-request
                            client request
                            :on-stream-open
                            (lambda (stream id cancel)
                              (ensure-equal 0 id)
                              (ensure-true (functionp cancel))
                              (when (= iteration 1)
                                (setf (http3-test-stream-reads stream)
                                      (list (list repeat-wire t)))))
                            :on-push-promise
                            (lambda (id received)
                              (incf calls)
                              (ensure-equal 2 id)
                              (ensure-equal fields received)
                              (ensure-false (eq received fields))
                              (push (list :promise
                                          (find :request
                                                (http3-qpack-contract-fixture-streams fixture)
                                                :key #'http3-test-stream-kind))
                                    (http3-qpack-contract-fixture-events fixture)))))
                   (http-protocol-error (condition) (setf failure condition)))
                 (let ((rejected (and (= iteration 1) (not (equal fields repeat)))))
                   (if rejected
                       (progn
                         (http3-public-error-check
                          failure "Repeated HTTP/3 PUSH_PROMISE fields differ." :h3-id-error)
                         (ensure-equal nil response))
                       (progn
                         (ensure-equal nil failure)
                         (ensure-equal 204 (http-response-status response))))
                   (ensure-equal (if rejected 0 1) (- calls calls-before))
                   (let* ((events (reverse (http3-qpack-contract-fixture-events fixture)))
                          (stream (second (first events))))
                     (ensure-equal (if rejected '(:open :write :write :read :close)
                                      '(:open :write :write :read :promise :close))
                                   (mapcar #'first events))
                     (dolist (event events) (ensure-true (eq stream (second event))))
                     (ensure-true (eq failure (third (car (last events)))))
                     (ensure-equal 2 (length (http3-test-stream-writes stream)))
                     (ensure-true (http3-test-stream-closed-p stream))))
                 (ensure-equal '(2) (http-kit/http3:http3-client-promised-push-ids client))
                 (ensure-equal fields-before fields)
                 (ensure-equal repeat-before repeat)
                 (ensure-octets-equal wire-before wire)
                 (ensure-octets-equal repeat-wire-before repeat-wire)
                 (ensure-equal source (http3-qpack-contract-request-source request))))
          (let* ((push-wire
                   (http3-test-concat-octets
                    (http-kit/http3:http3-varint-encode http-kit/http3:+http3-push-stream-type+)
                    (http-kit/http3:http3-varint-encode 2)
                    (http3-boundary-headers '((":status" . "200")))))
                 (push-before (copy-seq push-wire))
                 (stream (make-http3-test-stream :kind :push
                                                 :reads (list (list push-wire t)))))
            (setf (http3-qpack-contract-fixture-events fixture) nil)
            (multiple-value-bind (id response) (http-kit/http3:receive-http3-push client stream)
              (ensure-equal 2 id)
              (ensure-equal 200 (http-response-status response)))
            (let ((events (reverse (http3-qpack-contract-fixture-events fixture))))
              (ensure-equal '(:read :close) (mapcar #'first events))
              (dolist (event events) (ensure-true (eq stream (second event))))
              (ensure-true (eq nil (third (second events)))))
            (ensure-equal nil (http3-test-stream-writes stream))
            (ensure-true (http3-test-stream-closed-p stream))
            (ensure-true (http-kit/http3:http3-client-open-p client))
            (ensure-octets-equal push-before push-wire)))
          (http3-qpack-contract-cleanup fixture))))))

(deftest http3-five-public-read-callback-type
  (http3-five-response-case
   17 :message "HTTP/3 read-stream must return an octet vector or NIL."
   :detail (type-of 17))
  (http3-five-response-case (http3-boundary-headers '((":status" . "204"))))
  (dolist (invalid '(t nil))
    (let* ((fields '((":method" . "GET") (":scheme" . "https")
                     (":authority" . "example.test") (":path" . "/p")))
           (wire (http3-five-promise-wire fields))
           (before (copy-seq wire))
           (push-wire (http3-test-concat-octets
                       (http-kit/http3:http3-varint-encode http-kit/http3:+http3-push-stream-type+)
                       (http-kit/http3:http3-varint-encode 2)
                       (http3-boundary-headers '((":status" . "200")))))
           (push-before (copy-seq push-wire))
           (fixture (http3-qpack-contract-fixture wire 0 :max-push-id 2))
           (client (http3-qpack-contract-fixture-client fixture))
           (request (make-http-request :method "GET" :uri "https://example.test/"))
           (source (http3-qpack-contract-request-source request))
           (fields-before (copy-tree fields))
           (stream (make-http3-test-stream :kind :push
                                           :reads (list (list (if invalid 17 push-wire) t))))
           (promise-calls 0)
           (response nil)
           (failure nil))
      (unwind-protect
           (progn
             (ensure-equal 204
                           (http-response-status
                            (http-kit/http3:send-http3-request
                             client request
                             :on-push-promise
                             (lambda (id received)
                               (incf promise-calls)
                               (ensure-equal 2 id)
                               (ensure-equal fields received)))))
             (ensure-equal 1 promise-calls)
             (setf (http3-qpack-contract-fixture-events fixture) nil)
             (handler-case
                 (multiple-value-bind (id received) (http-kit/http3:receive-http3-push client stream)
                   (ensure-equal 2 id)
                   (setf response received))
               (http-protocol-error (condition) (setf failure condition)))
             (if invalid
                 (progn
                   (http3-public-error-check
                    failure "HTTP/3 read-stream must return an octet vector or NIL." (type-of 17))
                   (ensure-equal nil response))
                 (progn
                   (ensure-equal nil failure)
                   (ensure-equal 200 (http-response-status response))))
             (let ((events (reverse (http3-qpack-contract-fixture-events fixture))))
               (ensure-equal '(:read :close) (mapcar #'first events))
               (dolist (event events) (ensure-true (eq stream (second event))))
               (ensure-true (eq failure (third (second events)))))
             (ensure-equal nil (http3-test-stream-writes stream))
             (ensure-true (http3-test-stream-closed-p stream))
             (ensure-true (http-kit/http3:http3-client-open-p client))
             (ensure-octets-equal before wire)
             (ensure-octets-equal push-before push-wire)
             (ensure-equal fields-before fields)
             (ensure-equal source (http3-qpack-contract-request-source request)))
        (http3-qpack-contract-cleanup fixture)))))

(deftest http3-release-public-producer-noninteger-octet
  (dolist (invalid '(t nil))
    (let* ((chunk (if invalid (vector "bad") (vector 0 255)))
           (chunk-before (copy-seq chunk))
           (wire (http3-boundary-headers '((":status" . "204"))))
           (wire-before (copy-seq wire))
           (fixture (http3-qpack-contract-fixture wire 0))
           (client (http3-qpack-contract-fixture-client fixture))
           (request (make-http-request :method "POST" :uri "https://example.test/"))
           (source (http3-qpack-contract-request-source request))
           (calls 0) (response nil) (failure nil))
      (unwind-protect
           (progn
             (handler-case
                 (setf response
                       (http-kit/http3:send-http3-request
                        client request :request-body-function
                        (lambda (maximum)
                          (ensure-equal 16384 maximum)
                          (incf calls)
                          (push (list :produce calls)
                                (http3-qpack-contract-fixture-events fixture))
                          (when (= calls 1) chunk))))
               (http-protocol-error (condition) (setf failure condition)))
             (let* ((events (reverse (http3-qpack-contract-fixture-events fixture)))
                    (stream (second (first events)))
                    (writes (reverse (http3-test-stream-writes stream)))
                    (frames (http3-server-coverage-frames writes)))
               (ensure-equal (if invalid
                                 '(:open :write :produce :close)
                                 '(:open :write :produce :write :produce :write :read :close))
                             (mapcar #'first events))
               (ensure-true (eq request (third (first events))))
               (dolist (event (remove :produce events :key #'first))
                 (ensure-true (eq stream (second event))))
               (ensure-true (eq failure (third (car (last events)))))
               (ensure-true (http3-test-stream-closed-p stream))
               (ensure-equal (if invalid 1 2) calls)
               (ensure-equal (if invalid '(nil) '(nil nil t)) (mapcar #'second writes))
               (ensure-equal http-kit/http3:+http3-headers-type+
                             (http-kit/http3:http3-frame-type (first frames)))
               (if invalid
                   (progn
                     (http3-public-error-check
                      failure "An HTTP/3 request body chunk contains a non-octet value."
                      (aref chunk 0))
                     (ensure-true (eq (aref chunk 0) (http-protocol-error-detail failure)))
                     (ensure-equal nil response)
                     (ensure-equal 1 (length frames)))
                   (progn
                     (ensure-equal nil failure)
                     (ensure-equal 204 (http-response-status response))
                     (ensure-equal 3 (length frames))
                     (dolist (frame (rest frames))
                       (ensure-equal http-kit/http3:+http3-data-type+
                                     (http-kit/http3:http3-frame-type frame)))
                     (ensure-octets-equal (octets 0 255)
                                          (http-kit/http3:http3-frame-payload (second frames)))
                     (ensure-octets-equal (octets)
                                          (http-kit/http3:http3-frame-payload (third frames))))))
             (ensure-true (http-kit/http3:http3-client-open-p client))
             (ensure-true (equalp chunk-before chunk))
             (ensure-octets-equal wire-before wire)
             (ensure-equal source (http3-qpack-contract-request-source request)))
        (http3-qpack-contract-cleanup fixture)))))

(deftest http3-release-public-fixed-body-frame-boundary
  (dolist (size '(16384 16385))
    (let* ((body (make-array size :element-type '(unsigned-byte 8) :initial-element 97))
           (body-before (copy-seq body))
           (wire (http3-boundary-headers '((":status" . "204"))))
           (wire-before (copy-seq wire))
           (fixture (http3-qpack-contract-fixture wire 0))
           (client (http3-qpack-contract-fixture-client fixture))
           (request (make-http-request :method "POST" :uri "https://example.test/" :body body))
           (source (http3-qpack-contract-request-source request)))
      (unwind-protect
           (progn
             (ensure-equal 204 (http-response-status (http-kit/http3:send-http3-request client request)))
             (let* ((events (reverse (http3-qpack-contract-fixture-events fixture)))
                    (stream (second (first events)))
                    (writes (reverse (http3-test-stream-writes stream)))
                    (frames (http3-server-coverage-frames writes))
                    (data (rest frames)))
               (ensure-equal (if (= size 16384)
                                 '(:open :write :write :read :close)
                                 '(:open :write :write :write :read :close))
                             (mapcar #'first events))
               (dolist (event events) (ensure-true (eq stream (second event))))
               (ensure-true (eq request (third (first events))))
               (ensure-true (eq nil (third (car (last events)))))
               (ensure-true (http3-test-stream-closed-p stream))
               (ensure-equal (if (= size 16384) '(nil t) '(nil nil t))
                             (mapcar #'second writes))
               (ensure-equal http-kit/http3:+http3-headers-type+
                             (http-kit/http3:http3-frame-type (first frames)))
               (ensure-equal (if (= size 16384) '(16384) '(16384 1))
                             (mapcar (lambda (frame)
                                       (length (http-kit/http3:http3-frame-payload frame))) data))
               (dolist (frame data)
                 (ensure-equal http-kit/http3:+http3-data-type+
                               (http-kit/http3:http3-frame-type frame)))
               (ensure-octets-equal body
                                    (apply #'http3-test-concat-octets
                                           (mapcar #'http-kit/http3:http3-frame-payload data))))
             (ensure-octets-equal body-before body)
             (ensure-octets-equal wire-before wire)
             (ensure-equal source (http3-qpack-contract-request-source request)))
        (http3-qpack-contract-cleanup fixture)))))

(deftest http3-release-public-server-authority-host
  (dolist (host '("other.test" "EXAMPLE.TEST"))
    (let* ((invalid (string= host "other.test"))
           (fields (list '(":method" . "GET") '(":scheme" . "https")
                         '(":authority" . "example.test") '(":path" . "/")
                         (cons "host" host)))
           (fields-before (copy-tree fields))
           (wire (http3-boundary-headers fields))
           (wire-before (copy-seq wire))
           (stream (make-http3-test-stream :kind :request))
           (events nil) (writes nil) (failure nil) (response nil)
           (expected-response (make-http-response :status 204)))
      (handler-case
          (setf response
                (http-kit/http3:serve-http3-request-stream
                 stream
                 (lambda (request)
                   (push :handler events)
                   (ensure-equal host (http-header-value (http-request-headers request) "host"))
                   expected-response)
                 :read-stream
                 (lambda (received &key timeout deadline)
                   (declare (ignore timeout deadline))
                   (ensure-true (eq stream received))
                   (ensure-equal nil events)
                   (push :read events)
                   (values wire t))
                 :write-stream
                 (lambda (received bytes &key fin-p timeout deadline)
                   (declare (ignore timeout deadline))
                   (ensure-true (eq stream received))
                   (push :write events)
                   (push (list (copy-seq bytes) fin-p) writes))
                 :on-error
                 (lambda (condition request)
                   (ensure-true (typep condition 'http-invalid-header))
                   (ensure-equal nil request)
                   (push (list :error condition) events))
                 :close-stream
                 (lambda (received &key condition)
                   (ensure-true (eq stream received))
                   (ensure-false (http3-test-stream-closed-p stream))
                   (setf (http3-test-stream-closed-p stream) t)
                   (push (list :close condition) events))))
        (http-invalid-header (condition) (setf failure condition)))
      (let ((ordered (reverse events)))
        (ensure-equal (if invalid '(:read :error :close) '(:read :handler :write :close))
                      (mapcar (lambda (event) (if (consp event) (first event) event)) ordered))
        (ensure-true (eq failure (second (car (last ordered)))))
        (if invalid
            (progn
              (http3-reachable-check-header-error failure "host" "host must match :authority.")
              (ensure-true (eq failure (second (second ordered))))
              (ensure-equal nil response)
              (ensure-equal nil writes))
            (progn
              (ensure-equal nil failure)
              (ensure-true (eq expected-response response))
              (ensure-equal '(t) (mapcar #'second writes))
              (let ((frames (http3-server-coverage-frames (reverse writes))))
                (ensure-equal 1 (length frames))
                (ensure-equal http-kit/http3:+http3-headers-type+
                              (http-kit/http3:http3-frame-type (first frames)))))))
      (ensure-true (http3-test-stream-closed-p stream))
      (ensure-equal fields-before fields)
      (ensure-octets-equal wire-before wire))))

(deftest http3-release-public-peer-encoder-prefix-fin
  (dolist (row (list (list (octets) t :qpack-encoder-stream-error
                          "The peer QPACK encoder stream ended with truncated input.")
                    (list (octets 2) t :h3-closed-critical-stream
                          "The peer closed the QPACK encoder stream.")
                    (list (octets 2) nil nil nil)))
    (destructuring-bind (wire fin-p detail message) row
      (let* ((before (copy-seq wire))
             (fixture (http3-qpack-contract-fixture (octets) 0))
             (client (http3-qpack-contract-fixture-client fixture))
             (peer (make-http3-test-stream :kind :peer-encoder :reads (list (list wire fin-p))))
             (local-streams (reverse (http3-qpack-contract-fixture-streams fixture)))
             (failure nil))
        (push peer (http3-qpack-contract-fixture-streams fixture))
        (unwind-protect
             (progn
               (ensure-true (eq client (http-kit/http3:attach-http3-peer-qpack-encoder-stream client peer)))
               (handler-case
                   (multiple-value-bind (effects ended-p)
                       (http-kit/http3:read-http3-qpack-encoder-stream client)
                     (ensure-equal nil effects)
                     (ensure-false ended-p))
                 (http-protocol-error (condition) (setf failure condition)))
               (if detail
                   (http3-public-error-check failure message detail)
                   (ensure-equal nil failure))
               (ensure-equal (list (list :read peer))
                             (reverse (http3-qpack-contract-fixture-events fixture)))
               (ensure-true (http-kit/http3:http3-client-open-p client))
               (ensure-false (http3-test-stream-closed-p peer))
               (ensure-equal nil (http3-test-stream-writes peer))
               (ensure-octets-equal before wire))
          (ensure-true (http-kit/http3:close-http3-client client :condition failure))
          (ensure-false (http-kit/http3:http3-client-open-p client))
          (let ((events (reverse (http3-qpack-contract-fixture-events fixture))))
            (ensure-equal '(:read :close :close :close :close) (mapcar #'first events))
            (ensure-equal (append local-streams (list peer)) (mapcar #'second (rest events)))
            (dolist (event (rest events))
              (ensure-true (eq failure (third event)))
              (ensure-true (http3-test-stream-closed-p (second event))))
            (ensure-true (http-kit/http3:close-http3-client client :condition failure))
            (ensure-equal events (reverse (http3-qpack-contract-fixture-events fixture)))))))))

(deftest http3-release-public-manager-closed-retained-client
  (dolist (close-first '(t nil))
    (let* ((request (make-http-request :method "GET" :uri "https://example.test/"))
           (source (http3-qpack-contract-request-source request))
           (wire (http3-boundary-headers '((":status" . "204"))))
           (before (copy-seq wire))
           (fixtures nil) (opens 0)
           (manager (http-kit/http3:make-http3-connection-manager
                     :open-client
                     (lambda (received &key timeout deadline)
                       (declare (ignore timeout deadline))
                       (ensure-true (eq request received))
                       (incf opens)
                       (let ((fixture (http3-qpack-contract-fixture wire 0)))
                         (push fixture fixtures)
                         (http3-qpack-contract-fixture-client fixture))))))
      (unwind-protect
           (progn
             (ensure-equal 204
                           (http-response-status
                            (http-kit/http3:send-http3-request-over-connection-manager manager request)))
             (let* ((first-fixture (first fixtures))
                    (first-client (http3-qpack-contract-fixture-client first-fixture))
                    (first-events nil))
               (ensure-equal 1 opens)
               (ensure-equal (list first-client) (http-kit/http3:http3-connection-manager-clients manager))
               (when close-first
                 (ensure-true (http-kit/http3:close-http3-client first-client))
                 (ensure-false (http-kit/http3:http3-client-open-p first-client)))
               (setf first-events (copy-tree (http3-qpack-contract-fixture-events first-fixture)))
               (ensure-equal 204
                             (http-response-status
                              (http-kit/http3:send-http3-request-over-connection-manager manager request)))
               (ensure-equal (if close-first 2 1) opens)
               (ensure-equal 1 (http-kit/http3:http3-connection-manager-connection-count manager))
               (let* ((current (first fixtures))
                      (client (http3-qpack-contract-fixture-client current))
                      (events (reverse (http3-qpack-contract-fixture-events current))))
                 (ensure-equal (list client) (http-kit/http3:http3-connection-manager-clients manager))
                 (ensure-true (http-kit/http3:http3-client-open-p client))
                 (ensure-equal (if close-first '(:open :write :write :read :close)
                                   '(:open :write :write :read :close :open :write :write :read :close))
                               (mapcar #'first events))
                 (dolist (event (remove :close events :key #'first))
                   (ensure-false (member (second event)
                                         (remove :request (http3-qpack-contract-fixture-streams current)
                                                 :key #'http3-test-stream-kind) :test #'eq)))
                 (dolist (event (remove :close events :key #'first :test-not #'eq))
                   (ensure-true (eq nil (third event))))
                 (if close-first
                     (progn
                       (ensure-false (eq first-client client))
                       (ensure-false (http-kit/http3:http3-client-open-p first-client))
                       (ensure-equal first-events (http3-qpack-contract-fixture-events first-fixture)))
                     (ensure-true (eq first-client client))))
               (ensure-equal source (http3-qpack-contract-request-source request))
               (ensure-octets-equal before wire)))
        (ensure-true (http-kit/http3:close-http3-connection-manager manager))
        (ensure-false (http-kit/http3:http3-connection-manager-open-p manager))
        (ensure-equal 0 (http-kit/http3:http3-connection-manager-connection-count manager))
        (dolist (fixture fixtures)
          (ensure-false (http-kit/http3:http3-client-open-p (http3-qpack-contract-fixture-client fixture)))
          (dolist (stream (http3-qpack-contract-fixture-streams fixture))
            (ensure-true (http3-test-stream-closed-p stream)))
          (let ((events (http3-qpack-contract-fixture-events fixture)))
            (dolist (stream (http3-qpack-contract-fixture-streams fixture))
              (ensure-equal 1 (count stream (remove :close events :key #'first :test-not #'eq)
                                     :key #'second :test #'eq)))))
        (let ((events (mapcar #'http3-qpack-contract-fixture-events fixtures)))
          (ensure-true (http-kit/http3:close-http3-connection-manager manager))
          (ensure-equal events (mapcar #'http3-qpack-contract-fixture-events fixtures)))))))

(deftest http3-release-public-lifecycle-settings-noninteger-id
  (dolist (id '("1" :setting 33))
    (let* ((pair (cons id 7))
           (settings (list pair))
           (before (copy-tree settings))
           (events nil)
           (streams nil)
           (client nil)
           (failure nil))
      (unwind-protect
           (progn
             (handler-case
                 (setf client
                       (http3-transport-boundary-client
                        :qpack-settings settings
                        :open-stream
                        (lambda (request &key stream-type timeout deadline)
                          (declare (ignore timeout deadline))
                          (ensure-true (eq nil request))
                          (let ((stream (make-http3-test-stream :kind stream-type)))
                            (push stream streams)
                            (push (list :open stream) events)
                            stream))
                        :write-stream
                        (lambda (stream bytes &key fin-p timeout deadline)
                          (declare (ignore timeout deadline))
                          (push (list (copy-seq bytes) fin-p)
                                (http3-test-stream-writes stream))
                          (push (list :write stream) events))
                        :read-stream
                        (lambda (stream &key timeout deadline)
                          (declare (ignore timeout deadline))
                          (push (list :read stream) events)
                          (values nil t))
                        :close-stream
                        (lambda (stream &key condition)
                          (ensure-false (http3-test-stream-closed-p stream))
                          (setf (http3-test-stream-closed-p stream) t)
                          (push (list :close stream condition) events))))
               (http-protocol-error (condition) (setf failure condition)))
             (ensure-equal before settings)
             (ensure-true (eq pair (first settings)))
             (if (integerp id)
                 (progn
                   (ensure-true (eq nil failure))
                   (ensure-true (http-kit/http3:http3-client-open-p client))
                   (ensure-equal '(:open :open :open :write :write :write)
                                 (mapcar #'first (reverse events)))
                   (ensure-equal '(:control :qpack-encoder :qpack-decoder)
                                 (mapcar #'http3-test-stream-kind (reverse streams)))
                   (ensure-equal settings (http-kit/http3:http3-client-qpack-settings client))
                   (ensure-false (eq settings (http-kit/http3:http3-client-qpack-settings client)))
                   (ensure-false (eq pair (first (http-kit/http3:http3-client-qpack-settings client))))
                   (let* ((control (find :control streams :key #'http3-test-stream-kind))
                          (writes (http3-test-stream-writes control))
                          (wire (first (first writes))))
                     (ensure-equal 1 (length writes))
                     (ensure-false (second (first writes)))
                     (multiple-value-bind (type position) (http-kit/http3:http3-varint-decode wire)
                       (ensure-equal http-kit/http3:+http3-control-stream-type+ type)
                       (multiple-value-bind (frames remainder)
                           (http-kit/http3:decode-http3-frames (subseq wire position))
                         (ensure-equal 1 (length frames))
                         (ensure-equal 0 (length remainder))
                         (ensure-equal http-kit/http3:+http3-settings-type+
                                       (http-kit/http3:http3-frame-type (first frames)))
                         (ensure-equal 7
                                       (cdr (assoc 33 (http-kit/http3:decode-http3-settings
                                                      (http-kit/http3:http3-frame-payload (first frames))))))))))
                 (progn
                   (http3-public-error-check
                    failure
                    "HTTP/3 additional settings must be (integer . non-negative-integer) pairs."
                    pair)
                   (ensure-true (eq pair (http-protocol-error-detail failure)))
                   (ensure-true (eq nil client))
                   (dolist (operation '(:open :write :read :close))
                     (ensure-equal 0 (count operation events :key #'first)))
                   (ensure-equal nil streams))))
        (when client
          (setf events nil)
          (ensure-true (http-kit/http3:close-http3-client client))
          (ensure-false (http-kit/http3:http3-client-open-p client))
          (ensure-equal '(:control :qpack-encoder :qpack-decoder)
                        (mapcar (lambda (event) (http3-test-stream-kind (second event)))
                                (reverse events)))
          (dolist (stream streams)
            (ensure-true (http3-test-stream-closed-p stream))
            (let ((matches (remove stream events :key #'second :test-not #'eq)))
              (ensure-equal 1 (length matches))
              (ensure-equal :close (first (first matches)))
              (ensure-true (eq nil (third (first matches))))))
          (let ((closed-events (copy-list events)))
            (ensure-true (http-kit/http3:close-http3-client client))
            (ensure-equal closed-events events))))
      (ensure-equal before settings))))

(deftest http3-release-public-lifecycle-goaway-wire-boundaries
  (dolist (id '(nil "0" -1 4 0))
    (let* ((response-wire (http3-boundary-headers '((":status" . "204"))))
           (response-before (copy-seq response-wire))
           (fixture (http3-qpack-contract-fixture response-wire id))
           (client (http3-qpack-contract-fixture-client fixture))
           (peer-wire (http3-test-concat-octets
                       (http-kit/http3:http3-varint-encode http-kit/http3:+http3-control-stream-type+)
                       (http-kit/http3:encode-http3-frame (http-kit/http3:make-http3-settings-frame))
                       (http-kit/http3:encode-http3-frame
                        (http-kit/http3:make-http3-frame
                         :type http-kit/http3:+http3-goaway-type+
                         :payload (http-kit/http3:http3-varint-encode 4)))))
           (peer-before (copy-seq peer-wire))
           (peer (make-http3-test-stream :kind :peer-control :reads (list (list peer-wire nil))))
           (request (make-http-request :method "GET" :uri "https://example.test/"))
           (request-before (http3-qpack-contract-request-source request))
           (opened nil)
           (failure nil)
           (response nil))
      (push peer (http3-qpack-contract-fixture-streams fixture))
      (unwind-protect
           (progn
             (ensure-true (eq client (http-kit/http3:attach-http3-peer-control-stream client peer)))
             (multiple-value-bind (effects fin-p) (http-kit/http3:read-http3-control-stream client)
               (ensure-equal '(:settings :goaway) effects)
               (ensure-false fin-p))
             (let ((events (reverse (http3-qpack-contract-fixture-events fixture))))
               (ensure-equal '(:read) (mapcar #'first events))
               (ensure-true (eq peer (second (first events)))))
             (ensure-equal nil (http3-test-stream-reads peer))
             (ensure-false (http3-test-stream-closed-p peer))
             (setf (http3-qpack-contract-fixture-events fixture) nil)
             (handler-case
                 (setf response
                       (http-kit/http3:send-http3-request
                        client request
                        :on-stream-open
                        (lambda (stream stream-id cancel)
                          (ensure-true (functionp cancel))
                          (push (list stream stream-id) opened))))
               (http-protocol-error (condition) (setf failure condition)))
             (let* ((events (reverse (http3-qpack-contract-fixture-events fixture)))
                    (stream (second (first events)))
                    (close-event (car (last events))))
               (ensure-equal :request (http3-test-stream-kind stream))
               (ensure-true (eq request (third (first events))))
               (ensure-true (http3-test-stream-closed-p stream))
               (ensure-equal 1 (count :open events :key #'first))
               (ensure-equal 1 (count :close events :key #'first))
               (ensure-true (eq stream (second close-event)))
               (ensure-true (eq failure (third close-event)))
               (dolist (event events) (ensure-true (eq stream (second event))))
               (if (eql id 0)
                   (progn
                     (ensure-true (eq nil failure))
                     (ensure-equal 204 (http-response-status response))
                     (ensure-equal '(:open :write :write :read :close) (mapcar #'first events))
                     (ensure-equal 1 (length opened))
                     (ensure-true (eq stream (first (first opened))))
                     (ensure-equal 0 (second (first opened)))
                     (ensure-equal 2 (length (http3-test-stream-writes stream)))
                     (ensure-equal nil (http3-test-stream-reads stream)))
                   (progn
                     (http3-public-error-check failure
                                              "The peer GOAWAY excludes this HTTP/3 request stream."
                                              :h3-request-rejected)
                     (ensure-true (eq nil response))
                     (ensure-equal '(:open :close) (mapcar #'first events))
                     (ensure-equal 0 (length opened))
                     (ensure-equal 0 (count :write events :key #'first))
                     (ensure-equal 0 (count :read events :key #'first))
                     (ensure-equal nil (http3-test-stream-writes stream))
                     (ensure-equal 1 (length (http3-test-stream-reads stream)))
                     (ensure-true (eq response-wire (first (first (http3-test-stream-reads stream))))))))
             (ensure-true (http-kit/http3:http3-client-open-p client))
             (ensure-equal request-before (http3-qpack-contract-request-source request))
             (ensure-octets-equal peer-before peer-wire)
             (ensure-octets-equal response-before response-wire))
        (http3-qpack-contract-cleanup fixture)
        (ensure-equal '(:control :qpack-encoder :qpack-decoder :peer-control)
                      (mapcar (lambda (event) (http3-test-stream-kind (second event)))
                              (reverse (http3-qpack-contract-fixture-events fixture)))))
      (ensure-equal request-before (http3-qpack-contract-request-source request))
      (ensure-octets-equal peer-before peer-wire)
      (ensure-octets-equal response-before response-wire))))

(deftest http3-helper-header-name-revalidates-caller-owned-alias
  (let* ((name (copy-seq "X-Good"))
         (content (copy-seq "kept"))
         (header (make-http-header name content)))
    (ensure-true (eq name (http-header-name header)))
    (ensure-equal "x-good" (http-kit/http3::%h3-header-name header))
    (ensure-equal "X-Good" name)
    (ensure-equal "kept" content)
    (setf (char name 1) #\Space)
    (let* ((before (copy-seq name))
           (caught (handler-case (http-kit/http3::%h3-header-name header)
                     (http-invalid-header (condition) condition))))
      (ensure-true (typep caught 'http-invalid-header))
      (ensure-equal :http3-header (http-error-operation caught))
      (ensure-equal "x good" (http-invalid-header-name caught))
      (ensure-equal "the field name is not a token."
                    (http-invalid-header-reason caught))
      (ensure-true (eq name (http-header-name header)))
      (ensure-equal before name)
      (ensure-equal "kept" content)
      (ensure-equal "kept" (http-header-content header)))))

(deftest http3-helper-no-body-response-method-and-status-boundaries
  (dolist (case '((nil 200 nil) (nil 204 t) (nil 205 t) (nil 304 t)
                 ("HEAD" 200 t) ("HEAD" 204 t)
                 ("GET" 200 nil) ("GET" 204 t) ("GET" 205 t) ("GET" 304 t)
                 ("head" 200 nil)))
    (destructuring-bind (method status expected) case
      (let* ((input (and method (copy-seq method)))
             (before (and input (copy-seq input))))
        (ensure-equal expected
                      (not (null (http-kit/http3::%h3-no-body-response-p input status))))
        (when input
          (ensure-equal before input))))))
