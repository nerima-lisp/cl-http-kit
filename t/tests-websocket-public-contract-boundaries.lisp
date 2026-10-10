(in-package #:http-kit/test)

(deftest websocket-public-contract-payload-length-width
  (dolist (case '((0 0) (125 0) (126 2) (65535 2) (65536 8)
                  (#x7fffffffffffffff 8)))
    (ensure-equal (second case)
                  (http-kit/websocket::%websocket-payload-length-width (first case))))
  (dolist (length '(#x8000000000000000 #xffffffffffffffff))
    (let ((caught
            (handler-case
                (progn (http-kit/websocket::%websocket-payload-length-width length) nil)
              (http-protocol-error (condition) condition))))
      (ensure-true (typep caught 'http-protocol-error))
      (ensure-equal :websocket (http-error-operation caught))
      (ensure-equal length (http-protocol-error-detail caught)))))

(deftest websocket-public-contract-payload-length-wire-boundaries
  (dolist (case '((125 125) (126 126 0 126)
                  (65535 126 #xff #xff) (65536 127 0 0 0 0 0 1 0 0)))
    (dolist (masked '(nil t))
      (let* ((size (first case))
             (payload (make-array size :element-type '(unsigned-byte 8)))
             (key (and masked (octets #x13 #x57 #x9b #xdf)))
             (header (apply #'octets #x82
                            (logior (second case) (if masked #x80 0))
                            (cddr case)))
             (header-size (+ (length header) (if masked 4 0))))
        (dotimes (index size)
          (setf (aref payload index) (mod (* 37 index) 256)))
        (let ((wire (serialize-websocket-frame
                     (make-websocket-frame :opcode 2 :payload payload
                                           :mask-p masked :masking-key key))))
          (ensure-equal header (subseq wire 0 (length header)))
          (ensure-equal (+ header-size size) (length wire))
          (when masked
            (ensure-equal key (subseq wire (length header) header-size)))
          (let ((expected (copy-seq payload)))
            (when masked
              (dotimes (index size)
                (setf (aref expected index)
                      (logxor (aref payload index) (aref key (mod index 4))))))
            (ensure-equal expected (subseq wire header-size)))
          (multiple-value-bind (frame consumed)
              (parse-websocket-frame (concatenate-octets wire (octets #xff))
                                     :require-mask-p masked)
            (ensure-equal (length wire) consumed)
            (ensure-equal payload (websocket-frame-payload frame))
            (ensure-equal masked (websocket-frame-mask-p frame))))))))

(deftest websocket-public-contract-random-result-validation
  (dolist (invalid '(256 -1 :not-an-octet))
    (let ((result (map 'vector #'char-code "the sample nonce"))
          (calls 0))
      (let ((caught
              (handler-case
                  (progn
                    (make-websocket-client-key
                     (lambda (size)
                       (incf calls)
                       (ensure-equal 16 size)
                       (setf (aref result 15) invalid)
                       result))
                    nil)
                (http-protocol-error (condition) condition))))
        (ensure-true (typep caught 'http-protocol-error))
        (ensure-equal :websocket (http-error-operation caught))
        (ensure-true (eq result (http-protocol-error-detail caught)))
        (ensure-equal 1 calls))))
  (let ((result (map 'vector #'char-code "the sample nonce"))
        (calls 0))
    (let ((key (make-websocket-client-key
                (lambda (size)
                  (incf calls)
                  (ensure-equal 16 size)
                  result))))
      (ensure-equal "dGhlIHNhbXBsZSBub25jZQ==" key)
      (setf (aref result 0) 0)
      (ensure-equal "dGhlIHNhbXBsZSBub25jZQ==" key)
      (ensure-equal "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" (websocket-accept-key key))
      (ensure-equal 1 calls))))

(deftest websocket-public-contract-decompress-result-validation
  (dolist (case (list (list nil :websocket)
                     (list "not octets" :websocket)
                     (list (vector 256) :websocket)
                     (list (vector #xed #xa0 #x80) :websocket-utf8)))
    (destructuring-bind (result operation) case
      (let* ((wire (serialize-websocket-frame
                    (make-websocket-frame :opcode 1 :rsv1-p t
                                          :payload (octets #x21 #x22))))
             (stream (make-instance 'binary-test-stream
                                    :input (concatenate-octets wire (ascii "!"))))
             (calls 0))
        (unwind-protect
             (progn
               (let ((caught
                       (handler-case
                           (progn
                             (read-websocket-message
                              stream :decompress-function
                              (lambda (payload)
                                (incf calls)
                                (ensure-equal (octets #x21 #x22) payload)
                                result))
                             nil)
                         (http-protocol-error (condition) condition))))
                 (ensure-true (typep caught 'http-protocol-error))
                 (ensure-equal operation (http-error-operation caught))
                 (ensure-equal result (http-protocol-error-detail caught)))
               (ensure-equal 1 calls)
               (ensure-equal (length wire)
                             (http-kit/test-core::binary-test-input-position stream))
               (ensure-equal (char-code #\!) (read-byte stream))
               (ensure-equal 0 (length (binary-test-output stream))))
          (close stream))
        (ensure-false (open-stream-p stream)))))
  (let* ((result (vector #xc2 #xa2))
         (wire (serialize-websocket-frame
                (make-websocket-frame :opcode 1 :rsv1-p t :payload (octets #x21 #x22))))
         (stream (make-instance 'binary-test-stream
                                :input (concatenate-octets wire (ascii "!"))))
         (calls 0)
         (callback-input nil))
    (unwind-protect
         (progn
           (multiple-value-bind (payload opcode)
               (read-websocket-message
                stream :decompress-function
                (lambda (input)
                  (incf calls)
                  (setf callback-input input)
                  (ensure-equal (octets #x21 #x22) input)
                  result))
             (ensure-equal 1 opcode)
             (ensure-equal (octets #xc2 #xa2) payload)
             (ensure-false (eq result payload))
             (setf (aref result 0) 0
                   (aref callback-input 0) 0)
             (ensure-equal (octets #xc2 #xa2) payload))
           (ensure-equal 1 calls)
           (ensure-equal (length wire)
                         (http-kit/test-core::binary-test-input-position stream))
           (ensure-equal (char-code #\!) (read-byte stream))
           (ensure-equal 0 (length (binary-test-output stream))))
      (close stream))
    (ensure-false (open-stream-p stream))))

(deftest websocket-public-contract-reserved-opcode-wire-rejection
  (dolist (opcode '(3 4 5 6 7 11 12 13 14 15))
    (let ((stream (make-instance 'binary-test-stream
                                 :input (octets (logior #x80 opcode) 1 #x61 #x21)))
          (controls 0)
          (decompressions 0))
      (unwind-protect
           (progn
             (let ((caught
                     (handler-case
                         (progn
                           (read-websocket-message
                            stream
                            :on-control (lambda (frame)
                                          (declare (ignore frame))
                                          (incf controls))
                            :decompress-function (lambda (payload)
                                                   (incf decompressions)
                                                   payload))
                           nil)
                       (http-protocol-error (condition) condition))))
               (ensure-true (typep caught 'http-protocol-error))
               (ensure-equal :websocket (http-error-operation caught))
               (ensure-equal opcode (http-protocol-error-detail caught)))
             (ensure-equal 0 controls)
             (ensure-equal 0 decompressions)
             (ensure-equal 2 (http-kit/test-core::binary-test-input-position stream))
             (ensure-equal #x61 (read-byte stream))
             (ensure-equal #x21 (read-byte stream))
             (ensure-equal 0 (length (binary-test-output stream))))
        (close stream))
      (ensure-false (open-stream-p stream)))))

(deftest websocket-public-contract-legal-opcode-sequence
  (flet ((wire (opcode payload &optional (fin-p t))
           (serialize-websocket-frame
            (make-websocket-frame :opcode opcode :payload payload :fin-p fin-p))))
    (let* ((first-message (concatenate-octets
                           (wire 1 (octets #xc2) nil)
                           (wire 8 (make-websocket-close-payload :code 1000))
                           (wire 9 (ascii "p"))
                           (wire 10 (ascii "q"))
                           (wire 0 (octets #xa2))))
           (second-message (wire 2 (octets 0 #xff)))
           (stream (make-instance 'binary-test-stream
                                  :input (concatenate-octets first-message second-message
                                                             (ascii "!"))))
           (controls nil))
      (unwind-protect
           (progn
             (multiple-value-bind (payload opcode)
                 (read-websocket-message
                  stream :on-control
                  (lambda (frame)
                    (push (list (websocket-frame-opcode frame)
                                (copy-seq (websocket-frame-payload frame)))
                          controls)
                    (when (= 8 (websocket-frame-opcode frame))
                      (multiple-value-bind (code reason)
                          (parse-websocket-close-payload (websocket-frame-payload frame))
                        (ensure-equal 1000 code)
                        (ensure-equal "" reason)))
                    (setf (aref (websocket-frame-payload frame) 0) 0)))
               (ensure-equal 1 opcode)
               (ensure-equal (octets #xc2 #xa2) payload))
             (ensure-equal (list (list 8 (octets 3 #xe8))
                                 (list 9 (ascii "p")) (list 10 (ascii "q")))
                           (reverse controls))
             (ensure-equal (length first-message)
                           (http-kit/test-core::binary-test-input-position stream))
             (multiple-value-bind (payload opcode) (read-websocket-message stream)
               (ensure-equal 2 opcode)
               (ensure-equal (octets 0 #xff) payload))
             (ensure-equal (+ (length first-message) (length second-message))
                           (http-kit/test-core::binary-test-input-position stream))
             (ensure-equal (char-code #\!) (read-byte stream))
             (ensure-equal 0 (length (binary-test-output stream))))
        (close stream))
      (ensure-false (open-stream-p stream)))))

(deftest websocket-public-contract-strict-header-copy-and-rejection
  (dolist (name '("Upgrade" "Connection" "Sec-WebSocket-Protocol"))
    (let* ((request (make-websocket-upgrade-request
                     "http://example.test/chat" :key "dGhlIHNhbXBsZSBub25jZQ=="
                     :protocols '("chat")))
           (header (find name (http-request-headers request)
                         :key #'http-header-name :test #'string-equal))
           (original (http-header-content header)))
      (ensure-true (websocket-upgrade-request-p request))
      (unwind-protect
           (progn
             (setf (http-header-content header) 7)
             (ensure-equal 7 (http-header-content header))
             (ensure-equal original (http-header-value (http-request-headers request) name))
             (ensure-true (websocket-upgrade-request-p request))
             (let ((response (websocket-upgrade-response request :protocol "chat")))
               (ensure-equal 101 (http-response-status response))
               (ensure-equal "chat" (http-header-value (http-response-headers response)
                                                      "Sec-WebSocket-Protocol"))))
        (setf (http-header-content header) original))
      (ensure-true (eq original (http-header-content header)))
      (ensure-true (websocket-upgrade-request-p request))
      (ensure-equal 101 (http-response-status
                         (websocket-upgrade-response request :protocol "chat")))
      (let ((headers (http-request-headers request)))
        (setf (http-header-content (find name headers :key #'http-header-name
                                                    :test #'string-equal)) "")
        (let* ((invalid (make-http-request :method "GET" :uri "http://example.test/chat"
                                           :headers headers))
               (caught (handler-case (websocket-upgrade-response invalid)
                         (http-protocol-error (condition) condition))))
          (ensure-false (websocket-upgrade-request-p invalid))
          (ensure-true (typep caught 'http-protocol-error))
          (ensure-equal :websocket (http-error-operation caught))
          (ensure-equal nil (http-protocol-error-detail caught))
          (ensure-true (websocket-upgrade-request-p request)))))))

(deftest websocket-public-contract-strict-window-bits-validation
  (let ((request (make-websocket-upgrade-request
                  "http://example.test/chat" :key "dGhlIHNhbXBsZSBub25jZQ=="
                  :extensions "permessage-deflate; server_max_window_bits=15; client_max_window_bits")))
    (dolist (name '("server_max_window_bits" "client_max_window_bits"))
      (dolist (entry '(("08" "08") ("x" "x") ("7" 7) ("16" 16) ("\"\"" "") (nil nil)))
        (let* ((value (first entry))
               (selection (if value (format nil "permessage-deflate; ~A=~A" name value)
                              (format nil "permessage-deflate; ~A" name)))
               (caught (handler-case (websocket-upgrade-response request :extensions selection)
                         (http-protocol-error (condition) condition))))
          (ensure-true (typep caught 'http-protocol-error))
          (ensure-equal :websocket (http-error-operation caught))
          (ensure-equal (if value (second entry) name) (http-protocol-error-detail caught))
          (ensure-true (websocket-upgrade-request-p request))))
      (dolist (bits '(8 15))
        (let* ((selection (format nil "permessage-deflate; ~A=~D" name bits))
               (response (websocket-upgrade-response request :extensions selection)))
          (ensure-equal 101 (http-response-status response))
          (ensure-equal selection (http-header-value (http-response-headers response)
                                                     "Sec-WebSocket-Extensions"))
          (ensure-equal "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
                        (http-header-value (http-response-headers response)
                                           "Sec-WebSocket-Accept")))))))

(deftest websocket-public-header-type-prevalidation-contract
  (dolist (name '("Upgrade" "Connection" "Sec-WebSocket-Protocol"))
    (let* ((request (make-websocket-upgrade-request
                     "http://example.test/chat" :key "dGhlIHNhbXBsZSBub25jZQ=="
                     :protocols '("chat")))
           (headers (http-request-headers request))
           (header (find name headers :key #'http-header-name :test #'string-equal))
           (original (http-header-content header)))
      (setf (http-header-content header) 7)
      (dolist (constructor
                (list (lambda () (make-http-header name 7))
                      (lambda () (make-http-request :method "GET" :uri "http://example.test/chat"
                                                    :headers headers))
                      (lambda () (make-http-response :status 200 :headers headers))))
        (let ((caught (handler-case (funcall constructor)
                        (http-invalid-header (condition) condition))))
          (ensure-true (typep caught 'http-invalid-header))
          (ensure-equal :header (http-error-operation caught))
          (ensure-equal name (http-invalid-header-name caught))
          (ensure-equal :value-type (http-invalid-header-reason caught))
          (ensure-equal 7 (http-header-content header))
          (ensure-equal original (http-header-value (http-request-headers request) name))
          (ensure-true (websocket-upgrade-request-p request))))
      (let ((response (websocket-upgrade-response request :protocol "chat")))
        (ensure-equal 101 (http-response-status response))
        (ensure-equal "chat" (http-header-value (http-response-headers response)
                                               "Sec-WebSocket-Protocol"))))))

(deftest uri-public-alt-svc-quoted-value-contracts
  (let* ((origin (parse-http-uri "https://example.test/"))
         (other-origin (parse-http-uri "https://other.example.test/"))
         (clock (lambda () 100))
         (seed (make-http-response :status 200
                  :headers (list (make-http-header "Alt-Svc" "h3=\":443\"; ma=30")))))
    (dolist (case
              (list (list "h3=\"\"" nil)
                    (list "h3=\":443\"; ma=\"\"" nil)
                    (list "h3=\":443\"; persist=\"\"" t)
                    (list "h3=\":443\"; x=\"\"" t)
                    (list "h3=\"\\:443\"; ma=\"30\"" t)
                    (list "h3=\":443\"; x=\"a\\\"b\\\\\"" t)
                    (list (concatenate 'string "h3=\"" "\\" "\"") nil)
                    (list (concatenate 'string "h3=\":443\"; x=\"" "\\" "\"") nil)))
      (let* ((wire (first case))
             (before (copy-seq wire))
             (store (make-http-alternative-service-store :clock-function clock))
             (response (make-http-response :status 204
                         :headers (list (make-http-header "Alt-Svc" wire)))))
        (http-alternative-service-store-note-response store origin seed)
        (http-alternative-service-store-note-response store other-origin seed)
        (let ((other (first (http-alternative-service-store-services store other-origin))))
          (ensure-true (http-alternative-service-p other))
          (ensure-true (eq store
                           (http-alternative-service-store-note-response store origin response)))
          (let ((services (http-alternative-service-store-services store origin)))
            (if (second case)
                (progn
                  (ensure-equal 1 (length services))
                  (ensure-equal "h3" (http-alternative-service-protocol-id (first services)))
                  (ensure-equal "example.test" (http-alternative-service-host (first services)))
                  (ensure-equal 443 (http-alternative-service-port (first services)))
                  (ensure-false (http-alternative-service-persist-p (first services)))
                  (ensure-equal (if (search "ma=\"30\"" wire) 130 86500)
                                (http-alternative-service-expires-at (first services))))
                (ensure-false services)))
          (let ((others (http-alternative-service-store-services store other-origin)))
            (ensure-equal 1 (length others))
            (ensure-true (eq other (first others))))
          (ensure-equal before wire)
          (ensure-equal (list before)
                        (http-header-values (http-response-headers response) "Alt-Svc"))
          (ensure-equal "https://example.test/" (http-uri-string origin))
          (ensure-equal "https://other.example.test/" (http-uri-string other-origin)))))))
