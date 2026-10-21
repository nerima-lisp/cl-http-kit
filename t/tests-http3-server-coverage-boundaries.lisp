(in-package #:http-kit/test)

(defun http3-server-coverage-serve
    (handler &key (method "GET") (max-header-bytes 65536)
             on-qpack-section-acknowledgment qpack-decoder-context
             qpack-http3-decoder-context stream-id)
  (let* ((wire (http3-boundary-headers
                (list (cons ":method" method) '(":scheme" . "https")
                      '(":authority" . "example.test") '(":path" . "/"))))
         (stream (make-http3-test-stream :kind :request))
         (reads 0) (calls 0) (writes nil) (closed nil) (errors nil)
         (request nil) (response nil)
         (failure
           (http3-boundary-error
            (lambda ()
              (setf response
                    (http-kit/http3:serve-http3-request-stream
                     stream
                     (lambda (parsed)
                       (incf calls)
                       (setf request parsed)
                       (funcall handler parsed))
                     :max-header-bytes max-header-bytes
                     :qpack-decoder-context qpack-decoder-context
                     :qpack-http3-decoder-context qpack-http3-decoder-context
                     :stream-id stream-id
                     :on-qpack-section-acknowledgment
                     on-qpack-section-acknowledgment
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
                     :on-error
                     (lambda (condition parsed)
                       (push (list condition parsed) errors))))))))
    (ensure-equal 1 reads)
    (ensure-equal 1 calls)
    (ensure-equal 1 (length closed))
    (ensure-true (eq nil (first closed)))
    (if failure (ensure-equal 1 (length errors)) (ensure-true (null errors)))
    (when failure (ensure-true (eq failure (first (first errors)))))
    (when failure (ensure-true (eq request (second (first errors)))))
    (values failure response (nreverse writes) (nreverse errors) request)))

(defun http3-server-coverage-frames (writes)
  (multiple-value-bind (frames remainder)
      (http-kit/http3:decode-http3-frames
       (apply #'http3-test-concat-octets (mapcar #'first writes)))
    (ensure-equal (octets) remainder)
    frames))

(deftest http3-server-coverage-handler-return-contract
  (dolist (value (list nil 17
                      (make-http-response :status 100)
                      (make-http-response :status 199)
                      (make-http-response :status 200)
                      (make-http-response :status 599)))
    (multiple-value-bind (failure response writes)
        (http3-server-coverage-serve
         (lambda (request) (declare (ignore request)) value))
      (cond
        ((not (http-response-p value))
         (http3-public-error-check
          failure "An HTTP/3 handler must return an HTTP response or response stream."
          (type-of value))
         (ensure-equal nil writes))
        ((< (http-response-status value) 200)
         (http3-public-error-check
          failure "HTTP/3 handlers must return a final status from 200 through 599."
          (http-response-status value))
         (ensure-equal nil writes))
        (t
         (ensure-equal nil failure)
         (ensure-true (eq value response))
         (ensure-equal '(t) (mapcar #'second writes))
         (let ((frames (http3-server-coverage-frames writes)))
           (ensure-equal '(1) (mapcar #'http-kit/http3:http3-frame-type frames))
           (ensure-equal
            (write-to-string (http-response-status value))
            (http-message-kit:http-header-content
             (find ":status"
                   (http-message-kit:qpack-decode-header-block
                    (http-kit/http3:http3-frame-payload (first frames)))
                   :key (function http-message-kit:http-header-name)
                   :test (function string=))))))))))

(deftest http3-server-coverage-informational-return-contract
  (let ((final (make-http-response :status 204)))
    (dolist (information (list 17
                              (make-http-response :status 101)
                              (make-http-response :status 200)
                              (make-http-response :status 100)
                              (make-http-response :status 199)))
      (multiple-value-bind (failure response writes)
          (http3-server-coverage-serve
           (lambda (request)
             (declare (ignore request))
             (values final (list information))))
        (cond
          ((not (http-response-p information))
           (http3-public-error-check
            failure "HTTP/3 informational responses must be HTTP responses or response streams."
            (type-of information))
           (ensure-equal nil writes))
          ((member (http-response-status information) '(101 200))
           (http3-public-error-check
            failure "An HTTP/3 informational response must have status 100-199 other than 101."
            (http-response-status information))
           (ensure-equal nil writes))
          (t
           (ensure-equal nil failure)
           (ensure-true (eq final response))
           (ensure-equal '(nil t) (mapcar #'second writes))
           (let ((frames (http3-server-coverage-frames writes)))
             (ensure-equal '(1 1) (mapcar #'http-kit/http3:http3-frame-type frames))
             (ensure-equal
              (list (write-to-string (http-response-status information)) "204")
              (mapcar
               (lambda (frame)
                 (http-message-kit:http-header-content
                  (find ":status"
                        (http-message-kit:qpack-decode-header-block
                         (http-kit/http3:http3-frame-payload frame))
                        :key (function http-message-kit:http-header-name)
                        :test (function string=))))
               frames)))))))))

(deftest http3-server-coverage-encoded-header-limits-before-body
  (let ((value (make-string 400 :initial-element #\a)))
    (dolist (phase '(:information :final :trailer))
      (let* ((fields (if (eq phase :trailer)
                         (list (cons "x-boundary" value))
                         (list (cons ":status" (if (eq phase :information) "103" "200"))
                               (cons "x-boundary" value))))
             (encoded (http-message-kit:qpack-prepare-header-block fields))
             (size (length encoded)))
        (ensure-true (> size 256))
        (dolist (limit (list (1- size) size))
          (let* ((pulls 0)
                 (final
                   (if (eq phase :information)
                       (make-http-response :status 204)
                       (make-http-response-stream
                        :status 200 :body-length 2
                        :headers (when (eq phase :final) (list (cons "x-boundary" value)))
                        :trailers (when (eq phase :trailer) (list (cons "x-boundary" value)))
                        :body-function
                        (lambda () (incf pulls) (when (= pulls 1) (ascii "ok"))))))
                 (information (when (eq phase :information)
                                (make-http-response :status 103
                                                    :headers (list (cons "x-boundary" value))))))
            (multiple-value-bind (failure response writes)
                (http3-server-coverage-serve
                 (lambda (request)
                   (declare (ignore request))
                   (values final (when information (list information))))
                 :max-header-bytes limit)
              (if (< limit size)
                  (progn
                    (ensure-true (typep failure 'http-size-limit-exceeded))
                    (ensure-equal :http3-transport (http-error-operation failure))
                    (ensure-equal :headers (http-size-limit-exceeded-kind failure))
                    (ensure-equal limit (http-size-limit-exceeded-limit failure))
                    (ensure-equal size (http-size-limit-exceeded-observed failure))
                    (ensure-equal nil writes)
                    (ensure-equal 0 pulls))
                  (let ((frames (http3-server-coverage-frames writes)))
                    (ensure-equal nil failure)
                    (ensure-true (eq final response))
                    (ensure-equal (if information 0 2) pulls)
                    (ensure-equal (case phase (:information '(nil t))
                                              (:final '(nil t))
                                              (:trailer '(nil nil t)))
                                  (mapcar #'second writes))
                    (ensure-equal (case phase (:information '(1 1))
                                              (:final '(1 0))
                                              (:trailer '(1 0 1)))
                                  (mapcar #'http-kit/http3:http3-frame-type frames))
                    (let ((block (http-kit/http3:http3-frame-payload
                                  (if (eq phase :trailer) (third frames) (first frames)))))
                      (ensure-equal encoded block)
                      (ensure-equal value
                                    (http-message-kit:http-header-content
                                     (find "x-boundary"
                                           (http-message-kit:qpack-decode-header-block block)
                                           :key (function http-message-kit:http-header-name)
                                           :test (function string=)))))
                    (unless information
                      (ensure-equal (ascii "ok")
                                    (http-kit/http3:http3-frame-payload (second frames)))))))))))))

(deftest http3-server-coverage-suppressed-response-bodies
  (dolist (row '(("HEAD" 200 3 "3" nil)
                 ("GET" 200 3 "3" nil)
                 ("GET" 204 0 nil nil)
                 ("GET" 205 0 "0" nil)
                 ("GET" 204 3 nil t)
                 ("GET" 205 3 nil t)))
    (destructuring-bind (method status length content-length reject-p) row
      (dolist (stream-p '(nil t))
        (let* ((pulls 0)
               (headers (when content-length (list (cons "content-length" content-length))))
               (final
                 (if stream-p
                     (make-http-response-stream
                      :status status :headers headers :body-length length
                      :body-function
                      (lambda () (incf pulls) (when (= pulls 1) (ascii "abc"))))
                     (make-http-response :status status :headers headers
                                         :body (if (plusp length) (ascii "abc") (octets))))))
          (multiple-value-bind (failure response writes)
              (http3-server-coverage-serve
               (lambda (request) (declare (ignore request)) final) :method method)
            (if reject-p
                (progn
                  (http3-public-error-check failure "This HTTP/3 response cannot contain a body." nil)
                  (ensure-equal nil writes)
                  (ensure-equal 0 pulls))
                (let* ((body-p (and (string= method "GET") (= status 200)))
                       (frames (http3-server-coverage-frames writes))
                       (fields (http-message-kit:qpack-decode-header-block
                                (http-kit/http3:http3-frame-payload (first frames)))))
                  (ensure-equal nil failure)
                  (ensure-true (eq final response))
                  (ensure-equal (if (and stream-p body-p) 2 0) pulls)
                  (ensure-equal (if body-p '(nil t) '(t)) (mapcar #'second writes))
                  (ensure-equal (if body-p '(1 0) '(1))
                                (mapcar #'http-kit/http3:http3-frame-type frames))
                  (ensure-equal
                   (write-to-string status)
                   (http-message-kit:http-header-content
                    (find ":status" fields
                          :key (function http-message-kit:http-header-name)
                          :test (function string=))))
                  (ensure-equal content-length (http-message-kit:http-header-content
                                  (find "content-length" fields
                                        :key (function http-message-kit:http-header-name)
                                        :test (function string=))))
                  (when body-p
                    (ensure-equal (ascii "abc")
                                  (http-kit/http3:http3-frame-payload (second frames))))))))))))

(defun http3-server-push-ready-state ()
  (let ((state (http-kit/http3:make-http3-control-state :peer-role :client)))
    (ensure-equal :settings
                  (http-kit/http3:process-http3-control-frame
                   state (http-kit/http3:make-http3-settings-frame)))
    (ensure-equal :max-push-id
                  (http-kit/http3:process-http3-control-frame
                   state (http-kit/http3:make-http3-frame
                          :type http-kit/http3:+http3-max-push-id-type+
                          :payload (http-kit/http3:http3-varint-encode 3))))
    state))

(defun http3-server-push-check-rejection (request state options message detail serialized)
  (let ((counts (vector 0 0 0 0 0)))
    (let ((failure
            (handler-case
                (apply #'http-kit/http3:send-http3-push
                       :request-stream request (make-http-response :status 204) state
                       (append options
                               (list :open-stream (lambda (kind)
                                                    (declare (ignore kind))
                                                    (incf (aref counts 0)) :push-stream)
                                     :write-stream (lambda (stream bytes &key fin-p timeout deadline)
                                                     (declare (ignore stream bytes fin-p timeout deadline))
                                                     (incf (aref counts 1)))
                                     :close-stream (lambda (stream &key condition)
                                                     (declare (ignore stream condition))
                                                     (incf (aref counts 2)))
                                     :cancel-stream (lambda (stream code)
                                                      (declare (ignore stream code))
                                                      (incf (aref counts 3)))
                                     :serialize (lambda (thunk)
                                                  (incf (aref counts 4)) (funcall thunk)))))
              (http-protocol-error (condition) condition))))
      (http3-public-error-check failure message detail)
      (ensure-equal (vector 0 0 0 0 serialized) counts)
      (when (http-kit/http3:http3-control-state-p state)
        (ensure-equal nil (http-kit/http3:http3-control-state-promised-push-ids state))
        (ensure-equal nil (http-kit/http3:http3-control-state-cancelled-push-ids state))))))

(deftest http3-server-push-preflight-rejects-before-callbacks
  (let ((request (make-http-request :method "GET" :uri "https://example.test/asset.css"))
        (state (http3-server-push-ready-state)))
    (http3-server-push-check-rejection
     :not-a-request state nil "SEND-HTTP3-PUSH requires an HTTP request." (type-of :not-a-request) 0)
    (http3-server-push-check-rejection
     request :not-a-state nil "SEND-HTTP3-PUSH requires HTTP/3 control state." (type-of :not-a-state) 0)
    (http3-server-push-check-rejection
     request (http-kit/http3:make-http3-control-state :peer-role :server) nil
     "HTTP/3 server push requires control state for a client peer." nil 0)
    (dolist (keyword '(:open-stream :write-stream :serialize))
      (http3-server-push-check-rejection
       request state (list keyword :not-a-function)
       "HTTP/3 server push requires open-stream, write-stream, and serialize callbacks."
       (type-of :not-a-function) 0))
    (dolist (keyword '(:close-stream :cancel-stream))
      (http3-server-push-check-rejection
       request state (list keyword :not-a-function)
       "HTTP/3 server push close-stream and cancel-stream must be functions or NIL."
       (type-of :not-a-function) 0))
    (dolist (value '(0 -1 1/2 "1" nil))
      (http3-server-push-check-rejection
       request state (list :max-frame-size value)
       "max-frame-size must be a positive integer." value 0))
    (dolist (value '(-1 1/2 "1" nil))
      (http3-server-push-check-rejection
       request state (list :max-header-bytes value)
       "max-header-bytes must be a non-negative integer." value 0))
    (dolist (method '("POST" "PUT" "DELETE" "OPTIONS"))
      (http3-server-push-check-rejection
       (make-http-request :method method :uri "https://example.test/asset.css") state nil
       "An HTTP/3 pushed request must be safe and cacheable." method 0))
    (dolist (with-trailers '(nil t))
      (http3-server-push-check-rejection
       (make-http-request :method "GET" :uri "https://example.test/asset.css"
                          :body (if with-trailers (octets) (ascii "x"))
                          :trailers (when with-trailers (list (cons "x-end" "yes"))))
       state nil "An HTTP/3 pushed request cannot contain content or trailers." nil 0))
    (dolist (value '(-1 1/2 "1" 4611686018427387904))
      (http3-server-push-check-rejection
       request state (list :push-id value) "The HTTP/3 push ID is invalid." value 1))
    (ensure-equal 3 (http-kit/http3:http3-control-state-max-push-id state))))

(deftest http3-server-push-promise-size-limits-before-write
  (let* ((request (make-http-request :method "GET" :uri "https://example.test/asset.css"))
         (field-section (http-message-kit:qpack-prepare-header-block
                         '((":method" . "GET") (":scheme" . "https")
                           (":authority" . "example.test") (":path" . "/asset.css"))))
         (field-size (length field-section))
         (payload-size (+ (length (http-kit/http3:http3-varint-encode 0)) field-size)))
    (ensure-true (plusp field-size))
    (dolist (kind '(:headers :frame))
      (let ((state (http3-server-push-ready-state))
            (counts (vector 0 0 0 0 0))
            (limit (if (eq kind :headers) (1- field-size) (1- payload-size))))
        (let ((failure
                (handler-case
                    (http-kit/http3:send-http3-push
                     :request-stream request (make-http-response :status 204) state
                     :push-id 0
                     :max-header-bytes (if (eq kind :headers) limit field-size)
                     :max-frame-size (if (eq kind :frame) limit payload-size)
                     :open-stream (lambda (stream-kind)
                                    (declare (ignore stream-kind))
                                    (incf (aref counts 0)) :push-stream)
                     :write-stream (lambda (stream bytes &key fin-p timeout deadline)
                                     (declare (ignore stream bytes fin-p timeout deadline))
                                     (incf (aref counts 1)))
                     :close-stream (lambda (stream &key condition)
                                     (declare (ignore stream condition))
                                     (incf (aref counts 2)))
                     :cancel-stream (lambda (stream code)
                                      (declare (ignore stream code))
                                      (incf (aref counts 3)))
                     :serialize (lambda (thunk) (incf (aref counts 4)) (funcall thunk)))
                  (http-size-limit-exceeded (condition) condition))))
          (ensure-true (typep failure 'http-size-limit-exceeded))
          (ensure-equal :http3-transport (http-error-operation failure))
          (ensure-equal kind (http-size-limit-exceeded-kind failure))
          (ensure-equal limit (http-size-limit-exceeded-limit failure))
          (ensure-equal (if (eq kind :headers) field-size payload-size)
                        (http-size-limit-exceeded-observed failure))
          (ensure-equal #(0 0 0 0 1) counts)
          (ensure-equal nil (http-kit/http3:http3-control-state-promised-push-ids state))
          (ensure-equal 3 (http-kit/http3:http3-control-state-max-push-id state)))))))

(deftest http3-reachable-server-response-header-preflight-contract
  (dolist (row '(("connection" "close" "connection-specific fields are forbidden.")
                ("keep-alive" "timeout=5" "connection-specific fields are forbidden.")
                ("proxy-connection" "close" "connection-specific fields are forbidden.")
                ("transfer-encoding" "chunked" "connection-specific fields are forbidden.")
                ("upgrade" "websocket" "connection-specific fields are forbidden.")
                ("te" "trailers" "HTTP/3 TE is permitted only in request header sections.")))
    (destructuring-bind (name value reason) row
      (let* ((pulls 0)
             (reply (make-http-response-stream
                     :status 200 :headers (list (cons name value)) :body-length 1
                     :body-function (lambda () (incf pulls) (ascii "a")))))
        (multiple-value-bind (failure response writes)
            (http3-server-coverage-serve
             (lambda (request) (declare (ignore request)) reply))
          (http3-reachable-check-header-error failure name reason)
          (ensure-equal nil response)
          (ensure-equal nil writes)
          (ensure-equal 0 pulls)))))
  (let* ((chunks (list (ascii "a") nil))
         (pulls 0)
         (reply (make-http-response-stream
                 :status 200 :headers '(("x-contract" . "accepted")) :body-length 1
                 :body-function (lambda () (incf pulls) (pop chunks)))))
    (multiple-value-bind (failure response writes)
        (http3-server-coverage-serve
         (lambda (request) (declare (ignore request)) reply))
      (ensure-equal nil failure)
      (ensure-true (eq response reply))
      (ensure-equal 2 pulls)
      (let* ((frames (http3-server-coverage-frames writes))
             (fields (http-message-kit:qpack-decode-header-block
                      (http-kit/http3:http3-frame-payload (first frames)))))
        (ensure-equal "200" (http-message-kit:http-header-content
                             (find ":status" fields
                                   :key (function http-message-kit:http-header-name)
                                   :test (function string=))))
        (ensure-equal "accepted" (http-message-kit:http-header-content
                                  (find "x-contract" fields
                                        :key (function http-message-kit:http-header-name)
                                        :test (function string=))))
        (ensure-equal (ascii "a")
                      (apply #'http3-test-concat-octets
                             (mapcar #'http-kit/http3:http3-frame-payload
                                     (remove-if-not
                                      (lambda (frame)
                                        (= 0 (http-kit/http3:http3-frame-type frame)))
                                      frames)))))
      (ensure-true (second (car (last writes)))))))

(defun http3-public-server-copy-fields (fields)
  (mapcar (lambda (field)
            (cons (copy-seq (car field)) (copy-seq (cdr field))))
          fields))

(defun http3-public-server-request-contract
    (fields &key chunks trailers (collect-body-p t) failure-p)
  (let* ((fields-before (http3-public-server-copy-fields fields))
         (trailers-before (http3-public-server-copy-fields trailers))
         (chunks-before (mapcar #'copy-seq chunks))
         (wire (apply #'http3-test-concat-octets
                      (append (list (http3-boundary-headers fields))
                              (mapcar #'http3-boundary-data chunks)
                              (when trailers
                                (list (http3-boundary-headers trailers))))))
         (wire-before (copy-seq wire))
         (stream (make-http3-test-stream :kind :request))
         (reply (make-http-response :status 204 :body (octets)))
         (reads 0) (calls 0) (events nil) (actual-streams nil)
         (received nil) (writes nil) (closed nil) (errors nil)
         (request nil) (response nil)
         (failure
           (http3-boundary-error
            (lambda ()
              (setf response
                    (http-kit/http3:serve-http3-request-stream
                     stream
                     (lambda (parsed)
                       (push :handler events)
                       (incf calls)
                       (setf request parsed)
                       reply)
                     :collect-body-p collect-body-p
                     :on-body-chunk
                     (lambda (chunk)
                       (push :body events)
                       (push (copy-seq chunk) received))
                     :read-stream
                     (lambda (actual &key timeout deadline)
                       (declare (ignore timeout deadline))
                       (push actual actual-streams)
                       (push :read events)
                       (incf reads)
                       (values wire t))
                     :write-stream
                     (lambda (actual bytes &key fin-p timeout deadline)
                       (declare (ignore timeout deadline))
                       (push actual actual-streams)
                       (push :write events)
                       (push (list (copy-seq bytes) fin-p) writes))
                     :on-error
                     (lambda (condition parsed)
                       (push :error events)
                       (push (list condition parsed) errors))
                     :close-stream
                     (lambda (actual &key condition)
                       (push actual actual-streams)
                       (push :close events)
                       (push condition closed))))))))
    (ensure-equal fields-before fields)
    (ensure-equal trailers-before trailers)
    (ensure-equal chunks-before chunks)
    (ensure-equal wire-before wire)
    (ensure-equal 1 reads)
    (ensure-equal 1 (length closed))
    (ensure-equal (if failure-p 2 3) (length actual-streams))
    (dolist (actual actual-streams)
      (ensure-true (eq stream actual)))
    (ensure-equal (mapcar #'ascii chunks) (nreverse received))
    (ensure-equal
     (append '(:read) (make-list (length chunks) :initial-element :body)
             (if failure-p '(:error :close) '(:handler :write :close)))
     (nreverse events))
    (if failure-p
        (progn
          (ensure-true (typep failure 'error))
          (ensure-equal 0 calls)
          (ensure-equal nil request)
          (ensure-equal nil response)
          (ensure-equal nil writes)
          (ensure-equal 1 (length errors))
          (ensure-true (eq failure (first (first errors))))
          (ensure-equal nil (second (first errors)))
          (ensure-true (eq failure (first closed))))
        (progn
          (ensure-equal nil failure)
          (ensure-equal nil errors)
          (ensure-equal nil (first closed))
          (ensure-equal 1 calls)
          (ensure-true (eq reply response))
          (ensure-true (typep request 'http-request))
          (ensure-equal (cdr (assoc ":method" fields :test #'string=))
                        (http-request-method request))
          (ensure-equal
           (if collect-body-p
               (apply #'http3-test-concat-octets (mapcar #'ascii chunks))
               (octets))
           (http-request-body request))
          (ensure-equal 1 (length writes))
          (ensure-equal '(t) (mapcar #'second writes))
          (let ((frames (http3-server-coverage-frames writes)))
            (ensure-equal '(1) (mapcar #'http-kit/http3:http3-frame-type frames))
            (ensure-equal
             '((":status" . "204"))
             (mapcar
              (lambda (header)
                (cons (http-message-kit:http-header-name header)
                      (http-message-kit:http-header-content header)))
              (http-message-kit:qpack-decode-header-block
               (http-kit/http3:http3-frame-payload (first frames))))))))
    (values failure request)))

(defun http3-public-server-fields (&rest regular-fields)
  (append '((":method" . "POST") (":scheme" . "https")
            (":authority" . "example.test") (":path" . "/"))
          regular-fields))

(deftest http3-public-server-incoming-connection-fields-contract
  (dolist (field '(("connection" . "close")
                   ("keep-alive" . "timeout=5")
                   ("proxy-connection" . "close")
                   ("transfer-encoding" . "chunked")
                   ("upgrade" . "websocket")))
    (let ((failure (http3-public-server-request-contract
                    (http3-public-server-fields field) :failure-p t)))
      (http3-reachable-check-header-error
       failure (car field) "connection-specific fields are forbidden.")))
  (multiple-value-bind (failure request)
      (http3-public-server-request-contract
       (http3-public-server-fields '("x-contract" . "accepted")))
    (ensure-equal nil failure)
    (ensure-equal "accepted"
                  (http-header-value (http-request-headers request) "x-contract"))))

(deftest http3-public-server-incoming-te-values-contract
  (dolist (value '("gzip" "trailers, gzip"))
    (let ((failure (http3-public-server-request-contract
                    (http3-public-server-fields (cons "te" value)) :failure-p t)))
      (http3-reachable-check-header-error
       failure "te" "HTTP/3 permits only the trailers value for te.")))
  (dolist (value '("trailers" "trailers, trailers"))
    (multiple-value-bind (failure request)
        (http3-public-server-request-contract
         (http3-public-server-fields (cons "te" value)))
      (ensure-equal nil failure)
      (ensure-equal value (http-header-value (http-request-headers request) "te")))))

(deftest http3-public-server-trailer-pseudo-field-contract
  (let ((failure
          (http3-public-server-request-contract
           (http3-public-server-fields '("content-length" . "1"))
           :chunks '("a") :trailers '((":path" . "/late")) :failure-p t)))
    (ensure-true (typep failure 'http-protocol-error))
    (ensure-equal :http3-transport (http-error-operation failure))
    (ensure-equal ":path" (http-protocol-error-detail failure))
    (ensure-equal "HTTP/3 request trailers cannot contain pseudo-fields."
                  (http-error-message failure)))
  (multiple-value-bind (failure request)
      (http3-public-server-request-contract
       (http3-public-server-fields '("content-length" . "1"))
       :chunks '("a") :trailers '(("x-checksum" . "ok")))
    (ensure-equal nil failure)
    (ensure-equal "ok"
                  (http-header-value (http-request-trailers request) "x-checksum"))))

(deftest http3-public-server-noncollecting-body-contract
  (dolist (collect-body-p '(nil t))
    (multiple-value-bind (failure request)
        (http3-public-server-request-contract
         (http3-public-server-fields '("content-length" . "4"))
         :chunks '("ab" "cd") :collect-body-p collect-body-p)
      (ensure-equal nil failure)
      (ensure-equal "4"
                    (http-header-value (http-request-headers request) "content-length"))))
  (let ((failure
          (http3-public-server-request-contract
           (http3-public-server-fields '("content-length" . "5"))
           :chunks '("ab" "cd") :collect-body-p nil :failure-p t)))
    (http3-reachable-check-header-error
     failure "content-length" "content-length does not match the request body.")))

(defun http3-public-push-promise-callback-ownership (mutate-p)
  (let* ((fields '((":method" . "GET") (":scheme" . "https")
                   (":authority" . "example.test") (":path" . "/asset")
                   ("x-notice" . "original")))
         (fields-before (http3-public-server-copy-fields fields))
         (wire (http3-five-promise-wire fields))
         (wire-before (copy-seq wire))
         (fixture (http3-qpack-contract-fixture wire 0 :max-push-id 2))
         (client (http3-qpack-contract-fixture-client fixture))
         (request (make-http-request :method "GET" :uri "https://example.test/"))
         (received-fields nil)
         (push-wire (http3-test-concat-octets
                     (http-kit/http3:http3-varint-encode
                      http-kit/http3:+http3-push-stream-type+)
                     (http-kit/http3:http3-varint-encode 2)
                     (http3-boundary-headers '((":status" . "200")
                                              ("content-length" . "3")))
                     (http3-boundary-data "abc")))
         (push-before (copy-seq push-wire))
         (push-stream (make-http3-test-stream
                       :kind :push :reads (list (list push-wire t)))))
    (unwind-protect
         (progn
           (dotimes (iteration 2)
             (declare (ignore iteration))
             (ensure-equal
              204
              (http-response-status
               (http-kit/http3:send-http3-request
                client request
                :on-push-promise
                (lambda (id received)
                  (ensure-equal 2 id)
                  (ensure-equal fields received)
                  (let ((notice (assoc "x-notice" received :test #'string=)))
                    (dolist (previous received-fields)
                      (let ((old (car (last previous))))
                        (ensure-false (eq (car notice) (car old)))
                        (ensure-false (eq (cdr notice) (cdr old)))))
                    (when mutate-p
                      (setf (char (car notice) 0) #\y
                            (char (cdr notice) 0) #\X))
                    (ensure-equal (if mutate-p "y-notice" "x-notice") (car notice))
                    (ensure-equal (if mutate-p "Xriginal" "original") (cdr notice)))
                  (push received received-fields))))))
           (ensure-equal 2 (length received-fields))
           (ensure-equal '(2) (http-kit/http3:http3-client-promised-push-ids client))
           (multiple-value-bind (id response)
               (http-kit/http3:receive-http3-push client push-stream)
             (ensure-equal 2 id)
             (ensure-equal 200 (http-response-status response))
             (ensure-octets-equal (ascii "abc") (http-response-body response)))
           (ensure-true (http3-test-stream-closed-p push-stream))
           (ensure-true (http-kit/http3:http3-client-open-p client))
           (let ((events (http3-qpack-contract-fixture-events fixture)))
             (ensure-equal 3 (count :close events :key #'first))
             (dolist (event events)
               (when (eq :close (first event))
                 (ensure-equal nil (third event)))))
           (ensure-equal fields-before fields)
           (ensure-octets-equal wire-before wire)
           (ensure-octets-equal push-before push-wire))
      (http3-qpack-contract-cleanup fixture))))

(deftest http3-public-push-promise-callback-ownership-control
  (http3-public-push-promise-callback-ownership nil))

(deftest http3-public-push-promise-callback-ownership-mutated-strings
  (http3-public-push-promise-callback-ownership t))

(deftest http3-public-server-stream-final-status-boundaries
  (dolist (status '(100 199 200 599))
    (let* ((body-calls 0)
           (value (make-http-response-stream
                   :status status :body-length 0
                   :body-function (lambda () (incf body-calls) nil))))
      (multiple-value-bind (failure response writes)
          (http3-server-coverage-serve
           (lambda (request) (declare (ignore request)) value))
        (if (< status 200)
            (progn
              (http3-public-error-check
               failure "HTTP/3 handlers must return a final status from 200 through 599."
               status)
              (ensure-equal 0 body-calls)
              (ensure-equal nil response)
              (ensure-equal nil writes))
            (progn
              (ensure-equal nil failure)
              (ensure-true (eq value response))
              (ensure-equal 1 body-calls)
              (ensure-equal '(t) (mapcar #'second writes))
              (let ((frames (http3-server-coverage-frames writes)))
                (ensure-equal '(1) (mapcar #'http-kit/http3:http3-frame-type frames))
                (ensure-equal
                 (write-to-string status)
                 (http-message-kit:http-header-content
                   (find ":status"
                         (http-message-kit:qpack-decode-header-block
                          (http-kit/http3:http3-frame-payload (first frames)))
                         :key (function http-message-kit:http-header-name)
                         :test (function string=)))))))))))

(deftest http3-server-coverage-final-bodyless-status-rejects-content
  (dolist (method (list "GET" "HEAD"))
    (dolist (status (list 204 304))
      (dolist (stream-p (list nil t))
        (dolist (content-kind (list :body :trailers))
          (let* ((body-p (eq content-kind :body))
                 (pulls 0)
                 (trailers (unless body-p (list (cons "x-end" "yes"))))
                 (reply
                   (if stream-p
                       (make-http-response-stream
                        :status status :trailers trailers
                        :body-length (if body-p 1 0)
                        :body-function
                        (lambda ()
                          (incf pulls)
                          (when (and body-p (= pulls 1)) (ascii "x"))))
                       (make-http-response
                        :status status :body (if body-p (ascii "x") (octets))
                        :trailers trailers))))
            (multiple-value-bind (failure response writes)
                (http3-server-coverage-serve
                 (lambda (request) (declare (ignore request)) reply)
                 :method method)
              (http3-public-error-check
               failure
               (if body-p
                   "This HTTP/3 response cannot contain a body."
                   "This HTTP/3 response cannot contain trailers.")
               nil)
              (ensure-equal nil response)
              (ensure-equal nil writes)
              (ensure-equal 0 pulls))))))))

(deftest http3-server-coverage-informational-response-rejects-content
  (dolist (stream-p (list nil t))
    (dolist (content-kind (list :body :trailers))
      (let* ((body-p (eq content-kind :body))
             (pulls 0)
             (trailers (unless body-p (list (cons "x-end" "yes"))))
             (information
               (if stream-p
                   (make-http-response-stream
                    :status 103 :trailers trailers
                    :body-length (if body-p 1 0)
                    :body-function
                    (lambda ()
                      (incf pulls)
                      (when (and body-p (= pulls 1)) (ascii "x"))))
                   (make-http-response
                    :status 103 :body (if body-p (ascii "x") (octets))
                    :trailers trailers)))
             (final (make-http-response :status 204)))
        (multiple-value-bind (failure response writes)
            (http3-server-coverage-serve
             (lambda (request)
               (declare (ignore request))
               (values final (list information))))
          (http3-public-error-check
           failure
           (if body-p
               "An HTTP/3 informational response cannot contain a body."
               "An HTTP/3 informational response cannot contain trailers.")
           nil)
          (ensure-equal nil response)
          (ensure-equal nil writes)
          (ensure-equal 0 pulls))))))
(deftest http3-server-qpack-section-ack-callback-skips-static-section
  (let* ((decoder-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 0 :table-capacity 0 :huffman nil))
         (adapter
           (http-message-kit:qpack-make-http3-decoder-context decoder-context))
         (acknowledgments nil))
    (multiple-value-bind (failure response writes)
        (http3-server-coverage-serve
         (lambda (request)
           (declare (ignore request))
           (make-http-response :status 204))
         :qpack-decoder-context decoder-context
         :qpack-http3-decoder-context adapter
         :stream-id 4
         :on-qpack-section-acknowledgment
         (lambda (stream-id) (push stream-id acknowledgments)))
      (ensure-equal nil failure)
      (ensure-equal 204 (http-response-status response))
      (ensure-equal nil acknowledgments)
      (ensure-equal '(t) (mapcar #'second writes))
      (let ((frames (http3-server-coverage-frames writes)))
        (ensure-equal '(1) (mapcar #'http-kit/http3:http3-frame-type frames))
        (ensure-equal "204"
                      (http-message-kit:http-header-content
                       (find ":status"
                             (http-message-kit:qpack-decode-header-block
                              (http-kit/http3:http3-frame-payload (first frames)))
                             :key (function http-message-kit:http-header-name)
                             :test (function string=))))))))
