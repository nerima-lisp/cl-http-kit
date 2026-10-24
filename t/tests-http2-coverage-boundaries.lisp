(in-package #:http-kit/test)

#+sbcl
(progn
  (defclass h2-control-write-failure-stream (binary-session-stream)
    ((failure :initarg :failure :reader h2-control-write-failure)
     (armed-p :initform nil :accessor h2-control-write-failure-armed-p)))

  (defmethod sb-gray:stream-write-sequence :around
      ((stream h2-control-write-failure-stream) sequence &optional (start 0) end)
    (declare (ignore sequence start end))
    (if (h2-control-write-failure-armed-p stream)
        (error (h2-control-write-failure stream))
        (call-next-method)))

  (defun check-h2-public-control-write-failure (goaway-p)
    (let* ((failure (make-condition 'http-connection-error
                                    :operation :http2-write :cause :test-write))
           (stream (make-instance 'h2-control-write-failure-stream
                                  :input (h2-response-wire (octets 97))
                                  :failure failure))
           (closed 0)
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream
              :close-stream (lambda (owned-stream)
                              (ensure-true (eq stream owned-stream))
                              (incf closed)
                              (close owned-stream)))))
      (unwind-protect
           (progn
             (let ((response
                     (http-kit/http2:send-http2-request-over-connection
                      connection (make-http-request :method "GET"
                                                    :uri "https://example.com/"))))
               (ensure-equal 200 (http-response-status response))
               (ensure-equal (octets 97) (http-response-body response)))
             (ensure-equal (length (binary-session-input stream))
                           (binary-session-input-position stream))
             (ensure-true (http-kit/http2:http2-connection-open-p connection))
             (if goaway-p
                 (http-kit/http2:send-http2-goaway
                  connection :last-stream-id 1 :debug-data (octets 98))
                 (http-kit/http2:send-http2-priority-update
                  connection 1 :priority-field-value "u=3"))
             (let* ((frame (car (last (h2-output-frames stream))))
                    (payload (http-kit/http2::%h2-frame-payload frame)))
               (ensure-equal (if goaway-p 7 16)
                             (http-kit/http2::%h2-frame-type frame))
               (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id frame))
               (ensure-equal (if goaway-p
                                 (octets 0 0 0 1 0 0 0 0 98)
                                 (octets 0 0 0 1 117 61 51))
                             payload))
             (let ((before (copy-seq (binary-session-output stream)))
                   (position-before (binary-session-input-position stream))
                   (observed nil))
               (setf (h2-control-write-failure-armed-p stream) t)
               (handler-case
                   (if goaway-p
                       (http-kit/http2:send-http2-goaway
                        connection :last-stream-id 0 :debug-data (octets 99))
                       (http-kit/http2:send-http2-priority-update
                        connection 1 :priority-field-value "u=1"))
                 (http-connection-error (condition) (setf observed condition)))
               (ensure-true (eq failure observed))
               (ensure-equal :http2-write (http-error-operation observed))
               (ensure-equal :test-write (http-connection-error-cause observed))
               (ensure-equal before (binary-session-output stream))
               (ensure-equal position-before (binary-session-input-position stream))
               (ensure-equal 1 closed)
               (ensure-false (open-stream-p stream))
               (ensure-false (http-kit/http2:http2-connection-open-p connection))
               (ensure-equal goaway-p
                             (http-kit/http2:http2-connection-draining-p connection))
               (ensure-equal (if goaway-p 0 nil)
                             (http-kit/http2:http2-connection-local-goaway-last-stream-id
                              connection))
               (http-kit/http2:close-http2-connection connection)
               (ensure-equal 1 closed)))
        (http-kit/http2:close-http2-connection connection))))

  (deftest http2-coverage-goaway-write-failure-cleanup
    (check-h2-public-control-write-failure t))

  (deftest http2-coverage-priority-write-failure-cleanup
    (check-h2-public-control-write-failure nil))

  (deftest http2-coverage-server-te-request-values
    (let ((fields '((":method" . "GET") (":scheme" . "https")
                    (":authority" . "example.com") (":path" . "/"))))
      (dolist (value '("trailers" " Trailers " "trailers, trailers"))
        (check-server-public-request-validation
         (append fields (list (cons "te" value))) :method "GET"))
      (dolist (value '("gzip" "trailers, gzip"))
        (check-server-public-request-validation
         (append fields (list (cons "te" value)))
         :name "te" :reason :value :operation :http2-headers))))

  (deftest http2-coverage-client-control-window-boundaries
    (let ((calls 0)
          (request (make-http-request :method "GET" :uri "https://example.com/")))
      (flet ((exchange (seen-request wire &key timeout deadline)
               (incf calls)
               (ensure-true (eq request seen-request))
               (ensure-true (> (length wire) (length (h2-preface))))
               (ensure-equal (h2-preface) (subseq wire 0 (length (h2-preface))))
               (ensure-false timeout)
               (ensure-false deadline)
               (h2-response-wire (octets 97))))
        (dolist (value '(-1 "1" #C(1 1)))
          (let ((observed nil))
            (handler-case
                (http-kit/http2:make-http2-client
                 :exchange #'exchange :max-control-window value)
              (http-protocol-error (condition) (setf observed condition)))
            (ensure-true (typep observed 'http-protocol-error))
            (ensure-equal :http2-client (http-error-operation observed))
            (ensure-equal value (http-protocol-error-detail observed))
            (ensure-equal 0 calls)))
        (dolist (value '(0 1/2))
          (let* ((before calls)
                 (client (http-kit/http2:make-http2-client
                          :exchange #'exchange :max-control-window value))
                 (response (http-kit/http2:send-http2-request client request)))
            (ensure-equal (1+ before) calls)
            (ensure-equal 200 (http-response-status response))
            (ensure-equal (octets 97) (http-response-body response))))))))

#+sbcl
(deftest http2-coverage-batch-continuation-interrupted-by-ping
  (let* ((block (h2-header-block (cons ":status" "200")
                                (cons "content-length" "0")))
         (split (floor (length block) 2))
         (prefix (concatenate-octets (h2-frame 4 0 0 (octets))
                                (h2-frame 1 1 1 (subseq block 0 split)))))
    (ensure-true (< 0 split (length block)))
    (check-public-batch-boundary
     (concatenate-octets prefix
                    (h2-frame 9 4 1 (subseq block split))
                    (h2-frame 1 5 3 (h2-header-block (cons ":status" "201")))))
    (check-public-batch-boundary
     (concatenate-octets prefix (h2-frame 6 0 0 (octets 0 1 2 3 4 5 6 7)))
     :operation :http2-headers :detail '(6 0))))

#+sbcl
(deftest http2-coverage-batch-continuation-eof
  (let* ((block (h2-header-block (cons ":status" "200")))
         (split (max 1 (floor (length block) 2)))
         (fragment (subseq block 0 split))
         (reads 0)
         (condition
           (handler-case
               (http-kit/http2::%h2-batch-read-header-block
                (h2-frame-object 1 1 1 fragment)
                (lambda () (incf reads) :eof) 16384 4096)
             (http-protocol-error (error) error))))
    (ensure-true (typep condition 'http-protocol-error))
    (ensure-equal :http2-headers (http-error-operation condition))
    (ensure-equal :eof (http-protocol-error-detail condition))
    (ensure-equal 1 reads)
    (let* ((stream (make-instance 'binary-session-stream
                                 :input (concatenate-octets
                                         (h2-frame 4 0 0 (octets))
                                         (h2-frame 1 1 1 fragment))))
           (closed 0)
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream
              :close-stream (lambda (received)
                              (ensure-true (eq stream received))
                              (incf closed)
                              (close received)))))
      (unwind-protect
           (let ((error
                   (handler-case
                       (http-kit/http2:send-http2-requests-over-connection
                        connection
                        (list (make-http-request :method "GET" :uri "https://example.com/one")
                              (make-http-request :method "GET" :uri "https://example.com/two")))
                     (http-connection-error (error) error))))
             (ensure-true (typep error 'http-connection-error))
             (ensure-equal :http2-read (http-error-operation error))
             (ensure-equal :eof (http-connection-error-cause error))
             (ensure-equal 1 closed)
             (ensure-false (http-kit/http2:http2-connection-open-p connection))
             (ensure-false (open-stream-p stream))
             (ensure-equal (length (binary-session-input stream))
                           (binary-session-input-position stream))
             (ensure-equal '(1 3)
                           (mapcar #'http-kit/http2::%h2-frame-stream-id
                                   (remove-if-not
                                    (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                                    (h2-output-frames stream)))))
        (http-kit/http2:close-http2-connection connection))
      (ensure-equal 1 closed))))

#+sbcl
(deftest http2-coverage-server-priority-update-element-id
  (dolist (payload (list (octets 0 0 0 0) (octets 128 0 0 1)))
    (check-public-server-boundary
     (concatenate-octets (h2-frame 4 0 0 (octets))
                        (h2-frame 16 0 0 payload))
     :error-p t :detail (http-kit/http2::%h2-u32 payload 0)
     :handled 0 :last-stream 0 :statuses nil)))

#+sbcl
(deftest http2-coverage-server-reset-blocked-response
  (let* ((stream (make-instance 'binary-session-stream
                                :input (concatenate-octets
                                        (h2-preface)
                                        (h2-frame 4 0 0 (octets 0 4 0 0 0 0))
                                        (public-server-request-frame)
                                        (h2-frame 3 0 1 (octets 0 0 0 8)))))
         (calls 0) (closed 0) (notified nil)
         (result
           (multiple-value-list
            (http-kit/http2:serve-http2-session
             stream
             (lambda (request)
               (incf calls)
               (ensure-equal "GET" (http-request-method request))
               (ensure-equal "/" (http-request-target request))
               (ensure-equal (octets) (http-request-body request))
               (make-http-response
                :status 200
                :headers (list (make-http-header "content-length" "1"))
                :body (octets 97)))
             :on-error (lambda (condition request)
                         (declare (ignore request)) (setf notified condition))
             :close-stream (lambda (received)
                             (ensure-true (eq stream received))
                             (incf closed) (close received)))))
         (frames (server-output-frames (binary-session-output stream)))
         (headers (remove-if-not
                   (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                   frames)))
    (ensure-equal 1 calls)
    (ensure-equal '(0 :eof) result)
    (ensure-equal 1 closed)
    (ensure-false notified)
    (ensure-false (open-stream-p stream))
    (ensure-equal (length (binary-session-input stream))
                  (binary-session-input-position stream))
    (ensure-equal 1 (length headers))
    (ensure-equal "200" (cdr (assoc ":status" (server-response-fields (first headers))
                                   :test #'string=)))
    (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id (first headers)))
    (ensure-equal 4 (http-kit/http2::%h2-frame-flags (first headers)))
    (ensure-equal "1" (cdr (assoc "content-length"
                                 (server-response-fields (first headers))
                                 :test #'string=)))
    (ensure-false (find 0 frames :key #'http-kit/http2::%h2-frame-type))
    (ensure-false (find 7 frames :key #'http-kit/http2::%h2-frame-type))
    (ensure-equal 1
                  (count-if (lambda (frame)
                              (and (= 4 (http-kit/http2::%h2-frame-type frame))
                                   (= 1 (http-kit/http2::%h2-frame-flags frame))))
                            frames))))

#+sbcl
(deftest http2-coverage-response-positional-options-and-flow-order
  (dolist (mode '(:omitted :keyword :no-collection))
    (let* ((payload (octets 0 1 2 3 4 5 6 7))
           (reader (h2-reader-from-frames
                    (h2-frame 4 0 0 (octets))
                    (h2-frame 6 0 0 payload)
                    (h2-frame 1 4 1 (h2-header-block
                                     (cons ":status" "200")
                                     (cons "content-length" "2")))
                    (h2-frame 0 9 1 (octets 1 97 98 0))))
           (writes nil)
           (calls 0)
           (callback (lambda (chunk)
                       (incf calls)
                       (ensure-equal (octets 97 98) chunk)
                       (ensure-equal
                        (list (h2-frame 4 1 0 (octets)) (h2-frame 6 1 0 payload))
                        (reverse writes))))
           (options (ecase mode
                      (:omitted (list callback))
                      (:keyword (list callback :expected-stream-id 1))
                      (:no-collection (list callback nil :expected-stream-id 1))))
           (response (apply #'http-kit/http2::%h2-read-response
                            reader (lambda (wire) (push wire writes))
                            16384 nil (lambda () 0) 4096 16 "GET" options)))
      (ensure-equal 1 calls)
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "2" (http-header-value (http-response-headers response)
                                           "content-length"))
      (ensure-equal (if (eq mode :no-collection) (octets) (octets 97 98))
                    (http-response-body response))
      (ensure-equal
       (list (h2-frame 4 1 0 (octets)) (h2-frame 6 1 0 payload)
             (h2-frame 8 0 0 (octets 0 0 0 4)))
       (reverse writes)))))

#+sbcl
(deftest http2-coverage-response-option-errors-before-io
  (dolist (options '((:expected-stream-id) (:unknown-option 1)))
    (let ((reads 0) (writes 0) (clocks 0))
      (let ((condition
              (handler-case
                  (apply #'http-kit/http2::%h2-read-response
                         (lambda () (incf reads) :eof)
                         (lambda (wire) (declare (ignore wire)) (incf writes))
                         16384 nil (lambda () (incf clocks)) 4096 16 "GET"
                         options)
                (http-protocol-error (error) error))))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :http2-read (http-error-operation condition))
        (ensure-equal (first options) (http-protocol-error-detail condition)))
      (ensure-equal 0 reads)
      (ensure-equal 0 writes)
      (ensure-equal 0 clocks))))

#+sbcl
(deftest http2-coverage-data-adapter-stream-and-padding
  (let ((body (make-array 1 :element-type '(unsigned-byte 8)
                          :adjustable t :fill-pointer 1 :initial-element 97))
        (calls 0))
    (ensure-equal
     '(t 3 4)
     (multiple-value-list
      (http-kit/http2::%h2-append-data-frame
       (h2-frame-object 0 9 3 (octets 1 98 99 0)) 200 body 1 "GET" 3
       (lambda (chunk)
         (incf calls)
         (ensure-equal (octets 98 99) chunk)
         (ensure-equal (octets 97 98 99) body))
       t 3)))
    (ensure-equal 1 calls)
    (ensure-equal (octets 97 98 99) body)))

#+sbcl
(deftest http2-coverage-headers-adapter-explicit-stream
  (let ((reads 0))
    (multiple-value-bind (status headers response)
        (http-kit/http2::%h2-process-headers-frame
         (h2-frame-object 1 5 3 (h2-header-block (cons ":status" "201")
                                               (cons "content-length" "0")))
         (lambda () (incf reads) :eof) 16384 nil (lambda () 0) 4096
         (h2-hpack-context) nil nil (octets) 0 "GET" 3)
      (ensure-equal 201 status)
      (ensure-equal "0" (http-header-value headers "content-length"))
      (ensure-equal 201 (http-response-status response))
      (ensure-equal (octets) (http-response-body response))
      (ensure-equal 0 reads))))

#+sbcl
(deftest http2-coverage-frame-adapter-arity-errors
  (let ((body (make-array 1 :element-type '(unsigned-byte 8)
                          :adjustable t :fill-pointer 1 :initial-element 97))
        (reads 0))
    (dolist (arguments '(nil (1 "GET" 1 nil :extra)))
      (let ((condition
              (handler-case
                  (apply #'http-kit/http2::%h2-process-headers-frame
                         (h2-frame-object 1 5 1 (octets))
                         (lambda () (incf reads) :eof) 16384 nil (lambda () 0)
                         4096 (h2-hpack-context) nil nil body
                         arguments)
                (http-protocol-error (error) error))))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :http2-read (http-error-operation condition))
        (ensure-equal arguments (http-protocol-error-detail condition))))
    (dolist (arguments '(nil (1 "GET" 2) (1 "GET" 2 nil t 1 :extra)))
      (let ((condition
              (handler-case
                  (apply #'http-kit/http2::%h2-append-data-frame
                         (h2-frame-object 0 1 1 (octets 98)) 200 body arguments)
                (http-protocol-error (error) error))))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :http2-read (http-error-operation condition))
        (ensure-equal arguments (http-protocol-error-detail condition))))
    (ensure-equal (octets 97) body)
    (ensure-equal 0 reads)))

#+sbcl
(deftest http2-coverage-window-update-upper-bound
  (let ((writes nil))
    (flet ((writer (wire) (push wire writes)))
      (http-kit/http2::%h2-send-window-update #'writer 3 0)
      (ensure-false writes)
      (http-kit/http2::%h2-send-window-update #'writer 3 #x7fffffff)
      (ensure-equal (list (h2-frame 8 0 3 (octets 127 255 255 255))) writes)
      (let ((condition
              (handler-case
                  (http-kit/http2::%h2-send-window-update #'writer 3 #x80000000)
                (http-protocol-error (error) error))))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :http2-control (http-error-operation condition))
        (ensure-equal #x80000000 (http-protocol-error-detail condition)))
      (ensure-equal (list (h2-frame 8 0 3 (octets 127 255 255 255))) writes))))

#+sbcl
(deftest http2-coverage-nonbudgeted-frame-clock-isolation
  (let ((times (list 1 2)) (clocks 0))
    (dolist (type '(0 1 3 9 255))
      (multiple-value-bind (result exceeded)
          (http-kit/http2::%h2-control-budget-note
           times type 0 0 (lambda () (incf clocks)))
        (ensure-true (eq times result))
        (ensure-false exceeded)
        (ensure-equal '(1 2) times)
        (ensure-equal 0 clocks)))))

#+sbcl
(deftest http2-coverage-server-stream-omitted-defaults
  (let* ((first (http-kit/http2::%make-h2-server-stream 1))
         (second (http-kit/http2::%make-h2-server-stream 3))
         (body (http-kit/http2::%h2-server-stream-body first)))
    (ensure-equal 1 (http-kit/http2::%h2-server-stream-id first))
    (ensure-equal 0 (http-kit/http2::%h2-server-stream-body-length-seen first))
    (ensure-equal (octets) body)
    (ensure-true (adjustable-array-p body))
    (ensure-true (array-has-fill-pointer-p body))
    (ensure-true (not (eq body (http-kit/http2::%h2-server-stream-body second))))
    (vector-push-extend 97 body)
    (ensure-equal (octets 97) body)
    (ensure-equal (octets) (http-kit/http2::%h2-server-stream-body second))
    (dolist (state (list first second))
      (ensure-equal 65535 (http-kit/http2::%h2-server-stream-send-window state))
      (ensure-equal 65535 (http-kit/http2::%h2-server-stream-receive-window state))
      (ensure-false (http-kit/http2::%h2-server-stream-reset-p state))
      (ensure-false (http-kit/http2::%h2-server-stream-responded-p state)))
    (ensure-equal 65536
                  (http-kit/http2::%h2-server-window-add
                   (http-kit/http2::%h2-server-stream-send-window first) 1 :stream))
    (ensure-equal 65536
                  (http-kit/http2::%h2-server-window-add
                   (http-kit/http2::%h2-server-stream-receive-window first) 1 :stream))))

#+sbcl
(deftest http2-coverage-settings-peer-role-validation
  (dolist (role '(nil :invalid 1))
    (let ((condition
            (handler-case
                (http-kit/http2::%h2-settings (octets 0) :peer-role role)
              (http-protocol-error (error) error))))
      (ensure-true (typep condition 'http-protocol-error))
      (ensure-equal :http2-settings (http-error-operation condition))
      (ensure-equal role (http-protocol-error-detail condition))))
  (ensure-equal '(nil nil nil nil nil nil)
                (multiple-value-list (http-kit/http2::%h2-settings (octets))))
  (ensure-equal '(nil nil nil nil nil nil)
                (multiple-value-list
                 (http-kit/http2::%h2-settings (octets 0 2 0 0 0 1) :peer-role :client))))

#+sbcl
(deftest http2-coverage-server-window-increment-validation
  (dolist (increment '(nil 0 -1 1/2 "1"))
    (let ((condition
            (handler-case
                (http-kit/http2::%h2-server-window-add 65535 increment :stream)
              (http-protocol-error (error) error))))
      (ensure-true (typep condition 'http-protocol-error))
      (ensure-equal :http2-server (http-error-operation condition))
      (ensure-equal (list :stream increment) (http-protocol-error-detail condition))
      (ensure-equal 1 (http-kit::http-protocol-error-http2-error-code condition))))
  (ensure-equal #x7fffffff
                (http-kit/http2::%h2-server-window-add 65535 (- #x7fffffff 65535) :stream)))

#+sbcl
(deftest http2-coverage-response-omitted-body-length
  (let* ((body (octets 97 98 99))
         (headers (list (make-http-header "content-length" "3")))
         (trailers (list (make-http-header "x-end" "done"))))
    (dolist (response
             (list (http-kit/http2::%h2-finish-request-response
                    200 headers trailers body "GET")
                   (http-kit/http2::%h2-finish-request-response
                    200 headers trailers body "GET" :body-length 3)))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal "HTTP/2" (http-response-protocol-version response))
      (ensure-equal body (http-response-body response))
      (ensure-equal "3" (http-header-value (http-response-headers response) "content-length"))
      (ensure-equal "done" (http-header-value (http-response-trailers response) "x-end")))
    (let ((condition
            (handler-case
                (http-kit/http2::%h2-finish-request-response
                 200 headers nil (octets 97) "GET")
              (http-invalid-header (error) error))))
      (ensure-true (typep condition 'http-invalid-header))
      (ensure-equal :http2-response (http-error-operation condition))
      (ensure-equal "content-length" (http-invalid-header-name condition))
      (ensure-equal :mismatch (http-invalid-header-reason condition)))
    (dolist (response
             (list (http-kit/http2::%h2-finish-request-response
                    200 headers nil (octets) "HEAD")
                   (http-kit/http2::%h2-finish-request-response
                    200 headers nil (octets) "GET" :body-length 3)))
      (ensure-equal 200 (http-response-status response))
      (ensure-equal (octets) (http-response-body response))
      (ensure-equal "3" (http-header-value (http-response-headers response) "content-length")))))

#+sbcl
(deftest http2-coverage-batch-control-frame-payloads
  (check-public-batch-control-frame
   (h2-frame 3 0 1 (octets 0 0 8)) :operation :http2-control :detail '(1 3))
  (check-public-batch-control-frame
   (h2-frame 5 0 1 (octets 0 0 0 2)) :operation :http2-read :detail :protocol-error)
  (dolist (payload (list (octets 0 0 1) (octets 128 0 0 1)))
    (check-public-batch-control-frame
     (h2-frame 8 0 0 payload) :operation :http2-control :detail (list 0 payload))))

#+sbcl
(deftest http2-coverage-batch-ignored-control-frames
  (check-public-batch-control-frame (h2-frame 2 0 1 (octets 0 0 0 0 0)))
  (check-public-batch-control-frame (h2-frame 255 0 0 (octets 7))))

#+sbcl
(deftest http2-coverage-server-priority-and-closed-window
  (check-public-server-boundary
   (concatenate-octets (h2-frame 4 0 0 (octets))
                       (h2-frame 2 0 1 (octets 0 0 0 1 0)))
   :error-p t :detail 1 :last-stream 0 :statuses nil)
  (check-public-server-boundary
   (concatenate-octets (h2-frame 4 0 0 (octets))
                       (h2-frame 2 0 1 (octets 0 0 0 0 0))
                       (public-server-request-frame))
   :handled 1)
  (check-public-server-boundary
   (concatenate-octets (h2-frame 4 0 0 (octets))
                       (public-server-request-frame)
                       (h2-frame 8 0 1 (octets 0 0 0 1)))
   :handled 1))
#+sbcl
(deftest http2-coverage-hpack-adapter-validates-name-and-preserves-non-list-detail
  (let ((condition
          (handler-case
              (http-kit/http2::%h2-encode-header-block
               (list (cons 42 "value")))
            (http-invalid-header (condition) condition))))
    (ensure-true condition)
    (ensure-equal :header (http-error-operation condition))
    (ensure-equal 42 (http-invalid-header-name condition))
    (ensure-equal :name (http-invalid-header-reason condition)))
  (let ((condition
          (handler-case
              (http-kit/http2::%h2-decode-header-block
               (octets)
               (http-kit/http2::%h2-make-hpack-context)
               :max-fields -1)
            (http-protocol-error (condition) condition))))
    (ensure-true condition)
    (ensure-equal :hpack (http-error-operation condition))
    (ensure-equal -1 (http-protocol-error-detail condition))))

#+sbcl
(deftest http2-coverage-serial-fallback-direct-reuse
  (let* ((input
           (concatenate-octets
            (h2-frame 4 0 0 (octets))
            (h2-frame 1 4 1
                      (h2-header-block (cons ":status" "103")))
            (h2-frame 1 4 1
                      (h2-header-block (cons ":status" "200")
                                       (cons "content-length" "2")))
            (h2-frame 0 1 1 (octets 97 98))
            (h2-frame 1 4 3
                      (h2-header-block (cons ":status" "201")
                                       (cons "content-length" "1")))
            (h2-frame 0 1 3 (octets 99))))
         (stream (make-instance 'binary-session-stream :input input))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (information-statuses nil)
         (body-chunks nil)
         (first-response
           (http-kit/http2::%h2-send-http2-request-over-connection-serial
            connection
            (make-http-request :method "GET" :uri "https://example.com/one")
            :on-information
            (lambda (response)
              (push (http-response-status response) information-statuses))
            :on-body-chunk (lambda (chunk) (push (copy-seq chunk) body-chunks))))
         (second-response
           (http-kit/http2::%h2-send-http2-request-over-connection-serial
            connection
            (make-http-request :method "GET" :uri "https://example.com/two")
            :on-body-chunk (lambda (chunk) (push (copy-seq chunk) body-chunks))))
         (frames (h2-output-frames stream)))
    (ensure-equal '(103) (nreverse information-statuses))
    (ensure-equal 200 (http-response-status first-response))
    (ensure-equal (octets 97 98) (http-response-body first-response))
    (ensure-equal 201 (http-response-status second-response))
    (ensure-equal (octets 99) (http-response-body second-response))
    (ensure-equal (list (octets 97 98) (octets 99)) (nreverse body-chunks))
    (ensure-true (http-kit/http2:http2-connection-session-started-p connection))
    (ensure-equal 5 (http-kit/http2::%http2-connection-next-stream-id connection))
    (ensure-equal '(1 3)
                  (mapcar #'http-kit/http2::%h2-frame-stream-id
                          (remove-if-not
                           (lambda (frame)
                             (= 1 (http-kit/http2::%h2-frame-type frame)))
                           frames)))
    (ensure-equal 1
                  (count-if
                   (lambda (frame)
                     (and (= 4 (http-kit/http2::%h2-frame-type frame))
                          (= 1 (http-kit/http2::%h2-frame-flags frame))))
                   frames))
    (ensure-equal (length input) (binary-session-input-position stream))
    (http-kit/http2:close-http2-connection connection)))

#+sbcl
(deftest http2-coverage-serial-fallback-peer-reset-keeps-session
  (let* ((input
           (concatenate-octets
            (h2-frame 4 0 0 (octets))
            (h2-frame 3 0 1 (octets 0 0 0 8))))
         (stream (make-instance 'binary-session-stream :input input))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (condition
           (handler-case
               (http-kit/http2::%h2-send-http2-request-over-connection-serial
                connection
                (make-http-request :method "GET" :uri "https://example.com/reset"))
             (http-connection-error (error) error))))
    (ensure-true (typep condition 'http-connection-error))
    (ensure-equal :http2-control (http-error-operation condition))
    (ensure-equal '(:rst-stream 1 8)
                  (http-connection-error-cause condition))
    (ensure-true (http-kit/http2:http2-connection-open-p connection))
    (ensure-true (open-stream-p stream))
    (ensure-equal 3 (http-kit/http2::%http2-connection-next-stream-id connection))
    (http-kit/http2:close-http2-connection connection)
    (ensure-false (open-stream-p stream))))

#+sbcl
(deftest http2-coverage-scheduler-control-frame-state
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (clock (lambda () 0)))
    (setf (http-kit/http2::%http2-connection-scheduler-first-peer-frame-p
           connection) nil
          (http-kit/http2::%http2-connection-next-stream-id connection) 5)
    (labels ((dispatch (frame)
               (http-kit/http2::%h2-scheduler-dispatch-frame
                connection frame nil nil nil clock))
             (expect-protocol-error (frame operation detail)
               (let ((condition
                       (handler-case (dispatch frame)
                         (http-protocol-error (error) error))))
                 (ensure-true (typep condition 'http-protocol-error))
                 (ensure-equal operation (http-error-operation condition))
                 (ensure-equal detail (http-protocol-error-detail condition))))
             (read-output-frames ()
               (let ((reader
                       (http-kit/http2::%h2-reader-for
                        (binary-session-output stream)))
                     (frames nil))
                 (loop for frame = (http-kit/http2::%h2-read-frame
                                    reader 16384 nil nil)
                       until (eq frame :eof)
                       do (push frame frames))
                 (nreverse frames))))
      (expect-protocol-error
       (h2-frame-object 6 0 1 (octets 1 2 3 4 5 6 7 8))
       :http2-control '(1 8))
      (expect-protocol-error
       (h2-frame-object 8 0 5 (octets 0 0 0 1))
       :http2-control 5)
      (expect-protocol-error
       (h2-frame-object 3 0 5 (octets 0 0 0 8))
       :http2-control 5)
      (let ((payload (octets 1 2 3 4 5 6 7 8)))
        (dispatch (h2-frame-object 6 0 0 payload))
        (let ((ping (first (read-output-frames))))
          (ensure-equal 6 (http-kit/http2::%h2-frame-type ping))
          (ensure-equal 1 (http-kit/http2::%h2-frame-flags ping))
          (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id ping))
          (ensure-equal payload (http-kit/http2::%h2-frame-payload ping)))
        (let ((waiter
                (http-kit/http2::%make-h2-scheduler-ping-waiter payload)))
          (setf (http-kit/http2::%http2-connection-scheduler-ping-waiters
                 connection) (list waiter))
          (let* ((frame (h2-frame-object 6 0 1 payload))
                 (condition
                   (handler-case (dispatch frame)
                     (http-protocol-error (error) error))))
            (ensure-true (typep condition 'http-protocol-error))
            (ensure-equal :http2-client (http-error-operation condition))
            (ensure-true
             (eq frame (http-protocol-error-detail condition))))
          (dispatch (h2-frame-object 6 1 0 (octets 8 7 6 5 4 3 2 1)))
          (ensure-false
           (http-kit/http2::%h2-scheduler-ping-waiter-acknowledged-p waiter))
          (dispatch (h2-frame-object 6 1 0 payload))
          (ensure-true
           (http-kit/http2::%h2-scheduler-ping-waiter-acknowledged-p waiter)))
        (dispatch (h2-frame-object 8 0 0 (octets 0 0 0 1)))
        (ensure-equal 65536
                      (http-kit/http2::%http2-connection-peer-connection-window-size
                       connection)))
      (let* ((request
               (make-http-request :method "GET" :uri "https://example.com/reset"))
             (entry (http-kit/http2::%make-h2-batch-entry
                     :request request :stream-id 1)))
        (setf (http-kit/http2::%http2-connection-next-stream-id connection) 3
              (http-kit/http2::%http2-connection-scheduler-last-opened-stream-id
               connection) 1
              (http-kit/http2::%http2-connection-scheduler-entries connection)
              (list entry))
        (dispatch (h2-frame-object 3 0 1 (octets 0 0 0 8)))
        (ensure-true (typep (http-kit/http2::%h2-batch-entry-error entry)
                            'http-connection-error))
        (ensure-equal '(:rst-stream 1 8)
                      (http-connection-error-cause
                       (http-kit/http2::%h2-batch-entry-error entry)))
        (ensure-equal (list entry)
                      (http-kit/http2::%http2-connection-scheduler-entries
                       connection)))
      (let* ((request
               (make-http-request :method "GET" :uri "https://example.com/retired"))
             (entry (http-kit/http2::%make-h2-batch-entry
                     :request request :stream-id 3)))
        (setf (http-kit/http2::%h2-batch-entry-local-retired-p entry) t
              (http-kit/http2::%http2-connection-next-stream-id connection) 5
              (http-kit/http2::%http2-connection-scheduler-last-opened-stream-id
               connection) 3
              (http-kit/http2::%http2-connection-scheduler-entries connection)
              (list entry))
        (dispatch (h2-frame-object 7 0 0 (octets 0 0 0 1 0 0 0 0)))
        (ensure-equal 1
                      (http-kit/http2:http2-connection-goaway-last-stream-id
                       connection))
        (ensure-equal '(:goaway 1 0)
                      (http-connection-error-cause
                       (http-kit/http2::%h2-batch-entry-error entry)))
        (ensure-false
         (http-kit/http2::%http2-connection-scheduler-entries connection))))
    (http-kit/http2:close-http2-connection connection)))

#+sbcl
(deftest http2-coverage-scheduler-retired-stream-dispatch
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (clock (lambda () 0))
         (block (h2-header-block (cons ":status" "200")))
         (split (max 1 (floor (length block) 2)))
         (reader (h2-reader-from-frames
                  (h2-frame 9 4 1 (subseq block split)))))
    (setf (http-kit/http2::%http2-connection-scheduler-first-peer-frame-p
           connection) nil
          (http-kit/http2::%http2-connection-session-started-p connection) t
          (http-kit/http2::%http2-connection-next-stream-id connection) 3
          (http-kit/http2::%http2-connection-scheduler-last-opened-stream-id
           connection) 1)
    (http-kit/http2::%h2-scheduler-dispatch-frame
     connection
     (h2-frame-object 1 0 1 (subseq block 0 split))
     reader nil nil clock)
    (ensure-true (http-kit/http2:http2-connection-open-p connection))
    (ensure-false
     (http-kit/http2::%http2-connection-scheduler-entries connection))
    (http-kit/http2::%h2-scheduler-dispatch-frame
     connection (h2-frame-object 0 1 1 (octets 97 98)) reader nil nil clock)
    (let* ((output-reader
             (http-kit/http2::%h2-reader-for (binary-session-output stream)))
           (window-update
             (http-kit/http2::%h2-read-frame output-reader 16384 nil nil)))
      (ensure-equal 8 (http-kit/http2::%h2-frame-type window-update))
      (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id window-update))
      (ensure-equal (octets 0 0 0 2)
                    (http-kit/http2::%h2-frame-payload window-update)))
    (dolist (case '((9 4 :http2-headers 1)
                    (5 4 :http2-read :protocol-error)))
      (destructuring-bind (type flags operation detail) case
        (let ((condition
                (handler-case
                    (http-kit/http2::%h2-scheduler-dispatch-frame
                     connection (h2-frame-object type flags 1 (octets))
                     reader nil nil clock)
                  (http-protocol-error (error) error))))
          (ensure-true (typep condition 'http-protocol-error))
          (ensure-equal operation (http-error-operation condition))
          (ensure-equal detail (http-protocol-error-detail condition)))))
    (let ((condition
            (handler-case
                (http-kit/http2::%h2-scheduler-dispatch-frame
                 connection
                 (h2-frame-object 1 5 3 (h2-header-block (cons ":status" "200")))
                 reader nil nil clock)
              (http-protocol-error (error) error))))
      (ensure-true (typep condition 'http-protocol-error))
      (ensure-equal :http2-read (http-error-operation condition))
      (ensure-equal 3 (http-protocol-error-detail condition)))
    (let ((condition
            (handler-case
                (http-kit/http2::%h2-scheduler-dispatch-frame
                 connection (h2-frame-object 0 1 3 (octets 97))
                 reader nil nil clock)
              (http-protocol-error (error) error))))
      (ensure-true (typep condition 'http-protocol-error))
      (ensure-equal :http2-read (http-error-operation condition))
      (ensure-equal '(0 3) (http-protocol-error-detail condition)))
    (http-kit/http2:close-http2-connection connection)))

#+sbcl
(deftest http2-coverage-scheduler-callback-flow-credit-and-retirement
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (request (make-http-request :method "GET" :uri "https://example.com/body"))
         (entry (http-kit/http2::%make-h2-batch-entry
                 :request request :stream-id 1))
         (clock (lambda () 0))
         (chunks nil)
         (information-statuses nil))
    (setf (http-kit/http2::%http2-connection-scheduler-entries connection)
          (list entry))
    (ensure-true
     (http-kit/http2::%h2-scheduler-queue-callback
      connection entry
      (list :information
            (lambda (response)
              (push (http-response-status response) information-statuses))
            (make-http-response :status 103))))
    (http-kit/http2::%h2-scheduler-run-entry-callbacks connection entry clock)
    (ensure-equal '(103) (nreverse information-statuses))
    (ensure-true
     (http-kit/http2::%h2-scheduler-queue-callback
      connection entry (list :body (lambda (chunk) (push (copy-seq chunk) chunks))
                             (octets 97 98) 2 nil)))
    (ensure-true
     (http-kit/http2::%h2-scheduler-queue-callback
      connection entry (list :body (lambda (chunk) (push (copy-seq chunk) chunks))
                             (octets 99 100 101) 3 nil)))
    (http-kit/http2::%h2-scheduler-run-entry-callbacks connection entry clock)
    (ensure-equal (list (octets 97 98) (octets 99 100 101)) (nreverse chunks))
    (let* ((reader (http-kit/http2::%h2-reader-for (binary-session-output stream)))
           (frames nil))
      (loop for frame = (http-kit/http2::%h2-read-frame reader 16384 nil nil)
            until (eq frame :eof)
            do (push frame frames))
      (setf frames (nreverse frames))
      (ensure-equal '(1 0 1 0)
                    (mapcar #'http-kit/http2::%h2-frame-stream-id frames))
      (ensure-equal '(2 2 3 3)
                    (mapcar (lambda (frame)
                              (http-kit/http2::%h2-u32
                               (http-kit/http2::%h2-frame-payload frame) 0))
                            frames)))
    (let ((callback-error nil))
      (ensure-true
       (http-kit/http2::%h2-scheduler-queue-callback
        connection entry
        (list :body (lambda (chunk)
                      (declare (ignore chunk))
                      (error "Consumer rejected the response chunk."))
              (octets 120) 1 nil)))
      (handler-case
          (http-kit/http2::%h2-scheduler-run-entry-callbacks
           connection entry clock)
        (error (condition) (setf callback-error condition)))
      (ensure-true (typep callback-error 'simple-error))
      (ensure-true (eq callback-error (http-kit/http2::%h2-batch-entry-error entry)))
      (ensure-true (http-kit/http2::%h2-batch-entry-local-retired-p entry))
      (ensure-equal 0 (length (http-kit/http2::%h2-batch-entry-body-vector entry)))
      (ensure-false
       (http-kit/http2::%http2-connection-scheduler-entries connection))
      (ensure-false
       (http-kit/http2::%h2-scheduler-queue-callback
        connection entry (list :body #'identity (octets) 0 nil)))
      (let* ((reader
               (http-kit/http2::%h2-reader-for (binary-session-output stream)))
             (frames nil))
        (loop for frame = (http-kit/http2::%h2-read-frame reader 16384 nil nil)
              until (eq frame :eof)
              do (push frame frames))
        (setf frames (nreverse frames))
        (ensure-equal 6 (length frames))
        (let ((window-update (fifth frames))
              (reset (sixth frames)))
          (ensure-equal 8 (http-kit/http2::%h2-frame-type window-update))
          (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id window-update))
          (ensure-equal (octets 0 0 0 1)
                        (http-kit/http2::%h2-frame-payload window-update))
          (ensure-equal 3 (http-kit/http2::%h2-frame-type reset))
          (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id reset))
          (ensure-equal (octets 0 0 0 8)
                        (http-kit/http2::%h2-frame-payload reset)))))
    (http-kit/http2:close-http2-connection connection)))
