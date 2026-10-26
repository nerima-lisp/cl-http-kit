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
#+sbcl (deftest http2-coverage-response-uses-initial-frames
  (let* ((reads 0)
         (response
           (http-kit/http2::%h2-read-response
            (lambda () (incf reads) :eof)
            (lambda (wire) (declare (ignore wire)))
            16384 nil (lambda () 0) 4096 16 "GET"
            :read-initial-settings-p nil
            :initial-frames
            (list
             (h2-frame-object 1 5 1
                              (h2-header-block
                               (cons ":status" "200")
                               (cons "content-length" "0")))))))
    (ensure-equal 200 (http-response-status response))
    (ensure-equal (octets) (http-response-body response))
    (ensure-equal 0 reads)))

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
    (setf (http-kit/http2::%h2-batch-entry-body-vector entry) (octets 120)
          (http-kit/http2::%h2-batch-entry-body-length entry) 1)
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
      (ensure-equal '(1 1)
                    (mapcar #'http-kit/http2::%h2-frame-stream-id frames))
      (ensure-equal '(2 3)
                    (mapcar (lambda (frame)
                              (http-kit/http2::%h2-u32
                               (http-kit/http2::%h2-frame-payload frame) 0))
                            frames)))
    (let ((callback-called-p nil)
          (before (copy-seq (binary-session-output stream))))
      (ensure-true
       (http-kit/http2::%h2-scheduler-queue-callback
        connection entry
        (list :body (lambda (chunk)
                      (declare (ignore chunk))
                      (setf callback-called-p t))
              (cons 0 1) 1 t t)))
      (http-kit/http2::%h2-scheduler-run-entry-callbacks
       connection entry clock)
      (ensure-true callback-called-p)
      (ensure-equal before (binary-session-output stream)))
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
        (ensure-equal 3 (length frames))
        (let ((reset (third frames)))
          (ensure-equal 3 (http-kit/http2::%h2-frame-type reset))
          (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id reset))
          (ensure-equal (octets 0 0 0 8)
                        (http-kit/http2::%h2-frame-payload reset)))))
    (let* ((entry (http-kit/http2::%make-h2-batch-entry
                   :request request :stream-id 3))
           (callback-error nil)
           (before (copy-seq (binary-session-output stream))))
      (setf (http-kit/http2::%h2-batch-entry-body-vector entry) (octets 120)
            (http-kit/http2::%h2-batch-entry-body-length entry) 1)
      (setf (http-kit/http2::%http2-connection-scheduler-entries connection)
            (list entry))
      (ensure-true
       (http-kit/http2::%h2-scheduler-queue-callback
        connection entry
        (list :body (lambda (chunk)
                      (declare (ignore chunk))
                      (error "Replayed consumer rejected the chunk."))
              (cons 0 1) 1 t t)))
      (handler-case
          (http-kit/http2::%h2-scheduler-run-entry-callbacks
           connection entry clock)
        (error (condition) (setf callback-error condition)))
      (ensure-true (typep callback-error 'simple-error))
      (ensure-true (http-kit/http2::%h2-batch-entry-local-retired-p entry))
      (let* ((reader
               (http-kit/http2::%h2-reader-for (binary-session-output stream)))
             (frames nil))
        (loop for frame = (http-kit/http2::%h2-read-frame reader 16384 nil nil)
              until (eq frame :eof)
              do (push frame frames))
        (setf frames (nreverse frames))
        (ensure-equal
         (concatenate-octets before
                             (http-kit/http2::%h2-frame-wire
                              3 0 3 (octets 0 0 0 8)))
         (binary-session-output stream))))
    (http-kit/http2:close-http2-connection connection)))
#+sbcl
(deftest http2-coverage-scheduler-zero-pending-callback-counts
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (entry (http-kit/http2::%make-h2-batch-entry :stream-id 1))
         (clock (lambda () 0))
         (information-statuses nil)
         (body-chunks nil))
    (setf (http-kit/http2::%h2-batch-entry-response-headers-callback-complete-p
           entry) t
          (http-kit/http2::%h2-batch-entry-body-vector entry) (octets 120)
          (http-kit/http2::%h2-batch-entry-body-length entry) 1)
    (ensure-true
     (http-kit/http2::%h2-scheduler-queue-callback
      connection entry
      (list :information
            (lambda (response)
              (push (http-response-status response) information-statuses))
            (make-http-response :status 103))))
    (setf (http-kit/http2::%h2-batch-entry-pending-information-callback-count
           entry) 0)
    (http-kit/http2::%h2-scheduler-run-entry-callbacks
     connection entry clock)
    (ensure-equal '(103) (nreverse information-statuses))
    (ensure-equal 0
                  (http-kit/http2::%h2-batch-entry-pending-information-callback-count
                   entry))
    (http-kit/http2::%h2-with-scheduler-lock (connection)
      (http-kit/http2::%h2-scheduler-enqueue-body-callback-locked
       entry
       (lambda (chunk) (push (copy-seq chunk) body-chunks))
       (octets 97) 1 t t)
      (setf (http-kit/http2::%h2-batch-entry-pending-body-callback-count entry) 0))
    (http-kit/http2::%h2-scheduler-run-entry-callbacks
     connection entry clock)
    (ensure-equal (list (octets 120)) (nreverse body-chunks))
    (ensure-equal 0
                  (http-kit/http2::%h2-batch-entry-pending-body-callback-count
                   entry))
    (http-kit/http2:close-http2-connection connection)))
#+sbcl
(deftest http2-coverage-scheduler-retirement-and-control-state
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (request (make-http-request :method "GET" :uri "https://example.com/state"))
         (clock (lambda () 0))
         (entry (http-kit/http2::%make-h2-batch-entry
                 :request request :stream-id 1))
         (existing-error
           (make-condition 'simple-error :format-control "Existing entry error."))
         (replacement-error
           (make-condition 'simple-error :format-control "Replacement error.")))
    (setf (http-kit/http2::%http2-connection-scheduler-first-peer-frame-p
           connection) nil
          (http-kit/http2::%http2-connection-next-stream-id connection) 3
          (http-kit/http2::%http2-connection-scheduler-last-opened-stream-id
           connection) 1
          (http-kit/http2::%http2-connection-scheduler-entries connection)
          (list entry)
          (http-kit/http2::%h2-batch-entry-error entry) existing-error
          (http-kit/http2::%h2-batch-entry-callback-events entry)
          (list (list :body #'identity (octets 97 98) 2 nil)))
    (http-kit/http2::%h2-scheduler-retire-entry
     connection entry replacement-error clock)
    (ensure-true (eq existing-error (http-kit/http2::%h2-batch-entry-error entry)))
    (ensure-true (http-kit/http2::%h2-batch-entry-local-retired-p entry))
    (ensure-false (http-kit/http2::%http2-connection-scheduler-entries connection))
    (let ((expected
            (http-kit/http2::%h2-frame-wire 3 0 1 (octets 0 0 0 8))))
      (ensure-equal expected (binary-session-output stream)))
    (let* ((entry (http-kit/http2::%make-h2-batch-entry
                   :request request :stream-id 3))
           (callback-called-p nil)
           (before (copy-seq (binary-session-output stream))))
      (setf (http-kit/http2::%h2-batch-entry-error entry) existing-error
            (http-kit/http2::%h2-batch-entry-body-vector entry)
            (octets 100 101)
            (http-kit/http2::%h2-batch-entry-body-length entry) 2
            (http-kit/http2::%h2-batch-entry-callback-events entry)
            (list (list :body (lambda (chunk)
                                (declare (ignore chunk))
                                (setf callback-called-p t))
                        (octets 99) 1 t nil)
                  (list :body (lambda (chunk)
                                (declare (ignore chunk))
                                (setf callback-called-p t))
                        (cons 0 2) 2 t t)))
      (http-kit/http2::%h2-scheduler-run-entry-callbacks
       connection entry clock)
      (ensure-false callback-called-p)
      (ensure-false (http-kit/http2::%h2-batch-entry-callback-events entry))
      (ensure-equal before (binary-session-output stream)))
    (let* ((entry (http-kit/http2::%make-h2-batch-entry
                   :request request :stream-id 5))
           (callback-error nil))
      (setf (http-kit/http2::%http2-connection-scheduler-entries connection)
            (list entry))
      (http-kit/http2::%h2-scheduler-queue-callback
       connection entry
       (list :information (lambda (response)
                            (declare (ignore response))
                            (error "Information callback failed."))
             (make-http-response :status 103)))
      (handler-case
          (http-kit/http2::%h2-scheduler-run-entry-callbacks
           connection entry clock)
        (error (condition) (setf callback-error condition)))
      (ensure-true (typep callback-error 'simple-error))
      (ensure-true (http-kit/http2::%h2-batch-entry-local-retired-p entry)))
    (let* ((failure (make-condition 'simple-error
                                    :format-control "Control write failed."))
           (failing-stream
             (make-instance 'h2-control-write-failure-stream
                            :input (octets) :failure failure))
           (failing-connection
             (http-kit/http2:make-http2-connection :stream failing-stream))
           (entry (http-kit/http2::%make-h2-batch-entry
                   :request request :stream-id 1))
           (observed nil))
      (setf (http-kit/http2::%http2-connection-scheduler-entries failing-connection)
            (list entry)
            (h2-control-write-failure-armed-p failing-stream) t)
      (handler-case
          (http-kit/http2::%h2-scheduler-retire-entry
           failing-connection entry replacement-error clock)
        (error (condition) (setf observed condition)))
      (ensure-true (eq failure observed))
      (ensure-false (http-kit/http2:http2-connection-open-p failing-connection))
      (http-kit/http2:close-http2-connection failing-connection))
    (let* ((entry (http-kit/http2::%make-h2-batch-entry
                   :request request :stream-id 1))
           (response (make-http-response :status 200))
           (retirement-error
             (make-condition 'simple-error
                             :format-control "Retired entry response.")))
      (setf (http-kit/http2::%http2-connection-scheduler-entries connection)
            (list entry))
      (http-kit/http2::%h2-scheduler-retire-entry
       connection entry retirement-error clock)
      (http-kit/http2::%h2-scheduler-mark-response connection entry response)
      (ensure-false (http-kit/http2::%h2-batch-entry-response entry))
      (ensure-false
       (http-kit/http2::%http2-connection-scheduler-entries connection)))
    (setf (http-kit/http2::%http2-connection-goaway-last-stream-id connection) 1
          (http-kit/http2::%http2-connection-scheduler-entries connection) nil)
    (http-kit/http2::%h2-scheduler-close-goaway-retired-if-idle connection)
    (ensure-false (http-kit/http2:http2-connection-open-p connection))
    (http-kit/http2:close-http2-connection connection))
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (request (make-http-request :method "GET" :uri "https://example.com/control"))
         (clock (lambda () 0))
         (entry (http-kit/http2::%make-h2-batch-entry
                 :request request :stream-id 1)))
    (setf (http-kit/http2::%http2-connection-scheduler-first-peer-frame-p
           connection) nil
          (http-kit/http2::%http2-connection-next-stream-id connection) 3
          (http-kit/http2::%http2-connection-scheduler-last-opened-stream-id
           connection) 1
          (http-kit/http2::%http2-connection-scheduler-entries connection)
          (list entry))
    (labels ((control (frame)
               (http-kit/http2::%h2-scheduler-control-frame connection frame nil))
             (protocol-error (frame)
               (handler-case (progn (control frame) nil)
                 (http-protocol-error (condition) condition))))
      (let* ((failure (make-condition 'simple-error
                                      :format-control "Ping waiter failed."))
             (waiter
               (http-kit/http2::%make-h2-scheduler-ping-waiter
                (octets 1 2 3 4 5 6 7 8))))
        (setf (http-kit/http2::%http2-connection-scheduler-ping-waiters
               connection) (list waiter))
        (ensure-equal :http2-client
                      (http-error-operation
                       (protocol-error
                        (h2-frame-object 6 0 1 (octets 1 2 3 4 5 6 7 8)))))
        (setf (http-kit/http2::%h2-scheduler-ping-waiter-failure waiter) failure)
        (ensure-equal :http2-control
                      (http-error-operation
                       (protocol-error
                        (h2-frame-object 6 0 1 (octets 1 2 3 4 5 6 7 8)))))
        (setf (http-kit/http2::%h2-scheduler-ping-waiter-failure waiter) nil
              (http-kit/http2::%h2-scheduler-ping-waiter-acknowledged-p waiter) t)
        (control (h2-frame-object 6 1 0 (octets 1 2 3 4 5 6 7 8))))
      (dolist (case (list (list (h2-frame-object 3 0 0 (octets)) '(0 0))
                          (list (h2-frame-object 7 0 1
                                                 (octets 0 0 0 1 0 0 0 0))
                                '(1 8))
                          (list (h2-frame-object 7 0 0
                                                 (octets 128 0 0 0 0 0 0 0))
                                #x80000000)))
        (let ((failure (protocol-error (first case))))
          (ensure-true (typep failure 'http-protocol-error))
          (ensure-equal (second case) (http-protocol-error-detail failure))))
      (setf (http-kit/http2::%h2-batch-entry-error entry)
            (make-condition 'simple-error :format-control "Keep this error.")
            (http-kit/http2::%http2-connection-goaway-last-stream-id connection) nil)
      (control (h2-frame-object 7 0 0 (octets 0 0 0 0 0 0 0 0)))
      (ensure-true (http-kit/http2::%h2-batch-entry-error entry))
      (setf (http-kit/http2::%h2-batch-entry-error entry) nil
            (http-kit/http2::%h2-batch-entry-local-retired-p entry) t
            (http-kit/http2::%http2-connection-goaway-last-stream-id connection) nil)
      (control (h2-frame-object 7 0 0 (octets 0 0 0 0 0 0 0 0)))
      (ensure-false (http-kit/http2::%http2-connection-scheduler-entries connection))
      (setf (http-kit/http2::%http2-connection-scheduler-entries connection)
            (list entry)
            (http-kit/http2::%h2-batch-entry-error entry)
            (make-condition 'simple-error :format-control "Already reset.")
            (http-kit/http2::%h2-batch-entry-local-retired-p entry) t
            (http-kit/http2::%http2-connection-goaway-last-stream-id connection) nil)
      (control (h2-frame-object 3 0 1 (octets 0 0 0 8)))
      (ensure-false (http-kit/http2::%http2-connection-scheduler-entries connection))
      (ensure-false (http-kit/http2::%h2-scheduler-previously-opened-client-stream-p
                     connection 2))
      (control (h2-frame-object 2 0 1 (octets 0 0 0 0 0)))
      (http-kit/http2::%h2-scheduler-dispatch-frame
       connection (h2-frame-object 255 0 0 (octets)) nil nil nil clock))
    (http-kit/http2:close-http2-connection connection)))
#+sbcl
(deftest http2-coverage-scheduler-active-stream-frame-states
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (request (make-http-request :method "GET" :uri "https://example.com/active"))
         (entry (http-kit/http2::%make-h2-batch-entry
                 :request request :stream-id 1))
         (clock (lambda () 0))
         (information nil))
    (setf (http-kit/http2::%http2-connection-scheduler-first-peer-frame-p
           connection) nil
          (http-kit/http2::%http2-connection-session-started-p connection) t
          (http-kit/http2::%http2-connection-next-stream-id connection) 3
          (http-kit/http2::%http2-connection-scheduler-last-opened-stream-id
           connection) 1
          (http-kit/http2::%http2-connection-scheduler-entries connection)
          (list entry)
          (http-kit/http2::%h2-batch-entry-on-information entry)
          (lambda (response)
            (push (http-response-status response) information)))
    (let* ((block (h2-header-block (cons ":status" "103")))
           (split (max 1 (floor (length block) 2)))
           (reader (h2-reader-from-frames
                    (h2-frame 9 4 1 (subseq block split)))))
      (http-kit/http2::%h2-scheduler-dispatch-frame
       connection (h2-frame-object 1 0 1 (subseq block 0 split))
       reader nil nil clock))
    (ensure-equal '(103)
                  (mapcar (lambda (event)
                            (http-response-status (third event)))
                          (http-kit/http2::%h2-batch-entry-callback-events entry)))
    (http-kit/http2::%h2-scheduler-run-entry-callbacks connection entry clock)
    (ensure-equal '(103) information)
    (http-kit/http2::%h2-scheduler-dispatch-frame
     connection
     (h2-frame-object
      1 4 1 (h2-header-block (cons ":status" "200")
                             (cons "content-length" "2")))
     nil nil nil clock)
    (http-kit/http2::%h2-scheduler-dispatch-frame
     connection (h2-frame-object 0 0 1 (octets)) nil nil nil clock)
    (http-kit/http2::%h2-scheduler-dispatch-frame
     connection (h2-frame-object 0 1 1 (octets 97 98)) nil nil nil clock)
    (ensure-true (http-kit/http2::%h2-batch-entry-response entry))
    (ensure-equal (octets 97 98)
                  (http-response-body (http-kit/http2::%h2-batch-entry-response entry)))
    (dolist (frame (list
                    (h2-frame-object
                     1 4 1 (h2-header-block (cons ":status" "204")))
                    (h2-frame-object 0 1 1 (octets 99))))
      (let ((failure
              (handler-case
                  (progn
                    (http-kit/http2::%h2-scheduler-dispatch-frame
                     connection frame nil nil nil clock)
                    nil)
                (http-protocol-error (condition) condition))))
        (ensure-true (typep failure 'http-protocol-error))
        (ensure-equal :http2-read (http-error-operation failure))))
    (let* ((before-headers
             (http-kit/http2::%make-h2-batch-entry
              :request request :stream-id 3)))
      (setf (http-kit/http2::%http2-connection-next-stream-id connection) 5
            (http-kit/http2::%http2-connection-scheduler-last-opened-stream-id
             connection) 3
            (http-kit/http2::%http2-connection-scheduler-entries connection)
            (list before-headers))
      (let ((failure
              (handler-case
                  (progn
                    (http-kit/http2::%h2-scheduler-dispatch-frame
                     connection (h2-frame-object 0 0 3 (octets 99))
                     nil nil nil clock)
                    nil)
                (http-protocol-error (condition) condition))))
        (ensure-true (typep failure 'http-protocol-error))
        (ensure-equal :data-before-headers
                      (http-kit:http-protocol-error-detail failure))))
    (let* ((no-body
             (http-kit/http2::%make-h2-batch-entry
              :request request :stream-id 5)))
      (setf (http-kit/http2::%http2-connection-next-stream-id connection) 7
            (http-kit/http2::%http2-connection-scheduler-last-opened-stream-id
             connection) 5
            (http-kit/http2::%http2-connection-scheduler-entries connection)
            (list no-body)
            (http-kit/http2::%h2-batch-entry-status no-body) 204)
      (let ((failure
              (handler-case
                  (progn
                    (http-kit/http2::%h2-scheduler-dispatch-frame
                     connection (h2-frame-object 0 0 5 (octets 99))
                     nil nil nil clock)
                    nil)
                (http-protocol-error (condition) condition))))
        (ensure-true (typep failure 'http-protocol-error))
        (ensure-equal :http2-read (http-error-operation failure))
        (ensure-equal '("GET" 204)
                      (http-kit:http-protocol-error-detail failure))))
    (let* ((retired (http-kit/http2::%make-h2-batch-entry
                     :request request :stream-id 3)))
      (setf (http-kit/http2::%http2-connection-next-stream-id connection) 5
            (http-kit/http2::%http2-connection-scheduler-last-opened-stream-id
             connection) 3
            (http-kit/http2::%http2-connection-scheduler-entries connection)
            (list retired)
            (http-kit/http2::%h2-batch-entry-local-retired-p retired) t)
      (http-kit/http2::%h2-scheduler-dispatch-frame
       connection (h2-frame-object 0 0 3 (octets)) nil nil nil clock)
      (http-kit/http2::%h2-scheduler-dispatch-frame
       connection (h2-frame-object 0 1 3 (octets 120)) nil nil nil clock)
      (ensure-false
       (http-kit/http2::%http2-connection-scheduler-entries connection)))
    (let* ((errored (http-kit/http2::%make-h2-batch-entry
                     :request request :stream-id 5)))
      (setf (http-kit/http2::%http2-connection-next-stream-id connection) 7
            (http-kit/http2::%http2-connection-scheduler-last-opened-stream-id
             connection) 5
            (http-kit/http2::%http2-connection-scheduler-entries connection)
            (list errored)
            (http-kit/http2::%h2-batch-entry-status errored) 200
            (http-kit/http2::%h2-batch-entry-error errored)
            (make-condition 'simple-error :format-control "Retired response."))
      (vector-push-extend 97
                          (http-kit/http2::%h2-batch-entry-body-vector errored))
      (http-kit/http2::%h2-scheduler-dispatch-frame
       connection (h2-frame-object 0 0 5 (octets 98)) nil nil nil clock)
      (http-kit/http2::%h2-scheduler-dispatch-frame
       connection (h2-frame-object 0 0 5 (octets)) nil nil nil clock)
      (ensure-equal 0
                    (length (http-kit/http2::%h2-batch-entry-body-vector errored))))
    (setf (http-kit/http2::%http2-connection-closed-p connection) t)
    (http-kit/http2::%h2-scheduler-dispatch-frame
     connection (h2-frame-object 0 0 5 (octets 100)) nil nil nil clock)
    (http-kit/http2:close-http2-connection connection))
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (request (make-http-request :method "GET" :uri "https://example.com/closed"))
         (entry (http-kit/http2::%make-h2-batch-entry
                 :request request :stream-id 1))
         (clock (lambda () 0)))
    (setf (http-kit/http2::%http2-connection-scheduler-first-peer-frame-p
           connection) nil
          (http-kit/http2::%http2-connection-closed-p connection) t
          (http-kit/http2::%http2-connection-scheduler-entries connection)
          (list entry)
          (http-kit/http2::%h2-batch-entry-status entry) 200)
    (http-kit/http2::%h2-scheduler-dispatch-frame
     connection (h2-frame-object 0 0 1 (octets 97)) nil nil nil clock)
    (ensure-equal 0 (length (binary-session-output stream)))
    (http-kit/http2:close-http2-connection connection))
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (request (make-http-request :method "GET" :uri "https://example.com/retired"))
         (clock (lambda () 0))
         (waiter
           (http-kit/http2::%make-h2-scheduler-ping-waiter
            (octets 1 2 3 4 5 6 7 8)))
         (failure (make-condition 'simple-error :format-control "Ping failed.")))
    (setf (http-kit/http2::%http2-connection-scheduler-first-peer-frame-p
           connection) nil
          (http-kit/http2::%http2-connection-next-stream-id connection) 3
          (http-kit/http2::%http2-connection-scheduler-last-opened-stream-id
           connection) 1
          (http-kit/http2::%http2-connection-scheduler-ping-waiters connection)
          (list waiter))
    (let ((condition
            (handler-case
                (progn
                  (http-kit/http2::%h2-scheduler-dispatch-frame
                   connection (h2-frame-object 0 0 1 (octets 97))
                   nil nil nil clock)
                  nil)
              (http-protocol-error (observed) observed))))
      (ensure-equal :http2-client (http-error-operation condition)))
    (setf (http-kit/http2::%h2-scheduler-ping-waiter-failure waiter) failure)
    (http-kit/http2::%h2-scheduler-dispatch-frame
     connection (h2-frame-object 0 0 1 (octets 97)) nil nil nil clock)
    (let ((entry (http-kit/http2::%make-h2-batch-entry
                  :request request :stream-id 3)))
      (setf (http-kit/http2::%http2-connection-next-stream-id connection) 5
            (http-kit/http2::%http2-connection-scheduler-last-opened-stream-id
             connection) 3
            (http-kit/http2::%http2-connection-scheduler-entries connection)
            (list entry)
            (http-kit/http2::%h2-scheduler-ping-waiter-failure waiter) nil)
      (setf (http-kit/http2::%h2-batch-entry-error entry) failure)
      (let ((condition
              (handler-case
                  (progn
                    (http-kit/http2::%h2-scheduler-dispatch-frame
                     connection (h2-frame-object 0 0 1 (octets 97))
                     nil nil nil clock)
                    nil)
                (http-protocol-error (observed) observed))))
        (ensure-equal :http2-client (http-error-operation condition)))
      (setf (http-kit/http2::%h2-batch-entry-error entry) nil)
      (http-kit/http2::%h2-scheduler-dispatch-frame
       connection (h2-frame-object 0 0 1 (octets 97)) nil nil nil clock)
      (setf (http-kit/http2::%h2-scheduler-ping-waiter-acknowledged-p waiter) t)
      (http-kit/http2::%h2-scheduler-dispatch-frame
       connection (h2-frame-object 0 0 1 (octets 97)) nil nil nil clock))
    (ensure-true (plusp (length (binary-session-output stream))))
    (http-kit/http2:close-http2-connection connection)))

#+sbcl
(deftest http2-coverage-retired-data-credit-closed-connection
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (clock (lambda () 0)))
    (setf (http-kit/http2::%http2-connection-scheduler-first-peer-frame-p
           connection) nil
          (http-kit/http2::%http2-connection-next-stream-id connection) 3
          (http-kit/http2::%http2-connection-scheduler-last-opened-stream-id
           connection) 1)
    (http-kit/http2::%h2-scheduler-dispatch-frame
     connection (h2-frame-object 0 0 1 (octets 97)) nil nil nil clock)
    (ensure-true (plusp (length (binary-session-output stream))))
    (setf (http-kit/http2::%http2-connection-closed-p connection) t)
    (let ((output-length (length (binary-session-output stream))))
      (http-kit/http2::%h2-scheduler-dispatch-frame
       connection (h2-frame-object 0 0 1 (octets 98)) nil nil nil clock)
      (ensure-equal output-length (length (binary-session-output stream))))
    (http-kit/http2:close-http2-connection connection)))

#+sbcl
(deftest http2-coverage-publish-response-rejects-errored-entry
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (entry (http-kit/http2::%make-h2-batch-entry :stream-id 1))
         (response (list :pending))
         (failure (make-condition 'simple-error :format-control "Retired.")))
    (setf (http-kit/http2::%h2-batch-entry-error entry) failure
          (http-kit/http2::%h2-batch-entry-response-complete-p entry) t
          (http-kit/http2::%h2-batch-entry-response-headers-callback-complete-p
           entry) t
          (http-kit/http2::%h2-batch-entry-pending-response entry) response)
    (ensure-false
     (http-kit/http2::%h2-scheduler-publish-response-if-ready connection entry))
    (ensure-false (http-kit/http2::%h2-batch-entry-response entry))
    (ensure-equal response
                  (http-kit/http2::%h2-batch-entry-pending-response entry))
    (http-kit/http2:close-http2-connection connection)))

#+sbcl
(deftest http2-coverage-dropped-body-callbacks-after-retirement
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (entry (http-kit/http2::%make-h2-batch-entry :stream-id 1))
         (callback-count 0)
         (failure (make-condition 'simple-error :format-control "Retired."))
         (clock (lambda () 0)))
    (setf (http-kit/http2::%http2-connection-next-stream-id connection) 3
          (http-kit/http2::%http2-connection-scheduler-entries connection)
          (list entry)
          (http-kit/http2::%h2-batch-entry-response-headers-callback-complete-p
           entry) t
          (http-kit/http2::%h2-batch-entry-pending-body-callback-count entry) 1
          (http-kit/http2::%h2-batch-entry-error entry) failure
          (http-kit/http2::%h2-batch-entry-callback-events entry)
          (list (list :body (lambda (chunk)
                              (declare (ignore chunk))
                              (incf callback-count))
                      (octets 97) 1 nil nil)))
    (http-kit/http2::%h2-scheduler-run-entry-callbacks
     connection entry clock)
    (ensure-equal 0 callback-count)
    (ensure-equal 0
                  (http-kit/http2::%h2-batch-entry-pending-body-callback-count
                   entry))
    (ensure-equal 0 (length (binary-session-output stream)))
    (setf (http-kit/http2::%h2-batch-entry-callback-events entry)
          (list (list :body (lambda (chunk)
                              (declare (ignore chunk))
                              (incf callback-count))
                      (octets 98) 1 nil nil)))
    (http-kit/http2::%h2-scheduler-run-entry-callbacks
     connection entry clock)
    (ensure-equal 0 callback-count)
    (ensure-equal 0
                  (http-kit/http2::%h2-batch-entry-pending-body-callback-count
                   entry))
    (http-kit/http2:close-http2-connection connection)))

#+sbcl
(deftest http2-body-callback-cancel-clears-pending-count
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (entry (http-kit/http2::%make-h2-batch-entry :stream-id 1))
         (callback-count 0)
         (clock (lambda () 0)))
    (setf (http-kit/http2::%http2-connection-next-stream-id connection) 3
          (http-kit/http2::%http2-connection-scheduler-entries connection)
          (list entry)
          (http-kit/http2::%h2-batch-entry-response-headers-callback-complete-p
           entry) t
          (http-kit/http2::%h2-batch-entry-pending-body-callback-count entry) 1
          (http-kit/http2::%h2-batch-entry-callback-events entry)
          (list (list :body (lambda (chunk)
                              (declare (ignore chunk))
                              (incf callback-count)
                              (http-kit/http2:cancel-http2-stream connection 1))
                      (octets 97) 1 nil nil)))
    (write-sequence (h2-preface) stream)
    (http-kit/http2::%h2-scheduler-run-entry-callbacks
     connection entry clock)
    (ensure-equal 1 callback-count)
    (ensure-equal 0
                  (http-kit/http2::%h2-batch-entry-pending-body-callback-count
                   entry))
    (ensure-true
     (find-if (lambda (frame)
                (and (= 3 (http-kit/http2::%h2-frame-type frame))
                     (= 1 (http-kit/http2::%h2-frame-stream-id frame))))
              (h2-output-frames stream)))
    (ensure-false
     (find-if (lambda (frame)
                (and (= 8 (http-kit/http2::%h2-frame-type frame))
                     (= 1 (http-kit/http2::%h2-frame-stream-id frame))))
              (h2-output-frames stream)))
    (http-kit/http2:close-http2-connection connection)))
#+sbcl
(deftest http2-coverage-serial-fallback-boundaries
  (let* ((request (make-http-request :method "GET" :uri "https://example.com/serial"))
         (stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream)))
    (let ((failure
            (handler-case
                (http-kit/http2::%h2-send-http2-request-over-connection-serial
                 :invalid request)
              (http-protocol-error (condition) condition))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :http2-client (http-error-operation failure)))
    (dolist (options '((:on-body-chunk :invalid)
                       (:on-information :invalid)
                       (:on-stream-open :invalid)
                       (:collect-body-p :invalid)))
      (let ((failure
              (handler-case
                  (apply #'http-kit/http2::%h2-send-http2-request-over-connection-serial
                         connection request options)
                (http-protocol-error (condition) condition))))
        (ensure-true (typep failure 'http-protocol-error))
        (ensure-equal :http2-client (http-error-operation failure))))
    (ensure-equal 0 (length (binary-session-output stream)))
    (let* ((closed-stream (make-instance 'binary-session-stream :input (octets)))
           (closed-connection
             (http-kit/http2:make-http2-connection :stream closed-stream)))
      (http-kit/http2:close-http2-connection closed-connection)
      (let ((failure
              (handler-case
                  (http-kit/http2::%h2-send-http2-request-over-connection-serial
                   closed-connection request)
                (http-connection-error (condition) condition))))
        (ensure-true (typep failure 'http-connection-error))
        (ensure-equal :closed (http-connection-error-cause failure))))
    (http-kit/http2:close-http2-connection connection))
  (let* ((input
           (concatenate-octets
            (h2-frame 4 0 0 (octets))
            (h2-frame 1 4 1
                      (h2-header-block (cons ":status" "200")
                                       (cons "content-length" "2")))
            (h2-frame 0 1 1 (octets 97 98))))
         (stream (make-instance 'binary-session-stream :input input))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (opened nil)
         (chunks nil)
         (request
           (make-http-request :method "POST" :uri "https://example.com/upload"
                              :body (octets 120 121)))
         (response
           (http-kit/http2::%h2-send-http2-request-over-connection-serial
            connection request
            :collect-body-p nil
            :on-body-chunk (lambda (chunk) (push (copy-seq chunk) chunks))
            :on-stream-open
            (lambda (stream-id cancel)
              (declare (ignore cancel))
              (setf opened stream-id)))))
    (ensure-equal 1 opened)
    (ensure-equal 200 (http-response-status response))
    (ensure-equal (octets) (http-response-body response))
    (ensure-equal (list (octets 97 98)) chunks)
    (let* ((frames (h2-output-frames stream))
           (data (find 0 frames :key #'http-kit/http2::%h2-frame-type)))
      (ensure-true data)
      (ensure-equal (octets 120 121)
                    (http-kit/http2::%h2-frame-payload data)))
    (http-kit/http2:close-http2-connection connection))
  (let* ((input
           (concatenate-octets
            (h2-frame 4 0 0 (octets))
            (h2-frame 1 4 1
                      (h2-header-block (cons ":status" "200")
                                       (cons "content-length" "2")))
            (h2-frame 0 1 1 (octets 97 98))))
         (stream (make-instance 'binary-session-stream :input input))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (opened nil)
         (failure nil)
         (request
           (make-http-request :method "POST" :uri "https://example.com/upload"
                              :body (octets 120 121))))
    (unwind-protect
         (progn
           (handler-case
               (http-kit/http2::%h2-send-http2-request-over-connection-serial
                connection request
                :on-stream-open
                (lambda (stream-id cancel)
                  (setf opened stream-id)
                  (funcall cancel)))
             (http-connection-error (condition)
               (setf failure condition)))
           (ensure-equal 1 opened)
           (ensure-true (typep failure 'http-connection-error))
           (ensure-equal '(:local-cancel 1 8)
                         (http-connection-error-cause failure))
           (let* ((frames (h2-output-frames stream))
                  (reset (find 3 frames :key #'http-kit/http2::%h2-frame-type))
                  (data (find 0 frames :key #'http-kit/http2::%h2-frame-type)))
             (ensure-true reset)
             (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id reset))
             (ensure-equal (octets 0 0 0 8)
                           (http-kit/http2::%h2-frame-payload reset))
             (ensure-false data)))
      (http-kit/http2:close-http2-connection connection)))
  (let* ((stream
           (make-instance 'binary-session-stream
                          :input (h2-frame 6 0 0 (octets 1 2 3 4 5 6 7 8))))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (reader (http-kit/http2::%h2-reader-for stream))
         (writer (http-kit/http2::%h2-writer stream nil (lambda () 0)))
         (failure nil))
    (setf (http-kit/http2::%http2-connection-peer-connection-window-size
           connection) 0)
    (handler-case
        (http-kit/http2::%h2-connection-send-request-body
         connection reader writer 1 (octets 97) nil nil nil 16384 nil nil
         (lambda () 0) t nil)
      (http-protocol-error (condition) (setf failure condition)))
    (ensure-true (typep failure 'http-protocol-error))
    (ensure-equal :http2-read (http-error-operation failure))
    (http-kit/http2:close-http2-connection connection))
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (update (h2-frame-object 8 0 1 (octets 0 0 0 2))))
    (http-kit/http2::%h2-connection-note-window-update connection update 1)
    (ensure-equal 65537
                  (cdr (assoc 1
                              (http-kit/http2::%http2-connection-peer-stream-windows
                               connection))))
    (setf (http-kit/http2::%http2-connection-peer-stream-windows connection) nil)
    (http-kit/http2::%h2-connection-note-window-update connection update 3)
    (ensure-false
     (assoc 1 (http-kit/http2::%http2-connection-peer-stream-windows connection)))
    (http-kit/http2:close-http2-connection connection))
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (writer (http-kit/http2::%h2-writer stream nil (lambda () 0)))
         (condition nil))
    (handler-case
        (http-kit/http2::%h2-connection-control-handler
         connection (h2-frame-object 7 0 0
                                     (octets 0 0 0 1 0 0 0 0))
         writer 3)
      (http-connection-error (observed) (setf condition observed)))
    (ensure-equal '(:goaway 1 0) (http-connection-error-cause condition))
    (http-kit/http2::%h2-connection-control-handler
     connection (h2-frame-object 2 0 1 (octets 0 0 0 0 0)) writer 1)
    (http-kit/http2:close-http2-connection connection))
  (let* ((stream
           (make-instance 'binary-session-stream
                          :input (h2-frame 4 0 0 (octets 0 3 0 0 0 1))))
         (connection (http-kit/http2:make-http2-connection :stream stream)))
    (setf (http-kit/http2::%http2-connection-peer-max-concurrent-streams
           connection) 0)
    (ensure-equal 1
                  (http-kit/http2::%h2-connection-wait-for-stream-capacity
                   connection (http-kit/http2::%h2-reader-for stream)
                   (http-kit/http2::%h2-writer stream nil (lambda () 0))
                   nil (lambda () 0)))
    (http-kit/http2:close-http2-connection connection))
  (dolist (input (list (octets)
                       (h2-frame 1 4 1
                                 (h2-header-block (cons ":status" "200")))))
    (let* ((stream (make-instance 'binary-session-stream :input input))
           (connection (http-kit/http2:make-http2-connection :stream stream))
           (failure nil))
      (setf (http-kit/http2::%http2-connection-peer-max-concurrent-streams
             connection) 0)
      (handler-case
          (http-kit/http2::%h2-connection-wait-for-stream-capacity
           connection (http-kit/http2::%h2-reader-for stream)
           (http-kit/http2::%h2-writer stream nil (lambda () 0))
           nil (lambda () 0))
        (http-error (condition) (setf failure condition)))
      (ensure-true (typep failure 'http-error))
      (http-kit/http2:close-http2-connection connection)))
  (let* ((stream
           (make-instance 'binary-session-stream
                          :input (h2-frame 7 0 0
                                           (octets 0 0 0 1 0 0 0 0))))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (failure nil))
    (setf (http-kit/http2::%http2-connection-peer-max-concurrent-streams
           connection) 0)
    (handler-case
        (http-kit/http2::%h2-connection-wait-for-stream-capacity
         connection (http-kit/http2::%h2-reader-for stream)
         (http-kit/http2::%h2-writer stream nil (lambda () 0))
         nil (lambda () 0))
      (http-connection-error (condition) (setf failure condition)))
    (ensure-equal '(:goaway 1 1) (http-connection-error-cause failure))
    (http-kit/http2:close-http2-connection connection))
  (let* ((stream
           (make-instance 'binary-session-stream
                          :input (h2-frame 4 0 0 (octets))))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (request (make-http-request :method "CONNECT" :protocol "websocket"
                                    :uri "https://example.com/chat"))
         (failure nil))
    (handler-case
        (http-kit/http2::%h2-send-http2-request-over-connection-serial
         connection request)
      (http-protocol-error (condition) (setf failure condition)))
    (ensure-true (typep failure 'http-protocol-error))
    (ensure-equal :http2-write (http-error-operation failure))
    (ensure-false (http-kit/http2:http2-connection-open-p connection))
    (ensure-false (open-stream-p stream))
    (http-kit/http2:close-http2-connection connection)))
#+sbcl
(deftest http2-coverage-scheduler-upload-and-response-body
  (let* ((input
           (concatenate-octets
            (h2-frame 4 0 0 (octets))
            (h2-frame 1 4 1
                      (h2-header-block (cons ":status" "200")
                                       (cons "content-length" "2")))
            (h2-frame 0 1 1 (octets 111 107))))
         (stream (make-instance 'binary-session-stream :input input))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (request (make-http-request :method "POST"
                                     :uri "https://example.com/scheduled-upload"))
         (body-chunks (list (octets 117 112)))
         (received-chunks nil))
    (unwind-protect
         (let ((response
                 (http-kit/http2:send-http2-request-over-connection
                  connection request
                  :request-body-function
                  (lambda (maximum-size)
                    (ensure-true (>= maximum-size 2))
                    (pop body-chunks))
                  :request-body-length 2
                  :on-body-chunk
                  (lambda (payload)
                    (push (copy-seq payload) received-chunks)))))
           (ensure-false body-chunks)
           (ensure-equal 200 (http-response-status response))
           (ensure-equal (octets 111 107) (http-response-body response))
           (ensure-equal (list (octets 111 107)) (nreverse received-chunks))
           (let* ((frames (h2-output-frames stream))
                  (reset (find 3 frames :key #'http-kit/http2::%h2-frame-type))
                  (data (find 0 frames :key #'http-kit/http2::%h2-frame-type)))
             (ensure-false reset)
             (ensure-true data)
             (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id data))
             (ensure-equal (octets 117 112)
                           (http-kit/http2::%h2-frame-payload data))
             (ensure-equal 1
                           (logand 1 (http-kit/http2::%h2-frame-flags data)))
             (ensure-equal 1
                           (count 0 frames
                                  :key #'http-kit/http2::%h2-frame-type))))
      (http-kit/http2:close-http2-connection connection))))
#+sbcl
(deftest http2-coverage-scheduler-cancel-retires-slot-and-wakes-capacity-waiter
  (let* ((input
           (concatenate-octets
            (h2-frame 4 0 0 (octets 0 3 0 0 0 1))
            (h2-frame 1 5 1
                      (h2-header-block (cons ":status" "200")
                                       (cons "content-length" "0")))))
         (stream (make-instance 'binary-session-stream :input input))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (wait-symbol 'http-kit/http2::%h2-scheduler-condition-wait)
         (original-wait (symbol-function wait-symbol))
         (wait-entered (sb-thread:make-semaphore :count 0))
         (waiter-finished (sb-thread:make-semaphore :count 0))
         (waiter-thread nil)
         (first-stream-id nil)
         (second-stream-id nil)
         (second-entry-stream-ids nil)
         (first-error nil)
         (second-error nil)
         (producer-calls 0))
    (unwind-protect
         (progn
           (let ((response
                   (http-kit/http2:send-http2-request-over-connection
                    connection
                    (make-http-request :method "GET"
                                       :uri "https://example.com/cancel-warmup"))))
             (ensure-equal 200 (http-response-status response)))
           ;; Keep peer capacity and sleep the waiter without consuming fixture EOF.
           (http-kit/http2::%h2-with-scheduler-lock (connection)
             (setf (http-kit/http2::%http2-connection-scheduler-reader-active-p
                    connection)
                   t))
           (setf (symbol-function wait-symbol)
                 (lambda (wait-connection deadline clock-function)
                   (sb-thread:signal-semaphore wait-entered)
                   (funcall original-wait wait-connection deadline clock-function)))
           (handler-case
               (http-kit/http2:send-http2-request-over-connection
                connection
                (make-http-request :method "POST"
                                   :uri "https://example.com/cancel-owner")
                :request-body-function
                (lambda (maximum-size)
                  (declare (ignore maximum-size))
                  (incf producer-calls)
                  (octets 97))
                :request-body-length 1
                :on-stream-open
                (lambda (stream-id cancel)
                  (setf first-stream-id stream-id)
                  (setf waiter-thread
                        (sb-thread:make-thread
                         (lambda ()
                           (unwind-protect
                                (handler-case
                                    (http-kit/http2:send-http2-request-over-connection
                                     connection
                                     (make-http-request
                                      :method "POST"
                                      :uri "https://example.com/cancel-waiter")
                                     :request-body-function
                                     (lambda (maximum-size)
                                       (declare (ignore maximum-size))
                                       (incf producer-calls)
                                       (octets 98))
                                     :request-body-length 1
                                     :on-stream-open
                                     (lambda (opened-stream-id cancel-waiter)
                                       (setf second-stream-id opened-stream-id
                                             second-entry-stream-ids
                                             (http-kit/http2::%h2-with-scheduler-lock
                                                 (connection)
                                               (mapcar
                                                #'http-kit/http2::%h2-batch-entry-stream-id
                                                (http-kit/http2::%http2-connection-scheduler-entries
                                                 connection))))
                                       (funcall cancel-waiter)))
                                  (error (condition)
                                    (setf second-error condition)))
                             (sb-thread:signal-semaphore waiter-finished)))))
                  (ensure-true
                   (sb-thread:wait-on-semaphore wait-entered :timeout 5))
                  (funcall cancel)
                  (ensure-true
                   (sb-thread:wait-on-semaphore waiter-finished :timeout 5))))
             (error (condition)
               (setf first-error condition)))
           (when waiter-thread
             (sb-thread:join-thread waiter-thread)
             (setf waiter-thread nil))
           (ensure-equal 3 first-stream-id)
           (ensure-equal 5 second-stream-id)
           (ensure-equal '(5) second-entry-stream-ids)
           (ensure-true (typep first-error 'http-connection-error))
           (ensure-equal '(:local-cancel 3 8)
                         (http-connection-error-cause first-error))
           (ensure-true (typep second-error 'http-connection-error))
           (ensure-equal '(:local-cancel 5 8)
                         (http-connection-error-cause second-error))
           (ensure-equal 0 producer-calls)
           (ensure-false
            (http-kit/http2::%http2-connection-scheduler-entries connection))
           (ensure-true
            (http-kit/http2::%h2-scheduler-capacity-available-p connection))
           (let* ((frames (h2-output-frames stream))
                  (resets
                    (sort
                     (loop for frame in frames
                           when (= 3 (http-kit/http2::%h2-frame-type frame))
                             collect
                             (list
                              (http-kit/http2::%h2-frame-stream-id frame)
                              (http-kit/http2::%h2-u32
                               (http-kit/http2::%h2-frame-payload frame) 0)))
                     #'< :key #'first)))
             (ensure-equal '((3 8) (5 8)) resets)
             (ensure-equal 0
                           (count 0 frames
                                  :key #'http-kit/http2::%h2-frame-type)))
      (setf (symbol-function wait-symbol) original-wait)
      (http-kit/http2::%h2-with-scheduler-lock (connection)
        (setf (http-kit/http2::%http2-connection-scheduler-reader-active-p
               connection)
              nil)
        (http-kit/http2::%h2-scheduler-broadcast connection))
      (http-kit/http2:close-http2-connection connection)
      (when waiter-thread
        (ignore-errors (sb-thread:join-thread waiter-thread)))))))
#+sbcl
(deftest http2-coverage-scheduler-condition-wait-deadline
  (let* ((stream (make-instance (quote binary-session-stream) :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (clock-calls 0)
         (failure nil))
    (unwind-protect
         (progn
           (handler-case
               (http-kit/http2::%h2-with-scheduler-lock (connection)
                 (http-kit/http2::%h2-scheduler-condition-wait
                  connection 0.01
                  (lambda () (if (> (incf clock-calls) 2) 1 0))))
             (http-kit:http-timeout (condition)
               (setf failure condition)))
           (ensure-true (typep failure (quote http-kit:http-timeout)))
           (ensure-equal 3 clock-calls)
           (ensure-equal :read (http-kit:http-error-operation failure)))
      (http-kit/http2:close-http2-connection connection))))
#+sbcl
(deftest http2-coverage-scheduler-explicit-cancel-code
  (let* ((input
           (concatenate-octets
            (h2-frame 4 0 0 (octets))
            (h2-frame 1 5 1
                      (h2-header-block (cons ":status" "200")
                                       (cons "content-length" "0")))))
         (stream (make-instance 'binary-session-stream :input input))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (opened-stream nil)
         (failure nil))
    (unwind-protect
         (progn
           (handler-case
               (http-kit/http2:send-http2-request-over-connection
                connection
                (make-http-request :method "GET"
                                   :uri "https://example.com/cancel-code")
                :on-stream-open
                (lambda (stream-id cancel)
                  (setf opened-stream stream-id)
                  (funcall cancel :error-code 2)))
             (error (condition)
               (setf failure condition)))
           (ensure-equal 1 opened-stream)
           (ensure-true (typep failure 'http-connection-error))
           (ensure-equal '(:local-cancel 1 2)
                         (http-connection-error-cause failure))
           (ensure-false
            (http-kit/http2::%http2-connection-scheduler-entries connection))
           (let* ((frames (h2-output-frames stream))
                  (reset (find 3 frames :key #'http-kit/http2::%h2-frame-type)))
             (ensure-true reset)
             (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id reset))
             (ensure-equal (octets 0 0 0 2)
                           (http-kit/http2::%h2-frame-payload reset))))
      (http-kit/http2:close-http2-connection connection))))
#+sbcl
(deftest http2-coverage-scheduler-default-cancel-code
  (let* ((input
           (concatenate-octets
            (h2-frame 4 0 0 (octets))
            (h2-frame 1 5 1
                      (h2-header-block (cons ":status" "200")
                                       (cons "content-length" "0")))))
         (stream (make-instance 'binary-session-stream :input input))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (opened-stream nil)
         (failure nil))
    (unwind-protect
         (progn
           (handler-case
               (http-kit/http2:send-http2-request-over-connection
                connection
                (make-http-request :method "GET"
                                   :uri "https://example.com/default-cancel")
                :on-stream-open
                (lambda (stream-id cancel)
                  (setf opened-stream stream-id)
                  (funcall cancel)))
             (error (condition)
               (setf failure condition)))
           (ensure-equal 1 opened-stream)
           (ensure-true (typep failure 'http-connection-error))
           (ensure-equal '(:local-cancel 1 8)
                         (http-connection-error-cause failure))
           (ensure-false
            (http-kit/http2::%http2-connection-scheduler-entries connection))
           (let* ((frames (h2-output-frames stream))
                  (reset (find 3 frames :key #'http-kit/http2::%h2-frame-type)))
             (ensure-true reset)
             (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id reset))
             (ensure-equal (octets 0 0 0 8)
                           (http-kit/http2::%h2-frame-payload reset))))
      (http-kit/http2:close-http2-connection connection))))
#+sbcl
(deftest http2-coverage-scheduler-capacity-recheck-after-settings
  (let* ((input (h2-frame 4 0 0 (octets 0 3 0 0 0 1)))
         (session-prefix
           (concatenate-octets
            (h2-preface)
            (http-kit/http2::%h2-settings-wire
             http-kit/http2::+http2-default-max-frame-size+)))
         (stream (make-instance 'binary-session-stream :input input))
         (connection (http-kit/http2:make-http2-connection
                      :stream stream
                      :max-frame-size
                      http-kit/http2::+http2-default-max-frame-size+))
         (request
           (make-http-request :method "GET" :uri "https://example.com/capacity"))
         (reader (http-kit/http2::%h2-reader-for stream))
         (clock (lambda () 0))
         (writer (http-kit/http2::%h2-scheduler-writer connection nil clock))
         (capacity-symbol
           'http-kit/http2::%h2-scheduler-capacity-available-p)
         (original-capacity
           (symbol-function 'http-kit/http2::%h2-scheduler-capacity-available-p))
         (capacity-checks 0)
         (first-capacity nil)
         (second-capacity t)
         (entries-at-second-check nil)
         (entry nil))
    (unwind-protect
         (progn
           (write-sequence session-prefix stream)
           (setf (http-kit/http2::%http2-connection-session-started-p
                  connection)
                 t
                 (http-kit/http2::%http2-connection-scheduler-first-peer-frame-p
                  connection)
                 nil
                 (http-kit/http2::%http2-connection-peer-max-concurrent-streams
                  connection)
                 1
                 (symbol-function capacity-symbol)
                 (lambda (checked-connection)
                   (let ((available (funcall original-capacity
                                             checked-connection)))
                     (incf capacity-checks)
                     (cond
                       ((= capacity-checks 1)
                        (setf first-capacity available
                              (http-kit/http2::%http2-connection-peer-max-concurrent-streams
                               checked-connection)
                              0))
                       ((= capacity-checks 2)
                        (setf second-capacity available
                              entries-at-second-check
                              (length
                               (http-kit/http2::%http2-connection-scheduler-entries
                                checked-connection)))))
                     available)))
           (setf entry
                 (http-kit/http2::%h2-scheduler-open-entry
                  connection request nil nil
                  (http-kit/http2::%http2-connection-max-header-bytes
                   connection)
                  (http-kit/http2::%http2-connection-max-fields connection)
                  (http-kit/http2::%http2-connection-max-body-bytes connection)
                  nil clock nil nil nil nil t reader writer))
           (ensure-true first-capacity)
           (ensure-false second-capacity)
           (ensure-equal 0 entries-at-second-check)
           (ensure-true (>= capacity-checks 4))
           (ensure-equal 1
                         (http-kit/http2::%h2-batch-entry-stream-id entry))
           (ensure-equal (list entry)
                         (http-kit/http2::%http2-connection-scheduler-entries
                          connection))
           (ensure-equal 1
                         (http-kit/http2::%http2-connection-peer-max-concurrent-streams
                          connection))
           (ensure-equal 3
                         (http-kit/http2::%http2-connection-next-stream-id
                          connection))
           (ensure-equal (length input)
                         (binary-session-input-position stream))
           (ensure-equal session-prefix
                         (subseq (binary-session-output stream)
                                 0 (length session-prefix)))
           (let* ((frames (h2-output-frames stream))
                  (headers (find 1 frames
                                 :key #'http-kit/http2::%h2-frame-type)))
             (ensure-equal '(4 4 1)
                           (mapcar #'http-kit/http2::%h2-frame-type frames))
             (ensure-equal '(0 1 5)
                           (mapcar #'http-kit/http2::%h2-frame-flags frames))
             (ensure-true headers)
             (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id headers))))
      (setf (symbol-function capacity-symbol) original-capacity)
      (http-kit/http2:close-http2-connection connection))))
#+sbcl
(deftest http2-coverage-scheduler-upload-window-recheck
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (clock (lambda () 0))
         (reader (http-kit/http2::%h2-reader-for stream))
         (writer (http-kit/http2::%h2-writer stream nil clock))
         (check-symbol 'http-kit::%check-limit)
         (original-check (symbol-function 'http-kit::%check-limit))
         (check-lock (sb-thread:make-mutex :name "HTTP/2 upload check barrier"))
         (check-arrived (sb-thread:make-semaphore :count 0))
         (release-checks (sb-thread:make-semaphore :count 0))
         (check-count 0)
         (flow-waits 0)
         (producer-calls (make-array 2 :initial-element 0))
         (results (make-array 2 :initial-element nil))
         (thread-errors (make-array 2 :initial-element nil))
         (threads (make-array 2 :initial-element nil)))
    (unwind-protect
         (progn
           (setf (http-kit/http2::%http2-connection-peer-connection-window-size
                  connection)
                 1
                 (http-kit/http2::%http2-connection-peer-stream-windows
                  connection)
                 (list (cons 1 1) (cons 3 1))
                 (symbol-function check-symbol)
                 (lambda (&rest arguments)
                   (multiple-value-prog1
                       (apply original-check arguments)
                     (when (eq (first arguments) :body)
                       (let ((barrier-p nil))
                         (sb-thread:with-mutex (check-lock)
                           (when (< check-count 2)
                             (incf check-count)
                             (setf barrier-p t)))
                         (when barrier-p
                           (sb-thread:signal-semaphore check-arrived)
                           (unless (sb-thread:wait-on-semaphore
                                    release-checks :timeout 5)
                             (error "Timed out at the HTTP/2 upload barrier."))))))))
           (dotimes (index 2)
             (let* ((thread-index index)
                    (stream-id (if (zerop thread-index) 1 3))
                    (payload (if (zerop thread-index) 97 99)))
               (setf (aref threads thread-index)
                     (sb-thread:make-thread
                      (lambda ()
                        (handler-case
                            (setf (aref results thread-index)
                                  (multiple-value-list
                                   (http-kit/http2::%h2-connection-send-request-body
                                    connection reader writer stream-id (octets)
                                    (lambda (maximum-size)
                                      (declare (ignore maximum-size))
                                      (incf
                                       (aref producer-calls thread-index))
                                      (if (= (aref producer-calls thread-index) 1)
                                          (octets payload)
                                          nil))
                                    1 nil 16384 nil nil clock nil nil
                                    :scheduler-p t
                                    :flow-wait-function
                                    (lambda (flow-connection flow-stream-id
                                             deadline clock-function)
                                      (declare (ignore flow-stream-id
                                                       deadline
                                                       clock-function))
                                      (http-kit/http2::%h2-with-scheduler-lock
                                          (flow-connection)
                                        (incf
                                         (http-kit/http2::%http2-connection-peer-connection-window-size
                                          flow-connection))
                                        (incf flow-waits))))))
                          (error (condition)
                            (setf (aref thread-errors thread-index)
                                  condition))))))))
           (loop repeat 2
             do (ensure-true
                 (sb-thread:wait-on-semaphore check-arrived :timeout 5)))
           (loop repeat 2 do (sb-thread:signal-semaphore release-checks))
           (dotimes (index 2)
             (let ((thread (aref threads index)))
               (sb-thread:join-thread thread)
               (setf (aref threads index) nil)))
           (ensure-equal 2 check-count)
           (ensure-equal 1 flow-waits)
           (ensure-equal '(1 1) (coerce producer-calls 'list))
           (ensure-equal '((nil nil) (nil nil)) (coerce results 'list))
           (ensure-equal '(nil nil) (coerce thread-errors 'list))
           (ensure-equal 0
                         (http-kit/http2::%http2-connection-peer-connection-window-size
                          connection))
           (ensure-equal 0 (cdr (assoc 1
                                       (http-kit/http2::%http2-connection-peer-stream-windows
                                        connection))))
           (ensure-equal 0 (cdr (assoc 3
                                       (http-kit/http2::%http2-connection-peer-stream-windows
                                        connection))))
           (let* ((frame-reader
                    (http-kit/http2::%h2-reader-for
                     (binary-session-output stream)))
                  (first-frame (http-kit/http2::%h2-read-frame
                                frame-reader 16384 nil nil))
                  (second-frame (http-kit/http2::%h2-read-frame
                                 frame-reader 16384 nil nil))
                  (frames (list first-frame second-frame))
                  (first-data (find 1 frames
                                    :key #'http-kit/http2::%h2-frame-stream-id))
                  (second-data (find 3 frames
                                     :key #'http-kit/http2::%h2-frame-stream-id)))
             (ensure-equal 2 (count 0 frames
                                    :key #'http-kit/http2::%h2-frame-type))
             (ensure-true first-data)
             (ensure-true second-data)
             (ensure-equal (octets 97)
                           (http-kit/http2::%h2-frame-payload first-data))
             (ensure-equal (octets 99)
                           (http-kit/http2::%h2-frame-payload second-data))
             (ensure-equal 1
                           (logand 1
                                   (http-kit/http2::%h2-frame-flags first-data)))
             (ensure-equal 1
                           (logand 1
                                   (http-kit/http2::%h2-frame-flags second-data)))))
      (loop repeat 2 do (sb-thread:signal-semaphore release-checks))
      (dotimes (index 2)
        (let ((thread (aref threads index)))
          (when thread
            (sb-thread:join-thread thread)
            (setf (aref threads index) nil))))
      (setf (symbol-function check-symbol) original-check)
      (http-kit/http2:close-http2-connection connection))))
#+sbcl
(deftest http2-coverage-scheduler-upload-credit-and-malformed-goaway
  (let* ((input
           (concatenate-octets
            (h2-frame 4 0 0 (octets))
            (h2-frame 8 0 0 (octets 0 0 0 1))
            (h2-frame 1 4 1
                      (h2-header-block (cons ":status" "200")
                                       (cons "content-length" "2")))
            (h2-frame 0 1 1 (octets 111 107))))
         (stream (make-instance 'binary-session-stream :input input))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (body-chunks (list (octets 117 112)))
         (producer-calls 0))
    (unwind-protect
         (progn
           (setf (http-kit/http2::%http2-connection-peer-connection-window-size
                  connection)
                 1)
           (let ((response
                   (http-kit/http2:send-http2-request-over-connection
                    connection
                    (make-http-request :method "POST"
                                       :uri "https://example.com/window-resumption")
                    :request-body-function
                    (lambda (maximum-size)
                      (incf producer-calls)
                      (ensure-true (>= maximum-size 2))
                      (pop body-chunks))
                    :request-body-length 2)))
             (ensure-equal 200 (http-response-status response))
             (ensure-equal (octets 111 107) (http-response-body response)))
           (ensure-equal 1 producer-calls)
           (ensure-false body-chunks)
           (let* ((data-frames
                    (remove-if-not
                     (lambda (frame)
                       (= 0 (http-kit/http2::%h2-frame-type frame)))
                     (h2-output-frames stream))))
             (ensure-equal 2 (length data-frames))
             (ensure-equal (list (octets 117) (octets 112))
                           (mapcar #'http-kit/http2::%h2-frame-payload
                                   data-frames))
             (ensure-equal (list 0 1)
                           (mapcar (lambda (frame)
                                     (logand 1
                                             (http-kit/http2::%h2-frame-flags
                                              frame)))
                                   data-frames))
             (ensure-equal 0
                           (http-kit/http2::%http2-connection-peer-connection-window-size
                            connection))))
      (http-kit/http2:close-http2-connection connection)))
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (writer (http-kit/http2::%h2-writer stream nil (lambda () 0))))
    (unwind-protect
         (dolist (case
                  (list (list (h2-frame-object
                               7 0 1 (octets 0 0 0 0 0 0 0 0))
                              (list 1 8))
                        (list (h2-frame-object
                               7 0 0 (octets 128 0 0 0 0 0 0 0))
                              #x80000000)))
           (let ((failure
                   (handler-case
                       (progn
                         (http-kit/http2::%h2-connection-control-handler
                          connection (first case) writer 1)
                         nil)
                     (http-protocol-error (condition) condition))))
             (ensure-true (typep failure 'http-protocol-error))
             (ensure-equal :http2-control (http-error-operation failure))
             (ensure-equal (second case)
                           (http-protocol-error-detail failure))
             (ensure-false
              (http-kit/http2::%http2-connection-goaway-last-stream-id
               connection))))
      (http-kit/http2:close-http2-connection connection))))
#+sbcl
(deftest http2-coverage-serial-and-scheduled-upload-boundaries
  (let* ((input
           (concatenate-octets
            (h2-frame 8 0 0 (octets 0 0 0 1))
            (h2-frame 8 0 1 (octets 0 0 0 1))))
         (stream (make-instance 'binary-session-stream :input input))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (clock (lambda () 0))
         (reader (http-kit/http2::%h2-reader-for stream))
         (writer (http-kit/http2::%h2-writer stream nil clock)))
    (unwind-protect
         (progn
           (setf (http-kit/http2::%http2-connection-peer-connection-window-size
                  connection)
                 0
                 (http-kit/http2::%http2-connection-peer-stream-windows
                  connection)
                 (list (cons 1 0)))
           (multiple-value-bind (pending first-settings-p)
               (http-kit/http2::%h2-connection-send-request-body
                connection reader writer 1 (octets 97) nil 1 1 16384 nil nil
                clock nil nil)
             (ensure-false pending)
             (ensure-false first-settings-p))
           (let* ((frame-reader
                    (http-kit/http2::%h2-reader-for
                     (binary-session-output stream)))
                  (frame (http-kit/http2::%h2-read-frame
                          frame-reader 16384 nil nil)))
             (ensure-equal 0 (http-kit/http2::%h2-frame-type frame))
             (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id frame))
             (ensure-equal (octets 97)
                           (http-kit/http2::%h2-frame-payload frame))
             (ensure-equal 1
                           (logand 1 (http-kit/http2::%h2-frame-flags frame))))
           (ensure-equal 0
                         (http-kit/http2::%http2-connection-peer-connection-window-size
                          connection))
           (ensure-equal 0
                         (cdr (assoc
                               1
                               (http-kit/http2::%http2-connection-peer-stream-windows
                                connection))))
           (ensure-equal (length input) (binary-session-input-position stream)))
      (http-kit/http2:close-http2-connection connection)))
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (clock (lambda () 0))
         (reader (http-kit/http2::%h2-reader-for stream))
         (writer (http-kit/http2::%h2-writer stream nil clock))
         (producer-calls 0))
    (unwind-protect
         (progn
           (http-kit/http2::%h2-connection-send-request-body
            connection reader writer 1 (octets)
            (lambda (maximum-size)
              (ensure-true (plusp maximum-size))
              (incf producer-calls)
              (octets 98))
            1 1 16384 nil nil clock nil nil)
           (ensure-equal 1 producer-calls)
           (let* ((frame-reader
                    (http-kit/http2::%h2-reader-for
                     (binary-session-output stream)))
                  (frame (http-kit/http2::%h2-read-frame
                          frame-reader 16384 nil nil)))
             (ensure-equal 0 (http-kit/http2::%h2-frame-type frame))
             (ensure-equal (octets 98)
                           (http-kit/http2::%h2-frame-payload frame))
             (ensure-equal 1
                           (logand 1 (http-kit/http2::%h2-frame-flags frame))))
           (ensure-equal 65534
                         (http-kit/http2::%http2-connection-peer-connection-window-size
                          connection)))
      (http-kit/http2:close-http2-connection connection)))
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (clock (lambda () 0))
         (reader (http-kit/http2::%h2-reader-for stream))
         (writer (http-kit/http2::%h2-writer stream nil clock))
         (producer-calls 0))
    (unwind-protect
         (progn
           (http-kit/http2::%h2-connection-send-request-body
            connection reader writer 1 (octets)
            (lambda (maximum-size)
              (declare (ignore maximum-size))
              (incf producer-calls)
              nil)
            nil 1 16384 nil nil clock nil nil)
           (ensure-equal 1 producer-calls)
           (let* ((frame-reader
                    (http-kit/http2::%h2-reader-for
                     (binary-session-output stream)))
                  (frame (http-kit/http2::%h2-read-frame
                          frame-reader 16384 nil nil)))
             (ensure-equal 0 (http-kit/http2::%h2-frame-type frame))
             (ensure-equal (octets)
                           (http-kit/http2::%h2-frame-payload frame))
             (ensure-equal 1
                           (logand 1 (http-kit/http2::%h2-frame-flags frame)))))
      (http-kit/http2:close-http2-connection connection)))
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (clock (lambda () 0))
         (reader (http-kit/http2::%h2-reader-for stream))
         (writer (http-kit/http2::%h2-writer stream nil clock))
         (producer-calls 0))
    (unwind-protect
         (progn
           (setf (http-kit/http2::%http2-connection-peer-connection-window-size
                  connection)
                 0
                 (http-kit/http2::%http2-connection-peer-stream-windows
                  connection)
                 (list (cons 1 0)))
           (http-kit/http2::%h2-connection-send-request-body
            connection reader writer 1 (octets)
            (lambda (maximum-size)
              (declare (ignore maximum-size))
              (incf producer-calls)
              nil)
            nil 1 16384 nil nil clock nil nil :scheduler-p t)
           (ensure-equal 1 producer-calls)
           (let* ((frame-reader
                    (http-kit/http2::%h2-reader-for
                     (binary-session-output stream)))
                  (frame (http-kit/http2::%h2-read-frame
                          frame-reader 16384 nil nil)))
             (ensure-equal 0 (http-kit/http2::%h2-frame-type frame))
             (ensure-equal (octets)
                           (http-kit/http2::%h2-frame-payload frame))
             (ensure-equal 1
                           (logand 1 (http-kit/http2::%h2-frame-flags frame))))
           (ensure-equal 0
                         (http-kit/http2::%http2-connection-peer-connection-window-size
                          connection))
           (ensure-equal 0
                         (cdr (assoc
                               1
                               (http-kit/http2::%http2-connection-peer-stream-windows
                                connection)))))
      (http-kit/http2:close-http2-connection connection))))
#+sbcl
(deftest http2-coverage-extended-connect-waits-for-peer-capacity
  (let* ((input
           (concatenate-octets
            (h2-frame 4 0 0
                      (octets 0 8 0 0 0 1 0 3 0 0 0 0))
            (h2-frame 4 0 0 (octets 0 3 0 0 0 1))
            (h2-frame
             1 5 1
             (h2-header-block (cons ":status" "200")))))
         (stream (make-instance 'binary-session-stream :input input))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (request
           (make-http-request :method "CONNECT" :protocol "websocket"
                              :uri "https://example.com/chat")))
    (unwind-protect
         (let ((response
                 (http-kit/http2::%h2-send-http2-request-over-connection-serial
                  connection request)))
           (ensure-equal 200 (http-response-status response))
           (ensure-equal (octets) (http-response-body response))
           (ensure-true
            (http-kit/http2::%http2-connection-session-started-p connection))
           (ensure-true
            (http-kit/http2::%http2-connection-peer-enable-connect-protocol-p
             connection))
           (ensure-equal 1
                         (http-kit/http2::%http2-connection-peer-max-concurrent-streams
                          connection))
           (ensure-equal (length input)
                         (binary-session-input-position stream))
           (let* ((frames (h2-output-frames stream))
                  (headers (remove-if-not
                            (lambda (frame)
                              (= 1 (http-kit/http2::%h2-frame-type frame)))
                            frames))
                  (settings-acks
                    (remove-if-not
                     (lambda (frame)
                       (and (= 4 (http-kit/http2::%h2-frame-type frame))
                            (/= 0
                                (logand 1
                                        (http-kit/http2::%h2-frame-flags
                                         frame)))))
                     frames)))
             (ensure-equal 1 (length headers))
             (ensure-equal 1
                           (http-kit/http2::%h2-frame-stream-id
                            (first headers)))
             (ensure-equal 2 (length settings-acks))))
      (http-kit/http2:close-http2-connection connection))))
#+sbcl
(deftest http2-coverage-scheduler-retired-frame-lifecycle
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (request (make-http-request :method "GET" :uri "https://example.com/retired"))
         (entry (http-kit/http2::%make-h2-batch-entry
                 :request request :stream-id 1))
         (clock (lambda () 0)))
    (unwind-protect
         (progn
           (setf (http-kit/http2::%http2-connection-scheduler-entries connection)
                 (list entry)
                 (http-kit/http2::%h2-batch-entry-callback-events entry)
                 (list (list :information #'identity
                             (make-http-response :status 103))
                       (list :body #'identity (octets 97 98) 2 nil nil)
                       (list :body #'identity (octets 99) 1 nil t)))
           (http-kit/http2::%h2-scheduler-retire-entry
            connection entry
            (make-condition 'simple-error :format-control "Retire queued callbacks.")
            clock)
           (ensure-true
            (http-kit/http2::%h2-batch-entry-local-retired-p entry))
           (ensure-false
            (http-kit/http2::%http2-connection-scheduler-entries connection))
           (ensure-false
            (http-kit/http2::%h2-batch-entry-callback-events entry))
           (let* ((reader
                    (http-kit/http2::%h2-reader-for
                     (binary-session-output stream)))
                  (frames nil))
             (loop for frame = (http-kit/http2::%h2-read-frame
                                reader 16384 nil nil)
                   until (eq frame :eof)
                   do (push frame frames))
             (setf frames (nreverse frames))
             (ensure-equal '(3)
                           (mapcar #'http-kit/http2::%h2-frame-type frames))
             (ensure-equal 1
                           (http-kit/http2::%h2-frame-stream-id (first frames)))
             (ensure-equal (octets 0 0 0 8)
                           (http-kit/http2::%h2-frame-payload (first frames)))))
      (http-kit/http2:close-http2-connection connection)))
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (payload (octets 1 2 3 4 5 6 7 8))
         (waiter (http-kit/http2::%make-h2-scheduler-ping-waiter payload)))
    (unwind-protect
         (progn
           (setf (http-kit/http2::%http2-connection-scheduler-ping-waiters
                  connection)
                 (list waiter)
                 (http-kit/http2::%h2-scheduler-ping-waiter-acknowledged-p waiter)
                 t)
           (dolist (case '((t :http2-control) (nil :http2-client)))
             (setf (http-kit/http2::%h2-scheduler-ping-waiter-acknowledged-p
                    waiter)
                   (first case))
             (let ((failure
                     (handler-case
                         (progn
                           (http-kit/http2::%h2-scheduler-control-frame
                            connection (h2-frame-object 6 0 1 payload) nil)
                           nil)
                       (http-protocol-error (condition) condition))))
               (ensure-true (typep failure 'http-protocol-error))
               (ensure-equal (second case)
                             (http-error-operation failure)))))
      (http-kit/http2:close-http2-connection connection)))
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (request (make-http-request :method "GET" :uri "https://example.com/data"))
         (entry (http-kit/http2::%make-h2-batch-entry
                 :request request :stream-id 1))
         (clock (lambda () 0)))
    (unwind-protect
         (progn
           (setf (http-kit/http2::%http2-connection-scheduler-first-peer-frame-p
                  connection)
                 nil
                 (http-kit/http2::%http2-connection-next-stream-id connection) 3
                 (http-kit/http2::%http2-connection-scheduler-last-opened-stream-id
                  connection)
                 1
                 (http-kit/http2::%http2-connection-scheduler-entries connection)
                 (list entry)
                 (http-kit/http2::%h2-batch-entry-local-retired-p entry)
                 t)
           (http-kit/http2::%h2-scheduler-dispatch-frame
            connection (h2-frame-object 0 0 1 (octets 97)) nil nil nil clock)
           (ensure-equal (list entry)
                         (http-kit/http2::%http2-connection-scheduler-entries
                          connection))
           (http-kit/http2::%h2-scheduler-dispatch-frame
            connection (h2-frame-object 0 1 1 (octets 98)) nil nil nil clock)
           (ensure-false
            (http-kit/http2::%http2-connection-scheduler-entries connection))
           (let* ((reader
                    (http-kit/http2::%h2-reader-for
                     (binary-session-output stream)))
                  (updates nil))
             (loop for frame = (http-kit/http2::%h2-read-frame
                                reader 16384 nil nil)
                   until (eq frame :eof)
                   when (= 8 (http-kit/http2::%h2-frame-type frame))
                     do (push frame updates))
             (ensure-equal 2
                           (reduce #'+ updates
                                   :key (lambda (frame)
                                          (http-kit/http2::%h2-u32
                                           (http-kit/http2::%h2-frame-payload
                                            frame)
                                           0))
                                   :initial-value 0))
             (ensure-true
              (every (lambda (frame)
                       (zerop (http-kit/http2::%h2-frame-stream-id frame)))
                     updates))))
      (http-kit/http2:close-http2-connection connection)))
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection (http-kit/http2:make-http2-connection :stream stream))
         (request (make-http-request :method "GET" :uri "https://example.com/headers"))
         (entry (http-kit/http2::%make-h2-batch-entry
                 :request request :stream-id 1))
         (clock (lambda () 0))
         (information-calls 0))
    (unwind-protect
         (progn
           (setf (http-kit/http2::%http2-connection-scheduler-first-peer-frame-p
                  connection)
                 nil
                 (http-kit/http2::%http2-connection-next-stream-id connection) 3
                 (http-kit/http2::%http2-connection-scheduler-last-opened-stream-id
                  connection)
                 1
                 (http-kit/http2::%http2-connection-scheduler-entries connection)
                 (list entry)
                 (http-kit/http2::%h2-batch-entry-local-retired-p entry)
                 t
                 (http-kit/http2::%h2-batch-entry-on-information entry)
                 (lambda (response)
                   (declare (ignore response))
                   (incf information-calls)))
           (http-kit/http2::%h2-scheduler-dispatch-frame
            connection
            (h2-frame-object 1 4 1 (h2-header-block (cons ":status" "103")))
            nil nil nil clock)
           (ensure-equal (list entry)
                         (http-kit/http2::%http2-connection-scheduler-entries
                          connection))
           (ensure-equal 0 information-calls)
           (http-kit/http2::%h2-scheduler-dispatch-frame
            connection
            (h2-frame-object 1 5 1
                             (h2-header-block (cons ":status" "200")
                                              (cons "content-length" "0")))
            nil nil nil clock)
           (ensure-false
            (http-kit/http2::%http2-connection-scheduler-entries connection))
           (ensure-equal 0 information-calls))
      (http-kit/http2:close-http2-connection connection)))
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (closed 0)
         (connection
           (http-kit/http2:make-http2-connection
            :stream stream
            :close-stream (lambda (owned-stream)
                            (ensure-true (eq stream owned-stream))
                            (incf closed)
                            (close owned-stream))))
         (request (make-http-request :method "GET" :uri "https://example.com/goaway"))
         (entry (http-kit/http2::%make-h2-batch-entry
                 :request request :stream-id 1)))
    (unwind-protect
         (progn
           (setf (http-kit/http2::%http2-connection-goaway-last-stream-id
                  connection)
                 1
                 (http-kit/http2::%http2-connection-scheduler-entries connection)
                 (list entry))
           (http-kit/http2::%h2-scheduler-close-goaway-retired-if-idle connection)
           (ensure-true (http-kit/http2:http2-connection-open-p connection))
           (ensure-equal 0 closed)
           (setf (http-kit/http2::%http2-connection-scheduler-entries connection) nil)
           (http-kit/http2::%h2-scheduler-close-goaway-retired-if-idle connection)
           (ensure-false (http-kit/http2:http2-connection-open-p connection))
           (ensure-equal 1 closed))
      (http-kit/http2:close-http2-connection connection))))
#+sbcl
(progn
  (deftest http2-coverage-manager-claim-after-close
    (let ((manager
            (http-kit/http2:make-http2-connection-manager
             :open-connection
             (lambda (&rest arguments)
               (declare (ignore arguments))
               nil))))
      (http-kit/http2:close-http2-connection-manager manager)
      (let ((failure
              (handler-case
                  (progn
                    (http-kit/http2::%h2-manager-claim-opening
                     manager :closed)
                    nil)
                (http-connection-error (condition) condition))))
        (ensure-true (typep failure 'http-connection-error))
        (ensure-equal :closed (http-connection-error-cause failure)))))

  (deftest http2-coverage-manager-opening-release-identity
    (let* ((manager
             (http-kit/http2:make-http2-connection-manager
              :open-connection
              (lambda (&rest arguments)
                (declare (ignore arguments))
                nil)))
           (opening
             (nth-value 0
               (http-kit/http2::%h2-manager-claim-opening manager :first)))
           (other-opening
             (nth-value 0
               (http-kit/http2::%h2-manager-claim-opening manager :second))))
      (unwind-protect
           (progn
             (http-kit/http2::%h2-manager-release-opening
              manager :first other-opening)
             (ensure-true
              (eq opening
                  (gethash :first
                           (http-kit/http2::%http2-connection-manager-openings
                            manager))))
             (ensure-true
              (eq opening
                  (http-kit/http2::%h2-manager-release-opening
                   manager :first opening)))
             (ensure-false
              (gethash :first
                       (http-kit/http2::%http2-connection-manager-openings
                        manager))))
        (http-kit/http2::%h2-manager-release-opening
         manager :first opening)
        (http-kit/http2::%h2-manager-release-opening
         manager :second other-opening))))

  (deftest http2-coverage-manager-wait-lock-recovery
    (let ((lock (sb-thread:make-mutex)))
      (unwind-protect
           (progn
             (http-kit/http2::%h2-manager-recover-wait-lock lock)
             (ensure-true (sb-thread:holding-mutex-p lock))
             (sb-thread:release-mutex lock)
             (sb-thread:grab-mutex lock)
             (http-kit/http2::%h2-manager-recover-wait-lock lock)
             (ensure-true (sb-thread:holding-mutex-p lock)))
        (when (sb-thread:holding-mutex-p lock)
          (sb-thread:release-mutex lock)))
      (ensure-false (sb-thread:holding-mutex-p lock))))

  (deftest http2-coverage-manager-open-entry-reuses-existing
    (let* ((existing-close-count 0)
           (new-close-count 0)
           (open-count 0)
           (existing-stream
             (make-instance 'binary-session-stream :input (octets)))
           (new-stream
             (make-instance 'binary-session-stream :input (octets)))
           (existing-connection
             (http-kit/http2:make-http2-connection
              :stream existing-stream
              :close-stream
              (lambda (stream)
                (ensure-true (eq stream existing-stream))
                (incf existing-close-count)
                (close stream))))
           (new-connection
             (http-kit/http2:make-http2-connection
              :stream new-stream
              :close-stream
              (lambda (stream)
                (ensure-true (eq stream new-stream))
                (incf new-close-count)
                (close stream))))
           (request
             (make-http-request :method "GET"
                                :uri "https://example.com/open-race"))
           (key :same-key)
           (entry
             (http-kit/http2::%make-http2-managed-connection
              :key key
              :connection existing-connection
              :opened-at 0
              :last-used 0
              :last-used-at 0
              :active-count 1
              :exclusive-p nil))
           (manager
             (http-kit/http2:make-http2-connection-manager
              :open-connection
              (lambda (ignored-request &key timeout deadline)
                (declare (ignore ignored-request timeout deadline))
                (incf open-count)
                new-connection))))
      (setf (http-kit/http2::%http2-connection-manager-entries manager)
            (list entry))
      (unwind-protect
           (progn
             (multiple-value-bind (connection selected-entry)
                 (http-kit/http2::%h2-manager-open-entry
                  manager request key nil nil nil nil t nil
                  (http-kit/http2::%http2-connection-manager-clear-generation
                   manager))
               (ensure-true (eq existing-connection connection))
               (ensure-true (eq entry selected-entry))
               (ensure-equal 2
                             (http-kit/http2::%http2-managed-connection-active-count
                              entry))
               (ensure-false
                (http-kit/http2::%http2-managed-connection-exclusive-p entry))
               (ensure-equal 1
                             (http-kit/http2::%http2-managed-connection-last-used
                              entry))
               (ensure-equal 1 open-count)
               (ensure-true (http-kit/http2:http2-connection-open-p
                             existing-connection))
               (ensure-false (http-kit/http2:http2-connection-open-p
                              new-connection))
               (ensure-equal 1 new-close-count))
             (http-kit/http2::%h2-manager-finish-entry manager entry)
             (http-kit/http2::%h2-manager-finish-entry manager entry)
             (http-kit/http2:close-http2-connection-manager manager)
             (ensure-equal 1 existing-close-count)
             (ensure-equal 1 new-close-count))
        (http-kit/http2:close-http2-connection-manager manager)
        (http-kit/http2:close-http2-connection existing-connection)
        (http-kit/http2:close-http2-connection new-connection)))))
(deftest http2-coverage-manager-exclusive-entry-is-not-shared
  (let* ((stream (make-instance 'binary-session-stream :input (octets)))
         (connection
           (http-kit/http2:make-http2-connection
            :stream stream
            :close-stream
            (lambda (owned-stream)
              (ensure-true (eq stream owned-stream))
              (close owned-stream))))
         (key :same-key)
         (entry
           (http-kit/http2::%make-http2-managed-connection
            :key key
            :connection connection
            :opened-at 0
            :last-used 0
            :last-used-at 0
            :active-count 1
            :exclusive-p t))
         (manager
           (http-kit/http2:make-http2-connection-manager
            :open-connection
            (lambda (&rest arguments)
              (declare (ignore arguments))
              nil))))
    (setf (http-kit/http2::%http2-connection-manager-entries manager)
          (list entry))
    (unwind-protect
         (multiple-value-bind (selected-entry request-scoped-p actions
                               clear-generation)
             (http-kit/http2::%h2-manager-select-entry manager key 0 t)
           (ensure-true (null selected-entry))
           (ensure-true request-scoped-p)
           (ensure-true (null actions))
           (ensure-equal 0 clear-generation)
           (ensure-equal 1
                         (http-kit/http2::%http2-managed-connection-active-count
                          entry))
           (ensure-true
            (http-kit/http2::%http2-managed-connection-exclusive-p entry)))
      (http-kit/http2::%h2-manager-finish-entry manager entry)
      (http-kit/http2:close-http2-connection-manager manager)
      (http-kit/http2:close-http2-connection connection))))
#+sbcl
(deftest http2-coverage-scheduler-bounds-informational-callback-queue
  (let* ((information-block
           (h2-header-block (cons ":status" "103")))
         (information-frame (h2-frame 1 4 1 information-block))
         (frames
           (append
            (list (h2-frame 4 0 0 (octets)) information-frame)
            (make-list 17 :initial-element information-frame)
            (list
             (h2-frame 1 5 3
                       (h2-header-block (cons ":status" "200")
                                        (cons "content-length" "0"))))))
         (stream
           (make-instance 'binary-session-stream
                          :input (apply #'concatenate-octets frames)))
         (connection
           (http-kit/http2:make-http2-connection :stream stream))
         (information-calls 0)
         (nested-response nil)
         (outer-error nil))
    (unwind-protect
         (progn
           (setf outer-error
                 (handler-case
                     (progn
                       (http-kit/http2:send-http2-request-over-connection
                        connection
                        (make-http-request :method "GET"
                                           :uri "https://example.com/outer")
                        :on-information
                        (lambda (response)
                          (ensure-equal 103 (http-response-status response))
                          (incf information-calls)
                          (when (= information-calls 1)
                            (setf nested-response
                                  (http-kit/http2:send-http2-request-over-connection
                                   connection
                                   (make-http-request
                                    :method "GET"
                                    :uri "https://example.com/nested"))))))
                       nil)
                   (http-protocol-error (condition) condition)))
           (ensure-true (typep outer-error 'http-protocol-error))
           (ensure-equal :http2-read (http-error-operation outer-error))
           (ensure-equal
            (list :information-callback-queue 1 16)
            (http-protocol-error-detail outer-error))
           (ensure-equal 200 (http-response-status nested-response))
           (ensure-equal 1 information-calls)
           (ensure-true
            (null
             (http-kit/http2::%http2-connection-scheduler-entries connection)))
           (let ((reset-frame
                   (find-if
                    (lambda (frame)
                      (and
                       (= http-kit/http2::+http2-rst-stream-type+
                          (http-kit/http2::%h2-frame-type frame))
                       (= 1 (http-kit/http2::%h2-frame-stream-id frame))))
                    (h2-output-frames stream))))
             (ensure-true reset-frame)
             (ensure-equal
              8
              (http-kit/http2::%h2-u32
               (http-kit/http2::%h2-frame-payload reset-frame) 0))))
      (http-kit/http2:close-http2-connection connection))))
#+sbcl
(deftest http2-coverage-request-body-inactive-scheduler-entry
  (let* ((stream
           (make-instance 'binary-session-stream :input (octets)))
         (connection
           (http-kit/http2:make-http2-connection :stream stream))
         (entry
           (http-kit/http2::%make-h2-batch-entry
            :request
            (make-http-request :method "POST"
                               :uri "https://example.com/inactive")
            :stream-id 1))
         (writes nil)
         (condition nil))
    (ensure-false
     (member entry
             (http-kit/http2::%http2-connection-scheduler-entries connection)
             :test #'eq))
    (unwind-protect
         (progn
           (handler-case
               (http-kit/http2::%h2-connection-send-request-body
                connection nil
                (lambda (wire) (push (copy-seq wire) writes))
                1 (octets 97) nil nil nil 16384 nil nil
                (lambda () 0) nil nil
                :scheduler-p t :scheduler-entry entry)
             (http-connection-error (observed)
               (setf condition observed)))
           (ensure-true (typep condition 'http-connection-error))
           (ensure-equal :http2-write (http-error-operation condition))
           (ensure-equal '(:stream-inactive 1)
                         (http-connection-error-cause condition))
           (ensure-false writes)
           (ensure-false
            (member entry
                    (http-kit/http2::%http2-connection-scheduler-entries
                     connection)
                    :test #'eq)))
      (http-kit/http2:close-http2-connection connection))))
#+sbcl
(deftest http2-coverage-request-body-unscheduled-trailer-headers
  (let* ((stream
           (make-instance 'binary-session-stream :input (octets)))
         (connection
           (http-kit/http2:make-http2-connection :stream stream))
         (trailers '(("x-upload-result" . "complete")))
         (writes nil))
    (unwind-protect
         (progn
           (multiple-value-bind (pending-frames read-initial-settings-p)
               (http-kit/http2::%h2-connection-send-request-body
                connection nil
                (lambda (wire) (push (copy-seq wire) writes))
                1 (octets) nil nil nil 16384 trailers nil
                (lambda () 0) nil nil
                :scheduler-p nil)
             (ensure-false pending-frames)
             (ensure-false read-initial-settings-p))
           (ensure-equal 1 (length writes))
           (let* ((reader (h2-reader-from-frames (first writes)))
                  (frame
                    (http-kit/http2::%h2-read-frame reader 16384 nil nil)))
             (ensure-false (eq frame :eof))
             (ensure-equal http-kit/http2::+http2-headers-type+
                           (http-kit/http2::%h2-frame-type frame))
             (ensure-equal 5 (http-kit/http2::%h2-frame-flags frame))
             (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id frame))
             (ensure-equal trailers
                           (h2-decode-header-block
                            (http-kit/http2::%h2-frame-payload frame)
                            (h2-hpack-context)))
             (ensure-equal :eof
                           (http-kit/http2::%h2-read-frame
                            reader 16384 nil nil))))
      (http-kit/http2:close-http2-connection connection))))

#+sbcl
(progn
  (deftest http2-coverage-scheduler-condition-wait-reacquires-lock
    (let* ((connection
             (http-kit/http2:make-http2-connection
              :stream
              (make-instance 'binary-session-stream :input (octets))))
           (lock
             (http-kit/http2::%http2-connection-scheduler-lock connection))
           (wait-symbol
             'http-kit/http2::%h2-scheduler-condition-wait)
           (original-wait (symbol-function wait-symbol))
           (clock-active-p nil)
           (clock-values nil)
           (clock-function
             (lambda ()
               (if clock-active-p
                   (or (pop clock-values) 2)
                   0)))
           (wait-count 0)
           (lock-reacquired-p nil)
           (result nil))
      (unwind-protect
           (progn
             (http-kit/http2::%h2-with-scheduler-lock (connection)
               (setf
                (http-kit/http2::%http2-connection-scheduler-reader-active-p
                 connection)
                t))
             (unwind-protect
                  (progn
                    (setf (symbol-function wait-symbol)
                          (lambda (wait-connection deadline wait-clock-function)
                            (incf wait-count)
                            (ensure-true (sb-thread:holding-mutex-p lock))
                            (setf clock-active-p t
                                  clock-values (list 0 1 2))
                            (handler-case
                                (funcall original-wait
                                         wait-connection deadline
                                         wait-clock-function)
                              (http-kit:http-timeout (condition)
                                (setf lock-reacquired-p
                                      (sb-thread:holding-mutex-p lock))
                                (error condition)))))
                    (setf result
                          (handler-case
                              (progn
                                (http-kit/http2:send-http2-request-over-connection
                                 connection
                                 (make-http-request
                                  :method "GET"
                                  :uri "https://example.com/scheduler-wait")
                                 :deadline 1
                                 :clock-function clock-function)
                                :returned)
                            (http-kit:http-timeout () :timed-out))))
               (setf (symbol-function wait-symbol) original-wait))
             (ensure-equal :timed-out result)
             (ensure-equal 1 wait-count)
             (ensure-true lock-reacquired-p))
        (http-kit/http2::%h2-with-scheduler-lock (connection)
          (setf (http-kit/http2::%http2-connection-scheduler-reader-active-p
                 connection)
                nil)
          (http-kit/http2::%h2-scheduler-broadcast connection))
        (http-kit/http2:close-http2-connection connection))))
)
