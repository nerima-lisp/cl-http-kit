(in-package #:http-kit/test)

(deftest http2-response-finish-errors
  (let ((body (octets 1 2 3)))
    (ensure-equal 200
                  (http-response-status
                   (http-kit/http2::%h2-finish-response
                    200
                    (list (make-http-header "content-length" "3"))
                    '()
                    body)))
    (signals http-invalid-header
      (http-kit/http2::%h2-finish-response
       200 (list (make-http-header "content-length" "x")) '() body))
    (signals http-invalid-header
      (http-kit/http2::%h2-finish-response
       200
       (list (make-http-header "content-length" "3")
             (make-http-header "content-length" "2"))
       '() body))
    (signals http-invalid-header
      (http-kit/http2::%h2-finish-response
       200 (list (make-http-header "content-length" "2")) '() body))
    (signals http-invalid-header
      (http-kit/http2::%h2-finish-response
       204 (list (make-http-header "content-length" "1")) '() (octets 1)))
    (signals http-invalid-header
      (http-kit/http2::%h2-finish-response
       204 (list (make-http-header "content-length" "0")) '() (octets)))
    (signals http-invalid-header
      (http-kit/http2::%h2-finish-response
       200 (list (make-http-header "content-length" "0")) '() (octets)
       :request-method "CONNECT" :no-body t))
    (ensure-equal 205
                  (http-response-status
                   (http-kit/http2::%h2-finish-response
                    205 (list (make-http-header "content-length" "0"))
                    '() (octets) :no-body t)))
    (ensure-equal (octets)
                  (http-response-body
                   (http-kit/http2::%h2-finish-response
                    304
                    (list (make-http-header "content-length" "3"))
                    '()
                    (octets)
                    :no-body t)))))

(deftest http2-response-header-processing-errors
  (let* ((context (http-kit/http2::%make-hpack-context
                   :max-size http-kit/http2::+hpack-default-table-size+
                   :maximum-size http-kit/http2::+hpack-default-table-size+))
         (body (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0))
         (headers-frame
           (h2-frame-object
            http-kit/http2::+http2-headers-type+
            (logior http-kit/http2::+http2-end-headers-flag+
                   http-kit/http2::+http2-end-stream-flag+)
            1
            (h2-header-block (cons ":status" "200")
                             (cons "x-test" "yes")))))
    (multiple-value-bind (status headers response)
        (http-kit/http2::%h2-process-headers-frame
         headers-frame (h2-reader-from-frames) 16384 nil (lambda () 0d0)
         1024 context nil nil body "GET")
      (ensure-equal 200 status)
      (ensure-equal '("yes") (http-header-values headers "x-test"))
      (ensure-equal (octets) (http-response-body response)))
    (let ((informational
            (h2-frame-object
             http-kit/http2::+http2-headers-type+
             http-kit/http2::+http2-end-headers-flag+
             1
             (h2-header-block (cons ":status" "103")))))
      (multiple-value-bind (status headers response)
          (http-kit/http2::%h2-process-headers-frame
           informational (h2-reader-from-frames) 16384 nil (lambda () 0d0)
           1024 context nil nil body "GET")
        (declare (ignore headers response))
        (ensure-true (null status))))
    (signals http-invalid-header
      (http-kit/http2::%h2-process-headers-frame
       (h2-frame-object
        http-kit/http2::+http2-headers-type+
        http-kit/http2::+http2-end-headers-flag+
        1
        (h2-header-block (cons ":status" "103")
                         (cons "content-length" "0")))
       (h2-reader-from-frames) 16384 nil (lambda () 0d0)
       1024 context nil nil body "GET"))
    (signals http-protocol-error
      (http-kit/http2::%h2-process-headers-frame
       (h2-frame-object
        http-kit/http2::+http2-headers-type+
        (logior http-kit/http2::+http2-end-headers-flag+
                http-kit/http2::+http2-end-stream-flag+)
        1
        (h2-header-block (cons ":status" "103")))
       (h2-reader-from-frames) 16384 nil (lambda () 0d0) 1024 context
       nil nil body "GET"))
    (signals http-invalid-header
      (http-kit/http2::%h2-process-headers-frame
       headers-frame (h2-reader-from-frames) 16384 nil (lambda () 0d0)
       1024 context 200 '() body "GET")))

  (let* ((context (http-kit/http2::%make-hpack-context))
         (body (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0))
         (initial
           (h2-frame-object
            http-kit/http2::+http2-headers-type+
            http-kit/http2::+http2-end-headers-flag+
            1
            (h2-header-block (cons ":status" "200"))))
         (trailing
           (h2-frame-object
            http-kit/http2::+http2-headers-type+
            (logior http-kit/http2::+http2-end-headers-flag+
                   http-kit/http2::+http2-end-stream-flag+)
            1
            (h2-header-block (cons "x-trailer" "done"))))
         (trailing-without-end
           (h2-frame-object
            http-kit/http2::+http2-headers-type+
            http-kit/http2::+http2-end-headers-flag+
            1
            (h2-header-block (cons "x-trailer" "done")))))
    (multiple-value-bind (status headers response)
        (http-kit/http2::%h2-process-headers-frame
         initial (h2-reader-from-frames) 16384 nil (lambda () 0d0)
         1024 context nil nil body "GET")
      (declare (ignore response))
      (multiple-value-bind (final-status final-headers final-response)
          (http-kit/http2::%h2-process-headers-frame
           trailing (h2-reader-from-frames) 16384 nil (lambda () 0d0)
           1024 context status headers body "GET")
        (ensure-equal 200 final-status)
        (ensure-equal '("done")
                      (http-header-values
                       (http-response-trailers final-response)
                       "x-trailer"))
        (ensure-equal '("done")
                      (http-header-values
                       (http-response-trailers
                        (http-kit/http2::%h2-finish-response
                         final-status final-headers
                         (http-kit/http2::%h2-trailers
                          (list (cons "x-trailer" "done")))
                         body))
                       "x-trailer"))))
    (signals http-protocol-error
      (http-kit/http2::%h2-process-headers-frame
       trailing-without-end (h2-reader-from-frames) 16384 nil (lambda () 0d0)
       1024 context 200 '() body "GET"))))

(deftest http2-batch-rejects-informational-content-length
  (let ((entry
          (http-kit/http2::%make-h2-batch-entry
           :request (make-http-request :method "GET"
                                       :uri "https://127.0.0.1/")
           :stream-id 1))
        (frame
          (h2-frame-object
           http-kit/http2::+http2-headers-type+
           http-kit/http2::+http2-end-headers-flag+
           1
           (h2-header-block (cons ":status" "103")
                            (cons "content-length" "0"))))
        (context (http-kit/http2::%make-hpack-context)))
    (signals http-invalid-header
      (http-kit/http2::%h2-batch-process-headers-frame
       entry frame (lambda () :eof) 16384 1024 128 context "GET"))))

(deftest http2-response-data-boundaries
  (let ((body (make-array 0
                          :element-type '(unsigned-byte 8)
                          :adjustable t
                          :fill-pointer 0))
        (payload
          (make-array (1+ http-kit/http2::+http2-default-window-size+)
                      :element-type '(unsigned-byte 8)
                      :initial-element 0)))
    (multiple-value-bind (end-stream new-body-length data-length)
        (%test-h2-append-data-frame
         (h2-frame-object http-kit/http2::+http2-data-type+ 0 1 payload)
         200 body "GET" 2000000)
      (ensure-true (not end-stream))
      (ensure-equal (length payload) new-body-length)
      (ensure-equal (length payload) data-length)
      (ensure-equal (length payload) (length body)))
    (multiple-value-bind (end-stream new-body-length data-length)
        (http-kit/http2::%h2-append-data-frame
         (h2-frame-object http-kit/http2::+http2-data-type+
                          http-kit/http2::+http2-end-stream-flag+
                          1 (octets 1 2 3))
         200 body "CONNECT" 2000000)
      (ensure-true end-stream)
      (ensure-equal (+ (length payload) 3) new-body-length)
      (ensure-equal 3 data-length))))


(deftest http2-coverage-connection-window-overflow-is-atomic
  (cl-weave:expect-assertions 10)
  (let* ((connection
           (http-kit/http2::%make-http2-connection
            :peer-connection-window-size #x7ffffffe
            :peer-stream-windows (list (cons 1 7))))
         (update (h2-frame-object 8 0 0 (octets 0 0 0 1)))
         (caught nil))
    (http-kit/http2::%h2-connection-note-window-update connection update 1)
    (ensure-equal #x7fffffff
                  (http2-connection-peer-connection-window-size connection))
    (signals http-protocol-error
      (handler-bind ((http-protocol-error
                       (lambda (condition) (setf caught condition))))
        (http-kit/http2::%h2-connection-note-window-update connection update 1)))
    (ensure-equal :http2-control (http-error-operation caught))
    (ensure-equal #x80000000 (http-protocol-error-detail caught))
    (ensure-equal #x7fffffff
                  (http2-connection-peer-connection-window-size connection))
    (ensure-equal '((1 . 7))
                  (http-kit/http2::%http2-connection-peer-stream-windows
                   connection))))

(deftest http2-coverage-stream-window-overflow-is-atomic
  (cl-weave:expect-assertions 10)
  (let* ((connection
           (http-kit/http2::%make-http2-connection
            :peer-connection-window-size 11
            :peer-stream-windows (list (cons 1 #x7ffffffe) (cons 3 7))))
         (update (h2-frame-object 8 0 1 (octets 0 0 0 1)))
         (caught nil))
    (http-kit/http2::%h2-connection-note-window-update connection update 1)
    (ensure-equal #x7fffffff
                  (cdr (assoc 1
                              (http-kit/http2::%http2-connection-peer-stream-windows
                               connection))))
    (signals http-protocol-error
      (handler-bind ((http-protocol-error
                       (lambda (condition) (setf caught condition))))
        (http-kit/http2::%h2-connection-note-window-update connection update 1)))
    (ensure-equal :http2-control (http-error-operation caught))
    (ensure-equal #x80000000 (http-protocol-error-detail caught))
    (ensure-equal '((1 . #x7fffffff) (3 . 7))
                  (http-kit/http2::%http2-connection-peer-stream-windows
                   connection))
    (ensure-equal 11
                  (http2-connection-peer-connection-window-size connection))))

(deftest http2-coverage-settings-shrink-and-stream-credit-recovery
  (cl-weave:expect-assertions 18)
  (let ((connection
          (http-kit/http2::%make-http2-connection
           :peer-initial-window-size 10
           :peer-connection-window-size 11
           :peer-stream-windows (list (cons 1 3) (cons 3 8)))))
    (http-kit/http2::%h2-connection-note-peer-settings
     connection nil nil 2 nil)
    (ensure-equal 2 (http2-connection-peer-initial-window-size connection))
    (ensure-equal '((1 . -5) (3 . 0))
                  (http-kit/http2::%http2-connection-peer-stream-windows
                   connection))
    (ensure-equal 11
                  (http2-connection-peer-connection-window-size connection))
    (http-kit/http2::%h2-connection-note-window-update
     connection (h2-frame-object 8 0 1 (octets 0 0 0 5)) 1)
    (ensure-equal '((1 . 0) (3 . 0))
                  (http-kit/http2::%http2-connection-peer-stream-windows
                   connection))
    (http-kit/http2::%h2-connection-note-window-update
     connection (h2-frame-object 8 0 1 (octets 0 0 0 1)) 1)
    (ensure-equal '((1 . 1) (3 . 0))
                  (http-kit/http2::%http2-connection-peer-stream-windows
                   connection))
    (http-kit/http2::%h2-connection-note-window-update
     connection (h2-frame-object 8 0 5 (octets 0 0 0 1)) 5)
    (ensure-equal '((5 . 3) (1 . 1) (3 . 0))
                  (http-kit/http2::%http2-connection-peer-stream-windows
                   connection))))

#+sbcl
(progn
  (defun h2-flow-blocked-response-stream (&rest subsequent-frames)
    (make-instance
     'binary-session-stream
     :input (apply #'concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 5 1
                             (h2-header-block
                              (cons ":method" "GET")
                              (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/blocked")))
                   subsequent-frames)))

  (defun h2-response-error-output-frames (stream type)
    (remove-if-not
     (lambda (frame) (= type (http-kit/http2::%h2-frame-type frame)))
     (server-output-frames (binary-session-output stream))))

  (defun h2-assert-blocked-response-prefix (stream)
    (let ((data (h2-response-error-output-frames stream 0)))
      (ensure-equal '(16384 16384 16384 16383)
                    (mapcar (lambda (frame)
                              (length (http-kit/http2::%h2-frame-payload frame)))
                            data))
      (ensure-equal '((1 . 0) (1 . 0) (1 . 0) (1 . 0))
                    (mapcar (lambda (frame)
                              (cons (http-kit/http2::%h2-frame-stream-id frame)
                                    (http-kit/http2::%h2-frame-flags frame)))
                            data))
      (ensure-equal (make-array 65535 :element-type '(unsigned-byte 8)
                                      :initial-element #x78)
                    (apply #'concatenate-octets
                           (mapcar #'http-kit/http2::%h2-frame-payload data)))))

  (deftest http2-coverage-blocked-response-reset-releases-stream-slot
    (cl-weave:expect-assertions 15)
    (let ((stream
            (h2-flow-blocked-response-stream
             (h2-frame 3 0 1 (octets 0 0 0 8))
             (h2-frame 1 5 3
                       (h2-header-block
                        (cons ":method" "GET")
                        (cons ":scheme" "https")
                        (cons ":authority" "example.com")
                        (cons ":path" "/after-reset")))))
          (targets nil)
          (close-count 0))
      (multiple-value-bind (count reason)
          (serve-http2-session
           stream
           (lambda (request)
             (push (http-request-target request) targets)
             (if (string= "/blocked" (http-request-target request))
                 (make-http-response
                  :status 200
                  :body (make-array 65536 :element-type '(unsigned-byte 8)
                                          :initial-element #x78))
                 (make-http-response :status 204)))
           :max-concurrent-streams 1
           :close-stream (lambda (closed-stream)
                           (ensure-true (eq stream closed-stream))
                           (incf close-count)))
        (ensure-equal 1 count)
        (ensure-equal :eof reason))
      (ensure-equal '("/blocked" "/after-reset") (reverse targets))
      (ensure-equal 1 close-count)
      (let ((headers (h2-response-error-output-frames stream 1)))
        (ensure-equal '("200" "204")
                      (mapcar (lambda (frame)
                                (cdr (assoc ":status"
                                            (server-response-fields frame)
                                            :test #'string=)))
                              headers))
        (ensure-equal '((1 . 4) (3 . 5))
                      (mapcar (lambda (frame)
                                (cons (http-kit/http2::%h2-frame-stream-id frame)
                                      (http-kit/http2::%h2-frame-flags frame)))
                              headers)))
      (h2-assert-blocked-response-prefix stream)
      (ensure-equal nil (h2-response-error-output-frames stream 7))))

  (deftest http2-coverage-blocked-response-goaway-does-not-complete
    (cl-weave:expect-assertions 19)
    (let ((stream (h2-flow-blocked-response-stream
                   (h2-frame 7 0 0 (octets 0 0 0 1 0 0 0 0))))
          (handler-count 0)
          (close-count 0))
      (multiple-value-bind (count reason)
          (serve-http2-session
           stream
           (lambda (request)
             (declare (ignore request))
             (incf handler-count)
             (make-http-response
              :status 200
              :body (make-array 65536 :element-type '(unsigned-byte 8)
                                      :initial-element #x78)))
           :close-stream (lambda (closed-stream)
                           (ensure-true (eq stream closed-stream))
                           (incf close-count)))
        (ensure-equal 0 count)
        (ensure-equal :goaway reason))
      (ensure-equal 1 handler-count)
      (ensure-equal 1 close-count)
      (let ((headers (h2-response-error-output-frames stream 1)))
        (ensure-equal '("200")
                      (mapcar (lambda (frame)
                                (cdr (assoc ":status"
                                            (server-response-fields frame)
                                            :test #'string=)))
                              headers))
        (ensure-equal '(4) (mapcar #'http-kit/http2::%h2-frame-flags headers)))
      (h2-assert-blocked-response-prefix stream)
      (ensure-equal nil (h2-response-error-output-frames stream 7))))

  (deftest http2-coverage-blocked-response-eof-reports-current-request
    (cl-weave:expect-assertions 24)
    (let ((stream (h2-flow-blocked-response-stream))
          (request-seen nil)
          (error-request nil)
          (error-seen nil)
          (caught nil)
          (close-count 0))
      (signals http-protocol-error
        (handler-bind ((http-protocol-error
                         (lambda (condition) (setf caught condition))))
          (serve-http2-session
           stream
           (lambda (request)
             (setf request-seen request)
             (make-http-response
              :status 200
              :body (make-array 65536 :element-type '(unsigned-byte 8)
                                      :initial-element #x78)))
           :on-error (lambda (condition request)
                       (setf error-seen condition error-request request))
           :close-stream (lambda (closed-stream)
                           (ensure-true (eq stream closed-stream))
                           (incf close-count)))))
      (ensure-equal :http2-server (http-error-operation caught))
      (ensure-equal "HTTP/2 peer closed while response was flow-control blocked"
                    (http-error-message caught))
      (ensure-equal 1 (http-kit/http2::%h2-server-stream-id
                       (http-protocol-error-detail caught)))
      (ensure-false (http-kit/http2::%h2-server-stream-responded-p
                     (http-protocol-error-detail caught)))
      (ensure-true (eq request-seen error-request))
      (ensure-equal "/blocked" (http-request-target error-request))
      (ensure-true (eq caught error-seen))
      (ensure-equal 1 close-count)
      (let ((headers (h2-response-error-output-frames stream 1)))
        (ensure-equal '("200")
                      (mapcar (lambda (frame)
                                (cdr (assoc ":status"
                                            (server-response-fields frame)
                                            :test #'string=)))
                              headers))
        (ensure-equal '(4) (mapcar #'http-kit/http2::%h2-frame-flags headers)))
      (h2-assert-blocked-response-prefix stream)
      (ensure-equal (list (octets 0 0 0 1 0 0 0 1))
                    (mapcar #'http-kit/http2::%h2-frame-payload
                            (h2-response-error-output-frames stream 7)))))

  (deftest http2-coverage-blocked-response-rejects-idle-window-update
    (cl-weave:expect-assertions 27)
    (let ((stream (h2-flow-blocked-response-stream
                   (h2-frame 8 0 3 (octets 0 0 0 1))))
          (request-seen nil)
          (error-request nil)
          (error-seen nil)
          (caught nil)
          (close-count 0))
      (signals http-protocol-error
        (handler-bind ((http-protocol-error
                         (lambda (condition) (setf caught condition))))
          (serve-http2-session
           stream
           (lambda (request)
             (setf request-seen request)
             (make-http-response
              :status 200
              :body (make-array 65536 :element-type '(unsigned-byte 8)
                                      :initial-element #x78)))
           :on-error (lambda (condition request)
                       (setf error-seen condition error-request request))
           :close-stream (lambda (closed-stream)
                           (ensure-true (eq stream closed-stream))
                           (incf close-count)))))
      (ensure-equal :http2-server (http-error-operation caught))
      (ensure-equal "HTTP/2 WINDOW_UPDATE targeted an idle stream"
                    (http-error-message caught))
      (ensure-equal 3 (http-protocol-error-detail caught))
      (ensure-true (eq request-seen error-request))
      (ensure-equal "/blocked" (http-request-target error-request))
      (ensure-true (eq caught error-seen))
      (ensure-equal 1 close-count)
      (let ((headers (h2-response-error-output-frames stream 1)))
        (ensure-equal '("200")
                      (mapcar (lambda (frame)
                                (cdr (assoc ":status"
                                            (server-response-fields frame)
                                            :test #'string=)))
                              headers))
        (ensure-equal '(4) (mapcar #'http-kit/http2::%h2-frame-flags headers)))
      (h2-assert-blocked-response-prefix stream)
      (ensure-equal (list (octets 0 0 0 1 0 0 0 1))
                    (mapcar #'http-kit/http2::%h2-frame-payload
                            (h2-response-error-output-frames stream 7))))))
