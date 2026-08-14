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
      (ensure-equal (length payload) (length body)))))
