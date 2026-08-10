(in-package #:http-kit/test)

#+sbcl
(progn
  (defun server-output-frames
      (output &optional
              (max-frame-size http-kit/http2::+http2-default-max-frame-size+))
    (let ((reader (http-kit/http2::%h2-reader-for output))
          (frames '()))
      (loop for frame = (http-kit/http2::%h2-read-frame
                         reader max-frame-size nil
                         #'http-kit::%monotonic-time)
            until (eq frame :eof)
            do (push frame frames))
      (nreverse frames)))

  (defun server-response-fields (frame)
    (let ((context
            (http-kit/http2::%make-hpack-context
             :max-size http-kit/http2::+hpack-default-table-size+
             :maximum-size http-kit/http2::+hpack-default-table-size+)))
      (http-kit/http2::%hpack-decode-block
       (http-kit/http2::%h2-frame-payload frame)
       context
       :max-header-bytes 65536)))

  (deftest http2-server-serves-a-basic-request
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 5 1
                             (h2-header-block
                              (cons ":method" "GET")
                              (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/")))))
           (stream (make-instance 'binary-session-stream :input input))
           (seen-request nil)
           (closed nil))
      (multiple-value-bind (count reason)
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (setf seen-request request)
             (make-http-response :status 200 :body (ascii "ok")))
           :close-stream (lambda (closed-stream)
                           (ensure-equal stream closed-stream)
                           (setf closed t)))
        (let* ((frames (server-output-frames
                        (binary-session-output stream)))
               (headers (find-if (lambda (frame)
                                   (= (http-kit/http2::%h2-frame-type frame) 1))
                                 frames))
               (data (find-if (lambda (frame)
                                (= (http-kit/http2::%h2-frame-type frame) 0))
                              frames))
               (fields (server-response-fields headers))
               (status-field (assoc ":status" fields :test #'string=)))
          (ensure-equal 1 count)
          (ensure-equal :eof reason)
          (ensure-true closed)
          (ensure-equal "GET" (http-request-method seen-request))
          (ensure-equal "/" (http-request-target seen-request))
          (ensure-equal "200" (cdr status-field))
          (ensure-equal "ok"
                        (octets-as-string
                         (http-kit/http2::%h2-frame-payload data)))))))

  (deftest http2-server-collects-body-and-trailers
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 4 1
                             (h2-header-block
                              (cons ":method" "POST")
                              (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/upload")
                              (cons "content-length" "3")))
                   (h2-frame 0 0 1 (ascii "abc"))
                   (h2-frame 1 5 1
                             (h2-header-block (cons "x-check" "ok")))))
           (stream (make-instance 'binary-session-stream :input input))
           (seen-request nil)
           (chunks '())
           (closed nil))
      (multiple-value-bind (count reason)
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (setf seen-request request)
             (make-http-response :status 200))
           :on-body-chunk (lambda (state chunk)
                            (declare (ignore state))
                            (push (octets-as-string chunk) chunks))
           :close-stream (lambda (closed-stream)
                           (ensure-equal stream closed-stream)
                           (setf closed t)))
        (ensure-equal 1 count)
        (ensure-equal :eof reason)
        (ensure-true closed)
        (ensure-equal "POST" (http-request-method seen-request))
        (ensure-equal "/upload" (http-request-target seen-request))
        (ensure-equal "abc"
                      (octets-as-string (http-request-body seen-request)))
        (ensure-equal "ok"
                      (http-header-value (http-request-trailers seen-request)
                                         "x-check"))
        (ensure-equal '("abc") chunks))))

  (deftest http2-server-applies-peer-settings-to-response-frames
    (let* ((body (make-array 20000
                             :element-type '(unsigned-byte 8)
                             :initial-element (char-code #\x)))
           (input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0
                             (octets 0 5 0 0 #x80 0))
                   (h2-frame 1 5 1
                             (h2-header-block
                              (cons ":method" "GET")
                              (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/large")))))
           (stream (make-instance 'binary-session-stream :input input)))
      (http-kit/http2:serve-http2-session
       stream
       (lambda (request)
         (declare (ignore request))
         (make-http-response :status 200 :body body))
       :close-stream (lambda (closed-stream)
                       (declare (ignore closed-stream))))
      (let* ((frames (server-output-frames
                      (binary-session-output stream) 32768))
             (data-frames (remove-if-not
                           (lambda (frame)
                             (= (http-kit/http2::%h2-frame-type frame) 0))
                           frames)))
        (ensure-equal 1 (length data-frames))
        (ensure-equal 20000
                      (length
                       (http-kit/http2::%h2-frame-payload
                        (first data-frames)))))))

  (deftest http2-server-reads-continuations-from-pending-frame-queue
    (let* ((second-header-block
             (h2-header-block
              (cons ":method" "GET")
              (cons ":scheme" "https")
              (cons ":authority" "example.com")
              (cons ":path" "/second")))
           (input (concatenate-octets
                   (h2-preface)
                   ;; SETTINGS_INITIAL_WINDOW_SIZE = 1 makes the first
                   ;; response stop after one byte and forces the server to
                   ;; read ahead while it is flow-control blocked.
                   (h2-frame 4 0 0 (octets 0 4 0 0 0 1))
                   (h2-frame 1 5 1
                             (h2-header-block
                              (cons ":method" "GET")
                              (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/first")))
                   ;; The second request's HEADERS and CONTINUATION are
                   ;; queued while the first response waits for a window.
                   (h2-frame 1 1 3 (subseq second-header-block 0 1))
                   (h2-frame 9 4 3 (subseq second-header-block 1))
                   (h2-frame 8 0 1 (octets 0 0 0 1))))
           (stream (make-instance 'binary-session-stream :input input))
           (targets '()))
      (multiple-value-bind (count reason)
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (push (http-request-target request) targets)
             (if (string= (http-request-target request) "/first")
                 (make-http-response :status 200 :body (ascii "xy"))
                 (make-http-response :status 204)))
           :close-stream (lambda (closed-stream)
                           (declare (ignore closed-stream))))
        (ensure-equal 2 count)
        (ensure-equal :eof reason)
        (ensure-equal '("/first" "/second") (nreverse targets)))))

  (deftest http2-server-sends-goaway-at-max-requests
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 5 1
                             (h2-header-block
                              (cons ":method" "GET")
                              (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/one")))))
           (stream (make-instance 'binary-session-stream :input input)))
      (multiple-value-bind (count reason)
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (declare (ignore request))
             (make-http-response :status 204))
           :max-requests 1
           :close-stream (lambda (closed-stream)
                           (declare (ignore closed-stream))))
        (let ((goaway
                (find-if (lambda (frame)
                           (= (http-kit/http2::%h2-frame-type frame) 7))
                         (server-output-frames
                          (binary-session-output stream)))))
          (ensure-equal 1 count)
          (ensure-equal :max-requests reason)
          (ensure-true goaway)
          (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id goaway))
          (ensure-equal 8 (http-kit/http2::%h2-frame-length goaway))
          (ensure-equal 1
                        (http-kit/http2::%h2-u32
                         (http-kit/http2::%h2-frame-payload goaway) 0))
          (ensure-equal 0
                         (http-kit/http2::%h2-u32
                         (http-kit/http2::%h2-frame-payload goaway) 4))))))

  (deftest http2-server-rejects-nonempty-settings-ack
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 4 1 0 (octets 0))))
           (stream (make-instance 'binary-session-stream :input input))
           (errors '()))
      (signals http-protocol-error
        (http-kit/http2:serve-http2-session
         stream
         (lambda (request)
           (declare (ignore request))
           (make-http-response :status 200))
         :on-error (lambda (condition request)
                     (declare (ignore request))
                     (push condition errors))
         :close-stream (lambda (closed-stream)
                         (declare (ignore closed-stream)))))
      (ensure-equal 1 (length errors)))))
