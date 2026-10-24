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
            (h2-hpack-context)))
      (h2-decode-header-block
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

  (deftest http2-server-preserves-extended-connect-target
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 5 1
                             (h2-header-block
                              (cons ":method" "CONNECT")
                              (cons ":protocol" "websocket")
                              (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/chat?room=main")))))
           (stream (make-instance 'binary-session-stream :input input))
           (seen-request nil))
      (multiple-value-bind (count reason)
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (setf seen-request request)
             (make-http-response :status 204))
           :enable-connect-p t
           :close-stream (lambda (closed-stream)
                           (declare (ignore closed-stream))))
        (let ((uri (http-request-uri seen-request)))
          (ensure-equal 1 count)
          (ensure-equal :eof reason)
          (ensure-equal "CONNECT" (http-request-method seen-request))
          (ensure-equal "websocket" (http-request-protocol seen-request))
          (ensure-equal "/chat?room=main"
                        (http-request-target seen-request))
          (ensure-equal "https" (http-uri-scheme uri))
          (ensure-equal "/chat" (http-uri-path uri))
          (ensure-equal "room=main" (http-uri-query uri))))))

  (deftest http2-server-uses-host-when-authority-is-omitted
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 5 1
                             (h2-header-block
                              (cons ":method" "GET")
                              (cons ":scheme" "https")
                              (cons ":path" "/host-only")
                              (cons "host" "example.com")))))
           (stream (make-instance 'binary-session-stream :input input))
           (seen-request nil))
      (multiple-value-bind (count reason)
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (setf seen-request request)
             (make-http-response :status 204))
           :close-stream (lambda (closed-stream)
                           (declare (ignore closed-stream))))
        (let ((uri (http-request-uri seen-request)))
          (ensure-equal 1 count)
          (ensure-equal :eof reason)
          (ensure-equal "/host-only" (http-request-target seen-request))
          (ensure-equal "example.com" (http-uri-host uri))))))

  (deftest http2-server-restricts-asterisk-form-to-options
    (multiple-value-bind (method scheme authority target protocol headers)
        (http-kit/http2::%h2-server-header-fields
         (list (cons ":method" "OPTIONS")
               (cons ":scheme" "https")
               (cons ":authority" "example.com")
               (cons ":path" "*"))
         nil)
      (ensure-equal "OPTIONS" method)
      (ensure-equal "https" scheme)
      (ensure-equal "example.com" authority)
      (ensure-equal "*" target)
      (ensure-equal nil protocol)
      (ensure-equal nil headers))
    (signals http-invalid-header
      (http-kit/http2::%h2-server-header-fields
       (list (cons ":method" "GET")
             (cons ":scheme" "https")
             (cons ":authority" "example.com")
             (cons ":path" "*"))
       nil))
    (signals http-invalid-header
      (http-kit/http2::%h2-server-header-fields
       (list (cons ":method" "CONNECT")
             (cons ":protocol" "websocket")
             (cons ":scheme" "https")
             (cons ":authority" "example.com")
             (cons ":path" "*"))
       nil
       :enable-connect-p t)))

  (deftest http2-server-accepts-client-enable-push-and-unknown-flags
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 #x20 0 (octets 0 2 0 0 0 1))
                   (h2-frame 1 #x45 1
                             (h2-header-block
                              (cons ":method" "GET")
                              (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/")))))
           (stream (make-instance 'binary-session-stream :input input)))
      (multiple-value-bind (count reason)
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (declare (ignore request))
             (make-http-response :status 204))
           :close-stream (lambda (closed-stream)
                           (declare (ignore closed-stream))))
        (ensure-equal 1 count)
        (ensure-equal :eof reason))))

  (deftest http2-server-enforces-request-field-count-limit
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 5 1
                             (h2-header-block
                              (cons ":method" "GET")
                              (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/")))))
           (stream (make-instance 'binary-session-stream :input input)))
      (multiple-value-bind (count reason)
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (declare (ignore request))
             (error "The handler must not run for excessive fields."))
           :max-fields 3
           :close-stream nil)
        (ensure-equal 0 count)
        (ensure-true (typep reason 'http-size-limit-exceeded)))))

  (deftest http2-server-rejects-status-above-599
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 5 1
                             (h2-header-block
                              (cons ":method" "GET")
                              (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/")))))
           (stream (make-instance 'binary-session-stream :input input)))
      (signals http-invalid-status
        (http-kit/http2:serve-http2-session
         stream
         (lambda (request)
           (declare (ignore request))
           (make-http-response :status 600))))))

  (deftest http2-server-sends-informational-responses-before-final-response
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 5 1
                             (h2-header-block
                              (cons ":method" "GET")
                              (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/hints")))))
           (stream (make-instance 'binary-session-stream :input input)))
      (multiple-value-bind (count reason)
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (declare (ignore request))
             (values
              (make-http-response :status 204)
              (list
               (make-http-response
                :status 103
                :headers
                (list (make-http-header "link" "</style.css>; rel=preload"))))))
           :close-stream (lambda (closed-stream)
                           (declare (ignore closed-stream))))
        (let* ((frames (server-output-frames (binary-session-output stream)))
               (headers
                 (remove-if-not
                  (lambda (frame)
                    (= (http-kit/http2::%h2-frame-type frame) 1))
                  frames))
               (information-fields (server-response-fields (first headers)))
               (final-fields (server-response-fields (second headers))))
          (ensure-equal 1 count)
          (ensure-equal :eof reason)
          (ensure-equal 2 (length headers))
          (ensure-equal "103"
                        (cdr (assoc ":status" information-fields
                                    :test #'string=)))
          (ensure-equal "</style.css>; rel=preload"
                        (cdr (assoc "link" information-fields
                                    :test #'string=)))
          (ensure-equal 0
                        (logand
                         (http-kit/http2::%h2-frame-flags (first headers))
                         http-kit/http2::+http2-end-stream-flag+))
          (ensure-equal "204"
                        (cdr (assoc ":status" final-fields
                                    :test #'string=)))
          (ensure-true
           (/= 0
               (logand
                (http-kit/http2::%h2-frame-flags (second headers))
                http-kit/http2::+http2-end-stream-flag+)))))))

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

  (deftest http2-server-rejects-uncollected-trace-content
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 4 1
                             (h2-header-block
                              (cons ":method" "TRACE")
                              (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/")
                              (cons "content-length" "1")))
                   (h2-frame 0 1 1 (ascii "a"))))
           (stream (make-instance 'binary-session-stream :input input)))
      (signals http-protocol-error
        (http-kit/http2:serve-http2-session
         stream (lambda (request)
                  (declare (ignore request))
                  (make-http-response :status 200))
         :collect-body-p nil))))

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

  (deftest http2-server-enforces-peer-header-list-size
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0
                             (octets 0 6 0 0 0 42))
                   (h2-frame 1 5 1
                             (h2-header-block
                              (cons ":method" "GET")
                              (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/")))))
           (stream (make-instance 'binary-session-stream :input input)))
      (signals http-size-limit-exceeded
        (http-kit/http2:serve-http2-session
         stream
         (lambda (request)
           (declare (ignore request))
           (make-http-response
            :status 200
            :headers (list (make-http-header "x-extra" "value"))))))))

  (deftest http2-server-applies-updated-peer-header-list-size
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 4 0 0
                             (octets 0 6 0 0 0 42))
                   (h2-frame 1 5 1
                             (h2-header-block
                              (cons ":method" "GET")
                              (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/")))))
           (stream (make-instance 'binary-session-stream :input input)))
      (signals http-size-limit-exceeded
        (http-kit/http2:serve-http2-session
         stream
         (lambda (request)
           (declare (ignore request))
           (make-http-response
            :status 200
            :headers (list (make-http-header "x-extra" "value"))))))))

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

  (deftest http2-server-pending-control-preserves-stream-order
    (flet ((headers (id target flags)
             (h2-frame 1 flags id
                       (h2-header-block (cons ":method" "GET")
                                        (cons ":scheme" "https")
                                        (cons ":authority" "example.com")
                                        (cons ":path" target)))))
      (dolist (case '((nil nil) (t nil) (nil t) (nil :after-update)))
        (destructuring-bind (end-stream reset) case
          (dolist (read-ahead '(nil t))
            (let* ((first-update (h2-frame 8 0 1 (octets 0 0 0 1)))
                   (second-control
                     (if reset
                         (concatenate-octets
                          (if (eq reset :after-update)
                              (h2-frame 8 0 3 (octets 0 0 0 1))
                              (octets))
                          (h2-frame 3 0 3 (octets 0 0 0 8)))
                         (h2-frame 8 0 3 (octets 0 0 0 1))))
                   (input
                     (concatenate-octets
                      (h2-preface)
                      (h2-frame 4 0 0 (octets 0 4 0 0 0 1))
                      (headers 1 "/first" 5)
                      (if read-ahead (octets) first-update)
                      (headers 3 "/second" (if end-stream 5 4))
                      second-control
                      (if read-ahead first-update (octets))
                      (if (or end-stream reset)
                          (octets)
                          (h2-frame 0 1 3 (octets)))))
                   (stream (make-instance 'binary-session-stream :input input))
                   (targets '())
                   (closed 0))
              (multiple-value-bind (count reason)
                  (http-kit/http2:serve-http2-session
                   stream
                   (lambda (request)
                     (push (http-request-target request) targets)
                     (make-http-response :status 200 :body (ascii "xy")))
                   :close-stream (lambda (closed-stream)
                                   (ensure-equal stream closed-stream)
                                   (incf closed)))
                (ensure-equal (if reset 1 2) count)
                (ensure-equal :eof reason))
              (ensure-equal 1 closed)
              (ensure-equal (if reset '("/first") '("/first" "/second"))
                            (nreverse targets))
              (let* ((frames (server-output-frames (binary-session-output stream)))
                     (data (remove-if-not
                            (lambda (frame)
                              (= 0 (http-kit/http2::%h2-frame-type frame)))
                            frames)))
                (ensure-false (find 7 frames :key #'http-kit/http2::%h2-frame-type))
                (ensure-equal (cond (reset '((1 "x") (1 "y")))
                                    (end-stream '((1 "x") (1 "y") (3 "x") (3 "y")))
                                    (t '((1 "x") (1 "y") (3 "xy"))))
                              (mapcar
                               (lambda (frame)
                                 (list (http-kit/http2::%h2-frame-stream-id frame)
                                       (octets-as-string
                                        (http-kit/http2::%h2-frame-payload frame))))
                               data)))))))))

  (deftest http2-server-pending-control-rejects-header-interleaving
    (let ((block (h2-header-block (cons ":method" "GET")
                                 (cons ":scheme" "https")
                                 (cons ":authority" "example.com")
                                 (cons ":path" "/second"))))
      (dolist (control (list (h2-frame 4 0 0 (octets 0 4 0 0 0 2))
                            (h2-frame 6 0 0 (ascii "12345678"))
                            (h2-frame 8 0 1 (octets 0 0 0 1))
                            (h2-frame 3 0 1 (octets 0 0 0 8))
                            (h2-frame 7 0 0 (octets 0 0 0 1 0 0 0 0))))
        (dolist (blocked '(nil t))
          (let* ((input
                   (concatenate-octets
                    (server-peer-window-request 1)
                    (h2-frame 1 1 3 (subseq block 0 1))
                    control
                    (h2-frame 9 4 3 (subseq block 1))
                    (h2-frame 8 0 1 (octets 0 0 0 1))))
                 (stream (make-instance 'binary-session-stream :input input))
                 (targets '())
                 (observed nil)
                 (closed 0))
            (handler-case
                (http-kit/http2:serve-http2-session
                 stream
                 (lambda (request)
                   (push (http-request-target request) targets)
                   (if blocked
                       (make-http-response :status 200 :body (ascii "xy"))
                       (make-http-response :status 204)))
                 :close-stream (lambda (closed-stream)
                                 (ensure-equal stream closed-stream)
                                 (incf closed)))
              (http-protocol-error (condition) (setf observed condition)))
            (ensure-true (typep observed 'http-protocol-error))
            (ensure-equal "HTTP/2 HEADERS must be followed by same-stream CONTINUATION frames."
                          (http-kit:http-error-message observed))
            (ensure-equal 1 closed)
            (ensure-equal '("/") (nreverse targets))
            (let* ((frames (server-output-frames (binary-session-output stream)))
                   (goaway (find 7 frames :key #'http-kit/http2::%h2-frame-type)))
              (ensure-true goaway)
              (ensure-equal 1 (http-kit/http2::%h2-u32
                               (http-kit/http2::%h2-frame-payload goaway) 4))
              (ensure-false (find 6 frames :key #'http-kit/http2::%h2-frame-type))))))))

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
      (ensure-equal 1 (length errors))))

  (deftest http2-server-sends-protocol-error-for-idle-reset
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 5 1
                             (h2-header-block
                              (cons ":method" "GET")
                              (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/")))
                   (h2-frame 3 0 3 (octets 0 0 0 0))))
           (stream (make-instance 'binary-session-stream :input input)))
      (signals http-protocol-error
        (http-kit/http2:serve-http2-session
         stream
         (lambda (request)
           (declare (ignore request))
           (make-http-response :status 204))))
      (let ((goaway
              (find-if (lambda (frame)
                         (= (http-kit/http2::%h2-frame-type frame) 7))
                       (server-output-frames
                        (binary-session-output stream)))))
        (ensure-true goaway)
        (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id goaway))
        (ensure-equal 1
                      (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload goaway) 0))
        (ensure-equal 1
                      (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload goaway) 4)))))

  (deftest http2-server-error-goaway-retains-stream-after-hpack-error
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 5 1
                             (octets #x3f #xff #xff #xe3 #x1d
                                     #x82 #x87))))
           (stream (make-instance 'binary-session-stream :input input)))
      (signals http-protocol-error
        (http-kit/http2:serve-http2-session
         stream
         (lambda (request)
           (declare (ignore request))
           (make-http-response :status 204))))
      (let ((goaway
              (find-if (lambda (frame)
                         (= (http-kit/http2::%h2-frame-type frame) 7))
                       (server-output-frames
                        (binary-session-output stream)))))
        (ensure-true goaway)
        (ensure-equal 1
                      (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload goaway) 0))
        (ensure-equal 9
                      (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload goaway) 4)))))

  (deftest http2-server-priority-update-boundaries
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame #x10 0 0
                             (concatenate-octets (octets 0 0 0 1)
                                                 (ascii "u=0, i")))
                   (h2-frame 1 5 1
                             (h2-header-block
                              (cons ":method" "GET")
                              (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/priority")))))
           (stream (make-instance 'binary-session-stream :input input)))
      (multiple-value-bind (count reason)
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (ensure-equal "/priority" (http-request-target request))
             (make-http-response :status 204))
           :close-stream (lambda (closed-stream)
                           (declare (ignore closed-stream))))
        (ensure-equal 1 count)
        (ensure-equal :eof reason)))
    (dolist (payload (list (octets 0 0 0 0)
                           (octets #x80 0 0 1)
                           (octets 0 0 0 1 #xff)))
      (let ((stream
              (make-instance
               'binary-session-stream
               :input (concatenate-octets
                       (h2-preface)
                       (h2-frame 4 0 0 (octets))
                       (h2-frame #x10 0 0 payload)))))
        (signals http-protocol-error
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (declare (ignore request))
             (make-http-response :status 204))))))))

  (deftest http2-server-enforces-peer-and-local-concurrent-stream-limits
    (dolist (settings-and-arguments
             (list (list (octets 0 3 0 0 0 0) nil)
                   (list (octets) '(:max-concurrent-streams 0))))
      (destructuring-bind (settings arguments) settings-and-arguments
        (let* ((input (concatenate-octets
                       (h2-preface)
                       (h2-frame 4 0 0 settings)
                       (h2-frame 1 4 1
                                 (h2-header-block
                                  (cons ":method" "GET")
                                  (cons ":scheme" "https")
                                  (cons ":authority" "example.com")
                                  (cons ":path" "/one")))
                       (h2-frame 1 5 3
                                 (h2-header-block
                                  (cons ":method" "GET")
                                  (cons ":scheme" "https")
                                  (cons ":authority" "example.com")
                                  (cons ":path" "/two")))))
               (stream (make-instance 'binary-session-stream :input input)))
          (multiple-value-bind (count reason)
              (apply #'http-kit/http2:serve-http2-session
                     stream
                     (lambda (request)
                       (declare (ignore request))
                       (make-http-response :status 204))
                     arguments)
            (declare (ignore count))
            (ensure-equal :eof reason)
            (let ((resets
                    (remove-if-not
                     (lambda (frame)
                       (= (http-kit/http2::%h2-frame-type frame) 3))
                     (server-output-frames
                      (binary-session-output stream)))))
              (ensure-equal 2 (length resets))
              (dolist (frame resets)
                (ensure-equal 7
                              (http-kit/http2::%h2-u32
                               (http-kit/http2::%h2-frame-payload frame)
                               0)))))))))

  (deftest http2-server-releases-completed-and-reset-stream-state
    (let* ((headers (h2-header-block
                     (cons ":method" "GET")
                     (cons ":scheme" "https")
                     (cons ":authority" "example.com")
                     (cons ":path" "/ok")))
           (input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 5 1 headers)
                   (h2-frame 1 5 3 headers)))
           (stream (make-instance 'binary-session-stream :input input))
           (count 0))
      (multiple-value-bind (requests reason)
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (declare (ignore request))
             (incf count)
             (make-http-response :status 204))
           :max-concurrent-streams 1)
        (ensure-equal 2 count)
        (ensure-equal 2 requests)
        (ensure-equal :eof reason)))
    (let* ((headers (h2-header-block
                     (cons ":method" "GET")
                     (cons ":scheme" "https")
                     (cons ":authority" "example.com")
                     (cons ":path" "/reset")))
           (input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 4 1 headers)
                   (h2-frame 3 0 1 (octets 0 0 0 0))
                   (h2-frame 1 5 3 headers)))
           (stream (make-instance 'binary-session-stream :input input))
           (count 0))
      (multiple-value-bind (requests reason)
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (declare (ignore request))
             (incf count)
             (make-http-response :status 204))
           :max-concurrent-streams 1)
        (ensure-equal 1 count)
        (ensure-equal 1 requests)
        (ensure-equal :eof reason))))

  (deftest http2-server-bounds-reset-stream-budget
    (let* ((headers (h2-header-block
                     (cons ":method" "GET")
                     (cons ":scheme" "https")
                     (cons ":authority" "example.com")
                     (cons ":path" "/reset")))
           (input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 4 1 headers)
                   (h2-frame 3 0 1 (octets 0 0 0 0))
                   (h2-frame 3 0 1 (octets 0 0 0 0))))
           (stream (make-instance 'binary-session-stream :input input)))
      (signals http-protocol-error
        (http-kit/http2:serve-http2-session
         stream
         (lambda (request)
           (declare (ignore request))
           (make-http-response :status 204))
         :max-reset-streams 1))
      (let ((goaway
              (find-if (lambda (frame)
                         (= (http-kit/http2::%h2-frame-type frame) 7))
                       (server-output-frames
                        (binary-session-output stream)))))
        (ensure-true goaway)
        (ensure-equal 1
                      (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload goaway) 0))
        (ensure-equal 11
                      (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload goaway) 4)))))
  (deftest http2-server-bounds-aggregate-body-retention
    (let* ((body-headers (h2-header-block
                          (cons ":method" "POST")
                          (cons ":scheme" "https")
                          (cons ":authority" "example.com")
                          (cons ":path" "/reset")
                          (cons "content-length" "2")))
           (get-headers (h2-header-block
                         (cons ":method" "GET")
                         (cons ":scheme" "https")
                         (cons ":authority" "example.com")
                         (cons ":path" "/after-reset")))
           (input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 4 1 body-headers)
                   (h2-frame 0 0 1 (octets 97 98))
                   (h2-frame 3 0 1 (octets 0 0 0 0))
                   (h2-frame 1 5 3 get-headers)))
           (stream (make-instance 'binary-session-stream :input input))
           (charged 0)
           (events nil)
           (handled 0))
      (multiple-value-bind (requests reason)
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (incf handled)
             (ensure-equal "/after-reset" (http-request-target request))
             (make-http-response :status 204))
           :max-body-bytes 8
           :max-aggregate-body-bytes 2
           :reserve-body-bytes
           (lambda (amount)
             (incf charged amount)
             (push (list :reserve amount) events))
           :release-body-bytes
           (lambda (amount)
             (decf charged amount)
             (push (list :release amount) events))
           :close-stream nil)
        (ensure-equal 1 requests)
        (ensure-equal :eof reason))
      (ensure-equal 1 handled)
      (ensure-equal 0 charged)
      (ensure-true
       (equal (reverse events)
              (list (list :reserve 2) (list :release 2)))))
    (let* ((first-headers (h2-header-block
                           (cons ":method" "POST")
                           (cons ":scheme" "https")
                           (cons ":authority" "example.com")
                           (cons ":path" "/first")
                           (cons "content-length" "2")))
           (second-headers (h2-header-block
                            (cons ":method" "POST")
                            (cons ":scheme" "https")
                            (cons ":authority" "example.com")
                            (cons ":path" "/second")
                            (cons "content-length" "2")))
           (input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 4 1 first-headers)
                   (h2-frame 0 0 1 (octets 97 98))
                   (h2-frame 1 4 3 second-headers)
                   (h2-frame 0 0 3 (octets 99 100))))
           (stream (make-instance 'binary-session-stream :input input))
           (charged 0)
           (events nil)
           (handled 0)
           (request-count 0)
           (reason nil))
      (multiple-value-setq (request-count reason)
        (http-kit/http2:serve-http2-session
         stream
         (lambda (request)
           (declare (ignore request))
           (incf handled)
           (make-http-response :status 204))
         :max-body-bytes 8
         :max-aggregate-body-bytes 3
         :reserve-body-bytes
         (lambda (amount)
           (incf charged amount)
           (push (list :reserve amount) events))
         :release-body-bytes
         (lambda (amount)
           (decf charged amount)
           (push (list :release amount) events))
         :close-stream nil))
      (ensure-true
       (typep reason 'http-size-limit-exceeded))
      (ensure-equal 3 (http-size-limit-exceeded-limit reason))
      (ensure-equal 4 (http-size-limit-exceeded-observed reason))
      (ensure-equal 0 handled)
      (ensure-equal 0 request-count)
      (ensure-equal 0 charged)
      (ensure-true
       (equal (reverse events)
              (list (list :reserve 2) (list :release 2))))))

#+sbcl
(deftest http2-server-response-stream-materializes-chunks-before-end-stream
  (let* ((input (concatenate-octets
                 (h2-preface)
                 (h2-frame 4 0 0 (octets))
                 (h2-frame 1 5 1
                           (h2-header-block
                            (cons ":method" "GET")
                            (cons ":scheme" "https")
                            (cons ":authority" "example.com")
                            (cons ":path" "/stream")))))
         (stream (make-instance 'binary-session-stream :input input))
         (chunks (list (octets 97 98) (octets 99) nil))
         (producer-calls 0)
         (handled 0)
         (closed 0)
         (errors 0))
    (ensure-true (open-stream-p stream))
    (multiple-value-bind (count reason)
        (http-kit/http2:serve-http2-session
         stream
         (lambda (request)
           (incf handled)
           (ensure-equal "GET" (http-request-method request))
           (ensure-equal "/stream" (http-request-target request))
           (make-http-response-stream
            :status 200 :body-length 3
            :body-function (lambda ()
                             (incf producer-calls)
                             (pop chunks))))
         :on-error (lambda (condition request)
                     (declare (ignore condition request))
                     (incf errors))
         :close-stream (lambda (closed-stream)
                         (ensure-true (eq stream closed-stream))
                         (incf closed)
                         (close closed-stream)))
      (ensure-equal 1 count)
      (ensure-equal :eof reason))
    (ensure-equal 1 handled)
    (ensure-equal 3 producer-calls)
    (ensure-false chunks)
    (ensure-equal 0 errors)
    (ensure-equal 1 closed)
    (ensure-false (open-stream-p stream))
    (ensure-equal (length input) (binary-session-input-position stream))
    (let* ((frames (server-output-frames (binary-session-output stream)))
           (headers (remove-if-not
                     (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                     frames))
           (data (remove-if-not
                  (lambda (frame) (= 0 (http-kit/http2::%h2-frame-type frame)))
                  frames)))
      (ensure-equal 1 (length headers))
      (ensure-equal 1 (length data))
      (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id (first headers)))
      (ensure-equal 4 (http-kit/http2::%h2-frame-flags (first headers)))
      (ensure-equal "200"
                    (cdr (assoc ":status" (server-response-fields (first headers))
                                :test #'string=)))
      (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id (first data)))
      (ensure-equal "abc"
                    (octets-as-string
                     (http-kit/http2::%h2-frame-payload (first data))))
      (ensure-equal 1 (http-kit/http2::%h2-frame-flags (first data)))
      (ensure-false (find 7 frames :key #'http-kit/http2::%h2-frame-type)))))

#+sbcl
(deftest http2-server-response-stream-length-mismatch-precedes-response-wire
  (let* ((input (concatenate-octets
                 (h2-preface)
                 (h2-frame 4 0 0 (octets))
                 (h2-frame 1 5 1
                           (h2-header-block
                            (cons ":method" "GET")
                            (cons ":scheme" "https")
                            (cons ":authority" "example.com")
                            (cons ":path" "/stream")))))
         (stream (make-instance 'binary-session-stream :input input))
         (chunks (list (octets 97 98) (octets 99) nil))
         (producer-calls 0)
         (handled 0)
         (closed 0)
         (error-calls 0)
         (seen-request nil)
         (notified nil)
         (observed nil))
    (ensure-true (open-stream-p stream))
    (signals http-protocol-error
      (handler-bind ((http-protocol-error
                       (lambda (condition) (setf observed condition))))
        (http-kit/http2:serve-http2-session
         stream
         (lambda (request)
           (incf handled)
           (setf seen-request request)
           (make-http-response-stream
            :status 200 :body-length 2
            :body-function (lambda ()
                             (incf producer-calls)
                             (pop chunks))))
         :on-error (lambda (condition request)
                     (incf error-calls)
                     (setf notified condition)
                     (ensure-true (eq seen-request request)))
         :close-stream (lambda (closed-stream)
                         (ensure-true (eq stream closed-stream))
                         (incf closed)
                         (close closed-stream)))))
    (ensure-equal 1 handled)
    (ensure-equal "GET" (http-request-method seen-request))
    (ensure-equal "/stream" (http-request-target seen-request))
    (ensure-equal 3 producer-calls)
    (ensure-false chunks)
    (ensure-equal 1 error-calls)
    (ensure-true (typep observed 'http-protocol-error))
    (ensure-true (eq observed notified))
    (ensure-equal :http2-server (http-kit:http-error-operation observed))
    (ensure-equal '(2 3) (http-kit:http-protocol-error-detail observed))
    (ensure-equal 1 closed)
    (ensure-false (open-stream-p stream))
    (ensure-equal (length input) (binary-session-input-position stream))
    (let* ((frames (server-output-frames (binary-session-output stream)))
           (goaways (remove-if-not
                     (lambda (frame) (= 7 (http-kit/http2::%h2-frame-type frame)))
                     frames)))
      (ensure-false (find 1 frames :key #'http-kit/http2::%h2-frame-type))
      (ensure-false (find 0 frames :key #'http-kit/http2::%h2-frame-type))
      (ensure-equal 1 (length goaways))
      (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id (first goaways)))
      (ensure-equal 1 (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload (first goaways)) 0))
      (ensure-equal 1 (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload (first goaways)) 4)))))

#+sbcl
(deftest http2-server-preflight-rejects-options-before-session-ownership
  (let ((input (concatenate-octets
                (h2-preface)
                (h2-frame 4 0 0 (octets))
                (h2-frame 1 5 1
                          (h2-header-block
                           (cons ":method" "GET")
                           (cons ":scheme" "https")
                           (cons ":authority" "example.com")
                           (cons ":path" "/"))))))
    (dolist (case '((:handler :invalid)
                    (:close-stream :invalid)
                    (:on-error :invalid)
                    (:on-body-chunk :invalid)
                    (:default-authority 42)
                    (:max-requests -1)
                    (:max-requests 1/2)
                    (:collect-body-p :invalid)
                    (:enable-connect-p :invalid)))
      (destructuring-bind (option invalid) case
        (let* ((stream (make-instance 'binary-session-stream :input input))
               (handler-calls 0)
               (body-calls 0)
               (error-calls 0)
               (close-calls 0)
               (observed nil)
               (handler (lambda (request)
                          (declare (ignore request))
                          (incf handler-calls)
                          (make-http-response :status 200 :body (ascii "ok"))))
               (options
                 (list :close-stream (lambda (closed-stream)
                                       (incf close-calls)
                                       (close closed-stream))
                       :on-error (lambda (condition request)
                                   (declare (ignore condition request))
                                   (incf error-calls))
                       :on-body-chunk (lambda (state chunk)
                                        (declare (ignore state chunk))
                                        (incf body-calls))
                       :default-authority "example.com"
                       :max-requests nil
                       :collect-body-p t
                       :enable-connect-p nil)))
          (if (eq option :handler)
              (setf handler invalid)
              (setf (getf options option) invalid))
          (unwind-protect
               (progn
                 (handler-case
                     (apply #'http-kit/http2:serve-http2-session
                            stream handler options)
                   (http-protocol-error (condition)
                     (setf observed condition)))
                 (ensure-true (typep observed 'http-protocol-error))
                 (ensure-equal :http2-server
                               (http-kit:http-error-operation observed))
                 (ensure-equal (if (eq option :on-body-chunk)
                                   (list invalid nil nil)
                                   invalid)
                               (http-kit:http-protocol-error-detail observed))
                 (ensure-equal 0 handler-calls)
                 (ensure-equal 0 body-calls)
                 (ensure-equal 0 error-calls)
                 (ensure-equal 0 close-calls)
                 (ensure-equal 0 (binary-session-input-position stream))
                 (ensure-equal 0 (length (binary-session-output stream)))
                 (ensure-true (open-stream-p stream)))
            (when (open-stream-p stream)
              (close stream)))
          (ensure-false (open-stream-p stream)))))
    (let ((stream (make-instance 'binary-session-stream :input input))
          (handler-calls 0)
          (body-calls 0)
          (error-calls 0)
          (close-calls 0)
          (seen-request nil))
      (unwind-protect
           (multiple-value-bind (count reason)
               (http-kit/http2:serve-http2-session
                stream
                (lambda (request)
                  (incf handler-calls)
                  (setf seen-request request)
                  (make-http-response :status 200 :body (ascii "ok")))
                :default-authority "example.com"
                :max-requests nil
                :collect-body-p t
                :enable-connect-p nil
                :on-body-chunk (lambda (state chunk)
                                 (declare (ignore state chunk))
                                 (incf body-calls))
                :reserve-body-bytes #'identity
                :release-body-bytes #'identity
                :on-error (lambda (condition request)
                            (declare (ignore condition request))
                            (incf error-calls))
                :close-stream (lambda (closed-stream)
                                (incf close-calls)
                                (ensure-true (eq stream closed-stream))
                                (close closed-stream)))
             (ensure-equal 1 count)
             (ensure-equal :eof reason)
             (ensure-equal 1 handler-calls)
             (ensure-equal 0 body-calls)
             (ensure-equal 0 error-calls)
             (ensure-equal 1 close-calls)
             (ensure-false (open-stream-p stream))
             (ensure-equal (length input)
                           (binary-session-input-position stream))
             (ensure-equal "GET" (http-request-method seen-request))
             (ensure-equal "/" (http-request-target seen-request))
             (let* ((frames (server-output-frames
                             (binary-session-output stream)))
                    (settings (remove-if-not
                               (lambda (frame)
                                 (= 4 (http-kit/http2::%h2-frame-type frame)))
                               frames))
                    (headers (find 1 frames
                                   :key #'http-kit/http2::%h2-frame-type))
                    (data (find 0 frames
                                :key #'http-kit/http2::%h2-frame-type)))
               (ensure-equal 2 (length settings))
               (ensure-equal '(0 1)
                             (mapcar #'http-kit/http2::%h2-frame-flags settings))
               (ensure-true headers)
               (ensure-true data)
               (ensure-equal "200"
                             (cdr (assoc ":status" (server-response-fields headers)
                                         :test #'string=)))
               (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id headers))
               (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id data))
               (ensure-equal "ok"
                             (octets-as-string
                              (http-kit/http2::%h2-frame-payload data)))
               (ensure-equal 1 (logand 1 (http-kit/http2::%h2-frame-flags data)))))
        (when (open-stream-p stream)
          (close stream)))
      (ensure-false (open-stream-p stream)))))
(progn
  (deftest http2-server-preflight-rejects-invalid-server-options
    (dolist (case '((:request-timeout -1)
                    (:idle-timeout :invalid)
                    (:shutdown-p :invalid)
                    (:max-body-bytes :invalid)))
      (destructuring-bind (option invalid) case
        (let ((stream (make-instance 'binary-session-stream :input (octets)))
              (observed nil))
          (unwind-protect
               (progn
                 (handler-case
                     (apply #'http-kit/http2:serve-http2-session
                            stream #'identity
                            (list :close-stream nil option invalid))
                   (http-protocol-error (condition)
                     (setf observed condition)))
                 (ensure-true (typep observed 'http-protocol-error))
                 (ensure-equal :http2-server
                               (http-kit:http-error-operation observed))
                 (ensure-equal
                  (case option
                    (:request-timeout (cons option invalid))
                    (:idle-timeout (cons option invalid))
                    (otherwise invalid))
                  (http-kit:http-protocol-error-detail observed))
                 (ensure-equal 0 (binary-session-input-position stream))
                 (ensure-true (open-stream-p stream)))
            (when (open-stream-p stream)
              (close stream)))
          (ensure-false (open-stream-p stream))))))
  (deftest http2-server-preflight-rejects-unpaired-body-buffer-callbacks
    (let ((stream (make-instance 'binary-session-stream :input (octets)))
          (observed nil))
      (unwind-protect
           (progn
             (handler-case
                 (http-kit/http2:serve-http2-session
                  stream #'identity :close-stream nil
                  :reserve-body-bytes #'identity)
               (http-protocol-error (condition)
                 (setf observed condition)))
             (ensure-true (typep observed 'http-protocol-error))
             (ensure-equal :http2-server
                           (http-kit:http-error-operation observed))
             (ensure-equal nil
                           (first (http-kit:http-protocol-error-detail observed)))
             (ensure-true
              (functionp
               (second (http-kit:http-protocol-error-detail observed))))
             (ensure-equal nil
                           (third (http-kit:http-protocol-error-detail observed)))
             (ensure-equal 0 (binary-session-input-position stream))
             (ensure-true (open-stream-p stream)))
        (when (open-stream-p stream)
          (close stream)))
      (ensure-false (open-stream-p stream))))
  (deftest http2-server-internal-deadline-and-stream-guards
    (let ((deadlines (make-hash-table :test #'eq))
          (streams (make-hash-table)))
      (ensure-equal nil
                    (http-kit/http2::%h2-server-effective-deadline
                     nil nil deadlines))
      (ensure-equal 4
                    (http-kit/http2::%h2-server-effective-deadline
                     7 4 deadlines))
      (ensure-equal 7
                    (http-kit/http2::%h2-server-effective-deadline
                     7 9 deadlines))
      (ensure-equal 7
                    (http-kit/http2::%h2-server-effective-deadline
                     7 nil deadlines))
      (setf (gethash :stream deadlines) 5)
      (ensure-equal 5
                    (http-kit/http2::%h2-server-effective-deadline
                     nil nil deadlines))
      (ensure-equal 5
                    (http-kit/http2::%h2-server-effective-deadline
                     7 9 deadlines))
      (setf (gethash :stream deadlines) 10)
      (ensure-equal 7
                    (http-kit/http2::%h2-server-effective-deadline
                     7 9 deadlines))
      (ensure-true
       (http-kit/http2::%h2-server-new-client-stream-p 3 1 streams))
      (ensure-false
       (http-kit/http2::%h2-server-new-client-stream-p 1 1 streams))
      (setf (gethash 3 streams) :existing)
      (ensure-false
       (http-kit/http2::%h2-server-new-client-stream-p 3 1 streams))
      (ensure-equal
       4
       (http-kit/http2::%h2-server-request-deadline 2 4 10 (lambda () 0)))
      (ensure-equal
       7
       (http-kit/http2::%h2-server-request-deadline 2 nil 10 (lambda () 5)))
      (ensure-equal
       nil
       (http-kit/http2::%h2-server-check-body-release 3 3 :stream))
      (let ((observed nil))
        (handler-case
            (http-kit/http2::%h2-server-check-body-release 3 2 :stream)
          (http-protocol-error (condition)
            (setf observed condition)))
        (ensure-true (typep observed 'http-protocol-error))
        (ensure-equal :http2-server (http-kit:http-error-operation observed))
        (ensure-equal :stream
                      (http-kit:http-protocol-error-detail observed))))))

#+sbcl
(progn
  (defclass server-credit-observer-stream (binary-session-stream)
    ((boundary :initarg :boundary :reader server-credit-boundary)
     (before-credit-output :initform nil :accessor server-before-credit-output)
     (boundary-reads :initform 0 :accessor server-credit-boundary-reads)))

  (defmethod sb-gray:stream-read-sequence :before
      ((stream server-credit-observer-stream) sequence &optional (start 0) end)
    (declare (ignore sequence start end))
    (when (= (binary-session-input-position stream)
             (server-credit-boundary stream))
      (incf (server-credit-boundary-reads stream))
      (setf (server-before-credit-output stream)
            (copy-seq (binary-session-output stream)))))

  (defun server-peer-window-request (window)
    (concatenate-octets
     (h2-preface)
     (h2-frame 4 0 0 (octets 0 4 0 0 0 window))
     (h2-frame 1 5 1
               (h2-header-block (cons ":method" "GET")
                                (cons ":scheme" "https")
                                (cons ":authority" "example.com")
                                (cons ":path" "/")))))

  (deftest http2-server-peer-window-zero-blocks-data-at-eof
    (let ((stream (make-instance 'binary-session-stream
                                 :input (server-peer-window-request 0)))
          (observed nil)
          (handled 0)
          (closed 0))
      (signals http-protocol-error
        (handler-bind ((http-protocol-error
                         (lambda (condition) (setf observed condition))))
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (declare (ignore request))
             (incf handled)
             (make-http-response :status 200 :body (ascii "ok")))
           :close-stream (lambda (closed-stream)
                           (ensure-equal stream closed-stream)
                           (incf closed)))))
      (ensure-equal 1 handled)
      (ensure-equal 1 closed)
      (ensure-true (typep observed 'http-protocol-error))
      (ensure-equal :http2-server (http-kit:http-error-operation observed))
      (ensure-equal "HTTP/2 peer closed while response was flow-control blocked"
                    (http-kit:http-error-message observed))
      (let ((state (http-kit:http-protocol-error-detail observed)))
        (ensure-equal 1 (http-kit/http2::%h2-server-stream-id state))
        (ensure-equal 0 (http-kit/http2::%h2-server-stream-send-window state))
        (ensure-false (http-kit/http2::%h2-server-stream-responded-p state)))
      (let* ((frames (server-output-frames (binary-session-output stream)))
             (headers (find 1 frames :key #'http-kit/http2::%h2-frame-type))
             (goaway (find 7 frames :key #'http-kit/http2::%h2-frame-type)))
        (ensure-false (find 0 frames :key #'http-kit/http2::%h2-frame-type))
        (ensure-true headers)
        (ensure-equal 4 (http-kit/http2::%h2-frame-flags headers))
        (ensure-equal "200" (cdr (assoc ":status" (server-response-fields headers)
                                         :test #'string=)))
        (ensure-true goaway)
        (ensure-equal 1 (http-kit/http2::%h2-u32
                         (http-kit/http2::%h2-frame-payload goaway) 0))
        (ensure-equal 1 (http-kit/http2::%h2-u32
                         (http-kit/http2::%h2-frame-payload goaway) 4)))))

  (defun check-server-peer-window-resume (initial-window)
    (let* ((prefix (server-peer-window-request initial-window))
           (stream (make-instance
                    'server-credit-observer-stream
                    :boundary (length prefix)
                    :input (concatenate-octets
                            prefix
                            (h2-frame 8 0 1 (octets 0 0 0 (- 2 initial-window))))))
           (handled 0)
           (closed 0))
      (multiple-value-bind (count reason)
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (declare (ignore request))
             (incf handled)
             (make-http-response :status 200 :body (ascii "ok")))
           :close-stream (lambda (closed-stream)
                           (ensure-equal stream closed-stream)
                           (incf closed)))
        (ensure-equal 1 count)
        (ensure-equal :eof reason))
      (ensure-equal 1 handled)
      (ensure-equal 1 closed)
      (ensure-equal 1 (server-credit-boundary-reads stream))
      (let* ((before (server-output-frames (server-before-credit-output stream)))
             (headers (find 1 before :key #'http-kit/http2::%h2-frame-type))
             (early-data (remove-if-not
                          (lambda (frame)
                            (= 0 (http-kit/http2::%h2-frame-type frame)))
                          before)))
        (ensure-true headers)
        (ensure-equal 4 (http-kit/http2::%h2-frame-flags headers))
        (ensure-equal "200" (cdr (assoc ":status" (server-response-fields headers)
                                         :test #'string=)))
        (if (zerop initial-window)
            (ensure-false early-data)
            (progn
              (ensure-equal 1 (length early-data))
              (ensure-equal "o" (octets-as-string
                                 (http-kit/http2::%h2-frame-payload
                                  (first early-data))))
              (ensure-equal 0 (http-kit/http2::%h2-frame-flags
                               (first early-data))))))
      (let* ((frames (server-output-frames (binary-session-output stream)))
             (data (remove-if-not
                    (lambda (frame) (= 0 (http-kit/http2::%h2-frame-type frame)))
                    frames)))
        (ensure-equal (1+ initial-window) (length data))
        (ensure-true (every (lambda (frame)
                             (= 1 (http-kit/http2::%h2-frame-stream-id frame)))
                           data))
        (ensure-equal "ok" (octets-as-string
                            (apply #'concatenate-octets
                                   (mapcar #'http-kit/http2::%h2-frame-payload
                                           data))))
        (ensure-equal 1 (http-kit/http2::%h2-frame-flags (car (last data))))
        (ensure-false (find 7 frames :key #'http-kit/http2::%h2-frame-type)))))

  (deftest http2-server-peer-window-zero-resumes-after-update
    (check-server-peer-window-resume 0))

  (deftest http2-server-peer-window-one-sends-only-granted-credit
    (check-server-peer-window-resume 1)))

#+sbcl
(progn
  (defun check-server-partial-response-control (control)
    (let* ((prefix (server-peer-window-request 1))
           (input (concatenate-octets
                   prefix
                   (public-server-request-frame 3)
                   (ecase control
                     (:reset (h2-frame 3 0 1 (octets 0 0 0 8)))
                     (:credit (h2-frame 8 0 1 (octets 0 0 0 1))))))
           (stream (make-instance 'server-credit-observer-stream
                                  :boundary (length prefix) :input input))
           (calls 0)
           (closed 0)
           (notified nil))
      (multiple-value-bind (count reason)
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (ensure-equal "GET" (http-request-method request))
             (incf calls)
             (if (= calls 1)
                 (make-http-response :status 200 :body (ascii "xy"))
                 (make-http-response :status 204)))
           :on-error (lambda (condition request)
                       (declare (ignore request)) (setf notified condition))
           :close-stream (lambda (s)
                           (ensure-true (eq s stream))
                           (incf closed)
                           (close s)))
        (ensure-equal (ecase control (:reset 1) (:credit 2)) count)
        (ensure-equal :eof reason))
      (ensure-equal 2 calls)
      (ensure-equal 1 closed)
      (ensure-false (open-stream-p stream))
      (ensure-false notified)
      (ensure-equal (length input) (binary-session-input-position stream))
      (ensure-equal 1 (server-credit-boundary-reads stream))
      (let ((before (server-output-frames (server-before-credit-output stream))))
        (ensure-equal '(4 4 1 0) (mapcar #'http-kit/http2::%h2-frame-type before))
        (ensure-equal '(0 0 1 1) (mapcar #'http-kit/http2::%h2-frame-stream-id before))
        (ensure-equal '(0 1 4 0) (mapcar #'http-kit/http2::%h2-frame-flags before))
        (ensure-equal (ascii "x")
                      (http-kit/http2::%h2-frame-payload (car (last before)))))
      (let* ((frames (server-output-frames (binary-session-output stream)))
             (headers (remove-if-not
                       (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                       frames))
             (data (remove-if-not
                    (lambda (frame) (= 0 (http-kit/http2::%h2-frame-type frame)))
                    frames)))
        (ensure-equal (ecase control (:reset '(4 4 1 0 1)) (:credit '(4 4 1 0 0 1)))
                      (mapcar #'http-kit/http2::%h2-frame-type frames))
        (ensure-equal (ecase control (:reset '(0 0 1 1 3)) (:credit '(0 0 1 1 1 3)))
                      (mapcar #'http-kit/http2::%h2-frame-stream-id frames))
        (ensure-equal (ecase control (:reset '(0 1 4 0 5)) (:credit '(0 1 4 0 1 5)))
                      (mapcar #'http-kit/http2::%h2-frame-flags frames))
        (ensure-equal '("200" "204")
                      (mapcar (lambda (frame)
                                (cdr (assoc ":status" (server-response-fields frame)
                                            :test #'string=)))
                              headers))
        (ensure-equal (ecase control (:reset '("x")) (:credit '("x" "y")))
                      (mapcar (lambda (frame)
                                (octets-as-string (http-kit/http2::%h2-frame-payload frame)))
                              data))
        (ensure-equal (ascii (ecase control (:reset "x") (:credit "xy")))
                      (apply #'concatenate-octets
                             (mapcar #'http-kit/http2::%h2-frame-payload data)))
        (ensure-false (find 7 frames :key #'http-kit/http2::%h2-frame-type)))))

  (deftest http2-server-public-partial-response-reset
    (check-server-partial-response-control :reset))

  (deftest http2-server-public-partial-response-window-update
    (check-server-partial-response-control :credit)))

  (deftest http2-server-clamps-peer-hpack-table-size
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets 0 1 0 0 0 0))
                   (h2-frame 1 4 1
                             (octets #x3f #xff #xff #xe3 #x1d
                                     #x82 #x87))))
           (stream (make-instance 'binary-session-stream :input input)))
      (signals http-protocol-error
        (http-kit/http2:serve-http2-session
         stream
         (lambda (request)
           (declare (ignore request))
           (make-http-response :status 204))
         :max-hpack-table-size 0))))

  (deftest http2-server-control-budget-counts-all-but-rst-stream
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 5 1
                             (h2-header-block
                              (cons ":method" "GET")
                              (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/")))
                   (h2-frame 6 0 0 (octets 1 2 3 4 5 6 7 8))
                   (h2-frame 6 0 0 (octets 9 10 11 12 13 14 15 16))))
           (stream (make-instance 'binary-session-stream :input input)))
      (signals http-protocol-error
        (http-kit/http2:serve-http2-session
         stream
         (lambda (request)
           (declare (ignore request))
           (make-http-response :status 204))
         :max-control-frames 2
         :max-control-window 10.0d0
         :close-stream nil))
      (let* ((frames (server-output-frames (binary-session-output stream)))
             (acks (count-if
                    (lambda (frame)
                      (and (= (http-kit/http2::%h2-frame-type frame) 6)
                           (= (http-kit/http2::%h2-frame-flags frame) 1)))
                    frames))
             (goaway (find-if
                      (lambda (frame)
                        (= (http-kit/http2::%h2-frame-type frame) 7))
                      frames)))
        (ensure-equal 1 acks)
        (ensure-true goaway)
        (ensure-equal 1
                      (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload goaway) 0))
        (ensure-equal 11
                      (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload goaway) 4)))))
  #+sbcl
(progn
  (defclass server-deadline-blocking-read-stream (binary-session-stream)
    ((block-at :initarg :block-at :reader server-deadline-read-block-at)))

  (defmethod sb-gray:stream-read-sequence :around
      ((stream server-deadline-blocking-read-stream) sequence
       &optional (start 0) end)
    (declare (ignore sequence start end))
    (when (>= (binary-session-input-position stream)
              (server-deadline-read-block-at stream))
      (sleep 10))
    (call-next-method))

  (defclass server-deadline-blocking-write-stream (binary-session-stream)
    ((write-calls :initform 0 :accessor server-deadline-write-calls)))

  (defmethod sb-gray:stream-write-sequence :around
      ((stream server-deadline-blocking-write-stream) sequence
       &optional (start 0) end)
    (declare (ignore sequence start end))
    (when (>= (server-deadline-write-calls stream) 2)
      (sleep 10))
    (incf (server-deadline-write-calls stream))
    (call-next-method))

  (defun server-deadline-request-frame (stream-id flags)
    (h2-frame
     1 flags stream-id
     (h2-header-block
      (cons ":method" "GET")
      (cons ":scheme" "https")
      (cons ":authority" "example.test")
      (cons ":path" "/deadline"))))

  (deftest http2-server-request-timeout-late-handler-sends-no-response
    (let* ((now 0)
           (handler-calls 0)
           (observed nil)
           (input
             (concatenate-octets
              (h2-preface)
              (h2-frame 4 0 0 (octets))
              (server-deadline-request-frame 1 5)))
           (stream (make-instance 'binary-session-stream :input input)))
      (handler-case
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (declare (ignore request))
             (incf handler-calls)
             (setf now 2)
             (values (make-http-response :status 200)
                     (list (make-http-response :status 103))))
           :request-timeout 1
           :clock-function (lambda () now)
           :close-stream (lambda (closed-stream)
                           (declare (ignore closed-stream))))
        (http-timeout (condition)
          (setf observed condition)))
      (ensure-true (typep observed 'http-timeout))
      (ensure-equal :write (http-timeout-kind observed))
      (ensure-equal 1 handler-calls)
      (ensure-equal
       0
       (count-if (lambda (frame)
                   (= (http-kit/http2::%h2-frame-type frame) 1))
                 (server-output-frames (binary-session-output stream))))))

  (deftest http2-server-request-timeout-interrupts-frame-payload-read
    (let* ((frame (server-deadline-request-frame 1 4))
           (input
             (concatenate-octets
              (h2-preface)
              (h2-frame 4 0 0 (octets))
              (subseq frame 0 9)))
           (stream
             (make-instance 'server-deadline-blocking-read-stream
                            :input input
                            :block-at (length input)))
           (handler-calls 0)
           (observed nil))
      (handler-case
          (sb-ext:with-timeout 1
            (http-kit/http2:serve-http2-session
             stream
             (lambda (request)
               (declare (ignore request))
               (incf handler-calls)
               (make-http-response :status 204))
             :request-timeout 0.05d0
             :clock-function (lambda () 0)
             :close-stream (lambda (closed-stream)
                             (declare (ignore closed-stream)))))
        (http-timeout (condition)
          (setf observed condition)))
      (ensure-true (typep observed 'http-timeout))
      (ensure-equal :read (http-timeout-kind observed))
      (ensure-equal 0 handler-calls)
      (ensure-equal
       0
       (count-if (lambda (frame)
                   (= (http-kit/http2::%h2-frame-type frame) 1))
                 (server-output-frames (binary-session-output stream))))))

  (deftest http2-server-request-timeout-interrupts-response-write
    (let* ((input
             (concatenate-octets
              (h2-preface)
              (h2-frame 4 0 0 (octets))
              (server-deadline-request-frame 1 5)))
           (stream
             (make-instance 'server-deadline-blocking-write-stream
                            :input input))
           (handler-calls 0)
           (observed nil))
      (handler-case
          (sb-ext:with-timeout 1
            (http-kit/http2:serve-http2-session
             stream
             (lambda (request)
               (declare (ignore request))
               (incf handler-calls)
               (make-http-response :status 204))
             :request-timeout 0.05d0
             :clock-function (lambda () 0)
             :close-stream (lambda (closed-stream)
                             (declare (ignore closed-stream)))))
        (http-timeout (condition)
          (setf observed condition)))
      (ensure-true (typep observed 'http-timeout))
      (ensure-equal :write (http-timeout-kind observed))
      (ensure-equal 1 handler-calls)
      (ensure-equal
       0
       (count-if (lambda (frame)
                   (= (http-kit/http2::%h2-frame-type frame) 1))
                 (server-output-frames (binary-session-output stream))))))

  (deftest http2-server-idle-timeout-interrupts-initial-preface-read
    (let* ((stream
             (make-instance 'server-deadline-blocking-read-stream
                            :input (octets)
                            :block-at 0))
           (handler-calls 0)
           (observed nil))
      (handler-case
          (sb-ext:with-timeout 1
            (http-kit/http2:serve-http2-session
             stream
             (lambda (request)
               (declare (ignore request))
               (incf handler-calls)
               (make-http-response :status 204))
             :idle-timeout 0.05d0
             :clock-function (lambda () 0)
             :close-stream (lambda (closed-stream)
                             (declare (ignore closed-stream)))))
        (http-timeout (condition)
          (setf observed condition)))
      (ensure-true (typep observed 'http-timeout))
      (ensure-equal :read (http-timeout-kind observed))
      (ensure-equal 0 handler-calls)
      (ensure-equal 0 (length (binary-session-output stream)))))

  (deftest http2-server-idle-timeout-interrupts-initial-settings-read
    (let* ((preface (h2-preface))
           (stream
             (make-instance 'server-deadline-blocking-read-stream
                            :input preface
                            :block-at (length preface)))
           (handler-calls 0)
           (observed nil))
      (handler-case
          (sb-ext:with-timeout 1
            (http-kit/http2:serve-http2-session
             stream
             (lambda (request)
               (declare (ignore request))
               (incf handler-calls)
               (make-http-response :status 204))
             :idle-timeout 0.05d0
             :clock-function (lambda () 0)
             :close-stream (lambda (closed-stream)
                             (declare (ignore closed-stream)))))
        (http-timeout (condition)
          (setf observed condition)))
      (ensure-true (typep observed 'http-timeout))
      (ensure-equal :read (http-timeout-kind observed))
      (ensure-equal 0 handler-calls)
      (ensure-equal 0 (length (binary-session-output stream)))))

  (deftest http2-server-idle-timeout-interrupts-next-frame-read
    (let* ((input
             (concatenate-octets
              (h2-preface)
              (h2-frame 4 0 0 (octets))
              (server-deadline-request-frame 1 5)))
           (stream
             (make-instance 'server-deadline-blocking-read-stream
                            :input input
                            :block-at (length input)))
           (handler-calls 0)
           (observed nil))
      (handler-case
          (sb-ext:with-timeout 1
            (http-kit/http2:serve-http2-session
             stream
             (lambda (request)
               (declare (ignore request))
               (incf handler-calls)
               (make-http-response :status 204))
             :idle-timeout 0.05d0
             :clock-function (lambda () 0)
             :close-stream (lambda (closed-stream)
                             (declare (ignore closed-stream)))))
        (http-timeout (condition)
          (setf observed condition)))
      (ensure-true (typep observed 'http-timeout))
      (ensure-equal :read (http-timeout-kind observed))
      (ensure-equal 1 handler-calls)
      (ensure-equal
       1
       (count-if (lambda (frame)
                   (and (= (http-kit/http2::%h2-frame-type frame) 1)
                        (= (http-kit/http2::%h2-frame-stream-id frame) 1)))
                 (server-output-frames (binary-session-output stream))))))

  (deftest http2-server-shutdown-drains-active-stream-and-rejects-newer-streams
    (let* ((headers
             (server-deadline-request-frame 1 4))
           (first-request-prefix
             (concatenate-octets
              (h2-preface)
              (h2-frame 4 0 0 (octets))
              headers))
           (data-frame (h2-frame 0 1 1 (octets)))
           (newer-request (server-deadline-request-frame 3 5))
           (input (concatenate-octets
                   first-request-prefix data-frame newer-request))
           (stream (make-instance 'binary-session-stream :input input))
           (stop-position (length first-request-prefix))
           (handler-calls 0))
      (multiple-value-bind (count reason)
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (declare (ignore request))
             (incf handler-calls)
             (make-http-response :status 204))
           :shutdown-p
           (lambda ()
             (>= (binary-session-input-position stream) stop-position))
           :close-stream (lambda (closed-stream)
                           (declare (ignore closed-stream))))
        (let* ((frames (server-output-frames (binary-session-output stream)))
               (goaway (find-if
                        (lambda (frame)
                          (= (http-kit/http2::%h2-frame-type frame) 7))
                        frames)))
          (ensure-equal 1 count)
          (ensure-equal :goaway reason)
          (ensure-equal 1 handler-calls)
          (ensure-true goaway)
          (ensure-equal
           1
           (http-kit/http2::%h2-u32
            (http-kit/http2::%h2-frame-payload goaway) 0))
          (ensure-equal
           1
           (count-if (lambda (frame)
                       (and (= (http-kit/http2::%h2-frame-type frame) 1)
                            (= (http-kit/http2::%h2-frame-stream-id frame) 1)))
                     frames))
          (ensure-equal
           0
           (count-if (lambda (frame)
                       (and (= (http-kit/http2::%h2-frame-type frame) 1)
                            (= (http-kit/http2::%h2-frame-stream-id frame) 3)))
                     frames)
          (ensure-equal
           (+ stop-position (length data-frame))
           (binary-session-input-position stream))))))))
  #+sbcl
(progn
  (deftest http2-server-request-timeout-interrupts-frame-header-read
    (let* ((frame (server-deadline-request-frame 1 5))
           (input
             (concatenate-octets
              (h2-preface)
              (h2-frame 4 0 0 (octets))
              (subseq frame 0 1)))
           (stream
             (make-instance 'server-deadline-blocking-read-stream
                            :input input
                            :block-at (length input)))
           (handler-calls 0)
           (observed nil))
      (handler-case
          (sb-ext:with-timeout 1
            (http-kit/http2:serve-http2-session
             stream
             (lambda (request)
               (declare (ignore request))
               (incf handler-calls)
               (make-http-response :status 204))
             :request-timeout 0.05d0
             :clock-function (lambda () 0)
             :close-stream (lambda (closed-stream)
                             (declare (ignore closed-stream)))))
        (http-timeout (condition)
          (setf observed condition)))
      (ensure-true (typep observed 'http-timeout))
      (ensure-equal :read (http-timeout-kind observed))
      (ensure-equal 0 handler-calls)
      (ensure-equal
       0
       (count-if (lambda (frame)
                   (= (http-kit/http2::%h2-frame-type frame) 1))
                 (server-output-frames (binary-session-output stream)))))))
  #+sbcl
(progn
  (defclass server-deadline-idle-boundary-stream (binary-session-stream)
    ((frame-start-position
      :initarg :frame-start-position
      :reader server-deadline-idle-frame-start-position)
     (clock-value
      :initform 0
      :accessor server-deadline-idle-clock-value)
     (waited-p
      :initform nil
      :accessor server-deadline-idle-waited-p)))

  (defmethod sb-gray:stream-read-sequence :around
      ((stream server-deadline-idle-boundary-stream) sequence
       &optional (start 0) end)
    (declare (ignore sequence start end))
    (let ((position (binary-session-input-position stream)))
      (cond
        ((= position (server-deadline-idle-frame-start-position stream))
         (setf (server-deadline-idle-clock-value stream) 0.04d0))
        ((and (= position
                 (1+ (server-deadline-idle-frame-start-position stream)))
              (not (server-deadline-idle-waited-p stream)))
         (setf (server-deadline-idle-clock-value stream) 0.06d0
               (server-deadline-idle-waited-p stream) t)
         (sleep 0.1d0))))
    (call-next-method))

  (deftest http2-server-idle-expiry-does-not-cap-new-request-timeout
    (let* ((first-frame (server-deadline-request-frame 1 5))
           (second-frame (server-deadline-request-frame 3 5))
           (frame-start-position
             (+ (length (h2-preface))
                (length (h2-frame 4 0 0 (octets)))
                (length first-frame)))
           (input
             (concatenate-octets
              (h2-preface)
              (h2-frame 4 0 0 (octets))
              first-frame
              second-frame))
           (stream
             (make-instance 'server-deadline-idle-boundary-stream
                            :input input
                            :frame-start-position frame-start-position))
           (handler-calls 0)
           (observed nil)
           (result-count nil)
           (termination nil))
      (handler-case
          (sb-ext:with-timeout 1
            (multiple-value-setq (result-count termination)
              (http-kit/http2:serve-http2-session
               stream
               (lambda (request)
                 (declare (ignore request))
                 (incf handler-calls)
                 (make-http-response :status 204))
               :request-timeout 1
               :idle-timeout 0.05d0
               :clock-function
               (lambda () (server-deadline-idle-clock-value stream))
               :close-stream (lambda (closed-stream)
                               (declare (ignore closed-stream))))))
        (http-timeout (condition)
          (setf observed condition)))
      (ensure-true (null observed))
      (ensure-equal 0.06d0 (server-deadline-idle-clock-value stream))
      (ensure-equal 2 result-count)
      (ensure-equal :eof termination)
      (ensure-equal 2 handler-calls))))

#+sbcl
(progn
  (defun check-server-public-request-validation
      (fields &key name reason method (target "/") enable-connect-p
                   (operation :http2-server))
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 5 1 (apply #'h2-header-block fields))))
           (stream (make-instance 'binary-session-stream :input input))
           (handled 0)
           (closed 0)
           (notified nil)
           (observed nil)
           (seen-request nil))
      (handler-case
          (multiple-value-bind (count termination)
              (http-kit/http2:serve-http2-session
               stream
               (lambda (request)
                 (incf handled)
                 (setf seen-request request)
                 (make-http-response :status 204))
               :enable-connect-p enable-connect-p
               :on-error (lambda (condition request)
                           (setf notified condition)
                           (ensure-false request))
               :close-stream (lambda (closed-stream)
                               (ensure-true (eq stream closed-stream))
                               (incf closed)
                               (close closed-stream)))
            (ensure-equal 1 count)
            (ensure-equal :eof termination))
        (http-invalid-header (condition) (setf observed condition)))
      (ensure-equal 1 closed)
      (ensure-false (open-stream-p stream))
      (let* ((frames (server-output-frames (binary-session-output stream)))
             (headers (remove-if-not
                       (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                       frames))
             (goaway (find 7 frames :key #'http-kit/http2::%h2-frame-type)))
        (if name
            (progn
              (ensure-true (typep observed 'http-invalid-header))
              (ensure-equal operation (http-error-operation observed))
              (ensure-equal name (http-invalid-header-name observed))
              (ensure-equal reason (http-invalid-header-reason observed))
              (ensure-false (http-protocol-error-detail observed))
              (ensure-true (eq observed notified))
              (ensure-equal 0 handled)
              (ensure-false headers)
              (ensure-true goaway)
              (ensure-equal 1 (http-kit/http2::%h2-u32
                               (http-kit/http2::%h2-frame-payload goaway) 0))
              (ensure-equal 1 (http-kit/http2::%h2-u32
                               (http-kit/http2::%h2-frame-payload goaway) 4)))
            (progn
              (ensure-false observed)
              (ensure-false notified)
              (ensure-equal 1 handled)
              (ensure-equal method (http-request-method seen-request))
              (ensure-equal target (http-request-target seen-request))
              (ensure-equal 1 (length headers))
              (ensure-equal "204"
                            (cdr (assoc ":status"
                                        (server-response-fields (first headers))
                                        :test #'string=)))
              (ensure-false goaway))))))

  (deftest http2-server-public-pseudo-header-validation
    (let ((fields '((":method" . "GET") (":scheme" . "https")
                    (":authority" . "example.com") (":path" . "/"))))
      (check-server-public-request-validation fields :method "GET")
      (dolist (case
               (list
                (list (cons '("x-check" . "ok") fields) ":method"
                      "Pseudo-fields must precede regular fields")
                (list (append fields '((":method" . "GET"))) ":method"
                      "Duplicate pseudo-field")
                (list (append fields '((":unknown" . "value"))) ":unknown"
                      "Unknown request pseudo-field")
                (list (append fields '(("connection" . "close"))) "connection"
                      :connection-specific :http2-headers)
                (list (cdr fields) ":method" "Missing or invalid method")
                (list (cons '(":method" . "G ET") (cdr fields)) ":method"
                      "Missing or invalid method")))
        (destructuring-bind (invalid name reason &optional (operation :http2-server)) case
          (check-server-public-request-validation
           invalid :name name :reason reason :operation operation)))))

  (deftest http2-server-public-target-validation
    (let ((fields '((":method" . "GET") (":scheme" . "https")
                    (":authority" . "example.com") (":path" . "/"))))
      (check-server-public-request-validation fields :method "GET")
      (dolist (case
               '((":authority" nil "Missing authority")
                 (":scheme" nil "Missing scheme")
                 (":path" nil "Missing path")
                 (":path" "relative" "Path must be origin-form")
                 (":authority" "[broken" "Invalid authority")))
        (destructuring-bind (name value reason) case
          (check-server-public-request-validation
           (loop for field in fields
                 for replacement = (if (string= name (car field))
                                       (and value (cons name value))
                                       field)
                 when replacement collect replacement)
           :name name :reason reason)))))

  (deftest http2-server-public-connect-validation
    (check-server-public-request-validation
     '((":method" . "CONNECT") (":authority" . "example.com:443"))
     :method "CONNECT" :target "example.com:443")
    (dolist (case
             '((((":method" . "CONNECT") (":authority" . "example.com:443")
                 (":path" . "/"))
                ":path" "CONNECT must use authority-form")
               (((":method" . "GET") (":scheme" . "https")
                 (":authority" . "example.com") (":path" . "/")
                 (":protocol" . "websocket"))
                ":protocol" "Only CONNECT may carry a valid :protocol")
               (((":method" . "CONNECT") (":authority" . "example.com")
                 (":protocol" . "bad protocol"))
                ":protocol" "Only CONNECT may carry a valid :protocol")
               (((":method" . "CONNECT") (":authority" . "example.com")
                 (":path" . "/") (":protocol" . "websocket"))
                ":scheme" "Extended CONNECT requires :scheme")))
      (destructuring-bind (fields name reason) case
        (check-server-public-request-validation
         fields :name name :reason reason :enable-connect-p t))))

  (defun check-server-public-response-validation
      (response information &key detail message)
    (let* ((input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (h2-frame 1 5 1
                             (h2-header-block
                              (cons ":method" "GET") (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/")))))
           (stream (make-instance 'binary-session-stream :input input))
           (handled 0)
           (closed 0)
           (seen-request nil)
           (notified nil)
           (observed nil))
      (handler-case
          (multiple-value-bind (count termination)
              (http-kit/http2:serve-http2-session
               stream
               (lambda (request)
                 (incf handled)
                 (setf seen-request request)
                 (values response information))
               :on-error (lambda (condition request)
                           (ensure-true (eq request seen-request))
                           (setf notified condition))
               :close-stream (lambda (closed-stream)
                               (ensure-true (eq stream closed-stream))
                               (incf closed)
                               (close closed-stream)))
            (ensure-equal 1 count)
            (ensure-equal :eof termination))
        (http-protocol-error (condition) (setf observed condition)))
      (ensure-equal 1 handled)
      (ensure-equal 1 closed)
      (ensure-false (open-stream-p stream))
      (let* ((frames (server-output-frames (binary-session-output stream)))
             (headers (remove-if-not
                       (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                       frames))
             (goaway (find 7 frames :key #'http-kit/http2::%h2-frame-type)))
        (if message
            (progn
              (ensure-true (typep observed 'http-protocol-error))
              (ensure-equal :http2-server (http-error-operation observed))
              (ensure-equal message (http-error-message observed))
              (ensure-true (eq detail (http-protocol-error-detail observed)))
              (ensure-true (eq observed notified))
              (ensure-false headers)
              (ensure-false (find 0 frames :key #'http-kit/http2::%h2-frame-type))
              (ensure-true goaway)
              (ensure-equal 1 (http-kit/http2::%h2-u32
                               (http-kit/http2::%h2-frame-payload goaway) 0))
              (ensure-equal 1 (http-kit/http2::%h2-u32
                               (http-kit/http2::%h2-frame-payload goaway) 4)))
            (progn
              (ensure-false observed)
              (ensure-false notified)
              (ensure-equal '("103" "204")
                            (mapcar
                             (lambda (frame)
                               (cdr (assoc ":status" (server-response-fields frame)
                                           :test #'string=)))
                             headers))
              (ensure-equal 4 (http-kit/http2::%h2-frame-flags (first headers)))
              (ensure-equal 5 (http-kit/http2::%h2-frame-flags (second headers)))
              (ensure-false goaway))))))

  (deftest http2-server-public-response-chunk-validation
    (check-server-public-response-validation
     (make-http-response :status 204)
     (list (make-http-response :status 103)))
    (dolist (chunk (list "bad" (make-array '(1 1) :initial-element 90)
                        (vector 256)))
      (let ((chunks (list chunk)))
        (check-server-public-response-validation
         (make-http-response-stream
          :status 200 :body-function (lambda () (pop chunks)))
         nil :detail chunk
         :message (if (and (arrayp chunk) (= 1 (array-rank chunk))
                           (not (stringp chunk)))
                      "HTTP/2 body chunks must contain octets"
                      "HTTP/2 body chunks must be one-dimensional octet arrays")))))

  (deftest http2-server-public-response-role-validation
    (check-server-public-response-validation
     (make-http-response :status 204)
     (list (make-http-response :status 103)))
    (check-server-public-response-validation
     :not-a-response nil :detail :not-a-response
     :message "HTTP/2 handler must return an HTTP response")
    (check-server-public-response-validation
     (make-http-response :status 204) :not-a-list :detail :not-a-list
     :message "HTTP/2 informational responses must be supplied as a list")
    (dolist (status '(101 200))
      (check-server-public-response-validation
       (make-http-response :status 204)
       (list (make-http-response :status status)) :detail status
       :message "An HTTP/2 informational response must have status 100-199 other than 101"))
    (check-server-public-response-validation
     (make-http-response :status 103) nil :detail 103
     :message "The final HTTP/2 response must have status 200 or greater"))

  (deftest http2-server-public-uncollected-body-success
    (let* ((input (concatenate-octets
                   (h2-preface) (h2-frame 4 0 0 (octets))
                   (h2-frame 1 4 1
                             (h2-header-block
                              (cons ":method" "POST") (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/upload")
                              (cons "content-length" "1")))
                   (h2-frame 0 1 1 (octets 90))))
           (stream (make-instance 'binary-session-stream :input input))
           (handled 0)
           (closed 0)
           (chunks '()))
      (multiple-value-bind (count termination)
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (incf handled)
             (ensure-equal "POST" (http-request-method request))
             (ensure-equal "/upload" (http-request-target request))
             (ensure-equal "1" (http-header-value (http-request-headers request)
                                                 "content-length"))
             (ensure-equal 0 (length (http-request-body request)))
             (make-http-response :status 204))
           :collect-body-p nil
           :on-body-chunk (lambda (state chunk)
                            (declare (ignore state))
                            (ensure-equal 0 handled)
                            (push (octets-as-string chunk) chunks))
           :close-stream (lambda (closed-stream)
                           (ensure-true (eq stream closed-stream))
                           (incf closed)
                           (close closed-stream)))
        (ensure-equal 1 count)
        (ensure-equal :eof termination))
      (ensure-equal 1 handled)
      (ensure-equal 1 closed)
      (ensure-equal '("Z") chunks)
      (ensure-false (open-stream-p stream))
      (let* ((frames (server-output-frames (binary-session-output stream)))
             (headers (find 1 frames :key #'http-kit/http2::%h2-frame-type)))
        (ensure-true headers)
        (ensure-equal "204"
                      (cdr (assoc ":status" (server-response-fields headers)
                                  :test #'string=)))
        (ensure-false (find 7 frames :key #'http-kit/http2::%h2-frame-type))))))

#+sbcl
(progn
  (defun check-server-public-frame-validation
      (stream-id &key window-increment error-detail)
    (let* ((window-payload (make-array 4 :element-type '(unsigned-byte 8)
                                       :initial-element 0))
           (input (concatenate-octets
                   (h2-preface)
                   (h2-frame 4 0 0 (octets))
                   (if window-increment
                       (progn
                         (http-kit/http2::%h2-put-u32 window-payload 0 window-increment)
                         (h2-frame 8 0 0 window-payload))
                       (octets))
                   (h2-frame 1 5 stream-id
                             (h2-header-block
                              (cons ":method" "GET")
                              (cons ":scheme" "https")
                              (cons ":authority" "example.com")
                              (cons ":path" "/")))))
           (stream (make-instance 'binary-session-stream :input input))
           (handled 0)
           (closed 0)
           (observed nil)
           (notified nil))
      (handler-case
          (multiple-value-bind (count termination)
              (http-kit/http2:serve-http2-session
               stream
               (lambda (request)
                 (incf handled)
                 (ensure-equal "GET" (http-request-method request))
                 (ensure-equal "/" (http-request-target request))
                 (make-http-response :status 200 :body (ascii "ok")))
               :on-error (lambda (condition request)
                           (setf notified condition)
                           (ensure-false request))
               :close-stream (lambda (closed-stream)
                               (ensure-true (eq stream closed-stream))
                               (incf closed)
                               (close closed-stream)))
            (ensure-equal 1 count)
            (ensure-equal :eof termination))
        (http-protocol-error (condition) (setf observed condition)))
      (ensure-equal 1 closed)
      (ensure-false (open-stream-p stream))
      (let* ((frames (server-output-frames (binary-session-output stream)))
             (headers (remove-if-not
                       (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                       frames))
             (data (remove-if-not
                    (lambda (frame) (= 0 (http-kit/http2::%h2-frame-type frame)))
                    frames))
             (goaway (find 7 frames :key #'http-kit/http2::%h2-frame-type)))
        (if error-detail
            (progn
              (ensure-true (typep observed 'http-protocol-error))
              (ensure-equal :http2-server (http-error-operation observed))
              (ensure-equal error-detail (http-protocol-error-detail observed))
              (ensure-equal 1 (http-kit::http-protocol-error-http2-error-code observed))
              (ensure-true (eq observed notified))
              (ensure-equal 0 handled)
              (ensure-false headers)
              (ensure-false data)
              (ensure-true goaway)
              (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id goaway))
              (ensure-equal 0 (http-kit/http2::%h2-u32
                               (http-kit/http2::%h2-frame-payload goaway) 0))
              (ensure-equal 1 (http-kit/http2::%h2-u32
                               (http-kit/http2::%h2-frame-payload goaway) 4)))
            (progn
              (ensure-false observed)
              (ensure-false notified)
              (ensure-equal 1 handled)
              (ensure-equal 1 (length headers))
              (ensure-equal 1 (length data))
              (ensure-equal "200"
                            (cdr (assoc ":status"
                                        (server-response-fields (first headers))
                                        :test #'string=)))
              (ensure-equal "ok" (octets-as-string
                                 (http-kit/http2::%h2-frame-payload (first data))))
              (ensure-equal stream-id (http-kit/http2::%h2-frame-stream-id
                                      (first headers)))
              (ensure-equal stream-id (http-kit/http2::%h2-frame-stream-id
                                      (first data)))
              (ensure-equal 1 (logand 1 (http-kit/http2::%h2-frame-flags (first data))))
              (ensure-false goaway))))))

  (deftest http2-server-public-stream-id-boundaries
    (dolist (stream-id '(1 3))
      (check-server-public-frame-validation stream-id))
    (dolist (stream-id '(0 2))
      (check-server-public-frame-validation stream-id :error-detail stream-id)))

  (deftest http2-server-public-window-overflow-boundary
    (check-server-public-frame-validation 1 :window-increment #x7fff0000)
    (check-server-public-frame-validation
     1 :window-increment #x7fff0001
     :error-detail '(:connection 65535 #x7fff0001))))


#+sbcl
(progn
  (defun check-server-public-unknown-flags (flags &key data-p)
    (let* ((payload (if data-p (octets 0 255 7) (octets 0 1 2 3 4 5 6 7)))
           (fields (append
                    (list (cons ":method" (if data-p "POST" "GET"))
                          (cons ":scheme" "https")
                          (cons ":authority" "example.com")
                          (cons ":path" (if data-p "/upload" "/")))
                    (when data-p '(("content-length" . "3")))))
           (stream
             (make-instance
              'binary-session-stream
              :input (concatenate
                      '(vector (unsigned-byte 8))
                      (h2-preface) (h2-frame 4 0 0 (octets))
                      (if data-p
                          (concatenate '(vector (unsigned-byte 8))
                                       (h2-frame 1 4 1 (apply #'h2-header-block fields))
                                       (h2-frame 0 flags 1 payload))
                          (concatenate '(vector (unsigned-byte 8))
                                       (h2-frame 6 flags 0 payload)
                                       (h2-frame 1 5 1 (apply #'h2-header-block fields)))))))
           (handled 0)
           (closed 0)
           (notified nil)
           (chunks '()))
      (multiple-value-bind (count termination)
          (http-kit/http2:serve-http2-session
           stream
           (lambda (request)
             (incf handled)
             (ensure-equal (if data-p "POST" "GET") (http-request-method request))
             (ensure-equal (if data-p "/upload" "/") (http-request-target request))
             (ensure-equal (if data-p payload (octets)) (http-request-body request))
             (when data-p
               (ensure-equal "3" (http-header-value (http-request-headers request)
                                                   "content-length")))
             (make-http-response :status 204 :body (octets)))
           :on-body-chunk (lambda (state chunk)
                            (declare (ignore state))
                            (ensure-equal 0 handled)
                            (push chunk chunks))
           :on-error (lambda (condition request)
                       (declare (ignore request))
                       (setf notified condition))
           :close-stream (lambda (closed-stream)
                           (ensure-true (eq stream closed-stream))
                           (incf closed)
                           (close closed-stream)))
        (ensure-equal 1 count)
        (ensure-equal :eof termination))
      (ensure-equal 1 handled)
      (ensure-equal 1 closed)
      (ensure-false (open-stream-p stream))
      (ensure-false notified)
      (ensure-equal (length (binary-session-input stream))
                    (binary-session-input-position stream))
      (ensure-equal (if data-p 1 0) (length chunks))
      (when data-p (ensure-equal payload (first chunks)))
      (let* ((frames (server-output-frames (binary-session-output stream)))
             (headers (remove-if-not
                       (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                       frames))
             (pings (remove-if-not
                     (lambda (frame) (= 6 (http-kit/http2::%h2-frame-type frame)))
                     frames))
             (windows (remove-if-not
                       (lambda (frame) (= 8 (http-kit/http2::%h2-frame-type frame)))
                       frames)))
        (ensure-equal 1 (length headers))
        (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id (first headers)))
        (ensure-equal 5 (http-kit/http2::%h2-frame-flags (first headers)))
        (ensure-equal "204" (cdr (assoc ":status"
                                       (server-response-fields (first headers))
                                       :test #'string=)))
        (ensure-equal 1
                      (count-if
                       (lambda (frame)
                         (and (= 4 (http-kit/http2::%h2-frame-type frame))
                              (= 1 (http-kit/http2::%h2-frame-flags frame))))
                       frames))
        (ensure-false (find 7 frames :key #'http-kit/http2::%h2-frame-type))
        (ensure-false (find 0 frames :key #'http-kit/http2::%h2-frame-type))
        (if data-p
            (progn
              (ensure-false pings)
              (ensure-equal '(0 1) (mapcar #'http-kit/http2::%h2-frame-stream-id windows))
              (dolist (frame windows)
                (ensure-equal 3 (http-kit/http2::%h2-u32
                                 (http-kit/http2::%h2-frame-payload frame) 0))))
            (progn
              (ensure-false windows)
              (ensure-equal 1 (length pings))
              (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id (first pings)))
              (ensure-equal 1 (http-kit/http2::%h2-frame-flags (first pings)))
              (ensure-equal payload (http-kit/http2::%h2-frame-payload (first pings))))))))

  (deftest http2-server-public-ping-unknown-flags
    (check-server-public-unknown-flags 0)
    (check-server-public-unknown-flags #x20))

  (deftest http2-server-public-data-unknown-flags
    (check-server-public-unknown-flags 1 :data-p t)
    (check-server-public-unknown-flags #x21 :data-p t)))


#+sbcl
(progn
  (defun public-server-request-frame (&optional (stream-id 1) (flags 5))
    (h2-frame 1 flags stream-id
              (h2-header-block (cons ":method" "GET") (cons ":scheme" "https")
                               (cons ":authority" "example.com") (cons ":path" "/"))))

  (defun check-public-server-boundary
      (wire &key error-p detail frame-detail-p size-p (handled 0)
                 (last-stream 1) response information (statuses '("204"))
                 (body (octets)) trailer)
    (let* ((stream (make-instance
                    'binary-session-stream
                    :input (concatenate-octets (h2-preface) wire)))
           (calls 0) (closed 0) (notified nil) (notified-request nil)
           (seen nil)
           (result
             (handler-case
                 (multiple-value-list
                  (http-kit/http2:serve-http2-session
                   stream
                   (lambda (request)
                     (incf calls)
                     (setf seen request)
                     (ensure-equal "GET" (http-request-method request))
                     (ensure-equal "/" (http-request-target request))
                     (ensure-equal body (http-request-body request))
                     (ensure-equal trailer
                                   (http-header-value (http-request-trailers request) "x-end"))
                     (values (or response (make-http-response :status 204)) information))
                   :on-error (lambda (condition request)
                               (setf notified condition notified-request request))
                   :close-stream (lambda (closed-stream)
                                   (ensure-true (eq stream closed-stream))
                                   (incf closed)
                                   (close closed-stream))))
               (http-protocol-error (condition) condition))))
      (ensure-equal handled calls)
      (ensure-equal 1 closed)
      (ensure-false (open-stream-p stream))
      (ensure-equal (length (binary-session-input stream))
                    (binary-session-input-position stream))
      (if error-p
          (progn
            (ensure-true (typep result 'http-protocol-error))
            (ensure-true (eq result notified))
            (ensure-equal :http2-server (http-error-operation result))
            (if size-p
                (progn
                  (ensure-true (typep result 'http-size-limit-exceeded))
                  (ensure-equal :headers (http-kit:http-size-limit-exceeded-kind result))
                  (ensure-equal 128 (http-kit:http-size-limit-exceeded-limit result))
                  (ensure-equal 129 (http-kit:http-size-limit-exceeded-observed result))
                  (ensure-true (eq seen notified-request)))
                (if frame-detail-p
                    (let ((frame (http-protocol-error-detail result)))
                      (ensure-true (typep frame 'http-kit/http2::%h2-frame))
                      (ensure-equal 8 (http-kit/http2::%h2-frame-type frame))
                      (ensure-equal 0 (http-kit/http2::%h2-frame-flags frame))
                      (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id frame))
                      (ensure-equal (octets 0 0 1) (http-kit/http2::%h2-frame-payload frame)))
                    (ensure-equal detail (http-protocol-error-detail result)))))
          (progn
            (ensure-false notified)
            (ensure-equal handled (first result))
            (ensure-equal :eof (second result))))
      (let* ((frames (server-output-frames (binary-session-output stream)))
             (goaways (remove-if-not
                       (lambda (frame) (= 7 (http-kit/http2::%h2-frame-type frame))) frames))
             (headers (remove-if-not
                       (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame))) frames)))
        (ensure-equal statuses
                      (mapcar (lambda (frame)
                                (cdr (assoc ":status" (server-response-fields frame)
                                            :test #'string=)))
                              headers))
        (ensure-equal (if error-p 1 0) (length goaways))
        (when error-p
          (let ((frame (first goaways)))
            (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id frame))
            (ensure-equal 0 (http-kit/http2::%h2-frame-flags frame))
            (ensure-equal 8 (length (http-kit/http2::%h2-frame-payload frame)))
            (ensure-equal last-stream (http-kit/http2::%h2-u32
                                      (http-kit/http2::%h2-frame-payload frame) 0))
            (ensure-equal (if size-p 11 1) (http-kit/http2::%h2-u32
                                          (http-kit/http2::%h2-frame-payload frame) 4))))
        frames)))

  (deftest http2-server-public-settings-stream-window-boundary
    (dolist (overflow-p '(nil t))
      (check-public-server-boundary
       (concatenate-octets
        (h2-frame 4 0 0 (octets)) (public-server-request-frame 1 4)
        (h2-frame 8 0 1 (if overflow-p (octets 127 255 0 0) (octets 127 254 255 255)))
        (h2-frame 4 0 0 (octets 0 4 0 1 0 0))
        (unless overflow-p (h2-frame 0 1 1 (octets))))
       :error-p overflow-p :detail #x80000000 :handled (if overflow-p 0 1)
       :statuses (unless overflow-p '("204")))))

  (deftest http2-server-public-control-wire-boundary
    (let* ((payload (octets 0 1 2 3 4 5 6 7))
           (frames
             (check-public-server-boundary
              (concatenate-octets
               (h2-frame 4 0 0 (octets)) (h2-frame 6 0 0 payload)
               (h2-frame 8 0 0 (octets 0 0 0 1)) (public-server-request-frame))
              :handled 1))
           (ping (find 6 frames :key #'http-kit/http2::%h2-frame-type)))
      (ensure-true ping)
      (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id ping))
      (ensure-equal 1 (http-kit/http2::%h2-frame-flags ping))
      (ensure-equal payload (http-kit/http2::%h2-frame-payload ping)))
    (dolist (case '((6 1 (0 1 2 3 4 5 6 7) 1 nil)
                   (6 0 (0 1 2 3 4 5 6) 7 nil)
                   (8 0 (0 0 1) nil t)
                   (8 0 (0 0 0 0) 0 nil)))
      (destructuring-bind (type stream payload detail frame-p) case
        (check-public-server-boundary
         (concatenate-octets (h2-frame 4 0 0 (octets))
                            (h2-frame type 0 stream (apply #'octets payload)))
         :error-p t :detail detail :frame-detail-p frame-p :last-stream 0 :statuses nil))))

  (deftest http2-server-public-request-lifecycle-boundary
    (let ((settings (h2-frame 4 0 0 (octets)))
          (trailer (h2-header-block (cons "x-end" "yes"))))
      (check-public-server-boundary
       (concatenate-octets settings (public-server-request-frame 1 4)
                          (h2-frame 0 0 1 (octets 90)) (h2-frame 1 5 1 trailer))
       :handled 1 :body (octets 90) :trailer "yes")
      (check-public-server-boundary
       (concatenate-octets settings (public-server-request-frame 1 4)
                          (h2-frame 0 0 1 (octets 90)) (h2-frame 1 4 1 trailer))
       :error-p t :detail 1 :statuses nil)
      (check-public-server-boundary
       (concatenate-octets settings (public-server-request-frame)
                          (public-server-request-frame 3))
       :handled 2 :statuses '("204" "204"))
      (check-public-server-boundary
       (concatenate-octets settings (public-server-request-frame)
                          (public-server-request-frame))
       :error-p t :detail 1 :handled 1)
      (check-public-server-boundary
       (concatenate-octets settings (public-server-request-frame 1 4)
                          (h2-frame 3 0 1 (octets 0 0 0 8))
                          (public-server-request-frame 3))
       :handled 1)
      (check-public-server-boundary
       (concatenate-octets settings (public-server-request-frame 1 4)
                          (h2-frame 3 0 1 (octets 0 0 0 8))
                          (h2-frame 0 1 1 (octets 90)))
       :error-p t :detail 1 :statuses nil)))

  (deftest http2-server-public-response-boundaries
    (let ((request (concatenate-octets (h2-frame 4 0 0 (octets))
                                     (public-server-request-frame))))
      (check-public-server-boundary
       request :handled 1 :response (make-http-response :status 200)
       :information (list (make-http-response :status 103)) :statuses '("103" "200"))
      (dolist (information
               (list (make-http-response :status 103 :body (octets 90))
                     (make-http-response :status 103
                                         :trailers (list (make-http-header "x-end" "yes")))))
        (check-public-server-boundary
         request :handled 1 :error-p t :detail 103 :statuses nil
         :response (make-http-response :status 200) :information (list information))))
    (let ((request (concatenate-octets (h2-frame 4 0 0 (octets 0 6 0 0 0 128))
                                     (public-server-request-frame))))
      (dolist (over-p '(nil t))
        (let* ((value (make-string (if over-p 54 53) :initial-element #\x))
               (frames
                 (check-public-server-boundary
                  request :handled 1 :error-p over-p :size-p over-p
                  :statuses (unless over-p '("103" "200"))
                  :response (make-http-response :status 200)
                  :information (list (make-http-response
                                      :status 103 :headers (list (make-http-header "x" value)))))))
          (unless over-p
            (let ((headers (remove-if-not
                            (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame))) frames)))
              (ensure-equal value (cdr (assoc "x" (server-response-fields (first headers))
                                            :test #'string=)))
              (ensure-equal 4 (http-kit/http2::%h2-frame-flags (first headers)))
              (ensure-equal 5 (http-kit/http2::%h2-frame-flags (second headers)))))))
      (dolist (over-p '(nil t))
        (let* ((value (make-string (if over-p 96 95) :initial-element #\x))
               (frames
                 (check-public-server-boundary
                  request :handled 1 :error-p over-p :size-p over-p
                  :statuses (unless over-p '("200" nil))
                  :response (make-http-response
                             :status 200 :body (octets 90)
                             :trailers (list (make-http-header "x" value))))))
          (unless over-p
            (let ((data (find 0 frames :key #'http-kit/http2::%h2-frame-type))
                  (headers (remove-if-not
                            (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame))) frames)))
              (ensure-equal (octets 90) (http-kit/http2::%h2-frame-payload data))
              (ensure-equal 0 (http-kit/http2::%h2-frame-flags data))
              (ensure-equal value (cdr (assoc "x" (server-response-fields (second headers))
                                            :test #'string=)))
              (ensure-equal 5 (http-kit/http2::%h2-frame-flags (second headers))))))))))



#+sbcl
(progn
  (defun check-public-server-entry
      (input &key error-p detail frame-spec options (handled 0)
                  (termination :eof) (owned-p t) (error-code 1))
    (let* ((stream (make-instance 'binary-session-stream :input input))
           (calls 0) (closed 0) (notified nil) (notified-request nil)
           (result
             (handler-case
                 (multiple-value-list
                  (apply #'http-kit/http2:serve-http2-session
                         stream
                         (lambda (request)
                           (incf calls)
                           (ensure-equal "GET" (http-request-method request))
                           (ensure-equal "/" (http-request-target request))
                           (make-http-response :status 204))
                         :on-error (lambda (condition request)
                                     (setf notified condition notified-request request))
                         :close-stream (lambda (s)
                                         (ensure-true (eq s stream))
                                         (incf closed) (close s))
                         options))
               (http-protocol-error (condition) condition))))
      (ensure-equal handled calls)
      (ensure-equal (if owned-p 1 0) closed)
      (ensure-equal (not owned-p) (open-stream-p stream))
      (ensure-equal (if owned-p (length input) 0) (binary-session-input-position stream))
      (if error-p
          (progn
            (ensure-true (typep result 'http-protocol-error))
            (ensure-equal :http2-server (http-error-operation result))
            (ensure-true (eq (and owned-p result) notified))
            (ensure-false notified-request)
            (if frame-spec
                (destructuring-bind (type flags stream-id payload) frame-spec
                  (let ((frame (http-protocol-error-detail result)))
                    (ensure-true (typep frame 'http-kit/http2::%h2-frame))
                    (ensure-equal type (http-kit/http2::%h2-frame-type frame))
                    (ensure-equal flags (http-kit/http2::%h2-frame-flags frame))
                    (ensure-equal stream-id (http-kit/http2::%h2-frame-stream-id frame))
                    (ensure-equal payload (http-kit/http2::%h2-frame-payload frame))))
                (ensure-equal detail (http-protocol-error-detail result))))
          (progn
            (ensure-false notified)
            (ensure-equal handled (first result))
            (ensure-equal termination (second result))))
      (let* ((frames (server-output-frames (binary-session-output stream)))
             (goaways (remove-if-not (lambda (frame)
                                      (= 7 (http-kit/http2::%h2-frame-type frame))) frames))
             (headers (remove-if-not (lambda (frame)
                                      (= 1 (http-kit/http2::%h2-frame-type frame))) frames)))
        (ensure-equal handled (length headers))
        (dolist (frame headers)
          (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id frame))
          (ensure-equal 5 (http-kit/http2::%h2-frame-flags frame))
          (ensure-equal "204" (cdr (assoc ":status" (server-response-fields frame)
                                         :test #'string=))))
        (ensure-equal (if (and error-p owned-p) 1 0) (length goaways))
        (when goaways
          (let ((frame (first goaways)))
            (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id frame))
            (ensure-equal 0 (http-kit/http2::%h2-frame-flags frame))
            (ensure-equal 8 (length (http-kit/http2::%h2-frame-payload frame)))
            (ensure-equal 0 (http-kit/http2::%h2-u32 (http-kit/http2::%h2-frame-payload frame) 0))
            (ensure-equal error-code
                          (http-kit/http2::%h2-u32 (http-kit/http2::%h2-frame-payload frame) 4))))
        (unless owned-p (ensure-equal 0 (length (binary-session-output stream)))))
      (when (open-stream-p stream) (close stream))))

  (deftest http2-server-public-residual-control-shapes
    (let ((prefix (concatenate-octets (h2-preface) (h2-frame 4 0 0 (octets)))))
      (check-public-server-entry
       (concatenate-octets prefix
                           (h2-frame 2 0 1 (octets 0 0 0 0 10))
                           (h2-frame #x10 0 0 (octets 0 0 0 1))
                           (public-server-request-frame))
       :handled 1)
      (check-public-server-entry
       (concatenate-octets prefix (public-server-request-frame)
                           (h2-frame 7 0 0 (octets 0 0 0 1 0 0 0 0)))
       :handled 1 :termination :goaway)
      (check-public-server-entry
       (concatenate-octets prefix (h2-frame 4 0 1 (octets)))
       :error-p t :detail 1)
      (check-public-server-entry
       (concatenate-octets prefix (h2-frame 2 0 1 (octets 0 0 0 1 10)))
       :error-p t :detail 1)
      (dolist (spec '((3 0 0 (0 0 0 0)) (3 1 1 (0 0 0 0)) (3 0 1 (0 0 0))
                      (7 0 1 (0 0 0 0 0 0 0 0)) (7 1 0 (0 0 0 0 0 0 0 0))
                      (7 0 0 (0 0 0 0 0 0 0)) (7 0 0 (128 0 0 0 0 0 0 0))
                      (2 0 0 (0 0 0 1 10)) (2 1 1 (0 0 0 0 10))
                      (2 0 1 (0 0 0 0)) (2 0 1 (128 0 0 0 10))
                      (16 0 1 (0 0 0 1)) (16 1 0 (0 0 0 1)) (16 0 0 (0 0 0))))
        (destructuring-bind (type flags stream-id bytes) spec
          (let ((payload (apply #'octets bytes)))
            (check-public-server-entry
             (concatenate-octets prefix (h2-frame type flags stream-id payload))
             :error-p t :frame-spec (list type flags stream-id payload)))))))

  (deftest http2-server-public-entry-startup-boundary
    (let* ((settings (h2-frame 4 0 0 (octets)))
           (valid (concatenate-octets (h2-preface) settings (public-server-request-frame))))
      (dolist (window '(0 1/2))
        (check-public-server-entry valid :handled 1
                                  :options (list :max-reset-window window :max-control-window window)))
      (dolist (key '(:max-reset-window :max-control-window))
        (dolist (value '(-1 nil :invalid))
          (check-public-server-entry valid :error-p t :detail value
                                    :owned-p nil :options (list key value))))
      (let ((closed 0) (calls 0)
            (condition nil))
        (setf condition
              (handler-case
                  (http-kit/http2:serve-http2-session
                   nil (lambda (request) (declare (ignore request)) (incf calls))
                   :close-stream (lambda (stream) (declare (ignore stream)) (incf closed)))
                (http-protocol-error (caught) caught)))
        (ensure-true (typep condition 'http-protocol-error))
        (ensure-equal :http2-server (http-error-operation condition))
        (ensure-equal nil (http-protocol-error-detail condition))
        (ensure-equal 0 calls)
        (ensure-equal 0 closed))
      (check-public-server-entry (octets))
      (let ((bad-preface (make-array 24 :element-type '(unsigned-byte 8) :initial-element 0)))
        (check-public-server-entry bad-preface :error-p t :detail bad-preface))
      (check-public-server-entry (h2-preface) :error-p t :detail nil)
      (dolist (spec '((6 0 0 (0 1 2 3 4 5 6 7)) (4 1 0 ()) (4 0 1 ())))
        (destructuring-bind (type flags stream-id bytes) spec
          (let ((payload (apply #'octets bytes)))
            (check-public-server-entry
             (concatenate-octets (h2-preface) (h2-frame type flags stream-id payload))
             :error-p t :frame-spec (list type flags stream-id payload)))))
      (check-public-server-entry (concatenate-octets (h2-preface) settings)
                                :error-p t :detail '(4 1) :error-code 11
                                :options '(:max-control-frames 0)))))

#+sbcl
(progn
  (deftest http2-server-wave-options-asterisk
    (dolist (target '("*" "/"))
      (let* ((stream (make-instance
                      'binary-session-stream
                      :input (concatenate-octets
                              (h2-preface) (h2-frame 4 0 0 (octets))
                              (h2-frame 1 5 1
                                        (h2-header-block
                                         (cons ":method" "OPTIONS")
                                         (cons ":scheme" "https")
                                         (cons ":authority" "example.com")
                                         (cons ":path" target))))))
             (calls 0) (closed 0) (notified nil))
        (multiple-value-bind (count reason)
            (http-kit/http2:serve-http2-session
             stream (lambda (request)
                      (incf calls)
                      (ensure-equal "OPTIONS" (http-request-method request))
                      (ensure-equal target (http-request-target request))
                      (ensure-equal "/" (http-uri-path (http-request-uri request)))
                      (make-http-response :status 204))
             :on-error (lambda (condition request)
                         (declare (ignore request)) (setf notified condition))
             :close-stream (lambda (s)
                             (ensure-true (eq stream s)) (incf closed) (close s)))
          (ensure-equal 1 count)
          (ensure-equal :eof reason))
        (ensure-equal 1 calls)
        (ensure-equal 1 closed)
        (ensure-false notified)
        (ensure-false (open-stream-p stream))
        (ensure-equal (length (binary-session-input stream))
                      (binary-session-input-position stream))
        (let ((frames (server-output-frames (binary-session-output stream))))
          (ensure-equal '(4 4 1) (mapcar #'http-kit/http2::%h2-frame-type frames))
          (ensure-equal "204" (cdr (assoc ":status" (server-response-fields (third frames))
                                         :test #'string=)))
          (ensure-equal 5 (http-kit/http2::%h2-frame-flags (third frames)))))))

  (deftest http2-server-wave-updated-frame-size
    (dolist (limit '(16384 32768))
      (let* ((body (make-array 20000 :element-type '(unsigned-byte 8) :initial-element 90))
             (stream (make-instance
                      'binary-session-stream
                      :input (concatenate-octets
                              (h2-preface) (h2-frame 4 0 0 (octets))
                              (h2-frame 4 0 0 (if (= limit 16384)
                                                 (octets 0 5 0 0 64 0)
                                                 (octets 0 5 0 0 128 0)))
                              (public-server-request-frame))))
             (calls 0) (closed 0) (notified nil))
        (multiple-value-bind (count reason)
            (http-kit/http2:serve-http2-session
             stream (lambda (request)
                      (incf calls)
                      (ensure-equal "GET" (http-request-method request))
                      (ensure-equal "/" (http-request-target request))
                      (make-http-response :status 200 :body body))
             :on-error (lambda (condition request)
                         (declare (ignore request)) (setf notified condition))
             :close-stream (lambda (s)
                             (ensure-true (eq stream s)) (incf closed) (close s)))
          (ensure-equal 1 count)
          (ensure-equal :eof reason))
        (ensure-equal 1 calls)
        (ensure-equal 1 closed)
        (ensure-false notified)
        (ensure-false (open-stream-p stream))
        (ensure-equal (length (binary-session-input stream))
                      (binary-session-input-position stream))
        (let* ((frames (server-output-frames (binary-session-output stream) limit))
               (data (remove-if-not (lambda (frame) (= 0 (http-kit/http2::%h2-frame-type frame)))
                                    frames))
               (headers (remove-if-not (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                                       frames)))
          (ensure-equal '("200")
                        (mapcar (lambda (frame)
                                  (cdr (assoc ":status" (server-response-fields frame)
                                              :test #'string=))) headers))
          (ensure-equal (if (= limit 16384) '(4 4 4 1 0 0) '(4 4 4 1 0))
                        (mapcar #'http-kit/http2::%h2-frame-type frames))
        (ensure-equal (if (= limit 16384) '(16384 3616) '(20000))
                      (mapcar (lambda (frame) (length (http-kit/http2::%h2-frame-payload frame))) data))
        (ensure-equal (if (= limit 16384) '(0 1) '(1))
                      (mapcar #'http-kit/http2::%h2-frame-flags data))
        (ensure-true (every (lambda (frame) (= 1 (http-kit/http2::%h2-frame-stream-id frame))) data))
        (ensure-true (equalp body (apply #'concatenate-octets
                                      (mapcar #'http-kit/http2::%h2-frame-payload data))))
        (ensure-equal '(0 1 1)
                      (mapcar #'http-kit/http2::%h2-frame-flags
                              (remove-if-not (lambda (frame) (= 4 (http-kit/http2::%h2-frame-type frame)))
                                             frames)))))))

  (deftest http2-server-wave-updated-peer-limits
    (dolist (limit '(0 1))
      (let* ((frames (check-public-server-boundary
                      (concatenate-octets (h2-frame 4 0 0 (octets))
                                          (h2-frame 4 0 0 (octets 0 3 0 0 0 limit))
                                          (public-server-request-frame))
                      :handled limit :statuses (if (zerop limit) nil '("204"))))
             (resets (remove-if-not (lambda (frame) (= 3 (http-kit/http2::%h2-frame-type frame))) frames)))
        (ensure-equal (if (zerop limit) 1 0) (length resets))
        (when resets
          (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id (first resets)))
          (ensure-equal 0 (http-kit/http2::%h2-frame-flags (first resets)))
          (ensure-equal (octets 0 0 0 7) (http-kit/http2::%h2-frame-payload (first resets))))))
    (dolist (limit '(0 4096))
      (let ((frames (check-public-server-boundary
                     (concatenate-octets
                      (h2-frame 4 0 0 (octets))
                      (h2-frame 4 0 0 (if (zerop limit) (octets 0 1 0 0 0 0)
                                         (octets 0 1 0 0 16 0)))
                      (h2-frame 1 5 1
                                (concatenate-octets
                                 (octets 32)
                                 (h2-header-block (cons ":method" "GET") (cons ":scheme" "https")
                                                  (cons ":authority" "example.com") (cons ":path" "/")))))
                     :handled 1)))
        (ensure-equal '(4 4 4 1) (mapcar #'http-kit/http2::%h2-frame-type frames)))))

  (deftest http2-server-wave-ping-ack-and-extension
    (dolist (flags '(0 1))
      (let* ((payload (octets 0 1 2 3 4 5 6 7))
             (frames (check-public-server-boundary
                      (concatenate-octets (h2-frame 4 0 0 (octets))
                                          (h2-frame 6 flags 0 payload)
                                          (h2-frame 238 165 0 (ascii "extension"))
                                          (public-server-request-frame)) :handled 1))
             (pings (remove-if-not (lambda (frame) (= 6 (http-kit/http2::%h2-frame-type frame))) frames)))
        (ensure-equal (if (zerop flags) '(4 4 6 1) '(4 4 1))
                      (mapcar #'http-kit/http2::%h2-frame-type frames))
        (ensure-equal (if (zerop flags) 1 0) (length pings))
        (when pings
          (ensure-equal 1 (http-kit/http2::%h2-frame-flags (first pings)))
          (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id (first pings)))
          (ensure-equal payload (http-kit/http2::%h2-frame-payload (first pings)))))))

  (deftest http2-server-wave-unexpected-header-frames
    (dolist (spec '((9 4 1 ()) (5 4 1 (0 0 0 2))))
      (destructuring-bind (type flags stream-id bytes) spec
        (let ((payload (apply #'octets bytes)))
          (check-public-server-entry
           (concatenate-octets (h2-preface) (h2-frame 4 0 0 (octets))
                               (h2-frame type flags stream-id payload))
           :error-p t :frame-spec (list type flags stream-id payload)))))
    (let ((block (h2-header-block (cons ":method" "GET") (cons ":scheme" "https")
                                  (cons ":authority" "example.com") (cons ":path" "/"))))
      (check-public-server-boundary
       (concatenate-octets (h2-frame 4 0 0 (octets))
                           (h2-frame 1 1 1 (subseq block 0 1))
                           (h2-frame 9 4 1 (subseq block 1))) :handled 1)))

  (deftest http2-server-wave-blocked-response-termination
    (dolist (control '(:credit :reset :goaway))
      (let* ((input (concatenate-octets
                     (server-peer-window-request 0)
                     (public-server-request-frame 3)
                     (case control
                       (:credit (h2-frame 8 0 1 (octets 0 0 0 2)))
                       (:reset (h2-frame 3 0 1 (octets 0 0 0 8)))
                       (:goaway (h2-frame 7 0 0 (octets 0 0 0 1 0 0 0 0))))))
             (stream (make-instance 'binary-session-stream :input input))
             (calls 0) (closed 0) (notified nil))
        (multiple-value-bind (count reason)
            (http-kit/http2:serve-http2-session
             stream (lambda (request)
                      (ensure-equal "GET" (http-request-method request))
                      (incf calls)
                      (if (= calls 1) (make-http-response :status 200 :body (ascii "ok"))
                          (make-http-response :status 204)))
             :on-error (lambda (condition request)
                         (declare (ignore request)) (setf notified condition))
             :close-stream (lambda (s)
                             (ensure-true (eq s stream)) (incf closed) (close s)))
          (ensure-equal (case control (:credit 2) (:reset 1) (:goaway 0)) count)
          (ensure-equal (if (eq control :goaway) :goaway :eof) reason))
        (ensure-equal (if (eq control :goaway) 1 2) calls)
        (ensure-equal 1 closed)
        (ensure-false (open-stream-p stream))
        (ensure-false notified)
        (ensure-equal (length input) (binary-session-input-position stream))
        (let* ((frames (server-output-frames (binary-session-output stream)))
               (headers (remove-if-not (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame))) frames))
               (data (remove-if-not (lambda (frame) (= 0 (http-kit/http2::%h2-frame-type frame))) frames)))
          (ensure-equal (if (eq control :goaway) '(1) '(1 3))
                        (mapcar #'http-kit/http2::%h2-frame-stream-id headers))
          (ensure-equal (if (eq control :goaway) '("200") '("200" "204"))
                        (mapcar (lambda (frame) (cdr (assoc ":status" (server-response-fields frame)
                                                            :test #'string=))) headers))
          (ensure-equal (if (eq control :credit) 1 0) (length data))
          (when data
            (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id (first data)))
            (ensure-equal 1 (http-kit/http2::%h2-frame-flags (first data)))
            (ensure-equal (ascii "ok") (http-kit/http2::%h2-frame-payload (first data))))
          (ensure-false (find 7 frames :key #'http-kit/http2::%h2-frame-type))))))

  (deftest http2-server-wave-receive-window-boundary
    (dolist (size '(65535 65536))
      (let* ((body (make-array size :element-type '(unsigned-byte 8) :initial-element 90))
             (input (concatenate-octets (h2-preface) (h2-frame 4 0 0 (octets))
                                        (public-server-request-frame 1 4) (h2-frame 0 1 1 body)))
             (stream (make-instance 'binary-session-stream :input input))
             (calls 0) (chunks 0) (closed 0) (notified nil)
             (result (handler-case
                         (multiple-value-list
                          (http-kit/http2:serve-http2-session
                           stream (lambda (request)
                                    (incf calls)
                                    (ensure-true (equalp body (http-request-body request)))
                                    (make-http-response :status 204))
                           :max-frame-size 65536
                           :on-body-chunk (lambda (state chunk)
                                            (declare (ignore state)) (incf chunks)
                                            (ensure-true (equalp body chunk)))
                           :on-error (lambda (condition request)
                                       (ensure-false request) (setf notified condition))
                           :close-stream (lambda (s)
                                           (ensure-true (eq s stream)) (incf closed) (close s))))
                       (http-protocol-error (condition) condition))))
        (ensure-equal 1 closed)
        (ensure-false (open-stream-p stream))
        (ensure-equal (length input) (binary-session-input-position stream))
        (ensure-equal (if (= size 65535) 1 0) calls)
        (ensure-equal (if (= size 65535) 1 0) chunks)
        (let* ((frames (server-output-frames (binary-session-output stream)))
               (updates (remove-if-not (lambda (frame) (= 8 (http-kit/http2::%h2-frame-type frame))) frames))
               (goaways (remove-if-not (lambda (frame) (= 7 (http-kit/http2::%h2-frame-type frame))) frames)))
          (if (= size 65535)
              (progn
                (ensure-equal '(1 :eof) result)
                (ensure-false notified)
                (ensure-equal '(0 1) (mapcar #'http-kit/http2::%h2-frame-stream-id updates))
                (dolist (frame updates)
                  (ensure-equal (octets 0 0 255 255) (http-kit/http2::%h2-frame-payload frame)))
                (ensure-equal '(4 4 8 8 1) (mapcar #'http-kit/http2::%h2-frame-type frames))
                (ensure-equal "204" (cdr (assoc ":status" (server-response-fields (car (last frames)))
                                               :test #'string=)))
                (ensure-false goaways))
              (progn
                (ensure-true (typep result 'http-protocol-error))
                (ensure-true (eq result notified))
                (ensure-equal :http2-server (http-error-operation result))
                (ensure-equal 65536 (http-protocol-error-detail result))
                (ensure-false updates)
                (ensure-equal '(4 4 7) (mapcar #'http-kit/http2::%h2-frame-type frames))
                (ensure-equal 1 (length goaways))
                (ensure-equal (octets 0 0 0 1 0 0 0 1)
                              (http-kit/http2::%h2-frame-payload (first goaways)))))))))

  (deftest http2-server-wave-request-limit-zero
    (dolist (limit '(0 1))
      (let* ((startup (concatenate-octets (h2-preface) (h2-frame 4 0 0 (octets))))
             (first-request (public-server-request-frame))
             (input (concatenate-octets startup first-request (public-server-request-frame 3)))
             (stream (make-instance 'binary-session-stream :input input))
             (calls 0) (closed 0) (notified nil))
        (multiple-value-bind (count reason)
            (http-kit/http2:serve-http2-session
             stream (lambda (request)
                      (incf calls) (ensure-equal "GET" (http-request-method request))
                      (make-http-response :status 204))
             :max-requests limit
             :on-error (lambda (condition request)
                         (declare (ignore request)) (setf notified condition))
             :close-stream (lambda (s)
                             (ensure-true (eq s stream)) (incf closed) (close s)))
          (ensure-equal limit count)
          (ensure-equal :max-requests reason))
        (ensure-equal limit calls)
        (ensure-equal 1 closed)
        (ensure-false (open-stream-p stream))
        (ensure-false notified)
        (ensure-equal (+ (length startup) (if (zerop limit) 0 (length first-request)))
                      (binary-session-input-position stream))
        (let ((frames (server-output-frames (binary-session-output stream))))
          (ensure-equal (if (zerop limit) '(4 4 7) '(4 4 1 7))
                        (mapcar #'http-kit/http2::%h2-frame-type frames))
          (when (plusp limit)
            (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id (third frames)))
            (ensure-equal "204" (cdr (assoc ":status" (server-response-fields (third frames))
                                           :test #'string=))))
          (ensure-equal (octets 0 0 0 limit 0 0 0 0)
                        (http-kit/http2::%h2-frame-payload (car (last frames)))))))))

#+sbcl
(progn
  (deftest http2-server-probe-extended-connect-path
    (dolist (path '(nil "" "relative" "/chat?room=main"))
      (let* ((fields (append '((":method" . "CONNECT")
                              (":protocol" . "websocket")
                              (":scheme" . "https")
                              (":authority" . "example.com"))
                            (when path (list (cons ":path" path)))))
             (input (concatenate-octets
                     (h2-preface) (h2-frame 4 0 0 (octets))
                     (h2-frame 1 5 1 (apply #'h2-header-block fields))))
             (stream (make-instance 'binary-session-stream :input input))
             (handled 0) (closed 0) (notified nil) (observed nil)
             (valid (equal path "/chat?room=main")))
        (handler-case
            (multiple-value-bind (count reason)
                (http-kit/http2:serve-http2-session
                 stream
                 (lambda (request)
                   (incf handled)
                   (ensure-equal "CONNECT" (http-request-method request))
                   (ensure-equal "websocket" (http-request-protocol request))
                   (ensure-equal path (http-request-target request))
                   (make-http-response :status 204))
                 :enable-connect-p t
                 :on-error (lambda (condition request)
                             (ensure-false request)
                             (setf notified condition))
                 :close-stream (lambda (closed-stream)
                                 (ensure-true (eq stream closed-stream))
                                 (incf closed)
                                 (close closed-stream)))
              (ensure-equal 1 count)
              (ensure-equal :eof reason))
          (http-invalid-header (condition) (setf observed condition)))
        (ensure-equal 1 closed)
        (ensure-false (open-stream-p stream))
        (ensure-equal (length input) (binary-session-input-position stream))
        (let* ((frames (server-output-frames (binary-session-output stream)))
               (headers (find 1 frames :key #'http-kit/http2::%h2-frame-type))
               (goaway (find 7 frames :key #'http-kit/http2::%h2-frame-type)))
          (ensure-false (find 0 frames :key #'http-kit/http2::%h2-frame-type))
          (ensure-equal (if valid '(0 0 1) '(0 0 0))
                        (mapcar #'http-kit/http2::%h2-frame-stream-id frames))
          (if valid
              (progn
                (ensure-equal 1 handled)
                (ensure-false observed)
                (ensure-false notified)
                (ensure-false goaway)
                (ensure-equal '(4 4 1)
                              (mapcar #'http-kit/http2::%h2-frame-type frames))
                (ensure-equal "204" (cdr (assoc ":status"
                                               (server-response-fields headers)
                                               :test #'string=)))
                (ensure-equal 5 (http-kit/http2::%h2-frame-flags headers)))
              (progn
                (ensure-equal 0 handled)
                (ensure-true (typep observed 'http-invalid-header))
                (ensure-true (eq observed notified))
                (ensure-equal :http2-server (http-error-operation observed))
                (ensure-equal ":path" (http-invalid-header-name observed))
                (ensure-equal "Extended CONNECT requires origin-form :path"
                              (http-invalid-header-reason observed))
                (ensure-false (http-protocol-error-detail observed))
                (ensure-false headers)
                (ensure-equal '(4 4 7)
                              (mapcar #'http-kit/http2::%h2-frame-type frames))
                (ensure-equal 0 (http-kit/http2::%h2-frame-flags goaway))
                (ensure-equal 8 (length (http-kit/http2::%h2-frame-payload goaway)))
                (ensure-equal 1 (http-kit/http2::%h2-u32
                                 (http-kit/http2::%h2-frame-payload goaway) 0))
                (ensure-equal 1 (http-kit/http2::%h2-u32
                                 (http-kit/http2::%h2-frame-payload goaway) 4))))))))

  (deftest http2-server-probe-deferred-control
    (flet ((headers (id target)
             (h2-frame 1 5 id
                       (h2-header-block (cons ":method" "GET")
                                        (cons ":scheme" "https")
                                        (cons ":authority" "example.com")
                                        (cons ":path" target)))))
      (let* ((input (concatenate-octets
                     (h2-preface)
                     (h2-frame 4 0 0 (octets 0 4 0 0 0 1))
                     (headers 1 "/first")
                     (h2-frame 11 0 0 (octets))
                     (headers 3 "/middle")
                     (headers 5 "/last")
                     (h2-frame 8 0 5 (octets 0 0 0 1))
                     (h2-frame 8 0 1 (octets 0 0 0 1))))
             (stream (make-instance 'binary-session-stream :input input))
             (targets '()) (closed 0) (errors 0))
        (multiple-value-bind (count reason)
            (http-kit/http2:serve-http2-session
             stream
             (lambda (request)
               (let ((target (http-request-target request)))
                 (push target targets)
                 (if (string= target "/middle")
                     (make-http-response :status 204)
                     (make-http-response :status 200 :body (ascii "xy")))))
             :on-error (lambda (condition request)
                         (declare (ignore condition request))
                         (incf errors))
             :close-stream (lambda (closed-stream)
                             (ensure-true (eq stream closed-stream))
                             (incf closed)
                             (close closed-stream)))
          (ensure-equal 3 count)
          (ensure-equal :eof reason))
        (ensure-equal 0 errors)
        (ensure-equal 1 closed)
        (ensure-false (open-stream-p stream))
        (ensure-equal (length input) (binary-session-input-position stream))
        (ensure-equal '("/first" "/middle" "/last") (nreverse targets))
        (let* ((frames (server-output-frames (binary-session-output stream)))
               (headers (remove-if-not
                         (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                         frames))
               (data (remove-if-not
                      (lambda (frame) (= 0 (http-kit/http2::%h2-frame-type frame)))
                      frames)))
          (ensure-equal '(4 4 1 0 0 1 1 0 0)
                        (mapcar #'http-kit/http2::%h2-frame-type frames))
          (ensure-equal '(1 3 5)
                        (mapcar #'http-kit/http2::%h2-frame-stream-id headers))
          (ensure-equal '("200" "204" "200")
                        (mapcar (lambda (frame)
                                  (cdr (assoc ":status" (server-response-fields frame)
                                              :test #'string=)))
                                headers))
          (ensure-equal '((1 "x") (1 "y") (5 "x") (5 "y"))
                        (mapcar (lambda (frame)
                                  (list (http-kit/http2::%h2-frame-stream-id frame)
                                        (octets-as-string
                                         (http-kit/http2::%h2-frame-payload frame))))
                                data))
          (ensure-equal '(0 1 0 1)
                        (mapcar #'http-kit/http2::%h2-frame-flags data))
          (ensure-equal '(4 5 4)
                        (mapcar #'http-kit/http2::%h2-frame-flags headers))
          (ensure-false (find 7 frames :key #'http-kit/http2::%h2-frame-type))))))

  (deftest http2-server-probe-empty-settings-ack
    (dolist (ack-p '(nil t))
      (let* ((input (concatenate-octets
                     (h2-preface) (h2-frame 4 0 0 (octets))
                     (if ack-p (h2-frame 4 1 0 (octets)) (octets))
                     (h2-frame 1 5 1
                               (h2-header-block (cons ":method" "GET")
                                                (cons ":scheme" "https")
                                                (cons ":authority" "example.com")
                                                (cons ":path" "/")))))
             (stream (make-instance 'binary-session-stream :input input))
             (handled 0) (closed 0) (errors 0))
        (multiple-value-bind (count reason)
            (http-kit/http2:serve-http2-session
             stream
             (lambda (request)
               (incf handled)
               (ensure-equal "/" (http-request-target request))
               (make-http-response :status 204))
             :on-error (lambda (condition request)
                         (declare (ignore condition request))
                         (incf errors))
             :close-stream (lambda (closed-stream)
                             (ensure-true (eq stream closed-stream))
                             (incf closed)
                             (close closed-stream)))
          (ensure-equal 1 count)
          (ensure-equal :eof reason))
        (ensure-equal 1 handled)
        (ensure-equal 0 errors)
        (ensure-equal 1 closed)
        (ensure-false (open-stream-p stream))
        (ensure-equal (length input) (binary-session-input-position stream))
        (let* ((frames (server-output-frames (binary-session-output stream)))
               (acks (remove-if-not
                      (lambda (frame)
                        (and (= 4 (http-kit/http2::%h2-frame-type frame))
                             (= 1 (http-kit/http2::%h2-frame-flags frame))))
                      frames)))
          (ensure-equal '(4 4 1)
                        (mapcar #'http-kit/http2::%h2-frame-type frames))
          (ensure-equal '(0 1 5)
                        (mapcar #'http-kit/http2::%h2-frame-flags frames))
          (ensure-equal '(0 0 1)
                        (mapcar #'http-kit/http2::%h2-frame-stream-id frames))
          (ensure-equal 1 (length acks))
          (ensure-equal 0 (length (http-kit/http2::%h2-frame-payload (first acks))))
          (ensure-equal "204" (cdr (assoc ":status"
                                         (server-response-fields (third frames))
                                         :test #'string=)))))))

  (deftest http2-server-probe-known-handler-error
    (let ((known (make-condition 'simple-error
                                 :format-control "Known handler failure"
                                 :format-arguments nil)))
      (dolist (failure-p '(nil t))
        (let* ((input (concatenate-octets
                       (h2-preface) (h2-frame 4 0 0 (octets))
                       (h2-frame 1 5 1
                                 (h2-header-block (cons ":method" "GET")
                                                  (cons ":scheme" "https")
                                                  (cons ":authority" "example.com")
                                                  (cons ":path" "/")))))
               (stream (make-instance 'binary-session-stream :input input))
               (handled 0) (closed 0) (notifications 0)
               (seen-request nil) (notified-request nil)
               (notified nil) (observed nil))
          (handler-case
              (multiple-value-bind (count reason)
                  (http-kit/http2:serve-http2-session
                   stream
                   (lambda (request)
                     (incf handled)
                     (setf seen-request request)
                     (ensure-equal "/" (http-request-target request))
                     (if failure-p (error known) (make-http-response :status 204)))
                   :on-error (lambda (condition request)
                               (incf notifications)
                               (setf notified condition notified-request request))
                   :close-stream (lambda (closed-stream)
                                   (ensure-true (eq stream closed-stream))
                                   (incf closed)
                                   (close closed-stream)))
                (ensure-equal 1 count)
                (ensure-equal :eof reason))
            (simple-error (condition) (setf observed condition)))
          (ensure-equal 1 handled)
          (ensure-equal 1 closed)
          (ensure-false (open-stream-p stream))
          (ensure-equal (length input) (binary-session-input-position stream))
          (let* ((frames (server-output-frames (binary-session-output stream)))
                 (headers (find 1 frames :key #'http-kit/http2::%h2-frame-type)))
            (ensure-false (find 0 frames :key #'http-kit/http2::%h2-frame-type))
            (ensure-false (find 7 frames :key #'http-kit/http2::%h2-frame-type))
            (if failure-p
                (progn
                  (ensure-true (typep observed 'simple-error))
                  (ensure-true (eq known observed))
                  (ensure-true (eq known notified))
                  (ensure-true (eq seen-request notified-request))
                  (ensure-equal 1 notifications)
                  (ensure-false headers)
                  (ensure-equal '(4 4)
                                (mapcar #'http-kit/http2::%h2-frame-type frames))
                  (ensure-equal '(0 1)
                                (mapcar #'http-kit/http2::%h2-frame-flags frames)))
                (progn
                  (ensure-false observed)
                  (ensure-false notified)
                  (ensure-false notified-request)
                  (ensure-equal 0 notifications)
                  (ensure-equal '(4 4 1)
                                (mapcar #'http-kit/http2::%h2-frame-type frames))
                  (ensure-equal "204" (cdr (assoc ":status"
                                                 (server-response-fields headers)
                                                 :test #'string=)))
                  (ensure-equal 5 (http-kit/http2::%h2-frame-flags headers))))))))))

#+sbcl
(deftest http2-server-public-ignores-unknown-frame
  (let ((frames
          (check-public-server-boundary
           (concatenate-octets (h2-frame 4 0 0 (octets))
                               (h2-frame 255 0 0 (octets 7))
                               (public-server-request-frame))
           :handled 1)))
    (ensure-equal '(4 4 1) (mapcar #'http-kit/http2::%h2-frame-type frames))))

#+sbcl
(deftest http2-server-public-ignores-unknown-frame-during-send-wait
  (let* ((frames
           (check-public-server-boundary
            (concatenate-octets (h2-frame 4 0 0 (octets 0 4 0 0 0 0))
                                (public-server-request-frame)
                                (h2-frame 255 0 0 (octets 7))
                                (h2-frame 8 0 1 (octets 0 0 0 2)))
            :handled 1 :response (make-http-response :status 200 :body (ascii "ok"))
            :statuses '("200")))
         (data (find 0 frames :key #'http-kit/http2::%h2-frame-type)))
    (ensure-equal '(4 4 1 0) (mapcar #'http-kit/http2::%h2-frame-type frames))
    (ensure-true data)
    (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id data))
    (ensure-equal 1 (http-kit/http2::%h2-frame-flags data))
    (ensure-equal (ascii "ok") (http-kit/http2::%h2-frame-payload data))))

#+sbcl
(deftest http2-server-public-hpack-empty-name-rejected
  (let* ((input (concatenate-octets
                 (h2-preface) (h2-frame 4 0 0 (octets))
                 (h2-frame 1 5 1 (octets 0 0 0))))
         (stream (make-instance 'binary-session-stream :input input))
         (handled 0) (closed 0) (notified nil) (notified-request nil)
         (observed
           (handler-case
               (http-kit/http2:serve-http2-session
                stream
                (lambda (request)
                  (declare (ignore request))
                  (incf handled)
                  (make-http-response :status 204))
                :on-error (lambda (condition request)
                            (setf notified condition notified-request request))
                :close-stream (lambda (closed-stream)
                                (ensure-true (eq stream closed-stream))
                                (incf closed)
                                (close closed-stream)))
             (http-protocol-error (condition) condition))))
    (ensure-equal 0 handled)
    (ensure-equal 1 closed)
    (ensure-false (open-stream-p stream))
    (ensure-equal (length input) (binary-session-input-position stream))
    (ensure-true (typep observed 'http-protocol-error))
    (ensure-true (eq observed notified))
    (ensure-false notified-request)
    (ensure-equal :http2-server (http-error-operation observed))
    (ensure-equal 9 (http-kit::http-protocol-error-http2-error-code observed))
    (let ((detail (http-protocol-error-detail observed)))
      (ensure-true (typep detail 'http-invalid-header))
      (ensure-equal :hpack (http-error-operation detail))
      (ensure-equal "" (http-invalid-header-name detail))
      (ensure-equal :name (http-invalid-header-reason detail)))
    (let* ((frames (server-output-frames (binary-session-output stream)))
           (goaway (find 7 frames :key #'http-kit/http2::%h2-frame-type)))
      (ensure-equal '(4 4 7) (mapcar #'http-kit/http2::%h2-frame-type frames))
      (ensure-true goaway)
      (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id goaway))
      (ensure-equal 0 (http-kit/http2::%h2-frame-flags goaway))
      (ensure-equal 8 (length (http-kit/http2::%h2-frame-payload goaway)))
      (ensure-equal 1 (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload goaway) 0))
      (ensure-equal 9 (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload goaway) 4)))))
#+sbcl
(deftest http2-server-public-hpack-colon-only-name-rejected
  (let* ((input (concatenate-octets
                 (h2-preface) (h2-frame 4 0 0 (octets))
                 (h2-frame 1 5 1 (octets 0 1 #x3a 0))))
         (stream (make-instance 'binary-session-stream :input input))
         (handled 0) (closed 0) (notified nil) (notified-request nil)
         (observed
           (handler-case
               (http-kit/http2:serve-http2-session
                stream
                (lambda (request)
                  (declare (ignore request))
                  (incf handled)
                  (make-http-response :status 204))
                :on-error (lambda (condition request)
                            (setf notified condition notified-request request))
                :close-stream (lambda (closed-stream)
                                (ensure-true (eq stream closed-stream))
                                (incf closed)
                                (close closed-stream)))
             (http-protocol-error (condition) condition))))
    (ensure-equal 0 handled)
    (ensure-equal 1 closed)
    (ensure-false (open-stream-p stream))
    (ensure-equal (length input) (binary-session-input-position stream))
    (ensure-true (typep observed 'http-protocol-error))
    (ensure-true (eq observed notified))
    (ensure-false notified-request)
    (ensure-equal :http2-server (http-error-operation observed))
    (ensure-equal 9 (http-kit::http-protocol-error-http2-error-code observed))
    (let ((detail (http-protocol-error-detail observed)))
      (ensure-true (typep detail 'http-invalid-header))
      (ensure-equal :hpack (http-error-operation detail))
      (ensure-equal (string #\:) (http-invalid-header-name detail))
      (ensure-equal :name (http-invalid-header-reason detail)))
    (let* ((frames (server-output-frames (binary-session-output stream)))
           (goaway (find 7 frames :key #'http-kit/http2::%h2-frame-type)))
      (ensure-equal '(4 4 7) (mapcar #'http-kit/http2::%h2-frame-type frames))
      (ensure-true goaway)
      (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id goaway))
      (ensure-equal 0 (http-kit/http2::%h2-frame-flags goaway))
      (ensure-equal 8 (length (http-kit/http2::%h2-frame-payload goaway)))
      (ensure-equal 1 (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload goaway) 0))
      (ensure-equal 9 (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload goaway) 4)))))
#+sbcl
(deftest http2-server-public-hpack-uppercase-name-rejected
  (let* ((input (concatenate-octets
                 (h2-preface) (h2-frame 4 0 0 (octets))
                 (h2-frame 1 5 1 (octets 0 1 #x58 0))))
         (stream (make-instance 'binary-session-stream :input input))
         (handled 0) (closed 0) (notified nil) (notified-request nil)
         (observed
           (handler-case
               (http-kit/http2:serve-http2-session
                stream
                (lambda (request)
                  (declare (ignore request))
                  (incf handled)
                  (make-http-response :status 204))
                :on-error (lambda (condition request)
                            (setf notified condition notified-request request))
                :close-stream (lambda (closed-stream)
                                (ensure-true (eq stream closed-stream))
                                (incf closed)
                                (close closed-stream)))
             (http-protocol-error (condition) condition))))
    (ensure-equal 0 handled)
    (ensure-equal 1 closed)
    (ensure-false (open-stream-p stream))
    (ensure-equal (length input) (binary-session-input-position stream))
    (ensure-true (typep observed 'http-protocol-error))
    (ensure-true (eq observed notified))
    (ensure-false notified-request)
    (ensure-equal :http2-server (http-error-operation observed))
    (ensure-equal 9 (http-kit::http-protocol-error-http2-error-code observed))
    (let ((detail (http-protocol-error-detail observed)))
      (ensure-true (typep detail 'http-invalid-header))
      (ensure-equal :hpack (http-error-operation detail))
      (ensure-equal (string #\X) (http-invalid-header-name detail))
      (ensure-equal :name (http-invalid-header-reason detail)))
    (let* ((frames (server-output-frames (binary-session-output stream)))
           (goaway (find 7 frames :key #'http-kit/http2::%h2-frame-type)))
      (ensure-equal '(4 4 7) (mapcar #'http-kit/http2::%h2-frame-type frames))
      (ensure-true goaway)
      (ensure-equal 0 (http-kit/http2::%h2-frame-stream-id goaway))
      (ensure-equal 0 (http-kit/http2::%h2-frame-flags goaway))
      (ensure-equal 8 (length (http-kit/http2::%h2-frame-payload goaway)))
      (ensure-equal 1 (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload goaway) 0))
      (ensure-equal 9 (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload goaway) 4)))))

#+sbcl
(deftest http2-server-public-status-constructor-boundaries
  (dolist (streaming-p '(nil t))
    (dolist (status '(99 600 "200"))
      (let ((observed
              (handler-case
                  (if streaming-p
                      (make-http-response-stream
                       :status status :body-function (lambda () nil) :body-length 0)
                      (make-http-response :status status))
                (http-invalid-status (condition) condition))))
        (ensure-true (typep observed 'http-invalid-status))
        (ensure-equal status (http-kit:http-invalid-status-code observed))))
    (let* ((response (if streaming-p
                         (make-http-response-stream
                          :status 599 :body-function (lambda () nil) :body-length 0)
                         (make-http-response :status 599)))
           (frames
             (check-public-server-boundary
              (concatenate-octets (h2-frame 4 0 0 (octets))
                                 (public-server-request-frame))
              :handled 1 :response response
              :information (list (make-http-response :status 100))
              :statuses '("100" "599"))))
      (ensure-equal '(4 4 1 1) (mapcar #'http-kit/http2::%h2-frame-type frames))
      (ensure-equal '(0 1 4 5) (mapcar #'http-kit/http2::%h2-frame-flags frames)))))

#+sbcl
(deftest http2-server-response-validation-final-wire
  (let* ((frames
           (check-public-server-boundary
            (concatenate-octets (h2-frame 4 0 0 (octets))
                               (public-server-request-frame))
            :handled 1 :statuses '("200")
            :response (make-http-response
                       :status 200 :body (ascii "ok")
                       :headers (list (make-http-header "x-final" "ready")))))
         (headers (find 1 frames :key #'http-kit/http2::%h2-frame-type))
         (data (find 0 frames :key #'http-kit/http2::%h2-frame-type)))
    (ensure-equal '(4 4 1 0) (mapcar #'http-kit/http2::%h2-frame-type frames))
    (ensure-equal '(0 1 4 1) (mapcar #'http-kit/http2::%h2-frame-flags frames))
    (ensure-equal '(0 0 1 1) (mapcar #'http-kit/http2::%h2-frame-stream-id frames))
    (ensure-equal "ready" (cdr (assoc "x-final" (server-response-fields headers)
                                     :test #'string=)))
    (ensure-equal (ascii "ok") (http-kit/http2::%h2-frame-payload data))))

#+sbcl
(deftest http2-server-response-validation-informational-wire
  (let* ((frames
           (check-public-server-boundary
            (concatenate-octets (h2-frame 4 0 0 (octets))
                               (public-server-request-frame))
            :handled 1 :statuses '("103" "200")
            :information
            (list (make-http-response
                   :status 103
                   :headers (list (make-http-header "x-hint" "early"))))
            :response (make-http-response
                       :status 200 :body (ascii "ok")
                       :headers (list (make-http-header "x-final" "ready")))))
         (headers (remove-if-not
                   (lambda (frame) (= 1 (http-kit/http2::%h2-frame-type frame)))
                   frames))
         (data (find 0 frames :key #'http-kit/http2::%h2-frame-type)))
    (ensure-equal '(4 4 1 1 0) (mapcar #'http-kit/http2::%h2-frame-type frames))
    (ensure-equal '(0 1 4 4 1) (mapcar #'http-kit/http2::%h2-frame-flags frames))
    (ensure-equal '(0 0 1 1 1) (mapcar #'http-kit/http2::%h2-frame-stream-id frames))
    (ensure-equal "early" (cdr (assoc "x-hint" (server-response-fields (first headers))
                                     :test #'string=)))
    (ensure-equal "ready" (cdr (assoc "x-final" (server-response-fields (second headers))
                                     :test #'string=)))
    (ensure-equal (ascii "ok") (http-kit/http2::%h2-frame-payload data))))

#+sbcl
(deftest http2-server-response-validation-ordinary-arguments
  (ensure-true (http-kit/http2::%h2-server-response-header-valid-p "x-check" "ok"))
  (dolist (case '((":status" "200" :http2-server
                   "Response pseudo-fields are forbidden" "Invalid HTTP/2 header")
                  (":Status" "200" :http2-server
                   "Response pseudo-fields are forbidden" "Invalid HTTP/2 header")
                  ("" "ok" :http2-headers :name
                   "HTTP/2 header field names must be lowercase.")
                  ("X-check" "ok" :http2-headers :name
                   "HTTP/2 header field names must be lowercase.")
                  ("connection" "close" :http2-headers :connection-specific
                   "Connection-specific headers are forbidden in HTTP/2 responses.")
                  ("te" "trailers" :http2-headers :forbidden
                   "HTTP/2 TE is permitted only in request header sections.")))
    (destructuring-bind (name value operation reason message) case
      (let ((observed
              (handler-case
                  (http-kit/http2::%h2-server-response-header-valid-p name value)
                (http-invalid-header (condition) condition))))
        (ensure-true (typep observed 'http-invalid-header))
        (ensure-equal operation (http-error-operation observed))
        (ensure-equal name (http-invalid-header-name observed))
        (ensure-equal reason (http-invalid-header-reason observed))
        (ensure-equal message (http-error-message observed))))))

#+sbcl
(deftest http2-server-response-boundary-rejects-forbidden-content
  (dolist (status (list 204 304))
    (dolist (stream-p (list nil t))
      (dolist (content-kind (list :body :trailers))
        (let* ((body-p (eq content-kind :body))
               (chunks (if body-p (list (octets 90) nil) (list nil)))
               (trailers (unless body-p (list (make-http-header "x-end" "yes"))))
               (response
                 (if stream-p
                     (make-http-response-stream
                      :status status :trailers trailers
                      :body-length (if body-p 1 0)
                      :body-function (lambda () (pop chunks)))
                     (make-http-response
                      :status status :body (if body-p (octets 90) (octets))
                      :trailers trailers))))
          (check-public-server-boundary
           (concatenate-octets (h2-frame 4 0 0 (octets))
                               (public-server-request-frame))
           :handled 1 :error-p t :detail status :statuses nil
           :response response)))))
  (dolist (stream-p (list nil t))
    (dolist (content-kind (list :body :trailers))
      (let* ((body-p (eq content-kind :body))
             (chunks (if body-p (list (octets 90) nil) (list nil)))
             (trailers (unless body-p (list (make-http-header "x-end" "yes"))))
             (information
               (if stream-p
                   (make-http-response-stream
                    :status 103 :trailers trailers
                    :body-length (if body-p 1 0)
                    :body-function (lambda () (pop chunks)))
                   (make-http-response
                    :status 103 :body (if body-p (octets 90) (octets))
                    :trailers trailers))))
        (check-public-server-boundary
         (concatenate-octets (h2-frame 4 0 0 (octets))
                             (public-server-request-frame))
         :handled 1 :error-p t :detail 103 :statuses nil
         :response (make-http-response :status 200)
         :information (list information))))))
