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
                   (h2-frame 1 4 3 headers)
                   (h2-frame 3 0 3 (octets 0 0 0 0))))
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
        (ensure-equal 11
                      (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload goaway) 4)))))

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
        (ensure-equal 11
                      (http-kit/http2::%h2-u32
                       (http-kit/http2::%h2-frame-payload goaway) 4)))))
