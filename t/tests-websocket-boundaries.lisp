(in-package #:http-kit/test)

(defun websocket-boundaries-protocol-error (function)
  (let ((caught (handler-case (progn (funcall function) nil)
                  (http-protocol-error (condition) condition))))
    (ensure-true (typep caught 'http-protocol-error))
    (ensure-equal :websocket (http-error-operation caught))
    caught))

(defun websocket-boundaries-wire (opcode payload &key (fin-p t) (mask-p nil))
  (serialize-websocket-frame
   (make-websocket-frame
    :opcode opcode :payload payload :fin-p fin-p :mask-p mask-p
    :masking-key (when mask-p
                   (make-array 4 :element-type '(unsigned-byte 8)
                                 :initial-element 0)))))

(deftest websocket-boundaries-extension-quoted-pairs
  (let ((extensions
          (parse-websocket-extensions "X-TEST; Mode=\"f\\ast\", other")))
    (ensure-equal '("x-test" "other")
                  (mapcar #'websocket-extension-name extensions))
    (ensure-equal '(("mode" . "fast"))
                  (websocket-extension-parameters (first extensions)))
    (ensure-equal nil (websocket-extension-parameters (second extensions))))
  (dolist (value
            (list nil "x; =a" "x; mode=" "x other"
                  "x; mode=\"a" "x; mode=\"a\\"
                  (format nil "x; mode=\"~C\"" (code-char 1))
                  (format nil "x; mode=\"\\~C\"" (code-char 1))
                  (format nil "x; mode=\"~C\"" (code-char 128))
                  (format nil "x; mode=\"\\~C\"" #\Tab)))
    (websocket-boundaries-protocol-error
     (lambda () (parse-websocket-extensions value)))))

(deftest websocket-boundaries-deflate-response-parameters
  (let* ((request
           (make-websocket-upgrade-request
            "http://example.test/chat" :key "dGhlIHNhbXBsZSBub25jZQ=="
            :extensions "permessage-deflate; client_max_window_bits"))
         (selected "permessage-deflate; client_max_window_bits=8; server_max_window_bits=15; client_no_context_takeover; server_no_context_takeover")
         (response (websocket-upgrade-response request :extensions selected)))
    (ensure-equal 101 (http-response-status response))
    (ensure-equal selected
                  (http-header-value (http-response-headers response)
                                     "Sec-WebSocket-Extensions"))
    (dolist (value '("permessage-deflate; client_max_window_bits"
                     "permessage-deflate; server_max_window_bits"
                     "permessage-deflate; client_no_context_takeover=1"
                     "permessage-deflate; server_no_context_takeover=1"
                     "permessage-deflate, permessage-deflate"))
      (websocket-boundaries-protocol-error
       (lambda () (websocket-upgrade-response request :extensions value))))))

(deftest websocket-boundaries-client-key-padding
  (ensure-equal "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
                (websocket-accept-key "dGhlIHNhbXBsZSBub25jZQ=="))
  (dolist (key '(nil "" "AAA" "=AAA" "AA=A" "AA==AAAA"
                "AB==" "AAB=" "AAAAAAAAAAAAAAAAAAAAAB=="))
    (websocket-boundaries-protocol-error
     (lambda () (websocket-accept-key key)))))

#+sbcl
(defclass websocket-boundaries-output-stream (binary-test-stream)
  ((flushes :initform 0 :accessor websocket-boundaries-flushes)))

#+sbcl
(defmethod sb-gray:stream-finish-output :after
    ((stream websocket-boundaries-output-stream))
  (incf (websocket-boundaries-flushes stream)))

#+sbcl
(deftest websocket-boundaries-message-sequence-rejections
  (dolist (frames
            (list (websocket-boundaries-wire 0 (ascii "orphan"))
                  (concatenate-octets
                   (websocket-boundaries-wire 1 (ascii "a") :fin-p nil)
                   (websocket-boundaries-wire 2 (ascii "b")))))
    (let ((stream (make-instance 'binary-test-stream
                                :input (concatenate-octets frames (ascii "!")))))
      (websocket-boundaries-protocol-error
       (lambda () (read-websocket-message stream)))
      (ensure-equal (length frames)
                    (http-kit/test-core::binary-test-input-position stream))
      (ensure-equal (char-code #\!) (read-byte stream))
      (ensure-equal 0 (length (binary-test-output stream))))))

#+sbcl
(deftest websocket-boundaries-message-size-limits
  (dolist (frames
            (list (websocket-boundaries-wire 2 (ascii "abcd"))
                  (concatenate-octets
                   (websocket-boundaries-wire 2 (ascii "ab") :fin-p nil)
                   (websocket-boundaries-wire 0 (ascii "cd")))))
    (let ((accepted (make-instance 'binary-test-stream :input frames))
          (rejected (make-instance 'binary-test-stream
                                  :input (concatenate-octets frames (ascii "!"))))
          (caught nil))
      (multiple-value-bind (payload opcode)
          (read-websocket-message accepted :max-message-bytes 4)
        (ensure-equal (ascii "abcd") payload)
        (ensure-equal 2 opcode))
      (handler-case (read-websocket-message rejected :max-message-bytes 3)
        (http-size-limit-exceeded (condition) (setf caught condition)))
      (ensure-true (typep caught 'http-size-limit-exceeded))
      (ensure-equal :websocket (http-error-operation caught))
      (ensure-equal :websocket (http-size-limit-exceeded-kind caught))
      (ensure-equal 3 (http-size-limit-exceeded-limit caught))
      (ensure-equal 4 (http-size-limit-exceeded-observed caught))
      (ensure-equal (length frames)
                    (http-kit/test-core::binary-test-input-position rejected))
      (ensure-equal (char-code #\!) (read-byte rejected)))))

#+sbcl
(deftest websocket-boundaries-message-option-rejections
  (dolist (options '((:on-control 42) (:decompress-function 42)))
    (let ((stream (make-instance 'binary-test-stream
                                :input (websocket-boundaries-wire 1 (ascii "a")))))
      (websocket-boundaries-protocol-error
       (lambda () (apply #'read-websocket-message stream options)))
      (ensure-equal 0 (http-kit/test-core::binary-test-input-position stream))
      (ensure-equal 0 (length (binary-test-output stream))))))

#+sbcl
(deftest websocket-boundaries-empty-compressed-output
  (dolist (compressed-p '(nil t))
    (dolist (finish-output-p '(nil t))
      (let ((stream (make-instance 'websocket-boundaries-output-stream
                                  :input (ascii "")))
            (calls 0))
        (multiple-value-bind (frames payload-length)
            (write-websocket-message
             stream (if compressed-p "abc" "")
             :opcode 1 :max-frame-payload-bytes 1 :finish-output-p finish-output-p
             :compress-function
             (when compressed-p
               (lambda (payload)
                 (incf calls)
                 (ensure-equal (ascii "abc") payload)
                 (ascii ""))))
          (ensure-equal 1 frames)
          (ensure-equal (if compressed-p 3 0) payload-length))
        (ensure-equal (octets (if compressed-p #xc1 #x81) 0)
                      (binary-test-output stream))
        (ensure-equal (if compressed-p 1 0) calls)
        (ensure-equal (if finish-output-p 1 0)
                      (websocket-boundaries-flushes stream))))))

#+sbcl
(deftest websocket-boundaries-session-termination
  (dolist (case '((0 nil 0 :max-messages)
                  (1 nil 1 :max-messages)
                  (1 :close 1 :handler-close)
                  (nil :close 1 :handler-close)))
    (destructuring-bind (maximum handler-result expected-count expected-reason) case
      (let* ((frame (websocket-boundaries-wire 1 (ascii "a") :mask-p t))
             (stream (make-instance 'binary-test-stream
                                    :input (concatenate-octets frame frame)))
             (events nil))
        (multiple-value-bind (count reason)
            (serve-websocket-session
             stream
             (lambda (received payload opcode)
               (ensure-true (eq stream received))
               (ensure-equal (ascii "a") payload)
               (ensure-equal 1 opcode)
               (push :handler events)
               handler-result)
             :max-messages maximum
             :close-stream
             (lambda (received)
               (ensure-true (eq stream received))
               (ensure-equal (octets #x88 2 3 #xe8) (binary-test-output received))
               (push :cleanup events)))
          (ensure-equal expected-count count)
          (ensure-equal expected-reason reason))
        (ensure-equal (if (zerop expected-count) '(:cleanup) '(:handler :cleanup))
                      (reverse events))
        (ensure-equal (* expected-count (length frame))
                      (http-kit/test-core::binary-test-input-position stream))
        (multiple-value-bind (unread consumed) (read-websocket-frame stream)
          (ensure-equal (length frame) consumed)
          (ensure-equal (ascii "a") (websocket-frame-payload unread)))
        (ensure-true (open-stream-p stream))))))

#+sbcl
(deftest websocket-boundaries-session-pong-and-empty-close
  (let* ((stream
           (make-instance
            'binary-test-stream
            :input (concatenate-octets
                    (websocket-boundaries-wire 10 (ascii "pong") :mask-p t)
                    (websocket-boundaries-wire 8 (ascii "") :mask-p t)
                    (ascii "!"))))
         (events nil))
    (multiple-value-bind (count reason)
        (serve-websocket-session
         stream (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (push :unexpected-handler events))
         :on-control
         (lambda (frame)
           (ensure-equal 0 (length (binary-test-output stream)))
           (push (websocket-frame-opcode frame) events))
         :close-stream
         (lambda (received)
           (ensure-true (eq stream received))
           (ensure-equal (octets #x88 0) (binary-test-output stream))
           (push :cleanup events)))
      (ensure-equal 0 count)
      (ensure-equal :peer-close reason))
    (ensure-equal '(10 8 :cleanup) (reverse events))
    (ensure-equal (char-code #\!) (read-byte stream))
    (multiple-value-bind (code reason) (parse-websocket-close-payload (ascii ""))
      (ensure-equal nil code)
      (ensure-equal "" reason))))

#+sbcl
(deftest websocket-boundaries-session-error-lifecycle
  (dolist (failure '(:initial-size :fragmented-size :handler))
    (dolist (close-on-error-p '(nil t))
      (let* ((wire
               (if (eq failure :fragmented-size)
                   (concatenate-octets
                    (websocket-boundaries-wire 2 (ascii "ab") :fin-p nil :mask-p t)
                    (websocket-boundaries-wire 0 (ascii "cd") :mask-p t))
                   (websocket-boundaries-wire 2 (ascii "abcd") :mask-p t)))
             (stream (make-instance 'binary-test-stream :input wire))
             (handler-condition (make-condition 'simple-error
                                                :format-control "handler failure"))
             (notified nil)
             (caught nil)
             (events nil)
             (expected-close
               (if close-on-error-p
                   (concatenate-octets
                    (octets #x88 25 3 (if (eq failure :handler) #xf3 #xf1))
                    (ascii "WebSocket session error"))
                   (ascii ""))))
        (handler-case
            (serve-websocket-session
             stream
             (lambda (received payload opcode)
               (ensure-true (eq stream received))
               (ensure-equal (ascii "abcd") payload)
               (ensure-equal 2 opcode)
               (push :handler events)
               (error handler-condition))
             :max-message-bytes (if (eq failure :handler) 4 3)
             :close-on-error-p close-on-error-p
             :on-error
             (lambda (condition)
               (setf notified condition)
               (ensure-equal expected-close (binary-test-output stream))
               (push :on-error events))
             :close-stream
             (lambda (received)
               (ensure-true (eq stream received))
               (ensure-equal expected-close (binary-test-output stream))
               (push :cleanup events)))
          (error (condition)
            (setf caught condition)
            (push :caught events)))
        (ensure-true (typep caught (if (eq failure :handler)
                                     'simple-error 'http-size-limit-exceeded)))
        (ensure-true (eq caught notified))
        (if (eq failure :handler)
            (ensure-true (eq caught handler-condition))
            (progn
              (ensure-equal :websocket (http-error-operation caught))
              (ensure-equal 3 (http-size-limit-exceeded-limit caught))
              (ensure-equal 4 (http-size-limit-exceeded-observed caught))))
        (ensure-equal (append (when (eq failure :handler) '(:handler))
                              '(:on-error :cleanup :caught))
                      (reverse events))
        (ensure-equal expected-close (binary-test-output stream))
        (ensure-equal (length wire)
                      (http-kit/test-core::binary-test-input-position stream))))))

#+sbcl
(deftest websocket-boundaries-session-default-close
  (let ((stream (make-instance 'binary-test-stream :input (ascii ""))))
    (multiple-value-bind (count reason)
        (serve-websocket-session stream (lambda (&rest arguments)
                                         (declare (ignore arguments)))
                                 :max-messages 0)
      (ensure-equal 0 count)
      (ensure-equal :max-messages reason))
    (ensure-false (open-stream-p stream))
    (ensure-equal (octets #x88 2 3 #xe8) (binary-test-output stream))))

#+sbcl
(deftest websocket-boundaries-session-option-rejections
  (dolist (options '((:max-messages -1) (:max-messages 1/2)
                    (:on-control 42) (:on-error 42)
                    (:decompress-function 42) (:close-stream 42)))
    (let ((stream (make-instance 'binary-test-stream :input (ascii "!"))))
      (websocket-boundaries-protocol-error
       (lambda ()
         (apply #'serve-websocket-session stream
                (lambda (&rest arguments) (declare (ignore arguments))) options)))
      (ensure-equal 0 (http-kit/test-core::binary-test-input-position stream))
      (ensure-equal 0 (length (binary-test-output stream)))
      (ensure-true (open-stream-p stream))))
  (websocket-boundaries-protocol-error
   (lambda () (serve-websocket-session nil #'identity)))
  (let ((stream (make-instance 'binary-test-stream :input (ascii "!"))))
    (websocket-boundaries-protocol-error
     (lambda () (serve-websocket-session stream nil)))
    (ensure-equal 0 (http-kit/test-core::binary-test-input-position stream))
    (ensure-true (open-stream-p stream))))

#+sbcl
(deftest websocket-boundaries-handshake-rejected-responses
  (let ((request (make-websocket-upgrade-request
                  "http://example.test/chat" :key "dGhlIHNhbXBsZSBub25jZQ=="
                  :protocols '("chat"))))
    (dolist (case
              '(("HTTP/1.1 200 OK" "Upgrade: websocket|CRLF|Connection: Upgrade|CRLF|Content-Length: 0|CRLF|")
                ("HTTP/1.0 101 Switching Protocols" "Upgrade: websocket|CRLF|Connection: Upgrade|CRLF|")
                ("HTTP/1.1 101 Switching Protocols" "Connection: Upgrade|CRLF|")
                ("HTTP/1.1 101 Switching Protocols" "Upgrade: websocket|CRLF|Connection: close|CRLF|")
                ("HTTP/1.1 101 Switching Protocols" "Upgrade: websocket|CRLF|Connection: Upgrade|CRLF|Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=|CRLF|")
                ("HTTP/1.1 101 Switching Protocols" "Upgrade: websocket|CRLF|Connection: Upgrade|CRLF|Sec-WebSocket-Protocol: other|CRLF|")
                ("HTTP/1.1 101 Switching Protocols" "Upgrade: websocket|CRLF|Connection: Upgrade|CRLF|Sec-WebSocket-Protocol: chat, other|CRLF|")
                ("HTTP/1.1 101 Switching Protocols" "Upgrade: websocket|CRLF|Connection: Upgrade|CRLF|Sec-WebSocket-Protocol: chat|CRLF|Sec-WebSocket-Protocol: chat|CRLF|")))
      (destructuring-bind (status headers) case
        (let ((stream
                (make-instance
                 'binary-test-stream
                 :input (ascii (concatenate
                                'string status "|CRLF|" headers
                                "Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=|CRLF||CRLF|!")))))
          (websocket-boundaries-protocol-error
           (lambda () (websocket-client-handshake stream request)))
          (ensure-true (open-stream-p stream))
          (ensure-equal (serialize-http-request request) (binary-test-output stream))
          (ensure-equal (char-code #\!) (read-byte stream)))))))

#+sbcl
(deftest websocket-boundaries-handshake-extension-result
  (let* ((request
           (make-websocket-upgrade-request
            "http://example.test/chat" :key "dGhlIHNhbXBsZSBub25jZQ=="
            :extensions "permessage-deflate; client_max_window_bits"))
         (frame (websocket-boundaries-wire 2 (ascii "data")))
         (stream
           (make-instance
            'binary-test-stream
            :input (concatenate-octets
                    (ascii "HTTP/1.1 101 Switching Protocols|CRLF|Upgrade: websocket|CRLF|Connection: Upgrade|CRLF|Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=|CRLF|Sec-WebSocket-Extensions: permessage-deflate; client_max_window_bits=8|CRLF||CRLF|")
                    frame))))
    (multiple-value-bind (response reusable-p extensions)
        (websocket-client-handshake stream request)
      (declare (ignore reusable-p))
      (ensure-equal 101 (http-response-status response))
      (ensure-equal 1 (length extensions))
      (ensure-equal "permessage-deflate"
                    (websocket-extension-name (first extensions)))
      (ensure-equal '(("client_max_window_bits" . "8"))
                    (websocket-extension-parameters (first extensions))))
    (ensure-true (open-stream-p stream))
    (ensure-equal (serialize-http-request request) (binary-test-output stream))
    (multiple-value-bind (received consumed) (read-websocket-frame stream)
      (ensure-equal 2 (websocket-frame-opcode received))
      (ensure-equal (ascii "data") (websocket-frame-payload received))
      (ensure-equal (length frame) consumed))))

#+sbcl
(deftest websocket-boundaries-frame-header-rejections
  (dolist (case '(((#xa2 0) nil)
                  ((#x92 0) nil)
                  ((#x82 0) (:allow-unmasked-p nil))
                  ((#x82 127 #x80 0 0 0 0 0 0 0) nil)))
    (destructuring-bind (bytes options) case
      (let* ((header (apply #'octets bytes))
             (stream (make-instance 'binary-test-stream
                                    :input (concatenate-octets header (ascii "!")))))
        (websocket-boundaries-protocol-error
         (lambda () (apply #'read-websocket-frame stream options)))
        (ensure-equal (length header)
                      (http-kit/test-core::binary-test-input-position stream))
        (ensure-equal (char-code #\!) (read-byte stream))
        (ensure-equal 0 (length (binary-test-output stream))))))
  (websocket-boundaries-protocol-error
   (lambda () (read-websocket-frame nil))))

#+sbcl
(deftest websocket-boundaries-utf8-text-and-close
  (dolist (bytes '((#xc2) (#xc2 #x7f) (#xe0 #x9f #xbf)
                   (#xf0 #x90 #x80) (#xf0 #x8f #xbf #xbf)))
    (let* ((payload (apply #'octets bytes))
           (frame (websocket-boundaries-wire 1 payload))
           (stream (make-instance 'binary-test-stream
                                  :input (concatenate-octets frame (ascii "!")))))
      (dolist (operation (list (lambda () (read-websocket-message stream))
                              (lambda ()
                                (parse-websocket-close-payload
                                 (concatenate-octets (octets 3 #xe8) payload)))))
        (let ((caught (handler-case (progn (funcall operation) nil)
                        (http-protocol-error (condition) condition))))
          (ensure-true (typep caught 'http-protocol-error))
          (ensure-equal :websocket-utf8 (http-error-operation caught))))
      (ensure-equal (length frame)
                    (http-kit/test-core::binary-test-input-position stream))
      (ensure-equal (char-code #\!) (read-byte stream))
      (ensure-equal 0 (length (binary-test-output stream)))))
  (dolist (case '(((#xe0 #xa0 #x80) #x800)
                  ((#xf0 #x90 #x80 #x80) #x10000)))
    (destructuring-bind (bytes codepoint) case
      (let* ((payload (apply #'octets bytes))
             (stream (make-instance 'binary-test-stream
                                    :input (websocket-boundaries-wire 1 payload))))
        (multiple-value-bind (received opcode) (read-websocket-message stream)
          (ensure-equal payload received)
          (ensure-equal 1 opcode))
        (multiple-value-bind (code reason)
            (parse-websocket-close-payload
             (concatenate-octets (octets 3 #xe8) payload))
          (ensure-equal 1000 code)
          (ensure-equal (string (code-char codepoint)) reason)))))
  (websocket-boundaries-protocol-error
   (lambda () (make-websocket-close-payload :reason 42)))
  (websocket-boundaries-protocol-error
   (lambda () (parse-websocket-close-payload (vector 3 256)))))

#+sbcl
(deftest websocket-boundaries-output-option-rejections
  (let ((key (octets 0 0 0 0)))
    (dolist (options (list '(:opcode 0) '(:opcode 9)
                          '(:max-frame-payload-bytes 0)
                          '(:max-frame-payload-bytes 1/2)
                          '(:compress-function 42)
                          (list :compress-function (lambda (payload)
                                                     (declare (ignore payload))
                                                     "not octets"))
                          (list :mask-p t :masking-key key
                                :masking-key-function (lambda () key))
                          (list :masking-key key)
                          (list :masking-key-function (lambda () key))
                          '(:mask-p t :masking-key-function 42)
                          '(:mask-p t)))
      (let ((stream (make-instance 'websocket-boundaries-output-stream
                                  :input (ascii "!"))))
        (websocket-boundaries-protocol-error
         (lambda () (apply #'write-websocket-message stream (ascii "a") options)))
        (ensure-equal 0 (length (binary-test-output stream)))
        (ensure-equal 0 (websocket-boundaries-flushes stream))
        (ensure-equal 0 (http-kit/test-core::binary-test-input-position stream))
        (ensure-true (open-stream-p stream)))))
  (dolist (payload '("binary string" 42))
    (let ((stream (make-instance 'binary-test-stream :input (ascii ""))))
      (websocket-boundaries-protocol-error
       (lambda () (write-websocket-message stream payload)))
      (ensure-equal 0 (length (binary-test-output stream)))))
  (websocket-boundaries-protocol-error
   (lambda () (write-websocket-message nil (ascii "a")))))

#+sbcl
(deftest websocket-boundaries-empty-control-output
  (dolist (case (list (list #'websocket-ping #x89)
                     (list #'websocket-pong #x8a)))
    (destructuring-bind (writer first-byte) case
      (let ((stream (make-instance 'websocket-boundaries-output-stream
                                  :input (ascii "!"))))
        (funcall writer stream)
        (ensure-equal (octets first-byte 0) (binary-test-output stream))
        (ensure-equal 1 (websocket-boundaries-flushes stream))
        (ensure-equal (char-code #\!) (read-byte stream)))
      (let ((stream (make-instance 'websocket-boundaries-output-stream
                                  :input (ascii "!"))))
        (websocket-boundaries-protocol-error
         (lambda () (funcall writer stream :payload 42)))
        (ensure-equal 0 (length (binary-test-output stream)))
        (ensure-equal 0 (websocket-boundaries-flushes stream))
        (ensure-equal (char-code #\!) (read-byte stream))))))

#+sbcl
(deftest websocket-boundaries-session-without-stream-cleanup
  (let ((stream (make-instance 'binary-test-stream :input (ascii "!"))))
    (multiple-value-bind (count reason)
        (serve-websocket-session stream (lambda (&rest arguments)
                                         (declare (ignore arguments)))
                                 :max-messages 0 :close-stream nil)
      (ensure-equal 0 count)
      (ensure-equal :max-messages reason))
    (ensure-true (open-stream-p stream))
    (ensure-equal (octets #x88 2 3 #xe8) (binary-test-output stream))
    (ensure-equal (char-code #\!) (read-byte stream))))

#+sbcl
(deftest websocket-boundaries-handshake-option-rejections
  (dolist (options '((:protocols "chat") (:protocols (42))
                    (:protocols ("bad token")) (:extensions 42) (:headers 42)))
    (websocket-boundaries-protocol-error
     (lambda () (apply #'make-websocket-upgrade-request
                      "http://example.test/chat" :key "dGhlIHNhbXBsZSBub25jZQ=="
                      options))))
  (let ((request (make-websocket-upgrade-request
                  "http://example.test/chat" :key "dGhlIHNhbXBsZSBub25jZQ==")))
    (websocket-boundaries-protocol-error
     (lambda () (websocket-upgrade-response request :extensions 42)))
    (dolist (header (list '("Upgrade" . "other")
                          (make-http-header "Connection" "close")))
      (websocket-boundaries-protocol-error
       (lambda () (websocket-upgrade-response request :headers (list header)))))
    (let ((response (websocket-upgrade-response
                     request :headers '(("X-Trace" . "visible")))))
      (ensure-equal 101 (http-response-status response))
      (ensure-equal "visible" (http-header-value (http-response-headers response)
                                               "X-Trace"))
      (ensure-equal "websocket" (http-header-value (http-response-headers response)
                                                 "Upgrade")))
    (websocket-boundaries-protocol-error
     (lambda () (websocket-client-handshake nil request))))
  (let ((stream (make-instance 'binary-test-stream :input (ascii "!")))
        (request (make-http-request :method "GET" :uri "http://example.test/chat")))
    (ensure-false (websocket-upgrade-request-p request))
    (websocket-boundaries-protocol-error
     (lambda () (websocket-upgrade-response request)))
    (websocket-boundaries-protocol-error
     (lambda () (websocket-client-handshake stream request)))
    (ensure-equal 0 (length (binary-test-output stream)))
    (ensure-equal (char-code #\!) (read-byte stream))
    (ensure-true (open-stream-p stream))))

(deftest websocket-boundaries-upgrade-request-invalid-offers
  (dolist (case '(("Sec-WebSocket-Key" "AA==")
                  ("Sec-WebSocket-Extensions" "permessage-deflate; unknown=1")))
    (destructuring-bind (name value) case
      (let ((request
              (make-http-request
               :method "GET" :uri "http://example.test/chat"
               :headers (append
                         (list (make-http-header "Upgrade" "websocket")
                               (make-http-header "Connection" "Upgrade")
                               (make-http-header "Sec-WebSocket-Version" "13"))
                         (if (string= name "Sec-WebSocket-Key")
                             (list (make-http-header name value))
                             (list (make-http-header "Sec-WebSocket-Key"
                                                     "dGhlIHNhbXBsZSBub25jZQ==")
                                   (make-http-header name value)))))))
        (ensure-false (websocket-upgrade-request-p request))
        (websocket-boundaries-protocol-error
         (lambda () (websocket-upgrade-response request)))))))


#+sbcl
(deftest websocket-boundaries-default-frame-roundtrip
  (let* ((frame (make-websocket-frame))
         (stream (make-instance 'websocket-boundaries-output-stream
                                :input (octets #x81 0 #x21))))
    (ensure-true (websocket-frame-fin-p frame))
    (ensure-false (websocket-frame-rsv1-p frame))
    (ensure-equal 1 (websocket-frame-opcode frame))
    (ensure-false (websocket-frame-mask-p frame))
    (ensure-equal 0 (length (websocket-frame-payload frame)))
    (ensure-true (eq frame (write-websocket-frame stream frame)))
    (ensure-equal (octets #x81 0) (binary-test-output stream))
    (ensure-equal 1 (websocket-boundaries-flushes stream))
    (multiple-value-bind (parsed consumed)
        (parse-websocket-frame (octets #x81 0 #x21))
      (ensure-equal 2 consumed)
      (ensure-equal (octets #x81 0) (serialize-websocket-frame parsed)))
    (multiple-value-bind (read consumed) (read-websocket-frame stream)
      (ensure-equal 2 consumed)
      (ensure-equal (octets #x81 0) (serialize-websocket-frame read)))
    (ensure-equal #x21 (read-byte stream))))

#+sbcl
(deftest websocket-boundaries-default-frame-size-limit
  ;; The declared length exceeds 16 MiB; no payload allocation is needed.
  (let ((header (octets #x82 #x7f 0 0 0 0 1 0 0 1)))
    (websocket-boundaries-protocol-error
     (lambda () (parse-websocket-frame header)))
    (dolist (reader (list #'read-websocket-frame #'read-websocket-message))
      (let ((stream (make-instance 'binary-test-stream
                                  :input (concatenate-octets header (ascii "!")))))
        (websocket-boundaries-protocol-error (lambda () (funcall reader stream)))
        (ensure-equal 10 (http-kit/test-core::binary-test-input-position stream))
        (ensure-equal #x21 (read-byte stream))
        (ensure-equal 0 (length (binary-test-output stream)))))))

#+sbcl
(deftest websocket-boundaries-default-message-fragmentation
  (dolist (size '(65535 65536))
    (let* ((payload (make-array size :element-type '(unsigned-byte 8)
                                    :initial-element #x61))
           (stream (make-instance 'websocket-boundaries-output-stream
                                  :input (ascii "!")))
           (expected-frames (if (= size 65535) 1 2)))
      (multiple-value-bind (frames bytes) (write-websocket-message stream payload)
        (ensure-equal expected-frames frames)
        (ensure-equal size bytes))
      (ensure-equal 1 (websocket-boundaries-flushes stream))
      (ensure-equal #x21 (read-byte stream))
      (let ((wire (binary-test-output stream)))
        (ensure-equal (+ size 4 (if (= expected-frames 2) 2 0)) (length wire))
        (ensure-equal (octets (if (= expected-frames 1) #x82 #x02) #x7e #xff #xff)
                      (subseq wire 0 4))
        (multiple-value-bind (first consumed) (parse-websocket-frame wire)
          (ensure-equal 65539 consumed)
          (ensure-equal 2 (websocket-frame-opcode first))
          (ensure-equal 65535 (length (websocket-frame-payload first)))
          (ensure-equal (= expected-frames 1) (websocket-frame-fin-p first))
          (ensure-false (websocket-frame-mask-p first))
          (ensure-false (websocket-frame-rsv1-p first))
          (when (= expected-frames 2)
            (ensure-equal (octets #x80 1 #x61) (subseq wire consumed))))
        (let ((input (make-instance 'binary-test-stream
                                   :input (concatenate-octets wire (ascii "!")))))
          (multiple-value-bind (decoded opcode)
              (read-websocket-message input)
            (ensure-equal payload decoded)
            (ensure-equal 2 opcode))
          (ensure-equal #x21 (read-byte input)))))))

#+sbcl
(deftest websocket-boundaries-default-close-payload-and-output
  (let ((payload (make-websocket-close-payload))
        (stream (make-instance 'websocket-boundaries-output-stream
                               :input (ascii "!"))))
    (ensure-true (typep payload '(vector (unsigned-byte 8))))
    (ensure-equal (octets 3 #xe8) payload)
    (multiple-value-bind (code reason) (parse-websocket-close-payload payload)
      (ensure-equal 1000 code)
      (ensure-equal "" reason))
    (websocket-close stream)
    (ensure-equal (octets #x88 2 3 #xe8) (binary-test-output stream))
    (ensure-equal 1 (websocket-boundaries-flushes stream))
    (ensure-equal #x21 (read-byte stream))))

#+sbcl
(deftest websocket-boundaries-default-message-size-limit
  (let* ((stream (make-instance 'binary-test-stream
                               :input (concatenate-octets
                                       (octets #xc2 1 0) (ascii "!"))))
         (calls 0))
    (websocket-boundaries-protocol-error
     (lambda ()
       (read-websocket-message
        stream
        :decompress-function
        (lambda (wire)
          (incf calls)
          (ensure-equal (octets 0) wire)
          (make-array (1+ (* 16 1024 1024)) :element-type '(unsigned-byte 8)
                                               :initial-element 0)))))
    (ensure-equal 1 calls)
    (ensure-equal #x21 (read-byte stream))
    (ensure-equal 0 (length (binary-test-output stream)))))

#+sbcl
(deftest websocket-boundaries-default-session-peer-close
  (let* ((stream (make-instance
                  'websocket-boundaries-output-stream
                  :input (concatenate-octets
                          (websocket-boundaries-wire 1 (ascii "first") :mask-p t)
                          (websocket-boundaries-wire 2 (octets 0 #xff) :mask-p t)
                          (websocket-boundaries-wire 8 (octets 3 #xe8) :mask-p t)
                          (ascii "!"))))
         (messages nil))
    (multiple-value-bind (count reason)
        (serve-websocket-session
         stream
         (lambda (received payload opcode)
           (ensure-true (eq stream received))
           (push (list opcode payload) messages)))
      (ensure-equal 2 count)
      (ensure-equal :peer-close reason))
    (ensure-equal (list (list 1 (ascii "first")) (list 2 (octets 0 #xff)))
                  (reverse messages))
    (ensure-equal (octets #x88 2 3 #xe8) (binary-test-output stream))
    (ensure-equal 1 (websocket-boundaries-flushes stream))
    (ensure-equal (- (length (http-kit/test-core::binary-test-input stream)) 1)
                  (http-kit/test-core::binary-test-input-position stream))
    (ensure-false (open-stream-p stream))))

#+sbcl
(deftest websocket-boundaries-default-session-rejects-unmasked
  (let ((stream (make-instance 'websocket-boundaries-output-stream
                              :input (octets #x81 1 #x61 #x21)))
        (calls 0))
    (websocket-boundaries-protocol-error
     (lambda ()
       (serve-websocket-session stream
                                (lambda (&rest arguments)
                                  (declare (ignore arguments))
                                  (incf calls)))))
    (ensure-equal 0 calls)
    (ensure-equal (concatenate-octets (octets #x88 25 3 #xea)
                                    (ascii "WebSocket session error"))
                  (binary-test-output stream))
    (ensure-equal 1 (websocket-boundaries-flushes stream))
    (ensure-equal 2 (http-kit/test-core::binary-test-input-position stream))
    (ensure-false (open-stream-p stream))))

(deftest websocket-boundaries-upgrade-response-invalid-extra-header
  (let ((request (make-websocket-upgrade-request
                  "http://example.test/chat" :key "dGhlIHNhbXBsZSBub25jZQ==")))
    (dolist (header '(42 (42 . "value")))
      (let ((caught
              (handler-case
                  (progn (websocket-upgrade-response request :headers (list header))
                         nil)
                (http-invalid-header (condition) condition))))
        (ensure-true (typep caught 'http-invalid-header))
        (ensure-equal :header (http-error-operation caught))
        (ensure-equal :type (http-invalid-header-reason caught))))
    (ensure-true (websocket-upgrade-request-p request))
    (let ((response (websocket-upgrade-response
                     request :headers '(("X-Boundary" . "valid")))))
      (ensure-equal 101 (http-response-status response))
      (ensure-equal "valid" (http-header-value (http-response-headers response)
                                             "X-Boundary")))))

#+sbcl
(deftest websocket-native-boundaries-utf8-continuations
  (dolist (bytes '((#xe1 #x80 #x7f) (#xe1 #x80 #xc0)
                   (#xf1 #x80 #x7f #x80) (#xf1 #x80 #xc0 #x80)
                   (#xf1 #x80 #x80 #x7f) (#xf1 #x80 #x80 #xc0)
                   (#xed #xa0 #x80) (#xf4 #x90 #x80 #x80)))
    (let* ((payload (apply #'octets bytes))
           (frame (websocket-boundaries-wire 1 payload))
           (stream (make-instance 'binary-test-stream
                                  :input (concatenate-octets frame (ascii "!")))))
      (dolist (operation (list (lambda () (read-websocket-message stream))
                              (lambda ()
                                (parse-websocket-close-payload
                                 (concatenate-octets (octets 3 #xe8) payload)))))
        (let ((caught (handler-case (progn (funcall operation) nil)
                        (http-protocol-error (condition) condition))))
          (ensure-true (typep caught 'http-protocol-error))
          (ensure-equal :websocket-utf8 (http-error-operation caught))
          (ensure-equal payload (http-protocol-error-detail caught))))
      (ensure-equal (length frame)
                    (http-kit/test-core::binary-test-input-position stream))
      (ensure-equal (char-code #\!) (read-byte stream))
      (ensure-equal 0 (length (binary-test-output stream)))))
  (dolist (case '(((#xe1 #x80 #x80) #x1000)
                  ((#xe1 #x80 #xbf) #x103f)
                  ((#xf1 #x80 #x80 #x80) #x40000)
                  ((#xf1 #x80 #xbf #xbf) #x40fff)
                  ((#xed #x9f #xbf) #xd7ff)
                  ((#xf4 #x8f #xbf #xbf) #x10ffff)))
    (destructuring-bind (bytes codepoint) case
      (let* ((payload (apply #'octets bytes))
             (frame (websocket-boundaries-wire 1 payload))
             (close-payload (concatenate-octets (octets 3 #xe8) payload))
             (stream (make-instance 'binary-test-stream
                                    :input (concatenate-octets frame (ascii "!")))))
        (multiple-value-bind (received opcode) (read-websocket-message stream)
          (ensure-equal payload received)
          (ensure-equal 1 opcode))
        (multiple-value-bind (code reason)
            (parse-websocket-close-payload close-payload)
          (ensure-equal 1000 code)
          (ensure-equal (string (code-char codepoint)) reason)
          (ensure-equal close-payload
                        (make-websocket-close-payload :code code :reason reason)))
        (ensure-equal (length frame)
                      (http-kit/test-core::binary-test-input-position stream))
        (ensure-equal (char-code #\!) (read-byte stream))
        (ensure-equal 0 (length (binary-test-output stream)))))))

(deftest websocket-native-boundaries-upgrade-prerequisites
  (dolist (case '(("POST" "HTTP/1.1" "Upgrade" "13" nil)
                  ("GET" "HTTP/1.0" "Upgrade" "13" nil)
                  ("GET" "HTTP/1.1" "close" "13" nil)
                  ("GET" "HTTP/1.1" "Upgrade" "12" nil)
                  ("GET" "HTTP/1.1" "Upgrade" "13" t)))
    (destructuring-bind (method version connection ws-version duplicate-p) case
      (let ((request
              (make-http-request
               :method method :uri "http://example.test/chat"
               :protocol-version version
               :headers (append
                         (list (make-http-header "Upgrade" "websocket")
                               (make-http-header "Connection" connection)
                               (make-http-header "Sec-WebSocket-Key"
                                                 "dGhlIHNhbXBsZSBub25jZQ==")
                               (make-http-header "Sec-WebSocket-Version" ws-version))
                         (when duplicate-p
                           (list (make-http-header "Sec-WebSocket-Version" "13")))))))
        (ensure-false (websocket-upgrade-request-p request))
        (websocket-boundaries-protocol-error
         (lambda () (websocket-upgrade-response request))))))
  (let* ((request
           (make-http-request
            :method "GET" :uri "http://example.test/chat"
            :headers '(("Upgrade" . " , WebSocket, , ")
                       ("Connection" . " , Upgrade, , ")
                       ("Sec-WebSocket-Key" . "dGhlIHNhbXBsZSBub25jZQ==")
                       ("Sec-WebSocket-Version" . "13"))))
         (response (websocket-upgrade-response request)))
    (ensure-true (websocket-upgrade-request-p request))
    (ensure-equal 101 (http-response-status response))
    (ensure-equal "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
                  (http-header-value (http-response-headers response)
                                     "Sec-WebSocket-Accept"))
    (ensure-equal ", WebSocket, ,"
                  (http-header-value (http-request-headers request) "Upgrade"))))

(deftest websocket-native-boundaries-custom-extension-selection
  (let* ((extensions "x-boundary; mode=fast")
         (request (make-websocket-upgrade-request
                   "http://example.test/chat" :key "dGhlIHNhbXBsZSBub25jZQ=="
                   :protocols '("chat") :extensions extensions))
         (response (websocket-upgrade-response
                    request :protocol "chat" :extensions extensions)))
    (ensure-true (websocket-upgrade-request-p request))
    (ensure-equal 101 (http-response-status response))
    (ensure-equal extensions
                  (http-header-value (http-response-headers response)
                                     "Sec-WebSocket-Extensions"))
    (ensure-equal "chat"
                  (http-header-value (http-response-headers response)
                                     "Sec-WebSocket-Protocol"))
    (ensure-equal "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
                  (http-header-value (http-response-headers response)
                                     "Sec-WebSocket-Accept"))
    (let ((parsed (parse-websocket-extensions
                   (http-header-value (http-response-headers response)
                                      "Sec-WebSocket-Extensions"))))
      (ensure-equal 1 (length parsed))
      (ensure-equal "x-boundary" (websocket-extension-name (first parsed)))
      (ensure-equal '(("mode" . "fast"))
                    (websocket-extension-parameters (first parsed))))
    (dolist (protocol '("" "bad protocol"))
      (let ((caught (websocket-boundaries-protocol-error
                     (lambda ()
                       (websocket-upgrade-response
                        request :protocol protocol :extensions extensions)))))
        (ensure-equal protocol (http-protocol-error-detail caught))))
    (ensure-true (websocket-upgrade-request-p request))))

(deftest websocket-boundaries-close-code-type
  (dolist (code '(1000.0 "1000" nil))
    (ensure-false (websocket-valid-close-code-p code))
    (let ((caught (websocket-boundaries-protocol-error
                   (lambda () (make-websocket-close-payload :code code)))))
      (ensure-equal code (http-protocol-error-detail caught))))
  (dolist (code '(1000 3000 4999))
    (ensure-true (websocket-valid-close-code-p code))
    (multiple-value-bind (received reason)
        (parse-websocket-close-payload
         (make-websocket-close-payload :code code :reason "bye"))
      (ensure-equal code received)
      (ensure-equal "bye" reason))))

(deftest websocket-public-key-fourth-digit
  (let* ((key "AAA!")
         (caught (websocket-boundaries-protocol-error
                  (lambda () (websocket-accept-key key)))))
    (ensure-equal key (http-protocol-error-detail caught)))
  (ensure-equal "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
                (websocket-accept-key "dGhlIHNhbXBsZSBub25jZQ==")))

(deftest websocket-public-missing-handshake-fields
  (let ((valid (make-websocket-upgrade-request
                "http://example.test/chat" :key "dGhlIHNhbXBsZSBub25jZQ==")))
    (dolist (name '("Sec-WebSocket-Version" "Sec-WebSocket-Key"))
      (let ((request
              (make-http-request
               :method "GET" :uri "http://example.test/chat"
               :headers (remove name (http-request-headers valid)
                                :key #'http-header-name :test #'string-equal))))
        (ensure-equal nil (http-header-value (http-request-headers request) name))
        (ensure-false (websocket-upgrade-request-p request))
        (websocket-boundaries-protocol-error
         (lambda () (websocket-upgrade-response request)))))
    (ensure-true (websocket-upgrade-request-p valid))
    (let ((response (websocket-upgrade-response valid)))
      (ensure-equal 101 (http-response-status response))
      (ensure-equal "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
                    (http-header-value (http-response-headers response)
                                       "Sec-WebSocket-Accept")))))

#+sbcl
(deftest websocket-public-session-decompression
  (dolist (compressed-p '(nil t))
    (let* ((payload (ascii "decoded"))
           (encoded (if compressed-p (octets 1 2 3) payload))
           (wire (serialize-websocket-frame
                  (make-websocket-frame
                   :opcode 2 :payload encoded :rsv1-p compressed-p :mask-p t
                   :masking-key (octets 0 0 0 0))))
           (stream (make-instance 'binary-test-stream
                                  :input (concatenate-octets wire (ascii "!"))))
           (events nil)
           (decompress-count 0)
           (cleanup-count 0))
      (unwind-protect
           (progn
             (multiple-value-bind (count reason)
                 (serve-websocket-session
                  stream
                  (lambda (received body opcode)
                    (ensure-true (eq stream received))
                    (ensure-equal payload body)
                    (ensure-equal 2 opcode)
                    (push :handler events)
                    :close)
                  :decompress-function
                  (when compressed-p
                    (lambda (body)
                      (incf decompress-count)
                      (ensure-equal encoded body)
                      (push :decompress events)
                      payload))
                  :close-stream
                  (lambda (received)
                    (incf cleanup-count)
                    (ensure-true (eq stream received))
                    (ensure-equal (length wire)
                                  (http-kit/test-core::binary-test-input-position
                                   stream))
                    (ensure-equal (char-code #\!) (read-byte stream))
                    (ensure-equal (octets #x88 2 3 #xe8)
                                  (binary-test-output stream))
                    (push :cleanup events)
                    (close stream)))
               (ensure-equal 1 count)
               (ensure-equal :handler-close reason))
             (ensure-equal (if compressed-p 1 0) decompress-count)
             (ensure-equal 1 cleanup-count)
             (ensure-equal (if compressed-p
                               '(:decompress :handler :cleanup)
                               '(:handler :cleanup))
                           (nreverse events))
             (ensure-false (open-stream-p stream)))
        (when (open-stream-p stream)
          (close stream))))))

