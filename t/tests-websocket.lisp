(in-package #:http-kit/test)

(deftest websocket-rfc6455-frame-boundaries
  (let* ((masked-frame
           (make-websocket-frame
            :fin-p t
            :opcode 1
            :mask-p t
            :masking-key (octets #x37 #xfa #x21 #x3d)
            :payload (ascii "Hello")))
         (masked-wire (serialize-websocket-frame masked-frame)))
    (ensure-equal
     (octets #x81 #x85 #x37 #xfa #x21 #x3d
             #x7f #x9f #x4d #x51 #x58)
     masked-wire)
    (multiple-value-bind (parsed consumed)
        (parse-websocket-frame masked-wire :require-mask-p t)
      (ensure-equal 11 consumed)
      (ensure-true (websocket-frame-fin-p parsed))
      (ensure-equal 1 (websocket-frame-opcode parsed))
      (ensure-equal (ascii "Hello") (websocket-frame-payload parsed)))
    (let* ((payload (make-array 126
                                :element-type '(unsigned-byte 8)
                                :initial-element #x61))
           (frame (make-websocket-frame :opcode 2 :payload payload))
           (wire (serialize-websocket-frame frame)))
      (ensure-equal #x7e (aref wire 1))
      (ensure-equal 130 (length wire))
      (multiple-value-bind (parsed consumed)
          (parse-websocket-frame wire)
        (ensure-equal (length wire) consumed)
        (ensure-equal payload (websocket-frame-payload parsed))))
    (signals http-protocol-error
      (parse-websocket-frame
       (serialize-websocket-frame
        (make-websocket-frame :payload (ascii "unmasked")))
       :require-mask-p t))
    (signals http-protocol-error
      (parse-websocket-frame (octets #x81 #x7e 0 125)))
    (signals http-protocol-error
      (parse-websocket-frame
       (octets #x81 #x7f 0 0 0 0 0 0 0 125)))))

#+sbcl
(deftest websocket-stream-rejects-nonminimal-lengths
  (dolist (wire (list (octets #x81 #x7e 0 125)
                      (octets #x81 #x7f 0 0 0 0 0 0 0 125)))
    (signals http-protocol-error
      (read-websocket-frame
       (make-instance 'binary-test-stream :input wire)))))

(deftest websocket-http-upgrade-and-close-payload
  (let* ((request
           (make-http-request
            :method "GET"
            :uri "http://example.test/chat"
            :headers
            (list (make-http-header "Host" "example.test")
                  (make-http-header "Upgrade" "websocket")
                  (make-http-header "Connection" "keep-alive, Upgrade")
                  (make-http-header "Sec-WebSocket-Key"
                                    "dGhlIHNhbXBsZSBub25jZQ==")
                  (make-http-header "Sec-WebSocket-Version" "13")
                  (make-http-header "Sec-WebSocket-Protocol"
                                    "chat, superchat"))))
         (response
           (websocket-upgrade-response
            request
            :protocol "chat"
            :extensions "permessage-deflate")))
    (ensure-true (websocket-upgrade-request-p request))
    (ensure-equal "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
                  (websocket-accept-key
                   "dGhlIHNhbXBsZSBub25jZQ=="))
    (ensure-equal 101 (http-response-status response))
    (ensure-equal "websocket"
                  (http-header-value (http-response-headers response)
                                     "Upgrade"))
    (ensure-equal "chat"
                  (http-header-value (http-response-headers response)
                                     "Sec-WebSocket-Protocol"))
    (ensure-equal "permessage-deflate"
                  (http-header-value (http-response-headers response)
                                     "Sec-WebSocket-Extensions"))
    (signals http-protocol-error
      (websocket-upgrade-response request :protocol "not-offered"))
    (let ((close-payload
            (make-websocket-close-payload :code 1000 :reason "bye")))
      (multiple-value-bind (code reason)
          (parse-websocket-close-payload close-payload)
        (ensure-equal 1000 code)
        (ensure-equal "bye" reason)))
    (signals http-protocol-error
      (make-websocket-close-payload :code 1004))
    (signals http-protocol-error
      (parse-websocket-close-payload (octets 3)))))

#+sbcl
(deftest websocket-client-handshake-keeps-stream-open
  (let* ((key "dGhlIHNhbXBsZSBub25jZQ==")
         (request
           (make-websocket-upgrade-request
            "http://example.test/chat"
            :key key
            :protocols '("chat" "superchat")
            :headers (list (make-http-header "Origin" "http://example.test"))))
         (frame-wire
           (serialize-websocket-frame
            (make-websocket-frame :opcode 1 :payload (ascii "hello"))))
         (stream
           (make-instance
            'binary-test-stream
            :input
            (concatenate-octets
             (ascii
              (concatenate
               'string
               "HTTP/1.1 101 Switching Protocols|CRLF|"
               "Upgrade: websocket|CRLF|"
               "Connection: Upgrade|CRLF|"
               "Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=|CRLF|"
               "Sec-WebSocket-Protocol: chat|CRLF||CRLF|"))
             frame-wire))))
    (ensure-true (websocket-upgrade-request-p request))
    (multiple-value-bind (response reusable-p)
        (websocket-client-handshake stream request)
      (ensure-equal 101 (http-response-status response))
      (ensure-true (not reusable-p))
      (multiple-value-bind (frame consumed)
          (read-websocket-frame stream)
        (ensure-equal (length frame-wire) consumed)
        (ensure-equal 1 (websocket-frame-opcode frame))
        (ensure-equal (ascii "hello") (websocket-frame-payload frame))))
    (let ((wire (octets-as-string (binary-test-output stream))))
      (ensure-true (search "GET /chat HTTP/1.1" wire))
      (ensure-true (search (concatenate 'string "Sec-WebSocket-Key: " key) wire))
      (ensure-true (search "Sec-WebSocket-Protocol: chat, superchat" wire))
      (ensure-true (search "Origin: http://example.test" wire)))
    (signals http-protocol-error
      (make-websocket-upgrade-request
       "http://example.test/chat"
       :key key
       :headers (list (make-http-header "Upgrade" "other"))))
    (let ((bad-stream
            (make-instance
             'binary-test-stream
             :input
             (ascii
              (concatenate
               'string
               "HTTP/1.1 101 Switching Protocols|CRLF|"
               "Upgrade: websocket|CRLF|"
               "Connection: Upgrade|CRLF|"
               "Sec-WebSocket-Accept: invalid|CRLF||CRLF|")))))
      (signals http-protocol-error
        (websocket-client-handshake bad-stream request)))))

#+sbcl
(deftest websocket-fragmented-message-and-control-frames
  (let* ((wire
           (concatenate-octets
            (serialize-websocket-frame
             (make-websocket-frame :opcode 9 :payload (ascii "ping")))
            (serialize-websocket-frame
             (make-websocket-frame :fin-p nil
                                   :opcode 1
                                   :payload (ascii "Hel")))
            (serialize-websocket-frame
             (make-websocket-frame :fin-p t
                                   :opcode 0
                                   :payload (ascii "lo")))))
         (stream (make-instance 'binary-test-stream :input wire))
         (control-opcodes nil))
    (multiple-value-bind (message opcode)
        (read-websocket-message
         stream
         :on-control (lambda (frame)
                       (push (websocket-frame-opcode frame)
                             control-opcodes)))
      (ensure-equal (ascii "Hello") message)
      (ensure-equal 1 opcode))
    (ensure-equal '(9) control-opcodes)))

#+sbcl
(deftest websocket-message-and-control-frame-writers
  (let* ((stream
           (make-instance 'binary-test-stream :input (octets)))
         (masking-key-count 0)
         (message-result
           (multiple-value-list
            (write-websocket-message
             stream "Hello"
             :opcode 1
             :max-frame-payload-bytes 2
             :mask-p t
             :masking-key-function
             (lambda ()
               (incf masking-key-count)
               (octets 1 2 3 4)))))
         (wire (binary-test-output stream)))
    (ensure-equal '(3 5) message-result)
    (ensure-equal 3 masking-key-count)
    (multiple-value-bind (first first-consumed)
        (parse-websocket-frame wire :require-mask-p t)
      (multiple-value-bind (second second-consumed)
          (parse-websocket-frame (subseq wire first-consumed)
                                 :require-mask-p t
                                 :allow-unmasked-p nil)
        (multiple-value-bind (third third-consumed)
            (parse-websocket-frame
             (subseq wire (+ first-consumed second-consumed))
             :require-mask-p t)
          (ensure-equal 1 (websocket-frame-opcode first))
          (ensure-true (not (websocket-frame-fin-p first)))
          (ensure-equal (ascii "He") (websocket-frame-payload first))
          (ensure-equal 0 (websocket-frame-opcode second))
          (ensure-true (not (websocket-frame-fin-p second)))
          (ensure-equal (ascii "ll") (websocket-frame-payload second))
          (ensure-equal 0 (websocket-frame-opcode third))
          (ensure-true (websocket-frame-fin-p third))
          (ensure-equal (ascii "o") (websocket-frame-payload third))
          (ensure-equal (length wire)
                        (+ first-consumed second-consumed third-consumed))))))
  (let ((stream (make-instance 'binary-test-stream :input (octets))))
    (signals http-protocol-error
      (write-websocket-message
       stream (ascii "too long for one fixed key")
       :max-frame-payload-bytes 2
       :mask-p t
       :masking-key (octets 1 2 3 4)))))

#+sbcl
(deftest websocket-control-frame-writers
  (let ((stream (make-instance 'binary-test-stream :input (octets))))
    (websocket-ping stream
                    :payload "ping"
                    :mask-p t
                    :masking-key (octets 4 3 2 1))
    (websocket-pong stream :payload (octets 1 2 3))
    (websocket-close stream :code 1000 :reason "bye")
    (let ((wire (binary-test-output stream)))
      (multiple-value-bind (ping ping-consumed)
          (parse-websocket-frame wire :require-mask-p t)
        (multiple-value-bind (pong pong-consumed)
            (parse-websocket-frame (subseq wire ping-consumed))
          (multiple-value-bind (close close-consumed)
              (parse-websocket-frame
               (subseq wire (+ ping-consumed pong-consumed)))
            (ensure-equal 9 (websocket-frame-opcode ping))
            (ensure-equal (ascii "ping") (websocket-frame-payload ping))
            (ensure-equal 10 (websocket-frame-opcode pong))
            (ensure-equal (octets 1 2 3) (websocket-frame-payload pong))
            (ensure-equal 8 (websocket-frame-opcode close))
            (multiple-value-bind (code reason)
                (parse-websocket-close-payload
                 (websocket-frame-payload close))
              (ensure-equal 1000 code)
              (ensure-equal "bye" reason))
            (ensure-equal (length wire)
                          (+ ping-consumed pong-consumed close-consumed))))))
  (let ((stream (make-instance 'binary-test-stream :input (octets))))
    (signals http-protocol-error
      (websocket-close stream :payload (octets 0) :code 1000)))))

#+sbcl
(deftest websocket-server-session-ping-close-and-masking
  (let* ((close-payload
           (make-websocket-close-payload :code 1000 :reason "bye"))
         (input
           (concatenate-octets
            (serialize-websocket-frame
             (make-websocket-frame
              :opcode 9
              :mask-p t
              :masking-key (octets 1 2 3 4)
              :payload (ascii "ping")))
            (serialize-websocket-frame
             (make-websocket-frame
              :opcode 1
              :mask-p t
              :masking-key (octets 5 6 7 8)
              :payload (ascii "hello")))
            (serialize-websocket-frame
             (make-websocket-frame
              :opcode 8
              :mask-p t
              :masking-key (octets 9 10 11 12)
              :payload close-payload))))
         (stream (make-instance 'binary-test-stream :input input))
         (messages nil)
         (control-opcodes nil))
    (multiple-value-bind (count termination)
        (serve-websocket-session
         stream
         (lambda (received-stream payload opcode)
           (declare (ignore received-stream))
           (push (list payload opcode) messages))
         :close-stream nil
         :on-control
         (lambda (frame)
           (push (websocket-frame-opcode frame) control-opcodes)))
      (ensure-equal 1 count)
      (ensure-equal :peer-close termination))
    (ensure-equal (list (list (ascii "hello") 1)) (nreverse messages))
    (ensure-equal '(9 8) (nreverse control-opcodes))
    (let ((wire (binary-test-output stream)))
      (multiple-value-bind (pong pong-consumed)
          (parse-websocket-frame wire)
        (multiple-value-bind (close close-consumed)
            (parse-websocket-frame (subseq wire pong-consumed))
          (ensure-equal 10 (websocket-frame-opcode pong))
          (ensure-equal (ascii "ping") (websocket-frame-payload pong))
          (ensure-equal 8 (websocket-frame-opcode close))
          (ensure-equal close-payload (websocket-frame-payload close))
          (multiple-value-bind (code reason)
              (parse-websocket-close-payload
               (websocket-frame-payload close))
            (ensure-equal 1000 code)
            (ensure-equal "bye" reason))
          (ensure-equal (length wire) (+ pong-consumed close-consumed)))))))

#+sbcl
(deftest websocket-server-session-requires-masked-client-frames
  (let* ((input
           (serialize-websocket-frame
            (make-websocket-frame :opcode 1 :payload (ascii "bad"))))
         (stream (make-instance 'binary-test-stream :input input))
         (condition nil))
    (signals http-protocol-error
      (serve-websocket-session
       stream
       (lambda (received-stream payload opcode)
         (declare (ignore received-stream payload opcode)))
       :close-stream nil
       :on-error (lambda (seen-condition)
                   (setf condition seen-condition))))
    (ensure-true (typep condition 'http-protocol-error))
    (let ((wire (binary-test-output stream)))
      (multiple-value-bind (close consumed)
          (parse-websocket-frame wire)
        (ensure-equal 8 (websocket-frame-opcode close))
        (multiple-value-bind (code reason)
            (parse-websocket-close-payload
             (websocket-frame-payload close))
          (ensure-equal 1002 code)
          (ensure-equal "WebSocket session error" reason))
        (ensure-equal (length wire) consumed)))))
