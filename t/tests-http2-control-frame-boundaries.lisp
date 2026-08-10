(in-package #:http-kit/test)

(deftest http2-control-frame-boundaries
  (ensure-true
   (http-kit/http2::%h2-control-frame-p
    http-kit/http2::+http2-settings-type+))
  (ensure-true
   (not
    (http-kit/http2::%h2-control-frame-p
     http-kit/http2::+http2-continuation-type+)))
  (let ((sent '()))
    (http-kit/http2::%h2-send-control
     (lambda (wire) (push wire sent))
     http-kit/http2::+http2-ping-type+
     http-kit/http2::+http2-ack-flag+
     0
     (octets 1 2 3))
    (ensure-equal (list (h2-frame 6 1 0 (octets 1 2 3))) sent))
  (let ((sent '()))
    (http-kit/http2::%h2-send-control nil
     http-kit/http2::+http2-ping-type+ 0 0 (octets 1))
    (ensure-equal '() sent))
  (let ((sent '()))
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 6
      :type http-kit/http2::+http2-settings-type+
      :flags 0
      :stream-id 0
      :payload (octets 0 2 0 0 0 0))
     (lambda (wire) (push wire sent)))
    (ensure-equal (list (h2-frame 4 1 0 (octets))) sent))
  (signals http-protocol-error
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 1
      :type http-kit/http2::+http2-settings-type+
      :flags http-kit/http2::+http2-ack-flag+
      :stream-id 0
      :payload (octets 0))
     nil))
  (http-kit/http2::%h2-handle-control-frame
   (http-kit/http2::%make-h2-frame
    :length 0
    :type http-kit/http2::+http2-settings-type+
    :flags http-kit/http2::+http2-ack-flag+
    :stream-id 0
    :payload (octets))
   nil)
  (signals http-protocol-error
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 0
      :type http-kit/http2::+http2-settings-type+
      :flags 0
      :stream-id 1
      :payload (octets))
     nil))
  (let ((sent '()))
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 8
      :type http-kit/http2::+http2-ping-type+
      :flags 0
      :stream-id 0
      :payload (octets 1 2 3 4 5 6 7 8))
     (lambda (wire) (push wire sent)))
    (ensure-equal
     (list (h2-frame 6 1 0 (octets 1 2 3 4 5 6 7 8)))
     sent))
  (http-kit/http2::%h2-handle-control-frame
   (http-kit/http2::%make-h2-frame
    :length 8
    :type http-kit/http2::+http2-ping-type+
    :flags http-kit/http2::+http2-ack-flag+
    :stream-id 0
    :payload (octets 1 2 3 4 5 6 7 8))
   nil)
  (signals http-protocol-error
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 8
      :type http-kit/http2::+http2-ping-type+
      :flags 0
      :stream-id 1
      :payload (octets 1 2 3 4 5 6 7 8))
     nil))
  (signals http-protocol-error
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 3
      :type http-kit/http2::+http2-ping-type+
      :flags 0
      :stream-id 0
      :payload (octets 1 2 3))
     nil))
  (http-kit/http2::%h2-handle-control-frame
   (http-kit/http2::%make-h2-frame
    :length 4
    :type http-kit/http2::+http2-window-update-type+
    :flags 0
    :stream-id 0
    :payload (octets 0 0 0 1))
   nil)
  (http-kit/http2::%h2-handle-control-frame
   (http-kit/http2::%make-h2-frame
    :length 4
    :type http-kit/http2::+http2-window-update-type+
    :flags 0
    :stream-id 1
    :payload (octets 0 0 0 1))
   nil)
  (signals http-unsupported-feature
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 4
      :type http-kit/http2::+http2-window-update-type+
      :flags 0
      :stream-id 3
      :payload (octets 0 0 0 1))
     nil))
  (signals http-protocol-error
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 3
      :type http-kit/http2::+http2-window-update-type+
      :flags 0
      :stream-id 0
      :payload (octets 0 0 1))
     nil))
  (signals http-protocol-error
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 4
      :type http-kit/http2::+http2-window-update-type+
      :flags 0
      :stream-id 0
      :payload (octets 0 0 0 0))
     nil))
  (signals http-protocol-error
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 4
      :type http-kit/http2::+http2-goaway-type+
      :flags 0
      :stream-id 1
      :payload (octets 0 0 0 0 0 0 0 0))
     nil))
  (signals http-protocol-error
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 7
      :type http-kit/http2::+http2-goaway-type+
      :flags 0
      :stream-id 0
      :payload (octets 0 0 0 0 0 0 0))
     nil))
  (signals http-protocol-error
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 8
      :type http-kit/http2::+http2-goaway-type+
      :flags 0
      :stream-id 0
      :payload (octets #x80 0 0 0 0 0 0 0))
     nil))
  (signals http-connection-error
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 8
      :type http-kit/http2::+http2-goaway-type+
      :flags 0
      :stream-id 0
      :payload (octets 0 0 0 1 0 0 0 2))
     nil))
  (signals http-protocol-error
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 3
      :type http-kit/http2::+http2-rst-stream-type+
      :flags 0
      :stream-id 1
      :payload (octets 0 0 0))
     nil))
  (signals http-protocol-error
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 4
      :type http-kit/http2::+http2-rst-stream-type+
      :flags 0
      :stream-id 0
      :payload (octets 0 0 0 1))
     nil))
  (signals http-unsupported-feature
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 4
      :type http-kit/http2::+http2-rst-stream-type+
      :flags 0
      :stream-id 3
      :payload (octets 0 0 0 1))
     nil))
  (signals http-connection-error
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 4
      :type http-kit/http2::+http2-rst-stream-type+
      :flags 0
      :stream-id 1
      :payload (octets 0 0 0 1))
     nil))
  (signals http-protocol-error
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 4
      :type http-kit/http2::+http2-priority-type+
      :flags 0
      :stream-id 1
      :payload (octets 0 0 0 0))
     nil))
  (http-kit/http2::%h2-handle-control-frame
   (http-kit/http2::%make-h2-frame
    :length 5
    :type http-kit/http2::+http2-priority-type+
    :flags 0
    :stream-id 1
    :payload (octets 0 0 0 0 0))
   nil)
  (signals http-protocol-error
    (http-kit/http2::%h2-handle-control-frame
     (http-kit/http2::%make-h2-frame
      :length 5
      :type http-kit/http2::+http2-priority-type+
      :flags 0
      :stream-id 1
      :payload (octets 0 0 0 1 0))
     nil))
  (http-kit/http2::%h2-handle-control-frame
   (http-kit/http2::%make-h2-frame
    :length 0 :type 99 :flags 0 :stream-id 0 :payload (octets))
   nil))
