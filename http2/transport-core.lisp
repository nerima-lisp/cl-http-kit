(in-package #:http-kit/http2)

(defun %h2-client-control-budget-check (client frame writer)
  "Count a peer control frame and send GOAWAY before reporting a flood."
  (let ((times (if (http2-client-p client)
                   (%http2-control-frame-times client)
                   (%http2-connection-control-frame-times client)))
        (max-frames (if (http2-client-p client)
                        (%http2-max-control-frames client)
                        (%http2-connection-max-control-frames client)))
        (window (if (http2-client-p client)
                    (%http2-max-control-window client)
                    (%http2-connection-max-control-window client)))
        (clock (if (http2-client-p client)
                   (%http2-clock-function client)
                   (%http2-connection-clock-function client))))
    (multiple-value-bind (new-times exceeded)
        (%h2-control-budget-note times (%h2-frame-type frame)
                                 max-frames window clock)
      (if (http2-client-p client)
          (setf (%http2-control-frame-times client) new-times)
          (setf (%http2-connection-control-frame-times client) new-times))
    (when exceeded
      (let ((payload (make-array 8 :element-type '(unsigned-byte 8))))
        (%h2-put-u32 payload 0 0)
        (%h2-put-u32 payload 4 +http2-enhance-your-calm+)
        (%h2-send-control writer +http2-goaway-type+ 0 0 payload))
      (error 'http-kit:http-protocol-error
             :message "The HTTP/2 control frame budget was exceeded."
             :operation :http2-control
             :http2-error-code +http2-enhance-your-calm+
             :detail (%h2-frame-type frame)))))
  nil)

(defun %h2-validate-limit (name value &key allow-zero)
  (unless (and (integerp value)
               (if allow-zero (>= value 0) (plusp value)))
    (error 'http-kit:http-protocol-error
           :message (format nil "The HTTP/2 ~A must be a ~A integer."
                            name (if allow-zero "non-negative" "positive"))
           :operation :http2-client
           :detail value))
  value)

(defun %h2-validate-frame-size (value)
  (unless (and (integerp value)
               (<= 16384 value #xffffff))
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 maximum frame size must be between 16384 and 16777215."
           :operation :http2-client
           :detail value))
  value)

(defun make-http2-client (&key exchange open-stream connection close-stream
                               (max-frame-size +http2-default-max-frame-size+)
                               (max-header-bytes
                                http-kit::*default-max-header-bytes*)
                               (max-fields 256)
                               (max-body-bytes
                                http-kit::*default-max-body-bytes*)
                               (max-control-frames 100)
                               (max-control-window 1.0d0)
                               (clock-function #'http-kit::%monotonic-time))
  "Create an HTTP/2 client around an injected exchange, stream, or connection.

EXCHANGE is called as (REQUEST WIRE &KEY TIMEOUT DEADLINE) and returns the
peer's binary response frames.  OPEN-STREAM is called as
(REQUEST &KEY TIMEOUT DEADLINE), and must return a binary stream.  Exactly one
of EXCHANGE, OPEN-STREAM, and CONNECTION is required.  CONNECTION is a
persistent HTTP2-CONNECTION created by MAKE-HTTP2-CONNECTION."
  (unless (and (or (null exchange) (functionp exchange))
               (or (null open-stream) (functionp open-stream))
               (or (null connection) (http2-connection-p connection))
               (= 1 (count-if #'identity
                              (list exchange open-stream connection))))
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 client requires exactly one of :EXCHANGE, :OPEN-STREAM, or :CONNECTION."
           :operation :http2-client
           :detail (list exchange open-stream connection)))
  (unless (or (null close-stream) (functionp close-stream))
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 :CLOSE-STREAM must be a function."
           :operation :http2-client
           :detail close-stream))
  (%h2-validate-frame-size max-frame-size)
  (%h2-validate-limit :max-header-bytes max-header-bytes)
  (%h2-validate-limit :max-fields max-fields)
  (%h2-validate-limit :max-body-bytes max-body-bytes :allow-zero t)
  (%h2-validate-limit :max-control-frames max-control-frames :allow-zero t)
  (unless (and (realp max-control-window)
               (not (minusp max-control-window)))
    (error 'http-kit:http-protocol-error
           :message "The HTTP/2 max-control-window must be a non-negative real."
           :operation :http2-client
           :detail max-control-window))
  (unless (functionp clock-function)
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 clock function must be callable."
           :operation :http2-client
           :detail clock-function))
  (%make-http2-client
   :exchange exchange
   :open-stream open-stream
   :connection connection
   :close-stream (or close-stream #'close)
   :max-frame-size max-frame-size
   :max-header-bytes max-header-bytes
   :max-fields max-fields
   :max-body-bytes max-body-bytes
   :max-control-frames max-control-frames
   :max-control-window max-control-window
   :clock-function clock-function))
