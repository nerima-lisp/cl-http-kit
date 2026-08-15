(in-package #:http-kit/http2)

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
   :clock-function clock-function))
