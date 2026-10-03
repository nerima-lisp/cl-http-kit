(in-package #:http-kit/test)

(defmacro with-test-client ((client transport-function &rest options) &body body)
  "Bind CLIENT to a client using the in-process transport fake.

The transport boundary has a deliberately complete keyword contract.  Keeping
that construction in one test macro makes individual scenarios describe only
the behavior they exercise and prevents test doubles from drifting apart."
  `(let ((,client (make-http-client
                   :transport-function ,transport-function
                   ,@options)))
     ,@body))

(defun %test-h2-append-data-frame (frame status body request-method max-body-bytes)
  (http-kit/http2::%h2-append-data-frame
   frame status body (length body) request-method max-body-bytes nil t 1))

(defun h2-frame (type flags stream-id payload)
  (let* ((length (length payload))
         (result (make-array (+ 9 length)
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (setf (aref result 0) (ldb (byte 8 16) length)
          (aref result 1) (ldb (byte 8 8) length)
          (aref result 2) (ldb (byte 8 0) length)
          (aref result 3) type
          (aref result 4) flags
          (aref result 5) (ldb (byte 8 24) stream-id)
          (aref result 6) (ldb (byte 8 16) stream-id)
          (aref result 7) (ldb (byte 8 8) stream-id)
          (aref result 8) (ldb (byte 8 0) stream-id))
    (replace result payload :start1 9)
    result))

(defun h2-frame-object (type flags stream-id payload)
  (http-kit/http2::%make-h2-frame
   :length (length payload)
   :type type
   :flags flags
   :stream-id stream-id
   :payload payload))

(defun h2-reader-from-frames (&rest frames)
  (http-kit/http2::%h2-reader-for
   (apply #'concatenate-octets frames)))

(defun h2-header-block (&rest fields)
  (http-kit/http2::%hpack-encode-block fields))

(defun h2-response-wire (body)
  (concatenate-octets
   (h2-frame 4 0 0 (octets))
   (h2-frame 1 (if (zerop (array-total-size body)) 5 4) 1 (octets #x88))
   (if (zerop (array-total-size body))
       (octets)
       (h2-frame 0 1 1 body))))

(defun h2-preface ()
  (ascii "PRI * HTTP/2.0|CRLF||CRLF|SM|CRLF||CRLF|"))
