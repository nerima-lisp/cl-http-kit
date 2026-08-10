(in-package #:http-kit/test)

(deftest utility-boundaries
  (ensure-true (realp (http-kit::%monotonic-time)))
  (ensure-true (realp (http-deadline 0d0)))
  (ensure-equal 12d0
                (http-deadline 2d0 :clock-function (lambda () 10d0)))
  (ensure-equal 9d0
                (http-deadline 2d0 :deadline 9d0
                                :clock-function (lambda () 10d0)))
  (ensure-equal 12d0
                (with-http-deadline (deadline 2d0
                                         :clock-function (lambda () 10d0))
                  deadline))
  (signals http-protocol-error
    (http-deadline -1d0 :clock-function (lambda () 10d0)))
  (signals http-protocol-error
    (http-deadline 1d0 :deadline :invalid
                   :clock-function (lambda () 10d0)))
  (signals http-protocol-error
    (http-deadline "invalid" :clock-function (lambda () 10d0)))
  (signals http-timeout
    (http-kit::%check-deadline 10d0 (lambda () 10d0) :write))
  (let ((source (octets 1 2)))
    (let ((copy (http-kit::%copy-octets source)))
      (setf (aref copy 0) 9)
      (ensure-equal (octets 1 2) source)))
  (ensure-equal (octets 1 2)
                (http-kit::%copy-octets '(1 2)))
  (signals http-protocol-error
    (http-kit::%copy-octets "not-octets"))
  (signals http-protocol-error
    (http-kit::%copy-octets '(1 :bad)))
  (signals http-protocol-error
    (http-kit::%copy-octets (vector 1 :bad)))
  (signals http-protocol-error
    (http-kit::%copy-octets
     (make-array '(1 1)
                 :element-type '(unsigned-byte 8)
                 :initial-element 0)))
  (signals http-protocol-error
    (http-kit::%copy-octets '(1 2) :allow-list nil))
  (ensure-equal (octets 65 66)
                (http-kit::%string-octets "AB"))
  (let ((builder (http-kit::%make-byte-builder)))
    (http-kit::%builder-write-string builder "A")
    (ensure-equal (octets 65)
                  (subseq builder 0 (fill-pointer builder))))
  (signals http-protocol-error
    (http-kit::%string-octets (string (code-char #x100))))
  (ensure-equal "AB"
                (http-kit::%octets-string (octets 65 66)))
  (signals http-protocol-error
    (http-kit::%octets-string (list 256)))
  (signals http-protocol-error
    (http-kit::%octets-string (list :invalid)))
  (ensure-equal "abc" (http-kit::%ascii-lowercase "ABC"))
  (ensure-true (http-kit::%ascii-name-char-p #\!))
  (ensure-true (not (http-kit::%ascii-name-char-p #\Space)))
  (ensure-true
   (not (http-kit::%ascii-name-char-p (code-char #x80))))
  (ensure-true (http-kit::%token-p "x-test"))
  (ensure-true (not (http-kit::%token-p "")))
  (ensure-true (http-kit::%header-name-p "x-test"))
  (ensure-true (not (http-kit::%header-name-p "x test")))
  (ensure-true
   (http-kit::%header-value-p (format nil " ~Cvalue~C " #\Tab #\Tab)))
  (ensure-true (not (http-kit::%header-value-p (string (code-char 1)))))
  (ensure-equal
   "value"
   (http-kit::%trim-ows
    (format nil "  value~C~C " #\Tab #\Tab)))
  (ensure-equal (octets 1 2 3)
                (http-kit::%join-octets (octets 1) (octets 2 3)))
  (ensure-equal (octets 1 2 3)
                (http-kit::%append-octet (octets 1 2) 3))
  (ensure-true (http-kit::%decimal-string-p "123"))
  (ensure-true (not (http-kit::%decimal-string-p "12x")))
  (ensure-true (not (http-kit::%decimal-string-p "")))
  (ensure-equal 123 (http-kit::%parse-decimal "123"))
  (signals http-protocol-error
    (http-kit::%parse-decimal "12x"))
  (ensure-equal 15 (http-kit::%hex-digit #\f))
  (signals http-protocol-error
    (http-kit::%hex-digit #\g))
  (dolist (character (list #\/ (code-char #x40) (code-char #x60)))
    (signals http-protocol-error
      (http-kit::%hex-digit character)))
  (ensure-equal "abc..." (http-kit::%bounded-diagnostic "abcdef" 3)))

(deftest transport-validation-boundaries
  (let ((request (make-http-request :method "GET"
                                    :uri "http://127.0.0.1/")))
    (signals http-protocol-error
      (send-http-request-over-stream request))
    (signals http-protocol-error
      (send-http-request-over-stream
       request :open-stream #'identity :close-stream 7))
    (signals http-protocol-error
      (make-recording-session :responses 7))
    (signals http-protocol-error
      (make-recording-session :response-function 7))
    (signals http-protocol-error
      (http-kit/http2:make-http2-client
       :exchange 7
       :open-stream #'identity))
    (signals http-protocol-error
      (send-http-request-over-stream nil :open-stream #'identity))
    (signals http-protocol-error
      (send-recorded-http-request nil nil))
    (signals http-protocol-error
      (send-recorded-http-request (make-recording-session) nil))
    (signals http-connection-error
      (send-recorded-http-request (make-recording-session) request))
    (signals http-protocol-error
      (send-recorded-http-request
       (make-recording-session
        :response-function (lambda (ignored-request &key timeout deadline)
                             (declare (ignore ignored-request timeout deadline))
                             7))
       request))
    (signals http-connection-error
      (send-recorded-http-request
       (make-recording-session
        :response-function (lambda (ignored-request &key timeout deadline)
                             (declare (ignore ignored-request timeout deadline))
                             (error "recording failure")))
       request))
    (signals http-protocol-error
      (send-recorded-http-request
       (make-recording-session
        :responses (list (make-http-response :status 200)))
       request
       :max-header-bytes 0))
    (signals http-protocol-error
      (send-recorded-http-request
       (make-recording-session
        :responses (list (make-http-response :status 200)))
       request
       :max-body-bytes -1))
    (signals http-protocol-error
      (send-recorded-http-request
       (make-recording-session
        :responses (list (make-http-response :status 200)))
       request
       :max-header-bytes "invalid"))
    (signals http-protocol-error
      (send-recorded-http-request
       (make-recording-session
        :responses (list (make-http-response :status 200)))
       request
       :max-body-bytes "invalid"))))

(deftest http2-reader-and-data-boundaries
  (ensure-true (http-kit/http2::%h2-no-body-response-p "HEAD" 200))
  (ensure-true (http-kit/http2::%h2-no-body-response-p "GET" 204))
  (ensure-true (not (http-kit/http2::%h2-no-body-response-p "GET" 200)))
  (ensure-true
   (not (http-kit/http2::%h2-no-body-response-p nil 200)))
  (let ((wire (http-kit/http2::%h2-frame-wire
               http-kit/http2::+http2-data-type+
               http-kit/http2::+http2-end-stream-flag+
               1
               (octets 65))))
    (ensure-equal (h2-frame 0 1 1 (octets 65)) wire)
    (let* ((reader (http-kit/http2::%h2-reader-for wire))
           (frame (http-kit/http2::%h2-read-frame
                   reader 16384 nil (lambda () 0d0))))
    (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id frame))
      (ensure-equal (octets 65) (http-kit/http2::%h2-frame-payload frame))
      (ensure-equal :eof
                    (http-kit/http2::%h2-read-frame
                     reader 16384 nil (lambda () 0d0)))))
  (signals http-protocol-error
    (http-kit/http2::%h2-frame-wire
     http-kit/http2::+http2-data-type+ 256 1 (octets)))
  (let ((reader (http-kit/http2::%h2-reader-for '(1 2 3))))
    (ensure-equal (octets 1 2)
                  (http-kit/http2::%h2-reader-read
                   reader 2 nil (lambda () 0d0)))
    (signals http-protocol-error
      (http-kit/http2::%h2-reader-read
       reader 2 nil (lambda () 0d0)))
    (signals http-protocol-error
      (http-kit/http2::%h2-reader-read
       reader -1 nil (lambda () 0d0))))
  (signals http-protocol-error
    (http-kit/http2::%h2-reader-for 7))
  (let* ((body (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0))
         (frame (http-kit/http2::%make-h2-frame
                 :length 1
                 :type http-kit/http2::+http2-data-type+
                 :flags http-kit/http2::+http2-end-stream-flag+
                 :stream-id 1
                 :payload (octets 65))))
    (ensure-true
     (http-kit/http2::%h2-append-data-frame frame 200 body "GET" 1))
    (ensure-equal (octets 65) body)
    (signals http-kit:http-size-limit-exceeded
      (http-kit/http2::%h2-append-data-frame frame 200 body "GET" 1))
    (signals http-protocol-error
      (http-kit/http2::%h2-append-data-frame frame nil body "GET" 10))
    (signals http-protocol-error
      (http-kit/http2::%h2-append-data-frame frame 200 body "HEAD" 10))
    (let ((nil-method-frame
            (http-kit/http2::%make-h2-frame
             :length 0
             :type http-kit/http2::+http2-data-type+
             :flags http-kit/http2::+http2-end-stream-flag+
             :stream-id 1
             :payload (octets))))
      (ensure-true
       (http-kit/http2::%h2-append-data-frame
        nil-method-frame 200
        (make-array 0 :element-type '(unsigned-byte 8)
                    :adjustable t :fill-pointer 0)
        nil 10))))
  (let* ((body (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0))
         (frame (http-kit/http2::%make-h2-frame
                 :length 1
                 :type http-kit/http2::+http2-data-type+
                 :flags 0
                 :stream-id 1
                 :payload (octets 66))))
    (ensure-equal nil
                  (http-kit/http2::%h2-append-data-frame
                   frame 200 body "GET" 10))
    (ensure-equal (octets 66) body))
  (let ((no-body-frame
          (http-kit/http2::%make-h2-frame
           :length 1
           :type http-kit/http2::+http2-data-type+
           :flags http-kit/http2::+http2-end-stream-flag+
           :stream-id 1
           :payload (octets 65))))
    (signals http-protocol-error
      (http-kit/http2::%h2-append-data-frame
       no-body-frame 204
       (make-array 0 :element-type '(unsigned-byte 8)
                   :adjustable t :fill-pointer 0)
       "GET" 10)))
  (let ((padded-frame
          (http-kit/http2::%make-h2-frame
           :length 1
           :type http-kit/http2::+http2-data-type+
           :flags http-kit/http2::+http2-padded-flag+
           :stream-id 1
           :payload (octets 65))))
    (signals http-protocol-error
      (http-kit/http2::%h2-append-data-frame
       padded-frame 200
       (make-array 0 :element-type '(unsigned-byte 8)
                   :adjustable t :fill-pointer 0)
       "GET" 10)))
  (let ((other-stream-frame
          (http-kit/http2::%make-h2-frame
           :length 0
           :type http-kit/http2::+http2-data-type+
           :flags http-kit/http2::+http2-end-stream-flag+
           :stream-id 3
           :payload (octets))))
    (signals http-unsupported-feature
      (http-kit/http2::%h2-append-data-frame
       other-stream-frame 200
       (make-array 0 :element-type '(unsigned-byte 8)
                   :adjustable t :fill-pointer 0)
       "GET" 10))))
