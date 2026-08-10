(in-package #:http-kit/test)

(deftest http2-header-block-errors
  (let* ((first (h2-frame-object
                 http-kit/http2::+http2-headers-type+
                 http-kit/http2::+http2-end-stream-flag+
                 1
                 (octets #x88)))
         (reader (h2-reader-from-frames
                  (h2-frame http-kit/http2::+http2-continuation-type+
                            http-kit/http2::+http2-end-headers-flag+
                            1
                            (octets #x99)))))
    (multiple-value-bind (block end-stream)
        (http-kit/http2::%h2-read-header-block
         first reader 16384 nil (lambda () 0d0) 2)
      (ensure-equal (octets #x88 #x99) block)
      (ensure-true end-stream)))
  (let ((first (h2-frame-object
                http-kit/http2::+http2-data-type+ 0 1 (octets))))
    (signals http-protocol-error
      (http-kit/http2::%h2-read-header-block
       first (h2-reader-from-frames) 16384 nil (lambda () 0d0) 10)))
  (let ((first (h2-frame-object
                http-kit/http2::+http2-headers-type+ 0 1 (octets #x88))))
    (signals http-protocol-error
      (http-kit/http2::%h2-read-header-block
       (h2-frame-object
        http-kit/http2::+http2-headers-type+
        http-kit/http2::+http2-padded-flag+
        1
        (octets #x88))
       (h2-reader-from-frames) 16384 nil (lambda () 0d0) 10))
    (signals http-protocol-error
      (http-kit/http2::%h2-read-header-block
       first (h2-reader-from-frames) 16384 nil (lambda () 0d0) 10))
    (signals http-protocol-error
      (http-kit/http2::%h2-read-header-block
       first
       (h2-reader-from-frames
        (h2-frame http-kit/http2::+http2-data-type+ 0 1 (octets)))
       16384 nil (lambda () 0d0) 10))
    (signals http-protocol-error
      (http-kit/http2::%h2-read-header-block
       first
       (h2-reader-from-frames
        (h2-frame http-kit/http2::+http2-continuation-type+ 1 1 (octets)))
       16384 nil (lambda () 0d0) 10))
    (signals http-size-limit-exceeded
      (http-kit/http2::%h2-read-header-block
       first
       (h2-reader-from-frames
        (h2-frame http-kit/http2::+http2-continuation-type+
                  http-kit/http2::+http2-end-headers-flag+
                  1
                  (octets #x99 #x9a)))
       16384 nil (lambda () 0d0) 2)))
  (signals http-invalid-header
    (http-kit/http2::%h2-regular-header-valid-p "" "value"))
  (signals http-invalid-header
    (http-kit/http2::%h2-regular-header-valid-p "X-Test" "value"))
  (signals http-invalid-header
    (http-kit/http2::%h2-regular-header-valid-p "connection" "close"))
  (signals http-invalid-header
    (http-kit/http2::%h2-regular-header-valid-p "te" "gzip"))
  (ensure-true
   (http-kit/http2::%h2-regular-header-valid-p "te" "trailers"))
  (multiple-value-bind (status headers)
      (http-kit/http2::%h2-status-and-headers
       (list (cons ":status" "200")
             (cons "x-test" "yes")
             (cons "te" "trailers")))
    (ensure-equal 200 status)
    (ensure-equal '(("x-test" . "yes") ("te" . "trailers"))
                          (mapcar (lambda (header)
                            (cons (http-header-name header)
                                  (http-header-content header)))
                          headers)))
  (signals http-invalid-header
    (http-kit/http2::%h2-status-and-headers
     (list (cons "" "value"))))
  (signals http-protocol-error
    (http-kit/http2::%h2-status-and-headers
     (list (cons "x-test" "yes") (cons ":status" "200"))))
  (signals http-invalid-header
    (http-kit/http2::%h2-status-and-headers
     (list (cons ":method" "GET") (cons ":status" "200"))))
  (signals http-invalid-status
    (http-kit/http2::%h2-status-and-headers
     (list (cons ":status" "20x"))))
  (signals http-invalid-status
    (http-kit/http2::%h2-status-and-headers
     (list (cons ":status" "20"))))
  (signals http-invalid-status
    (http-kit/http2::%h2-status-and-headers
     (list (cons ":status" "099"))))
  (signals http-invalid-status
    (http-kit/http2::%h2-status-and-headers
     (list (cons ":status" "200") (cons ":status" "204"))))
  (signals http-unsupported-feature
    (http-kit/http2::%h2-status-and-headers
     (list (cons ":status" "101"))))
  (signals http-invalid-header
    (http-kit/http2::%h2-trailers
     (list (cons "" "value"))))
  (signals http-invalid-header
    (http-kit/http2::%h2-trailers
     (list (cons ":status" "200"))))
  (signals http-invalid-header
    (http-kit/http2::%h2-trailers
     (list (cons "content-length" "1"))))
  (signals http-invalid-header
    (http-kit/http2::%h2-trailers
     (list (cons "X-Trailer" "value"))))
  (ensure-equal '("done")
                (http-header-values
                 (http-kit/http2::%h2-trailers
                  (list (cons "x-trailer" "done")))
                 "x-trailer")))
