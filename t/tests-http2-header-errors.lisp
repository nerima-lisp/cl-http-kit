(in-package #:http-kit/test)

(deftest http2-header-block-errors
  (labels ((read-header-block-with (continuations frame-limit continuation-limit
                                    parts-limit)
             (let ((remaining (copy-list continuations))
                   (http-kit/http2::*h2-max-header-block-frames* frame-limit)
                   (http-kit/http2::*h2-max-continuation-frames*
                     continuation-limit)
                   (http-kit/http2::*h2-max-header-block-parts* parts-limit))
               (http-kit/http2::%h2-read-header-block
                (h2-frame-object http-kit/http2::+http2-headers-type+ 0 1
                                 (octets #x88))
                (h2-reader-from-frames) 16384 nil (lambda () 0d0) 16384 1
                (lambda () (pop remaining))))))
    (multiple-value-bind (block end-stream)
        (read-header-block-with
         (list (h2-frame-object http-kit/http2::+http2-continuation-type+
                                http-kit/http2::+http2-end-headers-flag+
                                1 (octets)))
         2 1 2)
      (ensure-equal (octets #x88) block)
      (ensure-false end-stream))
    (signals http-protocol-error
      (read-header-block-with
       (list (h2-frame-object http-kit/http2::+http2-continuation-type+ 0 1
                              (octets))
             (h2-frame-object http-kit/http2::+http2-continuation-type+
                              http-kit/http2::+http2-end-headers-flag+ 1
                              (octets)))
       2 10 10))
    (signals http-protocol-error
      (read-header-block-with
       (list (h2-frame-object http-kit/http2::+http2-continuation-type+ 0 1
                              (octets))
             (h2-frame-object http-kit/http2::+http2-continuation-type+
                              http-kit/http2::+http2-end-headers-flag+ 1
                              (octets)))
       10 1 10))
    (signals http-protocol-error
      (read-header-block-with
       (list (h2-frame-object http-kit/http2::+http2-continuation-type+ 0 1
                              (octets))
             (h2-frame-object http-kit/http2::+http2-continuation-type+
                              http-kit/http2::+http2-end-headers-flag+ 1
                              (octets)))
       10 10 2))
    (let ((continuations
            (loop repeat 127 collect
              (h2-frame-object http-kit/http2::+http2-continuation-type+
                               0 1 (octets)))))
      (setf continuations
            (nconc continuations
                   (list
                    (h2-frame-object http-kit/http2::+http2-continuation-type+
                                     http-kit/http2::+http2-end-headers-flag+
                                     1 (octets)))))
      (signals http-protocol-error
        (read-header-block-with continuations 128 127 128)))
    (let ((remaining
            (list (h2-frame-object http-kit/http2::+http2-continuation-type+
                                   0 1 (octets))
                  (h2-frame-object http-kit/http2::+http2-continuation-type+
                                   http-kit/http2::+http2-end-headers-flag+
                                   1 (octets))))
          (http-kit/http2::*h2-max-header-block-frames* 2)
          (http-kit/http2::*h2-max-continuation-frames* 1)
          (http-kit/http2::*h2-max-header-block-parts* 2))
      (signals http-protocol-error
        (http-kit/http2::%h2-batch-read-header-block
         (h2-frame-object http-kit/http2::+http2-headers-type+ 0 1
                          (octets #x88))
         (lambda () (pop remaining)) 16384 16384))))
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
   (http-kit/http2::%h2-regular-header-valid-p
    "te" "trailers" :allow-te-p t))
  (signals http-invalid-header
    (http-kit/http2::%h2-status-and-headers
     (list (cons ":status" "200") (cons "te" "trailers"))))
  (signals http-invalid-header
    (http-kit/http2::%h2-trailers (list (cons "te" "trailers"))))
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
  (signals http-invalid-status
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
     (list (cons "host" "example.test"))))
  (dolist (name '("authorization" "if-none-match" "content-type"
                  "cache-control" "set-cookie" "via"))
    (signals http-invalid-header
      (http-kit/http2::%h2-trailers
       (list (cons name "forbidden")))))
  (signals http-invalid-header
    (http-kit/http2::%h2-trailers
     (list (cons "X-Trailer" "value"))))
  (ensure-equal '("done")
                (http-header-values
                 (http-kit/http2::%h2-trailers
                  (list (cons "x-trailer" "done")))
                 "x-trailer")))
