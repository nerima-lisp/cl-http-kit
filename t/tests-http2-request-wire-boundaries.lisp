(in-package #:http-kit/test)

(deftest http2-request-wire-boundaries
  (ensure-equal (octets) (http-kit/http2::%h2-concat nil))
  (ensure-equal (octets 1 2 3)
                (http-kit/http2::%h2-concat
                 (list (octets 1) (octets 2 3))))
  (ensure-equal '("trailers" "gzip")
                (http-kit/http2::%h2-comma-items
                 (list " trailers, GZip ")
                 "te"))
  (signals http-invalid-header
    (http-kit/http2::%h2-comma-items (list "trailers,") "te"))
  (ensure-true (http-kit/http2::%h2-connection-specific-header-p
                "connection"))
  (ensure-true (not (http-kit/http2::%h2-connection-specific-header-p
                     "x-test")))
  (ensure-equal 34
                (http-kit/http2::%h2-header-list-size
                 (list (cons "a" "b"))))
  (let ((request
          (make-http-request
           :method "POST"
           :uri "https://example.test/path?q=1"
           :headers (list (make-http-header "Host" "example.test")
                           (make-http-header "X-Test" "yes")
                           (make-http-header "TE" "trailers"))
           :body (octets 65 66))))
    (ensure-equal
     (list (cons ":method" "POST")
           (cons ":scheme" "https")
           (cons ":authority" "example.test")
           (cons ":path" "/path?q=1")
           (cons "x-test" "yes")
           (cons "te" "trailers")
           (cons "content-length" "2"))
     (http-kit/http2::%h2-request-fields request))
    (ensure-true
     (plusp
      (array-total-size
       (http-kit/http2::%h2-request-wire request 32768 1000 1000)))))
  (let ((request
          (make-http-request
           :method "POST"
           :uri "https://example.test/path"
           :headers (list (make-http-header "Content-Length" "2"))
           :body (octets 65 66))))
    (ensure-equal
     (list (cons ":method" "POST")
           (cons ":scheme" "https")
           (cons ":authority" "example.test")
           (cons ":path" "/path")
           (cons "content-length" "2"))
     (http-kit/http2::%h2-request-fields request)))
  (ensure-equal
   (list (cons ":method" "CONNECT")
         (cons ":authority" "example.test"))
   (http-kit/http2::%h2-request-fields
    (make-http-request :method "CONNECT"
                       :uri "https://example.test/")))
  (ensure-equal
   "*"
   (cdr (assoc ":path"
               (http-kit/http2::%h2-request-fields
                (make-http-request :method "OPTIONS"
                                   :uri "https://example.test/"
                                   :request-target "*"))
               :test #'string=)))
  (signals http-invalid-header
    (http-kit/http2::%h2-request-fields
     (make-http-request :method "GET"
                        :uri "https://example.test/"
                        :request-target "*")))
  (signals http-invalid-header
    (http-kit/http2::%h2-request-fields
     (make-http-request :method "GET"
                        :uri "https://example.test/"
                        :request-target "https://example.test/")))
  (let ((request
          (make-http-request :method "CONNECT"
                             :protocol "websocket"
                             :uri "https://example.test/")))
    (ensure-equal "websocket" (http-request-protocol request))
    (ensure-equal
     (list (cons ":method" "CONNECT")
           (cons ":protocol" "websocket")
           (cons ":scheme" "https")
           (cons ":authority" "example.test")
           (cons ":path" "/"))
     (http-kit/http2::%h2-request-fields request)))
  (signals http-invalid-header
    (http-kit/http2::%h2-request-fields
     (make-http-request :method "CONNECT"
                        :protocol "websocket"
                        :uri "https://example.test/"
                        :request-target "*")))
  (signals http-invalid-header
    (http-kit/http2::%h2-request-fields
     (make-http-request
      :method "GET"
      :uri "https://example.test/"
      :headers (list (make-http-header "Connection" "close")))))
  (signals http-unsupported-feature
    (http-kit/http2::%h2-request-fields
     (make-http-request
      :method "GET"
      :uri "https://example.test/"
      :headers (list (make-http-header "TE" "gzip")))))
  (signals http-invalid-header
    (http-kit/http2::%h2-request-fields
     (make-http-request
      :method "GET"
      :uri "https://example.test/"
      :headers (list (make-http-header "Host" "example.test")
                     (make-http-header "host" "example.test")))))
  (signals http-invalid-header
    (http-kit/http2::%h2-request-fields
     (make-http-request
      :method "GET"
      :uri "https://example.test/"
      :headers (list (make-http-header "Host" "other.test")))))
  (signals http-invalid-header
    (http-kit/http2::%h2-request-fields
     (make-http-request
      :method "GET"
      :uri "https://example.test/"
      :headers (list (make-http-header "Content-Length" "x")))))
  (signals http-invalid-header
    (http-kit/http2::%h2-request-fields
     (make-http-request
      :method "POST"
      :uri "https://example.test/"
      :headers (list (make-http-header "Content-Length" "1")
                     (make-http-header "content-length" "2"))
      :body (octets 65))))
  (signals http-invalid-header
    (http-kit/http2::%h2-request-fields
     (make-http-request
      :method "POST"
      :uri "https://example.test/"
      :headers (list (make-http-header "Content-Length" "0"))
      :body (octets 65))))
  (let ((headers
          (http-kit/http2::%h2-header-frames
           (octets 1 2 3 4 5) t 3)))
    (ensure-equal
     (list (h2-frame 1 1 1 (octets 1 2 3))
           (h2-frame 9 4 1 (octets 4 5)))
     headers))
  (ensure-equal '()
                (http-kit/http2::%h2-header-frames (octets) nil 3))
  (ensure-equal
   (list (h2-frame 0 0 1 (octets 1 2))
         (h2-frame 0 0 1 (octets 3 4))
         (h2-frame 0 1 1 (octets 5)))
   (http-kit/http2::%h2-data-frames (octets 1 2 3 4 5) 2))
  (ensure-equal '()
                (http-kit/http2::%h2-data-frames (octets) 2))
  (let ((caught nil))
    (handler-case
        (http-kit/http2::%h2-data-frames
         (make-array (1+ http-kit/http2::+http2-default-window-size+)
                     :element-type '(unsigned-byte 8)
                     :initial-element 0)
         16384)
      (http-unsupported-feature (condition)
        (setf caught condition)))
    (ensure-true caught)
    (ensure-equal :http2-flow-control
                  (http-unsupported-feature-name caught)))
  (signals http-size-limit-exceeded
    (http-kit/http2::%h2-request-wire
     (make-http-request :method "GET" :uri "https://example.test/")
     16384 1 1000))
  (signals http-size-limit-exceeded
    (http-kit/http2::%h2-request-wire
     (make-http-request :method "POST"
                        :uri "https://example.test/"
                        :body (octets 1 2))
     16384 1000 1))
  (signals http-size-limit-exceeded
    (http-kit/http2::%h2-request-header-wire
     (make-http-request :method "GET" :uri "https://example.test/")
     16384 1000 1000
     :include-session-p nil
     :peer-max-header-list-size 1)))

(deftest http2-request-wire-huffman
  (let* ((request
           (make-http-request
            :method "GET"
            :uri "https://example.test/path"
            :headers (list (make-http-header
                            "X-Huffman" "www.example.com"))))
         (wire (http-kit/http2::%h2-request-wire
                request 16384 1000 1000
                :include-session-p nil
                :huffman-p t))
         (reader (http-kit/http2::%h2-reader-for wire))
         (headers (http-kit/http2::%h2-read-frame reader 16384 nil nil))
         (context (http-kit/http2::%make-hpack-context))
         (huffman-value
           (octets #x8c #xf1 #xe3 #xc2 #xe5 #xf2 #x3a
                   #x6b #xa0 #xab #x90 #xf4 #xff)))
    (ensure-equal 1 (http-kit/http2::%h2-frame-type headers))
    (ensure-equal 5 (http-kit/http2::%h2-frame-flags headers))
    (ensure-true (search huffman-value
                         (http-kit/http2::%h2-frame-payload headers)))
    (ensure-equal
     (list (cons ":method" "GET")
           (cons ":scheme" "https")
           (cons ":authority" "example.test")
           (cons ":path" "/path")
           (cons "x-huffman" "www.example.com"))
     (http-kit/http2::%hpack-decode-block
      (http-kit/http2::%h2-frame-payload headers) context))
    (ensure-equal :eof (http-kit/http2::%h2-read-frame reader 16384 nil nil))))

(deftest http2-client-huffman-option
  (let* ((observed-wire nil)
        (response
          (http-kit/http2:send-http2-request
           (http-kit/http2:make-http2-client
            :exchange (lambda (request wire &key timeout deadline)
                        (declare (ignore request timeout deadline))
                        (setf observed-wire wire)
                        (h2-response-wire (octets))))
           (make-http-request
            :method "GET"
            :uri "https://example.test/path"
            :headers (list (make-http-header
                            "X-Huffman" "www.example.com")))
           :huffman-p t)))
    (ensure-equal 200 (http-response-status response))
    (ensure-true observed-wire)
    (let* ((reader (http-kit/http2::%h2-reader-for
                    (subseq observed-wire (length (h2-preface)))))
           (settings (http-kit/http2::%h2-read-frame reader 16384 nil nil))
           (headers (http-kit/http2::%h2-read-frame reader 16384 nil nil))
           (context (http-kit/http2::%make-hpack-context)))
      (ensure-equal 4 (http-kit/http2::%h2-frame-type settings))
      (ensure-equal 1 (http-kit/http2::%h2-frame-type headers))
      (ensure-equal
       (list (cons ":method" "GET")
             (cons ":scheme" "https")
             (cons ":authority" "example.test")
             (cons ":path" "/path")
             (cons "x-huffman" "www.example.com"))
       (http-kit/http2::%hpack-decode-block
        (http-kit/http2::%h2-frame-payload headers) context)))))

(deftest http2-request-trailer-wire-boundaries
  (let* ((request
           (make-http-request
            :method "POST"
            :uri "https://example.test/trailers"
            :trailers (list (make-http-header "X-Checksum" "done"))))
         (wire (http-kit/http2::%h2-request-wire
                request 16384 1000 1000
                :include-session-p nil))
         (reader (http-kit/http2::%h2-reader-for wire))
         (initial (http-kit/http2::%h2-read-frame reader 16384 nil nil))
         (trailer (http-kit/http2::%h2-read-frame reader 16384 nil nil))
         (context (http-kit/http2::%make-hpack-context)))
    (ensure-equal 1 (http-kit/http2::%h2-frame-type initial))
    (ensure-equal 4 (http-kit/http2::%h2-frame-flags initial))
    (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id initial))
    (ensure-equal 1 (http-kit/http2::%h2-frame-type trailer))
    (ensure-equal 5 (http-kit/http2::%h2-frame-flags trailer))
    (ensure-equal 1 (http-kit/http2::%h2-frame-stream-id trailer))
    (http-kit/http2::%hpack-decode-block
     (http-kit/http2::%h2-frame-payload initial) context)
    (ensure-equal
     (list (cons "x-checksum" "done"))
     (http-kit/http2::%hpack-decode-block
      (http-kit/http2::%h2-frame-payload trailer) context))
    (ensure-equal :eof (http-kit/http2::%h2-read-frame reader 16384 nil nil)))
  (let* ((body (octets 65 66))
         (request
           (make-http-request
            :method "POST"
            :uri "https://example.test/trailers"
            :body body
            :trailers (list (make-http-header "X-Checksum" "done"))))
         (wire (http-kit/http2::%h2-request-wire
                request 16384 1000 1000
                :include-session-p nil))
         (reader (http-kit/http2::%h2-reader-for wire))
         (initial (http-kit/http2::%h2-read-frame reader 16384 nil nil))
         (data (http-kit/http2::%h2-read-frame reader 16384 nil nil))
         (trailer (http-kit/http2::%h2-read-frame reader 16384 nil nil)))
    (ensure-equal 1 (http-kit/http2::%h2-frame-type initial))
    (ensure-equal 4 (http-kit/http2::%h2-frame-flags initial))
    (ensure-equal 0 (http-kit/http2::%h2-frame-flags data))
    (ensure-equal body (http-kit/http2::%h2-frame-payload data))
    (ensure-equal 1 (http-kit/http2::%h2-frame-type trailer))
    (ensure-equal 5 (http-kit/http2::%h2-frame-flags trailer))
    (ensure-equal :eof (http-kit/http2::%h2-read-frame reader 16384 nil nil)))
  (dolist (trailers
            (list (list (make-http-header "Connection" "close"))
                  (list (make-http-header "Content-Length" "1"))
                  (list (make-http-header "Host" "example.test"))
                  (list (make-http-header "Authorization" "secret"))
                  (list (make-http-header "If-None-Match" "tag"))
                  (list (make-http-header "Content-Type" "text/plain"))
                  (list (make-http-header "Cache-Control" "no-cache"))
                  (list (make-http-header "Set-Cookie" "a=b"))
                  (list (make-http-header "TE" "trailers"))
                  (list (make-http-header "TE" "gzip"))))
    (signals http-invalid-header
      (http-kit/http2::%h2-request-wire
       (make-http-request :method "POST"
                          :uri "https://example.test/"
                          :trailers trailers)
       16384 1000 1000
       :include-session-p nil))))

#+sbcl
(deftest http2-public-peer-trailer-header-limit
  (dolist (limit '(255 256))
    (let* ((value (make-string 214 :initial-element #\x))
           (initial-input (concatenate-octets
                           (h2-frame 4 0 0 (octets 0 6 0 0 (floor limit 256) (mod limit 256)))
                           (h2-frame 1 5 1 (octets #x88))))
           (input (concatenate-octets initial-input (h2-frame 1 5 3 (octets #x88))))
           (stream (make-instance 'binary-session-stream :input input))
           (closes 0)
           (opened nil)
           (seen nil)
           (connection
             (http-kit/http2:make-http2-connection
              :stream stream
              :close-stream (lambda (actual)
                              (ensure-true (eq actual stream))
                              (incf closes)
                              (close actual)))))
      (unwind-protect
           (let* ((first-response
                    (http-kit/http2:send-http2-request-over-connection
                     connection (make-http-request :method "GET" :uri "https://example.test/")))
                  (before (copy-seq (binary-session-output stream)))
                  (result
                    (handler-case
                        (handler-bind ((http-error (lambda (condition) (setf seen condition))))
                          (http-kit/http2:send-http2-request-over-connection
                           connection
                           (make-http-request
                            :method "POST" :uri "https://example.test/"
                            :body (octets 65)
                            :trailers (list (make-http-header "x-checksum" value)))
                           :on-stream-open (lambda (id cancel)
                                             (ensure-true (functionp cancel))
                                             (push id opened))))
                      (http-error (condition) condition))))
             (ensure-equal 200 (http-response-status first-response))
             (ensure-equal (octets) (http-response-body first-response))
             (ensure-equal limit (http-kit/http2:http2-connection-peer-max-header-list-size connection))
             (if (= limit 255)
                 (progn
                   (ensure-true (typep result 'http-size-limit-exceeded))
                   (ensure-true (eq seen result))
                   (ensure-equal :response-parse (http-error-operation result))
                   (ensure-equal :headers (http-kit:http-size-limit-exceeded-kind result))
                   (ensure-equal 255 (http-kit:http-size-limit-exceeded-limit result))
                   (ensure-equal 256 (http-kit:http-size-limit-exceeded-observed result))
                   (ensure-equal before (binary-session-output stream))
                   (ensure-equal (length initial-input) (binary-session-input-position stream))
                   (ensure-false opened)
                   (ensure-equal 1 closes))
                 (progn
                   (ensure-equal 200 (http-response-status result))
                   (ensure-equal (octets) (http-response-body result))
                   (ensure-false seen)
                   (ensure-equal '(3) opened)
                   (ensure-equal (length input) (binary-session-input-position stream))
                   (ensure-equal 0 closes)
                   (let ((frames (nthcdr 3 (h2-output-frames stream))))
                     (ensure-equal '(1 0 1) (mapcar #'http-kit/http2::%h2-frame-type frames))
                     (ensure-equal '(4 0 5) (mapcar #'http-kit/http2::%h2-frame-flags frames))
                     (ensure-equal '(3 3 3) (mapcar #'http-kit/http2::%h2-frame-stream-id frames))
                     (ensure-equal (octets 65) (http-kit/http2::%h2-frame-payload (second frames)))
                     (ensure-equal (list (cons "x-checksum" value))
                                   (server-response-fields (third frames)))))))
        (http-kit/http2:close-http2-connection connection))
      (ensure-equal 1 closes)
      (ensure-false (open-stream-p stream)))))
