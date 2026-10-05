(in-package #:http-kit/test)

(deftest http2-public-transport-and-binary-body
  (let* ((expected-preface (h2-preface))
         (transport
           (http-kit/http2:make-http2-client
            :exchange (lambda (request wire &key timeout deadline)
                        (declare (ignore request timeout deadline))
                        (ensure-equal expected-preface
                                      (subseq wire 0 (length expected-preface))
                                      "HTTP/2 client preface")
                        (h2-response-wire (octets 0 #xff 7)))))
         (response (http-kit/http2:send-http2-request
                    transport
                    (make-http-request :method "GET"
                                       :uri "https://127.0.0.1/data"))))
    (ensure-equal 200 (http-response-status response) "HTTP/2 status")
    (ensure-equal "HTTP/2"
                  (http-response-protocol-version response)
                  "HTTP/2 response version")
    (ensure-equal (octets 0 #xff 7) (http-response-body response)
                  "HTTP/2 binary body")))

(deftest http2-streams-response-body-without-collecting
  (let* ((chunks '())
        (response
          (http-kit/http2:send-http2-request
           (http-kit/http2:make-http2-client
            :exchange (lambda (request wire &key timeout deadline)
                        (declare (ignore request wire timeout deadline))
                        (concatenate-octets
                         (h2-frame 4 0 0 (octets))
                         (h2-frame 1 4 1
                                   (h2-header-block
                                    (cons ":status" "200")
                                    (cons "content-length" "4")))
                         (h2-frame 0 0 1 (octets 1 2))
                         (h2-frame 0 1 1 (octets 3 4)))))
           (make-http-request :method "GET" :uri "https://127.0.0.1/")
           :on-body-chunk (lambda (chunk)
                            (push chunk chunks))
           :collect-body-p nil)))
    (ensure-equal (octets) (http-response-body response)
                  "HTTP/2 non-collecting response body")
    (ensure-equal (list (octets 1 2) (octets 3 4))
                  (nreverse chunks)
                  "HTTP/2 DATA callbacks preserve frame boundaries")))

(deftest http2-accepts-padded-response-frames
  (let* ((header-block (h2-header-block
                        (cons ":status" "200")
                        (cons "content-length" "3")))
         (response
           (http-kit/http2:send-http2-request
            (http-kit/http2:make-http2-client
             :exchange (lambda (request wire &key timeout deadline)
                         (declare (ignore request wire timeout deadline))
                         (concatenate-octets
                          (h2-frame 4 0 0 (octets))
                          (h2-frame 1 12 1
                                    (concatenate-octets
                                     (octets 2)
                                     header-block
                                     (octets 0 0)))
                          (h2-frame 0 9 1
                                    (octets 2 1 2 3 0 0)))))
            (make-http-request :method "GET"
                               :uri "https://127.0.0.1/"))))
    (ensure-equal 200 (http-response-status response)
                  "Padded HTTP/2 response status")
    (ensure-equal (octets 1 2 3) (http-response-body response)
                  "Padded HTTP/2 response body")))

(deftest http2-accepts-priority-response-headers
  (let ((response
          (http-kit/http2:send-http2-request
           (http-kit/http2:make-http2-client
            :exchange (lambda (request wire &key timeout deadline)
                        (declare (ignore request wire timeout deadline))
                        (concatenate-octets
                         (h2-frame 4 0 0 (octets))
                         (h2-frame 1 37 1
                                   (concatenate-octets
                                    (octets 0 0 0 0 0)
                                    (h2-header-block (cons ":status" "204")))))))
           (make-http-request :method "GET"
                              :uri "https://127.0.0.1/"))))
    (ensure-equal 204 (http-response-status response)
                  "Priority HTTP/2 response status")
    (ensure-equal (octets) (http-response-body response)
                  "Priority HTTP/2 response body")))

(deftest http2-sends-window-updates-after-data
  (let* ((sent '())
         (response
           (http-kit/http2::%h2-read-response
            (http-kit/http2::%h2-reader-for
             (let ((header-block (h2-header-block
                                  (cons ":status" "200")
                                  (cons "content-length" "3"))))
               (concatenate-octets
                (h2-frame 4 0 0 (octets))
                (h2-frame 1 4 1 header-block)
                (h2-frame 0 9 1 (octets 2 1 2 3 0 0)))))
            (lambda (wire) (push wire sent))
            16384 nil nil 65536 65536 "GET" nil t)))
    (ensure-equal 200 (http-response-status response)
                  "WINDOW_UPDATE response status")
    (ensure-equal (octets 1 2 3) (http-response-body response)
                  "WINDOW_UPDATE response body")
    (ensure-equal
     (list (h2-frame 8 0 0 (octets 0 0 0 6))
           (h2-frame 8 0 1 (octets 0 0 0 6))
           (h2-frame 4 1 0 (octets)))
     sent
     "HTTP/2 stream and connection receive windows are restored")))

(deftest http2-rejects-duplicate-host
  (signals http-invalid-header
    (http-kit/http2:send-http2-request
     (http-kit/http2:make-http2-client
      :exchange (lambda (request wire &key timeout deadline)
                  (declare (ignore request wire timeout deadline))
                  (error "The exchange must not run for duplicate Host.")))
     (make-http-request :method "GET"
                        :uri "https://127.0.0.1/"
                        :headers (list (make-http-header "Host" "127.0.0.1")
                                       (make-http-header "host" "127.0.0.1"))))))

(deftest http2-decodes-hpack-huffman
  (multiple-value-bind (value position)
      (http-kit/http2::%hpack-decode-string
       (octets #x8c #xf1 #xe3 #xc2 #xe5 #xf2 #x3a #x6b #xa0 #xab #x90 #xf4 #xff)
       0)
    (ensure-equal "www.example.com" value "RFC 7541 Huffman vector")
    (ensure-equal 13 position "RFC 7541 Huffman vector length")))

(deftest http2-rejects-uppercase-response-header
  (let ((uppercase-headers
          (http-kit/http2::%hpack-encode-block
           (list (cons ":status" "200")
                 (cons "X-Test" "yes")))))
    (signals http-invalid-header
      (http-kit/http2:send-http2-request
       (http-kit/http2:make-http2-client
        :exchange (lambda (request wire &key timeout deadline)
                    (declare (ignore request wire timeout deadline))
                    (concatenate-octets
                     (h2-frame 4 0 0 (octets))
                     (h2-frame 1 4 1 uppercase-headers))))
       (make-http-request :method "GET" :uri "https://127.0.0.1/")))))

(deftest http2-304-response-has-no-body
  (let ((not-modified-headers
          (http-kit/http2::%hpack-encode-block
           (list (cons ":status" "304")
                 (cons "content-length" "3")))))
    (let ((response
            (http-kit/http2:send-http2-request
             (http-kit/http2:make-http2-client
              :exchange (lambda (request wire &key timeout deadline)
                          (declare (ignore request wire timeout deadline))
                          (concatenate-octets
                           (h2-frame 4 0 0 (octets))
                           (h2-frame 1 5 1 not-modified-headers))))
             (make-http-request :method "GET" :uri "https://127.0.0.1/"))))
      (ensure-equal 304 (http-response-status response)
                    "HTTP/2 304 status")
      (ensure-equal (octets) (http-response-body response)
                    "HTTP/2 304 has no message body"))))

(deftest http2-204-response-rejects-content-length
  (let ((no-content-headers
          (http-kit/http2::%hpack-encode-block
           (list (cons ":status" "204")
                 (cons "content-length" "1")))))
    (signals http-invalid-header
      (http-kit/http2:send-http2-request
       (http-kit/http2:make-http2-client
        :exchange (lambda (request wire &key timeout deadline)
                    (declare (ignore request wire timeout deadline))
                    (concatenate-octets
                     (h2-frame 4 0 0 (octets))
                     (h2-frame 1 5 1 no-content-headers))))
       (make-http-request :method "GET" :uri "https://127.0.0.1/")))))

(deftest http2-enforces-header-size-limit
  (let ((large-header-block (make-array 5
                                        :element-type '(unsigned-byte 8)
                                        :initial-element #x88)))
    (signals http-size-limit-exceeded
      (http-kit/http2:send-http2-request
       (http-kit/http2:make-http2-client
        :exchange (lambda (request wire &key timeout deadline)
                    (declare (ignore request wire timeout deadline))
                    (concatenate-octets
                     (h2-frame 4 0 0 (octets))
                     (h2-frame 1 5 1 large-header-block))))
       (make-http-request :method "GET" :uri "https://127.0.0.1/")
       :max-header-bytes 4))))

(deftest http2-enforces-response-field-count-limit
  (let ((response-headers
          (h2-header-block
           (cons ":status" "200")
           (cons "content-type" "text/plain"))))
    (signals http-size-limit-exceeded
      (http-kit/http2:send-http2-request
       (http-kit/http2:make-http2-client
        :exchange (lambda (request wire &key timeout deadline)
                    (declare (ignore request wire timeout deadline))
                    (concatenate-octets
                     (h2-frame 4 0 0 (octets))
                     (h2-frame 1 5 1 response-headers))))
       (make-http-request :method "GET" :uri "https://127.0.0.1/")
       :max-fields 1))))

(deftest http2-request-cannot-weaken-client-field-count-limit
  (let ((response-headers
          (h2-header-block
           (cons ":status" "200")
           (cons "content-type" "text/plain"))))
    (signals http-size-limit-exceeded
      (http-kit/http2:send-http2-request
       (http-kit/http2:make-http2-client
        :exchange (lambda (request wire &key timeout deadline)
                    (declare (ignore request wire timeout deadline))
                    (concatenate-octets
                     (h2-frame 4 0 0 (octets))
                     (h2-frame 1 5 1 response-headers)))
        :max-fields 1)
       (make-http-request :method "GET" :uri "https://127.0.0.1/")
       :max-fields 2))))

(deftest http2-rejects-oversized-table-update
  (let ((oversized-table-update
          (http-kit/http2::%hpack-encode-integer 4097 5 0)))
    (signals http-protocol-error
      (http-kit/http2:send-http2-request
       (http-kit/http2:make-http2-client
        :exchange (lambda (request wire &key timeout deadline)
                    (declare (ignore request wire timeout deadline))
                    (concatenate-octets
                     (h2-frame 4 0 0 (octets))
                     (h2-frame 1 5 1
                               (concatenate-octets oversized-table-update
                                                    (octets #x88))))))
       (make-http-request :method "GET" :uri "https://127.0.0.1/")))))

(deftest http2-keeps-dynamic-table-local-limit
  (let ((dynamic-indexed-block
          (concatenate-octets
           (octets #x88 #x40 6)
           (ascii "x-test")
           (octets 1)
           (ascii "a")
           (octets #xbe))))
    (let ((response
            (http-kit/http2:send-http2-request
             (http-kit/http2:make-http2-client
              :exchange (lambda (request wire &key timeout deadline)
                          (declare (ignore request wire timeout deadline))
                          (concatenate-octets
                           (h2-frame 4 0 0 (octets 0 1 0 0 0 0))
                           (h2-frame 1 5 1 dynamic-indexed-block))))
             (make-http-request :method "GET" :uri "https://127.0.0.1/"))))
      (ensure-equal 200 (http-response-status response)
                    "HPACK decoder keeps its local table limit")
      (ensure-equal '("a" "a")
                    (http-header-values (http-response-headers response)
                                        "x-test")
                     "HPACK dynamic table response fields"))))
