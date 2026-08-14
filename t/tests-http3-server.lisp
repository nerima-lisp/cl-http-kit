(in-package #:http-kit/test)

(deftest http3-server-request-response
  (let* ((request-fields
           (list (cons ":method" "POST")
                 (cons ":scheme" "https")
                 (cons ":authority" "example.test")
                 (cons ":path" "/upload?q=1")
                 (cons "content-length" "5")
                 (cons "x-test" "yes")))
         (request-headers
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-headers-type+
             :payload
             (http-kit/http3:qpack-encode-field-section
              request-fields
              :huffman-p t))))
         (request-data
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-data-type+
             :payload (octets 104 101 108 108 111))))
         (request-trailers
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-headers-type+
             :payload
             (http-kit/http3:qpack-encode-field-section
              (list (cons "x-request-trailer" "done"))
              :huffman-p t))))
         (wire (http3-test-concat-octets request-headers
                                         request-data
                                         request-trailers))
         (reads
           (loop for start from 0 below (length wire) by 3
                 for end = (min (length wire) (+ start 3))
                 collect (list (subseq wire start end)
                               (= end (length wire)))))
         (stream (make-http3-test-stream :kind :request :reads reads))
         (writes nil)
         (body-chunks nil)
         (seen-request nil)
         (closed-condition :unset))
    (labels ((read-stream (ignored-stream &key timeout deadline)
               (declare (ignore ignored-stream timeout deadline))
               (let ((entry (pop (http3-test-stream-reads stream))))
                 (if entry
                     (values (first entry) (second entry))
                     (values nil t))))
             (write-stream (ignored-stream octets &key fin-p timeout deadline)
               (declare (ignore ignored-stream timeout deadline))
               (push (list (subseq octets 0) fin-p) writes))
             (close-stream (ignored-stream &key condition)
               (declare (ignore ignored-stream))
               (setf closed-condition condition)))
      (let ((response
              (http-kit/http3:serve-http3-request-stream
               stream
               (lambda (request)
                 (setf seen-request request)
                 (http-kit:make-http-response
                  :status 201
                  :headers (list (http-kit:make-http-header
                                  "content-type" "text/plain")
                                 (http-kit:make-http-header
                                  "content-length" "2"))
                  :trailers (list (http-kit:make-http-header
                                   "x-response-trailer" "done"))
                  :body (octets 111 107)
                  :protocol-version "HTTP/3"))
               :read-stream #'read-stream
               :write-stream #'write-stream
               :close-stream #'close-stream
               :huffman-p t
               :on-body-chunk
               (lambda (chunk)
                 (push (subseq chunk 0) body-chunks)))))
        (ensure-true (http-response-p response))
        (ensure-equal 201 (http-response-status response)))
      (ensure-true (http-request-p seen-request))
      (ensure-equal "POST" (http-request-method seen-request))
      (ensure-equal "HTTP/3" (http-request-protocol-version seen-request))
      (ensure-equal "https" (http-uri-scheme
                             (http-request-uri seen-request)))
      (ensure-equal "example.test" (http-request-authority seen-request))
      (ensure-equal "/upload?q=1" (http-request-target seen-request))
      (ensure-equal (octets 104 101 108 108 111)
                    (http-request-body seen-request))
      (ensure-equal "yes"
                    (http-header-value (http-request-headers seen-request)
                                       "x-test"))
      (ensure-equal "done"
                    (http-header-value (http-request-trailers seen-request)
                                       "x-request-trailer"))
      (ensure-equal (octets 104 101 108 108 111)
                    (apply #'http3-test-concat-octets
                           (nreverse body-chunks)))
      (ensure-equal nil closed-condition)
      (let* ((response-writes (nreverse writes))
             (response-wire
               (apply #'http3-test-concat-octets
                      (mapcar #'first response-writes))))
        (ensure-equal '(nil nil t)
                      (mapcar #'second response-writes))
        (multiple-value-bind (frames remainder)
            (http-kit/http3:decode-http3-frames response-wire)
          (ensure-equal 0 (length remainder))
          (ensure-equal 3 (length frames))
          (ensure-equal
           (list (cons ":status" "201")
                 (cons "content-type" "text/plain")
                 (cons "content-length" "2"))
           (http-kit/http3:qpack-decode-field-section
            (http-kit/http3:http3-frame-payload (first frames))))
          (ensure-equal (octets 111 107)
                        (http-kit/http3:http3-frame-payload
                         (second frames)))
          (ensure-equal
           (list (cons "x-response-trailer" "done"))
           (http-kit/http3:qpack-decode-field-section
            (http-kit/http3:http3-frame-payload (third frames)))))))))

(deftest http3-server-body-limit-closes-stream
  (let* ((request-fields
           (list (cons ":method" "POST")
                 (cons ":scheme" "https")
                 (cons ":authority" "example.test")
                 (cons ":path" "/")
                 (cons "content-length" "5")))
         (wire
           (http3-test-concat-octets
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-headers-type+
              :payload
              (http-kit/http3:qpack-encode-field-section request-fields)))
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-data-type+
              :payload (octets 104 101 108 108 111)))))
         (stream (make-http3-test-stream
                  :kind :request
                  :reads (list (list wire t))))
         (seen-error nil)
         (closed-condition :unset))
    (labels ((read-stream (ignored-stream &key timeout deadline)
               (declare (ignore ignored-stream timeout deadline))
               (let ((entry (pop (http3-test-stream-reads stream))))
                 (if entry
                     (values (first entry) (second entry))
                     (values nil t))))
             (write-stream (ignored-stream octets &key fin-p timeout deadline)
               (declare (ignore ignored-stream octets fin-p timeout deadline))
               (error "The handler must not write after a request-body limit error."))
             (close-stream (ignored-stream &key condition)
               (declare (ignore ignored-stream))
               (setf closed-condition condition)))
      (signals http-size-limit-exceeded
        (http-kit/http3:serve-http3-request-stream
         stream
         (lambda (request)
           (declare (ignore request))
           (error "The handler must not run after a request-body limit error."))
         :read-stream #'read-stream
         :write-stream #'write-stream
         :close-stream #'close-stream
         :max-body-bytes 4
         :on-error (lambda (condition request)
                     (setf seen-error (list condition request)))))
      (ensure-true (consp seen-error))
      (ensure-true (typep (first seen-error) 'http-size-limit-exceeded))
      (ensure-equal nil (second seen-error))
      (ensure-true (typep closed-condition 'http-size-limit-exceeded)))))
