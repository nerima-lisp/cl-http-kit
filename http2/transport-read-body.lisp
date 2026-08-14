(in-package #:http-kit/http2)

(defun %h2-validate-response-stream-id (stream-id frame-type
                                        &optional (expected-stream-id 1))
  (cond
    ((zerop stream-id)
     (error 'http-kit:http-protocol-error
            :message "HTTP/2 response frame cannot use stream zero."
            :operation :http2-read
            :detail (list frame-type stream-id)))
    ((/= stream-id expected-stream-id)
     (error 'http-kit:http-unsupported-feature
            :message "This HTTP/2 transport received a response on an unexpected stream."
            :operation :http2-read
            :feature :http2-multiplexing
            :detail (list :expected-stream-id expected-stream-id
                          :stream-id stream-id
                           :frame-type frame-type)))))

(defun %h2-no-body-response-p (request-method status)
  (or (and (stringp request-method)
           (string-equal request-method "HEAD"))
      (member status '(204 205 304) :test #'=)))

(defun %h2-data-payload (frame)
  (let* ((payload (%h2-frame-payload frame))
         (flags (%h2-frame-flags frame))
         (start 0)
         (end (length payload)))
    (when (/= 0 (logand flags +http2-padded-flag+))
      (when (zerop end)
        (error 'http-kit:http-protocol-error
               :message "A padded HTTP/2 DATA frame has no pad length."
               :operation :http2-read
               :detail :missing-pad-length))
      (let ((padding-length (aref payload 0)))
        (incf start)
        (when (> padding-length (- end start))
          (error 'http-kit:http-protocol-error
                 :message "HTTP/2 DATA padding exceeds the payload."
                 :operation :http2-read
                 :detail padding-length))
        (decf end padding-length)))
    (subseq payload start end)))

(defun %h2-finish-request-response
    (status headers trailers body request-method
     &key (body-length (length body)))
  (%h2-finish-response status headers trailers body
                       :no-body (%h2-no-body-response-p request-method status)
                       :body-length body-length))

(defun %h2-process-headers-frame*
    (frame reader max-frame-size deadline clock-function max-header-bytes
     context status headers body body-length request-method expected-stream-id)
  (%h2-validate-response-stream-id (%h2-frame-stream-id frame)
                                   (%h2-frame-type frame)
                                   expected-stream-id)
  (multiple-value-bind (block end-stream)
      (%h2-read-header-block frame reader max-frame-size deadline clock-function
                             max-header-bytes expected-stream-id)
    (let ((fields (%hpack-decode-block block context
                                       :max-header-bytes max-header-bytes)))
      (if status
          (progn
            (unless end-stream
              (error 'http-kit:http-protocol-error
                     :message "HTTP/2 trailing HEADERS must end the stream."
                     :operation :http2-trailers
                     :detail :missing-end-stream))
            (values status headers
                    (%h2-finish-request-response
                     status headers (%h2-trailers fields) body request-method
                     :body-length body-length)))
          (multiple-value-bind (candidate-status candidate-headers)
              (%h2-status-and-headers fields)
            (if (< candidate-status 200)
                (progn
                  (when end-stream
                    (error 'http-kit:http-protocol-error
                           :message "An informational HTTP/2 response cannot end the stream."
                           :operation :http2-read
                           :detail candidate-status))
                  (values nil nil nil))
                (values candidate-status candidate-headers
                        (when end-stream
                          (%h2-finish-request-response
                           candidate-status candidate-headers nil body
                            request-method :body-length body-length)))))))))

(defun %h2-process-headers-frame
    (frame reader max-frame-size deadline clock-function max-header-bytes
     context status headers body &rest arguments)
  "Process a response HEADERS frame in either supported call form.

The original internal helper accepted REQUEST-METHOD directly after BODY.
The streaming reader additionally supplies BODY-LENGTH so it can validate
Content-Length without rescanning a non-collecting response body."
  (cond
    ((= (length arguments) 1)
     (%h2-process-headers-frame*
      frame reader max-frame-size deadline clock-function max-header-bytes
      context status headers body (length body) (first arguments) 1))
    ((= (length arguments) 2)
     (%h2-process-headers-frame*
      frame reader max-frame-size deadline clock-function max-header-bytes
      context status headers body (first arguments) (second arguments) 1))
    ((= (length arguments) 3)
     (%h2-process-headers-frame*
      frame reader max-frame-size deadline clock-function max-header-bytes
      context status headers body (first arguments) (second arguments)
      (third arguments)))
    (t
     (error 'http-kit:http-protocol-error
            :message "Invalid HTTP/2 HEADERS processing arguments."
            :operation :http2-read
            :detail arguments))))

(defun %h2-append-data-frame
    (frame status body body-length request-method max-body-bytes
     on-body-chunk collect-body-p expected-stream-id)
  (%h2-validate-response-stream-id (%h2-frame-stream-id frame)
                                   (%h2-frame-type frame)
                                   expected-stream-id)
  (unless status
    (error 'http-kit:http-protocol-error
           :message "HTTP/2 DATA arrived before final response HEADERS."
           :operation :http2-read
           :detail :data-before-headers))
  (when (and (stringp request-method)
             (string-equal request-method "HEAD"))
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 HEAD response cannot carry DATA."
           :operation :http2-read
           :detail :head-body))
  (when (member status '(204 205 304) :test #'=)
    (error 'http-kit:http-protocol-error
           :message "This HTTP/2 response status cannot carry a body."
           :operation :http2-read
           :detail status))
  (let* ((payload (%h2-data-payload frame))
         (new-body-length (+ body-length (length payload))))
    (http-kit::%check-limit :body
                            new-body-length
                            max-body-bytes)
    (when collect-body-p
      (loop for octet across payload
            do (vector-push-extend octet body)))
    (when (and on-body-chunk (plusp (length payload)))
      (funcall on-body-chunk payload))
    (values (/= 0 (logand (%h2-frame-flags frame) +http2-end-stream-flag+))
            new-body-length
            (length payload))))
