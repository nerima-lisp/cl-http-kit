(in-package #:http-kit/http2)

(defun %h2-write-wire (stream wire deadline clock-function)
  (http-kit::%check-deadline deadline clock-function :write)
  (write-sequence wire stream)
  (finish-output stream)
  wire)

(defun %h2-writer (stream deadline clock-function)
  (and stream
       (lambda (wire)
         (%h2-write-wire stream wire deadline clock-function))))

(defun %h2-send-control (writer type flags stream-id payload)
  (when writer
    (funcall writer (%h2-frame-wire type flags stream-id payload))))

(defun %h2-send-window-update (writer stream-id increment)
  (when (plusp increment)
    (unless (<= increment #x7fffffff)
      (error 'http-kit:http-protocol-error
             :message "An HTTP/2 WINDOW_UPDATE increment is too large."
             :operation :http2-control
             :detail increment))
    (let ((payload (make-array 4 :element-type '(unsigned-byte 8))))
      (%h2-put-u32 payload 0 increment)
      (%h2-send-control writer +http2-window-update-type+ 0 stream-id payload))))

(defun %h2-send-rst-stream (writer stream-id error-code)
  (let ((payload (make-array 4 :element-type '(unsigned-byte 8))))
    (%h2-put-u32 payload 0 error-code)
    (%h2-send-control writer +http2-rst-stream-type+ 0 stream-id payload)))

(defun %h2-validate-settings-frame (frame writer)
  (unless (zerop (%h2-frame-stream-id frame))
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 SETTINGS frame must use stream zero."
           :operation :http2-settings
           :detail (%h2-frame-stream-id frame)))
  (if (/= 0 (logand (%h2-frame-flags frame) +http2-ack-flag+))
      (unless (zerop (%h2-frame-length frame))
        (error 'http-kit:http-protocol-error
               :message "An HTTP/2 SETTINGS acknowledgement must be empty."
               :operation :http2-settings
               :detail (%h2-frame-length frame)))
      (progn
        ;; SETTINGS_HEADER_TABLE_SIZE limits the dynamic table used by the
        ;; sender of the SETTINGS frame for outbound header blocks.  The
        ;; connection callback also applies it to the decoder context.
        (%h2-settings (%h2-frame-payload frame))
        (%h2-send-control writer +http2-settings-type+ +http2-ack-flag+ 0
                          (http-kit::%empty-octets)))))

(defun %h2-control-frame-p (type)
  (member type (list +http2-settings-type+ +http2-ping-type+
                     +http2-window-update-type+ +http2-goaway-type+
                     +http2-rst-stream-type+ +http2-priority-type+
                     +http2-priority-update-type+)
          :test #'=))

(defun %h2-budgeted-control-frame-p (type)
  "Return true for every control frame except RST_STREAM."
  (and (%h2-control-frame-p type)
       (/= type +http2-rst-stream-type+)))

(defun %h2-control-budget-note (times type max-frames window clock-function)
  "Record TYPE and return the pruned timestamps and whether the budget failed."
  (if (not (%h2-budgeted-control-frame-p type))
      (values times nil)
      (let* ((now (funcall clock-function))
             (live (delete-if (lambda (timestamp)
                               (> (- now timestamp) window))
                             times))
             (updated (cons now live)))
        (values updated (> (length updated) max-frames)))))

(defun %h2-handle-control-frame (frame writer &optional (expected-stream-id 1))
  (let ((type (%h2-frame-type frame))
        (stream-id (%h2-frame-stream-id frame))
        (payload (%h2-frame-payload frame)))
    (cond
      ((= type +http2-settings-type+)
       (%h2-validate-settings-frame frame writer))
      ((= type +http2-ping-type+)
       (unless (and (zerop stream-id) (= (length payload) 8))
         (error 'http-kit:http-protocol-error
                :message "An HTTP/2 PING must be an eight-byte stream-zero frame."
                :operation :http2-control
                :detail (list stream-id (length payload))))
       (when (zerop (logand (%h2-frame-flags frame) +http2-ack-flag+))
         (%h2-send-control writer +http2-ping-type+ +http2-ack-flag+ 0 payload)))
      ((= type +http2-window-update-type+)
       (let ((increment (and (= (length payload) 4)
                             (%h2-u32 payload 0))))
         (unless (or (zerop stream-id)
                     (= stream-id expected-stream-id)
                     (> expected-stream-id 1))
           (error 'http-kit:http-unsupported-feature
                  :message "This HTTP/2 transport received a flow-control frame for another stream."
                  :operation :http2-control
                  :feature :http2-multiplexing))
         (unless (and increment
                      (zerop (logand increment #x80000000))
                      (plusp increment))
           (error 'http-kit:http-protocol-error
                  :message "An HTTP/2 WINDOW_UPDATE increment is invalid."
                  :operation :http2-control
                  :detail (list stream-id payload)))))
      ((= type +http2-goaway-type+)
       (unless (and (zerop stream-id) (>= (length payload) 8))
         (error 'http-kit:http-protocol-error
                :message "An HTTP/2 GOAWAY frame is invalid."
                :operation :http2-control
                :detail (list stream-id (length payload))))
       (let ((last-stream-id (%h2-u32 payload 0)))
         (when (/= 0 (logand last-stream-id #x80000000))
           (error 'http-kit:http-protocol-error
                  :message "An HTTP/2 GOAWAY last-stream identifier is invalid."
                  :operation :http2-control
                  :detail last-stream-id))
         (error 'http-kit:http-connection-error
                :message "The HTTP/2 peer sent GOAWAY before the response completed."
                :operation :http2-control
                :cause (list :goaway last-stream-id (%h2-u32 payload 4)))))
      ((= type +http2-rst-stream-type+)
       (unless (and (plusp stream-id) (= (length payload) 4))
         (error 'http-kit:http-protocol-error
                :message "An HTTP/2 RST_STREAM frame is invalid."
                :operation :http2-control
                :detail (list stream-id (length payload))))
       (unless (or (= stream-id expected-stream-id)
                   (> expected-stream-id 1))
         (error 'http-kit:http-unsupported-feature
                :message "This HTTP/2 transport received a reset for another stream."
                :operation :http2-control
                :feature :http2-multiplexing))
       (error 'http-kit:http-connection-error
              :message "The HTTP/2 peer reset the response stream."
              :operation :http2-control
              :cause (list :rst-stream stream-id (%h2-u32 payload 0))))
      ((= type +http2-priority-type+)
       (unless (and (plusp stream-id) (= (length payload) 5))
         (error 'http-kit:http-protocol-error
                :message "An HTTP/2 PRIORITY frame is invalid."
                :operation :http2-control
                :detail (list stream-id (length payload))))
       (let ((dependency (logand (%h2-u32 payload 0) #x7fffffff)))
         (when (= dependency stream-id)
           (error 'http-kit:http-protocol-error
                  :message "An HTTP/2 PRIORITY frame cannot depend on itself."
                  :operation :http2-control
                  :detail stream-id)))
       ;; PRIORITY is advisory.  A client that does not schedule multiple
       ;; active streams can validate and ignore it without violating the
       ;; wire protocol.
       nil)
      ((= type +http2-priority-update-type+)
       (error 'http-kit:http-protocol-error
              :message "An HTTP/2 server cannot send PRIORITY_UPDATE."
              :operation :http2-control
              :detail (list stream-id (length payload))))
      (t nil))))
