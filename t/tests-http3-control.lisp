(in-package #:http-kit/test)

(deftest http3-varint-and-frame-boundaries
  (dolist (value '(0 63 64 16383 16384 #x3fffffff #x40000000
                   #x3fffffffffffffff))
    (let ((encoded (http-kit/http3:http3-varint-encode value)))
      (ensure-equal '(unsigned-byte 8)
                    (array-element-type encoded)
                    "HTTP/3 varints are binary octet vectors")
      (multiple-value-bind (decoded position)
          (http-kit/http3:http3-varint-decode encoded)
        (ensure-equal value decoded)
        (ensure-equal (length encoded) position))))
  (multiple-value-bind (decoded position)
      (http-kit/http3:http3-varint-decode
       (make-array 0 :element-type '(unsigned-byte 8))
       :allow-incomplete-p t)
    (ensure-equal nil decoded)
    (ensure-equal 0 position))
  (signals http-protocol-error
    (http-kit/http3:http3-varint-decode
     (make-array 0 :element-type '(unsigned-byte 8))))
  (let* ((settings-frame
           (http-kit/http3:make-http3-settings-frame
            :max-field-section-size 4096
            :enable-connect 1
            :extra-settings (list (cons #x21 7))))
         (wire (http-kit/http3:encode-http3-frame settings-frame)))
    (multiple-value-bind (frames remainder)
        (http-kit/http3:decode-http3-frames wire)
      (ensure-equal 1 (length frames))
      (ensure-equal 0 (length remainder))
      (let ((settings
              (http-kit/http3:decode-http3-settings
               (http-kit/http3:http3-frame-payload (first frames)))))
        (ensure-equal 4096 (cdr (assoc #x6 settings)))
        (ensure-equal 1 (cdr (assoc #x8 settings)))
        (ensure-equal 7 (cdr (assoc #x21 settings))))))
  (signals http-protocol-error
    (http-kit/http3:make-http3-settings-frame
     :extra-settings (list (cons http-kit/http3:+http3-setting-enable-push+ 0))))
  (signals http-protocol-error
    (http-kit/http3:decode-http3-settings
     (http3-test-concat-octets
      (http-kit/http3:http3-varint-encode
       http-kit/http3:+http3-setting-enable-push+)
      (http-kit/http3:http3-varint-encode 0)))))

(deftest http3-control-frame-state-boundaries
  (labels ((frame (type &optional (payload (octets)))
             (http-kit/http3:make-http3-frame :type type :payload payload)))
    (let ((state (http-kit/http3:make-http3-control-state)))
      (signals http-protocol-error
        (http-kit/http3:process-http3-control-frame
         state
         (frame http-kit/http3:+http3-goaway-type+
                (http-kit/http3:http3-varint-encode 7))))
      (ensure-equal :settings
                    (http-kit/http3:process-http3-control-frame
                     state
                     (frame http-kit/http3:+http3-settings-type+)))
      (ensure-true
       (http-kit/http3:http3-control-state-settings-received-p state))
      (ensure-equal nil
                    (http-kit/http3:http3-control-state-settings state))
      (signals http-protocol-error
        (http-kit/http3:process-http3-control-frame
         state
         (frame http-kit/http3:+http3-settings-type+)))
      (ensure-equal :goaway
                    (http-kit/http3:process-http3-control-frame
                     state
                     (frame http-kit/http3:+http3-goaway-type+
                            (http-kit/http3:http3-varint-encode 7))))
      (ensure-equal 7 (http-kit/http3:http3-control-state-goaway-id state))
      (ensure-equal :goaway
                    (http-kit/http3:process-http3-control-frame
                     state
                     (frame http-kit/http3:+http3-goaway-type+
                            (http-kit/http3:http3-varint-encode 3))))
      (signals http-protocol-error
        (http-kit/http3:process-http3-control-frame
         state
         (frame http-kit/http3:+http3-goaway-type+
                (http-kit/http3:http3-varint-encode 4))))
      (ensure-equal :max-push-id
                    (http-kit/http3:process-http3-control-frame
                     state
                     (frame http-kit/http3:+http3-max-push-id-type+
                            (http-kit/http3:http3-varint-encode 2))))
      (ensure-equal :max-push-id
                    (http-kit/http3:process-http3-control-frame
                     state
                     (frame http-kit/http3:+http3-max-push-id-type+
                            (http-kit/http3:http3-varint-encode 3))))
      (signals http-protocol-error
        (http-kit/http3:process-http3-control-frame
         state
         (frame http-kit/http3:+http3-max-push-id-type+
                (http-kit/http3:http3-varint-encode 1))))
      (ensure-equal :cancel-push
                    (http-kit/http3:process-http3-control-frame
                     state
                     (frame http-kit/http3:+http3-cancel-push-type+
                            (http-kit/http3:http3-varint-encode 9))))
      (ensure-equal :cancel-push
                    (http-kit/http3:process-http3-control-frame
                     state
                     (frame http-kit/http3:+http3-cancel-push-type+
                            (http-kit/http3:http3-varint-encode 9))))
      (ensure-equal '(9)
                    (http-kit/http3:http3-control-state-cancelled-push-ids
                     state))
      (signals http-protocol-error
        (http-kit/http3:process-http3-control-frame
         state
         (frame http-kit/http3:+http3-data-type+ (octets 1))))
      (signals http-protocol-error
        (http-kit/http3:process-http3-control-frame
         state
         (frame http-kit/http3:+http3-goaway-type+
                (http3-test-concat-octets
                 (http-kit/http3:http3-varint-encode 2)
                 (octets 0)))))
      (ensure-equal :extension
                    (http-kit/http3:process-http3-control-frame
                     state
                     (frame #x2a (octets 1 2)))))))

(deftest http3-control-stream-service-and-client-reader
  (let* ((settings-wire
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-settings-frame
             :max-field-section-size 4096
             :enable-connect 1
             :extra-settings (list (cons #x21 7)))))
         (goaway-wire
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-goaway-type+
             :payload (http-kit/http3:http3-varint-encode 7))))
         (max-push-id-wire
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-max-push-id-type+
             :payload (http-kit/http3:http3-varint-encode 3))))
         (cancel-push-wire
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-cancel-push-type+
             :payload (http-kit/http3:http3-varint-encode 9))))
         (wire
           (http3-test-concat-octets
            (http-kit/http3:http3-control-stream-prefix)
            settings-wire goaway-wire max-push-id-wire cancel-push-wire)))
    (labels ((chunks ()
               (list (list (subseq wire 0 2) nil)
                     (list (subseq wire 2) t))))
      (let ((stream (make-http3-test-stream
                     :kind :control
                     :reads (chunks)))
            (settings-seen nil)
            (goaways nil)
            (max-push-ids nil)
            (cancelled-push-ids nil)
            (closed-condition :unset))
        (labels ((read-stream (ignored-stream &key timeout deadline)
                   (declare (ignore ignored-stream timeout deadline))
                   (let ((entry (pop (http3-test-stream-reads stream))))
                     (if entry
                         (values (first entry) (second entry))
                         (values nil t))))
                 (close-stream (ignored-stream &key condition)
                   (declare (ignore ignored-stream))
                   (setf closed-condition condition
                         (http3-test-stream-closed-p stream) t)))
          (multiple-value-bind (state ended-p)
              (http-kit/http3:serve-http3-control-stream
               stream
               :read-stream #'read-stream
               :close-stream #'close-stream
               :on-settings (lambda (settings state)
                              (declare (ignore state))
                              (push settings settings-seen))
               :on-goaway (lambda (identifier state)
                            (declare (ignore state))
                            (push identifier goaways))
               :on-max-push-id (lambda (identifier state)
                                 (declare (ignore state))
                                 (push identifier max-push-ids))
               :on-cancel-push (lambda (identifier state)
                                 (declare (ignore state))
                                 (push identifier cancelled-push-ids)))
            (ensure-true ended-p)
            (ensure-true (http-kit/http3:http3-control-state-p state))
            (let ((settings (first settings-seen)))
              (ensure-equal 4096 (cdr (assoc #x6 settings)))
              (ensure-equal 1 (cdr (assoc #x8 settings)))
              (ensure-equal 7 (cdr (assoc #x21 settings))))
            (ensure-equal '(7) (nreverse goaways))
            (ensure-equal '(3) (nreverse max-push-ids))
            (ensure-equal '(9) (nreverse cancelled-push-ids))
            (ensure-equal 7
                          (http-kit/http3:http3-control-state-goaway-id state))
            (ensure-equal 3
                          (http-kit/http3:http3-control-state-max-push-id state))
            (ensure-true (http-kit/http3:http3-control-state-settings-received-p
                          state))
            (ensure-true (http-kit/http3:http3-control-state-cancelled-push-ids
                          state))
            (ensure-equal nil closed-condition)
            (ensure-true (http3-test-stream-closed-p stream)))))
      (let* ((peer-stream (make-http3-test-stream
                           :kind :peer-control
                           :reads (chunks)))
             (opened-streams nil)
             (closed-streams nil))
        (labels ((open-stream (ignored-request &key stream-type timeout deadline)
                   (declare (ignore ignored-request timeout deadline))
                   (ensure-equal :control stream-type)
                   (let ((stream (make-http3-test-stream :kind stream-type)))
                     (push stream opened-streams)
                     stream))
                 (write-stream (stream octets &key fin-p timeout deadline)
                   (declare (ignore fin-p timeout deadline))
                   (push (subseq octets 0)
                         (http3-test-stream-writes stream)))
                 (read-stream (stream &key timeout deadline)
                   (declare (ignore timeout deadline))
                   (ensure-equal peer-stream stream)
                   (let ((entry (pop (http3-test-stream-reads stream))))
                     (if entry
                         (values (first entry) (second entry))
                         (values nil t))))
                 (close-stream (stream &key condition)
                   (declare (ignore condition))
                   (push stream closed-streams)
                   (setf (http3-test-stream-closed-p stream) t)))
          (let ((client
                  (http-kit/http3:make-http3-client
                   :open-stream #'open-stream
                   :write-stream #'write-stream
                   :read-stream #'read-stream
                   :close-stream #'close-stream)))
            (ensure-equal client
                          (http-kit/http3:attach-http3-peer-control-stream
                           client peer-stream))
            (signals http-protocol-error
              (http-kit/http3:attach-http3-peer-control-stream
               client
               (make-http3-test-stream :kind :duplicate-peer-control)))
            (multiple-value-bind (events ended-p)
                (http-kit/http3:read-http3-control-stream client)
              (ensure-equal '() events)
              (ensure-equal nil ended-p))
            (multiple-value-bind (events ended-p)
                (http-kit/http3:read-http3-control-stream client)
              (ensure-equal '(:settings :goaway :max-push-id :cancel-push)
                            events)
              (ensure-true ended-p))
            (let ((state (http-kit/http3:http3-client-peer-control-state client)))
              (ensure-equal 7
                            (http-kit/http3:http3-control-state-goaway-id state))
              (ensure-equal 3
                            (http-kit/http3:http3-control-state-max-push-id state))
              (ensure-true
               (http-kit/http3:http3-control-state-settings-received-p state)))
            (ensure-true (http-kit/http3:http3-client-peer-control-fin-p client))
            (ensure-true (http-kit/http3:close-http3-client client))
            (ensure-true (every #'http3-test-stream-closed-p
                                (append opened-streams (list peer-stream))))))))))
