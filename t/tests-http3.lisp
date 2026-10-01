(in-package #:http-kit/test)

(defstruct http3-test-stream
  kind
  writes
  reads
  closed-p)

(defun http3-test-concat-octets (&rest parts)
  (let ((result (make-array (reduce #'+ parts :key #'length :initial-value 0)
                            :element-type '(unsigned-byte 8))))
    (loop with position = 0
          for part in parts
          do (replace result part :start1 position)
             (incf position (length part)))
    result))

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
  (dolist (identifier '(2 3 4 5))
    (signals http-protocol-error
      (http-kit/http3:make-http3-settings-frame
       :extra-settings (list (cons identifier 0))))
    (signals http-protocol-error
      (http-kit/http3:decode-http3-settings
       (http3-test-concat-octets
        (http-kit/http3:http3-varint-encode identifier)
        (http-kit/http3:http3-varint-encode 0)))))
  (signals http-protocol-error
    (http-kit/http3:make-http3-settings-frame :enable-connect 2))
  (signals http-protocol-error
    (http-kit/http3:make-http3-settings-frame :h3-datagram 2))
  (let* ((frame
           (http-kit/http3:make-http3-settings-frame
            :qpack-blocked-streams 1))
         (settings
           (http-kit/http3:decode-http3-settings
            (http-kit/http3:http3-frame-payload frame))))
    (ensure-equal
     1
     (cdr (assoc http-kit/http3:+http3-setting-qpack-blocked-streams+
                 settings))))
  (signals http-protocol-error
    (http-kit/http3:decode-http3-settings
     (http3-test-concat-octets
      (http-kit/http3:http3-varint-encode
       http-kit/http3:+http3-setting-enable-connect+)
      (http-kit/http3:http3-varint-encode 2)))))

(deftest http3-peer-unidirectional-stream-dispatch
  (labels ((make-client ()
             (http-kit/http3:make-http3-client
              :open-stream
              (lambda (request &key stream-type timeout deadline)
                (declare (ignore request timeout deadline))
                (list :local stream-type))
              :write-stream
              (lambda (stream octets &key fin-p timeout deadline)
                (declare (ignore stream octets fin-p timeout deadline)))
              :read-stream
              (lambda (stream &key timeout deadline)
                (declare (ignore timeout deadline))
                (let ((entry (pop (http3-test-stream-reads stream))))
                  (if entry
                      (values (first entry) (second entry))
                      (values nil t)))))))
    (let* ((settings
             (http-kit/http3:encode-http3-frame
              (http-kit/http3:make-http3-settings-frame)))
           (wire
             (http3-test-concat-octets
              (http-kit/http3:http3-control-stream-prefix) settings))
           (stream
             (make-http3-test-stream
              :kind :control :reads (list (list wire nil))))
           (client (make-client)))
      (multiple-value-bind (kind events ended-p)
          (http-kit/http3:process-http3-peer-unidirectional-stream
           client stream)
        (ensure-equal :control kind)
        (ensure-equal '(:settings) events)
        (ensure-equal nil ended-p))
      (ensure-equal '() (http3-test-stream-reads stream))
      (ensure-true
       (http-kit/http3:http3-control-state-settings-received-p
        (http-kit/http3:http3-client-peer-control-state client)))
      (let ((duplicate
              (make-http3-test-stream
               :kind :control
               :reads
               (list
                (list (http-kit/http3:http3-control-stream-prefix) nil)))))
        (signals http-protocol-error
          (http-kit/http3:accept-http3-peer-unidirectional-stream
           client duplicate))))
    (let* ((encoded (http-kit/http3:http3-varint-encode 64))
           (stream
             (make-http3-test-stream
              :kind :unknown
              :reads
              (list (list (subseq encoded 0 1) nil)
                    (list (subseq encoded 1) nil))))
           (client (make-client)))
      (multiple-value-bind (kind events ended-p)
          (http-kit/http3:process-http3-peer-unidirectional-stream
           client stream)
        (ensure-equal :unknown kind)
        (ensure-equal '() events)
        (ensure-equal nil ended-p))
      (ensure-equal nil
                    (http-kit/http3:http3-client-peer-control-stream client))
      (ensure-equal '() (http3-test-stream-reads stream)))
    (let* ((stream
             (make-http3-test-stream
              :kind :qpack-encoder
              :reads
              (list
               (list (http-kit/http3:http3-qpack-encoder-stream-prefix) nil))))
           (client (make-client)))
      (multiple-value-bind (kind events ended-p)
          (http-kit/http3:process-http3-peer-unidirectional-stream
           client stream)
        (ensure-equal :qpack-encoder kind)
        (ensure-equal '() events)
        (ensure-equal nil ended-p))
      (ensure-true
       (eq stream
           (http-kit/http3:http3-client-peer-qpack-encoder-stream client))))
    (let* ((stream
             (make-http3-test-stream
              :kind :qpack-decoder
              :reads
              (list
               (list (http-kit/http3:http3-qpack-decoder-stream-prefix) nil))))
           (client (make-client)))
      (multiple-value-bind (kind events ended-p)
          (http-kit/http3:process-http3-peer-unidirectional-stream
           client stream)
        (ensure-equal :qpack-decoder kind)
        (ensure-equal '() events)
        (ensure-equal nil ended-p))
      (ensure-true
       (eq stream
           (http-kit/http3:http3-client-peer-qpack-decoder-stream client))))
    (let* ((stream
             (make-http3-test-stream
              :kind :push
              :reads
              (list
               (list
                (http-kit/http3:http3-varint-encode
                 http-kit/http3:+http3-push-stream-type+)
                nil))))
           (client (make-client)))
      (multiple-value-bind (kind events ended-p)
          (http-kit/http3:process-http3-peer-unidirectional-stream
           client stream)
        (ensure-equal :push kind)
        (ensure-equal '() events)
        (ensure-equal nil ended-p)))
    (let ((client (make-client))
          (stream
            (make-http3-test-stream
             :kind :truncated
             :reads (list (list (octets #x40) t)))))
      (signals http-protocol-error
        (http-kit/http3:accept-http3-peer-unidirectional-stream
         client stream)))))

(deftest http3-control-frame-state-boundaries
  (labels ((frame (type &optional (payload (octets)))
             (http-kit/http3:make-http3-frame :type type :payload payload)))
    (let ((state (http-kit/http3:make-http3-control-state
                  :peer-role :client)))
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
      (push 3 (http-kit/http3:http3-control-state-promised-push-ids state))
      (ensure-equal :cancel-push
                    (http-kit/http3:process-http3-control-frame
                     state
                     (frame http-kit/http3:+http3-cancel-push-type+
                            (http-kit/http3:http3-varint-encode 3))))
      (ensure-equal :cancel-push
                    (http-kit/http3:process-http3-control-frame
                     state
                     (frame http-kit/http3:+http3-cancel-push-type+
                            (http-kit/http3:http3-varint-encode 3))))
      (ensure-equal '(3)
                    (http-kit/http3:http3-control-state-cancelled-push-ids
                     state))
      (dolist (identifier '(2 4))
        (handler-case
            (progn
              (http-kit/http3:process-http3-control-frame
               state
               (frame http-kit/http3:+http3-cancel-push-type+
                      (http-kit/http3:http3-varint-encode identifier)))
              (error "Expected invalid HTTP/3 CANCEL_PUSH to fail."))
          (http-protocol-error (condition)
            (ensure-equal :h3-id-error
                          (http-protocol-error-detail condition)))))
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

(deftest http3-server-control-role-boundaries
  (labels ((frame (type payload)
             (http-kit/http3:make-http3-frame :type type :payload payload)))
    (let ((state (http-kit/http3:make-http3-control-state
                  :settings-received-p t
                  :peer-role :server)))
      (signals http-protocol-error
        (http-kit/http3:process-http3-control-frame
         state
         (frame http-kit/http3:+http3-max-push-id-type+
                (http-kit/http3:http3-varint-encode 1))))
      (signals http-protocol-error
        (http-kit/http3:process-http3-control-frame
         state
         (frame http-kit/http3:+http3-goaway-type+
                (http-kit/http3:http3-varint-encode 7))))
      (ensure-equal
       :cancel-push
       (http-kit/http3:process-http3-control-frame
        state
        (frame http-kit/http3:+http3-cancel-push-type+
               (http-kit/http3:http3-varint-encode 0))))
      (ensure-equal
       :goaway
       (http-kit/http3:process-http3-control-frame
        state
        (frame http-kit/http3:+http3-goaway-type+
               (http-kit/http3:http3-varint-encode 4)))))))

(deftest http3-client-goaway-boundaries
  (labels ((goaway-wire (&rest identifiers)
             (apply #'http3-test-concat-octets
                    (http-kit/http3:http3-control-stream-prefix)
                    (http-kit/http3:encode-http3-frame
                     (http-kit/http3:make-http3-settings-frame))
                    (mapcar
                     (lambda (identifier)
                       (http-kit/http3:encode-http3-frame
                        (http-kit/http3:make-http3-frame
                         :type http-kit/http3:+http3-goaway-type+
                         :payload
                         (http-kit/http3:http3-varint-encode identifier))))
                     identifiers)))
           (read-goaways (identifiers)
             (let ((wire (apply #'goaway-wire identifiers))
                   (peer-stream (make-http3-test-stream :kind :peer-control)))
               (labels ((open-stream (ignored-request &key stream-type
                                                      timeout deadline)
                          (declare (ignore ignored-request timeout deadline))
                          (make-http3-test-stream :kind stream-type))
                        (write-stream (stream octets &key fin-p timeout deadline)
                          (declare (ignore stream octets fin-p timeout deadline)))
                        (read-stream (stream &key timeout deadline)
                          (declare (ignore timeout deadline))
                          (ensure-equal peer-stream stream)
                          (prog1 (values wire nil)
                            (setf wire nil))))
                 (let ((client
                         (http-kit/http3:make-http3-client
                          :open-stream #'open-stream
                          :write-stream #'write-stream
                          :read-stream #'read-stream
                          :peer-control-stream peer-stream)))
                   (http-kit/http3:read-http3-control-stream client))))))
    (signals http-protocol-error
      (read-goaways '(7)))
    (signals http-protocol-error
      (read-goaways '(8 12)))
    (multiple-value-bind (events ended-p)
        (read-goaways '(12 8 8 4))
      (ensure-equal '(:settings :goaway :goaway :goaway :goaway) events)
      (ensure-equal nil ended-p))))

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
             :payload (http-kit/http3:http3-varint-encode 4))))
         (max-push-id-wire
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-max-push-id-type+
             :payload (http-kit/http3:http3-varint-encode 3))))
         (cancel-push-wire
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-cancel-push-type+
             :payload (http-kit/http3:http3-varint-encode 2))))
         (wire
           (http3-test-concat-octets
            (http-kit/http3:http3-control-stream-prefix)
            settings-wire goaway-wire max-push-id-wire cancel-push-wire))
         (peer-wire
           (http3-test-concat-octets
            (http-kit/http3:http3-control-stream-prefix)
            settings-wire goaway-wire cancel-push-wire)))
    (labels ((chunks (source)
               (list (list (subseq source 0 2) nil)
                     (list (subseq source 2) t))))
      (let ((stream (make-http3-test-stream
                     :kind :control
                     :reads (chunks wire)))
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
          (signals http-protocol-error
              (http-kit/http3:serve-http3-control-stream
               stream
               :read-stream #'read-stream
               :close-stream #'close-stream
               :peer-role :client
               :promised-push-ids '(2)
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
                                 (push identifier cancelled-push-ids))))
          (let ((settings (first settings-seen)))
            (ensure-equal 4096 (cdr (assoc #x6 settings)))
            (ensure-equal 1 (cdr (assoc #x8 settings)))
            (ensure-equal 7 (cdr (assoc #x21 settings))))
          (ensure-equal '(4) (nreverse goaways))
          (ensure-equal '(3) (nreverse max-push-ids))
          (ensure-equal '(2) (nreverse cancelled-push-ids))
          (ensure-true (typep closed-condition 'http-protocol-error))
          (ensure-equal :h3-closed-critical-stream
                        (http-protocol-error-detail closed-condition))
          (ensure-true (http3-test-stream-closed-p stream))))
      (let* ((peer-stream (make-http3-test-stream
                           :kind :peer-control
                           :reads (chunks peer-wire)))
             (opened-streams nil)
             (closed-streams nil))
        (labels ((open-stream (ignored-request &key stream-type timeout deadline)
                   (declare (ignore ignored-request timeout deadline))
                   (ensure-true
                    (member stream-type
                            '(:control :qpack-encoder :qpack-decoder)))
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
            (setf (http-kit/http3:http3-control-state-max-push-id
                   (http-kit/http3:http3-client-peer-control-state client))
                  3
                  (http-kit/http3:http3-control-state-promised-push-ids
                   (http-kit/http3:http3-client-peer-control-state client))
                  '(2))
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
            (signals http-protocol-error
              (http-kit/http3:read-http3-control-stream client))
            (let ((state (http-kit/http3:http3-client-peer-control-state client)))
              (ensure-equal 4
                            (http-kit/http3:http3-control-state-goaway-id state))
              (ensure-equal 3
                            (http-kit/http3:http3-control-state-max-push-id state))
              (ensure-equal '(2)
                            (http-kit/http3:http3-control-state-cancelled-push-ids state))
              (ensure-true
               (http-kit/http3:http3-control-state-settings-received-p state)))
            (ensure-true (http-kit/http3:http3-client-peer-control-fin-p client))
            (ensure-equal
             '(:control :qpack-decoder :qpack-encoder)
             (sort (mapcar #'http3-test-stream-kind opened-streams)
                   #'string< :key #'symbol-name))
            (ensure-true (http-kit/http3:close-http3-client client))
            (ensure-true (every #'http3-test-stream-closed-p
                                (append opened-streams (list peer-stream))))))))))

(deftest http3-client-rejects-server-max-push-id
  (let* ((settings-wire
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-settings-frame)))
         (wire
           (http3-test-concat-octets
            (http-kit/http3:http3-control-stream-prefix)
            settings-wire
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-max-push-id-type+
              :payload (http-kit/http3:http3-varint-encode 1)))))
         (stream (make-http3-test-stream
                  :kind :peer-control
                  :reads (list (list (subseq wire 0 2) nil)
                                (list (subseq wire 2) nil))))
         (client
           (http-kit/http3:make-http3-client
            :open-stream
            (lambda (ignored-request &key stream-type timeout deadline)
              (declare (ignore ignored-request stream-type timeout deadline))
              (make-http3-test-stream :kind :local))
            :write-stream
            (lambda (ignored-stream ignored-octets &key fin-p timeout deadline)
              (declare (ignore ignored-stream ignored-octets fin-p timeout deadline)))
            :read-stream
            (lambda (read-stream &key timeout deadline)
              (declare (ignore timeout deadline))
              (let ((entry (pop (http3-test-stream-reads read-stream))))
                (if entry
                    (values (first entry) (second entry))
                    (values nil t)))))))
    (http-kit/http3:attach-http3-peer-control-stream client stream)
    (http-kit/http3:read-http3-control-stream client)
    (handler-case
        (progn
          (http-kit/http3:read-http3-control-stream client)
          (error "Expected server MAX_PUSH_ID to fail."))
      (http-protocol-error (condition)
        (ensure-equal :h3-frame-unexpected
                      (http-protocol-error-detail condition))))))

(deftest http3-qpack-static-field-sections
  (let* ((fields (list (cons ":method" "GET")
                       (cons ":scheme" "https")
                       (cons ":authority" "example.test")
                       (cons ":path" "/resource")
                       (cons "accept" "text/plain")))
         (encoded (http-kit/http3:qpack-encode-field-section fields))
         (decoded (http-kit/http3:qpack-decode-field-section encoded)))
    (ensure-equal '(unsigned-byte 8)
                  (array-element-type encoded)
                  "QPACK sections are binary octet vectors")
    (ensure-equal fields decoded))
  (signals http-protocol-error
    (http-kit/http3:qpack-decode-field-section
     (octets #x01 #x00)))
  (signals http-size-limit-exceeded
    (http-kit/http3:qpack-decode-field-section
     (http-kit/http3:qpack-encode-field-section
      (list (cons "x-test" "value")))
     :max-header-bytes 1)))

(deftest http3-qpack-dynamic-table-and-streams
  (let* ((encoder-table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256
            :capacity 0))
         (decoder-table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256
            :capacity 0))
         (encoder-stream
           (http3-test-concat-octets
            (http-kit/http3:qpack-encode-set-dynamic-table-capacity 256)
            (http-kit/http3:qpack-encode-insert-with-literal-name
             "x-request-id" "one")
            (http-kit/http3:qpack-encode-insert-with-name-reference
             0 "two" :static-p nil)
            (http-kit/http3:qpack-encode-duplicate 0)
            (http-kit/http3:qpack-encode-insert-with-name-reference
             17 "PATCH")))
         (fields (list (cons ":method" "PATCH")
                       (cons "x-request-id" "two"))))
    (multiple-value-bind (events consumed)
        (http-kit/http3:qpack-process-encoder-stream
         encoder-table encoder-stream)
      (ensure-equal 5 (length events))
      (ensure-equal (length encoder-stream) consumed))
    (multiple-value-bind (events consumed)
        (http-kit/http3:qpack-process-encoder-stream
         decoder-table encoder-stream)
      (ensure-equal 5 (length events))
      (ensure-equal (length encoder-stream) consumed))
    (ensure-equal 4
                  (http-kit/http3:qpack-dynamic-table-insert-count
                   encoder-table))
    (ensure-equal 4
                  (length (http-kit/http3:qpack-dynamic-table-entries
                           encoder-table)))
    (let ((encoded
            (http-kit/http3:qpack-encode-field-section
             fields :dynamic-table encoder-table)))
      (ensure-equal fields
                    (http-kit/http3:qpack-decode-field-section
                     encoded :dynamic-table decoder-table))
      (handler-case
          (progn
            (http-kit/http3:qpack-decode-field-section
             encoded
             :dynamic-table
             (http-kit/http3:make-qpack-dynamic-table
              :max-capacity 256
              :capacity 256))
            (error "Expected a blocked QPACK field section."))
        (http-kit/http3:qpack-blocked-field-section (condition)
          (ensure-equal 4
                        (http-kit/http3:qpack-blocked-field-section-required-insert-count
                         condition))
          (ensure-equal 0
                        (http-kit/http3:qpack-blocked-field-section-current-insert-count
                         condition))))))
  (let ((table
          (http-kit/http3:make-qpack-dynamic-table
           :max-capacity 64)))
    (http-kit/http3:qpack-dynamic-table-insert table "a" "b")
    (http-kit/http3:qpack-dynamic-table-insert table "c" "d")
    (ensure-equal 1
                  (length (http-kit/http3:qpack-dynamic-table-entries table)))
    (ensure-equal "c"
                  (http-kit/http3:qpack-dynamic-entry-name
                   (first (http-kit/http3:qpack-dynamic-table-entries table))))
    (signals http-protocol-error
      (http-kit/http3:qpack-dynamic-table-insert table
                                                  "this-name-is-definitely-too-large-for-the-table"
                                                  "value")))
  (let ((acknowledged nil)
        (cancelled nil)
        (increments nil))
    (multiple-value-bind (events consumed)
        (http-kit/http3:qpack-process-decoder-stream
         (http3-test-concat-octets
          (http-kit/http3:qpack-encode-section-acknowledgment 7)
          (http-kit/http3:qpack-encode-stream-cancellation 9)
          (http-kit/http3:qpack-encode-insert-count-increment 3))
         :on-section-acknowledgment
         (lambda (stream-id) (push stream-id acknowledged))
         :on-stream-cancellation
         (lambda (stream-id) (push stream-id cancelled))
         :on-insert-count-increment
         (lambda (increment) (push increment increments)))
      (ensure-equal '((:section-acknowledgment 7)
                      (:stream-cancellation 9)
                      (:insert-count-increment 3))
                    events)
      (ensure-equal 3 consumed))
    (ensure-equal '(7) acknowledged)
    (ensure-equal '(9) cancelled)
    (ensure-equal '(3) increments)))

(deftest http3-qpack-stream-fragmentation
  (let* ((table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256
            :capacity 256))
         (instruction
           (http-kit/http3:qpack-encode-insert-with-literal-name
            "x-fragmented" "fragmented-value"))
         (partial (subseq instruction 0 (1- (length instruction)))))
    (multiple-value-bind (events consumed)
        (http-kit/http3:qpack-process-encoder-stream
         table partial :allow-incomplete-p t)
      (ensure-equal '() events)
      (ensure-equal 0 consumed)
      (ensure-equal 0
                    (http-kit/http3:qpack-dynamic-table-insert-count table)))
    (multiple-value-bind (events consumed)
        (http-kit/http3:qpack-process-encoder-stream table instruction)
      (ensure-equal 1 (length events))
      (ensure-equal (length instruction) consumed)
      (ensure-equal 1
                    (http-kit/http3:qpack-dynamic-table-insert-count table)))
    (let* ((next
             (http-kit/http3:qpack-encode-insert-with-literal-name
              "x-next" "next-value"))
           (wire (http3-test-concat-octets instruction
                                           (subseq next 0 (1- (length next))))))
      (multiple-value-bind (events consumed)
          (http-kit/http3:qpack-process-encoder-stream
           table wire :allow-incomplete-p t)
        (ensure-equal 1 (length events))
        (ensure-equal (length instruction) consumed)
        (ensure-equal 2
                      (http-kit/http3:qpack-dynamic-table-insert-count table)))))
  (let* ((instruction
           (http-kit/http3:qpack-encode-section-acknowledgment 200))
         (partial (subseq instruction 0 1))
         (acknowledged nil))
    (multiple-value-bind (events consumed)
        (http-kit/http3:qpack-process-decoder-stream
         partial
         :allow-incomplete-p t
         :on-section-acknowledgment
         (lambda (stream-id) (push stream-id acknowledged)))
      (ensure-equal '() events)
      (ensure-equal 0 consumed)
      (ensure-equal nil acknowledged))
    (multiple-value-bind (events consumed)
        (http-kit/http3:qpack-process-decoder-stream
         instruction
         :on-section-acknowledgment
         (lambda (stream-id) (push stream-id acknowledged)))
      (ensure-equal '((:section-acknowledgment 200)) events)
      (ensure-equal (length instruction) consumed)
      (ensure-equal '(200) acknowledged))
    (let* ((complete
             (http-kit/http3:qpack-encode-section-acknowledgment 7))
           (wire (http3-test-concat-octets complete partial)))
      (setf acknowledged nil)
      (multiple-value-bind (events consumed)
          (http-kit/http3:qpack-process-decoder-stream
           wire
           :allow-incomplete-p t
           :on-section-acknowledgment
           (lambda (stream-id) (push stream-id acknowledged)))
        (ensure-equal '((:section-acknowledgment 7)) events)
        (ensure-equal (length complete) consumed)
        (ensure-equal '(7) acknowledged)))))

(deftest http3-qpack-critical-stream-lifecycle
  (let* ((decoder-table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256 :capacity 0))
         (encoder-table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256 :capacity 0))
         (decoder-state
           (http-kit/http3:make-qpack-decoder-stream-state
            encoder-table :blocked-stream-limit 1))
         (encoder-instructions
           (http3-test-concat-octets
            (http-kit/http3:qpack-encode-set-dynamic-table-capacity 256)
            (http-kit/http3:qpack-encode-insert-with-literal-name "x-peer" "yes")))
         (encoder-wire
           (http3-test-concat-octets
            (http-kit/http3:http3-qpack-encoder-stream-prefix)
            encoder-instructions))
         (encoder-stream
           (make-http3-test-stream
            :kind :peer-qpack-encoder
            :reads
            (list (list (subseq encoder-wire 0 (1- (length encoder-wire))) nil)
                  (list (subseq encoder-wire (1- (length encoder-wire))) nil))))
         (decoder-stream
           (make-http3-test-stream :kind :peer-qpack-decoder))
         (local-encoder-stream
           (make-http3-test-stream :kind :local-qpack-encoder))
         (encoder-writes '())
         (client
           (http-kit/http3::%make-http3-client
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore timeout deadline))
              (let ((entry (pop (http3-test-stream-reads stream))))
                (values (first entry) (second entry))))
            :write-stream
            (lambda (stream octets &key fin-p timeout deadline)
              (declare (ignore timeout deadline))
              (push (list stream octets fin-p) encoder-writes))
            :open-p t
            :qpack-encoder-stream local-encoder-stream
            :qpack-decoder-table decoder-table
            :qpack-decoder-context
            (http-kit/http3:make-http3-qpack-decoder-context decoder-table)
            :qpack-encoder-table encoder-table
            :qpack-decoder-state decoder-state)))
    (http-kit/http3:attach-http3-peer-qpack-encoder-stream client encoder-stream)
    (signals http-protocol-error
      (http-kit/http3:attach-http3-peer-qpack-encoder-stream
       client (make-http3-test-stream :kind :duplicate)))
    (multiple-value-bind (events ended-p)
        (http-kit/http3:read-http3-qpack-encoder-stream client)
      (ensure-equal 1 (length events))
      (ensure-equal nil ended-p)
      (ensure-equal 0
                    (http-kit/http3:qpack-dynamic-table-insert-count
                     decoder-table)))
    (multiple-value-bind (events ended-p)
        (http-kit/http3:read-http3-qpack-encoder-stream client)
      (ensure-equal 1 (length events))
      (ensure-equal nil ended-p)
      (ensure-equal 1
                    (http-kit/http3:qpack-dynamic-table-insert-count
                     decoder-table)))
    (http-kit/http3:http3-client-set-qpack-capacity client 256)
    (http-kit/http3:http3-client-insert-qpack-field
     client "x-local" "yes")
    (ensure-equal 2 (length encoder-writes))
    (ensure-equal local-encoder-stream (first (second encoder-writes)))
    (ensure-equal
     (coerce (http-kit/http3:qpack-encode-set-dynamic-table-capacity 256)
             'list)
     (coerce (second (second encoder-writes)) 'list))
    (ensure-equal nil (third (second encoder-writes)))
    (ensure-equal local-encoder-stream (first (first encoder-writes)))
    (ensure-equal
     (coerce (http-kit/http3:qpack-encode-insert-with-literal-name
              "x-local" "yes")
             'list)
     (coerce (second (first encoder-writes)) 'list))
    (ensure-equal nil (third (first encoder-writes)))
    (ensure-equal 256
                  (http-kit/http3:qpack-dynamic-table-capacity encoder-table))
    (ensure-equal 1
                  (http-kit/http3:qpack-dynamic-table-insert-count
                   encoder-table))
    (ensure-equal 1
                  (http-kit/http3:qpack-decoder-stream-state-sent-insert-count
                   decoder-state))
    (http-kit/http3:qpack-encode-field-section
     (list (cons "x-local" "yes"))
     :dynamic-table encoder-table :decoder-stream-state decoder-state :stream-id 200)
    (let* ((ack (http-kit/http3:qpack-encode-section-acknowledgment 200))
           (wire (http3-test-concat-octets
                  (http-kit/http3:http3-qpack-decoder-stream-prefix) ack)))
      (setf (http3-test-stream-reads decoder-stream)
            (list (list (subseq wire 0 (1- (length wire))) nil)
                  (list (subseq wire (1- (length wire))) nil)))
      (http-kit/http3:attach-http3-peer-qpack-decoder-stream client decoder-stream)
      (multiple-value-bind (events ended-p)
          (http-kit/http3:read-http3-qpack-decoder-stream client)
        (ensure-equal '() events)
        (ensure-equal nil ended-p))
      (multiple-value-bind (events ended-p)
          (http-kit/http3:read-http3-qpack-decoder-stream client)
        (ensure-equal '((:section-acknowledgment 200)) events)
        (ensure-equal nil ended-p)))
    (setf (http3-test-stream-reads encoder-stream) (list (list nil t)))
    (handler-case
        (progn
          (http-kit/http3:read-http3-qpack-encoder-stream client)
          (error "Expected QPACK critical-stream FIN to fail."))
      (http-protocol-error (condition)
        (ensure-equal :h3-closed-critical-stream
                      (http-protocol-error-detail condition))))))

(deftest http3-qpack-peer-encoder-application-is-serialized
  (let* ((table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256 :capacity 0))
         (wire
           (http3-test-concat-octets
            (http-kit/http3:http3-qpack-encoder-stream-prefix)
            (http-kit/http3:qpack-encode-set-dynamic-table-capacity 256)
            (http-kit/http3:qpack-encode-insert-with-literal-name
             "x-peer" "yes")))
         (stream (make-http3-test-stream :kind :peer-qpack-encoder))
         (inside-p nil)
         (events '())
         client)
    (setf client
          (http-kit/http3::%make-http3-client
           :serialize
           (lambda (thunk)
             (push (list :enter
                         (http-kit/http3:qpack-dynamic-table-insert-count table))
                   events)
             (let ((inside-p t))
               (multiple-value-prog1 (funcall thunk)
                 (push (list :leave
                             (http-kit/http3:qpack-dynamic-table-insert-count table)
                             (http-kit/http3::http3-client-peer-qpack-encoder-prefix-seen-p
                              client)
                             (length
                              (http-kit/http3::http3-client-peer-qpack-encoder-buffer
                               client)))
                       events))))
           :read-stream
           (lambda (read-stream &key timeout deadline)
             (declare (ignore read-stream timeout deadline))
             (push (list :read inside-p) events)
           (values wire nil))
           :open-p t
           :qpack-decoder-table table
           :qpack-decoder-context
           (http-kit/http3:make-http3-qpack-decoder-context table)))
    (http-kit/http3:attach-http3-peer-qpack-encoder-stream client stream)
    (multiple-value-bind (application-events ended-p)
        (http-kit/http3:read-http3-qpack-encoder-stream client)
      (ensure-equal 2 (length application-events))
      (ensure-equal nil ended-p))
    (ensure-equal '((:enter 0) (:leave 0 nil 0)
                   (:read nil) (:enter 0) (:leave 1 t 0))
                  (nreverse events))
    (ensure-equal 1
                  (http-kit/http3:qpack-dynamic-table-insert-count table))))

(deftest http3-qpack-local-write-failure-preserves-state
  (let* ((table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256 :capacity 0))
         (state
           (http-kit/http3:make-qpack-decoder-stream-state table))
         (client
           (http-kit/http3::%make-http3-client
            :write-stream
            (lambda (stream octets &key fin-p timeout deadline)
              (declare (ignore stream octets fin-p timeout deadline))
              (error "QPACK write failed"))
            :open-p t
            :qpack-encoder-stream
            (make-http3-test-stream :kind :local-qpack-encoder)
            :qpack-encoder-table table
            :qpack-decoder-state state)))
    (signals simple-error
      (http-kit/http3:http3-client-set-qpack-capacity client 256))
    (ensure-equal 0
                  (http-kit/http3:qpack-dynamic-table-capacity table))
    (http-kit/http3:qpack-dynamic-table-set-capacity table 256)
    (signals simple-error
      (http-kit/http3:http3-client-insert-qpack-field
       client "x-local" "yes"))
    (ensure-equal 0
                  (http-kit/http3:qpack-dynamic-table-insert-count table))
    (ensure-equal 0
                  (http-kit/http3:qpack-decoder-stream-state-sent-insert-count
                   state))))

(deftest http3-qpack-serializer-default-and-validation
  (let ((client (http-kit/http3::%make-http3-client)))
    (multiple-value-bind (first second)
        (http-kit/http3::%call-http3-serialized
         client (lambda () (values :first :second)))
      (ensure-equal :first first)
      (ensure-equal :second second))
    (signals simple-error
      (http-kit/http3::%call-http3-serialized
       client (lambda () (error "Serializer propagation")))))
  (signals http-protocol-error
    (http-kit/http3:make-http3-client
     :open-stream (lambda (&rest arguments) (declare (ignore arguments)))
     :write-stream (lambda (&rest arguments) (declare (ignore arguments)))
     :read-stream (lambda (&rest arguments) (declare (ignore arguments)))
     :serialize :not-a-function)))

(deftest http3-qpack-local-mutations-are-serialized-around-write
  (let* ((table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256 :capacity 0))
         (state (http-kit/http3:make-qpack-decoder-stream-state table))
         (inside-p nil)
         (events '())
         (client
           (http-kit/http3::%make-http3-client
            :serialize
            (lambda (thunk)
              (push :enter events)
              (let ((inside-p t))
                (declare (special inside-p))
                (multiple-value-prog1 (funcall thunk)
                  (push :leave events))))
            :write-stream
            (lambda (stream octets &key fin-p timeout deadline)
              (declare (ignore stream octets fin-p timeout deadline)
                       (special inside-p))
              (ensure-true inside-p)
              (push (list :write
                          (http-kit/http3:qpack-dynamic-table-capacity table)
                          (http-kit/http3:qpack-dynamic-table-insert-count table)
                          (http-kit/http3:qpack-decoder-stream-state-sent-insert-count
                           state))
                    events))
            :open-p t
            :qpack-encoder-stream
            (make-http3-test-stream :kind :local-qpack-encoder)
            :qpack-encoder-table table
            :qpack-decoder-state state)))
    (declare (special inside-p))
    (http-kit/http3:http3-client-set-qpack-capacity client 256)
    (http-kit/http3:http3-client-insert-qpack-field client "x-local" "yes")
    (ensure-equal
     '(:enter (:write 0 0 0) :leave
       :enter (:write 256 0 0) :leave)
     (nreverse events))
    (ensure-equal 1
                  (http-kit/http3:qpack-dynamic-table-insert-count table))
    (ensure-equal 1
                  (http-kit/http3:qpack-decoder-stream-state-sent-insert-count
                   state))))

(deftest http3-qpack-request-header-write-failure-does-not-register-section
  (let* ((table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256 :capacity 256))
         (state (http-kit/http3:make-qpack-decoder-stream-state table))
         (request-stream (make-http3-test-stream :kind :request))
         (inside-p nil)
         (open-outside-p nil)
         (close-outside-p nil))
    (http-kit/http3:qpack-dynamic-table-insert table "x-local" "yes")
    (http-kit/http3:qpack-decoder-stream-state-note-insertions-sent state 1)
    (let ((client
            (http-kit/http3::%make-http3-client
             :serialize
             (lambda (thunk)
               (let ((inside-p t))
                 (declare (special inside-p))
                 (funcall thunk)))
             :open-stream
             (lambda (request &key stream-type timeout deadline)
               (declare (ignore request stream-type timeout deadline))
               (setf open-outside-p (not inside-p))
               (values request-stream 4))
             :write-stream
             (lambda (stream octets &key fin-p timeout deadline)
               (declare (ignore stream octets fin-p timeout deadline)
                        (special inside-p))
               (ensure-true inside-p)
               (error "HEADERS write failed"))
             :close-stream
             (lambda (stream &key condition)
               (declare (ignore stream condition))
               (setf close-outside-p (not inside-p)))
             :open-p t
             :qpack-encoder-table table
             :qpack-decoder-table
             (http-kit/http3:make-qpack-dynamic-table)
             :peer-control-state
             (http-kit/http3:make-http3-control-state :peer-role :server)
             :qpack-decoder-state state)))
      (declare (special inside-p))
      (signals simple-error
        (http-kit/http3:send-http3-request
         client
         (make-http-request
          :method "GET" :uri "https://example.test/"
          :headers (list (make-http-header "x-local" "yes")))))
      (ensure-true open-outside-p)
      (ensure-true close-outside-p)
      (ensure-equal
       0
       (hash-table-count
        (http-kit/http3::qpack-decoder-stream-state-outstanding-sections
         state)))
      (ensure-equal
       0
       (hash-table-count
        (http-kit/http3::qpack-dynamic-table-protected-indices table))))))

(deftest http3-qpack-decoder-stream-state-validation
  (let* ((table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256
            :capacity 256))
         (state
           (http-kit/http3:make-qpack-decoder-stream-state
            table :blocked-stream-limit 3)))
    (http-kit/http3:qpack-dynamic-table-insert table "x-first" "one")
    (signals http-protocol-error
      (http-kit/http3:qpack-encode-field-section
       (list (cons "x-first" "one"))
       :dynamic-table table
       :decoder-stream-state state
       :stream-id 4))
    (http-kit/http3:qpack-decoder-stream-state-note-insertions-sent state 1)
    (http-kit/http3:qpack-encode-field-section
     (list (cons "x-first" "one"))
     :dynamic-table table
     :decoder-stream-state state
     :stream-id 4)
    (http-kit/http3:qpack-dynamic-table-insert table "x-second" "two")
    (http-kit/http3:qpack-decoder-stream-state-note-insertions-sent state 2)
    (http-kit/http3:qpack-encode-field-section
     (list (cons "x-second" "two"))
     :dynamic-table table
     :decoder-stream-state state
     :stream-id 4)
    (http-kit/http3:qpack-encode-field-section
     (list (cons "x-second" "two"))
     :dynamic-table table
     :decoder-stream-state state
     :stream-id 8)
    (http-kit/http3:qpack-process-decoder-stream
     (http-kit/http3:qpack-encode-section-acknowledgment 4)
     :state state)
    (ensure-equal 1
                  (http-kit/http3:qpack-decoder-stream-state-known-received-count
                   state))
    (http-kit/http3:qpack-process-decoder-stream
     (http-kit/http3:qpack-encode-section-acknowledgment 4)
     :state state)
    (ensure-equal 2
                  (http-kit/http3:qpack-decoder-stream-state-known-received-count
                   state))
    (signals http-protocol-error
      (http-kit/http3:qpack-process-decoder-stream
       (http-kit/http3:qpack-encode-section-acknowledgment 4)
       :state state))
    (http-kit/http3:qpack-process-decoder-stream
     (http-kit/http3:qpack-encode-stream-cancellation 8)
     :state state)
    (signals http-protocol-error
      (http-kit/http3:qpack-process-decoder-stream
       (http-kit/http3:qpack-encode-stream-cancellation 8)
       :state state))
    (signals http-protocol-error
      (http-kit/http3:qpack-process-decoder-stream
       (http-kit/http3:qpack-encode-insert-count-increment 1)
       :state state)))
  (let* ((table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 128
            :capacity 128))
         (state
           (http-kit/http3:make-qpack-decoder-stream-state
            table :blocked-stream-limit 1)))
    (http-kit/http3:qpack-dynamic-table-insert table "x" "y")
    (http-kit/http3:qpack-decoder-stream-state-note-insertions-sent state 1)
    (http-kit/http3:qpack-process-decoder-stream
     (http-kit/http3:qpack-encode-insert-count-increment 1)
     :state state)
    (ensure-equal 1
                  (http-kit/http3:qpack-decoder-stream-state-known-received-count
                   state))
    (signals http-protocol-error
      (http-kit/http3:qpack-process-decoder-stream
       (http-kit/http3:qpack-encode-insert-count-increment 1)
       :state state)))
  (let* ((table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 90
            :capacity 90))
         (state
           (http-kit/http3:make-qpack-decoder-stream-state
            table :blocked-stream-limit 1)))
    (http-kit/http3:qpack-dynamic-table-insert table "x-first" "one")
    (http-kit/http3:qpack-dynamic-table-insert table "x-second" "two")
    (http-kit/http3:qpack-decoder-stream-state-note-insertions-sent state 2)
    (http-kit/http3:qpack-encode-field-section
     (list (cons "x-first" "one"))
     :dynamic-table table
     :decoder-stream-state state
     :stream-id 12)
    (signals http-protocol-error
      (http-kit/http3:qpack-dynamic-table-insert table "x-third" "three"))
    (ensure-equal 2
                  (http-kit/http3:qpack-dynamic-table-insert-count table))
    (signals http-protocol-error
      (http-kit/http3:qpack-dynamic-table-set-capacity table 41))
    (ensure-equal 90
                  (http-kit/http3:qpack-dynamic-table-capacity table))
    (http-kit/http3:qpack-process-decoder-stream
     (http-kit/http3:qpack-encode-section-acknowledgment 12)
     :state state)
    (http-kit/http3:qpack-dynamic-table-insert table "x-third" "three")
    (ensure-equal 3
                  (http-kit/http3:qpack-dynamic-table-insert-count table))))

(deftest http3-qpack-huffman-encoding
  (let* ((input (octets 119 119 119 46 101 120 97 109 112 108 101 46 99 111 109))
         (encoded (http-kit/http2::%hpack-huffman-encode input)))
    (ensure-equal (octets #xf1 #xe3 #xc2 #xe5 #xf2 #x3a
                          #x6b #xa0 #xab #x90 #xf4 #xff)
                  encoded)
    (ensure-equal input
                  (http-kit/http2::%hpack-huffman-decode encoded)))
  (let* ((table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256
            :capacity 256))
         (encoder-stream
           (http-kit/http3:qpack-encode-insert-with-literal-name
            "x-huffman" "www.example.com" :huffman-p t)))
    (multiple-value-bind (events consumed)
        (http-kit/http3:qpack-process-encoder-stream table encoder-stream)
      (ensure-equal 1 (length events))
      (ensure-equal (length encoder-stream) consumed))
    (let ((entry (first (http-kit/http3:qpack-dynamic-table-entries table))))
      (ensure-equal "x-huffman"
                    (http-kit/http3:qpack-dynamic-entry-name entry))
      (ensure-equal "www.example.com"
                    (http-kit/http3:qpack-dynamic-entry-value entry))))
  (let* ((fields (list (cons "x-huffman" "www.example.com")
                       (cons "accept" "text/plain")))
         (encoded (http-kit/http3:qpack-encode-field-section
                   fields :huffman-p t)))
    (ensure-equal fields
                  (http-kit/http3:qpack-decode-field-section encoded))))

(defun http3-test-qpack-blocked-fixture ()
  (let* ((fields (list (cons "x-blocked" "ready")))
         (encoder-table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256 :capacity 256))
         (decoder-table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256 :capacity 256))
         (instruction
           (http-kit/http3:qpack-encode-insert-with-literal-name
            "x-blocked" "ready")))
    (http-kit/http3:qpack-dynamic-table-insert
     encoder-table "x-blocked" "ready")
    (values fields decoder-table
            (http-kit/http3:qpack-encode-field-section
             fields :dynamic-table encoder-table)
            instruction)))

(defun http3-test-qpack-blocked-section (fields)
  (let* ((encoder-table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256 :capacity 256))
         (decoder-table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256 :capacity 256))
         (instruction
           (http-kit/http3:qpack-encode-insert-with-literal-name
            "x-blocked" "ready")))
    (http-kit/http3:qpack-dynamic-table-insert
     encoder-table "x-blocked" "ready")
    (values decoder-table
            (http-kit/http3:qpack-encode-field-section
             fields :dynamic-table encoder-table)
            instruction)))

(defun http3-test-resume-qpack-token (context table instruction token)
  (http-kit/http3:qpack-process-encoder-stream table instruction)
  (let ((ready
          (http-kit/http3::%http3-qpack-detach-ready-streams context)))
    (ensure-equal 1 (length ready))
    (ensure-true (eq token (first ready)))
    (http-kit/http3:resume-http3-qpack-blocked-stream token)))

(deftest http3-qpack-blocked-condition-readers
  (multiple-value-bind (fields decoder-table section instruction)
      (http3-test-qpack-blocked-fixture)
    (declare (ignore fields instruction))
    (handler-case
        (progn
          (http-kit/http3:qpack-decode-field-section
           section :dynamic-table decoder-table)
          (error "Expected a blocked QPACK field section."))
      (http-kit/http3:qpack-blocked-field-section (condition)
        (ensure-equal
         1
         (http-kit/http3:qpack-blocked-field-section-required-insert-count
          condition))
        (ensure-equal
         0
         (http-kit/http3:qpack-blocked-field-section-current-insert-count
          condition))))))

(deftest http3-qpack-blocked-limit-zero-rejects
  (multiple-value-bind (fields decoder-table section instruction)
      (http3-test-qpack-blocked-fixture)
    (declare (ignore fields instruction))
    (let ((context
            (http-kit/http3:make-http3-qpack-decoder-context
             decoder-table :blocked-stream-limit 0)))
      (signals http-protocol-error
        (http-kit/http3:decode-http3-qpack-field-section
         section context 0)))))

(deftest http3-qpack-blocked-limit-one-rejects-distinct-stream
  (multiple-value-bind (fields decoder-table section instruction)
      (http3-test-qpack-blocked-fixture)
    (declare (ignore fields instruction))
    (let ((context
            (http-kit/http3:make-http3-qpack-decoder-context
             decoder-table :blocked-stream-limit 1)))
      (ensure-true
       (http-kit/http3:http3-qpack-blocked-stream-p
        (http-kit/http3:decode-http3-qpack-field-section
         section context 0)))
      (signals http-protocol-error
        (http-kit/http3:decode-http3-qpack-field-section
         section context 4)))))

(deftest http3-qpack-reregister-same-stream-does-not-consume-limit
  (multiple-value-bind (fields decoder-table section instruction)
      (http3-test-qpack-blocked-fixture)
    (declare (ignore fields instruction))
    (let ((context
            (http-kit/http3:make-http3-qpack-decoder-context
             decoder-table :blocked-stream-limit 1)))
      (dotimes (ignored 2)
        (declare (ignore ignored))
        (ensure-true
         (http-kit/http3:http3-qpack-blocked-stream-p
          (http-kit/http3:decode-http3-qpack-field-section
           section context 0))))
      (ensure-equal
       1
       (hash-table-count
        (http-kit/http3::http3-qpack-decoder-context-blocked-streams
         context))))))

(deftest http3-qpack-ready-threshold-is-inclusive
  (let* ((table (http-kit/http3:make-qpack-dynamic-table
                 :max-capacity 256 :capacity 256))
         (context (http-kit/http3:make-http3-qpack-decoder-context
                   table :blocked-stream-limit 1)))
    (http-kit/http3::%http3-qpack-register-blocked-stream
     context 0 2 (lambda () :resumed))
    (http-kit/http3:qpack-dynamic-table-insert table "x-first" "one")
    (ensure-equal
     nil
     (http-kit/http3::%http3-qpack-detach-ready-streams context))
    (http-kit/http3:qpack-dynamic-table-insert table "x-second" "two")
    (let ((ready
            (http-kit/http3::%http3-qpack-detach-ready-streams context)))
      (ensure-equal 1 (length ready))
      (ensure-equal 2
                    (http-kit/http3:http3-qpack-blocked-stream-required-insert-count
                     (first ready))))))

(deftest http3-qpack-ready-stream-detaches-once
  (let* ((table (http-kit/http3:make-qpack-dynamic-table
                 :max-capacity 256 :capacity 256))
         (context (http-kit/http3:make-http3-qpack-decoder-context
                   table :blocked-stream-limit 1)))
    (http-kit/http3::%http3-qpack-register-blocked-stream
     context 0 1 (lambda () :resumed))
    (http-kit/http3:qpack-dynamic-table-insert table "x-ready" "yes")
    (ensure-equal
     1
     (length (http-kit/http3::%http3-qpack-detach-ready-streams context)))
    (ensure-equal
     nil
     (http-kit/http3::%http3-qpack-detach-ready-streams context))))

(deftest http3-qpack-stale-token-cannot-resume
  (let* ((table (http-kit/http3:make-qpack-dynamic-table
                 :max-capacity 256 :capacity 256))
         (context (http-kit/http3:make-http3-qpack-decoder-context
                   table :blocked-stream-limit 1))
         (stale
           (http-kit/http3::%http3-qpack-register-blocked-stream
            context 0 1 (lambda () :stale))))
    (http-kit/http3::%http3-qpack-register-blocked-stream
     context 0 1 (lambda () :current))
    (signals http-protocol-error
      (http-kit/http3:resume-http3-qpack-blocked-stream stale))))

(deftest http3-qpack-detached-token-becomes-stale-on-reregister
  (let* ((table (http-kit/http3:make-qpack-dynamic-table
                 :max-capacity 256 :capacity 256))
         (context (http-kit/http3:make-http3-qpack-decoder-context
                   table :blocked-stream-limit 1)))
    (http-kit/http3::%http3-qpack-register-blocked-stream
     context 0 1 (lambda () :stale))
    (http-kit/http3:qpack-dynamic-table-insert table "x-first" "one")
    (let ((stale
            (first
             (http-kit/http3::%http3-qpack-detach-ready-streams context))))
      (http-kit/http3::%http3-qpack-register-blocked-stream
       context 0 2 (lambda () :current))
      (signals http-protocol-error
        (http-kit/http3:resume-http3-qpack-blocked-stream stale))
      (http-kit/http3:qpack-dynamic-table-insert table "x-second" "two")
      (let ((current
              (first
               (http-kit/http3::%http3-qpack-detach-ready-streams context))))
        (ensure-equal
         :current
         (http-kit/http3:resume-http3-qpack-blocked-stream current))))))

(deftest http3-qpack-ready-token-resumes-once
  (let* ((table (http-kit/http3:make-qpack-dynamic-table
                 :max-capacity 256 :capacity 256))
         (context (http-kit/http3:make-http3-qpack-decoder-context
                   table :blocked-stream-limit 1)))
    (http-kit/http3::%http3-qpack-register-blocked-stream
     context 0 1 (lambda () :resumed))
    (http-kit/http3:qpack-dynamic-table-insert table "x-ready" "yes")
    (let ((token
            (first
             (http-kit/http3::%http3-qpack-detach-ready-streams context))))
      (ensure-equal
       :resumed
       (http-kit/http3:resume-http3-qpack-blocked-stream token))
      (signals http-protocol-error
        (http-kit/http3:resume-http3-qpack-blocked-stream token)))))

(deftest http3-qpack-resume-callback-runs-outside-serializer
  (multiple-value-bind (fields decoder-table section instruction)
      (http3-test-qpack-blocked-fixture)
    (let* ((context
             (http-kit/http3:make-http3-qpack-decoder-context
              decoder-table :blocked-stream-limit 1))
           (inside-serializer-p nil)
           (callback-inside-p :not-called)
           (token
             (http-kit/http3:decode-http3-qpack-field-section
              section context 0
              :serialize
              (lambda (thunk)
                (setf inside-serializer-p t)
                (unwind-protect
                     (funcall thunk)
                  (setf inside-serializer-p nil)))
              :on-decoded
              (lambda (decoded)
                (setf callback-inside-p inside-serializer-p)
                decoded))))
      (http-kit/http3:qpack-process-encoder-stream decoder-table instruction)
      (let ((ready
              (http-kit/http3::%http3-qpack-detach-ready-streams context)))
        (ensure-equal 1 (length ready))
        (ensure-equal
         fields
         (http-kit/http3:resume-http3-qpack-blocked-stream (first ready))))
      (ensure-equal nil callback-inside-p))))

(deftest http3-qpack-peer-zero-falls-back-to-literals
  (let* ((fields (list (cons "x-unacknowledged" "value")))
         (encoder-table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256 :capacity 256))
         (decoder-table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256 :capacity 256))
         (state
           (http-kit/http3:make-qpack-decoder-stream-state
            encoder-table :blocked-stream-limit 0)))
    (http-kit/http3:qpack-dynamic-table-insert
     encoder-table "x-unacknowledged" "value")
    (http-kit/http3:qpack-decoder-stream-state-note-insertions-sent state 1)
    (let ((section
            (http-kit/http3:qpack-encode-field-section
             fields :dynamic-table encoder-table
             :decoder-stream-state state :stream-id 0)))
      (ensure-equal
       fields
       (http-kit/http3:qpack-decode-field-section
        section :dynamic-table decoder-table))
      (ensure-equal
       0
       (hash-table-count
        (http-kit/http3:qpack-decoder-stream-state-outstanding-sections
         state))))))

(deftest http3-qpack-peer-one-allows-dynamic-reference
  (let* ((fields (list (cons "x-unacknowledged" "value")))
         (encoder-table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256 :capacity 256))
         (decoder-table
           (http-kit/http3:make-qpack-dynamic-table
            :max-capacity 256 :capacity 256))
         (state
           (http-kit/http3:make-qpack-decoder-stream-state
            encoder-table :blocked-stream-limit 1)))
    (http-kit/http3:qpack-dynamic-table-insert
     encoder-table "x-unacknowledged" "value")
    (http-kit/http3:qpack-decoder-stream-state-note-insertions-sent state 1)
    (let ((section
            (http-kit/http3:qpack-encode-field-section
             fields :dynamic-table encoder-table
             :decoder-stream-state state :stream-id 0)))
      (handler-case
          (progn
            (http-kit/http3:qpack-decode-field-section
             section :dynamic-table decoder-table)
            (error "Expected a blocked QPACK field section."))
        (http-kit/http3:qpack-blocked-field-section (condition)
          (ensure-equal
           1
           (http-kit/http3:qpack-blocked-field-section-required-insert-count
            condition))
          (ensure-equal
           0
           (http-kit/http3:qpack-blocked-field-section-current-insert-count
            condition))))
      (ensure-equal
       1
       (hash-table-count
        (http-kit/http3:qpack-decoder-stream-state-outstanding-sections
         state))))))

(deftest http3-qpack-malformed-section-is-not-blocked
  (let* ((table (http-kit/http3:make-qpack-dynamic-table
                 :max-capacity 256 :capacity 256))
         (context (http-kit/http3:make-http3-qpack-decoder-context
                   table :blocked-stream-limit 1)))
    (signals http-protocol-error
      (http-kit/http3:decode-http3-qpack-field-section
       (octets) context 0))
    (ensure-equal
     0
     (hash-table-count
      (http-kit/http3::http3-qpack-decoder-context-blocked-streams
       context)))))

(deftest http3-response-qpack-decode-serialization-ownership
  (labels ((malformed-headers-wire ()
             (http-kit/http3:encode-http3-frame
              (http-kit/http3:make-http3-frame
               :type http-kit/http3:+http3-headers-type+
               :payload (octets)))))
    (let ((inside-p nil)
          (events '())
          client)
      (setf client
            (http-kit/http3::%make-http3-client
             :serialize
             (lambda (thunk)
               (push :serialize-enter events)
               (let ((inside-p t))
                 (handler-bind
                     ((error (lambda (condition)
                               (declare (ignore condition))
                               (push (list :decode-condition inside-p) events))))
                   (funcall thunk))))
             :read-stream
             (lambda (stream &key timeout deadline)
               (declare (ignore stream timeout deadline))
               (push (list :read inside-p) events)
               (values (malformed-headers-wire) t))))
      (signals http-protocol-error
        (http-kit/http3::%h3-read-response
         client :stream :collect-body-p t
         :qpack-decoder-table
         (http-kit/http3::http3-client-qpack-decoder-table client)))
      (ensure-equal '(:serialize-enter (:read nil)
                      :serialize-enter (:decode-condition t))
                    (nreverse events)))
    (let* ((inside-p nil)
           (events '())
           (caller-table (http-kit/http3:make-qpack-dynamic-table))
           (client
             (http-kit/http3::%make-http3-client
              :serialize
              (lambda (thunk)
                (declare (ignore thunk))
                (push :unexpected-serialize events)
                (error "Caller-owned QPACK decode was serialized."))
              :read-stream
              (lambda (stream &key timeout deadline)
                (declare (ignore stream timeout deadline))
                (push (list :read inside-p) events)
                (values (malformed-headers-wire) t)))))
      (signals simple-error
        (http-kit/http3::%h3-read-response
         client :stream :collect-body-p t
         :qpack-decoder-table caller-table))
      (ensure-equal '(:unexpected-serialize) (nreverse events)))))

(deftest http3-client-blocked-response-pauses-same-chunk
  (multiple-value-bind (decoder-table section instruction)
      (http3-test-qpack-blocked-section
       (list (cons ":status" "200")
             (cons "content-length" "3")
             (cons "x-blocked" "ready")))
    (let* ((context
             (http-kit/http3:make-http3-qpack-decoder-context
              decoder-table :blocked-stream-limit 1))
           (wire
             (http3-test-concat-octets
              (http-kit/http3:encode-http3-frame
               (http-kit/http3:make-http3-frame
                :type http-kit/http3:+http3-headers-type+ :payload section))
              (http-kit/http3:encode-http3-frame
               (http-kit/http3:make-http3-frame
                :type http-kit/http3:+http3-data-type+
                :payload (octets 1 2 3)))))
           (events '())
           (inside-serializer-p nil)
           (read-count 0)
           (await-count 0)
           (encoder-table (http-kit/http3:make-qpack-dynamic-table))
           (client
             (http-kit/http3::%make-http3-client
              :qpack-decoder-table decoder-table
              :qpack-decoder-context context
              :open-stream
              (lambda (request &key stream-type timeout deadline)
                (declare (ignore request stream-type timeout deadline))
                (values :request-stream 12))
              :write-stream
              (lambda (stream octets &key fin-p timeout deadline)
                (declare (ignore stream octets fin-p timeout deadline)))
              :read-stream
              (lambda (stream &key timeout deadline)
                (declare (ignore stream timeout deadline))
                (incf read-count)
                (push :read events)
                (values wire t))
              :close-stream
              (lambda (stream &key condition)
                (declare (ignore stream condition)))
              :serialize
              (lambda (thunk)
                (setf inside-serializer-p t)
                (unwind-protect
                     (funcall thunk)
                  (setf inside-serializer-p nil))))))
      (let ((response
              (http-kit/http3:send-http3-request
               client
               (make-http-request :method "GET" :uri "https://example.test/")
               :qpack-encoder-table encoder-table
               :qpack-decoder-context context
               :await-qpack
               (lambda (token)
                 (incf await-count)
                 (push (list :await inside-serializer-p) events)
                 (ensure-equal nil
                               (find :data events :key (lambda (event)
                                                        (if (consp event)
                                                            (first event)
                                                            event))))
                 (http3-test-resume-qpack-token
                  context decoder-table instruction token)
                 (push :resumed events))
               :on-body-chunk (lambda (chunk)
                                (push (list :data (subseq chunk 0)) events)))))
        (ensure-equal (octets 1 2 3) (http-response-body response)))
      (ensure-equal 1 read-count)
      (ensure-equal 1 await-count)
      (ensure-equal
       '(:read (:await nil) :resumed (:data #(1 2 3)))
       (nreverse events)))))

(deftest http3-server-blocked-request-pauses-same-chunk
  (multiple-value-bind (decoder-table section instruction)
      (http3-test-qpack-blocked-section
       (list (cons ":method" "POST")
             (cons ":scheme" "https")
             (cons ":authority" "example.test")
             (cons ":path" "/")
             (cons "content-length" "3")
             (cons "x-blocked" "ready")))
    (let* ((context
             (http-kit/http3:make-http3-qpack-decoder-context
              decoder-table :blocked-stream-limit 1))
           (wire
             (http3-test-concat-octets
              (http-kit/http3:encode-http3-frame
               (http-kit/http3:make-http3-frame
                :type http-kit/http3:+http3-headers-type+ :payload section))
              (http-kit/http3:encode-http3-frame
               (http-kit/http3:make-http3-frame
                :type http-kit/http3:+http3-data-type+
                :payload (octets 4 5 6)))))
           (events '())
           (inside-serializer-p nil)
           (await-count 0))
      (http-kit/http3:serve-http3-request-stream
       :request-stream
       (lambda (request)
         (push (list :handler (http-request-body request)) events)
         (http-kit:make-http-response
          :status 200
          :headers (list (http-kit:make-http-header "content-length" "0"))
          :body (octets)))
       :read-stream
       (let ((read-p nil))
         (lambda (stream &key timeout deadline)
           (declare (ignore stream timeout deadline))
           (if read-p
               (values nil t)
               (progn
                 (setf read-p t)
                 (push :read events)
                 (values wire t)))))
       :write-stream
       (lambda (stream octets &key fin-p timeout deadline)
         (declare (ignore stream octets fin-p timeout deadline)))
       :qpack-decoder-context context
       :stream-id 16
       :qpack-serialize
       (lambda (thunk)
         (setf inside-serializer-p t)
         (unwind-protect
              (funcall thunk)
           (setf inside-serializer-p nil)))
       :await-qpack
       (lambda (token)
         (incf await-count)
         (push (list :await inside-serializer-p) events)
         (ensure-equal nil
                       (find :data events :key (lambda (event)
                                                (if (consp event)
                                                    (first event)
                                                    event))))
         (http3-test-resume-qpack-token
          context decoder-table instruction token)
         (push :resumed events))
       :on-body-chunk
       (lambda (chunk) (push (list :data (subseq chunk 0)) events)))
      (ensure-equal 1 await-count)
      (ensure-equal
       '(:read (:await nil) :resumed (:data #(4 5 6)) (:handler #(4 5 6)))
       (nreverse events)))))

(deftest http3-dynamic-qpack-integration-requires-stream-id-and-await
  (let* ((table (http-kit/http3:make-qpack-dynamic-table))
         (context
           (http-kit/http3:make-http3-qpack-decoder-context
            table :blocked-stream-limit 1))
         (reads 0)
         (client
           (http-kit/http3::%make-http3-client
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore stream timeout deadline))
              (incf reads)
              (values nil t)))))
    (signals http-protocol-error
      (http-kit/http3::%h3-read-response
       client :stream :qpack-decoder-context context
       :stream-id nil :await-qpack (lambda (token) (declare (ignore token)))))
    (signals http-protocol-error
      (http-kit/http3::%h3-read-response
       client :stream :qpack-decoder-context context :stream-id 0))
    (flet ((read-stream (stream &key timeout deadline)
             (declare (ignore stream timeout deadline))
             (incf reads)
             (values nil t))
           (write-stream (stream octets &key fin-p timeout deadline)
             (declare (ignore stream octets fin-p timeout deadline))))
      (signals http-protocol-error
        (http-kit/http3:serve-http3-request-stream
         :stream (lambda (request) (declare (ignore request)))
         :read-stream #'read-stream :write-stream #'write-stream
         :qpack-decoder-context context :stream-id nil
         :await-qpack (lambda (token) (declare (ignore token)))))
      (signals http-protocol-error
        (http-kit/http3:serve-http3-request-stream
         :stream (lambda (request) (declare (ignore request)))
         :read-stream #'read-stream :write-stream #'write-stream
         :qpack-decoder-context context :stream-id 0)))
    (ensure-equal 0 reads)))

(deftest http3-server-rejects-uncollected-trace-content
  (signals http-protocol-error
    (http-kit/http3::%h3-server-make-request
     (list "TRACE" nil "https" "example.test" "/" "/" nil
           (list (make-http-header "content-length" "1")))
     (octets) 1 nil nil)))

(deftest http3-response-body-boundaries
  (signals http-invalid-header
      (http-kit/http3::%h3-response-content-length
       (list (make-http-header "content-length" "0")) 0
       :status 204 :no-body t))
    (signals http-invalid-header
      (http-kit/http3::%h3-response-content-length
       (list (make-http-header "content-length" "0")) 0
       :status 200 :request-method "CONNECT" :no-body t))
    (signals http-invalid-header
      (http-kit/http3::%h3-response-content-length
       (list (make-http-header "content-length" "1")) 0
       :status 205 :no-body t))
    (http-kit/http3::%h3-response-content-length
     (list (make-http-header "content-length" "3")) 0
     :status 304 :no-body t)
    (http-kit/http3::%h3-response-content-length
     (list (make-http-header "content-length" "3")) 0
     :status 200 :request-method "HEAD" :no-body t)
    (let* ((wire
             (http3-test-concat-octets
              (http-kit/http3:encode-http3-frame
               (http-kit/http3:make-http3-frame
                :type http-kit/http3:+http3-headers-type+
                :payload
                (http-kit/http3:qpack-encode-field-section
                 (list (cons ":status" "204")))))
              (http-kit/http3:encode-http3-frame
               (http-kit/http3:make-http3-frame
                :type http-kit/http3:+http3-data-type+
                :payload (octets 1)))))
           (client
             (http-kit/http3::%make-http3-client
              :read-stream
              (lambda (stream &key timeout deadline)
                (declare (ignore stream timeout deadline))
                (values wire t)))))
      (signals http-protocol-error
        (http-kit/http3::%h3-read-response
         client :stream :request-method "GET" :collect-body-p t)))
  (let* ((wire
           (http3-test-concat-octets
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-headers-type+
              :payload
              (http-kit/http3:qpack-encode-field-section
               (list (cons ":status" "200")))))
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-data-type+
              :payload (octets 1 2 3)))))
         (client
           (http-kit/http3::%make-http3-client
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore stream timeout deadline))
              (values wire t)))))
    (ensure-equal
     (octets 1 2 3)
    (http-response-body
      (http-kit/http3::%h3-read-response
       client :stream :request-method "CONNECT" :collect-body-p t)))))

(deftest http3-disabled-server-push-is-id-error
  (let* ((wire
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-push-promise-type+
             :payload (http-kit/http3:http3-varint-encode 0))))
         (client
           (http-kit/http3::%make-http3-client
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore stream timeout deadline))
              (values wire t)))))
    (handler-case
        (progn
          (http-kit/http3::%h3-read-response
           client :stream :request-method "GET" :collect-body-p t)
          (error "Expected HTTP/3 PUSH_PROMISE to fail."))
      (http-protocol-error (condition)
        (ensure-equal :h3-id-error
                      (http-protocol-error-detail condition))))))

(deftest http3-push-promise-is-validated-tracked-and-reported
  (let* ((fields
           (list (cons ":method" "GET")
                 (cons ":scheme" "https")
                 (cons ":authority" "example.test")
                 (cons ":path" "/asset.css")))
         (wire
           (http3-test-concat-octets
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-push-promise-type+
              :payload
              (http3-test-concat-octets
               (http-kit/http3:http3-varint-encode 2)
               (http-kit/http3:qpack-encode-field-section fields))))
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-headers-type+
              :payload
              (http-kit/http3:qpack-encode-field-section
               (list (cons ":status" "204")))))))
         (promises '())
         (client
           (http-kit/http3::%make-http3-client
            :max-push-id 2
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore stream timeout deadline))
              (values wire t)))))
    (ensure-equal
     204
     (http-response-status
      (http-kit/http3::%h3-read-response
       client :stream :stream-id 0 :request-method "GET" :collect-body-p t
       :on-push-promise
       (lambda (push-id promised-fields)
         (push (list push-id promised-fields) promises)))))
    (ensure-equal '(2) (http-kit/http3:http3-client-promised-push-ids client))
    (ensure-equal (list (list 2 fields)) promises)))

(deftest http3-push-promise-rejects-out-of-range-identifiers
  (let* ((fields
           (http-kit/http3:qpack-encode-field-section
            (list (cons ":method" "GET")
                  (cons ":scheme" "https")
                  (cons ":authority" "example.test")
                  (cons ":path" "/asset.css"))))
         (make-wire
           (lambda (push-id)
             (http-kit/http3:encode-http3-frame
              (http-kit/http3:make-http3-frame
               :type http-kit/http3:+http3-push-promise-type+
               :payload
               (http3-test-concat-octets
                (http-kit/http3:http3-varint-encode push-id) fields))))))
    (let ((client
            (http-kit/http3::%make-http3-client
             :max-push-id 2
             :read-stream
             (lambda (stream &key timeout deadline)
               (declare (ignore stream timeout deadline))
               (values (funcall make-wire 3) t)))))
      (handler-case
          (progn
            (http-kit/http3::%h3-read-response
             client :stream :stream-id 0 :request-method "GET"
             :collect-body-p t)
            (error "Expected HTTP/3 PUSH_PROMISE identifier to fail."))
        (http-protocol-error (condition)
          (ensure-equal :h3-id-error
                        (http-protocol-error-detail condition)))))))

(deftest http3-repeated-push-promise-requires-identical-fields
  (let* ((fields
           (list (cons ":method" "GET")
                 (cons ":scheme" "https")
                 (cons ":authority" "example.test")
                 (cons ":path" "/asset.css")))
         (wire nil)
         (callbacks '())
         (client
           (http-kit/http3::%make-http3-client
            :max-push-id 2
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore stream timeout deadline))
              (values wire t))))
         (make-wire
           (lambda (promised-fields)
             (http3-test-concat-octets
              (http-kit/http3:encode-http3-frame
               (http-kit/http3:make-http3-frame
                :type http-kit/http3:+http3-push-promise-type+
                :payload
                (http3-test-concat-octets
                 (http-kit/http3:http3-varint-encode 2)
                 (http-kit/http3:qpack-encode-field-section
                  promised-fields))))
              (http-kit/http3:encode-http3-frame
               (http-kit/http3:make-http3-frame
                :type http-kit/http3:+http3-headers-type+
                :payload
                (http-kit/http3:qpack-encode-field-section
                 (list (cons ":status" "204")))))))))
    (dotimes (stream-id 2)
      (setf wire (funcall make-wire fields))
      (ensure-equal
       204
       (http-response-status
        (http-kit/http3::%h3-read-response
         client :stream :stream-id stream-id :request-method "GET"
         :collect-body-p t
         :on-push-promise
         (lambda (push-id promised-fields)
           (push (list push-id promised-fields) callbacks))))))
    (ensure-equal '(2) (http-kit/http3:http3-client-promised-push-ids client))
    (ensure-equal 1 (length (http-kit/http3::http3-client-push-promises client)))
    (ensure-equal 2 (length callbacks))
    (setf wire
          (funcall make-wire
                   (append (butlast fields) (list (cons ":path" "/other")))))
    (handler-case
        (progn
          (http-kit/http3::%h3-read-response
           client :stream :stream-id 2 :request-method "GET"
           :collect-body-p t)
          (error "Expected inconsistent PUSH_PROMISE fields to fail."))
      (http-protocol-error (condition)
        (ensure-equal :h3-id-error
                      (http-protocol-error-detail condition))))))

(deftest http3-push-stream-is-matched-consumed-and-read-in-chunks
  (let* ((promised-fields
           (list (cons ":method" "GET")
                 (cons ":scheme" "https")
                 (cons ":authority" "example.test")
                 (cons ":path" "/asset.css")))
         (response-wire
           (http3-test-concat-octets
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-headers-type+
              :payload
              (http-kit/http3:qpack-encode-field-section
               (list (cons ":status" "200")
                     (cons "content-length" "3")))))
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-data-type+
              :payload (octets 1 2 3)))
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-headers-type+
              :payload
              (http-kit/http3:qpack-encode-field-section
               (list (cons "x-push-trailer" "done")))))))
         (wire
           (http3-test-concat-octets
            (http-kit/http3:http3-varint-encode
             http-kit/http3:+http3-push-stream-type+)
            (http-kit/http3:http3-varint-encode 2)
            response-wire))
         (reads
           (list (list (subseq wire 0 1) nil)
                 (list (subseq wire 1 5) nil)
                 (list (subseq wire 5) t)))
         (closed '())
         (client
           (http-kit/http3::%make-http3-client
            :push-promises (list (cons 2 promised-fields))
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore stream timeout deadline))
              (let ((read (pop reads)))
                (values (first read) (second read))))
            :close-stream
            (lambda (stream &key condition)
              (push (list stream condition) closed)))))
    (multiple-value-bind (push-id response)
        (http-kit/http3:receive-http3-push client :push-stream)
      (ensure-equal 2 push-id)
      (ensure-equal 200 (http-response-status response))
      (ensure-equal (octets 1 2 3) (http-response-body response))
      (ensure-equal
       "done"
       (http-header-value (http-response-trailers response) "x-push-trailer")))
    (ensure-equal '(2) (http-kit/http3:http3-client-consumed-push-ids client))
    (ensure-equal (list (list :push-stream nil)) closed)
    (ensure-equal '() reads)))

(deftest http3-push-stream-enforces-per-receive-field-limits
  (let* ((promised-fields
           (list (cons ":method" "GET")
                 (cons ":scheme" "https")
                 (cons ":authority" "example.test")
                 (cons ":path" "/asset.css")))
         (wire
           (http3-test-concat-octets
            (http-kit/http3:http3-varint-encode
             http-kit/http3:+http3-push-stream-type+)
            (http-kit/http3:http3-varint-encode 2)
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-headers-type+
              :payload
              (http-kit/http3:qpack-encode-field-section
               (list (cons ":status" "200")
                     (cons "content-type" "text/css")))))))
         (make-client
           (lambda ()
             (http-kit/http3::%make-http3-client
              :push-promises (list (cons 2 promised-fields))
              :read-stream
              (lambda (stream &key timeout deadline)
                (declare (ignore stream timeout deadline))
                (values wire t))))))
    (signals http-size-limit-exceeded
      (http-kit/http3:receive-http3-push
       (funcall make-client) :push-stream :max-fields 1))
    (signals http-size-limit-exceeded
      (http-kit/http3:receive-http3-push
       (funcall make-client) :push-stream :max-header-bytes 1))
    (signals http-protocol-error
      (http-kit/http3:receive-http3-push
       (funcall make-client) :push-stream :max-fields 0))
    (signals http-protocol-error
      (http-kit/http3:receive-http3-push
       (funcall make-client) :push-stream :max-header-bytes 0))))

(deftest http3-push-stream-can-await-later-push-promise
  (let* ((promised-fields
           (list (cons ":method" "GET")
                 (cons ":scheme" "https")
                 (cons ":authority" "example.test")
                 (cons ":path" "/early.css")))
         (wire
           (http3-test-concat-octets
            (http-kit/http3:http3-varint-encode
             http-kit/http3:+http3-push-stream-type+)
            (http-kit/http3:http3-varint-encode 2)
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-headers-type+
              :payload
              (http-kit/http3:qpack-encode-field-section
               (list (cons ":status" "204")))))))
         (awaited '())
         (client
           (http-kit/http3::%make-http3-client
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore stream timeout deadline))
              (values wire t)))))
    (multiple-value-bind (push-id response)
        (http-kit/http3:receive-http3-push
         client :push-stream
         :await-push-promise
         (lambda (push-id)
           (push push-id awaited)
           (push (cons push-id (copy-tree promised-fields))
                 (http-kit/http3::http3-client-push-promises client))))
      (ensure-equal 2 push-id)
      (ensure-equal 204 (http-response-status response)))
    (ensure-equal '(2) awaited)
    (ensure-equal '(2) (http-kit/http3:http3-client-consumed-push-ids client))))

(deftest http3-push-stream-rejects-invalid-type-id-and-reuse
  (let ((promised-fields
          (list (cons ":method" "GET")
                (cons ":scheme" "https")
                (cons ":authority" "example.test")
                (cons ":path" "/asset.css"))))
    (dolist (case
             (list
              (list 0 2 '() :h3-stream-creation-error)
              (list http-kit/http3:+http3-push-stream-type+ 3 '() :h3-id-error)
              (list http-kit/http3:+http3-push-stream-type+ 2 '(2) :h3-id-error)))
      (let* ((wire
               (http3-test-concat-octets
                (http-kit/http3:http3-varint-encode (first case))
                (http-kit/http3:http3-varint-encode (second case))))
             (closed '())
             (client
               (http-kit/http3::%make-http3-client
                :push-promises (list (cons 2 promised-fields))
                :consumed-push-ids (copy-list (third case))
                :read-stream
                (lambda (stream &key timeout deadline)
                  (declare (ignore stream timeout deadline))
                  (values wire t))
                :close-stream
                (lambda (stream &key condition)
                  (declare (ignore stream))
                  (push condition closed)))))
        (handler-case
            (progn
              (http-kit/http3:receive-http3-push client :push-stream)
              (error "Expected invalid HTTP/3 push stream to fail."))
          (http-protocol-error (condition)
            (ensure-equal (fourth case)
                          (http-protocol-error-detail condition))))
        (ensure-equal 1 (length closed))
        (ensure (typep (first closed) 'http-protocol-error))))))

(deftest http3-client-cancels-promised-push-atomically
  (let ((writes '())
        (serialized 0)
        (promise (list (cons 2 (list (cons ":method" "GET"))))))
    (let ((client
            (http-kit/http3::%make-http3-client
             :control-stream :control
             :push-promises promise
             :write-stream
             (lambda (stream octets &key fin-p timeout deadline)
               (declare (ignore fin-p timeout deadline))
               (push (list stream octets) writes))
             :serialize
             (lambda (thunk)
               (incf serialized)
               (funcall thunk)))))
      (http-kit/http3:cancel-http3-push client 2)
      (http-kit/http3:cancel-http3-push client 2)
      (ensure-equal '(2)
                    (http-kit/http3:http3-client-cancelled-push-ids client))
      (ensure-equal 2 serialized)
      (ensure-equal 1 (length writes))
      (destructuring-bind (stream wire) (first writes)
        (ensure-equal :control stream)
        (let ((frames (http-kit/http3:decode-http3-frames wire)))
          (ensure-equal 1 (length frames))
          (ensure-equal http-kit/http3:+http3-cancel-push-type+
                        (http-kit/http3:http3-frame-type (first frames)))
          (multiple-value-bind (push-id end)
              (http-kit/http3:http3-varint-decode
               (http-kit/http3:http3-frame-payload (first frames)))
            (ensure-equal 2 push-id)
            (ensure-equal 1 end))))
      (signals http-protocol-error
        (http-kit/http3:cancel-http3-push client 3))))
  (let ((client
          (http-kit/http3::%make-http3-client
           :control-stream :control
           :push-promises (list (cons 2 '()))
           :write-stream
           (lambda (stream octets &key fin-p timeout deadline)
             (declare (ignore stream octets fin-p timeout deadline))
             (error "write failed")))))
    (signals error (http-kit/http3:cancel-http3-push client 2))
    (ensure-equal '()
                  (http-kit/http3:http3-client-cancelled-push-ids client))))

(deftest http3-client-increases-max-push-id-atomically
  (let ((writes '())
        (serialized 0))
    (let ((client
            (http-kit/http3::%make-http3-client
             :control-stream :control
             :max-push-id 2
             :write-stream
             (lambda (stream octets &key fin-p timeout deadline)
               (declare (ignore fin-p timeout deadline))
               (push (list stream octets) writes))
             :serialize
             (lambda (thunk)
               (incf serialized)
               (funcall thunk)))))
      (http-kit/http3:advertise-http3-max-push-id client 5)
      (ensure-equal 5 (http-kit/http3:http3-client-max-push-id client))
      (ensure-equal 1 serialized)
      (ensure-equal 1 (length writes))
      (destructuring-bind (stream wire) (first writes)
        (ensure-equal :control stream)
        (let ((frames (http-kit/http3:decode-http3-frames wire)))
          (ensure-equal 1 (length frames))
          (ensure-equal http-kit/http3:+http3-max-push-id-type+
                        (http-kit/http3:http3-frame-type (first frames)))
          (multiple-value-bind (push-id end)
              (http-kit/http3:http3-varint-decode
               (http-kit/http3:http3-frame-payload (first frames)))
            (ensure-equal 5 push-id)
            (ensure-equal 1 end))))
      (signals http-protocol-error
        (http-kit/http3:advertise-http3-max-push-id client 5))
      (signals http-protocol-error
        (http-kit/http3:advertise-http3-max-push-id client 1))
      (ensure-equal 5 (http-kit/http3:http3-client-max-push-id client))
      (ensure-equal 1 (length writes))))
  (let ((client
          (http-kit/http3::%make-http3-client
           :control-stream :control
           :max-push-id 2
           :write-stream
           (lambda (stream octets &key fin-p timeout deadline)
             (declare (ignore stream octets fin-p timeout deadline))
             (error "write failed")))))
    (signals error
      (http-kit/http3:advertise-http3-max-push-id client 3))
    (ensure-equal 2 (http-kit/http3:http3-client-max-push-id client))))

(deftest http3-cancelled-push-stream-is-aborted
  (let ((cancelled '())
        (closed '())
        (wire
          (http3-test-concat-octets
           (http-kit/http3:http3-varint-encode
            http-kit/http3:+http3-push-stream-type+)
           (http-kit/http3:http3-varint-encode 2))))
    (let ((client
            (http-kit/http3::%make-http3-client
             :push-promises (list (cons 2 '()))
             :cancelled-push-ids '(2)
             :read-stream
             (lambda (stream &key timeout deadline)
               (declare (ignore stream timeout deadline))
               (values wire t))
             :cancel-stream
             (lambda (stream &key error-code timeout deadline)
               (declare (ignore timeout deadline))
               (push (list stream error-code) cancelled))
             :close-stream
             (lambda (stream &key condition)
               (push (list stream condition) closed)))))
      (handler-case
          (progn
            (http-kit/http3:receive-http3-push client :push-stream)
            (error "Expected a cancelled HTTP/3 push stream to fail."))
        (http-protocol-error (condition)
          (ensure-equal :h3-request-cancelled
                        (http-protocol-error-detail condition))))
      (ensure-equal (list (list :push-stream #x10c)) cancelled)
      (ensure-equal 1 (length closed))
      (ensure-equal :push-stream (first (first closed)))
      (ensure (typep (second (first closed)) 'http-protocol-error)))))

(deftest http3-injected-quic-request-response
  (let* ((request-table
           (let ((table
                   (http-kit/http3:make-qpack-dynamic-table
                    :max-capacity 256
                    :capacity 256)))
             (http-kit/http3:qpack-dynamic-table-insert
              table "x-test" "yes")
             table))
         (response-table
           (let ((table
                   (http-kit/http3:make-qpack-dynamic-table
                    :max-capacity 256
                    :capacity 256)))
             (http-kit/http3:qpack-dynamic-table-insert
              table "content-type" "text/plain")
             table))
         (response-fields (list (cons ":status" "200")
                                (cons "content-type" "text/plain")
                                (cons "content-length" "5")))
         (response-headers
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-headers-type+
             :payload
             (http-kit/http3:qpack-encode-field-section
              response-fields
              :dynamic-table response-table
              :huffman-p t))))
         (response-data
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-data-type+
             :payload (octets 104 101 108 108 111))))
         (response-wire (http3-test-concat-octets response-headers response-data))
         (response-chunks
           (list (list (subseq response-wire 0 1) nil)
                 (list (subseq response-wire 1 4) nil)
                 (list (subseq response-wire 4) t)))
         (opened-streams nil)
         (request-stream nil)
         (body-chunks nil))
    (labels ((open-stream (request &key stream-type timeout deadline)
               (declare (ignore timeout deadline))
               (let ((stream
                       (make-http3-test-stream
                        :kind stream-type
                        :reads (if (eq stream-type :request)
                                   response-chunks
                                   nil))))
                 (when (eq stream-type :request)
                   (ensure-true (http-request-p request))
                   (setf request-stream stream))
                 (push stream opened-streams)
                 stream))
             (write-stream (stream octets &key fin-p timeout deadline)
               (declare (ignore timeout deadline))
               (push (list (subseq octets 0) fin-p)
                     (http3-test-stream-writes stream)))
             (read-stream (stream &key timeout deadline)
               (declare (ignore timeout deadline))
               (let ((entry (pop (http3-test-stream-reads stream))))
                 (if entry
                     (values (first entry) (second entry))
                     (values nil t))))
             (close-stream (stream &key condition)
               (declare (ignore condition))
               (setf (http3-test-stream-closed-p stream) t)))
      (let ((client
              (http-kit/http3:make-http3-client
               :open-stream #'open-stream
               :write-stream #'write-stream
               :read-stream #'read-stream
               :close-stream #'close-stream
               :max-push-id 3)))
        (ensure-true (http-kit/http3:http3-client-open-p client))
        (let ((response
                (http-kit/http3:send-http3-request
                 client
                 (make-http-request
                  :method "POST"
                  :uri "https://example.test/resource?q=1"
                  :headers (list (make-http-header "x-test" "yes"))
                  :body (octets 65 66))
                 :collect-body-p nil
                 :qpack-encoder-table request-table
                 :qpack-decoder-table response-table
                 :huffman-p t
                 :on-body-chunk
                 (lambda (chunk)
                   (push (subseq chunk 0) body-chunks)))))
          (ensure-equal "HTTP/3" (http-response-protocol-version response))
          (ensure-equal 200 (http-response-status response))
          (ensure-equal 0 (length (http-response-body response)))
          (ensure-equal "5"
                        (http-header-value
                         (http-response-headers response)
                         "content-length"))
          (ensure-equal (octets 104 101 108 108 111)
                        (apply #'http3-test-concat-octets
                               (nreverse body-chunks))))
        (let* ((control-stream
                 (find :control opened-streams
                       :key #'http3-test-stream-kind))
               (control-wire
                 (first (http3-test-stream-writes control-stream))))
          (multiple-value-bind (stream-type position)
              (http-kit/http3:http3-varint-decode (first control-wire))
            (ensure-equal http-kit/http3:+http3-control-stream-type+
                          stream-type)
            (multiple-value-bind (frames remainder)
                (http-kit/http3:decode-http3-frames
                 (subseq (first control-wire) position))
              (ensure-equal 0 (length remainder))
              (ensure-equal 2 (length frames))
              (ensure-equal http-kit/http3:+http3-settings-type+
                            (http-kit/http3:http3-frame-type (first frames)))
              (ensure-equal http-kit/http3:+http3-max-push-id-type+
                            (http-kit/http3:http3-frame-type (second frames)))
              (multiple-value-bind (max-push-id end)
                  (http-kit/http3:http3-varint-decode
                   (http-kit/http3:http3-frame-payload (second frames)))
                (ensure-equal 3 max-push-id)
                (ensure-equal 1 end)))))
        (let* ((request-writes
                 (nreverse (http3-test-stream-writes request-stream)))
               (header-wire (first (first request-writes)))
               (data-wire (first (second request-writes))))
          (multiple-value-bind (frames remainder)
              (http-kit/http3:decode-http3-frames header-wire)
            (ensure-equal 0 (length remainder))
            (ensure-equal http-kit/http3:+http3-headers-type+
                          (http-kit/http3:http3-frame-type (first frames)))
            (ensure-equal
             (list (cons ":method" "POST")
                   (cons ":scheme" "https")
                   (cons ":authority" "example.test")
                   (cons ":path" "/resource?q=1")
                   (cons "content-length" "2")
                   (cons "x-test" "yes"))
             (http-kit/http3:qpack-decode-field-section
              (http-kit/http3:http3-frame-payload (first frames))
              :dynamic-table request-table)))
          (multiple-value-bind (frames remainder)
              (http-kit/http3:decode-http3-frames data-wire)
            (ensure-equal 0 (length remainder))
            (ensure-equal http-kit/http3:+http3-data-type+
                          (http-kit/http3:http3-frame-type (first frames)))))
        (http-kit/http3:close-http3-client client)
        (ensure-true (every #'http3-test-stream-closed-p opened-streams))
        (ensure-true (not (http-kit/http3:http3-client-open-p client)))))))

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
                                   "x-response-trailer" "done") )
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

(deftest http3-extended-connect-request-fields
  (let ((request
          (make-http-request
           :method "CONNECT"
           :protocol "websocket"
           :uri (make-http-uri
                 :scheme "https"
                 :authority "example.test"
                 :path "/chat")
           :headers nil
           :body (octets))))
    (ensure-equal
     (list (cons ":method" "CONNECT")
           (cons ":protocol" "websocket")
           (cons ":scheme" "https")
           (cons ":authority" "example.test")
           (cons ":path" "/chat"))
     (http-kit/http3::%h3-request-fields request)))
  (signals http-invalid-header
    (http-kit/http3::%h3-request-fields
     (make-http-request :method "CONNECT"
                        :protocol "websocket"
                        :uri "https://example.test/"
                        :request-target "*")))
  (multiple-value-bind (method protocol scheme authority target path query headers)
      (http-kit/http3::%h3-server-parse-request-fields
       (list (cons ":method" "CONNECT")
             (cons ":protocol" "websocket")
             (cons ":scheme" "https")
             (cons ":authority" "example.test")
             (cons ":path" "/chat"))
       :enable-connect-p t)
    (ensure-equal "CONNECT" method)
    (ensure-equal "websocket" protocol)
    (ensure-equal "https" scheme)
    (ensure-equal "example.test" authority)
    (ensure-equal "/chat" target)
    (ensure-equal "/chat" path)
    (ensure-equal nil query)
    (ensure-equal nil headers)))

(deftest http3-request-target-form-boundaries
  (ensure-equal
   "*"
   (cdr (assoc ":path"
               (http-kit/http3::%h3-request-fields
                (make-http-request :method "OPTIONS"
                                   :uri "https://example.test/"
                                   :request-target "*"))
               :test #'string=)))
  (signals http-invalid-header
    (http-kit/http3::%h3-request-fields
     (make-http-request :method "GET"
                        :uri "https://example.test/"
                        :request-target "*")))
  (signals http-invalid-header
    (http-kit/http3::%h3-request-fields
     (make-http-request :method "GET"
                        :uri "https://example.test/"
                        :request-target "https://example.test/"))))

(deftest http3-extended-connect-request-boundaries
  (handler-case
      (progn
        (http-kit/http3::%h3-server-parse-request-fields
         (list (cons ":method" "GET")
               (cons ":scheme" "https")
               (cons ":authority" "example.test")
               (cons ":path" "/")
               (cons ":unknown" "value")))
        (error "Expected an unknown HTTP/3 request pseudo-field to fail."))
    (http-protocol-error (condition)
      (ensure-equal :h3-message-error
                    (http-protocol-error-detail condition))))
  (dolist (fields
            (list
             (list (cons ":method" "CONNECT")
                   (cons ":protocol" "websocket")
                   (cons ":authority" "example.test")
                   (cons ":path" "/chat"))
             (list (cons ":method" "CONNECT")
                   (cons ":protocol" "websocket")
                   (cons ":scheme" "ftp")
                   (cons ":authority" "example.test")
                   (cons ":path" "/chat"))
             (list (cons ":method" "CONNECT")
                   (cons ":protocol" "websocket")
                   (cons ":scheme" "https")
                   (cons ":authority" "example.test"))))
    (signals http-protocol-error
      (http-kit/http3::%h3-server-parse-request-fields fields)))
  (signals http-protocol-error
    (http-kit/http3::%h3-server-parse-request-fields
     (list (cons ":method" "CONNECT")
           (cons ":protocol" "websocket")
           (cons ":scheme" "https")
           (cons ":authority" "example.test")
           (cons ":path" "*"))
     :enable-connect-p t))
  (signals http-protocol-error
    (http-kit/http3::%h3-server-parse-request-fields
     (list (cons ":method" "CONNECT")
           (cons ":protocol" "websocket")
           (cons ":scheme" "https")
           (cons ":authority" "example.test")
           (cons ":path" "/chat"))))
  (signals http-protocol-error
    (http-kit/http3::%h3-server-parse-request-fields
     (list (cons ":method" "GET")
           (cons ":protocol" "websocket")
           (cons ":scheme" "https")
           (cons ":authority" "example.test")
           (cons ":path" "/chat"))
     :enable-connect-p t)))

(deftest http3-request-effective-authority-boundaries
  (multiple-value-bind (method protocol scheme authority target path query headers)
      (http-kit/http3::%h3-server-parse-request-fields
       (list (cons ":method" "GET")
             (cons ":scheme" "https")
             (cons ":path" "/resource")
             (cons "host" "example.test")))
    (declare (ignore protocol scheme target path query))
    (ensure-equal "GET" method)
    (ensure-equal "example.test" authority)
    (ensure-equal '("host") (mapcar #'http-header-name headers)))
  (dolist (fields
            (list
             (list (cons ":method" "GET")
                   (cons ":scheme" "https")
                   (cons ":path" "/"))
             (list (cons ":method" "GET")
                   (cons ":scheme" "https")
                   (cons ":path" "/")
                   (cons "host" ""))
             (list (cons ":method" "GET")
                   (cons ":scheme" "https")
                   (cons ":path" "/")
                   (cons "host" "first.test")
                   (cons "host" "second.test"))
             (list (cons ":method" "GET")
                   (cons ":scheme" "https")
                   (cons ":authority" "")
                   (cons ":path" "/")
                   (cons "host" "example.test"))
             (list (cons ":method" "CONNECT")
                   (cons "host" "example.test:443"))
             (list (cons ":method" "GET")
                   (cons ":scheme" "https")
                   (cons ":authority" "authority.test")
                   (cons ":path" "/")
                   (cons "host" "host.test"))))
    (signals http-protocol-error
      (http-kit/http3::%h3-server-parse-request-fields fields))))

(deftest http3-te-and-trailer-direction-boundaries
  (let ((request
          (make-http-request
           :method "GET"
           :uri "https://example.test/"
           :headers (list (make-http-header "trailer" "x-checksum")
                          (make-http-header "te" "trailers")))))
    (ensure-equal
     (list (cons ":method" "GET")
           (cons ":scheme" "https")
           (cons ":authority" "example.test")
           (cons ":path" "/")
           (cons "trailer" "x-checksum")
           (cons "te" "trailers"))
     (http-kit/http3::%h3-request-fields request)))
  (multiple-value-bind (status headers)
      (http-kit/http3::%h3-response-fields
       (list (cons ":status" "200")
             (cons "trailer" "x-checksum")))
    (ensure-equal 200 status)
    (ensure-equal '("trailer")
                  (mapcar #'http-header-name headers)))
  (signals http-invalid-header
    (http-kit/http3::%h3-response-fields
     (list (cons ":status" "200") (cons "te" "trailers"))))
  (signals http-invalid-header
    (http-kit/http3::%h3-trailer-fields
     (make-http-request
      :method "GET"
      :uri "https://example.test/"
      :trailers (list (make-http-header "te" "trailers")))))
  (dolist (name '("authorization" "if-none-match" "content-type"
                  "cache-control" "set-cookie" "via"))
    (signals http-invalid-header
      (http-kit/http3::%h3-trailer-fields
       (make-http-request
        :method "GET"
        :uri "https://example.test/"
        :trailers (list (make-http-header name "forbidden")))))
    (signals http-invalid-header
      (http-kit/http3::%h3-response-fields
       (list (cons name "forbidden")) :trailers-p t))
    (signals http-invalid-header
      (http-kit/http3::%h3-server-parse-trailer-fields
       (list (cons name "forbidden"))))
    (signals http-invalid-header
      (http-kit/http3::%h3-server-response-trailer-fields
       (list (make-http-header name "forbidden")))))
  (multiple-value-bind (method protocol scheme authority target path query headers)
      (http-kit/http3::%h3-server-parse-request-fields
       (list (cons ":method" "GET")
             (cons ":scheme" "https")
             (cons ":authority" "example.test")
             (cons ":path" "/")
             (cons "trailer" "x-checksum")
             (cons "te" "trailers")))
    (declare (ignore protocol scheme authority target path query))
    (ensure-equal "GET" method)
    (ensure-equal '("trailer" "te")
                  (mapcar #'http-header-name headers)))
  (signals http-invalid-header
    (http-kit/http3::%h3-server-parse-trailer-fields
     (list (cons "te" "trailers"))))
  (ensure-equal
   (list (cons ":status" "200") (cons "trailer" "x-checksum"))
   (http-kit/http3::%h3-server-response-fields
    200 (list (make-http-header "trailer" "x-checksum"))))
  (signals http-invalid-header
    (http-kit/http3::%h3-server-response-fields
     200 (list (make-http-header "te" "trailers"))))
  (signals http-invalid-header
    (http-kit/http3::%h3-server-response-trailer-fields
     (list (make-http-header "te" "trailers")))))

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

(deftest http3-client-cancellation-callbacks-require-functions
  (labels ((open-stream (request &key stream-type timeout deadline)
             (declare (ignore request timeout deadline))
             (make-http3-test-stream :kind stream-type))
           (write-stream (stream octets &key fin-p timeout deadline)
             (declare (ignore stream octets fin-p timeout deadline)))
           (read-stream (stream &key timeout deadline)
             (declare (ignore stream timeout deadline))
             (values nil t)))
    (signals http-protocol-error
      (http-kit/http3:make-http3-client
       :open-stream #'open-stream
       :write-stream #'write-stream
       :read-stream #'read-stream
       :cancel-stream :not-a-function))
    (let ((client
            (http-kit/http3:make-http3-client
             :open-stream #'open-stream
             :write-stream #'write-stream
             :read-stream #'read-stream
             :cancel-stream (lambda (stream &key error-code timeout deadline)
                              (declare (ignore stream error-code timeout deadline))))))
      (signals http-protocol-error
        (http-kit/http3:send-http3-request
         client
         (make-http-request :method "GET" :uri "https://example.test/")
         :on-stream-open :not-a-function)))))

(deftest http3-client-rejects-request-stream-excluded-by-goaway
  (let ((request-stream (make-http3-test-stream :kind :request))
        (request-writes 0)
        (closed-condition nil))
    (labels ((open-stream (request &key stream-type timeout deadline)
               (declare (ignore request timeout deadline))
               (if (eq stream-type :request)
                   (values request-stream 4)
                   (make-http3-test-stream :kind stream-type)))
             (write-stream (stream octets &key fin-p timeout deadline)
               (declare (ignore octets fin-p timeout deadline))
               (when (eq stream request-stream)
                 (incf request-writes)))
             (read-stream (stream &key timeout deadline)
               (declare (ignore stream timeout deadline))
               (error "A GOAWAY-rejected request must not be read."))
             (close-stream (stream &key condition)
               (when (eq stream request-stream)
                 (setf closed-condition condition))))
      (let ((client
              (http-kit/http3:make-http3-client
               :open-stream #'open-stream
               :write-stream #'write-stream
               :read-stream #'read-stream
               :close-stream #'close-stream)))
        (setf (http-kit/http3:http3-control-state-goaway-id
               (http-kit/http3:http3-client-peer-control-state client))
              4)
        (signals http-protocol-error
          (http-kit/http3:send-http3-request
           client
           (make-http-request :method "GET" :uri "https://example.test/")))))
    (ensure-equal 0 request-writes)
    (ensure-true (typep closed-condition 'http-protocol-error))
    (ensure-equal :h3-request-rejected
                  (http-protocol-error-detail closed-condition))))

(deftest http3-request-cancel-function-forwards-default-code-and-timing
  (let ((request-stream (make-http3-test-stream :kind :request))
        (cancel-call nil)
        (closed-condition :unset))
    (labels ((open-stream (request &key stream-type timeout deadline)
               (declare (ignore request timeout deadline))
               (if (eq stream-type :request)
                   (values request-stream 17)
                   (make-http3-test-stream :kind stream-type)))
             (write-stream (stream octets &key fin-p timeout deadline)
               (declare (ignore stream octets fin-p timeout deadline)))
             (read-stream (stream &key timeout deadline)
               (declare (ignore stream timeout deadline))
               (error "Stop after observing request cancellation."))
             (close-stream (stream &key condition)
               (when (eq stream request-stream)
                 (setf closed-condition condition)))
             (cancel-stream (stream &key error-code timeout deadline)
               (setf cancel-call (list stream error-code timeout deadline))))
      (let ((client
              (http-kit/http3:make-http3-client
               :open-stream #'open-stream
               :write-stream #'write-stream
               :read-stream #'read-stream
               :close-stream #'close-stream
               :cancel-stream #'cancel-stream)))
        (signals error
          (http-kit/http3:send-http3-request
           client
           (make-http-request :method "GET" :uri "https://example.test/")
           :timeout 23
           :deadline 47
           :on-stream-open
           (lambda (stream stream-id cancel-function)
             (ensure-true (eq request-stream stream))
             (ensure-equal 17 stream-id)
             (ensure-true (functionp cancel-function))
             (funcall cancel-function))))))
    (ensure-equal (list request-stream #x10c 23 47) cancel-call)
    (ensure-true (typep closed-condition 'error))))

(deftest http3-request-cancel-failure-still-closes-request-stream
  (let ((request-stream (make-http3-test-stream :kind :request))
        (cancel-attempted-p nil)
        (closed-condition :unset))
    (labels ((open-stream (request &key stream-type timeout deadline)
               (declare (ignore request timeout deadline))
               (if (eq stream-type :request)
                   (values request-stream 19)
                   (make-http3-test-stream :kind stream-type)))
             (write-stream (stream octets &key fin-p timeout deadline)
               (declare (ignore stream octets fin-p timeout deadline)))
             (read-stream (stream &key timeout deadline)
               (declare (ignore stream timeout deadline))
               (error "The cancellation failure must stop before reading."))
             (close-stream (stream &key condition)
               (when (eq stream request-stream)
                 (setf closed-condition condition)))
             (cancel-stream (stream &key error-code timeout deadline)
               (declare (ignore stream error-code timeout deadline))
               (setf cancel-attempted-p t)
               (error "QUIC cancellation failed.")))
      (let ((client
              (http-kit/http3:make-http3-client
               :open-stream #'open-stream
               :write-stream #'write-stream
               :read-stream #'read-stream
               :close-stream #'close-stream
               :cancel-stream #'cancel-stream)))
        (signals error
          (http-kit/http3:send-http3-request
           client
           (make-http-request :method "GET" :uri "https://example.test/")
           :on-stream-open
           (lambda (stream stream-id cancel-function)
             (declare (ignore stream stream-id))
             (funcall cancel-function))))))
    (ensure-true cancel-attempted-p)
    (ensure-true (typep closed-condition 'error))))

(deftest http3-control-role-is-required-for-role-sensitive-processing
  (let ((state (http-kit/http3:make-http3-control-state
                :settings-received-p t)))
    (signals http-protocol-error
      (http-kit/http3:process-http3-control-frame
       state
       (http-kit/http3:make-http3-frame
        :type http-kit/http3:+http3-goaway-type+
        :payload (http-kit/http3:http3-varint-encode 0)))))
  (signals http-protocol-error
    (http-kit/http3:serve-http3-control-stream
     :stream
     :read-stream (lambda (stream &key timeout deadline)
                    (declare (ignore stream timeout deadline))
                    (values nil t)))))

(deftest http3-priority-update-boundaries
  (let ((request
          (http-kit/http3:make-http3-priority-update-frame
           :element-id 8 :priority-field-value "u=0, i")))
    (multiple-value-bind (element-id value kind)
        (http-kit/http3:decode-http3-priority-update request)
      (ensure-equal 8 element-id)
      (ensure-equal "u=0, i" value)
      (ensure-equal :request kind))
    (let ((state (http-kit/http3:make-http3-control-state
                  :settings-received-p t :peer-role :client)))
      (ensure-equal :priority-update
                    (http-kit/http3:process-http3-control-frame state request))
      (ensure-equal "u=0, i"
                    (cdr (assoc (cons :request 8)
                                (http-kit/http3:http3-control-state-priority-updates
                                 state)
                                :test #'equal)))
      (http-kit/http3:process-http3-control-frame
       state
       (http-kit/http3:make-http3-priority-update-frame
        :element-id 8 :priority-field-value "u=5"))
      (ensure-equal "u=5"
                    (cdr (assoc (cons :request 8)
                                (http-kit/http3:http3-control-state-priority-updates
                                 state)
                                :test #'equal)))))
  (signals http-protocol-error
    (http-kit/http3:process-http3-control-frame
     (http-kit/http3:make-http3-control-state
      :settings-received-p t :peer-role :client)
     (http-kit/http3:make-http3-priority-update-frame
      :element-id 3 :priority-field-value "u=1")))
  (let ((state (http-kit/http3:make-http3-control-state
                :settings-received-p t :peer-role :client
                :max-push-id 4 :promised-push-ids '(2))))
    (ensure-equal
     :priority-update
     (http-kit/http3:process-http3-control-frame
      state
      (http-kit/http3:make-http3-priority-update-frame
       :kind :push :element-id 2 :priority-field-value "i")))
    (signals http-protocol-error
      (http-kit/http3:process-http3-control-frame
       state
       (http-kit/http3:make-http3-priority-update-frame
        :kind :push :element-id 3 :priority-field-value "u=7"))))
  (handler-case
      (progn
        (http-kit/http3:process-http3-control-frame
         (http-kit/http3:make-http3-control-state
          :settings-received-p t :peer-role :server)
         (http-kit/http3:make-http3-priority-update-frame
          :element-id 0 :priority-field-value "u=3"))
        (error "Expected a server PRIORITY_UPDATE to fail."))
    (http-protocol-error (condition)
      (ensure-equal :h3-frame-unexpected
                    (http-protocol-error-detail condition))))
  (signals http-protocol-error
    (http-kit/http3:make-http3-priority-update-frame
     :element-id 0 :priority-field-value (string #\Newline)))
  (let* ((frame
           (http-kit/http3:make-http3-priority-update-frame
            :element-id 0 :priority-field-value "u=1"))
         (wire (http-kit/http3:encode-http3-frame frame))
         (client
           (http-kit/http3::%make-http3-client
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore stream timeout deadline))
              (values wire t)))))
    (signals http-protocol-error
      (http-kit/http3::%h3-read-response
       client :stream :request-method "GET" :collect-body-p t)))
  (let ((wire
          (http-kit/http3:encode-http3-frame
           (http-kit/http3:make-http3-priority-update-frame
            :element-id 0 :priority-field-value "u=1"))))
    (signals http-protocol-error
      (http-kit/http3:serve-http3-request-stream
       :stream
       (lambda (request)
         (declare (ignore request))
         (make-http-response :status 204))
       :read-stream
       (lambda (stream &key timeout deadline)
         (declare (ignore stream timeout deadline))
         (values wire t))
       :write-stream
       (lambda (stream octets &key fin-p timeout deadline)
         (declare (ignore stream octets fin-p timeout deadline)))))))

(deftest http3-public-priority-update
  (let ((writes nil)
        (serialized 0))
    (let ((client
            (http-kit/http3::%make-http3-client
             :control-stream :control
             :write-stream
             (lambda (stream octets &key fin-p timeout deadline)
               (declare (ignore fin-p timeout deadline))
               (push (list stream octets) writes))
             :serialize
             (lambda (thunk)
               (incf serialized)
               (funcall thunk)))))
      (http-kit/http3:send-http3-priority-update
       client 8 :urgency 0 :incremental t)
      (destructuring-bind (stream wire) (first writes)
        (ensure-equal :control stream)
        (let ((frames (http-kit/http3:decode-http3-frames wire)))
          (ensure-equal 1 (length frames))
          (multiple-value-bind (element-id value kind)
              (http-kit/http3:decode-http3-priority-update (first frames))
            (ensure-equal 8 element-id)
            (ensure-equal "u=0, i" value)
            (ensure-equal :request kind))))
      (ensure-equal 1 serialized)
      (signals http-protocol-error
        (http-kit/http3:send-http3-priority-update client 3))
      (signals http-protocol-error
        (http-kit/http3:send-http3-priority-update client 2 :kind :push)))))

(deftest http3-server-push-sends-promise-before-push-response
  (let* ((control-state
           (http-kit/http3:make-http3-control-state
            :peer-role :client :max-push-id 3))
         (request
           (make-http-request :method "GET"
                              :uri "https://example.test/asset.css"))
         (response
           (make-http-response
            :status 200
            :headers (list (make-http-header "content-length" "2"))
            :body (octets 4 5)))
         (writes '())
         (opened '())
         (closed '())
         (serialized 0))
    (multiple-value-bind (push-id returned-response)
        (http-kit/http3:send-http3-push
         :request-stream request response control-state
         :open-stream
         (lambda (opened-request &key stream-type timeout deadline)
           (declare (ignore timeout deadline))
           (push (list opened-request stream-type) opened)
           :push-stream)
         :write-stream
         (lambda (stream octets &key fin-p timeout deadline)
           (declare (ignore timeout deadline))
           (push (list stream octets fin-p) writes))
         :close-stream
         (lambda (stream &key condition)
           (push (list stream condition) closed))
         :serialize
         (lambda (thunk)
           (incf serialized)
           (funcall thunk)))
      (ensure-equal 0 push-id)
      (ensure (eq response returned-response)))
    (ensure-equal 1 serialized)
    (ensure-equal '(0)
                  (http-kit/http3:http3-control-state-promised-push-ids
                   control-state))
    (ensure-equal (list (list request :push)) opened)
    (ensure-equal (list (list :push-stream nil)) closed)
    (let* ((ordered (nreverse writes))
           (promise-write (first ordered))
           (prefix-write (second ordered))
           (response-writes (cddr ordered)))
      (ensure-equal :request-stream (first promise-write))
      (let ((frames (http-kit/http3:decode-http3-frames
                     (second promise-write))))
        (ensure-equal 1 (length frames))
        (ensure-equal http-kit/http3:+http3-push-promise-type+
                      (http-kit/http3:http3-frame-type (first frames)))
        (multiple-value-bind (push-id position)
            (http-kit/http3:http3-varint-decode
             (http-kit/http3:http3-frame-payload (first frames)))
          (ensure-equal 0 push-id)
          (ensure (< position
                     (length (http-kit/http3:http3-frame-payload
                              (first frames)))))))
      (ensure-equal :push-stream (first prefix-write))
      (ensure-equal
       (http3-test-concat-octets
        (http-kit/http3:http3-varint-encode
         http-kit/http3:+http3-push-stream-type+)
        (http-kit/http3:http3-varint-encode 0))
       (second prefix-write))
      (ensure-equal nil (third prefix-write))
      (ensure-equal 2 (length response-writes))
      (ensure-equal t (third (second response-writes))))))

(deftest http3-server-push-enforces-credit-and-cancellation
  (let ((request
          (make-http-request :method "GET"
                             :uri "https://example.test/asset.css"))
        (response (make-http-response :status 204 :body (octets))))
    (dolist (state
             (list
              (http-kit/http3:make-http3-control-state :peer-role :client)
              (http-kit/http3:make-http3-control-state
               :peer-role :client :max-push-id 0 :promised-push-ids '(0))
              (http-kit/http3:make-http3-control-state
               :peer-role :client :max-push-id 0
               :cancelled-push-ids '(0))))
      (signals http-protocol-error
        (http-kit/http3:send-http3-push
         :request-stream request response state
         :push-id 0
         :open-stream
         (lambda (&rest arguments)
           (declare (ignore arguments))
           (error "A rejected push must not open a stream."))
         :write-stream
         (lambda (&rest arguments)
           (declare (ignore arguments))
           (error "A rejected push must not write.")))))))

(deftest http3-connection-manager-reuses-and-expires-clients
  (let* ((now 0)
         (opened 0)
         (control-streams-closed 0)
         (response-wire
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-headers-type+
             :payload
             (http-kit/http3:qpack-encode-field-section
              (list (cons ":status" "204"))))))
         (manager
           (http-kit/http3:make-http3-connection-manager
            :max-connection-age 5
            :clock-function (lambda () now)
            :open-client
            (lambda (request &key timeout deadline)
              (declare (ignore timeout deadline))
              (ensure-true (http-request-p request))
              (incf opened)
              (http-kit/http3:make-http3-client
               :open-stream
               (lambda (stream-request &key stream-type timeout deadline)
                 (declare (ignore stream-request timeout deadline))
                 (values (list stream-type (gensym "H3-STREAM-"))
                         (and (eq stream-type :request) 0)))
               :write-stream
               (lambda (stream octets &key fin-p timeout deadline)
                 (declare (ignore stream octets fin-p timeout deadline)))
               :read-stream
               (lambda (stream &key timeout deadline)
                 (declare (ignore stream timeout deadline))
                 (values response-wire t))
               :close-stream
               (lambda (stream &key condition)
                 (declare (ignore condition))
                 (when (eq (first stream) :control)
                   (incf control-streams-closed))))))))
    (dolist (time '(0 4 5))
      (setf now time)
      (ensure-equal
       204
       (http-response-status
        (http-kit/http3:send-http3-request-over-connection-manager
         manager
         (make-http-request :method "GET"
                            :uri "https://manager.example/resource")))))
    (ensure-equal 2 opened)
    (ensure-equal 1 control-streams-closed)
    (ensure-equal 1
                  (http-kit/http3:http3-connection-manager-connection-count
                   manager))
    (ensure-equal 5
                  (http-kit/http3:http3-connection-manager-max-connection-age
                   manager))
    (ensure-true
     (http-kit/http3:http3-connection-manager-open-p manager))
    (http-kit/http3:close-http3-connection-manager manager)
    (ensure-equal 2 control-streams-closed)
    (ensure-equal 0
                  (http-kit/http3:http3-connection-manager-connection-count
                   manager))
    (signals http-protocol-error
      (http-kit/http3:send-http3-request-over-connection-manager
       manager
       (make-http-request :method "GET"
                          :uri "https://manager.example/closed")))))

(deftest http3-connection-manager-validates-policies
  (let ((opener
          (lambda (request &key timeout deadline)
            (declare (ignore request timeout deadline))
            nil)))
    (dolist (arguments
             (list (list :max-connections 0)
                   (list :idle-timeout -1)
                   (list :idle-timeout "later")
                   (list :max-connection-age -1)
                   (list :max-connection-age "later")
                   (list :clock-function 7)))
      (signals http-protocol-error
        (apply #'http-kit/http3:make-http3-connection-manager
               :open-client opener arguments)))
    (let ((manager
            (http-kit/http3:make-http3-connection-manager
             :open-client opener
             :idle-timeout 1
             :clock-function (lambda () "now"))))
      (signals http-protocol-error
       (http-kit/http3:send-http3-request-over-connection-manager
         manager
         (make-http-request :method "GET"
                            :uri "https://manager.example/invalid-clock"))))))

(deftest http3-connection-manager-transport-enforces-client-contract
  (let* ((opened 0)
         (opened-client nil)
         (response-wire
           (http3-test-concat-octets
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-headers-type+
              :payload
              (http-kit/http3:qpack-encode-field-section
               (list (cons ":status" "103")
                     (cons "link" "</style.css>; rel=preload")))))
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-headers-type+
              :payload
              (http-kit/http3:qpack-encode-field-section
               (list (cons ":status" "199")))))
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-headers-type+
              :payload
              (http-kit/http3:qpack-encode-field-section
               (list (cons ":status" "204")))))))
         (manager
           (http-kit/http3:make-http3-connection-manager
            :open-client
            (lambda (request &key timeout deadline)
              (declare (ignore request timeout deadline))
              (incf opened)
              (setf opened-client
                    (http-kit/http3:make-http3-client
                     :max-header-bytes 1024 :max-fields 4
                     :open-stream
                     (lambda (stream-request &key stream-type timeout deadline)
                       (declare (ignore stream-request timeout deadline))
                       (values (list stream-type) 0))
                     :write-stream
                     (lambda (stream octets &key fin-p timeout deadline)
                       (declare (ignore stream octets fin-p timeout deadline)))
                     :read-stream
                     (lambda (stream &key timeout deadline)
                       (declare (ignore stream timeout deadline))
                       (values response-wire t))
                     :close-stream
                     (lambda (stream &key condition)
                       (declare (ignore stream condition))))))))
         (transport
           (http-kit/http3:make-http3-connection-manager-transport manager))
         (request
           (make-http-request :method "GET"
                              :uri "https://manager.example/transport")))
    (ensure-equal
     204
     (http-response-status
      (funcall transport request
               :timeout 1 :deadline 2
               :max-header-bytes 2048 :max-body-bytes 4096
               :proxy nil :proxy-plan :direct
               :on-body-chunk nil :on-information nil
               :collect-body-p t :unrelated-option :ignored)))
    (ensure-equal 1 opened)
    (ensure-equal 4 (http-kit/http3:http3-client-max-fields opened-client))
    (ensure-equal
     204
     (http-response-status
      (funcall transport request
               :request-body-function (lambda (maximum-size)
                                        (declare (ignore maximum-size))
                                        nil)
               :request-body-length 0)))
    (let ((information nil))
      (ensure-equal
       204
       (http-response-status
        (funcall transport request
                 :on-information
                 (lambda (response)
                   (push (list (http-response-status response)
                               (http-header-value
                                (http-response-headers response) "link")
                               (length (http-response-body response)))
                         information)))))
      (ensure-equal
       '((103 "</style.css>; rel=preload" 0) (199 nil 0))
       (nreverse information)))
    (signals http-protocol-error
      (funcall transport request :on-information :not-a-function))
    (signals http-protocol-error
      (funcall transport request :proxy "http://proxy.example"))
    (ensure-equal 1 opened)
    (ensure-equal
     204
     (http-response-status
      (funcall transport request :max-header-bytes 512)))
    (signals http-protocol-error
      (funcall transport request :max-header-bytes 1))
    (ensure-equal 1 opened)
    (signals http-size-limit-exceeded
      (funcall transport request :max-fields 1))
    (signals http-protocol-error
      (funcall transport request :max-fields 0))
    (http-kit/http3:close-http3-connection-manager manager)))

(deftest http3-client-streams-request-body-with-declared-length
  (let* ((response-wire
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-headers-type+
             :payload
             (http-kit/http3:qpack-encode-field-section
              (list (cons ":status" "204"))))))
         (stream (make-http3-test-stream :kind :request))
         (writes '())
         (producer-limits '())
         (chunks (list #(1 2) #(3) nil)))
         (client
           (http-kit/http3:make-http3-client
            :max-frame-size 4
            :open-stream
            (lambda (request &key stream-type timeout deadline)
              (declare (ignore request stream-type timeout deadline))
              (values stream 0))
            :write-stream
            (lambda (written-stream octets &key fin-p timeout deadline)
              (declare (ignore timeout deadline))
              (ensure-true (eq stream written-stream))
              (push (list (subseq octets 0) fin-p) writes))
            :read-stream
            (lambda (read-stream &key timeout deadline)
              (declare (ignore timeout deadline))
              (ensure-true (eq stream read-stream))
              (values response-wire t))
            :close-stream
            (lambda (closed-stream &key condition)
              (declare (ignore condition))
              (ensure-true (eq stream closed-stream))))))
    (ensure-equal
     204
     (http-response-status
      (http-kit/http3:send-http3-request
       client
       (make-http-request :method "POST" :uri "https://example.test/upload")
       :request-body-function
       (lambda (maximum-size)
         (push maximum-size producer-limits)
         (pop chunks))
       :request-body-length 3)))
    (ensure-equal '(4 4 4) (nreverse producer-limits))
    (let* ((ordered-writes (nreverse writes))
           (header-frame
             (first (http-kit/http3:decode-http3-frames
                     (first (first ordered-writes)))))
           (request-fields
             (http-kit/http3:qpack-decode-field-section
              (http-kit/http3:http3-frame-payload header-frame)))
           (first-data
             (first (http-kit/http3:decode-http3-frames
                     (first (second ordered-writes)))))
           (second-data
             (first (http-kit/http3:decode-http3-frames
                     (first (third ordered-writes)))))
           (final-data
             (first (http-kit/http3:decode-http3-frames
                     (first (fourth ordered-writes))))))
      (ensure-equal 4 (length ordered-writes))
      (ensure-equal "3" (cdr (assoc "content-length" request-fields
                                     :test #'string=)))
      (ensure-equal '(1 2)
                    (coerce (http-kit/http3:http3-frame-payload first-data)
                            'list))
      (ensure-equal '(3)
                    (coerce (http-kit/http3:http3-frame-payload second-data)
                            'list))
      (ensure-equal 0
                    (length (http-kit/http3:http3-frame-payload final-data)))
      (ensure-equal '(nil nil nil t) (mapcar #'second ordered-writes))))

(deftest http3-client-rejects-invalid-streaming-request-bodies
  (let ((opened 0))
    (labels ((open-stream (request &key stream-type timeout deadline)
               (declare (ignore request stream-type timeout deadline))
               (incf opened)
               (values (make-http3-test-stream :kind :request) 0))
             (write-stream (stream octets &key fin-p timeout deadline)
               (declare (ignore stream octets fin-p timeout deadline)))
             (read-stream (stream &key timeout deadline)
               (declare (ignore stream timeout deadline))
               (error "Invalid request bodies must not reach response reading.")))
      (let ((client
              (http-kit/http3:make-http3-client
               :max-frame-size 2
               :open-stream #'open-stream
               :write-stream #'write-stream
               :read-stream #'read-stream)))
        (signals http-protocol-error
          (http-kit/http3:send-http3-request
           client
           (make-http-request :method "POST" :uri "https://example.test/")
           :request-body-function :invalid))
        (signals http-protocol-error
          (http-kit/http3:send-http3-request
           client
           (make-http-request :method "POST" :uri "https://example.test/")
           :request-body-length 1))
        (signals http-protocol-error
          (http-kit/http3:send-http3-request
           client
           (make-http-request :method "POST" :uri "https://example.test/"
                              :body #(1))
           :request-body-function (lambda (maximum-size)
                                    (declare (ignore maximum-size))
                                    nil)))
        (ensure-equal 0 opened)
        (signals http-protocol-error
          (http-kit/http3:send-http3-request
           client
           (make-http-request :method "POST" :uri "https://example.test/")
           :request-body-function (lambda (maximum-size)
                                    (declare (ignore maximum-size))
                                    #(1))
           :request-body-length 2))
        (signals http-protocol-error
          (http-kit/http3:send-http3-request
           client
           (make-http-request :method "POST" :uri "https://example.test/")
           :request-body-function (lambda (maximum-size)
                                    (declare (ignore maximum-size))
                                    #(1 2 3))))
        (ensure-equal 2 opened)))))

(deftest http3-server-sends-informational-responses-before-final-response
  (let* ((request-wire
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-headers-type+
             :payload
             (http-kit/http3:qpack-encode-field-section
              (list (cons ":method" "GET")
                    (cons ":scheme" "https")
                    (cons ":authority" "example.test")
                    (cons ":path" "/hints"))))))
         (stream
           (make-http3-test-stream
            :kind :request :reads (list (list request-wire t))))
         (writes nil))
    (labels ((read-stream (ignored-stream &key timeout deadline)
               (declare (ignore ignored-stream timeout deadline))
               (let ((entry (pop (http3-test-stream-reads stream))))
                 (if entry
                     (values (first entry) (second entry))
                     (values nil t))))
             (write-stream (ignored-stream octets &key fin-p timeout deadline)
               (declare (ignore ignored-stream timeout deadline))
               (push (list (subseq octets 0) fin-p) writes)))
      (let ((response
              (http-kit/http3:serve-http3-request-stream
               stream
               (lambda (request)
                 (declare (ignore request))
                 (values
                  (make-http-response :status 204 :protocol-version "HTTP/3")
                  (list
                   (make-http-response
                    :status 103
                    :headers
                    (list (make-http-header
                           "link" "</style.css>; rel=preload"))
                    :protocol-version "HTTP/3"))))
               :read-stream #'read-stream
               :write-stream #'write-stream)))
        (ensure-equal 204 (http-response-status response)))
      (let* ((ordered-writes (nreverse writes))
             (information-frame
               (first (http-kit/http3:decode-http3-frames
                       (first (first ordered-writes)))))
             (final-frame
               (first (http-kit/http3:decode-http3-frames
                       (first (second ordered-writes)))))
             (information-fields
               (http-kit/http3:qpack-decode-field-section
                (http-kit/http3:http3-frame-payload information-frame)))
             (final-fields
               (http-kit/http3:qpack-decode-field-section
                (http-kit/http3:http3-frame-payload final-frame))))
        (ensure-equal 2 (length ordered-writes))
        (ensure-equal '(nil t) (mapcar #'second ordered-writes))
        (ensure-equal "103"
                      (cdr (assoc ":status" information-fields :test #'string=)))
        (ensure-equal "</style.css>; rel=preload"
                      (cdr (assoc "link" information-fields :test #'string=)))
        (ensure-equal "204"
                      (cdr (assoc ":status" final-fields :test #'string=)))))))

(deftest http3-server-rejects-invalid-informational-responses
  (let ((writes 0))
    (flet ((write-stream (stream octets &key fin-p timeout deadline)
             (declare (ignore stream octets fin-p timeout deadline))
             (incf writes)))
      (dolist (response
               (list
                (make-http-response :status 101 :protocol-version "HTTP/3")
                (make-http-response :status 103 :body #(1)
                                    :protocol-version "HTTP/3")
                (make-http-response
                 :status 103
                 :trailers (list (make-http-header "x-late" "no"))
                 :protocol-version "HTTP/3")
                (make-http-response
                 :status 103
                 :headers (list (make-http-header "content-length" "0"))
                 :protocol-version "HTTP/3")))
        (signals http-protocol-error
          (http-kit/http3::%h3-server-send-information
           :stream #'write-stream response 16384 65536 nil nil nil nil)))
      (ensure-equal 0 writes))))
