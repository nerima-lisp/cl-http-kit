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
(defun http3-test-qpack-headers (fields)
  (mapcar
   (lambda (field)
     (if (http-message-kit:http-header-p field)
         field
         (let ((name (car field))
               (value (cdr field)))
           (if (and (plusp (length name))
                    (char= (char name 0) #\:))
               (http-message-kit:make-http-pseudo-header name value)
               (http-message-kit:make-http-header name value)))))
   fields))
(defun http3-test-qpack-fields (headers)
  (mapcar
   (lambda (header)
     (cons (http-message-kit:http-header-name header)
           (http-message-kit:http-header-content header)))
   headers))
(defun http3-test-qpack-encode (fields &rest options)
  (multiple-value-bind (octets transaction)
      (apply (function http-message-kit:qpack-prepare-header-block)
             (http3-test-qpack-headers fields)
             options)
    (when transaction
      (http-message-kit:qpack-commit-encoding transaction))
    octets))
(progn
  (defun http3-test-qpack-encode-set-capacity (capacity)
    (http-message-kit:qpack-encode-set-capacity
     (http-message-kit:make-qpack-context
      :max-table-capacity #x3fffffffffffffff
      :table-capacity 0
      :huffman nil)
     capacity))
  (defun http3-test-qpack-encode-insert-with-literal-name (name value)
    (http-message-kit:qpack-encode-insert-with-literal-name
     (http-message-kit:make-qpack-context
      :max-table-capacity 128
      :table-capacity 128
      :huffman nil)
     name value))
  (defun http3-test-qpack-encode-insert-with-name-reference (name-index value)
    (http-message-kit:qpack-encode-insert-with-name-reference
     (http-message-kit:make-qpack-context
      :max-table-capacity 128
      :table-capacity 128
      :huffman nil)
     name-index value :static-p t))
  (defun http3-test-qpack-encode-duplicate (relative-index)
    (let ((context
            (http-message-kit:make-qpack-context
             :max-table-capacity 128
             :table-capacity 128
             :huffman nil)))
      (http-message-kit:qpack-encode-insert-with-literal-name
       context "x" "y")
      (http-message-kit:qpack-encode-duplicate context relative-index))))
(defun http3-test-qpack-decode (octets &rest options)
  (multiple-value-bind (headers consumed required base)
      (apply (function http-message-kit:qpack-decode-header-block)
             octets options)
    (values (http3-test-qpack-fields headers) consumed required base)))

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

(deftest http3-qpack-dynamic-table-and-streams
  (let* ((encoder-context
 (http-message-kit:make-qpack-context
  :max-table-capacity 256 :table-capacity 0
  :max-blocked-streams 1 :huffman nil))
         (decoder-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :huffman nil))
         (encoder-stream
           (http3-test-concat-octets
            (http-message-kit:qpack-encode-set-capacity encoder-context 256)
            (http-message-kit:qpack-encode-insert-with-literal-name
             encoder-context "x-request-id" "one" :huffman nil)
            (http-message-kit:qpack-encode-insert-with-name-reference
             encoder-context 0 "two" :static-p nil :huffman nil)
            (http-message-kit:qpack-encode-duplicate encoder-context 0)
            (http-message-kit:qpack-encode-insert-with-name-reference
             encoder-context 17 "PATCH" :huffman nil)))
         (fields (list (cons ":method" "PATCH")
                       (cons ":scheme" "https")
                       (cons ":authority" "example.com")
                       (cons ":path" "/")
                       (cons "x-request-id" "two")))
         (headers (list (http-message-kit:make-http-pseudo-header
                        ":method" "PATCH")
                       (http-message-kit:make-http-pseudo-header
                        ":scheme" "https")
                       (http-message-kit:make-http-pseudo-header
                        ":authority" "example.com")
                       (http-message-kit:make-http-pseudo-header
                        ":path" "/")
                       (http-message-kit:make-http-header
                        "x-request-id" "two"))))
    (multiple-value-bind (events consumed)
        (http-message-kit:qpack-decode-encoder-stream
         decoder-context encoder-stream)
      (ensure-equal 5 (length events))
      (ensure-equal (length encoder-stream) consumed))
    (ensure-equal 4
                  (http-message-kit:qpack-context-insert-count
                   encoder-context))
    (ensure-equal 4
                  (http-message-kit:qpack-context-dynamic-entry-count
                   encoder-context))
    (let* ((encoded
             (http-message-kit:qpack-encode-header-block
              headers :context encoder-context :use-dynamic-p t
              :stream-id 0 :huffman nil))
           (decoded
             (multiple-value-bind (decoded consumed required-insert-count base)
                 (http-message-kit:qpack-decode-header-block
                  encoded :context decoder-context :stream-id 0)
               (declare (ignore consumed required-insert-count base))
               (mapcar
                (lambda (header)
                  (cons (http-message-kit:http-header-name header)
                        (http-message-kit:http-header-content header)))
                decoded)))
           (blocked-context
             (http-message-kit:make-qpack-context
              :max-table-capacity 256 :table-capacity 256 :huffman nil))
           (blocked-adapter
             (http-message-kit:qpack-make-http3-decoder-context
              blocked-context :max-blocked-streams 1))
           (blocked
             (http-message-kit:qpack-decode-http3-field-section
              encoded blocked-adapter 0 :role :request)))
      (ensure-equal fields decoded)
      (ensure-true (http-message-kit:qpack-blocked-stream-p blocked))
      (ensure-equal 0
                    (http-message-kit:qpack-context-insert-count
                     blocked-context))))
  (let ((context
          (http-message-kit:make-qpack-context
 :max-table-capacity 64 :table-capacity 64
 :max-blocked-streams 1 :huffman nil)))
    (http-message-kit:qpack-encode-insert-with-literal-name
     context "a" "b" :huffman nil)
    (http-message-kit:qpack-encode-insert-with-literal-name
     context "c" "d" :huffman nil)
    (ensure-equal 1
                  (http-message-kit:qpack-context-dynamic-entry-count context))
    (ensure-equal 34 (http-message-kit:qpack-context-table-size context))
    (let* ((wire
             (http-message-kit:qpack-encode-header-block
              (list (http-message-kit:make-http-header "c" "d"))
              :context context :use-dynamic-p t :stream-id 1 :huffman nil))
           (decoded
             (nth-value 0
                       (http-message-kit:qpack-decode-header-block
                        wire :context context :stream-id 1))))
      (ensure-equal "c"
                    (http-message-kit:http-header-name (first decoded)))
      (ensure-equal "d"
                    (http-message-kit:http-header-content (first decoded))))
    (signals http-protocol-error
      (http-message-kit:qpack-encode-insert-with-literal-name
       context "this-name-is-definitely-too-large-for-the-table"
       "value" :huffman nil)))
  (let ((context
          (http-message-kit:make-qpack-context
           :max-table-capacity 175 :table-capacity 175
           :max-blocked-streams 4 :huffman nil)))
    (http-message-kit:qpack-encode-insert-with-literal-name
     context "x-stream" "one" :huffman nil)
    (dolist (stream-id '(7 9))
      (http-message-kit:qpack-encode-header-block
       (list (http-message-kit:make-http-header "x-stream" "one"))
       :context context :use-dynamic-p t :stream-id stream-id :huffman nil))
    (dolist (value '("two" "three" "four"))
      (http-message-kit:qpack-encode-insert-with-literal-name
       context "x-stream" value :huffman nil))
    (multiple-value-bind (events consumed)
        (http-message-kit:qpack-decode-decoder-stream
         context
         (http3-test-concat-octets
          (http-message-kit:qpack-encode-section-ack 7)
          (http-message-kit:qpack-encode-stream-cancellation 9)
          (http-message-kit:qpack-encode-insert-count-increment 3)))
      (ensure-equal '((:section-ack 7)
                      (:stream-cancellation 9)
                      (:insert-count-increment 3))
                    events)
      (ensure-equal 3 consumed))))
(deftest http3-qpack-static-field-sections
  (let* ((fields (list (cons ":method" "GET")
                       (cons ":scheme" "https")
                       (cons ":authority" "example.test")
                       (cons ":path" "/resource")
                       (cons "accept" "text/plain")))
         (headers (list (http-message-kit:make-http-pseudo-header ":method" "GET")
                        (http-message-kit:make-http-pseudo-header ":scheme" "https")
                        (http-message-kit:make-http-pseudo-header ":authority" "example.test")
                        (http-message-kit:make-http-pseudo-header ":path" "/resource")
                        (http-message-kit:make-http-header "accept" "text/plain")))
         (encoded (http-message-kit:qpack-encode-header-block
                   headers :huffman nil)))
    (multiple-value-bind (decoded consumed)
        (http-message-kit:qpack-decode-header-block encoded)
      (ensure-equal fields
                    (mapcar (lambda (header)
                              (cons (http-message-kit:http-header-name header)
                                    (http-message-kit:http-header-content header)))
                            decoded))
      (ensure-equal (length encoded) consumed))
    (signals http-protocol-error
      (http-message-kit:qpack-decode-header-block #(#x01 #x00)))
    (signals http-size-limit-exceeded
      (http-message-kit:qpack-decode-header-block
       (http-message-kit:qpack-encode-header-block
        (list (http-message-kit:make-http-header "x-test" "oversized"))
        :huffman nil)
       :max-header-bytes 1))))

(deftest http3-qpack-stream-fragmentation
  (let* ((peer-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :huffman nil))
         (decoder-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :huffman nil))
         (encoder-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0
            :max-blocked-streams 1 :huffman nil))
         (adapter
           (http-message-kit:qpack-make-http3-decoder-context
            decoder-context :max-blocked-streams 1))
         (capacity-wire nil)
         (insert-wire nil)
         (next-wire nil)
         (last-wire nil)
         (encoder-stream nil)
         (decoder-stream nil)
         (client nil))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-set-capacity peer-context 256)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf capacity-wire wire))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-insert-with-literal-name
         peer-context "x-fragmented" "fragmented-value" :huffman nil)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf insert-wire wire))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-insert-with-literal-name
         peer-context "x-next" "next-value" :huffman nil)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf next-wire wire))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-insert-with-literal-name
         peer-context "x-last" "last-value" :huffman nil)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf last-wire wire))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-set-capacity encoder-context 256)
      (declare (ignore wire))
      (http-message-kit:qpack-commit-encoding transaction))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-insert-with-literal-name
         encoder-context "x-local" "value" :huffman nil)
      (declare (ignore wire))
      (http-message-kit:qpack-commit-encoding transaction))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-header-block
         (list (http-message-kit:make-http-header "x-local" "value"))
         :context encoder-context :use-dynamic-p t :huffman nil :stream-id 200)
      (declare (ignore wire))
      (http-message-kit:qpack-commit-encoding transaction))
    (setf encoder-stream
          (make-http3-test-stream
           :kind :peer-qpack-encoder
           :reads (list
                   (list (http3-test-concat-octets
                          (http-kit/http3:http3-qpack-encoder-stream-prefix)
                          capacity-wire
                          (subseq insert-wire 0 (1- (length insert-wire))))
                         nil)
                   (list (subseq insert-wire (1- (length insert-wire))) nil)
                   (list (http3-test-concat-octets
                          next-wire
                          (subseq last-wire 0 (1- (length last-wire))))
                         nil)
                   (list (subseq last-wire (1- (length last-wire))) nil)))
          decoder-stream
          (make-http3-test-stream
           :kind :peer-qpack-decoder
           :reads (list
                   (list (http3-test-concat-octets
                          (http-kit/http3:http3-qpack-decoder-stream-prefix)
                          (subseq (http-message-kit:qpack-encode-section-ack 200)
                                  0 1))
                         nil)
                   (list (subseq (http-message-kit:qpack-encode-section-ack 200)
                                 1)
                         nil))))
    (setf client
          (http-kit/http3::%make-http3-client
           :open-p t
           :read-stream
           (lambda (stream &key timeout deadline)
             (declare (ignore timeout deadline))
             (let ((entry (pop (http3-test-stream-reads stream))))
               (values (first entry) (second entry))))
           :write-stream
           (lambda (stream bytes &key fin-p timeout deadline)
             (declare (ignore stream bytes fin-p timeout deadline)))
           :close-stream
           (lambda (stream &key condition)
             (declare (ignore stream condition)))
           :qpack-encoder-context encoder-context
           :qpack-decoder-context decoder-context
           :qpack-http3-decoder-context adapter))
    (http-kit/http3:attach-http3-peer-qpack-encoder-stream client encoder-stream)
    (multiple-value-bind (events ended-p)
        (http-kit/http3:read-http3-qpack-encoder-stream client)
      (ensure-equal '((:set-capacity 256)) events)
      (ensure-equal nil ended-p))
    (multiple-value-bind (events ended-p)
        (http-kit/http3:read-http3-qpack-encoder-stream client)
      (ensure-equal 1 (length events))
      (ensure-equal :insert (first (first events)))
      (ensure-equal "x-fragmented" (second (first events)))
      (ensure-equal "fragmented-value" (third (first events)))
      (ensure-equal nil ended-p))
    (multiple-value-bind (events ended-p)
        (http-kit/http3:read-http3-qpack-encoder-stream client)
      (ensure-equal 1 (length events))
      (ensure-equal "x-next" (second (first events)))
      (ensure-equal "next-value" (third (first events)))
      (ensure-equal nil ended-p))
    (multiple-value-bind (events ended-p)
        (http-kit/http3:read-http3-qpack-encoder-stream client)
      (ensure-equal 1 (length events))
      (ensure-equal "x-last" (second (first events)))
      (ensure-equal "last-value" (third (first events)))
      (ensure-equal nil ended-p))
    (http-kit/http3:attach-http3-peer-qpack-decoder-stream client decoder-stream)
    (multiple-value-bind (events ended-p)
        (http-kit/http3:read-http3-qpack-decoder-stream client)
      (ensure-equal '() events)
      (ensure-equal nil ended-p))
    (multiple-value-bind (events ended-p)
        (http-kit/http3:read-http3-qpack-decoder-stream client)
      (ensure-equal '((:section-ack 200)) events)
      (ensure-equal nil ended-p))
    (http-kit/http3:close-http3-client client)))

(deftest http3-qpack-critical-stream-lifecycle
  (let* ((decoder-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :huffman nil))
         (peer-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :huffman nil))
         (encoder-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0
            :max-blocked-streams 1 :huffman nil))
         (expected-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :huffman nil))
         (adapter
           (http-message-kit:qpack-make-http3-decoder-context
            decoder-context :max-blocked-streams 1))
         (capacity-bytes nil)
         (insert-bytes nil)
         (peer-instructions nil)
         (peer-wire nil)
         (peer-stream nil)
         (decoder-stream (make-http3-test-stream :kind :peer-qpack-decoder))
         (local-encoder-stream (make-http3-test-stream :kind :local-qpack-encoder))
         (encoder-writes '())
         (client nil))
    (multiple-value-bind (bytes transaction)
        (http-message-kit:qpack-prepare-set-capacity peer-context 256)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf peer-instructions bytes))
    (multiple-value-bind (bytes transaction)
        (http-message-kit:qpack-prepare-insert-with-literal-name
         peer-context "x-peer" "yes" :huffman nil)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf peer-instructions
            (http3-test-concat-octets peer-instructions bytes)))
    (multiple-value-bind (bytes transaction)
        (http-message-kit:qpack-prepare-set-capacity expected-context 256)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf capacity-bytes bytes))
    (multiple-value-bind (bytes transaction)
        (http-message-kit:qpack-prepare-insert-with-literal-name
         expected-context "x-local" "yes" :huffman nil)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf insert-bytes bytes))
    (setf peer-wire
          (http3-test-concat-octets
           (http-kit/http3:http3-qpack-encoder-stream-prefix)
           peer-instructions)
          peer-stream
          (make-http3-test-stream
           :kind :peer-qpack-encoder
           :reads (list (list (subseq peer-wire 0 (1- (length peer-wire))) nil)
                        (list (subseq peer-wire (1- (length peer-wire))) nil)))
          client
          (http-kit/http3::%make-http3-client
           :open-p t
           :read-stream
           (lambda (stream &key timeout deadline)
             (declare (ignore timeout deadline))
             (let ((entry (pop (http3-test-stream-reads stream))))
               (values (first entry) (second entry))))
           :write-stream
           (lambda (stream octets &key fin-p timeout deadline)
             (declare (ignore timeout deadline))
             (push (list stream octets fin-p) encoder-writes))
           :qpack-encoder-stream local-encoder-stream
           :qpack-encoder-context encoder-context
           :qpack-decoder-context decoder-context
           :qpack-http3-decoder-context adapter))
    (http-kit/http3:attach-http3-peer-qpack-encoder-stream client peer-stream)
    (signals http-protocol-error
      (http-kit/http3:attach-http3-peer-qpack-encoder-stream
       client (make-http3-test-stream :kind :duplicate)))
    (multiple-value-bind (events ended-p)
        (http-kit/http3:read-http3-qpack-encoder-stream client)
      (ensure-equal '((:set-capacity 256)) events)
      (ensure-equal nil ended-p)
      (ensure-equal 0 (http-message-kit:qpack-context-insert-count decoder-context)))
    (multiple-value-bind (events ended-p)
        (http-kit/http3:read-http3-qpack-encoder-stream client)
      (ensure-equal 1 (length events))
      (ensure-equal :insert (first (first events)))
      (ensure-equal "x-peer" (second (first events)))
      (ensure-equal "yes" (third (first events)))
      (ensure-equal nil ended-p)
      (ensure-equal 1 (http-message-kit:qpack-context-insert-count decoder-context)))
    (http-kit/http3:http3-client-set-qpack-capacity client 256)
    (ensure-equal 1
                  (http-kit/http3:http3-client-insert-qpack-field
                   client "x-local" "yes"))
    (ensure-equal 2 (length encoder-writes))
    (ensure-equal local-encoder-stream (first (second encoder-writes)))
    (ensure-equal (coerce capacity-bytes 'list)
                  (coerce (second (second encoder-writes)) 'list))
    (ensure-equal nil (third (second encoder-writes)))
    (ensure-equal local-encoder-stream (first (first encoder-writes)))
    (ensure-equal (coerce insert-bytes 'list)
                  (coerce (second (first encoder-writes)) 'list))
    (ensure-equal nil (third (first encoder-writes)))
    (ensure-equal 256 (http-message-kit:qpack-context-table-capacity encoder-context))
    (ensure-equal 1 (http-message-kit:qpack-context-insert-count encoder-context))
    (multiple-value-bind (section transaction)
        (http-message-kit:qpack-prepare-header-block
         (list (http-message-kit:make-http-header "x-local" "yes"))
         :context encoder-context :use-dynamic-p t :huffman nil :stream-id 200)
      (declare (ignore section))
      (http-message-kit:qpack-commit-encoding transaction))
    (let* ((ack (http-message-kit:qpack-encode-section-ack 200))
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
        (ensure-equal '((:section-ack 200)) events)
        (ensure-equal nil ended-p)))
    (setf (http3-test-stream-reads peer-stream) (list (list nil t)))
    (handler-case
        (progn
          (http-kit/http3:read-http3-qpack-encoder-stream client)
          (error "Expected QPACK critical-stream FIN to fail."))
      (http-protocol-error (condition)
        (ensure-equal :h3-closed-critical-stream
                      (http-protocol-error-detail condition))))))

(deftest http3-qpack-peer-encoder-application-is-serialized
  (let* ((peer-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :huffman nil))
         (encoder-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :huffman nil))
         (decoder-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :huffman nil))
         (adapter
           (http-message-kit:qpack-make-http3-decoder-context
            decoder-context :max-blocked-streams 1))
         (instructions nil)
         (wire nil)
         (stream (make-http3-test-stream :kind :peer-qpack-encoder))
         (inside-p nil)
         (events '())
         (client nil))
    (multiple-value-bind (bytes transaction)
        (http-message-kit:qpack-prepare-set-capacity peer-context 256)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf instructions bytes))
    (multiple-value-bind (bytes transaction)
        (http-message-kit:qpack-prepare-insert-with-literal-name
         peer-context "x-peer" "yes" :huffman nil)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf instructions (http3-test-concat-octets instructions bytes)))
    (setf wire
          (http3-test-concat-octets
           (http-kit/http3:http3-qpack-encoder-stream-prefix)
           instructions)
          (http3-test-stream-reads stream) (list (list wire nil))
          client
          (http-kit/http3::%make-http3-client
           :open-p t
           :serialize
           (lambda (thunk)
             (push (list :enter
                         (http-message-kit:qpack-context-insert-count
                          decoder-context))
                   events)
             (let ((previous-inside-p inside-p))
               (setf inside-p t)
               (unwind-protect
                    (multiple-value-prog1 (funcall thunk)
                      (push (list :leave
                                  (http-message-kit:qpack-context-insert-count
                                   decoder-context))
                            events))
                 (setf inside-p previous-inside-p))))
           :read-stream
           (lambda (actual &key timeout deadline)
             (declare (ignore timeout deadline))
             (push (list :read inside-p) events)
             (let ((entry (pop (http3-test-stream-reads actual))))
               (values (first entry) (second entry))))
           :qpack-encoder-context encoder-context
           :qpack-decoder-context decoder-context
           :qpack-http3-decoder-context adapter))
    (http-kit/http3:attach-http3-peer-qpack-encoder-stream client stream)
    (multiple-value-bind (application-events ended-p)
        (http-kit/http3:read-http3-qpack-encoder-stream client)
      (ensure-equal '(:set-capacity :insert) (mapcar #'first application-events))
      (ensure-equal nil ended-p))
    (ensure-equal '((:enter 0)
                    (:leave 0)
                    (:read nil)
                    (:enter 0)
                    (:leave 1))
                  (nreverse events))
    (ensure-equal 1 (http-message-kit:qpack-context-insert-count decoder-context))))

(deftest http3-qpack-local-write-failure-preserves-state
  (let* ((capacity-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :huffman nil))
         (capacity-client
           (http-kit/http3::%make-http3-client
            :write-stream
            (lambda (stream octets &key fin-p timeout deadline)
              (declare (ignore stream octets fin-p timeout deadline))
              (error "QPACK capacity write failed"))
            :open-p t
            :qpack-encoder-stream (make-http3-test-stream :kind :local-qpack-encoder)
            :qpack-encoder-context capacity-context)))
    (signals simple-error
      (http-kit/http3:http3-client-set-qpack-capacity capacity-client 256))
    (ensure-equal 0
                  (http-message-kit:qpack-context-table-capacity capacity-context))
    (ensure-equal 0
                  (http-message-kit:qpack-context-insert-count capacity-context))
    (ensure-equal nil (http-kit/http3:http3-client-open-p capacity-client)))
  (let* ((insert-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 256 :huffman nil))
         (insert-client
           (http-kit/http3::%make-http3-client
            :write-stream
            (lambda (stream octets &key fin-p timeout deadline)
              (declare (ignore stream octets fin-p timeout deadline))
              (error "QPACK insert write failed"))
            :open-p t
            :qpack-encoder-stream (make-http3-test-stream :kind :local-qpack-encoder)
            :qpack-encoder-context insert-context)))
    (signals simple-error
      (http-kit/http3:http3-client-insert-qpack-field
       insert-client "x-local" "yes"))
    (ensure-equal 0 (http-message-kit:qpack-context-insert-count insert-context))
    (ensure-equal 0 (http-message-kit:qpack-context-table-size insert-context))
    (ensure-equal nil (http-kit/http3:http3-client-open-p insert-client))))

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
  (let* ((context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :huffman nil))
         (inside-p nil)
         (events '())
         (client
           (http-kit/http3::%make-http3-client
            :serialize
            (lambda (thunk)
              (push :enter events)
              (setf inside-p t)
              (unwind-protect
                   (multiple-value-prog1 (funcall thunk)
                     (push :leave events))
                (setf inside-p nil)))
            :write-stream
            (lambda (stream octets &key fin-p timeout deadline)
              (declare (ignore stream octets fin-p timeout deadline))
              (ensure-true inside-p)
              (push (list :write
                          (http-message-kit:qpack-context-table-capacity context)
                          (http-message-kit:qpack-context-insert-count context))
                    events))
            :open-p t
            :qpack-encoder-stream
            (make-http3-test-stream :kind :local-qpack-encoder)
            :qpack-encoder-context context)))
    (http-kit/http3:http3-client-set-qpack-capacity client 256)
    (http-kit/http3:http3-client-insert-qpack-field client "x-local" "yes")
    (ensure-equal
     '(:enter (:write 0 0) :leave
       :enter (:write 256 0) :leave)
     (nreverse events))
    (ensure-equal 1 (http-message-kit:qpack-context-insert-count context))))

(deftest http3-qpack-request-header-write-failure-does-not-register-section
  (let* ((encoder-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0
            :max-blocked-streams 1 :huffman nil))
         (decoder-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :huffman nil))
         (request-stream (make-http3-test-stream :kind :request))
         (encoder-instructions nil)
         (writes nil)
         (inside-p nil)
         (open-outside-p nil)
         (close-outside-p nil))
    (multiple-value-bind (capacity-instruction transaction)
        (http-message-kit:qpack-prepare-set-capacity encoder-context 256)
      (setf encoder-instructions capacity-instruction)
      (http-message-kit:qpack-commit-encoding transaction))
    (multiple-value-bind (instruction transaction)
        (http-message-kit:qpack-prepare-insert-with-literal-name
         encoder-context "x-local" "yes" :huffman nil)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf encoder-instructions
            (http3-test-concat-octets encoder-instructions instruction)))
    (http-message-kit:qpack-decode-encoder-stream
     decoder-context encoder-instructions)
    (let ((client
            (http-kit/http3::%make-http3-client
             :serialize
             (lambda (thunk)
               (setf inside-p t)
               (unwind-protect
                    (funcall thunk)
                 (setf inside-p nil)))
             :open-stream
             (lambda (request &key stream-type timeout deadline)
               (declare (ignore request stream-type timeout deadline))
               (setf open-outside-p (not inside-p))
               (values request-stream 4))
             :write-stream
             (lambda (stream octets &key fin-p timeout deadline)
               (declare (ignore stream fin-p timeout deadline))
               (ensure-true inside-p)
               (push (copy-seq octets) writes)
               (error "HEADERS write failed"))
             :close-stream
             (lambda (stream &key condition)
               (declare (ignore stream condition))
               (setf close-outside-p (not inside-p)))
             :open-p t
             :qpack-encoder-context encoder-context
             :qpack-decoder-context
             nil
             :peer-control-state
             (http-kit/http3:make-http3-control-state :peer-role :server))))
      (signals simple-error
        (http-kit/http3:send-http3-request
         client
         (make-http-request
          :method "GET" :uri "https://example.test/"
          :headers (list (make-http-header "x-local" "yes")))))
      (ensure-true open-outside-p)
      (ensure-true close-outside-p)
      (ensure-equal 1 (length writes))
      (multiple-value-bind (frames remainder)
          (http-kit/http3:decode-http3-frames (first writes))
        (ensure-equal (octets) remainder)
        (ensure-equal 1 (length frames))
        (let ((payload (http-kit/http3:http3-frame-payload (first frames))))
          (multiple-value-bind (headers consumed required-insert-count base)
              (http-message-kit:qpack-decode-header-block (progn (ensure-equal (quote (unsigned-byte 8)) (array-element-type payload) "HTTP/3 field sections are binary octet vectors") payload) :context decoder-context)
            (declare (ignore base))
            (ensure-equal (length payload) consumed)
            (ensure-equal 1 required-insert-count)
            (ensure-equal '((":method" . "GET") (":scheme" . "https") (":authority" . "example.test") (":path" . "/") ("x-local" . "yes"))
                          (mapcar (lambda (header)
                                    (cons (http-message-kit:http-header-name header)
                                          (http-message-kit:http-header-content header)))
                                  headers)))))
      (ensure-equal 1 (http-message-kit:qpack-context-insert-count encoder-context))
      (http-message-kit:qpack-context-set-capacity encoder-context 0)
      (ensure-equal 0 (http-message-kit:qpack-context-table-capacity encoder-context))
      (ensure-equal 0 (http-message-kit:qpack-context-dynamic-entry-count encoder-context)))))

(deftest http3-qpack-decoder-stream-state-validation
  (let ((context
          (http-message-kit:make-qpack-context
           :max-table-capacity 256 :table-capacity 256
           :max-blocked-streams 3 :huffman nil)))
    (http-message-kit:qpack-encode-insert-with-literal-name
     context "x-first" "one" :huffman nil)
    (http-message-kit:qpack-encode-header-block
     (list (http-message-kit:make-http-header "x-first" "one"))
     :context context :use-dynamic-p t :stream-id 4 :huffman nil)
    (http-message-kit:qpack-encode-insert-with-literal-name
     context "x-second" "two" :huffman nil)
    (http-message-kit:qpack-encode-header-block
     (list (http-message-kit:make-http-header "x-second" "two"))
     :context context :use-dynamic-p t :stream-id 4 :huffman nil)
    (http-message-kit:qpack-encode-header-block
     (list (http-message-kit:make-http-header "x-second" "two"))
     :context context :use-dynamic-p t :stream-id 8 :huffman nil)
    (http-message-kit:qpack-decode-decoder-stream
     context (http-message-kit:qpack-encode-section-ack 4))
    (ensure-equal 1
                  (http-message-kit:qpack-context-known-received-count context))
    (http-message-kit:qpack-decode-decoder-stream
     context (http-message-kit:qpack-encode-section-ack 4))
    (ensure-equal 2
                  (http-message-kit:qpack-context-known-received-count context))
    (signals http-protocol-error
      (http-message-kit:qpack-decode-decoder-stream
       context (http-message-kit:qpack-encode-section-ack 4)))
    (multiple-value-bind (events consumed)
        (http-message-kit:qpack-decode-decoder-stream
         context (http-message-kit:qpack-encode-stream-cancellation 8))
      (ensure-equal '((:stream-cancellation 8)) events)
      (ensure-equal 1 consumed))
    (multiple-value-bind (events consumed)
        (http-message-kit:qpack-decode-decoder-stream
         context (http-message-kit:qpack-encode-stream-cancellation 8))
      (ensure-equal '((:stream-cancellation 8)) events)
      (ensure-equal 1 consumed))
    (signals http-protocol-error
      (http-message-kit:qpack-decode-decoder-stream
       context (http-message-kit:qpack-encode-insert-count-increment 1))))
  (let ((context
          (http-message-kit:make-qpack-context
           :max-table-capacity 128 :table-capacity 128
           :max-blocked-streams 1 :huffman nil)))
    (http-message-kit:qpack-encode-insert-with-literal-name
     context "x" "y" :huffman nil)
    (http-message-kit:qpack-decode-decoder-stream
     context (http-message-kit:qpack-encode-insert-count-increment 1))
    (ensure-equal 1
                  (http-message-kit:qpack-context-known-received-count context))
    (signals http-protocol-error
      (http-message-kit:qpack-decode-decoder-stream
       context (http-message-kit:qpack-encode-insert-count-increment 1))))
  (let ((context
          (http-message-kit:make-qpack-context
           :max-table-capacity 90 :table-capacity 90
           :max-blocked-streams 1 :huffman nil)))
    (http-message-kit:qpack-encode-insert-with-literal-name
     context "x-first" "one" :huffman nil)
    (http-message-kit:qpack-encode-insert-with-literal-name
     context "x-second" "two" :huffman nil)
    (http-message-kit:qpack-encode-header-block
     (list (http-message-kit:make-http-header "x-first" "one"))
     :context context :use-dynamic-p t :stream-id 12 :huffman nil)
    (signals http-protocol-error
      (http-message-kit:qpack-encode-insert-with-literal-name
       context "x-third" "three" :huffman nil))
    (ensure-equal 2 (http-message-kit:qpack-context-insert-count context))
    (signals http-protocol-error
      (http-message-kit:qpack-context-set-capacity context 41))
    (ensure-equal 90 (http-message-kit:qpack-context-table-capacity context))
    (http-message-kit:qpack-decode-decoder-stream
     context (http-message-kit:qpack-encode-section-ack 12))
    (http-message-kit:qpack-encode-insert-with-literal-name
     context "x-third" "three" :huffman nil)
    (ensure-equal 3 (http-message-kit:qpack-context-insert-count context))))

(deftest http3-qpack-huffman-encoding
  (let* ((huffman-value
           (octets #xf1 #xe3 #xc2 #xe5 #xf2 #x3a
                   #x6b #xa0 #xab #x90 #xf4 #xff))
         (fields
           (list (http-message-kit:make-http-header
                  "x-huffman" "www.example.com")
                 (http-message-kit:make-http-header "accept" "text/plain")))
         (wire
           (http-message-kit:qpack-prepare-header-block
            fields :huffman t)))
    (ensure-true (search huffman-value wire))
    (multiple-value-bind (decoded consumed required-insert-count base)
        (http-message-kit:qpack-decode-header-block wire)
      (declare (ignore required-insert-count base))
      (ensure-equal 2 (length decoded))
      (ensure-equal "x-huffman"
                    (http-message-kit:http-header-name (first decoded)))
      (ensure-equal "www.example.com"
                    (http-message-kit:http-header-content (first decoded)))
      (ensure-equal "accept"
                    (http-message-kit:http-header-name (second decoded)))
      (ensure-equal "text/plain"
                    (http-message-kit:http-header-content (second decoded)))
      (ensure-equal (length wire) consumed)))
  (let* ((encoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :huffman nil))
         (decoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :huffman nil))
         (huffman-value
           (octets #xf1 #xe3 #xc2 #xe5 #xf2 #x3a
                   #x6b #xa0 #xab #x90 #xf4 #xff)))
    (multiple-value-bind (instruction transaction)
        (http-message-kit:qpack-prepare-set-capacity encoder 256)
      (http-message-kit:qpack-commit-encoding transaction)
      (http-message-kit:qpack-decode-encoder-stream decoder instruction))
    (multiple-value-bind (instruction transaction)
        (http-message-kit:qpack-prepare-insert-with-literal-name
         encoder "x-huffman" "www.example.com" :huffman t)
      (ensure-true (search huffman-value instruction))
      (http-message-kit:qpack-commit-encoding transaction)
      (http-message-kit:qpack-decode-encoder-stream decoder instruction))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-header-block
         (list (http-message-kit:make-http-header
                "x-huffman" "www.example.com"))
         :context encoder :use-dynamic-p t :huffman t :stream-id 4)
      (http-message-kit:qpack-abort-encoding transaction)
      (multiple-value-bind (decoded consumed required-insert-count base)
          (http-message-kit:qpack-decode-header-block
           wire :context decoder :stream-id 4)
        (declare (ignore required-insert-count base))
        (ensure-equal 1 (length decoded))
        (ensure-equal "x-huffman"
                      (http-message-kit:http-header-name (first decoded)))
        (ensure-equal "www.example.com"
                      (http-message-kit:http-header-content (first decoded)))
        (ensure-equal (length wire) consumed)))))


(defun http3-test-qpack-blocked-fixture ()
  (let* ((fields (list (cons "x-blocked" "ready")))
         (encoder-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0
            :max-blocked-streams 1 :huffman nil))
         (decoder-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0
            :max-blocked-streams 0 :huffman nil))
         (headers
           (list (http-message-kit:make-http-header
                  "x-blocked" "ready")))
         (capacity-wire
           (multiple-value-bind (wire transaction)
               (http-message-kit:qpack-prepare-set-capacity
                encoder-context 256)
             (http-message-kit:qpack-commit-encoding transaction)
             wire))
         (instruction
           (multiple-value-bind (wire transaction)
               (http-message-kit:qpack-prepare-insert-with-literal-name
                encoder-context "x-blocked" "ready" :huffman nil)
             (http-message-kit:qpack-commit-encoding transaction)
             (http3-test-concat-octets capacity-wire wire)))
         (section
           (multiple-value-bind (wire transaction)
               (http-message-kit:qpack-prepare-header-block
                headers :context encoder-context :use-dynamic-p t
                :stream-id 0 :huffman nil)
             (http-message-kit:qpack-commit-encoding transaction)
             wire)))
    (values fields decoder-context section instruction)))

(defun http3-test-qpack-blocked-section (fields)
  (let* ((encoder-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0
            :max-blocked-streams 1 :huffman nil))
         (decoder-context
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0
            :max-blocked-streams 0 :huffman nil))
         (headers
           (mapcar
            (lambda (field)
              (if (char= #\: (char (car field) 0))
                  (http-message-kit:make-http-pseudo-header
                   (car field) (cdr field))
                  (http-message-kit:make-http-header
                   (car field) (cdr field))))
            fields))
         (capacity-wire
           (multiple-value-bind (wire transaction)
               (http-message-kit:qpack-prepare-set-capacity
                encoder-context 256)
             (http-message-kit:qpack-commit-encoding transaction)
             wire))
         (instruction
           (multiple-value-bind (wire transaction)
               (http-message-kit:qpack-prepare-insert-with-literal-name
                encoder-context "x-blocked" "ready" :huffman nil)
             (http-message-kit:qpack-commit-encoding transaction)
             (http3-test-concat-octets capacity-wire wire)))
         (section
           (multiple-value-bind (wire transaction)
               (http-message-kit:qpack-prepare-header-block
                headers :context encoder-context :use-dynamic-p t
                :stream-id 0 :huffman nil)
             (http-message-kit:qpack-commit-encoding transaction)
             wire)))
    (values decoder-context section instruction)))

(defun http3-test-decode-inserts-and-resume
    (adapter decoder-context instruction token
     &optional
       (resume (function http-message-kit:qpack-resume-blocked-stream)))
  (http-message-kit:qpack-decode-encoder-stream
   decoder-context instruction)
  (let ((ready
          (http-message-kit:qpack-take-ready-streams adapter)))
    (ensure-equal 1 (length ready))
    (ensure-true (eq token (first ready)))
    (funcall resume token)))

(deftest http3-qpack-blocked-section-returns-opaque-token
  (multiple-value-bind (fields decoder-context section instruction)
      (http3-test-qpack-blocked-fixture)
    (declare (ignore fields instruction))
    (let* ((adapter
             (http-message-kit:qpack-make-http3-decoder-context
              decoder-context :max-blocked-streams 1))
           (token
             (http-message-kit:qpack-decode-http3-field-section
              section adapter 0 :role :trailers)))
      (ensure-true (http-message-kit:qpack-blocked-stream-p token)))))

(deftest http3-qpack-blocked-limit-zero-rejects
  (multiple-value-bind (fields decoder-context section instruction)
      (http3-test-qpack-blocked-fixture)
    (declare (ignore fields instruction))
    (let ((adapter
            (http-message-kit:qpack-make-http3-decoder-context
             decoder-context :max-blocked-streams 0)))
      (signals http-message-kit:http-connection-error
        (http-message-kit:qpack-decode-http3-field-section
         section adapter 0 :role :trailers)))))

(deftest http3-qpack-blocked-limit-one-rejects-distinct-stream
  (multiple-value-bind (fields decoder-context section instruction)
      (http3-test-qpack-blocked-fixture)
    (declare (ignore fields instruction))
    (let* ((adapter
             (http-message-kit:qpack-make-http3-decoder-context
              decoder-context :max-blocked-streams 1))
           (token
             (http-message-kit:qpack-decode-http3-field-section
              section adapter 0 :role :trailers)))
      (ensure-true (http-message-kit:qpack-blocked-stream-p token))
      (signals http-message-kit:http-connection-error
        (http-message-kit:qpack-decode-http3-field-section
         section adapter 4 :role :trailers)))))

(deftest http3-qpack-reregister-same-stream-does-not-consume-limit
  (multiple-value-bind (fields decoder-context section instruction)
      (http3-test-qpack-blocked-fixture)
    (declare (ignore fields instruction))
    (let* ((adapter
             (http-message-kit:qpack-make-http3-decoder-context
              decoder-context :max-blocked-streams 1))
           (first-token
             (http-message-kit:qpack-decode-http3-field-section
              section adapter 0 :role :trailers))
           (second-token
             (http-message-kit:qpack-decode-http3-field-section
              section adapter 0 :role :trailers)))
      (ensure-true (http-message-kit:qpack-blocked-stream-p first-token))
      (ensure-true (http-message-kit:qpack-blocked-stream-p second-token))
      (signals http-message-kit:http-connection-error
        (http-message-kit:qpack-decode-http3-field-section
         section adapter 4 :role :trailers)))))

(deftest http3-qpack-ready-threshold-is-inclusive
  (let* ((encoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :max-blocked-streams 1
            :huffman nil))
         (decoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :max-blocked-streams 0
            :huffman nil))
         (adapter
           (http-message-kit:qpack-make-http3-decoder-context
            decoder :max-blocked-streams 1))
         (capacity-wire nil)
         (first-insert nil)
         (second-insert nil)
         (section nil))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-set-capacity encoder 256)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf capacity-wire wire))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-insert-with-literal-name
         encoder "x-first" "one" :huffman nil)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf first-insert wire))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-insert-with-literal-name
         encoder "x-second" "two" :huffman nil)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf second-insert wire))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-header-block
         (list (http-message-kit:make-http-header "x-second" "two"))
         :context encoder :use-dynamic-p t :huffman nil :stream-id 0)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf section wire))
    (let ((token
            (http-message-kit:qpack-decode-http3-field-section
             section adapter 0 :role :trailers)))
      (ensure-true (http-message-kit:qpack-blocked-stream-p token))
      (http-message-kit:qpack-decode-encoder-stream decoder capacity-wire)
      (http-message-kit:qpack-decode-encoder-stream decoder first-insert)
      (ensure-equal nil (http-message-kit:qpack-take-ready-streams adapter))
      (http-message-kit:qpack-decode-encoder-stream decoder second-insert)
      (let ((ready (http-message-kit:qpack-take-ready-streams adapter)))
        (ensure-equal 1 (length ready))
        (ensure-true (eq token (first ready))))
      (let ((headers (http-message-kit:qpack-resume-blocked-stream token)))
        (ensure-equal "x-second" (car (first headers)))
        (ensure-equal "two" (cdr (first headers)))))))

(deftest http3-qpack-ready-stream-detaches-once
  (multiple-value-bind (fields decoder-context section instruction)
      (http3-test-qpack-blocked-fixture)
    (declare (ignore fields))
    (let* ((adapter
             (http-message-kit:qpack-make-http3-decoder-context
              decoder-context :max-blocked-streams 1))
           (token
             (http-message-kit:qpack-decode-http3-field-section
              section adapter 0 :role :trailers)))
      (ensure-true (http-message-kit:qpack-blocked-stream-p token))
      (http-message-kit:qpack-decode-encoder-stream
       decoder-context instruction)
      (let ((ready (http-message-kit:qpack-take-ready-streams adapter)))
        (ensure-equal 1 (length ready))
        (ensure-true (eq token (first ready))))
      (ensure-equal nil (http-message-kit:qpack-take-ready-streams adapter))
      (let ((headers (http-message-kit:qpack-resume-blocked-stream token)))
        (ensure-equal "x-blocked" (car (first headers)))
        (ensure-equal "ready" (cdr (first headers)))))))

(deftest http3-qpack-cancelled-token-cannot-resume
  (multiple-value-bind (fields decoder-context section instruction)
      (http3-test-qpack-blocked-fixture)
    (declare (ignore fields instruction))
    (let* ((adapter
             (http-message-kit:qpack-make-http3-decoder-context
              decoder-context :max-blocked-streams 1))
           (token
             (http-message-kit:qpack-decode-http3-field-section
              section adapter 0 :role :trailers)))
      (ensure-true (http-message-kit:qpack-blocked-stream-p token))
      (multiple-value-bind (released-p cancellation)
          (http-message-kit:qpack-cancel-blocked-stream adapter 0)
        (ensure-true released-p)
        (ensure-equal (octets #x40) cancellation))
      (signals http-message-kit:http-protocol-error
        (http-message-kit:qpack-resume-blocked-stream token)))))

(deftest http3-qpack-detached-token-becomes-stale-on-reregister
  (let* ((encoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :max-blocked-streams 1
            :huffman nil))
         (decoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :max-blocked-streams 0
            :huffman nil))
         (adapter
           (http-message-kit:qpack-make-http3-decoder-context
            decoder :max-blocked-streams 1))
         (capacity-wire nil)
         (first-insert nil)
         (second-insert nil)
         (first-section nil)
         (second-section nil))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-set-capacity encoder 256)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf capacity-wire wire))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-insert-with-literal-name
         encoder "x-first" "one" :huffman nil)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf first-insert wire))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-header-block
         (list (http-message-kit:make-http-header "x-first" "one"))
         :context encoder :use-dynamic-p t :huffman nil :stream-id 0)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf first-section wire))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-insert-with-literal-name
         encoder "x-second" "two" :huffman nil)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf second-insert wire))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-header-block
         (list (http-message-kit:make-http-header "x-second" "two"))
         :context encoder :use-dynamic-p t :huffman nil :stream-id 0)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf second-section wire))
    (let ((stale
            (http-message-kit:qpack-decode-http3-field-section
             first-section adapter 0 :role :trailers)))
      (ensure-true (http-message-kit:qpack-blocked-stream-p stale))
      (http-message-kit:qpack-decode-encoder-stream
       decoder (http3-test-concat-octets capacity-wire first-insert))
      (ensure-true
       (eq stale (first
                  (http-message-kit:qpack-take-ready-streams adapter))))
      (let ((current
              (http-message-kit:qpack-decode-http3-field-section
               second-section adapter 0 :role :trailers)))
        (ensure-true (http-message-kit:qpack-blocked-stream-p current))
        (signals http-message-kit:http-protocol-error
          (http-message-kit:qpack-resume-blocked-stream stale))
        (http-message-kit:qpack-decode-encoder-stream decoder second-insert)
        (ensure-true
         (eq current (first
                      (http-message-kit:qpack-take-ready-streams adapter))))
        (let ((headers (http-message-kit:qpack-resume-blocked-stream current)))
          (ensure-equal "x-second" (car (first headers)))
          (ensure-equal "two" (cdr (first headers))))))))

(deftest http3-qpack-ready-token-resumes-once
  (multiple-value-bind (fields decoder-context section instruction)
      (http3-test-qpack-blocked-fixture)
    (declare (ignore fields))
    (let* ((adapter
             (http-message-kit:qpack-make-http3-decoder-context
              decoder-context :max-blocked-streams 1))
           (token
             (http-message-kit:qpack-decode-http3-field-section
              section adapter 0 :role :trailers)))
      (http-message-kit:qpack-decode-encoder-stream
       decoder-context instruction)
      (ensure-true
       (eq token (first
                  (http-message-kit:qpack-take-ready-streams adapter))))
      (ensure-equal "x-blocked" (car (first (http-message-kit:qpack-resume-blocked-stream token))))
      (signals http-message-kit:http-protocol-error
        (http-message-kit:qpack-resume-blocked-stream token)))))

(deftest http3-qpack-section-ack-callback-runs-outside-serializer
  (multiple-value-bind (fields decoder-context section instruction)
      (http3-test-qpack-blocked-fixture)
    (declare (ignore fields))
    (let* ((adapter
             (http-message-kit:qpack-make-http3-decoder-context
              decoder-context :max-blocked-streams 1))
           (inside-serializer-p nil)
           (callback-inside-p :not-called)
           (acknowledged-stream-id nil)
           (serializer-calls 0))
      (http-message-kit:qpack-decode-encoder-stream
       decoder-context instruction)
      (let ((headers
              (http-message-kit:qpack-decode-http3-field-section
               section adapter 0 :role :trailers
               :serialize
               (lambda (thunk)
                 (incf serializer-calls)
                 (setf inside-serializer-p t)
                 (unwind-protect
                      (funcall thunk)
                   (setf inside-serializer-p nil)))
               :on-section-acknowledgment
               (lambda (stream-id)
                 (setf callback-inside-p inside-serializer-p
                       acknowledged-stream-id stream-id)))))
        (ensure-equal "x-blocked" (car (first headers)))
        (ensure-equal "ready" (cdr (first headers))))
      (ensure-equal 1 serializer-calls)
      (ensure-equal nil callback-inside-p)
      (ensure-equal 0 acknowledged-stream-id))))

(deftest http3-qpack-peer-zero-falls-back-to-literals
  (let* ((fields (list (cons "x-unacknowledged" "value")))
         (encoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 256
            :max-blocked-streams 0 :huffman nil))
         (decoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 256
            :max-blocked-streams 0 :huffman nil))
         (section nil))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-insert-with-literal-name
         encoder "x-unacknowledged" "value" :huffman nil)
      (declare (ignore wire))
      (http-message-kit:qpack-commit-encoding transaction))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-header-block
         (list (http-message-kit:make-http-header
                "x-unacknowledged" "value"))
         :context encoder :use-dynamic-p t :huffman nil :stream-id 0)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf section wire))
    (multiple-value-bind (headers consumed required-count base)
        (http-message-kit:qpack-decode-header-block
         section :context decoder :stream-id 0)
      (declare (ignore consumed base))
      (ensure-equal fields
                    (mapcar
                     (lambda (header)
                       (cons (http-message-kit:http-header-name header)
                             (http-message-kit:http-header-content header)))
                     headers))
      (ensure-equal 0 required-count))))

(deftest http3-qpack-peer-one-allows-dynamic-reference
  (let* ((fields (list (cons "x-unacknowledged" "value")))
         (encoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 256
            :max-blocked-streams 1 :huffman nil))
         (decoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 256
            :max-blocked-streams 0 :huffman nil))
         (adapter
           (http-message-kit:qpack-make-http3-decoder-context
            decoder :max-blocked-streams 1))
         (insert-wire nil)
         (section nil))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-insert-with-literal-name
         encoder "x-unacknowledged" "value" :huffman nil)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf insert-wire wire))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-header-block
         (list (http-message-kit:make-http-header
                "x-unacknowledged" "value"))
         :context encoder :use-dynamic-p t :huffman nil :stream-id 0)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf section wire))
    (let ((token
            (http-message-kit:qpack-decode-http3-field-section
             section adapter 0 :role :trailers)))
      (ensure-true (http-message-kit:qpack-blocked-stream-p token))
      (http-message-kit:qpack-decode-encoder-stream decoder insert-wire)
      (ensure-true
       (eq token (first
                  (http-message-kit:qpack-take-ready-streams adapter))))
      (let ((headers
              (http-message-kit:qpack-resume-blocked-stream token)))
        (ensure-equal fields headers)))))

(deftest http3-qpack-malformed-section-is-not-blocked
  (let* ((decoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0
            :max-blocked-streams 0 :huffman nil))
         (adapter
           (http-message-kit:qpack-make-http3-decoder-context
            decoder :max-blocked-streams 1)))
    (signals http-message-kit:http-connection-error
      (http-message-kit:qpack-decode-http3-field-section
       (octets) adapter 0 :role :trailers))))

(deftest http3-response-qpack-decode-serialization-ownership
  (labels ((malformed-headers-wire ()
             (http-kit/http3:encode-http3-frame
              (http-kit/http3:make-http3-frame
               :type http-kit/http3:+http3-headers-type+
               :payload (octets))))
           (valid-response-wire ()
             (http-kit/http3:encode-http3-frame
              (http-kit/http3:make-http3-frame
               :type http-kit/http3:+http3-headers-type+
               :payload
               (http3-test-qpack-encode
                (list (cons ":status" "204")) :huffman nil)))))
    (let* ((decoder-context
             (http-message-kit:make-qpack-context
              :max-table-capacity 0 :table-capacity 0
              :max-blocked-streams 0 :huffman nil))
           (adapter
             (http-message-kit:qpack-make-http3-decoder-context
              decoder-context :max-blocked-streams 0))
           (inside-p nil)
           (events '())
           (client
             (http-kit/http3::%make-http3-client
              :qpack-decoder-context decoder-context
              :qpack-http3-decoder-context adapter
              :open-p t
              :serialize
              (lambda (thunk)
                (push :serialize-enter events)
                (let ((previous inside-p))
                  (setf inside-p t)
                  (unwind-protect
                       (handler-bind
                           ((http-message-kit:http-connection-error
                              (lambda (condition)
                                (declare (ignore condition))
                                (push (list :decode-condition inside-p)
                                      events))))
                         (funcall thunk))
                    (setf inside-p previous))))
              :read-stream
              (lambda (stream &key timeout deadline)
                (declare (ignore stream timeout deadline))
                (push (list :read inside-p) events)
                (values (malformed-headers-wire) t)))))
      (signals http-message-kit:http-connection-error
        (http-kit/http3::%h3-read-response
         client :stream :collect-body-p t
         :qpack-http3-decoder-context adapter :stream-id 0))
      (ensure-equal '(:serialize-enter (:read nil)
                      :serialize-enter (:decode-condition t))
                    (nreverse events)))
    (let* ((decoder-context
             (http-message-kit:make-qpack-context
              :max-table-capacity 0 :table-capacity 0
              :max-blocked-streams 0 :huffman nil))
           (adapter
             (http-message-kit:qpack-make-http3-decoder-context
              decoder-context :max-blocked-streams 0))
           (caller-context
             (http-message-kit:make-qpack-context
              :max-table-capacity 0 :table-capacity 0
              :max-blocked-streams 0 :huffman nil))
           (caller-adapter
             (http-message-kit:qpack-make-http3-decoder-context
              caller-context :max-blocked-streams 0))
           (inside-p nil)
           (events '())
           (client
             (http-kit/http3::%make-http3-client
              :qpack-decoder-context decoder-context
              :qpack-http3-decoder-context adapter
              :open-p t
              :serialize
              (lambda (thunk)
                (push :serialize-enter events)
                (let ((previous inside-p))
                  (setf inside-p t)
                  (unwind-protect
                       (handler-bind
                           ((http-protocol-error
                              (lambda (condition)
                                (declare (ignore condition))
                                (push (list :decode-condition inside-p)
                                      events))))
                         (funcall thunk))
                    (setf inside-p previous))))
              :read-stream
              (lambda (stream &key timeout deadline)
                (declare (ignore stream timeout deadline))
                (push (list :read inside-p) events)
                (values (valid-response-wire) t)))))
      (signals http-protocol-error
        (http-kit/http3::%h3-read-response
         client :stream :collect-body-p t
         :qpack-http3-decoder-context caller-adapter :stream-id 0))
      (ensure-equal '(:serialize-enter (:read nil)
                      :serialize-enter (:decode-condition t))
                    (nreverse events)))))

(deftest http3-client-blocked-response-pauses-same-chunk
  (multiple-value-bind (decoder-table section instruction)
      (http3-test-qpack-blocked-section
       (list (cons ":status" "200")
             (cons "content-length" "3")
             (cons "x-blocked" "ready")))
    (let* ((context
             (http-message-kit:qpack-make-http3-decoder-context
              decoder-table :max-blocked-streams 1))
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
           (serializations 0)
           (section-ack-serialized-p nil)
           (read-count 0)
           (await-count 0)
           (encoder-table
             (http-message-kit:make-qpack-context
              :max-table-capacity 0 :table-capacity 0 :huffman nil))
           (client
             (http-kit/http3::%make-http3-client
              :qpack-decoder-context decoder-table
              :qpack-http3-decoder-context context
              :qpack-decoder-stream :decoder-stream
              :peer-control-state
              (http-kit/http3:make-http3-control-state :peer-role :server)
              :open-stream
              (lambda (request &key stream-type timeout deadline)
                (declare (ignore request stream-type timeout deadline))
                (values :request-stream 12))
              :write-stream
              (lambda (stream octets &key fin-p timeout deadline)
                (declare (ignore stream fin-p timeout deadline))
                (when (equalp octets
                              (http-message-kit:qpack-encode-section-ack 12))
                  (setf section-ack-serialized-p inside-serializer-p)))
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
                (incf serializations)
                (setf inside-serializer-p t)
                (unwind-protect
                     (funcall thunk)
                  (setf inside-serializer-p nil))))))
      (let ((response
              (http-kit/http3:send-http3-request
               client
               (make-http-request :method "GET" :uri "https://example.test/")
               :qpack-encoder-context encoder-table
               :qpack-decoder-context decoder-table
               :qpack-http3-decoder-context context
               :await-qpack
               (lambda (token)
                 (incf await-count)
                 (push (list :await inside-serializer-p) events)
                 (ensure-equal nil
                               (find :data events :key (lambda (event)
                                                        (if (consp event)
                                                            (first event)
                                                            event))))
                 (let ((before-resume serializations))
                   (prog1
                       (http3-test-decode-inserts-and-resume
                        context decoder-table instruction token
                        (lambda (token)
                          (let ((before-resume serializations))
                            (prog1
                                (http-message-kit:qpack-resume-blocked-stream
                                 token)
                              (ensure-true
                               (> serializations before-resume))))))
                     (ensure-equal (+ before-resume 3) serializations)
                     (push :resumed events))))
               :on-body-chunk
               (lambda (chunk)
                 (push (list :data (subseq chunk 0)) events)))))
        (ensure-equal (octets 1 2 3) (http-response-body response)))
      (ensure-equal 1 read-count)
      (ensure-equal 1 await-count)
      (ensure-true section-ack-serialized-p)
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
             (http-message-kit:qpack-make-http3-decoder-context
              decoder-table :max-blocked-streams 1))
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
       :qpack-decoder-context decoder-table
       :qpack-http3-decoder-context context
       :on-qpack-section-acknowledgment
       (lambda (stream-id) (ensure-equal 16 stream-id))
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
         (prog1
             (http3-test-decode-inserts-and-resume
              context decoder-table instruction token)
           (push :resumed events)))
       :on-body-chunk
       (lambda (chunk) (push (list :data (subseq chunk 0)) events)))
      (ensure-equal 1 await-count)
      (ensure-equal
       '(:read (:await nil) :resumed (:data #(4 5 6)) (:handler #(4 5 6)))
       (nreverse events)))))

(deftest http3-qpack-decoder-context-adapter-pairing
  (let* ((owned-table
           (http-message-kit:make-qpack-context
            :max-table-capacity 0 :table-capacity 0 :huffman nil))
         (owned-adapter
           (http-message-kit:qpack-make-http3-decoder-context
            owned-table :max-blocked-streams 1))
         (other-table
           (http-message-kit:make-qpack-context
            :max-table-capacity 0 :table-capacity 0 :huffman nil))
         (other-adapter
           (http-message-kit:qpack-make-http3-decoder-context
            other-table :max-blocked-streams 1))
         (response-wire
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-headers-type+
             :payload (http3-test-qpack-encode
                       (list (cons ":status" "204")) :huffman nil))))
         (opens 0)
         (reads 0)
         (writes 0)
         (request
           (make-http-request :method "GET" :uri "https://example.test/"))
         (client
           (http-kit/http3::%make-http3-client
            :qpack-encoder-context
            (http-message-kit:make-qpack-context
             :max-table-capacity 0 :table-capacity 0 :huffman nil)
            :qpack-decoder-context owned-table
            :qpack-http3-decoder-context owned-adapter
            :peer-control-state
            (http-kit/http3:make-http3-control-state :peer-role :server)
            :open-stream
            (lambda (request &key stream-type timeout deadline)
              (declare (ignore request stream-type timeout deadline))
              (incf opens)
              (values :request-stream 0))
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore stream timeout deadline))
              (incf reads)
              (values response-wire t))
            :write-stream
            (lambda (stream octets &key fin-p timeout deadline)
              (declare (ignore stream octets fin-p timeout deadline))
              (incf writes))
            :close-stream
            (lambda (stream &key condition)
              (declare (ignore stream condition)))))
         (server-reads 0)
         (server-writes 0)
         (handler
           (lambda (incoming-request)
             (declare (ignore incoming-request))
             (http-kit:make-http-response :status 204 :body (octets))))
         (acknowledge
           (lambda (stream-id) (declare (ignore stream-id))))
         (serialize
           (lambda (thunk) (funcall thunk))))
    (ensure-equal
     204
     (http-response-status
      (http-kit/http3:send-http3-request
       client request :qpack-decoder-context owned-table
       :await-qpack (lambda (token) (declare (ignore token))))))
    (ensure-equal
     204
     (http-response-status
      (http-kit/http3:send-http3-request
       client request :qpack-http3-decoder-context owned-adapter
       :await-qpack (lambda (token) (declare (ignore token))))))
    (ensure-equal 2 opens)
    (ensure-equal 2 reads)
    (ensure-equal 4 writes)
    (let ((opens-before opens) (reads-before reads) (writes-before writes))
      (let ((failure
              (handler-case
                  (progn
                    (http-kit/http3:send-http3-request
                     client request :qpack-decoder-context other-table
                     :await-qpack (lambda (token) (declare (ignore token))))
                    nil)
                (http-protocol-error (condition) condition))))
        (http3-public-error-check
         failure "A custom QPACK decoder context must be the client-owned context."
         other-table))
      (ensure-equal opens-before opens)
      (ensure-equal reads-before reads)
      (ensure-equal writes-before writes)
      (let ((failure
              (handler-case
                  (progn
                    (http-kit/http3:send-http3-request
                     client request :qpack-http3-decoder-context other-adapter
                     :await-qpack (lambda (token) (declare (ignore token))))
                    nil)
                (http-protocol-error (condition) condition))))
        (http3-public-error-check
         failure "A custom HTTP/3 decoder adapter must be the client-owned adapter."
         other-adapter))
      (ensure-equal opens-before opens)
      (ensure-equal reads-before reads)
      (ensure-equal writes-before writes))
    (let ((reads-before reads))
      (signals http-protocol-error
        (http-kit/http3::%h3-read-response
         client :response-stream
         :qpack-http3-decoder-context owned-adapter :stream-id nil
         :await-qpack (lambda (token) (declare (ignore token)))))
      (ensure-equal reads-before reads))
    (labels
        ((run-push-case (mode)
           (let* ((push-table
                    (http-message-kit:make-qpack-context
                     :max-table-capacity 0 :table-capacity 0 :huffman nil))
                  (push-adapter
                    (http-message-kit:qpack-make-http3-decoder-context
                     push-table :max-blocked-streams 1))
                  (foreign-table
                    (http-message-kit:make-qpack-context
                     :max-table-capacity 0 :table-capacity 0 :huffman nil))
                  (foreign-adapter
                    (http-message-kit:qpack-make-http3-decoder-context
                     foreign-table :max-blocked-streams 1))
                  (promise-fields
                    (list (cons ":method" "GET")
                          (cons ":scheme" "https")
                          (cons ":authority" "example.test")
                          (cons ":path" "/asset.css")))
                  (push-wire
                    (http3-test-concat-octets
                     (http-kit/http3:http3-varint-encode
                      http-kit/http3:+http3-push-stream-type+)
                     (http-kit/http3:http3-varint-encode 2)
                     (http-kit/http3:encode-http3-frame
                      (http-kit/http3:make-http3-frame
                       :type http-kit/http3:+http3-headers-type+
                       :payload (http3-test-qpack-encode
                                 (list (cons ":status" "200")) :huffman nil)))))
                  (stream
                    (make-http3-test-stream :kind :push
                                            :reads (list (list push-wire t))))
                  (read-count 0)
                  (await-count 0)
                  (closed nil)
                  (client
                    (http-kit/http3::%make-http3-client
                     :peer-control-state
                     (http-kit/http3:make-http3-control-state :peer-role :server)
                     :qpack-decoder-context push-table
                     :qpack-http3-decoder-context push-adapter
                     :promised-push-ids (list 2)
                     :push-promises (list (cons 2 promise-fields))
                     :read-stream
                     (lambda (actual &key timeout deadline)
                       (declare (ignore timeout deadline))
                       (ensure-true (eq stream actual))
                       (incf read-count)
                       (let ((entry (pop (http3-test-stream-reads stream))))
                         (values (first entry) (second entry))))
                     :close-stream
                     (lambda (actual &key condition)
                       (declare (ignore condition))
                       (ensure-true (eq stream actual))
                       (setf closed t))))
                  (arguments
                    (case mode
                      (:owner-context (list :qpack-decoder-context push-table))
                      (:owner-adapter (list :qpack-http3-decoder-context push-adapter))
                      (:foreign-context
                       (list :qpack-decoder-context foreign-table))
                      (:foreign-adapter
                       (list :qpack-http3-decoder-context foreign-adapter))))
                  (failure nil)
                  (result nil))
             (handler-case
                 (setf result
                       (multiple-value-list
                        (apply #'http-kit/http3:receive-http3-push
                               client stream :stream-id 0
                               :await-qpack
                               (lambda (token) (declare (ignore token)))
                               :await-push-promise
                               (lambda (push-id)
                                 (declare (ignore push-id))
                                 (incf await-count))
                               arguments)))
               (http-protocol-error (condition) (setf failure condition)))
             (if (member mode '(:foreign-context :foreign-adapter))
                 (progn
                   (http3-public-error-check
                    failure
                    (if (eq mode :foreign-context)
                        "A custom QPACK decoder context must be the client-owned context."
                        "A custom HTTP/3 decoder adapter must be the client-owned adapter.")
                    (if (eq mode :foreign-context) foreign-table foreign-adapter))
                   (ensure-equal nil result)
                   (ensure-equal 0 read-count)
                   (ensure-equal 1 (length (http3-test-stream-reads stream)))
                   (ensure-false closed)
                   (ensure-equal '(2)
                                 (http-kit/http3:http3-client-promised-push-ids client))
                   (ensure-equal nil
                                 (http-kit/http3:http3-client-consumed-push-ids client)))
                 (progn
                   (ensure-equal nil failure)
                   (ensure-equal 2 (first result))
                   (ensure-equal 200 (http-response-status (second result)))
                   (ensure-equal 1 read-count)
                   (ensure-equal nil (http3-test-stream-reads stream))
                   (ensure-true closed)
                   (ensure-equal '(2)
                                 (http-kit/http3:http3-client-consumed-push-ids client))))
             (ensure-equal 0 await-count))))
      (run-push-case :owner-context)
      (run-push-case :owner-adapter)
      (run-push-case :foreign-context)
      (run-push-case :foreign-adapter))
    (let ((read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore stream timeout deadline))
              (incf server-reads)
              (values nil t)))
          (write-stream
            (lambda (stream octets &key fin-p timeout deadline)
              (declare (ignore stream octets fin-p timeout deadline))
              (incf server-writes))))
      (signals http-protocol-error
        (http-kit/http3:serve-http3-request-stream
         :request-stream handler :read-stream read-stream :write-stream write-stream
         :qpack-decoder-context owned-table :stream-id 0))
      (signals http-protocol-error
        (http-kit/http3:serve-http3-request-stream
         :request-stream handler :read-stream read-stream :write-stream write-stream
         :qpack-http3-decoder-context owned-adapter :stream-id 0
         :on-qpack-section-acknowledgment acknowledge
         :qpack-serialize serialize
         :await-qpack (lambda (token) (declare (ignore token)))))
      (signals http-protocol-error
        (http-kit/http3:serve-http3-request-stream
         :request-stream handler :read-stream read-stream :write-stream write-stream
         :qpack-decoder-context owned-table
         :qpack-http3-decoder-context owned-adapter
         :stream-id nil :on-qpack-section-acknowledgment acknowledge
         :qpack-serialize serialize
         :await-qpack (lambda (token) (declare (ignore token))))))
    (ensure-equal 0 server-reads)
    (ensure-equal 0 server-writes)))

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
                (http3-test-qpack-encode (list (cons ":status" "204")))))
              (http-kit/http3:encode-http3-frame
               (http-kit/http3:make-http3-frame
                :type http-kit/http3:+http3-data-type+
                :payload (octets 1)))))
           (client
             (http-kit/http3::%make-http3-client
              :peer-control-state
              (http-kit/http3:make-http3-control-state :peer-role :server)
              :read-stream
              (lambda (stream &key timeout deadline)
                (declare (ignore stream timeout deadline))
                (values wire t)))))
      (signals http-protocol-error
        (http-kit/http3::%h3-read-response
         client :stream :stream-id 0 :request-method "GET" :collect-body-p t)))
  (let* ((wire
           (http3-test-concat-octets
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-headers-type+
              :payload
              (http3-test-qpack-encode (list (cons ":status" "200")))))
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-data-type+
              :payload (octets 1 2 3)))))
         (client
           (http-kit/http3::%make-http3-client
            :peer-control-state
            (http-kit/http3:make-http3-control-state :peer-role :server)
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore stream timeout deadline))
              (values wire t))
            :close-stream
            (lambda (stream &key condition)
              (declare (ignore stream condition))))))
    (ensure-equal
     (octets 1 2 3)
    (http-response-body
      (http-kit/http3::%h3-read-response
       client :stream :stream-id 0 :request-method "CONNECT" :collect-body-p t)))))

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
              (values wire t))
            :close-stream
            (lambda (stream &key condition)
              (declare (ignore stream condition))))))
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
               (http3-test-qpack-encode fields))))
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-headers-type+
              :payload
              (http3-test-qpack-encode (list (cons ":status" "204")))))))
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
           (http3-test-qpack-encode (list (cons ":method" "GET")
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
                 (http3-test-qpack-encode promised-fields))))
              (http-kit/http3:encode-http3-frame
               (http-kit/http3:make-http3-frame
                :type http-kit/http3:+http3-headers-type+
                :payload
                (http3-test-qpack-encode (list (cons ":status" "204")))))))))
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
              (http3-test-qpack-encode (list (cons ":status" "200")
                     (cons "content-length" "3")))))
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-data-type+
              :payload (octets 1 2 3)))
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-headers-type+
              :payload
              (http3-test-qpack-encode (list (cons "x-push-trailer" "done")))))))
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
              (http3-test-qpack-encode (list (cons ":status" "200")
                     (cons "content-type" "text/css")))))))
         (make-client
           (lambda ()
             (http-kit/http3::%make-http3-client
              :push-promises (list (cons 2 promised-fields))
              :read-stream
              (lambda (stream &key timeout deadline)
                (declare (ignore stream timeout deadline))
                (values wire t))
              :close-stream
              (lambda (stream &key condition)
                (declare (ignore stream condition)))))))
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
              (http3-test-qpack-encode (list (cons ":status" "204")))))))
         (awaited '())
         (client
           (http-kit/http3::%make-http3-client
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore stream timeout deadline))
              (values wire t))
            :close-stream
            (lambda (stream &key condition)
              (declare (ignore stream condition))))))
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
        (ensure-true (typep (first closed) 'http-protocol-error))))))

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
      (ensure-true (typep (second (first closed)) 'http-protocol-error)))))

(deftest http3-injected-quic-request-response
  (let* ((request-table
           (let ((table
                   (http-message-kit:make-qpack-context :max-table-capacity 256 :table-capacity 256 :huffman nil)))
             (http-message-kit:qpack-encode-insert-with-literal-name table "x-test" "yes")
             table))
         (response-table
           (let ((table
                   (http-message-kit:make-qpack-context :max-table-capacity 256 :table-capacity 256 :huffman nil)))
             (http-message-kit:qpack-encode-insert-with-literal-name table "content-type" "text/plain")
             table))
         (response-fields (list (cons ":status" "200")
                                (cons "content-type" "text/plain")
                                (cons "content-length" "5")))
         (response-headers
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-headers-type+
             :payload
             (http3-test-qpack-encode response-fields
              :context response-table
              :huffman t))))
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
                  :headers (list (make-http-header "Accept-Encoding" "gzip")
                                 (make-http-header "x-test" "yes"))
                  :body (octets 65 66))
                 :collect-body-p nil
                 :qpack-encoder-context request-table
                 :qpack-decoder-context response-table
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
                   (cons "accept-encoding" "gzip")
                   (cons "x-test" "yes"))
             (http3-test-qpack-decode (http-kit/http3:http3-frame-payload (first frames))
              :context request-table)))
          (multiple-value-bind (frames remainder)
              (http-kit/http3:decode-http3-frames data-wire)
            (ensure-equal 0 (length remainder))
            (ensure-equal http-kit/http3:+http3-data-type+
                          (http-kit/http3:http3-frame-type (first frames)))))
        (http-kit/http3:close-http3-client client)
        (ensure-true (every #'http3-test-stream-closed-p opened-streams))
        (ensure-true (not (http-kit/http3:http3-client-open-p client)))))))

(deftest http3-quic-adapter-validates-poll-interval
  (signals http-protocol-error
    (http-kit/http3:make-http3-quic-adapter :poll-interval -1))
  (signals http-protocol-error
    (http-kit/http3:make-http3-quic-adapter))
  (signals http-protocol-error
    (http-kit/http3:http3-quic-adapter-poll :not-an-adapter)))

(deftest http3-quic-adapter-validates-options
  (ensure-true
   (http-kit/http3::%http3-quic-adapter-property-list-p nil))
  (ensure-true
   (http-kit/http3::%http3-quic-adapter-property-list-p
    '(:max-fields 16 :max-header-bytes 1024)))
  (ensure-false
   (http-kit/http3::%http3-quic-adapter-property-list-p '(:max-fields)))
  (ensure-false
   (http-kit/http3::%http3-quic-adapter-property-list-p '(max-fields 16)))
  (signals http-protocol-error
    (http-kit/http3:make-http3-quic-adapter
     :hostname "example.test"
     :http3-options '(:max-fields)))
  (signals http-protocol-error
    (http-kit/http3:make-http3-quic-adapter
     :hostname "example.test"
     :http3-options '(max-fields 16))))

(defun make-http3-quic-adapter-test-client (&key (established-p t))
  (http-kit/http3::%http3-quic-adapter-configure-protection)
  (let* ((connection (cl-quic-kit:make-quic-connection
                      :now-fn (constantly 0)))
         (client (cl-quic-kit:make-quic-client
                  :connection connection :hostname "example.test"
                  :local-connection-id (octets 1 2 3 4)
                  :destination-connection-id (octets 5 6 7 8)
                  :now-fn (constantly 0))))
    (setf (cl-quic-kit::quic-client-started-p client) t)
    (when established-p
      (cl-quic-kit:connection-set-state connection :established))
    client))

(defun call-with-http3-quic-lifecycle-spies (operation)
  ;; These process-global spies require an isolated, serial test subprocess.
  (let* ((symbols '(cl-quic-kit:client-start cl-quic-kit:client-poll
                    cl-quic-kit:client-close))
         (originals (mapcar #'symbol-function symbols))
         (calls (make-hash-table :test #'eq)))
    (unwind-protect
         (progn
           (loop for symbol in symbols
                 for original in originals
                 do (let ((name symbol)
                          (function original))
                      (setf (symbol-function name)
                            (lambda (&rest arguments)
                              (push arguments (gethash name calls))
                              (apply function arguments)))))
           (funcall operation calls))
      (loop for symbol in symbols
            for original in originals
            do (setf (symbol-function symbol) original)))))

(deftest http3-quic-adapter-lifecycle-start-and-poll
  (let* ((quic (make-http3-quic-adapter-test-client))
         (adapter (http-kit/http3::%make-http3-quic-adapter :quic-client quic)))
    (unwind-protect
         (call-with-http3-quic-lifecycle-spies
          (lambda (calls)
           (ensure-false (http-kit/http3::http3-quic-adapter-started-p adapter))
           (ensure-true (eq adapter (http-kit/http3:http3-quic-adapter-start adapter)))
           (ensure-true (http-kit/http3::http3-quic-adapter-started-p adapter))
           (ensure-equal (list (list quic)) (gethash 'cl-quic-kit:client-start calls))
           (ensure-true (eq adapter (http-kit/http3:http3-quic-adapter-start adapter)))
           (ensure-equal (list (list quic)) (gethash 'cl-quic-kit:client-start calls))
           (ensure-true (eq adapter (http-kit/http3:http3-quic-adapter-poll adapter 0)))
           (ensure-equal (list (list quic)) (gethash 'cl-quic-kit:client-start calls))
           (ensure-equal (list (list quic 0)) (gethash 'cl-quic-kit:client-poll calls))
           (ensure-false (gethash 'cl-quic-kit:client-close calls))
           (ensure-equal :established
                         (cl-quic-kit:connection-state
                          (cl-quic-kit::quic-client-connection quic)))
           (ensure-false (cl-quic-kit::quic-client-closed-p quic))))
      (http-kit/http3:close-http3-quic-adapter adapter))))

(deftest http3-quic-adapter-lifecycle-close-and-closed-poll
  (let* ((quic (make-http3-quic-adapter-test-client))
         (client (http-kit/http3::%make-http3-client))
         (adapter (http-kit/http3::%make-http3-quic-adapter
                   :quic-client quic :http3-client client)))
    (unwind-protect
         (call-with-http3-quic-lifecycle-spies
          (lambda (calls)
           (ensure-true (http-kit/http3:http3-client-open-p client))
           (ensure-true (http-kit/http3:close-http3-quic-adapter
                         adapter :error-code :no-error :reason "Lifecycle close."))
           (ensure-equal (list (list quic :error-code :no-error :reason "Lifecycle close."))
                         (gethash 'cl-quic-kit:client-close calls))
           (ensure-true (http-kit/http3::http3-quic-adapter-closed-p adapter))
           (ensure-true (cl-quic-kit::quic-client-closed-p quic))
           (ensure-false (http-kit/http3:http3-client-open-p client))
           (let ((state (cl-quic-kit:connection-state
                         (cl-quic-kit::quic-client-connection quic)))
                 (frames (copy-list (cl-quic-kit::quic-client-pending-frames quic))))
             (ensure-true (http-kit/http3:close-http3-quic-adapter adapter))
             (ensure-equal (list (list quic :error-code :no-error :reason "Lifecycle close."))
                           (gethash 'cl-quic-kit:client-close calls))
             (ensure-true (eq adapter
                              (http-kit/http3:http3-quic-adapter-poll adapter 100000)))
             (ensure-equal state (cl-quic-kit:connection-state
                                  (cl-quic-kit::quic-client-connection quic)))
             (ensure-equal frames (cl-quic-kit::quic-client-pending-frames quic)))
           (signals http-protocol-error
             (http-kit/http3:http3-quic-adapter-start adapter))
           (ensure-false (gethash 'cl-quic-kit:client-start calls))
           (ensure-false (gethash 'cl-quic-kit:client-poll calls))
           (ensure-equal (list (list quic :error-code :no-error :reason "Lifecycle close."))
                         (gethash 'cl-quic-kit:client-close calls))))
      (http-kit/http3:close-http3-quic-adapter adapter))))

(deftest http3-quic-adapter-lifecycle-invalid-arguments
  (call-with-http3-quic-lifecycle-spies
   (lambda (calls)
     (signals http-protocol-error
       (http-kit/http3:http3-quic-adapter-start :not-an-adapter))
     (ensure-equal 0 (hash-table-count calls))
     (signals http-protocol-error
       (http-kit/http3:close-http3-quic-adapter :not-an-adapter))
     (ensure-equal 0 (hash-table-count calls)))))

(deftest http3-quic-adapter-lifecycle-stream-cleanup-failure
  (let* ((quic (make-http3-quic-adapter-test-client))
         (stream (make-http3-test-stream :kind :control))
         (failure (make-condition 'simple-error :format-control "Stream cleanup failed."))
         (cleanup-calls nil)
         (signaled 0)
         (client (http-kit/http3::%make-http3-client
                  :control-stream stream
                  :close-stream (lambda (closed-stream &key condition)
                                  (push (list closed-stream condition) cleanup-calls)
                                  (handler-bind ((simple-error
                                                   (lambda (condition)
                                                     (when (eq condition failure)
                                                       (incf signaled)))))
                                    (error failure)))))
         (adapter (http-kit/http3::%make-http3-quic-adapter
                   :quic-client quic :http3-client client)))
    (unwind-protect
         (call-with-http3-quic-lifecycle-spies
          (lambda (calls)
            (ensure-true (http-kit/http3:close-http3-quic-adapter
                          adapter :reason "Cleanup failure."))
            (ensure-equal 1 signaled)
            (ensure-equal (list (list stream nil)) cleanup-calls)
            (ensure-equal (list (list quic :error-code :no-error :reason "Cleanup failure."))
                          (gethash 'cl-quic-kit:client-close calls))
            (ensure-true (http-kit/http3::http3-quic-adapter-closed-p adapter))
            (ensure-true (cl-quic-kit::quic-client-closed-p quic))
            (ensure-false (http-kit/http3:http3-client-open-p client))))
      (http-kit/http3:close-http3-quic-adapter adapter))))

(defun ensure-http3-quic-adapter-timeout (operation &key connect-p)
  (handler-case
      (progn
        (funcall operation)
        (error "The expired QUIC adapter deadline must signal HTTP-TIMEOUT."))
    (http-timeout (condition)
      (ensure-equal :deadline (http-timeout-kind condition))
      (ensure-equal :http3-quic-adapter (http-error-operation condition))
      (if connect-p
          (progn
            (ensure-true (typep condition 'http-connection-error))
            (ensure-false (http-connection-error-request-sent-p condition))
            (ensure-true (typep (http-connection-error-cause condition) 'http-timeout)))
          (ensure-false (typep condition 'http-connection-error))))))

(deftest http3-quic-adapter-connect-timeout-metadata
  (dolist (kind '(:deadline :handshake))
    (let* ((original (make-condition 'http-timeout
                                     :message "Handshake deadline expired."
                                     :operation :connect :kind kind))
           (caught nil))
      (handler-case
          (http-kit/http3::%http3-quic-adapter-connect
           (lambda () (error original)))
        (http-timeout (condition) (setf caught condition)))
      (ensure-true (typep caught 'http-timeout))
      (ensure-true (typep caught 'http-connection-error))
      (ensure-false (eq original caught))
      (ensure-true (eq original (http-connection-error-cause caught)))
      (ensure-false (http-connection-error-request-sent-p caught))
      (ensure-equal (http-error-message original) (http-error-message caught))
      (ensure-equal :connect (http-error-operation caught))
      (ensure-equal kind (http-timeout-kind caught)))))

(deftest http3-quic-adapter-deadline-minimum
  (let* ((clock 'http-kit/http3::%http3-quic-adapter-now)
         (original (symbol-function clock))
         (calls 0))
    (unwind-protect
         (progn
           (setf (symbol-function clock)
                 (lambda () (incf calls) 100))
           (ensure-equal 105 (http-kit/http3::%http3-quic-adapter-deadline 5 110))
           (ensure-equal 110 (http-kit/http3::%http3-quic-adapter-deadline 20 110))
           (ensure-equal 105 (http-kit/http3::%http3-quic-adapter-deadline 5 nil))
           (ensure-equal 110 (http-kit/http3::%http3-quic-adapter-deadline nil 110))
           (ensure-false (http-kit/http3::%http3-quic-adapter-deadline nil nil))
           (ensure-equal 3 calls))
      (setf (symbol-function clock) original))
    (ensure-true (eq original (symbol-function clock)))))

(deftest http3-quic-adapter-read-stream-input-after-empty
  (dolist (payload (list (octets 17 23 42) (octets)))
    (let* ((quic (make-http3-quic-adapter-test-client))
           (stream (cl-quic-kit:client-open-stream quic nil))
           (poll (symbol-function 'cl-quic-kit:client-poll))
           (read-stream (symbol-function 'cl-quic-kit:client-read-stream))
           (polls 0)
           (empty-reads 0)
           (reads 0))
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-quic-kit:client-poll)
                   (lambda (client &optional at)
                     (ensure-true (eq quic client))
                     (prog1 (funcall poll client at)
                       (when (= 2 (incf polls))
                         (cl-quic-kit::%client-receive-frames
                          client :1-rtt 1
                          (list (cl-quic-kit:make-frame
                                 :stream :stream-id (cl-quic-kit:stream-id stream)
                                 :offset 0 :data payload :fin t :len-present t)))))))
             (setf (symbol-function 'cl-quic-kit:client-read-stream)
                   (lambda (&rest arguments)
                     (incf reads)
                     (ensure-true (eq quic (first arguments)))
                     (ensure-true (eq stream (second arguments)))
                     (ensure-equal 1 (getf (cddr arguments) :timeout))
                     (multiple-value-bind (data fin-p) (apply read-stream arguments)
                       (when (and (zerop (length data)) (not fin-p))
                         (incf empty-reads))
                       (values data fin-p))))
             (multiple-value-bind (data fin-p)
                 (http-kit/http3::%http3-quic-adapter-read
                  quic nil stream :timeout 1 :poll-interval 0)
               (ensure-octets-equal payload data)
               (ensure-true fin-p))
             (ensure-equal 2 polls)
             (ensure-equal 2 reads)
             (ensure-equal 1 empty-reads)
             (ensure-equal 0 (cl-quic-kit:stream-readable-bytes stream)))
        (setf (symbol-function 'cl-quic-kit:client-poll) poll
              (symbol-function 'cl-quic-kit:client-read-stream) read-stream)
        (cl-quic-kit:client-close quic))
      (ensure-true (eq poll (symbol-function 'cl-quic-kit:client-poll)))
      (ensure-true (eq read-stream (symbol-function 'cl-quic-kit:client-read-stream))))))

(deftest http3-quic-adapter-write-resumes-on-flow-credit
  (http-kit/http3::%http3-quic-adapter-configure-protection)
  (let* ((writes nil)
         (connection (cl-quic-kit:make-quic-connection
                      :now-fn (constantly 0)
                      :io-write (lambda (ignored bytes)
                                  (declare (ignore ignored))
                                  (push bytes writes))))
         (quic (cl-quic-kit:make-quic-client
                :connection connection :hostname "example.test"
                :local-connection-id (octets 1 2 3 4)
                :destination-connection-id (octets 5 6 7 8)
                :now-fn (constantly 0)))
         (stream (cl-quic-kit:client-open-stream quic nil))
         (flow (cl-quic-kit::quic-client-flow-control quic))
         (payload (octets 17 23 42))
         (poll (symbol-function 'cl-quic-kit:client-poll))
         (polls 0))
    (setf (cl-quic-kit::quic-client-started-p quic) t
          (cl-quic-kit::stream-send-max-offset stream) 0
          (cl-quic-kit::flow-control-state-connection-max-data flow) 0)
    (cl-quic-kit:connection-set-state connection :established)
    (unwind-protect
         (progn
           (cl-quic-kit:client-write-stream quic stream payload :fin-p t)
           (ensure-true (cl-quic-kit:client-stream-write-pending-p quic stream))
           (setf (symbol-function 'cl-quic-kit:client-poll)
                 (lambda (client &optional at)
                   (ensure-true (eq quic client))
                   (prog1 (funcall poll client at)
                     (case (incf polls)
                       (1
                        (cl-quic-kit::%client-receive-frames
                         client :1-rtt 1
                         (list (cl-quic-kit:make-frame :max-data :maximum 3)))
                        (ensure-equal 3 (cl-quic-kit:flow-control-connection-max-data flow))
                        (ensure-equal 0 (cl-quic-kit::stream-send-offset stream))
                        (ensure-true (cl-quic-kit:client-stream-write-pending-p client stream)))
                       (2
                        (cl-quic-kit::%client-receive-frames
                         client :1-rtt 2
                         (list (cl-quic-kit:make-frame
                                :max-stream-data :stream-id (cl-quic-kit:stream-id stream)
                                :maximum 3))))))))
           (ensure-true
            (eq quic (http-kit/http3::%http3-quic-adapter-await-stream-write
                      quic stream :timeout 1)))
           (ensure-equal 2 polls)
           (ensure-false (cl-quic-kit:client-stream-write-pending-p quic stream))
           (ensure-equal 3 (cl-quic-kit::stream-send-offset stream))
           (ensure-equal 3 (cl-quic-kit:flow-control-connection-sent flow))
           (let ((frames (remove-if-not
                          (lambda (frame) (eq :stream (cl-quic-kit:frame-type frame)))
                          (mapcar #'cl-quic-kit:decode-frame (reverse writes)))))
             (ensure-equal 1 (length frames))
             (let ((frame (first frames)))
               (ensure-equal (cl-quic-kit:stream-id stream)
                             (cl-quic-kit:frame-field frame :stream-id))
               (ensure-equal 0 (cl-quic-kit:frame-field frame :offset))
               (ensure-octets-equal payload (cl-quic-kit:frame-field frame :data))
               (ensure-true (cl-quic-kit:frame-field frame :fin)))))
      (setf (symbol-function 'cl-quic-kit:client-poll) poll)
      (cl-quic-kit:client-close quic))
    (ensure-true (eq poll (symbol-function 'cl-quic-kit:client-poll)))))

(deftest http3-quic-adapter-factory-options-and-timeout-cleanup
  (let* ((constructor (symbol-function 'cl-quic-kit:make-quic-client))
         (provider (cl-tls-kit:make-cl-crypto-kit-provider))
         (clock (constantly 0))
         (local-id (octets 1 2 3 4))
         (destination-id (octets 5 6 7 8))
         (alpn (list "h3"))
         (algorithms (vector #x0804 #x0403))
         (writes nil)
         (closed 0)
         (write (lambda (ignored bytes)
                  (declare (ignore ignored)) (push bytes writes)))
         (close (lambda (&rest ignored)
                  (declare (ignore ignored)) (incf closed)))
         (arguments nil)
         (quic nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'cl-quic-kit:make-quic-client)
                 (lambda (&rest options)
                   (setf arguments options quic (apply constructor options))))
           (call-with-http3-quic-lifecycle-spies
            (lambda (calls)
              (ensure-http3-quic-adapter-timeout
               (lambda ()
                 (http-kit/http3:make-http3-quic-adapter
                  :hostname "example.test" :alpn alpn :tls-provider provider
                  :tls-signature-algorithms algorithms :now-fn clock
                  :local-connection-id local-id :destination-connection-id destination-id
                  :idle-timeout 17 :io-write write :on-close close :timeout 0))
               :connect-p t)
              (ensure-equal (list (list quic)) (gethash 'cl-quic-kit:client-start calls))
              (ensure-equal 1 (length (gethash 'cl-quic-kit:client-close calls)))
              (ensure-false (gethash 'cl-quic-kit:client-poll calls))))
           (dolist (option (list (cons :tls-provider provider) (cons :now-fn clock)
                                (cons :alpn alpn) (cons :tls-signature-algorithms algorithms)
                                (cons :local-connection-id local-id)
                                (cons :destination-connection-id destination-id)
                                (cons :io-write write) (cons :on-close close)))
             (ensure-true (eq (cdr option) (getf arguments (car option)))))
           (ensure-equal "example.test" (getf arguments :hostname))
           (ensure-equal 17 (getf arguments :idle-timeout))
           (let ((driver (cl-quic-kit::quic-client-tls-driver quic)))
             (ensure-true (eq provider (cl-tls-kit::tls13-client-driver-provider driver)))
             (ensure-equal "example.test" (cl-tls-kit::tls13-client-driver-hostname driver))
             (ensure-equal :required (cl-tls-kit::tls13-client-driver-verify driver))
             (ensure-true (eq algorithms (cl-tls-kit::tls13-client-driver-signature-algorithms driver))))
           (ensure-true (cl-quic-kit::quic-client-started-p quic))
           (ensure-true (cl-quic-kit::quic-client-closed-p quic))
           (ensure-true (some (lambda (bytes) (plusp (length bytes))) writes))
           ;; The close callback runs after draining, not when cleanup initiates close.
           (ensure-equal :closing
                         (cl-quic-kit:connection-state
                          (cl-quic-kit:quic-client-connection quic)))
           (ensure-equal 0 closed))
      (setf (symbol-function 'cl-quic-kit:make-quic-client) constructor)
      (when (and quic (not (cl-quic-kit::quic-client-closed-p quic)))
        (cl-quic-kit:client-close quic)))
    (ensure-true (eq constructor (symbol-function 'cl-quic-kit:make-quic-client)))))

(deftest http3-quic-adapter-factory-tls-refusal-cleanup
  (let ((constructor (symbol-function 'cl-quic-kit:make-quic-client))
        (poll (symbol-function 'cl-quic-kit:client-poll))
        (quic nil)
        (tls-failure nil)
        (caught nil)
        (polls 0))
    (unwind-protect
         (progn
           (setf (symbol-function 'cl-quic-kit:make-quic-client)
                 (lambda (&rest arguments)
                   (setf quic (apply constructor arguments))))
           (setf (symbol-function 'cl-quic-kit:client-poll)
                 (lambda (client &optional at)
                   (prog1 (funcall poll client at)
                     (incf polls)
                     ;; Finished before ServerHello exercises the actual TLS state machine.
                     (handler-bind ((cl-tls-kit:tls13-client-driver-error
                                      (lambda (condition) (setf tls-failure condition))))
                       (cl-quic-kit::%client-receive-frames
                        client :initial 1
                        (list (cl-quic-kit:make-frame
                               :crypto :offset 0
                               :data (concatenate '(vector (unsigned-byte 8))
                                                  (octets 20 0 0 32)
                                                  (make-array 32 :element-type '(unsigned-byte 8)
                                                                 :initial-element 0)))))))))
           (call-with-http3-quic-lifecycle-spies
            (lambda (calls)
              (handler-case
                  (http-kit/http3:make-http3-quic-adapter
                   :hostname "example.test"
                   :local-connection-id (octets 1 2 3 4)
                   :destination-connection-id (octets 5 6 7 8) :timeout 1)
                (http-connection-error (condition) (setf caught condition)))
              (ensure-equal (list (list quic)) (gethash 'cl-quic-kit:client-start calls))
              (ensure-equal (list (list quic)) (gethash 'cl-quic-kit:client-poll calls))
              (ensure-equal 1 (length (gethash 'cl-quic-kit:client-close calls)))))
           (ensure-true (typep tls-failure 'cl-tls-kit:tls13-client-driver-error))
           (ensure-equal :unexpected-message
                         (cl-tls-kit:tls13-client-driver-error-reason tls-failure))
           (ensure-true (typep caught 'http-connection-error))
           (ensure-false (http-connection-error-request-sent-p caught))
           (let ((cause (http-connection-error-cause caught)))
             (ensure-true (typep cause 'http-protocol-error))
             (ensure-equal :http3-quic-adapter (http-error-operation cause))
             (ensure-equal (cl-tls-kit:tls13-client-driver-error-reason tls-failure)
                           (http-protocol-error-detail cause)))
           (ensure-equal 1 polls)
           (ensure-true (cl-quic-kit::quic-client-closed-p quic)))
      (setf (symbol-function 'cl-quic-kit:make-quic-client) constructor
            (symbol-function 'cl-quic-kit:client-poll) poll)
      (when (and quic (not (cl-quic-kit::quic-client-closed-p quic)))
        (cl-quic-kit:client-close quic)))
    (ensure-true (eq constructor (symbol-function 'cl-quic-kit:make-quic-client)))
    (ensure-true (eq poll (symbol-function 'cl-quic-kit:client-poll)))))

(deftest http3-quic-adapter-expired-read-deadlines
  (dolist (limits '((:timeout 0) (:deadline 0)))
    (let* ((quic (make-http3-quic-adapter-test-client))
           (stream (cl-quic-kit:client-open-stream quic nil)))
      (unwind-protect
           (progn
             (ensure-equal 0 (cl-quic-kit:stream-readable-bytes stream))
             (ensure-http3-quic-adapter-timeout
              (lambda ()
                (apply #'http-kit/http3::%http3-quic-adapter-read
                       quic nil stream :poll-interval 0 limits))))
        (cl-quic-kit:client-close quic)))))

(deftest http3-quic-adapter-expired-write-flow-control-deadlines
  (dolist (limits '((:timeout 0) (:deadline 0)))
    (let* ((quic (make-http3-quic-adapter-test-client))
           (stream (cl-quic-kit:client-open-stream quic nil))
           (flow (cl-quic-kit::quic-client-flow-control quic)))
      (unwind-protect
           (progn
             (setf (cl-quic-kit::flow-control-state-connection-max-data flow) 0)
             (cl-quic-kit:client-write-stream quic stream (octets 1))
             (ensure-true (cl-quic-kit:client-stream-write-pending-p quic stream))
             (ensure-http3-quic-adapter-timeout
              (lambda ()
                (apply #'http-kit/http3::%http3-quic-adapter-await-stream-write
                       quic stream limits)))
             (ensure-true (cl-quic-kit:client-stream-write-pending-p quic stream)))
        (cl-quic-kit:client-close quic)))))

(deftest http3-quic-adapter-expired-peer-control-deadlines
  (dolist (limits '((:timeout 0) (:deadline 0)))
    (let ((quic (make-http3-quic-adapter-test-client)))
      (unwind-protect
           (ensure-http3-quic-adapter-timeout
            (lambda ()
              (apply #'http-kit/http3::%http3-quic-adapter-await-peer-control-data
                     quic limits)))
        (cl-quic-kit:client-close quic)))))

(deftest http3-quic-adapter-expired-peer-settings-deadlines
  (dolist (limits '((:timeout 0) (:deadline 0)))
    (let* ((quic (make-http3-quic-adapter-test-client))
           (client (http-kit/http3::%make-http3-client
                    :peer-control-state
                    (http-kit/http3:make-http3-control-state :peer-role :server))))
      (unwind-protect
           (ensure-http3-quic-adapter-timeout
            (lambda ()
              (apply #'http-kit/http3::%http3-quic-adapter-await-peer-settings
                     quic client limits)))
        (http-kit/http3:close-http3-client client)
        (cl-quic-kit:client-close quic)))))

(deftest http3-quic-adapter-closed-peer-remains-protocol-error
  (let* ((quic (make-http3-quic-adapter-test-client :established-p nil))
         (client (http-kit/http3::%make-http3-client
                  :peer-control-state
                  (http-kit/http3:make-http3-control-state :peer-role :server))))
    (unwind-protect
         (progn
           (cl-quic-kit:client-close quic)
           (signals http-protocol-error
             (http-kit/http3::%http3-quic-adapter-await-established quic))
           (signals http-protocol-error
             (http-kit/http3::%http3-quic-adapter-await-peer-control-data quic))
           (signals http-protocol-error
             (http-kit/http3::%http3-quic-adapter-await-peer-settings quic client)))
      (http-kit/http3:close-http3-client client)
      (cl-quic-kit:client-close quic))))

(deftest http3-quic-adapter-settings-failure-closes-clients
  (let* ((quic (make-http3-quic-adapter-test-client))
         (client (http-kit/http3::%make-http3-client
                  :peer-control-state
                  (http-kit/http3:make-http3-control-state :peer-role :server)))
         (adapter (http-kit/http3::%make-http3-quic-adapter :quic-client quic)))
    (unwind-protect
         (progn
           (ensure-http3-quic-adapter-timeout
            (lambda ()
             (http-kit/http3::%http3-quic-adapter-initialize
              adapter
              (lambda ()
                (setf (http-kit/http3:http3-quic-adapter-http3-client adapter) client)
                (http-kit/http3::%http3-quic-adapter-await-peer-settings
                 quic client :timeout 0)))))
           (ensure-true (cl-quic-kit::quic-client-closed-p quic))
           (ensure-true (not (http-kit/http3:http3-client-open-p client)))
           (ensure-true (http-kit/http3::http3-quic-adapter-closed-p adapter)))
      (http-kit/http3:close-http3-quic-adapter adapter))))

(deftest http3-quic-adapter-success-keeps-clients-open
  (let* ((quic (make-http3-quic-adapter-test-client))
         (client (http-kit/http3::%make-http3-client))
         (adapter (http-kit/http3::%make-http3-quic-adapter :quic-client quic)))
    (unwind-protect
         (progn
           (ensure-true
            (eq adapter
                (http-kit/http3::%http3-quic-adapter-initialize
                 adapter (lambda ()
                           (setf (http-kit/http3:http3-quic-adapter-http3-client
                                  adapter) client)
                           adapter))))
           (ensure-true (not (cl-quic-kit::quic-client-closed-p quic)))
           (ensure-true (http-kit/http3:http3-client-open-p client)))
      (http-kit/http3:close-http3-quic-adapter adapter))))

(deftest http3-quic-adapter-expired-handshake-deadlines
  (dolist (limits '((:timeout 0) (:deadline 0)))
    (let* ((quic (make-http3-quic-adapter-test-client :established-p nil))
           (adapter (http-kit/http3::%make-http3-quic-adapter :quic-client quic)))
      (unwind-protect
           (progn
             (ensure-http3-quic-adapter-timeout
              (lambda ()
                (http-kit/http3::%http3-quic-adapter-initialize
                 adapter
                 (lambda ()
                   (http-kit/http3::%http3-quic-adapter-connect
                    (lambda ()
                      (cl-quic-kit:client-start quic)
                      (apply #'http-kit/http3::%http3-quic-adapter-await-established
                             quic limits)))
                   (error "HTTP/3 initialization must not run after a failed handshake."))))
              :connect-p t)
             (ensure-true (cl-quic-kit::quic-client-closed-p quic))
             (ensure-equal 0 (hash-table-count (cl-quic-kit::quic-client-streams quic))))
        (http-kit/http3:close-http3-quic-adapter adapter)))))

(deftest http3-quic-adapter-handshake-failure-is-unsent
  (let* ((quic (make-http3-quic-adapter-test-client :established-p nil))
         (adapter (http-kit/http3::%make-http3-quic-adapter :quic-client quic))
         (failure nil))
    (unwind-protect
         (progn
           (handler-case
               (http-kit/http3::%http3-quic-adapter-initialize
                adapter
                (lambda ()
                  (http-kit/http3::%http3-quic-adapter-connect
                   (lambda ()
                     (cl-quic-kit:client-close quic)
                     (http-kit/http3::%http3-quic-adapter-await-established
                      quic)))
                  (error "HTTP/3 initialization must not run after a failed handshake.")))
             (http-connection-error (condition)
               (setf failure condition)))
           (ensure-true failure)
           (ensure-true (cl-quic-kit::quic-client-closed-p quic))
           (ensure-true (not (http-connection-error-request-sent-p failure)))
           (ensure-true (typep (http-connection-error-cause failure) 'http-protocol-error))
           (ensure-equal 0 (hash-table-count (cl-quic-kit::quic-client-streams quic))))
      (http-kit/http3:close-http3-quic-adapter adapter))))

(deftest http3-quic-adapter-request-failures-remain-sent
  (dolist (phase '(:write :body))
    (let* ((quic (make-http3-quic-adapter-test-client))
           (adapter (http-kit/http3::%make-http3-quic-adapter :quic-client quic))
           (failure (make-condition 'http-connection-error :message "Request failure."))
           (request-writes 0)
           (client nil)
           (caught nil))
      (unwind-protect
           (progn
             (http-kit/http3::%http3-quic-adapter-initialize
              adapter
              (lambda ()
                (http-kit/http3::%http3-quic-adapter-connect
                 (lambda ()
                   (http-kit/http3::%http3-quic-adapter-await-established quic :timeout 0)))
                (setf client
                      (http-kit/http3:make-http3-client
                       :open-stream
                       (lambda (request &key stream-type timeout deadline)
                         (declare (ignore request timeout deadline))
                         (values (make-http3-test-stream :kind stream-type) 0))
                       :write-stream
                       (lambda (stream octets &key fin-p timeout deadline)
                         (declare (ignore octets fin-p timeout deadline))
                         (when (eq (http3-test-stream-kind stream) :request)
                           (incf request-writes)
                           (when (eq phase :write)
                             (error failure))))
                       :read-stream
                       (lambda (&rest arguments)
                         (declare (ignore arguments))
                         (values (octets) t))
                       :close-stream
                       (lambda (stream &key condition)
                         (declare (ignore condition))
                         (setf (http3-test-stream-closed-p stream) t))))
                (setf (http-kit/http3:http3-quic-adapter-http3-client adapter) client)
                adapter))
             (handler-case
                 (http-kit/http3:send-http3-request
                  client (make-http-request :method "POST" :uri "https://example.test/resource")
                  :request-body-function
                  (lambda (maximum-size)
                    (declare (ignore maximum-size))
                    (ensure-true (plusp request-writes))
                    (error failure)))
               (http-connection-error (condition)
                 (setf caught condition)))
             (ensure-true (plusp request-writes))
             (ensure-true (eq failure caught))
             (ensure-true (http-connection-error-request-sent-p caught)))
        (http-kit/http3:close-http3-quic-adapter adapter)))))

(deftest http3-quic-adapter-peer-control-stream-id
  (dolist (id '(3 7))
    (let* ((quic (make-http3-quic-adapter-test-client))
           (stream (cl-quic-kit:make-stream id)))
      (unwind-protect
           (progn
             (cl-quic-kit:stream-receive-data stream 0 (octets 0 4 0))
             (setf (gethash id (cl-quic-kit::quic-client-streams quic)) stream)
             (ensure-true
              (eq quic
                  (http-kit/http3::%http3-quic-adapter-await-peer-control-data
                   quic :timeout 0))))
        (cl-quic-kit:client-close quic)))))

(deftest http3-quic-adapter-qpack-before-control
  (let* ((quic (make-http3-quic-adapter-test-client))
         (encoder (cl-quic-kit:make-stream 3))
         (control (cl-quic-kit:make-stream 7))
         (polls 0)
         (reads 0)
         (table (http-message-kit:make-qpack-context :max-table-capacity 0 :table-capacity 0 :huffman nil))
         (client
           (http-kit/http3::%make-http3-client
            :qpack-encoder-context table
            :qpack-decoder-context table
            :peer-control-state
            (http-kit/http3:make-http3-control-state :peer-role :server)
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore timeout deadline))
              (incf reads)
              (ensure-true (plusp (cl-quic-kit:stream-readable-bytes stream)))
              (cl-quic-kit:client-read-stream quic stream)))))
    (unwind-protect
         (progn
           (ensure-true
            (eq client
                (http-kit/http3::%http3-quic-adapter-await-peer-settings
                 quic client :timeout 1
                 :poll
                 (lambda (ignored-client)
                   (declare (ignore ignored-client))
                   (incf polls)
                   (case polls
                     (1
                      (cl-quic-kit:stream-receive-data encoder 0 (octets 2))
                      (setf (gethash 3 (cl-quic-kit::quic-client-streams quic)) encoder))
                     (2
                      (cl-quic-kit:stream-receive-data encoder 1 (octets 32))
                      (setf (gethash 7 (cl-quic-kit::quic-client-streams quic)) control))
                     (3
                      (cl-quic-kit:stream-receive-data control 0 (octets 0 4 0))))))))
           (ensure-equal 3 polls)
           (ensure-equal 2 reads)
           (ensure-equal 2 (http-kit/http3::http3-client-peer-unidirectional-stream-count
                            client))
           (ensure-true (eq encoder
                            (http-kit/http3:http3-client-peer-qpack-encoder-stream client)))
           (ensure-true (eq control
                            (http-kit/http3:http3-client-peer-control-stream client)))
           (ensure-equal 1 (cl-quic-kit:stream-readable-bytes encoder)))
      (cl-quic-kit:client-close quic))))

(deftest http3-quic-adapter-fragmented-prefix-does-not-block-control
  (let* ((quic (make-http3-quic-adapter-test-client))
         (incomplete (cl-quic-kit:make-stream 3))
         (control (cl-quic-kit:make-stream 7))
         (reads 0)
         (table (http-message-kit:make-qpack-context :max-table-capacity 0 :table-capacity 0 :huffman nil))
         (client
           (http-kit/http3::%make-http3-client
            :qpack-encoder-context table
            :qpack-decoder-context table
            :peer-control-state
            (http-kit/http3:make-http3-control-state :peer-role :server)
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore timeout deadline))
              (incf reads)
              (ensure-true (eq stream control))
              (cl-quic-kit:client-read-stream quic stream)))))
    (unwind-protect
         (progn
           (cl-quic-kit:stream-receive-data incomplete 0 (octets 64))
           (cl-quic-kit:stream-receive-data control 0 (octets 0 4 0))
           (setf (gethash 3 (cl-quic-kit::quic-client-streams quic)) incomplete
                 (gethash 7 (cl-quic-kit::quic-client-streams quic)) control)
           (ensure-true
            (eq client
                (http-kit/http3::%http3-quic-adapter-await-peer-settings
                 quic client :timeout 1
                 :poll (lambda (ignored-client) (declare (ignore ignored-client))))))
           (ensure-equal 1 reads)
           (ensure-equal 1 (http-kit/http3::http3-client-peer-unidirectional-stream-count
                            client))
           (ensure-equal 1 (cl-quic-kit:stream-readable-bytes incomplete)))
      (cl-quic-kit:client-close quic))))

(deftest http3-quic-adapter-fragmented-settings
  (let* ((quic (make-http3-quic-adapter-test-client))
         (control (cl-quic-kit:make-stream 3))
         (polls 0)
         (reads 0)
         (table (http-message-kit:make-qpack-context :max-table-capacity 0 :table-capacity 0 :huffman nil))
         (client
           (http-kit/http3::%make-http3-client
            :qpack-encoder-context table
            :qpack-decoder-context table
            :peer-control-state
            (http-kit/http3:make-http3-control-state :peer-role :server)
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore timeout deadline))
              (incf reads)
              (ensure-true (plusp (cl-quic-kit:stream-readable-bytes stream)))
              (cl-quic-kit:client-read-stream quic stream)))))
    (unwind-protect
         (progn
           (setf (gethash 3 (cl-quic-kit::quic-client-streams quic)) control)
           (ensure-true
            (eq client
                (http-kit/http3::%http3-quic-adapter-await-peer-settings
                 quic client :timeout 1
                 :poll
                 (lambda (ignored-client)
                   (declare (ignore ignored-client))
                   (incf polls)
                   (case polls
                     (1 (cl-quic-kit:stream-receive-data control 0 (octets 0)))
                     (3 (cl-quic-kit:stream-receive-data control 1 (octets 4)))
                     (5 (cl-quic-kit:stream-receive-data control 2 (octets 0))))))))
           (ensure-equal 5 polls)
           (ensure-equal 3 reads)
           (ensure-true
            (http-kit/http3:http3-control-state-settings-received-p
             (http-kit/http3::http3-client-peer-control-state client))))
      (cl-quic-kit:client-close quic))))

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
             (http3-test-qpack-encode request-fields
              :huffman t))))
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
             (http3-test-qpack-encode (list (cons "x-request-trailer" "done"))
              :huffman t))))
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
           (http3-test-qpack-decode (http-kit/http3:http3-frame-payload (first frames))))
          (ensure-equal (octets 111 107)
                        (http-kit/http3:http3-frame-payload
                         (second frames)))
          (ensure-equal
           (list (cons "x-response-trailer" "done"))
           (http3-test-qpack-decode (http-kit/http3:http3-frame-payload (third frames)))))))))

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
              (http3-test-qpack-encode request-fields)))
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
      (ensure-true (eq response returned-response)))
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
          (ensure-true (< position
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
             (http3-test-qpack-encode (list (cons ":status" "204"))))))
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
              (http3-test-qpack-encode (list (cons ":status" "103")
                     (cons "link" "</style.css>; rel=preload")))))
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-headers-type+
              :payload
              (http3-test-qpack-encode (list (cons ":status" "199")))))
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-headers-type+
              :payload
              (http3-test-qpack-encode (list (cons ":status" "204")))))))
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
             (http3-test-qpack-encode (list (cons ":status" "204"))))))
         (stream (make-http3-test-stream :kind :request))
         (writes '())
         (producer-limits '())
         (chunks (list #(1 2) #(3) nil))
         (client
           (http-kit/http3:make-http3-client
            :max-frame-size 64
            :open-stream
            (lambda (request &key stream-type timeout deadline)
              (declare (ignore request timeout deadline))
              (if (eq stream-type :request)
                  (values stream 0)
                  (values (make-http3-test-stream :kind stream-type) 0)))
            :write-stream
            (lambda (written-stream octets &key fin-p timeout deadline)
              (declare (ignore timeout deadline))
              (when (eq stream written-stream)
                (push (list (subseq octets 0) fin-p) writes)))
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
    (ensure-equal '(64 64 64) (nreverse producer-limits))
    (let* ((ordered-writes (nreverse writes))
           (header-frame
             (first (http-kit/http3:decode-http3-frames
                     (first (first ordered-writes)))))
           (request-fields
             (http3-test-qpack-decode (http-kit/http3:http3-frame-payload header-frame)))
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
      (ensure-equal '(nil nil nil t) (mapcar #'second ordered-writes)))))

(deftest http3-client-rejects-invalid-streaming-request-bodies
  (let ((opened 0))
    (labels ((open-stream (request &key stream-type timeout deadline)
             (declare (ignore request stream-type timeout deadline))
               (when (eq stream-type :request)
                 (incf opened))
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
             (http3-test-qpack-encode (list (cons ":method" "GET")
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
               (http3-test-qpack-decode (http-kit/http3:http3-frame-payload information-frame)))
             (final-fields
               (http3-test-qpack-decode (http-kit/http3:http3-frame-payload final-frame))))
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

(deftest http3-qpack-instruction-and-field-accounting-limits
  (let ((table (http-message-kit:make-qpack-context :max-table-capacity 0 :table-capacity 0 :huffman nil)))
    (signals http-protocol-error
      (http-message-kit:qpack-decode-encoder-stream table (octets #x40 #x00 #x00)
       :allow-incomplete-p t
       :max-instruction-bytes 2))
    (signals http-protocol-error
      (http-message-kit:qpack-decode-decoder-stream (http-message-kit:make-qpack-context :max-table-capacity 0 :table-capacity 0 :huffman nil) (octets #x40 #x00) :allow-incomplete-p t :max-buffer-bytes 1)))
  (let ((section
          (http3-test-qpack-encode (list (cons "x" "y")))))
    (signals http-size-limit-exceeded
      (http3-test-qpack-decode section :max-header-bytes 32)))
  (ensure-equal 34
                (+ 32 (length (octets-as-string (octets 120)))
                   (length (octets-as-string (octets 121))))))

(deftest http3-qpack-decoder-transport-limits
  (flet ((read-decoder-stream (instruction max-instruction-bytes max-buffer-bytes)
           (let* ((wire
                    (http3-test-concat-octets
                     (http-kit/http3:http3-qpack-decoder-stream-prefix)
                     instruction))
                  (stream
                    (make-http3-test-stream
                     :kind :peer-qpack-decoder
                     :reads (list (list wire nil))))
                  (client
                    (http-kit/http3::%make-http3-client
                     :open-p t
                     :qpack-max-instruction-bytes max-instruction-bytes
                     :qpack-max-buffer-bytes max-buffer-bytes
                     :read-stream
                     (lambda (read-stream &key timeout deadline)
                       (declare (ignore timeout deadline))
                       (let ((entry (pop (http3-test-stream-reads
                                          read-stream))))
                         (values (first entry) (second entry)))))))
             (http-kit/http3:attach-http3-peer-qpack-decoder-stream
              client stream)
             (http-kit/http3:read-http3-qpack-decoder-stream client))))
    (multiple-value-bind (events ended-p)
        (read-decoder-stream
         (http-message-kit:qpack-encode-section-ack 0)
         1 2)
      (ensure-equal '((:section-ack 0)) events)
      (ensure-equal nil ended-p))
    (signals http-protocol-error
      (read-decoder-stream
       (http-message-kit:qpack-encode-stream-cancellation 128)
       1 2))
    (multiple-value-bind (events ended-p)
        (read-decoder-stream
         (http-message-kit:qpack-encode-section-ack 0)
         2 1)
      (ensure-equal '((:section-ack 0)) events)
      (ensure-equal nil ended-p))
    (signals http-protocol-error
      (read-decoder-stream
       (http-message-kit:qpack-encode-stream-cancellation 128)
       4 1))))

(deftest http3-client-connection-limits-are-configurable
  (let ((client
          (http-kit/http3:make-http3-client
           :max-request-streams 3
           :max-peer-unidirectional-streams 4
           :max-state-bytes 128
           :qpack-max-instruction-bytes 16
           :qpack-max-buffer-bytes 32
           :open-stream (lambda (request &key stream-type timeout deadline)
                          (declare (ignore request stream-type timeout deadline))
                          (list :stream))
           :write-stream (lambda (stream octets &key fin-p timeout deadline)
                           (declare (ignore stream octets fin-p timeout deadline)))
           :read-stream (lambda (stream &key timeout deadline)
                          (declare (ignore stream timeout deadline))
                          (values nil t)))))
    (ensure-equal 3 (http-kit/http3:http3-client-max-request-streams client))
    (ensure-equal 4
                  (http-kit/http3:http3-client-max-peer-unidirectional-streams
                   client))
    (ensure-equal 128 (http-kit/http3:http3-client-max-state-bytes client))
    (ensure-equal 16
                  (http-kit/http3:http3-client-qpack-max-instruction-bytes client))
    (ensure-equal 32
                  (http-kit/http3:http3-client-qpack-max-buffer-bytes client))
    (http-kit/http3:close-http3-client client)))

(defstruct http3-boundary-fixture client streams closes (reads 0))

(defun http3-boundary-fixture (entries &key (max-request-streams 100))
  (let ((fixture (make-http3-boundary-fixture)))
    (setf (http3-boundary-fixture-client fixture)
          (http-kit/http3:make-http3-client
           :max-request-streams max-request-streams
           :open-stream
           (lambda (request &key stream-type timeout deadline)
             (declare (ignore request timeout deadline))
             (let ((stream (make-http3-test-stream
                            :kind stream-type
                            :reads (when (eq stream-type :request)
                                     (copy-list entries)))))
               (when (eq stream-type :request)
                 (push stream (http3-boundary-fixture-streams fixture)))
               (values stream 0)))
           :write-stream
           (lambda (stream bytes &key fin-p timeout deadline)
             (declare (ignore timeout deadline))
             (push (list (copy-seq bytes) fin-p)
                   (http3-test-stream-writes stream)))
           :read-stream
           (lambda (stream &key timeout deadline)
             (declare (ignore timeout deadline))
             (incf (http3-boundary-fixture-reads fixture))
             (let ((entry (pop (http3-test-stream-reads stream))))
               (values (first entry) (second entry))))
           :close-stream
           (lambda (stream &key condition)
             (setf (http3-test-stream-closed-p stream) t)
             (push (cons stream condition)
                   (http3-boundary-fixture-closes fixture)))))
    fixture))

(defun http3-boundary-headers (fields)
  (http-kit/http3:encode-http3-frame
   (http-kit/http3:make-http3-frame
    :type http-kit/http3:+http3-headers-type+
    :payload (http-message-kit:qpack-prepare-header-block
             (mapcar (lambda (field)
                       (make-http-header (car field) (cdr field)))
                     fields)
             :huffman nil))))

(defun http3-boundary-data (text)
  (http-kit/http3:encode-http3-frame
   (http-kit/http3:make-http3-frame
    :type http-kit/http3:+http3-data-type+ :payload (ascii text))))

(defun http3-boundary-error (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error (condition) condition)))

(defun http3-boundary-check-release (fixture condition)
  (let ((stream (first (http3-boundary-fixture-streams fixture))))
    (ensure-true (http3-test-stream-closed-p stream))
    (ensure-true (eq stream (caar (http3-boundary-fixture-closes fixture))))
    (ensure-true (eq condition (cdar (http3-boundary-fixture-closes fixture))))
    (ensure-equal 0 (http-kit/http3::http3-client-request-stream-count
                     (http3-boundary-fixture-client fixture)))))

(deftest http3-boundary-cumulative-body-limit-without-collection
  (dolist (limit '(4 3))
    (let* ((wire (http3-test-concat-octets
                  (http3-boundary-headers '((":status" . "200")
                                            ("content-length" . "4")))
                  (http3-boundary-data "ab") (http3-boundary-data "cd")))
           (fixture (http3-boundary-fixture (list (list wire t))))
           (chunks nil)
           (response nil)
           (failure (http3-boundary-error
                     (lambda ()
                       (setf response
                             (http-kit/http3:send-http3-request
                              (http3-boundary-fixture-client fixture)
                              (make-http-request :method "GET" :uri "https://example.test/")
                              :collect-body-p nil :max-body-bytes limit
                              :on-body-chunk (lambda (chunk) (push chunk chunks))))))))
      (if (= limit 4)
          (progn
            (ensure-equal nil failure)
            (ensure-equal 200 (http-response-status response))
            (ensure-equal 0 (length (http-response-body response)))
            (ensure-equal (list (ascii "ab") (ascii "cd")) (reverse chunks)))
          (progn
            (ensure-true (typep failure 'http-size-limit-exceeded))
            (ensure-equal :body (http-size-limit-exceeded-kind failure))
            (ensure-equal 3 (http-size-limit-exceeded-limit failure))
            (ensure-equal 4 (http-size-limit-exceeded-observed failure))
            (ensure-equal (list (ascii "ab")) chunks)))
      (http3-boundary-check-release fixture failure))))

(deftest http3-boundary-data-after-trailers
  (dolist (late-data-p '(nil t))
    (let* ((wire (http3-test-concat-octets
                  (http3-boundary-headers '((":status" . "200")))
                  (http3-boundary-data "a")
                  (http3-boundary-headers '(("x-end" . "yes")))
                  (if late-data-p (http3-boundary-data "b") (ascii ""))))
           (fixture (http3-boundary-fixture (list (list wire t))))
           (chunks nil)
           (response nil)
           (failure (http3-boundary-error
                     (lambda ()
                       (setf response
                             (http-kit/http3:send-http3-request
                              (http3-boundary-fixture-client fixture)
                              (make-http-request :method "GET" :uri "https://example.test/")
                              :on-body-chunk (lambda (chunk) (push chunk chunks))))))))
      (ensure-equal (list (ascii "a")) chunks)
      (if late-data-p
          (progn
            (ensure-true (typep failure 'http-protocol-error))
            (ensure-printed-contains failure "after response trailers"))
          (progn
            (ensure-equal nil failure)
            (ensure-equal (ascii "a") (http-response-body response))
            (ensure-equal "yes" (http-header-value
                                 (http-response-trailers response) "x-end"))))
      (http3-boundary-check-release fixture failure))))

(deftest http3-boundary-fragmented-response-and-truncated-eof
  (let* ((headers (http3-boundary-headers '((":status" . "200"))))
         (data (http3-boundary-data "ab")))
    (dolist (ending '(:complete :fin :nil))
      (let* ((entries
               (append (list (list headers nil) (list (subseq data 0 1) nil)
                             (list (ascii "") nil))
                       (if (eq ending :complete)
                           (list (list (subseq data 1) t))
                           (list (list (subseq data 1 (1- (length data))) nil)
                                 (list (if (eq ending :fin) (ascii "") nil)
                                       (eq ending :fin))))))
             (fixture (http3-boundary-fixture entries))
             (chunks nil)
             (response nil)
             (failure (http3-boundary-error
                       (lambda ()
                         (setf response
                               (http-kit/http3:send-http3-request
                                (http3-boundary-fixture-client fixture)
                                (make-http-request :method "GET" :uri "https://example.test/")
                                :on-body-chunk (lambda (chunk) (push chunk chunks))))))))
        (ensure-equal (length entries) (http3-boundary-fixture-reads fixture))
        (if (eq ending :complete)
            (progn
              (ensure-equal nil failure)
              (ensure-equal (ascii "ab") (http-response-body response))
              (ensure-equal (list (ascii "ab")) chunks))
            (progn
              (ensure-true (typep failure 'http-protocol-error))
              (ensure-printed-contains failure "truncated frame")
              (ensure-equal nil chunks)))
        (http3-boundary-check-release fixture failure)))))

(deftest http3-boundary-information-before-missing-final-headers
  (dolist (final-p '(t nil))
    (let* ((information (http3-boundary-headers '((":status" . "103")
                                                 ("link" . "</a>"))))
           (fixture (http3-boundary-fixture
                     (list (list information nil)
                           (list (when final-p
                                   (http3-boundary-headers '((":status" . "204"))))
                                 t))))
           (statuses nil)
           (failure (http3-boundary-error
                     (lambda ()
                       (let ((response
                               (http-kit/http3:send-http3-request
                                (http3-boundary-fixture-client fixture)
                                (make-http-request :method "GET" :uri "https://example.test/")
                                :on-information
                                (lambda (response)
                                  (ensure-equal 1 (http3-boundary-fixture-reads fixture))
                                  (ensure-equal 0 (length (http-response-body response)))
                                  (push (http-response-status response) statuses)))))
                         (ensure-equal 204 (http-response-status response)))))))
      (ensure-equal '(103) statuses)
      (ensure-equal 2 (http3-boundary-fixture-reads fixture))
      (if final-p
          (ensure-equal nil failure)
          (progn
            (ensure-true (typep failure 'http-protocol-error))
            (ensure-printed-contains failure "before final response HEADERS")))
      (http3-boundary-check-release fixture failure))))

(deftest http3-boundary-request-stream-limit-releases-capacity
  (let* ((fixture (http3-boundary-fixture
                   (list (list (http3-boundary-headers '((":status" . "204"))) t))
                   :max-request-streams 1))
         (client (http3-boundary-fixture-client fixture))
         (request (make-http-request :method "GET" :uri "https://example.test/")))
    (ensure-equal
     204
     (http-response-status
      (http-kit/http3:send-http3-request
       client request :on-stream-open
       (lambda (stream id cancel)
         (declare (ignore stream id cancel))
         (ensure-equal 1 (http-kit/http3::http3-client-request-stream-count client))
         (let ((failure (http3-boundary-error
                         (lambda () (http-kit/http3:send-http3-request client request)))))
           (ensure-true (typep failure 'http-protocol-error))
           (ensure-equal :h3-excessive-load (http-protocol-error-detail failure)))
         (ensure-equal 1 (length (http3-boundary-fixture-streams fixture)))
         (ensure-equal nil (http3-boundary-fixture-closes fixture))))))
    (http3-boundary-check-release fixture nil)
    (ensure-equal 204 (http-response-status
                       (http-kit/http3:send-http3-request client request)))
    (ensure-equal 2 (length (http3-boundary-fixture-streams fixture)))
    (ensure-equal 2 (length (http3-boundary-fixture-closes fixture)))
    (http3-boundary-check-release fixture nil)))

(deftest http3-boundary-producer-underflow-does-not-send-fin
  (dolist (declared '(2 3))
    (let* ((fixture (http3-boundary-fixture
                     (list (list (http3-boundary-headers '((":status" . "204"))) t))))
           (chunks (list (ascii "ab")))
           (failure (http3-boundary-error
                     (lambda ()
                       (http-kit/http3:send-http3-request
                        (http3-boundary-fixture-client fixture)
                        (make-http-request :method "POST" :uri "https://example.test/")
                        :request-body-length declared
                        :request-body-function (lambda (limit)
                                                 (declare (ignore limit))
                                                 (pop chunks))))))
           (writes (reverse (http3-test-stream-writes
                             (first (http3-boundary-fixture-streams fixture)))))
           (frames (mapcar (lambda (entry)
                             (first (http-kit/http3:decode-http3-frames (first entry))))
                           writes)))
      (ensure-equal (ascii "ab") (http-kit/http3:http3-frame-payload (second frames)))
      (if (= declared 2)
          (progn
            (ensure-equal nil failure)
            (ensure-equal '(nil nil t) (mapcar #'second writes))
            (ensure-equal 0 (length (http-kit/http3:http3-frame-payload (third frames))))
            (ensure-equal 1 (http3-boundary-fixture-reads fixture)))
          (progn
            (ensure-true (typep failure 'http-protocol-error))
            (ensure-equal '(2 3) (http-protocol-error-detail failure))
            (ensure-equal '(nil nil) (mapcar #'second writes))
            (ensure-equal 0 (http3-boundary-fixture-reads fixture))))
      (http3-boundary-check-release fixture failure))))

(deftest http3-boundary-qpack-decoder-eof-classification
  (dolist (ending '(:open :clean :missing-prefix :truncated))
    (let* ((prefix (http-kit/http3:http3-qpack-decoder-stream-prefix))
           (instruction (http-message-kit:qpack-encode-stream-cancellation 128))
           (wire (case ending
                   (:missing-prefix (ascii ""))
                   (:truncated (http3-test-concat-octets
                                 prefix (subseq instruction 0 1)))
                   (t (http3-test-concat-octets
                       prefix (http-message-kit:qpack-encode-section-ack 0)))))
           (stream (make-http3-test-stream :reads (list (list wire (not (eq ending :open))))))
           (client (http-kit/http3::%make-http3-client
                    :open-p t
                    :read-stream (lambda (stream &key timeout deadline)
                                   (declare (ignore timeout deadline))
                                   (let ((entry (pop (http3-test-stream-reads stream))))
                                     (values (first entry) (second entry))))))
           (events nil)
           (failure nil))
      (http-kit/http3:attach-http3-peer-qpack-decoder-stream client stream)
      (setf failure (http3-boundary-error
                     (lambda ()
                       (multiple-value-bind (result ended-p)
                           (http-kit/http3:read-http3-qpack-decoder-stream client)
                         (setf events result)
                         (ensure-equal nil ended-p)))))
      (if (eq ending :open)
          (progn
            (ensure-equal nil failure)
            (ensure-equal '((:section-ack 0)) events)
            (ensure-equal 0 (length (http-kit/http3::http3-client-peer-qpack-decoder-buffer client))))
          (progn
            (ensure-true (typep failure 'http-protocol-error))
            (ensure-equal (if (eq ending :clean) :h3-closed-critical-stream
                             :qpack-decoder-stream-error)
                          (http-protocol-error-detail failure))
            (ensure-equal nil events)))
      (ensure-equal (not (eq ending :open))
                    (http-kit/http3::http3-client-peer-qpack-decoder-fin-p client))
      (ensure-equal (not (eq ending :missing-prefix))
                    (http-kit/http3::http3-client-peer-qpack-decoder-prefix-seen-p client)))))

(deftest http3-varint-invalid-encode-values
  (dolist (value '(-1 #x4000000000000000 1/2 1.0 "64"))
    (let ((failure
            (handler-case
                (progn (http-kit/http3:http3-varint-encode value) nil)
              (http-protocol-error (condition) condition))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :http3-varint (http-error-operation failure))
      (ensure-equal value (http-protocol-error-detail failure))))
  (let ((encoded
          (http-kit/http3:http3-varint-encode #x3fffffffffffffff)))
    (ensure-equal '(unsigned-byte 8) (array-element-type encoded))
    (ensure-equal '(255 255 255 255 255 255 255 255)
                  (coerce encoded 'list))
    (multiple-value-bind (value position)
        (http-kit/http3:http3-varint-decode encoded)
      (ensure-equal #x3fffffffffffffff value)
      (ensure-equal 8 position))))

(deftest http3-varint-invalid-decode-inputs-and-positions
  (dolist (input (list nil '(0) "0" 7 #2A((0))
                      #(-1) #(256) #(1/2) #(#\a) #(64 256)))
    (dolist (allow-incomplete-p '(nil t))
      (let ((failure
              (handler-case
                  (progn
                    (http-kit/http3:http3-varint-decode
                     input :allow-incomplete-p allow-incomplete-p)
                    nil)
                (http-protocol-error (condition) condition))))
        (ensure-true (typep failure 'http-protocol-error))
        (ensure-equal :http3-varint (http-error-operation failure))
        (ensure-equal (type-of input) (http-protocol-error-detail failure)))))
  (dolist (offset '(-1 2 1/2 1.0 nil "0"))
    (dolist (allow-incomplete-p '(nil t))
      (let ((failure
              (handler-case
                  (progn
                    (http-kit/http3:http3-varint-decode
                     #(0) :position offset
                     :allow-incomplete-p allow-incomplete-p)
                    nil)
                (http-protocol-error (condition) condition))))
        (ensure-true (typep failure 'http-protocol-error))
        (ensure-equal :http3-varint (http-error-operation failure))
        (ensure-equal offset (http-protocol-error-detail failure)))))
  (multiple-value-bind (value position)
      (http-kit/http3:http3-varint-decode #(63) :position 0)
    (ensure-equal 63 value)
    (ensure-equal 1 position))
  (signals http-protocol-error
    (http-kit/http3:http3-varint-decode #(63) :position 1))
  (multiple-value-bind (value position)
      (http-kit/http3:http3-varint-decode
       #(63) :position 1 :allow-incomplete-p t)
    (ensure-equal nil value)
    (ensure-equal 1 position)))

(deftest http3-varint-truncated-multibyte-widths
  (dolist (case '((2 #x7f #x3fff)
                 (4 #xbf #x3fffffff)
                 (8 #xff #x3fffffffffffffff)))
    (destructuring-bind (width prefix maximum) case
      (let ((encoded (make-array width :element-type '(unsigned-byte 8)
                                      :initial-element 255)))
        (setf (aref encoded 0) prefix)
        (loop for available from 1 below width
              for input = (http3-test-concat-octets
                           #(9 8) (subseq encoded 0 available))
              do (signals http-protocol-error
                   (http-kit/http3:http3-varint-decode input :position 2))
                 (multiple-value-bind (value position)
                     (http-kit/http3:http3-varint-decode
                      input :position 2 :allow-incomplete-p t)
                   (ensure-equal nil value)
                   (ensure-equal 2 position)))
        (dolist (allow-incomplete-p '(nil t))
          (multiple-value-bind (value position)
              (http-kit/http3:http3-varint-decode
               (http3-test-concat-octets #(9 8) encoded #(42))
               :position 2 :allow-incomplete-p allow-incomplete-p)
            (ensure-equal maximum value)
            (ensure-equal (+ 2 width) position)))))))

(defun http3-transport-boundary-client (&rest options)
  (apply #'http-kit/http3:make-http3-client
         (append
          options
          (list
           :open-stream
           (lambda (request &key stream-type timeout deadline)
             (declare (ignore request timeout deadline))
             (make-http3-test-stream :kind stream-type))
           :write-stream
           (lambda (stream bytes &key fin-p timeout deadline)
             (declare (ignore timeout deadline))
             (push (list (copy-seq bytes) fin-p)
                   (http3-test-stream-writes stream)))
           :read-stream
           (lambda (stream &key timeout deadline)
             (declare (ignore timeout deadline))
             (let ((entry (pop (http3-test-stream-reads stream))))
               (if entry
                   (values (first entry) (second entry))
                   (values nil t))))
           :close-stream
           (lambda (stream &key condition)
             (declare (ignore condition))
             (setf (http3-test-stream-closed-p stream) t))))))

(deftest http3-transport-boundary-settings-copy-and-validation
  (let* ((settings (list (cons 1 128) (cons 7 2) (cons #x21 7)))
         (opened 0)
         (client
           (http3-transport-boundary-client
            :qpack-settings settings
            :open-stream
            (lambda (request &key stream-type timeout deadline)
              (declare (ignore request timeout deadline))
              (incf opened)
              (when (= opened 1)
                (setf (cdr (first settings)) 0
                      (cdr (second settings)) 3
                      (cdr (third settings)) 8))
              (make-http3-test-stream :kind stream-type)))))
    (unwind-protect
         (progn
           (ensure-equal 3 opened)
           (ensure-equal '((1 . 128) (7 . 2) (33 . 7))
                         (http-kit/http3:http3-client-qpack-settings client))
           (ensure-true
            (not (eq (first settings)
                     (first (http-kit/http3:http3-client-qpack-settings client)))))
           (let* ((writes (http3-test-stream-writes
                           (http-kit/http3::http3-client-control-stream client)))
                  (wire (first (first writes))))
             (ensure-equal 1 (length writes))
             (ensure-equal nil (second (first writes)))
             (ensure-equal 0 (aref wire 0))
             (multiple-value-bind (frames remainder)
                 (http-kit/http3:decode-http3-frames (subseq wire 1))
               (ensure-equal 1 (length frames))
               (ensure-equal 0 (length remainder))
               (ensure-equal http-kit/http3:+http3-settings-type+
                             (http-kit/http3:http3-frame-type (first frames)))
               (let ((decoded (http-kit/http3:decode-http3-settings
                               (http-kit/http3:http3-frame-payload (first frames)))))
                 (dolist (pair '((1 . 128) (7 . 2) (33 . 7)))
                   (ensure-equal (cdr pair) (cdr (assoc (car pair) decoded))))))))
      (http-kit/http3:close-http3-client client)))
  (dolist (invalid (list 9 '(7) '((1 . -1)) '((-1 . 0))
                        (list (cons 1 (1+ http-kit/http3::+http3-max-varint+)))))
    (let ((opened 0) (written 0))
      (signals http-protocol-error
        (http3-transport-boundary-client
         :qpack-settings invalid
         :open-stream
         (lambda (request &key stream-type timeout deadline)
           (declare (ignore request timeout deadline))
           (incf opened)
           (make-http3-test-stream :kind stream-type))
         :write-stream
         (lambda (stream bytes &key fin-p timeout deadline)
           (declare (ignore stream bytes fin-p timeout deadline))
           (incf written))))
      (ensure-equal 0 opened)
      (ensure-equal 0 written))))

(deftest http3-transport-boundary-initialization-cleanup
  (let ((opened nil) (closed nil) (written 0) (primary nil))
    (handler-case
        (http3-transport-boundary-client
         :open-stream
         (lambda (request &key stream-type timeout deadline)
           (declare (ignore request timeout deadline))
           (push stream-type opened)
           (unless (eq stream-type :qpack-decoder)
             (make-http3-test-stream :kind stream-type)))
         :write-stream
         (lambda (stream bytes &key fin-p timeout deadline)
           (declare (ignore stream bytes fin-p timeout deadline))
           (incf written))
         :close-stream
         (lambda (stream &key condition)
           (push (cons stream condition) closed)
           (setf (http3-test-stream-closed-p stream) t)
           (when (eq :control (http3-test-stream-kind stream))
             (error "Injected control cleanup failure."))))
      (http-protocol-error (condition) (setf primary condition)))
    (ensure-true (typep primary 'http-protocol-error))
    (ensure-equal '(:control :qpack-encoder :qpack-decoder) (reverse opened))
    (ensure-equal 0 written)
    (ensure-equal '(:control :qpack-encoder)
                  (mapcar (lambda (entry) (http3-test-stream-kind (car entry)))
                          (reverse closed)))
    (dolist (entry closed)
      (ensure-true (eq primary (cdr entry)))
      (ensure-true (http3-test-stream-closed-p (car entry))))))

(deftest http3-transport-boundary-aggregate-state-three-four
  (let* ((client (http3-transport-boundary-client :max-state-bytes 3))
         (control (make-http3-test-stream
                   :reads (list (list (octets #x00 #x04 #x04) nil))))
         (decoder (make-http3-test-stream
                   :reads (list (list (octets #x03 #xff) nil)
                                (list (octets #x80) nil)))))
    (unwind-protect
         (progn
           (http-kit/http3:attach-http3-peer-control-stream client control)
           (http-kit/http3:attach-http3-peer-qpack-decoder-stream client decoder)
           (multiple-value-bind (events fin-p)
               (http-kit/http3:read-http3-control-stream client)
             (ensure-equal nil events)
             (ensure-equal nil fin-p))
           (multiple-value-bind (events fin-p)
               (http-kit/http3:read-http3-qpack-decoder-stream client)
             (ensure-equal nil events)
             (ensure-equal nil fin-p))
           (ensure-equal 2 (length (http-kit/http3::http3-client-peer-control-buffer
                                    client)))
           (ensure-equal 1
                         (length (http-kit/http3::http3-client-peer-qpack-decoder-buffer
                                  client)))
           (let ((failure nil))
             (handler-case
                 (http-kit/http3:read-http3-qpack-decoder-stream client)
               (http-protocol-error (condition) (setf failure condition)))
             (ensure-true (typep failure 'http-protocol-error))
             (ensure-equal '(:h3-excessive-load 3 4)
                           (http-protocol-error-detail failure)))
           (ensure-equal nil (http3-test-stream-reads decoder)))
      (http-kit/http3:close-http3-client client))))

(deftest http3-transport-boundary-peer-stream-cap
  (let* ((reads 0)
         (client
           (http3-transport-boundary-client
            :max-peer-unidirectional-streams 1
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore timeout deadline))
              (incf reads)
              (let ((entry (pop (http3-test-stream-reads stream))))
                (values (first entry) (second entry))))))
         (first (make-http3-test-stream
                 :reads (list (list (http-kit/http3:http3-varint-encode 64) nil))))
         (second (make-http3-test-stream
                  :reads (list (list (http-kit/http3:http3-varint-encode 64) nil)))))
    (unwind-protect
         (progn
           (ensure-equal :unknown
                         (http-kit/http3:accept-http3-peer-unidirectional-stream
                          client first))
           (ensure-equal 1 reads)
           (let ((failure nil))
             (handler-case
                 (http-kit/http3:accept-http3-peer-unidirectional-stream client second)
               (http-protocol-error (condition) (setf failure condition)))
             (ensure-true (typep failure 'http-protocol-error))
             (ensure-equal :h3-excessive-load (http-protocol-error-detail failure)))
           (ensure-equal 1 reads)
           (ensure-equal 1 (length (http3-test-stream-reads second)))
           (ensure-equal 1
                         (http-kit/http3::http3-client-peer-unidirectional-stream-count
                          client)))
      (http-kit/http3:close-http3-client client))))

(deftest http3-transport-boundary-idle-refresh-and-expiry
  (let* ((now 0) (fixtures nil)
         (manager
           (http-kit/http3:make-http3-connection-manager
            :idle-timeout 5 :max-connection-age nil :clock-function (lambda () now)
            :open-client
            (lambda (request &key timeout deadline)
              (declare (ignore request timeout deadline))
              (let ((fixture
                      (http3-boundary-fixture
                       (list (list (http3-boundary-headers '((":status" . "204"))) t)))))
                (push fixture fixtures)
                (http3-boundary-fixture-client fixture))))))
    (unwind-protect
         (progn
           (dolist (time '(0 4 8 13))
             (setf now time)
             (ensure-equal
              204 (http-response-status
                   (http-kit/http3:send-http3-request-over-connection-manager
                    manager (make-http-request :method "GET"
                                               :uri "https://idle.example/resource"))))
             (ensure-equal (if (= time 13) 2 1) (length fixtures))
             (ensure-equal 1 (http-kit/http3:http3-connection-manager-connection-count
                              manager))
             (ensure-true
              (eq (http3-boundary-fixture-client (first fixtures))
                  (first (http-kit/http3:http3-connection-manager-clients manager)))))
           (let ((expired (second fixtures)))
             (ensure-equal nil (http-kit/http3:http3-client-open-p
                                (http3-boundary-fixture-client expired)))
             (ensure-equal 1
                           (count :control (http3-boundary-fixture-closes expired)
                                  :key (lambda (entry)
                                         (http3-test-stream-kind (car entry)))))
             (ensure-equal 3 (length (http3-boundary-fixture-streams expired)))))
      (http-kit/http3:close-http3-connection-manager manager))))

(deftest http3-transport-gap-control-error-preserves-condition
  (let* ((settings (http-kit/http3:encode-http3-frame
                    (http-kit/http3:make-http3-settings-frame
                     :max-field-section-size 4096)))
         (stream (make-http3-test-stream
                  :kind :control
                  :reads (list (list (http3-test-concat-octets
                                      (http-kit/http3:http3-control-stream-prefix)
                                      (subseq settings 0 (1- (length settings))))
                                     t))))
         (secondary (make-condition 'simple-error
                                    :format-control "Injected observer failure."))
         (reads 0) (errors 0) (closes 0)
         (seen-condition nil) (seen-state nil) (closed-condition :unset))
    (let ((primary
            (http3-boundary-error
             (lambda ()
               (http-kit/http3:serve-http3-control-stream
                stream :peer-role :server
                :read-stream
                (lambda (actual &key timeout deadline)
                  (declare (ignore timeout deadline))
                  (ensure-true (eq stream actual))
                  (incf reads)
                  (let ((entry (pop (http3-test-stream-reads actual))))
                    (values (first entry) (second entry))))
                :on-error
                (lambda (condition state)
                  (incf errors)
                  (setf seen-condition condition seen-state state)
                  (error secondary))
                :close-stream
                (lambda (actual &key condition)
                  (ensure-true (eq stream actual))
                  (incf closes)
                  (setf closed-condition condition
                        (http3-test-stream-closed-p actual) t)))))))
      (ensure-true (typep primary 'http-protocol-error))
      (ensure-true (eq primary seen-condition))
      (ensure-true (eq primary closed-condition))
      (ensure-true (not (eq primary secondary)))
      (ensure-true (http-kit/http3:http3-control-state-p seen-state))
      (ensure-equal :server (http-kit/http3:http3-control-state-peer-role seen-state))
      (ensure-equal nil (http-kit/http3:http3-control-state-settings-received-p seen-state))
      (ensure-equal 1 reads)
      (ensure-equal 1 errors)
      (ensure-equal 1 closes)
      (ensure-true (http3-test-stream-closed-p stream)))))

(deftest http3-transport-gap-manager-replaces-failed-client
  (let* ((fixtures nil)
         (opens 0)
         (notifications 0)
         (original (make-condition 'http-protocol-error
                                   :message "Injected request failure."))
         (request (make-http-request :method "GET" :uri "https://example.test/resource"))
         (manager
           (http-kit/http3:make-http3-connection-manager
            :open-client
            (lambda (actual &key timeout deadline)
              (declare (ignore timeout deadline))
              (ensure-true (eq request actual))
              (incf opens)
              (let ((fixture
                      (http3-boundary-fixture
                       (list (list (http3-boundary-headers '((":status" . "204"))) t)))))
                (push fixture fixtures)
                (http3-boundary-fixture-client fixture))))))
    (unwind-protect
         (progn
           (let ((primary
                   (http3-boundary-error
                    (lambda ()
                      (http-kit/http3:send-http3-request-over-connection-manager
                       manager request
                       :on-stream-open
                       (lambda (stream stream-id cancel)
                         (incf notifications)
                         (ensure-true (eq stream (first (http3-boundary-fixture-streams
                                                        (first fixtures)))))
                         (ensure-equal 0 stream-id)
                         (ensure-true (functionp cancel))
                         (http-kit/http3:close-http3-client
                          (http3-boundary-fixture-client (first fixtures))
                          :condition original)
                         (error original)))))))
             (ensure-true (eq original primary)))
           (let ((failed (first fixtures)))
             (ensure-equal 1 opens)
             (ensure-equal 1 notifications)
             (ensure-equal 0 (http3-boundary-fixture-reads failed))
             (ensure-equal 0 (http-kit/http3:http3-connection-manager-connection-count manager))
             (ensure-equal nil (http-kit/http3:http3-client-open-p
                                (http3-boundary-fixture-client failed)))
             (http3-boundary-check-release failed original)
             (ensure-equal 4 (length (http3-boundary-fixture-closes failed)))
             (dolist (kind '(:control :qpack-encoder :qpack-decoder :request))
               (ensure-equal 1 (count kind (http3-boundary-fixture-closes failed)
                                      :key (lambda (entry)
                                             (http3-test-stream-kind (car entry))))))
             (dolist (entry (http3-boundary-fixture-closes failed))
               (ensure-true (eq original (cdr entry))))
             (let ((response (http-kit/http3:send-http3-request-over-connection-manager
                              manager request)))
               (ensure-true (http-response-p response))
               (ensure-equal 204 (http-response-status response))
               (ensure-equal 0 (length (http-response-body response))))
             (ensure-equal 2 opens)
             (ensure-equal 1 notifications)
             (ensure-true (not (eq failed (first fixtures))))
             (ensure-equal 1 (http3-boundary-fixture-reads (first fixtures)))
             (ensure-equal 1 (http-kit/http3:http3-connection-manager-connection-count manager))
             (http3-boundary-check-release (first fixtures) nil)
             (ensure-equal 4 (length (http3-boundary-fixture-closes failed)))))
      (http-kit/http3:close-http3-connection-manager manager))
    (ensure-equal 0 (http-kit/http3:http3-connection-manager-connection-count manager))
    (ensure-equal 4 (length (http3-boundary-fixture-closes (first fixtures))))))

(deftest http3-transport-gap-streaming-response-empty-and-split
  (let* ((body (make-array 65 :element-type '(unsigned-byte 8)))
         (chunks nil)
         (produced 0) (handled 0) (reads 0) (closes 0)
         (closed-condition :unset)
         (stream (make-http3-test-stream
                  :kind :request
                  :reads (list (list (http3-boundary-headers
                                      '((":method" . "GET") (":scheme" . "https")
                                        (":authority" . "example.test") (":path" . "/")))
                                     t)))))
    (dotimes (index (length body)) (setf (aref body index) index))
    (setf chunks (list (octets) body nil))
    (let ((response
            (http-kit/http3:serve-http3-request-stream
             stream
             (lambda (request)
               (incf handled)
               (ensure-equal "GET" (http-request-method request))
               (ensure-equal 0 (length (http-request-body request)))
               (make-http-response-stream
                :status 200 :protocol-version "HTTP/3" :body-length 65
                :headers (list (make-http-header "content-length" "65"))
                :body-function (lambda () (incf produced) (pop chunks))))
             :max-frame-size 64
             :read-stream
             (lambda (actual &key timeout deadline)
               (declare (ignore timeout deadline))
               (ensure-true (eq stream actual))
               (incf reads)
               (let ((entry (pop (http3-test-stream-reads actual))))
                 (values (first entry) (second entry))))
             :write-stream
             (lambda (actual bytes &key fin-p timeout deadline)
               (declare (ignore timeout deadline))
               (ensure-true (eq stream actual))
               (push (list (copy-seq bytes) fin-p) (http3-test-stream-writes actual)))
             :close-stream
             (lambda (actual &key condition)
               (ensure-true (eq stream actual))
               (incf closes)
               (setf closed-condition condition
                     (http3-test-stream-closed-p actual) t)))))
      (ensure-true (http-response-stream-p response))
      (ensure-equal 200 (http-response-stream-status response)))
    (ensure-equal 1 handled)
    (ensure-equal 1 reads)
    (ensure-equal 3 produced)
    (ensure-equal nil chunks)
    (ensure-equal 1 closes)
    (ensure-equal nil closed-condition)
    (ensure-true (http3-test-stream-closed-p stream))
    (let ((writes (reverse (http3-test-stream-writes stream)))
          (frames nil))
      (ensure-equal 3 (length writes))
      (ensure-equal '(nil nil t) (mapcar #'second writes))
      (dolist (write writes)
        (multiple-value-bind (decoded remainder)
            (http-kit/http3:decode-http3-frames (first write))
          (ensure-equal 1 (length decoded))
          (ensure-equal 0 (length remainder))
          (push (first decoded) frames)))
      (setf frames (nreverse frames))
      (ensure-equal (list http-kit/http3:+http3-headers-type+
                          http-kit/http3:+http3-data-type+ http-kit/http3:+http3-data-type+)
                    (mapcar #'http-kit/http3:http3-frame-type frames))
      (let ((fields (http3-test-qpack-decode (http-kit/http3:http3-frame-payload (first frames)))))
        (ensure-equal "200" (cdr (assoc ":status" fields :test #'equal)))
        (ensure-equal "65" (cdr (assoc "content-length" fields :test #'equal))))
      (let ((payloads (mapcar #'http-kit/http3:http3-frame-payload (rest frames))))
        (ensure-equal '(64 1) (mapcar #'length payloads))
        (ensure-equal body (apply #'http3-test-concat-octets payloads))))))

(deftest http3-transport-boundary-lru-a-b-a-c
  (let* ((fixtures nil)
         (manager
           (http-kit/http3:make-http3-connection-manager
            :max-connections 2
            :open-client
            (lambda (request &key timeout deadline)
              (declare (ignore timeout deadline))
              (let ((fixture
                      (http3-boundary-fixture
                       (list (list (http3-boundary-headers '((":status" . "204"))) t)))))
                (push (cons (http-uri-host (http-request-uri request)) fixture) fixtures)
                (http3-boundary-fixture-client fixture))))))
    (unwind-protect
         (labels ((send (host)
                    (ensure-equal
                     204 (http-response-status
                          (http-kit/http3:send-http3-request-over-connection-manager
                           manager (make-http-request
                                    :method "GET"
                                    :uri (format nil "https://~A/resource" host))))))
                  (fixture (host) (cdr (assoc host fixtures :test #'equal))))
           (send "a.example")
           (send "b.example")
           (let ((a (fixture "a.example")) (b (fixture "b.example")))
             (send "a.example")
             (ensure-equal 2 (length fixtures))
             (send "c.example")
             (ensure-equal 3 (length fixtures))
             (ensure-equal 2 (http-kit/http3:http3-connection-manager-connection-count
                              manager))
             (ensure-true (http-kit/http3:http3-client-open-p
                           (http3-boundary-fixture-client a)))
             (ensure-equal nil (http-kit/http3:http3-client-open-p
                                (http3-boundary-fixture-client b)))
             (ensure-equal 1
                           (count :control (http3-boundary-fixture-closes b)
                                  :key (lambda (entry)
                                         (http3-test-stream-kind (car entry)))))
             (ensure-true
              (member (http3-boundary-fixture-client a)
                      (http-kit/http3:http3-connection-manager-clients manager) :test #'eq))
             (ensure-true
              (member (http3-boundary-fixture-client (fixture "c.example"))
                      (http-kit/http3:http3-connection-manager-clients manager) :test #'eq))
             (send "a.example")
             (ensure-equal 3 (length fixtures))
             (ensure-true (eq a (fixture "a.example")))
             (ensure-equal 3 (length (http3-boundary-fixture-streams a)))
             (send "b.example")
             (ensure-equal 4 (length fixtures))
             (ensure-true (not (eq b (fixture "b.example"))))
             (ensure-equal 2 (http-kit/http3:http3-connection-manager-connection-count
                              manager))))
      (http-kit/http3:close-http3-connection-manager manager))))

(defun make-http3-poll-deadline-fixture (clock)
  (http-kit/http3::%http3-quic-adapter-configure-protection)
  (let* ((connection (cl-quic-kit:make-quic-connection :now-fn clock))
         (client (cl-quic-kit:make-quic-client
                  :connection connection :now-fn clock :hostname "example.test"
                  :local-connection-id
                  (make-array 4 :element-type '(unsigned-byte 8)
                                :initial-element 1)
                  :destination-connection-id
                  (make-array 4 :element-type '(unsigned-byte 8)
                                :initial-element 2))))
    (setf (cl-quic-kit::quic-client-started-p client) t)
    (cl-quic-kit:connection-set-state connection :established)
    (let ((stream (cl-quic-kit:client-open-stream client nil)))
      (cl-quic-kit:stream-receive-data
       stream 0 (make-array 1 :element-type '(unsigned-byte 8)
                             :initial-element 90) :fin t)
      (values client stream))))

(defun check-http3-poll-deadline-scenario (initial-now polled-now timeout deadline outcome
                          &optional injected-location)
  (let* ((now initial-now) (armed nil) (polls 0) (reads 0)
         (now-name 'http-kit/http3::%http3-quic-adapter-now)
         (poll-name 'cl-quic-kit:client-poll)
         (read-name 'cl-quic-kit:client-read-stream)
         (original-now (symbol-function now-name))
         (original-poll (symbol-function poll-name))
         (original-read (symbol-function read-name))
         (injected (make-condition 'cl-quic-kit:quic-error
                                   :message "Independent unrelated failure"))
         (client nil) (stream nil) (observed nil) (retained nil)
         (returned nil) (chunk nil) (fin nil))
    (unwind-protect
         (progn
           (multiple-value-setq (client stream)
             (make-http3-poll-deadline-fixture (lambda ()
                                 (when armed (setf now polled-now))
                                 now)))
           (setf (symbol-function now-name) (lambda () now)
                 (symbol-function poll-name)
                 (lambda (&rest arguments)
                   (incf polls)
                   (setf armed t)
                   (when (eq injected-location :poll) (error injected))
                   (apply original-poll arguments))
                 (symbol-function read-name)
                 (lambda (&rest arguments)
                   (incf reads)
                   (when (eq injected-location :read) (error injected))
                   (apply original-read arguments)))
           (handler-case
               (handler-bind ((cl-quic-kit:quic-error
                                (lambda (condition) (setf observed condition))))
                 (multiple-value-setq (chunk fin)
                   (http-kit/http3::%http3-quic-adapter-read
                    client nil stream :timeout timeout :deadline deadline
                    :poll-interval 0))
                 (setf returned t))
             (error (condition) (setf retained condition)))
           (ecase outcome
             (:expired
              (ensure-true (typep retained 'http-kit:http-timeout) :http-timeout)
              (when (typep retained 'http-kit:http-timeout)
                (ensure-true (eq :http3-quic-adapter
                                    (http-kit:http-error-operation retained))
                                :timeout-operation)
                (ensure-true (eq :deadline (http-kit:http-timeout-kind retained))
                                :timeout-kind))
              (ensure-true (not returned) :no-return)
              (ensure-true (zerop reads) :no-lower-read)
              (ensure-true (= polls (if (= initial-now polled-now) 0 1))
                              :poll-count)
              (ensure-true (>= now deadline) :deadline-crossed)
              (ensure-true (zerop (cl-quic-kit:stream-read-offset stream))
                              :unconsumed-offset)
              (ensure-true (= 1 (cl-quic-kit:stream-readable-bytes stream))
                              :unconsumed-data))
             (:success
              (ensure-true returned :returned)
              (ensure-true (null retained) :no-condition)
              (ensure-true (and chunk (= (length chunk) 1) (= (aref chunk 0) 90))
                              :data-byte)
              (ensure-true fin :fin)
              (ensure-true (= polls 1) :one-poll)
              (ensure-true (= reads 1) :one-read)
              (ensure-true (= 1 (cl-quic-kit:stream-read-offset stream))
                              :consumed-offset)
              (ensure-true (zerop (cl-quic-kit:stream-readable-bytes stream))
                              :consumed-data))
             (:unrelated
              (ensure-true (eq injected retained) :condition-identity)
              (ensure-true (eq injected observed) :signaled-identity)
              (ensure-true (typep retained 'cl-quic-kit:quic-error) :quic-condition)
              (ensure-true (not (typep retained 'http-kit:http-timeout))
                              :not-converted)
              (ensure-true (not returned) :no-return)
              (ensure-true (= polls 1) :one-poll)
              (ensure-true (= reads (if (eq injected-location :read) 1 0))
                              :read-count)
              (ensure-true (zerop (cl-quic-kit:stream-read-offset stream))
                              :unconsumed-offset)))
           (ensure-true (not (cl-quic-kit:quic-client-closed-p client))
                           :client-still-open))
      (setf (symbol-function now-name) original-now
            (symbol-function poll-name) original-poll
            (symbol-function read-name) original-read)
      (when client (cl-quic-kit:client-close client)))
    (ensure-true (eq original-now (symbol-function now-name)) :clock-restored)
    (ensure-true (eq original-poll (symbol-function poll-name)) :poll-restored)
    (ensure-true (eq original-read (symbol-function read-name)) :read-restored)))

(deftest http3-quic-adapter-poll-deadline-entry-expired
  (check-http3-poll-deadline-scenario 10 10 10 10 :expired))

(deftest http3-quic-adapter-poll-deadline-crosses
  (check-http3-poll-deadline-scenario 9 11 10 10 :expired))

(deftest http3-quic-adapter-poll-deadline-within
  (check-http3-poll-deadline-scenario 8 9 10 10 :success))

(deftest http3-quic-adapter-poll-deadline-unbounded
  (check-http3-poll-deadline-scenario 9 11 nil nil :success))

(deftest http3-quic-adapter-poll-deadline-unrelated
  (check-http3-poll-deadline-scenario 8 9 10 10 :unrelated :read)
  (check-http3-poll-deadline-scenario 8 9 10 10 :unrelated :poll))


(defun call-with-http3-factory-prefix-fixture (operation &key (poll-interval 0))
  ;; Only the connection/handshake entry is substituted.  Stream creation,
  ;; queues, flushing and peer SETTINGS consumption use the real implementations.
  (let* ((name 'cl-quic-kit:make-quic-client)
         (original (symbol-function name))
         (quic nil)
         (adapter nil)
         (writes nil)
         (connection
           (cl-quic-kit:make-quic-connection
            :now-fn (constantly 0)
            :io-write (lambda (ignored bytes)
                        (declare (ignore ignored))
                        (push bytes writes)))))
    (unwind-protect
         (progn
           (setf (symbol-function name)
                 (lambda (&rest arguments)
                   (setf quic (apply original arguments))
                   (let ((peer-control (cl-quic-kit:make-stream 3)))
                     (cl-quic-kit:stream-receive-data peer-control 0 (octets 0 4 0))
                     (setf (gethash 3 (cl-quic-kit::quic-client-streams quic))
                           peer-control))
                   (setf (cl-quic-kit::quic-client-started-p quic) t)
                   (cl-quic-kit:connection-set-state connection :established)
                   quic))
           (setf adapter
                 (http-kit/http3:make-http3-quic-adapter
                  :connection connection :hostname "example.test"
                  :local-connection-id (octets 1 2 3 4)
                  :destination-connection-id (octets 5 6 7 8)
                  :now-fn (constantly 0) :timeout 1 :poll-interval poll-interval))
           (ensure-true (eq quic
                            (http-kit/http3:http3-quic-adapter-quic-client adapter)))
           (ensure-true
            (http-kit/http3::http3-control-state-settings-received-p
             (http-kit/http3::http3-client-peer-control-state
              (http-kit/http3:http3-quic-adapter-http3-client adapter))))
           (funcall operation
                    (http-kit/http3:http3-quic-adapter-http3-client adapter)
                    quic
                    (lambda ()
                      ;; This is the existing QUIC test's plaintext IO boundary,
                      ;; not a claim about encrypted packets or QPACK semantics.
                      (mapcar #'cl-quic-kit:decode-frame (reverse writes)))))
      (setf (symbol-function name) original)
      (if adapter
          (http-kit/http3:close-http3-quic-adapter adapter)
          (when quic (cl-quic-kit:client-close quic))))
    (ensure-true (eq original (symbol-function name)))
    (ensure-true (cl-quic-kit:quic-client-closed-p quic))))

(defun check-http3-factory-stream-bytes (frames stream expected &key fin-p)
  (let* ((id (cl-quic-kit:stream-id stream))
         (selected
           (remove-if-not
            (lambda (frame)
              (and (eq :stream (cl-quic-kit:frame-type frame))
                   (= id (cl-quic-kit:frame-field frame :stream-id))))
            frames))
         (offset 0)
         (fin-count 0)
         (received (make-array 0 :element-type '(unsigned-byte 8))))
    (ensure-true (plusp (length selected)))
    (dolist (frame selected)
      (ensure-equal offset (cl-quic-kit:frame-field frame :offset 0))
      (let ((data (cl-quic-kit:frame-field frame :data)))
        (incf offset (length data))
        (setf received (http3-test-concat-octets received data)))
      (when (cl-quic-kit:frame-field frame :fin)
        (incf fin-count)
        (ensure-equal (length expected) offset)))
    (ensure-octets-equal expected received)
    (ensure-equal (length expected) offset)
    (ensure-equal offset (cl-quic-kit::stream-send-offset stream))
    (ensure-equal (if fin-p 1 0) fin-count)
    selected))

(deftest http3-quic-adapter-factory-prefix-once
  (call-with-http3-factory-prefix-fixture
   (lambda (client quic frames)
     (cl-quic-kit:client-flush quic)
     (loop for stream in
           (list (http-kit/http3::http3-client-control-stream client)
                 (http-kit/http3:http3-client-qpack-encoder-stream client)
                 (http-kit/http3:http3-client-qpack-decoder-stream client))
           for id in '(2 6 10)
           for expected in (list (octets 0 4 4 7 0 1 0) (octets 2) (octets 3))
           do (ensure-equal id (cl-quic-kit:stream-id stream))
              (let ((selected (check-http3-factory-stream-bytes
                               (funcall frames) stream expected)))
                (ensure-equal 1
                              (count 0 selected
                                     :key (lambda (frame)
                                            (cl-quic-kit:frame-field frame :offset))))
                (ensure-octets-equal
                 (subseq expected 0 1)
                 (cl-quic-kit:frame-field (first selected) :data)))
              (ensure-false (cl-quic-kit:client-stream-write-pending-p quic stream))))))

(deftest http3-quic-adapter-factory-prefix-subsequent-qpack
  (call-with-http3-factory-prefix-fixture
   (lambda (client quic frames)
     (let ((write (http-kit/http3:http3-client-write-stream client))
           (encoder (http-kit/http3:http3-client-qpack-encoder-stream client))
           (decoder (http-kit/http3:http3-client-qpack-decoder-stream client)))
       ;; Prefix-equal leading bytes are content after initialization, even when
       ;; a write is empty or begins with a different byte in between.
       (funcall write encoder (octets) :fin-p nil)
       (funcall write encoder (octets 2 97 98 0) :fin-p nil)
       (funcall write encoder (octets 32) :fin-p nil)
       (funcall write decoder (octets) :fin-p nil)
       (funcall write decoder (octets 3) :fin-p nil)
       (funcall write decoder (octets 128) :fin-p nil)
       (cl-quic-kit:client-flush quic)
       (check-http3-factory-stream-bytes
        (funcall frames) encoder (octets 2 2 97 98 0 32))
       (check-http3-factory-stream-bytes
        (funcall frames) decoder (octets 3 3 128))
       (ensure-false (cl-quic-kit:client-stream-write-pending-p quic encoder))
       (ensure-false (cl-quic-kit:client-stream-write-pending-p quic decoder))))))

(deftest http3-quic-adapter-factory-prefix-bidirectional-unchanged
  (call-with-http3-factory-prefix-fixture
   (lambda (client quic frames)
     (multiple-value-bind (stream id)
         (funcall (http-kit/http3:http3-client-open-stream client)
                  nil :stream-type :request)
       (ensure-equal 0 id)
       (ensure-equal id (cl-quic-kit:stream-id stream))
       (ensure-equal 0 (cl-quic-kit::stream-send-offset stream))
       (funcall (http-kit/http3:http3-client-write-stream client)
                stream (octets 0 2 3 90) :fin-p t)
       (ensure-false (cl-quic-kit:client-stream-write-pending-p quic stream))
       (check-http3-factory-stream-bytes
        (funcall frames) stream (octets 0 2 3 90) :fin-p t)))))

(deftest http3-transport-public-callback-validation-before-io
  (dolist (key '(:open-stream :write-stream :read-stream
                :close-stream :on-control-stream-ready))
    (let ((opened 0) (written 0) (closed 0))
      (let ((failure
              (handler-case
                  (apply #'http3-transport-boundary-client
                         key :not-a-function
                         (list :open-stream
                               (lambda (request &key stream-type timeout deadline)
                                 (declare (ignore request timeout deadline))
                                 (incf opened)
                                 (make-http3-test-stream :kind stream-type))
                               :write-stream
                               (lambda (stream bytes &key fin-p timeout deadline)
                                 (declare (ignore stream bytes fin-p timeout deadline))
                                 (incf written))
                               :close-stream
                               (lambda (stream &key condition)
                                 (declare (ignore stream condition))
                                 (incf closed))))
                (http-protocol-error (condition) condition))))
        (ensure-true (typep failure 'http-protocol-error))
        (ensure-equal :http3-transport (http-error-operation failure))
        (ensure-equal (type-of :not-a-function)
                      (http-protocol-error-detail failure)))
      (ensure-equal 0 opened)
      (ensure-equal 0 written)
      (ensure-equal 0 closed)))
  (let ((client (http3-transport-boundary-client
                 :close-stream nil :cancel-stream nil :on-control-stream-ready nil)))
    (unwind-protect
         (progn
           (ensure-true (http-kit/http3:http3-client-open-p client))
           (dolist (stream (list (http-kit/http3::http3-client-control-stream client)
                                (http-kit/http3::http3-client-qpack-encoder-stream client)
                                (http-kit/http3::http3-client-qpack-decoder-stream client)))
             (ensure-equal 1 (length (http3-test-stream-writes stream)))))
      (ensure-true (http-kit/http3:close-http3-client client)))))

(deftest http3-transport-public-positive-limit-boundaries
  (dolist (key '(:max-frame-size :max-header-bytes :max-fields
                :max-request-streams :max-peer-unidirectional-streams
                :max-state-bytes :qpack-max-instruction-bytes :qpack-max-buffer-bytes))
    (dolist (invalid (list 0 -1 1/2 (1+ http-kit/http3::+http3-max-varint+)))
      (let ((opened 0))
        (let ((failure
                (handler-case
                    (http3-transport-boundary-client
                     key invalid :open-stream
                     (lambda (request &key stream-type timeout deadline)
                       (declare (ignore request timeout deadline))
                       (incf opened)
                       (make-http3-test-stream :kind stream-type)))
                  (http-protocol-error (condition) condition))))
          (ensure-true (typep failure 'http-protocol-error))
          (ensure-equal :http3-transport (http-error-operation failure))
          (ensure-equal
           (if (member key '(:max-frame-size :max-header-bytes :max-fields))
               invalid
               (cons key invalid))
           (http-protocol-error-detail failure)))
        (ensure-equal 0 opened)))
    (dolist (valid (list 1 http-kit/http3::+http3-max-varint+))
      (let ((client (http3-transport-boundary-client key valid)))
        (unwind-protect
             (ensure-true (http-kit/http3:http3-client-open-p client))
          (http-kit/http3:close-http3-client client))))))

(deftest http3-transport-public-max-push-id-boundaries
  (dolist (invalid (list -1 1/2 (1+ http-kit/http3::+http3-max-varint+)))
    (let ((opened 0))
      (let ((failure
              (handler-case
                  (http3-transport-boundary-client
                   :max-push-id invalid :open-stream
                   (lambda (request &key stream-type timeout deadline)
                     (declare (ignore request timeout deadline))
                     (incf opened)
                     (make-http3-test-stream :kind stream-type)))
                (http-protocol-error (condition) condition))))
        (ensure-true (typep failure 'http-protocol-error))
        (ensure-equal :http3-transport (http-error-operation failure))
        (ensure-equal invalid (http-protocol-error-detail failure)))
      (ensure-equal 0 opened)))
  (dolist (valid (list nil 0 http-kit/http3::+http3-max-varint+))
    (let ((client (http3-transport-boundary-client :max-push-id valid)))
      (unwind-protect
           (let* ((stream (http-kit/http3::http3-client-control-stream client))
                  (wire (first (first (http3-test-stream-writes stream)))))
             (ensure-equal valid (http-kit/http3::http3-client-max-push-id client))
             (multiple-value-bind (frames remainder)
                 (http-kit/http3:decode-http3-frames (subseq wire 1))
               (ensure-equal 0 (length remainder))
               (ensure-equal (if valid 2 1) (length frames))
               (ensure-equal http-kit/http3:+http3-settings-type+
                             (http-kit/http3:http3-frame-type (first frames)))
               (when valid
                 (ensure-equal http-kit/http3:+http3-max-push-id-type+
                               (http-kit/http3:http3-frame-type (second frames)))
                 (multiple-value-bind (value position)
                     (http-kit/http3:http3-varint-decode
                      (http-kit/http3:http3-frame-payload (second frames)))
                   (ensure-equal valid value)
                   (ensure-equal (length (http-kit/http3:http3-frame-payload
                                         (second frames)))
                                 position)))))
        (http-kit/http3:close-http3-client client)))))

(deftest http3-transport-public-early-nil-stream-cleanup
  (dolist (failed-kind '(:control :qpack-encoder))
    (let ((opened nil) (closed nil) (written 0))
      (let ((failure
              (handler-case
                  (http3-transport-boundary-client
                   :open-stream
                   (lambda (request &key stream-type timeout deadline)
                     (declare (ignore request timeout deadline))
                     (push stream-type opened)
                     (unless (eq stream-type failed-kind)
                       (make-http3-test-stream :kind stream-type)))
                   :write-stream
                   (lambda (stream bytes &key fin-p timeout deadline)
                     (declare (ignore stream bytes fin-p timeout deadline))
                     (incf written))
                   :close-stream
                   (lambda (stream &key condition)
                     (push (cons stream condition) closed)
                     (setf (http3-test-stream-closed-p stream) t)))
                (http-protocol-error (condition) condition))))
        (ensure-true (typep failure 'http-protocol-error))
        (ensure-equal :http3-transport (http-error-operation failure))
        (ensure-equal (if (eq failed-kind :control)
                          '(:control) '(:control :qpack-encoder))
                      (reverse opened))
        (ensure-equal 0 written)
        (ensure-equal (if (eq failed-kind :control) 0 1) (length closed))
        (dolist (entry closed)
          (ensure-equal :control (http3-test-stream-kind (car entry)))
          (ensure-true (eq failure (cdr entry)))
          (ensure-true (http3-test-stream-closed-p (car entry))))))))

(deftest http3-transport-public-open-failure-cleanup
  (dolist (failed-kind '(:control :qpack-encoder :qpack-decoder))
    (let ((primary (make-condition 'simple-error :format-control "Injected open failure."))
          (opened nil) (closed nil) (written 0))
      (let ((failure
              (handler-case
                  (http3-transport-boundary-client
                   :open-stream
                   (lambda (request &key stream-type timeout deadline)
                     (declare (ignore request timeout deadline))
                     (push stream-type opened)
                     (when (eq stream-type failed-kind) (error primary))
                     (make-http3-test-stream :kind stream-type))
                   :write-stream
                   (lambda (stream bytes &key fin-p timeout deadline)
                     (declare (ignore stream bytes fin-p timeout deadline))
                     (incf written))
                   :close-stream
                   (lambda (stream &key condition)
                     (push (cons stream condition) closed)
                     (setf (http3-test-stream-closed-p stream) t)))
                (simple-error (condition) condition))))
        (ensure-true (eq primary failure))
        (ensure-equal
         (subseq '(:control :qpack-encoder :qpack-decoder)
                 0 (1+ (position failed-kind '(:control :qpack-encoder :qpack-decoder))))
         (reverse opened))
        (ensure-equal (butlast (reverse opened))
                      (mapcar (lambda (entry) (http3-test-stream-kind (car entry)))
                              (reverse closed)))
        (ensure-equal 0 written)
        (dolist (entry closed)
          (ensure-true (eq primary (cdr entry)))
          (ensure-true (http3-test-stream-closed-p (car entry))))))))

(deftest http3-transport-public-write-failure-cleanup
  (dolist (failed-kind '(:control :qpack-encoder :qpack-decoder))
    (let ((primary (make-condition 'simple-error :format-control "Injected write failure."))
          (opened nil) (written nil) (closed nil))
      (let ((failure
              (handler-case
                  (http3-transport-boundary-client
                   :open-stream
                   (lambda (request &key stream-type timeout deadline)
                     (declare (ignore request timeout deadline))
                     (push stream-type opened)
                     (make-http3-test-stream :kind stream-type))
                   :write-stream
                   (lambda (stream bytes &key fin-p timeout deadline)
                     (declare (ignore bytes fin-p timeout deadline))
                     (push (http3-test-stream-kind stream) written)
                     (when (eq failed-kind (http3-test-stream-kind stream))
                       (error primary)))
                   :close-stream
                   (lambda (stream &key condition)
                     (push (cons stream condition) closed)
                     (setf (http3-test-stream-closed-p stream) t)
                     (when (eq :control (http3-test-stream-kind stream))
                       (error "Injected secondary cleanup failure."))))
                (simple-error (condition) condition))))
        (ensure-true (eq primary failure))
        (ensure-equal '(:control :qpack-encoder :qpack-decoder) (reverse opened))
        (ensure-equal
         (subseq (reverse opened) 0 (1+ (position failed-kind (reverse opened))))
         (reverse written))
        (ensure-equal (reverse opened)
                      (mapcar (lambda (entry) (http3-test-stream-kind (car entry)))
                              (reverse closed)))
        (dolist (entry closed)
          (ensure-true (eq primary (cdr entry)))
          (ensure-true (http3-test-stream-closed-p (car entry))))))))

(deftest http3-transport-public-ready-failure-cleanup
  (let ((primary (make-condition 'simple-error :format-control "Injected ready failure."))
        (written nil) (closed nil) (ready-call nil))
    (let ((failure
            (handler-case
                (http3-transport-boundary-client
                 :timeout 23 :deadline 47
                 :write-stream
                 (lambda (stream bytes &key fin-p timeout deadline)
                   (declare (ignore bytes fin-p))
                   (push (list (http3-test-stream-kind stream) timeout deadline) written))
                 :on-control-stream-ready
                 (lambda (&key timeout deadline)
                   (setf ready-call (list timeout deadline))
                   (error primary))
                 :close-stream
                 (lambda (stream &key condition)
                   (push (cons stream condition) closed)
                   (setf (http3-test-stream-closed-p stream) t)))
              (simple-error (condition) condition))))
      (ensure-true (eq primary failure))
      (ensure-equal '(23 47) ready-call)
      (ensure-equal '((:control 23 47)) written)
      (ensure-equal '(:control :qpack-encoder :qpack-decoder)
                    (mapcar (lambda (entry) (http3-test-stream-kind (car entry)))
                            (reverse closed)))
      (dolist (entry closed)
        (ensure-true (eq primary (cdr entry)))
        (ensure-true (http3-test-stream-closed-p (car entry)))))))

(deftest http3-transport-public-close-validation-and-idempotence
  (dolist (invalid '(nil :not-a-client))
    (let ((failure
            (handler-case (http-kit/http3:close-http3-client invalid)
              (http-protocol-error (condition) condition))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :http3-transport (http-error-operation failure))
      (ensure-equal (type-of invalid) (http-protocol-error-detail failure))))
  (let* ((primary (make-condition 'simple-error :format-control "Client shutdown."))
         (peer (make-http3-test-stream :kind :peer-control))
         (closed nil)
         (client
           (http3-transport-boundary-client
            :peer-control-stream peer
            :close-stream
            (lambda (stream &key condition)
              (push (cons stream condition) closed)
              (setf (http3-test-stream-closed-p stream) t)))))
    (unwind-protect
         (progn
           (ensure-true (http-kit/http3:http3-client-open-p client))
           (ensure-true (http-kit/http3:close-http3-client client :condition primary))
           (ensure-false (http-kit/http3:http3-client-open-p client))
           (ensure-equal '(:control :qpack-encoder :qpack-decoder :peer-control)
                         (mapcar (lambda (entry) (http3-test-stream-kind (car entry)))
                                 (reverse closed)))
           (dolist (entry closed)
             (ensure-true (eq primary (cdr entry)))
             (ensure-true (http3-test-stream-closed-p (car entry))))
           (ensure-true (http-kit/http3:close-http3-client client))
           (ensure-equal 4 (length closed)))
      (http-kit/http3:close-http3-client client))))

(deftest http3-qpack-public-decoder-limit-boundaries
  (ensure-equal nil
                (http3-test-qpack-decode (octets 0 0) :max-header-bytes 0 :max-fields 1))
  (dolist (fields '((("x" . "y")) (("x" . "y") ("z" . "w"))))
    (ensure-equal
     fields
     (http3-test-qpack-decode (http3-test-qpack-encode fields)
      :max-header-bytes (* 34 (length fields))
      :max-fields (length fields)))))

(deftest http3-qpack-public-decoder-limit-size-conditions
  (dolist (row '(((("x" . "y")) 33 1 :headers 33 34)
                 ((("x" . "y") ("z" . "w")) 68 1 :fields 1 2)))
    (destructuring-bind (fields header-limit field-limit kind limit observed) row
      (let ((failure
              (handler-case
                  (progn
                    (http3-test-qpack-decode (http3-test-qpack-encode fields)
                     :max-header-bytes header-limit :max-fields field-limit)
                    nil)
                (http-size-limit-exceeded (condition) condition))))
        (ensure-true (typep failure 'http-size-limit-exceeded))
        (ensure-equal :qpack (http-error-operation failure))
        (ensure-equal kind (http-size-limit-exceeded-kind failure))
        (ensure-equal limit (http-size-limit-exceeded-limit failure))
        (ensure-equal observed (http-size-limit-exceeded-observed failure))))))

(deftest http3-qpack-public-decoder-limit-invalid-limits
  (let ((section
          (http3-test-qpack-encode '(("x" . "y")))))
    (dolist (row '((:max-header-bytes -1) (:max-header-bytes 1/2)
                   (:max-header-bytes nil) (:max-fields 0) (:max-fields 1/2)))
      (destructuring-bind (key value) row
        (declare (ignore key))
        (let ((failure
                (handler-case
                    (progn
                      (apply #'http3-test-qpack-decode
                             section row)
                      nil)
                  (http-protocol-error (condition) condition))))
          (ensure-true (typep failure 'http-protocol-error))
          (ensure-equal :qpack (http-error-operation failure))
          (ensure-equal value (http-protocol-error-detail failure)))))))

(defun http3-public-error-serve (entries handler &key before-read)
  (let ((stream (make-http3-test-stream :kind :request :reads entries))
        (calls 0) (writes nil) (closed nil) (observed-error nil)
        (seen-request nil) (error-request nil) (response nil))
    (let ((failure
            (http3-boundary-error
             (lambda ()
               (setf response
                     (http-kit/http3:serve-http3-request-stream
                      stream
                      (lambda (request)
                        (incf calls)
                        (setf seen-request request)
                        (funcall handler request))
                      :read-stream
                      (lambda (ignored &key timeout deadline)
                        (declare (ignore ignored timeout deadline))
                        (when before-read (funcall before-read calls writes))
                        (let ((entry (pop (http3-test-stream-reads stream))))
                          (values (first entry) (second entry))))
                      :write-stream
                      (lambda (ignored bytes &key fin-p timeout deadline)
                        (declare (ignore ignored timeout deadline))
                        (push (list (copy-seq bytes) fin-p) writes))
                      :close-stream
                      (lambda (ignored &key condition)
                        (declare (ignore ignored))
                        (push condition closed))
                      :on-error
                      (lambda (condition request)
                        (setf observed-error condition error-request request))))))))
      (values failure response calls (nreverse writes) (nreverse closed)
              observed-error seen-request error-request))))

(defun http3-public-error-check (failure message detail)
  (ensure-true (typep failure 'http-protocol-error))
  (ensure-equal :http3-transport (http-error-operation failure))
  (ensure-equal message (http-error-message failure))
  (ensure-equal detail (http-protocol-error-detail failure)))

(deftest http3-public-errors-control-fin
  (dolist (row
           (list (list (octets) t
                       "The peer HTTP/3 control stream ended before its stream-type prefix.")
                 (list (octets 0) t
                       "The peer HTTP/3 control stream ended before SETTINGS.")
                 (list (octets 0 4 1) t
                       "The peer HTTP/3 control stream ended with a truncated frame.")
                 (list (octets 0 4 0) nil nil)
                 (list (octets 0 4 0) t
                       "The peer closed the HTTP/3 control stream."
                       :h3-closed-critical-stream)))
    (destructuring-bind (wire fin-p message &optional detail) row
      (let* ((stream (make-http3-test-stream
                      :kind :control :reads (list (list wire fin-p))))
             (reads 0)
             (client
               (http3-transport-boundary-client
                :peer-control-stream stream
                :read-stream
                (lambda (actual &key timeout deadline)
                  (declare (ignore timeout deadline))
                  (ensure-true (eq stream actual))
                  (incf reads)
                  (let ((entry (pop (http3-test-stream-reads stream))))
                    (values (first entry) (second entry))))))
             (events nil) (ended nil))
        (unwind-protect
             (let ((failure
                     (http3-boundary-error
                      (lambda ()
                        (multiple-value-setq (events ended)
                          (http-kit/http3:read-http3-control-stream client))))))
               (if message
                   (progn
                     (ensure-true (typep failure 'http-protocol-error))
                     (ensure-equal :http3-control-stream (http-error-operation failure))
                     (ensure-equal message (http-error-message failure))
                     (ensure-equal detail (http-protocol-error-detail failure)))
                   (progn
                     (ensure-equal nil failure)
                     (ensure-equal '(:settings) events)
                     (ensure-equal nil ended)))
               (ensure-equal 1 reads)
               (when detail
                 (ensure-true (http-kit/http3:http3-client-peer-control-fin-p client))
                 (multiple-value-bind (repeated-events repeated-fin)
                     (http-kit/http3:read-http3-control-stream client)
                   (ensure-equal nil repeated-events)
                   (ensure-equal t repeated-fin))
                 (ensure-equal 1 reads)
                 (ensure-true (http-kit/http3:http3-client-open-p client)))
               (ensure-equal nil (http3-test-stream-writes stream)))
          (http-kit/http3:close-http3-client client))))))

(deftest http3-public-errors-request-order
  (let ((headers (http3-boundary-headers
                  '((":method" . "GET") (":scheme" . "https")
                    (":authority" . "example.test") (":path" . "/"))))
        (trailers (http3-boundary-headers '(("x-end" . "yes"))))
        (data (http3-boundary-data "Z")))
    (dolist (row
             (list (list data "An HTTP/3 DATA frame arrived before request HEADERS.")
                   (list (http3-test-concat-octets headers trailers data)
                         "An HTTP/3 DATA frame arrived after request trailers.")
                   (list (http3-test-concat-octets headers trailers trailers)
                         "An HTTP/3 request cannot contain multiple trailer blocks.")
                   (list (http3-test-concat-octets headers data trailers) nil)))
      (destructuring-bind (wire message) row
        (let ((final (make-http-response :status 204 :protocol-version "HTTP/3")))
          (multiple-value-bind (failure response calls writes closed observed request error-request)
              (http3-public-error-serve
               (list (list wire t))
               (lambda (request)
                 (ensure-equal "GET" (http-request-method request))
                 (ensure-equal (octets 90) (http-request-body request))
                 (ensure-equal "yes" (http-header-value
                                      (http-request-trailers request) "x-end"))
                 final))
            (ensure-equal 1 (length closed))
            (ensure-true (eq failure (first closed)))
            (ensure-true (eq failure observed))
            (if message
                (progn
                  (http3-public-error-check failure message nil)
                  (ensure-equal nil response)
                  (ensure-equal 0 calls)
                  (ensure-equal nil writes)
                  (ensure-equal nil request)
                  (ensure-equal nil error-request))
                (progn
                  (ensure-equal nil failure)
                  (ensure-true (eq final response))
                  (ensure-equal 1 calls)
                  (ensure-equal 1 (length writes))
                  (ensure-equal '(t) (mapcar #'second writes))
                  (ensure-equal nil error-request)))))))))

(deftest http3-public-errors-request-eof
  (let ((headers (http3-boundary-headers
                  '((":method" . "GET") (":scheme" . "https")
                    (":authority" . "example.test") (":path" . "/"))))
        (final (make-http-response :status 204 :protocol-version "HTTP/3")))
    (dolist (row
             (list (list (list (list (http3-test-concat-octets headers (octets 0 1)) t))
                         "The HTTP/3 request stream ended with a truncated frame.")
                   (list (list (list (octets) t))
                         "The HTTP/3 request stream ended before request HEADERS.")
                   (list (list (list (http3-test-concat-octets headers (octets 0 1)) nil)
                               (list (octets 90) t)) nil)))
      (destructuring-bind (entries message) row
        (multiple-value-bind (failure response calls writes closed observed request error-request)
            (http3-public-error-serve
             entries
             (lambda (request)
               (ensure-equal (octets 90) (http-request-body request))
               final)
             :before-read (unless message
                            (lambda (calls writes)
                              (ensure-equal 0 calls)
                              (ensure-equal nil writes))))
          (ensure-equal 1 (length closed))
          (ensure-true (eq failure (first closed)))
          (ensure-true (eq failure observed))
          (if message
              (progn
                (http3-public-error-check failure message nil)
                (ensure-equal nil response)
                (ensure-equal 0 calls)
                (ensure-equal nil writes)
                (ensure-equal nil request)
                (ensure-equal nil error-request))
              (progn
                (ensure-equal nil failure)
                (ensure-true (eq final response))
                (ensure-equal 1 calls)
                (ensure-equal 1 (length writes))
                (ensure-equal '(t) (mapcar #'second writes))
                (ensure-equal nil error-request))))))))

(deftest http3-public-errors-informational-stream
  (let ((headers (http3-boundary-headers
                  '((":method" . "GET") (":scheme" . "https")
                    (":authority" . "example.test") (":path" . "/"))))
        (final (make-http-response :status 204 :protocol-version "HTTP/3")))
    (dolist (row (list (list 1 (octets) 0
                            "An HTTP/3 informational response cannot contain a body.")
                      (list nil "bad" 1
                            "HTTP/3 response body functions must return octet vectors or NIL.")
                      (list nil (octets 90) 1
                            "An HTTP/3 informational response cannot contain a body.")
                      (list 0 (octets) 2 nil)))
      (destructuring-bind (length chunk expected-calls message) row
        (let* ((body-calls 0)
               (information
                 (make-http-response-stream
                  :status 103 :protocol-version "HTTP/3" :body-length length
                  :body-function (lambda ()
                                   (incf body-calls)
                                   (when (= 1 body-calls) chunk)))))
          (multiple-value-bind (failure response calls writes closed observed request error-request)
              (http3-public-error-serve
               (list (list headers t))
               (lambda (request)
                 (declare (ignore request))
                 (values final (list information))))
            (ensure-equal expected-calls body-calls)
            (ensure-equal 1 calls)
            (ensure-true (typep request 'http-request))
            (ensure-equal 1 (length closed))
            (ensure-true (eq failure (first closed)))
            (ensure-true (eq failure observed))
            (if message
                (progn
                  (http3-public-error-check failure message
                                            (when (stringp chunk) (type-of chunk)))
                  (ensure-true (eq request error-request))
                  (ensure-equal nil response)
                  (ensure-equal nil writes))
                (progn
                  (ensure-equal nil failure)
                  (ensure-true (eq final response))
                  (ensure-equal nil error-request)
                  (ensure-equal 2 (length writes))
                  (ensure-equal '(nil t) (mapcar #'second writes))
                  (multiple-value-bind (frames remainder)
                      (http-kit/http3:decode-http3-frames
                       (apply #'http3-test-concat-octets (mapcar #'first writes)))
                    (ensure-equal 0 (length remainder))
                    (ensure-equal '(1 1) (mapcar #'http-kit/http3:http3-frame-type frames))
                    (ensure-equal '("103" "204")
                                  (mapcar
                                   (lambda (frame)
                                     (cdr (assoc ":status"
                                                 (http3-test-qpack-decode (http-kit/http3:http3-frame-payload frame))
                                                 :test #'string=))) frames)))))))))))

(deftest http3-public-errors-final-stream
  (let ((headers (http3-boundary-headers
                  '((":method" . "GET") (":scheme" . "https")
                    (":authority" . "example.test") (":path" . "/")))))
    (dolist (row
             (list (list nil "bad" 1
                         "HTTP/3 response body functions must return octet vectors or NIL.")
                   (list 2 (octets 90) 2
                         "HTTP/3 response stream body-length does not match emitted bytes.")
                   (list 1 (octets 90) 2 nil)))
      (destructuring-bind (length chunk expected-calls message) row
        (let* ((body-calls 0)
               (final (make-http-response-stream
                       :status 200 :protocol-version "HTTP/3" :body-length length
                       :body-function (lambda ()
                                        (incf body-calls)
                                        (when (= 1 body-calls) chunk)))))
          (multiple-value-bind (failure response calls writes closed observed request error-request)
              (http3-public-error-serve
               (list (list headers t))
               (lambda (request) (declare (ignore request)) final))
            (ensure-equal expected-calls body-calls)
            (ensure-equal 1 calls)
            (ensure-true (typep request 'http-request))
            (ensure-equal 1 (length closed))
            (ensure-true (eq failure (first closed)))
            (ensure-true (eq failure observed))
            (if message
                (progn
                  (http3-public-error-check
                   failure message (if (stringp chunk) (type-of chunk) '(2 1)))
                  (ensure-true (eq request error-request))
                  (ensure-equal nil response))
                (progn
                  (ensure-equal nil failure)
                  (ensure-true (eq final response))
                  (ensure-equal nil error-request)))
            (if (stringp chunk)
                (ensure-equal nil writes)
                (progn
                  (ensure-equal 2 (length writes))
                  (ensure-equal '(nil t) (mapcar #'second writes))
                  (multiple-value-bind (frames remainder)
                      (http-kit/http3:decode-http3-frames
                       (apply #'http3-test-concat-octets (mapcar #'first writes)))
                    (ensure-equal 0 (length remainder))
                    (ensure-equal '(1 0) (mapcar #'http-kit/http3:http3-frame-type frames))
                    (ensure-equal "200"
                                  (cdr (assoc ":status"
                                              (http3-test-qpack-decode (http-kit/http3:http3-frame-payload (first frames)))
                                              :test #'string=)))
                    (ensure-equal (octets 90)
                                  (http-kit/http3:http3-frame-payload (second frames))))))))))))

(deftest http3-qpack-behavior-field-representations
  (let* ((header (make-http-header "X-Tag" "a"))
         (pair (cons "x-next" "b"))
         (list-field (list "x-last" "c"))
         (fields (list header pair list-field)))
    (ensure-equal '(("x-tag" . "a") ("x-next" . "b") ("x-last" . "c"))
                  (http3-test-qpack-decode (http3-test-qpack-encode fields)))
    (ensure-equal "X-Tag" (http-header-name header))
    (ensure-equal '("x-next" . "b") pair)
    (ensure-equal '("x-last" "c") list-field))
  (dolist (field '(nil ("x") ("x" "a" "extra") ("x" . 7)))
    (let ((condition
            (handler-case
                (http-message-kit:qpack-prepare-header-block (list field))
              (http-protocol-error (condition) condition))))
      (ensure-true (typep condition 'http-protocol-error))
      (ensure-equal :qpack (http-error-operation condition))
      (ensure-equal field (http-protocol-error-detail condition))))
  (let* ((decoder
          (http-message-kit:make-qpack-context
           :max-table-capacity 0 :table-capacity 0 :huffman nil))
         (adapter
          (http-message-kit:qpack-make-http3-decoder-context decoder))
         (condition
          (handler-case
              (http-message-kit:qpack-decode-http3-field-section
               (http3-test-qpack-encode
                (quote (("x" . "a") (":path" . "/"))))
               adapter 0 :role :request)
            (http-protocol-error (condition) condition))))
    (ensure-true (typep condition (quote http-invalid-header)))
    (ensure-equal :http/3 (http-error-operation condition))
    (ensure-equal ":path" (http-invalid-header-name condition))
    (ensure-equal :pseudo-after-regular
                  (http-invalid-header-reason condition))
    (ensure-equal nil (http-protocol-error-detail condition))))

(deftest http3-qpack-behavior-table-validation-is-atomic
  (let ((context
          (http-message-kit:make-qpack-context
           :max-table-capacity 64 :table-capacity 64 :huffman nil)))
    (http-message-kit:qpack-encode-insert-with-literal-name context "x" "a")
    (flet ((check (thunk detail)
             (let ((condition
                     (handler-case (funcall thunk)
                       (http-protocol-error (condition) condition))))
               (ensure-true (typep condition 'http-protocol-error))
               (ensure-equal :qpack (http-error-operation condition))
               (ensure-equal detail (http-protocol-error-detail condition))
               (ensure-equal 64
                             (http-message-kit:qpack-context-table-capacity context))
               (ensure-equal 34
                             (http-message-kit:qpack-context-table-size context))
               (ensure-equal 1
                             (http-message-kit:qpack-context-insert-count context))
               (ensure-equal 1
                             (http-message-kit:qpack-context-dynamic-entry-count context)))))
      (dolist (capacity '(-1 65 1/2))
        (check (lambda ()
                 (http-message-kit:qpack-context-set-capacity context capacity))
               capacity))
      (check (lambda () (http-message-kit:qpack-context-set-capacity :bad 0)) :bad)
      (check (lambda ()
               (http-message-kit:qpack-encode-insert-with-literal-name :bad "x" "a"))
             :bad)
      (check (lambda ()
               (http-message-kit:qpack-encode-insert-with-literal-name context 7 "a"))
             7)
      (check (lambda ()
               (http-message-kit:qpack-encode-insert-with-literal-name context "x" 7))
             7)
      (let ((value (string (code-char 10))))
        (check (lambda ()
                 (http-message-kit:qpack-encode-insert-with-literal-name context "x" value))
               value)))
    (ensure-equal '(("x" . "a"))
                  (http3-test-qpack-decode
                   (http3-test-qpack-encode '(("x" . "a")) :context context)
                   :context context))))

(deftest http3-qpack-public-capacity-shrink-protects-outstanding-section
  (let* ((response-wire (http3-boundary-headers '((":status" . "204"))))
         (streams nil)
         (encoder-stream nil)
         (client
           (http-kit/http3:make-http3-client
            :open-stream
            (lambda (request &key stream-type timeout deadline)
              (declare (ignore request timeout deadline))
              (let ((stream
                      (make-http3-test-stream
                       :kind stream-type
                       :reads (when (eq stream-type :request)
                                (list (list response-wire t))))))
                (when (eq stream-type :qpack-encoder)
                  (setf encoder-stream stream))
                (push stream streams)
                (values stream (when (eq stream-type :request) 0))))
            :write-stream
            (lambda (stream bytes &key fin-p timeout deadline)
              (declare (ignore timeout deadline))
              (push (list (copy-seq bytes) fin-p)
                    (http3-test-stream-writes stream)))
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore timeout deadline))
              (ensure-true (consp (http3-test-stream-reads stream)))
              (let ((entry (pop (http3-test-stream-reads stream))))
                (values (first entry) (second entry))))
            :close-stream
            (lambda (stream &key condition)
              (declare (ignore condition))
              (ensure-false (http3-test-stream-closed-p stream))
              (setf (http3-test-stream-closed-p stream) t)))))
    (unwind-protect
         (progn
           (ensure-true encoder-stream)
           (let* ((settings-wire
                    (http3-test-concat-octets
                     (http-kit/http3:http3-varint-encode
                      http-kit/http3:+http3-control-stream-type+)
                     (http-kit/http3:encode-http3-frame
                      (http-kit/http3:make-http3-settings-frame
                       :qpack-max-table-capacity 34
                       :qpack-blocked-streams 1))))
                  (peer-control
                    (make-http3-test-stream
                     :kind :peer-control
                     :reads (list (list settings-wire nil)))))
             (push peer-control streams)
             (http-kit/http3:attach-http3-peer-control-stream client peer-control)
             (multiple-value-bind (events ended-p)
                 (http-kit/http3:read-http3-control-stream client)
               (ensure-equal '(:settings) events)
               (ensure-equal nil ended-p)))
           (let ((initial-writes
                   (length (http3-test-stream-writes encoder-stream))))
             (http-kit/http3:http3-client-set-qpack-capacity client 34)
             (http-kit/http3:http3-client-insert-qpack-field client "a" "1")
             (ensure-equal (+ initial-writes 2)
                           (length (http3-test-stream-writes encoder-stream)))
             (ensure-equal 204
                           (http-response-status
                            (http-kit/http3:send-http3-request
                             client
                             (make-http-request
                              :method "GET" :uri "https://example.test/"
                              :headers (list (cons "a" "1"))))))
             (let ((protected-write-count
                     (length (http3-test-stream-writes encoder-stream))))
               (dolist (operation
                        (list (lambda ()
                                (http-kit/http3:http3-client-set-qpack-capacity
                                 client 0))
                              (lambda ()
                                (http-kit/http3:http3-client-insert-qpack-field
                                 client "b" "2"))))
                 (let ((failure
                         (handler-case (funcall operation)
                           (http-protocol-error (condition) condition))))
                   (ensure-true (typep failure 'http-protocol-error))
                   (ensure-equal :qpack (http-error-operation failure))
                   (ensure-equal protected-write-count
                                 (length (http3-test-stream-writes encoder-stream)))
                   (ensure-true (http-kit/http3:http3-client-open-p client))))
               (let* ((ack-wire
                        (http3-test-concat-octets
                         (http-kit/http3:http3-varint-encode
                          http-kit/http3:+http3-qpack-decoder-stream-type+)
                         (http-message-kit:qpack-encode-section-ack 0)))
                      (peer-decoder
                        (make-http3-test-stream
                         :kind :peer-qpack-decoder
                         :reads (list (list ack-wire nil)))))
                 (push peer-decoder streams)
                 (http-kit/http3:attach-http3-peer-qpack-decoder-stream
                  client peer-decoder)
                 (multiple-value-bind (events ended-p)
                     (http-kit/http3:read-http3-qpack-decoder-stream client)
                   (ensure-equal '((:section-ack 0)) events)
                   (ensure-equal nil ended-p)))
               (http-kit/http3:http3-client-set-qpack-capacity client 0)
               (http-kit/http3:http3-client-set-qpack-capacity client 34)
               (http-kit/http3:http3-client-insert-qpack-field client "b" "2")
               (ensure-equal (+ protected-write-count 3)
                             (length (http3-test-stream-writes encoder-stream))))))
      (http-kit/http3:close-http3-client client))
    (dolist (stream streams)
      (ensure-true (http3-test-stream-closed-p stream)))))

(deftest http3-qpack-behavior-sensitive-dynamic-name
  (let* ((encoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 128 :table-capacity 0 :huffman nil))
         (decoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 128 :table-capacity 0 :huffman nil)))
    (multiple-value-bind (instruction transaction)
        (http-message-kit:qpack-prepare-set-capacity encoder 128)
      (http-message-kit:qpack-commit-encoding transaction)
      (http-message-kit:qpack-decode-encoder-stream decoder instruction))
    (multiple-value-bind (instruction transaction)
        (http-message-kit:qpack-prepare-insert-with-literal-name
         encoder "proxy-authorization" "old" :huffman nil)
      (http-message-kit:qpack-commit-encoding transaction)
      (http-message-kit:qpack-decode-encoder-stream decoder instruction))
    (let ((table-size (http-message-kit:qpack-context-table-size encoder)))
      (dolist (huffman '(nil t))
        (multiple-value-bind (wire transaction)
            (http-message-kit:qpack-prepare-header-block
             (list (http-message-kit:make-http-header
                    "proxy-authorization" "new"))
             :context encoder :use-dynamic-p t :huffman huffman :stream-id 4)
          (ensure-equal 2 (aref wire 0))
          (ensure-equal 0 (aref wire 1))
          (ensure-equal #x60 (logand #xf0 (aref wire 2)))
          (http-message-kit:qpack-abort-encoding transaction)
          (multiple-value-bind (decoded consumed required-insert-count base)
              (http-message-kit:qpack-decode-header-block
               wire :context decoder :stream-id 4)
            (declare (ignore required-insert-count base))
            (ensure-equal 1 (length decoded))
            (ensure-equal "proxy-authorization"
                          (http-message-kit:http-header-name (first decoded)))
            (ensure-equal "new"
                          (http-message-kit:http-header-content (first decoded)))
            (ensure-equal (length wire) consumed))
          (ensure-equal 1 (http-message-kit:qpack-context-insert-count encoder))
          (ensure-equal 1 (http-message-kit:qpack-context-insert-count decoder))
          (ensure-equal table-size
                        (http-message-kit:qpack-context-table-size encoder)))))))


(deftest http3-qpack-behavior-required-count-wrap
  (let* ((encoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 64 :table-capacity 0 :huffman nil))
         (decoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 64 :table-capacity 0 :huffman nil))
         (empty-decoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 64 :table-capacity 0 :huffman nil)))
    (multiple-value-bind (instruction transaction)
        (http-message-kit:qpack-prepare-set-capacity encoder 64)
      (http-message-kit:qpack-commit-encoding transaction)
      (http-message-kit:qpack-decode-encoder-stream decoder instruction))
    (dolist (field '(("a" . "1") ("b" . "2") ("c" . "3")))
      (multiple-value-bind (instruction transaction)
          (http-message-kit:qpack-prepare-insert-with-literal-name
           encoder (car field) (cdr field) :huffman nil)
        (http-message-kit:qpack-commit-encoding transaction)
        (http-message-kit:qpack-decode-encoder-stream decoder instruction)))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-header-block
         (list (http-message-kit:make-http-header "c" "3"))
         :context encoder :use-dynamic-p t :huffman nil :stream-id 4)
      (http-message-kit:qpack-abort-encoding transaction)
      (ensure-equal (octets 4 0 #x80) wire)
      (multiple-value-bind (headers consumed required-insert-count base)
          (http-message-kit:qpack-decode-header-block
           wire :context decoder :stream-id 4)
        (declare (ignore base))
        (ensure-equal 1 (length headers))
        (ensure-equal "c"
                      (http-message-kit:http-header-name (first headers)))
        (ensure-equal "3"
                      (http-message-kit:http-header-content (first headers)))
        (ensure-equal (length wire) consumed)
        (ensure-equal 3 required-insert-count)))
    (flet ((check (wire context detail)
             (let ((failure
                     (handler-case
                         (http-message-kit:qpack-decode-header-block
                          wire :context context)
                       (http-message-kit:http-protocol-error (condition)
                         condition))))
               (ensure-true
                (typep failure 'http-message-kit:http-protocol-error))
               (ensure-equal :qpack
                             (http-message-kit:http-error-operation failure))
               (ensure-equal detail
                             (http-message-kit:http-protocol-error-detail failure)))))
      (check (octets 5 0) decoder 5)
      (check (octets 1 0) empty-decoder 1)
      (check (octets 2 0) empty-decoder 1))
    (ensure-equal 3 (http-message-kit:qpack-context-insert-count decoder))
    (ensure-equal 34 (http-message-kit:qpack-context-table-size decoder))
    (ensure-equal 1 (http-message-kit:qpack-context-dynamic-entry-count decoder))
    (let* ((adapter
             (http-message-kit:qpack-make-http3-decoder-context
              decoder :max-blocked-streams 1))
           (token
             (http-message-kit:qpack-decode-http3-field-section
              (octets 1 0) adapter 8 :role :trailers)))
      (ensure-true (http-message-kit:qpack-blocked-stream-p token))
      (ensure-equal 3 (http-message-kit:qpack-context-insert-count decoder))
      (multiple-value-bind (instruction transaction)
          (http-message-kit:qpack-prepare-insert-with-literal-name
           encoder "d" "4" :huffman nil)
        (http-message-kit:qpack-commit-encoding transaction)
        (http-message-kit:qpack-decode-encoder-stream decoder instruction))
      (let ((ready (http-message-kit:qpack-take-ready-streams adapter)))
        (ensure-equal 1 (length ready))
        (ensure-true (eq token (first ready))))
      (ensure-equal nil (http-message-kit:qpack-resume-blocked-stream token))
      (ensure-equal 4 (http-message-kit:qpack-context-insert-count decoder)))))


(deftest http3-qpack-behavior-wire-boundaries-and-postbase
  (flet ((check (wire detail &optional table)
           (let ((condition
                   (handler-case
                       (http3-test-qpack-decode wire :context table)
                     (http-protocol-error (condition) condition))))
             (ensure-true (typep condition 'http-protocol-error))
             (ensure-equal :qpack (http-error-operation condition))
             (ensure-equal detail (http-protocol-error-detail condition)))))
    (dolist (case (list (list (octets 0) nil)
                       (list (octets 0 #x80) 0)
                       (list (octets 0 0 #xff 36) 99)
                       (list (octets 0 0 #x80) -1)
                       (list (octets 0 0 #x21 #x78) 4)
                       (list (octets 0 0 #x21 #x78 2 #x61) 2)
                       (list (octets 0 0 #x21 #x58 0) "X")
                       (list (octets 0 0 #x21 #x78 0 #xc1) ":path")))
      (check (first case) (second case)))
    (ensure-equal '(("x" . "a"))
                  (http3-test-qpack-decode (octets 0 0 #x21 #x78 1 #x61)))
    (let ((table (http-message-kit:make-qpack-context :max-table-capacity 64 :table-capacity 64 :huffman nil)))
      (http-message-kit:qpack-encode-insert-with-literal-name table "x" "1")
      (ensure-equal '(("x" . "1"))
                    (http3-test-qpack-decode (octets 2 #x80 #x10) :context table))
      (ensure-equal '(("x" . "2"))
                    (http3-test-qpack-decode (octets 2 #x80 #x00 1 #x32) :context table))
      (check (octets 2 #x80 #x11) 1 table)
      (check (octets 2 1 #x80) 1 table)
      (ensure-equal 1 (http-message-kit:qpack-context-insert-count table)))))

(deftest http3-qpack-behavior-decoder-configuration
  (flet ((check (thunk detail)
           (let ((condition (handler-case (funcall thunk)
                              (http-protocol-error (condition) condition))))
             (ensure-true (typep condition 'http-protocol-error))
             (ensure-equal :qpack (http-error-operation condition))
             (ensure-equal detail (http-protocol-error-detail condition)))))
    (dolist (wire (list "bad" (vector 0 256)))
      (check (lambda () (http3-test-qpack-decode wire)) (type-of wire)))
    (ensure-equal nil (http3-test-qpack-decode (vector 0 0)))
    (check (lambda () (http3-test-qpack-decode (octets 0 0) :context :bad)) :bad)
    (dolist (capacity '(-1 1/2))
      (check (lambda () (http3-test-qpack-decode (octets 0 0) :max-table-capacity capacity)) capacity))
    (let ((table (http-message-kit:make-qpack-context :max-table-capacity 64 :table-capacity 64 :huffman nil)))
      (check (lambda () (http3-test-qpack-decode (octets 0 0) :context table :max-table-capacity 65)) 65)
      (ensure-equal nil
                    (http3-test-qpack-decode (octets 0 0) :context table :max-table-capacity 32))
      (ensure-equal 0 (http-message-kit:qpack-context-insert-count table)))))

(deftest http3-transport-behavior-qpack-encoder-errors
  (let* ((capacity (http3-test-qpack-encode-set-capacity 128))
         (prefix (http-kit/http3:http3-qpack-encoder-stream-prefix))
         (valid-wire
           (http3-test-concat-octets
            prefix capacity
            (http3-test-qpack-encode-insert-with-literal-name "x-peer" "yes"))))
    (dolist (case (list (list (http-kit/http3:http3-qpack-decoder-stream-prefix)
                             nil :http3-transport nil)
                       (list (http3-test-concat-octets
                              prefix (http3-test-qpack-encode-duplicate 0))
                             nil :qpack nil)
                       (list (http3-test-concat-octets prefix (subseq capacity 0 1))
                             t :http3-transport t)
                       (list valid-wire nil nil nil)))
      (destructuring-bind (wire fin-p operation expected-fin-p) case
        (let* ((reads 0)
               (writes 0)
               (peer (make-http3-test-stream
                      :reads (if operation
                                 (list (list wire fin-p))
                                 (list (list (subseq wire 0 2) nil)
                                       (list (subseq wire 2) nil)))))
               (client
                 (http-kit/http3:make-http3-client
                  :qpack-settings
                  (list (cons http-kit/http3:+http3-setting-qpack-max-table-capacity+ 128))
                  :open-stream
                  (lambda (request &key stream-type timeout deadline)
                    (declare (ignore request timeout deadline))
                    (make-http3-test-stream :kind stream-type))
                  :write-stream
                  (lambda (stream octets &key fin-p timeout deadline)
                    (declare (ignore stream octets fin-p timeout deadline))
                    (incf writes))
                  :read-stream
                  (lambda (stream &key timeout deadline)
                    (declare (ignore timeout deadline))
                    (ensure-true (eq peer stream))
                    (ensure-true (consp (http3-test-stream-reads stream)))
                    (incf reads)
                    (let ((entry (pop (http3-test-stream-reads stream))))
                      (values (first entry) (second entry)))))))
          (ensure-equal 3 writes)
          (ensure-true (eq client (http-kit/http3:attach-http3-peer-qpack-encoder-stream
                                  client peer)))
          (if operation
              (let ((condition
                      (handler-case (http-kit/http3:read-http3-qpack-encoder-stream client)
                        (http-protocol-error (condition) condition))))
                (ensure-true (typep condition 'http-protocol-error))
                (ensure-equal operation (http-error-operation condition))
                (ensure-equal :qpack-encoder-stream-error
                              (http-protocol-error-detail condition))
                (ensure-equal 1 reads)
                (ensure-equal 0 (http-message-kit:qpack-context-insert-count table))
                (ensure-equal 0 (http-message-kit:qpack-context-table-capacity table)))
              (progn
                (multiple-value-bind (events ended-p)
                    (http-kit/http3:read-http3-qpack-encoder-stream client)
                  (ensure-equal nil events)
                  (ensure-equal nil ended-p))
                (ensure-equal 0 (http-message-kit:qpack-context-insert-count table))
                (multiple-value-bind (events ended-p)
                    (http-kit/http3:read-http3-qpack-encoder-stream client)
                  (ensure-equal 2 (length events))
                  (ensure-equal nil ended-p))
                (ensure-equal 2 reads)
                (ensure-equal 128 (http-message-kit:qpack-context-table-capacity table))
                (ensure-equal 1 (http-message-kit:qpack-context-insert-count table))
                (ensure-equal 0 (length (http-kit/http3::http3-client-peer-qpack-encoder-buffer client)))))
          (ensure-equal nil (http3-test-stream-reads peer))
          (ensure-equal expected-fin-p
                        (http-kit/http3::http3-client-peer-qpack-encoder-fin-p client))
          (ensure-equal 3 writes)
          (ensure-true (http-kit/http3:http3-client-open-p client)))))))

(deftest http3-transport-behavior-priority-payload-limit
  (dolist (limit '(6 7))
    (let ((writes nil)
          (serialized 0))
      (let* ((client
               (http-kit/http3:make-http3-client
                :max-frame-size limit
                :open-stream
                (lambda (request &key stream-type timeout deadline)
                  (declare (ignore request timeout deadline))
                  stream-type)
                :write-stream
                (lambda (stream octets &key fin-p timeout deadline)
                  (push (list stream octets fin-p timeout deadline) writes))
                :read-stream
                (lambda (stream &key timeout deadline)
                  (declare (ignore stream timeout deadline))
                  (error "Unexpected read in priority update."))
                :serialize (lambda (thunk) (incf serialized) (funcall thunk))))
             (initial-writes (length writes)))
        (ensure-equal 3 initial-writes)
        (ensure-equal 0 serialized)
        (if (= limit 6)
            (let ((condition
                    (handler-case
                        (http-kit/http3:send-http3-priority-update
                         client 8 :urgency 0 :incremental t :timeout 2 :deadline 100)
                      (http-protocol-error (condition) condition))))
              (ensure-true (typep condition 'http-protocol-error))
              (ensure-equal :http3-transport (http-error-operation condition))
              (ensure-equal 7 (http-protocol-error-detail condition))
              (ensure-equal initial-writes (length writes))
              (ensure-equal 0 serialized))
            (progn
              (ensure-true
               (eq client (http-kit/http3:send-http3-priority-update
                           client 8 :urgency 0 :incremental t :timeout 2 :deadline 100)))
              (ensure-equal (1+ initial-writes) (length writes))
              (ensure-equal 1 serialized)
              (destructuring-bind (stream wire fin-p timeout deadline) (first writes)
                (ensure-equal :control stream)
                (ensure-equal nil fin-p)
                (ensure-equal 2 timeout)
                (ensure-equal 100 deadline)
                (multiple-value-bind (frames remainder)
                    (http-kit/http3:decode-http3-frames wire)
                  (ensure-equal 0 (length remainder))
                  (ensure-equal 1 (length frames))
                  (ensure-equal 7 (length (http-kit/http3:http3-frame-payload (first frames))))
                  (multiple-value-bind (element-id value kind)
                      (http-kit/http3:decode-http3-priority-update (first frames))
                    (ensure-equal 8 element-id)
                    (ensure-equal "u=0, i" value)
                    (ensure-equal :request kind))))))
        (ensure-true (http-kit/http3:http3-client-open-p client))))))

(deftest http3-transport-behavior-cancel-received-push
  (let ((writes 0) (reads 0) (closes nil) (promises nil))
    (let* ((fields '((":method" . "GET") (":scheme" . "https")
                   (":authority" . "example.test") (":path" . "/asset.css")))
         (response-wire
           (http3-test-concat-octets
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-push-promise-type+
              :payload (http3-test-concat-octets
                        (http-kit/http3:http3-varint-encode 2)
                        (http3-test-qpack-encode fields))))
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-headers-type+
              :payload (http3-test-qpack-encode '((":status" . "204")))))))
         (push-wire
           (http3-test-concat-octets
            (http-kit/http3:http3-varint-encode http-kit/http3:+http3-push-stream-type+)
            (http-kit/http3:http3-varint-encode 2)
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-headers-type+
              :payload (http3-test-qpack-encode '((":status" . "200") ("content-length" . "3")))))
            (http-kit/http3:encode-http3-frame
             (http-kit/http3:make-http3-frame
              :type http-kit/http3:+http3-data-type+ :payload (octets 1 2 3)))))
         (push-stream (make-http3-test-stream :reads (list (list push-wire t))))
         (client
           (http-kit/http3:make-http3-client
            :max-push-id 2
            :open-stream
            (lambda (request &key stream-type timeout deadline)
              (declare (ignore request timeout deadline))
              (values (make-http3-test-stream
                       :kind stream-type
                       :reads (when (eq stream-type :request) (list (list response-wire t))))
                      (when (eq stream-type :request) 0)))
            :write-stream
            (lambda (stream octets &key fin-p timeout deadline)
              (declare (ignore stream octets fin-p timeout deadline))
              (incf writes))
            :read-stream
            (lambda (stream &key timeout deadline)
              (declare (ignore timeout deadline))
              (ensure-true (consp (http3-test-stream-reads stream)))
              (incf reads)
              (let ((entry (pop (http3-test-stream-reads stream))))
                (values (first entry) (second entry))))
            :close-stream
            (lambda (stream &key condition) (push (list stream condition) closes)))))
    (ensure-equal 204
                  (http-response-status
                   (http-kit/http3:send-http3-request
                    client (make-http-request :method "GET" :uri "https://example.test/")
                    :on-push-promise
                    (lambda (id promised-fields) (push (list id promised-fields) promises)))))
    (ensure-equal (list (list 2 fields)) promises)
    (ensure-equal '(2) (http-kit/http3:http3-client-promised-push-ids client))
    (multiple-value-bind (id response) (http-kit/http3:receive-http3-push client push-stream)
      (ensure-equal 2 id)
      (ensure-equal 200 (http-response-status response))
      (ensure-equal (octets 1 2 3) (http-response-body response)))
    (ensure-equal 2 reads)
    (ensure-equal nil (http3-test-stream-reads push-stream))
    (ensure-equal 2 (length closes))
    (ensure-true (eq push-stream (first (first closes))))
    (ensure-equal '(nil nil) (mapcar #'second closes))
    (ensure-equal '(2) (http-kit/http3::http3-client-consumed-push-ids client))
    (let* ((initial-writes writes)
           (condition (handler-case (http-kit/http3:cancel-http3-push client 2)
                        (http-protocol-error (condition) condition))))
      (ensure-true (typep condition 'http-protocol-error))
      (ensure-equal :http3-transport (http-error-operation condition))
      (ensure-equal :h3-id-error (http-protocol-error-detail condition))
      (ensure-equal initial-writes writes)
      (ensure-equal nil (http-kit/http3::http3-client-cancelled-push-ids client))
      (ensure-equal '(2) (http-kit/http3::http3-client-consumed-push-ids client))
      (ensure-equal 2 (length closes))
      (ensure-true (http-kit/http3:http3-client-open-p client))))))

(deftest http3-transport-behavior-request-cps-contract
  (dolist (fail-p '(nil t))
    (let* ((response-wire
             (http3-test-concat-octets
              (http-kit/http3:encode-http3-frame
               (http-kit/http3:make-http3-frame
                :type http-kit/http3:+http3-headers-type+
                :payload (http3-test-qpack-encode '((":status" . "200") ("content-length" . "3")))))
              (http-kit/http3:encode-http3-frame
               (http-kit/http3:make-http3-frame
                :type http-kit/http3:+http3-data-type+ :payload (octets 65 66 67)))))
           (failure (make-condition 'http-protocol-error :message "Read failed."
                                    :operation :http3-transport :detail :fixture-read))
           (request-stream nil)
           (read-count 0)
           (closes nil)
           (events nil)
           (client
             (http-kit/http3:make-http3-client
              :open-stream
              (lambda (request &key stream-type timeout deadline)
                (let ((stream (make-http3-test-stream :kind stream-type)))
                  (when (eq stream-type :request)
                    (ensure-true (http-request-p request))
                    (ensure-equal 2 timeout)
                    (ensure-equal 100 deadline)
                    (setf request-stream stream))
                  (values stream (when (eq stream-type :request) 0))))
              :write-stream
              (lambda (stream octets &key fin-p timeout deadline)
                (when (eq stream request-stream)
                  (ensure-equal 2 timeout)
                  (ensure-equal 100 deadline))
                (push (list octets fin-p) (http3-test-stream-writes stream)))
              :read-stream
              (lambda (stream &key timeout deadline)
                (ensure-true (eq request-stream stream))
                (ensure-equal 2 timeout)
                (ensure-equal 100 deadline)
                (incf read-count)
                (if fail-p (error failure) (values response-wire t)))
              :close-stream
              (lambda (stream &key condition) (push (list stream condition) closes)))))
      (ensure-equal
       (if fail-p :rejected :delivered)
       (http-kit/http3:send-http3-request/cps
        client (make-http-request :method "GET" :uri "https://example.test/")
        (lambda (response)
          (push :success events)
          (ensure-equal 200 (http-response-status response))
          (ensure-equal "HTTP/3" (http-response-protocol-version response))
          (ensure-equal 0 (length (http-response-body response)))
          :delivered)
        :on-error (lambda (condition)
                    (ensure-true (eq failure condition))
                    (push :error events)
                    :rejected)
        :on-body-chunk (lambda (chunk)
                         (ensure-equal (octets 65 66 67) chunk)
                         (push :body events))
        :collect-body-p nil :timeout 2 :deadline 100))
      (ensure-equal (if fail-p '(:error) '(:body :success)) (reverse events))
      (ensure-equal 1 read-count)
      (ensure-equal 1 (length closes))
      (ensure-true (eq request-stream (first (first closes))))
      (ensure-true (eq (when fail-p failure) (second (first closes))))
      (ensure-equal 0 (http-kit/http3::http3-client-request-stream-count client))
      (ensure-true (http-kit/http3:http3-client-open-p client))
      (let ((writes (reverse (http3-test-stream-writes request-stream))))
        (ensure-equal 2 (length writes))
        (ensure-equal '(nil t) (mapcar #'second writes))
        (multiple-value-bind (frames remainder)
            (http-kit/http3:decode-http3-frames
             (apply #'http3-test-concat-octets (mapcar #'first writes)))
          (ensure-equal 0 (length remainder))
          (ensure-equal (list http-kit/http3:+http3-headers-type+ http-kit/http3:+http3-data-type+)
                        (mapcar #'http-kit/http3:http3-frame-type frames))
          (ensure-equal '((":method" . "GET") (":scheme" . "https")
                          (":authority" . "example.test") (":path" . "/"))
                        (http3-test-qpack-decode (http-kit/http3:http3-frame-payload (first frames))))
          (ensure-equal 0 (length (http-kit/http3:http3-frame-payload (second frames)))))))))

(deftest http3-public-boundary-qpack-partial-instruction-limits
  (dolist (row '((2 3 "QPACK incomplete encoder instruction exceeds its configured limit.")
                 (3 2 "QPACK encoder instruction buffer exceeds its configured limit.")
                 (3 3 nil)))
    (destructuring-bind (instruction-limit buffer-limit message) row
      (let ((table
              (http-message-kit:make-qpack-context
               :max-table-capacity 128 :table-capacity 128 :huffman nil))
            (wire (octets #x41 120 5))
            (events nil)
            (consumed nil))
        (let ((failure
                (http3-boundary-error
                 (lambda ()
                   (multiple-value-setq (events consumed)
                     (http-message-kit:qpack-decode-encoder-stream
                      table wire
                      :allow-incomplete-p t
                      :max-instruction-bytes instruction-limit
                      :max-buffer-bytes buffer-limit))))))
          (if message
              (progn
                (ensure-true (typep failure 'http-protocol-error))
                (ensure-equal :qpack (http-error-operation failure))
                (ensure-equal message (http-error-message failure))
                (ensure-equal 3 (http-protocol-error-detail failure)))
              (progn
                (ensure-equal nil failure)
                (ensure-equal nil events)
                (ensure-equal 0 consumed)))
          (ensure-equal 0 (http-message-kit:qpack-context-insert-count table))
          (ensure-equal 128 (http-message-kit:qpack-context-table-capacity table))
          (ensure-equal (octets #x41 120 5) wire)))))
  (dolist (limit '(1 2))
    (let* ((context
             (http-message-kit:make-qpack-context
              :max-table-capacity 128 :table-capacity 128 :huffman nil))
           (wire (octets #xff #x80))
           (events nil)
           (consumed nil))
      (let ((failure
              (http3-boundary-error
               (lambda ()
                 (multiple-value-setq (events consumed)
                   (http-message-kit:qpack-decode-decoder-stream
                    context wire
                    :allow-incomplete-p t
                    :max-instruction-bytes limit
                    :max-buffer-bytes 2))))))
        (if (= limit 1)
            (progn
              (ensure-true (typep failure 'http-protocol-error))
              (ensure-equal :qpack (http-error-operation failure))
              (ensure-equal
               "QPACK incomplete decoder instruction exceeds its configured limit."
               (http-error-message failure))
              (ensure-equal 2 (http-protocol-error-detail failure)))
            (progn
              (ensure-equal nil failure)
              (ensure-equal nil events)
              (ensure-equal 0 consumed)))
        (ensure-equal 0 (http-message-kit:qpack-context-known-received-count context))
        (ensure-equal (octets #xff #x80) wire)))))

(deftest http3-public-boundary-qpack-zero-insert-increment
  (let* ((context
           (http-message-kit:make-qpack-context
            :max-table-capacity 128 :table-capacity 128 :huffman nil))
         (failure
           (http3-boundary-error
            (lambda ()
              (http-message-kit:qpack-decode-decoder-stream
               context (octets 0))))))
    (ensure-true (typep failure 'http-protocol-error))
    (ensure-equal :qpack (http-error-operation failure))
    (ensure-equal "QPACK insert-count increments must be positive."
                  (http-error-message failure))
    (ensure-equal 0 (http-protocol-error-detail failure))
    (ensure-equal 0 (http-message-kit:qpack-context-known-received-count context))
    (http-message-kit:qpack-encode-insert-with-literal-name context "x" "y")
    (multiple-value-bind (events consumed)
        (http-message-kit:qpack-decode-decoder-stream context (octets 1))
      (ensure-equal '((:insert-count-increment 1)) events)
      (ensure-equal 1 consumed))
    (ensure-equal 1 (http-message-kit:qpack-context-known-received-count context))))

(deftest http3-public-boundary-frame-length-before-incomplete
  (dolist (incremental '(nil t))
    (let* ((wire (octets 0 3 42))
           (failure (http3-boundary-error
                     (lambda ()
                       (http-kit/http3:decode-http3-frames
                        wire :max-frame-size 2 :allow-incomplete-p incremental)))))
      (ensure-true (typep failure 'http-size-limit-exceeded))
      (ensure-equal :http3-frame (http-error-operation failure))
      (ensure-equal :frame (http-size-limit-exceeded-kind failure))
      (ensure-equal 2 (http-size-limit-exceeded-limit failure))
      (ensure-equal 3 (http-size-limit-exceeded-observed failure))
      (ensure-equal (octets 0 3 42) wire)))
  (let ((failure (http3-boundary-error
                  (lambda ()
                    (http-kit/http3:decode-http3-frames (octets 0 3 42) :max-frame-size 3)))))
    (ensure-true (typep failure 'http-protocol-error))
    (ensure-equal :http3-frame (http-error-operation failure))
    (ensure-equal "HTTP/3 frame payload is incomplete." (http-error-message failure))
    (ensure-equal nil (http-protocol-error-detail failure)))
  (multiple-value-bind (frames remainder)
      (http-kit/http3:decode-http3-frames
       (octets 0 3 42) :max-frame-size 3 :allow-incomplete-p t)
    (ensure-equal nil frames)
    (ensure-equal (octets 0 3 42) remainder))
  (multiple-value-bind (frames remainder)
      (http-kit/http3:decode-http3-frames (octets 0 3 42 43 44) :max-frame-size 3)
    (ensure-equal 1 (length frames))
    (ensure-equal http-kit/http3:+http3-data-type+ (http-kit/http3:http3-frame-type (first frames)))
    (ensure-equal (octets 42 43 44) (http-kit/http3:http3-frame-payload (first frames)))
    (ensure-equal 0 (length remainder))))

(deftest http3-public-boundary-settings-duplicate-identifiers
  (dolist (thunk (list (lambda () (http-kit/http3:make-http3-settings-frame :extra-settings '((1 . 0))))
                      (lambda () (http-kit/http3:decode-http3-settings (octets 1 0 1 0)))))
    (let ((failure (http3-boundary-error thunk)))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :http3-frame (http-error-operation failure))
      (ensure-equal 1 (http-protocol-error-detail failure))))
  (let ((settings
          (http-kit/http3:decode-http3-settings
           (http-kit/http3:http3-frame-payload
            (http-kit/http3:make-http3-settings-frame :extra-settings '((#x21 . 7)))))))
    (ensure-equal 3 (length settings))
    (ensure-equal '(1 . 0) (assoc 1 settings))
    (ensure-equal '(7 . 0) (assoc 7 settings))
    (ensure-equal '(33 . 7) (assoc 33 settings)))
  (ensure-equal '((1 . 0) (7 . 0) (33 . 7))
                (http-kit/http3:decode-http3-settings (octets 1 0 7 0 33 7))))

(deftest http3-public-boundary-priority-update-invalid-wire
  (dolist (code '(127 128))
    (let* ((wire (octets 0 code))
           (failure
             (http3-boundary-error
              (lambda ()
                (http-kit/http3:decode-http3-priority-update
                 (http-kit/http3:make-http3-frame
                  :type http-kit/http3:+http3-priority-update-request-type+ :payload wire))))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :http3-frame (http-error-operation failure))
      (ensure-equal "An HTTP/3 PRIORITY_UPDATE value contains a non-ASCII field-value octet."
                    (http-error-message failure))
      (ensure-equal (list 1 code) (http-protocol-error-detail failure))
      (ensure-equal (octets 0 code) wire)))
  (multiple-value-bind (id value kind)
      (http-kit/http3:decode-http3-priority-update
       (http-kit/http3:make-http3-frame
        :type http-kit/http3:+http3-priority-update-request-type+ :payload (octets 0 9 117 61 48)))
    (ensure-equal 0 id)
    (ensure-equal (concatenate 'string (string #\Tab) "u=0") value)
    (ensure-equal :request kind)))

(deftest http3-public-boundary-push-prefix-fin
  (dolist (prefix (list (octets) (octets 1) (octets 1 64) (octets 1 2)))
    (let* ((valid-p (equalp prefix (octets 1 2)))
           (fields '((":method" . "GET") (":scheme" . "https")
                     (":authority" . "example.test") (":path" . "/asset")))
           (response-wire
             (http3-test-concat-octets
              (http-kit/http3:encode-http3-frame
               (http-kit/http3:make-http3-frame
                :type http-kit/http3:+http3-push-promise-type+
                :payload (http3-test-concat-octets
                          (http-kit/http3:http3-varint-encode 2)
                          (http3-test-qpack-encode fields))))
              (http3-boundary-headers '((":status" . "204")))))
           (wire (if valid-p
                     (http3-test-concat-octets
                      prefix (http3-boundary-headers '((":status" . "200") ("content-length" . "3")))
                      (http-kit/http3:encode-http3-frame
                       (http-kit/http3:make-http3-frame
                        :type http-kit/http3:+http3-data-type+ :payload (octets 1 2 3))))
                     prefix))
           (stream (make-http3-test-stream :kind :push :reads (list (list wire t))))
           (closes nil) (awaits 0) (chunks nil) (id nil) (response nil)
           (client
             (http-kit/http3:make-http3-client
              :max-push-id 2
              :open-stream
              (lambda (request &key stream-type timeout deadline)
                (declare (ignore request timeout deadline))
                (values (make-http3-test-stream
                         :kind stream-type
                         :reads (when (eq stream-type :request) (list (list response-wire t))))
                        (when (eq stream-type :request) 0)))
              :write-stream
              (lambda (target bytes &key fin-p timeout deadline)
                (declare (ignore target bytes fin-p timeout deadline)))
              :read-stream
              (lambda (target &key timeout deadline)
                (declare (ignore timeout deadline))
                (ensure-true (consp (http3-test-stream-reads target)))
                (let ((entry (pop (http3-test-stream-reads target))))
                  (values (first entry) (second entry))))
              :close-stream (lambda (target &key condition) (push (list target condition) closes)))))
      (unwind-protect
           (progn
             (ensure-equal 204
                           (http-response-status
                            (http-kit/http3:send-http3-request
                             client (make-http-request :method "GET" :uri "https://example.test/"))))
             (ensure-equal '(2) (http-kit/http3:http3-client-promised-push-ids client))
             (let* ((failure
                      (http3-boundary-error
                       (lambda ()
                         (multiple-value-setq (id response)
                           (http-kit/http3:receive-http3-push
                            client stream
                            :await-push-promise (lambda (push-id) (ensure-equal 2 push-id) (incf awaits))
                            :on-body-chunk (lambda (chunk) (push (copy-seq chunk) chunks)))))))
                    (push-closes (remove-if-not (lambda (entry) (eq stream (first entry))) closes)))
               (ensure-equal nil (http3-test-stream-reads stream))
               (ensure-equal 1 (length push-closes))
               (ensure-true (eq failure (second (first push-closes))))
               (ensure-equal 0 awaits)
               (ensure-equal '(2) (http-kit/http3:http3-client-promised-push-ids client))
               (if valid-p
                   (progn
                     (ensure-equal nil failure)
                     (ensure-equal 2 id)
                     (ensure-equal 200 (http-response-status response))
                     (ensure-equal (octets 1 2 3) (http-response-body response))
                     (ensure-equal (list (octets 1 2 3)) chunks)
                     (ensure-equal '(2) (http-kit/http3::http3-client-consumed-push-ids client)))
                   (progn
                     (http3-public-error-check
                      failure "An HTTP/3 push stream ended with a truncated prefix." :h3-stream-creation-error)
                     (ensure-equal nil chunks)
                     (ensure-equal nil (http-kit/http3::http3-client-consumed-push-ids client))))))
        (http-kit/http3:close-http3-client client)))))

(deftest http3-public-boundary-request-content-length-preflight
  (dolist (row '(((("content-length" . "3x")) "content-length must be an ASCII decimal integer.")
                 ((("content-length" . "3") ("content-length" . "2")) "duplicate content-length values must agree.")
                 ((("content-length" . "2")) "content-length does not match the request body.")
                 ((("content-length" . "03") ("content-length" . "3")) nil)))
    (destructuring-bind (headers reason) row
      (let* ((opens 0) (reads 0) (writes nil) (closes nil)
             (reply (http3-boundary-headers '((":status" . "204"))))
             (client
               (http-kit/http3:make-http3-client
                :open-stream
                (lambda (request &key stream-type timeout deadline)
                  (declare (ignore request timeout deadline))
                  (when (eq stream-type :request) (incf opens))
                  (values (make-http3-test-stream
                           :kind stream-type :reads (when (eq stream-type :request) (list (list reply t))))
                          (when (eq stream-type :request) 0)))
                :write-stream
                (lambda (stream bytes &key fin-p timeout deadline)
                  (declare (ignore timeout deadline))
                  (push (list stream (copy-seq bytes) fin-p) writes))
                :read-stream
                (lambda (stream &key timeout deadline)
                  (declare (ignore timeout deadline))
                  (incf reads)
                  (ensure-true (consp (http3-test-stream-reads stream)))
                  (let ((entry (pop (http3-test-stream-reads stream)))) (values (first entry) (second entry))))
                :close-stream (lambda (stream &key condition) (push (list stream condition) closes)))))
        (unwind-protect
             (let* ((initial-writes (length writes))
                    (response nil)
                    (failure
                      (http3-boundary-error
                       (lambda ()
                         (setf response
                               (http-kit/http3:send-http3-request
                                client (make-http-request :method "POST" :uri "https://example.test/"
                                                          :headers headers :body (octets 1 2 3))))))))
               (if reason
                   (progn
                     (ensure-true (typep failure 'http-invalid-header))
                     (ensure-equal :http3-header (http-error-operation failure))
                     (ensure-equal "content-length" (http-invalid-header-name failure))
                     (ensure-equal reason (http-invalid-header-reason failure))
                     (ensure-equal nil response)
                     (ensure-equal 0 opens)
                     (ensure-equal 0 reads)
                     (ensure-equal initial-writes (length writes))
                     (ensure-equal nil closes))
                   (let ((request-writes
                           (remove-if-not (lambda (entry) (eq :request (http3-test-stream-kind (first entry))))
                                          (reverse writes))))
                     (ensure-equal nil failure)
                     (ensure-equal 204 (http-response-status response))
                     (ensure-equal 1 opens)
                     (ensure-equal 1 reads)
                     (ensure-equal 1 (length closes))
                     (ensure-equal nil (second (first closes)))
                     (ensure-true (consp request-writes))
                     (ensure-equal t (third (car (last request-writes))))
                     (multiple-value-bind (frames remainder)
                         (http-kit/http3:decode-http3-frames
                          (apply #'http3-test-concat-octets (mapcar #'second request-writes)))
                       (ensure-equal 0 (length remainder))
                       (ensure-equal http-kit/http3:+http3-headers-type+
                                     (http-kit/http3:http3-frame-type (first frames)))
                       (ensure-equal (octets 1 2 3)
                                     (apply #'http3-test-concat-octets
                                            (mapcar #'http-kit/http3:http3-frame-payload
                                                    (remove-if-not
                                                     (lambda (frame) (= http-kit/http3:+http3-data-type+
                                                                        (http-kit/http3:http3-frame-type frame)))
                                                     frames)))))))))
          (http-kit/http3:close-http3-client client)))))

(deftest http3-public-boundary-response-content-length-needs-body-length
  (dolist (declared '(nil 3))
    (let* ((calls 0) (chunks (list (octets 1 2) (octets 3) nil))
           (final (make-http-response-stream
                   :status 200 :headers '(("content-length" . "3")) :body-length declared
                   :body-function (lambda () (incf calls) (pop chunks))))
           (request-wire (http3-boundary-headers
                          '((":method" . "GET") (":scheme" . "https")
                            (":authority" . "example.test") (":path" . "/")))))
      (multiple-value-bind (failure response handlers writes closed observed request error-request)
          (http3-public-error-serve (list (list request-wire t))
                                    (lambda (incoming) (declare (ignore incoming)) final))
        (ensure-equal 1 handlers)
        (ensure-true (typep request 'http-request))
        (ensure-equal 1 (length closed))
        (ensure-true (eq failure (first closed)))
        (ensure-true (eq failure observed))
        (if declared
            (progn
              (ensure-equal nil failure)
              (ensure-true (eq final response))
              (ensure-equal nil error-request)
              (ensure-equal 3 calls)
              (ensure-equal nil chunks)
              (ensure-equal '(nil nil t) (mapcar #'second writes))
              (multiple-value-bind (frames remainder)
                  (http-kit/http3:decode-http3-frames
                   (apply #'http3-test-concat-octets (mapcar #'first writes)))
                (ensure-equal 0 (length remainder))
                (ensure-equal (list http-kit/http3:+http3-headers-type+
                                    http-kit/http3:+http3-data-type+ http-kit/http3:+http3-data-type+)
                              (mapcar #'http-kit/http3:http3-frame-type frames))
                (ensure-equal '((":status" . "200") ("content-length" . "3"))
                              (http3-test-qpack-decode (http-kit/http3:http3-frame-payload (first frames))))
                (ensure-equal (octets 1 2) (http-kit/http3:http3-frame-payload (second frames)))
                (ensure-equal (octets 3) (http-kit/http3:http3-frame-payload (third frames)))))
            (progn
              (http3-public-error-check
               failure "An HTTP/3 response stream with Content-Length requires body-length." nil)
              (ensure-equal nil response)
              (ensure-true (eq request error-request))
              (ensure-equal 0 calls)
              (ensure-equal nil writes)
              (ensure-equal (list (octets 1 2) (octets 3) nil) chunks)))))))

(deftest http3-public-preflight-qpack-decoder-context
  (flet ((reject (thunk detail)
           (let ((failure (http3-boundary-error thunk)))
             (ensure-true (typep failure 'http-protocol-error))
             (ensure-equal :qpack (http-error-operation failure))
             (ensure-equal detail (http-protocol-error-detail failure)))))
    (let* ((table
             (http-message-kit:make-qpack-context
              :max-table-capacity 0 :table-capacity 0 :huffman nil))
           (context
             (http-message-kit:qpack-make-http3-decoder-context
              table :max-blocked-streams 0))
           (fields '((":status" . "200")))
           (wire (http3-test-qpack-encode fields))
           (calls nil))
      (dolist (value '(nil :not-a-table))
        (reject
         (lambda ()
           (http-message-kit:qpack-make-http3-decoder-context
            value :max-blocked-streams 0))
         value)
        (reject (lambda () (http-message-kit:qpack-resume-blocked-stream value))
                value))
      (dolist (value '(-1 1/2 nil))
        (reject
         (lambda ()
           (http-message-kit:qpack-make-http3-decoder-context
            table :max-blocked-streams value))
         value))
      (reject
       (lambda ()
         (http-message-kit:qpack-decode-http3-field-section
          wire :not-a-context 0
          :role :response
          :serialize (lambda (thunk) (push :serialize calls) (funcall thunk))))
       :not-a-context)
      (reject
       (lambda ()
         (http-message-kit:qpack-decode-http3-field-section
          wire context 0 :role :response :serialize nil))
       nil)
      (reject
       (lambda ()
         (http-message-kit:qpack-decode-http3-field-section
          wire context 0 :role :response
          :on-section-acknowledgment :not-a-function
          :serialize (lambda (thunk) (push :serialize calls) (funcall thunk))))
       :not-a-function)
      (ensure-equal nil calls)
      (ensure-equal
       fields
       (http-message-kit:qpack-decode-http3-field-section
        wire context 0 :role :response
        :serialize (lambda (thunk)
                     (push :serialize-enter calls)
                     (funcall thunk)
                     (push :serialize-exit calls))))
      (ensure-equal '(:serialize-enter :serialize-exit)
                    (nreverse calls)))
    (multiple-value-bind (fields table wire instruction)
        (http3-test-qpack-blocked-fixture)
      (let ((context
              (http-message-kit:qpack-make-http3-decoder-context
               table :max-blocked-streams 0)))
        (let ((failure
                (http3-boundary-error
                 (lambda ()
                   (http-message-kit:qpack-decode-http3-field-section
                    wire context 4 :role :trailers)))))
          (ensure-true (typep failure 'http-protocol-error))
          (ensure-equal :qpack (http-error-operation failure))
          (ensure-equal 4 (http-protocol-error-detail failure))))
      (let* ((context
               (http-message-kit:qpack-make-http3-decoder-context
                table :max-blocked-streams 1))
             (acknowledged nil)
             (ack-callback
               (lambda (stream-id) (push stream-id acknowledged)))
             (failure
               (http3-boundary-error
                (lambda ()
                  (http-message-kit:qpack-decode-http3-field-section
                   wire context -1 :role :trailers
                   :on-section-acknowledgment ack-callback))))
             (token
               (http-message-kit:qpack-decode-http3-field-section
                wire context 4 :role :trailers
                :on-section-acknowledgment ack-callback)))
        (ensure-true (typep failure 'http-protocol-error))
        (ensure-equal :qpack (http-error-operation failure))
        (ensure-equal -1 (http-protocol-error-detail failure))
        (ensure-equal nil acknowledged)
        (ensure-true (http-message-kit:qpack-blocked-stream-p token))
        (ensure-equal nil (http-message-kit:qpack-take-ready-streams context))
        (http-message-kit:qpack-decode-encoder-stream table instruction)
        (let ((ready (http-message-kit:qpack-take-ready-streams context)))
          (ensure-equal 1 (length ready))
          (ensure-true (eq token (first ready)))
          (ensure-equal
           fields
           (http-message-kit:qpack-resume-blocked-stream token)))
        (ensure-equal '(4) (nreverse acknowledged))
        (ensure-equal
         fields
         (http-message-kit:qpack-decode-http3-field-section
          wire context 8 :role :trailers
          :on-section-acknowledgment ack-callback))
        (ensure-equal '(8 4) acknowledged)))))

(deftest http3-public-preflight-qpack-encoder-tracking
  (flet ((reject (thunk detail)
           (let ((failure (http3-boundary-error thunk)))
             (ensure-true (typep failure 'http-protocol-error))
             (ensure-equal :qpack (http-error-operation failure))
             (ensure-equal detail (http-protocol-error-detail failure)))))
    (let* ((context
             (http-message-kit:make-qpack-context
              :max-table-capacity 256
              :table-capacity 256
              :max-blocked-streams 1
              :huffman nil))
           (fields '(("x-tracked" . "ready"))))
      (reject (lambda () (http-message-kit:qpack-context-known-received-count :not-a-state))
              :not-a-state)
      (reject (lambda () (http-message-kit:qpack-context-set-capacity :not-a-context 0))
              :not-a-context)
      (reject (lambda () (http3-test-qpack-encode fields :context :not-a-table))
              :not-a-table)
      (dolist (stream-id '(nil -1 1/2))
        (reject (lambda ()
                  (http3-test-qpack-encode
                   fields :context context :use-dynamic-p t :stream-id stream-id))
                stream-id))
      (ensure-equal 0 (http-message-kit:qpack-context-insert-count context))
      (http-message-kit:qpack-encode-insert-with-literal-name
       context "x-tracked" "ready")
      (let ((wire
              (http3-test-qpack-encode
               fields :context context :use-dynamic-p t :stream-id 4)))
        (ensure-equal fields (http3-test-qpack-decode wire :context context))
        (multiple-value-bind (events consumed)
            (http-message-kit:qpack-decode-decoder-stream
             context (http-message-kit:qpack-encode-section-ack 4))
          (ensure-equal '((:section-ack 4)) events)
          (ensure-equal 1 consumed))
        (ensure-equal 1 (http-message-kit:qpack-context-known-received-count context))
        (reject (lambda ()
                  (http-message-kit:qpack-decode-decoder-stream
                   context (http-message-kit:qpack-encode-section-ack 4)))
                '(:qpack-decoder-stream-error :section-acknowledgment 4))
        (http-message-kit:qpack-context-set-capacity context 0)
        (ensure-equal 0 (http-message-kit:qpack-context-table-capacity context))))))


(deftest http3-public-preflight-qpack-instruction-arguments
  (flet ((reject (thunk detail)
           (let ((failure (http3-boundary-error thunk)))
             (ensure-true (typep failure 'http-protocol-error))
             (ensure-equal :qpack (http-error-operation failure))
             (ensure-equal detail (http-protocol-error-detail failure)))))
    (let* ((encoder-context
             (http-message-kit:make-qpack-context
              :max-table-capacity 256 :table-capacity 0 :huffman nil))
           (decoder-stream-context
             (http-message-kit:make-qpack-context
              :max-table-capacity 256 :table-capacity 256
              :max-blocked-streams 4 :huffman nil))
           (decoder-wire nil)
           (encoder-wire (http3-test-qpack-encode-set-capacity 64)))
      (http-message-kit:qpack-encode-insert-with-literal-name
       decoder-stream-context "x-first" "one")
      (http3-test-qpack-encode
       '(("x-first" . "one"))
       :context decoder-stream-context :use-dynamic-p t :stream-id 4 :huffman nil)
      (http-message-kit:qpack-encode-insert-with-literal-name
       decoder-stream-context "x-second" "two")
      (http3-test-qpack-encode
       '(("x-second" . "two"))
       :context decoder-stream-context :use-dynamic-p t :stream-id 8 :huffman nil)
      (setf decoder-wire
            (http3-test-concat-octets
             (http-message-kit:qpack-encode-section-ack 4)
             (http-message-kit:qpack-encode-stream-cancellation 8)
             (http-message-kit:qpack-encode-insert-count-increment 1)))
      (reject (lambda ()
                (http-message-kit:qpack-decode-encoder-stream :not-a-context encoder-wire))
              :not-a-context)
      (dolist (input (list "bad" #(0 256)))
        (reject (lambda ()
                  (http-message-kit:qpack-decode-encoder-stream
                   encoder-context input))
                (type-of input))
        (reject (lambda ()
                  (http-message-kit:qpack-decode-decoder-stream
                   decoder-stream-context input))
                (type-of input)))
      (dolist (value '(0 -1 1/2 nil))
        (reject (lambda ()
                  (http-message-kit:qpack-decode-encoder-stream
                   encoder-context encoder-wire :max-instruction-bytes value))
                value)
        (reject (lambda ()
                  (http-message-kit:qpack-decode-encoder-stream
                   encoder-context encoder-wire :max-buffer-bytes value))
                value)
        (reject (lambda ()
                  (http-message-kit:qpack-decode-decoder-stream
                   decoder-stream-context decoder-wire :max-instruction-bytes value))
                value)
        (reject (lambda ()
                  (http-message-kit:qpack-decode-decoder-stream
                   decoder-stream-context decoder-wire :max-buffer-bytes value))
                value))
      (reject (lambda ()
                (http-message-kit:qpack-decode-decoder-stream
                 :not-a-context decoder-wire))
              :not-a-context)
      (ensure-equal 0
                    (http-message-kit:qpack-context-table-capacity encoder-context))
      (multiple-value-bind (events consumed)
          (http-message-kit:qpack-decode-encoder-stream encoder-context encoder-wire)
        (ensure-equal '((:set-capacity 64)) events)
        (ensure-equal (length encoder-wire) consumed))
      (ensure-equal 64
                    (http-message-kit:qpack-context-table-capacity encoder-context))
      (multiple-value-bind (events consumed)
          (http-message-kit:qpack-decode-decoder-stream
           decoder-stream-context decoder-wire)
        (ensure-equal '((:section-ack 4)
                        (:stream-cancellation 8)
                        (:insert-count-increment 1))
                      events)
        (ensure-equal (length decoder-wire) consumed))
      (ensure-equal 2
                    (http-message-kit:qpack-context-known-received-count
                     decoder-stream-context)))))

(deftest http3-public-preflight-send-push-cleanup
  (dolist (scenario '(:normal :late-cancel :nil-open :write-error))
    (let* ((state (http-kit/http3:make-http3-control-state :peer-role :client))
           (request (make-http-request :method "GET" :uri "https://example.test/asset.css"))
           (response (make-http-response :status 200
                                         :headers (list (make-http-header "content-length" "2"))
                                         :body (octets 4 5)))
           (write-failure (make-condition 'http-protocol-error :message "Push prefix write failed."
                                                               :operation :test :detail :write))
           (writes nil) (opened nil) (closed nil) (cancelled nil) (order nil)
           (serialized 0) (returned-id nil) (returned-response nil))
      (ensure-equal :settings
                    (http-kit/http3:process-http3-control-frame
                     state (http-kit/http3:make-http3-frame :type http-kit/http3:+http3-settings-type+ :payload (octets))))
      (ensure-equal :max-push-id
                    (http-kit/http3:process-http3-control-frame
                     state (http-kit/http3:make-http3-frame :type http-kit/http3:+http3-max-push-id-type+
                                                          :payload (http-kit/http3:http3-varint-encode 3))))
      (let ((failure
              (http3-boundary-error
               (lambda ()
                 (multiple-value-setq (returned-id returned-response)
                   (http-kit/http3:send-http3-push
                    :request-stream request response state
                    :serialize (lambda (thunk)
                                 (incf serialized)
                                 (funcall thunk)
                                 (when (eq scenario :late-cancel)
                                   (ensure-equal :cancel-push
                                                 (http-kit/http3:process-http3-control-frame
                                                  state (http-kit/http3:make-http3-frame
                                                         :type http-kit/http3:+http3-cancel-push-type+
                                                         :payload (http-kit/http3:http3-varint-encode 0))))))
                    :open-stream (lambda (opened-request &key stream-type timeout deadline)
                                   (declare (ignore timeout deadline))
                                   (push :open order)
                                   (push (list opened-request stream-type) opened)
                                   (unless (eq scenario :nil-open) :push-stream))
                    :write-stream (lambda (stream bytes &key fin-p timeout deadline)
                                    (declare (ignore timeout deadline))
                                    (push stream order)
                                    (push (list stream (copy-seq bytes) fin-p) writes)
                                    (when (and (eq scenario :write-error) (eq stream :push-stream))
                                      (error write-failure)))
                    :cancel-stream (lambda (stream &key error-code timeout deadline)
                                     (declare (ignore timeout deadline))
                                     (push :cancel order)
                                     (push (list stream error-code) cancelled))
                    :close-stream (lambda (stream &key condition)
                                    (push :close order)
                                    (push (list stream condition) closed))))))))
        (ensure-equal 1 serialized)
        (ensure-equal '(0) (http-kit/http3:http3-control-state-promised-push-ids state))
        (setf writes (nreverse writes) order (nreverse order))
        (ensure-equal :request-stream (first (first writes)))
        (ensure-equal nil (third (first writes)))
        (multiple-value-bind (frames remainder) (http-kit/http3:decode-http3-frames (second (first writes)))
          (ensure-equal 0 (length remainder))
          (ensure-equal 1 (length frames))
          (ensure-equal http-kit/http3:+http3-push-promise-type+ (http-kit/http3:http3-frame-type (first frames)))
          (let ((payload (http-kit/http3:http3-frame-payload (first frames))))
            (multiple-value-bind (id offset) (http-kit/http3:http3-varint-decode payload)
              (ensure-equal 0 id)
              (ensure-equal '((":method" . "GET") (":scheme" . "https")
                              (":authority" . "example.test") (":path" . "/asset.css"))
                            (http3-test-qpack-decode (subseq payload offset))))))
        (case scenario
          (:normal
           (ensure-equal nil failure)
           (ensure-equal 0 returned-id)
           (ensure-true (eq response returned-response))
           (ensure-equal (list (list request :push)) opened)
           (ensure-equal nil cancelled)
           (ensure-equal (list (list :push-stream nil)) closed)
           (ensure-equal '(:request-stream :open :push-stream :push-stream :push-stream :close) order)
           (ensure-equal 4 (length writes))
           (ensure-equal (list :push-stream (octets 1 0) nil) (second writes))
           (ensure-equal '(nil nil nil t) (mapcar #'third writes))
           (let ((frames (http-kit/http3:decode-http3-frames
                          (http3-test-concat-octets (second (third writes)) (second (fourth writes))))))
             (ensure-equal (list http-kit/http3:+http3-headers-type+ http-kit/http3:+http3-data-type+)
                           (mapcar #'http-kit/http3:http3-frame-type frames))
             (ensure-equal '((":status" . "200") ("content-length" . "2"))
                           (http3-test-qpack-decode (http-kit/http3:http3-frame-payload (first frames))))
             (ensure-equal (octets 4 5) (http-kit/http3:http3-frame-payload (second frames)))))
          (:late-cancel
           (ensure-true (typep failure 'http-protocol-error))
           (ensure-equal :http3-transport (http-error-operation failure))
           (ensure-equal :h3-request-cancelled (http-protocol-error-detail failure))
           (ensure-equal '(0) (http-kit/http3:http3-control-state-cancelled-push-ids state))
           (ensure-equal nil opened)
           (ensure-equal nil closed)
           (ensure-equal nil cancelled)
           (ensure-equal '(:request-stream) order)
           (ensure-equal 1 (length writes)))
          (:nil-open
           (ensure-true (typep failure 'http-protocol-error))
           (ensure-equal :http3-transport (http-error-operation failure))
           (ensure-equal nil (http-protocol-error-detail failure))
           (ensure-equal (list (list request :push)) opened)
           (ensure-equal nil closed)
           (ensure-equal nil cancelled)
           (ensure-equal '(:request-stream :open) order)
           (ensure-equal 1 (length writes)))
          (:write-error
           (ensure-true (eq write-failure failure))
           (ensure-equal (list (list request :push)) opened)
           (ensure-equal (list (list :push-stream #x10c)) cancelled)
           (ensure-equal 1 (length closed))
           (ensure-equal :push-stream (first (first closed)))
           (ensure-true (eq write-failure (second (first closed))))
           (ensure-equal '(:request-stream :open :push-stream :cancel :close) order)
           (ensure-equal 2 (length writes))
           (ensure-equal (list :push-stream (octets 1 0) nil) (second writes))))))))

(deftest http3-public-reader-manager-non-octet-read-recovery
  (dolist (kind '(:control :encoder :decoder :dispatch))
    (let* ((stream (make-http3-test-stream :kind :peer))
           (wire (ecase kind
                   ((:control :dispatch)
                    (http3-test-concat-octets
                     (http-kit/http3:http3-control-stream-prefix)
                     (http-kit/http3:encode-http3-frame
                      (http-kit/http3:make-http3-settings-frame))))
                   (:encoder (http-kit/http3:http3-qpack-encoder-stream-prefix))
                   (:decoder (http-kit/http3:http3-qpack-decoder-stream-prefix))))
           (chunks (list "not-octets" wire))
           (reads '())
           (closed '())
           (client
             (http3-transport-boundary-client
              :read-stream
              (lambda (actual &key timeout deadline)
                (push (list actual timeout deadline) reads)
                (values (pop chunks) nil))
              :close-stream
              (lambda (actual &key condition)
                (declare (ignore condition))
                (push actual closed)
                (setf (http3-test-stream-closed-p actual) t))))
           (reader
             (ecase kind
               (:control #'http-kit/http3:read-http3-control-stream)
               (:encoder #'http-kit/http3:read-http3-qpack-encoder-stream)
               (:decoder #'http-kit/http3:read-http3-qpack-decoder-stream)
               (:dispatch #'http-kit/http3:process-http3-peer-unidirectional-stream))))
      (unwind-protect
           (progn
             (ecase kind
               (:control (http-kit/http3:attach-http3-peer-control-stream client stream))
               (:encoder (http-kit/http3:attach-http3-peer-qpack-encoder-stream client stream))
               (:decoder (http-kit/http3:attach-http3-peer-qpack-decoder-stream client stream))
               (:dispatch nil))
             (let ((failure
                     (http3-boundary-error
                      (lambda ()
                        (if (eq kind :dispatch)
                            (funcall reader client stream)
                            (funcall reader client))))))
               (ensure-true (typep failure 'http-protocol-error))
               (ensure-equal :http3-transport (http-error-operation failure))
               (ensure-equal (if (member kind '(:control :dispatch))
                                 (type-of "not-octets") nil)
                             (http-protocol-error-detail failure)))
             (ensure-equal (list (list stream nil nil)) reads)
             (ensure-true (http-kit/http3:http3-client-open-p client))
             (if (eq kind :dispatch)
                 (multiple-value-bind (actual-kind events ended-p)
                     (funcall reader client stream)
                   (ensure-equal :control actual-kind)
                   (ensure-equal '(:settings) events)
                   (ensure-equal nil ended-p))
                 (multiple-value-bind (events ended-p) (funcall reader client)
                   (ensure-equal (if (eq kind :control) '(:settings) nil) events)
                   (ensure-equal nil ended-p)))
             (ensure-equal (list (list stream nil nil) (list stream nil nil)) reads)
             (ensure-equal nil chunks))
        (http-kit/http3:close-http3-client client))
      (ensure-equal 4 (length closed))
      (ensure-equal 4 (length (remove-duplicates closed :test #'eq)))
      (ensure-true (every #'http3-test-stream-closed-p closed))
      (ensure-equal nil (http-kit/http3:http3-client-open-p client))
      (http-kit/http3:close-http3-client client)
      (ensure-equal 4 (length closed)))))

(deftest http3-public-reader-manager-read-and-attach-preflight
  (dolist (row (list
                (list #'http-kit/http3:read-http3-control-stream
                      #'http-kit/http3:attach-http3-peer-control-stream (type-of :invalid))
                (list #'http-kit/http3:read-http3-qpack-encoder-stream
                      #'http-kit/http3:attach-http3-peer-qpack-encoder-stream nil)
                (list #'http-kit/http3:read-http3-qpack-decoder-stream
                      #'http-kit/http3:attach-http3-peer-qpack-decoder-stream nil)))
    (destructuring-bind (reader attach invalid-detail) row
      (let* ((reads 0)
             (stream (make-http3-test-stream :kind :peer))
             (client (http3-transport-boundary-client
                      :read-stream
                      (lambda (actual &key timeout deadline)
                        (declare (ignore actual timeout deadline))
                        (incf reads)
                        (values nil t)))))
        (unwind-protect
             (progn
               (dolist (entry (list (list :invalid invalid-detail) (list client nil)))
                 (let ((failure (http3-boundary-error
                                 (lambda () (funcall reader (first entry))))))
                   (ensure-true (typep failure 'http-protocol-error))
                   (ensure-equal :http3-transport (http-error-operation failure))
                   (ensure-equal (second entry) (http-protocol-error-detail failure))))
               (ensure-equal 0 reads)
               (dolist (entry (list (list :invalid stream invalid-detail)
                                   (list client nil nil)))
                 (let ((failure (http3-boundary-error
                                 (lambda ()
                                   (funcall attach (first entry) (second entry))))))
                   (ensure-true (typep failure 'http-protocol-error))
                   (ensure-equal :http3-transport (http-error-operation failure))
                   (ensure-equal (third entry) (http-protocol-error-detail failure))))
               (ensure-equal 0 reads)
               (ensure-equal nil (http3-test-stream-closed-p stream))
               (ensure-true (eq client (funcall attach client stream)))
               (http-kit/http3:close-http3-client client)
               (let ((failure (http3-boundary-error (lambda () (funcall reader client)))))
                 (ensure-true (typep failure 'http-protocol-error))
                 (ensure-equal :http3-transport (http-error-operation failure))
                 (ensure-equal nil (http-protocol-error-detail failure)))
               (ensure-equal 0 reads)
               (ensure-true (http3-test-stream-closed-p stream)))
          (http-kit/http3:close-http3-client client)))))
  (let* ((reads 0)
         (stream (make-http3-test-stream :kind :peer))
         (other (make-http3-test-stream :kind :peer))
         (client (http3-transport-boundary-client
                  :read-stream
                  (lambda (actual &key timeout deadline)
                    (declare (ignore timeout deadline))
                    (ensure-true (eq stream actual))
                    (incf reads)
                    (values (http-kit/http3:http3-qpack-decoder-stream-prefix) nil)))))
    (unwind-protect
         (progn
           (dolist (args (list (list :invalid stream) (list client nil)))
             (let ((failure (http3-boundary-error
                             (lambda ()
                               (apply #'http-kit/http3:attach-http3-peer-qpack-decoder-stream args)))))
               (ensure-true (typep failure 'http-protocol-error))
               (ensure-equal :http3-transport (http-error-operation failure))
               (ensure-equal nil (http-protocol-error-detail failure))))
           (ensure-true
            (eq client (http-kit/http3:attach-http3-peer-qpack-decoder-stream client stream)))
           (let ((failure (http3-boundary-error
                           (lambda ()
                             (http-kit/http3:attach-http3-peer-qpack-decoder-stream client other)))))
             (ensure-true (typep failure 'http-protocol-error))
             (ensure-equal :http3-transport (http-error-operation failure))
             (ensure-equal :h3-stream-creation-error (http-protocol-error-detail failure)))
           (ensure-equal 0 reads)
           (multiple-value-bind (events ended-p)
               (http-kit/http3:read-http3-qpack-decoder-stream client)
             (ensure-equal nil events)
             (ensure-equal nil ended-p))
           (ensure-equal 1 reads))
      (http-kit/http3:close-http3-client client))
    (ensure-true (http3-test-stream-closed-p stream))
    (ensure-equal nil (http3-test-stream-closed-p other))))

(deftest http3-public-reader-manager-invalid-opener-recovery
  (let ((closed-client (http3-transport-boundary-client)))
    (http-kit/http3:close-http3-client closed-client)
    (dolist (invalid (list nil "not-a-client" closed-client))
      (let* ((request (make-http-request :method "GET" :uri "https://manager.example/resource"))
             (opened '())
             (reads 0)
             (closed '())
             (valid-client nil)
             (manager
               (http-kit/http3:make-http3-connection-manager
                :open-client
                (lambda (actual &key timeout deadline)
                  (push (list actual timeout deadline) opened)
                  (if (= 1 (length opened)) invalid
                      (setf valid-client
                            (http3-transport-boundary-client
                             :read-stream
                             (lambda (stream &key timeout deadline)
                               (declare (ignore stream timeout deadline))
                               (incf reads)
                               (values (http3-boundary-headers (list (cons ":status" "204"))) t))
                             :close-stream
                             (lambda (stream &key condition)
                               (push (list stream condition) closed)
                               (setf (http3-test-stream-closed-p stream) t)))))))))
        (unwind-protect
             (progn
               (let ((failure
                       (http3-boundary-error
                        (lambda ()
                          (http-kit/http3:send-http3-request-over-connection-manager manager request)))))
                 (ensure-true (typep failure 'http-protocol-error))
                 (ensure-equal :http3-transport (http-error-operation failure))
                 (ensure-equal (type-of invalid) (http-protocol-error-detail failure)))
               (ensure-equal nil (http-kit/http3:http3-connection-manager-clients manager))
               (ensure-true (http-kit/http3:http3-connection-manager-open-p manager))
               (ensure-equal 0 reads)
               (ensure-equal nil closed)
               (dotimes (index 2)
                 (declare (ignore index))
                 (ensure-equal 204
                               (http-response-status
                                (http-kit/http3:send-http3-request-over-connection-manager manager request))))
               (ensure-equal (list (list request nil nil) (list request nil nil)) opened)
               (ensure-equal 2 reads)
               (ensure-equal (list valid-client) (http-kit/http3:http3-connection-manager-clients manager))
               (ensure-equal 0 (http-kit/http3::http3-client-request-stream-count valid-client))
               (ensure-equal 2 (length closed))
               (ensure-true (every (lambda (entry) (null (second entry))) closed)))
          (http-kit/http3:close-http3-connection-manager manager))
        (ensure-equal 5 (length closed))
        (ensure-equal 5 (length (remove-duplicates (mapcar #'first closed) :test #'eq)))
        (ensure-true (every (lambda (entry) (http3-test-stream-closed-p (first entry))) closed))
        (ensure-equal nil (http-kit/http3:http3-client-open-p valid-client))
        (ensure-equal nil (http-kit/http3:http3-connection-manager-clients manager))
        (http-kit/http3:close-http3-connection-manager manager)
        (ensure-equal 5 (length closed))))))

(deftest http3-public-reader-manager-explicit-nil-key
  (let* ((first-request (make-http-request :method "GET" :uri "https://first.example/resource"))
         (second-request (make-http-request :method "GET" :uri "https://second.example/resource"))
         (opened '())
         (clients '())
         (reads 0)
         (closed '())
         (manager
           (http-kit/http3:make-http3-connection-manager
            :open-client
            (lambda (request &key timeout deadline)
              (push (list request timeout deadline) opened)
              (let ((client
                      (http3-transport-boundary-client
                       :read-stream
                       (lambda (stream &key timeout deadline)
                         (declare (ignore stream timeout deadline))
                         (incf reads)
                         (values (http3-boundary-headers (list (cons ":status" "204"))) t))
                       :close-stream
                       (lambda (stream &key condition)
                         (push (list stream condition) closed)
                         (setf (http3-test-stream-closed-p stream) t)))))
                (push client clients)
                client))))
         (transport (http-kit/http3:make-http3-connection-manager-transport manager)))
    (unwind-protect
         (progn
           (dolist (row (list (list first-request '(:connection-key nil) 1)
                             (list second-request '(:connection-key nil) 1)
                             (list first-request nil 2)
                             (list second-request nil 3)
                             (list first-request nil 3)
                             (list second-request '(:connection-key nil) 3)))
             (ensure-equal 204 (http-response-status (apply transport (first row) (second row))))
             (ensure-equal (third row) (length opened))
             (ensure-equal (third row) (length (http-kit/http3:http3-connection-manager-clients manager))))
           (ensure-equal (list (list second-request nil nil)
                               (list first-request nil nil)
                               (list first-request nil nil)) opened)
           (ensure-equal 6 reads)
           (ensure-equal 6 (length closed))
           (ensure-true (every (lambda (entry) (null (second entry))) closed))
           (ensure-true (every #'http-kit/http3:http3-client-open-p clients))
           (ensure-true (every (lambda (client)
                                (zerop (http-kit/http3::http3-client-request-stream-count client))) clients)))
      (http-kit/http3:close-http3-connection-manager manager))
    (ensure-equal 15 (length closed))
    (ensure-equal 15 (length (remove-duplicates (mapcar #'first closed) :test #'eq)))
    (ensure-true (every (lambda (entry) (http3-test-stream-closed-p (first entry))) closed))
    (ensure-equal nil (http-kit/http3:http3-connection-manager-clients manager))
    (ensure-equal nil (http-kit/http3:http3-connection-manager-open-p manager))
    (ensure-true (every (lambda (client) (not (http-kit/http3:http3-client-open-p client))) clients))
    (http-kit/http3:close-http3-connection-manager manager)
    (ensure-equal 15 (length closed))))

(deftest http3-public-gap-frame-construction
  (flet ((reject (thunk detail)
           (let ((failure (http3-boundary-error thunk)))
             (ensure-true (typep failure 'http-protocol-error))
             (ensure-equal :http3-frame (http-error-operation failure))
             (ensure-equal detail (http-protocol-error-detail failure)))))
    (dolist (value (list -1 (ash 1 62) 1/2 "0" nil))
      (reject (lambda () (http-kit/http3:make-http3-frame :type value)) value))
    (dolist (payload (list '(1) #(256) #(-1) #(1/2) "x"))
      (reject (lambda () (http-kit/http3:make-http3-frame :type 0 :payload payload))
              (type-of payload)))
    (reject (lambda () (http-kit/http3:encode-http3-frame :not-a-frame))
            (type-of :not-a-frame))
    (let* ((maximum (1- (ash 1 62)))
           (frame (http-kit/http3:make-http3-frame :type maximum))
           (wire (http-kit/http3:encode-http3-frame frame)))
      (ensure-equal maximum (http-kit/http3:http3-frame-type frame))
      (ensure-equal 0 (length (http-kit/http3:http3-frame-payload frame)))
      (ensure-equal '(255 255 255 255 255 255 255 255 0) (coerce wire 'list))
      (ensure-equal '(0 0)
                    (coerce (http-kit/http3:encode-http3-frame
                             (http-kit/http3:make-http3-frame :type 0 :payload nil)) 'list)))
    (dolist (payload (list (make-array 2 :element-type '(unsigned-byte 8)
                                        :initial-contents '(7 8))
                          (vector 7 8)))
      (let ((frame (http-kit/http3:make-http3-frame :type 0 :payload payload)))
        (setf (aref payload 0) 99)
        (ensure-equal '(7 8) (coerce (http-kit/http3:http3-frame-payload frame) 'list))
        (ensure-equal '(0 2 7 8) (coerce (http-kit/http3:encode-http3-frame frame) 'list))))))

(deftest http3-public-gap-frame-prefix-and-input
  (flet ((reject (thunk operation detail)
           (let ((failure (http3-boundary-error thunk)))
             (ensure-true (typep failure 'http-protocol-error))
             (ensure-equal operation (http-error-operation failure))
             (ensure-equal detail (http-protocol-error-detail failure)))))
    (dolist (value (list nil '(0 0) #(256) #(-1) #(1/2) "xx"))
      (reject (lambda () (http-kit/http3:decode-http3-frames value))
              :http3-frame (type-of value)))
    (let ((empty (make-array 0 :element-type '(unsigned-byte 8))))
      (dolist (limit '(-1 1/2 nil))
        (reject (lambda () (http-kit/http3:decode-http3-frames empty :max-frame-size limit))
                :http3-frame limit)))
    (multiple-value-bind (frames remainder)
        (http-kit/http3:decode-http3-frames (vector 0 0))
      (ensure-equal 1 (length frames))
      (ensure-equal 0 (http-kit/http3:http3-frame-type (first frames)))
      (ensure-equal nil (coerce (http-kit/http3:http3-frame-payload (first frames)) 'list))
      (ensure-equal nil (coerce remainder 'list)))
    (let ((prefix (http-kit/http3:encode-http3-frame
                   (http-kit/http3:make-http3-frame :type 0))))
      (dolist (type (list 64 16384 (ash 1 30)))
        (let ((encoded-type (http-kit/http3:http3-varint-encode type)))
          (loop for cut from 1 below (length encoded-type)
                for suffix = (subseq encoded-type 0 cut)
                for wire = (http3-test-concat-octets prefix suffix)
                do (let ((failure (http3-boundary-error
                                   (lambda () (http-kit/http3:decode-http3-frames wire)))))
                     (ensure-true (typep failure 'http-protocol-error))
                     (ensure-equal :http3-varint (http-error-operation failure)))
                   (multiple-value-bind (frames remainder)
                       (http-kit/http3:decode-http3-frames wire :allow-incomplete-p t)
                     (ensure-equal 1 (length frames))
                     (ensure-equal 0 (http-kit/http3:http3-frame-type (first frames)))
                     (ensure-equal 0 (length (http-kit/http3:http3-frame-payload (first frames))))
                     (ensure-equal (coerce suffix 'list) (coerce remainder 'list))))))
      (multiple-value-bind (frames remainder)
          (http-kit/http3:decode-http3-frames prefix :max-frame-size 0)
        (ensure-equal 1 (length frames))
        (ensure-equal 0 (length remainder))))))

(deftest http3-public-gap-settings-value-and-shape
  (flet ((reject (thunk detail)
           (let ((failure (http3-boundary-error thunk)))
             (ensure-true (typep failure 'http-protocol-error))
             (ensure-equal :http3-frame (http-error-operation failure))
             (ensure-equal detail (http-protocol-error-detail failure)))))
    (dolist (keyword '(:qpack-max-table-capacity :qpack-blocked-streams
                       :max-field-section-size :enable-connect :h3-datagram))
      (dolist (value (list -1 (ash 1 62) 1/2))
        (reject (lambda () (apply #'http-kit/http3:make-http3-settings-frame
                                 (list keyword value)))
                (list keyword value))))
    (dolist (pair (list :bad (cons -1 0) (cons (ash 1 62) 0) (cons 1/2 0)))
      (reject (lambda () (http-kit/http3:make-http3-settings-frame :extra-settings (list pair)))
              pair))
    (let ((pair (cons 100 -1)))
      (reject (lambda () (http-kit/http3:make-http3-settings-frame :extra-settings (list pair)))
              (list pair -1)))
    (reject (lambda () (http-kit/http3:decode-http3-settings
                       (http-kit/http3:make-http3-frame :type 0))) 0)
    (reject (lambda () (http-kit/http3:decode-http3-settings "settings"))
            (type-of "settings"))
    (let* ((maximum (1- (ash 1 62)))
           (frame (http-kit/http3:make-http3-settings-frame
                   :max-field-section-size 0 :enable-connect 0 :h3-datagram 0
                   :extra-settings (list (cons maximum maximum))))
           (settings (http-kit/http3:decode-http3-settings frame)))
      (ensure-equal 6 (length settings))
      (dolist (identifier '(1 7 6 8 51))
        (ensure-equal 0 (cdr (assoc identifier settings))))
      (ensure-equal maximum (cdr (assoc maximum settings)))
      (ensure-equal settings (http-kit/http3:decode-http3-settings
                              (http-kit/http3:http3-frame-payload frame))))))

(deftest http3-public-gap-priority-arguments
  (flet ((reject (thunk detail)
           (let ((failure (http3-boundary-error thunk)))
             (ensure-true (typep failure 'http-protocol-error))
             (ensure-equal :http3-frame (http-error-operation failure))
             (ensure-equal detail (http-protocol-error-detail failure)))))
    (dolist (value (list -1 (ash 1 62) 1/2 nil))
      (reject (lambda () (http-kit/http3:make-http3-priority-update-frame
                         :element-id value :priority-field-value "u=3")) value))
    (dolist (value '(nil 7 :bad))
      (reject (lambda () (http-kit/http3:make-http3-priority-update-frame
                         :element-id 0 :priority-field-value value)) (type-of value)))
    (reject (lambda () (http-kit/http3:decode-http3-priority-update :bad)) nil)
    (reject (lambda () (http-kit/http3:decode-http3-priority-update
                       (http-kit/http3:make-http3-frame :type 4))) 4)
    (dolist (kind '(:request :push))
      (let* ((maximum (1- (ash 1 62)))
             (frame (http-kit/http3:make-http3-priority-update-frame
                     :element-id maximum :priority-field-value "u=3, i" :kind kind))
             (wire (http-kit/http3:encode-http3-frame frame)))
        (multiple-value-bind (frames remainder) (http-kit/http3:decode-http3-frames wire)
          (ensure-equal 1 (length frames))
          (ensure-equal 0 (length remainder))
          (multiple-value-bind (id value decoded-kind)
              (http-kit/http3:decode-http3-priority-update (first frames))
            (ensure-equal maximum id)
            (ensure-equal "u=3, i" value)
            (ensure-equal kind decoded-kind)))))))

(deftest http3-public-gap-control-preflight
  (let ((state (http-kit/http3:make-http3-control-state :peer-role :server))
        (settings (http-kit/http3:make-http3-settings-frame)))
    (flet ((reject (thunk detail)
             (let ((failure (http3-boundary-error thunk)))
               (ensure-true (typep failure 'http-protocol-error))
               (ensure-equal :http3-control-stream (http-error-operation failure))
               (ensure-equal detail (http-protocol-error-detail failure))
               (ensure-equal nil (http-kit/http3:http3-control-state-settings-received-p state))
               (ensure-equal nil (http-kit/http3:http3-control-state-settings state)))))
      (reject (lambda () (http-kit/http3:process-http3-control-frame :bad settings)) (type-of :bad))
      (reject (lambda () (http-kit/http3:process-http3-control-frame state :bad)) (type-of :bad))
      (reject (lambda () (http-kit/http3:process-http3-control-bytes
                         :bad (http-kit/http3:encode-http3-frame settings))) (type-of :bad)))
    (let* ((suffix (subseq (http-kit/http3:http3-varint-encode 64) 0 1))
           (wire (http3-test-concat-octets
                  (http-kit/http3:encode-http3-frame settings)
                  (http-kit/http3:encode-http3-frame (http-kit/http3:make-http3-frame :type 64))
                  suffix))
           (failure (http3-boundary-error
                     (lambda () (http-kit/http3:process-http3-control-bytes state wire)))))
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :http3-varint (http-error-operation failure))
      (ensure-equal nil (http-kit/http3:http3-control-state-settings-received-p state))
      (ensure-equal nil (http-kit/http3:http3-control-state-settings state))
      (multiple-value-bind (effects remainder)
          (http-kit/http3:process-http3-control-bytes state wire :allow-incomplete-p t)
        (ensure-equal '(:settings :extension) effects)
        (ensure-equal (coerce suffix 'list) (coerce remainder 'list))
        (ensure-equal t (http-kit/http3:http3-control-state-settings-received-p state))
        (ensure-equal '((7 . 0) (1 . 0)) (http-kit/http3:http3-control-state-settings state))))))

(deftest http3-public-gap-qpack-table-construction-and-pins
  (flet ((reject (thunk detail)
           (let ((failure (http3-boundary-error thunk)))
             (ensure-true (typep failure 'http-protocol-error))
             (ensure-equal :qpack (http-error-operation failure))
             (ensure-equal detail (http-protocol-error-detail failure)))))
    (dolist (value '(-1 1/2 nil))
      (reject
       (lambda ()
         (http-message-kit:make-qpack-context
          :max-table-capacity value :table-capacity value :huffman nil))
       value))
    (dolist (value '(-1 65 1/2 nil))
      (reject
       (lambda ()
         (http-message-kit:make-qpack-context
          :max-table-capacity 64 :table-capacity value :huffman nil))
       value))
    (let* ((encoder (http-message-kit:make-qpack-context
                     :max-table-capacity 64 :table-capacity 64 :huffman nil))
           (decoder (http-message-kit:make-qpack-context
                     :max-table-capacity 64 :table-capacity 0 :huffman nil))
           (insert-wire
             (http-message-kit:qpack-encode-insert-with-literal-name
              encoder "x" "y")))
      (multiple-value-bind (events consumed)
          (http-message-kit:qpack-decode-encoder-stream decoder insert-wire)
        (ensure-equal '((:insert "x" "y")) events)
        (ensure-equal (length insert-wire) consumed))
      (let ((section
              (http3-test-qpack-encode
               '(("x" . "y"))
               :context encoder :use-dynamic-p t :stream-id 4 :huffman nil)))
        (ensure-equal '(("x" . "y"))
                      (http3-test-qpack-decode
                       section :context decoder :stream-id 4))
        (dolist (thunk
                 (list (lambda ()
                         (http-message-kit:qpack-context-set-capacity encoder 0))
                       (lambda ()
                         (http-message-kit:qpack-encode-insert-with-literal-name
                          encoder "x" "z"))))
          (reject thunk 0)
          (ensure-equal 64
                        (http-message-kit:qpack-context-table-capacity encoder))
          (ensure-equal 34
                        (http-message-kit:qpack-context-table-size encoder))
          (ensure-equal 1
                        (http-message-kit:qpack-context-insert-count encoder))
          (ensure-equal 1
                        (http-message-kit:qpack-context-dynamic-entry-count encoder)))
        (multiple-value-bind (events consumed)
            (http-message-kit:qpack-decode-decoder-stream
             encoder (http-message-kit:qpack-encode-section-ack 4))
          (ensure-equal '((:section-ack 4)) events)
          (ensure-equal 1 consumed))
        (ensure-equal 1
                      (http-message-kit:qpack-context-known-received-count encoder))
        (http-message-kit:qpack-context-set-capacity encoder 0)
        (ensure-equal 0
                      (http-message-kit:qpack-context-dynamic-entry-count encoder))
        (ensure-equal 0
                      (http-message-kit:qpack-context-table-size encoder))
        (http-message-kit:qpack-context-set-capacity encoder 64)
        (http-message-kit:qpack-encode-insert-with-literal-name
         encoder "x" "z")
        (ensure-equal 2
                      (http-message-kit:qpack-context-insert-count encoder))
        (ensure-equal 1
                      (http-message-kit:qpack-context-dynamic-entry-count encoder))))))

(deftest http3-public-gap-qpack-instruction-encoding
  (flet ((reject (thunk detail)
           (let ((failure (http3-boundary-error thunk)))
             (ensure-true (typep failure 'http-protocol-error))
             (ensure-equal :qpack (http-error-operation failure))
             (ensure-equal detail (http-protocol-error-detail failure)))))
    (dolist (value '(-1 1/2 nil))
      (dolist (encoder
               (list #'http3-test-qpack-encode-set-capacity
                     (lambda (index)
                       (http3-test-qpack-encode-insert-with-name-reference
                        index "y"))
                     #'http3-test-qpack-encode-duplicate
                     #'http-message-kit:qpack-encode-section-ack
                     #'http-message-kit:qpack-encode-stream-cancellation))
        (reject (lambda () (funcall encoder value)) value)))
    (dolist (value '(0 -1 1/2 nil))
      (reject
       (lambda ()
         (http-message-kit:qpack-encode-insert-count-increment value))
       value))
    (ensure-equal '(63 33)
                  (coerce (http3-test-qpack-encode-set-capacity 64) 'list))
    (ensure-equal '(192 1 121)
                  (coerce (http3-test-qpack-encode-insert-with-name-reference
                           0 "y")
                          'list))
    (ensure-equal '(0)
                  (coerce (http3-test-qpack-encode-duplicate 0) 'list))
    (let* ((context
             (http-message-kit:make-qpack-context
              :max-table-capacity 128 :table-capacity 128 :huffman nil))
           (wire (http3-test-concat-octets
                  (http3-test-qpack-encode-set-capacity 128)
                  (http3-test-qpack-encode-insert-with-literal-name "x" "y")
                  (http3-test-qpack-encode-duplicate 0)
                  (http3-test-qpack-encode-insert-with-name-reference 0 "y"))))
      (multiple-value-bind (events consumed)
          (http-message-kit:qpack-decode-encoder-stream context wire)
        (ensure-equal '(:set-capacity :insert :duplicate :insert)
                      (mapcar #'first events))
        (ensure-equal (length wire) consumed))
      (ensure-equal 128
                    (http-message-kit:qpack-context-table-capacity context))
      (ensure-equal 3
                    (http-message-kit:qpack-context-insert-count context))
      (let ((fields '((":authority" . "y")
                      ("x" . "y")
                      ("x" . "y"))))
        (ensure-equal
         fields
         (http3-test-qpack-decode
          (http3-test-qpack-encode
           fields :context context :use-dynamic-p t :stream-id 0 :huffman nil)
          :context context :stream-id 0))))
    (let* ((context
             (http-message-kit:make-qpack-context
              :max-table-capacity 128 :table-capacity 128 :huffman nil))
           (wire nil))
      (http-message-kit:qpack-encode-insert-with-literal-name
       context "x" "one")
      (http3-test-qpack-encode
       '(("x" . "one"))
       :context context :use-dynamic-p t :stream-id 0 :huffman nil)
      (http-message-kit:qpack-encode-insert-with-literal-name
       context "x" "two")
      (http3-test-qpack-encode
       '(("x" . "two"))
       :context context :use-dynamic-p t :stream-id 0 :huffman nil)
      (setf wire
            (http3-test-concat-octets
             (http-message-kit:qpack-encode-section-ack 0)
             (http-message-kit:qpack-encode-stream-cancellation 0)
             (http-message-kit:qpack-encode-insert-count-increment 1)))
      (ensure-equal '(128 64 1) (coerce wire 'list))
      (multiple-value-bind (events consumed)
          (http-message-kit:qpack-decode-decoder-stream context wire)
        (ensure-equal '((:section-ack 0)
                        (:stream-cancellation 0)
                        (:insert-count-increment 1))
                      events)
        (ensure-equal 3 consumed)
        (ensure-equal 2
                      (http-message-kit:qpack-context-known-received-count
                       context))))))

(deftest http3-public-gap-qpack-complete-instruction-errors
  (flet ((reject (thunk operation detail)
           (let ((failure (http3-boundary-error thunk)))
             (ensure-true (typep failure 'http-protocol-error))
             (ensure-equal operation (http-error-operation failure))
             (ensure-equal detail (http-protocol-error-detail failure)))))
    (let* ((encoder-context
             (http-message-kit:make-qpack-context
              :max-table-capacity 64 :table-capacity 0 :huffman nil))
           (wire (http3-test-qpack-encode-set-capacity 64)))
      (reject
       (lambda ()
         (http-message-kit:qpack-decode-encoder-stream
          encoder-context wire :max-instruction-bytes 1))
       :qpack 2)
      (ensure-equal 0
                    (http-message-kit:qpack-context-table-capacity
                     encoder-context))
      (multiple-value-bind (events consumed)
          (http-message-kit:qpack-decode-encoder-stream
           encoder-context wire :max-instruction-bytes 2)
        (ensure-equal '((:set-capacity 64)) events)
        (ensure-equal 2 consumed))
      (ensure-equal 64
                    (http-message-kit:qpack-context-table-capacity
                     encoder-context))
      (dolist (incremental '(nil t))
        (reject
         (lambda ()
           (http-message-kit:qpack-decode-encoder-stream
            encoder-context
            (http3-test-qpack-encode-insert-with-name-reference 999 "y")
            :allow-incomplete-p incremental))
         :qpack 999)
        (reject
         (lambda ()
           (http-message-kit:qpack-decode-encoder-stream
            encoder-context
            (make-array 6 :element-type '(unsigned-byte 8)
                        :initial-contents '(63 128 128 128 128 128))
            :allow-incomplete-p incremental))
         :hpack 31)
        (ensure-equal 64
                      (http-message-kit:qpack-context-table-capacity
                       encoder-context))
        (ensure-equal 0
                      (http-message-kit:qpack-context-dynamic-entry-count
                       encoder-context))
        (ensure-equal 0
                      (http-message-kit:qpack-context-insert-count
                       encoder-context)))
      (let ((partial (subseq wire 0 1)))
        (reject
         (lambda ()
           (http-message-kit:qpack-decode-encoder-stream
            encoder-context partial))
         :hpack 1)
        (multiple-value-bind (events consumed)
            (http-message-kit:qpack-decode-encoder-stream
             encoder-context partial :allow-incomplete-p t)
          (ensure-equal nil events)
          (ensure-equal 0 consumed))
        (ensure-equal 64
                      (http-message-kit:qpack-context-table-capacity
                       encoder-context))))
    (let* ((decoder-context
             (http-message-kit:make-qpack-context
              :max-table-capacity 64 :table-capacity 64 :huffman nil))
           (wire (http-message-kit:qpack-encode-section-ack 128)))
      (http-message-kit:qpack-encode-insert-with-literal-name
       decoder-context "x" "y")
      (http3-test-qpack-encode
       '(("x" . "y"))
       :context decoder-context :use-dynamic-p t :stream-id 128 :huffman nil)
      (reject
       (lambda ()
         (http-message-kit:qpack-decode-decoder-stream
          decoder-context wire :max-instruction-bytes 1))
       :qpack 2)
      (ensure-equal 0
                    (http-message-kit:qpack-context-known-received-count
                     decoder-context))
      (dolist (incremental '(nil t))
        (reject
         (lambda ()
           (http-message-kit:qpack-decode-decoder-stream
            decoder-context
            (make-array 6 :element-type '(unsigned-byte 8)
                        :initial-contents '(255 128 128 128 128 128))
            :allow-incomplete-p incremental))
         :hpack 127)
        (ensure-equal 0
                      (http-message-kit:qpack-context-known-received-count
                       decoder-context)))
      (let ((partial (subseq wire 0 1)))
        (reject
         (lambda ()
           (http-message-kit:qpack-decode-decoder-stream
            decoder-context partial))
         :hpack 1)
        (multiple-value-bind (events consumed)
            (http-message-kit:qpack-decode-decoder-stream
             decoder-context partial :allow-incomplete-p t)
          (ensure-equal nil events)
          (ensure-equal 0 consumed))
        (ensure-equal 0
                      (http-message-kit:qpack-context-known-received-count
                       decoder-context)))
      (multiple-value-bind (events consumed)
          (http-message-kit:qpack-decode-decoder-stream
           decoder-context wire :max-instruction-bytes 2)
        (ensure-equal '((:section-ack 128)) events)
        (ensure-equal 2 consumed))
      (ensure-equal 1
                    (http-message-kit:qpack-context-known-received-count
                     decoder-context)))))

(deftest http3-public-response-information-rejections
  (dolist (fields '(((":status" . "101"))
                    ((":status" . "103") ("content-length" . "0"))
                    ((":status" . "103") ("link" . "</a>"))))
    (let* ((valid-p (and (equal (cdr (first fields)) "103")
                         (equal (car (second fields)) "link")))
           (fixture (http3-boundary-fixture
                     (list (list (http3-boundary-headers fields) nil)
                           (list (http3-boundary-headers '((":status" . "204"))) t))))
           (statuses nil)
           (body-calls 0)
           (failure
             (http3-boundary-error
              (lambda ()
                (let ((response
                        (http-kit/http3:send-http3-request
                         (http3-boundary-fixture-client fixture)
                         (make-http-request :method "GET" :uri "https://example.test/")
                         :on-information
                         (lambda (response)
                           (ensure-equal 1 (http3-boundary-fixture-reads fixture))
                           (ensure-equal 0 (length (http-response-body response)))
                           (push (http-response-status response) statuses))
                         :on-body-chunk (lambda (chunk)
                                          (declare (ignore chunk))
                                          (incf body-calls)))))
                  (ensure-equal 204 (http-response-status response)))))))
      (ensure-equal 0 body-calls)
      (ensure-equal (if valid-p '(103) nil) statuses)
      (ensure-equal (if valid-p 2 1) (http3-boundary-fixture-reads fixture))
      (cond
        (valid-p (ensure-equal nil failure))
        ((equal (cdr (first fields)) "101")
         (ensure-true (typep failure 'http-protocol-error))
         (ensure-equal :http3-transport (http-error-operation failure))
         (ensure-equal nil (http-protocol-error-detail failure)))
        (t
         (ensure-true (typep failure 'http-invalid-header))
         (ensure-equal :http3-header (http-error-operation failure))
         (ensure-equal "content-length" (http-invalid-header-name failure))))
      (ensure-equal 1 (length (http3-boundary-fixture-closes fixture)))
      (http3-boundary-check-release fixture failure)
      (http-kit/http3:close-http3-client (http3-boundary-fixture-client fixture)))))

(deftest http3-public-response-repeated-trailer-blocks
  (dolist (repeated-p '(nil t))
    (let* ((entries
             (list (list (http3-boundary-headers '((":status" . "200"))) nil)
                   (list (http3-boundary-data "a") nil)
                   (list (http3-boundary-headers '(("x-end" . "one")))
                         (not repeated-p))))
           (fixture
             (http3-boundary-fixture
              (if repeated-p
                  (append entries
                          (list (list (http3-boundary-headers '(("x-end" . "two"))) t)))
                  entries)))
           (chunks nil)
           (failure
             (http3-boundary-error
              (lambda ()
                (let ((response
                        (http-kit/http3:send-http3-request
                         (http3-boundary-fixture-client fixture)
                         (make-http-request :method "GET" :uri "https://example.test/")
                         :on-body-chunk (lambda (chunk) (push chunk chunks)))))
                  (ensure-equal 200 (http-response-status response))
                  (ensure-true (equalp (ascii "a") (http-response-body response)))
                  (ensure-equal "one" (http-header-value (http-response-trailers response)
                                                        "x-end")))))))
      (ensure-true (equalp (list (ascii "a")) chunks))
      (ensure-equal (if repeated-p 4 3) (http3-boundary-fixture-reads fixture))
      (if repeated-p
          (progn
            (ensure-true (typep failure 'http-protocol-error))
            (ensure-equal :http3-transport (http-error-operation failure))
            (ensure-equal nil (http-protocol-error-detail failure)))
          (ensure-equal nil failure))
      (ensure-equal 1 (length (http3-boundary-fixture-closes fixture)))
      (http3-boundary-check-release fixture failure)
      (http-kit/http3:close-http3-client (http3-boundary-fixture-client fixture)))))

(deftest http3-public-response-callback-order-and-request-fin
  (let* ((fixture
           (http3-boundary-fixture
            (list (list (http3-boundary-headers '((":status" . "103"))) nil)
                  (list (http3-boundary-headers '((":status" . "200")
                                                  ("content-length" . "1"))) nil)
                  (list (http3-boundary-data "a") t))))
         (events nil)
         (response
           (http-kit/http3:send-http3-request
            (http3-boundary-fixture-client fixture)
            (make-http-request :method "GET" :uri "https://example.test/")
            :collect-body-p nil
            :on-stream-open
            (lambda (stream stream-id cancel)
              (ensure-true (eq stream (first (http3-boundary-fixture-streams fixture))))
              (ensure-equal 0 stream-id)
              (ensure-true (functionp cancel))
              (ensure-equal nil (http3-test-stream-writes stream))
              (ensure-equal 0 (http3-boundary-fixture-reads fixture))
              (push :open events))
            :on-information
            (lambda (response)
              (ensure-equal 103 (http-response-status response))
              (ensure-equal 1 (http3-boundary-fixture-reads fixture))
              (push :information events))
            :on-body-chunk
            (lambda (chunk)
              (ensure-true (equalp (ascii "a") chunk))
              (ensure-equal 3 (http3-boundary-fixture-reads fixture))
              (push :body events)))))
    (ensure-equal '(:open :information :body) (reverse events))
    (ensure-equal 200 (http-response-status response))
    (ensure-equal 0 (length (http-response-body response)))
    (let* ((writes (reverse (http3-test-stream-writes
                            (first (http3-boundary-fixture-streams fixture)))))
           (headers (http-kit/http3:decode-http3-frames (first (first writes))))
           (data (http-kit/http3:decode-http3-frames (first (second writes)))))
      (ensure-equal 2 (length writes))
      (ensure-equal nil (second (first writes)))
      (ensure-equal t (second (second writes)))
      (ensure-equal 1 (length headers))
      (ensure-equal 1 (length data))
      (ensure-equal http-kit/http3:+http3-headers-type+
                    (http-kit/http3:http3-frame-type (first headers)))
      (ensure-true (member '(":method" . "GET")
                           (http3-test-qpack-decode (http-kit/http3:http3-frame-payload (first headers)))
                           :test #'equal))
      (ensure-equal http-kit/http3:+http3-data-type+
                    (http-kit/http3:http3-frame-type (first data)))
      (ensure-equal 0 (length (http-kit/http3:http3-frame-payload (first data)))))
    (ensure-equal 1 (length (http3-boundary-fixture-closes fixture)))
    (http3-boundary-check-release fixture nil)
    (http-kit/http3:close-http3-client (http3-boundary-fixture-client fixture))))

(deftest http3-public-response-callback-failure-releases-capacity
  (dolist (phase '(:open :information :body))
    (let* ((fixture
             (http3-boundary-fixture
              (list (list (http3-boundary-headers '((":status" . "103"))) nil)
                    (list (http3-boundary-headers '((":status" . "200")
                                                    ("content-length" . "1"))) nil)
                    (list (http3-boundary-data "a") t))
              :max-request-streams 1))
           (primary (make-condition 'simple-error :format-control "Callback failure."))
           (calls nil)
           (failure
             (http3-boundary-error
              (lambda ()
                (http-kit/http3:send-http3-request
                 (http3-boundary-fixture-client fixture)
                 (make-http-request :method "GET" :uri "https://example.test/")
                 :on-stream-open
                 (lambda (stream stream-id cancel)
                   (declare (ignore stream stream-id cancel))
                   (push :open calls)
                   (when (eq phase :open) (error primary)))
                 :on-information
                 (lambda (response)
                   (ensure-equal 103 (http-response-status response))
                   (push :information calls)
                   (when (eq phase :information) (error primary)))
                 :on-body-chunk
                 (lambda (chunk)
                   (ensure-true (equalp (ascii "a") chunk))
                   (push :body calls)
                   (when (eq phase :body) (error primary))))))))
      (ensure-true (eq primary failure))
      (ensure-equal (ecase phase (:open '(:open))
                                (:information '(:open :information))
                                (:body '(:open :information :body)))
                    (reverse calls))
      (ensure-equal (ecase phase (:open 0) (:information 1) (:body 3))
                    (http3-boundary-fixture-reads fixture))
      (when (eq phase :open)
        (ensure-equal nil (http3-test-stream-writes
                           (first (http3-boundary-fixture-streams fixture)))))
      (ensure-equal 1 (length (http3-boundary-fixture-closes fixture)))
      (http3-boundary-check-release fixture primary)
      (let ((response
              (http-kit/http3:send-http3-request
               (http3-boundary-fixture-client fixture)
               (make-http-request :method "GET" :uri "https://example.test/"))))
        (ensure-equal 200 (http-response-status response))
        (ensure-true (equalp (ascii "a") (http-response-body response))))
      (ensure-equal 2 (length (http3-boundary-fixture-streams fixture)))
      (ensure-equal 2 (length (http3-boundary-fixture-closes fixture)))
      (http3-boundary-check-release fixture nil)
      (http-kit/http3:close-http3-client (http3-boundary-fixture-client fixture)))))

(deftest http3-public-request-transport-failure-and-reuse
  (dolist (phase '(:write :read))
    (let ((primary (make-condition 'simple-error :format-control "Transport failure."))
          (armed-p t)
          (streams nil)
          (closes nil)
          (read-calls 0)
          (write-calls 0))
      (labels ((open-stream (request &key stream-type timeout deadline)
                 (declare (ignore request timeout deadline))
                 (let ((stream (make-http3-test-stream
                                :kind stream-type
                                :reads (list (list (http3-boundary-headers
                                                    '((":status" . "204"))) t)))))
                   (when (eq stream-type :request) (push stream streams))
                   (values stream 0)))
               (write-stream (stream bytes &key fin-p timeout deadline)
                 (declare (ignore timeout deadline))
                 (when (eq :request (http3-test-stream-kind stream))
                   (incf write-calls)
                   (when (and armed-p (eq phase :write)) (error primary)))
                 (push (list (copy-seq bytes) fin-p) (http3-test-stream-writes stream)))
               (read-stream (stream &key timeout deadline)
                 (declare (ignore timeout deadline))
                 (incf read-calls)
                 (when (and armed-p (eq phase :read)) (error primary))
                 (let ((entry (pop (http3-test-stream-reads stream))))
                   (values (first entry) (second entry))))
               (close-stream (stream &key condition)
                 (setf (http3-test-stream-closed-p stream) t)
                 (when (eq :request (http3-test-stream-kind stream))
                   (push (cons stream condition) closes))))
        (let* ((client (http-kit/http3:make-http3-client
                        :max-request-streams 1 :open-stream #'open-stream
                        :write-stream #'write-stream :read-stream #'read-stream
                        :close-stream #'close-stream))
               (request (make-http-request :method "GET" :uri "https://example.test/"))
               (failure (http3-boundary-error
                         (lambda () (http-kit/http3:send-http3-request client request)))))
          (ensure-true (eq primary failure))
          (ensure-equal (if (eq phase :write) 1 2) write-calls)
          (ensure-equal (if (eq phase :write) 0 1) read-calls)
          (ensure-equal 1 (length streams))
          (ensure-equal 1 (length closes))
          (ensure-true (eq (first streams) (caar closes)))
          (ensure-true (eq primary (cdar closes)))
          (ensure-true (http3-test-stream-closed-p (first streams)))
          (ensure-equal 0 (http-kit/http3::http3-client-request-stream-count client))
          (setf armed-p nil)
          (ensure-equal 204 (http-response-status
                             (http-kit/http3:send-http3-request client request)))
          (ensure-equal 2 (length streams))
          (ensure-equal 2 (length closes))
          (ensure-equal nil (cdar closes))
          (ensure-true (http3-test-stream-closed-p (first streams)))
          (ensure-equal 0 (http-kit/http3::http3-client-request-stream-count client))
          (http-kit/http3:close-http3-client client))))))

(deftest http3-public-request-nil-open-leaves-capacity-available
  (let ((armed-p t) (streams nil) (closes nil) (reads 0) (writes 0))
    (labels ((open-stream (request &key stream-type timeout deadline)
               (declare (ignore request timeout deadline))
               (if (and armed-p (eq stream-type :request))
                   (values nil nil)
                   (let ((stream (make-http3-test-stream
                                  :kind stream-type
                                  :reads (list (list (http3-boundary-headers
                                                      '((":status" . "204"))) t)))))
                     (when (eq stream-type :request) (push stream streams))
                     (values stream 0))))
             (write-stream (stream bytes &key fin-p timeout deadline)
               (declare (ignore bytes fin-p timeout deadline))
               (when (eq :request (http3-test-stream-kind stream)) (incf writes)))
             (read-stream (stream &key timeout deadline)
               (declare (ignore timeout deadline))
               (incf reads)
               (let ((entry (pop (http3-test-stream-reads stream))))
                 (values (first entry) (second entry))))
             (close-stream (stream &key condition)
               (setf (http3-test-stream-closed-p stream) t)
               (push (cons stream condition) closes)))
      (let* ((client (http-kit/http3:make-http3-client
                      :max-request-streams 1 :open-stream #'open-stream
                      :write-stream #'write-stream :read-stream #'read-stream
                      :close-stream #'close-stream))
             (request (make-http-request :method "GET" :uri "https://example.test/"))
             (failure (http3-boundary-error
                       (lambda () (http-kit/http3:send-http3-request client request)))))
        (ensure-true (typep failure 'http-protocol-error))
        (ensure-equal :http3-transport (http-error-operation failure))
        (ensure-equal nil (http-protocol-error-detail failure))
        (ensure-equal nil streams)
        (ensure-equal nil closes)
        (ensure-equal 0 reads)
        (ensure-equal 0 writes)
        (ensure-equal 0 (http-kit/http3::http3-client-request-stream-count client))
        (setf armed-p nil)
        (ensure-equal 204 (http-response-status
                           (http-kit/http3:send-http3-request client request)))
        (ensure-equal 1 reads)
        (ensure-equal 2 writes)
        (ensure-equal 1 (length streams))
        (ensure-equal 1 (length closes))
        (ensure-true (eq (first streams) (caar closes)))
        (ensure-equal nil (cdar closes))
        (ensure-true (http3-test-stream-closed-p (first streams)))
        (ensure-equal 0 (http-kit/http3::http3-client-request-stream-count client))
        (http-kit/http3:close-http3-client client)
        (ensure-equal 4 (length closes))
        (http-kit/http3:close-http3-client client)
        (ensure-equal 4 (length closes))))))

(deftest http3-public-producer-overflow-stops-before-offending-data
  (dolist (declared '(0 2 3))
    (let* ((fixture (http3-boundary-fixture
                     (list (list (http3-boundary-headers '((":status" . "204"))) t))))
           (client (http3-boundary-fixture-client fixture))
           (chunks (list #(1 2) #(3) nil))
           (limits nil)
           (response nil)
           (failure
             (http3-boundary-error
              (lambda ()
                (setf response
                      (http-kit/http3:send-http3-request
                       client (make-http-request :method "POST" :uri "https://example.test/upload")
                       :request-body-length declared
                       :request-body-function
                       (lambda (maximum)
                         (push maximum limits)
                         (pop chunks)))))))
           (writes (reverse (http3-test-stream-writes
                             (first (http3-boundary-fixture-streams fixture)))))
           (frames (mapcar (lambda (entry)
                             (first (http-kit/http3:decode-http3-frames (first entry))))
                           writes)))
      (ensure-equal (make-list (case declared (0 1) (2 2) (3 3)) :initial-element 16384)
                    (reverse limits))
      (ensure-equal http-kit/http3:+http3-headers-type+
                    (http-kit/http3:http3-frame-type (first frames)))
      (ensure-equal (write-to-string declared)
                    (cdr (assoc "content-length"
                                (http3-test-qpack-decode (http-kit/http3:http3-frame-payload (first frames)))
                                :test #'string=)))
      (if (= declared 3)
          (progn
            (ensure-equal nil failure)
            (ensure-equal 204 (http-response-status response))
            (ensure-equal 4 (length writes))
            (ensure-equal '(nil nil nil t) (mapcar #'second writes))
            (ensure-equal '(1 2) (coerce (http-kit/http3:http3-frame-payload (second frames)) 'list))
            (ensure-equal '(3) (coerce (http-kit/http3:http3-frame-payload (third frames)) 'list))
            (ensure-equal 0 (length (http-kit/http3:http3-frame-payload (fourth frames)))))
          (progn
            (ensure-true (typep failure 'http-protocol-error))
            (ensure-equal :http3-transport (http-error-operation failure))
            (ensure-equal (if (zerop declared) '(2 0) '(3 2))
                          (http-protocol-error-detail failure))
            (ensure-equal nil response)
            (ensure-equal (if (zerop declared) 1 2) (length writes))
            (ensure-equal (if (zerop declared) '(nil) '(nil nil)) (mapcar #'second writes))
            (when (= declared 2)
              (ensure-equal '(1 2) (coerce (http-kit/http3:http3-frame-payload (second frames)) 'list)))))
      (ensure-equal (if (= declared 3) 1 0) (http3-boundary-fixture-reads fixture))
      (ensure-equal 1 (length (http3-boundary-fixture-closes fixture)))
      (http3-boundary-check-release fixture failure)
      (http-kit/http3:close-http3-client client))))

(deftest http3-public-producer-invalid-chunks-and-reuse
  (dolist (chunk (list 42 "a" (make-array '(1 1) :initial-element 0)
                       #() #(256)
                       (make-array 16385 :element-type '(unsigned-byte 8) :initial-element 0)))
    (let* ((fixture (http3-boundary-fixture
                     (list (list (http3-boundary-headers '((":status" . "204"))) t))
                     :max-request-streams 1))
           (client (http3-boundary-fixture-client fixture))
           (calls 0)
           (failure
             (http3-boundary-error
              (lambda ()
                (http-kit/http3:send-http3-request
                 client (make-http-request :method "POST" :uri "https://example.test/upload")
                 :request-body-function
                 (lambda (maximum)
                   (incf calls)
                   (ensure-equal 16384 maximum)
                   chunk))))))
      (ensure-equal 1 calls)
      (ensure-true (typep failure 'http-protocol-error))
      (ensure-equal :http3-transport (http-error-operation failure))
      (ensure-equal (cond ((or (not (arrayp chunk)) (stringp chunk)
                              (/= 1 (array-rank chunk))) (type-of chunk))
                          ((zerop (length chunk)) chunk)
                          ((> (length chunk) 16384) '(16385 16384))
                          (t 256))
                    (http-protocol-error-detail failure))
      (let ((writes (http3-test-stream-writes (first (http3-boundary-fixture-streams fixture)))))
        (ensure-equal 1 (length writes))
        (ensure-equal nil (second (first writes)))
        (ensure-equal http-kit/http3:+http3-headers-type+
                      (http-kit/http3:http3-frame-type
                       (first (http-kit/http3:decode-http3-frames (first (first writes)))))))
      (ensure-equal 0 (http3-boundary-fixture-reads fixture))
      (ensure-equal 1 (length (http3-boundary-fixture-closes fixture)))
      (http3-boundary-check-release fixture failure)
      (let* ((chunks (list #(0 255) nil))
             (good-calls 0)
             (response
               (http-kit/http3:send-http3-request
                client (make-http-request :method "POST" :uri "https://example.test/upload")
                :request-body-length 2
                :request-body-function
                (lambda (maximum)
                  (incf good-calls)
                  (ensure-equal 16384 maximum)
                  (pop chunks))))
             (writes (reverse (http3-test-stream-writes
                               (first (http3-boundary-fixture-streams fixture)))))
             (frames (mapcar (lambda (entry)
                               (first (http-kit/http3:decode-http3-frames (first entry))))
                             writes)))
        (ensure-equal 204 (http-response-status response))
        (ensure-equal 2 good-calls)
        (ensure-equal 3 (length writes))
        (ensure-equal '(nil nil t) (mapcar #'second writes))
        (ensure-equal http-kit/http3:+http3-data-type+ (http-kit/http3:http3-frame-type (second frames)))
        (ensure-equal '(0 255) (coerce (http-kit/http3:http3-frame-payload (second frames)) 'list))
        (ensure-equal http-kit/http3:+http3-data-type+ (http-kit/http3:http3-frame-type (third frames)))
        (ensure-equal 0 (length (http-kit/http3:http3-frame-payload (third frames)))))
      (ensure-equal 1 (http3-boundary-fixture-reads fixture))
      (ensure-equal 2 (length (http3-boundary-fixture-streams fixture)))
      (ensure-equal 2 (length (http3-boundary-fixture-closes fixture)))
      (http3-boundary-check-release fixture nil)
      (http-kit/http3:close-http3-client client))))

(deftest http3-public-request-trailers-carry-final-fin
  (dolist (mode '(:producer :memory :empty))
    (let* ((fixture (http3-boundary-fixture
                     (list (list (http3-boundary-headers '((":status" . "204"))) t))))
           (client (http3-boundary-fixture-client fixture))
           (chunks (list #(1 2) nil))
           (calls 0)
           (request (make-http-request :method "POST" :uri "https://example.test/upload"
                                       :body (when (eq mode :memory) #(1 2))
                                       :trailers '(("x-end" . "done"))))
           (response
             (if (eq mode :producer)
                 (http-kit/http3:send-http3-request
                  client request :request-body-length 2
                  :request-body-function
                  (lambda (maximum)
                    (incf calls)
                    (ensure-equal 16384 maximum)
                    (pop chunks)))
                 (http-kit/http3:send-http3-request client request)))
           (writes (reverse (http3-test-stream-writes
                             (first (http3-boundary-fixture-streams fixture)))))
           (frames (mapcar (lambda (entry)
                             (first (http-kit/http3:decode-http3-frames (first entry))))
                           writes)))
      (ensure-equal 204 (http-response-status response))
      (ensure-equal (if (eq mode :producer) 2 0) calls)
      (ensure-equal (if (eq mode :empty) 2 3) (length writes))
      (ensure-equal (if (eq mode :empty) '(nil t) '(nil nil t)) (mapcar #'second writes))
      (ensure-equal (if (eq mode :empty)
                        (list http-kit/http3:+http3-headers-type+ http-kit/http3:+http3-headers-type+)
                        (list http-kit/http3:+http3-headers-type+ http-kit/http3:+http3-data-type+
                              http-kit/http3:+http3-headers-type+))
                    (mapcar #'http-kit/http3:http3-frame-type frames))
      (ensure-equal '(("x-end" . "done"))
                    (http3-test-qpack-decode (http-kit/http3:http3-frame-payload (car (last frames)))))
      (unless (eq mode :empty)
        (ensure-equal '(1 2) (coerce (http-kit/http3:http3-frame-payload (second frames)) 'list)))
      (ensure-equal 1 (http3-boundary-fixture-reads fixture))
      (ensure-equal 1 (length (http3-boundary-fixture-closes fixture)))
      (http3-boundary-check-release fixture nil)
      (http-kit/http3:close-http3-client client))))

(deftest http3-public-response-data-requires-final-headers
  (dolist (mode '(:initial :informational :valid))
    (let* ((wire (apply #'http3-test-concat-octets
                        (append (unless (eq mode :initial)
                                  (list (http3-boundary-headers '((":status" . "103")))))
                                (when (eq mode :valid)
                                  (list (http3-boundary-headers '((":status" . "200")
                                                                  ("content-length" . "2")))))
                                (list (http3-boundary-data "ok")))))
           (fixture (http3-boundary-fixture (list (list wire t))))
           (client (http3-boundary-fixture-client fixture))
           (events nil)
           (response nil)
           (failure
             (http3-boundary-error
              (lambda ()
                (setf response
                      (http-kit/http3:send-http3-request
                       client (make-http-request :method "GET" :uri "https://example.test/")
                       :on-information
                       (lambda (information)
                         (ensure-equal 103 (http-response-status information))
                         (push :information events))
                       :on-body-chunk
                       (lambda (chunk)
                         (ensure-equal (ascii "ok") chunk)
                         (push :body events))))))))
      (ensure-equal (case mode (:initial nil) (:informational '(:information))
                              (:valid '(:information :body)))
                    (reverse events))
      (if (eq mode :valid)
          (progn
            (ensure-equal nil failure)
            (ensure-equal 200 (http-response-status response))
            (ensure-equal (ascii "ok") (http-response-body response)))
          (progn
            (ensure-true (typep failure 'http-protocol-error))
            (ensure-equal :http3-transport (http-error-operation failure))
            (ensure-equal nil (http-protocol-error-detail failure))
            (ensure-equal nil response)))
      (let ((writes (reverse (http3-test-stream-writes
                             (first (http3-boundary-fixture-streams fixture))))))
        (ensure-equal 2 (length writes))
        (ensure-equal '(nil t) (mapcar #'second writes)))
      (ensure-equal 1 (http3-boundary-fixture-reads fixture))
      (ensure-equal 1 (length (http3-boundary-fixture-closes fixture)))
      (http3-boundary-check-release fixture failure)
      (http-kit/http3:close-http3-client client))))

(defun http3-public-qpack-fixture (request-entries encoder-entries)
  (let* ((fixture (make-http3-boundary-fixture))
         (peer (make-http3-test-stream :kind :peer-qpack-encoder
                                       :reads (copy-list encoder-entries))))
    (push peer (http3-boundary-fixture-streams fixture))
    (setf (http3-boundary-fixture-client fixture)
          (http-kit/http3:make-http3-client
           :qpack-settings (list (cons http-kit/http3:+http3-setting-qpack-max-table-capacity+ 256)
                           (cons http-kit/http3:+http3-setting-qpack-blocked-streams+ 1))
           :open-stream
           (lambda (request &key stream-type timeout deadline)
             (declare (ignore request timeout deadline))
             (let ((stream (make-http3-test-stream
                            :kind stream-type
                            :reads (when (eq stream-type :request)
                                     (copy-list request-entries)))))
               (push stream (http3-boundary-fixture-streams fixture))
               (values stream 0)))
           :write-stream
           (lambda (stream bytes &key fin-p timeout deadline)
             (declare (ignore timeout deadline))
             (push (list (copy-seq bytes) fin-p) (http3-test-stream-writes stream)))
           :read-stream
           (lambda (stream &key timeout deadline)
             (declare (ignore timeout deadline))
             (incf (http3-boundary-fixture-reads fixture))
             (ensure-true (consp (http3-test-stream-reads stream)))
             (let ((entry (pop (http3-test-stream-reads stream))))
               (values (first entry) (second entry))))
           :close-stream
           (lambda (stream &key condition)
             (ensure-equal nil (http3-test-stream-closed-p stream))
             (setf (http3-test-stream-closed-p stream) t)
             (push (cons stream condition) (http3-boundary-fixture-closes fixture)))))
    (http-kit/http3:attach-http3-peer-qpack-encoder-stream
     (http3-boundary-fixture-client fixture) peer)
    (values fixture peer)))

(deftest http3-public-qpack-await-must-complete
  (labels ((fields-as-pairs (headers)
             (mapcar (lambda (header)
                       (cons (http-message-kit:http-header-name header)
                             (http-message-kit:http-header-content header)))
                     headers)))
    (dolist (role (list :client :server))
      (dolist (complete-p (list nil t))
        (multiple-value-bind (decoder-context section instruction)
            (http3-test-qpack-blocked-section
             (append (if (eq role :client)
                         (list (cons ":status" "200"))
                         (list (cons ":method" "POST")
                               (cons ":scheme" "https")
                               (cons ":authority" "example.test")
                               (cons ":path" "/")))
                     (list (cons "content-length" "3")
                           (cons "x-blocked" "ready"))))
          (let* ((wire
                   (http3-test-concat-octets
                    (http-kit/http3:encode-http3-frame
                     (http-kit/http3:make-http3-frame
                      :type http-kit/http3:+http3-headers-type+ :payload section))
                    (http3-boundary-data "abc")))
                 (encoder-wire
                   (http3-test-concat-octets
                    (http-kit/http3:http3-varint-encode
                     http-kit/http3:+http3-qpack-encoder-stream-type+)
                    instruction))
                 (adapter
                   (http-message-kit:qpack-make-http3-decoder-context
                    decoder-context :max-blocked-streams 1)))
            (multiple-value-bind (fixture peer)
                (http3-public-qpack-fixture
                 (list (list wire t)) (list (list encoder-wire nil)))
              (declare (ignore peer))
              (let* ((client (http3-boundary-fixture-client fixture))
                     (stream-id (if (eq role :client) 0 16))
                     (stream nil) (result nil) (failure nil)
                     (await-count 0) (body-count 0) (handler-count 0)
                     (error-count 0) (observed-error nil) (events nil))
                (flet ((await (token)
                         (incf await-count)
                         (push :await events)
                         (ensure-true
                          (http-message-kit:qpack-blocked-stream-p token))
                         (ensure-equal 0 body-count)
                         (ensure-equal 0 handler-count)
                         (when complete-p
                           (if (eq role :client)
                               (multiple-value-bind (notifications ended-p)
                                   (http-kit/http3:read-http3-qpack-encoder-stream
                                    client)
                                 (let ((ready
                                         (remove-if-not
                                          (lambda (event)
                                            (eq (first event)
                                                :qpack-stream-unblocked))
                                          notifications)))
                                   (ensure-equal 1 (length ready))
                                   (ensure-true
                                    (eq token (second (first ready))))
                                   (ensure-equal nil ended-p)))
                               (progn
                                 (multiple-value-bind (notifications ended-p)
                                     (http-kit/http3:read-http3-qpack-encoder-stream
                                      client)
                                   (ensure-equal nil notifications)
                                   (ensure-equal nil ended-p))
                                 (http-message-kit:qpack-decode-encoder-stream
                                  decoder-context instruction)
                                 (let ((ready
                                         (http-message-kit:qpack-take-ready-streams
                                          adapter)))
                                   (ensure-equal 1 (length ready))
                                   (ensure-true (eq token (first ready))))))
                           (ensure-equal 0 body-count)
                           (ensure-equal 0 handler-count)
                           (let ((headers
                                   (http-message-kit:qpack-resume-blocked-stream
                                    token)))
                             (push :resumed events)
                             (fields-as-pairs headers))))
                       (body (chunk)
                         (incf body-count)
                         (ensure-equal (ascii "abc") chunk)
                         (push :body events)))
                  (unwind-protect
                       (progn
                         (when (eq role :server)
                           (setf stream
                                 (funcall
                                  (http-kit/http3:http3-client-open-stream client)
                                  nil :stream-type :request)))
                         (setf failure
                               (http3-boundary-error
                                (lambda ()
                                  (setf result
                                        (if (eq role :client)
                                            (http-kit/http3:send-http3-request client (make-http-request :method "GET" :uri "https://example.test/") :await-qpack (function await) :on-body-chunk (function body))
                                            (http-kit/http3:serve-http3-request-stream
                                             stream
                                             (lambda (request)
                                               (incf handler-count)
                                               (push :handler events)
                                               (ensure-equal "POST"
                                                             (http-request-method request))
                                               (ensure-equal
                                                (ascii "abc")
                                                (http-request-body request))
                                               (make-http-response :status 204))
                                             :read-stream
                                             (http-kit/http3:http3-client-read-stream client)
                                             :write-stream
                                             (http-kit/http3:http3-client-write-stream client)
                                             :close-stream
                                             (http-kit/http3:http3-client-close-stream client)
                                             :stream-id stream-id
                                             :qpack-decoder-context decoder-context
                                             :qpack-http3-decoder-context adapter
                                             :on-qpack-section-acknowledgment
                                             (lambda (acknowledged-stream-id)
                                               (push (list :section-ack
                                                           acknowledged-stream-id)
                                                     events))
                                             :await-qpack (function await)
                                             :on-body-chunk (function body)
                                             :on-error
                                             (lambda (condition request)
                                               (incf error-count)
                                               (setf observed-error condition)
                                               (ensure-equal nil request))))))))
                         (when (eq role :client)
                           (setf stream
                                 (first
                                  (http3-boundary-fixture-streams fixture))))
                         (ensure-equal 1 await-count)
                         (ensure-equal (if complete-p 1 0) body-count)
                         (ensure-equal
                          (if (and complete-p (eq role :server)) 1 0)
                          handler-count)
                         (ensure-equal
                          (if complete-p
                              (if (eq role :server)
                                  (list :await (list :section-ack stream-id)
                                        :resumed :body :handler)
                                  (list :await :resumed :body))
                              (list :await))
                          (reverse events))
                         (if complete-p
                             (progn
                               (ensure-equal nil failure)
                               (ensure-equal
                                (if (eq role :client) 200 204)
                                (http-response-status result))
                               (when (eq role :client)
                                 (ensure-equal
                                  (ascii "abc")
                                  (http-response-body result))))
                             (progn
                               (ensure-true
                                (typep failure (quote http-protocol-error)))
                               (ensure-equal :http3-transport
                                             (http-error-operation failure))
                               (ensure-equal nil
                                             (http-protocol-error-detail failure))
                               (ensure-equal
                                (if (eq role :client)
                                    "An HTTP/3 response HEADERS block lacks :status."
                                    "An HTTP/3 request must contain a valid :method pseudo-field.")
                                (http-error-message failure))
                               (ensure-equal nil result)))
                         (ensure-equal
                          (if (and (not complete-p) (eq role :server)) 1 0)
                          error-count)
                         (when (plusp error-count)
                           (ensure-true (eq failure observed-error)))
                         (ensure-equal (if complete-p 2 1)
                                       (http3-boundary-fixture-reads fixture))
                         (ensure-true (http3-test-stream-closed-p stream))
                         (let ((closes
                                 (remove stream
                                         (http3-boundary-fixture-closes fixture)
                                         :key (function car) :test-not (function eq))))
                           (ensure-equal 1 (length closes))
                           (ensure-true (eq failure (cdar closes))))
                         (let ((writes
                                 (reverse (http3-test-stream-writes stream))))
                           (cond
                             ((eq role :client)
                              (ensure-equal 2 (length writes))
                              (ensure-equal (list nil t) (mapcar (function second) writes)))
                             (complete-p
                              (ensure-equal 1 (length writes))
                              (ensure-equal t (second (first writes)))
                              (let* ((frames
                                       (http-kit/http3:decode-http3-frames
                                        (first (first writes))))
                                     (decoded
                                       (http-message-kit:qpack-decode-header-block
                                        (http-kit/http3:http3-frame-payload
                                         (first frames)))))
                                (ensure-equal 1 (length frames))
                                (ensure-equal
                                 (list (cons ":status" "204"))
                                 (fields-as-pairs decoded))))
                             (t (ensure-equal nil writes)))))
                    (http-kit/http3:close-http3-client client)))
                (ensure-equal 5 (length (http3-boundary-fixture-streams fixture)))
                (ensure-equal 5 (length (http3-boundary-fixture-closes fixture)))
                (dolist (owned (http3-boundary-fixture-streams fixture))
                  (ensure-true (http3-test-stream-closed-p owned))
                  (ensure-equal
                   1
                   (count owned (http3-boundary-fixture-closes fixture)
                          :key (function car) :test (function eq))))))))))
))

(deftest http3-public-qpack-encoder-unblocked-event
  (let ((fields (list (cons ":status" "200")
                      (cons "x-blocked" "ready"))))
    (labels ((fields-as-pairs (headers)
               (mapcar (lambda (header)
                         (cons (http-message-kit:http-header-name header)
                               (http-message-kit:http-header-content header)))
                       headers)))
      (multiple-value-bind (unused-context section instruction)
          (http3-test-qpack-blocked-section fields)
        (declare (ignore unused-context))
        (let* ((response-wire
                 (http-kit/http3:encode-http3-frame
                  (http-kit/http3:make-http3-frame
                   :type http-kit/http3:+http3-headers-type+
                   :payload section)))
               (encoder-wire
                 (http3-test-concat-octets
                  (http-kit/http3:http3-varint-encode
                   http-kit/http3:+http3-qpack-encoder-stream-type+)
                  instruction)))
          (multiple-value-bind (fixture peer)
              (http3-public-qpack-fixture
               (list (list response-wire t))
               (list (list encoder-wire nil) (list (octets) nil)))
            (let* ((client (http3-boundary-fixture-client fixture))
                   (decoded-count 0)
                   (ready-count 0)
                   (token nil)
                   (events nil))
              (unwind-protect
                   (flet ((await (blocked-token)
                            (setf token blocked-token)
                            (push :await events)
                            (ensure-true
                             (http-message-kit:qpack-blocked-stream-p token))
                            (ensure-equal 0 decoded-count)
                            (multiple-value-bind (notifications ended-p)
                                (http-kit/http3:read-http3-qpack-encoder-stream
                                 client)
                              (let ((ready
                                      (remove-if-not
                                       (lambda (event)
                                         (eq (first event)
                                             :qpack-stream-unblocked))
                                       notifications)))
                                (ensure-equal 1 (length ready))
                                (ensure-true
                                 (eq token (second (first ready))))
                                (progn (incf ready-count) (push :unblocked events))
                                (ensure-equal nil ended-p)))
                            (ensure-equal 0 decoded-count)
                            (let ((headers
                                    (http-message-kit:qpack-resume-blocked-stream
                                     token)))
                              (incf decoded-count)
                              (push :resumed events)
                              (fields-as-pairs headers))))
                     (let ((response
                             (http-kit/http3:send-http3-request
                              client
                              (make-http-request
                               :method "GET" :uri "https://example.test/")
                              :await-qpack (function await))))
                       (ensure-equal 200 (http-response-status response))
                       (ensure-equal "ready"
                                     (http-header-value
                                      (http-response-headers response)
                                      "x-blocked"))
                       (ensure-equal 1 decoded-count)
                       (ensure-equal 1 ready-count)
                       (ensure-equal (list :await :unblocked :resumed) (reverse events))
                       (multiple-value-bind (notifications ended-p)
                           (http-kit/http3:read-http3-qpack-encoder-stream client)
                         (ensure-equal nil notifications)
                         (ensure-equal nil ended-p))
                       (ensure-equal nil (http3-test-stream-reads peer))
                       (signals http-message-kit:http-protocol-error
                         (http-message-kit:qpack-resume-blocked-stream token))
                       (let* ((decoder
                                (http-message-kit:make-qpack-context
                                 :max-table-capacity 256 :table-capacity 0
                                 :huffman nil))
                              (adapter
                                (http-message-kit:qpack-make-http3-decoder-context
                                 decoder :max-blocked-streams 1)))
                         (http-message-kit:qpack-decode-encoder-stream
                          decoder instruction)
                         (let ((headers
                                 (http-message-kit:qpack-decode-http3-field-section
                                  section adapter 28 :role :response
                                  :on-section-acknowledgment
                                  (lambda (stream-id)
                                    (ensure-equal 28 stream-id)
                                    (incf ready-count)))))
                           (ensure-equal fields (fields-as-pairs headers))
                           (ensure-equal 2 ready-count)))
                       (ensure-equal 3 (http3-boundary-fixture-reads fixture)))
                (http-kit/http3:close-http3-client client))
              (ensure-equal 5 (length (http3-boundary-fixture-streams fixture)))
              (ensure-equal 5 (length (http3-boundary-fixture-closes fixture)))
              (dolist (owned (http3-boundary-fixture-streams fixture))
                (ensure-true (http3-test-stream-closed-p owned))
                (ensure-equal
                 1
                 (count owned (http3-boundary-fixture-closes fixture)
                        :key (function car) :test (function eq))))))))))
))

(defun http3-check-manager-proxy-plan (plan accepted-p)
  (let* ((request (make-http-request
                   :method "GET" :uri "https://proxy-plan.example/resource"))
         (response-wire
           (http-kit/http3:encode-http3-frame
            (http-kit/http3:make-http3-frame
             :type http-kit/http3:+http3-headers-type+
             :payload (http3-test-qpack-encode (list (cons ":status" "204"))))))
         (events nil)
         (clients nil)
         (streams nil)
         (response nil)
         (condition nil)
         (retained-count nil)
         (request-events nil)
         (manager
           (http-kit/http3:make-http3-connection-manager
            :open-client
            (lambda (opened-request &key timeout deadline)
              (declare (ignore timeout deadline))
              (push (list :open-client (eq request opened-request)) events)
              (let ((client
                      (http-kit/http3:make-http3-client
                       :open-stream
                       (lambda (stream-request &key stream-type timeout deadline)
                         (declare (ignore stream-request timeout deadline))
                         (let ((stream (make-http3-test-stream :kind stream-type)))
                           (push :open-stream events)
                           (push stream streams)
                           (values stream 0)))
                       :write-stream
                       (lambda (stream octets &key fin-p timeout deadline)
                         (declare (ignore stream octets fin-p timeout deadline))
                         (push :write-stream events))
                       :read-stream
                       (lambda (stream &key timeout deadline)
                         (declare (ignore stream timeout deadline))
                         (push :read-stream events)
                         (values response-wire t))
                       :close-stream
                       (lambda (stream &key condition)
                         (declare (ignore condition))
                         (push :close-stream events)
                         (setf (http3-test-stream-closed-p stream) t)))))
                (push client clients)
                client)))))
    (unwind-protect
         (progn
           (ensure-true (http-kit/http3:http3-connection-manager-open-p manager))
           (ensure-equal 0 (http-kit/http3:http3-connection-manager-connection-count
                            manager))
           (handler-case
               (setf response
                     (funcall
                      (http-kit/http3:make-http3-connection-manager-transport manager)
                      request :proxy nil :proxy-plan plan))
             (http-protocol-error (error-condition)
               (setf condition error-condition)))
           (setf retained-count
                 (http-kit/http3:http3-connection-manager-connection-count manager)
                 request-events (reverse events)))
      (http-kit/http3:close-http3-connection-manager manager))
    (cl-weave:journal-note
     :manager-proxy-plan-observation
     (list :proxy-plan plan :accepted-p accepted-p
           :response-status (and response (http-response-status response))
           :condition-type (and condition (type-of condition))
           :request-events request-events :retained-count retained-count
           :manager-closed-p
           (not (http-kit/http3:http3-connection-manager-open-p manager))))
    (if accepted-p
        (progn
          (ensure-equal nil condition)
          (ensure-equal 204 (http-response-status response))
          (ensure-equal 1 (length clients))
          (ensure-equal '(:open-client t) (first request-events))
          (ensure-true (member :write-stream request-events))
          (ensure-true (member :read-stream request-events))
          (ensure-equal 1 retained-count))
        (progn
          (ensure-true (typep condition 'http-protocol-error))
          (ensure-equal nil response)
          (ensure-equal nil request-events)
          (ensure-equal nil clients)
          (ensure-equal nil streams)
          (ensure-equal 0 retained-count)))
    (ensure-false (http-kit/http3:http3-connection-manager-open-p manager))
    (ensure-equal 0 (http-kit/http3:http3-connection-manager-connection-count manager))
    (dolist (client clients)
      (ensure-false (http-kit/http3:http3-client-open-p client)))
    (dolist (stream streams)
      (ensure-true (http3-test-stream-closed-p stream)))))

(deftest http3-manager-proxy-plan-rejects-connect-before-open
  (http3-check-manager-proxy-plan
   (http-proxy-plan (make-http-proxy :scheme :http :host "proxy.example" :port 8080)
                    "https://proxy-plan.example/resource")
   nil))

(deftest http3-manager-proxy-plan-rejects-socks5-before-open
  (http3-check-manager-proxy-plan
   (http-proxy-plan (make-http-proxy :scheme :socks5 :host "proxy.example" :port 1080)
                    "https://proxy-plan.example/resource")
   nil))

(deftest http3-manager-proxy-plan-rejects-forward-before-open
  (http3-check-manager-proxy-plan
   (http-proxy-plan (make-http-proxy :scheme :http :host "proxy.example" :port 8080)
                    "http://proxy-plan.example/resource")
   nil))

(deftest http3-manager-proxy-plan-rejects-missing-mode-before-open
  (http3-check-manager-proxy-plan
   (list :target (parse-http-uri "https://proxy-plan.example/resource")) nil))

(deftest http3-manager-proxy-plan-accepts-direct-marker
  (http3-check-manager-proxy-plan :direct t))

(deftest http3-manager-proxy-plan-accepts-public-direct-plan
  (http3-check-manager-proxy-plan
   (http-proxy-plan nil "https://proxy-plan.example/resource") t))

(deftest http3-manager-proxy-plan-rejects-contradictory-direct-before-open
  (http3-check-manager-proxy-plan
   (list :mode :direct
         :proxy (make-http-proxy :scheme :http :host "proxy.example" :port 8080)
         :target (parse-http-uri "https://proxy-plan.example/resource"))
   nil))

(deftest http3-qpack-cleanup-prefix-controls
  (let ((counts (make-array 5 :initial-element 0)))
    (dotimes (first 256)
      (let* ((branches (list (/= 0 (logand first #x80))
                             (= (logand first #xc0) #x40)
                             (= (logand first #xe0) #x20)
                             (= (logand first #xf0) #x10)
                             (= (logand first #xf0) 0)))
             (chosen (position t branches)))
        (ensure-true chosen)
        (ensure-equal 1 (count t branches))
        (incf (aref counts chosen))))
    (ensure-equal #(128 64 32 16 16) counts))
  (ensure-equal '((":path" . "/"))
                (http3-test-qpack-decode (octets 0 0 #xc1)))
  (ensure-equal '((":path" . "a"))
                (http3-test-qpack-decode (octets 0 0 #x51 1 #x61)))
  (ensure-equal '(("x" . "a"))
                (http3-test-qpack-decode (octets 0 0 #x21 #x78 1 #x61)))
  (let ((table (http-message-kit:make-qpack-context :max-table-capacity 512 :table-capacity 512 :huffman nil)))
    (dotimes (index 8)
      (http-message-kit:qpack-encode-insert-with-literal-name table (format nil "x~D" index) "y"))
    (dotimes (first 16)
      (let* ((index (logand first 7))
             (wire (apply #'octets
                          (append (list 9 #x87 first)
                                  (when (= index 7) (list 0))
                                  (list 1 #x32)))))
        (ensure-equal (list (cons (format nil "x~D" index) "2"))
                      (http3-test-qpack-decode wire :context table))))
    (ensure-equal 8 (http-message-kit:qpack-context-insert-count table))
    (ensure-equal 8 (http-message-kit:qpack-context-dynamic-entry-count table)))
  (let ((table (http-message-kit:make-qpack-context :max-table-capacity 64 :table-capacity 64 :huffman nil)))
    (http-message-kit:qpack-encode-insert-with-literal-name table "x" "1")
    (ensure-equal '(("x" . "1"))
                  (http3-test-qpack-decode (octets 2 #x80 #x10) :context table))))

(deftest http3-qpack-cleanup-instruction-controls
  (dolist (kind '(:encoder :decoder))
    (let* ((context
             (http-message-kit:make-qpack-context
              :max-table-capacity 128 :table-capacity 128 :huffman nil))
           (partial (if (eq kind :encoder) (octets 63) (octets 255)))
           (oversized
             (if (eq kind :encoder)
                 (octets 63 128 128 128 128 128)
                 (octets 255 128 128 128 128 128))))
      (flet ((process (wire allow)
               (if (eq kind :encoder)
                   (http-message-kit:qpack-decode-encoder-stream
                    context wire :allow-incomplete-p allow)
                   (http-message-kit:qpack-decode-decoder-stream
                    context wire :allow-incomplete-p allow))))
        (multiple-value-bind (events consumed) (process partial t)
          (ensure-equal nil events)
          (ensure-equal 0 consumed))
        (dolist (case
                 (list (list partial nil
                             "An HPACK integer continuation is truncated." 1)
                       (list oversized nil
                             "An HPACK integer is too large."
                             (if (eq kind :encoder) 31 127))
                       (list oversized t
                             "An HPACK integer is too large."
                             (if (eq kind :encoder) 31 127))))
          (destructuring-bind (wire allow message detail) case
            (let* ((original nil)
                   (failure
                     (handler-case
                         (handler-bind
                             ((http-protocol-error
                                (lambda (condition)
                                  (unless original
                                    (setf original condition)))))
                           (process wire allow))
                       (http-protocol-error (condition) condition))))
              (ensure-true (typep failure 'http-protocol-error))
              (ensure-true (eq original failure))
              (ensure-equal :hpack (http-error-operation failure))
              (ensure-equal message (http-error-message failure))
              (ensure-equal detail
                            (http-protocol-error-detail failure)))))
        (ensure-equal 0 (http-message-kit:qpack-context-insert-count context))
        (ensure-equal 128 (http-message-kit:qpack-context-table-capacity context))
        (if (eq kind :encoder)
            (progn
              (multiple-value-bind (events consumed)
                  (process (octets 32) t)
                (ensure-equal '((:set-capacity 0)) events)
                (ensure-equal 1 consumed))
              (ensure-equal 0
                            (http-message-kit:qpack-context-table-capacity context)))
            (progn
              (http-message-kit:qpack-encode-insert-with-literal-name
               context "x" "y")
              (http3-test-qpack-encode
               '(("x" . "y"))
               :context context :use-dynamic-p t :stream-id 0 :huffman nil)
              (multiple-value-bind (events consumed)
                  (process (octets 128) t)
                (ensure-equal '((:section-ack 0)) events)
                (ensure-equal 1 consumed))
              (ensure-equal 1
                            (http-message-kit:qpack-context-known-received-count
                             context))))))))

(deftest http3-qpack-cleanup-serializer-capacity-controls
  (let* ((small
           (http-message-kit:make-qpack-context
            :max-table-capacity 32 :table-capacity 32 :huffman nil))
         (failure
           (handler-case
               (http-message-kit:qpack-encode-insert-with-literal-name
                small "x" "y")
             (http-protocol-error (condition) condition))))
    (ensure-true (typep failure 'http-protocol-error))
    (ensure-equal :qpack (http-error-operation failure))
    (ensure-equal 34 (http-protocol-error-detail failure))
    (ensure-equal 0 (http-message-kit:qpack-context-dynamic-entry-count small))
    (ensure-equal 0 (http-message-kit:qpack-context-table-size small))
    (ensure-equal 0 (http-message-kit:qpack-context-insert-count small)))
  (let* ((encoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 34 :table-capacity 34 :huffman nil))
         (decoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 34 :table-capacity 34 :huffman nil))
         (context
           (http-message-kit:qpack-make-http3-decoder-context
            decoder :max-blocked-streams 1))
         (calls 0)
         (inside-serializer-p nil)
         (acknowledged nil)
         (fields '(("x" . "y"))))
    (http-message-kit:qpack-encode-insert-with-literal-name
     encoder "x" "y")
    (ensure-equal 34
                  (http-message-kit:qpack-context-table-size encoder))
    (let* ((wire
             (http3-test-qpack-encode
              fields :context encoder :use-dynamic-p t :stream-id 4 :huffman nil))
           (serializer
             (lambda (thunk)
               (incf calls)
               (setf inside-serializer-p t)
               (unwind-protect
                    (funcall thunk)
                 (setf inside-serializer-p nil))))
           (ack-callback
             (lambda (stream-id)
               (ensure-equal nil inside-serializer-p)
               (push stream-id acknowledged)))
           (token
             (http-message-kit:qpack-decode-http3-field-section
              wire context 4 :role :trailers
              :serialize serializer
              :on-section-acknowledgment ack-callback)))
      (ensure-true (http-message-kit:qpack-blocked-stream-p token))
      (ensure-equal nil acknowledged)
      (ensure-equal nil (http-message-kit:qpack-take-ready-streams context))
      (ensure-equal 1 calls)
      (http-message-kit:qpack-decode-encoder-stream
       decoder
       (http3-test-qpack-encode-insert-with-literal-name "x" "y"))
      (ensure-equal 1
                    (http-message-kit:qpack-context-insert-count decoder))
      (let ((ready (http-message-kit:qpack-take-ready-streams context)))
        (ensure-equal 1 (length ready))
        (ensure-true (eq token (first ready)))
        (ensure-equal fields
                      (http-message-kit:qpack-resume-blocked-stream token)))
      (ensure-equal '(4) (nreverse acknowledged))
      (ensure-equal 3 calls)
      (ensure-equal
       fields
       (http-message-kit:qpack-decode-http3-field-section
        wire context 8 :role :trailers
        :on-section-acknowledgment ack-callback))
      (ensure-equal '(8 4) acknowledged))))

(deftest http3-qpack-public-context-boundary-shared-release-order
  (dolist (order '(:cancel-first :ack-first))
    (let* ((context
             (http-message-kit:make-qpack-context
              :max-table-capacity 34 :table-capacity 34 :huffman nil))
           (fields '(("x" . "y"))))
      (labels ((expect-error (thunk message detail)
                 (let* ((original nil)
                        (failure
                          (handler-case
                              (handler-bind
                                  ((http-protocol-error
                                     (lambda (condition)
                                       (setf original condition))))
                                (funcall thunk))
                            (http-protocol-error (condition) condition))))
                   (ensure-true (typep failure 'http-protocol-error))
                   (ensure-true (eq original failure))
                   (ensure-equal :qpack (http-error-operation failure))
                   (ensure-equal message (http-error-message failure))
                   (ensure-equal detail
                                 (http-protocol-error-detail failure))))
               (protect-table ()
                 (dolist (operation
                          (list (lambda ()
                                  (http-message-kit:qpack-context-set-capacity
                                   context 0))
                                (lambda ()
                                  (http-message-kit:qpack-encode-insert-with-literal-name
                                   context "z" "w"))))
                   (expect-error
                    operation
                    "QPACK cannot evict an entry referenced by a field section."
                    0)
                   (ensure-equal 34
                                 (http-message-kit:qpack-context-table-capacity
                                  context))
                   (ensure-equal 34
                                 (http-message-kit:qpack-context-table-size context))
                   (ensure-equal 1
                                 (http-message-kit:qpack-context-insert-count
                                  context))
                   (ensure-equal 1
                                 (http-message-kit:qpack-context-dynamic-entry-count
                                  context))))
               (release-section (action stream-id)
                 (let ((wire
                         (if (eq action :ack)
                             (http-message-kit:qpack-encode-section-ack stream-id)
                             (http-message-kit:qpack-encode-stream-cancellation
                              stream-id))))
                   (multiple-value-bind (events consumed)
                       (http-message-kit:qpack-decode-decoder-stream
                        context wire)
                     (ensure-equal
                      (list (list (if (eq action :ack)
                                      :section-ack
                                      :stream-cancellation)
                                  stream-id))
                      events)
                     (ensure-equal (length wire) consumed)))))
        (http-message-kit:qpack-encode-insert-with-literal-name
         context "x" "y")
        (ensure-equal 34
                      (http-message-kit:qpack-context-table-size context))
        (http3-test-qpack-encode
         (append fields fields)
         :context context :use-dynamic-p t :stream-id 4 :huffman nil)
        (http3-test-qpack-encode
         fields :context context :use-dynamic-p t :stream-id 4 :huffman nil)
        (http3-test-qpack-encode
         fields :context context :use-dynamic-p t :stream-id 8 :huffman nil)
        (ensure-equal 0
                      (http-message-kit:qpack-context-known-received-count
                       context))
        (protect-table)
        (if (eq order :cancel-first)
            (progn
              (release-section :cancel 4)
              (protect-table)
              (release-section :ack 8))
            (progn
              (release-section :ack 4)
              (ensure-equal 1
                            (http-message-kit:qpack-context-known-received-count
                             context))
              (protect-table)
              (release-section :cancel 8)
              (protect-table)
              (release-section :ack 4)))
        (ensure-equal 1
                      (http-message-kit:qpack-context-known-received-count
                       context))
        (http-message-kit:qpack-context-set-capacity context 0)
        (ensure-equal 0
                      (http-message-kit:qpack-context-table-size context))
        (ensure-equal 0
                      (http-message-kit:qpack-context-dynamic-entry-count
                       context))
        (ensure-equal 1
                      (http-message-kit:qpack-context-insert-count context))
        (http-message-kit:qpack-context-set-capacity context 34)
        (http-message-kit:qpack-encode-insert-with-literal-name
         context "z" "w")
        (ensure-equal 2
                      (http-message-kit:qpack-context-insert-count context))
        (ensure-equal 34
                      (http-message-kit:qpack-context-table-size context))
        (let* ((expected '(("z" . "w")))
               (wire
                 (http3-test-qpack-encode
                  expected :context context :use-dynamic-p t
                  :stream-id 12 :huffman nil)))
          (ensure-equal expected
                        (http3-test-qpack-decode
                         wire :context context :stream-id 12)))))))

(deftest http3-qpack-behavior-capacity-shrink-evicts-oldest
  (let* ((encoder (http-message-kit:make-qpack-context
                   :max-table-capacity 102 :table-capacity 0 :huffman nil))
         (decoder (http-message-kit:make-qpack-context
                   :max-table-capacity 102 :table-capacity 0 :huffman nil)))
    (multiple-value-bind (instruction transaction)
        (http-message-kit:qpack-prepare-set-capacity encoder 102)
      (http-message-kit:qpack-commit-encoding transaction)
      (multiple-value-bind (events consumed)
          (http-message-kit:qpack-decode-encoder-stream decoder instruction)
        (ensure-equal 1 (length events))
        (ensure-equal (length instruction) consumed)))
    (dolist (field '(("a" . "1") ("b" . "2") ("c" . "3")))
      (multiple-value-bind (instruction transaction)
          (http-message-kit:qpack-prepare-insert-with-literal-name
           encoder (car field) (cdr field) :huffman nil)
        (http-message-kit:qpack-commit-encoding transaction)
        (multiple-value-bind (events consumed)
            (http-message-kit:qpack-decode-encoder-stream decoder instruction)
          (ensure-equal 1 (length events))
          (ensure-equal (length instruction) consumed))))
    (ensure-equal 102 (http-message-kit:qpack-context-table-size encoder))
    (ensure-equal 3 (http-message-kit:qpack-context-dynamic-entry-count encoder))
    (ensure-equal 3 (http-message-kit:qpack-context-insert-count encoder))
    (multiple-value-bind (instruction transaction)
        (http-message-kit:qpack-prepare-set-capacity encoder 68)
      (http-message-kit:qpack-commit-encoding transaction)
      (multiple-value-bind (events consumed)
          (http-message-kit:qpack-decode-encoder-stream decoder instruction)
        (ensure-equal '((:capacity 68)) events)
        (ensure-equal (length instruction) consumed)))
    (ensure-equal 68 (http-message-kit:qpack-context-table-capacity encoder))
    (ensure-equal 68 (http-message-kit:qpack-context-table-size encoder))
    (ensure-equal 2 (http-message-kit:qpack-context-dynamic-entry-count encoder))
    (ensure-equal 3 (http-message-kit:qpack-context-insert-count encoder))
    (dolist (case '(("b" "2" 2) ("c" "3" 3)))
      (destructuring-bind (name value required-count) case
        (multiple-value-bind (wire transaction)
            (http-message-kit:qpack-prepare-header-block
             (list (http-message-kit:make-http-header name value))
             :context encoder :use-dynamic-p t :huffman nil :stream-id 4)
          (http-message-kit:qpack-abort-encoding transaction)
          (multiple-value-bind (headers consumed actual-required-count base)
              (http-message-kit:qpack-decode-header-block
               wire :context decoder :stream-id 4)
            (declare (ignore base))
            (ensure-equal (list (cons name value))
                          (mapcar
                           (lambda (header)
                             (cons (http-message-kit:http-header-name header)
                                   (http-message-kit:http-header-content header)))
                           headers))
            (ensure-equal (length wire) consumed)
            (ensure-equal required-count actual-required-count)))))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-header-block
         (list (http-message-kit:make-http-header "a" "1"))
         :context encoder :use-dynamic-p t :huffman nil :stream-id 8)
      (http-message-kit:qpack-abort-encoding transaction)
      (multiple-value-bind (headers consumed required-count base)
          (http-message-kit:qpack-decode-header-block
           wire :context decoder :stream-id 8)
        (declare (ignore base))
        (ensure-equal '(("a" . "1"))
                      (mapcar
                       (lambda (header)
                         (cons (http-message-kit:http-header-name header)
                               (http-message-kit:http-header-content header)))
                       headers))
        (ensure-equal (length wire) consumed)
        (ensure-equal 0 required-count)))
    (multiple-value-bind (instruction transaction)
        (http-message-kit:qpack-prepare-set-capacity encoder 0)
      (http-message-kit:qpack-commit-encoding transaction)
      (multiple-value-bind (events consumed)
          (http-message-kit:qpack-decode-encoder-stream decoder instruction)
        (ensure-equal '((:capacity 0)) events)
        (ensure-equal (length instruction) consumed)))
    (ensure-equal 0 (http-message-kit:qpack-context-table-size encoder))
    (ensure-equal 0 (http-message-kit:qpack-context-dynamic-entry-count encoder))
    (ensure-equal 3 (http-message-kit:qpack-context-insert-count encoder))
    (multiple-value-bind (instruction transaction)
        (http-message-kit:qpack-prepare-set-capacity encoder 34)
      (http-message-kit:qpack-commit-encoding transaction)
      (multiple-value-bind (events consumed)
          (http-message-kit:qpack-decode-encoder-stream decoder instruction)
        (ensure-equal '((:capacity 34)) events)
        (ensure-equal (length instruction) consumed)))
    (multiple-value-bind (instruction transaction)
        (http-message-kit:qpack-prepare-insert-with-literal-name
         encoder "d" "4" :huffman nil)
      (http-message-kit:qpack-commit-encoding transaction)
      (multiple-value-bind (events consumed)
          (http-message-kit:qpack-decode-encoder-stream decoder instruction)
        (ensure-equal 1 (length events))
        (ensure-equal (length instruction) consumed)))
    (ensure-equal 34 (http-message-kit:qpack-context-table-size encoder))
    (ensure-equal 4 (http-message-kit:qpack-context-insert-count encoder))
    (multiple-value-bind (wire transaction)
        (http-message-kit:qpack-prepare-header-block
         (list (http-message-kit:make-http-header "d" "4"))
         :context encoder :use-dynamic-p t :huffman nil :stream-id 12)
      (http-message-kit:qpack-abort-encoding transaction)
      (multiple-value-bind (headers consumed required-count base)
          (http-message-kit:qpack-decode-header-block
           wire :context decoder :stream-id 12)
        (declare (ignore base))
        (ensure-equal '(("d" . "4"))
                      (mapcar
                       (lambda (header)
                         (cons (http-message-kit:http-header-name header)
                               (http-message-kit:http-header-content header)))
                       headers))
        (ensure-equal (length wire) consumed)
        (ensure-equal 4 required-count)))))
(deftest http3-public-server-section-ack-callback-after-blocked-resume
  (let* ((stream-id 16)
         (fields '((":method" . "GET")
                   (":scheme" . "https")
                   (":authority" . "example.test")
                   (":path" . "/")
                   ("x-blocked" . "ready")))
         (encoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :huffman nil))
         (decoder
           (http-message-kit:make-qpack-context
            :max-table-capacity 256 :table-capacity 0 :huffman nil))
         (outbound
           (http-message-kit:make-qpack-context
            :max-table-capacity 0 :table-capacity 0 :huffman nil))
         (adapter
           (http-message-kit:qpack-make-http3-decoder-context
            decoder :max-blocked-streams 1))
         (encoder-wire (octets))
         (field-section nil)
         (events nil))
    (multiple-value-bind (capacity-bytes transaction)
        (http-message-kit:qpack-prepare-set-capacity encoder 256)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf encoder-wire capacity-bytes))
    (multiple-value-bind (insert-bytes transaction)
        (http-message-kit:qpack-prepare-insert-with-literal-name
         encoder "x-blocked" "ready" :huffman nil)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf encoder-wire
            (http3-test-concat-octets encoder-wire insert-bytes)))
    (multiple-value-bind (section transaction)
        (http-message-kit:qpack-prepare-header-block
         (mapcar (lambda (field)
                   (http-message-kit:make-http-header (car field) (cdr field)))
                 fields)
         :context encoder :use-dynamic-p t :huffman nil :stream-id stream-id)
      (http-message-kit:qpack-commit-encoding transaction)
      (setf field-section section))
    (let* ((stream (make-http3-test-stream :kind :request))
           (request-wire
             (http-kit/http3:encode-http3-frame
              (http-kit/http3:make-http3-frame
               :type http-kit/http3:+http3-headers-type+
               :payload field-section)))
           (reads 0)
           (writes nil)
           (closes nil)
           (response
             (http-kit/http3:serve-http3-request-stream
              stream
              (lambda (request)
                (push :handler events)
                (ensure-equal "GET" (http-request-method request))
                (ensure-equal "ready"
                              (http-header-value (http-request-headers request)
                                                 "x-blocked"))
                (make-http-response :status 204))
              :stream-id stream-id
              :qpack-encoder-context outbound
              :qpack-decoder-context decoder
              :qpack-http3-decoder-context adapter
              :on-qpack-section-acknowledgment
              (lambda (acknowledged-stream-id)
                (push (list :section-ack acknowledged-stream-id) events))
              :await-qpack
              (lambda (token)
                (push :await events)
                (ensure-true (http-message-kit:qpack-blocked-stream-p token))
                (multiple-value-bind (encoder-events consumed)
                    (http-message-kit:qpack-decode-encoder-stream
                     decoder encoder-wire)
                  (declare (ignore encoder-events))
                  (ensure-equal (length encoder-wire) consumed))
                (let ((ready (http-message-kit:qpack-take-ready-streams adapter)))
                  (ensure-equal 1 (length ready))
                  (ensure-true (eq token (first ready))))
                (let ((headers (http-message-kit:qpack-resume-blocked-stream token)))
                  (push :resumed events)
                  headers))
              :qpack-serialize (lambda (thunk) (funcall thunk))
              :read-stream
              (lambda (actual &key timeout deadline)
                (declare (ignore timeout deadline))
                (ensure-true (eq stream actual))
                (incf reads)
                (ensure-equal 1 reads)
                (values request-wire t))
              :write-stream
              (lambda (actual bytes &key fin-p timeout deadline)
                (declare (ignore timeout deadline))
                (ensure-true (eq stream actual))
                (push (list (copy-seq bytes) fin-p) writes))
              :close-stream
              (lambda (actual &key condition)
                (ensure-true (eq stream actual))
                (push (cons actual condition) closes)))))
      (ensure-equal 1 reads)
      (ensure-equal 204 (http-response-status response))
      (ensure-equal '(:await (:section-ack 16) :resumed :handler)
                    (nreverse events))
      (ensure-equal 1 (length writes))
      (ensure-equal '(t) (mapcar #'second writes))
      (ensure-equal 1 (length closes))
      (ensure-true (eq stream (caar closes)))
      (ensure-equal nil (cdar closes))
      (let ((frames (http-kit/http3:decode-http3-frames (first (first writes)))))
        (ensure-equal 1 (length frames))
        (ensure-equal 1 (http-kit/http3:http3-frame-type (first frames)))
        (let ((headers
                (http-message-kit:qpack-decode-header-block
                 (http-kit/http3:http3-frame-payload (first frames)))))
          (ensure-equal "204"
                        (http-message-kit:http-header-content
                         (find ":status" headers
                               :key (function http-message-kit:http-header-name)
                               :test (function string=)))))))))
(deftest http3-client-qpack-stream-id-is-validated-before-read
  (multiple-value-bind (decoder-context section instruction)
      (http3-test-qpack-blocked-section
       (list (cons ":status" "204") (cons "x-blocked" "ready")))
    (declare (ignore decoder-context instruction))
    (let* ((response-wire
             (http-kit/http3:encode-http3-frame
              (http-kit/http3:make-http3-frame
               :type http-kit/http3:+http3-headers-type+ :payload section)))
           (reads 0)
           (client
             (http-kit/http3:make-http3-client
              :qpack-settings
              (list
               (cons http-kit/http3:+http3-setting-qpack-max-table-capacity+ 256)
               (cons http-kit/http3:+http3-setting-qpack-blocked-streams+ 1))
              :open-stream
              (lambda (request &key stream-type timeout deadline)
                (declare (ignore request timeout deadline))
                (values (make-http3-test-stream :kind stream-type)
                        (when (eq stream-type :request) nil)))
              :write-stream
              (lambda (stream bytes &key fin-p timeout deadline)
                (declare (ignore stream bytes fin-p timeout deadline)))
              :read-stream
              (lambda (stream &key timeout deadline)
                (declare (ignore stream timeout deadline))
                (incf reads)
                (values response-wire t))
              :close-stream
              (lambda (stream &key condition)
                (declare (ignore stream condition)))))
           (failure
             (http3-boundary-error
              (lambda ()
                (http-kit/http3:send-http3-request
                 client
                 (make-http-request :method "GET"
                                    :uri "https://example.test/")
                 :await-qpack
                 (lambda (token) (declare (ignore token))))))))
      (http3-public-error-check
       failure
       "Dynamic QPACK response decoding requires a numeric stream ID."
       nil)
      (ensure-equal 0 reads)
      (http-kit/http3:close-http3-client client))))
(deftest http3-client-rejects-missing-qpack-await-before-read
  (multiple-value-bind (decoder-context section instruction)
      (http3-test-qpack-blocked-section
       (list (cons ":status" "204") (cons "x-blocked" "ready")))
    (declare (ignore decoder-context instruction))
    (let* ((response-wire
             (http-kit/http3:encode-http3-frame
              (http-kit/http3:make-http3-frame
               :type http-kit/http3:+http3-headers-type+ :payload section)))
           (reads 0)
           (client
             (http-kit/http3:make-http3-client
              :qpack-settings
              (list
               (cons http-kit/http3:+http3-setting-qpack-max-table-capacity+ 256)
               (cons http-kit/http3:+http3-setting-qpack-blocked-streams+ 1))
              :open-stream
              (lambda (request &key stream-type timeout deadline)
                (declare (ignore request timeout deadline))
                (values (make-http3-test-stream :kind stream-type)
                        (when (eq stream-type :request) 0)))
              :write-stream
              (lambda (stream bytes &key fin-p timeout deadline)
                (declare (ignore stream bytes fin-p timeout deadline)))
              :read-stream
              (lambda (stream &key timeout deadline)
                (declare (ignore stream timeout deadline))
                (incf reads)
                (values response-wire t))
              :close-stream
              (lambda (stream &key condition)
                (declare (ignore stream condition)))))
           (failure
             (http3-boundary-error
              (lambda ()
                (http-kit/http3:send-http3-request
                 client
                 (make-http-request :method "GET"
                                    :uri "https://example.test/"))))))
      (http3-public-error-check
       failure
       "A blocked QPACK field section requires an await-qpack callback."
       0)
      (ensure-equal 0 reads)
      (http-kit/http3:close-http3-client client))))

