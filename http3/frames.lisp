(in-package #:http-kit/http3)

(defconstant +http3-data-type+ 0)
(defconstant +http3-headers-type+ 1)
(defconstant +http3-cancel-push-type+ 3)
(defconstant +http3-settings-type+ 4)
(defconstant +http3-push-promise-type+ 5)
(defconstant +http3-goaway-type+ 7)
(defconstant +http3-max-push-id-type+ 13)
(defconstant +http3-priority-update-request-type+ #xf0700)
(defconstant +http3-priority-update-push-type+ #xf0701)

(defconstant +http3-control-stream-type+ 0)
(defconstant +http3-push-stream-type+ 1)
(defconstant +http3-qpack-encoder-stream-type+ 2)
(defconstant +http3-qpack-decoder-stream-type+ 3)

(defconstant +http3-setting-qpack-max-table-capacity+ 1)
(defconstant +http3-setting-enable-push+ 2)
(defconstant +http3-setting-max-field-section-size+ 6)
(defconstant +http3-setting-qpack-blocked-streams+ 7)
(defconstant +http3-setting-enable-connect+ 8)
(defconstant +http3-setting-h3-datagram+ #x33)

(defconstant +http3-default-max-frame-size+ #x4000)

(defstruct (http3-frame
            (:constructor %make-http3-frame (type payload)))
  (type 0 :type integer)
  (payload #() :type vector))

(defun %http3-copy-octets (octets)
  (let ((copy (make-array (length octets)
                         :element-type '(unsigned-byte 8))))
    (replace copy octets)
    copy))

(defun %http3-frame-error (message &optional detail)
  (error 'http-protocol-error
         :message message
         :operation :http3-frame
         :detail detail))

(defun make-http3-frame (&key type payload)
  (unless (and (integerp type) (<= 0 type +http3-max-varint+))
    (%http3-frame-error
     "HTTP/3 frame types must be non-negative QUIC variable-length integers."
     type))
  (let ((bytes (or payload (make-array 0 :element-type '(unsigned-byte 8)))))
    (unless (%http3-octet-vector-p bytes)
      (%http3-frame-error "HTTP/3 frame payload must be an octet vector."
                          (type-of payload)))
    (%make-http3-frame type (%http3-copy-octets bytes))))

(defun %http3-concatenate-octets (&rest parts)
  (let ((result (make-array (reduce #'+ parts :key #'length :initial-value 0)
                            :element-type '(unsigned-byte 8)))
        (position 0))
    (dolist (part parts result)
      (replace result part :start1 position)
      (incf position (length part)))))

(defun encode-http3-frame (frame)
  (unless (http3-frame-p frame)
    (%http3-frame-error "ENCODE-HTTP3-FRAME expects an HTTP/3 frame."
                        (type-of frame)))
  (let ((payload (http3-frame-payload frame)))
    (%http3-concatenate-octets
     (http3-varint-encode (http3-frame-type frame))
     (http3-varint-encode (length payload))
     payload)))

(defun %http3-priority-field-value-octets (value)
  (unless (stringp value)
    (%http3-frame-error "An HTTP Priority Field Value must be a string."
                        (type-of value)))
  (let ((octets (make-array (length value) :element-type '(unsigned-byte 8))))
    (loop for character across value
          for code = (char-code character)
          for position from 0
          unless (or (= code 9) (<= 32 code 126))
            do (%http3-frame-error
                "An HTTP Priority Field Value must contain only ASCII field-value characters."
                (list position code))
          do (setf (aref octets position) code))
    octets))

(defun make-http3-priority-update-frame
    (&key element-id priority-field-value (kind :request))
  "Construct an RFC 9218 PRIORITY_UPDATE frame for a request or push."
  (unless (and (integerp element-id) (<= 0 element-id +http3-max-varint+))
    (%http3-frame-error "An HTTP/3 prioritized element identifier is invalid."
                        element-id))
  (let ((type (ecase kind
                (:request +http3-priority-update-request-type+)
                (:push +http3-priority-update-push-type+))))
    (make-http3-frame
     :type type
     :payload (%http3-concatenate-octets
               (http3-varint-encode element-id)
               (%http3-priority-field-value-octets priority-field-value)))))

(defun decode-http3-priority-update (frame)
  "Return the element identifier, Priority Field Value, and kind of FRAME."
  (unless (and (http3-frame-p frame)
               (member (http3-frame-type frame)
                       (list +http3-priority-update-request-type+
                             +http3-priority-update-push-type+)
                       :test #'=))
    (%http3-frame-error "The frame is not an HTTP/3 PRIORITY_UPDATE frame."
                        (and (http3-frame-p frame) (http3-frame-type frame))))
  (let ((payload (http3-frame-payload frame)))
    (multiple-value-bind (element-id position)
        (http3-varint-decode payload)
      (let ((value
              (coerce
               (loop for index from position below (length payload)
                     for code = (aref payload index)
                     unless (or (= code 9) (<= 32 code 126))
                       do (%http3-frame-error
                           "An HTTP/3 PRIORITY_UPDATE value contains a non-ASCII field-value octet."
                           (list index code))
                     collect (code-char code))
               'string)))
        (values element-id value
                (if (= (http3-frame-type frame)
                       +http3-priority-update-request-type+)
                    :request
                    :push))))))

(defun %http3-decode-one-frame (octets position allow-incomplete-p max-frame-size)
  (multiple-value-bind (type after-type)
      (http3-varint-decode octets :position position
                           :allow-incomplete-p allow-incomplete-p)
    (if (null type)
        (values nil position)
        (multiple-value-bind (length after-length)
            (http3-varint-decode octets :position after-type
                                 :allow-incomplete-p allow-incomplete-p)
          (if (null length)
              (values nil position)
              (progn
                (when (> length max-frame-size)
                  (error 'http-size-limit-exceeded
                         :message "HTTP/3 frame exceeds the configured frame-size limit."
                         :operation :http3-frame
                         :limit max-frame-size
                         :observed length
                         :kind :frame))
                (let ((end (+ after-length length)))
                  (if (> end (length octets))
                      (if allow-incomplete-p
                          (values nil position)
                          (%http3-frame-error "HTTP/3 frame payload is incomplete."))
                      (values
                       (make-http3-frame
                        :type type
                        :payload (subseq octets after-length end))
                       end)))))))))

(defun decode-http3-frames (octets &key allow-incomplete-p
                                      (max-frame-size +http3-default-max-frame-size+))
  "Decode all complete HTTP/3 frames in OCTETS.

Returns a list of frames and the unconsumed suffix.  The suffix is empty for
a complete input.  With ALLOW-INCOMPLETE-P, a partial frame is returned as the
second value; otherwise it signals a protocol error."
  (unless (%http3-octet-vector-p octets)
    (%http3-frame-error "HTTP/3 frame input must be an octet vector."
                        (type-of octets)))
  (unless (and (integerp max-frame-size) (>= max-frame-size 0))
    (%http3-frame-error "HTTP/3 frame-size limit must be a non-negative integer."
                        max-frame-size))
  (let ((position 0)
        (frames '()))
    (loop while (< position (length octets))
          do (multiple-value-bind (frame next)
                 (%http3-decode-one-frame octets position allow-incomplete-p
                                          max-frame-size)
               (if frame
                   (progn
                     (push frame frames)
                     (setf position next))
                   (return (values (nreverse frames)
                                   (subseq octets position)))))
          finally (return (values (nreverse frames)
                                  (make-array 0 :element-type '(unsigned-byte 8)))))))

(defun %http3-setting-value (value name)
  (unless (and (integerp value) (<= 0 value +http3-max-varint+))
    (%http3-frame-error "HTTP/3 setting values must be non-negative QUIC variable-length integers."
                        (list name value)))
  value)

(defun %http3-validate-setting (identifier value)
  (when (<= +http3-setting-enable-push+ identifier 5)
    (%http3-frame-error
     "HTTP/2-specific settings are prohibited in HTTP/3."
     identifier))
  (when (and (member identifier
                     (list +http3-setting-enable-connect+
                           +http3-setting-h3-datagram+)
                     :test #'=)
             (> value 1))
    (%http3-frame-error "HTTP/3 boolean settings must be either 0 or 1."
                        (cons identifier value)))
  value)

(defun %http3-setting-pairs (qpack-max-table-capacity
                             max-field-section-size
                             qpack-blocked-streams
                             enable-connect
                             h3-datagram
                             extra-settings)
  (let ((pairs (list
                (cons +http3-setting-qpack-max-table-capacity+
                      (%http3-setting-value qpack-max-table-capacity
                                             :qpack-max-table-capacity))
                (cons +http3-setting-qpack-blocked-streams+
                      (%http3-setting-value qpack-blocked-streams
                                             :qpack-blocked-streams)))))
    (when max-field-section-size
      (push (cons +http3-setting-max-field-section-size+
                  (%http3-setting-value max-field-section-size
                                         :max-field-section-size))
            pairs))
    (when enable-connect
      (push (cons +http3-setting-enable-connect+
                  (%http3-setting-value enable-connect :enable-connect))
            pairs))
    (when h3-datagram
      (push (cons +http3-setting-h3-datagram+
                  (%http3-setting-value h3-datagram :h3-datagram))
            pairs))
    (dolist (pair extra-settings)
      (unless (and (consp pair)
                   (integerp (car pair))
                   (<= 0 (car pair) +http3-max-varint+))
        (%http3-frame-error "Extra HTTP/3 settings must be (identifier . value) pairs."
                            pair))
      (push (cons (car pair) (%http3-setting-value (cdr pair) pair)) pairs))
    (setf pairs (nreverse pairs))
    (let ((seen '()))
      (dolist (pair pairs)
        (%http3-validate-setting (car pair) (cdr pair))
        (when (member (car pair) seen)
          (%http3-frame-error "An HTTP/3 SETTINGS frame cannot contain duplicate identifiers."
                              (car pair)))
        (push (car pair) seen)))
    pairs))

(defun make-http3-settings-frame
    (&key (qpack-max-table-capacity 0)
          max-field-section-size
          (qpack-blocked-streams 0)
          enable-connect
          h3-datagram
          (extra-settings '()))
  (let ((payload (make-array 0 :element-type '(unsigned-byte 8))))
    (dolist (pair (%http3-setting-pairs qpack-max-table-capacity
                                        max-field-section-size
                                        qpack-blocked-streams
                                        enable-connect
                                        h3-datagram
                                        extra-settings))
      (setf payload (%http3-concatenate-octets
                     payload
                     (http3-varint-encode (car pair))
                     (http3-varint-encode (cdr pair)))))
    (make-http3-frame :type +http3-settings-type+
                      :payload payload)))

(defun decode-http3-settings (payload-or-frame)
  "Decode an HTTP/3 SETTINGS payload or SETTINGS frame into an alist."
  (let ((payload (if (http3-frame-p payload-or-frame)
                     (progn
                       (unless (= (http3-frame-type payload-or-frame)
                                  +http3-settings-type+)
                         (%http3-frame-error
                          "DECODE-HTTP3-SETTINGS received a non-SETTINGS frame."
                          (http3-frame-type payload-or-frame)))
                       (http3-frame-payload payload-or-frame))
                     payload-or-frame)))
    (unless (%http3-octet-vector-p payload)
      (%http3-frame-error "HTTP/3 SETTINGS input must be a frame or octet vector."
                          (type-of payload)))
    (let ((position 0)
          (settings '()))
      (loop while (< position (length payload))
            do (multiple-value-bind (identifier next-id)
                   (http3-varint-decode payload :position position)
                 (multiple-value-bind (value next-value)
                     (http3-varint-decode payload :position next-id)
                   (when (assoc identifier settings)
                     (%http3-frame-error
                      "An HTTP/3 SETTINGS payload cannot contain duplicate identifiers."
                      identifier))
                   (%http3-validate-setting identifier value)
                   (push (cons identifier value) settings)
                   (setf position next-value))))
      (nreverse settings))))

(defstruct (http3-control-state
            (:constructor make-http3-control-state
                (&key (settings-received-p nil) settings goaway-id
                      max-push-id (promised-push-ids '())
                      (cancelled-push-ids '()) (priority-updates '()) peer-role)))
  "Mutable state for one HTTP/3 control stream."
  (settings-received-p nil :type boolean)
  settings
  goaway-id
  max-push-id
  peer-role
  (promised-push-ids '() :type list)
  (cancelled-push-ids '() :type list)
  (priority-updates '() :type list))

(defun %http3-control-error (message &optional detail)
  (error 'http-protocol-error
         :message message
         :operation :http3-control-stream
         :detail detail))

(defun %http3-control-payload-varint (frame description)
  (let ((payload (http3-frame-payload frame)))
    (handler-case
        (multiple-value-bind (value position)
            (http3-varint-decode payload)
          (unless (= position (length payload))
            (%http3-control-error
             (format nil "HTTP/3 ~A payload must contain exactly one varint."
                     description)
             payload))
          value)
      (http-protocol-error (condition)
        (error condition)))))

(defun process-http3-control-frame (state frame)
  "Validate and apply one decoded HTTP/3 control-stream FRAME to STATE.

Returns a keyword identifying the frame's effect.  Unknown extension frames
are ignored and return :EXTENSION, as required by HTTP/3 extensibility rules."
  (unless (http3-control-state-p state)
    (%http3-control-error "HTTP/3 control-frame processing requires a control state."
                          (type-of state)))
  (unless (http3-frame-p frame)
    (%http3-control-error "HTTP/3 control-frame processing requires a frame."
                          (type-of frame)))
  (let ((type (http3-frame-type frame)))
    (when (and (not (http3-control-state-settings-received-p state))
               (/= type +http3-settings-type+))
      (%http3-control-error
       "The first HTTP/3 control-stream frame must be SETTINGS."
       type))
    (when (and (null (http3-control-state-peer-role state))
               (member type (list +http3-goaway-type+
                                  +http3-max-push-id-type+
                                  +http3-cancel-push-type+
                                  +http3-priority-update-request-type+
                                  +http3-priority-update-push-type+)))
      (%http3-control-error
       "HTTP/3 role-sensitive control frames require a peer role."
       type))
    (cond
      ((= type +http3-settings-type+)
       (when (http3-control-state-settings-received-p state)
         (%http3-control-error
          "An HTTP/3 control stream cannot contain more than one SETTINGS frame."))
       (setf (http3-control-state-settings state)
             (decode-http3-settings frame)
             (http3-control-state-settings-received-p state) t)
       :settings)
      ((= type +http3-goaway-type+)
       (let ((id (%http3-control-payload-varint frame "GOAWAY")))
         (when (and (eq (http3-control-state-peer-role state) :server)
                    (not (zerop (mod id 4))))
           (%http3-control-error
            "A server HTTP/3 GOAWAY identifier must identify a client-initiated bidirectional stream."
            id))
         (when (and (http3-control-state-goaway-id state)
                    (> id (http3-control-state-goaway-id state)))
           (%http3-control-error
            "HTTP/3 GOAWAY identifiers must not increase."
            id))
         (setf (http3-control-state-goaway-id state) id)
         :goaway))
      ((= type +http3-max-push-id-type+)
       (when (eq (http3-control-state-peer-role state) :server)
         (%http3-control-error
          "An HTTP/3 server cannot send MAX_PUSH_ID."))
       (let ((id (%http3-control-payload-varint frame "MAX_PUSH_ID")))
         (when (and (http3-control-state-max-push-id state)
                    (< id (http3-control-state-max-push-id state)))
           (%http3-control-error
            "HTTP/3 MAX_PUSH_ID identifiers must not decrease."
            id))
         (setf (http3-control-state-max-push-id state) id)
         :max-push-id))
      ((= type +http3-cancel-push-type+)
       (let ((id (%http3-control-payload-varint frame "CANCEL_PUSH")))
         (when (eq (http3-control-state-peer-role state) :server)
           (%http3-control-error
            "An HTTP/3 server cannot send CANCEL_PUSH."
            :h3-id-error))
         (unless (and (http3-control-state-max-push-id state)
                      (<= id (http3-control-state-max-push-id state))
                      (member id
                              (http3-control-state-promised-push-ids state)
                              :test #'eql))
           (%http3-control-error
            "HTTP/3 CANCEL_PUSH must identify a promised push within MAX_PUSH_ID."
            :h3-id-error))
         (pushnew id (http3-control-state-cancelled-push-ids state) :test #'eql)
         :cancel-push))
      ((member type (list +http3-priority-update-request-type+
                          +http3-priority-update-push-type+)
               :test #'=)
       (when (eq (http3-control-state-peer-role state) :server)
         (%http3-control-error
          "An HTTP/3 server cannot send PRIORITY_UPDATE."
          :h3-frame-unexpected))
       (multiple-value-bind (element-id value kind)
           (decode-http3-priority-update frame)
         (if (eq kind :request)
             (unless (zerop (mod element-id 4))
               (%http3-control-error
                "An HTTP/3 request PRIORITY_UPDATE must identify a client-initiated bidirectional stream."
                :h3-id-error))
             (unless (and (http3-control-state-max-push-id state)
                          (<= element-id
                              (http3-control-state-max-push-id state))
                          (member element-id
                                  (http3-control-state-promised-push-ids state)
                                  :test #'eql))
               (%http3-control-error
                "An HTTP/3 push PRIORITY_UPDATE must identify a promised push within MAX_PUSH_ID."
                :h3-id-error)))
         (let* ((key (cons kind element-id))
                (entry (assoc key
                              (http3-control-state-priority-updates state)
                              :test #'equal)))
           (if entry
               (setf (cdr entry) value)
               (push (cons key value)
                     (http3-control-state-priority-updates state))))
         :priority-update))
      ((member type (list +http3-data-type+
                         +http3-headers-type+
                         +http3-push-promise-type+)
               :test #'=)
       (%http3-control-error
        "HTTP/3 DATA, HEADERS, and PUSH_PROMISE frames are invalid on a control stream."
        type))
      (t
       :extension))))

(defun process-http3-control-bytes
    (state octets &key allow-incomplete-p
                         (max-frame-size +http3-default-max-frame-size+))
  "Decode and apply complete control-stream frames in OCTETS.

Returns a list of effect keywords and the unconsumed octet suffix.  The
caller must reject a non-empty suffix when the stream reaches FIN."
  (unless (http3-control-state-p state)
    (%http3-control-error "HTTP/3 control-byte processing requires a control state."
                          (type-of state)))
  (multiple-value-bind (frames remainder)
      (decode-http3-frames octets
                           :allow-incomplete-p allow-incomplete-p
                           :max-frame-size max-frame-size)
    (values (mapcar (lambda (frame)
                      (process-http3-control-frame state frame))
                    frames)
            remainder)))

(defun http3-control-stream-prefix ()
  "Return the unidirectional stream-type prefix for an HTTP/3 control stream."
  (http3-varint-encode +http3-control-stream-type+))

(defun http3-qpack-encoder-stream-prefix ()
  "Return the unidirectional stream-type prefix for a QPACK encoder stream."
  (http3-varint-encode +http3-qpack-encoder-stream-type+))

(defun http3-qpack-decoder-stream-prefix ()
  "Return the unidirectional stream-type prefix for a QPACK decoder stream."
  (http3-varint-encode +http3-qpack-decoder-stream-type+))
