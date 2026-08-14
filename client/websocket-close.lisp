(in-package #:http-kit/websocket)

(defun websocket-valid-close-code-p (code)
  (and (integerp code)
       (or (member code '(1000 1001 1002 1003 1007 1008 1009 1010 1011)
                   :test #'=)
           (<= 3000 code 4999))))

(defun %websocket-utf8-continuation-p (byte)
  (<= #x80 byte #xbf))

(defun %websocket-utf8-string (octets)
  (unless (%websocket-octet-vector-p octets)
    (%websocket-protocol-error "A WebSocket reason must be UTF-8 octets." octets))
  (with-output-to-string (result)
    (loop with index = 0
          while (< index (length octets))
          do (let ((first (aref octets index)))
               (cond ((<= first #x7f)
                      (write-char (code-char first) result)
                      (incf index))
                     ((<= #xc2 first #xdf)
                      (when (> (1+ index) (1- (length octets)))
                        (%websocket-protocol-error
                         "A WebSocket close reason ended in a partial UTF-8 sequence."))
                      (let ((second (aref octets (1+ index))))
                        (unless (%websocket-utf8-continuation-p second)
                          (%websocket-protocol-error
                           "A WebSocket close reason contains invalid UTF-8."
                           octets))
                        (write-char
                         (code-char (+ (ash (logand first #x1f) 6)
                                       (logand second #x3f)))
                         result)
                        (incf index 2)))
                     ((<= #xe0 first #xef)
                      (when (> (+ index 2) (1- (length octets)))
                        (%websocket-protocol-error
                         "A WebSocket close reason ended in a partial UTF-8 sequence."))
                      (let ((second (aref octets (1+ index)))
                            (third (aref octets (+ index 2))))
                        (unless (and (%websocket-utf8-continuation-p second)
                                     (%websocket-utf8-continuation-p third)
                                     (or (/= first #xe0) (>= second #xa0))
                                     (or (/= first #xed) (<= second #x9f)))
                          (%websocket-protocol-error
                           "A WebSocket close reason contains invalid UTF-8."
                           octets))
                        (write-char
                         (code-char (+ (ash (logand first #x0f) 12)
                                       (ash (logand second #x3f) 6)
                                       (logand third #x3f)))
                         result)
                        (incf index 3)))
                     ((<= #xf0 first #xf4)
                      (when (> (+ index 3) (1- (length octets)))
                        (%websocket-protocol-error
                         "A WebSocket close reason ended in a partial UTF-8 sequence."))
                      (let ((second (aref octets (1+ index)))
                            (third (aref octets (+ index 2)))
                            (fourth (aref octets (+ index 3))))
                        (unless (and (%websocket-utf8-continuation-p second)
                                     (%websocket-utf8-continuation-p third)
                                     (%websocket-utf8-continuation-p fourth)
                                     (or (/= first #xf0) (>= second #x90))
                                     (or (/= first #xf4) (<= second #x8f)))
                          (%websocket-protocol-error
                           "A WebSocket close reason contains invalid UTF-8."
                           octets))
                        (write-char
                         (code-char (+ (ash (logand first #x07) 18)
                                       (ash (logand second #x3f) 12)
                                       (ash (logand third #x3f) 6)
                                       (logand fourth #x3f)))
                         result)
                        (incf index 4)))
                     (t
                      (%websocket-protocol-error
                       "A WebSocket close reason contains invalid UTF-8."
                       octets)))))))

(defun make-websocket-close-payload (&key (code 1000) (reason ""))
  "Construct the payload for a WebSocket close control frame."
  (unless (websocket-valid-close-code-p code)
    (%websocket-protocol-error "The WebSocket close code is not permitted." code))
  (unless (stringp reason)
    (%websocket-protocol-error "The WebSocket close reason must be a string." reason))
  (let ((reason-octets (cl-codec-kit:string-to-octets reason :encoding :utf-8)))
    (when (> (length reason-octets) 123)
      (%websocket-size-error
       "A WebSocket close reason exceeded its 123-octet limit."
       123 (length reason-octets)))
    (let ((payload (make-array (+ 2 (length reason-octets))
                               :element-type '(unsigned-byte 8))))
      (%websocket-store-integer payload 0 2 code)
      (replace payload reason-octets :start1 2)
      payload)))

(defun parse-websocket-close-payload (payload)
  "Parse a close payload and return its code and UTF-8 reason."
  (unless (%websocket-octet-vector-p payload)
    (%websocket-protocol-error "A WebSocket close payload must be octets." payload))
  (cond ((zerop (array-total-size payload))
         (values nil ""))
        ((= (length payload) 1)
         (%websocket-protocol-error
          "A WebSocket close payload cannot contain one octet."))
        (t
         (let ((code (%websocket-read-integer payload 0 2)))
           (unless (websocket-valid-close-code-p code)
             (%websocket-protocol-error
              "The WebSocket close code is not permitted."
              code))
           (values code (%websocket-utf8-string (subseq payload 2)))))))
