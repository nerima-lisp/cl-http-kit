(in-package #:http-kit/http3)

(defun qpack-process-encoder-stream (table octets)
  "Apply complete QPACK encoder-stream instructions to TABLE.

Returns two values: an event list and the consumed position.  The parser is
intentionally strict and signals when OCTETS ends in the middle of an
instruction; stream buffering belongs to the HTTP/3 transport."
  (unless (qpack-dynamic-table-p table)
    (%qpack-error "A QPACK dynamic table object is required." table))
  (unless (%http3-octet-vector-p octets)
    (%qpack-error "QPACK encoder streams must be octet vectors." (type-of octets)))
  (let ((position 0)
        (events '()))
    (loop while (< position (length octets))
          do (let ((first (aref octets position)))
               (cond
                 ((= (logand first #xe0) #x20)
                  (multiple-value-bind (capacity next)
                      (http-kit/http2::%hpack-read-integer octets position 5)
                    (qpack-dynamic-table-set-capacity table capacity)
                    (push (list :set-capacity capacity) events)
                    (setf position next)))
                 ((/= 0 (logand first #x80))
                  (let ((static-p (/= 0 (logand first #x40))))
                    (multiple-value-bind (name-index next)
                        (http-kit/http2::%hpack-read-integer octets position 6)
                      (let ((entry
                              (if static-p
                                  (or (%qpack-static-entry name-index)
                                      (%qpack-error
                                       "QPACK static-table name index is out of range."
                                       name-index))
                                  (%qpack-dynamic-entry-at-relative
                                   table name-index))))
                        (multiple-value-bind (value after-value)
                            (%qpack-decode-string octets next 7 #x80)
                          (let ((inserted
                                  (qpack-dynamic-table-insert
                                   table
                                   (if (qpack-dynamic-entry-p entry)
                                       (qpack-dynamic-entry-name entry)
                                       (first entry))
                                   value)))
                            (push (list :insert inserted) events)
                            (setf position after-value)))))))
                 ((= (logand first #xc0) #x40)
                  (multiple-value-bind (name next)
                      (%qpack-decode-string octets position 5 #x20)
                    (multiple-value-bind (value after-value)
                        (%qpack-decode-string octets next 7 #x80)
                      (let ((inserted (qpack-dynamic-table-insert
                                       table name value)))
                        (push (list :insert inserted) events)
                        (setf position after-value)))))
                 (t
                  (multiple-value-bind (relative-index next)
                      (http-kit/http2::%hpack-read-integer octets position 5)
                    (let* ((source (%qpack-dynamic-entry-at-relative
                                    table relative-index))
                           (inserted
                             (qpack-dynamic-table-insert
                              table
                              (qpack-dynamic-entry-name source)
                              (qpack-dynamic-entry-value source))))
                      (push (list :duplicate inserted) events)
                      (setf position next)))))))
    (values (nreverse events) position)))

(defun qpack-process-decoder-stream
    (octets &key on-section-acknowledgment on-stream-cancellation
            on-insert-count-increment)
  "Decode QPACK decoder-stream instructions and return event records."
  (unless (%http3-octet-vector-p octets)
    (%qpack-error "QPACK decoder streams must be octet vectors." (type-of octets)))
  (dolist (callback (list on-section-acknowledgment
                          on-stream-cancellation
                          on-insert-count-increment))
    (when (and callback (not (functionp callback)))
      (%qpack-error "QPACK decoder-stream callbacks must be functions." callback)))
  (let ((position 0)
        (events '()))
    (loop while (< position (length octets))
          do (let ((first (aref octets position)))
               (cond
                 ((/= 0 (logand first #x80))
                  (multiple-value-bind (stream-id next)
                      (http-kit/http2::%hpack-read-integer octets position 7)
                    (when on-section-acknowledgment
                      (funcall on-section-acknowledgment stream-id))
                    (push (list :section-acknowledgment stream-id) events)
                    (setf position next)))
                 ((= (logand first #xc0) #x40)
                  (multiple-value-bind (stream-id next)
                      (http-kit/http2::%hpack-read-integer octets position 6)
                    (when on-stream-cancellation
                      (funcall on-stream-cancellation stream-id))
                    (push (list :stream-cancellation stream-id) events)
                    (setf position next)))
                 (t
                  (multiple-value-bind (increment next)
                      (http-kit/http2::%hpack-read-integer octets position 6)
                    (unless (plusp increment)
                      (%qpack-error
                       "QPACK insert-count increments must be positive."
                       increment))
                    (when on-insert-count-increment
                      (funcall on-insert-count-increment increment))
                    (push (list :insert-count-increment increment) events)
                    (setf position next))))))
    (values (nreverse events) position)))
