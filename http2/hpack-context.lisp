(in-package #:http-kit/http2)

(defparameter *h2-max-peer-hpack-table-size* +hpack-default-table-size+
  "Maximum HPACK dynamic-table capacity accepted from a peer.")

(defstruct (%hpack-entry (:constructor %make-hpack-entry))
  name
  value
  size)

(defun %hpack-calculated-size (name value)
  (+ 32 (length name) (length value)))

(defun %hpack-evict (context)
  (loop while (> (%hpack-context-size context) (%hpack-context-max-size context))
        do (let ((last (car (last (%hpack-context-entries context)))))
             (unless last
               (return))
             (decf (%hpack-context-size context)
                   (%hpack-calculated-size (%hpack-entry-name last)
                                           (%hpack-entry-value last)))
             (setf (%hpack-context-entries context)
                   (butlast (%hpack-context-entries context))))))

(defun %hpack-set-maximum-size (context size)
  (unless (and (integerp size) (<= 0 size #xffffffff))
    (error 'http-protocol-error
           :message "An HTTP/2 HPACK maximum dynamic table size is invalid."
           :operation :hpack
           :detail size))
  (setf (%hpack-context-maximum-size context) size)
  (when (> (%hpack-context-max-size context) size)
    (setf (%hpack-context-max-size context) size)
    (%hpack-evict context)))

(defun %hpack-set-max-size (context size)
  (unless (and (integerp size) (<= 0 size #xffffffff))
    (error 'http-protocol-error
           :message "An HTTP/2 HPACK dynamic table size is invalid."
           :operation :hpack
           :detail size))
  (when (> size (%hpack-context-maximum-size context))
    (error 'http-protocol-error
           :message "An HPACK table size update exceeds the peer setting."
           :operation :hpack
           :detail (list size (%hpack-context-maximum-size context))))
  (setf (%hpack-context-max-size context) size)
  (%hpack-evict context))

(defun %hpack-add (context name value)
  (let ((entry-size (%hpack-calculated-size name value)))
    (if (> entry-size (%hpack-context-max-size context))
        (setf (%hpack-context-entries context) '()
              (%hpack-context-size context) 0)
        (progn
          (push (%make-hpack-entry :name name :value value :size entry-size)
                (%hpack-context-entries context))
          (incf (%hpack-context-size context) entry-size)
          (%hpack-evict context)))))
