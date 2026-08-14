(in-package #:http-kit)

(defstruct (%byte-source (:constructor %make-byte-source))
  vector
  position
  stream)

(defun %make-byte-source-for (input &key (operation :response-parse))
  (cond
    ((and (arrayp input) (= (array-rank input) 1)
          (not (stringp input)))
     (%make-byte-source :vector (%copy-octets input) :position 0))
    ((and (listp input)
          (every (lambda (octet) (and (integerp octet) (<= 0 octet 255))) input))
     (%make-byte-source :vector (%copy-octets input) :position 0))
    ((streamp input)
     (%make-byte-source :stream input))
    (t
     (error 'http-protocol-error
            :message "HTTP input must be an octet vector or binary stream."
            :operation operation
            :detail (type-of input)))))

(defun %source-read-byte (source deadline clock-function)
  (%check-deadline deadline clock-function :read)
  (if (%byte-source-stream source)
      (read-byte (%byte-source-stream source) nil :eof)
      (let ((position (%byte-source-position source))
            (vector (%byte-source-vector source)))
        (if (>= position (length vector))
            :eof
            (prog1 (aref vector position)
              (incf (%byte-source-position source)))))))

(defun %read-required-byte
    (source deadline clock-function detail &key (operation :response-parse))
  (let ((octet (%source-read-byte source deadline clock-function)))
    (when (eq octet :eof)
      (error 'http-protocol-error
             :message "The HTTP message ended before the complete message was read."
             :operation operation
             :detail detail))
    octet))

(defun %check-limit (kind observed limit &key (operation :response-parse))
  (when (and limit (> observed limit))
    (error 'http-size-limit-exceeded
           :message (format nil "The HTTP ~A limit was exceeded." kind)
           :operation operation
           :limit limit
           :observed observed
           :kind kind)))

(defun %read-crlf-line
    (source deadline clock-function max-header-bytes header-used
     &key (operation :response-parse) allow-eof-p)
  (let ((builder (make-array 64 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0))
        (previous nil)
        (bytes header-used))
    (loop
      for octet = (%source-read-byte source deadline clock-function)
      do (when (eq octet :eof)
         (if (and allow-eof-p
                  (zerop (fill-pointer builder))
                  (= bytes header-used))
             (return (values :eof bytes))
             (error 'http-protocol-error
                    :message "The HTTP message ended before CRLF."
                    :operation operation
                    :detail :missing-crlf)))
         (incf bytes)
         (%check-limit :headers bytes max-header-bytes :operation operation)
         (cond
           ((and previous (= previous 13) (= octet 10))
            (return (values (%octets-string builder) bytes)))
           ((= octet 10)
            (error 'http-protocol-error
                   :message "HTTP lines must end in CRLF."
                   :operation operation
                   :detail :bare-lf))
           ((and previous (= previous 13))
            (error 'http-protocol-error
                   :message "A carriage return in an HTTP line must be followed by LF."
                   :operation operation
                   :detail :bare-cr))
           ((or (= octet #x7f)
                (and (< octet #x20) (/= octet #x09) (/= octet #x0d)))
            (error 'http-protocol-error
                   :message "HTTP lines cannot contain control characters."
                   :operation operation
                   :detail (list :line-control octet)))
           ((/= octet 13)
            (vector-push-extend octet builder)))
         (setf previous octet))))

(defun %read-framing-crlf
    (source deadline clock-function &key (operation :response-parse))
  (unless (= (%read-required-byte source deadline clock-function :crlf
                                    :operation operation)
             13)
    (error 'http-protocol-error
           :message "Expected CRLF after an HTTP body chunk."
           :operation operation
           :detail :chunk-crlf))
  (unless (= (%read-required-byte source deadline clock-function :crlf
                                    :operation operation)
             10)
    (error 'http-protocol-error
           :message "Expected CRLF after an HTTP body chunk."
           :operation operation
           :detail :chunk-crlf)))
