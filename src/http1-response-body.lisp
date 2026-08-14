(in-package #:http-kit)

(defparameter *http-body-read-chunk-size* 4096)

(defun %make-body-collector (collect-body-p)
  (when collect-body-p
    (make-array 1024
                :element-type '(unsigned-byte 8)
                :adjustable t
                :fill-pointer 0)))

(defun %append-body-chunk (collector chunk)
  (when collector
    (loop for octet across chunk
          do (vector-push-extend octet collector))))

(defun %finish-body-collector (collector)
  (if collector
      (let ((result (make-array (length collector)
                                :element-type '(unsigned-byte 8))))
        (replace result collector)
        result)
      (make-array 0 :element-type '(unsigned-byte 8))))

(defun %read-body-segment
    (source length deadline clock-function detail collector on-body-chunk)
  (loop with remaining = length
        while (plusp remaining)
        do (let* ((size (min remaining *http-body-read-chunk-size*))
                  (chunk (make-array size :element-type '(unsigned-byte 8))))
             (loop for index below size
                   do (setf (aref chunk index)
                            (%read-required-byte source deadline
                                                 clock-function detail)))
             (%append-body-chunk collector chunk)
             (when on-body-chunk
               (funcall on-body-chunk chunk))
             (decf remaining size)))
  collector)

(defun %read-exact-body
    (source length deadline clock-function max-body-bytes
     &key on-body-chunk (collect-body-p t))
  (%check-limit :body length max-body-bytes)
  (%finish-body-collector
   (%read-body-segment source length deadline clock-function :body
                       (%make-body-collector collect-body-p)
                       on-body-chunk)))

(defun %read-close-body
    (source deadline clock-function max-body-bytes
     &key on-body-chunk (collect-body-p t))
  (let ((body (%make-body-collector collect-body-p))
        (chunk (make-array *http-body-read-chunk-size*
                          :element-type '(unsigned-byte 8)))
        (chunk-length 0)
        (body-length 0))
    (flet ((flush-chunk ()
             (when (plusp chunk-length)
               (let ((piece (make-array chunk-length
                                        :element-type '(unsigned-byte 8))))
                 (replace piece chunk :end2 chunk-length)
                 (%append-body-chunk body piece)
                 (when on-body-chunk
                   (funcall on-body-chunk piece))
                 (setf chunk-length 0)))))
      (loop for octet = (%source-read-byte source deadline clock-function)
            do (if (eq octet :eof)
                   (progn
                     (flush-chunk)
                     (return))
                   (progn
                     (incf body-length)
                     (%check-limit :body body-length max-body-bytes)
                     (setf (aref chunk chunk-length) octet)
                     (incf chunk-length)
                     (when (= chunk-length *http-body-read-chunk-size*)
                       (flush-chunk)))))
      (%finish-body-collector body))))

(defun %read-chunked-body
    (source deadline clock-function max-header-bytes max-body-bytes header-used
     &key on-body-chunk (collect-body-p t))
  (let ((body (%make-body-collector collect-body-p))
        (trailers '())
        (bytes header-used)
        (body-length 0))
    (loop
      (multiple-value-bind (line updated-bytes)
          (%read-crlf-line source deadline clock-function max-header-bytes bytes)
        (setf bytes updated-bytes)
        (let* ((separator (position #\; line))
               (size-text (%trim-ows (if separator (subseq line 0 separator) line))))
          (unless (and (not (string= size-text ""))
                       (every (lambda (character) (%hex-character-p character))
                              size-text))
            (error 'http-protocol-error
                   :message "A chunk size is not a valid hexadecimal integer."
                   :operation :response-parse
                   :detail line))
          (let ((size (parse-integer size-text :radix 16)))
            (if (zerop size)
                (progn
                  (multiple-value-bind (parsed-trailers trailer-bytes)
                      (%read-response-headers source deadline clock-function
                                              max-header-bytes bytes)
                    (setf trailers parsed-trailers bytes trailer-bytes))
                  (return))
                (progn
                  (incf body-length size)
                  (%check-limit :body body-length max-body-bytes)
                  (%read-body-segment source size deadline clock-function
                                      :chunk-data body on-body-chunk)
                  (%read-framing-crlf source deadline clock-function)
                  (incf bytes (+ size 2))))))))
    (values (%finish-body-collector body) trailers)))
