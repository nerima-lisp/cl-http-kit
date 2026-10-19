(in-package #:http-kit/test)

#+sbcl
(deftest client-sse-reader-rejects-out-of-range-stream-bytes
  (dolist (byte '(-1 256))
    (let ((stream (make-instance 'binary-test-stream :input (vector byte)))
          (caught nil)
          (callbacks 0))
      (handler-case
          (read-http-sse-events stream
                                :on-event (lambda (event)
                                            (declare (ignore event))
                                            (incf callbacks)))
        (http-protocol-error (condition) (setf caught condition)))
      (ensure-true (typep caught 'http-protocol-error))
      (ensure-equal :sse (http-error-operation caught))
      (ensure-equal byte (http-protocol-error-detail caught))
      (ensure-equal 0 callbacks)))
  (let* ((stream (make-instance 'binary-test-stream
                                :input (ascii (format nil "data:ok~%~%"))))
         (events (read-http-sse-events stream)))
    (ensure-equal 1 (length events))
    (ensure-equal "ok" (http-sse-event-data (first events)))))

(deftest client-sse-serializer-rejects-non-events
  (dolist (value (list nil t 42 "data:x" #() '(event)))
    (let ((caught nil))
      (handler-case (serialize-http-sse-event value)
        (http-protocol-error (condition) (setf caught condition)))
      (ensure-true (typep caught 'http-protocol-error))
      (ensure-equal :sse (http-error-operation caught))
      (ensure-true (eq value (http-protocol-error-detail caught))))))

(deftest client-sse-serializer-revalidates-mutated-fields
  ;; Public structure accessors can change a value after constructor validation.
  (dolist (case (list (list :event nil)
                     (list :event 42)
                     (list :event (format nil "name~Cother" #\Return))
                     (list :event (format nil "name~Cother" #\Linefeed))
                     (list :data nil)
                     (list :data #())
                     (list :id 42)
                     (list :id (format nil "id~Cother" #\Return))
                     (list :id (format nil "id~Cother" #\Linefeed))
                     (list :id (format nil "id~Cother" #\Null))
                     (list :retry -1)
                     (list :retry 1.5)
                     (list :retry "10")
                     (list :comments 42)
                     (list :comments '("ok" . "tail"))
                     (list :comments '("ok" 42))
                     (list :comments (format nil "note~Cother" #\Return))))
    (destructuring-bind (field value) case
      (let ((event (make-http-sse-event :data "unchanged"))
            (caught nil))
        (ecase field
          (:event (setf (http-sse-event-event event) value))
          (:data (setf (http-sse-event-data event) value))
          (:id (setf (http-sse-event-id event) value))
          (:retry (setf (http-sse-event-retry event) value))
          (:comments (setf (http-sse-event-comments event) value)))
        (handler-case (serialize-http-sse-event event)
          (http-protocol-error (condition) (setf caught condition)))
        (ensure-true (typep caught 'http-protocol-error))
        (ensure-equal :sse (http-error-operation caught))
        (ensure-true (eq value (http-protocol-error-detail caught)))))))

(deftest client-sse-serializer-accepts-valid-mutated-fields
  (let ((event (make-http-sse-event)))
    (setf (http-sse-event-event event) "update"
          (http-sse-event-data event) (format nil " first~%second")
          (http-sse-event-id event) " id"
          (http-sse-event-retry event) 0
          (http-sse-event-comments event) '("note" ""))
    (let* ((wire (serialize-http-sse-event event))
           (parsed (parse-http-sse-events wire)))
      (ensure-octets-equal
       (ascii (format nil ":note~C~C:~C~Cevent:update~C~Cid:  id~C~Cretry:0~C~Cdata:  first~C~Cdata:second~C~C~C~C"
                      #\Return #\Linefeed #\Return #\Linefeed
                      #\Return #\Linefeed #\Return #\Linefeed
                      #\Return #\Linefeed #\Return #\Linefeed
                      #\Return #\Linefeed #\Return #\Linefeed))
       wire)
      (ensure-equal 1 (length parsed))
      (ensure-equal "update" (http-sse-event-event (first parsed)))
      (ensure-equal (format nil " first~%second")
                    (http-sse-event-data (first parsed)))
      (ensure-equal " id" (http-sse-event-id (first parsed)))
      (ensure-equal 0 (http-sse-event-retry (first parsed)))
      (ensure-equal '("note" "") (http-sse-event-comments (first parsed))))))

(deftest client-sse-comments-reject-cycles-without-mutation
  (dolist (size '(1 2 3))
    (let* ((comments (loop repeat size collect "note"))
           (nodes (loop for node on comments collect node)))
      (setf (cdr (car (last nodes)))
            (if (= size 3) (second nodes) (first nodes)))
      (let ((tails (mapcar #'cdr nodes)))
        (dolist (operation '(:construct :serialize))
          (let ((event (make-http-sse-event :data "unchanged"))
                (caught nil))
            (handler-case
                (ecase operation
                  (:construct (make-http-sse-event :comments comments))
                  (:serialize
                   (setf (http-sse-event-comments event) comments)
                   (serialize-http-sse-event event)))
              (http-protocol-error (condition) (setf caught condition)))
            (ensure-true (typep caught 'http-protocol-error))
            (ensure-equal :sse (http-error-operation caught))
            (ensure-true (eq comments (http-protocol-error-detail caught)))
            (loop for node in nodes
                  for tail in tails
                  do (ensure-equal "note" (car node))
                     (ensure-true (eq tail (cdr node))))
            (when (eq operation :serialize)
              (ensure-equal "unchanged" (http-sse-event-data event))
              (ensure-true (eq comments (http-sse-event-comments event)))))))))
  (let* ((comments (list "first" "second"))
         (event (make-http-sse-event :data "ok" :comments comments))
         (parsed (parse-http-sse-events (serialize-http-sse-event event))))
    (ensure-equal comments (http-sse-event-comments event))
    (ensure-false (eq comments (http-sse-event-comments event)))
    (ensure-equal comments (http-sse-event-comments (first parsed)))))

(deftest client-sse-parser-ignores-invalid-metadata-and-preserves-id
  (let* ((wire (format nil "id:stable~%event:~%retry:5~%retry:~%retry:-1~%retry:1x~%id:bad~Cvalue~%unknown:ignored~%data:first~%data:second~%~%retry:bad~%data:third~%~%:not-dispatched~%~%"
                       #\Null))
         (octets (cl-codec-kit:string-to-octets wire :encoding :utf-8)))
    (dolist (case (list (list wire nil nil nil nil)
                       (list octets nil nil nil nil)
                       (list wire 2 32 16 128)))
      (destructuring-bind (input events-limit line-limit data-limit event-limit) case
        (let* ((original (copy-seq input))
               (events (parse-http-sse-events
                        input :max-events events-limit :max-line-bytes line-limit
                        :max-data-bytes data-limit :max-event-bytes event-limit)))
          (ensure-equal 2 (length events))
          (ensure-true (equalp original input))
          (loop for event in events
                for data in (list (format nil "first~%second") "third")
                for retry in '(5 nil)
                do (ensure-equal "message" (http-sse-event-event event))
                   (ensure-equal data (http-sse-event-data event))
                   (ensure-equal "stable" (http-sse-event-id event))
                   (ensure-equal retry (http-sse-event-retry event))
                   (ensure-false (http-sse-event-comments event))))))))

(deftest client-sse-constructor-rejects-invalid-metadata
  (dolist (case (list (list :event (format nil "name~Cother" #\Return))
                     (list :event (format nil "name~Cother" #\Linefeed))
                     (list :id (format nil "id~Cother" #\Return))
                     (list :id (format nil "id~Cother" #\Linefeed))
                     (list :id (format nil "id~Cother" #\Null))
                     (list :retry 1.5)
                     (list :retry "10")
                     (list :comments 42)
                     (list :comments '("ok" 42))
                     (list :comments (format nil "note~Cother" #\Return))
                     (list :comments (list (format nil "note~Cother" #\Linefeed)))))
    (let ((caught nil))
      (handler-case (apply #'make-http-sse-event case)
        (http-protocol-error (condition) (setf caught condition)))
      (ensure-true (typep caught 'http-protocol-error))
      (ensure-equal :sse (http-error-operation caught))
      (ensure-true (eq (second case) (http-protocol-error-detail caught)))))
  (let ((event (make-http-sse-event)))
    (ensure-equal "message" (http-sse-event-event event))
    (ensure-equal "" (http-sse-event-data event))
    (ensure-false (http-sse-event-id event))
    (ensure-false (http-sse-event-retry event))
    (ensure-false (http-sse-event-comments event))))

(deftest client-sse-limits-reject-invalid-values-before-reading
  (dolist (limit '(:max-events :max-line-bytes :max-data-bytes :max-event-bytes))
    (dolist (value '(-1 1.5 "10"))
      (dolist (operation '(:parse :read))
        (let ((stream (make-string-input-stream (format nil "data:ok~%~%")))
              (caught nil))
          (unwind-protect
               (progn
                 (handler-case
                     (ecase operation
                       (:parse (apply #'parse-http-sse-events "" (list limit value)))
                       (:read (apply #'read-http-sse-events stream (list limit value))))
                   (http-protocol-error (condition) (setf caught condition)))
                 (ensure-true (typep caught 'http-protocol-error))
                 (ensure-equal :sse (http-error-operation caught))
                 (ensure-true (eq value (http-protocol-error-detail caught)))
                 (ensure-equal #\d (read-char stream)))
            (close stream)))))))

#+sbcl
(deftest client-sse-reader-rejects-non-integer-numeric-bytes
  (dolist (byte '(1/2 1.5 0.0 255.0))
    (let* ((input (vector byte 100))
           (original (copy-seq input))
           (stream (make-instance 'binary-test-stream :input input))
           (caught nil)
           (callbacks 0))
      (unwind-protect
           (progn
             (handler-case
                 (read-http-sse-events
                  stream :on-event (lambda (event)
                                     (declare (ignore event))
                                     (incf callbacks)))
               (type-error (condition) (setf caught condition)))
             (ensure-true (typep caught 'type-error))
             (ensure-equal 'integer (type-error-expected-type caught))
             (ensure-true (eql byte (type-error-datum caught)))
             (ensure-equal 0 callbacks)
             (ensure-equal 100 (read-byte stream))
             (ensure-equal original input))
        (close stream)))))

(deftest client-sse-serializer-normalizes-empty-event-name
  (dolist (operation '(:construct :mutate))
    (let ((event (if (eq operation :construct)
                     (make-http-sse-event :event "" :data "ok")
                     (make-http-sse-event :data "ok"))))
      (when (eq operation :mutate)
        (setf (http-sse-event-event event) ""))
      (let* ((wire (serialize-http-sse-event event))
             (events (parse-http-sse-events wire)))
        (ensure-octets-equal (ascii "data:ok|CRLF||CRLF|") wire)
        (ensure-equal 1 (length events))
        (ensure-equal "message" (http-sse-event-event (first events)))
        (ensure-equal "ok" (http-sse-event-data (first events)))
        (ensure-equal "" (http-sse-event-event event))
        (ensure-equal "ok" (http-sse-event-data event))))))

