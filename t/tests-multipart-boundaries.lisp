(in-package #:http-kit/test)

(deftest client-multipart-quoted-parameter-escape-boundaries
  (let* ((disposition "form-data; name=\"a\\\"b\\\\c;d\"; filename=\"f\\\"g\\\\h\"")
         (wire (ascii (format nil "--b|CRLF|Content-Disposition: ~A|CRLF||CRLF|value|CRLF|--b--|CRLF|"
                              disposition)))
         (original (copy-seq wire))
         (parts (parse-http-multipart-body wire :boundary "b")))
    (ensure-equal 1 (length parts))
    (ensure-equal "a\"b\\c;d" (http-multipart-part-name (first parts)))
    (ensure-equal "f\"g\\h" (http-multipart-part-filename (first parts)))
    (ensure-octets-equal (ascii "value") (http-multipart-part-value (first parts)))
    (ensure-octets-equal original wire))
  (dolist (disposition '("form-data; name=\"a\\\"" "form-data; name=\"a\\"))
    (let* ((wire (ascii (format nil "--b|CRLF|Content-Disposition: ~A|CRLF||CRLF|value|CRLF|--b--|CRLF|"
                               disposition)))
           (original (copy-seq wire))
           (caught (handler-case (parse-http-multipart-body wire :boundary "b")
                     (http-protocol-error (condition) condition))))
      (ensure-true (typep caught 'http-protocol-error))
      (ensure-equal :client (http-error-operation caught))
      (ensure-equal disposition (http-protocol-error-detail caught))
      (ensure-true (search "unterminated quoted value" (http-error-message caught)))
      (ensure-octets-equal original wire))))

(deftest client-multipart-revalidates-mutated-header-types
  (dolist (field '(:name :filename :content-type))
    (dolist (value (if (eq field :name)
                      (list nil 42 #() 'invalid)
                      (list 42 #() 'invalid)))
      (let ((part (make-http-multipart-part :name "field" :value "unchanged"))
            (caught nil))
        (ecase field
          (:name (setf (http-multipart-part-name part) value))
          (:filename (setf (http-multipart-part-filename part) value))
          (:content-type (setf (http-multipart-part-content-type part) value)))
        (handler-case (make-http-multipart-body (list part) :boundary "b")
          (http-protocol-error (condition) (setf caught condition)))
        (ensure-true (typep caught 'http-protocol-error))
        (ensure-equal :client (http-error-operation caught))
        (ensure-true (eq value (http-protocol-error-detail caught)))
        (ensure-equal "unchanged" (http-multipart-part-value part))
        (ensure-true
         (eq value (ecase field
                     (:name (http-multipart-part-name part))
                     (:filename (http-multipart-part-filename part))
                     (:content-type (http-multipart-part-content-type part))))))))
  (let ((part (make-http-multipart-part :name "field" :value "ok"
                                        :filename "note.txt"
                                        :content-type "text/plain")))
    (multiple-value-bind (wire content-type)
        (make-http-multipart-body (list part) :boundary "b")
      (let ((parsed (parse-http-multipart-body wire :content-type content-type)))
        (ensure-equal 1 (length parsed))
        (ensure-equal "field" (http-multipart-part-name (first parsed)))
        (ensure-octets-equal (ascii "ok") (http-multipart-part-value (first parsed)))
        (ensure-equal "note.txt" (http-multipart-part-filename (first parsed)))
        (ensure-equal "text/plain" (http-multipart-part-content-type (first parsed)))))))

(deftest client-multipart-accepts-final-delimiter-at-eof
  (dolist (padding (list "" (format nil " ~C" #\Tab)))
    (dolist (empty-p '(t nil))
      (let* ((wire (ascii
                    (concatenate
                     'string
                     (if empty-p ""
                         "--b|CRLF|Content-Disposition: form-data; name=\"field\"|CRLF||CRLF|payload|CRLF|")
                     "--b--" padding)))
             (original (copy-seq wire))
             (parts (parse-http-multipart-body wire :boundary "b"
                                               :max-input-bytes (length wire))))
        (ensure-equal (if empty-p 0 1) (length parts))
        (ensure-octets-equal original wire)
        (unless empty-p
          (ensure-equal "field" (http-multipart-part-name (first parts)))
          (ensure-octets-equal (ascii "payload") (http-multipart-part-value (first parts)))
          (ensure-false (http-multipart-part-filename (first parts)))
          (ensure-false (http-multipart-part-content-type (first parts)))))))
  (dolist (text '("--b" "--b "))
    (let* ((wire (ascii text)) (original (copy-seq wire)) (caught nil))
      (handler-case (parse-http-multipart-body wire :boundary "b")
        (http-protocol-error (condition) (setf caught condition)))
      (ensure-true (typep caught 'http-protocol-error))
      (ensure-equal :client (http-error-operation caught))
      (ensure-equal "b" (http-protocol-error-detail caught))
      (ensure-true (search "opening boundary" (http-error-message caught)))
      (ensure-octets-equal original wire))))

(deftest client-multipart-ignores-markers-without-line-boundary
  (dolist (prefix (list "x" "xx" (format nil "x~C" #\Return)))
    (let* ((wire (ascii (concatenate 'string prefix "--b--")))
           (original (copy-seq wire))
           (caught nil))
      (handler-case (parse-http-multipart-body wire :boundary "b")
        (http-protocol-error (condition) (setf caught condition)))
      (ensure-true (typep caught 'http-protocol-error))
      (ensure-equal :client (http-error-operation caught))
      (ensure-equal "b" (http-protocol-error-detail caught))
      (ensure-true (search "opening boundary" (http-error-message caught)))
      (ensure-octets-equal original wire))
    (let* ((wire (ascii (concatenate 'string prefix "--b--|CRLF|--b|CRLF|Content-Disposition: form-data; name=\"field\"|CRLF||CRLF|ok|CRLF|--b--")))
           (original (copy-seq wire))
           (parts (parse-http-multipart-body wire :boundary "b")))
      (ensure-equal 1 (length parts))
      (ensure-equal "field" (http-multipart-part-name (first parts)))
      (ensure-octets-equal (ascii "ok") (http-multipart-part-value (first parts)))
      (ensure-octets-equal original wire))))

(deftest client-multipart-generated-boundary-collision-outcomes
  ;; Control only candidate generation; collision detection uses the public body.
  (let* ((symbol 'http-kit/client::%generated-boundary)
         (original (fdefinition symbol))
         (candidate (funcall original))
         (payload (format nil "--~A--" candidate))
         (part (make-http-multipart-part :name "field" :value payload)))
    (dolist (exhaust-p '(t nil))
      (let ((calls 0) (caught nil))
        (unwind-protect
             (progn
               (setf (fdefinition symbol)
                     (lambda ()
                       (incf calls)
                       (if (or exhaust-p (= calls 1)) candidate "safe")))
               (handler-case
                   (multiple-value-bind (wire content-type)
                       (make-http-multipart-body (list part))
                     (ensure-false exhaust-p)
                     (ensure-equal "multipart/form-data; boundary=\"safe\"" content-type)
                     (let ((parts (parse-http-multipart-body wire :content-type content-type)))
                       (ensure-equal 1 (length parts))
                       (ensure-octets-equal (ascii payload) (http-multipart-part-value (first parts)))))
                 (http-protocol-error (condition) (setf caught condition))))
          (setf (fdefinition symbol) original))
        (ensure-true (eq original (fdefinition symbol)))
        (ensure-equal (if exhaust-p 100 2) calls)
        (ensure-equal payload (http-multipart-part-value part))
        (if exhaust-p
            (progn
              (ensure-true (typep caught 'http-protocol-error))
              (ensure-equal :client (http-error-operation caught))
              (ensure-false (http-protocol-error-detail caught))
              (ensure-true (search "collision-free" (http-error-message caught))))
            (ensure-false caught))))))

(deftest client-multipart-default-input-limit-is-64-mib
  (let* ((symbol 'http-kit/client::%copy-client-octets)
         (original (fdefinition symbol))
         (small (ascii "--b--|CRLF|"))
         (large (make-array 67108865 :element-type '(unsigned-byte 8)
                                    :initial-element 0))
         (calls 0)
         (caught nil))
    ;; The remaining bytes are epilogue after a real final delimiter.
    (replace large small)
    (ensure-equal 67108865 (length large))
    (ensure-true (typep large '(simple-array (unsigned-byte 8) (*))))
    (unwind-protect
         (progn
           (setf (fdefinition symbol)
                 (lambda (body)
                   (incf calls)
                   (funcall original body)))
           (ensure-false (parse-http-multipart-body small :boundary "b"))
           (ensure-equal 0 calls)
           (handler-case (parse-http-multipart-body large :boundary "b")
             (http-size-limit-exceeded (condition) (setf caught condition)))
           (ensure-true (typep caught 'http-size-limit-exceeded))
           (ensure-equal :multipart (http-error-operation caught))
           (ensure-equal 67108864
                         (getf (http-protocol-error-detail caught) :limit))
           (ensure-equal 67108865
                         (getf (http-protocol-error-detail caught) :observed))
           (ensure-equal 0 calls)
           (ensure-false (parse-http-multipart-body large :boundary "b"
                                                         :max-input-bytes nil))
           (ensure-equal 0 calls))
      (setf (fdefinition symbol) original))
    (ensure-true (eq original (fdefinition symbol)))))
