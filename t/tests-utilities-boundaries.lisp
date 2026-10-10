(in-package #:http-kit/test-core)

(deftest utilities-priority-value-contract
  (dolist (urgency '(nil :invalid "3" 3.0 1/2 -1 8))
    (let ((condition
            (handler-case (format-http-priority-field-value :urgency urgency)
              (http-protocol-error (condition) condition))))
      (ensure-true (typep condition 'http-protocol-error))
      (ensure-equal :http-priority (http-error-operation condition))
      (ensure-equal urgency (http-protocol-error-detail condition))))
  (dolist (incremental '(0 "true" :true))
    (let ((condition
            (handler-case (format-http-priority-field-value :incremental incremental)
              (http-protocol-error (condition) condition))))
      (ensure-true (typep condition 'http-protocol-error))
      (ensure-equal :http-priority (http-error-operation condition))
      (ensure-equal incremental (http-protocol-error-detail condition))))
  (loop for urgency from 0 to 7
        do (ensure-equal (format nil "u=~D" urgency)
                         (format-http-priority-field-value :urgency urgency))
           (ensure-equal (format nil "u=~D, i" urgency)
                         (format-http-priority-field-value :urgency urgency :incremental t))))

(deftest utilities-chunk-extension-quoted-character-contract
  (dolist (escaped-p '(nil t))
    (dolist (code '(0 8 10 13 31 127 256))
      (let ((line (concatenate 'string "a;x=\""
                               (if escaped-p "\\" "")
                               (string (code-char code)) "\"")))
        (ensure-equal nil (http-kit::%http1-chunk-size-text line))))
    (dolist (code '(9 32 33 35 91 93 126 128 255))
      (let ((line (concatenate 'string "a;x=\""
                               (if escaped-p "\\" "")
                               (string (code-char code)) "\"")))
        (ensure-equal "a" (http-kit::%http1-chunk-size-text line)))))
  (dolist (code '(34 92))
    (ensure-equal "a"
                  (http-kit::%http1-chunk-size-text
                   (concatenate 'string "a;x=\"\\" (string (code-char code)) "\"")))))

(deftest utilities-conversion-boundaries
  (ensure-conversion-cases http-kit::%string-octets
    ("AB" (octets 65 66)))
  (ensure-signals-cases http-protocol-error
    (http-kit::%string-octets 1)
    (http-kit::%string-octets (string (code-char #x100))))
  (ensure-conversion-cases http-kit::%octets-string
    ((octets 65 66) "AB"))
  (ensure-signals-cases http-protocol-error
    (http-kit::%octets-string 1)
    (http-kit::%octets-string (list 256))
    (http-kit::%octets-string (list :invalid)))
  (ensure-equal 0 (http-kit::%hex-digit #\0))
  (ensure-equal 10 (http-kit::%hex-digit #\A))
  (ensure-equal 15 (http-kit::%hex-digit #\f)))

(deftest utilities-decimal-limit-boundaries
  (dolist (case '(("0" 0 0) ("000" 0 0) ("9" 9 9)
                  ("09" 9 9) ("10" 10 10) ("099" 100 99)
                  ("00100" 100 100) ("12345678901234567890" nil
                                      12345678901234567890)))
    (destructuring-bind (text limit expected) case
      (ensure-equal expected (http-kit::%parse-decimal-limited text limit))))
  (dolist (text '("1" "0001" "10" "99999999999999999999"))
    (let ((caught (handler-case
                      (http-kit::%parse-decimal-limited
                       text 0 :operation :content-length)
                    (http-size-limit-exceeded (condition) condition))))
      (ensure-true (typep caught 'http-size-limit-exceeded))
      (ensure-equal 0 (http-size-limit-exceeded-limit caught))
      (ensure-equal text (http-size-limit-exceeded-observed caught))
      (ensure-equal :body (http-size-limit-exceeded-kind caught))
      (ensure-equal :content-length (http-error-operation caught))))
  (dolist (text '("" "-1" "+1" " 1" "1 " "1.0" "１"))
    (signals http-protocol-error
      (http-kit::%parse-decimal-limited text nil))))

(deftest utilities-hex-limit-boundaries
  (dolist (case '(("0" 0 0) ("000" 0 0) ("a" 10 10)
                  ("A" 10 10) ("000f" 15 15) ("FF" 255 255)
                  ("100" 256 256) ("ff" 256 255)
                  ("123456789abcdef" nil #x123456789abcdef)))
    (destructuring-bind (text limit expected) case
      (ensure-equal expected (http-kit::%parse-hex-limited text limit))))
  (dolist (text '("10" "0010" "ff" "FFFFFFFFFFFFFFFF"))
    (let ((caught (handler-case
                      (http-kit::%parse-hex-limited
                       text 15 :operation :chunk-size)
                    (http-size-limit-exceeded (condition) condition))))
      (ensure-true (typep caught 'http-size-limit-exceeded))
      (ensure-equal 15 (http-size-limit-exceeded-limit caught))
      (ensure-equal text (http-size-limit-exceeded-observed caught))
      (ensure-equal :body (http-size-limit-exceeded-kind caught))
      (ensure-equal :chunk-size (http-error-operation caught)))))

(deftest utilities-diagnostic-printers-redact-sensitive-values
  (ensure-true (search "<redacted>"
                  (princ-to-string
                   (make-http-header "Authorization" "value"))))
  (ensure-true (search "?<redacted>"
                  (princ-to-string
                   (parse-http-uri "https://example.test/a?q=x")))))

(defclass utilities-diagnostic-printer-fixture () ())

(http-kit::define-http-diagnostic-printer
    (utilities-diagnostic-printer-fixture object stream)
  (:char #\[)
  (:princ 42)
  (:string "]"))

(deftest utilities-diagnostic-printer-write-operations
  (let ((printed (with-standard-io-syntax
                   (let ((*print-readably* nil))
                     (princ-to-string
                      (make-instance 'utilities-diagnostic-printer-fixture))))))
    (ensure-true (search "[42]" printed))
    (ensure-false (search "\"[42]\"" printed)))
  (signals error
    (macroexpand-1
     '(http-kit::define-http-diagnostic-printer
          (utilities-diagnostic-printer-fixture object stream)
        (:invalid "value")))))
