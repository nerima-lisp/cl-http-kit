(in-package #:http-kit)

(defun %decimal-string-p (string)
  (and (string/= string "")
       (every (lambda (character)
                (let ((code (char-code character)))
                  (and (<= (char-code #\0) code)
                       (<= code (char-code #\9)))))
              string)))

(defun %parse-decimal (string)
  (unless (%decimal-string-p string)
    (error 'http-protocol-error
           :message "Expected an ASCII decimal integer."
           :operation :integer
           :detail string))
  (parse-integer string))

(defun %decimal-exceeds-limit-p (string limit)
  (when limit
    (let* ((first (or (position-if-not (lambda (character)
                                        (char= character #\0))
                                      string)
                      (length string)))
           (digits (subseq string first))
           (limit-text (princ-to-string limit)))
      (or (> (length digits) (length limit-text))
          (and (= (length digits) (length limit-text))
               (string> digits limit-text))))))

(defun %parse-decimal-limited (string limit &key (operation :integer))
  (when (%decimal-exceeds-limit-p string limit)
    (error 'http-size-limit-exceeded
           :message "The HTTP body limit was exceeded."
           :operation operation
           :limit limit
           :observed string
           :kind :body))
  (%parse-decimal string))

(defun %hex-string-exceeds-limit-p (string limit)
  (when limit
    (let* ((first (or (position-if-not (lambda (character)
                                        (char= character #\0))
                                      string)
                      (length string)))
           (digits (subseq string first))
           (limit-text (format nil "~X" limit)))
      (or (> (length digits) (length limit-text))
          (and (= (length digits) (length limit-text))
               (string> (string-upcase digits) limit-text))))))

(defun %parse-hex-limited (string limit &key (operation :chunk-size))
  (when (%hex-string-exceeds-limit-p string limit)
    (error 'http-size-limit-exceeded
           :message "The HTTP body limit was exceeded."
           :operation operation
           :limit limit
           :observed string
           :kind :body))
  (parse-integer string :radix 16))

(defun %hex-digit (character)
  (let ((code (char-code character)))
    (cond ((and (<= (char-code #\0) code)
                (<= code (char-code #\9)))
           (- code (char-code #\0)))
          ((and (<= (char-code #\A) code)
                (<= code (char-code #\F)))
           (+ 10 (- code (char-code #\A))))
          ((and (<= (char-code #\a) code)
                (<= code (char-code #\f)))
           (+ 10 (- code (char-code #\a))))
          (t
           (error 'http-protocol-error
                  :message "Expected a hexadecimal digit."
                  :operation :chunk-size
                  :detail character)))))
