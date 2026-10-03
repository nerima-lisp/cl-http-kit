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
