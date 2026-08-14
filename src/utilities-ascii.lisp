(in-package #:http-kit)

(defun %ascii-lowercase (string)
  (string-downcase string))

(defun %ascii-name-char-p (character)
  (let ((code (char-code character)))
    (and (<= code #x7f)
         (or (char= character #\')
             (char= character #\`)
             (not
              (null
               (find character
                     "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!#$%&*+-.^_|~"
                     :test #'char=)))))))

(defun %token-p (string)
  (and (string/= string "")
       (every #'%ascii-name-char-p string)))

(defun %header-name-p (string)
  (%token-p string))

(defun %header-value-p (string)
  (every (lambda (character)
           (let ((code (char-code character)))
             (or (= code #x09)
                 (<= #x20 code #x7e)
                 (<= #x80 code #xff))))
         string))

(defun %trim-ows (string)
  (string-trim '(#\Space #\Tab) string))
