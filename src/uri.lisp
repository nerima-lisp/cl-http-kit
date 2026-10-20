(in-package #:http-kit)

(defun %hex-character-p (character)
  (or (and (char>= character #\0) (char<= character #\9))
      (and (char>= character #\A) (char<= character #\F))
      (and (char>= character #\a) (char<= character #\f))))

