(in-package #:http-kit)

(defun %request-target-value-p (target)
  (and (stringp target)
       (plusp (length target))
       (loop for character across target
             for code = (char-code character)
             always (and (>= code #x21)
                         (<= code #x7e)))))

