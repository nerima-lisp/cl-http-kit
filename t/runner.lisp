(in-package #:http-kit/test)

(defun run-tests ()
  (unless (run-all :reporter :spec :pass-with-no-tests nil)
    (error "cl-http-kit tests failed."))
  t)
