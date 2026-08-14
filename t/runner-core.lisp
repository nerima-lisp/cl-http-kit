(in-package #:http-kit/test-core)

(defun run-tests ()
  (unless (run-all :reporter :spec :pass-with-no-tests nil)
    (error "cl-http-kit core tests failed."))
  t)
