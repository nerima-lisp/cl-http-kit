(in-package #:http-kit/test-core)

(defun run-tests ()
  (unless (run-all :reporter :spec
                   :location-filter
                   (mapcar #'asdf:component-pathname
                           (asdf:component-children
                            (asdf:find-system "cl-http-kit/test-core")))
                   :pass-with-no-tests nil)
    (error "cl-http-kit core tests failed."))
  t)
