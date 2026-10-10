(in-package #:http-kit/test)

(defun run-tests ()
  (unless (run-all :reporter :spec
                   :location-filter
                   (loop for system in '("cl-http-kit/test-core" "cl-http-kit/test")
                         append (mapcar #'asdf:component-pathname
                                        (asdf:component-children
                                         (asdf:find-system system))))
                   :pass-with-no-tests nil)
    (error "cl-http-kit tests failed."))
  t)
