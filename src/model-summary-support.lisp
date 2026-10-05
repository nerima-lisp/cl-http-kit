(in-package #:http-kit)

(defmacro define-http-summary (name (object) &body operations)
  "Define a summary function from declarative write operations."
  (labels ((write-form (stream operation)
             (destructuring-bind (kind value) operation
               (ecase kind
                 (:char `(write-char ,value ,stream))
                 (:princ `(princ ,value ,stream))
                 (:string `(write-string ,value ,stream))))))
    (let ((stream (gensym "STREAM-")))
      `(defun ,name (,object)
         (with-output-to-string (,stream)
           ,@(mapcar (lambda (operation)
                       (write-form stream operation))
                     operations))))))

(defmacro define-http-diagnostic-printer ((type object stream) &body operations)
  "Define a PRINT-OBJECT method from declarative write operations."
  (labels ((write-form (stream-name operation)
             (destructuring-bind (kind value) operation
               (ecase kind
                 (:char `(write-char ,value ,stream-name))
                 (:princ `(princ ,value ,stream-name))
                 (:string `(write-string ,value ,stream-name))))))
    `(defmethod print-object ((,object ,type) ,stream)
       (print-unreadable-object (,object ,stream :type t)
         ,@(mapcar (lambda (operation)
                     (write-form stream operation))
                   operations)))))
