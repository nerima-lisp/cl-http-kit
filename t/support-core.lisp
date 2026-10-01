(in-package #:http-kit/test-core)

(defmacro deftest (name &body body)
  `(it ,(string-downcase (symbol-name name)) ,@body))

(defmacro ensure-true (condition &optional control &rest arguments)
  (declare (ignore control arguments))
  `(expect ,condition))

(defmacro ensure-false (condition &optional control &rest arguments)
  (declare (ignore control arguments))
  `(expect (not ,condition)))

(defmacro ensure-equal (expected actual &optional description)
  (declare (ignore description))
  `(expect ,actual :to-equalp ,expected)) ; paredit:ignore macro-parameter-reordering -- cl-weave EXPECT takes the actual expression before the expected matcher value.

(defmacro ensure-octets-equal (expected actual &optional description)
  (declare (ignore description))
  `(ensure-equal ,expected ,actual))

(defmacro ensure-summary= (expected form)
  "Assert that FORM produces exactly the human-facing summary EXPECTED."
  `(ensure-equal ,expected ,form))

(defmacro ensure-summary-contains (form &rest substrings)
  "Assert that FORM contains every expected summary fragment."
  (let ((summary (gensym "SUMMARY-")))
    `(let ((,summary ,form))
       ,@(mapcar (lambda (substring)
                   `(ensure-true (search ,substring ,summary)))
                 substrings))))

(defmacro ensure-printed= (expected form)
  "Assert that FORM prints exactly as EXPECTED with PRINC-TO-STRING."
  `(ensure-equal ,expected (princ-to-string ,form)))

(defmacro ensure-printed-contains (form &rest substrings)
  "Assert that FORM prints a string containing every expected fragment."
  (let ((printed (gensym "PRINTED-"))
        (expected-fragments (gensym "EXPECTED-FRAGMENTS-")))
    `(let ((,printed (princ-to-string ,form))
           (,expected-fragments (list ,@substrings)))
       (dolist (substring ,expected-fragments)
         (ensure-true (search substring ,printed))))))

(defmacro ensure-conversion-cases (converter &body cases)
  "Assert that CONVERTER maps each input to the expected output."
  `(progn
     ,@(mapcar (lambda (case)
                 `(ensure-equal ,(second case)
                                (,converter ,(first case))))
               cases)))

(defmacro ensure-serialization-cases (serializer &body cases)
  "Assert that SERIALIZER produces the expected wire octets for each case."
  `(progn
     ,@(mapcar (lambda (case)
                 `(ensure-equal ,(first case)
                                (,serializer ,(second case) ,@(cddr case))))
               cases)))

(defmacro ensure-signals-cases (condition &body forms)
  "Assert that every form signals CONDITION."
  `(progn
     ,@(mapcar (lambda (form)
                 `(signals ,condition
                    ,form))
               forms)))

(defun octets (&rest values)
  (let ((result (make-array (length values)
                            :element-type '(unsigned-byte 8))))
    (loop for value in values
          for index from 0
          do (ensure-true (and (integerp value) (<= 0 value #xff))
                          "Test octet is invalid: ~S." value)
             (setf (aref result index) value))
    result))

(defun ascii (string)
  (let ((result (make-array 0
                            :element-type '(unsigned-byte 8)
                            :adjustable t
                            :fill-pointer 0)))
    (loop with index = 0
          while (< index (length string))
          do (if (and (<= (+ index 6) (length string))
                      (string= "|CRLF|" string
                               :start1 0
                               :end1 6
                               :start2 index
                               :end2 (+ index 6)))
                 (progn
                   (vector-push-extend 13 result)
                   (vector-push-extend 10 result)
                   (incf index 6))
                 (progn
                   (vector-push-extend
                    (char-code (char string index))
                    result)
                   (incf index))))
    (let ((copy (make-array (length result)
                            :element-type '(unsigned-byte 8))))
      (replace copy result)
      copy)))

(defun concatenate-octets (&rest vectors)
  (let ((result (make-array (reduce #'+ vectors :key #'length :initial-value 0)
                            :element-type '(unsigned-byte 8)))
        (position 0))
    (dolist (vector vectors result)
      (replace result vector :start1 position)
      (incf position (length vector)))))

(defun octets-as-string (vector)
  (map 'string #'code-char vector))
