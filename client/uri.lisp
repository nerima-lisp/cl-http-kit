(in-package #:http-kit/client)

(defun %client-uri (uri)
  (cond ((http-uri-p uri)
         (make-http-uri :scheme (http-uri-scheme uri)
                        :authority (http-uri-authority uri)
                        :path (http-uri-path uri)
                        :query (http-uri-query uri)))
        ((stringp uri) (parse-http-uri uri))
        (t (%client-protocol-error
            "An HTTP URI must be an HTTP-URI value or an absolute URI string."
            uri))))

(defun %client-string-prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string-equal prefix string :end2 (length prefix))))

(defun %strip-uri-fragment (reference)
  (let ((position (position #\# reference)))
    (if position
        (subseq reference 0 position)
        reference)))

(defun %uri-scheme-character-p (character &key firstp)
  (or (and firstp
           (alpha-char-p character))
      (and (not firstp)
           (or (alphanumericp character)
               (find character "+-." :test #'char=)))))

(defun %uri-scheme-prefix-position (reference)
  (let ((colon (position #\: reference)))
    (when (and colon
               (plusp colon)
               (loop for index from 0 below colon
                     always (%uri-scheme-character-p
                             (char reference index)
                             :firstp (zerop index))))
      colon)))

(defun %path-segments (path)
  (let ((start 0)
        (length (length path))
        (segments nil))
    (loop for slash = (position #\/ path :start start)
          do (push (subseq path start (or slash length)) segments)
          while slash
          do (setf start (1+ slash)))
    (nreverse segments)))

(defun %remove-dot-segments (path)
  (let* ((segments (%path-segments path))
         (trailing-slash (and (> (length path) 1)
                              (char= (char path (1- (length path))) #\/)))
         (stack nil))
    (dolist (segment segments)
      (cond ((or (zerop (length segment)) (string= segment ".")) nil)
            ((string= segment "..")
             (when stack (pop stack)))
            (t (push segment stack))))
    (let ((normalized (if stack
                         (format nil "/~{~A~^/~}" (nreverse stack))
                         "/")))
      (if trailing-slash
          (concatenate 'string normalized "/")
          normalized))))

(defun %merge-relative-path (base-path reference-path)
  (let ((slash (position #\/ base-path :from-end t)))
    (if slash
        (concatenate 'string
                     (subseq base-path 0 (1+ slash))
                     reference-path)
        (concatenate 'string "/" reference-path))))

(defun resolve-http-uri (base reference)
  "Resolve an absolute or relative HTTP reference against BASE.

Fragments are deliberately discarded because they are client-side identifiers
and are never sent as part of an HTTP request target.  The result is always a
fresh HTTP-URI value with dot segments removed from its path."
  (let* ((base (%client-uri base))
         (reference (cond ((http-uri-p reference)
                           (http-uri-string reference))
                          ((stringp reference) reference)
                          (t (%client-protocol-error
                              "A URI reference must be an HTTP-URI or string."
                              reference))))
         (reference (%strip-uri-fragment reference))
         (scheme-position (%uri-scheme-prefix-position reference)))
    (cond
      (scheme-position
       (unless (or (string-equal "http"
                                reference
                                :end2 scheme-position)
                   (string-equal "https"
                                 reference
                                 :end2 scheme-position))
         (%client-protocol-error
          "Only http and https URI references are supported."
          reference))
       (parse-http-uri reference))
      ((%client-string-prefix-p "//" reference)
       (parse-http-uri (format nil "~A:~A" (http-uri-scheme base) reference)))
      (t
       (let* ((query-position (position #\? reference))
              (reference-path (if query-position
                                  (subseq reference 0 query-position)
                                  reference))
              (has-query-p (not (null query-position)))
              (reference-query (and has-query-p
                                    (subseq reference (1+ query-position))))
              (path (cond ((zerop (length reference-path))
                           (http-uri-path base))
                          ((char= (char reference-path 0) #\/) reference-path)
                          (t (%merge-relative-path
                              (http-uri-path base)
                              reference-path))))
              (query (if has-query-p
                         reference-query
                         (if (zerop (length reference-path))
                             (http-uri-query base)
                             nil))))
         (make-http-uri :scheme (http-uri-scheme base)
                        :authority (http-uri-authority base)
                        :path (%remove-dot-segments path)
                        :query query))))))

(defun %http-uri-effective-port (uri)
  (or (http-uri-port uri)
      (if (string= (http-uri-scheme uri) "https") 443 80)))

(defun %http-uri-origin-authority (uri)
  (let ((host (http-uri-host uri))
        (port (%http-uri-effective-port uri)))
    (let ((host (if (find #\: host) (format nil "[~A]" host) host))
          (default-port (if (string= (http-uri-scheme uri) "https")
                            443
                            80)))
      (if (and (= port default-port)
               (null (http-uri-port uri)))
          host
          (format nil "~A:~D" host port)))))

(defun http-uri-origin (uri)
  "Return URI's normalized origin as a string.

Default HTTP and HTTPS ports are omitted; explicit default ports therefore
have the same origin as their omitted forms."
  (let ((uri (%client-uri uri)))
    (format nil "~A://~A"
            (http-uri-scheme uri)
            (%http-uri-origin-authority uri))))

(defun http-same-origin-p (left right)
  "Return true when LEFT and RIGHT have the same scheme, host, and effective port."
  (let ((left (%client-uri left))
        (right (%client-uri right)))
    (and (string-equal (http-uri-scheme left) (http-uri-scheme right))
         (string-equal (http-uri-host left) (http-uri-host right))
         (= (%http-uri-effective-port left)
            (%http-uri-effective-port right)))))
