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
  (let ((segments (rest (%path-segments path)))
        (stack nil))
    (loop for remaining on segments
          for segment = (first remaining)
          for finalp = (null (rest remaining))
          do (cond ((string= segment ".")
                    (when finalp (push "" stack)))
                   ((string= segment "..")
                    (when stack (pop stack))
                    (when finalp (push "" stack)))
                   (t (push segment stack))))
    (format nil "/~{~A~^/~}" (nreverse stack))))

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
              (path (cond ((string= reference-path "")
                           (http-uri-path base))
                          ((char= (char reference-path 0) #\/) reference-path)
                          (t (%merge-relative-path
                              (http-uri-path base)
                              reference-path))))
              (query (if has-query-p
                         reference-query
                         (if (string= reference-path "")
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

(defun make-http-alternative-service-store
    (&key (clock-function #'get-universal-time))
  (%ensure-function clock-function
                    "The alternative service clock must be a function.")
  (%make-http-alternative-service-store :clock-function clock-function))

(defun %alt-svc-split (string delimiter)
  (let ((parts nil)
        (start 0)
        (quoted-p nil)
        (escaped-p nil))
    (loop for index below (length string)
          for character = (char string index)
          do (cond (escaped-p (setf escaped-p nil))
                   ((and quoted-p (char= character #\\))
                    (setf escaped-p t))
                   ((char= character #\")
                    (setf quoted-p (not quoted-p)))
                   ((and (not quoted-p) (char= character delimiter))
                    (push (string-trim '(#\Space #\Tab)
                                       (subseq string start index))
                          parts)
                    (setf start (1+ index)))))
    (unless (or quoted-p escaped-p)
      (push (string-trim '(#\Space #\Tab) (subseq string start)) parts)
      (nreverse parts))))

(defun %alt-svc-unquote (string)
  (when (and (>= (length string) 2)
             (char= (char string 0) #\")
             (char= (char string (1- (length string))) #\"))
    (with-output-to-string (output)
      (loop with escaped-p = nil
            for index from 1 below (1- (length string))
            for character = (char string index)
            do (cond (escaped-p
                      (write-char character output)
                      (setf escaped-p nil))
                     ((char= character #\\) (setf escaped-p t))
                     (t (write-char character output)))
            finally (when escaped-p (return-from %alt-svc-unquote nil))))))

(defun %alt-svc-protocol-id-p (string)
  (and (plusp (length string))
       (loop with index = 0
             while (< index (length string))
             for character = (char string index)
             do (if (char= character #\%)
                    (unless
                        (and (< (+ index 2) (length string))
                             (find (char string (1+ index))
                                   "0123456789ABCDEF")
                             (find (char string (+ index 2))
                                   "0123456789ABCDEF")
                             (let* ((octet (parse-integer
                                            string :start (1+ index)
                                            :end (+ index 3) :radix 16))
                                    (decoded (and (< octet 128)
                                                  (code-char octet))))
                               (or (= octet #x25)
                                   (null decoded)
                                   (not (http-kit::%ascii-name-char-p
                                         decoded)))))
                      (return nil))
                    (unless (http-kit::%ascii-name-char-p character)
                      (return nil)))
                (incf index (if (char= character #\%) 3 1))
             finally (return t))))

(defun %ensure-alternative-service-store (store)
  (unless (http-alternative-service-store-p store)
    (%client-protocol-error
     "The alternative service store must be an HTTP-ALTERNATIVE-SERVICE-STORE."
     store)))

(defun %alt-svc-parameter-value (text)
  (if (and (plusp (length text)) (char= (char text 0) #\"))
      (%alt-svc-unquote text)
      (and (http-kit::%token-p text) text)))

(defun %parse-alt-svc-authority (authority origin-uri)
  (handler-case
      (let* ((authority (if (and (plusp (length authority))
                                 (char= (char authority 0) #\:))
                            (format nil "~A~A"
                                    (let ((host (http-uri-host origin-uri)))
                                      (if (find #\: host)
                                          (format nil "[~A]" host)
                                          host))
                                    authority)
                            authority))
             (uri (parse-http-uri (format nil "https://~A/" authority))))
        (when (and (http-uri-port uri)
                   (plusp (http-uri-port uri))
                   (<= (http-uri-port uri) 65535)
                   (string= authority (http-uri-authority uri)))
          (values (http-uri-host uri) (http-uri-port uri))))
    (http-error () nil)
    (error () nil)))

(defun %parse-alt-svc-value (text origin-uri origin now age)
  (let* ((parts (%alt-svc-split text #\;))
         (alternative (and parts (first parts)))
         (equals (and alternative (position #\= alternative))))
    (when (and equals (plusp equals))
      (let* ((protocol-id (subseq alternative 0 equals))
             (authority (%alt-svc-unquote (subseq alternative (1+ equals))))
             (max-age 86400)
             (persist-p nil)
             (valid-p (and authority (%alt-svc-protocol-id-p protocol-id))))
        (dolist (parameter (rest parts))
          (let ((separator (position #\= parameter)))
            (when (and separator (plusp separator))
              (let ((name (subseq parameter 0 separator))
                    (value (%alt-svc-parameter-value
                            (subseq parameter (1+ separator)))))
                (cond ((string-equal name "ma")
                       (let ((parsed (and value
                                          (%client-parse-integer
                                           value :allow-sign-p nil))))
                         (if parsed
                             (setf max-age parsed)
                             (setf valid-p nil))))
                      ((and (string-equal name "persist")
                            (string= value "1"))
                       (setf persist-p t)))))))
        (when valid-p
          (multiple-value-bind (host port)
              (%parse-alt-svc-authority authority origin-uri)
            (when host
              (%make-http-alternative-service
               :origin origin
               :protocol-id protocol-id
               :host host
               :port port
               :expires-at (+ now (max 0 (- max-age age)))
               :persist-p persist-p))))))))

(defun %alt-svc-prune (store)
  (let ((now (funcall (%http-alternative-service-store-clock-function store))))
    (setf (%http-alternative-service-store-entries store)
          (loop for (origin . services)
                  in (%http-alternative-service-store-entries store)
                for fresh = (remove-if
                             (lambda (service)
                               (<= (http-alternative-service-expires-at service)
                                   now))
                             services)
                when fresh collect (cons origin fresh)))))

(defun http-alternative-service-store-services (store uri)
  (%ensure-alternative-service-store store)
  (%alt-svc-prune store)
  (copy-list (cdr (assoc (http-uri-origin uri)
                         (%http-alternative-service-store-entries store)
                         :test #'string=))))

(defun http-alternative-service-store-note-response (store uri response)
  (unless (and (http-alternative-service-store-p store)
               (http-response-p response))
    (%client-protocol-error "Invalid alternative service response context."
                            (list store uri response)))
  (unless (= (http-response-status response) 421)
    (let ((values (http-header-values (http-response-headers response) "Alt-Svc")))
      (when values
        (let* ((uri (%client-uri uri))
               (origin (http-uri-origin uri))
               (elements (loop for value in values
                               append (or (%alt-svc-split value #\,) nil)))
               (clear-p (member "clear" elements :test #'string=))
               (age-text (http-header-value
                          (http-response-headers response) "Age"))
               (age (or (and age-text
                             (%client-parse-integer age-text :allow-sign-p nil))
                        0))
               (now (funcall
                     (%http-alternative-service-store-clock-function store)))
               (services (unless clear-p
                           (remove nil
                                   (mapcar (lambda (element)
                                             (%parse-alt-svc-value
                                              element uri origin now age))
                                           elements)))))
          (let ((remaining
                  (remove origin
                          (%http-alternative-service-store-entries store)
                          :key #'car :test #'string=)))
            (setf (%http-alternative-service-store-entries store)
                  (if services
                      (acons origin services remaining)
                      remaining)))))))
  store)

(defun http-alternative-service-store-remove (store uri service)
  (%ensure-alternative-service-store store)
  (unless (http-alternative-service-p service)
    (%client-protocol-error
     "The alternative service must be an HTTP-ALTERNATIVE-SERVICE."
     service))
  (let* ((origin (http-uri-origin uri))
         (entry (assoc origin (%http-alternative-service-store-entries store)
                       :test #'string=)))
    (when entry
      (setf (cdr entry) (remove service (cdr entry) :test #'eq))
      (when (null (cdr entry))
        (setf (%http-alternative-service-store-entries store)
              (delete entry (%http-alternative-service-store-entries store)
                      :test #'eq)))))
  store)

(defun http-alternative-service-store-network-changed (store)
  (%ensure-alternative-service-store store)
  (%alt-svc-prune store)
  (setf (%http-alternative-service-store-entries store)
        (loop for (origin . services)
                in (%http-alternative-service-store-entries store)
              for persistent = (remove-if-not
                                #'http-alternative-service-persist-p services)
              when persistent collect (cons origin persistent)))
  store)

(defun http-alternative-service-store-clear (store &optional uri)
  (%ensure-alternative-service-store store)
  (if uri
      (let ((origin (http-uri-origin uri)))
        (setf (%http-alternative-service-store-entries store)
              (remove origin (%http-alternative-service-store-entries store)
                      :key #'car :test #'string=)))
      (setf (%http-alternative-service-store-entries store) nil))
  store)
