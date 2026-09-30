(in-package #:http-kit)

(defstruct (http-uri
            (:constructor %make-http-uri)
            (:conc-name %http-uri-))
  scheme
  authority
  host
  port
  path
  query)

(defun http-uri-scheme (uri)
  (copy-seq (%http-uri-scheme uri)))

(defun http-uri-authority (uri)
  (copy-seq (%http-uri-authority uri)))

(defun http-uri-host (uri)
  (copy-seq (%http-uri-host uri)))

(defun http-uri-port (uri)
  (%http-uri-port uri))

(defun http-uri-path (uri)
  (copy-seq (%http-uri-path uri)))

(defsetf http-uri-path (uri) (value)
  `(setf (%http-uri-path ,uri) ,value))

(defun http-uri-query (uri)
  (let ((query (%http-uri-query uri)))
    (and query (copy-seq query))))

(defun %uri-error (input detail)
  (error 'http-invalid-uri
         :message (format nil "Invalid HTTP URI (~A)." detail)
         :operation :uri
         :detail detail
         :input (%bounded-diagnostic input)))

(defun %uri-pchar-character-p (character)
  (or (alphanumericp character)
      (find character "-._~!$&'()*+,;=:@%" :test #'char=)))

(defun %validate-uri-component (component input kind)
  (loop for index from 0 below (length component)
        for character = (char component index)
        for code = (char-code character)
        do (when (or (< code #x20)
                     (> code #x7e)
                     (char= character #\Space))
             (%uri-error input (list kind :control-or-space index)))
           (when (char= character #\%)
             (unless (and (< (+ index 2) (length component))
                          (%hex-character-p (char component (1+ index)))
                          (%hex-character-p (char component (+ index 2))))
               (%uri-error input (list kind :bad-percent-escape index))))
           (unless (case kind
                     (:path
                      (or (char= character #\/)
                          (%uri-pchar-character-p character)))
                     (:query
                      (or (find character "/?" :test #'char=)
                          (%uri-pchar-character-p character)))
                     (otherwise t))
             (%uri-error input (list kind :invalid-character index)))))

(defun %hex-character-p (character)
  (or (and (char>= character #\0) (char<= character #\9))
      (and (char>= character #\A) (char<= character #\F))
      (and (char>= character #\a) (char<= character #\f))))

(defun %uri-reg-name-character-p (character)
  (or (alphanumericp character)
      (find character "-._~!$&'()*+,;=%" :test #'char=)))

(defun %uri-split (string delimiter)
  (loop with start = 0
        for end = (position delimiter string :start start)
        collect (subseq string start end)
        while end
        do (setf start (1+ end))))

(defun %uri-ipv4-address-p (string)
  (let ((parts (%uri-split string #\.)))
    (and (= (length parts) 4)
         (every (lambda (part)
                  (and (%decimal-string-p part)
                       (or (= (length part) 1)
                           (char/= (char part 0) #\0))
                       (<= (%parse-decimal part) 255)))
                parts))))

(defun %uri-ipv6-parts-units (parts final-p)
  (loop for part in parts
        for index from 0
        for ipv4-p = (find #\. part)
        unless (and (plusp (length part))
                    (if ipv4-p
                        (and final-p
                             (= index (1- (length parts)))
                             (%uri-ipv4-address-p part))
                        (and (<= (length part) 4)
                             (every #'%hex-character-p part))))
          do (return-from %uri-ipv6-parts-units nil)
        sum (if ipv4-p 2 1)))

(defun %uri-ipv6-address-p (string)
  (let ((compression (search "::" string)))
    (if compression
        (and (not (search "::" string :start2 (+ compression 2)))
             (let* ((left-string (subseq string 0 compression))
                    (right-string (subseq string (+ compression 2)))
                    (left (if (zerop (length left-string))
                              nil
                              (%uri-split left-string #\:)))
                    (right (if (zerop (length right-string))
                               nil
                               (%uri-split right-string #\:)))
                    (left-units (%uri-ipv6-parts-units left nil))
                    (right-units (%uri-ipv6-parts-units right t)))
               (and left-units right-units
                    (< (+ left-units right-units) 8))))
        (let* ((parts (%uri-split string #\:))
               (units (%uri-ipv6-parts-units parts t)))
          (and units (= units 8))))))

(defun %uri-ipvfuture-p (string)
  (and (> (length string) 3)
       (char-equal (char string 0) #\v)
       (let ((dot (position #\. string :start 1)))
         (and dot
              (> dot 1)
              (< dot (1- (length string)))
              (every #'%hex-character-p (subseq string 1 dot))
              (every (lambda (character)
                       (or (alphanumericp character)
                           (find character "-._~!$&'()*+,;=:"
                                 :test #'char=)))
                     (subseq string (1+ dot)))))))

(defun %uri-ip-literal-p (string)
  (or (%uri-ipv6-address-p string)
      (%uri-ipvfuture-p string)))

(defun %authority-parts (authority input)
  (when (or (string= authority "") (find #\@ authority))
    (%uri-error input :authority))
  (let (host port)
    (if (char= (char authority 0) #\[)
        (let ((close (position #\] authority)))
          (unless close (%uri-error input :ipv6-brackets))
          (setf host (subseq authority 1 close))
          (let ((suffix (subseq authority (1+ close))))
            (unless (or (string= suffix "") (char= (char suffix 0) #\:))
              (%uri-error input :authority-suffix))
            (when (and (= (length suffix) 1) (char= (char suffix 0) #\:))
              (%uri-error input :port))
            (when (> (length suffix) 1)
              (unless (%decimal-string-p (subseq suffix 1))
                (%uri-error input :port))
              (setf port (%parse-decimal (subseq suffix 1))))))
        (let ((colon (position #\: authority :from-end t)))
          (when (and colon (position #\: authority :end colon))
            (%uri-error input :unbracketed-ipv6))
          (if colon
              (progn
                (setf host (subseq authority 0 colon))
                (unless (%decimal-string-p (subseq authority (1+ colon)))
                  (%uri-error input :port))
                (setf port (%parse-decimal (subseq authority (1+ colon)))))
              (setf host authority))))
    (when (or (string= host "")
              (find-if (lambda (character)
                         (or (< (char-code character) #x21)
                             (= (char-code character) #x7f)
                             (find character "/?#[]" :test #'char=)))
                       host))
      (%uri-error input :host))
    (%validate-uri-component host input :host)
    (if (char= (char authority 0) #\[)
        (unless (%uri-ip-literal-p host)
          (%uri-error input :ip-literal))
        (unless (every #'%uri-reg-name-character-p host)
          (%uri-error input :host-character)))
    (when (and port (> port 65535))
      (%uri-error input :port-range))
    (values (string-downcase host) port)))

(defun %format-uri-authority (host port ipv6-p)
  (let ((authority (if ipv6-p (format nil "[~A]" host) host)))
    (if port
        (format nil "~A:~D" authority port)
        authority)))

(defun make-http-uri (&key (scheme "http") authority (path "/") query)
  (let* ((normalized-scheme (string-downcase (string scheme)))
         (scheme-input (string scheme)))
    (unless (member normalized-scheme '("http" "https") :test #'string=)
      (%uri-error scheme-input :scheme))
    (unless (stringp authority)
      (%uri-error authority :authority))
    (multiple-value-bind (host port) (%authority-parts authority authority)
      (let ((ipv6-p (char= (char authority 0) #\[)))
        (unless (and (stringp path) (string/= path "") (char= (char path 0) #\/))
          (%uri-error path :path))
        (when (and query (not (stringp query)))
          (%uri-error query :query))
        (when (find #\# path)
          (%uri-error path :fragment))
        (when (find #\? path)
          (%uri-error path :query-delimiter))
        (when (and query (find #\# query))
          (%uri-error query :fragment))
        (%validate-uri-component path path :path)
        (when query (%validate-uri-component query query :query))
        (%make-http-uri :scheme normalized-scheme
                        :authority (%format-uri-authority host port ipv6-p)
                        :host host
                        :port port
                        :path (copy-seq path)
                        :query (and query (copy-seq query)))))))

(defun parse-http-uri (input)
  (unless (stringp input) (%uri-error input :type))
  (let ((scheme-end (search "://" input)))
    (unless scheme-end (%uri-error input :scheme))
    (let* ((scheme (subseq input 0 scheme-end))
           (rest (subseq input (+ scheme-end 3)))
           (fragment (position #\# rest)))
      (when fragment (%uri-error input :fragment))
      (let* ((query-position (position #\? rest))
             (without-query (if query-position
                                (subseq rest 0 query-position)
                                rest))
             (query (and query-position (subseq rest (1+ query-position))))
             (path-position (position #\/ without-query))
             (authority (if path-position
                            (subseq without-query 0 path-position)
                            without-query))
             (path (if path-position
                       (subseq without-query path-position)
                       "/")))
        (make-http-uri :scheme scheme :authority authority :path path :query query)))))

(defun http-uri-string (uri)
  (check-type uri http-uri)
  (if (http-uri-query uri)
      (format nil "~A://~A~A?~A"
              (http-uri-scheme uri)
              (http-uri-authority uri)
              (http-uri-path uri)
              (http-uri-query uri))
      (format nil "~A://~A~A"
              (http-uri-scheme uri)
              (http-uri-authority uri)
              (http-uri-path uri))))

(define-http-diagnostic-printer (http-uri uri stream)
  (:string (http-uri-scheme uri))
  (:string "://")
  (:string (http-uri-authority uri))
  (:string (http-uri-path uri))
  (:string (if (http-uri-query uri) "?<redacted>" "")))
