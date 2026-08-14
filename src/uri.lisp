(in-package #:http-kit)

(defstruct (http-uri (:constructor %make-http-uri))
  scheme
  authority
  host
  port
  path
  query)

(defun %uri-error (input detail)
  (error 'http-invalid-uri
         :message (format nil "Invalid HTTP URI (~A)." detail)
         :operation :uri
         :detail detail
         :input (%bounded-diagnostic input)))

(defun %hex-character-p (character)
  (or (and (char>= character #\0) (char<= character #\9))
      (and (char>= character #\A) (char<= character #\F))
      (and (char>= character #\a) (char<= character #\f))))

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
               (%uri-error input (list kind :bad-percent-escape index))))))

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
                        :path path
                        :query query)))))

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
