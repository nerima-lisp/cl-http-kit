(in-package #:http-kit/client)

(defun %cookie-trim (string)
  (string-trim '(#\Space #\Tab) string))

(defun %cookie-name-p (name)
  (and (stringp name)
       (not (string= name ""))
       (every (lambda (character)
                (let ((code (char-code character)))
                  (and (<= #x21 code #x7e)
                       (not (find character "()<>@,;:\\\"/[]?={} \t"
                                   :test #'char=)))))
              name)))

(defun %cookie-value-p (value)
  (and (stringp value)
       (not (find-if (lambda (character)
                       (or (char= character #\Return)
                           (char= character #\Linefeed)
                           (char= character #\Semicolon)))
                     value))))

(defun %cookie-prefix-p (name prefix)
  (and (stringp name)
       (>= (length name) (length prefix))
       (string= prefix name :end2 (length prefix))))

(defun %copy-cookie (cookie)
  (%make-http-cookie
   :name (copy-seq (http-cookie-name cookie))
   :value (copy-seq (http-cookie-value cookie))
   :domain (copy-seq (http-cookie-domain cookie))
   :path (copy-seq (http-cookie-path cookie))
   :expires (http-cookie-expires cookie)
   :max-age (http-cookie-max-age cookie)
   :secure-p (http-cookie-secure-p cookie)
   :http-only-p (http-cookie-http-only-p cookie)
   :same-site (http-cookie-same-site cookie)
   :host-only-p (http-cookie-host-only-p cookie)
   :creation-time (http-cookie-creation-time cookie)
   :partitioned-p (http-cookie-partitioned-p cookie)
   :partition-key (and (http-cookie-partition-key cookie)
                       (copy-seq (http-cookie-partition-key cookie)))))

(defun make-http-cookie
    (&key name value domain (path "/") expires max-age secure-p http-only-p
          same-site (host-only-p nil) (creation-time (get-universal-time))
          (partitioned-p nil) partition-key)
  (unless (%cookie-name-p name)
    (%client-protocol-error "Cookie names must be valid cookie tokens." name))
  (unless (%cookie-value-p value)
    (%client-protocol-error "Cookie values cannot contain controls or semicolons."
                            value))
  (unless (and (stringp domain) (not (string= domain "")))
    (%client-protocol-error "Cookies require a non-empty domain." domain))
  (unless (and (stringp path)
               (not (string= path ""))
               (char= (char path 0) #\/))
    (%client-protocol-error "Cookie paths must begin with '/'." path))
  (when (and expires (not (integerp expires)))
    (%client-protocol-error "Cookie expiration must be universal time or NIL."
                            expires))
  (when (and max-age (not (integerp max-age)))
    (%client-protocol-error "Cookie max-age must be an integer or NIL." max-age))
  (unless (integerp creation-time)
    (%client-protocol-error "Cookie creation time must be an integer." creation-time))
  (when (and same-site
             (not (member same-site '(:strict :lax :none)
                         :test #'string-equal)))
    (%client-protocol-error "Cookie SameSite must be Strict, Lax, None, or NIL."
                            same-site))
  (when (and partition-key
             (or (not (stringp partition-key))
                 (string= partition-key "")))
    (%client-protocol-error
     "A partition key must be a non-empty string or NIL."
     partition-key))
  (let* ((secure-p (not (null secure-p)))
         (host-only-p (not (null host-only-p)))
         (partitioned-p (not (null partitioned-p)))
         (secure-prefix-p (%cookie-prefix-p name "__Secure-"))
         (host-prefix-p (%cookie-prefix-p name "__Host-")))
    (when (and (or secure-prefix-p host-prefix-p)
               (not secure-p))
      (%client-protocol-error
       "__Secure- and __Host- cookies require Secure."
       name))
    (when (and host-prefix-p
               (or (not host-only-p)
                   (not (string= path "/"))))
      (%client-protocol-error
       "__Host- cookies require host-only scope and path '/'."
       name))
    (when (and same-site
               (string-equal (string same-site) "none")
               (not secure-p))
      (%client-protocol-error
       "SameSite=None cookies require Secure."
       name))
    (when (and partitioned-p
               (or (not secure-p) (null partition-key)))
      (%client-protocol-error
       "Partitioned cookies require Secure and a partition key."
       name))
    (%make-http-cookie
     :name (copy-seq name)
     :value (copy-seq value)
     :domain (string-downcase
              (if (and (> (length domain) 1)
                       (char= (char domain 0) #\.))
                  (subseq domain 1)
                  domain))
     :path (copy-seq path)
     :expires expires
     :max-age max-age
     :secure-p secure-p
     :http-only-p (not (null http-only-p))
     :same-site (and same-site (intern (string-upcase (string same-site)) :keyword))
     :host-only-p host-only-p
     :creation-time creation-time
     :partitioned-p partitioned-p
     :partition-key (and partition-key (copy-seq partition-key)))))

(defun make-http-cookie-jar (&key (clock-function #'get-universal-time))
  (%ensure-function clock-function "A cookie jar clock must be a function.")
  (%make-http-cookie-jar :cookies nil :clock-function clock-function))

(defun http-cookie-jar-cookies (jar)
  (unless (http-cookie-jar-p jar)
    (%client-protocol-error "Expected an HTTP-COOKIE-JAR value." jar))
  (mapcar #'%copy-cookie (%http-cookie-jar-cookies jar)))

(defun http-cookie-jar-clear (jar)
  (unless (http-cookie-jar-p jar)
    (%client-protocol-error "Expected an HTTP-COOKIE-JAR value." jar))
  (setf (%http-cookie-jar-cookies jar) nil)
  jar)

(defun %cookie-domain-match-p (host domain)
  (or (string-equal host domain)
      (let ((offset (- (length host) (length domain))))
        (and (plusp offset)
             (char= (char host (1- offset)) #\.)
             (string-equal domain host
                           :start2 offset
                           :end2 (length host))))))

(defun %cookie-default-path (path)
  (let ((slash (position #\/ path :from-end t)))
    (cond ((or (null slash) (zerop slash)) "/")
          (t (subseq path 0 slash)))))

(defun %cookie-path-match-p (request-path cookie-path)
  (or (string= request-path cookie-path)
      (and (%client-string-prefix-p cookie-path request-path)
           (or (char= (char cookie-path (1- (length cookie-path))) #\/)
               (and (> (length request-path) (length cookie-path))
                    (char= (char request-path (length cookie-path)) #\/))))))

(defun %cookie-split-first (string character)
  (let ((position (position character string)))
    (if position
        (values (subseq string 0 position) (subseq string (1+ position)))
        (values string nil))))

(defun %cookie-signed-integer (string)
  (%client-parse-integer string))

(defun %cookie-parse-set-cookie (set-cookie request-uri now &key partition-key)
  (multiple-value-bind (pair ignored) (%cookie-split-first set-cookie #\;)
    (declare (ignore ignored))
    (multiple-value-bind (name value) (%cookie-split-first (%cookie-trim pair) #\=)
      (when (and value (%cookie-name-p (%cookie-trim name)))
        (let* ((name (%cookie-trim name))
               (value (%cookie-trim value))
               (domain (http-uri-host request-uri))
               (path (%cookie-default-path (http-uri-path request-uri)))
               (host-only-p t)
               (expires nil)
               (max-age nil)
               (secure-p nil)
               (http-only-p nil)
               (same-site nil)
               (partitioned-p nil)
               (domain-attribute-p nil)
               (invalid nil)
               (attribute-string (subseq set-cookie
                                         (or (position #\; set-cookie)
                                             (length set-cookie)))))
          (unless (%cookie-value-p value)
            (setf invalid t))
          (dolist (attribute (when (not (string= attribute-string ""))
                               (loop with start = 1
                                     for separator = (position #\; set-cookie
                                                              :start start)
                                     collect (%cookie-trim
                                              (subseq set-cookie start
                                                      (or separator (length set-cookie))))
                                     while separator
                                     do (setf start (1+ separator)))))
            (multiple-value-bind (attribute-name attribute-value)
                (%cookie-split-first attribute #\=)
              (let ((attribute-name (string-downcase (%cookie-trim attribute-name)))
                    (attribute-value (and attribute-value
                                          (%cookie-trim attribute-value))))
                (cond ((string= attribute-name "domain")
                       (when (and attribute-value (not (string= attribute-value "")))
                         (setf domain (string-downcase
                                       (if (char= (char attribute-value 0) #\.)
                                           (subseq attribute-value 1)
                                           attribute-value))
                               host-only-p nil
                               domain-attribute-p t)))
                      ((string= attribute-name "path")
                       (when (and attribute-value
                                  (not (string= attribute-value ""))
                                  (char= (char attribute-value 0) #\/))
                         (setf path attribute-value)))
                      ((string= attribute-name "expires")
                       (setf expires (and attribute-value
                                          (http-parse-date attribute-value))))
                      ((string= attribute-name "max-age")
                       (setf max-age (and attribute-value
                                          (%cookie-signed-integer attribute-value))))
                      ((string= attribute-name "secure") (setf secure-p t))
                      ((string= attribute-name "httponly") (setf http-only-p t))
                      ((string= attribute-name "partitioned")
                       (setf partitioned-p t))
                      ((string= attribute-name "samesite")
                       (when attribute-value
                         (let ((value (string-downcase attribute-value)))
                           (when (member value '("strict" "lax" "none")
                                         :test #'string=)
                             (setf same-site
                                   (intern (string-upcase value) :keyword))))))))))
          (let ((https-p (string-equal (http-uri-scheme request-uri) "https"))
                (secure-prefix-p (%cookie-prefix-p name "__Secure-"))
                (host-prefix-p (%cookie-prefix-p name "__Host-")))
            (when (or invalid
                      (string= domain "")
                      (not (%cookie-domain-match-p (http-uri-host request-uri)
                                                   domain))
                      (and secure-p (not https-p))
                      (and (or secure-prefix-p host-prefix-p)
                           (not secure-p))
                      (and host-prefix-p
                           (or domain-attribute-p
                               (not (string= path "/"))))
                      (and (not https-p) partitioned-p)
                      (and partitioned-p
                           (or (not secure-p) (null partition-key)))
                      (and same-site
                           (string-equal (string same-site) "none")
                           (not secure-p)))
            (return-from %cookie-parse-set-cookie nil))
            (make-http-cookie :name name
                              :value value
                              :domain domain
                              :path path
                              :expires expires
                              :max-age max-age
                              :secure-p secure-p
                              :http-only-p http-only-p
                              :same-site same-site
                              :host-only-p host-only-p
                              :creation-time now
                              :partitioned-p partitioned-p
                              :partition-key partition-key)))))))

(defun %cookie-expired-p (cookie now)
  (or (and (http-cookie-max-age cookie)
           (<= (+ (http-cookie-creation-time cookie)
                  (http-cookie-max-age cookie))
               now))
      (and (null (http-cookie-max-age cookie))
           (http-cookie-expires cookie)
           (<= (http-cookie-expires cookie) now))))

(defun %cookie-identity-p (left right)
  (and (string-equal (http-cookie-name left) (http-cookie-name right))
       (string-equal (http-cookie-domain left) (http-cookie-domain right))
       (string= (http-cookie-path left) (http-cookie-path right))
       (eql (http-cookie-partitioned-p left)
            (http-cookie-partitioned-p right))
       (or (not (http-cookie-partitioned-p left))
           (string= (http-cookie-partition-key left)
                    (http-cookie-partition-key right)))))

(defun http-cookie-jar-accept-response
    (jar request-uri response &key now partition-key)
  "Store valid Set-Cookie fields from RESPONSE for REQUEST-URI.

Malformed cookies and cookies for another domain are ignored, matching the
interoperable browser behavior rather than making a response unusable."
  (unless (http-cookie-jar-p jar)
    (%client-protocol-error "Expected an HTTP-COOKIE-JAR value." jar))
  (let* ((request-uri (%client-uri request-uri))
         (now (or now (funcall (%http-cookie-jar-clock-function jar))))
         (cookies (%http-cookie-jar-cookies jar)))
    (dolist (set-cookie (http-header-values
                         (http-response-headers response)
                         "Set-Cookie"))
      (let ((cookie (%cookie-parse-set-cookie
                     set-cookie request-uri now
                     :partition-key partition-key)))
        (when cookie
          (setf cookies (delete-if (lambda (existing)
                                    (%cookie-identity-p existing cookie))
                                  cookies))
          (unless (%cookie-expired-p cookie now)
            (push cookie cookies)))))
    (setf (%http-cookie-jar-cookies jar) cookies)
    jar))

(defun %cookie-same-site-applicable-p (cookie same-site-context method)
  (or (null same-site-context)
      (not (string-equal (string same-site-context) "cross-site"))
      (eq (http-cookie-same-site cookie) :none)
      (and (eq (http-cookie-same-site cookie) :lax)
           (member (string-upcase (string (or method "GET")))
                   '("GET" "HEAD" "OPTIONS" "TRACE")
                   :test #'string=))))

(defun %cookie-request-applicable-p
    (cookie uri now partition-key same-site-context method)
  (and (or (and (http-cookie-host-only-p cookie)
               (string-equal (http-uri-host uri) (http-cookie-domain cookie)))
           (and (not (http-cookie-host-only-p cookie))
                (%cookie-domain-match-p (http-uri-host uri)
                                        (http-cookie-domain cookie))))
       (%cookie-path-match-p (http-uri-path uri) (http-cookie-path cookie))
       (or (not (http-cookie-secure-p cookie))
           (string-equal (http-uri-scheme uri) "https"))
       (or (not (http-cookie-partitioned-p cookie))
           (and partition-key
                (string= partition-key (http-cookie-partition-key cookie))))
       (%cookie-same-site-applicable-p
        cookie same-site-context method)
       (not (%cookie-expired-p cookie now))))

(defun http-cookie-jar-cookie-header
    (jar request-uri &key now partition-key same-site-context method)
  "Return the Cookie request-header value applicable to REQUEST-URI, or NIL."
  (unless (http-cookie-jar-p jar)
    (%client-protocol-error "Expected an HTTP-COOKIE-JAR value." jar))
  (let* ((request-uri (%client-uri request-uri))
         (now (or now (funcall (%http-cookie-jar-clock-function jar))))
         (cookies (remove-if-not
                   (lambda (cookie)
                     (%cookie-request-applicable-p
                      cookie request-uri now partition-key
                      same-site-context method))
                   (%http-cookie-jar-cookies jar))))
    (setf cookies
          (sort cookies
                (lambda (left right)
                  (or (> (length (http-cookie-path left))
                         (length (http-cookie-path right)))
                      (and (= (length (http-cookie-path left))
                              (length (http-cookie-path right)))
                               (< (http-cookie-creation-time left)
                               (http-cookie-creation-time right)))))))
    (when cookies
      (format nil "~{~A~^; ~}"
              (mapcar (lambda (cookie)
                        (format nil "~A=~A"
                                (http-cookie-name cookie)
                                (http-cookie-value cookie)))
                      cookies)))))
