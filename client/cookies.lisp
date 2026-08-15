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

(defun %cookie-octets-p (value &key (start 0) (end (length value)))
  (loop for index from start below end
        for code = (char-code (char value index))
        always (or (= code #x21)
                   (<= #x23 code #x2b)
                   (<= #x2d code #x3a)
                   (<= #x3c code #x5b)
                   (<= #x5d code #x7e))))

(defun %cookie-value-p (value)
  (and (stringp value)
       (or (%cookie-octets-p value)
           (and (>= (length value) 2)
                (char= (char value 0) #\")
                (char= (char value (1- (length value))) #\")
                (%cookie-octets-p value
                                  :start 1
                                  :end (1- (length value)))))))

(defun %cookie-prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string-equal prefix string :end2 (length prefix))))

(defun %cookie-partition-key (partition-key)
  (when (and partition-key
             (not (and (stringp partition-key)
                       (plusp (length partition-key)))))
    (%client-protocol-error
     "A cookie partition key must be a non-empty string or NIL."
     partition-key))
  partition-key)

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
   :expiry-time (http-cookie-expiry-time cookie)
   :secure-p (http-cookie-secure-p cookie)
   :http-only-p (http-cookie-http-only-p cookie)
   :same-site (http-cookie-same-site cookie)
   :partition-key (and (http-cookie-partition-key cookie)
                       (copy-seq (http-cookie-partition-key cookie)))
   :host-only-p (http-cookie-host-only-p cookie)
   :creation-time (http-cookie-creation-time cookie)
   :last-access-time (http-cookie-last-access-time cookie)))

(defun make-http-cookie
    (&key name value domain (path "/") expires max-age secure-p http-only-p
          same-site partition-key (host-only-p nil)
          (creation-time (get-universal-time)))
  (unless (%cookie-name-p name)
    (%client-protocol-error "Cookie names must be valid cookie tokens." name))
  (unless (%cookie-value-p value)
    (%client-protocol-error "Cookie values must contain only cookie-octet characters."
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
  (when (and same-site
             (string-equal same-site :none)
             (not secure-p))
    (%client-protocol-error "SameSite=None cookies must be Secure." name))
  (%cookie-partition-key partition-key)
  (when (and partition-key (not secure-p))
    (%client-protocol-error "Partitioned cookies must be Secure." name))
  (when (and (%cookie-prefix-p "__Secure-" name)
             (not secure-p))
    (%client-protocol-error "__Secure- cookies must be Secure." name))
  (when (and (%cookie-prefix-p "__Host-" name)
             (or (not secure-p)
                 (not host-only-p)
                 (not (string= path "/"))))
    (%client-protocol-error
     "__Host- cookies must be Secure, host-only, and use Path=/ ."
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
   :expiry-time (if max-age (+ creation-time max-age) expires)
   :secure-p (not (null secure-p))
   :http-only-p (not (null http-only-p))
   :same-site (and same-site (intern (string-upcase (string same-site)) :keyword))
   :partition-key (and partition-key (copy-seq partition-key))
   :host-only-p (not (null host-only-p))
   :creation-time creation-time
   :last-access-time creation-time))

(defun make-http-cookie-jar
    (&key (clock-function #'get-universal-time) public-suffix-p-function
          (max-cookies 3000) (max-cookies-per-domain 180)
          (max-cookie-bytes 4096) (max-total-cookie-bytes 12288000))
  (%ensure-function clock-function "A cookie jar clock must be a function.")
  (when public-suffix-p-function
    (%ensure-function public-suffix-p-function
                      "A public suffix predicate must be a function or NIL."))
  (%ensure-positive-integer max-cookies
                            "A cookie jar maximum must be a positive integer.")
  (%ensure-positive-integer
   max-cookies-per-domain
   "A per-domain cookie maximum must be a positive integer.")
  (%ensure-positive-integer
   max-cookie-bytes
   "A per-cookie byte maximum must be a positive integer.")
  (%ensure-positive-integer
   max-total-cookie-bytes
   "A cookie jar byte maximum must be a positive integer.")
  (%make-http-cookie-jar
   :cookies nil
   :clock-function clock-function
   :public-suffix-p-function public-suffix-p-function
   :max-cookies max-cookies
   :max-cookies-per-domain max-cookies-per-domain
   :max-cookie-bytes max-cookie-bytes
   :max-total-cookie-bytes max-total-cookie-bytes))

(defun http-cookie-jar-cookies (jar)
  (unless (http-cookie-jar-p jar)
    (%client-protocol-error "Expected an HTTP-COOKIE-JAR value." jar))
  (mapcar #'%copy-cookie (%http-cookie-jar-cookies jar)))

(defun http-cookie-jar-clear (jar)
  (unless (http-cookie-jar-p jar)
    (%client-protocol-error "Expected an HTTP-COOKIE-JAR value." jar))
  (setf (%http-cookie-jar-cookies jar) nil)
  jar)

(defun %cookie-ipv4-address-p (host)
  (let ((parts nil)
        (start 0))
    (loop for separator = (position #\. host :start start)
          do (push (subseq host start separator) parts)
          while separator
          do (setf start (1+ separator)))
    (and (= (length parts) 4)
         (every (lambda (part)
                  (and (plusp (length part))
                       (every #'digit-char-p part)
                       (<= (parse-integer part) 255)))
                parts))))

(defun %cookie-ip-address-p (host)
  (or (find #\: host)
      (%cookie-ipv4-address-p host)))

(defun %cookie-domain-match-p (host domain)
  (or (string-equal host domain)
      (let ((offset (- (length host) (length domain))))
        (and (plusp offset)
             (not (%cookie-ip-address-p host))
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

(defun %cookie-parse-set-cookie (set-cookie request-uri now partition-key)
  (multiple-value-bind (pair ignored) (%cookie-split-first set-cookie #\;)
    (declare (ignore ignored))
    (multiple-value-bind (name value) (%cookie-split-first (%cookie-trim pair) #\=)
      (when (and value (%cookie-name-p (%cookie-trim name)))
        (let* ((name (%cookie-trim name))
               (value (%cookie-trim value))
               (domain (http-uri-host request-uri))
               (path (%cookie-default-path (http-uri-path request-uri)))
               (path-attribute-p nil)
               (host-only-p t)
               (expires nil)
               (max-age nil)
               (secure-p nil)
               (http-only-p nil)
               (same-site nil)
               (partitioned-p nil)
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
                       (setf path-attribute-p t)
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
          (when (or invalid
                    (zerop (length domain))
                    (not (%cookie-domain-match-p (http-uri-host request-uri)
                                                 domain))
                    (and secure-p
                         (not (string= (http-uri-scheme request-uri) "https")))
                    (and (eq same-site :none) (not secure-p))
                    (and partitioned-p
                         (or (not secure-p) (null partition-key)))
                    (and (%cookie-prefix-p "__Secure-" name)
                         (not secure-p))
                    (and (%cookie-prefix-p "__Host-" name)
                         (or (not secure-p)
                             (not host-only-p)
                             (not path-attribute-p)
                             (not (string= path "/")))))
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
                            :partition-key (and partitioned-p partition-key)
                            :host-only-p host-only-p
                            :creation-time now))))))

(defun %cookie-expired-p (cookie now)
  (and (http-cookie-expiry-time cookie)
       (<= (http-cookie-expiry-time cookie) now)))

(defun %cookie-identity-p (left right)
  (and (string= (http-cookie-name left) (http-cookie-name right))
       (string-equal (http-cookie-domain left) (http-cookie-domain right))
       (equal (http-cookie-partition-key left)
              (http-cookie-partition-key right))
       (string= (http-cookie-path left) (http-cookie-path right))))

(defun %cookie-secure-overlay-p (cookie request-uri cookies)
  (and (not (http-cookie-secure-p cookie))
       (not (string= (http-uri-scheme request-uri) "https"))
       (some (lambda (existing)
               (and (http-cookie-secure-p existing)
                    (equal (http-cookie-partition-key existing)
                           (http-cookie-partition-key cookie))
                    (string= (http-cookie-name existing)
                             (http-cookie-name cookie))
                    (or (%cookie-domain-match-p (http-cookie-domain existing)
                                                (http-cookie-domain cookie))
                        (%cookie-domain-match-p (http-cookie-domain cookie)
                                                (http-cookie-domain existing)))
                    (%cookie-path-match-p (http-cookie-path cookie)
                                          (http-cookie-path existing))))
             cookies)))

(defun %cookie-public-suffix-acceptable-p (cookie request-uri predicate)
  (or (http-cookie-host-only-p cookie)
      (and predicate
           (or (not (funcall predicate (http-cookie-domain cookie)))
               (when (string-equal (http-cookie-domain cookie)
                                   (http-uri-host request-uri))
                 (setf (http-cookie-host-only-p cookie) t)
                 t)))))

(defun %cookie-eviction-older-p (left right)
  (or (< (http-cookie-last-access-time left)
         (http-cookie-last-access-time right))
      (and (= (http-cookie-last-access-time left)
              (http-cookie-last-access-time right))
           (< (http-cookie-creation-time left)
              (http-cookie-creation-time right)))))

(defun %cookie-evict-oldest (cookies candidates)
  (let ((oldest (first (sort (copy-list candidates)
                             #'%cookie-eviction-older-p))))
    (delete oldest cookies :count 1 :test #'eq)))

(defun %cookie-string-octet-length (value)
  (loop for character across value
        for code = (char-code character)
        sum (cond ((<= code #x7f) 1)
                  ((<= code #x7ff) 2)
                  ((<= code #xffff) 3)
                  (t 4))))

(defun %cookie-storage-size (cookie)
  (loop for value in (list (http-cookie-name cookie)
                           (http-cookie-value cookie)
                           (http-cookie-domain cookie)
                           (http-cookie-path cookie)
                           (http-cookie-partition-key cookie))
        when value
          sum (%cookie-string-octet-length value)))

(defun %cookie-total-storage-size (cookies)
  (loop for cookie in cookies sum (%cookie-storage-size cookie)))

(defun %cookie-enforce-limits (cookies jar now)
  (let ((cookies (delete-if (lambda (cookie) (%cookie-expired-p cookie now))
                            cookies)))
    (dolist (scope (remove-duplicates
                    (mapcar (lambda (cookie)
                              (cons (http-cookie-domain cookie)
                                    (http-cookie-partition-key cookie)))
                            cookies)
                    :test (lambda (left right)
                            (and (string-equal (car left) (car right))
                                 (equal (cdr left) (cdr right))))))
      (loop for domain-cookies =
              (remove-if-not
               (lambda (cookie)
                 (and (string-equal (car scope) (http-cookie-domain cookie))
                      (equal (cdr scope)
                             (http-cookie-partition-key cookie))))
               cookies)
            while (> (length domain-cookies)
                     (%http-cookie-jar-max-cookies-per-domain jar))
            do (setf cookies (%cookie-evict-oldest cookies domain-cookies))))
    (loop while (> (length cookies) (%http-cookie-jar-max-cookies jar))
          do (setf cookies (%cookie-evict-oldest cookies cookies)))
    (loop while (> (%cookie-total-storage-size cookies)
                   (%http-cookie-jar-max-total-cookie-bytes jar))
          do (setf cookies (%cookie-evict-oldest cookies cookies)))
    cookies))

(defun http-cookie-jar-accept-response
    (jar request-uri response
     &key now partition-key (same-site-p t) (top-level-navigation-p nil))
  "Store valid Set-Cookie fields from RESPONSE for REQUEST-URI.

Malformed cookies and cookies for another domain are ignored, matching the
interoperable browser behavior rather than making a response unusable."
  (unless (http-cookie-jar-p jar)
    (%client-protocol-error "Expected an HTTP-COOKIE-JAR value." jar))
  (%cookie-partition-key partition-key)
  (let* ((request-uri (%client-uri request-uri))
         (now (or now (funcall (%http-cookie-jar-clock-function jar))))
         (cookies (%http-cookie-jar-cookies jar)))
    (dolist (set-cookie (http-header-values
                         (http-response-headers response)
                         "Set-Cookie"))
      (let ((cookie (and (<= (%cookie-string-octet-length set-cookie)
                             (%http-cookie-jar-max-cookie-bytes jar))
                         (%cookie-parse-set-cookie
                          set-cookie request-uri now partition-key))))
        (when (and cookie
                   (or same-site-p
                       top-level-navigation-p
                       (not (member (http-cookie-same-site cookie)
                                    '(:strict :lax))))
                   (%cookie-public-suffix-acceptable-p
                    cookie request-uri
                    (%http-cookie-jar-public-suffix-p-function jar))
                   (not (%cookie-secure-overlay-p cookie request-uri cookies)))
          (let ((existing (find-if (lambda (existing)
                                     (%cookie-identity-p existing cookie))
                                   cookies)))
            (when existing
              (setf (http-cookie-creation-time cookie)
                    (http-cookie-creation-time existing))))
          (setf cookies (delete-if (lambda (existing)
                                    (%cookie-identity-p existing cookie))
                                  cookies))
          (unless (%cookie-expired-p cookie now)
            (push cookie cookies)))))
    (setf (%http-cookie-jar-cookies jar)
          (%cookie-enforce-limits cookies jar now))
    jar))

(defun %cookie-safe-method-p (method)
  (member (string method)
          '("GET" "HEAD" "OPTIONS" "TRACE")
          :test #'string=))

(defun %cookie-same-site-applicable-p
    (cookie same-site-p top-level-navigation-p method)
  (case (http-cookie-same-site cookie)
    (:strict same-site-p)
    (:none t)
    (otherwise
     (or same-site-p
         (and top-level-navigation-p (%cookie-safe-method-p method))))))

(defun %cookie-request-applicable-p
    (cookie uri now same-site-p top-level-navigation-p method partition-key)
  (and (or (and (http-cookie-host-only-p cookie)
               (string-equal (http-uri-host uri) (http-cookie-domain cookie)))
           (and (not (http-cookie-host-only-p cookie))
                (%cookie-domain-match-p (http-uri-host uri)
                                        (http-cookie-domain cookie))))
       (%cookie-path-match-p (http-uri-path uri) (http-cookie-path cookie))
       (or (not (http-cookie-secure-p cookie))
           (string= (http-uri-scheme uri) "https"))
       (%cookie-same-site-applicable-p
        cookie same-site-p top-level-navigation-p method)
       (or (null (http-cookie-partition-key cookie))
           (equal (http-cookie-partition-key cookie) partition-key))
       (not (%cookie-expired-p cookie now))))

(defun http-cookie-jar-cookie-header
    (jar request-uri
     &key now (same-site-p t) (top-level-navigation-p nil) (method "GET")
       partition-key)
  "Return the Cookie request-header value applicable to REQUEST-URI, or NIL.

SAME-SITE-P states whether the request is same-site with its initiating
context.  Cross-site Lax cookies are sent only for safe top-level navigations."
  (unless (http-cookie-jar-p jar)
    (%client-protocol-error "Expected an HTTP-COOKIE-JAR value." jar))
  (%cookie-partition-key partition-key)
  (let* ((request-uri (%client-uri request-uri))
         (now (or now (funcall (%http-cookie-jar-clock-function jar))))
         (stored-cookies (delete-if (lambda (cookie)
                                      (%cookie-expired-p cookie now))
                                    (%http-cookie-jar-cookies jar)))
         (cookies (remove-if-not
                   (lambda (cookie)
                     (%cookie-request-applicable-p
                      cookie request-uri now same-site-p
                      top-level-navigation-p method partition-key))
                   stored-cookies)))
    (setf (%http-cookie-jar-cookies jar) stored-cookies)
    (dolist (cookie cookies)
      (setf (http-cookie-last-access-time cookie) now))
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
