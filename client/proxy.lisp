(in-package #:http-kit/client)

(defparameter *proxy-environment-function*
  (lambda (name)
    (uiop:getenv name)))

(defun %proxy-environment-value (&rest names)
  (loop for name in names
        for value = (funcall *proxy-environment-function* name)
        when value do
          (unless (stringp value)
            (%client-protocol-error
             "A proxy environment variable must contain a string."
             name))
          (return value)))

(defun %proxy-environment-scheme (scheme)
  (intern (string-upcase scheme) :keyword))

(defun %proxy-environment-hex-value (character)
  (position character "0123456789abcdefABCDEF" :test #'char=))

(defun %proxy-environment-unescape (value variable)
  (with-output-to-string (stream)
    (loop for index from 0 below (length value)
          for character = (char value index)
          do (if (char= character #\%)
                 (if (<= (+ index 2) (1- (length value)))
                     (let ((high (%proxy-environment-hex-value
                                  (char value (1+ index))))
                           (low (%proxy-environment-hex-value
                                 (char value (+ index 2)))))
                       (unless (and high low)
                         (%client-protocol-error
                          "A proxy environment URL contains an invalid escape."
                          variable))
                       (write-char (code-char (+ (* 16 high) low)) stream)
                       (incf index 2))
                     (%client-protocol-error
                      "A proxy environment URL contains an incomplete escape."
                      variable))
                 (write-char character stream)))))

(defun %proxy-environment-port (value variable)
  (let ((port (%client-parse-integer value :allow-sign-p nil)))
    (unless (and port (<= 1 port 65535))
      (%client-protocol-error
       "A proxy environment URL contains an invalid port."
       variable))
    port))

(defun %proxy-environment-proxy (value variable)
  (let* ((value (%cache-trim value))
         (scheme-position (search "://" value))
         (scheme (if scheme-position
                     (subseq value 0 scheme-position)
                     "http"))
         (scheme-keyword (%proxy-environment-scheme scheme))
         (authority-start (if scheme-position (+ scheme-position 3) 0))
         (authority-end (position-if
                         (lambda (character)
                           (find character "/?#" :test #'char=))
                         value :start authority-start))
         (authority (subseq value authority-start authority-end)))
    (unless (and (member scheme-keyword '(:http :https :socks5 :socks5h))
                 (plusp (length authority))
                 (or (null authority-end)
                     (string= (subseq value authority-end) "/")))
      (%client-protocol-error
       "A proxy environment URL has an unsupported scheme or path."
       variable))
    (let* ((at (position #\@ authority :from-end t))
           (userinfo (and at (subseq authority 0 at)))
           (hostport (if at (subseq authority (1+ at)) authority))
           (username nil)
           (password nil))
      (when userinfo
        (let ((separator (position #\: userinfo)))
          (if separator
              (setf username
                    (%proxy-environment-unescape
                     (subseq userinfo 0 separator) variable)
                    password
                    (%proxy-environment-unescape
                     (subseq userinfo (1+ separator)) variable))
              (setf username
                    (%proxy-environment-unescape userinfo variable)))))
      (multiple-value-bind (host port)
          (cond
            ((and (plusp (length hostport))
                  (char= (char hostport 0) #\[))
             (let ((close (position #\] hostport)))
               (unless close
                 (%client-protocol-error
                  "A proxy environment URL contains an invalid host."
                  variable))
               (let ((rest (subseq hostport (1+ close))))
                 (values (subseq hostport 1 close)
                         (if (string= rest "")
                             nil
                             (if (and (plusp (length rest))
                                      (char= (char rest 0) #\:))
                                 (%proxy-environment-port
                                  (subseq rest 1) variable)
                                 (%client-protocol-error
                                  "A proxy environment URL contains an invalid port."
                                  variable))))))
            (t
             (let ((colon (position #\: hostport :from-end t)))
               (if (and colon
                        (= colon (position #\: hostport))
                        (< (1+ colon) (length hostport)))
                   (values (subseq hostport 0 colon)
                           (%proxy-environment-port
                            (subseq hostport (1+ colon)) variable))
                   (values hostport nil)))))
        (unless (plusp (length host))
          (%client-protocol-error
           "A proxy environment URL requires a host."
           variable))
        (make-http-proxy
         :scheme scheme-keyword
         :host host
         :port (or port (case scheme-keyword
                          ((:http) 80)
                          ((:https) 443)
                          (otherwise 1080)))
         :username username
         :password password))))))

(defun %proxy-environment-proxy-for-uri (uri)
  (let* ((scheme (http-uri-scheme uri))
         (proxy-value
           (if (string= scheme "https")
               (or (%proxy-environment-value "https_proxy" "HTTPS_PROXY")
                   (%proxy-environment-value "http_proxy" "HTTP_PROXY"))
               (%proxy-environment-value "http_proxy" "HTTP_PROXY"))))
    (when (and proxy-value (plusp (length (%cache-trim proxy-value))))
      (let ((proxy (%proxy-environment-proxy proxy-value
                                             (if (string= scheme "https")
                                                 :https-proxy
                                                 :http-proxy))))
        (setf (http-proxy-no-proxy proxy)
              (%proxy-environment-value "no_proxy" "NO_PROXY"))
        proxy))))

(defun %proxy-no-proxy-tokens (value)
  (cond ((null value) nil)
        ((stringp value) (%cache-split-comma value))
        ((listp value)
         (mapcar (lambda (item)
                   (unless (stringp item)
                     (%client-protocol-error
                      "NO-PROXY entries must be strings."
                      item))
                   (%cache-trim item))
                 value))
        (t (%client-protocol-error
            "NO-PROXY must be a string or list of strings."
            value))))

(defun %proxy-ipv4-octets (host)
  (when (stringp host)
    (let ((start 0)
          (values nil)
          (valid-p t))
      (loop
        for separator = (position #\. host :start start)
        for end = (or separator (length host))
        for part = (and (< start end) (subseq host start end))
        do (unless (and part
                        (let ((value (%client-parse-integer
                                      part
                                      :allow-sign-p nil)))
                          (and value (<= value 255))))
             (setf valid-p nil))
           (when valid-p
             (push (%client-parse-integer
                    part
                    :allow-sign-p nil)
                   values))
           (if separator
               (setf start (1+ separator))
               (return)))
      (when (and valid-p (= (length values) 4))
        (let ((result (make-array 4 :element-type '(unsigned-byte 8))))
          (loop for value in (nreverse values)
                for index from 0
                do (setf (aref result index) value))
          result)))))

(defun %proxy-hex-value (character)
  (position character "0123456789abcdefABCDEF" :test #'char=))

(defun %proxy-hex-word (token)
  (when (and (stringp token)
             (plusp (length token))
             (<= (length token) 4)
             (every (lambda (character)
                      (%proxy-hex-value character))
                    token))
    (let ((value 0))
      (loop for character across token
            for digit = (%proxy-hex-value character)
            do (setf value (+ (* value 16)
                              (if (< digit 16) digit (- digit 6)))))
      value)))

(defun %proxy-colon-parts (string)
  (let ((start 0)
        (parts nil))
    (loop
      for separator = (position #\: string :start start)
      for end = (or separator (length string))
      do (push (subseq string start end) parts)
         (if separator
             (setf start (1+ separator))
             (return (nreverse parts))))))

(defun %proxy-ipv6-octets (host)
  (when (and (stringp host)
             (not (find #\[ host)))
    (let* ((double (search "::" host))
           (second-double (and double
                               (search "::" host
                                       :start2 (1+ double))))
           (left-string (if double (subseq host 0 double) host))
           (right-string (and double (subseq host (+ double 2))))
           (left (if (zerop (length left-string))
                     nil
                     (%proxy-colon-parts left-string)))
           (right (if (or (null right-string)
                          (zerop (length right-string)))
                      nil
                      (%proxy-colon-parts right-string)))
           (parts (append left right))
           (last-index (1- (length parts)))
           (word-groups nil)
           (valid-p (null second-double)))
      (when (and (not double)
                 (or (some (lambda (part) (zerop (length part))) left)
                     (some (lambda (part) (zerop (length part))) right)))
        (setf valid-p nil))
      (loop for part in parts
            for index from 0
            do (cond
                 ((zerop (length part))
                  (setf valid-p nil))
                 ((find #\. part)
                  (let ((octets (%proxy-ipv4-octets part)))
                    (if (and octets (= index last-index))
                        (push (list (+ (ash (aref octets 0) 8)
                                       (aref octets 1))
                                    (+ (ash (aref octets 2) 8)
                                       (aref octets 3)))
                              word-groups)
                        (setf valid-p nil))))
                 (t
                  (let ((word (%proxy-hex-word part)))
                    (if word
                        (push (list word) word-groups)
                        (setf valid-p nil))))))
      (setf word-groups (nreverse word-groups))
      (let* ((words (loop for group in word-groups append group))
             (left-word-count
               (loop for part in left
                     sum (if (find #\. part) 2 1))))
        (when (and valid-p
                   (if double
                       (< (length words) 8)
                       (= (length words) 8)))
          (let* ((zeroes (if double (- 8 (length words)) 0))
                 (expanded (if double
                               (append (subseq words 0 left-word-count)
                                       (make-list zeroes :initial-element 0)
                                       (subseq words left-word-count))
                               words))
                 (result (make-array 16 :element-type '(unsigned-byte 8))))
            (loop for word in expanded
                  for index from 0 by 2
                  do (setf (aref result index) (ldb (byte 8 8) word)
                           (aref result (1+ index)) (ldb (byte 8 0) word)))
            result))))))

(defun %proxy-cidr-match-p (host token)
  (let ((slash (position #\/ token :from-end t)))
    (when slash
      (let* ((network-host (subseq token 0 slash))
             (prefix (%client-parse-integer token
                                            :start (1+ slash)
                                            :allow-sign-p nil))
             (host-octets (or (%proxy-ipv4-octets host)
                              (%proxy-ipv6-octets host)))
             (network-octets (or (%proxy-ipv4-octets network-host)
                                 (%proxy-ipv6-octets network-host))))
        (when (and prefix host-octets network-octets
                   (= (length host-octets) (length network-octets))
                   (<= prefix (* 8 (length host-octets))))
          (multiple-value-bind (whole-bits remaining-bits)
              (floor prefix 8)
            (and (loop for index below whole-bits
                       always (= (aref host-octets index)
                                 (aref network-octets index)))
                 (or (zerop remaining-bits)
                     (let ((mask (- 256
                                    (ash 1 (- 8 remaining-bits)))))
                       (= (logand mask (aref host-octets whole-bits))
                          (logand mask
                                  (aref network-octets whole-bits))))))))))))

(defun %proxy-domain-match-p (host domain)
  (let ((offset (- (length host) (length domain))))
    (or (string-equal host domain)
        (and (plusp offset)
             (char= (char host (1- offset)) #\.)
             (string-equal domain host
                           :start2 offset
                           :end2 (length host))))))

(defun %proxy-port-token (token)
  (let ((length (length token)))
    (cond
      ((and (> length 2) (char= (char token 0) #\[))
       (let ((close (position #\] token)))
         (when close
           (let ((rest (subseq token (1+ close))))
             (cond
               ((string= rest "")
                (values (subseq token 1 close) nil))
               ((and (char= (char rest 0) #\:)
                     (> (length rest) 1))
                (values (subseq token 1 close)
                        (%client-parse-integer rest
                                               :start 1
                                               :allow-sign-p nil)))
               (t (values nil nil)))))))
      (t
       (let ((colon (position #\: token :from-end t)))
         (if (and colon
                  (not (position #\: token :end colon))
                  (< (1+ colon) length))
             (values (subseq token 0 colon)
                     (%client-parse-integer token
                                            :start (1+ colon)
                                            :allow-sign-p nil))
             (values token nil)))))))

(defun %proxy-host-token-match-p (host port token)
  (multiple-value-bind (token-host token-port) (%proxy-port-token token)
    (and token-host
         (or (null token-port) (= token-port port))
         (let ((token-host (string-downcase token-host)))
           (or (%proxy-cidr-match-p host token-host)
               (%proxy-domain-match-p
                host
                (if (and (plusp (length token-host))
                         (char= (char token-host 0) #\.))
                    (subseq token-host 1)
                    token-host)))))))

(defun http-proxy-no-proxy-p (proxy uri)
  "Return true when PROXY's NO-PROXY rules exclude URI."
  (unless (or (null proxy) (http-proxy-p proxy))
    (%client-protocol-error "Expected an HTTP-PROXY value or NIL." proxy))
  (if (null proxy)
      nil
      (let* ((uri (%client-uri uri))
             (host (string-downcase (http-uri-host uri)))
             (port (%http-uri-effective-port uri)))
        (some (lambda (token)
                (or (string= token "*")
                    (and (not (string= token ""))
                         (%proxy-host-token-match-p host port token))))
              (%proxy-no-proxy-tokens (http-proxy-no-proxy proxy))))))

(defun http-proxy-for-uri (proxy uri)
  "Resolve a proxy specification for URI, honoring its NO-PROXY rules.

PROXY may be an HTTP-PROXY object, a function of one URI argument, or a list
of proxy objects/functions.  The first applicable result is returned."
  (let ((uri (%client-uri uri)))
    (cond
      ((null proxy) (%proxy-environment-proxy-for-uri uri))
      ((http-proxy-p proxy)
       (and (not (http-proxy-no-proxy-p proxy uri)) proxy))
      ((functionp proxy)
       (let ((result (funcall proxy uri)))
         (unless (or (null result) (http-proxy-p result))
           (%client-protocol-error
            "A proxy resolver must return an HTTP-PROXY value or NIL."
            result))
         (and result (http-proxy-for-uri result uri))))
      ((listp proxy)
       (loop for candidate in proxy
             for result = (http-proxy-for-uri candidate uri)
             when result do (return result)))
      (t (%client-protocol-error
          "A proxy must be an HTTP-PROXY, resolver, list, or NIL."
          proxy)))))

(defun http-proxy-plan (proxy uri)
  "Return a transport-neutral plan for connecting to URI through PROXY."
  (let* ((uri (%client-uri uri))
         (proxy (http-proxy-for-uri proxy uri)))
    (if (null proxy)
        (list :mode :direct
              :target uri
              :request-target (http-uri-string uri))
        (let* ((target-scheme (http-uri-scheme uri))
               (proxy-scheme (http-proxy-scheme proxy))
               (connect-p (or (string= target-scheme "https")
                              (member proxy-scheme '(:socks5 :socks5h))))
               (authorization
                 (and (member proxy-scheme '(:http :https))
                      (http-proxy-username proxy)
                      (http-basic-authorization
                       (http-proxy-username proxy)
                       (or (http-proxy-password proxy) "")))))
          (cond
            ((member proxy-scheme '(:socks5 :socks5h))
             (list :mode :socks5
                   :target uri
                   :proxy proxy
                   :connect-host (http-uri-host uri)
                   :connect-port (%http-uri-effective-port uri)
                   :proxy-host (http-proxy-host proxy)
                   :proxy-port (http-proxy-port proxy)
                   :remote-dns-p (eq proxy-scheme :socks5h)))
            (connect-p
             (list :mode :connect
                   :target uri
                   :proxy proxy
                   :connect-host (http-uri-host uri)
                   :connect-port (%http-uri-effective-port uri)
                   :proxy-host (http-proxy-host proxy)
                   :proxy-port (http-proxy-port proxy)
                   :proxy-authorization authorization
                   :tunnel-p t))
            (t
             (list :mode :forward
                   :target uri
                   :proxy proxy
                   :request-target (http-uri-string uri)
                   :connect-host (http-proxy-host proxy)
                   :connect-port (http-proxy-port proxy)
                   :proxy-authorization authorization)))))))
