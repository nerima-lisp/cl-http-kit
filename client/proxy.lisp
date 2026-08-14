(in-package #:http-kit/client)

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

(defun %proxy-port-token (token)
  (let ((length (length token)))
    (cond
      ((and (> length 2) (char= (char token 0) #\[))
       (let ((close (position #\] token)))
         (when (and close
                    (< (1+ close) length)
                    (char= (char token (1+ close)) #\:))
           (values (subseq token 1 close)
                   (%client-parse-integer token
                                          :start (+ close 2)
                                          :allow-sign-p nil)))))
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
           (if (and (not (string= token-host ""))
                    (char= (char token-host 0) #\.))
               (let ((domain (subseq token-host 1))
                     (offset (- (length host) (length (subseq token-host 1)))))
                 (or (string-equal host domain)
                     (and (plusp offset)
                          (char= (char host (1- offset)) #\.)
                          (string-equal domain host
                                        :start2 offset
                                        :end2 (length host)))))
               (string-equal host token-host))))))

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
      ((null proxy) nil)
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
                 (and (http-proxy-username proxy)
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
                   :remote-dns-p (eq proxy-scheme :socks5h)
                   :proxy-authorization authorization))
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
