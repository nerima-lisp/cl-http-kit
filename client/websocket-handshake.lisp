(in-package #:http-kit/websocket)

(defun %websocket-header-tokens (value)
  (unless (stringp value)
    (%websocket-protocol-error "A WebSocket header value must be a string." value))
  (let ((tokens '())
        (start 0)
        (length (length value)))
    (loop
      (let* ((comma (position #\, value :start start))
             (end (or comma length))
             (token (string-trim '(#\Space #\Tab)
                                 (subseq value start end))))
        (when (not (string= token ""))
          (push token tokens))
        (if comma
            (setf start (1+ comma))
            (return (nreverse tokens)))))))

(defun %websocket-header-has-token-p (headers name token)
  (some (lambda (value)
          (some (lambda (candidate)
                  (string-equal candidate token))
                (%websocket-header-tokens value)))
        (http-header-values headers name)))

(defun %websocket-single-header-value (headers name)
  (let ((values (http-header-values headers name)))
    (when (and (consp values) (null (cdr values)))
      (string-trim '(#\Space #\Tab) (first values)))))

(defun %websocket-token-string-p (value)
  (and (stringp value)
       (not (string= value ""))
       (loop for character across value
             for code = (char-code character)
             always (and (<= #x21 code #x7e)
                         (not (find character
                                    '(#\( #\) #\< #\> #\@ #\, #\;
                                      #\: #\\ #\" #\/ #\[ #\] #\?
                                      #\= #\{ #\} #\Space #\Tab)
                                    :test #'char=))))))

(defstruct (websocket-extension
             (:constructor %make-websocket-extension (name parameters)))
  (name "" :type string)
  (parameters '() :type list))

(defun parse-websocket-extensions (value)
  "Parse one Sec-WebSocket-Extensions field value using RFC 6455 grammar.

Return a list of WEBSOCKET-EXTENSION objects.  Extension and parameter names
are normalized to lowercase.  Each parameter is represented by a cons whose
CAR is its name and whose CDR is its decoded token or quoted-string value;
NIL denotes a valueless parameter."
  (unless (stringp value)
    (%websocket-protocol-error
     "A WebSocket extension header must be a string."
     value))
  (let ((index 0)
        (length (length value)))
    (labels ((skip-ows ()
               (loop while (and (< index length)
                                (find (char value index) '(#\Space #\Tab)
                                      :test #'char=))
                     do (incf index)))
             (read-token ()
               (let ((start index))
                 (loop while (and (< index length)
                                  (%websocket-token-string-p
                                   (string (char value index))))
                       do (incf index))
                 (unless (< start index)
                   (%websocket-protocol-error
                    "A WebSocket extension token was expected."
                    value))
                 (subseq value start index)))
             (read-quoted-string ()
               (incf index)
               (with-output-to-string (output)
                 (loop
                   (when (>= index length)
                     (%websocket-protocol-error
                      "A WebSocket extension quoted string is unterminated."
                      value))
                   (let* ((character (char value index))
                          (code (char-code character)))
                     (incf index)
                     (cond ((char= character #\")
                            (return))
                           ((char= character #\\)
                            (when (>= index length)
                              (%websocket-protocol-error
                               "A WebSocket extension quoted string is unterminated."
                               value))
                            (let* ((escaped (char value index))
                                   (escaped-code (char-code escaped)))
                              (unless (or (= escaped-code #x09)
                                          (<= #x20 escaped-code #x7e)
                                          (<= #x80 escaped-code #xff))
                                (%websocket-protocol-error
                                 "A WebSocket extension contains an invalid quoted character."
                                 value))
                              (write-char escaped output)
                              (incf index)))
                           ((or (= code #x09) (= code #x20)
                                (= code #x21) (<= #x23 code #x5b)
                                (<= #x5d code #x7e) (<= #x80 code #xff))
                            (write-char character output))
                           (t
                            (%websocket-protocol-error
                             "A WebSocket extension contains an invalid quoted character."
                             value)))))))
             (read-parameter-value ()
               (if (and (< index length) (char= (char value index) #\"))
                   (let ((decoded (read-quoted-string)))
                     (unless (%websocket-token-string-p decoded)
                       (%websocket-protocol-error
                        "A decoded WebSocket extension value must be a token."
                        decoded))
                     decoded)
                   (read-token))))
      (skip-ows)
      (when (= index length)
        (%websocket-protocol-error
         "A WebSocket extension header cannot be empty."
         value))
      (let ((extensions '()))
        (loop
          (let ((name (string-downcase (read-token)))
                (parameters '())
                (seen '()))
            (skip-ows)
            (loop while (and (< index length)
                             (char= (char value index) #\;))
                  do (incf index)
                     (skip-ows)
                     (let ((parameter-name (string-downcase (read-token)))
                           (parameter-value nil))
                       (when (member parameter-name seen :test #'string=)
                         (%websocket-protocol-error
                          "A WebSocket extension parameter is duplicated."
                          parameter-name))
                       (push parameter-name seen)
                       (skip-ows)
                       (when (and (< index length)
                                  (char= (char value index) #\=))
                         (incf index)
                         (skip-ows)
                         (setf parameter-value (read-parameter-value)))
                       (push (cons parameter-name parameter-value) parameters)
                       (skip-ows)))
            (push (%make-websocket-extension name (nreverse parameters))
                  extensions))
          (cond ((= index length)
                 (return (nreverse extensions)))
                ((char= (char value index) #\,)
                 (incf index)
                 (skip-ows)
                 (when (= index length)
                   (%websocket-protocol-error
                    "A WebSocket extension header cannot end with a comma."
                    value)))
                (t
                 (%websocket-protocol-error
                  "A WebSocket extension separator was expected."
                  value))))))))

(defun %websocket-header-extensions (headers)
  (loop for value in (http-header-values headers "Sec-WebSocket-Extensions")
        append (parse-websocket-extensions value)))

(defun %websocket-window-bits (value parameter-name &key allow-absent-p)
  (when (null value)
    (if allow-absent-p
        (return-from %websocket-window-bits nil)
        (%websocket-protocol-error
         "A WebSocket window-bits response parameter requires a value."
         parameter-name)))
  (unless (and (plusp (length value))
               (every #'digit-char-p value)
               (or (= (length value) 1)
                   (not (char= (char value 0) #\0))))
    (%websocket-protocol-error
     "A WebSocket window-bits parameter must be a decimal integer without leading zeroes."
     value))
  (let ((bits (parse-integer value)))
    (unless (<= 8 bits 15)
      (%websocket-protocol-error
       "A WebSocket window-bits parameter must be between 8 and 15."
       bits))
    bits))

(defun %validate-permessage-deflate (extension role)
  (dolist (parameter (websocket-extension-parameters extension))
    (let ((name (car parameter))
          (value (cdr parameter)))
      (cond ((member name '("server_no_context_takeover"
                            "client_no_context_takeover")
                     :test #'string=)
             (when value
               (%websocket-protocol-error
                "A no-context-takeover parameter cannot have a value."
                parameter)))
            ((string= name "server_max_window_bits")
             (%websocket-window-bits value name))
            ((string= name "client_max_window_bits")
             (%websocket-window-bits value name
                                     :allow-absent-p (eq role :offer)))
            (t
             (%websocket-protocol-error
              "An unsupported permessage-deflate parameter was supplied."
              name)))))
  extension)

(defun %websocket-parameter (extension name)
  (assoc name (websocket-extension-parameters extension) :test #'string=))

(defun %validate-selected-websocket-extensions (offered selected)
  (let ((selected-names '()))
    (dolist (extension selected)
      (let ((name (websocket-extension-name extension)))
        (when (and (string= name "permessage-deflate")
                   (member name selected-names :test #'string=))
          (%websocket-protocol-error
           "A server selected permessage-deflate more than once."
           name))
        (push name selected-names)
        (let ((offers (remove-if-not
                       (lambda (offer)
                         (string= name (websocket-extension-name offer)))
                       offered)))
          (unless offers
            (%websocket-protocol-error
             "The server selected a WebSocket extension that was not offered."
             name))
          (when (string= name "permessage-deflate")
            (mapc (lambda (offer)
                    (%validate-permessage-deflate offer :offer))
                  offers)
            (%validate-permessage-deflate extension :response)
            (let ((client-bits
                    (%websocket-parameter extension
                                          "client_max_window_bits")))
              (when (and client-bits
                         (notany (lambda (offer)
                                   (%websocket-parameter
                                    offer "client_max_window_bits"))
                                 offers))
                (%websocket-protocol-error
                 "The server selected client_max_window_bits without an offer."
                 client-bits))))))))
  selected)

(defun %websocket-valid-subprotocol-headers-p (headers)
  (let ((values (http-header-values headers "Sec-WebSocket-Protocol"))
        (seen '()))
    (or (null values)
        (handler-case
            (progn
              (dolist (value values)
                (unless (stringp value)
                  (%websocket-protocol-error
                   "A WebSocket subprotocol header must be a string."
                   value))
                (let ((start 0)
                      (length (length value)))
                  (loop
                    (let* ((comma (position #\, value :start start))
                           (end (or comma length))
                           (protocol
                             (string-trim '(#\Space #\Tab)
                                          (subseq value start end))))
                      (unless (%websocket-token-string-p protocol)
                        (%websocket-protocol-error
                         "A WebSocket subprotocol must be a token."
                         protocol))
                      (when (member protocol seen :test #'string=)
                        (%websocket-protocol-error
                         "WebSocket subprotocols must be unique."
                         protocol))
                      (push protocol seen)
                      (if comma
                          (setf start (1+ comma))
                          (return))))))
              t)
          (http-protocol-error () nil)))))

(defun websocket-upgrade-request-p (request)
  "Return true when REQUEST satisfies the RFC 6455 HTTP/1.1 handshake."
  (and (http-request-p request)
       (string= (http-request-method request) "GET")
       (string= (http-request-protocol-version request) "HTTP/1.1")
       (%websocket-header-has-token-p (http-request-headers request)
                                      "Upgrade" "websocket")
       (%websocket-header-has-token-p (http-request-headers request)
                                      "Connection" "upgrade")
       (%websocket-valid-subprotocol-headers-p
        (http-request-headers request))
       (handler-case
           (progn
             (dolist (extension
                       (%websocket-header-extensions
                        (http-request-headers request)))
               (when (string= (websocket-extension-name extension)
                              "permessage-deflate")
                 (%validate-permessage-deflate extension :offer)))
             t)
         (http-protocol-error () nil))
       (let ((version (%websocket-single-header-value
                       (http-request-headers request)
                       "Sec-WebSocket-Version"))
             (key (%websocket-single-header-value
                   (http-request-headers request)
                   "Sec-WebSocket-Key")))
         (and version
              (string= version "13")
              key
              (handler-case
                  (progn (websocket-accept-key key) t)
                (http-protocol-error () nil))))))

(defun %websocket-extra-header-name (header)
  (cond ((http-header-p header)
         (http-header-name header))
        ((and (consp header) (stringp (car header)))
         (car header))
        (t nil)))

(defun %websocket-reserved-header-p (name)
  (member (string-downcase name)
          '("upgrade" "connection" "sec-websocket-accept"
            "sec-websocket-protocol" "sec-websocket-extensions")
          :test #'string=))

(defun websocket-upgrade-response
    (request &key protocol extensions headers)
  "Create a validated HTTP 101 response for REQUEST.

PROTOCOL, when supplied, must have been offered by the client.  EXTENSIONS is
the already-negotiated extension value; this API does not silently negotiate
an extension it does not understand."
  (unless (websocket-upgrade-request-p request)
    (%websocket-protocol-error
     "An HTTP request does not satisfy the WebSocket upgrade handshake."))
  (when (and protocol
             (not (and (%websocket-token-string-p protocol) (%websocket-header-has-token-p
                       (http-request-headers request)
                       "Sec-WebSocket-Protocol"
                       protocol))))
    (%websocket-protocol-error
     "The selected WebSocket subprotocol was not offered by the client."
     protocol))
  (when extensions
    (unless (stringp extensions)
      (%websocket-protocol-error
       "WebSocket extensions must be a string or NIL."
       extensions))
    (%validate-selected-websocket-extensions
     (%websocket-header-extensions (http-request-headers request))
     (parse-websocket-extensions extensions)))
  (dolist (header headers)
    (let ((name (%websocket-extra-header-name header)))
      (when (and name (%websocket-reserved-header-p name))
        (%websocket-protocol-error
         "Custom WebSocket handshake headers cannot replace reserved headers."
         name))))
  (let ((response-headers
          (list (make-http-header "Upgrade" "websocket")
                (make-http-header "Connection" "Upgrade")
                (make-http-header
                 "Sec-WebSocket-Accept"
                 (websocket-accept-key
                  (%websocket-single-header-value
                   (http-request-headers request)
                   "Sec-WebSocket-Key"))))))
    (when protocol
      (setf response-headers
            (append response-headers
                    (list (make-http-header "Sec-WebSocket-Protocol"
                                             protocol)))))
    (when extensions
      (setf response-headers
            (append response-headers
                    (list (make-http-header "Sec-WebSocket-Extensions"
                                             extensions)))))
    (make-http-response :status 101
                        :reason "Switching Protocols"
                        :protocol-version "HTTP/1.1"
                        :headers (append response-headers headers))))

(defun %websocket-client-reserved-header-p (name)
  (member (string-downcase name)
          '("upgrade" "connection" "sec-websocket-version"
            "sec-websocket-key" "sec-websocket-protocol"
            "sec-websocket-extensions")
          :test #'string=))

(defun make-websocket-upgrade-request
    (uri &key key random-octets-function protocols extensions headers)
  "Create an HTTP/1.1 WebSocket client upgrade request for URI.

Supply exactly one of KEY, an already-generated Base64 Sec-WebSocket-Key, or
RANDOM-OCTETS-FUNCTION, a cryptographically secure source accepted by
MAKE-WEBSOCKET-CLIENT-KEY.  PROTOCOLS is a list of offered subprotocol tokens.
EXTENSIONS is an optional already-serialized Sec-WebSocket-Extensions value;
extension negotiation is intentionally left to the caller.

The reserved handshake headers are generated by this function and cannot be
overridden through HEADERS."
  (when (and key random-octets-function)
    (%websocket-protocol-error
     "Supply either a WebSocket client key or a random octet source, not both."
     nil))
  (when random-octets-function
    (setf key (make-websocket-client-key random-octets-function)))
  (unless (stringp key)
    (%websocket-protocol-error
     "Sec-WebSocket-Key must be a Base64 string."
     key))
  (let ((key (string-trim '(#\Space #\Tab) key)))
    (websocket-accept-key key)
    (unless (or (null protocols) (listp protocols))
      (%websocket-protocol-error
       "WebSocket subprotocols must be supplied as a list."
       protocols))
    (let ((seen-protocols '()))
      (dolist (protocol protocols)
        (unless (%websocket-token-string-p protocol)
          (%websocket-protocol-error
           "A WebSocket subprotocol must be a token."
           protocol))
        (when (member protocol seen-protocols :test #'string=)
          (%websocket-protocol-error
           "WebSocket subprotocols must be unique."
           protocol))
        (push protocol seen-protocols)))
    (when extensions
      (unless (stringp extensions)
        (%websocket-protocol-error
         "WebSocket extensions must be a string or NIL."
         extensions))
      (dolist (extension (parse-websocket-extensions extensions))
        (when (string= (websocket-extension-name extension)
                       "permessage-deflate")
          (%validate-permessage-deflate extension :offer))))
    (unless (listp headers)
      (%websocket-protocol-error
       "Additional WebSocket handshake headers must be a list."
       headers))
    (dolist (header headers)
      (let ((name (%websocket-extra-header-name header)))
        (when (and name (%websocket-client-reserved-header-p name))
          (%websocket-protocol-error
           "Custom WebSocket handshake headers cannot replace reserved headers."
           name))))
    (make-http-request
     :method "GET"
     :uri uri
     :headers
     (append
      (list (make-http-header "Upgrade" "websocket")
            (make-http-header "Connection" "Upgrade")
            (make-http-header "Sec-WebSocket-Version" "13")
            (make-http-header "Sec-WebSocket-Key" key))
      (when protocols
        (list (make-http-header "Sec-WebSocket-Protocol"
                                (format nil "~{~A~^, ~}" protocols))))
      (when extensions
        (list (make-http-header "Sec-WebSocket-Extensions" extensions)))
      headers))))

(defun websocket-client-handshake
    (stream request &key timeout deadline max-header-bytes max-body-bytes
                         (clock-function #'%monotonic-time))
  "Send REQUEST on STREAM and validate its RFC 6455 HTTP/1.1 response.

The HTTP response is returned as the primary value, the conservative
HTTP-RESPONSE-REUSABLE-P result as the second value, and the parsed negotiated
WEBSOCKET-EXTENSION list as the third value.  STREAM stays open, including
after a successful 101 response, so the caller can immediately use
READ-WEBSOCKET-FRAME or READ-WEBSOCKET-MESSAGE on it.  The caller owns the
stream and must close it when the handshake or subsequent WebSocket session
ends."
  (unless (streamp stream)
    (%websocket-protocol-error
     "The WebSocket client handshake requires an open stream."
     stream))
  (unless (websocket-upgrade-request-p request)
    (%websocket-protocol-error
     "An HTTP request does not satisfy the WebSocket upgrade handshake."
     request))
  (multiple-value-bind (response reusable-p)
      (send-http-request-over-open-stream
       request stream
       :timeout timeout
       :deadline deadline
       :max-header-bytes max-header-bytes
       :max-body-bytes max-body-bytes
       :collect-body-p nil
       :clock-function clock-function)
    (unless (and (http-response-p response)
                 (= 101 (http-response-status response))
                 (string= "HTTP/1.1"
                          (http-response-protocol-version response))
                 (%websocket-header-has-token-p
                  (http-response-headers response) "Upgrade" "websocket")
                 (%websocket-header-has-token-p
                  (http-response-headers response) "Connection" "upgrade"))
      (%websocket-protocol-error
       "The server response is not a valid WebSocket 101 upgrade response."
       response))
    (let* ((request-headers (http-request-headers request))
           (response-headers (http-response-headers response))
           (key (%websocket-single-header-value
                 request-headers "Sec-WebSocket-Key"))
           (accept (%websocket-single-header-value
                   response-headers "Sec-WebSocket-Accept")))
      (unless (and accept key
                   (string= accept (websocket-accept-key key)))
        (%websocket-protocol-error
         "The server returned an invalid Sec-WebSocket-Accept value."
         accept))
      (let ((selected-protocol-values
              (http-header-values response-headers "Sec-WebSocket-Protocol"))
            (requested-protocol-values
              (http-header-values request-headers "Sec-WebSocket-Protocol")))
        (when selected-protocol-values
          (let ((selected-protocol
                  (%websocket-single-header-value
                   response-headers "Sec-WebSocket-Protocol")))
            (unless (and (consp selected-protocol-values)
                         (null (cdr selected-protocol-values))
                         (%websocket-token-string-p selected-protocol)
                         requested-protocol-values
                         (%websocket-header-has-token-p
                          request-headers
                          "Sec-WebSocket-Protocol"
                          selected-protocol))
              (%websocket-protocol-error
               "The server selected an invalid WebSocket subprotocol."
               selected-protocol))))
        (%validate-selected-websocket-extensions
         (%websocket-header-extensions request-headers)
         (%websocket-header-extensions response-headers))))
    (values response reusable-p
            (%websocket-header-extensions
             (http-response-headers response)))))
