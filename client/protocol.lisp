(in-package #:http-kit/client)

(defun %protocol-name-string (protocol)
  (cond ((stringp protocol) (string-downcase protocol))
        ((symbolp protocol) (string-downcase (symbol-name protocol)))
        (t nil)))

(defun http-alpn-protocol-name (protocol)
  "Return the canonical ALPN name for a supported HTTP protocol.

The result is one of \"http/1.1\", \"h2\", or \"h3\", or NIL for a
protocol that this library does not negotiate automatically."
  (let ((name (%protocol-name-string protocol)))
    (cond ((member name '("http/1.1" "http1" "http-1.1" "http-1-1")
                          :test #'string=)
           "http/1.1")
          ((member name '("h2" "http2" "http-2") :test #'string=)
           "h2")
          ((member name '("h3" "http3" "http-3") :test #'string=)
           "h3")
          (t nil))))

(defun http-select-protocol (offered supported)
  "Select the first protocol in OFFERED that is present in SUPPORTED.

Both arguments may contain strings or symbols.  Only HTTP/1.1, HTTP/2, and
HTTP/3 ALPN names are recognized; the canonical selected name is returned."
  (let ((supported-names
          (remove nil (mapcar #'http-alpn-protocol-name supported))))
    (dolist (protocol offered)
      (let ((name (http-alpn-protocol-name protocol)))
        (when (and name (member name supported-names :test #'string=))
          (return name))))))
