(in-package #:http-kit/network)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (require :sb-bsd-sockets))

(defun %network-monotonic-time ()
  (/ (float (get-internal-real-time))
     (float internal-time-units-per-second)))

(defun %network-unsupported ()
  (error 'http-unsupported-feature
         :message "The native TCP network boundary is only available on SBCL."
         :operation :network
         :detail :sbcl-required
         :feature :native-tcp))

(defun %network-check-deadline (deadline clock-function &optional (kind :connect))
  (when (and deadline (>= (funcall clock-function) deadline))
    (error 'http-timeout
           :message "The network operation exceeded its deadline."
           :operation kind
           :kind kind)))

(defun %network-remaining (deadline clock-function &optional (kind :connect))
  (%network-check-deadline deadline clock-function kind)
  (when deadline
    (- deadline (funcall clock-function))))

(defun %network-connection-error (host port operation &optional cause)
  (error 'http-connection-error
         :message (format nil "The network ~A operation failed for ~A:~A."
                          operation host port)
         :operation operation
         :cause cause))

(defun %network-effective-port (uri)
  (or (http-uri-port uri)
      (if (string= (http-uri-scheme uri) "https") 443 80)))

(defun %network-endpoint (request proxy-plan)
  (let* ((uri (http-request-uri request))
         (mode (and proxy-plan (getf proxy-plan :mode))))
    (cond
      ((or (null proxy-plan) (eq mode :direct))
       (values (http-uri-host uri) (%network-effective-port uri)))
      ((eq mode :forward)
       (values (getf proxy-plan :connect-host)
               (getf proxy-plan :connect-port)))
      ((member mode '(:connect :socks5))
       (values (getf proxy-plan :proxy-host)
               (getf proxy-plan :proxy-port)))
      (t
       (%network-connection-error
        (or (http-uri-host uri) "<unknown>")
        (%network-effective-port uri)
        :endpoint
        proxy-plan)))))

(defun %network-valid-port-p (port)
  (and (integerp port) (<= 1 port 65535)))

#+sbcl
(progn
  (defconstant +network-ipv4-address-type+ 2)
  (defconstant +network-ipv6-address-type+ 30)

  (defun %network-address-entries (host)
    (unless (and (stringp host) (string/= host ""))
      (%network-connection-error (or host "<unknown>") 0 :resolve))
    (multiple-value-bind (first second)
        (sb-bsd-sockets:get-host-by-name host)
      (loop for host-entry in (remove nil (list first second))
            for address-type =
              (sb-bsd-sockets:host-ent-address-type host-entry)
            when (member address-type
                         (list +network-ipv4-address-type+
                               +network-ipv6-address-type+))
            append (loop for address
                           in (sb-bsd-sockets:host-ent-addresses host-entry)
                         collect (list address-type address)))))

  (defun %network-address-text (address-type address)
    (if (= address-type +network-ipv4-address-type+)
        (format nil "~{~D~^.~}" (coerce address 'list))
        (with-output-to-string (stream)
          (loop for index from 0 below 16 by 2
                for first = t then nil
                do (unless first
                     (write-char #\: stream))
                   (format stream "~(~4,'0X~)"
                           (+ (ash (aref address index) 8)
                              (aref address (1+ index))))))))

  (defun %network-socket-class (address-type)
    (if (= address-type +network-ipv4-address-type+)
        'sb-bsd-sockets:inet-socket
        'sb-bsd-sockets:inet6-socket))

  (defstruct (http-network-listener
               (:constructor %make-http-network-listener))
    socket
    address
    port
    address-family
    closed-p)

  (defun %network-listener-error (operation &optional detail)
    (error 'http-protocol-error
           :message (format nil "The native TCP listener ~A operation is invalid."
                            operation)
           :operation operation
           :detail detail))

  (defun %network-address-family (address-type)
    (if (= address-type +network-ipv4-address-type+)
        :ipv4
        :ipv6))

  (defun %network-listener-address-entry
      (host ipv6-p deadline clock-function)
    (cond
      ((null host)
       (list (if ipv6-p
                 +network-ipv6-address-type+
                 +network-ipv4-address-type+)
             (if ipv6-p
                 (make-array 16
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)
                 #(0 0 0 0))))
      ((not (stringp host))
       (%network-connection-error host 0 :listen-resolve))
      (t
       (let* ((entries (%network-resolve-entries
                        host deadline clock-function))
              (preferred-type
                (cond
                  (ipv6-p +network-ipv6-address-type+)
                  ((find #\: host) +network-ipv6-address-type+)
                  (t +network-ipv4-address-type+)))
              (entry (or (find preferred-type entries :key #'first)
                         (first entries))))
         (unless entry
           (%network-connection-error host 0 :listen-resolve))
         entry))))

  (defun open-http-tcp-listener
      (&key host (port 0) (backlog 128) (reuse-address t)
              ipv6-p timeout deadline
              (clock-function #'%network-monotonic-time))
    "Bind and listen on a native TCP socket.

HOST defaults to the IPv4 wildcard address.  Set IPV6-P to true to use the
IPv6 wildcard address when HOST is omitted.  PORT may be zero, in which case
the operating system chooses an available port.  The returned listener is
consumed by ACCEPT-HTTP-TCP-STREAM and should be closed with
CLOSE-HTTP-TCP-LISTENER."
    (unless (or (null host) (stringp host))
      (%network-listener-error :open host))
    (unless (and (integerp port) (<= 0 port 65535))
      (%network-listener-error :open (list :port port)))
    (unless (and (integerp backlog) (plusp backlog))
      (%network-listener-error :open (list :backlog backlog)))
    (unless (functionp clock-function)
      (%network-listener-error :open :clock-function))
    (let ((effective-deadline
            (http-deadline timeout
                           :deadline deadline
                           :clock-function clock-function))
          (socket nil)
          (retained-p nil))
      (unwind-protect
           (destructuring-bind (address-type address)
               (%network-listener-address-entry
                host ipv6-p effective-deadline clock-function)
             (setf socket
                   (make-instance (%network-socket-class address-type)
                                  :type :stream
                                  :protocol :tcp))
             (when reuse-address
               (setf (sb-bsd-sockets:sockopt-reuse-address socket) t))
             (sb-bsd-sockets:socket-bind socket address port)
             (sb-bsd-sockets:socket-listen socket backlog)
             (multiple-value-bind (bound-address bound-port)
                 (sb-bsd-sockets:socket-name socket)
               (setf retained-p t)
               (%make-http-network-listener
                :socket socket
                :address (%network-address-text address-type bound-address)
                :port bound-port
                :address-family (%network-address-family address-type)
                :closed-p nil)))
        (unless retained-p
          (when socket
            (http-kit::%with-http-cleanup
              (sb-bsd-sockets:socket-close socket)))))))

  (defun %network-accept-socket
      (listener deadline clock-function)
    (let ((remaining (%network-remaining deadline clock-function :accept)))
      (if remaining
          (handler-case
              (sb-ext:with-timeout remaining
                (sb-bsd-sockets:socket-accept
                 (http-network-listener-socket listener)))
            (sb-ext:timeout ()
              (error 'http-timeout
                     :message "The TCP accept operation exceeded its deadline."
                     :operation :accept
                     :kind :accept)))
          (sb-bsd-sockets:socket-accept
           (http-network-listener-socket listener)))))

  (defun accept-http-tcp-stream
      (listener &key timeout deadline
                         (clock-function #'%network-monotonic-time))
    "Accept one TCP connection from LISTENER.

Returns three values: a binary stream, the numeric peer address as a string,
and the peer port.  TIMEOUT and DEADLINE apply to accept and become the
stream's read timeout after a connection is accepted."
    (unless (http-network-listener-p listener)
      (%network-listener-error :accept listener))
    (when (or (http-network-listener-closed-p listener)
              (null (http-network-listener-socket listener)))
      (%network-listener-error :accept :closed-listener))
    (unless (functionp clock-function)
      (%network-listener-error :accept :clock-function))
    (let ((effective-deadline
            (http-deadline timeout
                           :deadline deadline
                           :clock-function clock-function))
          (accepted-socket nil)
          (stream nil)
          (retained-p nil))
      (unwind-protect
           (progn
             (multiple-value-bind (socket ignored-address)
                 (%network-accept-socket listener
                                         effective-deadline
                                         clock-function)
               (declare (ignore ignored-address))
               (setf accepted-socket socket)
               (let ((remaining (%network-remaining effective-deadline
                                                      clock-function
                                                      :accept)))
                 (setf stream
                       (if remaining
                           (sb-bsd-sockets:socket-make-stream
                            socket
                            :input t
                            :output t
                            :element-type '(unsigned-byte 8)
                            :buffering :full
                            :timeout remaining)
                           (sb-bsd-sockets:socket-make-stream
                            socket
                            :input t
                            :output t
                            :element-type '(unsigned-byte 8)
                            :buffering :full))))
               (multiple-value-bind (peer-address peer-port)
                   (sb-bsd-sockets:socket-peername socket)
                 (setf retained-p t)
                 (values stream
                         (%network-address-text
                          (if (= (length peer-address) 4)
                              +network-ipv4-address-type+
                              +network-ipv6-address-type+)
                          peer-address)
                         peer-port))))
        (unless retained-p
          (when stream
            (http-kit::%with-http-cleanup
              (close stream :abort t)))
          (when accepted-socket
            (http-kit::%with-http-cleanup
              (sb-bsd-sockets:socket-close accepted-socket)))))))

  (defun %network-connect-socket (socket address port deadline clock-function)
    (let ((remaining (%network-remaining deadline clock-function :connect)))
      (if remaining
          (handler-case
              (sb-ext:with-timeout remaining
                (sb-bsd-sockets:socket-connect socket address port))
            (sb-ext:timeout ()
              (error 'http-timeout
                     :message "The TCP connection exceeded its deadline."
                     :operation :connect
                     :kind :connect)))
          (sb-bsd-sockets:socket-connect socket address port))))

  (defun %network-open-address
      (address-entry port deadline clock-function)
    (destructuring-bind (address-type address) address-entry
      (let ((socket nil)
            (stream nil)
            (retained-p nil))
        (unwind-protect
             (progn
               (setf socket
                     (make-instance (%network-socket-class address-type)
                                    :type :stream
                                    :protocol :tcp))
               (%network-connect-socket socket address port
                                        deadline clock-function)
               (let ((remaining (%network-remaining deadline
                                                      clock-function
                                                      :connect)))
                 (setf stream
                       (if remaining
                           (sb-bsd-sockets:socket-make-stream
                            socket
                            :input t
                            :output t
                            :element-type '(unsigned-byte 8)
                            :buffering :full
                            :timeout remaining)
                           (sb-bsd-sockets:socket-make-stream
                            socket
                            :input t
                            :output t
                            :element-type '(unsigned-byte 8)
                            :buffering :full))))
               (setf retained-p t)
               stream)
          (unless retained-p
            (when stream
              (http-kit::%with-http-cleanup
                (close stream :abort t)))
            (when socket
              (http-kit::%with-http-cleanup
                (sb-bsd-sockets:socket-close socket)))))))))

  (defun %network-resolve-entries (host deadline clock-function)
    (%network-check-deadline deadline clock-function :resolve)
    (handler-case
        (let ((entries (%network-address-entries host)))
          (%network-check-deadline deadline clock-function :resolve)
          entries)
      (http-error (condition)
        (error condition))
      (error (condition)
        (%network-connection-error host 0 :resolve condition))))

  (defun http-network-resolve-host
      (host &key timeout deadline (clock-function #'%network-monotonic-time))
    "Resolve HOST to a numeric address string for SOCKS5 local resolution."
    (let ((effective-deadline
            (http-deadline timeout
                           :deadline deadline
                           :clock-function clock-function)))
      (let ((entry (first (%network-resolve-entries
                           host effective-deadline clock-function))))
        (unless entry
          (%network-connection-error host 0 :resolve))
        (%network-address-text (first entry) (second entry)))))

  (defun open-http-tcp-stream
      (request &key timeout deadline proxy-plan
                 (clock-function #'%network-monotonic-time))
    "Open a binary TCP stream to REQUEST's endpoint or its proxy endpoint.

This is the raw endpoint boundary consumed by
HTTP-KIT/CLIENT:MAKE-HTTP-PROXY-STREAM-OPENER.  It deliberately does not
perform TLS or proxy negotiation."
    (multiple-value-bind (host port) (%network-endpoint request proxy-plan)
      (unless (and (stringp host) (string/= host "")
                   (%network-valid-port-p port))
        (%network-connection-error (or host "<unknown>") (or port 0)
                                   :endpoint proxy-plan))
      (let ((effective-deadline
              (http-deadline timeout
                             :deadline deadline
                             :clock-function clock-function)))
        (let ((entries (%network-resolve-entries
                        host effective-deadline clock-function))
              (last-cause nil))
          (dolist (entry entries)
            (handler-case
                (return-from open-http-tcp-stream
                  (%network-open-address entry port effective-deadline
                                         clock-function))
              (http-timeout (condition)
                (error condition))
              (http-error (condition)
                (setf last-cause condition))
              (error (condition)
                (setf last-cause condition))))
          (%network-connection-error host port :connect last-cause)))))
#-sbcl
(progn
  (defun http-network-resolve-host
      (host &key timeout deadline (clock-function #'%network-monotonic-time))
    (declare (ignore host timeout deadline clock-function))
    (%network-unsupported))

  (defun open-http-tcp-stream
      (request &key timeout deadline proxy-plan
                 (clock-function #'%network-monotonic-time))
    (declare (ignore request timeout deadline proxy-plan clock-function))
    (%network-unsupported))

  (defun open-http-tcp-listener
      (&key host port backlog reuse-address ipv6-p timeout deadline
              (clock-function #'%network-monotonic-time))
    (declare (ignore host port backlog reuse-address ipv6-p timeout deadline
                     clock-function))
    (%network-unsupported))

  (defun accept-http-tcp-stream
      (listener &key timeout deadline
                         (clock-function #'%network-monotonic-time))
    (declare (ignore listener timeout deadline clock-function))
    (%network-unsupported))

  (defun http-network-listener-p (listener)
    (declare (ignore listener))
    nil)

  (defun http-network-listener-address (listener)
    (declare (ignore listener))
    (%network-unsupported))

  (defun http-network-listener-port (listener)
    (declare (ignore listener))
    (%network-unsupported))

  (defun http-network-listener-address-family (listener)
    (declare (ignore listener))
    (%network-unsupported))

  (defun close-http-tcp-listener (listener)
    (declare (ignore listener))
    (%network-unsupported)))

#+sbcl
(defun close-http-tcp-listener (listener)
  "Close a listener returned by OPEN-HTTP-TCP-LISTENER."
  (unless (http-network-listener-p listener)
    (%network-listener-error :close listener))
  (unless (http-network-listener-closed-p listener)
    (setf (http-network-listener-closed-p listener) t)
    (let ((socket (http-network-listener-socket listener)))
      (setf (http-network-listener-socket listener) nil)
      (when socket
        (http-kit::%with-http-cleanup
          (sb-bsd-sockets:socket-close socket)))))
  nil)

(defun close-http-tcp-stream (stream)
  "Close a stream returned by OPEN-HTTP-TCP-STREAM."
  (when stream
    (http-kit::%with-http-cleanup
      (close stream :abort t)))
  nil)

#+sbcl
(progn
  (defun %network-keyword-plist-p (plist)
    (loop
      (cond
        ((null plist) (return t))
        ((or (not (consp plist))
             (not (consp (cdr plist))))
         (return nil))
        ((not (keywordp (car plist)))
         (return nil))
        (t
         (setf plist (cddr plist))))))

  (defun %network-plist-key-p (key plist)
    (loop
      (cond
        ((null plist) (return nil))
        ((or (not (consp plist))
             (not (consp (cdr plist))))
         (return nil))
        ((eq key (car plist))
         (return t))
        (t
         (setf plist (cddr plist))))))

  (defun serve-http1-listener
      (listener handler &key max-connections accept-timeout accept-deadline
                         on-accept on-error (session-options nil)
                         (clock-function #'%network-monotonic-time))
    "Serve HTTP/1 sessions accepted from LISTENER.

The listener remains owned by the caller and is not closed by this function.
HANDLER and SESSION-OPTIONS are passed to HTTP-KIT:SERVE-HTTP1-SESSION for
each accepted connection.  SESSION-OPTIONS may contain any session keyword;
unless it contains :CLOSE-STREAM, the accepted stream is closed by the
session boundary.  ON-ACCEPT receives STREAM, PEER-ADDRESS, and PEER-PORT.
ON-ERROR receives CONDITION, PEER-ADDRESS, and PEER-PORT; session errors are
reported and the listener continues accepting, while accept errors are
re-signaled after the callback.  ACCEPT-TIMEOUT is applied to each accept.

Returns the number of accepted connections and either :MAX-CONNECTIONS or
:TIMEOUT.  MAX-CONNECTIONS of zero returns without accepting a connection."
    (unless (http-network-listener-p listener)
      (%network-listener-error :serve listener))
    (unless (functionp handler)
      (%network-listener-error :serve (list :handler handler)))
    (unless (or (null max-connections)
                (and (integerp max-connections)
                     (not (minusp max-connections))))
      (%network-listener-error :serve
                               (list :max-connections max-connections)))
    (unless (or (null on-accept) (functionp on-accept))
      (%network-listener-error :serve (list :on-accept on-accept)))
    (unless (or (null on-error) (functionp on-error))
      (%network-listener-error :serve (list :on-error on-error)))
    (unless (%network-keyword-plist-p session-options)
      (%network-listener-error :serve
                               (list :session-options session-options)))
    (unless (functionp clock-function)
      (%network-listener-error :serve (list :clock-function clock-function)))
    (let ((effective-session-options
            (if (%network-plist-key-p :close-stream session-options)
                session-options
                (append session-options
                        (list :close-stream #'close-http-tcp-stream))))
          (count 0)
          (termination :running))
      (loop while (eq termination :running)
            do (cond
                 ((and max-connections
                       (>= count max-connections))
                  (setf termination :max-connections))
                 (t
                  (handler-case
                      (multiple-value-bind (stream peer-address peer-port)
                          (accept-http-tcp-stream
                           listener
                           :timeout accept-timeout
                           :deadline accept-deadline
                           :clock-function clock-function)
                        (incf count)
                        (handler-case
                            (progn
                              (when on-accept
                                (funcall on-accept
                                         stream peer-address peer-port))
                              (apply #'serve-http1-session
                                     stream
                                     handler
                                     effective-session-options))
                          (error (condition)
                            (close-http-tcp-stream stream)
                            (when on-error
                              (funcall on-error
                                       condition peer-address peer-port)))
                    (http-timeout (condition)
                      (when on-error
                        (funcall on-error condition nil nil))
                      (setf termination :timeout))
                    (error (condition)
                      (when on-error
                        (funcall on-error condition nil nil))
                      (error condition))))))))
      (values count termination))))

(defun make-http-network-stream-opener
    (&key (clock-function #'%network-monotonic-time))
  "Return a raw stream opener backed by native TCP and DNS.

Use this as the :OPEN-STREAM function of HTTP-KIT/CLIENT:MAKE-HTTP-CLIENT.
TLS, ALPN, and proxy negotiation remain policy callbacks at the client layer."
  (unless (functionp clock-function)
    (error 'http-protocol-error
           :message "The network clock must be a function."
           :operation :network
           :detail clock-function))
  (lambda (request &key timeout deadline proxy-plan proxy &allow-other-keys)
    (declare (ignore proxy))
    (open-http-tcp-stream request
                          :timeout timeout
                          :deadline deadline
                          :proxy-plan proxy-plan
                          :clock-function clock-function)))

#-sbcl
(defun serve-http1-listener
    (listener handler &key max-connections accept-timeout accept-deadline
                   on-accept on-error session-options
                   (clock-function #'%network-monotonic-time))
  (declare (ignore listener handler max-connections accept-timeout
                   accept-deadline on-accept on-error session-options
                   clock-function))
  (%network-unsupported))
