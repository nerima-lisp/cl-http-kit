(in-package #:http-kit/client)

(defstruct (http-pooled-connection
             (:constructor %make-http-pooled-connection)
             (:conc-name %http-pooled-connection-))
  key
  stream
  opened-at
  last-used)

(defstruct (http-connection-pool
             (:constructor %make-http-connection-pool)
             (:conc-name %http-connection-pool-))
  open-stream
  close-stream
  tls-upgrade
  resolve-host
  resolve-host-style
  (max-idle 16)
  idle-timeout
  max-connection-age
  clock-function
  lock
  (generation 0)
  (clear-callbacks nil)
  (entries nil))

(defvar *client-preopened-http-stream* nil)

(define-condition %http-client-alpn-handoff (error)
  ((stream :initarg :stream :reader %http-client-alpn-handoff-stream)))
  (defun %pool-monotonic-time ()
  (/ (float (get-internal-real-time))
     internal-time-units-per-second))

(defun %pool-protocol-error (message detail)
  (error 'http-protocol-error
         :message message
         :operation :connection-pool
         :detail detail))

(defun %pool-ensure-function (value message)
  (unless (functionp value)
    (%pool-protocol-error message value))
  value)

(defun %pool-ensure-pool (pool)
  (unless (http-connection-pool-p pool)
    (%pool-protocol-error
     "The connection pool must be an HTTP-CONNECTION-POOL."
     pool))
  pool)

(defun %http-connection-pool-register-clear-callback (pool callback)
  (%pool-ensure-pool pool)
  (%pool-ensure-function
   callback
   "A connection pool clear callback must be a function.")
  (%with-client-lock ((%http-connection-pool-lock pool))
    (pushnew callback
             (%http-connection-pool-clear-callbacks pool)
             :test (function eq)))
  callback)

(defun %pool-now (pool)
  (let ((now (funcall (%http-connection-pool-clock-function pool))))
    (unless (realp now)
      (%pool-protocol-error
       "The connection pool clock must return a real number."
       now))
    now))

(defun %pool-close (pool stream)
  (when stream
    (http-kit::%with-http-cleanup
      (funcall (%http-connection-pool-close-stream pool) stream)))
  nil)

(defun %pool-expired-p (pool entry now)
  (let ((idle-timeout (%http-connection-pool-idle-timeout pool))
        (max-age (%http-connection-pool-max-connection-age pool)))
    (or (and idle-timeout
             (>= (- now (%http-pooled-connection-last-used entry))
                 idle-timeout))
        (and max-age
             (>= (- now (%http-pooled-connection-opened-at entry))
                 max-age)))))

(defun %pool-purge-expired (pool)
  (when (or (%http-connection-pool-idle-timeout pool)
            (%http-connection-pool-max-connection-age pool))
    (let ((now (%pool-now pool))
          (retained nil))
      (dolist (entry (%http-connection-pool-entries pool))
        (if (%pool-expired-p pool entry now)
            (%pool-close pool (%http-pooled-connection-stream entry))
            (push entry retained)))
      (setf (%http-connection-pool-entries pool) (nreverse retained))))
  pool)

(defun make-http-connection-pool
    (&key open-stream close-stream
          tls-upgrade
          resolve-host
          (resolve-host-style :keywords)
          (max-idle 16)
          (idle-timeout 60.0)
          (max-connection-age 300.0)
          (clock-function #'%pool-monotonic-time))
  "Construct a reusable, thread-safe HTTP connection pool.

OPEN-STREAM is called with REQUEST and :TIMEOUT, :DEADLINE, :PROXY-PLAN, and
:PROXY keyword arguments.  CLOSE-STREAM receives a stream when the pool
discards it.  RESOLVE-HOST-STYLE is :KEYWORDS by default; use :HOST-ONLY for
a legacy RESOLVE-HOST callback that accepts only HOST.  Pool operations may
be used concurrently from multiple threads."
  (%pool-ensure-function open-stream
                         "A connection pool requires an :OPEN-STREAM function.")
  (when close-stream
    (%pool-ensure-function close-stream
                           "The connection pool close function must be a function."))
  (when tls-upgrade
    (%pool-ensure-function tls-upgrade
                           "The connection pool TLS upgrade function must be a function."))
  (when resolve-host
    (%pool-ensure-function resolve-host
                           "The connection pool host resolver must be a function."))
  (unless (member resolve-host-style '(:keywords :host-only) :test #'eq)
    (%pool-protocol-error
     "The connection pool host resolver style must be :KEYWORDS or :HOST-ONLY."
     resolve-host-style))
  (unless (and (integerp max-idle) (>= max-idle 0))
    (%pool-protocol-error
     "The connection pool maximum idle count must be a non-negative integer."
     max-idle))
  (when (and idle-timeout
             (or (not (realp idle-timeout)) (< idle-timeout 0)))
    (%pool-protocol-error
     "The connection pool idle timeout must be a non-negative real or NIL."
     idle-timeout))
  (when (and max-connection-age
             (or (not (realp max-connection-age)) (< max-connection-age 0)))
    (%pool-protocol-error
     "The connection pool maximum connection age must be a non-negative real or NIL."
     max-connection-age))
  (%pool-ensure-function clock-function
                         "The connection pool clock must be a function.")
  (let ((close-stream (or close-stream #'close)))
    (%make-http-connection-pool
     :open-stream (make-http-proxy-stream-opener
                   open-stream close-stream
                   :tls-upgrade tls-upgrade
                   :resolve-host resolve-host
                   :resolve-host-style resolve-host-style
                   :clock-function clock-function)
     :close-stream close-stream
     :tls-upgrade tls-upgrade
     :resolve-host resolve-host
     :resolve-host-style resolve-host-style
     :max-idle max-idle
     :idle-timeout idle-timeout
     :max-connection-age max-connection-age
     :clock-function clock-function
     :lock (%make-client-lock "http-connection-pool"))))

(defun http-connection-pool-max-idle (pool)
  "Return the maximum number of idle streams retained by POOL."
  (%http-connection-pool-max-idle (%pool-ensure-pool pool)))

(defun http-connection-pool-idle-timeout (pool)
  "Return POOL's idle-stream expiry interval, or NIL when disabled."
  (%http-connection-pool-idle-timeout (%pool-ensure-pool pool)))

(defun http-connection-pool-max-connection-age (pool)
  "Return POOL's maximum connection lifetime, or NIL when disabled."
  (%http-connection-pool-max-connection-age (%pool-ensure-pool pool)))

(defun http-connection-pool-clock-function (pool)
  "Return the clock function used by POOL."
  (%http-connection-pool-clock-function (%pool-ensure-pool pool)))

(defun http-connection-pool-tls-upgrade (pool)
  "Return POOL's optional TLS stream-upgrade function."
  (%http-connection-pool-tls-upgrade (%pool-ensure-pool pool)))

(defun http-connection-pool-resolve-host (pool)
  "Return POOL's optional host resolver function."
  (%http-connection-pool-resolve-host (%pool-ensure-pool pool)))

(defun %pool-default-key (request proxy-plan proxy)
  (let ((effective-proxy (or proxy (and proxy-plan (getf proxy-plan :proxy))))
        (origin (http-uri-origin (http-request-uri request))))
    (list origin
          (and proxy-plan (getf proxy-plan :mode))
          (and proxy-plan (getf proxy-plan :connect-host))
          (and proxy-plan (getf proxy-plan :connect-port))
          (and proxy-plan (getf proxy-plan :proxy-host))
          (and proxy-plan (getf proxy-plan :proxy-port))
          (and proxy-plan (getf proxy-plan :remote-dns-p))
          (and proxy-plan (getf proxy-plan :proxy-authorization))
          (when effective-proxy
            (list (http-proxy-scheme effective-proxy)
                  (http-proxy-host effective-proxy)
                  (http-proxy-port effective-proxy)
                  (http-proxy-username effective-proxy)
                  (http-proxy-password effective-proxy))))))

(defun %pool-take (pool key)
  (let ((entry (find key
                    (%http-connection-pool-entries pool)
                    :key #'%http-pooled-connection-key
                    :test #'equal)))
    (when entry
      (setf (%http-connection-pool-entries pool)
            (remove entry
                    (%http-connection-pool-entries pool)
                    :test #'eq)))
    entry))

(defun %pool-evict-oldest (pool)
  (let ((entries (%http-connection-pool-entries pool)))
    (when entries
      (let ((oldest
              (car (sort (copy-list entries)
                         #'<
                         :key #'%http-pooled-connection-last-used))))
        (setf (%http-connection-pool-entries pool)
              (remove oldest entries :test #'eq))
        (%pool-close pool (%http-pooled-connection-stream oldest))))))

(defun %pool-retain (pool key stream opened-at last-used generation)
  (let ((max-idle (%http-connection-pool-max-idle pool)))
    (cond
      ((/= generation (%http-connection-pool-generation pool))
       (%pool-close pool stream))
      ((zerop max-idle)
       (%pool-close pool stream))
      (t
       (when (>= (length (%http-connection-pool-entries pool)) max-idle)
         (%pool-evict-oldest pool))
       (push (%make-http-pooled-connection
              :key key
              :stream stream
              :opened-at opened-at
              :last-used last-used)
             (%http-connection-pool-entries pool)))))
  pool)

(defun %pool-open (pool request &key timeout deadline proxy-plan proxy)
  (handler-case
      (funcall (%http-connection-pool-open-stream pool)
               request
               :timeout timeout
               :deadline deadline
               :proxy-plan proxy-plan
               :proxy proxy)
    (http-error (condition)
      (error condition))
    (error (condition)
      (error 'http-connection-error
             :message "The connection pool opener signaled an error."
             :operation :connect
             :cause condition))))

(defun http-connection-pool-stats (pool)
  "Return non-sensitive idle connection statistics for POOL."
  (%pool-ensure-pool pool)
  (%with-client-lock ((%http-connection-pool-lock pool))
    (%pool-purge-expired pool)
    (list :idle-count (length (%http-connection-pool-entries pool))
          :max-idle (%http-connection-pool-max-idle pool)
          :idle-timeout (%http-connection-pool-idle-timeout pool)
          :max-connection-age
          (%http-connection-pool-max-connection-age pool))))

(defun http-connection-pool-clear (pool)
  "Close idle entries in POOL and invoke its clear callbacks. Return the number of idle entries removed."
  (%pool-ensure-pool pool)
  (let ((entries nil)
        (callbacks nil)
        (callback-error nil))
    (%with-client-lock ((%http-connection-pool-lock pool))
      (incf (%http-connection-pool-generation pool))
      (setf entries (%http-connection-pool-entries pool)
            callbacks (copy-list
                       (%http-connection-pool-clear-callbacks pool))
            (%http-connection-pool-entries pool) nil))
    (dolist (entry entries)
      (%pool-close pool (%http-pooled-connection-stream entry)))
    (dolist (callback callbacks)
      (handler-case
          (funcall callback)
        (error (condition)
          (unless callback-error
            (setf callback-error condition)))))
    (when callback-error
      (error callback-error))
    (length entries)))

(defun %http-connection-pool-send-with-alpn-handoff
    (pool request &key key timeout deadline max-header-bytes max-fields max-body-bytes
          proxy-plan proxy request-target request-body-function
          request-body-length on-body-chunk on-information
          on-response-headers on-cancel
          (collect-body-p t))
  (%pool-ensure-pool pool)
  (unless (http-request-p request)
    (%pool-protocol-error "The pooled request must be an HTTP-REQUEST." request))
  (let* ((effective-proxy-plan
          (if proxy
              (http-proxy-plan proxy (http-request-uri request))
            proxy-plan))
         (key
          (handler-case
              (or key (%pool-default-key request effective-proxy-plan proxy))
            (type-error (condition)
              (error 'http-connection-error
                     :message "The default connection pool key could not be derived."
                     :operation :connect
                     :request-sent-p nil
                     :cause condition))))
         (stream nil)
         (opened-at nil)
         (generation nil)
         (retained-p nil)
         (stream-closed-p nil)
         (stream-close-lock (%make-client-lock "http-pool-active-stream-close")))
    (flet ((close-current-stream ()
             (%with-client-lock (stream-close-lock)
               (when (and stream (not stream-closed-p))
                 (setf stream-closed-p t)
                 (%pool-close pool stream))
               nil)))
    (handler-case
        (with-http-deadline
         (absolute-deadline timeout
                            :inherited deadline
                            :clock-function #'%pool-monotonic-time)
         (let ((entry
                (%with-client-lock ((%http-connection-pool-lock pool))
                  (setf generation (%http-connection-pool-generation pool))
                  (unless *client-preopened-http-stream*
                    (%pool-purge-expired pool)
                    (%pool-take pool key)))))
           (cond
            (*client-preopened-http-stream*
             (setf stream (prog1 *client-preopened-http-stream*
                            (setf *client-preopened-http-stream* nil))
                   opened-at (%pool-now pool)))
            (entry
             (setf stream (%http-pooled-connection-stream entry)
                   opened-at (%http-pooled-connection-opened-at entry)))
            (t
             (setf opened-at (%pool-now pool))
             (multiple-value-bind (opened-stream negotiated-alpn)
                 (%pool-open pool request
                             :timeout timeout
                             :deadline absolute-deadline
                             :proxy-plan proxy-plan
                             :proxy proxy)
               (setf stream opened-stream)
               (cond
                ((equal negotiated-alpn "h2")
                 (setf stream nil)
                 (error '%http-client-alpn-handoff
                        :stream opened-stream))
                ((or (null negotiated-alpn)
                     (equal negotiated-alpn "http/1.1"))
                 nil)
                (t
                 (%pool-protocol-error
                  "The TLS opener selected an unsupported ALPN protocol."
                  negotiated-alpn))))))
           (multiple-value-bind (response reusable-p)
               (send-http-request-over-open-stream
                request
                stream
                :deadline absolute-deadline
                :max-header-bytes max-header-bytes
                :max-fields max-fields
                :max-body-bytes max-body-bytes
                :request-target
                (or request-target
                    (and (eq (getf effective-proxy-plan :mode) :forward)
                         (getf effective-proxy-plan :request-target)))
                :request-body-function request-body-function
                :request-body-length request-body-length
                :on-body-chunk on-body-chunk
                :on-information on-information
                :on-response-headers on-response-headers
                :on-cancel on-cancel
                :cancel-stream-function #'close-current-stream
                :collect-body-p collect-body-p
                :clock-function #'%pool-monotonic-time)
             (if reusable-p
                 (%with-client-lock (stream-close-lock)
                   (if stream-closed-p
                       nil
                       (progn
                         (%with-client-lock ((%http-connection-pool-lock pool))
                           (%pool-retain pool key stream opened-at (%pool-now pool)
                                         generation))
                         (setf retained-p t
                               stream nil))))
                 (close-current-stream))
             (unless retained-p
               (close-current-stream)
               (setf stream nil))
             response)))
      (%http-client-alpn-handoff (condition)
        (error condition))
      (http-error (condition)
        (unless retained-p
          (close-current-stream))
        (error condition))
      (error (condition)
        (unless retained-p
          (close-current-stream))
        (error 'http-connection-error
               :message "The pooled HTTP exchange signaled an error."
               :operation :transport
               :cause condition))))))

(defun http-connection-pool-send
    (pool request &key key timeout deadline max-header-bytes max-fields max-body-bytes
                     proxy-plan proxy request-target request-body-function
                     request-body-length on-body-chunk on-information
                     on-response-headers on-cancel
                     (collect-body-p t))
  "Send REQUEST using an idle connection or a newly opened stream.

The request is never retried automatically after a transport failure. A stream
is retained only when the core parser proves that the response is self-delimited
and neither side requested connection closure."
  (handler-case
      (%http-connection-pool-send-with-alpn-handoff
       pool request
       :key key
       :timeout timeout
       :deadline deadline
       :max-header-bytes max-header-bytes
       :max-fields max-fields
       :max-body-bytes max-body-bytes
       :proxy-plan proxy-plan
       :proxy proxy
       :request-target request-target
       :request-body-function request-body-function
       :request-body-length request-body-length
       :on-body-chunk on-body-chunk
       :on-information on-information
       :on-response-headers on-response-headers
       :on-cancel on-cancel
       :collect-body-p collect-body-p)
    (%http-client-alpn-handoff (condition)
      (%pool-close pool (%http-client-alpn-handoff-stream condition))
      (%pool-protocol-error
       "HTTP/2 ALPN requires the negotiated client transport."
       :http2))))

(defun %http-connection-pool-send-on-stream
    (pool request stream &rest arguments)
  (let ((*client-preopened-http-stream* stream))
    (unwind-protect
         (apply #'%http-connection-pool-send-with-alpn-handoff pool request arguments)
      (when *client-preopened-http-stream*
        (%pool-close pool *client-preopened-http-stream*)))))
