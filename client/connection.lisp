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
  (max-idle 16)
  idle-timeout
  max-connection-age
  clock-function
  (entries nil))

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

(defun %pool-purge-expired (pool &optional now)
  (when (or (%http-connection-pool-idle-timeout pool)
            (%http-connection-pool-max-connection-age pool))
    (let ((now (if now now (%pool-now pool)))
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
          (max-idle 16)
          idle-timeout
          max-connection-age
          (clock-function #'%pool-monotonic-time))
  "Construct a reusable, owner-thread HTTP connection pool.

OPEN-STREAM is called with REQUEST and :TIMEOUT, :DEADLINE, :PROXY-PLAN, and
:PROXY keyword arguments.  CLOSE-STREAM receives a stream when the pool
discards it.  The pool is intentionally synchronization-free: callers must
serialize access when they share one pool between threads."
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
                   :clock-function clock-function)
     :close-stream close-stream
     :tls-upgrade tls-upgrade
     :resolve-host resolve-host
     :max-idle max-idle
     :idle-timeout idle-timeout
     :max-connection-age max-connection-age
     :clock-function clock-function)))

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

(defun %pool-default-key (request)
  (http-uri-origin (http-request-uri request)))

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

(defun %pool-retain (pool key stream opened-at last-used)
  (let ((max-idle (%http-connection-pool-max-idle pool)))
    (if (zerop max-idle)
        (%pool-close pool stream)
        (progn
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
      (let ((stream
              (funcall (%http-connection-pool-open-stream pool)
                       request
                       :timeout timeout
                       :deadline deadline
                       :proxy-plan proxy-plan
                       :proxy proxy)))
        (unless (streamp stream)
          (error 'http-connection-error
                 :message "The connection pool opener did not return a stream."
                 :operation :connect
                 :cause stream))
        stream)
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
  (%pool-purge-expired pool)
  (list :idle-count (length (%http-connection-pool-entries pool))
        :max-idle (%http-connection-pool-max-idle pool)
        :idle-timeout (%http-connection-pool-idle-timeout pool)
        :max-connection-age
        (%http-connection-pool-max-connection-age pool)))

(defun http-connection-pool-clear (pool)
  "Close and remove every idle connection in POOL, returning its count."
  (%pool-ensure-pool pool)
  (let ((entries (%http-connection-pool-entries pool)))
    (setf (%http-connection-pool-entries pool) nil)
    (dolist (entry entries)
      (%pool-close pool (%http-pooled-connection-stream entry)))
    (length entries)))

(defun http-connection-pool-send
    (pool request &key key timeout deadline max-header-bytes max-fields max-body-bytes
                     proxy-plan proxy request-target request-body-function
                     request-body-length on-body-chunk on-information
                     (collect-body-p t))
  "Send REQUEST using an idle connection or a newly opened stream.

The request is never retried automatically after a transport failure.  A
stream is retained only when the core parser proves that the response is
self-delimited and neither side requested connection closure."
  (%pool-ensure-pool pool)
  (unless (http-request-p request)
    (%pool-protocol-error "The pooled request must be an HTTP-REQUEST." request))
  (let ((key (or key (%pool-default-key request)))
        (stream nil)
        (opened-at nil)
        (retained-p nil))
    (handler-case
        (with-http-deadline
            (absolute-deadline timeout
                               :inherited deadline
                               :clock-function #'%pool-monotonic-time)
          (%pool-purge-expired pool)
          (let ((entry (%pool-take pool key)))
            (if entry
                (progn
                  (setf stream (%http-pooled-connection-stream entry)
                        opened-at (%http-pooled-connection-opened-at entry)))
                (progn
                  (setf opened-at (%pool-now pool)
                        stream (%pool-open
                                pool request
                                :timeout timeout
                                :deadline absolute-deadline
                                :proxy-plan proxy-plan
                                :proxy proxy))))
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
                     (and (eq (getf proxy-plan :mode) :forward)
                          (getf proxy-plan :request-target)))
                 :request-body-function request-body-function
                 :request-body-length request-body-length
                 :on-body-chunk on-body-chunk
                 :on-information on-information
                 :collect-body-p collect-body-p
                 :clock-function #'%pool-monotonic-time)
              (if reusable-p
                  (progn
                    (%pool-retain pool key stream opened-at (%pool-now pool))
                    (setf retained-p t))
                  (%pool-close pool stream))
              (setf stream nil)
              response)))
      (http-error (condition)
        (unless retained-p
          (%pool-close pool stream))
        (error condition))
      (error (condition)
        (unless retained-p
          (%pool-close pool stream))
        (error 'http-connection-error
               :message "The pooled HTTP exchange signaled an error."
               :operation :transport
               :cause condition)))))
