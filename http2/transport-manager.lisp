(in-package #:http-kit/http2)

(defstruct (%http2-managed-connection
            (:constructor %make-http2-managed-connection
                          (&key key connection opened-at last-used last-used-at
                                active-count transient-p exclusive-p retired-p close-started-p)))
  key
  connection
  opened-at
  last-used
  last-used-at
  (active-count 0)
  transient-p
  exclusive-p
  retired-p
  close-started-p)

(progn
  (defstruct (%h2-manager-opening
              (:constructor %make-h2-manager-opening (owner waitqueue)))
    owner
    waitqueue)

  (defstruct (http2-connection-manager
              (:conc-name %http2-connection-manager-)
              (:constructor %make-http2-connection-manager
                            (&key open-connection max-connections idle-timeout
                                  max-connection-age clock-function on-evict
                                  on-transient-close lock)))
    open-connection
    max-connections
    idle-timeout
    max-connection-age
    clock-function
    on-evict
    on-transient-close
    lock
    (entries nil)
    (openings (make-hash-table :test #'equal))
    (sequence 0)
    (clear-generation 0)
    (closed-p nil)))

#+sbcl
(defun %h2-manager-make-lock ()
  (sb-thread:make-mutex :name "HTTP/2 connection manager"))

#-sbcl
(defun %h2-manager-make-lock ()
  nil)

(defmacro %h2-manager-with-lock ((manager) &body body)
  #+sbcl
  `(sb-thread:with-mutex ((%http2-connection-manager-lock ,manager))
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun make-http2-connection-manager
    (&key open-connection (max-connections 8) idle-timeout max-connection-age
          (clock-function #'http-kit::%monotonic-time) on-evict
          on-transient-close)
  "Create a reusable pool of HTTP/2 connections.

OPEN-CONNECTION receives the request and must return an HTTP2-CONNECTION. The
manager always passes TIMEOUT and DEADLINE; proxy-routed requests also pass
PROXY-PLAN and PROXY. By default, the cache key includes the request scheme,
host, port, proxy plan, and proxy. MAX-CONNECTIONS bounds retained cache
entries, not total open connections; request-scoped connections do not count
toward it. Concurrent single-request calls for the same key share a retained
connection. Its scheduler assigns streams and enforces peer concurrent-stream
and flow-control limits. A batch call holds an exclusive lease because the
batch operation is cooperative rather than thread-safe. If an incompatible
operation is already using the same key, the manager uses a request-scoped
connection. At capacity, requests for other keys use a request-scoped
connection when every retained entry is active.

IDLE-TIMEOUT retires entries after the configured time without a completed
manager call. MAX-CONNECTION-AGE retires entries after the configured time from
opening. Either policy can retire an entry while it is active; retired entries
are not reused, and active calls finish before their connection closes. NIL
disables each policy. CLOCK-FUNCTION must return a real monotonic time value.

When supplied, ON-EVICT receives the key of each retained entry when it leaves
the cache, including expiry, capacity eviction, clear, close, and loss of
connection usability. It runs without the manager lock held and may run
concurrently with other callbacks. ON-TRANSIENT-CLOSE receives the key of a
request-scoped connection after its final lease ends and the connection closes,
when no retained usable entry for that key remains. It also runs without the
manager lock held and may run concurrently with other manager calls. Errors
from either callback propagate after required close and retirement actions have
been attempted. OPEN-CONNECTION, CLOCK-FUNCTION, and both callbacks may run
concurrently with other manager calls, so they must synchronize shared state
they use. Errors from OPEN-CONNECTION and CLOCK-FUNCTION propagate to the call.
On SBCL, manager operations protect entry state for concurrent callers. Use
SEND-HTTP2-REQUESTS-OVER-CONNECTION-MANAGER when one manager call needs a batch
of concurrent streams on one connection. Use CLEAR-HTTP2-CONNECTION-MANAGER to
clear retained entries while keeping the manager usable;
CLOSE-HTTP2-CONNECTION-MANAGER also prevents future calls."
  (unless (functionp open-connection)
    (error 'http-kit:http-protocol-error
           :message "MAKE-HTTP2-CONNECTION-MANAGER requires an open callback."
           :operation :http2-manager
           :detail open-connection))
  (unless (and (integerp max-connections) (plusp max-connections))
    (error 'http-kit:http-protocol-error
           :message "MAX-CONNECTIONS must be a positive integer."
           :operation :http2-manager
           :detail max-connections))
  (when (and idle-timeout
             (or (not (realp idle-timeout)) (< idle-timeout 0)))
    (error 'http-kit:http-protocol-error
           :message "IDLE-TIMEOUT must be a non-negative real or NIL."
           :operation :http2-manager
           :detail idle-timeout))
  (when (and max-connection-age
             (or (not (realp max-connection-age))
                 (< max-connection-age 0)))
    (error 'http-kit:http-protocol-error
           :message "MAX-CONNECTION-AGE must be a non-negative real or NIL."
           :operation :http2-manager
           :detail max-connection-age))
  (unless (functionp clock-function)
    (error 'http-kit:http-protocol-error
           :message "CLOCK-FUNCTION must be a function."
           :operation :http2-manager
           :detail clock-function))
  (when (and on-evict (not (functionp on-evict)))
    (error 'http-kit:http-protocol-error
           :message "ON-EVICT must be a function or NIL."
           :operation :http2-manager
           :detail on-evict))
  (when (and on-transient-close (not (functionp on-transient-close)))
    (error 'http-kit:http-protocol-error
           :message "ON-TRANSIENT-CLOSE must be a function or NIL."
           :operation :http2-manager
           :detail on-transient-close))
  (%make-http2-connection-manager
   :open-connection open-connection
   :max-connections max-connections
   :idle-timeout idle-timeout
   :max-connection-age max-connection-age
   :clock-function clock-function
   :on-evict on-evict
   :on-transient-close on-transient-close
   :lock (%h2-manager-make-lock)))

(defun http2-connection-manager-open-p (manager)
  (and (http2-connection-manager-p manager)
       (%h2-manager-with-lock (manager)
         (not (%http2-connection-manager-closed-p manager)))))

(defun http2-connection-manager-max-connections (manager)
  (and (http2-connection-manager-p manager)
       (%http2-connection-manager-max-connections manager)))

(defun http2-connection-manager-idle-timeout (manager)
  "Return MANAGER's idle-connection expiry interval, or NIL when disabled."
  (and (http2-connection-manager-p manager)
       (%http2-connection-manager-idle-timeout manager)))

(defun http2-connection-manager-max-connection-age (manager)
  "Return MANAGER's maximum connection lifetime, or NIL when disabled."
  (and (http2-connection-manager-p manager)
       (%http2-connection-manager-max-connection-age manager)))

(defun http2-connection-manager-clock-function (manager)
  "Return the monotonic clock function used by MANAGER."
  (and (http2-connection-manager-p manager)
       (%http2-connection-manager-clock-function manager)))

(defun http2-connection-manager-connection-count (manager)
  (and (http2-connection-manager-p manager)
       (%h2-manager-with-lock (manager)
         (length (%http2-connection-manager-entries manager)))))

(defun http2-connection-manager-connections (manager)
  "Return the currently retained HTTP2-CONNECTION objects in MANAGER."
  (unless (http2-connection-manager-p manager)
    (error 'http-kit:http-protocol-error
           :message "HTTP2-CONNECTION-MANAGER-CONNECTIONS requires a manager."
           :operation :http2-manager
           :detail (type-of manager)))
  (%h2-manager-with-lock (manager)
    (mapcar #'%http2-managed-connection-connection
            (%http2-connection-manager-entries manager))))

(defun %h2-manager-request-key (request proxy-plan proxy)
  (let ((uri (http-kit:http-request-uri request)))
    (list (http-kit:http-uri-scheme uri)
          (http-kit:http-uri-host uri)
          (http-kit:http-uri-port uri)
          proxy-plan
          proxy)))

(defun %h2-manager-entry-usable-p (entry)
  (let ((connection (%http2-managed-connection-connection entry)))
    (and (not (%http2-managed-connection-retired-p entry))
         (not (%http2-managed-connection-close-started-p entry))
         (http2-connection-open-p connection)
         (not (http2-connection-draining-p connection))
         (null (http2-connection-goaway-last-stream-id connection)))))

(defun %h2-manager-now (manager)
  (when (or (%http2-connection-manager-idle-timeout manager)
            (%http2-connection-manager-max-connection-age manager))
    (let ((now (funcall
                (%http2-connection-manager-clock-function manager))))
      (unless (realp now)
        (error 'http-kit:http-protocol-error
               :message "The HTTP/2 manager clock must return a real number."
               :operation :http2-manager
               :detail now))
      now)))

(defun %h2-manager-entry-expired-p (manager entry now)
  (let ((idle-timeout (%http2-connection-manager-idle-timeout manager))
        (max-age (%http2-connection-manager-max-connection-age manager)))
    (or (and idle-timeout
             now
             (%http2-managed-connection-last-used-at entry)
             (>= (- now (%http2-managed-connection-last-used-at entry))
                 idle-timeout))
        (and max-age
             now
             (%http2-managed-connection-opened-at entry)
             (>= (- now (%http2-managed-connection-opened-at entry))
                 max-age)))))

(defun %h2-manager-touch-locked (manager entry now)
  (setf (%http2-managed-connection-last-used entry)
        (incf (%http2-connection-manager-sequence manager)))
  (when (or (%http2-connection-manager-idle-timeout manager)
            (%http2-connection-manager-max-connection-age manager))
    (setf (%http2-managed-connection-last-used-at entry) now))
  entry)

(defun %h2-manager-retire-entry-locked (manager entry)
  (when (member entry (%http2-connection-manager-entries manager) :test #'eq)
    (setf (%http2-connection-manager-entries manager)
          (delete entry (%http2-connection-manager-entries manager)
                  :test #'eq)
          (%http2-managed-connection-retired-p entry) t)
    (let ((close-now-p
           (zerop (%http2-managed-connection-active-count entry))))
      (when close-now-p
        (setf (%http2-managed-connection-close-started-p entry) t))
      (list entry close-now-p))))

(defun %h2-manager-prune-locked (manager now)
  (let ((actions nil))
    (dolist (entry (copy-list (%http2-connection-manager-entries manager)))
      (when (or (not (%h2-manager-entry-usable-p entry))
                (%h2-manager-entry-expired-p manager entry now))
        (let ((action (%h2-manager-retire-entry-locked manager entry)))
          (when action
            (push action actions)))))
    (nreverse actions)))

(defun %h2-manager-close-connection (connection)
  (when (http2-connection-open-p connection)
    (close-http2-connection connection))
  connection)

(defun %h2-manager-run-retirements (manager actions)
  (let ((failure nil)
        (callback (%http2-connection-manager-on-evict manager)))
    (flet ((record-failure (condition)
                           (unless failure
                             (setf failure condition))))
      (dolist (action actions)
        (let ((entry (first action))
              (close-now-p (second action)))
          (when callback
            (handler-case
                (funcall callback
                         (%http2-managed-connection-key entry))
              (error (condition)
                     (record-failure condition))))
          (when close-now-p
            (%h2-manager-close-connection
             (%http2-managed-connection-connection entry)))))
      (when failure
        (error failure))))
  manager)

(defun %h2-manager-prune (manager)
  (let ((now (%h2-manager-now manager))
        (actions nil))
    (%h2-manager-with-lock (manager)
                           (when (%http2-connection-manager-closed-p manager)
                             (error 'http-kit:http-connection-error
                                    :message "The HTTP/2 connection manager is closed."
                                    :operation :http2-manager
                                    :cause :closed))
                           (setf actions (%h2-manager-prune-locked manager now)))
    (%h2-manager-run-retirements manager actions)))

(defun %h2-manager-entry-for-key-locked
    (manager key &optional share-active-p)
  (reduce (lambda (latest entry)
            (if (and (equal key (%http2-managed-connection-key entry))
                     (%h2-manager-entry-usable-p entry)
                     (or (zerop (%http2-managed-connection-active-count entry))
                         (and share-active-p
                              (not (%http2-managed-connection-exclusive-p entry)))))
                (if (or (null latest)
                        (> (%http2-managed-connection-last-used entry)
                           (%http2-managed-connection-last-used latest)))
                    entry
                  latest)
              latest))
          (%http2-connection-manager-entries manager)
          :initial-value nil))

(defun %h2-manager-same-key-entry-locked (manager key)
  (find key (%http2-connection-manager-entries manager)
        :key #'%http2-managed-connection-key
        :test #'equal))

(defun %h2-manager-oldest-idle-entry-locked (manager)
  (reduce (lambda (oldest entry)
            (if (or (null oldest)
                    (< (%http2-managed-connection-last-used entry)
                       (%http2-managed-connection-last-used oldest)))
                entry
              oldest))
          (remove-if-not
           (lambda (entry)
             (zerop (%http2-managed-connection-active-count entry)))
           (%http2-connection-manager-entries manager))
          :initial-value nil))

(defun %h2-manager-select-entry (manager key now &optional (share-active-p nil))
  (%h2-manager-with-lock (manager)
    (when (%http2-connection-manager-closed-p manager)
      (error 'http-kit:http-connection-error
             :message "The HTTP/2 connection manager is closed."
             :operation :http2-manager
             :cause :closed))
    (let ((entry (%h2-manager-entry-for-key-locked manager key share-active-p))
          (same-key (%h2-manager-same-key-entry-locked manager key))
          (actions nil)
          (request-scoped-p nil))
      (cond
        (entry
         (incf (%http2-managed-connection-active-count entry))
         (setf (%http2-managed-connection-exclusive-p entry)
               (not share-active-p))
         (%h2-manager-touch-locked manager entry now))
        (same-key
         (setf request-scoped-p t))
        ((>= (length (%http2-connection-manager-entries manager))
             (%http2-connection-manager-max-connections manager))
         (let ((oldest (%h2-manager-oldest-idle-entry-locked manager)))
           (if oldest
               (setf actions
                     (list (%h2-manager-retire-entry-locked manager oldest)))
             (setf request-scoped-p t)))))
      (values entry request-scoped-p actions
              (%http2-connection-manager-clear-generation manager)))))

(defun %h2-manager-open-connection
    (manager request timeout deadline proxy-plan proxy)
  (if (or proxy-plan proxy)
      (funcall (%http2-connection-manager-open-connection manager)
               request
               :timeout timeout
               :deadline deadline
               :proxy-plan proxy-plan
               :proxy proxy)
    (funcall (%http2-connection-manager-open-connection manager)
             request
             :timeout timeout
             :deadline deadline)))

(defun %h2-manager-new-entry (key connection now transient-p exclusive-p)
  (%make-http2-managed-connection
   :key key
   :connection connection
   :opened-at now
   :last-used 0
   :last-used-at now
   :active-count 1
   :transient-p transient-p
   :exclusive-p exclusive-p))

(progn
  #+sbcl
  (defun %h2-manager-claim-opening (manager key)
    (%h2-manager-with-lock (manager)
      (when (%http2-connection-manager-closed-p manager)
        (error 'http-kit:http-connection-error
               :message "The HTTP/2 connection manager is closed."
               :operation :http2-manager
               :cause :closed))
      (multiple-value-bind (opening present-p)
          (gethash key (%http2-connection-manager-openings manager))
        (if present-p
            (values opening
                    nil
                    (eq (%h2-manager-opening-owner opening)
                        sb-thread:*current-thread*))
            (let ((opening
                    (%make-h2-manager-opening
                     sb-thread:*current-thread*
                     (sb-thread:make-waitqueue
                      :name "HTTP/2 manager cold open"))))
              (setf (gethash key (%http2-connection-manager-openings manager))
                    opening)
              (values opening t nil))))))

  #-sbcl
  (defun %h2-manager-claim-opening (manager key)
    (declare (ignore manager key))
    (values nil t nil))

  #+sbcl
  (defun %h2-manager-release-opening (manager key opening)
    (%h2-manager-with-lock (manager)
      (when (eq (gethash key (%http2-connection-manager-openings manager))
                opening)
        (remhash key (%http2-connection-manager-openings manager))
        (sb-thread:condition-broadcast
         (%h2-manager-opening-waitqueue opening))))
    opening)

  #-sbcl
  (defun %h2-manager-release-opening (manager key opening)
    (declare (ignore manager key))
    opening)

  #+sbcl
  (defun %h2-manager-wait-for-opening
      (manager key opening deadline clock-function)
    (let ((lock (%http2-connection-manager-lock manager)))
      (sb-thread:with-mutex (lock)
        (loop while (and
                     (eq (gethash key
                                  (%http2-connection-manager-openings manager))
                         opening)
                     (not (%http2-connection-manager-closed-p manager)))
              do (http-kit::%check-deadline deadline clock-function :read)
                 (sb-thread:condition-wait
                  (%h2-manager-opening-waitqueue opening)
                  lock
                  :timeout (and deadline
                                (max 0 (- deadline
                                         (funcall clock-function)))))
                 (unless (sb-thread:holding-mutex-p lock)
                   (sb-sys:without-interrupts
                     (sb-sys:allow-with-interrupts
                       (sb-thread:grab-mutex lock))))
                 (http-kit::%check-deadline deadline clock-function :read))))
    opening)

  #-sbcl
  (defun %h2-manager-wait-for-opening
      (manager key opening deadline clock-function)
    (declare (ignore manager key deadline clock-function))
    opening)

  #+sbcl
  (defun %h2-manager-broadcast-openings-locked (manager)
    (maphash (lambda (key opening)
               (declare (ignore key))
               (sb-thread:condition-broadcast
                (%h2-manager-opening-waitqueue opening)))
             (%http2-connection-manager-openings manager)))

  #-sbcl
  (defun %h2-manager-broadcast-openings-locked (manager)
    (declare (ignore manager))
    nil)

  (defun %h2-manager-open-entry
      (manager request key timeout deadline proxy-plan proxy share-active-p
       request-scoped-p clear-generation)
    (let ((connection
            (%h2-manager-open-connection
             manager request timeout deadline proxy-plan proxy)))
      (unless (http2-connection-p connection)
        (error 'http-kit:http-protocol-error
               :message "The HTTP/2 manager open callback returned a non-connection."
               :operation :http2-manager
               :detail (type-of connection)))
      (let ((close-new-p t)
            (result-entry nil)
            (result-connection nil)
            (closed-during-open-p nil)
            (retirement-actions nil))
        (unwind-protect
             (let ((opened-at (%h2-manager-now manager)))
               (%h2-manager-prune manager)
               (%h2-manager-with-lock (manager)
                 (if (%http2-connection-manager-closed-p manager)
                     (setf closed-during-open-p t)
                     (let ((existing
                             (%h2-manager-entry-for-key-locked
                              manager key share-active-p))
                           (same-key
                             (%h2-manager-same-key-entry-locked manager key))
                           (transient-p
                             (or request-scoped-p
                                 (/= clear-generation
                                     (%http2-connection-manager-clear-generation
                                      manager)))))
                       (cond
                         (existing
                          (incf
                           (%http2-managed-connection-active-count existing))
                          (setf (%http2-managed-connection-exclusive-p existing)
                                (not share-active-p))
                          (%h2-manager-touch-locked manager existing opened-at)
                          (setf result-entry existing
                                result-connection
                                (%http2-managed-connection-connection existing)))
                         (same-key
                          (setf transient-p t))
                         ((>= (length
                               (%http2-connection-manager-entries manager))
                              (%http2-connection-manager-max-connections manager))
                          (let ((oldest
                                  (%h2-manager-oldest-idle-entry-locked manager)))
                            (if oldest
                                (setf retirement-actions
                                      (list
                                       (%h2-manager-retire-entry-locked
                                        manager oldest)))
                                (setf transient-p t))))
                       )
                       (unless result-entry
                         (setf result-entry
                               (%h2-manager-new-entry
                                key connection opened-at transient-p
                                (not share-active-p))
                               result-connection connection)
                         (unless transient-p
                           (push result-entry
                                 (%http2-connection-manager-entries manager)))
                         (%h2-manager-touch-locked
                          manager result-entry opened-at)))))
               (when closed-during-open-p
                 (error 'http-kit:http-connection-error
                        :message "The HTTP/2 connection manager is closed."
                        :operation :http2-manager
                        :cause :closed))
               (setf close-new-p (not (eq result-connection connection)))
               (handler-case
                   (%h2-manager-run-retirements manager retirement-actions)
                 (error (condition)
                   (%h2-manager-finish-entry manager result-entry)
                   (error condition)))
               (values result-connection result-entry))
          (when close-new-p
            (%h2-manager-close-connection connection))))))

  (defun %h2-manager-connection-for
      (manager request connection-key connection-key-supplied-p timeout deadline
       proxy-plan proxy &optional (share-active-p nil) clock-function)
    (unless (http2-connection-manager-p manager)
      (error 'http-kit:http-protocol-error
             :message "An HTTP/2 request requires an HTTP2-CONNECTION-MANAGER."
             :operation :http2-manager
             :detail (type-of manager)))
    (http-kit::%check-http-request request)
    (let* ((key (if connection-key-supplied-p
                    connection-key
                    (%h2-manager-request-key request proxy-plan proxy)))
           (wait-clock (or clock-function #'http-kit::%monotonic-time))
           (wait-deadline
             (http-kit:http-deadline timeout :deadline deadline
                                     :clock-function wait-clock)))
      (loop
        do (%h2-manager-prune manager)
           (multiple-value-bind (entry request-scoped-p actions clear-generation)
               (%h2-manager-select-entry manager key (%h2-manager-now manager)
                                         share-active-p)
             (%h2-manager-run-retirements manager actions)
             (when entry
               (return-from %h2-manager-connection-for
                 (values (%http2-managed-connection-connection entry) entry)))
             (when request-scoped-p
               (return-from %h2-manager-connection-for
                 (%h2-manager-open-entry
                  manager request key timeout deadline proxy-plan proxy
                  share-active-p t clear-generation)))
             (unless share-active-p
               (return-from %h2-manager-connection-for
                 (%h2-manager-open-entry
                  manager request key timeout deadline proxy-plan proxy
                  nil nil clear-generation)))
             (multiple-value-bind (opening owner-p reentrant-p)
                 (%h2-manager-claim-opening manager key)
               (cond
                 (owner-p
                  (unwind-protect
                       (progn
                         (%h2-manager-prune manager)
                         (multiple-value-bind
                               (current-entry current-request-scoped-p
                                current-actions current-clear-generation)
                             (%h2-manager-select-entry
                              manager key (%h2-manager-now manager)
                              share-active-p)
                           (%h2-manager-run-retirements
                            manager current-actions)
                           (if current-entry
                               (return-from %h2-manager-connection-for
                                 (values
                                  (%http2-managed-connection-connection
                                   current-entry)
                                  current-entry))
                             (return-from %h2-manager-connection-for
                               (%h2-manager-open-entry
                                manager request key timeout deadline proxy-plan
                                proxy t current-request-scoped-p
                                current-clear-generation)))))
                    (%h2-manager-release-opening manager key opening)))
                 (reentrant-p
                  (return-from %h2-manager-connection-for
                    (%h2-manager-open-entry
                     manager request key timeout deadline proxy-plan proxy
                     t t clear-generation)))
                 (t
                  (%h2-manager-wait-for-opening
                   manager key opening wait-deadline wait-clock))))))))
)

(defun %h2-manager-finish-entry (manager entry)
  (let ((now nil)
        (clock-failure nil)
        (actions nil)
        (connection-to-close nil)
        (transient-connection-to-close nil)
        (transient-close-key nil)
        (notify-transient-close-p nil)
        (failure nil))
    (handler-case
        (setf now (%h2-manager-now manager))
      (error (condition)
             (setf clock-failure condition)))
    (%h2-manager-with-lock (manager)
                           (when (plusp (%http2-managed-connection-active-count entry))
                             (decf (%http2-managed-connection-active-count entry))
                             (when (zerop (%http2-managed-connection-active-count entry))
                               (cond
                                ((%http2-managed-connection-transient-p entry)
                                 (let ((retained
                                        (%h2-manager-same-key-entry-locked
                                         manager
                                         (%http2-managed-connection-key entry))))
                                   (when (and retained
                                              (or (not (%h2-manager-entry-usable-p retained))
                                                  (and now
                                                       (%h2-manager-entry-expired-p
                                                        manager retained now))))
                                     (setf actions
                                           (list
                                            (%h2-manager-retire-entry-locked manager retained))
                                           retained nil))
                                   (setf (%http2-managed-connection-close-started-p entry) t
                                         transient-connection-to-close
                                         (%http2-managed-connection-connection entry)
                                         transient-close-key
                                         (%http2-managed-connection-key entry)
                                         notify-transient-close-p (null retained))))
                                ((%http2-managed-connection-retired-p entry)
                                 (setf (%http2-managed-connection-close-started-p entry) t
                                       connection-to-close
                                       (%http2-managed-connection-connection entry)))
                                ((not (%h2-manager-entry-usable-p entry))
                                 (setf actions
                                       (list
                                        (%h2-manager-retire-entry-locked manager entry))))
                                (t
                                 (when now
                                   (%h2-manager-touch-locked manager entry now))
                                 (when (and now
                                            (%h2-manager-entry-expired-p manager entry now))
                                   (setf actions
                                         (list
                                          (%h2-manager-retire-entry-locked manager entry)))))))))
    (flet ((record-failure (condition)
                           (unless failure
                             (setf failure condition))))
      (handler-case
          (%h2-manager-run-retirements manager actions)
        (error (condition)
               (record-failure condition)))
      (when connection-to-close
        (%h2-manager-close-connection connection-to-close))
      (when transient-connection-to-close
        (%h2-manager-close-connection
         transient-connection-to-close)
        (when (and notify-transient-close-p
                   (%http2-connection-manager-on-transient-close manager))
          (handler-case
              (funcall
               (%http2-connection-manager-on-transient-close manager)
               transient-close-key)
            (error (condition)
                   (record-failure condition)))))
      (when clock-failure
        (record-failure clock-failure))
      (when failure
        (error failure)))
    entry))

(defun %h2-manager-unprocessed-retry-p
    (condition request-body-function)
  (and (typep condition 'http-kit:http-connection-error)
       (let ((cause (http-kit:http-connection-error-cause condition)))
         (and (consp cause)
              (or (eq (first cause) :goaway)
                  (and (eq (first cause) :rst-stream)
                       (eql (third cause) 7)))))
       (null request-body-function)))

(defun send-http2-request-over-connection-manager
    (manager request
     &key (connection-key nil connection-key-supplied-p)
       timeout deadline max-header-bytes max-fields max-body-bytes clock-function
       request-body-function request-body-length
       on-body-chunk on-information (collect-body-p t) (huffman-p nil)
       proxy-plan proxy)
  (loop
    with retried-p = nil
    do
       (multiple-value-bind (connection entry)
           (%h2-manager-connection-for manager request connection-key connection-key-supplied-p timeout deadline proxy-plan proxy t clock-function)
         (let ((retry-p nil)
               (response nil))
           (unwind-protect
                (handler-case
                    (setf response
                          (send-http2-request-over-connection
                           connection request
                           :timeout timeout
                           :deadline deadline
                           :max-header-bytes max-header-bytes
                           :max-fields max-fields
                           :max-body-bytes max-body-bytes
                           :clock-function clock-function
                           :request-body-function request-body-function
                           :request-body-length request-body-length
                           :on-body-chunk on-body-chunk
                           :on-information on-information
                           :collect-body-p collect-body-p
                           :huffman-p huffman-p))
                  (error (condition)
                    (if (and (not retried-p)
                             (%h2-manager-unprocessed-retry-p
                              condition request-body-function))
                        (setf retried-p t
                              retry-p t)
                      (error condition))))
             (%h2-manager-finish-entry manager entry))
           (unless retry-p
             (return response))))))

(defun send-http2-requests-over-connection-manager
    (manager requests
     &key (connection-key nil connection-key-supplied-p)
       timeout deadline max-header-bytes max-fields max-body-bytes clock-function
       request-body-functions request-body-lengths
       on-body-chunk (collect-body-p t) (huffman-p nil))
  "Send a non-empty batch through one reusable HTTP/2 connection manager."
  (unless (and (listp requests) requests)
    (error 'http-kit:http-protocol-error
           :message "HTTP/2 manager batches must be a non-empty proper list."
           :operation :http2-manager
           :detail requests))
  (multiple-value-bind (connection entry)
      (%h2-manager-connection-for
       manager (first requests) connection-key connection-key-supplied-p
       timeout deadline nil nil nil)
    (unwind-protect
         (send-http2-requests-over-connection
          connection requests
          :timeout timeout
          :deadline deadline
          :max-header-bytes max-header-bytes
          :max-fields max-fields
          :max-body-bytes max-body-bytes
          :clock-function clock-function
          :request-body-functions request-body-functions
          :request-body-lengths request-body-lengths
          :on-body-chunk on-body-chunk
          :collect-body-p collect-body-p
          :huffman-p huffman-p)
      (%h2-manager-finish-entry manager entry))))

(defun make-http2-connection-manager-transport (manager)
  "Return a high-level client transport backed by MANAGER."
  (unless (http2-connection-manager-p manager)
    (error 'http-kit:http-protocol-error
           :message "MAKE-HTTP2-CONNECTION-MANAGER-TRANSPORT requires a manager."
           :operation :http2-manager
           :detail (type-of manager)))
  (lambda (request
           &key timeout deadline max-header-bytes max-fields max-body-bytes clock-function
             request-body-function request-body-length
             on-body-chunk on-information (collect-body-p t) (huffman-p nil)
             (connection-key nil connection-key-supplied-p)
             proxy-plan proxy
             &allow-other-keys)
    (if connection-key-supplied-p
        (send-http2-request-over-connection-manager
         manager request
         :connection-key connection-key
         :timeout timeout
         :deadline deadline
         :max-header-bytes max-header-bytes
         :max-fields max-fields
         :max-body-bytes max-body-bytes
         :clock-function clock-function
         :request-body-function request-body-function
         :request-body-length request-body-length
         :on-body-chunk on-body-chunk
         :on-information on-information
         :collect-body-p collect-body-p
         :huffman-p huffman-p
         :proxy-plan proxy-plan
         :proxy proxy)
        (send-http2-request-over-connection-manager
         manager request
         :timeout timeout
         :deadline deadline
         :max-header-bytes max-header-bytes
         :max-fields max-fields
         :max-body-bytes max-body-bytes
         :clock-function clock-function
         :request-body-function request-body-function
         :request-body-length request-body-length
         :on-body-chunk on-body-chunk
         :on-information on-information
         :collect-body-p collect-body-p
         :huffman-p huffman-p
         :proxy-plan proxy-plan
         :proxy proxy))))

(defun %h2-manager-retire-all (manager terminal-p)
  (let ((actions nil))
    (%h2-manager-with-lock (manager)
      (incf (%http2-connection-manager-clear-generation manager))
      (when terminal-p
        (setf (%http2-connection-manager-closed-p manager) t)
        (%h2-manager-broadcast-openings-locked manager))
      (dolist (entry
               (copy-list (%http2-connection-manager-entries manager)))
        (let ((action (%h2-manager-retire-entry-locked manager entry)))
          (when action
            (push action actions)))))
    (%h2-manager-run-retirements manager (nreverse actions))
    manager))

(defun clear-http2-connection-manager (manager)
  "Retire all retained connections without closing MANAGER.
On success, return MANAGER. Repeating this operation is safe. An open MANAGER
remains reusable. Active entries leave the cache immediately; connections whose
opens were in progress when clear began are not retained. Connections close
after their final lease is released."
  (unless (http2-connection-manager-p manager)
    (error 'http-kit:http-protocol-error
           :message "CLEAR-HTTP2-CONNECTION-MANAGER requires a manager."
           :operation :http2-manager
           :detail (type-of manager)))
  (%h2-manager-retire-all manager nil))

(defun close-http2-connection-manager (manager)
  "Retire retained connections and prevent new calls from using MANAGER.
Active leases are released before their connections are closed."
  (unless (http2-connection-manager-p manager)
    (error 'http-kit:http-protocol-error
           :message "CLOSE-HTTP2-CONNECTION-MANAGER requires a manager."
           :operation :http2-manager
           :detail (type-of manager)))
  (%h2-manager-retire-all manager t))
