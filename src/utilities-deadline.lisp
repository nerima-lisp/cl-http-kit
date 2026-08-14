(in-package #:http-kit)

(defun %monotonic-time ()
  (/ (float (get-internal-real-time))
     (float internal-time-units-per-second)))

(defun http-deadline (timeout &key deadline (clock-function #'%monotonic-time))
  "Return an absolute monotonic deadline from TIMEOUT and DEADLINE.
Both values are seconds.  NIL means that no bound was requested."
  (unless (or (null timeout) (and (realp timeout) (>= timeout 0)))
    (error 'http-protocol-error
           :message "HTTP timeout must be a non-negative real number."
           :operation :deadline
           :detail timeout))
  (unless (or (null deadline) (realp deadline))
    (error 'http-protocol-error
           :message "HTTP deadline must be a real number."
           :operation :deadline
           :detail deadline))
  (let ((timeout-deadline (and timeout (+ (funcall clock-function) timeout))))
    (cond ((and timeout-deadline deadline) (min timeout-deadline deadline))
          (timeout-deadline timeout-deadline)
          (deadline deadline)
          (t nil))))

(defun %check-deadline (deadline clock-function &optional (kind :read))
  (when (and deadline (>= (funcall clock-function) deadline))
    (error 'http-timeout
           :message "The HTTP operation exceeded its deadline."
           :operation kind
           :kind kind)))
