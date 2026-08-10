(in-package #:http-kit/http2)

(defstruct (%http2-managed-connection
             (:constructor %make-http2-managed-connection
                 (&key key connection last-used)))
  key
  connection
  last-used)

(defstruct (http2-connection-manager
             (:conc-name %http2-connection-manager-)
             (:constructor %make-http2-connection-manager
                 (&key open-connection max-connections)))
  open-connection
  max-connections
  (entries nil)
  (sequence 0)
  (closed-p nil))

(defun make-http2-connection-manager
    (&key open-connection (max-connections 8))
  "Create a cooperative pool of reusable HTTP/2 connections.

OPEN-CONNECTION receives the request being sent and may accept TIMEOUT and
DEADLINE keyword arguments.  It must return an HTTP2-CONNECTION.  The manager
selects connections by CONNECTION-KEY (the request's scheme, host, and port
by default), keeps at most MAX-CONNECTIONS entries, and evicts the least
recently used idle entry when a new key needs a slot.

The manager is deliberately owner-thread and non-reentrant, like the
underlying HTTP/2 connection APIs.  Use SEND-HTTP2-REQUESTS-OVER-CONNECTION
when a caller needs several concurrent streams on one connection."
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
  (%make-http2-connection-manager
   :open-connection open-connection
   :max-connections max-connections))

(defun http2-connection-manager-open-p (manager)
  (and (http2-connection-manager-p manager)
       (not (%http2-connection-manager-closed-p manager))))

(defun http2-connection-manager-max-connections (manager)
  (and (http2-connection-manager-p manager)
       (%http2-connection-manager-max-connections manager)))

(defun http2-connection-manager-connection-count (manager)
  (and (http2-connection-manager-p manager)
       (length (%http2-connection-manager-entries manager))))

(defun http2-connection-manager-connections (manager)
  "Return the currently retained HTTP2-CONNECTION objects in MANAGER."
  (unless (http2-connection-manager-p manager)
    (error 'http-kit:http-protocol-error
           :message "HTTP2-CONNECTION-MANAGER-CONNECTIONS requires a manager."
           :operation :http2-manager
           :detail (type-of manager)))
  (mapcar #'%http2-managed-connection-connection
          (%http2-connection-manager-entries manager)))

(defun %h2-manager-request-key (request)
  (let ((uri (http-kit:http-request-uri request)))
    (list (http-kit:http-uri-scheme uri)
          (http-kit:http-uri-host uri)
          (http-kit:http-uri-port uri))))

(defun %h2-manager-entry-usable-p (entry)
  (let ((connection (%http2-managed-connection-connection entry)))
    (and (http2-connection-open-p connection)
         (not (http2-connection-draining-p connection))
         (null (http2-connection-goaway-last-stream-id connection)))))

(defun %h2-manager-discard-entry (manager entry)
  (setf (%http2-connection-manager-entries manager)
        (delete entry (%http2-connection-manager-entries manager)
                :test #'eq))
  (let ((connection (%http2-managed-connection-connection entry)))
    (when (http2-connection-open-p connection)
      (close-http2-connection connection)))
  entry)

(defun %h2-manager-prune (manager)
  (dolist (entry (copy-list (%http2-connection-manager-entries manager)))
    (unless (%h2-manager-entry-usable-p entry)
      (%h2-manager-discard-entry manager entry)))
  manager)

(defun %h2-manager-entry-for-key (manager key)
  (let ((candidate nil))
    (dolist (entry (%http2-connection-manager-entries manager) candidate)
      (when (and (equal key (%http2-managed-connection-key entry))
                 (%h2-manager-entry-usable-p entry)
                 (or (null candidate)
                     (> (%http2-managed-connection-last-used entry)
                        (%http2-managed-connection-last-used candidate))))
        (setf candidate entry)))))

(defun %h2-manager-oldest-entry (manager)
  (reduce (lambda (oldest entry)
            (if (or (null oldest)
                    (< (%http2-managed-connection-last-used entry)
                       (%http2-managed-connection-last-used oldest)))
                entry
                oldest))
          (%http2-connection-manager-entries manager)
          :initial-value nil))

(defun %h2-manager-touch (manager entry)
  (setf (%http2-managed-connection-last-used entry)
        (incf (%http2-connection-manager-sequence manager)))
  entry)

(defun %h2-manager-open-entry
    (manager request key timeout deadline)
  (%h2-manager-prune manager)
  (when (>= (length (%http2-connection-manager-entries manager))
            (%http2-connection-manager-max-connections manager))
    (let ((oldest (%h2-manager-oldest-entry manager)))
      (when oldest
        (%h2-manager-discard-entry manager oldest))))
  (let ((connection
          (funcall (%http2-connection-manager-open-connection manager)
                   request
                   :timeout timeout
                   :deadline deadline)))
    (unless (http2-connection-p connection)
      (error 'http-kit:http-protocol-error
             :message "The HTTP/2 manager open callback returned a non-connection."
             :operation :http2-manager
             :detail (type-of connection)))
    (let ((entry (%make-http2-managed-connection
                  :key key
                  :connection connection
                  :last-used 0)))
      (push entry (%http2-connection-manager-entries manager))
      (%h2-manager-touch manager entry)
      entry)))

(defun %h2-manager-connection-for
    (manager request connection-key connection-key-supplied-p timeout deadline)
  (unless (http2-connection-manager-p manager)
    (error 'http-kit:http-protocol-error
           :message "An HTTP/2 request requires an HTTP2-CONNECTION-MANAGER."
           :operation :http2-manager
           :detail (type-of manager)))
  (unless (http2-connection-manager-open-p manager)
    (error 'http-kit:http-connection-error
           :message "The HTTP/2 connection manager is closed."
           :operation :http2-manager
           :cause :closed))
  (http-kit::%check-http-request request)
  (let* ((key (if connection-key-supplied-p
                  connection-key
                  (%h2-manager-request-key request)))
         (entry nil))
    (%h2-manager-prune manager)
    (setf entry (%h2-manager-entry-for-key manager key))
    (unless entry
      (setf entry (%h2-manager-open-entry
                   manager request key timeout deadline)))
    (%h2-manager-touch manager entry)
    (values (%http2-managed-connection-connection entry) entry)))

(defun %h2-manager-finish-entry (manager entry)
  (if (%h2-manager-entry-usable-p entry)
      (%h2-manager-touch manager entry)
      (%h2-manager-discard-entry manager entry)))

(defun send-http2-request-over-connection-manager
    (manager request
     &key (connection-key nil connection-key-supplied-p)
       timeout deadline max-header-bytes max-body-bytes clock-function
       request-body-function request-body-length
       on-body-chunk (collect-body-p t) (huffman-p nil))
  "Send REQUEST through a reusable HTTP/2 connection manager."
  (multiple-value-bind (connection entry)
      (%h2-manager-connection-for
       manager request connection-key connection-key-supplied-p timeout deadline)
    (handler-case
        (let ((response
                (send-http2-request-over-connection
                 connection request
                 :timeout timeout
                 :deadline deadline
                 :max-header-bytes max-header-bytes
                 :max-body-bytes max-body-bytes
                 :clock-function clock-function
                 :request-body-function request-body-function
                 :request-body-length request-body-length
                 :on-body-chunk on-body-chunk
                 :collect-body-p collect-body-p
                 :huffman-p huffman-p)))
          (%h2-manager-finish-entry manager entry)
          response)
      (error (condition)
        (unless (http2-connection-open-p connection)
          (%h2-manager-discard-entry manager entry))
        (error condition)))))

(defun send-http2-requests-over-connection-manager
    (manager requests
     &key (connection-key nil connection-key-supplied-p)
       timeout deadline max-header-bytes max-body-bytes clock-function
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
       timeout deadline)
    (handler-case
        (let ((responses
                (send-http2-requests-over-connection
                 connection requests
                 :timeout timeout
                 :deadline deadline
                 :max-header-bytes max-header-bytes
                 :max-body-bytes max-body-bytes
                 :clock-function clock-function
                 :request-body-functions request-body-functions
                 :request-body-lengths request-body-lengths
                 :on-body-chunk on-body-chunk
                 :collect-body-p collect-body-p
                 :huffman-p huffman-p)))
          (%h2-manager-finish-entry manager entry)
          responses)
      (error (condition)
        (unless (http2-connection-open-p connection)
          (%h2-manager-discard-entry manager entry))
        (error condition)))))

(defun make-http2-connection-manager-transport (manager)
  "Return a high-level client transport backed by MANAGER."
  (unless (http2-connection-manager-p manager)
    (error 'http-kit:http-protocol-error
           :message "MAKE-HTTP2-CONNECTION-MANAGER-TRANSPORT requires a manager."
           :operation :http2-manager
           :detail (type-of manager)))
  (lambda (request
           &key timeout deadline max-header-bytes max-body-bytes clock-function
             request-body-function request-body-length
             on-body-chunk (collect-body-p t) (huffman-p nil)
             (connection-key nil connection-key-supplied-p)
             &allow-other-keys)
    (if connection-key-supplied-p
        (send-http2-request-over-connection-manager
         manager request
         :connection-key connection-key
         :timeout timeout
         :deadline deadline
         :max-header-bytes max-header-bytes
         :max-body-bytes max-body-bytes
         :clock-function clock-function
         :request-body-function request-body-function
         :request-body-length request-body-length
         :on-body-chunk on-body-chunk
         :collect-body-p collect-body-p
         :huffman-p huffman-p)
        (send-http2-request-over-connection-manager
         manager request
         :timeout timeout
         :deadline deadline
         :max-header-bytes max-header-bytes
         :max-body-bytes max-body-bytes
         :clock-function clock-function
         :request-body-function request-body-function
         :request-body-length request-body-length
         :on-body-chunk on-body-chunk
         :collect-body-p collect-body-p
         :huffman-p huffman-p))))

(defun close-http2-connection-manager (manager)
  "Close every retained connection and make MANAGER unusable."
  (unless (http2-connection-manager-p manager)
    (error 'http-kit:http-protocol-error
           :message "CLOSE-HTTP2-CONNECTION-MANAGER requires a manager."
           :operation :http2-manager
           :detail (type-of manager)))
  (unless (%http2-connection-manager-closed-p manager)
    (setf (%http2-connection-manager-closed-p manager) t)
    (dolist (entry (copy-list (%http2-connection-manager-entries manager)))
      (%h2-manager-discard-entry manager entry)))
  manager)
