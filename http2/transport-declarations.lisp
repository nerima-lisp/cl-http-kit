(in-package #:http-kit/http2)

(defstruct (http2-client
             (:conc-name %http2-)
             (:constructor %make-http2-client
                 (&key exchange open-stream connection close-stream
                       max-frame-size max-header-bytes max-fields max-body-bytes
                       clock-function)))
  (exchange nil)
  (open-stream nil)
  (connection nil)
  (close-stream #'close)
  (max-frame-size nil)
  (max-header-bytes nil)
  (max-fields 256)
  (max-body-bytes nil)
  (clock-function nil))

(defstruct (http2-connection
             (:conc-name %http2-connection-)
             (:constructor %make-http2-connection
                 (&key stream close-stream max-frame-size max-header-bytes max-fields
                       max-body-bytes clock-function hpack-context
                       next-stream-id peer-max-frame-size peer-max-table-size
                       peer-max-concurrent-streams peer-max-header-list-size
                       peer-initial-window-size peer-enable-connect-protocol-p
                       peer-connection-window-size
                       peer-stream-windows session-started-p
                       goaway-last-stream-id closed-p)))
  stream
  (close-stream #'close)
  max-frame-size
  max-header-bytes
  (max-fields 256)
  max-body-bytes
  clock-function
  (hpack-context nil)
  (next-stream-id 1)
  (peer-max-frame-size +http2-default-max-frame-size+)
  (peer-max-table-size +hpack-default-table-size+)
  (peer-max-concurrent-streams nil)
  (peer-max-header-list-size nil)
  (peer-initial-window-size +http2-default-window-size+)
  (peer-enable-connect-protocol-p nil)
  (peer-connection-window-size +http2-default-window-size+)
  (peer-stream-windows nil)
  (session-started-p nil)
  (goaway-last-stream-id nil)
  (local-goaway-last-stream-id nil)
  (draining-p nil)
  (closed-p nil))

(defun http2-connection-open-p (connection)
  (and (http2-connection-p connection)
       (not (%http2-connection-closed-p connection))))

(defun http2-connection-session-started-p (connection)
  (and (http2-connection-p connection)
       (%http2-connection-session-started-p connection)))

(defun http2-connection-peer-max-frame-size (connection)
  (when (http2-connection-p connection)
    (%http2-connection-peer-max-frame-size connection)))

(defun http2-connection-peer-max-table-size (connection)
  (when (http2-connection-p connection)
    (%http2-connection-peer-max-table-size connection)))

(defun http2-connection-peer-initial-window-size (connection)
  (when (http2-connection-p connection)
    (%http2-connection-peer-initial-window-size connection)))

(defun http2-connection-peer-connection-window-size (connection)
  (when (http2-connection-p connection)
    (%http2-connection-peer-connection-window-size connection)))

(defun http2-connection-goaway-last-stream-id (connection)
  (when (http2-connection-p connection)
    (%http2-connection-goaway-last-stream-id connection)))

(defun http2-connection-local-goaway-last-stream-id (connection)
  (when (http2-connection-p connection)
    (%http2-connection-local-goaway-last-stream-id connection)))

(defun http2-connection-draining-p (connection)
  (and (http2-connection-p connection)
       (%http2-connection-draining-p connection)))
