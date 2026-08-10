(in-package #:http-kit/http2)

(defstruct (http2-client
             (:conc-name %http2-)
             (:constructor %make-http2-client
                 (&key exchange open-stream connection close-stream
                       max-frame-size max-header-bytes max-body-bytes
                       clock-function)))
  (exchange nil)
  (open-stream nil)
  (connection nil)
  (close-stream #'close)
  (max-frame-size nil)
  (max-header-bytes nil)
  (max-body-bytes nil)
  (clock-function nil))

(defstruct (http2-connection
             (:conc-name %http2-connection-)
             (:constructor %make-http2-connection
                 (&key stream close-stream max-frame-size max-header-bytes
                       max-body-bytes clock-function hpack-context
                       next-stream-id peer-max-frame-size peer-max-table-size
                       peer-initial-window-size peer-connection-window-size
                       peer-stream-windows session-started-p
                       goaway-last-stream-id closed-p)))
  stream
  (close-stream #'close)
  max-frame-size
  max-header-bytes
  max-body-bytes
  clock-function
  (hpack-context nil)
  (next-stream-id 1)
  (peer-max-frame-size +http2-default-max-frame-size+)
  (peer-max-table-size +hpack-default-table-size+)
  (peer-initial-window-size +http2-default-window-size+)
  (peer-connection-window-size +http2-default-window-size+)
  (peer-stream-windows nil)
  (session-started-p nil)
  (goaway-last-stream-id nil)
  (local-goaway-last-stream-id nil)
  (draining-p nil)
  (closed-p nil))
