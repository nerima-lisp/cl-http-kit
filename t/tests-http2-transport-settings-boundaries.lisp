(in-package #:http-kit/test)

(deftest http2-client-and-settings-boundaries
  (signals http-protocol-error
    (http-kit/http2:make-http2-client))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :open-stream #'identity))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :close-stream 7))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :max-frame-size 16383))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :max-frame-size #x1000000))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :max-frame-size "bad"))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :max-header-bytes 0))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :max-header-bytes "bad"))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :max-fields 0))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :max-fields "bad"))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :max-body-bytes -1))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :max-body-bytes "bad"))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :clock-function 7))
  (signals http-protocol-error
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :clock-function nil))
  (ensure-true
   (http-kit/http2:http2-client-p
    (http-kit/http2:make-http2-client
     :exchange #'identity
     :max-body-bytes 0)))
  (multiple-value-bind (max-frame max-table initial-window enable-connect
                        max-concurrent max-header-list)
      (http-kit/http2::%h2-settings
       (octets 0 1 0 0 0 42
               0 3 0 0 0 9
               0 5 0 0 64 0
               0 6 0 0 1 0
               0 4 0 0 255 255
               0 8 0 0 0 1))
    (ensure-equal 16384 max-frame)
    (ensure-equal 42 max-table)
    (ensure-equal 65535 initial-window)
    (ensure-equal 1 enable-connect)
    (ensure-equal 9 max-concurrent)
    (ensure-equal 256 max-header-list))
  (signals http-protocol-error
    (http-kit/http2::%h2-settings (octets 0 1 0)))
  (signals http-protocol-error
    (http-kit/http2::%h2-settings (octets 0 0 0 0 0 0)))
  (signals http-protocol-error
    (http-kit/http2::%h2-settings (octets 0 2 0 0 0 1)))
  (http-kit/http2::%h2-settings (octets 0 2 0 0 0 1)
                                :peer-role :client)
  (signals http-protocol-error
    (http-kit/http2::%h2-settings (octets 0 2 0 0 0 2)
                                  :peer-role :client))
  (signals http-protocol-error
    (http-kit/http2::%h2-settings (octets 0 4 128 0 0 0)))
  (signals http-protocol-error
    (http-kit/http2::%h2-settings (octets 0 5 0 0 63 255)))
  (signals http-protocol-error
    (http-kit/http2::%h2-settings (octets 0 8 0 0 0 2)))
  (let* ((wire (http-kit/http2::%h2-settings-wire
                16384 :enable-connect-p t))
         (reader (http-kit/http2::%h2-reader-for wire))
         (frame (http-kit/http2::%h2-read-frame reader 16384 nil nil)))
    (ensure-equal 4 (http-kit/http2::%h2-frame-type frame))
    (ensure-equal (octets 0 2 0 0 0 0
                          0 5 0 0 64 0
                          0 8 0 0 0 1)
                  (http-kit/http2::%h2-frame-payload frame)))
  (multiple-value-bind (max-frame max-table initial-window)
      (http-kit/http2::%h2-settings (octets 0 4 0 0 255 254))
    (ensure-equal nil max-frame)
    (ensure-equal nil max-table)
    (ensure-equal 65534 initial-window))
  (let ((connection (http-kit/http2::%make-http2-connection)))
    (http-kit/http2::%h2-connection-note-peer-settings
     connection nil nil nil 0)
    (ensure-equal nil
                  (http-kit/http2::%http2-connection-peer-enable-connect-protocol-p
                   connection))
    (http-kit/http2::%h2-connection-note-peer-settings
     connection nil nil nil 1 7 512)
    (ensure-true
     (http-kit/http2::%http2-connection-peer-enable-connect-protocol-p
      connection))
    (ensure-equal 7
                  (http-kit/http2:http2-connection-peer-max-concurrent-streams
                   connection))
    (ensure-equal 512
                  (http-kit/http2:http2-connection-peer-max-header-list-size
                   connection))
    (http-kit/http2::%h2-connection-note-peer-settings
     connection nil nil nil nil 0 0)
    (ensure-equal 0
                  (http-kit/http2:http2-connection-peer-max-concurrent-streams
                   connection))
    (ensure-equal 0
                  (http-kit/http2:http2-connection-peer-max-header-list-size
                   connection))
    (http-kit/http2::%h2-connection-note-peer-settings
     connection nil nil nil 0)
    (ensure-equal nil
                  (http-kit/http2::%http2-connection-peer-enable-connect-protocol-p
                   connection)))
  (let ((connection
          (http-kit/http2::%make-http2-connection
           :peer-initial-window-size 0
           :peer-stream-windows (list (cons 1 #x7fffffff)))))
    (signals http-protocol-error
      (http-kit/http2::%h2-connection-note-peer-settings
       connection nil nil 1 nil))
    (ensure-equal #x7fffffff
                  (cdr (assoc 1
                              (http-kit/http2::%http2-connection-peer-stream-windows
                               connection))))))
  (let ((connection
          (http-kit/http2::%make-http2-connection
           :peer-initial-window-size #x7fffffff)))
    (signals http-protocol-error
      (http-kit/http2::%h2-connection-note-window-update
       connection
       (http-kit/http2::%make-h2-frame
        :length 4
        :type http-kit/http2::+http2-window-update-type+
        :flags 0
        :stream-id 1
        :payload (octets 0 0 0 1))
       1))
    (ensure-equal nil
                  (http-kit/http2::%http2-connection-peer-stream-windows
                   connection)))
  (let ((connection (http-kit/http2::%make-http2-connection)))
    (signals http-protocol-error
      (http-kit/http2::%h2-connection-note-window-update
       connection
       (http-kit/http2::%make-h2-frame
        :length 4
        :type http-kit/http2::+http2-window-update-type+
        :flags 0
        :stream-id 3
        :payload (octets 0 0 0 1))
       1)))
  (let ((connection
          (http-kit/http2::%make-http2-connection
           :goaway-last-stream-id 7)))
    (signals http-connection-error
      (http-kit/http2::%h2-connection-next-stream-id connection)))

(deftest http2-server-extended-connect-setting-boundary
  (let ((fields (list (cons ":method" "CONNECT")
                      (cons ":protocol" "websocket")
                      (cons ":scheme" "https")
                      (cons ":authority" "example.com")
                      (cons ":path" "/chat?room=main"))))
    (signals http-invalid-header
      (http-kit/http2::%h2-server-header-fields fields nil))
    (multiple-value-bind (method scheme authority target protocol headers)
        (http-kit/http2::%h2-server-header-fields
         fields nil :enable-connect-p t)
      (ensure-equal "CONNECT" method)
      (ensure-equal "https" scheme)
      (ensure-equal "example.com" authority)
      (ensure-equal "/chat?room=main" target)
      (ensure-equal "websocket" protocol)
      (ensure-equal nil headers))
    (signals http-unsupported-feature
      (http-kit/http2::%h2-server-header-fields
       (list (cons ":method" "CONNECT")
             (cons ":protocol" "websocket")
             (cons ":scheme" "ftp")
             (cons ":authority" "example.com")
             (cons ":path" "/chat"))
       nil :enable-connect-p t))))
