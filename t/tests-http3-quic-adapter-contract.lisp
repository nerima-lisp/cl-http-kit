(in-package #:http-kit/test)

(deftest http3-quic-adapter-contract-unbounded-handshake-poll-exhaustion
  (let* ((symbols '(cl-quic-kit:client-start cl-quic-kit:client-poll cl-quic-kit:client-close))
         (originals (mapcar #'symbol-function symbols))
         (now (lambda () 0))
         (local-id (octets 1 2 3 4))
         (destination-id (octets 5 6 7 8))
         (writes nil)
         (failure nil)
         (connection
           (cl-quic-kit:make-quic-connection
            :local-connection-id local-id
            :now-fn now
            :io-write (lambda (actual bytes)
                        (push (cons actual (copy-seq bytes)) writes)))))
    ;; With no peer and no deadline, the real TLS client exhausts the adapter's
    ;; finite poll budget rather than manufacturing an established connection.
    (call-with-http3-quic-lifecycle-spies
     (lambda (calls)
       (handler-case
           (http-kit/http3:make-http3-quic-adapter
            :connection connection :hostname "example.test"
            :now-fn now
            :tls-provider (cl-tls-kit:make-cl-crypto-kit-provider)
            :local-connection-id local-id :destination-connection-id destination-id)
         (http-connection-error (condition) (setf failure condition)))
       (ensure-true (typep failure 'http-connection-error))
       (ensure-equal :http3-quic-adapter (http-error-operation failure))
       (ensure-false (http-connection-error-request-sent-p failure))
       (let ((cause (http-connection-error-cause failure)))
         (ensure-true (typep cause 'http-protocol-error))
         (ensure-equal :http3-quic-adapter (http-error-operation cause))
         (ensure-equal "The QUIC TLS handshake did not complete."
                       (http-error-message cause))
         (ensure-equal :handshaking (http-protocol-error-detail cause)))
       (ensure-equal 1 (length (gethash 'cl-quic-kit:client-start calls)))
       (ensure-equal 3000 (length (gethash 'cl-quic-kit:client-poll calls)))
       (ensure-equal 1 (length (gethash 'cl-quic-kit:client-close calls)))
       (let ((client (caar (gethash 'cl-quic-kit:client-start calls))))
         (ensure-true (eq connection (cl-quic-kit:quic-client-connection client)))
         (ensure-true (cl-quic-kit:quic-client-closed-p client))
         (ensure-true
          (every (lambda (arguments) (eq client (car arguments)))
                 (gethash 'cl-quic-kit:client-poll calls)))
         (ensure-true (eq client (caar (gethash 'cl-quic-kit:client-close calls)))))))
    (ensure-equal :closing (cl-quic-kit:connection-state connection))
    (ensure-true (consp writes))
    (ensure-true
     (every (lambda (write)
              (and (eq connection (car write)) (plusp (length (cdr write)))))
            writes))
    (let ((initial (cdr (car (last writes)))))
      (ensure-true (>= (length initial) 1200))
      (multiple-value-bind (header end) (cl-quic-kit:decode-packet-header initial)
        (ensure-equal :initial (cl-quic-kit:packet-header-type header))
        (ensure-equal 1 (cl-quic-kit:packet-header-version header))
        (ensure-true
         (equalp destination-id (cl-quic-kit:packet-header-destination-connection-id header)))
        (ensure-true
         (equalp local-id (cl-quic-kit:packet-header-source-connection-id header)))
        (ensure-true (plusp (length (cl-quic-kit:packet-header-payload header))))
        (ensure-equal (length initial) end)))
    (loop for symbol in symbols for original in originals
          do (ensure-true (eq original (symbol-function symbol))))))
