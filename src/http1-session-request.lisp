(in-package #:http-kit)

(defun %http1-session-validate-serve-arguments
    (stream handler max-requests on-error on-upgrade on-expect-continue close-stream)
  (unless (streamp stream)
    (%http1-session-error "HTTP/1 session stream must be a stream."
                          (type-of stream)))
  (unless (functionp handler)
    (%http1-session-error "HTTP/1 session handler must be a function."
                          (type-of handler)))
  (unless (or (null max-requests)
              (and (integerp max-requests)
                   (>= max-requests 0)))
    (%http1-session-error
     "HTTP/1 session max-requests must be a non-negative integer or NIL."
     max-requests))
  (unless (or (null on-error) (functionp on-error))
    (%http1-session-error "HTTP/1 session on-error must be a function or NIL."
                          (type-of on-error)))
  (unless (or (null on-upgrade) (functionp on-upgrade))
    (%http1-session-error "HTTP/1 session on-upgrade must be a function or NIL."
                          (type-of on-upgrade)))
  (unless (or (eq on-expect-continue :automatic)
              (null on-expect-continue)
              (functionp on-expect-continue))
    (%http1-session-error
     "HTTP/1 session on-expect-continue must be :AUTOMATIC, a function, or NIL."
     (type-of on-expect-continue)))
  (unless (or (null close-stream) (functionp close-stream))
    (%http1-session-error "HTTP/1 session close-stream must be a function or NIL."
                          (type-of close-stream))))

(defun %http1-session-make-expect-continue-callback
    (stream on-expect-continue)
  (cond
    ((eq on-expect-continue :automatic)
     (lambda (request)
       (%http1-session-write-continue stream request)))
    ((null on-expect-continue) nil)
    (t on-expect-continue)))

(defun %http1-session-read-request
    (stream absolute-deadline max-header-bytes max-body-bytes default-authority
     on-body-chunk collect-body-p expect-continue-callback clock-function)
  (parse-http-request
   stream
   :deadline absolute-deadline
   :max-header-bytes max-header-bytes
   :max-body-bytes max-body-bytes
   :default-authority default-authority
   :on-body-chunk on-body-chunk
   :collect-body-p collect-body-p
   :on-expect-continue expect-continue-callback
   :clock-function clock-function
   :allow-eof-p t))

(defun %http1-session-dispatch-request
    (stream handler request on-upgrade)
  (%http1-session-write-handler-response
   stream
   request
   (funcall handler request)
   :on-upgrade on-upgrade))
