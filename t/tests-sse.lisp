(in-package #:http-kit/test)

(deftest client-sse-parse-and-serialize
  (let* ((linefeed (string #\Linefeed))
         (input
           (concatenate-octets
            (octets #xef #xbb #xbf)
            (ascii
             "event: update|CRLF|data: hello|CRLF|data: world|CRLF|id: 7|CRLF|")
            (ascii "retry: 1500|CRLF||CRLF|:keepalive")
            (octets #x0a)
            (ascii "data: final")))
         (events (parse-http-sse-events input)))
    (ensure-equal 2 (length events))
    (let ((first (first events))
          (second (second events)))
      (ensure-equal "update" (http-sse-event-event first))
      (ensure-equal (concatenate 'string "hello" linefeed "world")
                    (http-sse-event-data first))
      (ensure-equal "7" (http-sse-event-id first))
      (ensure-equal 1500 (http-sse-event-retry first))
      (ensure-equal "message" (http-sse-event-event second))
      (ensure-equal "final" (http-sse-event-data second))
      (ensure-equal '("keepalive") (http-sse-event-comments second)))
    (let* ((event
             (make-http-sse-event
              :event "notice"
              :data (concatenate 'string "a" linefeed "b")
              :id "9"
              :retry 10
              :comments '("c" "d")))
           (wire (serialize-http-sse-event event)))
      (ensure-equal
       (ascii
        (concatenate
         'string
         ":c|CRLF|:d|CRLF|event:notice|CRLF|id:9|CRLF|retry:10|CRLF|"
         "data:a|CRLF|data:b|CRLF||CRLF|"))
       wire)
      (let ((round-trip (first (parse-http-sse-events wire))))
        (ensure-equal "notice" (http-sse-event-event round-trip))
        (ensure-equal (concatenate 'string "a" linefeed "b")
                      (http-sse-event-data round-trip))
        (ensure-equal "9" (http-sse-event-id round-trip))
        (ensure-equal 10 (http-sse-event-retry round-trip))
        (ensure-equal '("c" "d") (http-sse-event-comments round-trip))))))

#+sbcl
(deftest client-sse-stream-callback-and-limits
  (let* ((linefeed (string #\Linefeed))
         (wire
           (concatenate-octets
            (ascii "data:one|CRLF|data:two")
            (octets #x0a #x0a)))
         (stream (make-instance 'binary-test-stream :input wire))
         (seen nil)
         (events
           (read-http-sse-events
            stream
            :on-event (lambda (event)
                        (push (http-sse-event-data event) seen)))))
    (ensure-equal (list (concatenate 'string "one" linefeed "two"))
                  (mapcar #'http-sse-event-data events))
    (ensure-equal (list (concatenate 'string "one" linefeed "two"))
                  (nreverse seen)))
  (signals http-size-limit-exceeded
    (parse-http-sse-events (concatenate-octets (ascii "data:one")
                                               (octets #x0a #x0a))
                           :max-line-bytes 4))
  (signals http-size-limit-exceeded
    (parse-http-sse-events
     (concatenate-octets
      (ascii "data:one")
      (octets #x0a #x0a)
      (ascii "data:two")
      (octets #x0a #x0a))
     :max-events 1))
  (signals http-protocol-error
    (parse-http-sse-events
     (octets #x64 #x61 #x74 #x61 #x3a #xc3 #x28 #x0a #x0a))))
