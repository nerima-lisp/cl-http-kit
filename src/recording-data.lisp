(in-package #:http-kit)

(defstruct (recording-session
             (:conc-name %recording-)
             (:constructor %make-recording-session
                 (&key (pending-responses nil) response-function)))
  (pending-responses nil)
  (response-function nil)
  (requests nil)
  (responses nil))
