(in-package #:http-kit/client)

(defparameter *base64-alphabet*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun %base64-encode-octets (octets)
  (with-output-to-string (stream)
    (loop for index from 0 below (length octets) by 3
          for remaining = (- (length octets) index)
          for first = (aref octets index)
          for second = (and (> remaining 1) (aref octets (1+ index)))
          for third = (and (> remaining 2) (aref octets (+ index 2)))
          for combined = (+ (ash first 16)
                            (ash (or second 0) 8)
                            (or third 0))
          do (write-char (char *base64-alphabet* (ldb (byte 6 18) combined)) stream)
             (write-char (char *base64-alphabet* (ldb (byte 6 12) combined)) stream)
             (if second
                 (write-char (char *base64-alphabet* (ldb (byte 6 6) combined)) stream)
                 (write-char #\= stream))
             (if third
                 (write-char (char *base64-alphabet* (ldb (byte 6 0) combined)) stream)
                 (write-char #\= stream)))))

(defun %auth-value-p (value)
  (and (stringp value)
       (not (find-if (lambda (character)
                       (or (char= character #\Return)
                           (char= character #\Linefeed)))
                     value))))

(defun http-basic-authorization (username password)
  "Return a Basic authorization value using UTF-8 credentials."
  (unless (%auth-value-p username)
    (%client-protocol-error "Basic authentication requires a valid username."
                            username))
  (unless (%auth-value-p password)
    (%client-protocol-error "Basic authentication requires a valid password."
                            password))
  (let* ((credentials (concatenate 'string username ":" password))
         (encoded (%base64-encode-octets (cl-codec-kit:string-to-octets credentials :encoding :utf-8))))
    (concatenate 'string "Basic " encoded)))

(defun http-bearer-authorization (token)
  "Return a Bearer authorization value for TOKEN."
  (unless (%auth-value-p token)
    (%client-protocol-error "Bearer authentication requires a valid token." token))
  (concatenate 'string "Bearer " token))
