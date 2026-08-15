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
                       (let ((code (char-code character)))
                         (or (<= code 31)
                             (= code 127))))
                     value))))

(defun %bearer-token-character-p (character)
  (or (and (char<= #\A character) (char<= character #\Z))
      (and (char<= #\a character) (char<= character #\z))
      (and (char<= #\0 character) (char<= character #\9))
      (find character "-._~+/" :test #'char=)))

(defun %bearer-token-p (token)
  (and (stringp token)
       (plusp (length token))
       (let ((padding (position #\= token)))
         (and (loop for index below (or padding (length token))
                    always (%bearer-token-character-p (char token index)))
              (or (null padding)
                  (and (plusp padding)
                       (loop for index from padding below (length token)
                             always (char= (char token index) #\=))))))))

(defun %auth-whitespace-p (character)
  (or (char= character #\Space) (char= character #\Tab)))

(defun %auth-trim (string)
  (string-trim '(#\Space #\Tab) string))

(defun %auth-comma-parts (value)
  (let ((parts nil)
        (start 0)
        (quoted-p nil)
        (escaped-p nil))
    (loop for index below (length value)
          for character = (char value index)
          do (cond (escaped-p (setf escaped-p nil))
                   ((and quoted-p (char= character #\\))
                    (setf escaped-p t))
                   ((char= character #\")
                    (setf quoted-p (not quoted-p)))
                   ((and (not quoted-p) (char= character #\,))
                    (push (%auth-trim (subseq value start index)) parts)
                    (setf start (1+ index)))))
    (unless (or quoted-p escaped-p)
      (push (%auth-trim (subseq value start)) parts)
      (nreverse parts))))

(defun %auth-parameter-part-p (part)
  (let ((equals (position #\= part)))
    (and equals
         (http-kit::%token-p (%auth-trim (subseq part 0 equals))))))

(defun %auth-unquote (value)
  (if (and (>= (length value) 2)
           (char= (char value 0) #\")
           (char= (char value (1- (length value))) #\"))
      (with-output-to-string (output)
        (loop with escaped-p = nil
              for index from 1 below (1- (length value))
              for character = (char value index)
              for code = (char-code character)
              do (cond (escaped-p
                        (unless (or (= code 9) (= code 32) (<= 33 code 126)
                                    (>= code 128))
                          (return-from %auth-unquote nil))
                        (write-char character output)
                        (setf escaped-p nil))
                       ((char= character #\\) (setf escaped-p t))
                       ((or (= code 9) (= code 32) (= code 33)
                            (<= 35 code 91) (<= 93 code 126)
                            (>= code 128))
                        (write-char character output))
                       (t (return-from %auth-unquote nil)))
              finally (when escaped-p (return-from %auth-unquote nil))))
      (and (http-kit::%token-p value) value)))

(defun %auth-parameter (part)
  (let ((equals (position #\= part)))
    (when equals
      (let ((name (%auth-trim (subseq part 0 equals)))
            (value (%auth-trim (subseq part (1+ equals)))))
        (let ((decoded (%auth-unquote value)))
          (when (and (http-kit::%token-p name) decoded)
            (cons name decoded)))))))

(defun %auth-parse-challenge (parts)
  (let* ((first (first parts))
         (space (position-if #'%auth-whitespace-p first))
         (scheme (if space (subseq first 0 space) first))
         (rest (and space (%auth-trim (subseq first space)))))
    (when (http-kit::%token-p scheme)
      (cond ((and rest (%bearer-token-p rest) (null (rest parts)))
             (%make-http-authentication-challenge
              :scheme scheme :token68 rest))
            ((or (null rest) (%auth-parameter-part-p rest))
             (let* ((parameter-parts (if rest (cons rest (rest parts))
                                         (rest parts)))
                    (parameters (mapcar #'%auth-parameter parameter-parts)))
               (when (and (every #'identity parameters)
                          (= (length parameters)
                             (length (remove-duplicates
                                      parameters :key #'car
                                      :test #'string-equal))))
                 (%make-http-authentication-challenge
                  :scheme scheme :parameters parameters))))))))

(defun http-parse-authentication-challenges (values)
  "Parse RFC 9110 authentication challenge field VALUES."
  (let ((values (if (stringp values) (list values) values))
        (result nil))
    (unless (and (listp values) (every #'stringp values))
      (%client-protocol-error
       "Authentication challenges must be a string or list of strings."
       values))
    (dolist (value values)
      (let ((groups nil)
            (current nil))
        (dolist (part (or (%auth-comma-parts value) nil))
          (cond ((zerop (length part)))
                ((and current (%auth-parameter-part-p part))
                 (setf current (append current (list part))))
                (t
                 (when current (push current groups))
                 (setf current (list part)))))
        (when current (push current groups))
        (dolist (group (nreverse groups))
          (let ((challenge (%auth-parse-challenge group)))
            (when challenge (push challenge result))))))
    (nreverse result)))

(defun http-authentication-challenge-parameter (challenge name)
  (unless (and (http-authentication-challenge-p challenge)
               (stringp name))
    (%client-protocol-error "Invalid authentication challenge lookup."
                            (list challenge name)))
  (cdr (assoc name (http-authentication-challenge-parameters challenge)
              :test #'string-equal)))

(defun http-basic-authorization (username password)
  "Return a Basic authorization value using UTF-8 credentials."
  (unless (%auth-value-p username)
    (%client-protocol-error "Basic authentication requires a valid username."
                            :username))
  (when (find #\: username)
    (%client-protocol-error "Basic authentication usernames cannot contain a colon."
                            :username))
  (unless (%auth-value-p password)
    (%client-protocol-error "Basic authentication requires a valid password."
                            :password))
  (let* ((credentials (concatenate 'string username ":" password))
         (encoded (%base64-encode-octets (cl-codec-kit:string-to-octets credentials :encoding :utf-8))))
    (concatenate 'string "Basic " encoded)))

(defun http-bearer-authorization (token)
  "Return a Bearer authorization value for TOKEN."
  (unless (%bearer-token-p token)
    (%client-protocol-error "Bearer authentication requires a valid token." :token))
  (concatenate 'string "Bearer " token))

(defun %digest-rotate-left (value count)
  (logand #xffffffff
          (logior (ash value count)
                  (ash value (- count 32)))))

(defun %digest-store-word (octets offset value little-endian-p)
  (dotimes (index 4)
    (setf (aref octets (+ offset index))
          (ldb (byte 8 (if little-endian-p
                           (* index 8)
                           (* (- 3 index) 8)))
               value))))

(defun %digest-store-double-word (octets offset value)
  (dotimes (index 8)
    (setf (aref octets (+ offset index))
          (ldb (byte 8 (* (- 7 index) 8)) value))))

(defun %digest-pad (octets little-endian-p)
  (let* ((length (length octets))
         (padded-length (+ length 1 (mod (- 56 (mod (1+ length) 64)) 64) 8))
         (result (make-array padded-length :element-type '(unsigned-byte 8)))
         (bit-length (* length 8)))
    (replace result octets)
    (setf (aref result length) #x80)
    (dotimes (index 8)
      (setf (aref result (+ (- padded-length 8) index))
            (ldb (byte 8 (if little-endian-p
                             (* index 8)
                             (* (- 7 index) 8)))
                 bit-length)))
    result))

(defun %digest-md5 (octets)
  (let ((padded (%digest-pad octets t))
        (a0 #x67452301)
        (b0 #xefcdab89)
        (c0 #x98badcfe)
        (d0 #x10325476)
        (shifts #(7 12 17 22 7 12 17 22 7 12 17 22 7 12 17 22
                  5 9 14 20 5 9 14 20 5 9 14 20 5 9 14 20
                  4 11 16 23 4 11 16 23 4 11 16 23 4 11 16 23
                  6 10 15 21 6 10 15 21 6 10 15 21 6 10 15 21))
        (constants
          #(#xd76aa478 #xe8c7b756 #x242070db #xc1bdceee #xf57c0faf #x4787c62a
            #xa8304613 #xfd469501 #x698098d8 #x8b44f7af #xffff5bb1 #x895cd7be
            #x6b901122 #xfd987193 #xa679438e #x49b40821 #xf61e2562 #xc040b340
            #x265e5a51 #xe9b6c7aa #xd62f105d #x02441453 #xd8a1e681 #xe7d3fbc8
            #x21e1cde6 #xc33707d6 #xf4d50d87 #x455a14ed #xa9e3e905 #xfcefa3f8
            #x676f02d9 #x8d2a4c8a #xfffa3942 #x8771f681 #x6d9d6122 #xfde5380c
            #xa4beea44 #x4bdecfa9 #xf6bb4b60 #xbebfbc70 #x289b7ec6 #xeaa127fa
            #xd4ef3085 #x04881d05 #xd9d4d039 #xe6db99e5 #x1fa27cf8 #xc4ac5665
            #xf4292244 #x432aff97 #xab9423a7 #xfc93a039 #x655b59c3 #x8f0ccc92
            #xffeff47d #x85845dd1 #x6fa87e4f #xfe2ce6e0 #xa3014314 #x4e0811a1
            #xf7537e82 #xbd3af235 #x2ad7d2bb #xeb86d391)))
    (loop for offset from 0 below (length padded) by 64
          do (let ((words (make-array 16 :element-type '(unsigned-byte 32)))
                   (a a0) (b b0) (c c0) (d d0))
               (dotimes (index 16)
                 (setf (aref words index)
                       (loop for byte below 4
                             sum (ash (aref padded (+ offset (* index 4) byte))
                                      (* byte 8)))))
               (dotimes (index 64)
                 (multiple-value-bind (function word-index)
                     (cond ((< index 16)
                            (values (logior (logand b c) (logand (lognot b) d))
                                    index))
                           ((< index 32)
                            (values (logior (logand d b) (logand (lognot d) c))
                                    (mod (+ (* 5 index) 1) 16)))
                           ((< index 48)
                            (values (logxor b c d) (mod (+ (* 3 index) 5) 16)))
                           (t
                            (values (logxor c (logior b (lognot d)))
                                    (mod (* 7 index) 16))))
                   (let ((next-b
                           (logand #xffffffff
                                   (+ b (%digest-rotate-left
                                         (logand #xffffffff
                                                 (+ a function
                                                    (aref constants index)
                                                    (aref words word-index)))
                                         (aref shifts index))))))
                     (setf a d d c c b b next-b))))
               (setf a0 (logand #xffffffff (+ a0 a))
                     b0 (logand #xffffffff (+ b0 b))
                     c0 (logand #xffffffff (+ c0 c))
                     d0 (logand #xffffffff (+ d0 d)))))
    (let ((result (make-array 16 :element-type '(unsigned-byte 8))))
      (%digest-store-word result 0 a0 t)
      (%digest-store-word result 4 b0 t)
      (%digest-store-word result 8 c0 t)
      (%digest-store-word result 12 d0 t)
      result)))

(defun %digest-sha256 (octets)
  (let ((padded (%digest-pad octets nil))
        (state (copy-seq
                #(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                  #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))
        (constants
          #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1
            #x923f82a4 #xab1c5ed5 #xd807aa98 #x12835b01 #x243185be #x550c7dc3
            #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174 #xe49b69c1 #xefbe4786
            #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
            #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147
            #x06ca6351 #x14292967 #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
            #x650a7354 #x766a0abb #x81c2c92e #x92722c85 #xa2bfe8a1 #xa81a664b
            #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
            #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a
            #x5b9cca4f #x682e6ff3 #x748f82ee #x78a5636f #x84c87814 #x8cc70208
            #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2)))
    (labels ((ror (value count)
               (logand #xffffffff
                       (logior (ash value (- count))
                               (ash value (- 32 count))))))
      (loop for offset from 0 below (length padded) by 64
            do (let ((words (make-array 64 :element-type '(unsigned-byte 32))))
                 (dotimes (index 16)
                   (setf (aref words index)
                         (loop for byte below 4
                               sum (ash (aref padded (+ offset (* index 4) byte))
                                        (* (- 3 byte) 8)))))
                 (loop for index from 16 below 64
                       for x = (aref words (- index 15))
                       for y = (aref words (- index 2))
                       for sigma0 = (logxor (ror x 7) (ror x 18) (ash x -3))
                       for sigma1 = (logxor (ror y 17) (ror y 19) (ash y -10))
                       do (setf (aref words index)
                                (logand #xffffffff
                                        (+ (aref words (- index 16)) sigma0
                                           (aref words (- index 7)) sigma1))))
                 (let ((a (aref state 0)) (b (aref state 1))
                       (c (aref state 2)) (d (aref state 3))
                       (e (aref state 4)) (f (aref state 5))
                       (g (aref state 6)) (h (aref state 7)))
                   (dotimes (index 64)
                     (let* ((sum1 (logxor (ror e 6) (ror e 11) (ror e 25)))
                            (choice (logxor (logand e f) (logand (lognot e) g)))
                            (temporary1 (logand #xffffffff
                                                     (+ h sum1 choice
                                                        (aref constants index)
                                                        (aref words index))))
                            (sum0 (logxor (ror a 2) (ror a 13) (ror a 22)))
                            (majority (logxor (logand a b) (logand a c)
                                              (logand b c)))
                            (temporary2 (logand #xffffffff (+ sum0 majority))))
                       (setf h g g f f e
                             e (logand #xffffffff (+ d temporary1))
                             d c c b b a
                             a (logand #xffffffff (+ temporary1 temporary2)))))
                   (map-into state
                             (lambda (old new) (logand #xffffffff (+ old new)))
                             state (vector a b c d e f g h))))))
    (let ((result (make-array 32 :element-type '(unsigned-byte 8))))
      (dotimes (index 8)
        (%digest-store-word result (* index 4) (aref state index) nil))
      result)))

(defun %content-digest-octets (content)
  (unless (typep content '(vector (unsigned-byte 8)))
    (%client-protocol-error
     "Content-Digest requires an octet vector." :content))
  content)

(defun %content-digest-algorithm (algorithm)
  (unless (eq algorithm :sha-256)
    (%client-protocol-error
     "Unsupported Content-Digest algorithm." algorithm))
  (values "sha-256" #'%digest-sha256 32))

(defun %structured-field-key-p (key)
  (and (plusp (length key))
       (or (char<= #\a (char key 0) #\z)
           (char= (char key 0) #\*))
       (loop for character across key
             always (or (char<= #\a character #\z)
                        (char<= #\0 character #\9)
                        (find character "_-.*" :test #'char=)))))

(defun %base64-value (character)
  (position character *base64-alphabet* :test #'char=))

(defun %base64-decode-octets (value)
  (let ((length (length value)))
    (when (zerop length)
      (return-from %base64-decode-octets
        (make-array 0 :element-type '(unsigned-byte 8))))
    (unless (zerop (mod length 4))
      (return-from %base64-decode-octets nil))
    (let* ((padding (cond ((char= (char value (1- length)) #\=)
                           (if (and (> length 1)
                                    (char= (char value (- length 2)) #\=))
                               2 1))
                          (t 0)))
           (result (make-array (- (* (/ length 4) 3) padding)
                               :element-type '(unsigned-byte 8)))
           (output-index 0))
      (loop for index from 0 below length by 4
            for last-p = (= (+ index 4) length)
            for a = (%base64-value (char value index))
            for b = (%base64-value (char value (+ index 1)))
            for c-character = (char value (+ index 2))
            for d-character = (char value (+ index 3))
            for c = (and (char/= c-character #\=)
                         (%base64-value c-character))
            for d = (and (char/= d-character #\=)
                         (%base64-value d-character))
            do (unless (and a b
                            (or c (and last-p (= padding 2)))
                            (or d (and last-p (plusp padding)))
                            (or (not last-p)
                                (case padding
                                  (0 t)
                                  (1 (and c (zerop (logand c 3))))
                                  (2 (zerop (logand b 15))))))
                 (return-from %base64-decode-octets nil))
               (let ((combined (+ (ash a 18) (ash b 12)
                                  (ash (or c 0) 6) (or d 0))))
                 (when (< output-index (length result))
                   (setf (aref result output-index)
                         (ldb (byte 8 16) combined))
                   (incf output-index))
                 (when (< output-index (length result))
                   (setf (aref result output-index)
                         (ldb (byte 8 8) combined))
                   (incf output-index))
                 (when (< output-index (length result))
                   (setf (aref result output-index)
                         (ldb (byte 8 0) combined))
                   (incf output-index))))
      result)))

(defun %structured-field-token-p (value)
  (and (plusp (length value))
       (or (alpha-char-p (char value 0))
           (char= (char value 0) #\*))
       (loop for character across value
             always (or (alphanumericp character)
                        (find character "!#$%&'*+-.^_`|~:/"
                              :test #'char=)))))

(defun %structured-field-number-p (value)
  (let* ((start (if (and (plusp (length value))
                         (char= (char value 0) #\-))
                    1 0))
         (dot (position #\. value :start start)))
    (and (< start (length value))
         (loop for index from start below (length value)
               for character = (char value index)
               always (or (digit-char-p character)
                          (and dot (= index dot))))
         (if (null dot)
             (<= (- (length value) start) 15)
             (and (< start dot)
                  (< dot (1- (length value)))
                  (<= (- dot start) 12)
                  (<= (- (length value) dot 1) 3))))))

(defun %structured-field-string-p (value)
  (and (>= (length value) 2)
       (char= (char value 0) #\")
       (char= (char value (1- (length value))) #\")
       (loop with escaped-p = nil
             for index from 1 below (1- (length value))
             for character = (char value index)
             for code = (char-code character)
             always (cond (escaped-p
                           (prog1 (find character "\\\"" :test #'char=)
                             (setf escaped-p nil)))
                          ((char= character #\\)
                           (setf escaped-p t))
                          (t (or (= code 32) (<= 33 code 126))))
             finally (return (not escaped-p)))))

(defun %structured-field-bare-item-p (value)
  (or (%structured-field-number-p value)
      (%structured-field-string-p value)
      (%structured-field-token-p value)
      (and (= (length value) 2)
           (char= (char value 0) #\?)
           (find (char value 1) "01" :test #'char=))
      (and (>= (length value) 2)
           (char= (char value 0) #\:)
           (char= (char value (1- (length value))) #\:)
           (%base64-decode-octets (subseq value 1 (1- (length value)))))))

(defun %structured-field-parameters-p (value)
  (let ((index 0)
        (length (length value))
        (keys nil))
    (loop while (< index length)
          do (unless (char= (char value index) #\;)
               (return-from %structured-field-parameters-p nil))
             (incf index)
             (let ((start index))
               (loop while (and (< index length)
                                (not (find (char value index) "=;"
                                           :test #'char=)))
                     do (incf index))
               (let ((key (subseq value start index)))
                 (unless (and (%structured-field-key-p key)
                              (not (member key keys :test #'string=)))
                   (return-from %structured-field-parameters-p nil))
                 (push key keys)))
             (when (and (< index length) (char= (char value index) #\=))
               (incf index)
               (let ((start index)
                     (quoted-p nil)
                     (escaped-p nil))
                 (loop while (< index length)
                       for character = (char value index)
                       do (cond (escaped-p (setf escaped-p nil))
                                ((and quoted-p (char= character #\\))
                                 (setf escaped-p t))
                                ((char= character #\")
                                 (setf quoted-p (not quoted-p)))
                                ((and (not quoted-p) (char= character #\;))
                                 (loop-finish)))
                          (incf index))
                 (unless (and (not quoted-p) (not escaped-p)
                              (%structured-field-bare-item-p
                               (subseq value start index)))
                   (return-from %structured-field-parameters-p nil))))
          finally (return t))))

(defun %content-digest-members (values)
  (let ((values (if (stringp values) (list values) values))
        (members nil))
    (unless (and (consp values) (every #'stringp values))
      (%client-protocol-error
       "Content-Digest must be a string or non-empty list of strings." :field))
    (dolist (value values)
      (let ((parts (%auth-comma-parts value)))
        (unless (and parts (every #'plusp (mapcar #'length parts)))
          (%client-protocol-error "Malformed Content-Digest field." :field))
        (dolist (part parts)
        (let* ((part (%auth-trim part))
               (equals (position #\= part))
               (semicolon (position #\; part))
               (end (or semicolon (length part)))
               (key (and equals (%auth-trim (subseq part 0 equals))))
               (item (and equals (%auth-trim (subseq part (1+ equals) end)))))
          (unless (and key (%structured-field-key-p key)
                       item (>= (length item) 2)
                       (char= (char item 0) #\:)
                       (char= (char item (1- (length item))) #\:)
                       (or (null semicolon)
                           (%structured-field-parameters-p
                            (subseq part semicolon))))
            (%client-protocol-error "Malformed Content-Digest field." :field))
          (when (assoc key members :test #'string=)
            (%client-protocol-error "Duplicate Content-Digest algorithm." key))
          (let ((decoded (%base64-decode-octets
                          (subseq item 1 (1- (length item))))))
            (unless decoded
              (%client-protocol-error "Malformed Content-Digest byte sequence."
                                      key))
            (push (cons key decoded) members))))))
    (nreverse members)))

(defun %constant-time-octets= (left right)
  (let ((difference (logxor (length left) (length right))))
    (dotimes (index (max (length left) (length right)) (zerop difference))
      (setf difference
            (logior difference
                    (logxor (if (< index (length left)) (aref left index) 0)
                            (if (< index (length right)) (aref right index) 0)))))))

(defun http-content-digest (content &key (algorithm :sha-256))
  "Return an RFC 9530 Content-Digest field value for CONTENT."
  (multiple-value-bind (key function expected-length)
      (%content-digest-algorithm algorithm)
    (declare (ignore expected-length))
    (format nil "~A=:~A:" key
            (%base64-encode-octets
             (funcall function (%content-digest-octets content))))))

(defun http-content-digest-valid-p (content values &key (algorithm :sha-256))
  "Return true when VALUES contains a valid digest of CONTENT for ALGORITHM."
  (multiple-value-bind (key function expected-length)
      (%content-digest-algorithm algorithm)
    (let ((member (assoc key (%content-digest-members values) :test #'string=)))
      (and member
           (= (length (cdr member)) expected-length)
           (%constant-time-octets=
            (funcall function (%content-digest-octets content))
            (cdr member))))))

(defun %digest-sha512-256 (octets)
  (let* ((length (length octets))
         (padded-length (+ length 1 (mod (- 112 (mod (1+ length) 128)) 128) 16))
         (padded (make-array padded-length :element-type '(unsigned-byte 8)))
         (state (copy-seq
                 #(#x22312194fc2bf72c #x9f555fa3c84c64c2
                   #x2393b86b6f53b151 #x963877195940eabd
                   #x96283ee2a88effe3 #xbe5e1e2553863992
                   #x2b0199fc2c85b8aa #x0eb72ddc81c52ca2)))
         (constants
           #(#x428a2f98d728ae22 #x7137449123ef65cd #xb5c0fbcfec4d3b2f
             #xe9b5dba58189dbbc #x3956c25bf348b538 #x59f111f1b605d019
             #x923f82a4af194f9b #xab1c5ed5da6d8118 #xd807aa98a3030242
             #x12835b0145706fbe #x243185be4ee4b28c #x550c7dc3d5ffb4e2
             #x72be5d74f27b896f #x80deb1fe3b1696b1 #x9bdc06a725c71235
             #xc19bf174cf692694 #xe49b69c19ef14ad2 #xefbe4786384f25e3
             #x0fc19dc68b8cd5b5 #x240ca1cc77ac9c65 #x2de92c6f592b0275
             #x4a7484aa6ea6e483 #x5cb0a9dcbd41fbd4 #x76f988da831153b5
             #x983e5152ee66dfab #xa831c66d2db43210 #xb00327c898fb213f
             #xbf597fc7beef0ee4 #xc6e00bf33da88fc2 #xd5a79147930aa725
             #x06ca6351e003826f #x142929670a0e6e70 #x27b70a8546d22ffc
             #x2e1b21385c26c926 #x4d2c6dfc5ac42aed #x53380d139d95b3df
             #x650a73548baf63de #x766a0abb3c77b2a8 #x81c2c92e47edaee6
             #x92722c851482353b #xa2bfe8a14cf10364 #xa81a664bbc423001
             #xc24b8b70d0f89791 #xc76c51a30654be30 #xd192e819d6ef5218
             #xd69906245565a910 #xf40e35855771202a #x106aa07032bbd1b8
             #x19a4c116b8d2d0c8 #x1e376c085141ab53 #x2748774cdf8eeb99
             #x34b0bcb5e19b48a8 #x391c0cb3c5c95a63 #x4ed8aa4ae3418acb
             #x5b9cca4f7763e373 #x682e6ff3d6b2b8a3 #x748f82ee5defb2fc
             #x78a5636f43172f60 #x84c87814a1f0ab72 #x8cc702081a6439ec
             #x90befffa23631e28 #xa4506cebde82bde9 #xbef9a3f7b2c67915
             #xc67178f2e372532b #xca273eceea26619c #xd186b8c721c0c207
             #xeada7dd6cde0eb1e #xf57d4f7fee6ed178 #x06f067aa72176fba
             #x0a637dc5a2c898a6 #x113f9804bef90dae #x1b710b35131c471b
             #x28db77f523047d84 #x32caab7b40c72493 #x3c9ebe0a15c9bebc
             #x431d67c49c100d4c #x4cc5d4becb3e42b6 #x597f299cfc657e2a
             #x5fcb6fab3ad6faec #x6c44198c4a475817)))
    (replace padded octets)
    (setf (aref padded length) #x80)
    (%digest-store-double-word padded (- padded-length 8) (* length 8))
    (labels ((ror (value count)
               (logand #xffffffffffffffff
                       (logior (ash value (- count))
                               (ash value (- 64 count))))))
      (loop for offset from 0 below padded-length by 128
            do (let ((words (make-array 80 :element-type '(unsigned-byte 64))))
                 (dotimes (index 16)
                   (setf (aref words index)
                         (loop for byte below 8
                               sum (ash (aref padded (+ offset (* index 8) byte))
                                        (* (- 7 byte) 8)))))
                 (loop for index from 16 below 80
                       for x = (aref words (- index 15))
                       for y = (aref words (- index 2))
                       for sigma0 = (logxor (ror x 1) (ror x 8) (ash x -7))
                       for sigma1 = (logxor (ror y 19) (ror y 61) (ash y -6))
                       do (setf (aref words index)
                                (logand #xffffffffffffffff
                                        (+ (aref words (- index 16)) sigma0
                                           (aref words (- index 7)) sigma1))))
                 (let ((a (aref state 0)) (b (aref state 1))
                       (c (aref state 2)) (d (aref state 3))
                       (e (aref state 4)) (f (aref state 5))
                       (g (aref state 6)) (h (aref state 7)))
                   (dotimes (index 80)
                     (let* ((sum1 (logxor (ror e 14) (ror e 18) (ror e 41)))
                            (choice (logxor (logand e f) (logand (lognot e) g)))
                            (temporary1
                              (logand #xffffffffffffffff
                                      (+ h sum1 choice (aref constants index)
                                         (aref words index))))
                            (sum0 (logxor (ror a 28) (ror a 34) (ror a 39)))
                            (majority (logxor (logand a b) (logand a c)
                                              (logand b c)))
                            (temporary2
                              (logand #xffffffffffffffff (+ sum0 majority))))
                       (setf h g g f f e
                             e (logand #xffffffffffffffff (+ d temporary1))
                             d c c b b a
                             a (logand #xffffffffffffffff
                                       (+ temporary1 temporary2)))))
                   (map-into state
                             (lambda (old new)
                               (logand #xffffffffffffffff (+ old new)))
                             state (vector a b c d e f g h))))))
    (let ((result (make-array 32 :element-type '(unsigned-byte 8))))
      (dotimes (index 4)
        (%digest-store-double-word result (* index 8) (aref state index)))
      result)))

(defun %digest-hex (octets)
  (with-output-to-string (stream)
    (loop for octet across octets
          do (format stream "~2,'0x" octet))))

(defun %digest-hash-string (algorithm string)
  (%digest-hex
   (funcall (ecase algorithm
              (:md5 #'%digest-md5)
              (:sha-256 #'%digest-sha256)
              (:sha-512-256 #'%digest-sha512-256))
            (http-utf8-octets string))))

(defun %digest-quote (value)
  (unless (%auth-value-p value)
    (%client-protocol-error "Digest authentication contains an invalid value."
                            value))
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop for character across value
          do (when (or (char= character #\") (char= character #\\))
               (write-char #\\ stream))
             (write-char character stream))
    (write-char #\" stream)))

(defun %digest-extended-username (username)
  (with-output-to-string (stream)
    (write-string "UTF-8''" stream)
    (loop for octet across (http-utf8-octets username)
          for character = (code-char octet)
          do (if (or (<= (char-code #\A) octet (char-code #\Z))
                     (<= (char-code #\a) octet (char-code #\z))
                     (<= (char-code #\0) octet (char-code #\9))
                     (find character "!#$&+-.^_`|~" :test #'char=))
                 (write-char character stream)
                 (format stream "%~2,'0X" octet)))))

(defun %digest-qop (challenge requested body)
  (let* ((offered (http-authentication-challenge-parameter challenge "qop"))
         (options (and offered
                       (mapcar #'%auth-trim (%auth-comma-parts offered))))
         (requested (and requested (string-downcase (string requested)))))
    (cond ((null offered) nil)
          ((and requested (member requested options :test #'string-equal)) requested)
          (requested
           (%client-protocol-error "The requested Digest qop was not offered."
                                   requested))
          ((member "auth" options :test #'string-equal) "auth")
          ((and body (member "auth-int" options :test #'string-equal)) "auth-int")
          (t (%client-protocol-error "No supported Digest qop was offered."
                                     offered)))))

(defun http-digest-authorization (challenge method request-target username password
                                  cnonce &key (nonce-count 1) qop entity-body)
  "Return an RFC 7616 Digest authorization value for CHALLENGE."
  (unless (and (http-authentication-challenge-p challenge)
               (string-equal (http-authentication-challenge-scheme challenge)
                             "Digest"))
    (%client-protocol-error "Digest authentication requires a Digest challenge."
                            challenge))
  (dolist (value (list method request-target username password cnonce))
    (unless (%auth-value-p value)
      (%client-protocol-error "Digest authentication requires valid string inputs."
                              value)))
  (unless (and (integerp nonce-count) (<= 1 nonce-count #xffffffff))
    (%client-protocol-error "Digest nonce-count must be between 1 and 2^32-1."
                            nonce-count))
  (unless (or (null entity-body)
              (and (vectorp entity-body)
                   (every (lambda (value) (typep value '(unsigned-byte 8)))
                          entity-body)))
    (%client-protocol-error "Digest entity-body must be a vector of octets."
                            :entity-body))
  (let* ((realm (http-authentication-challenge-parameter challenge "realm"))
         (nonce (http-authentication-challenge-parameter challenge "nonce"))
         (opaque (http-authentication-challenge-parameter challenge "opaque"))
         (charset (http-authentication-challenge-parameter challenge "charset"))
         (userhash (http-authentication-challenge-parameter challenge "userhash"))
         (algorithm-name
           (or (http-authentication-challenge-parameter challenge "algorithm")
               "MD5"))
         (session-p (or (string-equal algorithm-name "MD5-sess")
                        (string-equal algorithm-name "SHA-256-sess")
                        (string-equal algorithm-name "SHA-512-256-sess")))
         (algorithm
           (cond ((or (string-equal algorithm-name "MD5")
                      (string-equal algorithm-name "MD5-sess")) :md5)
                 ((or (string-equal algorithm-name "SHA-256")
                      (string-equal algorithm-name "SHA-256-sess")) :sha-256)
                 ((or (string-equal algorithm-name "SHA-512-256")
                      (string-equal algorithm-name "SHA-512-256-sess"))
                  :sha-512-256)
                 (t (%client-protocol-error
                     "Unsupported Digest algorithm." algorithm-name))))
         (selected-qop (%digest-qop challenge qop entity-body))
         (nc (format nil "~8,'0x" nonce-count)))
    (unless (and realm nonce)
      (%client-protocol-error "Digest challenge requires realm and nonce."
                              challenge))
    (when (and charset (not (string-equal charset "UTF-8")))
      (%client-protocol-error "Unsupported Digest charset." charset))
    (let* ((userhash-p (and userhash (string-equal userhash "true")))
           (extended-username-p
             (and (not userhash-p)
                  (some (lambda (character) (> (char-code character) 127))
                        username)))
           (authorization-username
             (if userhash-p
                 (%digest-hash-string
                  algorithm (format nil "~A:~A" username realm))
                 username))
           (initial-a1 (%digest-hash-string
                        algorithm (format nil "~A:~A:~A" username realm password)))
           (a1 (if session-p
                   (%digest-hash-string
                    algorithm (format nil "~A:~A:~A" initial-a1 nonce cnonce))
                   initial-a1))
           (body-hash
             (and selected-qop
                  (string-equal selected-qop "auth-int")
                  (%digest-hex
                   (funcall (ecase algorithm
                              (:md5 #'%digest-md5)
                              (:sha-256 #'%digest-sha256)
                              (:sha-512-256 #'%digest-sha512-256))
                            (or entity-body
                                (make-array 0 :element-type '(unsigned-byte 8)))))))
           (a2 (%digest-hash-string
                algorithm
                (if body-hash
                    (format nil "~A:~A:~A" method request-target body-hash)
                    (format nil "~A:~A" method request-target))))
           (response
             (%digest-hash-string
              algorithm
              (if selected-qop
                  (format nil "~A:~A:~A:~A:~A:~A"
                          a1 nonce nc cnonce selected-qop a2)
                  (format nil "~A:~A:~A" a1 nonce a2))))
           (base
             (if selected-qop
                 (format nil
                         "Digest ~A=~A, realm=~A, uri=~A, algorithm=~A, nonce=~A, nc=~A, cnonce=~A, qop=~A, response=~A"
                         (if extended-username-p "username*" "username")
                         (if extended-username-p
                             (%digest-extended-username authorization-username)
                             (%digest-quote authorization-username))
                         (%digest-quote realm)
                         (%digest-quote request-target) algorithm-name
                         (%digest-quote nonce) nc (%digest-quote cnonce)
                         selected-qop (%digest-quote response))
                 (format nil
                         "Digest ~A=~A, realm=~A, uri=~A, algorithm=~A, nonce=~A, response=~A"
                         (if extended-username-p "username*" "username")
                         (if extended-username-p
                             (%digest-extended-username authorization-username)
                             (%digest-quote authorization-username))
                         (%digest-quote realm)
                         (%digest-quote request-target) algorithm-name
                         (%digest-quote nonce) (%digest-quote response)))))
      (let ((with-opaque
              (if opaque
                  (format nil "~A, opaque=~A" base (%digest-quote opaque))
                  base)))
        (cond (userhash-p (format nil "~A, userhash=true" with-opaque))
              ((and userhash (string-equal userhash "false"))
               (format nil "~A, userhash=false" with-opaque))
              (t with-opaque))))))
