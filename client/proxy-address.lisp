(in-package #:http-kit/client)

(defun %proxy-ipv4-octets (host)
  (when (stringp host)
    (let ((start 0)
          (values nil)
          (valid-p t))
      (loop
        for separator = (position #\. host :start start)
        for end = (or separator (length host))
        for part = (and (< start end) (subseq host start end))
        do (unless (and part
                        (let ((value (%client-parse-integer
                                      part
                                      :allow-sign-p nil)))
                          (and value (<= value 255))))
             (setf valid-p nil))
           (when valid-p
             (push (%client-parse-integer
                    part
                    :allow-sign-p nil)
                   values))
           (if separator
               (setf start (1+ separator))
               (return)))
      (when (and valid-p (= (length values) 4))
        (let ((result (make-array 4 :element-type '(unsigned-byte 8))))
          (loop for value in (nreverse values)
                for index from 0
                do (setf (aref result index) value))
          result)))))

(defun %proxy-hex-value (character)
  (position character "0123456789abcdefABCDEF" :test #'char=))

(defun %proxy-hex-word (token)
  (when (and (stringp token)
             (not (string= token ""))
             (<= (length token) 4)
             (every (lambda (character)
                      (%proxy-hex-value character))
                    token))
    (let ((value 0))
      (loop for character across token
            for digit = (%proxy-hex-value character)
            do (setf value (+ (* value 16)
                              (if (< digit 16) digit (- digit 6)))))
      value)))

(defun %proxy-colon-parts (string)
  (let ((start 0)
        (parts nil))
    (loop
      for separator = (position #\: string :start start)
      for end = (or separator (length string))
      do (push (subseq string start end) parts)
         (if separator
             (setf start (1+ separator))
             (return (nreverse parts))))))

(defun %proxy-ipv6-octets (host)
  (when (and (stringp host)
             (not (find #\[ host)))
    (let* ((double (search "::" host))
           (second-double (and double
                               (search "::" host
                                       :start2 (1+ double))))
           (left-string (if double (subseq host 0 double) host))
           (right-string (and double (subseq host (+ double 2))))
           (left (if (string= left-string "")
                     nil
                     (%proxy-colon-parts left-string)))
           (right (if (or (null right-string)
                          (string= right-string ""))
                      nil
                      (%proxy-colon-parts right-string)))
           (parts (append left right))
           (last-index (1- (length parts)))
           (word-groups nil)
           (valid-p (null second-double)))
      (when (and (not double)
                 (or (some (lambda (part) (string= part "")) left)
                     (some (lambda (part) (string= part "")) right)))
        (setf valid-p nil))
      (loop for part in parts
            for index from 0
            do (cond
                 ((string= part "")
                  (setf valid-p nil))
                 ((find #\. part)
                  (let ((octets (%proxy-ipv4-octets part)))
                    (if (and octets (= index last-index))
                        (push (list (+ (ash (aref octets 0) 8)
                                       (aref octets 1))
                                    (+ (ash (aref octets 2) 8)
                                       (aref octets 3)))
                              word-groups)
                        (setf valid-p nil))))
                 (t
                  (let ((word (%proxy-hex-word part)))
                    (if word
                        (push (list word) word-groups)
                        (setf valid-p nil))))))
      (setf word-groups (nreverse word-groups))
      (let* ((words (loop for group in word-groups append group))
             (left-word-count
               (loop for part in left
                     sum (if (find #\. part) 2 1))))
        (when (and valid-p
                   (if double
                       (< (length words) 8)
                       (= (length words) 8)))
          (let* ((zeroes (if double (- 8 (length words)) 0))
                 (expanded (if double
                               (append (subseq words 0 left-word-count)
                                       (make-list zeroes :initial-element 0)
                                       (subseq words left-word-count))
                               words))
                 (result (make-array 16 :element-type '(unsigned-byte 8))))
            (loop for word in expanded
                  for index from 0 by 2
                  do (setf (aref result index) (ldb (byte 8 8) word)
                           (aref result (1+ index)) (ldb (byte 8 0) word)))
            result))))))

(defun %proxy-resolved-address (host resolve-host)
  (or (%proxy-ipv4-octets host)
      (%proxy-ipv6-octets host)
      (when resolve-host
        (let ((resolved (funcall resolve-host host)))
          (or (and (stringp resolved)
                   (or (%proxy-ipv4-octets resolved)
                       (%proxy-ipv6-octets resolved)))
              (%proxy-error
               "The local proxy resolver must return a numeric IPv4 or IPv6 address."
               resolved))))))

(defun %proxy-socks-address (host remote-dns-p resolve-host)
  (if remote-dns-p
      (let* ((octets (cl-codec-kit:string-to-octets host :encoding :utf-8))
             (octet-count (array-total-size octets)))
        (when (or (zerop octet-count) (> octet-count 255))
          (%proxy-error "A SOCKS5 domain name must fit in one octet length." host))
        (let ((builder (%proxy-byte-builder)))
          (%proxy-builder-byte builder 3)
          (%proxy-builder-byte builder octet-count)
          (%proxy-builder-octets builder octets)
          builder))
      (let ((address (%proxy-resolved-address host resolve-host)))
        (unless address
          (%proxy-error
           "A numeric address or a local resolver is required for SOCKS5."
           host))
        (cond
          ((= (length address) 4)
           (let ((builder (%proxy-byte-builder)))
             (%proxy-builder-byte builder 1)
             (%proxy-builder-octets builder address)
             builder))
          ((= (length address) 16)
           (let ((builder (%proxy-byte-builder)))
             (%proxy-builder-byte builder 4)
             (%proxy-builder-octets builder address)
             builder))
          (t (%proxy-error "The SOCKS5 address has an unsupported length."
                           (length address)))))))
