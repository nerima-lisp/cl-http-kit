(in-package #:http-kit/client)

(defun %date-tokens (string)
  (let ((tokens nil)
        (start nil))
    (labels ((finish (end)
               (when start
                 (push (subseq string start end) tokens)
                 (setf start nil))))
      (loop for index from 0 below (length string)
            for character = (char string index)
            do (if (member character '(#\Space #\Tab) :test #'char=)
                   (finish index)
                   (unless start (setf start index))))
      (finish (length string)))
    (nreverse tokens)))

(defun %date-integer (string)
  (and (not (string= string ""))
       (every (lambda (character)
                (and (char>= character #\0) (char<= character #\9)))
              string)
       (%client-parse-integer string :allow-sign-p nil)))

(defun %date-digits-p (string width)
  (and (= (length string) width)
       (%date-integer string)))

(defun %date-month (string)
  (position string
            '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
              "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
            :test #'string=))

(defun %date-time-parts (string)
  (when (and (= (length string) 8)
             (char= (char string 2) #\:)
             (char= (char string 5) #\:))
      (let ((hour (%date-digits-p (subseq string 0 2) 2))
            (minute (%date-digits-p (subseq string 3 5) 2))
            (second-value (%date-digits-p (subseq string 6 8) 2)))
        (when (and hour minute second-value
                   (<= 0 hour 23)
                   (<= 0 minute 59)
                   (<= 0 second-value 60))
          (values hour minute (min second-value 59))))))

(defun %date-rfc850-year (string now)
  (when (%date-digits-p string 2)
    (let* ((year (%date-integer string))
           (current-year (nth-value 5 (decode-universal-time now 0)))
           (candidate (+ (* (floor current-year 100) 100) year)))
      (if (> candidate (+ current-year 50))
          (- candidate 100)
          candidate))))

(defun %date-leap-year-p (year)
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100)))
           (zerop (mod year 400)))))

(defun %date-days-in-month (month year)
  (case month
    ((0 2 4 6 7 9 11) 31)
    ((3 5 8 10) 30)
    (1 (if (%date-leap-year-p year) 29 28))))

(defun %date-encode (day month year time)
  (when (and day month year time
             (<= 1 day (%date-days-in-month month year))
             (<= 1900 year 9999))
    (multiple-value-bind (hour minute second) (%date-time-parts time)
      (when (and hour minute second)
        (encode-universal-time second minute hour day (1+ month) year 0)))))

(defun %parse-http-date* (string now)
  (let ((tokens (%date-tokens string)))
    (cond
      ;; IMF-fixdate: Sun, 06 Nov 1994 08:49:37 GMT
      ((and (= (length tokens) 6)
            (member (first tokens)
                    '("Mon," "Tue," "Wed," "Thu," "Fri," "Sat," "Sun,")
                    :test #'string=)
            (%date-digits-p (second tokens) 2)
            (%date-digits-p (fourth tokens) 4)
            (string= (sixth tokens) "GMT")
            (string= string (format nil "~{~A~^ ~}" tokens)))
       (%date-encode (%date-integer (second tokens))
                     (%date-month (third tokens))
                     (%date-integer (fourth tokens))
                     (fifth tokens)))
      ;; RFC 850: Sunday, 06-Nov-94 08:49:37 GMT
      ((and (= (length tokens) 4)
            (member (first tokens)
                    '("Monday," "Tuesday," "Wednesday," "Thursday,"
                      "Friday," "Saturday," "Sunday,")
                    :test #'string=)
            (string= (fourth tokens) "GMT")
            (string= string (format nil "~{~A~^ ~}" tokens)))
       (let* ((date (second tokens))
              (first-hyphen (position #\- date))
              (second-hyphen (and first-hyphen
                                  (position #\- date :start (1+ first-hyphen)))))
         (when (and (eql first-hyphen 2)
                    (eql second-hyphen 6)
                    (= (length date) 9))
           (%date-encode
            (%date-integer (subseq date 0 first-hyphen))
            (%date-month (subseq date (1+ first-hyphen) second-hyphen))
            (%date-rfc850-year (subseq date (1+ second-hyphen)) now)
            (third tokens)))))
      ;; ANSI C asctime: Sun Nov  6 08:49:37 1994
      ((and (= (length tokens) 5)
            (member (first tokens)
                    '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
                    :test #'string=)
            (member (length (third tokens)) '(1 2))
            (%date-integer (third tokens))
            (%date-digits-p (fifth tokens) 4)
            (string= string
                     (format nil "~A ~A ~2D ~A ~A"
                             (first tokens) (second tokens)
                             (%date-integer (third tokens))
                             (fourth tokens) (fifth tokens))))
       (%date-encode (%date-integer (third tokens))
                     (%date-month (second tokens))
                     (%date-integer (fifth tokens))
                     (fourth tokens)))
      (t nil))))

(defun http-parse-date (value &key (now (get-universal-time)))
  "Parse an HTTP-date and return universal time, or NIL when it is invalid."
  (when (stringp value)
    (%parse-http-date* value now)))

(defun %retry-after-seconds (value now)
  (let ((seconds (%date-integer value)))
    (cond (seconds (max 0 seconds))
          (t (let ((date (http-parse-date value :now now)))
               (and date (max 0 (- date now))))))))
