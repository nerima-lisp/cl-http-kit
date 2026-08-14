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

(defun %date-month (string)
  (position (string-capitalize string)
            '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
              "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
            :test #'string=))

(defun %date-time-parts (string)
  (let* ((first (position #\: string))
         (second (and first (position #\: string :start (1+ first)))))
    (when (and first second
               (= (count #\: string) 2))
      (let ((hour (%date-integer (subseq string 0 first)))
            (minute (%date-integer (subseq string (1+ first) second)))
            (second-value (%date-integer (subseq string (1+ second)))))
        (when (and hour minute second-value
                   (<= 0 hour 23)
                   (<= 0 minute 59)
                   (<= 0 second-value 60))
          (values hour minute (min second-value 59)))))))

(defun %date-year (string)
  (let ((year (%date-integer string)))
    (cond ((null year) nil)
          ((< year 100) (if (>= year 70) (+ 1900 year) (+ 2000 year)))
          (t year))))

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

(defun %parse-http-date* (string)
  (let ((tokens (%date-tokens string)))
    (cond
      ;; IMF-fixdate: Sun, 06 Nov 1994 08:49:37 GMT
      ((and (= (length tokens) 6)
            (search "," (first tokens) :from-end t))
       (%date-encode (%date-integer (second tokens))
                     (%date-month (third tokens))
                     (%date-year (fourth tokens))
                     (fifth tokens)))
      ;; RFC 850: Sunday, 06-Nov-94 08:49:37 GMT
      ((and (= (length tokens) 4)
            (search "," (first tokens) :from-end t))
       (let* ((date (second tokens))
              (first-hyphen (position #\- date))
              (second-hyphen (and first-hyphen
                                  (position #\- date :start (1+ first-hyphen)))))
         (when (and first-hyphen second-hyphen)
           (%date-encode
            (%date-integer (subseq date 0 first-hyphen))
            (%date-month (subseq date (1+ first-hyphen) second-hyphen))
            (%date-year (subseq date (1+ second-hyphen)))
            (third tokens)))))
      ;; ANSI C asctime: Sun Nov  6 08:49:37 1994
      ((= (length tokens) 5)
       (%date-encode (%date-integer (third tokens))
                     (%date-month (second tokens))
                     (%date-year (fifth tokens))
                     (fourth tokens)))
      (t nil))))

(defun http-parse-date (value)
  "Parse an HTTP-date and return universal time, or NIL when it is invalid."
  (when (stringp value)
    (%parse-http-date* value)))

(defun %retry-after-seconds (value now)
  (let ((seconds (%date-integer value)))
    (cond (seconds (max 0 seconds))
          (t (let ((date (http-parse-date value)))
               (and date (max 0 (- date now))))))))
