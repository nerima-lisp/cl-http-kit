(in-package #:http-kit/client)

(defstruct (http-cache-entry
             (:constructor %make-http-cache-entry)
             (:conc-name http-cache-entry-))
  key
  method
  uri
  response
  stored-at
  age-at-store
  expires-at
  etag
  last-modified
  vary
  request-vary-values
  accessed-at)

(defun http-cache-max-entries (cache)
  (unless (http-cache-p cache)
    (%client-protocol-error "Expected an HTTP-CACHE value." cache))
  (%http-cache-max-entries cache))

(defun http-cache-clock-function (cache)
  (unless (http-cache-p cache)
    (%client-protocol-error "Expected an HTTP-CACHE value." cache))
  (%http-cache-clock-function cache))

(defun http-cache-status-identifier (cache)
  (unless (http-cache-p cache)
    (%client-protocol-error "Expected an HTTP-CACHE value." cache))
  (%http-cache-status-identifier cache))

(defun %cache-structured-string-p (value)
  (and (stringp value)
       (plusp (length value))
       (every (lambda (character)
                (let ((code (char-code character)))
                  (and (>= code #x20) (<= code #x7e))))
              value)))

(defun %cache-quote-structured-string (value)
  (with-output-to-string (output)
    (write-char #\" output)
    (loop for character across value
          do (when (member character '(#\" #\\) :test #'char=)
               (write-char #\\ output))
             (write-char character output))
    (write-char #\" output)))

(defun %cache-status-response (cache response parameters)
  (let ((identifier (%http-cache-status-identifier cache)))
    (if identifier
        (%copy-client-response
         response
         :headers
         (append (http-response-headers response)
                 (list (make-http-header
                        "Cache-Status"
                        (concatenate
                         'string
                         (%cache-quote-structured-string identifier)
                         parameters)))))
        response)))

(defun %cache-status-hit-response (cache response entry &optional now)
  (%cache-status-response
   cache response
   (format nil "; hit; ttl=~D"
           (floor (- (http-cache-entry-expires-at entry)
                     (or now
                         (funcall (%http-cache-clock-function cache))))))))

(defun %cache-status-forward-response
    (cache response reason &key forwarded-status stored-p)
  (%cache-status-response
   cache response
   (format nil "; fwd=~(~A~)~@[; fwd-status=~D~]~@[; stored~]"
           reason forwarded-status stored-p)))

(defun %copy-client-response
    (response &key (body nil body-supplied-p)
                   (headers nil headers-supplied-p))
  (make-http-response
   :protocol-version (http-response-protocol-version response)
   :status (http-response-status response)
   :reason (http-response-reason response)
   :headers (if headers-supplied-p
                headers
                (http-response-headers response))
   :trailers (http-response-trailers response)
   :body (if body-supplied-p body (http-response-body response))))

(defun make-http-cache
    (&key (max-entries 256) (clock-function #'get-universal-time)
          status-identifier)
  (%ensure-positive-integer
   max-entries
   "The cache entry limit must be a positive integer.")
  (%ensure-function clock-function "A cache clock must be a function.")
  (when (and status-identifier
             (not (%cache-structured-string-p status-identifier)))
    (%client-protocol-error
     "A cache status identifier must be a non-empty visible ASCII string."
     status-identifier))
  (%make-http-cache :entries nil
                    :max-entries max-entries
                    :clock-function clock-function
                    :status-identifier status-identifier))

(defun %cache-request-uri (request-or-uri)
  (cond ((http-request-p request-or-uri)
         (http-request-uri request-or-uri))
        (t (%client-uri request-or-uri))))

(defun http-cache-key (request-or-uri)
  "Return the normalized URI key used by the private response cache."
  (http-uri-string (%cache-request-uri request-or-uri)))

(defun %cache-request-method (request-or-uri)
  (if (http-request-p request-or-uri)
      (http-request-method request-or-uri)
      "GET"))

(defun %cache-trim (value)
  (string-trim '(#\Space #\Tab) value))

(defun %cache-split-comma (value)
  (let ((parts nil)
        (start 0)
        (quoted-p nil)
        (escaped-p nil))
    (loop for index from 0 below (length value)
          for character = (char value index)
          do (cond
               (escaped-p
                (setf escaped-p nil))
               ((and quoted-p (char= character #\\))
                (setf escaped-p t))
               ((char= character #\")
                (setf quoted-p (not quoted-p)))
               ((and (not quoted-p) (char= character #\,))
                (push (%cache-trim (subseq value start index)) parts)
                (setf start (1+ index))))
          finally
             (push (%cache-trim (subseq value start)) parts)
             (return (nreverse parts)))))

(defun %cache-connection-fields (headers)
  (let ((fields nil))
    (dolist (value (http-header-values headers "Connection") fields)
      (dolist (field (%cache-split-comma value))
        (unless (zerop (length field))
          (push field fields))))))

(defun %cache-storage-excluded-fields (headers)
  (append '("Connection" "Keep-Alive" "Proxy-Connection"
            "Proxy-Authenticate" "Proxy-Authentication-Info"
            "Proxy-Authorization" "TE" "Trailer" "Transfer-Encoding"
            "Upgrade")
          (%cache-connection-fields headers)))

(defun %cache-storable-headers (headers directives)
  (let ((excluded
          (append (%cache-storage-excluded-fields headers)
                  (%cache-no-cache-fields directives))))
    (remove-if
     (lambda (header)
       (member (http-header-name header) excluded :test #'string-equal))
     headers)))

(defun %cache-split-assignment (value)
  (let ((position (position #\= value)))
    (if position
        (values (%cache-trim (subseq value 0 position))
                (%cache-trim (subseq value (1+ position))))
        (values (%cache-trim value) nil))))

(defun %cache-unquote (value)
  (if (and (>= (length value) 2)
           (char= (char value 0) #\")
           (char= (char value (1- (length value))) #\"))
      (with-output-to-string (output)
        (loop with end = (1- (length value))
              with index = 1
              while (< index end)
              for character = (char value index)
              do (if (and (char= character #\\)
                          (< (1+ index) end))
                     (progn
                       (incf index)
                       (write-char (char value index) output))
                     (write-char character output))
                 (incf index)))
      value))

(defun %cache-control-directives (headers)
  (let ((directives nil))
    (dolist (header (http-header-values headers "Cache-Control") directives)
      (dolist (directive (%cache-split-comma header))
        (multiple-value-bind (name value)
            (%cache-split-assignment directive)
          (push (cons (string-downcase name)
                      (and value (%cache-unquote value)))
                directives))))))

(defun %cache-request-directives (headers)
  (let ((directives (%cache-control-directives headers)))
    (if (or directives
            (notany (lambda (value)
                      (find "no-cache" (%cache-split-comma value)
                            :test #'string-equal))
                    (http-header-values headers "Pragma")))
        directives
        (list (cons "no-cache" nil)))))

(defun %cache-nonnegative-seconds (value)
  (let ((number (%client-parse-integer value :allow-sign-p nil)))
    (and number (>= number 0) number)))

(defun %cache-request-no-store-p (request)
  (%cache-directive-present-p
   (%cache-request-directives (http-request-headers request))
   "no-store"))

(defun %cache-request-no-cache-p (request)
  (let ((directives (%cache-request-directives (http-request-headers request))))
    (or (%cache-directive-present-p directives "no-cache")
        (let ((max-age (%cache-nonnegative-seconds
                        (%cache-directive directives "max-age"))))
          (and max-age (zerop max-age))))))

(defun %cache-request-min-fresh (request)
  (%cache-nonnegative-seconds
   (%cache-directive
    (%cache-request-directives (http-request-headers request))
    "min-fresh")))

(defun %cache-request-max-stale (request)
  (let* ((directives (%cache-request-directives (http-request-headers request)))
         (name "max-stale"))
    (when (%cache-directive-present-p directives name)
      (let ((value (%cache-directive directives name)))
        (if value (%cache-nonnegative-seconds value) :unbounded)))))

(defun %cache-response-must-revalidate-p (headers)
  (let ((directives (%cache-control-directives headers)))
    (or (%cache-directive-present-p directives "must-revalidate")
        (%cache-directive-present-p directives "proxy-revalidate"))))

(defun %cache-directive (directives name)
  (cdr (assoc name directives :test #'string=)))

(defun %cache-directive-values (directives name)
  (loop for (directive-name . value) in directives
        when (string= directive-name name)
          collect value))

(defun %cache-directive-present-p (directives name)
  (not (null (assoc name directives :test #'string=))))

(defun %cache-restrictive-delta-seconds (directives name reducer)
  (let ((values (%cache-directive-values directives name)))
    (when values
      (let ((seconds
              (mapcar (lambda (value)
                        (and value (%client-parse-integer value)))
                      values)))
        (when (every (lambda (value)
                       (and value (>= value 0)))
                     seconds)
          (reduce reducer seconds))))))

(defun %cache-max-stale-limit (directives)
  (let ((values (%cache-directive-values directives "max-stale"))
        (limits nil))
    (dolist (value values)
      (when value
        (let ((seconds (%client-parse-integer value)))
          (unless (and seconds (>= seconds 0))
            (return-from %cache-max-stale-limit :invalid))
          (push seconds limits))))
    (when limits
      (reduce #'min limits))))

(defun %cache-no-cache-fields (directives)
  (let ((fields nil))
    (dolist (value (%cache-directive-values directives "no-cache") fields)
      (when value
        (dolist (field (%cache-split-comma value))
          (unless (zerop (length field))
            (push field fields)))))))

(defun %cache-heuristically-cacheable-status-p (status)
  (member status '(200 203 204 300 301 308 404 405 410 414 501) :test #'=))

(defun %cache-explicitly-cacheable-p (headers directives)
  (or (http-header-present-p headers "Expires")
      (%cache-directive-present-p directives "max-age")
      (%cache-directive-present-p directives "public")
      (%cache-directive-present-p directives "private")))

(defun %cache-understands-status-p (status)
  (not (null (assoc status *http-status-reasons*))))

(defun %cache-response-no-store-p (status directives)
  (and (%cache-directive-present-p directives "no-store")
       (not (and (%cache-directive-present-p directives "must-understand")
                 (%cache-understands-status-p status)))))

(defun %cache-valid-status-p (status headers directives)
  (and (<= 200 status 599)
       (not (member status '(206 304) :test #'=))
       (or (%cache-heuristically-cacheable-status-p status)
           (%cache-explicitly-cacheable-p headers directives))
       (or (not (%cache-directive-present-p directives "must-understand"))
           (%cache-understands-status-p status))))

(defun %cache-vary-fields (headers)
  (let ((fields nil))
    (dolist (value (http-header-values headers "Vary") fields)
      (dolist (field (%cache-split-comma value))
        (when (zerop (length field))
          (return-from %cache-vary-fields nil))
        (if (string= field "*")
            (return-from %cache-vary-fields :star)
            (push (string-downcase field) fields))))
    (remove-duplicates (nreverse fields) :test #'string=)))

(defun %cache-request-vary-values (request fields)
  (when fields
    (mapcar (lambda (field)
              (cons field
                    (if (http-request-p request)
                        (http-header-values (http-request-headers request)
                                            field)
                        nil)))
            fields)))

(defun %cache-effective-vary-fields (fields)
  (let ((result (copy-list fields)))
    (dolist (field '("authorization" "cookie") result)
      (when (not (member field result :test #'string=))
        (setf result (append result (list field)))))))

(defun %cache-vary-match-p (entry request)
  (every (lambda (field-and-values)
           (equal (cdr field-and-values)
                  (http-header-values (http-request-headers request)
                                      (car field-and-values))))
         (http-cache-entry-request-vary-values entry)))

(defun %cache-freshness
    (response request-time response-time)
  (let* ((headers (http-response-headers response))
         (directives (%cache-control-directives headers))
         (max-age-values (%cache-directive-values directives "max-age"))
         (max-age
           (and (= (length max-age-values) 1)
                (%client-parse-integer (first max-age-values))))
         (valid-max-age-p (and max-age (>= max-age 0)))
         (expires (let ((value (http-header-value headers "Expires")))
                    (and value (http-parse-date value))))
         (date (let ((value (http-header-value headers "Date")))
                 (and value (http-parse-date value))))
         (last-modified (let ((value (http-header-value headers "Last-Modified")))
                          (and value (http-parse-date value))))
         (age (let ((value (http-header-value headers "Age")))
                (and value
                     (%client-parse-integer value))))
         (date (or date response-time))
         (age (if (and age (>= age 0)) age 0))
         (apparent-age (max 0 (- response-time date)))
         (response-delay (max 0 (- response-time request-time)))
         (corrected-age (max apparent-age (+ age response-delay)))
         (freshness-lifetime
           (cond (max-age-values (if valid-max-age-p max-age 0))
                 (expires (max 0 (- expires date)))
                 (last-modified
                  (min 86400
                       (max 0 (floor (- date last-modified) 10))))
                 (t 0))))
    (cond
      ((%cache-response-no-store-p (http-response-status response) directives)
       (values nil nil nil))
      (t
       (values t
               (if (%cache-directive-present-p directives "no-cache")
                   response-time
                   (+ response-time
                      (max 0 (- freshness-lifetime corrected-age))))
               corrected-age)))))

(defun %cache-copy-entry (entry)
  (%make-http-cache-entry
   :key (copy-seq (http-cache-entry-key entry))
   :method (copy-seq (http-cache-entry-method entry))
   :uri (%client-uri (http-cache-entry-uri entry))
   :response (%copy-client-response (http-cache-entry-response entry))
   :stored-at (http-cache-entry-stored-at entry)
   :age-at-store (http-cache-entry-age-at-store entry)
   :expires-at (http-cache-entry-expires-at entry)
   :etag (and (http-cache-entry-etag entry)
              (copy-seq (http-cache-entry-etag entry)))
   :last-modified (and (http-cache-entry-last-modified entry)
                       (copy-seq (http-cache-entry-last-modified entry)))
   :vary (copy-list (http-cache-entry-vary entry))
   :request-vary-values
   (mapcar (lambda (field-and-values)
             (cons (copy-seq (car field-and-values))
                   (copy-list (cdr field-and-values))))
           (http-cache-entry-request-vary-values entry))
   :accessed-at (http-cache-entry-accessed-at entry)))

(defun http-cache-entries (cache)
  "Return detached cache-entry snapshots in most-recently-used order."
  (unless (http-cache-p cache)
    (%client-protocol-error "Expected an HTTP-CACHE value." cache))
  (mapcar #'%cache-copy-entry (%http-cache-entries cache)))

(defun %cache-response-headers-with-age (response current-age)
  (append
   (remove-if (lambda (header)
                (string-equal (http-header-name header) "Age"))
              (http-response-headers response))
   (list (make-http-header "Age" (write-to-string (floor current-age))))))

(defun %cache-entry-response-for-request (entry request current-age)
  (let* ((response (http-cache-entry-response entry))
         (headers (%cache-response-headers-with-age response current-age)))
    (if (string= (http-request-method request) "HEAD")
        (%copy-client-response
         response
         :headers headers
         :body (make-array 0 :element-type '(unsigned-byte 8)))
        (%copy-client-response response :headers headers))))

(defun %cache-non-negative-delta-seconds (directives name)
  (%cache-restrictive-delta-seconds directives name #'min))

(defun %cache-stale-if-error-response (entry request now)
  (let* ((request-directives
           (%cache-request-directives (http-request-headers request)))
         (response-directives
           (%cache-control-directives
            (http-response-headers (http-cache-entry-response entry))))
         (request-window
           (%cache-non-negative-delta-seconds request-directives
                                              "stale-if-error"))
         (response-window
           (%cache-non-negative-delta-seconds response-directives
                                              "stale-if-error"))
         (window (cond
                   ((and request-window response-window)
                    (min request-window response-window))
                   (request-window request-window)
                   (response-window response-window)))
         (staleness (max 0 (- now (http-cache-entry-expires-at entry))))
         (current-age
           (+ (http-cache-entry-age-at-store entry)
              (max 0 (- now (http-cache-entry-stored-at entry))))))
    (when (and window (<= staleness window))
      (%cache-entry-response-for-request entry request current-age))))

(defun %cache-stale-while-revalidate-response (entry request now)
  (let* ((request-directives
           (%cache-request-directives (http-request-headers request)))
         (response-directives
           (%cache-control-directives
            (http-response-headers (http-cache-entry-response entry))))
         (window
           (%cache-non-negative-delta-seconds response-directives
                                              "stale-while-revalidate"))
         (staleness (- now (http-cache-entry-expires-at entry)))
         (current-age
           (+ (http-cache-entry-age-at-store entry)
              (max 0 (- now (http-cache-entry-stored-at entry))))))
    (when (and window
               (>= staleness 0)
               (<= staleness window)
               (not (%cache-directive-present-p request-directives "no-cache"))
               (not (%cache-directive-present-p request-directives "max-age"))
               (not (%cache-directive-present-p request-directives "min-fresh")))
      (%cache-entry-response-for-request entry request current-age))))

(defun %cache-find-entry (cache request)
  (let ((key (http-cache-key request))
        (method (http-request-method request)))
    (or (find-if (lambda (entry)
                   (and (string= key (http-cache-entry-key entry))
                        (string= method (http-cache-entry-method entry))
                        (%cache-vary-match-p entry request)))
                 (%http-cache-entries cache))
        (and (string= method "HEAD")
             (find-if (lambda (entry)
                        (and (string= key (http-cache-entry-key entry))
                             (string= "GET" (http-cache-entry-method entry))
                             (%cache-vary-match-p entry request)))
                      (%http-cache-entries cache))))))

(defun %cache-conditional-request-p (headers)
  (some (lambda (name) (http-header-present-p headers name))
        '("If-Match" "If-None-Match" "If-Modified-Since"
          "If-Unmodified-Since" "If-Range")))

(defun http-cache-lookup (cache request &key now)
  "Look up REQUEST and return (VALUES RESPONSE STATE ENTRY FORWARD-REASON).

STATE is :FRESH, :STALE, or :MISS.  RESPONSE is non-NIL when the stored
response is directly reusable; a :STALE response can be reusable when REQUEST
explicitly permits staleness.  ENTRY is an opaque snapshot useful to the
client implementation for conditional revalidation.  FORWARD-REASON is an
RFC 9211 reason token when the request must be forwarded."
  (unless (http-cache-p cache)
    (%client-protocol-error "Expected an HTTP-CACHE value." cache))
  (unless (http-request-p request)
    (%client-protocol-error "Cache lookup requires an HTTP-REQUEST." request))
  (unless (member (http-request-method request) '("GET" "HEAD")
                  :test #'string=)
    (return-from http-cache-lookup (values nil :miss nil :method)))
  (when (http-header-present-p (http-request-headers request) "Range")
    (return-from http-cache-lookup (values nil :miss nil :partial)))
  (when (%cache-conditional-request-p (http-request-headers request))
    (return-from http-cache-lookup (values nil :miss nil :request)))
  (let* ((request-directives
           (%cache-request-directives (http-request-headers request)))
         (now (or now (funcall (%http-cache-clock-function cache))))
         (entry (%cache-find-entry cache request)))
    (when (%cache-directive-present-p request-directives "no-store")
      (return-from http-cache-lookup (values nil :miss nil :bypass)))
    (unless entry
      (return-from http-cache-lookup (values nil :miss nil :miss)))
    (setf (%http-cache-entries cache)
          (cons entry
                (delete entry (%http-cache-entries cache))))
    (setf (http-cache-entry-accessed-at entry) now)
    (let* ((response-directives
             (%cache-control-directives
              (http-response-headers (http-cache-entry-response entry))))
           (current-age
             (+ (http-cache-entry-age-at-store entry)
                (max 0 (- now (http-cache-entry-stored-at entry)))))
           (remaining (- (http-cache-entry-expires-at entry) now))
           (request-max-age-present-p
             (%cache-directive-present-p request-directives "max-age"))
           (request-max-age
             (%cache-restrictive-delta-seconds request-directives
                                                "max-age" #'min))
           (min-fresh-present-p
             (%cache-directive-present-p request-directives "min-fresh"))
           (min-fresh
             (%cache-restrictive-delta-seconds request-directives
                                                "min-fresh" #'max))
           (max-stale-present-p
             (%cache-directive-present-p request-directives "max-stale"))
           (max-stale (%cache-max-stale-limit request-directives))
           (immutable-reload-p
             (and (> remaining 0)
                  (string= (http-uri-scheme (http-request-uri request))
                           "https")
                  (%cache-directive-present-p response-directives "immutable")
                  request-max-age
                  (zerop request-max-age)))
           (fresh-p
             (and (not (%cache-directive-present-p request-directives
                                                    "no-cache"))
                  (or immutable-reload-p
                      (not request-max-age-present-p)
                      (and request-max-age
                           (<= current-age request-max-age)))
                  (or (not min-fresh-present-p)
                      (and min-fresh
                           (>= remaining min-fresh)))
                  (> remaining 0)))
           (stale-allowed-p
             (and (not fresh-p)
                  (<= remaining 0)
                  (not (%cache-directive-present-p request-directives
                                                   "no-cache"))
                  (not min-fresh-present-p)
                  (or (not request-max-age-present-p)
                      (and request-max-age
                           (<= current-age request-max-age)))
                  max-stale-present-p
                  (not (%cache-directive-present-p response-directives
                                                   "must-revalidate"))
                  (not (%cache-directive-present-p response-directives
                                                   "no-cache"))
                  (not (eq max-stale :invalid))
                  (or (null max-stale)
                      (and max-stale
                           (<= (max 0 (- remaining)) max-stale))))))
      (cond
        (fresh-p
         (values (%cache-entry-response-for-request entry request current-age)
                 :fresh
                 (%cache-copy-entry entry)))
        (stale-allowed-p
         (values (%cache-entry-response-for-request entry request current-age)
                 :stale-allowed
                 (%cache-copy-entry entry)))
        (t
         (values nil :stale (%cache-copy-entry entry)
                 (if (> remaining 0) :request :stale)))))))

(defun %cache-remove-key (cache key &optional method)
  (setf (%http-cache-entries cache)
        (delete-if (lambda (entry)
                     (and (string= key (http-cache-entry-key entry))
                          (or (null method)
                              (string= method
                                       (http-cache-entry-method entry)))))
                   (%http-cache-entries cache))))

(defun %cache-remove-variant (cache entry)
  (setf (%http-cache-entries cache)
        (delete-if
         (lambda (candidate)
           (and (string= (http-cache-entry-key entry)
                         (http-cache-entry-key candidate))
                (string= (http-cache-entry-method entry)
                         (http-cache-entry-method candidate))
                (equal (http-cache-entry-vary entry)
                       (http-cache-entry-vary candidate))
                (equal (http-cache-entry-request-vary-values entry)
                       (http-cache-entry-request-vary-values candidate))))
         (%http-cache-entries cache))))

(defun http-cache-store
    (cache request response &key now request-time response-time)
  "Store a cacheable GET/HEAD RESPONSE and return its detached entry."
  (unless (http-cache-p cache)
    (%client-protocol-error "Expected an HTTP-CACHE value." cache))
  (unless (and (http-request-p request) (http-response-p response))
    (%client-protocol-error
     "Cache storage requires an HTTP-REQUEST and HTTP-RESPONSE."
     (list request response)))
  (let* ((method (http-request-method request))
         (now (or now (funcall (%http-cache-clock-function cache))))
         (response-time (or response-time now))
         (request-time (or request-time response-time))
         (request-directives
           (%cache-request-directives (http-request-headers request)))
         (headers (http-response-headers response))
         (response-directives (%cache-control-directives headers))
         (vary (%cache-vary-fields headers)))
    (unless (and (member method '("GET" "HEAD") :test #'string=)
                 (%cache-valid-status-p (http-response-status response)
                                        headers
                                        response-directives)
                 (not (http-header-present-p (http-request-headers request)
                                             "Range"))
                 (not (eq vary :star))
                 (not (%cache-directive-present-p request-directives
                                                  "no-store"))
                 (not (%cache-response-no-store-p
                       (http-response-status response)
                       response-directives)))
      (return-from http-cache-store nil))
    (multiple-value-bind (cacheable-p expires-at age-at-store)
        (%cache-freshness response request-time response-time)
      (unless cacheable-p
        (return-from http-cache-store nil))
      (let* ((effective-vary (%cache-effective-vary-fields (or vary '())))
             (key (http-cache-key request))
             (entry (%make-http-cache-entry
                     :key key
                     :method method
                     :uri (http-request-uri request)
                     :response
                     (%copy-client-response
                      response
                      :headers
                      (%cache-storable-headers headers response-directives))
                     :stored-at response-time
                     :age-at-store age-at-store
                     :expires-at expires-at
                     :etag (http-header-value headers "ETag")
                     :last-modified (http-header-value headers "Last-Modified")
                     :vary effective-vary
                     :request-vary-values
                     (%cache-request-vary-values request effective-vary)
                     :accessed-at now)))
        (%cache-remove-variant cache entry)
        (push entry (%http-cache-entries cache))
        (when (> (length (%http-cache-entries cache))
                 (%http-cache-max-entries cache))
          (setf (%http-cache-entries cache)
                (butlast (%http-cache-entries cache))))
        (%cache-copy-entry entry)))))

(defun http-cache-invalidate (cache request-or-uri)
  (unless (http-cache-p cache)
    (%client-protocol-error "Expected an HTTP-CACHE value." cache))
  (%cache-remove-key cache (http-cache-key request-or-uri))
  cache)

(defun http-cache-clear (cache)
  (unless (http-cache-p cache)
    (%client-protocol-error "Expected an HTTP-CACHE value." cache))
  (setf (%http-cache-entries cache) nil)
  cache)
