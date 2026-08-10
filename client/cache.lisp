(in-package #:http-kit/client)

(defstruct (http-cache-entry
             (:constructor %make-http-cache-entry)
             (:conc-name http-cache-entry-))
  key
  method
  uri
  response
  stored-at
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

(defun %copy-client-response (response &key body)
  (make-http-response
   :protocol-version (http-response-protocol-version response)
   :status (http-response-status response)
   :reason (http-response-reason response)
   :headers (http-response-headers response)
   :trailers (http-response-trailers response)
   :body (or body (http-response-body response))))

(defun make-http-cache
    (&key (max-entries 256) (clock-function #'get-universal-time))
  (%ensure-positive-integer
   max-entries
   "The cache entry limit must be a positive integer.")
  (%ensure-function clock-function "A cache clock must be a function.")
  (%make-http-cache :entries nil
                    :max-entries max-entries
                    :clock-function clock-function))

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
        (start 0))
    (loop for separator = (position #\, value :start start)
          do (push (%cache-trim
                    (subseq value start (or separator (length value))))
                   parts)
          if separator
            do (setf start (1+ separator))
          else
            do (return (nreverse parts)))))

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
      (subseq value 1 (1- (length value)))
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

(defun %cache-directive (directives name)
  (cdr (assoc name directives :test #'string=)))

(defun %cache-valid-status-p (status)
  (member status '(200 203 204 206 300 301 404 410) :test #'=))

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

(defun %cache-vary-match-p (entry request)
  (every (lambda (field-and-values)
           (equal (cdr field-and-values)
                  (http-header-values (http-request-headers request)
                                      (car field-and-values))))
         (http-cache-entry-request-vary-values entry)))

(defun %cache-freshness
    (response now)
  (let* ((headers (http-response-headers response))
         (directives (%cache-control-directives headers))
         (max-age (%cache-directive directives "max-age"))
         (max-age (and max-age
                       (%client-parse-integer max-age)))
         (expires (and (%cache-directive directives "no-store") nil))
         (expires (or expires
                      (let ((value (http-header-value headers "Expires")))
                        (and value (http-parse-date value)))))
         (date (let ((value (http-header-value headers "Date")))
                 (and value (http-parse-date value))))
         (last-modified (let ((value (http-header-value headers "Last-Modified")))
                          (and value (http-parse-date value))))
         (age (let ((value (http-header-value headers "Age")))
                (and value
                     (%client-parse-integer value)))))
    (cond
      ((%cache-directive directives "no-store")
       (values nil nil))
      ((or (and max-age (< max-age 0))
           (and (%cache-directive directives "s-maxage")
                (null max-age)))
       (values nil nil))
      (max-age
       (values t (+ now (max 0 (- max-age (or age 0))))))
      (expires
       (values t (max now (+ expires (- now (or date now))))))
      (last-modified
       (values t (+ now (min 86400
                             (max 0 (floor (- (or date now) last-modified)
                                           10))))))
      (t
       (values t now)))))

(defun %cache-copy-entry (entry)
  (%make-http-cache-entry
   :key (copy-seq (http-cache-entry-key entry))
   :method (copy-seq (http-cache-entry-method entry))
   :uri (%client-uri (http-cache-entry-uri entry))
   :response (%copy-client-response (http-cache-entry-response entry))
   :stored-at (http-cache-entry-stored-at entry)
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

(defun %cache-entry-response-for-request (entry request)
  (if (string= (http-request-method request) "HEAD")
      (%copy-client-response
       (http-cache-entry-response entry)
       :body (make-array 0 :element-type '(unsigned-byte 8)))
      (%copy-client-response (http-cache-entry-response entry))))

(defun %cache-find-entry (cache request)
  (let ((key (http-cache-key request))
        (method (http-request-method request)))
    (or (find-if (lambda (entry)
                   (and (string= key (http-cache-entry-key entry))
                        (string= method (http-cache-entry-method entry))))
                 (%http-cache-entries cache))
        (and (string= method "HEAD")
             (find-if (lambda (entry)
                        (and (string= key (http-cache-entry-key entry))
                             (string= "GET" (http-cache-entry-method entry))))
                      (%http-cache-entries cache))))))

(defun http-cache-lookup (cache request &key now)
  "Look up REQUEST and return (VALUES RESPONSE STATE ENTRY).

STATE is :FRESH, :STALE, or :MISS.  ENTRY is an opaque snapshot useful to
the client implementation for conditional revalidation."
  (unless (http-cache-p cache)
    (%client-protocol-error "Expected an HTTP-CACHE value." cache))
  (unless (http-request-p request)
    (%client-protocol-error "Cache lookup requires an HTTP-REQUEST." request))
  (unless (member (http-request-method request) '("GET" "HEAD") :test #'string=)
    (return-from http-cache-lookup (values nil :miss nil)))
  (let* ((now (or now (funcall (%http-cache-clock-function cache))))
         (entry (%cache-find-entry cache request)))
    (unless (and entry (%cache-vary-match-p entry request))
      (return-from http-cache-lookup (values nil :miss nil)))
    (setf (%http-cache-entries cache)
          (cons entry
                (delete entry (%http-cache-entries cache))))
    (setf (http-cache-entry-accessed-at entry) now)
    (if (< now (http-cache-entry-expires-at entry))
        (values (%cache-entry-response-for-request entry request)
                :fresh
                (%cache-copy-entry entry))
        (values nil :stale (%cache-copy-entry entry)))))

(defun %cache-remove-key (cache key &optional method)
  (setf (%http-cache-entries cache)
        (delete-if (lambda (entry)
                     (and (string= key (http-cache-entry-key entry))
                          (or (null method)
                              (string= method
                                       (http-cache-entry-method entry)))))
                   (%http-cache-entries cache))))

(defun http-cache-store (cache request response &key now)
  "Store a cacheable GET/HEAD RESPONSE and return its detached entry."
  (unless (http-cache-p cache)
    (%client-protocol-error "Expected an HTTP-CACHE value." cache))
  (unless (and (http-request-p request) (http-response-p response))
    (%client-protocol-error
     "Cache storage requires an HTTP-REQUEST and HTTP-RESPONSE."
     (list request response)))
  (let* ((method (http-request-method request))
         (now (or now (funcall (%http-cache-clock-function cache))))
         (headers (http-response-headers response))
         (vary (%cache-vary-fields headers)))
    (unless (and (member method '("GET" "HEAD") :test #'string=)
                 (%cache-valid-status-p (http-response-status response))
                 (not (eq vary :star))
                 (not (%cache-directive (%cache-control-directives headers)
                                        "no-store"))
                 (not (and (http-header-present-p headers "Set-Cookie")
                           (not (%cache-directive
                                 (%cache-control-directives headers)
                                 "public"))))
                 (multiple-value-bind (authorization-present)
                     (values (http-header-present-p
                              (http-request-headers request)
                              "Authorization"))
                   (or (not authorization-present)
                       (%cache-directive (%cache-control-directives headers)
                                         "public"))))
      (return-from http-cache-store nil))
    (multiple-value-bind (cacheable-p expires-at)
        (%cache-freshness response now)
      (unless cacheable-p
        (return-from http-cache-store nil))
      (let* ((key (http-cache-key request))
             (entry (%make-http-cache-entry
                     :key key
                     :method method
                     :uri (http-request-uri request)
                     :response (%copy-client-response response)
                     :stored-at now
                     :expires-at expires-at
                     :etag (http-header-value headers "ETag")
                     :last-modified (http-header-value headers "Last-Modified")
                     :vary (or vary '())
                     :request-vary-values
                     (%cache-request-vary-values request (or vary '()))
                     :accessed-at now)))
        (%cache-remove-key cache key method)
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
