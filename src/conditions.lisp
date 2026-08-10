(in-package #:http-kit)

(define-condition http-error (error)
  ((message :initarg :message :reader http-error-message)
   (operation :initarg :operation :initform nil :reader http-error-operation))
  (:report (lambda (condition stream)
             (format stream "~A" (http-error-message condition)))))

(define-condition http-protocol-error (http-error)
  ((detail :initarg :detail :initform nil :reader http-protocol-error-detail)))

(define-condition http-invalid-uri (http-protocol-error)
  ((input :initarg :input :reader http-invalid-uri-input)))

(define-condition http-invalid-header (http-protocol-error)
  ((name :initarg :name :initform nil :reader http-invalid-header-name)
   (reason :initarg :reason :initform nil :reader http-invalid-header-reason)))

(define-condition http-invalid-status (http-protocol-error)
  ((line :initarg :line :reader http-invalid-status-line)
   (code :initarg :code :initform nil :reader http-invalid-status-code)))

(define-condition http-connection-error (http-error)
  ((cause :initarg :cause :initform nil :reader http-connection-error-cause)))

(define-condition http-timeout (http-error)
  ((kind :initarg :kind :initform :deadline :reader http-timeout-kind)))

(define-condition http-size-limit-exceeded (http-protocol-error)
  ((limit :initarg :limit :reader http-size-limit-exceeded-limit)
   (observed :initarg :observed :reader http-size-limit-exceeded-observed)
   (kind :initarg :kind :reader http-size-limit-exceeded-kind)))

(define-condition http-unsupported-feature (http-protocol-error)
  ((feature :initarg :feature :reader http-unsupported-feature-name)))
