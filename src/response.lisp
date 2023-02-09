(in-package #:clory)

#||
This file contains code contains a generic wrapper over responses from the various
APIs
||#

(define-condition ory-condition (clory-condition)
  ((status-code
    :accessor status-code
    :initarg :status-code
    :documentation "HTTP status code.")
   (result 
    :accessor result 
    :initarg :result 
    :documentation "HTTP request body.")
   (headers
    :accessor headers
    :initarg :headers)
   (uri
    :accessor uri
    :initarg :uri)
   (stream 
    :accessor estream
    :initarg :stream)
   (ory
    :accessor ory
    :initarg :ory
    :type ory
    :documentation "The instance of class 'ory used to make the request."))
  (:documentation "Signalled and populated when there is an error from Ory."))
(c2mop:ensure-finalized (find-class 'ory-condition))

(define-condition ory-default-error (ory-condition)
  ((error-debug
    :accessor error-debug
    :initarg :error-debug
    :type string )
   (error-name
    :accessor error-name
    :initarg :error-name
    :type string)
   (error-description
    :accessor error-description
    :initarg :error-description
    :type string)
   (error 
    :accessor eerror
    :initarg :error 
    :type string)))
(c2mop:ensure-finalized (find-class 'ory-default-error))

(define-condition ory-generic-error (ory-condition)
  ((code
    :accessor code
    :initarg :code
    :type (or null integer))
   (debug
    :accessor edebug
    :initarg :debug
    :type (or null string))
   (details
    :accessor details
    :initarg :details)
   (id
    :accessor id
    :initarg :id
    :type (or null string))
   (message
    :accessor message
    :initarg :message
    :type string)
   (reason 
    :accessor reason 
    :initarg :reason 
    :type (or null string))
   (request
    :accessor request
    :initarg :request
    :type (or null string))
   (status
    :accessor status
    :initarg :status
    :type (or null string))))
(c2mop:ensure-finalized (find-class 'ory-generic-error))

(defun ory-error-class-prototype (result)
  (c2mop:class-prototype (find-class 
                          (if (gethash "message" result)
                              'ory-generic-error
                              'ory-default-error))))

 
(defclass* ory-response ()
  ((status-code
    :accessor status-code
    :initarg :status-code
    :documentation "HTTP status code.")
   (result 
    :accessor result 
    :initarg :result 
    :documentation "HTTP request body.")
   (headers
    :accessor headers
    :initarg :headers)
   (uri
    :accessor uri
    :initarg :uri)
   (stream 
    :accessor estream
    :initarg :stream)))
(c2mop:ensure-finalized (find-class 'ory-response))

(defmethod print-object ((obj ory-response) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (print-all-slots obj stream)))

(defun determine-response-type (status)
  (cond ((<= 100 status 399)
         'ory-response)
        ((<= 400 status 599)
         'ory-error-condition)
        (t 'ory-condition)))
         
(defun construct-response-from-api (response)
  (destructuring-bind (result status-code headers uri stream)
      response
    (let* ((response-type (determine-response-type status-code))
           (response-proto (c2mop:class-prototype (find-class response-type)))
           (response-inits (construct-initargs-for-response response-proto result
                                                            status-code headers
                                                            uri stream))
           (response (build-response response-proto response-inits)))
      (signal-when-condition response))))
 
(defgeneric build-response (proto initargs)
  (:method ((proto condition) initargs)    
    (apply #'make-condition (class-of proto) initargs))
  (:method (proto initargs)
    (apply #'make-instance (class-of proto) initargs)))

(defgeneric construct-initargs-for-response (proto result status-code headers uri stream)
  (:method-combination append :most-specific-last)
  (:method append (proto result status-code headers uri stream)
    (list :result (shasht:read-json result)
          :status-code status-code
          :headers headers
          :uri uri
          :stream stream))
  (:method append ((proto ory-condition) result status-code headers uri stream)
    (list :ory *ory*))
  (:method append ((proto ory-default-error) result status-code headers uri stream)
    (let ((ed (gethash "error_debug" result))
          (en (gethash "error_name" result))
          (edesc (gethash "error_description" result))
          (eh (gethash "error_hint" result)))
      (list :error-debug ed
            :error-name en
            :error-description edesc
            :error-hint eh)))
  (:method append ((proto ory-generic-error) result status-code headers uri stream)
    (let ((code (gethash "code" result))
          (de (gethash "debug" result))
          (det (gethash "details" result))
          (id (gethash "id" result))
          (mes (gethash "message" result))
          (re (gethash "reason" result))
          (req (gethash "request" result))
          (sta (gethash "status" result)))
      (list :code code
            :debug de
            :details det
            :id id
            :message mes
            :reason re
            :request req
            :status sta))))
            
(defun signal-when-condition (c)
  "Signals a condition when C is a condition."
  (typecase c
    (condition (error c))
    (otherwise c)))

