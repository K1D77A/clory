(in-package #:clory)

#||
This file contains code contains a generic wrapper over responses from the various
APIs
||#

(define-condition ory-error-condition (clory-condition)
  ((request
    :accessor request
    :initarg :request
    :type request
    :documentation "The instance of 'request used to make the request.")
   (dex
    :accessor dex
    :initarg :dex
    :documentation "The dex response.")
   (status-code
    :accessor status-code
    :initarg :status-code
    :type integer)
   (ory
    :accessor ory
    :initarg :ory
    :type ory
    :documentation "The instance of class 'ory used to make the request.")
   (ory-error-response
    :accessor ory-error-response
    :initarg :ory-error-response
    :type ory-error-response))
  (:documentation "Signalled and populated when there is an error from Ory."))

(defclass* ory-error-response ()
  ((debug
    :accessor debug
    :initarg :debug
    :type string )
   (error-name
    :accessor error-name
    :initarg :error-name
    :type string)
   (error-description
    :accessor error-description
    :initarg :error-description
    :type string)
   (message
    :accessor message
    :initarg :message
    :type string)
   (status-code
    :accessor status-code
    :initarg :status-code
    :type string)))

(define-condition client-error-response (ory-error-condition)
  ())

(define-condition server-error-response (ory-error-condition)
  ())

(define-condition unknown-server-response (ory-error-condition)
  ())

(defclass ory-response-class ()
  ((status-code
    :accessor status-code
    :initarg :status-code
    :documentation "HTTP status code.")
   (body
    :accessor body
    :initarg :body
    :documentation "HTTP request body.")
   (dex
    :accessor dex
    :initarg :dex
    :documentation "The Dex response.")))

(defmethod print-object ((obj ory-response-class) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (print-all-slots obj stream)))

(defclass information-response (ory-response-class)
  ())

(defclass successful-response (ory-response-class)
  ())

(defclass redirection-response (ory-response-class)
  ())

(defun determine-response (response)
  (let ((n (dexador:response-status response)))
    (cond ((<= 100 n 199)
           (values #'make-instance  'information-response))
          ((<= 200 n 299)
           (values #'make-instance  'successful-response))
          ((<= 300 n 399)
           (values #'make-instance  'redirection-response))
          ((<= 400 n 499)
           (values #'make-condition 'client-error-response))
          ((<= 500 n 599)
           (values #'make-condition 'server-error-response))
          (t
           (values #'make-condition 'unknown-server-response)))))

(defun construct-response-from-api (response)
  (let ((status (dexador:response-status response))
        (body (dexador:response-body response)))
    (multiple-value-bind (fun class)
        (determine-response processor response)
      (signal-when-condition
       processor 
       (apply fun class
              (construct-initargs-for-response processor status body response))))))

(defun signal-when-condition (c)
  "Signals a condition when C is a condition."
  (typecase c
    (condition (error c))
    (otherwise c)))

