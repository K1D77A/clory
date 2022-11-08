(in-package #:clory)

#||
This file contains the code for a Metaclass used to create the request objects for each
payment processor.
||#

(defclass clory-api-call (c2mop:funcallable-standard-class)
  ((string-constructor
    :accessor string-constructor
    :initform nil)
   (endpoint
    :accessor endpoint
    :initarg :endpoint
    :initform nil)
   (genned-slot-names
    :accessor genned-slot-names
    :initarg :genned-slot-names)
   (query-slot-names
    :accessor query-slot-names
    :initarg :query-slot-names)
   (query-constructor
    :accessor query-constructor
    :initarg :query-constructor
    :initform nil)))

(defmethod string-constructor ((class clory-api-call))
  (in-list (slot-value class 'string-constructor)))

(defmethod query-constructor ((class clory-api-call))
  (in-list (slot-value class 'query-constructor)))

(defmethod endpoint ((class clory-api-call))
  (in-list (slot-value class 'endpoint)))


(defclass clory-api-slot (c2mop:slot-definition)
  ((as-string
    :accessor as-string
    :initarg :as-string
    :type string
    :documentation "String version of the slot-name. When set this is used in place 
of the slot name when encoding in the Query string.")))


(defclass clory-api-slot-direct (clory-api-slot c2mop:standard-direct-slot-definition)
  ())

(defclass clory-api-slot-effective (clory-api-slot
                                       c2mop:standard-effective-slot-definition)
  ())

(defmethod c2mop:validate-superclass ((class clory-api-call)
                                      (metaclass c2mop:funcallable-standard-class))
  t)

(defmethod c2mop:validate-superclass ((class clory-api-slot)
                                      (metaclass standard-class))
  t)

(defmethod c2mop:effective-slot-definition-class ((class clory-api-call) &rest initargs)
  (declare (ignore initargs))
  (find-class 'clory-api-slot-effective))

(defmethod c2mop:direct-slot-definition-class ((class clory-api-call) &rest initargs)
  (declare (ignore initargs))
  (find-class 'clory-api-slot-direct))

(defclass request ()
  ((request-fun
    :reader request-fun
    :initarg :request-fun
    :initform 'dex:get)
   (content-type
    :accessor content-type
    :initarg :content-type
    :initform "application/json"))
  (:documentation "Top level request class")
  (:metaclass clory-api-call))

(defmethod print-object ((obj request) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (print-all-slots obj stream)))

(defclass request-without-content (request)
  ()
  (:metaclass clory-api-call))

(defclass get-request (request-without-content)
  ((request-fun :initform 'dex:get))
  (:metaclass clory-api-call))

(defclass delete-request (request-without-content)
  ((request-fun :initform 'dex:delete))
  (:metaclass clory-api-call))

(defclass request-with-content (request)
  ((content
    :accessor content
    :initarg :content
    :type list))
  (:metaclass clory-api-call))

(defclass post-request (request-with-content)
  ((request-fun :initform 'dex:post))
  (:metaclass clory-api-call))

(defclass post-files-request (post-request)
  ((content-type
    :initform "multipart/related"))
  (:metaclass clory-api-call))

(defclass put-request (request-with-content)
  ((request-fun :initform 'dex:put))
  (:metaclass clory-api-call))

(defclass patch-request (request-with-content)
  ((request-fun :initform 'dex:patch))
  (:metaclass clory-api-call))

(defclass response ()
  ()
  (:documentation "Top level response class"))

(defclass api-failure ()
  ()
  (:documentation "API Failure superclass."))

(defmethod print-object ((obj api-failure) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~%")
    (print-all-slots obj stream)))
