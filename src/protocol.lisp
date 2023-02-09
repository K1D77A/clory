(in-package #:clory)

#||
Many helpers for defining MOP protocols for API wrappers.
||#

(defclass* ory ()
  ((base-url
    :accessor base-url
    :initarg :base-url
    :type string)
   (api-key
    :accessor api-key
    :initarg :api-key
    :initform nil
    :type (or null string)))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-slot-names-p t))

(defun replace-vars-for-slot-names (split slots)
  "Takes in a split list split by #\/ (url) produces a list of slot names"
  (mapcar (lambda (str)
            (let ((found? (find (subseq str 1) slots :test #'string-equal)))
              (if found?
                  found?
                  str)))
          split))

(defun gen-url-generator (class)
  "Compiles a function for CLASS that when called with the request object. When evaluated
the function returns a string with specific slots values written into a string."
  (with-accessors ((endpoint endpoint)
                   (genned-slot-names genned-slot-names))
      class 
    (let* ((split (str:split #\/ (in-list endpoint) :omit-nulls t))
           (slots (in-list (genned-slot-names class)))
           (compared (replace-vars-for-slot-names split slots)))
      (if slots 
          (compile nil
                   `(lambda (request)
                      (format nil "/~{~A~^/~}"
                              (loop :for slot? :in ',compared
                                    :collect
                                    (if (stringp slot?)
                                        slot? 
                                        (quri:url-encode 
                                         (slot-value request slot?)))))))
          (lambda (request)
            (declare (ignore request))
            (in-list endpoint))))))

(defun gen-query-generator (query-slots query-slot-names)
  "Given QUERY-SLOTS and QUERY-SLOT-NAMES compiles a function that when evaluated 
with a request will step through the query slots and concatenate the query slot names and
values into a string. Uses #'encode-query-slot to encode the query params."
  (if query-slots 
      (compile nil
               `(lambda (request)
                  (let ((str
                          (format nil "?~{~A~^&~}"
                                  (loop :for slot :in ',query-slots
                                        :for slot-name :in ',query-slot-names
                                        :if (slot-boundp request slot-name)
                                          :collect
                                          (encode-query-slot request slot slot-name)))))
                    (if (string= str "?")
                        ""
                        str))))
      (lambda (req)
        (declare (ignore req))
        "")))

(defgeneric encode-query-slot (request slot slot-name)
  (:documentation "Based on REQUEST SLOT and SLOT-NAME encode the slot in a HTTP Query 
param format Uses #'encode-query-value for value encoding."))

(defmethod encode-query-slot (request slot slot-name)
  (format nil "~A=~A"
          (if (slot-boundp slot 'as-string)
              (slot-value slot 'as-string)
              (string-downcase (symbol-name slot-name)))
          (encode-query-value slot (slot-value request slot-name))))

(defgeneric encode-query-value (slot value)
  (:documentation "Given a SLOT and a VALUE encodes the value correctly for a HTTP 
Query param. Encode differently based on the type of the Value."))

(defmethod encode-query-value (slot (value string))
  (quri:url-encode value))

(defmethod encode-query-value (slot (value number))
  (quri:url-encode (format nil "~D" value)))

(defmethod encode-query-value (slot (value (eql nil)))
  "false")

(defmethod encode-query-value (slot (value (eql t)))
  "true")

(defmethod encode-query-value (slot (value sequence))
  "When a VALUE is sequence, then we will encode it like a query param array."
  (let ((name (string-downcase (c2mop:slot-definition-name slot))))
    (format nil "~{~A~^&~}" 
            (map 'list
                 (lambda (val)
                   (format nil "~A[]=~A" name (quri:url-encode val)))
                 value))))

(defun slots-from-url (url)
  "Given a URL splits the strings out by #\: and then generates a list of plists 
used for creating slots in a class."
  (let* ((split (str:split #\/ url :omit-nulls t))
         (slots (remove-if-not (lambda (ele) (char= #\: (aref ele 0))) split)))
    (mapcar (lambda (slot)
              (let* ((name (subseq slot 1))
                     (upcase (string-upcase name))
                     (intern (intern upcase))
                     (key (intern upcase :keyword)))
                (list intern :accessor intern :initarg key)))
            slots)))

(defgeneric generate-url (ory request)
  (:documentation "Generate a request URL that is passed to Dex."))

(defmethod generate-url (ory req)
  "Default URL generator."
  (with-accessors ((string-constructor string-constructor)
                   (query-constructor query-constructor))
      (class-of req)
    (concatenate 'string
                 (base-url ory)
                 (funcall string-constructor req)
                 (when query-constructor
                   (funcall query-constructor req)))))

(defgeneric generate-dex-list (ory request)
  (:method-combination append :most-specific-last)
  (:documentation "Generate a list passed to dex using #'apply.")
  (:method append (ory request)
    (list :headers (list (cons "Content-Type"
                               (content-type-to-string (content-type request))))))
  (:method append (ory (request request-with-content))
    `(:content ,(content request))))

(defgeneric content-type-to-string (ct)
  (:method ((ct (eql :json)))
    "application/json")
  (:method ((ct (eql :form)))
    "application/x-www-form-urlencoded"))
    
(defmacro wrap-dex-call (&body body)
    `(handler-case
        (multiple-value-list (locally ,@body))
      (dexador:http-request-failed (c)
        c)))

(defun call-api (ory request)
  "Call the API using ORY."
  (let ((url (generate-url ory request))
        (args (generate-dex-list ory request))
        (fun (request-fun request)))
    (construct-response-from-api 
     (wrap-dex-call 
       (apply fun url args)))))


(defmacro defapi (name (endpoint super)
                  &optional query-slots)
  (let* ((slots (slots-from-url endpoint))
         (names (mapcar #'first slots))
         (query-slot-names (mapcar #'first query-slots)))
    `(let ((class 
             (defclass* ,name (,super)
               ,(append slots query-slots)
               ,@(append `((:metaclass clory-api-call)
                           (:genned-slot-names ,names)
                           (:query-slot-names ,query-slot-names)
                           (:endpoint ,endpoint)))
               (:export-class-name-p t)
               (:export-accessor-names-p t))))
       (c2mop:ensure-finalized class)
       (let* ((direct-slots (c2mop:class-direct-slots class))
              (direct-query-slots
                (mapcar (lambda (slot-name)
                          (find slot-name direct-slots
                                :key #'c2mop:slot-definition-name
                                :test #'string-equal))
                        ',query-slot-names)))
         (with-slots (string-constructor query-constructor)
             class
           (setf (string-constructor class) (gen-url-generator class))
           (when ',query-slots
             (setf (query-constructor class)
                   (gen-query-generator direct-query-slots
                                        ',query-slot-names))))))))
