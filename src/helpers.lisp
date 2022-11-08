(in-package #:clory)

(defgeneric to-array (ele)
  (:documentation "Convert ELE to an array")
  (:method ((str string))
    (babel:string-to-octets str))
  (:method ((str array))
    str))

(defun crc-raw (raw-body)
  (ironclad:octets-to-integer (ironclad:digest-sequence :crc32 raw-body)))

(defun in-list (obj)
  (if (listp obj)
      (first obj)
      obj))

(defgeneric print-all-slots (obj stream))

(defmethod print-all-slots (obj stream)
  (let ((slots (c2mop:class-slots (class-of obj))))
    (format stream "~%")
    (mapc (lambda (slot)
            (let ((name (c2mop:slot-definition-name slot)))
              (when (slot-boundp obj name)
                (format stream "~A: ~A~%" name (slot-value obj name)))))
          slots)))

(defmacro with-hash-keys (keys hash &body body)
  "Creates a let binding for each of the keys listed in KEYS in HASH using gethash, 
each of these KEYS has to have a non nil value otherwise signals 'malformed-json."
  (alexandria:once-only (hash)
    `(let ,(mapcar (lambda (key)
                     `(,key (gethash ,(string key) ,hash)))
            keys)
       (locally ,@body))))

