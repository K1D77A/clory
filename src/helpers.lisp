(in-package #:clory)

(defun in-list (obj)
  (if (listp obj)
      (first obj)
      obj))

(defgeneric print-all-slots (obj stream)
  (:documentation "Print all the values of the slots in OBJ to STREAMR")
  (:method (obj stream)
    (let ((slots (c2mop:class-slots (class-of obj))))
      (format stream "~%")
      (mapc (lambda (slot)
              (let ((name (c2mop:slot-definition-name slot)))
                (when (slot-boundp obj name)
                  (format stream "~A: ~A~%" name (slot-value obj name)))))
            slots))))

