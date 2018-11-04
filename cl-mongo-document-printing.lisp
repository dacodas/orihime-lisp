(in-package :cl-mongo)

;; These are taken right out of cl-mongo, but I've replace the ~A
;; format directives with ~S instead so it's a bit clearer for me

(defmethod print-object.dacoda ((document document) stream)
  (format stream "<~S> : { ~%" (type-of document))
  (when (and (slot-boundp  document '_local_id) (slot-boundp  document '_id))
    (unless (_local document) (format stream "~3,8T_id : ~S~%" (_id document))))
  (if (slot-boundp document 'elements)
      (progn
	(format stream "~3,8Telements : ~S" (hash-table-count (elements document)))
	(print-hash.dacoda (elements document) stream :max 20))
      "no elements set..")
  (format stream "}~%"))

(defun print-hash.dacoda (ht stream &key (max (hash-table-count ht)))
  (labels ((prdocs (docs)
	     (format stream "~1,1T[")
	     (block print-array
	       (let ((counter 0))
		 (dolist (doc docs)
		   (incf counter)
		   (if (typep doc 'document)
		       (print-hash.dacoda (elements doc) stream :max max)
		       (format stream "~S," doc))
		   (when (> counter 100)
		     (progn
		       (format stream "[....](~A elements left)" (- (length docs) 100))
		       (return-from print-array nil))))))
	     (format stream "]~%"))
	   (vpr (v)
	     (cond ((typep v 'cons)     (prdocs v))
		   ((typep v 'document) (prdocs (list v)))
		   (t                  (format stream "~S~%" v)))))
    (format stream "~%~3,8T{~%")
    (with-hash-table-iterator (iterator ht)
      (dotimes (repeat max)
	(multiple-value-bind (exists-p key value) (iterator)
	  (if exists-p 
	      (progn 
		(format stream "~3,8T~S -> " key)
		(vpr value))))))
    (when (< max (hash-table-count ht)) (format stream "~3,8T[..............]~%"))
    (format stream "~3,8T}~%")))
