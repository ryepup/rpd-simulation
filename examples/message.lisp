(in-package :rpd-simulation-examples)

;; ported from http://simpy.sourceforge.net/SimPyDocs/Manuals/Manual.html#creating-a-process-object

(defprocess message ()
  ((i :initarg :i :accessor i)
   (len :initarg :len :accessor len))
  (:documentation "a simple process")
  (:action
   (note "Starting ~a" (i self))
   (yield :hold 100)
   (note "Arrived ~a" (i self))))

(defun message ()
  (with-simulation ()
    (let ((p1 (make-instance 'message :i 1 :len 204))
	  (p2 (make-instance 'message :i 2 :len 33)))
      (activate p1)
      (activate p2 :at 6)
      (simulate :until 200)
      (note "done"))))
