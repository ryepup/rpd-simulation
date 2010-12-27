(in-package #:rpd-simulation)

(defclass simulation ()
  ((queue :accessor queue
	  :initform (pileup:make-heap #'< :key #'cdr ))
   (current-time :accessor current-time :initform 0)))

(defun make-simulation ()
  "Create a new basic simulation"
  (make-instance 'simulation))

(defun make-schedule-item (actor time-to-run)
  (cons actor time-to-run))
(defun scheduled-actor (schedule-item)
  (car schedule-item))
(defun scheduled-time (schedule-item)
  (cdr schedule-item))

(defmethod activate ((self simulation) (actor actor)
		     &optional ticks-from-now)
	   (setf (simulation actor) self)
	   
	   (pileup:heap-insert
	    (make-schedule-item
	     actor (+ (or ticks-from-now 1)
		      (current-time self)))
	    (queue self)))

(defgeneric next-actor (sim)
  (:documentation "returns the next actor before the tick"))

(defmethod next-actor ((self simulation))
	   
	   )

(defmethod simulation-step ((sim simulation))
	   (let ((queue (queue sim)))
	     (incf (current-time sim))
	     (iterate
	       (with time = (current-time sim))
	       (for next-priority = (priority queue))
	       (while (and next-priority
			   (= next-priority time)))
	       (let ((p (next-process sim)))
		 (destructuring-bind (result &rest args)
		     (multiple-value-list (run p))
		   (when *process-after-step-hook*
		     (funcall *process-after-step-hook* p))
		   (ecase result
		     (:hold (schedule sim p (or (first args) 1)))
					;this process is dead!
		     (:done
			(when *process-dead-hook*
			  (funcall *process-dead-hook* p))
			(done sim p))))))

	     (run-hooks *simulation-step-hook*)
	     ))