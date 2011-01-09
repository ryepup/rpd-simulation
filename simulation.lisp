(in-package #:rpd-simulation)

(defclass simulation ()
  ((queue :accessor queue
	  :initform (pileup:make-heap #'< :key #'cdr ))
   (current-time :accessor current-time :initform 0)))

(defmethod print-object ((self simulation) stream)
	   (print-unreadable-object (self stream :type t :identity t)
	     (format stream "~a" (current-time self))))

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
	   (multiple-value-bind (sched-item not-empty-p) (pileup:heap-top (queue self))
	     (when (and not-empty-p
			(= (current-time self)
			   (scheduled-time sched-item)))
	       (scheduled-actor (pileup:heap-pop (queue self))))))

(defmethod simulation-step ((sim simulation))
	   (cl-log:log-message :debug "~a step start~%" sim)
	   (incf (current-time sim))
	   (iter (for actor = (next-actor sim))
		 (while actor)
		 (cl-log:log-message :debug "running ~a~%" actor)
		 (let ((results (simulation-step actor)))
		   (etypecase results
		     (number (schedule actor results)))))
	   (cl-log:log-message :debug "~a step end~%" sim))