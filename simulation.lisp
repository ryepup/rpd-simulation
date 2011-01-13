(in-package #:rpd-simulation)

(defclass simulation ()
  ((queue :accessor queue
	  :initform (pileup:make-heap #'< :key #'cdr ))
   (actors :accessor actors :initform (list))
   (current-time :accessor current-time :initform 0)))

(defmethod print-object ((self simulation) stream)
	   (print-unreadable-object (self stream :type t :identity t)
	     (format stream "~a" (current-time self))))

(defun make-simulation (&key board)
  "Create a new basic simulation"
  (if board
      (make-instance 'spatial-simulation :board board)
      (make-instance 'simulation)))

(defun make-schedule-item (actor time-to-run)
  (cons actor time-to-run))
(defun scheduled-actor (schedule-item)
  (car schedule-item))
(defun scheduled-time (schedule-item)
  (cdr schedule-item))

(defmethod activate ((self simulation) (actor actor)
		     &optional ticks-from-now)
	   (unless (get 'activated actor nil) 
	     (setf (simulation actor) self
		   (get 'activated actor) T)	     
	     (push actor (actors self)))	   
	   (pileup:heap-insert
	    (make-schedule-item
	     actor (+ (or ticks-from-now 1)
		      (current-time self)))
	    (queue self)))

(defgeneric deactivate (sim actor)
  (:method ((sim simulation) (actor actor)) 
	   (setf (actors sim)
		 (delete actor (actors sim)))))

(defgeneric next-actor (sim)
  (:documentation "returns the next actor before the tick"))

(defmethod next-actor ((self simulation))
	   (multiple-value-bind (sched-item not-empty-p) (pileup:heap-top (queue self))
	     (when (and not-empty-p
			(>= (current-time self)
			   (scheduled-time sched-item)))
	       (scheduled-actor (pileup:heap-pop (queue self))))))

(defun process-results (sim actor results &rest args)
  (etypecase results
    (number (schedule actor results))
    (keyword (ecase results
	       (:done (deactivate sim actor))
	       ((:incf :decf) (destructuring-bind (res &optional (value 1 value-supplied-p)) args
			(let* ((testfn (if value-supplied-p
					   (lambda () (adjust res results value))
					   (lambda () (adjust res results))))
			       (successfn (lambda ()
					    (schedule actor 1)))
			       (qr (make-instance 'queuer
						  :test testfn
						  :success successfn)))
			  ;; run it once, reschedule if we don't succeed
			  (unless (eq :done (simulation-step qr))
			    (log-message :debug "need to queue")
			    (activate sim qr)))))))))

(defmethod simulation-step ((sim simulation))
	   (incf (current-time sim))
	   (iter (for actor = (next-actor sim))
		 (while actor)
		 (apply #'process-results sim actor
			(multiple-value-list
			    (simulation-step actor))))
	   (not (pileup:heap-empty-p (queue sim))))

(defun simulate (sim &key until stop-if)
  "runs the simulation until we have no more actors or the limit
specified in UNTIL is reached."
  (let ((stop-conditions (list (lambda (i)
				 (declare (ignore i))
				 (simulation-step sim)))))
    (when until
      (push (lambda (i) (< i until)) stop-conditions))
    (when stop-if      
      (push (lambda (i)
	      (declare (ignore i))
	      (not (funcall stop-if)))
	    stop-conditions))
    (setf stop-conditions (nreverse stop-conditions))
    (iter
      (for i from 1)
      (while (every (lambda (sc) (funcall sc i)) stop-conditions)))))
