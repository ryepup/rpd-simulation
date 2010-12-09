;;;; rpd-simulation.lisp
(in-package #:rpd-simulation-impl)
(defvar *simulation-step-hook* nil
  "list of callback functions called during each step of simulation")

;;; "rpd-simulation" goes here. Hacks and glory await!
(defclass simulation ()
  ((queue :accessor queue
	  :initform (pileup:make-heap #'< :key #'cdr ))
   (processes :accessor processes :initform nil)
   (current-time :accessor current-time :initform 0)))

(defclass process ()
  ((coroutine :accessor coroutine :initarg :coroutine)))

(defvar *simulation* nil "The current simulation")
(defvar *process nil "The current running process")

(defmacro with-simulation (() &rest body)
  "opens a simulation context"
  `(let ((*simulation* (make-instance 'simulation )))
     ,@body))

(defgeneric schedule (simulation process &optional at)
  (:method ((sim simulation) process &optional (at 1))
	   (pileup:heap-insert
	    (cons process (+ at (current-time sim)))
	    (queue sim))))

(defgeneric activate (simulation process at)
  (:method ((sim simulation) process at)
	   (push process (processes sim))
	   (schedule sim process at)))

(defgeneric priority (thing)
  (:method ((q pileup:heap))
	   (cdr (pileup:heap-top q))))

(defgeneric next-process (thing)
  (:method ((sim simulation))
	   (car (pileup:heap-pop (queue sim)))))

(defgeneric simulate-step (sim)
  (:method ((sim simulation))
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
		   (ecase result
		     (:hold (schedule sim p (or (first args) 1)))
		     ;this process is dead!
		     (:done (done sim p))))))

	     (run-hooks *simulation-step-hook*)
	     )))

(defun run-hooks (hook)
  (when hook
    (mapc #'funcall (ensure-list hook))))

(defgeneric done (sim process)
  (:method ((sim simulation) process)
	   (setf (processes sim) (remove process (processes sim)))))

(defgeneric simulate (thing &key &allow-other-keys))
(defmethod simulate ((sim simulation) &key until stop-if for &allow-other-keys) 
	   (when (and for until)
	     (error "Cannot specify both :for and :until.  Pick one."))
	     
	   (when for
	     (setf until (+ for (current-time sim))))
	   
	   (iterate
	     (with stop-condition =
		   (cond
		     ((and until stop-if)
		      (lambda (i)
			(and (< i until)
			     (not (funcall stop-if (processes sim))))))
		     (until (lambda (i) (< i until)))
		     (stop-if (lambda (i)
				(declare (ignore i))
				(not (funcall stop-if
					      (processes sim)))))))
	     (for i from (current-time sim))
	     (while (funcall stop-condition i))
	     (simulate-step sim)
	     (finally (return (values i sim)))))

(defgeneric run (thing)
  (:method ((p process))
	   (funcall (coroutine p))))

(defgeneric note (thing format &optional args)
  (:method ((sim simulation) format &optional args)
	   (format T "~%[~a] " (current-time sim))
	   (apply #'format `(T ,format ,@(ensure-list args)))))



(defmacro defprocess (name superclasses slots &rest extras)
  (let ((action-body (rest (find :action extras :key #'car)))
	(func-body (rest (find :function extras :key #'car)))
	(extras (remove-if  (lambda (i)
			      (member i '(:action :function)))
			    extras :key #'car)))
    `(progn
       (defclass ,name (process ,@superclasses)
	 ,slots
	 ,@extras)
       ,(when func-body
	 `(defmethod run ((self ,name)) ,@func-body))
       ,(when action-body
	 `(defmethod initialize-instance :after ((p ,name) &key &allow-other-keys)
		     (setf (coroutine p)
			   (let ((self p))
			     (declare (ignorable self))
			     (make-coroutine ()			    
			       ,@action-body
			       ))))))))
