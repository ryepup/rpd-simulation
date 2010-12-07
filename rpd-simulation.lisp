;;;; rpd-simulation.lisp

(in-package #:rpd-simulation)

;;; "rpd-simulation" goes here. Hacks and glory await!
(defclass simulation ()
  ((queue :accessor queue :initform (pileup:make-heap #'< :key #'cdr )
	  :initarg :queue)
   (processes :accessor processes :initform nil)
   (current-time :accessor current-time :initform 0)))

(defvar *simulation* nil "The current simulation")
(defvar *process nil "The current running process")

(defmacro with-simulation (() &rest body)
  "opens a simulation context"
  `(let ((*simulation* (make-instance 'simulation :queue )))
     ,@body))


(defgeneric %schedule (simulation process at)
  (:method ((sim simulation) process at)
	   (pileup:heap-insert
	    (cons process (+ at (current-time sim)))
	    (queue sim))))

(defun schedule (process &optional (at 1))
  (%schedule *simulation* process at))

(defgeneric %activate (simulation process at)
  (:method ((sim simulation) process at)
	   (push process (processes sim))
	   (%schedule sim process at)))

(defun activate (process &key (at 1))
  (%activate *simulation* process at)
  process)

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
	       (for next-priority = (priority queue))
	       (while (and next-priority
			   (= next-priority
			      (current-time sim))))
	       (let ((process (next-process sim)))
		 (%simulate process nil nil))))
	   (note "step")))

(defgeneric %simulate (thing until stop-if))
(defmethod %simulate ((sim simulation) until stop-if)
	   (iterate
	     (for i from 0)
	     (while (and (or (null until)
			     (< i until))
			 (or (null stop-if)
			     (not (funcall stop-if
					   (processes sim))))))
	     (simulate-step sim)
	     (finally (return (values i sim)))))

(defclass process ()
  ((coroutine :accessor coroutine :initarg :coroutine)))

(defgeneric generate (thing)
  (:method ((p process))
	   (funcall (coroutine p))))

(defmethod %simulate ((p process) until stop-if)
	   (declare (ignore until stop-if))
	   (destructuring-bind (result &rest args)
	       (multiple-value-list (generate p))
	     (ecase result
	       (:hold (apply #'schedule `(,p ,@args)))
	       (:done nil))))

(defun simulate (&key until stop-if)
  (%simulate *simulation* until stop-if))

(defgeneric %note (thing format args)
  (:method ((sim simulation) format args)
	   (format T "~%[~a] " (current-time sim))
	   (apply #'format `(T ,format ,@(ensure-list args)))))

(defun note (format &rest args)
  (%note *simulation* format args))

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
	 `(defmethod generate ((self ,name)) ,@func-body))
       ,(when action-body
	 `(defmethod initialize-instance :after ((p ,name) &key &allow-other-keys)
		     (setf (coroutine p)
			   (let ((self p))
			     (declare (ignorable self))
			     (make-coroutine ()			    
			       ,@action-body
			       ))))))))
