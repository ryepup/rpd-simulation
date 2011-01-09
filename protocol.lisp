(in-package #:rpd-simulation)

(defgeneric schedule (actor &optional ticks-from-now)
  (:documentation "Schedule an actor to run again in ticks-from-now.

Returns the scheduled item"))

(defgeneric activate (sim actor &optional ticks-from-now)
  (:documentation "adds an actor the the simulation, and schedules it to start based on ticks-from-now."))

(defgeneric simulation-step (thing)
  (:documentation "runs this things simulation step"))

;;;
#+nil
(progn
  (export 'yield
	  :rpd-simulation)
  (export 'next-actor
	  :rpd-simulation)
  (shadowing-import  (list 'make-coroutine
			   'yield)
		     :rpd-coroutines)

  (shadowing-import (list 'log-message) :cl-log))