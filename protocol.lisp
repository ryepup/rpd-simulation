(in-package #:rpd-simulation)

(defgeneric schedule (actor &optional ticks-from-now)
  (:documentation "Schedule an actor to run again in ticks-from-now.

Returns the scheduled item"))

(defgeneric activate (thing new-actor &optional ticks-from-now)
  (:documentation "adds an actor the the simulation, and schedules it to start based on ticks-from-now.")
  (:method (sim (actors list) &optional ticks-from-now)
	   (dolist (actor actors)
	     (activate sim actor ticks-from-now))))

(defgeneric simulation-step (thing)
  (:documentation "runs this things simulation step"))

(defgeneric amount (thing))
(defgeneric full-p (thing))
(defgeneric can-accept-p (thing value))
(defgeneric empty-p (thing))
(defgeneric can-provide-p (thing value))
(defgeneric adjust (thing direction &optional value))
