(in-package :rpd-simulation-tests)

(rpd-simulation:defactor counter ()
  ((n :accessor n :initform 0))
  (:function self
	     (incf (n self))))

(rpd-simulation:defactor count-every-n ()
  ((n :accessor n :initform 0)
   (delay :accessor delay :initform 5 :initarg :delay))
  (:action self
	   (iter	     
	     (incf (n self))
	     (rpd-simulation:yield (delay self)))))

(define-test activate-actor
  (let* ((sim (rpd-simulation:make-simulation))
	 (cnt (make-instance 'counter)))
    (rpd-simulation:activate sim cnt)
    (assert-eq sim (rpd-simulation::simulation cnt))))

(define-test simple-step/actor-fn
  (let* ((cnt (make-instance 'counter)))
    (assert-eql 0 (n cnt))
    (rpd-simulation::simulation-step cnt)
    (assert-eql 1 (n cnt))
    (rpd-simulation::simulation-step cnt)
    (assert-eql 2 (n cnt))))

(define-test simple-step/actor-routine
  (let* ((cnt (make-instance 'count-every-n
			     :delay 5)))
    (assert-eql 0 (n cnt))
    (rpd-simulation::simulation-step cnt)
    (assert-eql 1 (n cnt))
    (rpd-simulation::simulation-step cnt)    
    (assert-eql 2 (n cnt))))