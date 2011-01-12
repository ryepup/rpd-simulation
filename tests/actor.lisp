(in-package :rpd-simulation-tests)

(defactor counter ()
  ((n :accessor n :initform 0))
  (:function self
	     (incf (n self))))

(defactor count-every-n ()
  ((n :accessor n :initform 0)
   (delay :accessor delay :initform 5 :initarg :delay))
  (:action self
	   (iter	     
	     (incf (n self))
	     (yield (delay self)))))

(define-test activate-actor
  (let* ((sim (make-simulation))
	 (cnt (make-instance 'counter)))
    (activate sim cnt)
    (assert-eq sim (simulation cnt))))

(define-test simple-step/actor-fn
  (let* ((cnt (make-instance 'counter)))
    (assert-eql 0 (n cnt))
    (simulation-step cnt)
    (assert-eql 1 (n cnt))
    (simulation-step cnt)
    (assert-eql 2 (n cnt))))

(define-test simple-step/actor-routine
  (let* ((cnt (make-instance 'count-every-n
			     :delay 5)))
    (assert-eql 0 (n cnt))
    (simulation-step cnt)
    (assert-eql 1 (n cnt))
    (simulation-step cnt)    
    (assert-eql 2 (n cnt))))