(in-package :rpd-simulation-tests)

(defactor deliverer ()
    ((level :accessor level :initarg :level))
  (:action self
	   (iter
	     (yield :incf (level self)))))

(define-test levels/simple
  (let* ((lvl (make-level))
	 (sim (make-simulation))
	 (dv (make-instance 'deliverer :level lvl)))
    (activate sim dv)
    (assert-eq 0 (amount lvl))
    (simulation-step sim)
    (assert-eq 1 (amount lvl))
    (simulation-step sim)
    (assert-eq 2 (amount lvl))
    (setf (level dv) (make-level :initial-amount 10))
    (assert-eq 10 (amount (level dv)))
    (simulation-step sim)
    (assert-eq 11 (amount (level dv)))
    ))

(define-test levels/capacity
  (let* ((lvl (make-level :capacity 2))
	 (sim (make-simulation))
	 (dv (make-instance 'deliverer :level lvl)))
    (activate sim dv)    
    (assert-eq 2 (capacity lvl))
    (assert-false (full-p lvl))
    (assert-eq 0 (amount lvl))
    (simulation-step sim)
    (assert-eq 1 (amount lvl))
    (assert-false (full-p lvl))
    (simulation-step sim)
    (assert-eq 2 (amount lvl))
    (assert-true (full-p lvl))
    (simulation-step sim)
    (assert-eq 2 (amount lvl))
    (assert-eq 3 (rpd-simulation::lifespan dv))
    (simulation-step sim)
    (assert-eq 2 (amount lvl))
    (assert-eq 3 (rpd-simulation::lifespan dv)))

  )

(defactor deliverer-n ()
    ((level :accessor level :initarg :level)
     (n :accessor n :initarg :n :initform 1))
  (:action self
	   (iter
	     (yield :incf (level self) (n self)))))

(define-test levels/simple-n
  (let* ((lvl (make-level))
	 (sim (make-simulation))
	 (dv (make-instance 'deliverer-n :level lvl :n 5)))
    (activate sim dv)
    (assert-eq 0 (amount lvl))
    (simulation-step sim)
    (assert-eq 5 (amount lvl))
    (simulation-step sim)
    (assert-eq 10 (amount lvl))))

(defactor consumer ()
    ((level :accessor level :initarg :level))
  (:action self
	   (iter
	     (yield :decf (level self)))))

(defactor consumer-n ()
    ((level :accessor level :initarg :level)
     (n :accessor n :initarg :n :initform 1))
  (:action self
	   (iter
	     (yield :decf (level self) (n self)))))


(define-test levels/simple-dec
  (let* ((lvl (make-level :initial-amount 2))
	 (sim (make-simulation))
	 (dv (make-instance 'consumer :level lvl)))
    (activate sim dv)
    (assert-eq 2 (amount lvl))
    (simulation-step sim)
    (assert-eq 1 (amount lvl))
    (simulation-step sim)
    (assert-eq 0 (amount lvl))
    (simulation-step sim)
    (assert-eq 3 (rpd-simulation::lifespan dv))
    (assert-eq 0 (amount lvl))
    (simulation-step sim)
    (assert-eq 3 (rpd-simulation::lifespan dv) "still waiting")))

(define-test levels/simple-dec-n
  (let* ((lvl (make-level :initial-amount 10))
	 (sim (make-simulation))
	 (dv (make-instance 'consumer-n :n 5 :level lvl)))
    (activate sim dv)
    (assert-eq 10 (amount lvl))
    (simulation-step sim)
    (assert-eq 5 (amount lvl))
    (simulation-step sim)
    (assert-eq 0 (amount lvl))))

(define-test levels/deliver-consume
  (let* ((lvl (make-level :initial-amount 1))
	 (sim (make-simulation))
	 (dv (make-instance 'deliverer :level lvl))
	 (cr (make-instance 'consumer-n :n 5 :level lvl)))
    (activate sim (list dv cr))
    (assert-eq 1 (amount lvl))
    (simulation-step sim)
    (assert-eq 2 (amount lvl))
    (simulation-step sim)
    (assert-eq 3 (amount lvl))
    (simulation-step sim)
    (assert-eq 4 (amount lvl))
    (simulation-step sim)
    (assert-eq 0 (amount lvl))
    (simulation-step sim)
    (assert-eq 1 (amount lvl))))