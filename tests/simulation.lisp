(in-package :rpd-simulation-tests)

(define-test schedule-items
  (let ((item (rpd-simulation::make-schedule-item 1 2)))
    (assert-eql 1 (rpd-simulation::scheduled-actor item))
    (assert-eql 2 (rpd-simulation::scheduled-time item))))

(define-test simple-scheduling
  (let* ((sim (make-simulation))
	 (next (activate sim (make-instance 'counter)))
	 (future (activate sim (make-instance 'counter) 2)))
    (assert-eql 1 (rpd-simulation::scheduled-time next))
    (assert-eql 2 (rpd-simulation::scheduled-time future))))

(define-test next-actor
  (let* ((sim (make-simulation))
	 (cnt (make-instance 'counter))
	 (dcnt (make-instance 'counter)))
    (activate sim cnt)
    (activate sim dcnt 5)
    (assert-false (rpd-simulation::next-actor sim))
    (setf (rpd-simulation::current-time sim) 1)
    (assert-eq cnt (rpd-simulation::next-actor sim))
    (assert-false (rpd-simulation::next-actor sim))
    (setf (rpd-simulation::current-time sim) 5)
    (assert-eq dcnt (rpd-simulation::next-actor sim) "dcnt" )))

(define-test simple-step/simulation
  (let* ((sim (make-simulation))
	 (cnt (make-instance 'counter))
	 (dcnt (make-instance 'count-every-n :delay 5)))
    (assert-eql 0 (n cnt))
    (assert-eql 0 (n dcnt))
    (activate sim cnt)
    (activate sim dcnt)
    (simulation-step sim)
    (assert-eql 1 (n cnt))
    (assert-eql 1 (n dcnt))
    (simulation-step sim)
    (assert-eql 2 (n cnt))
    (assert-eql 1 (n dcnt))
    (dotimes (n 4) (simulation-step sim))
    (assert-eql 6 (n cnt))
    (assert-eql 2 (n dcnt))))

(define-test simulation/until
  (let* ((sim (make-simulation))
	 (cnt (make-instance 'counter)))
    (assert-eql 0 (n cnt))
    (activate sim cnt)
    (simulate sim :until 5)
    (assert-eql 5 (n cnt))))

(define-test simulation/stop-if
  (let* ((sim (make-simulation))
	 (cnt (make-instance 'counter)))
    (assert-eql 0 (n cnt))
    (activate sim cnt)
    (simulate sim :stop-if (lambda () (eq (n cnt) 4)))
    (assert-eql 4 (rpd-simulation::current-time sim))))

