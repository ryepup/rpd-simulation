(in-package :rpd-simulation-tests)

(rpd-simulation:defactor box (rpd-simulation::spatial)
  ()
  (:function self 1))

(define-test board/adding
  (let ((sim (rpd-simulation:make-simulation
	      :board (rpd-simulation::make-board 90 90)))
	(box (make-instance 'box :location (rpd-simulation::make-location 30 30))))
    (rpd-simulation:activate sim box)
    (assert-eq box (first (rpd-simulation::board-elt
			   sim (rpd-simulation::location box))))))

(define-test board/do-board
  (let ((b (rpd-simulation::make-board 90 90))
	(counter 0))
    (rpd-simulation::do-board (b loc)
      (declare (ignore loc))
      (incf counter))
    (assert-eq (* 90 90) counter)))

(define-test board/look
  (let ((sim (rpd-simulation:make-simulation
	      :board (rpd-simulation::make-board 90 90)))
	(box (make-instance 'box :location (rpd-simulation::make-location 30 30)))
	(box2 (make-instance 'box :location (rpd-simulation::make-location 31 30)))
	(box3 (make-instance 'box :location (rpd-simulation::make-location 32 30))))
    (rpd-simulation:activate sim (list box box2 box3))
    (assert-eq 1 (length (rpd-simulation::look box)) "a")
    (assert-eq box2 (first (rpd-simulation::look box)) "a")
    (assert-eq box2 (first (rpd-simulation::look box3)) "b")
    (assert-eq 2 (length (rpd-simulation::look box2)))
    (assert-eq 2 (length (rpd-simulation::look box :range 2)))    
    (assert-eq box3 (first (rpd-simulation::look box :range 2
						 :predicate (lambda (match)
							      (eq box3 match)))))
    (rpd-simulation::simulation-step sim)
    ;;still the same
    (assert-eq 1 (length (rpd-simulation::look box)) "a")
    (assert-eq box2 (first (rpd-simulation::look box)) "a")
    (assert-eq box2 (first (rpd-simulation::look box3)) "b")
    (assert-eq 2 (length (rpd-simulation::look box2)))
    (assert-eq 2 (length (rpd-simulation::look box :range 2)))    
    (assert-eq box3 (first (rpd-simulation::look box :range 2
						 :predicate (lambda (match)
							      (eq box3 match)))))
    
    ))