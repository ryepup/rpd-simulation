(in-package :rpd-simulation-tests)

(defactor box (spatial)
  ()
  (:function self 1))

(define-test board/adding
  (let ((sim (make-simulation
	      :board (make-board 90 90)))
	(box (make-instance 'box :location (make-location 30 30))))
    (activate sim box)
    (assert-eq box (first (board-elt
			   sim (location box))))))

(define-test board/do-board
  (let ((b (make-board 90 90))
	(counter 0))
    (do-board (b loc)
      (declare (ignore loc))
      (incf counter))
    (assert-eq (* 90 90) counter)))

(define-test board/look
  (let ((sim (make-simulation
	      :board (make-board 90 90)))
	(box (make-instance 'box :location (make-location 30 30)))
	(box2 (make-instance 'box :location (make-location 31 30)))
	(box3 (make-instance 'box :location (make-location 32 30))))
    (activate sim (list box box2 box3))
    (assert-eq 1 (length (look box)) "a")
    (assert-eq box2 (first (look box)) "a")
    (assert-eq box2 (first (look box3)) "b")
    (assert-eq 2 (length (look box2)))
    (assert-eq 2 (length (look box :range 2)))    
    (assert-eq box3 (first (look box :range 2
						 :predicate (lambda (match)
							      (eq box3 match)))))
    (simulation-step sim)
    ;;still the same
    (assert-eq 1 (length (look box)) "a")
    (assert-eq box2 (first (look box)) "a")
    (assert-eq box2 (first (look box3)) "b")
    (assert-eq 2 (length (look box2)))
    (assert-eq 2 (length (look box :range 2)))    
    (assert-eq box3 (first (look box :range 2
						 :predicate (lambda (match)
							      (eq box3 match)))))
    
    ))



(define-test location-basics
  (let ((l (make-location 3 4)) m)
    (assert-float-equal 5 (magnitude l))
    (setf m (limit 5 l))
    (assert-true (location= m l))
    (assert-float-equal 5 (magnitude m))
    (setf m (limit 3 l))
    (assert-float-equal 3 (magnitude m) m)
    (assert-true (location= (make-location 3 3)
			    (location+ (make-location 1 1) 2)))
    (assert-true (location= (make-location 3 3)
			    (location+ (make-location 1 1)
				       (list (make-location 1 1)
					     (make-location 1 1)))))
    
    )
  )