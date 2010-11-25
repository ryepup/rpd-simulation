(in-package :rpd-simulation-examples)

(defprocess brian-braincell (spatial)
  ((state :accessor state :initarg :state))
  (:action
   (iterate
     (for state = (state self))
     (setf (state self)
	   (cond ((eq :on state) :dying)
		 ((eq :dying state) :off)
		 ((eq 2 (length (look-around
				 self (lambda (c) (eq :on (state c)))))) :on)
		 (T :off)))
     (yield :hold))))

(defun brains-brain ()
  (with-spatial-simulation (:board '(90 90))
    (do-board (location)
      (activate (make-instance 'brian-braincell :location location
			       :state (random-elt '(:on :off)))))
    (simulate :until 20
	      :stop-if (lambda (processes)
			 (every (lambda (p)
				  (eq :off (state p)))
				
				processes)))))