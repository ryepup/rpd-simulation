(in-package :rpd-simulation-examples)

(defprocess brian-braincell (spatial)
  ((state :accessor state :initarg :state))
  (:function
   (let ((state (state self)))
     (setf (state self)
	   (cond ((eq :on state) :dying)
		 ((eq :dying state) :off)
		 ((eq 2 (length (look-around
				 self (lambda (c) (eq :on (state c)))))) :on)
		 (T :off)))
     :hold)))

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

(defun brains-brain-sdl ()
  (sdl:with-init ()
    (sdl:window 540 540)
    (setf (sdl:frame-rate) 0)
    
    (flet ((sdl-cell (color)
	     (let ((surface (sdl:create-surface 6 6)))
	       (sdl:fill-surface color :surface surface)
	       surface)))
      (with-spatial-simulation (:board '(90 90))
	(do-board (location)
	  (activate (make-instance 'brian-braincell
				   :location location
				   :state (random-elt '(:on :off)))))
	(let ((dying-surf (sdl-cell sdl:*red*) )
	      (alive-surf (sdl-cell sdl:*white*)))
	  (flet ((render ()
		   (dolist (p (rpd-simulation::processes
			       rpd-simulation::*simulation*))
		     (unless (eq (state p) :off)
		     (destructuring-bind (x y)
			 (rpd-simulation::location p)
		       (let ((surf (if (eq (state p) :dying)
				       dying-surf alive-surf)))
		
			 (sdl:set-point-* surf
					  :x (1+ (* 6 x))
					  :y (1+ (* 6 y)))
			 (sdl:blit-surface surf)
			 ))))
		   (sdl:update-display)))
	    (sdl:clear-display sdl:*black*)
	    (render)
	    (sdl:update-display)
	    (sdl:with-events ()
	      (:quit-event () t)
	      (:video-expose-event () (sdl:update-display))
	      (:idle ()
		     (rpd-simulation::simulate-step rpd-simulation::*simulation*)
		     (sdl:clear-display sdl:*black*)
		     (render)
		     (sdl:update-display)
		     )))
	  (sdl:free dying-surf)
	  (sdl:free alive-surf)
	  

	  
	  )))))