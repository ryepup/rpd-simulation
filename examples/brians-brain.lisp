(in-package :rpd-simulation-examples)

(defvar *brain-cells* (make-hash-table))

(defactor brian-braincell (spatial)
  ((state :accessor state :initarg :state))
  (:function self
   (let ((state (state self)))
     (setf (state self)
	   (cond ((eq :on state) :dying)
		 ((eq :dying state) :off)
		 ((eq 2 (length (look
				 self
				 :predicate (lambda (c) (eq :on (state c)))))) :on)
		 (T :off)))
     (incf (gethash (state self) *brain-cells* 0)))))

(defmethod print-object ((self brian-braincell) stream)
	   (print-unreadable-object (self stream :type t :identity t)
	     (format stream "~a" (state self))))

(defun make-brians-brain ()
  (let ((sim (make-simulation :board (make-board 90 90))))
    (do-board (sim location)
      (activate sim
		(make-instance 'brian-braincell
			       :location location
			       :state (random-elt '(:on :off)))))
    sim))

(defun brains-brain ()
  (let ((*brain-cells* (make-hash-table))
	(sim (make-brians-brain)))
    (simulate sim
	      :stop-if
	      (lambda ()
		(prog1
		    (zerop (+ (gethash :on *brain-cells* 0)
			      (gethash :dying *brain-cells* 0)))
		  (clrhash *brain-cells*))))
    sim))

(defun brains-brain-sdl ()
  (sdl:with-init ()
    (sdl:window 540 540)
    (setf (sdl:frame-rate) 0) 
    (flet ((sdl-cell (color)
	     (let ((surface (sdl:create-surface 6 6)))
	       (sdl:fill-surface color :surface surface)
	       surface)))	
      (let ((dying-surf (sdl-cell sdl:*red*))
	    (alive-surf (sdl-cell sdl:*white*))
	    (sim (make-brians-brain)))
	(sdl:with-events ()
	  (:quit-event () t)
	  (:video-expose-event () (sdl:update-display))
	  (:idle () 
		 (simulation-step sim)
		 (sdl:clear-display sdl:*black*)
		 (dolist (cell (actors sim))
		   (unless (eq (state cell) :off)
		     (let ((y (y cell))
			   (x (x cell))
			   (surf (if (eq (state cell) :dying)
				     dying-surf alive-surf)))
		       (sdl:set-point-* surf
					:x (1+ (* 6 x))
					:y (1+ (* 6 y)))
		       (sdl:blit-surface surf))))
		 (sdl:update-display)
		 ))
	(sdl:free dying-surf)
	(sdl:free alive-surf)))))

