(in-package :rpd-simulation-examples)

(defactor plant (spatial)
  ((solar-energy :accessor solar-energy :initform 1))
  (:documentation "a plant")
  (:action self
	   (iterate
	     (while (alive-p self))
	     ;; get power from the sun
	     (incf (solar-energy self))
	     (when (can-grow-p self)
	       (let* ((nearby (look self))
		      (neighbors (neighbors (location self)))
		      (candidates (remove-if
				   (lambda (loc)
				     (member loc nearby
					     :key #'location
					     :test #'location=))
				   neighbors )))
		 (when-let ((candidates (and (> 6 (length nearby))
					     candidates)))
		   (decf (solar-energy self) 8)
		   (activate (simulation self)
			     (make-instance (class-name (class-of self))
					    :location (random-elt candidates)))
		   (yield 20))))
	     (yield 5))))

(defun alive-p (p)
  (and (plusp (solar-energy p))
       (< (solar-energy p) 100)))

(defmethod can-grow-p ((self plant))
	   (> (solar-energy self) 10))

(defclass sdl-plant (plant) ())

(defun make-ecology (&optional for-sdl-p)
  (let ((sim (make-simulation
	      :board (make-board 90 90))))
    (activate
     sim (make-instance (if for-sdl-p 'sdl-plant 'plant)
			:location (make-location 40 60 )))
    sim))


(defun ecology ()
  (simulate (make-ecology) :until 1000))

(defvar *color-by-number* )
(defvar *plant-surfaces* )

(defun plant-color (p)
  (cond ((not (alive-p p)) sdl:*black*)
	((not (can-grow-p p)) sdl:*yellow*)
	((< (solar-energy p) 20) sdl:*green*)
	(T (let ((g (- 255 (* 2 (solar-energy p)))))
	     (or (aref *color-by-number* g)
		 (setf (aref *color-by-number* g)
		       (sdl:color :g g)))))))

(defun sdl-cell (color)
  (let ((surface (sdl:create-surface 6 6)))
    (sdl:fill-surface color :surface surface)
    surface))

(defun plant-surf-from-color (c)  
  (or
   (gethash c *plant-surfaces*)
   (setf (gethash c *plant-surfaces*) (sdl-cell c))))

(defmethod simulation-step :after ((p sdl-plant))
	   (let ((y (y p))
		 (x (x p))
		 (surf (plant-surf-from-color (plant-color p))))
	     (unless (eql surf (get 'last-surf p))
	       (setf (get 'last-surf p) surf)
	       (sdl:set-point-* surf
				:x (1+ (* 6 x))
				:y (1+ (* 6 y)))
	       (sdl:blit-surface surf))))

(defun ecology-sdl ()
  (sdl:with-init ()
    (sdl:window 540 540)
    (setf (sdl:frame-rate) 0) 
    (let ((sim (make-ecology T))
	  (*color-by-number* (make-array 255 :initial-element nil))
	  (*plant-surfaces* (make-hash-table)))
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:idle () 
	       (simulation-step sim)
	       (sdl:update-display)
	       ))
      (maphash-values #'sdl:free *plant-surfaces*))))
