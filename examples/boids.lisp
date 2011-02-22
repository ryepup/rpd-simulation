(in-package :rpd-simulation-examples)

;; port of http://harry.me/2011/02/17/neat-algorithms---flocking/

(defactor boid (spatial)
  ((velocity :accessor velocity :initarg :velocity :initform nil)
   (max-speed :reader max-speed :initform 2)
   (max-force :reader max-force :initform 0.1)
   (nearby-threshold :reader nearby-threshold :initform 200)
   (cohere/distance-freakout :reader cohere/distance-freakout :initform 50)
   (separate/desired :reader separate/desired :initform 25)
   (max-coords :reader max-coords :initarg :max-coords))
  (:documentation "a flocking boid")
  (:function self
	     (setf (velocity self) (limit (max-speed self)
					  (location+ (velocity self) (flock self)))
		   (location self) (location+ (velocity self) (location self)))
	     ;;wrap if needed
	     ))


(defmethod to-sdl-point ((self boid))	   
	   (sdl:point :x (x self) :y (y self)))
(defmethod to-sdl-point ((self location))	   
	   (sdl:point :x (x self) :y (y self)))

(defmethod flock ((self boid))
	   "returns a location representing the flocking acceleration"
	   ;;look for boids within range
	   (let ((neighbors (look self :range (nearby-threshold self))))
	     (location+ (edges self)
			(list
			 (scale (separate self (look self
						     :range (separate/desired self)))
				1)
			 (scale (align self neighbors) .5)
			 (scale (cohere self neighbors) .5)))))

(defmethod edges ((self boid) &aux (relevant-edges (list)) (dst (* 2 (separate/desired self))))
	   (flet ((process-edge (edge-location)
		    (let* ((edge->self (location- (location self)
						  edge-location))
			   (distance (magnitude edge->self)))
		      (when (< distance dst)
			(push 
			 (scale (limit 1 edge->self)
				(/ (separate/desired self) (* distance distance)))
			 relevant-edges)))))
	     (process-edge (make-location (x self) 0))
	     (process-edge (make-location 0 (y self)))
	     (process-edge (make-location (x self) (y (max-coords self))))
	     (process-edge (make-location (x (max-coords self)) (y self)))

	     (if (plusp (length relevant-edges))
		 (scale (location+ (make-location 0 0) relevant-edges)
		      (/ 1 (length relevant-edges)))
		 (make-location 0 0))))

(defmethod separate ((self boid) neighbors)
	   (if (plusp (length neighbors))
	       (scale (location+ (make-location 0 0)
				 (iter (for boid in neighbors)
				       (for me->boid = (location- (location self)
								  (location boid)))
				       (for distance = (magnitude me->boid))
				       (collect (scale (limit 1 me->boid)
						       (if (zerop distance)
							   1
							   (/ 1 distance))
						       ))))
		      (/ 1 (length neighbors)))
	       (make-location 0 0)))

(defmethod align ((self boid) neighbors)
	   (if (plusp (length neighbors))
	       (limit (max-force self)
		      (scale (location+ (make-location 0 0)
					(mapcar #'velocity neighbors))
			     (/ 1 (length neighbors))))
	       (make-location 0 0)))

(defmethod cohere ((self boid) neighbors)
	   (or
	    (when (plusp (length neighbors))
	      (let* ((sum (location+ (make-location 0 0)
				     (mapcar #'location neighbors)))
		     (avg (scale sum (/ 1 (length neighbors))))
		     (self->avg (location- avg (location self)))
		     (distance (magnitude self->avg)))		
		(when (plusp distance)
		  (limit (max-force self)
			 (location-
			  (scale (limit 1 self->avg)
				 (if (> distance (cohere/distance-freakout self))
				     (max-speed self)
				     (/ distance (cohere/distance-freakout self))))
			  (velocity self))))))
	    (make-location 0 0)))


(defun boids (&key (n 100) (w 600) (h 480))
  (let ((sim (make-simulation :board (make-board w h)))
	(start-loc (make-location (/ w 2) (/ h 2)))
	(max-coords (make-location w h)))
    (dotimes (_ n)
      (activate sim (make-instance 'boid :location start-loc
				   :max-coords max-coords
				   :velocity (make-location (- (* 2 (random 1.0)) 1)
							    (- (* 2 (random 1.0)) 1))))
      )
    sim))

(defmethod simulation-step :after ((b boid))
	   (sdl:draw-filled-circle (to-sdl-point b) 5 :color sdl:*red*)
	   (let ((next (location+ (location b) (scale (velocity b) 5))))
	     (sdl:draw-line (to-sdl-point b) (to-sdl-point next)
			    :color sdl:*green*)))

(defun boids-sdl (&key (n 100) (w 600) (h 480))
  (sdl:with-init ()
    (sdl:window w h)
    (setf (sdl:frame-rate) 0)
    (let ((sim (boids :n n :w w :h h)))
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:idle ()
	       (sdl:clear-display sdl:*black*)
	       (simulation-step sim)
	       (sdl:update-display))
	)

      )
    )
  )
