(in-package #:rpd-simulation-impl)

(defclass spatial ()
  ((location :accessor location :initarg :location)
   (sight-range :accessor sight-range
		:initarg :sight-range
		:initform 1)
   (size :accessor size :initarg :size :initform 1)))

(defun %rectangle (location size
		   &aux (x (x location))
		   (y (y location)))
  (rectangles:make-rectangle
   :lows (list (- x size) (- y size)) 
   :highs (list (+ x size) (+ y size))))

(defmethod rectangle ((obj spatial))
	   (%rectangle (location obj) (size obj)))

(defclass spatial-simulation (simulation)
  ((board :accessor board)))

(defmethod done :after ((sim spatial-simulation) process)
	   (set-cell (x (location process))
		     (y (location process))
		     nil sim))

(defgeneric rectangle (thing))
(defclass spatial2d-simulation (spatial-simulation)
  ((spatial-tree
    :accessor spatial-tree
    :initform (spatial-trees:make-spatial-tree
	       :r :rectfun #'rectangle))))

(defgeneric nearby-spots (thing)
  (:method ((s spatial))
	   (let* ((location (location s))
		  (range (sight-range s))
		  (x (x location))
		  (y (y location))
		  (dim (board-dimensions *simulation*))
		  (offsets (loop for i from (* -1 range) to range
				 collect i)))
	     (loop for x-offset in offsets
		   nconc
		   (iter (for y-offset in offsets)
			 (unless (and (zerop x-offset)
				      (zerop y-offset)))
			 (for nx = (mod (+ x x-offset)
				       (first dim)))
			 (for ny = (mod (+ y y-offset)
				       (second dim)))

			 (collect (cons (make-location nx ny)
					(get-cell nx ny))))))))
(defgeneric process (thing)
  (:method ((c cons)) (cdr c)))
(defmethod location ((c cons)) (car c))

(defgeneric look-around (thing &optional predicate)
  (:documentation "returns all the spots the thing can see that match the predicate")
  (:method ((spatial spatial) &optional predicate)
	   (if predicate
	       (remove-if-not predicate (look-around spatial))
	       (find-near *simulation*
			  (location spatial)
			  (sight-range spatial)))))

(defgeneric find-near (sim location range)
  (:method ((sim spatial2d-simulation) location range)
	   (spatial-trees:search (%rectangle location range)
				 (spatial-tree sim)))
  (:method ((sim spatial-simulation) location range)
	   (let ((x (x location))
		 (y (y location))
		 (dim (board-dimensions sim))
		 (offsets (loop for i from (* -1 range) to range
				collect i)))
	     (loop for x-offset in offsets
		   nconc
		   (loop for y-offset in offsets
			 unless (and (zerop x-offset)
				     (zerop y-offset))
			   collect (get-cell
				    (mod (+ x x-offset)
					 (first dim))
				    (mod (+ y y-offset)
					 (second dim))))))))

(defgeneric empty-p (thing))

(defclass mobile (spatial)
  ((velocity :accessor velocity
	     :initarg :velocity
	     :initform nil)
   (max-speed :accessor max-speed
	      :initarg :max-speed
	      :initform nil)))


(defmethod activate :after ((sim spatial-simulation) (p spatial) at)
	   (declare (ignore at))
	   (set-cell (x (location p))
		     (y (location p))
		     p sim))

(defmethod activate :after ((sim spatial2d-simulation) (p spatial) at)
	   (declare (ignore at))
	   (spatial-trees:insert p (spatial-tree sim)))

(defun create-board (dimensions &optional (sim *simulation*))
  (setf (board sim) (make-array dimensions :initial-element nil)))

(defun board-dimensions (&optional (sim *simulation*))
  (unless (get 'board-dimensions sim)
    (setf (get 'board-dimensions sim)
	  (array-dimensions (board sim))))
  (get 'board-dimensions sim))
  
(defun set-cell (x y process &optional (sim *simulation*))
  (setf (aref (board sim) x y) process))
(defun get-cell (x y &optional (sim *simulation*))
  (aref (board sim) x y))

(defmacro with-spatial-simulation ((&key (board nil board-supplied-p)) &body body)
  `(let ((*simulation* (make-instance 'spatial-simulation)))
     ,(when board-supplied-p
	`(create-board ,board))
     ,@body))

(defmacro do-board ((location-var) &rest body)
  (with-unique-names (x y dim)
    `(let ((,dim (board-dimensions)))
       (dotimes (,x (first ,dim))
	 (dotimes (,y (second ,dim))
	   (let ((,location-var (make-location ,x ,y)))
	     ,@body))))))

(defun make-location (x y)
;  (make-instance 'cl-geometry:point :x x :y y  )
  (cons x y)
  )

(defgeneric x (thing)
  (:method ((p cl-geometry:point)) (cl-geometry:x p))
  (:method ((s spatial)) (x (location s)))
  (:method ((c cons)) (car c)))

(defgeneric y (thing)
  (:method ((p cl-geometry:point)) (cl-geometry:y p))
  (:method ((s spatial)) (y (location s)))
  (:method ((c cons)) (cdr c)))
