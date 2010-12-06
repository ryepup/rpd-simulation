(in-package #:rpd-simulation)

(defclass spatial ()
  ((location :accessor location :initarg :location)
   (sight-range :accessor sight-range
		:initarg :sight-range
		:initform 1)
   (size :accessor size :initarg :size :initform 1)))

(defun %rectangle (location size)
  (rectangles:make-rectangle
   :lows (iter (for c in-vector location)
	       (collect (- c size)))
   :highs (iter (for c in-vector location)
		(collect (+ c size)))))

(defmethod rectangle ((obj spatial))
	   (%rectangle (location obj) (size obj)))

(defclass spatial-simulation (simulation)
  ((board :accessor board)))

(defgeneric rectangle (thing))
(defclass spatial2d-simulation (spatial-simulation)
  ((spatial-tree
    :accessor spatial-tree
    :initform (spatial-trees:make-spatial-tree
	       :r :rectfun #'rectangle))))


(defgeneric look-around (thing &optional predicate)
  (:documentation "returns all the spots the thing can see that match the predicate")
  (:method ((spatial spatial) &optional (predicate #'identity))
	   (remove-if-not predicate
			  (find-near *simulation*
				     (location spatial)
				     (sight-range spatial)))))

(defgeneric find-near (sim location range)
  (:method ((sim spatial2d-simulation) location range)
	   (spatial-trees:search (%rectangle location range)
				 (spatial-tree sim)))
  (:method ((sim spatial-simulation) location range)
	   (let ((x (first location))
		 (y (second location))
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


(defmethod %activate :after ((sim spatial-simulation) (p spatial) at)
	   (declare (ignore at))
	   (set-cell (first (location p))
		     (second (location p))
		     p sim))

(defmethod %activate :after ((sim spatial2d-simulation) (p spatial) at)
	   (declare (ignore at))
	   (spatial-trees:insert p (spatial-tree sim)))

(defun create-board (dimensions &optional (sim *simulation*))
  (setf (board sim) (make-array dimensions)))
(defun board-dimensions (&optional (sim *simulation*))
  (array-dimensions (board sim)))
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
  (list x y))