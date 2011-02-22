(in-package #:rpd-simulation)

;;; locations in space
(defclass location ()
  ((point :accessor point :initarg :point :initform nil)))

(defmethod print-object ((self location) stream)
	   (print-unreadable-object (self stream :type t :identity t)
	     (format stream "~a" (point self))))


(defmethod x ((self location))
	   (paths:point-x (point self)))
(defmethod (setf x) (val (l location))
	   (setf (point l)
		 (paths:p+ (point l)
			   (paths:make-point val 0))))
(defmethod y ((self location))
	   (paths:point-y (point self)))
(defmethod (setf y) (val (l location))
	   (setf (point l)
		 (paths:p+ (point l)
			   (paths:make-point 0 val))))
(defun make-location (x y)
  (make-instance 'location :point (paths:make-point x y)))

(defgeneric location- (l1 l2)
  (:method ((l1 location) (l2 location))
	   (make-instance 'location
			  :point (paths:p- (point l1) (point l2))
	   )
  ))

(defgeneric location+ (location thing)
  (:method (x (_ null)) x)
  (:method ((_ null) x) x)

  (:method ((l location) (list list))
	   (reduce #'location+ list :initial-value l))
  (:method ((l1 location) (l2 location))
	   (make-location (+ (x l1) (x l2))
			  (+ (y l1) (y l2))))
  (:method ((location location) (n number))
	   (make-location (+ (x location) n)
			  (+ (y location) n))))

(defun location= (loc1 loc2)
  (and (= (x loc1) (x loc2))
       (= (y loc1) (y loc2))))

(defgeneric normalize (thing)
  (:method ((self location)) (limit 1 self)))

(defgeneric scale (thing amount)
  (:method ((_ null) amt) nil)
  (:method ((self location) amount)
	   (make-instance 'location :point (paths:p* (point self) amount))))

(defgeneric limit (limit thing)
  (:method (limit (self location))
	   (let ((mag (magnitude self)))
	     (if (< limit mag)
		 (scale self (/ limit mag))
		 (deep-copy self)))))

(defgeneric deep-copy (thing)
  (:method ((loc location))
	   (make-location (x loc) (y loc))))

(defclass spatial ()
  ((location :accessor location :initarg :location))
  (:documentation "indicates the actor is spatially-aware"))

(defmethod (setf location) :around (new-value (self spatial))
	   (remove-from-world (simulation self) self)
	   (prog1
	       (call-next-method)
	     (add-to-world (simulation self) self)
	     )
	   )

(defgeneric magnitude (thing)
  (:method ((self location)) (paths:point-norm (point self)))
  (:method ((self spatial)) (magnitude (location self))))


(defmethod x ((s spatial)) (x (location s)))
(defmethod (setf x) (val (s spatial))
	   (setf (x (location s))
		 val))

(defmethod y ((s spatial)) (y (location s)))
(defmethod (setf y) (val (s spatial))
	   (setf (y (location s))
		 val))

(defmethod print-object ((self spatial) stream)
	   (print-unreadable-object (self stream :type t :identity t)
	     (format stream "(~a,~a)" (x self) (y self))))

(defclass spatial-simulation (simulation)
  ((board :accessor board :initarg :board))
  (:documentation "handles space"))

(defmethod deactivate :after ((sim spatial-simulation) (actor spatial))
	   (deactivate (board sim) actor))

(defmethod activate :after ((sim spatial-simulation) (actor spatial)
			    &optional ticks-from-now)
	   (declare (ignore ticks-from-now))
	   (when (zerop (lifespan actor))
	     (add-to-world sim actor)))

(defun make-bounding-box (location size
		   &aux (x (x location))
		   (y (y location))) 
  (rectangles:make-rectangle
   :lows (list x y) 
   :highs (list (+ x size) (+ y size))))

(defgeneric bounding-box (thing)
  (:method ((self spatial)) (bounding-box (location self)))
  (:method ((self location)) (make-bounding-box self 1)))

(defclass board ()
  ((index :accessor index :initform (make-index))
   (next-index :accessor next-index :initform (make-index))
   (bounds :accessor bounds :initarg :bounds)))

(defgeneric add-to-world (world thing)
  (:method ((self spatial-simulation) (actor spatial))
	   (add-to-world (board self) actor))
  (:method ((self board) (actor spatial))
	   (spatial-trees:insert actor (index self))))

(defgeneric remove-from-world (world thing)
  (:method ((self spatial-simulation) (actor spatial))
	   (remove-from-world (board self) actor))
  (:method ((self board) (actor spatial))
	   (spatial-trees:delete actor (index self))))

(defmethod deactivate ((self board) (actor spatial))
	   (remove-from-world self actor))

(defun make-index ()
  (spatial-trees:make-spatial-tree :r :rectfun #'bounding-box))

(defun make-board (rows columns &optional use-array-p)
  (if use-array-p
      (make-array (list rows columns) :initial-element nil)
      (make-instance 'board :bounds (make-location rows columns))))

(defgeneric board-elt (thing location)
  (:documentation "returns the element on the board in that space")
  (:method ((self spatial) location)
	   (board-elt (simulation self) location))
  (:method ((self spatial-simulation) location)
	   (board-elt (board self) location))
  (:method ((self array) location)
	   (aref self (x location) (y location)))
  (:method ((self board) location)
	   (board-search self (bounding-box location))))

(defgeneric set-board-elt (thing location value)
  (:documentation "sets element on the board in that space to value")
  (:method ((self spatial-simulation) location value)
	   (set-board-elt (board self) location value))
  (:method ((self array) location value)
	   (setf (aref self (x location) (y location))
		 value))
  (:method ((self board) location (value spatial))
	   (setf (location value) location)
	   (add-to-world self value)))

(defgeneric board-search (thing bounding-box &optional predicate)
  (:documentation "finds things on the board inside the bounding box")
  (:method :around (thing bounding-box &optional predicate)
	   (if predicate
	       (remove-if-not predicate (call-next-method))
	       (call-next-method)))
  (:method ((self spatial-simulation) bounding-box &optional predicate)
	   (board-search (board self) bounding-box predicate))
  (:method ((self board) bounding-box &optional predicate)
	   (declare (ignore predicate))	   
	   (spatial-trees:search bounding-box (index self))))

(defgeneric %do-board (thing fn)
  (:documentation "run fn for every square on the board")
  (:method ((self spatial-simulation) fn)
	   (%do-board (board self) fn))
  (:method ((self board) fn)
	   (with-accessors ((b bounds)) self
	     (dotimes (x (x b))
	       (dotimes (y (y b))
		 (funcall fn (make-location x y)))))))

(defmacro do-board ((thing location-var) &rest body)
  `(%do-board ,thing (lambda (,location-var) ,@body)))

(defgeneric look (thing &key range predicate location)
  (:documentation "searches nearby space")
  (:method ((self spatial) &key (range 1) predicate location)
	   ;; find neighbors
	   (let ((not-me (lambda (match) (not (eq self match)))))
	     (board-search (simulation self)
			   (if location
			       (bounding-box location)
			       (make-bounding-box (location+ (location self)
							     (- range))
						  (+ range range)))
			   (if predicate
			       (lambda (match)
				 (and (funcall not-me match)
				      (funcall predicate match)))
			       not-me)))))

(defun neighbors (location &optional (range 1))
  (let ((offsets (iter (for i from (- range) to range) (collect i)))
	(neighbors (list)))
    (dolist (x offsets neighbors)
      (dolist (y offsets)
	(let ((loc (make-location (+ x (x location))
				  (+ y (y location)))))
	  (when (not (location= loc location))
	    (push loc neighbors)))))))

;;;
