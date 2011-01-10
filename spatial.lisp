(in-package #:rpd-simulation)

(defclass spatial ()
  ((location :accessor location :initarg :location))
  (:documentation "indicates the actor is spatially-aware"))

(defmethod print-object ((self spatial) stream)
	   (print-unreadable-object (self stream :type t :identity t)
	     (format stream "(~a,~a)" (x (location self))
		     (y (location self))
		     )))

(defclass spatial-simulation (simulation)
  ((board :accessor board :initarg :board))
  (:documentation "handles space"))

(defmethod activate :after ((sim spatial-simulation) (actor spatial)
			    &optional ticks-from-now)
	   (declare (ignore ticks-from-now))
	   (setf (board-elt sim (location actor)) actor))

(defun make-bounding-box (location size
		   &aux (x (x location))
		   (y (y location)))
  (rectangles:make-rectangle
   :lows (list x y) 
   :highs (list (+ x size) (+ y size))))

(defgeneric bounding-box (thing)
  (:method ((self spatial)) (bounding-box (location self)))
  (:method ((self cons)) (make-bounding-box self 1)))

(defclass board ()
  ((index :accessor index
	  :initform (spatial-trees:make-spatial-tree :r :rectfun #'bounding-box))
   (bounds :accessor bounds :initarg :bounds)))

(defun make-board (rows columns &optional use-array-p)
  (if use-array-p
      (make-array (list rows columns) :initial-element nil)
      (make-instance 'board :bounds (make-location rows columns))))

(defgeneric board-elt (thing location)
  (:documentation "returns the element on the board in that space")
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
	   (spatial-trees:insert value (index self))))

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

(defgeneric look (thing &key range predicate)
  (:documentation "searches nearby space")
  (:method ((self spatial) &key (range 1) predicate)
	   ;; find neighbors
	   (let ((not-me (lambda (match) (not (eq self match)))))
	     (board-search (simulation self)
			   (make-bounding-box (location+ (location self)
							 (- range))
					      (+ range range))
			   (if predicate
			       (lambda (match)
				 (and (funcall not-me match)
				      (funcall predicate match)))
			       not-me)))))


;;;
;;; locations in space
(defun make-location (x y)
;  (make-instance 'cl-geometry:point :x x :y y  )
  (cons x y))
(declaim (inline make-location))

(defun location+ (location n)  
  (make-location (+ (x location) n)
		 (+ (y location) n)))

(defgeneric x (thing)
  (:method ((s spatial)) (x (location s)))
  (:method ((c cons)) (car c)))

(defgeneric set-x (thing value)
  (:method ((s spatial) value)
	   (set-x (location s) value))
  (:method ((c cons) value) (setf (car c) value)))
(defsetf x set-x)

(defgeneric y (thing)
  (:method ((s spatial)) (y (location s)))
  (:method ((c cons)) (cdr c)))

(defgeneric set-y (thing value)
  (:method ((s spatial) value) (set-y (location s) value))
  (:method ((c cons) value) (setf (cdr c) value)))
(defsetf y set-y)