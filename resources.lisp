(in-package #:rpd-simulation)

(defclass level ()
  ((amount :accessor amount :initarg :amount :initform 0)
   (capacity :accessor capacity :initarg :capacity :initform nil)))

(defmethod print-object ((self level) stream)
	   (print-unreadable-object (self stream :type t :identity t)
	     (format stream "~a/~a" (amount self)
		     (capacity self))))

(defmethod full-p ((self level))
	   (not (can-accept-p self 1)))

(defmethod can-accept-p ((self level) (value number))
	   (let ((cap (capacity self)))
	     (if cap
		 (if (< cap value)
		     (error "Can never accept ~a; cap is ~a ~a" value cap self)
		     (>= cap (+ (amount self) value)))
		 T)))
(defmethod can-provide-p ((self level) (value number))
	   (not (minusp (- (amount self) value))))

(defun make-level (&key capacity initial-amount &aux (level (make-instance 'level)))
  (when capacity
    (setf (capacity level) capacity))
  (when initial-amount
    (setf (amount level) initial-amount))
  level)

(defmethod adjust ((self level) (dir (eql :incf)) &optional (value 1))
	   (when (can-accept-p self value)
	     (incf (amount self) value)))
(defmethod adjust ((self level) (dir (eql :decf)) &optional (value 1))
	   (when (can-provide-p self value)
	     (decf (amount self) value)))

(defactor queuer ()
  ((test :accessor test :initarg :test)
   (success :accessor success :initarg :success))
  (:function self
	     (when (funcall (test self))
		 (progn
		   (funcall (success self))
		   (return :done)))))