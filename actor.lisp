(in-package #:rpd-simulation)

(defclass actor ()
  ((coroutine :accessor coroutine :initarg :coroutine)
   (simulation :accessor simulation :initform nil)
   (lifespan :accessor lifespan :initform 0)))

(defmethod print-object ((self actor) stream)
	   (print-unreadable-object (self stream :type t :identity t)
	     (format stream "age:~a" (lifespan self))))

(defmethod schedule ((self actor) &optional ticks-from-now)
	   (activate (simulation self) self ticks-from-now))

(defmethod activate ((self actor) (actor actor) &optional ticks-from-now)
	   (activate (simulation self) actor ticks-from-now))

(defmethod simulation-step :before ((self actor))
	   (incf (lifespan self)))

(defmethod simulation-step ((self actor))
	   (funcall (coroutine self)))

(defmacro defactor (name superclasses slots &rest extras)
  (let ((action-body (rest (find :action extras :key #'car)))
	(func-body (rest (find :function extras :key #'car)))
	(extras (remove-if  (lambda (i)
			      (member i '(:action :function)))
			    extras :key #'car)))
    `(progn
       (defclass ,name (actor ,@superclasses)
	 ,slots
	 ,@extras)
       ,(when func-body
	  `(defmethod simulation-step ((,(first func-body) ,name)) ,@(rest func-body)
		      1 ;;indicate we run again next turn
		      ))
       ,(when action-body
	  `(defmethod initialize-instance :after ((p ,name) &key &allow-other-keys)
		      (setf (coroutine p)
			    (let ((,(first action-body) p))
			      (declare (ignorable ,(first action-body)))
			      (make-coroutine ()
				,@(rest action-body)
				))))))))