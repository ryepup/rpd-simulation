(in-package :rpd-simulation-examples)

;; slightly more complicated example with one process creating another

(defactor beehive ()
  ((honey :initarg :honey :accessor honey))
  (:documentation "a beehive that makes bees")
  (:action self
   (iterate
     (while (plusp (honey self)))
     (if (< 5 (honey self))
	 (progn
	   (yield 10)
	   (activate self (make-instance 'bee :hive self))
	   (decf (honey self) 5)
	   (log-message :info "made a bee"))
	 (yield 1)))
   (log-message :info "Hive dead")))

(defactor bee ()
  ((hive :initarg :hive :accessor hive))
  (:documentation "a bee that brings honey to it's hive")
  (:action self
   (dotimes (n 3)
     (yield (+ 5 (random 5)))
     (incf (honey (hive self)) 5)
     (log-message :info "Returned some honey"))
   (log-message :info "bee dying")))

(defun bees ()
  (let ((sim (make-simulation))
	(hive (make-instance 'beehive :honey 10)))
    (activate sim hive)
    (simulate sim :until 200)
    (log-message :info "Honey at the end: ~a" (honey hive))))
