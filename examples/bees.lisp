(in-package :rpd-simulation-examples)

;; slightly more complicated example with one process creating another

(defprocess beehive ()
  ((honey :initarg :honey :accessor honey))
  (:documentation "a beehive that makes bees")
  (:action
   (iterate
     (while (plusp (honey self)))
     (if (< 5 (honey self))
	 (progn
	   (yield :hold 10)
	   (activate (make-instance 'bee :hive self))
	   (decf (honey self) 5)
	   (note "made a bee"))
	 (yield :hold)))
   (note "Hive dead")))

(defprocess bee ()
  ((hive :initarg :hive :accessor hive))
  (:documentation "a bee that brings honey to it's hive")
  (:action
   (dotimes (n 3)
     (yield :hold (+ 5 (random 5)))
     (incf (honey (hive self)) 5)
     (note "Returned some honey"))
   (note "bee dying")))

(defun bees ()
  (with-simulation ()
    (let ((hive (make-instance 'beehive :honey 10)))
    (activate hive)
    (simulate :until 200)
    (note "Honey at the end: ~a" (honey hive)))))
