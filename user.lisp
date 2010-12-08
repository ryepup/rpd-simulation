(in-package :rpd-simulation)

(defun schedule (process &optional (at 1))
  (rpd-sim-impl:schedule *simulation*
			 process at))

(defun activate (process &key (at 1))
  (rpd-sim-impl:activate *simulation* process at)
  process)

(defun simulate (&key until stop-if for)
  (rpd-sim-impl:simulate *simulation*
			 :until until
			 :stop-if stop-if
			 :for for))

(defun note (format &rest args)
  (rpd-sim-impl:note *simulation* format args))

(defmacro do-processes ((process-var &optional result) &body body)
  `(dolist (,process-var
	     (rpd-sim-impl::processes *simulation*)
	    ,result)
     ,@body))
