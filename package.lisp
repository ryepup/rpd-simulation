;;;; package.lisp

(defpackage #:rpd-simulation
  (:use #:cl #:iterate)
  (:shadowing-import-from #:rpd-coroutines
			  #:make-coroutine
			  #:yield)
  (:shadowing-import-from #:alexandria
			  #:when-let
			  #:ensure-list)
  (:export #:activate
	   #:simulate
	   #:note
	   #:self
	   #:defprocess
	   #:yield
	   #:with-simulation))

