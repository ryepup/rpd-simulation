;;;; package.lisp

(defpackage #:rpd-simulation
  (:use #:cl #:iterate)
  (:shadowing-import-from #:rpd-coroutines
			  #:make-coroutine
			  #:yield)
  (:shadowing-import-from #:alexandria
			  #:when-let
			  #:with-unique-names
			  #:ensure-list)
  (:export #:activate
	   #:simulate
	   #:note
	   #:self
	   #:defprocess
	   #:yield
	   #:spatial
	   #:look-around
	   #:empty-p
	   #:location
	   #:with-simulation
	   #:do-board
	   #:with-spatial-simulation))

