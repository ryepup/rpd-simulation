;;;; package.lisp

(defpackage #:rpd-simulation-impl
    (:use #:cl #:iterate)
  (:nicknames #:rpd-sim-impl)
  (:shadowing-import-from #:rpd-coroutines
			  #:make-coroutine
			  #:yield)
  (:shadowing-import-from #:alexandria
			  #:when-let
			  #:with-unique-names
			  #:ensure-list)
  (:export #:activate
	   #:simulate
	   #:schedule
	   #:*simulation*
	   #:note
	   #:self
	   #:defprocess
	   #:yield
	   #:processes
	   #:spatial
	   #:activate
	   #:look-around
	   #:empty-p
	   #:location
	   #:with-simulation
	   #:do-board
	   #:x #:y
	   #:with-spatial-simulation))

(defpackage #:rpd-simulation
    (:use #:cl #:iterate)
  (:shadowing-import-from #:rpd-sim-impl
			  #:self
			  #:*simulation*
			  #:defprocess
			  #:yield
			  #:spatial
			  #:look-around
			  #:empty-p
			  #:location
			  #:processes
			  #:with-simulation
			  #:do-board
			  #:x #:y
			  #:with-spatial-simulation)
  (:export #:activate
	   #:simulate
	   #:note
	   #:self
	   #:defprocess
	   #:do-processes
	   #:yield
	   #:spatial
	   #:look-around
	   #:empty-p
	   #:location
	   #:with-simulation
	   #:do-board
	   #:x #:y
	   #:with-spatial-simulation))