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
	   #:make-location
	   #:with-simulation
	   #:do-board
	   #:*simulation-step-hook*
	   #:*process-dead-hook*
	   #:*process-after-step-hook*
	   #:x #:y
	   #:with-spatial-simulation
	   #:process
	   #:nearby-spots))

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
			  #:make-location
			  #:processes
			  #:with-simulation
			  #:do-board
			  #:x #:y
			  #:*simulation-step-hook*
			  #:*process-dead-hook*
			  #:*process-after-step-hook*
			  #:with-spatial-simulation
			  #:process
			  #:nearby-spots)
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
	   #:make-location
	   #:with-simulation
	   #:do-board
	   #:x #:y
	   #:*simulation-step-hook*
	   #:*process-dead-hook*
	   #:*process-after-step-hook*
	   #:with-spatial-simulation
	   #:process
	   #:nearby-spots))