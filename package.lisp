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
  (:shadowing-import-from #:rpd-coroutines
			  #:make-coroutine
			  #:yield)

  (:export #:make-simulation
	   #:defactor #:yield
	   #:schedule
	   #:scheduled-time
	   #:scheduled-item))