;;;; package.lisp

(defpackage #:rpd-simulation
    (:use #:cl #:iterate)
  (:shadowing-import-from #:rpd-coroutines
			  #:make-coroutine
			  #:yield)
  (:shadowing-import-from #:cl-log #:log-message)
  (:export #:make-simulation
	   #:actors
	   #:defactor #:yield
	   #:schedule
	   #:activate
	   #:simulate
	   #:simulation
	   #:simulation-step
	   #:spatial
	   #:look
	   #:do-board
	   #:make-board
	   #:make-location
	   #:location=
	   #:location
	   #:x #:y
	   #:neighbors
	   #:board-elt))