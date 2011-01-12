;;;; rpd-simulation.asd

(asdf:defsystem #:rpd-simulation
  :serial t
  :depends-on (#:alexandria #:cl-geometry
               #:rpd-coroutines #:cl-log
               #:iterate #:spatial-trees
	       #:pileup)
  :components ((:file "package")
	       (:file "protocol")
	       (:file "actor")
	       (:file "simulation")
	       (:file "spatial")
	       (:file "logging")))

(asdf:defsystem #:rpd-simulation-examples
  :serial t
  :depends-on (#:rpd-simulation #:iterate
				#:lispbuilder-sdl)
  :components ((:module
		:examples
		:serial t
		:components ((:file "package")
			     (:file "message")
			     (:file "bees")
			     (:file "brians-brain")
			     (:file "ecology")))))

(asdf:defsystem #:rpd-simulation-tests
  :serial t
  :depends-on (#:rpd-simulation #:lisp-unit)
  :components ((:module
		:tests
		:serial t
		:components ((:file "package")
			     (:file "actor")
			     (:file "simulation")
			     (:file "spatial")))))
