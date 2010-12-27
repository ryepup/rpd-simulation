;;;; rpd-simulation.asd

(asdf:defsystem #:rpd-simulation
  :serial t
  :depends-on (#:alexandria #:cl-geometry
               #:rpd-coroutines
               #:iterate #:spatial-trees
	       #:pileup)
  :components ((:file "package")
               (:file "rpd-simulation")
	       (:file "spatial-simulation")
	       (:file "user")))

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
			     (:file "simulation")))))