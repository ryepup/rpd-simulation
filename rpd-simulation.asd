;;;; rpd-simulation.asd

(asdf:defsystem #:rpd-simulation
  :serial t
  :depends-on (#:alexandria
               #:rpd-coroutines
               #:iterate #:spatial-trees
	       #:pileup #:cl-heap)
  :components ((:file "package")
               (:file "rpd-simulation")
	       (:file "spatial-simulation")))

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
			     (:file "brians-brain")))))

