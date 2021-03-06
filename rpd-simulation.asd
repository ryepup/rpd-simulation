;;;; rpd-simulation.asd

(asdf:defsystem #:rpd-simulation
  :serial t
  :depends-on (#:alexandria #:rpd-coroutines
			    #:cl-log #:cl-paths
			    #:iterate #:spatial-trees
			    #:pileup)
  :components ((:file "package")
	       (:file "protocol")
	       (:file "actor")
	       (:file "simulation")
	       (:file "spatial")
	       (:file "resources")
	       (:file "logging")))

(asdf:defsystem #:rpd-simulation-examples
  :serial t
  :depends-on (#:rpd-simulation #:iterate
				#:cl-paths
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
  :depends-on (#:rpd-simulation #:lisp-unit #:cl-log)
  :components ((:module
		:tests
		:serial t
		:components ((:file "package")
			     (:file "actor")
			     (:file "simulation")
			     (:file "spatial")
			     (:file "resources")))))
