;;;; rpd-simulation.asd

(asdf:defsystem #:rpd-simulation
  :serial t
  :depends-on (#:alexandria
               #:rpd-coroutines
               #:iterate
               #:cl-heap)
  :components ((:file "package")
               (:file "rpd-simulation")))

(asdf:defsystem #:rpd-simulation-examples
  :serial t
  :depends-on (#:rpd-simulation #:iterate)
  :components ((:module
		:examples
		:serial t
		:components ((:file "package")
			     (:file "message")))))

