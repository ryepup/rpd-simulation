;;;; rpd-simulation.asd

(asdf:defsystem #:rpd-simulation
  :serial t
  :depends-on (#:alexandria
               #:rpd-coroutines
               #:iterate
               #:cl-heap)
  :components ((:file "package")
               (:file "rpd-simulation")))

