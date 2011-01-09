(in-package :rpd-simulation-examples)

;; ported from http://simpy.sourceforge.net/SimPyDocs/Manuals/Manual.html#creating-a-process-object
;;;;;;;;;;;;;;;;
;; from SimPy.Simulation import *
;; class Message(Process):
;;  """ a simple Process """
;;  def __init__(self,i,len):
;;      Process.__init__(self,name="Message"+str(i))
;;      self.i = i
;;      self.len = len

;;  def go(self):
;;      print now( ), self.i, "Starting"
;;      yield hold,self,100.0
;;      print now( ), self.i, "Arrived"

;; initialize( )
;; p1  = Message(1,203)  # new message
;; activate(p1,p1.go( )) # activate it
;; p2  = Message(2,33)
;; activate(p2,p2.go( ),at=6.0)
;; simulate(until=200)
;; print 'Current time is ',now( ) # will print 106.0
;;;;;;;;;;;;;;;;;;;;;

(defactor message ()
  ((i :initarg :i :accessor i)
   (len :initarg :len :accessor len))
  (:documentation "a simple process")
  (:action self
   (cl-log:log-message :info "Starting ~a" (i self))
   (yield 100)
   (cl-log:log-message :info "Arrived ~a" (i self))))

(defun message ()
  (let ((sim (make-simulation))
	(p1 (make-instance 'message :i 1 :len 204))
	(p2 (make-instance 'message :i 2 :len 33)))
    (activate sim p1)
    (activate sim p2 6)
    (rpd-simulation::simulate sim :until 200 )
    (cl-log:log-message :info "done")))
