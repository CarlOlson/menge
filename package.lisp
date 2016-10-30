
(declaim (optimize (debug 3) (safety 3)))

(defpackage :menge
  (:use :cl)
  (:shadow :union)
  (:export :mkbound
	   :*null-set-instance*
	   :*all-set-instance*
	   :bag-of :mkrange
	   :contains :insert :union :inverse))
