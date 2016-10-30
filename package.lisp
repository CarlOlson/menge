
(declaim (optimize (debug 3) (safety 3)))

(defpackage :menge
  (:use :cl)
  (:shadow :union)
  (:export :mkbound :ordered?
	   :*null-set-instance*
	   :*all-set-instance*
	   :mkrange
	   :contains :union :inverse))
