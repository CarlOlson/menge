
(asdf:defsystem menge
  :name "menge"
  :version "0.1.0"
  :author "Carl Olson"
  :licence "Public Domain"
  :description
  "Optimized sets for representing groups of numbers and infinite
sets (not streams)."
  :depends-on ()
  :serial t
  :components ((:file "package")
	       (:file "menge")))
