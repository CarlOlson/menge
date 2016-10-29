
(in-package :menge)

;; Helpers

(defun length<=1 (seq)
  (if (consp seq)
      (or (null seq)
	  (null (cdr seq)))
      (<= (length seq) 1)))

;; Bounds

(defclass bound ()
  ((value     :type t       :initarg :value)
   (inclusive :type boolean :initarg :inclusive))
  (:documentation
   "A bound of one value, can be inclusive or exclusive."))

(defun mkbound (value inclusive)
  (make-instance 'bound
		 :value value
		 :inclusive inclusive))

(defmethod print-object ((b bound) stream)
  (format stream "(~A ~A)"
	  (bound-value b)
	  (if (inclusive? b)
	      "inclusive"
	      "exclusive")))

(defgeneric bound-value (t)
  (:documentation
   "Returns a value as if the argument were a bound.  Not all
types make sense.")

  (:method ((x t))
    x)

  (:method ((b bound))
    (bound-value (slot-value b 'value))))

(defgeneric inclusive? (t)
  (:documentation
   "Is the value inclusive as a bound?")

  (:method ((x t))
    t)

  (:method ((b bound))
    (slot-value b 'inclusive)))

(defgeneric exclusive? (t)
  (:documentation
   "Is the value exclusive as a bound?")

  (:method ((x t))
    (not (inclusive? x))))

(defgeneric ordered< (t t)
  (:documentation
   "Helper function for `ordered?'.")

  (:method ((a t) (b t))
    "Returns true when (= A B) if A is inclusive, otherwise (< A B)."
    (with-accessors ((av bound-value)) a
      (with-accessors ((bv bound-value)) b
	(or (< av bv)
	    (and (= av bv)
		 (inclusive? a))))))
  
  (:method ((a number) (b number))
    (<= a b)))

(defun ordered? (&rest xs)
  "Are these well ordered?  Or lexicographically ordered for
appropriate types?"
  (cond
    ((length<=1 xs) t)
    ((ordered< (first xs) (second xs))
     (apply 'ordered? (cdr xs)))))

;; Sets

(defclass base-set ()
  ()
  (:documentation
   "The superclass for all other sets.  Often implements null-set
behavior.  null-set isn't the base class to allow optimizations with
null sets."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass null-set (base-set)
    ()
    (:documentation
     "An empty set.")))

(defvar *null-set-instance*
  (make-instance 'null-set))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass all-set (base-set)
    ()
    (:documentation
     "A set containing every possible value.")))

(defvar *all-set-instance*
  (make-instance 'all-set))

(defclass inverse-set (base-set)
  ((anti-set :initarg :not
	     :initform *all-set-instance*
	     :type base-set))
  (:documentation
   "A set containing everything not in anti-set."))

(defmethod print-object ((s inverse-set) stream)
  (format stream "#<INVERSE-SET of ~A>" (slot-value s 'anti-set)))

(defclass union-set (base-set)
  ((members :initarg :members :type list)))

(defmethod print-object ((s union-set) stream)
  (format stream "#<UNION-SET ~{~A~^, ~}>" (slot-value s 'members)))

(defclass list-set (base-set)
  ((members :initarg :members :type list)))

(defmethod print-object ((s list-set) stream)
  (format stream "#<LIST-SET ~{~A~^, ~}>" (slot-value s 'members)))

;; TODO should subclass real-set
(defclass int-set (base-set)
  ((lower-bound :initarg :lower :reader lower-bound)
   (upper-bound :initarg :upper :reader upper-bound)))

(defun mkrange (lower upper
		&optional
		  (lower-inclusive t lower-supplied?)
		  (upper-inclusive t upper-supplied?))
  (declare (type integer lower upper))
  (let ((lower (if lower-supplied?
		   (mkbound (bound-value lower) lower-inclusive)
		   lower))
	(upper (if upper-supplied?
		   (mkbound (bound-value upper) upper-inclusive)
		   upper)))
    (unless (ordered? lower upper)
      (error "lower > upper :: ~A > ~A" lower upper))
    (make-instance 'int-set :lower lower :upper upper)))

(defmethod print-object ((s int-set) stream)
  (with-accessors ((l lower-bound) (u upper-bound)) s
    (format stream "#<INT-SET ~A~A,~A~A>"
	    (if (inclusive? l) "[" "(")
	    (bound-value l)
	    (bound-value u)
	    (if (inclusive? u) "]" ")"))))

(defgeneric eqls (t t)
  (:documentation
   "Returns true if arguments are `eql'.  Or are equal objects defined
in this library.  Should not be considered a public API.")

  ;; TODO add eq optimization?
  (:method ((a t) (b t))
    (and (eql (bound-value a)
	      (bound-value b))
	 (eq (inclusive? a)
	     (inclusive? b))))
  
  (:method ((s1 int-set) (s2 int-set))
    (and (eqls (lower-bound s1)
	       (lower-bound s2))
	 (eqls (upper-bound s1)
	       (upper-bound s2)))))

(defgeneric contains (base-set t)
  (:documentation
   "Checks if a set contains a value.")
  
  (:method ((s base-set) x)
    nil)
  
  (:method ((s all-set) x)
    t)
  
  (:method ((s inverse-set) x)
    (not (contains (slot-value s 'anti-set) x)))
  
  (:method ((s union-set) x)
    (some (lambda (s) (contains s x))
	  (slot-value s 'members)))

  (:method ((s list-set) x)
    (member x (slot-value s 'members) :test 'eql))

  (:method ((s int-set) (n integer))
    (ordered? (lower-bound s) n (upper-bound s))))

(defgeneric simplify (base-set base-set)
  (:documentation
   "Returns a simplified union or nil.")

  (:method ((s1 base-set) (s2 base-set))
    nil)

  ;; TODO how to simplify a union set?
  
  (:method ((s1 int-set) (s2 int-set))
    (labels ((merge-bounds (s1 s2)
	       (with-slots ((min1 lower-bound) (max1 upper-bound)) s1
		 (with-slots ((min2 lower-bound) (max2 upper-bound)) s2
		   (cond
		     ((ordered? min1 min2 max2 max1)
		      s1)
		     ((ordered? min1 min2 max1)
		      (mkrange min1 max2))
		     ((ordered? min1 max2 max1)
		      (mkrange min2 max1))
		     ((and (= (bound-value max1)
			      (bound-value min2))
			   (or (inclusive? max1)
			       (inclusive? min2)))
		      ;; (x, y] ∪ [y, z)
		      (mkrange min1 max2)))))))
      (or (merge-bounds s1 s2)
	  (merge-bounds s2 s1)
	  (call-next-method)))))

(defgeneric union (base-set base-set)
  (:documentation
   "Returns a union of the two sets given.")

  (:method ((s1 base-set) (s2 base-set))
    ;; TODO should simplify?
    (make-instance 'union-set :members (list s1 s2)))

  (:method ((s1 null-set) (s2 null-set))
    *null-set-instance*)
  
  (:method ((s1 all-set) s2)
    *all-set-instance*)

  (:method (s1 (s2 all-set))
    *all-set-instance*)

  (:method ((s1 int-set) (s2 int-set))
    (or (simplify s1 s2) nil)))

(defgeneric inverse (base-set)
  (:documentation
   "Creates an inverse set.")

  (:method ((s all-set))
    *null-set-instance*)

  (:method ((s null-set))
    *all-set-instance*)

  (:method ((s inverse-set))
    (slot-value s 'anti-set)))

(defun make-key-test (key test)
  (lambda (&rest args)
    (apply test (mapcar key args))))

(defmacro defalias (orig-name new-name)
  `(setf (symbol-function ',new-name)
	 (symbol-function ',orig-name)))

(defalias inverse  ¬)
(defalias union    ∪)
(defalias contains ∋)
