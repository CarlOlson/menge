
(in-package :menge)

;; Helpers

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defun length<= (seq x)
  (cond
    ((null seq) t)
    ((<= x 0) nil)
    ((consp seq)
     (length<= (cdr seq) (1- x)))
    (t
     (<= (length seq) x))))

(defun key-test (key test &rest args)
  (apply test (mapcar key args)))

(defun multiple-of (number base &key (start 0))
  (let ((rem (rem (- number start) base)))
    (values (zerop rem) rem)))

(defun create-wheel (s1 m1 s2 m2)
  (labels ((multiples-of (number start stop)
	     (loop
		for multiple from start to stop by number
		collect multiple)))
    (let* ((lcm (lcm m1 m2))
	   (max (+ lcm (max s1 s2)))
	   (multiples (append (multiples-of m1 s1 max)
			      (multiples-of m2 s2 max)))
	   (multiples (sort (remove-duplicates multiples)
			    '<))
	   (multiples (remove-if (lambda (x) (< x (max s1 s2)))
				 multiples)))
      (loop
	 for (a b) on multiples
	 when b collect (- b a) into wheel
	 finally (return (if (and wheel
				  (apply '= wheel))
			     (list (car wheel))
			     wheel))))))

;; Bounds

(defclass bound ()
  ((value     :type t       :initarg :value     :reader bound-value)
   (inclusive :type boolean :initarg :inclusive :reader inclusive?))
  (:documentation
   "A bound of one value, can be inclusive or exclusive."))

(defmethod print-object ((b bound) stream)
  (format stream "(~A ~A)"
	  (bound-value b)
	  (if (inclusive? b)
	      "inclusive"
	      "exclusive")))

(defun mkbound (value inclusive)
  (make-instance 'bound
		 :value value
		 :inclusive inclusive))

(defmethod bound-value ((x integer)) x)
(defmethod inclusive?  ((x integer)) t)

(defgeneric exclusive? (t)
  (:documentation
   "Is the value exclusive as a bound?")

  (:method ((x t))
    (not (inclusive? x))))

(defgeneric ordered<= (t t)
  (:documentation
   "Returns true when (< A B), helper function.")

  (:method ((b bound) (n number))
    (with-accessors ((bval bound-value)) b
      (or (< bval n)
	  (and (= bval n)
	       (inclusive? b)))))

  (:method ((n number) (b bound))
    (with-accessors ((bval bound-value)) b
      (or (< n bval)
	  (and (= n bval)
	       (inclusive? b)))))
  
  (:method ((a bound) (b bound))
    (with-accessors ((av bound-value)) a
      (with-accessors ((bv bound-value)) b
	(or (< av bv)
	    (and (= av bv)
		 (= (inclusive? a)
		    (inclusive? b)))))))
  
  (:method ((a number) (b number))
    (<= a b)))

(defun bounds-ordered? (a b c d)
  (or (key-test 'bound-value '< a b c d)
      (and (key-test 'bound-value '<= a b c d)
	   (and (or (inclusive? a)
		    (exclusive? b))
		(or (inclusive? c)
		    (exclusive? d))))))

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
  (print-unreadable-object (s stream :type t)
    (format stream "of ~A" (slot-value s 'anti-set))))

(defclass union-set (base-set)
  ((members :initarg :members :type list)))

(defmethod print-object ((s union-set) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~{~A~^, ~}" (slot-value s 'members))))

(defclass list-set (base-set)
  ((members :initarg :members :type list :reader bag-contents)))

(defmethod print-object ((s list-set) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~{~A~^, ~}" (slot-value s 'members))))

(defun bag-of (x &rest xs)
  (make-instance 'list-set :members (cons x xs)))

;; TODO should subclass real-set and discrete-set
(defclass int-set (base-set)
  ((lower-bound :type integer :initarg :lower :reader lower-bound)
   (upper-bound :type integer :initarg :upper :reader upper-bound)
   (increment   :type integer :initarg :by    :reader increment)))

(defgeneric mkrange (t t &optional integer boolean boolean)
  (:method ((a integer) (b integer)
  	    &optional (by 1) (inclusive-a t) (inclusive-b t))
    (when (<= by 0)
      (error "increment <= 0"))
    (when (>= a b)
      (error "lower-bound >= upper-bound :: ~A >= ~A" a b))
    (unless inclusive-a
      (incf a by)
      (setq inclusive-a t))
    (unless inclusive-b
      (decf b)
      (setq inclusive-b t))
    (multiple-value-bind (truthy rem) (multiple-of b by :start a)
      (unless truthy
	(decf b rem)))
    (if (>= a b)
	*null-set-instance*
	(make-instance 'int-set :lower a :upper b :by by))))

(defmethod print-object ((s int-set) stream)
  (print-unreadable-object (s stream :type t)
    (with-accessors ((l lower-bound) (u upper-bound)) s
      (format stream "~A~A,~A~A:~A"
	      (if (inclusive? l) "[" "(")
	      (bound-value l)
	      (bound-value u)
	      (if (inclusive? u) "]" ")")
	      (increment s)))))

(defgeneric equal-bounds (int-set int-set)
  (:documentation
   "Do two sets have equal upper and lower bounds?")
  
  (:method ((s1 int-set) (s2 int-set))
    (let ((inc1 (increment s1))
	  (inc2 (increment s2))
	  (low1 (lower-bound s1))
	  (low2 (lower-bound s2))
	  (high1 (upper-bound s1))
	  (high2 (upper-bound s2)))
      (and (or (>= low2 low1 (- low2 inc2))
	       (>= low1 low2 (- low1 inc1)))
	   (or (<= high2 high1 (+ high2 inc2))
	       (<= high1 high2 (+ high1 inc1)))))))

(defgeneric eqls (t t)
  (:documentation
   "Returns true if arguments are `eql'.  Or are equal objects defined
in this library.  Should not be considered a public API.")

  (:method ((a t) (b t))
    (eql a b))
  
  (:method ((a null-set) (b null-set))
    t)
  
  (:method ((a all-set) (b all-set))
    t)

  (:method ((a union-set) (b union-set))
    (labels ((every-eqls (a b)
	       (cond
		 ((and (null a) (null b))
		  t)
		 ((or (null a) (null b))
		  nil)
		 ((eqls (car a) (car b))
		  (every-eqls (cdr a) (cdr b))))))
      (every-eqls
       (slot-value a 'members)
       (slot-value b 'members))))
  
  (:method ((a bound) (b bound))
    (and (eql (bound-value a)
  	      (bound-value b))
  	 (eq (inclusive? a)
  	     (inclusive? b))))
  
  (:method ((s1 int-set) (s2 int-set))
    (and (equal-bounds s1 s2)
	 (eqls (increment s1)
	       (increment s2)))))

(defgeneric members (base-set)
  (:documentation
   "Returns a list of all sets in a compound set.  Only returns sets
that are part of a union, not sets as members.")

  (:method ((s base-set))
    (list s))
  
  (:method ((s union-set))
    (reduce #'append
	    (mapcar 'members (slot-value s 'members)))))

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
    (and (ordered<= (lower-bound s) n)
	 (ordered<= n (upper-bound s))
	 (multiple-of n
		      (increment s)
		      :start (lower-bound s)))))

(defgeneric insert (base-set t)
  (:documentation
   "Inserts an element into a set, returns a new set.  Sets may share
memory.")

  (:method ((s base-set) x)
    (union s (bag-of x)))
  
  (:method ((s null-set) x)
    (bag-of x))
  
  (:method ((s all-set) x)
    *all-set-instance*)
  
  (:method ((s list-set) x)
    (apply 'bag-of x (bag-contents x)))

  (:method ((s int-set) x)
    (let ((lower (lower-bound s))
	  (upper (upper-bound s))
	  (inc   (increment s)))
      (cond
	((not (integerp x))
	 (union s (bag-of x)))
	((contains s x)
	 s)
	((= x lower)
	 (if (inclusive? lower)
	     s
	     (mkrange (mkbound x t) upper inc)))
	((= x upper)
	 ;; NOTE upper bound should be multiple of increment
	 (if (inclusive? upper)
	     s
	     (mkrange lower (mkbound x t) inc)))
	((= x (- lower inc))
	 (mkrange x upper inc))
	((= x (+ upper inc))
	 (mkrange lower x inc))
	(t
	 (union s (bag-of x)))))))

(defun simplify-sets (sets)
  (labels ((rec (x rest)
	     (let (simplified)
	       (values
		(mapcar (lambda (y)
			  (aif (and (not simplified)
				    (simplify x y))
			       (prog1 it
				 (setq simplified t))
			       y))
			rest)
		simplified))))
    (loop
       for (x . sets) on sets
       do (multiple-value-bind (new-sets simplified)
	      (rec x sets)
	    (when simplified
	      (return (simplify-sets (append prev new-sets)))))
       collecting x into prev
       finally (return prev))))

(defgeneric simplify (base-set base-set)
  (:documentation
   "Returns a simplified union or nil.")

  (:method ((s1 base-set) (s2 base-set))
    nil)

  (:method ((s1 union-set) (s2 union-set))
    (let ((members (append (members s1) (members s2))))
      (make-instance 'union-set :members (simplify-sets members))))
  
  (:method ((s1 int-set) (s2 int-set))
    (labels ((merge-bounds (s1 s2 inc)
	       ;; NOTE only works for increments of one
	       (with-slots ((min1 lower-bound) (max1 upper-bound)) s1
		 (with-slots ((min2 lower-bound) (max2 upper-bound)) s2
		   (cond
		     ((and (bounds-ordered? min1 min2 max2 max1)
			   (contains s1 min2))
		      s1)
		     ((bounds-ordered? min1 min2 max1 max2)
		      (mkrange min1 max2 inc))
		     ((bounds-ordered? min2 min1 max2 max1)
		      (mkrange min2 max1 inc))
		     ((and (= (bound-value max1)
			      (bound-value min2))
			   (or (inclusive? max1)
			       (inclusive? min2)))
		      ;; (x, y) ∪ [y, z)
		      (mkrange min1 max2 inc)))))))
      (let ((inc1  (increment s1))
	    (inc2  (increment s2))
	    (low1  (lower-bound s1))
	    (low2  (lower-bound s2))
	    (high1 (upper-bound s1))
	    (high2 (upper-bound s2)))
	;; TODO split overlapping ranges
	(cond
	  ((= inc1 inc2 1)
	   ;; merge when increment is one
	   (or (merge-bounds s1 s2 1)
	       (merge-bounds s2 s1 1)))
	  ((and (or (= (gcd inc1 inc2) 1)
		    (= inc1 inc2))
		(equal-bounds s1 s2))
	   ;; increments are coprime or equal
	   (let ((wheel (create-wheel low1 inc1 low2 inc2)))
	     ;; TODO (length wheel) > 1
	     (when (length<= wheel 1)
	       ;; two ranges fill bounds
	       (mkrange (min low1 low2)
			(max high1 high2)
			(car wheel)))))
	  ((= inc1 inc2)
	   (or (merge-bounds s1 s2 inc1)
	       (merge-bounds s2 s1 inc1)))
	  ((and (or (multiple-of inc1 inc2)
		    (multiple-of inc2 inc1))
		(/= inc1 inc2))
	   (when (equal-bounds s1 s2)
	     ;; larger increment is multiple of smaller
	     (mkrange (min low1 low2)
		      (max high1 high2)
		      (min inc1 inc2)))))))))

(defgeneric union (base-set base-set)
  (:documentation
   "Returns a union of the two sets given.")

  (:method ((s1 base-set) (s2 base-set))
    (let* ((members (append (members s1) (members s2)))
	   (sets    (simplify-sets members)))
      (if (length<= sets 1)
	  (car sets)
	  (make-instance 'union-set
			 :members sets))))

  (:method ((s1 null-set) (s2 base-set))
    s2)

  (:method ((s1 base-set) (s2 null-set))
    s1)
  
  (:method ((s1 null-set) (s2 null-set))
    *null-set-instance*)
  
  (:method ((s1 all-set) s2)
    *all-set-instance*)

  (:method (s1 (s2 all-set))
    *all-set-instance*)

  (:method ((s1 int-set) (s2 int-set))
    (or (simplify s1 s2)
	(call-next-method))))

(defgeneric inverse (base-set)
  (:documentation
   "Creates an inverse set.")

  (:method ((s base-set))
    (make-instance 'inverse-set :not s))
  
  (:method ((s all-set))
    *null-set-instance*)

  (:method ((s null-set))
    *all-set-instance*)

  (:method ((s inverse-set))
    (slot-value s 'anti-set)))

(defmacro defalias (orig-name new-name)
  `(setf (symbol-function ',new-name)
	 (symbol-function ',orig-name)))

(defalias inverse  ¬)
(defalias union    ∪)
(defalias contains ∋)
