
(in-package :menge)

;; Helpers

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

(defgeneric exclusive? (bound)
  (:documentation
   "Is the value exclusive as a bound?")

  (:method ((x bound))
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
  ;; TODO base on hash
  ((members :initarg :members :type list)))

(defmethod print-object ((s union-set) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~{~A~^, ~}" (slot-value s 'members))))

(defclass list-set (base-set)
  ;; TODO seperate hash-set and bag
  ((members :initarg :members :type list :reader bag-contents)))

(defmethod print-object ((s list-set) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~{~A~^, ~}" (slot-value s 'members))))

(defun bag-of (x &rest xs)
  (make-instance 'list-set :members (cons x xs)))

;; TODO should subclass real-set
(defclass int-set (base-set)
  ((lower-bound :type bound :initarg :lower :reader lower-bound)
   (upper-bound :type bound :initarg :upper :reader upper-bound)))

(defgeneric mkrange (t t &optional t t)
  (:method ((a number) (b number)
	    &optional (inclusive-a t) (inclusive-b t))
    (mkrange (mkbound a inclusive-a)
	     (mkbound b inclusive-b)))
  
  (:method ((a bound) (b bound)
	    &optional (inclusive-a nil ia?) (inclusive-b nil ib?))
    (let ((a (if ia? (mkbound (bound-value a) inclusive-a) a))
	  (b (if ib? (mkbound (bound-value b) inclusive-b) b)))
      (unless (ordered<= a b)
	(error "lower-bound >= upper-bound :: ~A >= ~A" a b))
      (make-instance 'int-set :lower a :upper b))))

(defmethod print-object ((s int-set) stream)
  (print-unreadable-object (s stream :type t)
    (with-accessors ((l lower-bound) (u upper-bound)) s
      (format stream "~A~A,~A~A"
	      (if (inclusive? l) "[" "(")
	      (bound-value l)
	      (bound-value u)
	      (if (inclusive? u) "]" ")")))))

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
    (and (ordered<= (lower-bound s) n)
	 (ordered<= n (upper-bound s)))))

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
	  (upper (upper-bound s)))
      (cond
	((not (integerp x))
	 (union s (bag-of x)))
	((and (ordered<= lower x)
	      (ordered<= x upper))
	 s)
	((= x (bound-value lower))
	 (if (inclusive? lower)
	     s
	     (mkrange (mkbound x t) upper)))
	((= x (bound-value upper))
	 (if (inclusive? upper)
	     s
	     (mkrange lower (mkbound x t))))
	(t
	 (union s (bag-of x)))))))

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
		     ((bounds-ordered? min1 min2 max2 max1)
		      s1)
		     ((bounds-ordered? min1 min2 max1 max2)
		      (mkrange min1 max2))
		     ((bounds-ordered? min2 min1 max2 max1)
		      (mkrange min2 max1))
		     ((and (= (bound-value max1)
			      (bound-value min2))
			   (or (inclusive? max1)
			       (inclusive? min2)))
		      ;; (x, y) ∪ [y, z)
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
    ;; (or (simplify s1 s2) nil)
    (call-next-method)))

(defgeneric inverse (base-set)
  (:documentation
   "Creates an inverse set.")

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
