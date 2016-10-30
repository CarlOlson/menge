
(ql:quickload :fiasco)
(fiasco:define-test-package :menge.test
  (:use    :menge)
  (:export :test-all)
  (:import-from :menge :eqls))
(in-package :menge.test)

(defun test-all (&optional (interactive nil))
  (run-package-tests :package     :menge.test
		     :interactive interactive))

(deftest test-eqls ()
  (is (eqls 1 1))
  (is (not (eqls 1 1.0)))
  (is (eqls (mkbound 1 t) (mkbound 1 t)))
  (is (eqls (mkrange 1 10) (mkrange 1 10))))

(deftest test-range ()
  (is (contains (mkrange 1 2) 1))
  (is (contains (mkrange 1 2) 2))
  (is (not (contains (mkrange 1 2 nil t) 1)))
  (is (not (contains (mkrange 1 2 t nil) 2))))

(deftest test-contains ()
  (is (contains *all-set-instance* 1))
  (is (not (contains *null-set-instance* 1)))
  (is (contains (mkrange 1 10) 5))
  (is (not (contains (mkrange 1 10) 15))))

(deftest test-insert ()
  (is (contains (insert *null-set-instance* 1) 1))
  (is (contains (insert (mkrange 1 2 nil nil) 1) 1))
  (is (eqls (insert (mkrange 1 2) 1) (mkrange 1 2)))
  (dolist (x '(1 2 3))
    (is (contains (bag-of 1 2 3) x))))

(deftest test-union ()
  (let ((set1 (mkrange 0 9))
	(set2 (mkrange 10 20))
	(set3 (bag-of 1 2))
	(set4 (bag-of 3 4)))
    (dotimes (x 21)
      (is (contains (union set1 set2) x)))
    (dolist (x '(1 2 3 4))
      (is (contains (union set3 set4) x)))))

(deftest test-inverse ()
  (is (eqls (inverse *null-set-instance*)
	    *all-set-instance*))
  (is (eqls (inverse *all-set-instance*)
	    *null-set-instance*))
  (is (not (contains (inverse (bag-of 1)) 1)))
  (is (contains (inverse (inverse (bag-of 1))) 1)))
