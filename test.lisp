
(ql:quickload :fiasco)
(fiasco:define-test-package :menge.test
  (:use    :menge)
  (:export :test-all))
(in-package :menge.test)

(defun test-all (&optional (interactive nil))
  (run-package-tests :package     :menge.test
		     :interactive interactive))

(deftest test-ordered? ()
  (is (ordered? 1 1 2 (mkbound 2 t) (mkbound 3 t) 3))
  (is (not (ordered? (mkbound 1 nil) 1)))
  (is (ordered? (mkbound 1 t) (mkbound 1 nil)))
  (is (not (ordered? (mkbound 1 nil) (mkbound 1 t)))))

(deftest test-contains ()
  (is (contains *all-set-instance* 1))
  (is (not (contains *null-set-instance* 1)))
  (is (contains (mkrange 1 10) 5))
  (is (not (contains (mkrange 1 10) 15))))

(deftest test-range ()
  (is (contains (mkrange 1 2) 1))
  (is (contains (mkrange 1 2) 2))
  (is (not (contains (mkrange 1 2 nil t) 1)))
  (is (not (contains (mkrange 1 2 t nil) 2))))

