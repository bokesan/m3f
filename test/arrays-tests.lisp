(defpackage :arrays-tests
  (:use :cl :fiveam :arrays))

(in-package :arrays-tests)

(def-suite arrays-tests)
(in-suite arrays-tests)

(test all
      (let ((a (make-array 9 :element-type '(unsigned-byte 8))))
	(dotimes (i (length a))
	  (setf (aref a i) (+ i 1)))
	(copy-bytes a 4 a 1 2)
	(is (equalp #(1 5 6 4 5 6 7 8 9) a))))
