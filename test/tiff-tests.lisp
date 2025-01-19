(defpackage :tiff-tests
  (:use :cl :fiveam :tiff))

(in-package :tiff-tests)

(def-suite tiff-tests)
(in-suite tiff-tests)

(test all
      (let ((f (read-tiff "test/resources/raw1.3FR")))
	(is (= 4 (length (tiff-ifds f))))
	(let ((ifd0 (elt (tiff-ifds f) 0)))
	  (is (string= "IFD0" (ifd-name ifd0))))))
