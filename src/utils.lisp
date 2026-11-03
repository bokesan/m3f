(defpackage :utils
  (:use :cl)
  (:import-from :alexandria :array-length)
  (:export :parse-as-rational))

(in-package :utils)


(declaim (ftype (function (string) (or null rational)) parse-as-rational))

(defun parse-as-rational (s)
  "Try to convert the string S into a rational number.
If it can not be parsed, return NIL.

The string must be in one of these formats:

An integer: a non-empty sequence of ASCII digits.
A rational: two integers separated by `/'.
A float: two integers separated by `.'.

The parts of rational and floats must not be empty
and S must not contain any whitespace."
  (let ((len (length s)))
    (labels ((scan (p sep)
	       (declare (type array-length p))
	       (if (>= p len)
		   t
		   (let ((c (char s p)))
		     (cond ((digit-char-p c) (scan (+ p 1) sep))
			   (sep nil)
			   ((find c "./") (scan (+ p 1) t))
			   (t nil))))))
      (if (scan 0 nil)
	  (let ((x (read-from-string s)))
	    (etypecase x
	      (rational x)
	      (float (rationalize x))))
	  nil))))
