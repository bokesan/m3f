(defpackage :lenses
  (:use :cl)
  (:export :lens :lens-p :make-lens
	   :lens-focal-length
	   :lens-max-aperture
	   :lens-min-focal-length
	   :lens-max-focal-length
	   :lens-max-aperture-at-min-focal-length
	   :lens-max-aperture-at-max-focal-length
	   :lens-name
	   :lens-serial-number
	   :lens-nicknames))

(in-package :lenses)


(defun either (&rest values)
  "Return true if exactly one of VALUES is true."
  (labels ((scan (values)
	     (cond ((null values) nil)
		   ((car values) (all-false-p (cdr values)))
		   (t (scan (cdr values)))))
	   (all-false-p (xs)
	     (cond ((null xs) t)
		   ((car xs) nil)
		   (t (all-false-p (cdr xs))))))
    (scan values)))


(defstruct (lens (:constructor %make-lens))
  (name nil :type string)
  (nicknames nil :type sequence)
  (serial-number nil :type (or null string))
  (min-focal-length 0 :type rational)
  (max-focal-length 0 :type rational)
  (max-aperture-at-min-focal-length 0 :type rational)
  (max-aperture-at-max-focal-length 0 :type rational))

(defun lens-focal-length (lens)
  (values (lens-min-focal-length lens)
	  (lens-max-focal-length lens)))

(defun lens-max-aperture (lens)
  (values (lens-max-aperture-at-min-focal-length lens)
	  (lens-max-aperture-at-max-focal-length lens)))

(defun make-lens (name &key focal-length max-aperture
			 min-focal-length max-focal-length
			 max-aperture-at-min-focal-length
			 max-aperture-at-max-focal-length
			 serial-number
			 nicknames)
  (declare (type string name)
	   (type (or null rational)
		 focal-length min-focal-length max-focal-length
		 max-aperture-at-min-focal-length
		 max-aperture-at-max-focal-length)
	   (type (or null string) serial-number))
  (unless (either (and focal-length max-aperture)
		  (and min-focal-length max-focal-length
		       max-aperture-at-min-focal-length
		       max-aperture-at-max-focal-length))
    (error "expected either :focal-length or :min-focal-length and :max-focal-length"))
  (unless (or (not min-focal-length) (<= min-focal-length max-focal-length))
    (error ":min-focal-length must be less than or equal to :max-focal-length"))
  (when focal-length
    (setf min-focal-length focal-length
	  max-focal-length focal-length
	  max-aperture-at-min-focal-length max-aperture
	  max-aperture-at-max-focal-length max-aperture))
  (%make-lens :name name
	      :serial-number serial-number
	      :nicknames nicknames
	      :min-focal-length min-focal-length
	      :max-focal-length max-focal-length
	      :max-aperture-at-min-focal-length max-aperture-at-min-focal-length
	      :max-aperture-at-max-focal-length max-aperture-at-max-focal-length))
