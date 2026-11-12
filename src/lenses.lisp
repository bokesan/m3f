(defpackage :lenses
  (:use :cl :utils)
  (:export :lens :lens-p
	   :from-config
	   :lens-focal-length
	   :lens-max-aperture
	   :lens-min-focal-length
	   :lens-max-focal-length
	   :lens-max-aperture-at-min-focal-length
	   :lens-max-aperture-at-max-focal-length
	   :lens-name :lens-make
	   :lens-serial-number))

(in-package :lenses)


(defstruct lens
  (name "" :type string)
  (make nil :type (or null string))
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

(defun from-config (config)
  (declare (type hash-table config))
  (let ((data (gethash "lens" config))
	(lenses (make-hash-table :test #'equalp)))
    (loop for alist in data
	  do
	  (labels ((get-field (key)
		     (declare (type string key))
		     (cdr (assoc key alist :test #'equalp)))
		   (first-value (list)
		     (if list
			 (parse-as-rational (car list))
			 0))
		   (second-value (list)
		     (if (null (cdr list))
			 (first-value list)
			 (parse-as-rational (cadr list)))))
	    (let ((tags (split-and-trim "," (get-field "tag")))
		  (name (get-field "name"))
		  (make (get-field "make"))
		  (snr (get-field "serial number"))
		  (fl (split-and-trim "-" (get-field "focal length")))
		  (ap (split-and-trim "-" (get-field "max aperture"))))
	      (let ((lens (make-lens :name name
				     :make make
				     :serial-number snr
				     :min-focal-length (first-value fl)
				     :max-focal-length (second-value fl)
				     :max-aperture-at-min-focal-length (first-value ap)
				     :max-aperture-at-max-focal-length (second-value ap))))
		(loop for tag in tags
		      do (setf (gethash tag lenses) lens))))))
    lenses))
