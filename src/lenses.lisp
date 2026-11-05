(defpackage :lenses
  (:use :cl :utils)
  (:export :lens :lens-p :make-lens
	   :lens-focal-length
	   :lens-max-aperture
	   :lens-min-focal-length
	   :lens-max-focal-length
	   :lens-max-aperture-at-min-focal-length
	   :lens-max-aperture-at-max-focal-length
	   :lens-name :lens-make
	   :lens-serial-number
	   :lens-nicknames
	   :*lenses*))

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
  (name "" :type string)
  (make nil :type (or null string))
  (nicknames nil :type list)
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
			 make
			 serial-number
			 nicknames)
  (declare (type string name)
	   (type (or null real)
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
  (if focal-length
      (let ((fl (rationalize focal-length))
	    (ap (rationalize max-aperture)))
	(setf min-focal-length fl
	      max-focal-length fl
	      max-aperture-at-min-focal-length ap
	      max-aperture-at-max-focal-length ap))
      (setf min-focal-length (rationalize min-focal-length)
	    max-focal-length (rationalize max-focal-length)
	    max-aperture-at-min-focal-length (rationalize max-aperture-at-min-focal-length)
	    max-aperture-at-max-focal-length (rationalize max-aperture-at-max-focal-length)))
  (%make-lens :name name
	      :make make
	      :serial-number serial-number
	      :nicknames nicknames
	      :min-focal-length min-focal-length
	      :max-focal-length max-focal-length
	      :max-aperture-at-min-focal-length max-aperture-at-min-focal-length
	      :max-aperture-at-max-focal-length max-aperture-at-max-focal-length))


(defun nullify-empty-sequence (s)
  (if (zerop (length s)) nil s))

(defun stringify (obj)
  (nullify-empty-sequence
   (typecase obj
     (string obj)
     (symbol (string obj))
     (t (format nil "~A" obj)))))

(defun as-rational (obj)
  (etypecase obj
    (rational obj)
    (float (rationalize obj))
    (string (or (parse-as-rational obj) 0))
    (null 0)))

(defun parse-lens (form)
  "Parse lens from a form ((key1 value1 ...) (key2 value2 ...) ...)."
  (let ((lens (%make-lens)))
    (loop for (key value . other-values) in form do
	  (case key
	    (name
	     (setf (lens-name lens) (stringify value))
	     (push (mapcar #'stringify other-values) (lens-nicknames lens)))
	    (nicknames
	     (push (stringify value) (lens-nicknames lens))
	     (push (mapcar #'stringify other-values) (lens-nicknames lens)))
	    (serial-number
	     (setf (lens-serial-number lens) (stringify value)))
	    (make
	     (setf (lens-make lens) (stringify value)))
	    (focal-length
	     (let* ((fl1 (as-rational value))
		    (fl2 (if other-values (as-rational (car other-values)) fl1)))
	       (setf (lens-min-focal-length lens) fl1
		     (lens-max-focal-length lens) fl2)))
	    (max-aperture
	     (let* ((ap1 (as-rational value))
		    (ap2 (if other-values (as-rational (car-other-values)) ap1)))
	       (setf (lens-max-aperture-at-min-focal-length lens) ap1
		     (lens-max-aperture-at-max-focal-length lens) ap2)))))))

(declaim (inline first-char last-char))

(defun first-char (s)
  "First character of non-empty string."
  (char s 0))

(defun last-char (s)
  "Last character of non-empty string."
  (char s (- (length s) 1)))


(defun read-config (stream)
  "Read config file, returning a hash table containing the headers as keys
and alists as values.
Headers, keys, and values are always strings."
  (let ((table (make-hash-table :test #'equalp))
	(whitespace '(#\Space #\Tab))
	(whitespace-and-brackets '(#\Space #\Tab #\[ #\])))
    (labels ((accept (header entries)
	       (declare (type string header)
			(type list entries))
	       (when entries
		 (push (nreverse entries) (gethash header table nil))))
	     
	     (parse (header entries)
	       (declare (type string header)
			(type list entries))
	       (let ((line (read-line stream nil)))
		 (if (not line)
		     (accept header entries)
		     (let ((line (string-trim whitespace line)))
		       (cond ((or (zerop (length line)) (char= (first-char line) #\#))
			      (parse header entries))
			     ((char= (first-char line) #\[)
			      (unless (char= (last-char line) #\])
				(error "[header] expected. ~S" line))
			      (accept header entries)
			      (parse (string-trim whitespace-and-brackets line) nil))
			     (t (parse
				 header
				 (cons (let ((sep (position #\= line)))
					 (if (not sep)
					     (cons line "")
					     (cons (string-right-trim whitespace (subseq line 0 sep))
						   (string-left-trim whitespace (subseq line (+ sep 1))))))
				       entries)))))))))
      (parse "" nil)
      table)))


(defun read-lenses (stream)
  (uiop:with-safe-io-syntax (:package :lenses)
    (mapcar #'parse-lens
	    (uiop:slurp-stream-forms stream :count 1000))))

(defun lens-matches-p (lens &key name)
  (when (stringp name)
    (or (string-equal (lens-name lens) name)
	(some #'(lambda (nick) (string-equal nick name)) (lens-nicknames lens)))))


(defun find-lenses (lenses &key name)
  (map 'list
       #'(lambda (lens) (lens-matches-p lens :name name))
       lenses))

(defparameter *sample-lenses*
  (list (make-lens "NIKKOR-H 1:2.8 f=75mm"
		   :make "Nikon"
		   :focal-length 75
		   :max-aperture 2.8
		   :serial-number "776068"
		   :nicknames '("75P"))))

(defvar *lenses*
  (let ((table (make-hash-table :test #'equalp)))
    (mapc #'(lambda (lens)
	      (mapc #'(lambda (nick) (setf (gethash nick table) lens)) (lens-nicknames lens)))
	  *sample-lenses*)
    table))
