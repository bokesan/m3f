(defpackage :arrays
  (:documentation "Utility functions for arrays.")
  (:use :cl)
  (:import-from :alexandria :array-index :array-length)
  (:export :copy-bytes))

(in-package :arrays)
  
(declaim (ftype (function ((simple-array (unsigned-byte 8) 1)
			   array-index
			   (simple-array (unsigned-byte 8) 1)
			   array-index
			   array-length))
		copy-bytes))

(defun copy-bytes (src src-offs dest dest-offs count)
  "Copy elements from src to dest. Both must be simple arrays of rank 1
with element-type (unsigned-byte 8). If src and dest are the same vector,
the source and destination region must not overlap."
  (declare (optimize speed (safety 0)))
  (when (or (< src-offs 0) (< dest-offs 0)
	    (> (+ src-offs count) (length src))
	    (> (+ dest-offs count) (length dest)))
    ;; TODO: signal error more in line what AREF would signal
    (error "index out of bounds"))
  (dotimes (i count)
    (setf (aref dest (+ dest-offs i))
	  (aref src (+ src-offs i)))))
