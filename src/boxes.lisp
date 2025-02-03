(defpackage :boxes
  (:use :cl :binary-buffer)
  (:export :read-boxes))

(in-package :boxes)

(defstruct box
  (size 0 :type unsigned-byte :read-only t)
  content-length
  (offset 0 :type unsigned-byte :read-only t)
  (type "????" :type string :read-only t))

(declaim (ftype (function ((unsigned-byte 8)) standard-char) byte-to-ascii))
(defun byte-to-ascii (byte)
  (if (<= 32 byte 126)
      (code-char byte)
      #\.))

(defun parse-box (buf offset)
  (declare (type binary-buffer buf))
  (handler-case
      (let* ((size32 (get-u32 buf offset))
	     (type (map 'string #'byte-to-ascii (get-bytes buf (+ offset 4) 4)))
	     (size (if (= size32 1) (get-u64 buf (+ offset 8)) size32)))
	(format t "box at #x~8,'0X: type ~A, size ~D~%"
		offset type size)
	(values (make-box :size size
			  :content-length (- size (if (= size32 1) 16 8))
			  :type type
			  :offset offset)
		t))
    (end-of-file () (values nil nil))))

    
(defun parse-boxes (buf)
  (declare (type binary-buffer buf))
  (let ((boxen nil)
	(offset 0))
    (loop
       (multiple-value-bind (box more-p)
	   (parse-box buf offset)
	 (when box
	   (push box boxen)
	   (incf offset (box-size box)))
	 (unless (and box more-p)
	   (return-from parse-boxes (nreverse boxen)))))))

(defun read-boxes (filename)
  "Read boxes/atoms from filename."
  (with-open-file (in filename :element-type '(unsigned-byte 8))
    (with-binary-buffer (raw in :chunk-size 8192 :big-endian-p t)
      (parse-boxes raw))))
