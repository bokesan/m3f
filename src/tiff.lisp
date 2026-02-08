(defpackage :tiff
  (:use :cl :binary-buffer)
  (:import-from :alexandria :array-index :array-length :if-let :when-let)
  (:export :tiff :tiff-ifds :tiff-regions :read-tiff :tag-value
           :ifd :ifd-name :ifd-address :ifd-entries
           :ifd-entry :ifd-entry-tag :ifd-entry-type :ifd-entry-count :ifd-entry-values
	   :make-tag-table :*standard-tags*
	   :tiff-type-name
	   :metering-mode-name
           :decode-orientation :portrait-orientation-p
	   :volatile-tag-p :sensitive-tag-p
	   :read-hex
           :region :region-start :region-end :region-description))

(in-package :tiff)

(declaim (inline read-hex))
(defun read-hex (s start end)
  (parse-integer s :start start :end end :radix 16))


(defstruct ifd-entry
  (tag 0 :type (unsigned-byte 16) :read-only t)
  (type 0 :type (unsigned-byte 16) :read-only t)
  (count 0 :type (unsigned-byte 32) :read-only t)
  (value-address 0 :type (unsigned-byte 32) :read-only t)
  values)

(defstruct ifd
  (name "" :type string :read-only t)
  (address 0 :type (unsigned-byte 32) :read-only t)
  (entries nil :type (simple-array ifd-entry 1) :read-only t))

(defstruct region
  (start 0 :type array-index :read-only t)
  (end 0 :type array-index :read-only t)
  (description "" :type string :read-only t))

(defstruct tiff
  (big-endian-p nil :read-only t)
  ifds
  regions)

(declaim (ftype (function (tiff array-index array-index string)) note-region))
(defun note-region (f start len name)
  (push (make-region :start start :end (+ start len) :description name) (tiff-regions f)))

#+SBCL (declaim (sb-ext:freeze-type ifd ifd-entry region tiff))

(declaim (ftype (function (tiff (unsigned-byte 16))
			  (or null ifd-entry))
		find-tag))

(defun find-tag (raw tag)
  (declare (optimize speed))
  (map nil 
       #'(lambda (ifd)
	   (let ((e (find-if #'(lambda (entry) (= (ifd-entry-tag entry) tag))
			     (ifd-entries ifd))))
	     (when e
	       (return-from find-tag e))))
       (tiff-ifds raw)))

(defun tag-value (raw &rest tags)
  "Return the value of the first tag that is present and has a non-empty value."
  (declare (type tiff raw))
  (do ((tags tags (cdr tags))
       (default nil))
      ((null tags) default)
    (let ((tag (car tags)))
      (if (eq tag :default)
	  (progn (setq tags (cdr tags))
		 (setq default (car tags)))
	  (when-let ((entry (find-tag raw tag)))
            (let ((value (ifd-entry-values entry)))
	      (unless (or (null value) (equalp value ""))
		(return-from tag-value value))))))))

(declaim (ftype (function (region region) t) region-precedes-p))
(defun region-precedes-p (a b)
  (declare (optimize speed))
  (or (< (region-start a) (region-start b))
      (and (= (region-start a) (region-start b))
	   (< (region-end a) (region-end b)))))


(declaim (ftype (function (binary-buffer) tiff)
		parse-tiff))

(defun read-tiff (filename &key (max-bytes #xffffffff))
  "Read tiff file."
  (with-open-file (in filename :element-type '(unsigned-byte 8))
    (with-binary-buffer (raw in :chunk-size 8192)
      (parse-tiff raw))))

;; Data types:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +BYTE+ 1)
  (defconstant +ASCII+ 2)
  (defconstant +SHORT+ 3)
  (defconstant +LONG+ 4)
  (defconstant +RATIONAL+ 5)
  (defconstant +SBYTE+ 6)
  (defconstant +UNDEFINED+ 7)
  (defconstant +SSHORT+ 8)
  (defconstant +SLONG+ 9)
  (defconstant +SRATIONAL+ 10)
  (defconstant +FLOAT+ 11)
  (defconstant +DOUBLE+ 12))

(declaim (ftype (function ((unsigned-byte 16)) string) tiff-type-name))
(let ((names (vector "0" "BYTE" "ASCII" "SHORT" "LONG" "RATIONAL"
		     "SBYTE" "UNDEFINED" "SSHORT" "SLONG" "SRATIONAL"
		     "FLOAT" "DOUBLE")))
  (declare (type (simple-array string (#.(1+ +DOUBLE+))) names))
  (defun tiff-type-name (type)
    (if (<= 0 type +DOUBLE+)
	(aref names type)
	(format nil "~D" type))))

(defun tiff-type-size (type)
  (case type
    ((#.+BYTE+ #.+ASCII+ #.+SBYTE+ #.+UNDEFINED+) 1)
    ((#.+SHORT+ #.+SSHORT+) 2)
    ((#.+LONG+ #.+SLONG+ #.+FLOAT+) 4)
    ((#.+RATIONAL+ #.+SRATIONAL+ #.+DOUBLE+) 8)
    (t (error "unknown type: ~S" type))))

(declaim (inline ifd-value-inline-p))
(defun ifd-value-inline-p (type count)
  (<= (* count (tiff-type-size type)) 4))

(declaim (ftype (function (binary-buffer array-index array-index) list) get-ascii))

(declaim (ftype (function (binary-buffer (unsigned-byte 16) (unsigned-byte 16) (unsigned-byte 32) (unsigned-byte 32)) t) get-values))
(defun get-values (raw tag type count addr)
  (let ((addr (if (ifd-value-inline-p type count) addr (get-u32 raw addr))))
    (declare (optimize speed))
    (case count
      (0 nil)
      (1 (case type
	   ((#.+BYTE+ #.+UNDEFINED+) (get-u8 raw addr))
	   (#.+SBYTE+ (get-s8 raw addr))
	   (#.+ASCII+ (let ((b (get-u8 raw addr)))
			(if (zerop b)
			    ""
			    (string (code-char b)))))
	   (#.+SHORT+ (get-u16 raw addr))
	   (#.+LONG+ (get-u32 raw addr))
	   (#.+RATIONAL+ (let ((n (get-u32 raw addr))
			       (d (get-u32 raw (+ addr 4))))
			   (if (apex-tag-p tag)
			       (apex-value n d)
			       (/ n d))))
	   (#.+SSHORT+ (get-s16 raw addr))
	   (#.+SLONG+ (get-s32 raw addr))
	   (#.+SRATIONAL+ (let ((n (get-s32 raw addr))
				(d (get-s32 raw (+ addr 4))))
			    (if (apex-tag-p tag)
				(apex-value n d)
				(/ n d))))
	   (#.+FLOAT+ (get-float raw addr))
	   (#.+DOUBLE+ (get-double raw addr))
	   (t 'unknown-type)))
      (t (case type
	   ((#.+BYTE+ #.+UNDEFINED+)
	    (get-bytes raw addr count))
	   (#.+ASCII+ (let ((strings (get-ascii raw addr count)))
			(if (null (cdr strings))
			    (car strings)
			    (coerce strings 'vector))))
	   (#.+SBYTE+ (let ((xs (make-array count :element-type '(signed-byte 8))))
			(dotimes (i count xs)
			  (setf (aref xs i) (get-s8 raw (+ addr i))))))
	   (#.+SHORT+ (let ((xs (make-array count :element-type '(unsigned-byte 16))))
			(dotimes (i count xs)
			  (setf (aref xs i) (get-u16 raw (+ addr (* i 2)))))))
	   (#.+LONG+ (let ((xs (make-array count :element-type '(unsigned-byte 32))))
		       (dotimes (i count xs)
			 (setf (aref xs i) (get-u32 raw (+ addr (* i 4)))))))
	   (#.+RATIONAL+ (let ((xs (make-array count :element-type 'rational)))
			   (dotimes (i count xs)
			     (let ((n (get-u32 raw (+ addr (* i 8))))
				   (d (get-u32 raw (+ addr (* i 8) 4))))
			       (setf (aref xs i)
				     (if (apex-tag-p tag)
					 (apex-value n d)
					 (/ n d)))))))
	   (#.+SRATIONAL+ (let ((xs (make-array count :element-type 'rational)))
			    (dotimes (i count xs)
			      (let ((n (get-s32 raw (+ addr (* i 8))))
				    (d (get-s32 raw (+ addr (* i 8) 4))))
				(setf (aref xs i)
				      (if (apex-tag-p tag)
					  (apex-value n d)
					  (/ n d)))))))
	   (#.+SSHORT+ (let ((xs (make-array count :element-type '(signed-byte 16))))
			 (dotimes (i count xs)
			   (setf (aref xs i) (get-s16 raw (+ addr (* i 2)))))))
	   (#.+SLONG+ (let ((xs (make-array count :element-type '(signed-byte 32))))
			(dotimes (i count xs)
			  (setf (aref xs i) (get-s32 raw (+ addr (* i 4)))))))
	   (#.+FLOAT+ (let ((xs (make-array count :element-type 'short-float)))
			(dotimes (i count xs)
			  (setf (aref xs i) (get-float raw (+ addr (* i 4)))))))
	   (#.+DOUBLE+ (let ((xs (make-array count :element-type 'double-float)))
			 (dotimes (i count xs)
			   (setf (aref xs i) (get-double raw (+ addr (* i 8)))))))
	   (t 'error-unknown-type))))))


(declaim
 (ftype (function (binary-buffer tiff cons (unsigned-byte 32)) (values ifd list)) parse-ifd)
 (ftype (function (cons) cons) next-ifd-name))

(defun next-ifd-name (name)
  (cons (car name) (1+ (cdr name))))

(defun parse-tiff (bytes)
  (let ((b0 (get-u8 bytes 0))
	(b1 (get-u8 bytes 1)))
    (cond ((and (= b0 (char-code #\I))
		(= b1 (char-code #\I)))
	   (setf (binary-buffer-big-endian-p bytes) nil))
	  ((and (= b0 (char-code #\M))
		(= b1 (char-code #\M)))
	   (setf (binary-buffer-big-endian-p bytes) t))
	  (t (error "invalid byte order marker: ~2,'0x ~2,'0x" b0 b1)))
    (let* ((info (make-tiff :big-endian-p (binary-buffer-big-endian-p bytes)))
	   (magic (get-u16 bytes 2))
	   (ifds nil))
      (declare (type list ifds))
      (unless (= magic 42)
	(error "invalid magic number: ~a" magic))
      (note-region info 0 2 "BOM")
      (note-region info 2 2 "Magic")
      (note-region info 4 4 "First IFD address")
      (do ((rest-ifds (list (cons (cons "IFD" 0) (get-u32 bytes 4))) (cdr rest-ifds)))
	  ((null rest-ifds))
	(destructuring-bind (name . addr) (car rest-ifds)
	  (handler-case
	      (multiple-value-bind (ifd more-ifds)
		  (parse-ifd bytes info name addr)
		(push ifd ifds)
		(setq rest-ifds (append rest-ifds more-ifds)))
	    (end-of-file () (format t "warning: EOF while reading IFD ~S at 0x~8,'0X~%" name addr)))))
      (setf (tiff-ifds info) (sort (coerce ifds 'vector) #'< :key #'ifd-address))
      (setf (tiff-regions info) (sort (coerce (tiff-regions info) 'vector) #'region-precedes-p))
      info)))

(defun parse-ifd (buf raw name offs)
  "Parse IFD, returning a list of other IFDs to parse as second value."
  (let* ((num-entries (get-u16 buf offs))
	 (entries (make-array num-entries :element-type 'ifd-entry))
	 (ifds nil)
	 (image-length 0)
	 (strip-offsets nil)
	 (strip-byte-counts nil))
    (note-region raw offs (+ 2 (* num-entries 12) 4)
		 (format nil "~a~a (~D entries, dec. addr: ~D)"
			 (car name) (cdr name) num-entries offs))
    (dotimes (i num-entries)
      (let ((tag (get-u16 buf (+ offs (* i 12) 2)))
	    (type (get-u16 buf (+ offs (* i 12) 4)))
	    (count (get-u32 buf (+ offs (* i 12) 6)))
	    (voffs (+ offs (* i 12) 10)))
	(case tag
	  (257
	   (unless (= count 1)
	     (error "ImageLength should have count 1, but has ~S" count))
	   (unless (zerop image-length)
	     (error "Multiple ImageLength tags encountered"))
	   (setq image-length (get-u32 buf voffs)))
	  (273 ; StripOffsets
	   (setq strip-offsets (make-array count :element-type '(unsigned-byte 32)))
	   (if (= count 1)
	       (setf (aref strip-offsets 0) (get-u32 buf voffs))
	       (let ((offs (get-u32 buf voffs)))
		 (dotimes (i count)
		   (setf (aref strip-offsets i) (get-u32 buf (+ offs (* 4 i))))))))
	  (278 ; RowsPerStrip
	   )
	  (279 ; StripByteCounts
	   (setq strip-byte-counts (make-array count :element-type '(unsigned-byte 32)))
	   (if (= count 1)
	       (setf (aref strip-byte-counts 0) (get-u32 buf voffs))
	       (let ((offs (get-u32 buf voffs)))
		 (dotimes (i count)
		   (setf (aref strip-byte-counts i) (get-u32 buf (+ offs (* 4 i))))))))
	  (#x14A
	   (let ((xoffs (get-u32 buf voffs)))
	     (if (= count 1)
		 (push (cons (cons "SubIFD" 0) xoffs) ifds)
		 (progn
		   (note-region raw xoffs (* 4 count) (format nil "~a~a entry ~d: ~d SubIFD offsets" (car name) (cdr name) i count))
		   (do ((k (- count 1) (- k 1)))
		       ((< k 0))
		     (push (cons (cons "SubIFD" k) (get-u32 buf (+ xoffs (* 4 k)))) ifds))))))
	  (#x8769
	   (let ((xoffs (get-u32 buf voffs)))
	     (if (= count 1)
		 (push (cons (cons "ExifIFD" 0) xoffs) ifds)
		 (progn
		   (note-region raw xoffs (* 4 count) (format nil "~a~a entry ~d: ~d ExifIFD offsets" (car name) (cdr name) i count))
		   (do ((k (- count 1) (- k 1)))
		       ((< k 0))
		     (push (cons (cons "ExifIFD" k) (get-u32 buf (+ xoffs (* 4 k)))) ifds))))))
	  (#xA005
	   (let ((xoffs (get-u32 buf voffs)))
	     (if (= count 1)
		 (push (cons (cons "InteropIFD" 0) xoffs) ifds)
		 (progn
		   (note-region raw xoffs (* 4 count) (format nil "~a~a entry ~d: ~d InteropIFD offsets" (car name) (cdr name) i count))
		   (do ((k (- count 1) (- k 1)))
		       ((< k 0))
		     (push (cons (cons "InteropIFD" k) (get-u32 buf (+ xoffs (* 4 k)))) ifds))))))	   
	  (#x927c
	   (let ((xoffs (get-u32 buf voffs)))
	     (push (cons (cons "MakerNote" 0) xoffs) ifds))))
	(unless (ifd-value-inline-p type count)
	  (note-region raw (get-u32 buf voffs) (* count (tiff-type-size type))
		       (format nil "~a~a entry ~d: ~d ~A values" (car name) (cdr name) i count (tiff-type-name type))))
	(setf (aref entries i)
	      (make-ifd-entry :tag tag :type type :count count
			      :value-address (if (ifd-value-inline-p type count)
						 voffs
						 (get-u32 buf voffs))
			      :values (handler-case (get-values buf tag type count voffs)
					(end-of-file ()
					  (format t "warning: EOF while reading values of ~a~a entry ~a~%"
						  (car name) (cdr name) i)
					  :EOF))))))
    (unless (= (length strip-offsets) (length strip-byte-counts))
      (error "StripOffsets / StripByteCounts mismatch: ~S ~S" (length strip-offsets) (length strip-byte-counts)))
    (dotimes (i (length strip-offsets))
      (note-region raw (aref strip-offsets i) (aref strip-byte-counts i)
		   (format nil "~a~a strip ~d/~d" (car name) (cdr name) (1+ i) (length strip-offsets))))
    (let ((next (get-u32 buf (+ offs 2 (* num-entries 12)))))
      (values (make-ifd :name (format nil "~A~A" (car name) (cdr name))
			:address offs
			:entries entries)
	      (if (zerop next)
		  ifds
		  (cons (cons (next-ifd-name name) next) ifds))))))

(defun get-ascii (buf offs num-bytes)
  "Get strings from ASCII field. Returns a list of string (usually the list
contains only one element)."
  (let ((strings nil)
	(bytes (get-bytes buf offs num-bytes))
	(offset 0))
    (declare (optimize speed))
    (loop
      (when (>= offset num-bytes)
	(return (nreverse strings)))
      (let ((nul (position 0 bytes :start offset)))
	(cond ((not nul)
	       (push (map 'string #'code-char (subseq bytes offset)) strings)
	       (setq offset num-bytes))
	      (t
	       (push (map 'string #'code-char (subseq bytes offset nul)) strings)
	       (setq offset (1+ nul))))))))
