(defpackage :tiff
  (:use :cl :binary-buffer)
  (:import-from :alexandria :array-index :array-length :if-let :when-let)
  (:export :tiff :tiff-ifds :tiff-regions :read-tiff :tag-value
	   :collect-ifds
           :*read-images*
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


(defparameter *read-images* nil
  "Read image data when parsing TIFF?")

(declaim (inline read-hex))
(defun read-hex (s start end)
  (parse-integer s :start start :end end :radix 16))


(defstruct ifd-entry
  (tag 0 :type (unsigned-byte 16) :read-only t)
  (type 0 :type (integer 1 12) :read-only t)
  (count 0 :type (unsigned-byte 32) :read-only t)
  ;; If count = 1, values is the single value. Otherwise, it's a vector of the values.
  ;; The exception is type ASCII, which may have a single string even with count > 1.
  (values nil :type (or atom simple-vector))
  (ifds (vector) :type (vector ifd)))

(defstruct ifd
  (name "" :type string :read-only t)
  (address 0 :type (unsigned-byte 32) :read-only t)
  (entries nil :type (simple-array ifd-entry (*)) :read-only t)
  (image nil :type (or null (simple-array (unsigned-byte 8) (*)))))

(defstruct region
  (start 0 :type array-index :read-only t)
  (end 0 :type array-index :read-only t)
  (description "" :type string :read-only t))

(defstruct tiff
  (big-endian-p nil :read-only t)
  (ifds (vector) :type (vector ifd))
  regions)

(declaim (ftype (function (tiff array-index array-index string)) note-region))
(defun note-region (f start len name)
  (push (make-region :start start :end (+ start len) :description name) (tiff-regions f)))

#+SBCL (declaim (sb-ext:freeze-type ifd ifd-entry region tiff))


(declaim (ftype (function (ifd (unsigned-byte 16)) (or null ifd-entry)) find-entry))

(defun find-entry (ifd tag)
  (find-if #'(lambda (e) (= (ifd-entry-tag e) tag)) (ifd-entries ifd)))


(declaim (ftype (function (tiff (unsigned-byte 16))
			  (or null ifd-entry))
		find-tag))

(defun find-tag (raw tag)
  (declare (optimize speed))
  (labels ((search-ifds (ifds)
	     (loop for ifd across ifds do (search-entries (ifd-entries ifd))))
	   (search-entries (entries)
	     (loop for e across entries do (check-entry e)))
	   (check-entry (e)
	     (when (= (ifd-entry-tag e) tag)
	       (return-from find-tag e))
	     (search-ifds (ifd-entry-ifds e))))
    (search-ifds (tiff-ifds raw))))

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


(declaim (ftype (function (tiff &key (:order (member :breadth-first :depth-first)))
			  (vector ifd))
		collect-ifds))

(defun collect-ifds (tiff &key (order :depth-first))
  (let ((result (make-array 0 :element-type 'ifd :fill-pointer t :adjustable t)))
    (labels ((dfs (ifds)
	       (loop for ifd across ifds do
		     (vector-push-extend ifd result)
		     (loop for entry across (ifd-entries ifd) do
			   (dfs (ifd-entry-ifds entry)))))
	     (bfs (ifds)
	       (error "BFS not implemenbted")))
      (ecase order
	(:depth-first (dfs (tiff-ifds tiff)))
	(:breadth-first (bfs (tiff-ifds tiff))))
      result)))


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

;; A few important tags
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +image-length+ 257)
  (defconstant +strip-offsets+ 273)
  (defconstant +rows-per-strip+ 278)
  (defconstant +strip-byte-counts+ 279))

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


(declaim (ftype (function ((or (unsigned-byte 32) (signed-byte 32))
			   (or (unsigned-byte 32) (signed-byte 32)))
			  rational)
		safe/))
(defun safe/ (n d)
  (if (zerop d)
      0
      (/ n d)))


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
			       (safe/ n d))))
	   (#.+SSHORT+ (get-s16 raw addr))
	   (#.+SLONG+ (get-s32 raw addr))
	   (#.+SRATIONAL+ (let ((n (get-s32 raw addr))
				(d (get-s32 raw (+ addr 4))))
			    (if (apex-tag-p tag)
				(apex-value n d)
				(safe/ n d))))
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
					 (safe/ n d)))))))
	   (#.+SRATIONAL+ (let ((xs (make-array count :element-type 'rational)))
			    (dotimes (i count xs)
			      (let ((n (get-s32 raw (+ addr (* i 8))))
				    (d (get-s32 raw (+ addr (* i 8) 4))))
				(setf (aref xs i)
				      (if (apex-tag-p tag)
					  (apex-value n d)
					  (safe/ n d)))))))
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
 (ftype (function (binary-buffer tiff cons (unsigned-byte 32)) (values ifd (unsigned-byte 32))) parse-ifd)
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
      (setf (tiff-ifds info) (read-ifds bytes info "IFD" (get-u32 bytes 4)))
      (setf (tiff-regions info) (sort (coerce (tiff-regions info) 'vector) #'region-precedes-p))
      info)))


(declaim (ftype (function (binary-buffer tiff string (unsigned-byte 32))
			  (vector ifd))
		read-ifds))

(defun read-ifds (buf tiff ifd-type addr)
  "Read IFDs, returning a vector."
  (labels ((next-ifd (ifd-num addr)
	     (handler-case
		 (multiple-value-bind (ifd next-addr)
		     (parse-ifd buf tiff (cons ifd-type ifd-num) addr)
		   (cons ifd (if (zerop next-addr)
				 nil
				 (next-ifd (+ ifd-num 1) next-addr))))
	       (end-of-file () (format t "warning: EOF while reading ~A~D at 0x~8,'0X~%" ifd-type ifd-num addr)))))
    (coerce (next-ifd 0 addr) 'vector)))

(declaim (ftype (function (binary-buffer (or null vector) (or null vector))
			  (or null (simple-array (unsigned-byte 8) (*))))
		read-image))

(defun read-image (buf strip-offsets strip-byte-counts)
  (when strip-offsets
    (let* ((len (reduce #'+ strip-byte-counts))
	   (img (make-array len :element-type '(unsigned-byte 8))))
      (loop for strip across strip-offsets
	    and count across strip-byte-counts
	    and offs = 0 then (+ offs count)
	    do
	    (setf (subseq img offs (+ offs count)) (get-bytes buf strip count)))
      img)))

(declaim (ftype (function (binary-buffer tiff string (unsigned-byte 32) (integer 1 100))
			  (vector ifd))
		read-sub-ifds))

(defun read-sub-ifds (buf tiff name voffs count)
  (let ((xoffs (get-u32 buf voffs)))
    (if (= count 1)
	(read-ifds buf tiff name xoffs)
	(let ((ifds (make-array count :element-type 'ifd)))
	  (dotimes (i count)
	    (let ((addr (get-u32 buf (+ xoffs (* 4 i)))))
	      (handler-case
		  (multiple-value-bind (ifd next-addr)
		      (parse-ifd buf tiff (cons name i) addr)
		    (unless (zerop next-addr)
		      (error "SubIFD with count ~S chained" count))
		    (setf (aref ifds i) ifd))
		(end-of-file () (format t "warning: EOF while reading ~A~D at 0x~8,'0X~%" name i addr)))))
	  ifds))))

(defun parse-ifd (buf raw name offs)
  "Parse IFD. Returns two values: the IFD and the address of the next IFD."
  (let* ((num-entries (get-u16 buf offs))
	 (entries (make-array num-entries :element-type 'ifd-entry))
	 (image-length 0)
	 (strip-offsets nil)
	 (strip-byte-counts nil))
    (note-region raw offs (+ 2 (* num-entries 12) 4)
		 (format nil "~a~a (~D entries, dec. addr: ~D)"
			 (car name) (cdr name) num-entries offs))
    (dotimes (i num-entries)
      (let* ((tag (get-u16 buf (+ offs (* i 12) 2)))
	     (type (get-u16 buf (+ offs (* i 12) 4)))
	     (count (get-u32 buf (+ offs (* i 12) 6)))
	     (entry (make-ifd-entry :tag tag :type type :count count))
	     (voffs (+ offs (* i 12) 10)))
	(case tag
	  (#.+image-length+
	   (unless (= count 1)
	     (error "ImageLength should have count 1, but has ~S" count))
	   (unless (zerop image-length)
	     (error "Multiple ImageLength tags encountered"))
	   (setq image-length (get-u32 buf voffs)))
	  (#.+strip-offsets+
	   (setq strip-offsets (make-array count :element-type '(unsigned-byte 32)))
	   (if (= count 1)
	       (setf (aref strip-offsets 0) (get-u32 buf voffs))
	       (let ((offs (get-u32 buf voffs)))
		 (dotimes (i count)
		   (setf (aref strip-offsets i) (get-u32 buf (+ offs (* 4 i))))))))
	  (#.+strip-byte-counts+
	   (setq strip-byte-counts (make-array count :element-type '(unsigned-byte 32)))
	   (if (= count 1)
	       (setf (aref strip-byte-counts 0) (get-u32 buf voffs))
	       (let ((offs (get-u32 buf voffs)))
		 (dotimes (i count)
		   (setf (aref strip-byte-counts i) (get-u32 buf (+ offs (* 4 i))))))))
	  (#x014A
	   (setf (ifd-entry-ifds entry) (read-sub-ifds buf raw "SubIFD" voffs count)))
	  (#x8769
	   (setf (ifd-entry-ifds entry) (read-sub-ifds buf raw "ExifIFD" voffs count)))
	  (#xA005
	   (setf (ifd-entry-ifds entry) (read-sub-ifds buf raw "InteropIFD" voffs count)))
	  (#x927c
	   (setf (ifd-entry-ifds entry) (read-sub-ifds buf raw "MakerNote" voffs 1))))
	(unless (ifd-value-inline-p type count)
	  (note-region raw (get-u32 buf voffs) (* count (tiff-type-size type))
		       (format nil "~a~a entry ~d: ~d ~A values" (car name) (cdr name) i count (tiff-type-name type))))
	(setf (ifd-entry-values entry)
	      (handler-case (get-values buf tag type count voffs)
					(end-of-file ()
					  (format t "warning: EOF while reading values of ~a~a entry ~a~%"
						  (car name) (cdr name) i)
					  :EOF)))
	(setf (aref entries i) entry)))
    (unless (= (length strip-offsets) (length strip-byte-counts))
      (error "StripOffsets / StripByteCounts mismatch: ~S ~S" (length strip-offsets) (length strip-byte-counts)))
    (dotimes (i (length strip-offsets))
      (note-region raw (aref strip-offsets i) (aref strip-byte-counts i)
		   (format nil "~a~a strip ~d/~d" (car name) (cdr name) (1+ i) (length strip-offsets))))
    (let ((next (get-u32 buf (+ offs 2 (* num-entries 12)))))
      (values (make-ifd :name (format nil "~A~A" (car name) (cdr name))
			:address offs
			:entries entries
			:image (if *read-images* (read-image buf strip-offsets strip-byte-counts) nil))
	      next))))

(defun get-ascii (buf offs num-bytes)
  "Get strings from ASCII field. Returns a list of strings (usually the list
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
