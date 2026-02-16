(in-package :tiff)

(defconstant +BLOCK-SIZE+ 4096)


(defstruct (writer (:constructor %make-writer))
  (stream nil :type stream :read-only t)
  (big-endian-p nil :type boolean :read-only t)
  (buf nil :type (vector (unsigned-byte 8)) :read-only t)
  (buf-start 0 :type (unsigned-byte 32))
  (holes nil :type list))

#+sbcl (declaim (sb-ext:freeze-type writer))

(defun make-writer (stream &key big-endian)
  (%make-writer
   :stream stream
   :big-endian-p big-endian
   :buf (make-array +BLOCK-SIZE+ :element-type '(unsigned-byte 8) :fill-pointer 0)))

(declaim (inline writer-current-pos))
(defun writer-current-pos (writer)
  (+ (writer-buf-start writer) (length (writer-buf writer))))

(defun flush-when-possible (writer)
  "Flush the buffer when +BLOCK-SIZE+ bytes are available for writing."
  (let* ((buf (writer-buf writer))
	 (len (length buf)))
    (when (and (>= len +BLOCK-SIZE+)
	       (notany #'(lambda (addr) (< (- addr (writer-buf-start writer)) +BLOCK-SIZE+))
		       (writer-holes writer)))
      (write-sequence buf (writer-stream writer) :end +BLOCK-SIZE+)
      (incf (writer-buf-start writer) +BLOCK-SIZE+)
      (decf len +BLOCK-SIZE+)
      (setf (fill-pointer buf) len)
      (dotimes (i len)
	(setf (aref buf i) (aref buf (+ i +BLOCK-SIZE+)))))))

(defun writer-flush (writer)
  (unless (null (writer-holes writer))
    (error "output still has holes ~S" writer))
  (write-sequence (writer-buf writer) (writer-stream writer))
  (setf (fill-pointer (writer-buf writer)) 0))

(declaim (ftype (function (writer (unsigned-byte 32) (unsigned-byte 32))) set-u32))

(defun set-u32 (writer pos value)
  (let ((pos (- pos (writer-buf-start writer)))
	(buf (writer-buf writer))
	(b0 (logand value #xFF))
	(b1 (logand (floor value #x100) #xFF))
	(b2 (logand (floor value #x10000) #xFF))
	(b3 (logand (floor value #x1000000))))
    (unless (<= 0 pos (- (length buf) 4))
      (error "index ~S outside buffer of writer ~S" pos writer))
    (cond ((writer-big-endian-p writer)
	   (setf (aref buf pos) b3)
	   (setf (aref buf (+ pos 1)) b2)
	   (setf (aref buf (+ pos 2)) b1)
	   (setf (aref buf (+ pos 3)) b0))
	  (t
	   (setf (aref buf pos) b0)
	   (setf (aref buf (+ pos 1)) b1)
	   (setf (aref buf (+ pos 2)) b2)
	   (setf (aref buf (+ pos 3)) b3)))))


(declaim (ftype (function (writer (unsigned-byte 8))) write-u8))
(declaim (ftype (function (writer (unsigned-byte 16))) write-u16))
(declaim (ftype (function (writer (unsigned-byte 32))) write-u32))
(declaim (ftype (function (writer (signed-byte 8))) write-s8))
(declaim (ftype (function (writer (signed-byte 16))) write-s16))
(declaim (ftype (function (writer (signed-byte 32))) write-s32))

(defun write-u8 (writer byte)
  (vector-push-extend byte (writer-buf writer) 1024)
  (flush-when-possible writer))

(defun write-s8 (writer byte)
  (write-u8 writer (if (minusp byte) (+ 256 byte) byte)))

(defun write-u16 (writer word)
  (multiple-value-bind (b1 b0)
      (floor word 256)
    (cond ((writer-big-endian-p writer)
	   (write-u8 writer b1)
	   (write-u8 writer b0))
	  (t
	   (write-u8 writer b0)
	   (write-u8 writer b1)))))

(defun write-u32 (writer value)
  (let ((b0 (logand value #xFF))
	(b1 (logand (floor value #x100) #xFF))
	(b2 (logand (floor value #x10000) #xFF))
	(b3 (logand (floor value #x1000000))))
    (cond ((writer-big-endian-p writer)
	   (write-u8 writer b3)
	   (write-u8 writer b2)
	   (write-u8 writer b1)
	   (write-u8 writer b0))
	  (t
	   (write-u8 writer b0)
	   (write-u8 writer b1)
	   (write-u8 writer b2)
	   (write-u8 writer b3)))))

(defun write-s16 (writer n)
  (write-u16 writer (if (minusp n) (+ #x10000 n) n)))

(defun write-s32 (writer n)
  (write-u32 writer (if (minusp n) (+ #x100000000 n) n)))

(defun writer-align-2 (writer)
  (when (oddp (writer-current-pos writer))
    (write-u8 writer 0)))


(declaim
 (ftype (function (writer) (unsigned-byte 32)) write-forward-ref)
 (ftype (function (writer (unsigned-byte 32)) (unsigned-byte 32)) record-forward-ref))
 

(defun write-forward-ref (writer)
  (let ((ref-addr (writer-current-pos writer)))
    (push ref-addr (writer-holes writer))
    (write-u32 writer 0)
    ref-addr))

(defun record-forward-ref (writer offset)
  (let ((ref-addr (+ (writer-current-pos writer) offset)))
    (push ref-addr (writer-holes writer))
    ref-addr))


(declaim (ftype (function (writer (unsigned-byte 32))) resolve-forward-ref))

(defun resolve-forward-ref (writer ref)
  (setf (writer-holes writer) (delete ref (writer-holes writer)))
  (set-u32 writer ref (writer-current-pos writer)))


(defun write-rational (writer r)
  (write-u32 writer (numerator r))
  (write-u32 writer (denominator r)))

(defun write-srational (writer r)
  (write-s32 writer (numerator r))
  (write-s32 writer (denominator r)))


(declaim
 (ftype (function (writer tiff)) write-tiff)
 (ftype (function (writer ifd) (or null (unsigned-byte 32))) write-ifd)
 (ftype (function (writer ifd (unsigned-byte 32))) write-ifd-image)
 (ftype (function (writer (vector ifd) (unsigned-byte 32)) hash-table) write-ifds))

(defun write-ifds (writer ifds ref)
  "Write IFDs, returning a hash table of IFD -> image ref address."
  (let ((last-ifd-index (- (length ifds) 1))
	(image-refs (make-hash-table :test #'eq)))
    (dotimes (i num-ifds image-refs)
      (resolve-forward-ref writer ref)
      (setf (gethash (aref ifds i) image-refs)
	    (write-ifd writer (aref ifds i)))
      (if (= i last-ifd-index)
	  (write-u32 writer 0)
	  (setf ref (write-forward-ref writer))))))

(defun write-tiff (writer tiff)
  "Write TIFF to WRITER."
  (write-u16 writer (if (tiff-big-endian-p tiff) #x4D4D #x4949))
  (write-u16 writer 42)
  (let ((image-refs (write-ifds writer (tiff-ifds tiff) (write-forward-ref writer))))
    ;; TODO: sort images by length, writing the smaller ones first
    (loop for ifd across (collect-ifds tiff) do
	  (when-let (ref (gethash ifd image-refs))
	    (write-ifd-image writer ifd refs)))))


(defun save-tiff (filename tiff)
  (with-open-file (out filename :direction :output :if-exists :error :element-type '(unsigned-byte 8))
    (let ((writer (make-writer out :big-endian (tiff-big-endian-p tiff))))
      (write-tiff writer tiff)
      (writer-flush writer))))
      

(declaim
 (ftype (function (writer ifd-entry) (or null (unsigned-byte 32))) write-ifd-entry)
 (ftype (function (writer ifd-entry (unsigned-byte 32))) write-ifd-entry-values))


(defun write-ifd (writer ifd)
  "Write IFD. TODO: return image ref address or nil if no image."
  (writer-align-2 writer)
  (write-u16 writer (length (ifd-entries ifd)))
  (let* ((strip-offsets-ref nil)
	 (value-refs (map 'vector
			  #'(lambda (e)
			      (when (and (= (ifd-entry-tag e) 273) (integerp (ifd-entry-values e)))
				(setf strip-offsets-ref (record-forward-ref writer 8)))
			      (write-ifd-entry writer e))
			  (ifd-entries ifd))))
    (map nil
	 #'(lambda (e ref)
	     (when ref
	       (when (= (ifd-entry-tag e) 273)
		 (setf strip-offsets-ref (record-forward-ref writer 0)))
	       (write-ifd-entry-values writer e ref)))
	 (ifd-entries ifd)
	 value-refs)
    strip-offsets-ref))

(defun write-ifd-entry (writer entry)
  "Write IFD entry.
If the value if too long to write it inline, returns the adress of the forward reference to the value.
Otherwise, returns nil."
  (write-u16 writer (ifd-entry-tag entry))
  (write-u16 writer (ifd-entry-type entry))
  (write-u32 writer (ifd-entry-count entry))
  (if (ifd-value-inline-p (ifd-entry-type entry) (ifd-entry-count entry))
      (progn (write-inline-value writer entry)
	     nil)
      (write-forward-ref writer)))

(defun write-inline-value (writer entry)
  (let ((count (ifd-entry-count entry))
	(val (ifd-entry-values entry)))
    (ecase (ifd-entry-type entry)
      ((#.+BYTE+ #.+UNDEFINED+)
       (write-bytes writer val)
       (dotimes (i (- 4 count))
	 (write-u8 writer 0)))
      (#.+ASCII+
       (cond ((stringp val)
	      (loop for c across val do
		    (write-u8 writer (char-code c)))
	      (dotimes (i (- 4 (length val)))
		(write-u8 writer 0)))
	     (t
	      (let ((num-written 0))
		(loop for str across val do
		      (loop for c across str do
			    (write-u8 writer (char-code c)))
		      (write-u8 writer 0)
		      (incf num-written (+ (length str) 1)))
		(dotimes (i (- 4 num-written))
		  (write-u8 writer 0))))))
      (#.+SHORT+
       (ecase count
	 (1 (write-u16 writer val)
	    (write-u16 writer 0))
	 (2 (write-u16 writer (aref val 0))
	    (write-u16 writer (aref val 1)))))
      (#.+SSHORT+
       (ecase count
	 (1 (write-s16 writer val)
	    (write-s16 writer 0))
	 (2 (write-s16 writer (aref val 0))
	    (write-s16 writer (aref val 1)))))
      (#.+LONG+ (write-u32 writer val))
      (#.+SLONG+ (write-s32 writer val))
      ;; TODO: +FLOAT+
      )))

(defun write-bytes (writer bs)
  (if (vectorp bs)
      (loop for b across bs do (write-u8 writer b))
      (write-u8 writer bs)))
  
(defun write-ifd-entry-values (writer entry ref)
  (writer-align-2 writer)
  (resolve-forward-ref writer ref)
  (let ((values (ifd-entry-values entry)))
    (ecase (ifd-entry-type entry)
      ((#.+BYTE+ #.+UNDEFINED+)
       (write-bytes writer values))
      (#.+SBYTE+
       (write-sbytes writer values))
      (#.+ASCII+
       (cond ((stringp values)
	      (loop for c across values do
		    (write-u8 writer (char-code c)))
	      (write-u8 writer 0))
	     (t
	      (loop for str across values do
		    (loop for c across str do
			  (write-u8 writer (char-code c)))
		    (write-u8 writer 0)))))
      (#.+SHORT+
       (map nil #'(lambda (w) (write-u16 writer w)) values))
      (#.+LONG+
       (map nil #'(lambda (w) (write-u32 writer w)) values))
      (#.+RATIONAL+
       (if (vectorp values)
	   (map nil #'(lambda (r) (write-rational writer r)) values)
	   (write-rational writer values)))
      (#.+SSHORT+
       (map nil #'(lambda (w) (write-s16 writer w)) values))
      (#.+SLONG+
       (map nil #'(lambda (w) (write-s32 writer w)) values))
      (#.+SRATIONAL+
       (if (vectorp values)
	   (map nil #'(lambda (r) (write-srational writer r)) values)
	   (write-srational writer values)))
      ;; TODO: +FLOAT+ +DOUBLE+
      )))

(defun test-copy ()
  (let* ((*read-images* t)
	 (tiff (read-tiff "B0001400.3FR")))
    (save-tiff "B0001400-copy.3FR" tiff)))
