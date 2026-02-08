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

(defun writer-align-2 (writer)
  (when (oddp (writer-current-pos writer))
    (write-u8 writer 0)))


(declaim (ftype (function (writer) (unsigned-byte 32)) write-forward-ref))

(defun write-forward-ref (writer)
  (let ((ref-addr (writer-current-pos writer)))
    (push ref-addr (writer-holes writer))
    (write-u32 writer 0)
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


(defun write-tiff (writer tiff)
  "Write TIFF to WRITER."
  (write-u16 writer (if (tiff-big-endian-p tiff) #x4D4D #x4949))
  (write-u16 writer 42)
  (let ((ref (write-forward-ref writer))
	(num-ifds (length (tiff-ifds tiff))))
    (dotimes (i num-ifds)
      (resolve-forward-ref writer ref)
      (write-ifd writer (elt (tiff-ifds tiff) i))
      (if (= i (- num-ifds 1))
	  (write-u32 writer 0)
	  (setf ref (write-forward-ref writer))))))

(defun save-tiff (filename tiff)
  (with-open-file (out filename :direction :output :if-exists :error)
    (let ((writer (make-writer out :big-endian (tiff-big-endian-p tiff))))
      (write-tiff writer tiff)
      (writer-flush writer))))
      


(defun write-ifd (writer ifd)
  (writer-align-2 writer)
  (write-u16 writer (length (ifd-entries ifd)))
  (let ((value-refs (map 'vector #'(lambda (e) (write-ifd-entry writer e)) (ifd-entries ifd))))
    (map nil #'(lambda (e ref) (write-ifd-entry-value writer e ref))
	 (ifd-entries ifd)
	 value-refs)))

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
  (ecase (ifd-entry-type entry)
    ((#.+BYTE+ #.+UNDEFINED+)
     (write-bytes writer (ifd-entry-value entry))
     (dotimes (i (- 4 (ifd-entry-count entry)))
       (write-u8 writer 0)))
    ))
       

(defun write-ifd-entry-value (writer entry ref)
  (when ref
    (writer-align-2 writer)
    (resolve-forward-ref writer ref)
    (let ((values (ifd-entry-value entry)))
      (ecase (ifd-entry-type entry)
	((#.+BYTE+ #.+UNDEFINED+)
	 (write-bytes writer values))
	(#.+SBYTE+
	 (write-sbytes writer values))
	(#.+ASCII+
	 (write-ascii write values))
	(#.+SHORT+
	 (map nil #'(lambda (w) (write-u16 writer w)) values))
	(#.+LONG+
	 (map nil #'(lambda (w) (write-u32 writer w)) values))
	(#.+RATIONAL+
	 (map nil #'(lambda (r) (write-rational writer r)) values))
	(#.+SSHORT+
	 (map nil #'(lambda (w) (write-s16 writer w)) values))
	(#.+SLONG+
	 (map nil #'(lambda (w) (write-s32 writer w)) values))
	(#.+SRATIONAL+
	 (map nil #'(lambda (r) (write-srational writer r)) values))
	;; TODO: +FLOAT+ +DOUBLE+
	))))
