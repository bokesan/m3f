(defpackage :binary-buffer
  (:use :cl)
  (:shadow :close)
  (:import-from :alexandria :array-length :array-index)
  (:import-from :arrays :copy-bytes)
  (:export :binary-buffer :binary-buffer-p
	   :binary-buffer-big-endian-p
	   :with-binary-buffer
	   :get-u8 :get-s8 :get-u16 :get-s16 :get-u32 :get-s32 :get-bytes))

(in-package :binary-buffer)

(declaim (inline make-binary-buffer
		 binary-buffer-stream
		 binary-buffer-big-endian-p
		 binary-buffer-chunk-size
		 binary-buffer-chunks
		 binary-buffer-eof-p))

(defstruct binary-buffer
  (stream (error "missing :stream") :type stream :read-only t)
  (big-endian-p nil :type boolean)
  ;; min chunk size 7 so that largest word (8 bytes) covers at most 2 chunks
  (chunk-size 65536 :type (integer 7 #x80000000) :read-only t)
  (chunks nil :type (array (simple-array (unsigned-byte 8) 1) 1) :read-only t)
  (eof-p nil :type boolean))

#+SBCL (declaim (sb-ext:freeze-type binary-buffer))

(defun create (stream &key chunk-size)
  (make-binary-buffer
   :stream stream
   :chunk-size (or chunk-size 65536)
   :chunks (make-array 10 :adjustable t :fill-pointer 0)))

(defun close (bs)
  (setf (binary-buffer-eof-p bs) t)
  (cl:close (binary-buffer-stream bs)))

(defmacro with-binary-buffer ((buf stream &rest options) &body body)
  `(let ((,buf (create ,stream ,@options)))
     (unwind-protect
	  (progn ,@body)
       (when ,buf (close ,buf)))))

(declaim (inline index))
(defun index (bs offs)
  (declare (type binary-buffer bs)
	   (type array-index offs))
  (floor offs (binary-buffer-chunk-size bs)))


(declaim (ftype (function (binary-buffer array-index)
			  (simple-array (unsigned-byte 8) 1))
		get-chunk))

(defun get-chunk (bs i)
  (let* ((chunks (binary-buffer-chunks bs))
	 (chunk-size (binary-buffer-chunk-size bs))
	 (len (length chunks)))
    (unless (< i len)
      (when (binary-buffer-eof-p bs)
	(error 'end-of-file))
      (do ((k len (+ k 1)))
	  ((> k i))
	(let* ((chunk (make-array chunk-size :element-type '(unsigned-byte 8)))
	       (bytes-read (read-sequence chunk (binary-buffer-stream bs))))
	  (unless (zerop bytes-read)
	    (vector-push-extend
	     (if (= bytes-read chunk-size)
		 chunk
		 (adjust-array chunk bytes-read))
	     chunks))
	  (when (< bytes-read chunk-size)
	    (setf (binary-buffer-eof-p bs) t)))))
    (aref chunks i)))

(declaim
  (ftype (function (binary-buffer array-index) (unsigned-byte 8)) get-u8)
  (ftype (function (binary-buffer array-index) (signed-byte 8)) get-s8)
  (ftype (function (binary-buffer array-index) (unsigned-byte 16)) get-u16)
  (ftype (function (binary-buffer array-index) (signed-byte 16)) get-s16)
  (ftype (function (binary-buffer array-index) (unsigned-byte 32)) get-u32)
  (ftype (function (binary-buffer array-index) (signed-byte 32)) get-s32)
  (ftype (function (binary-buffer array-index array-length) (simple-array (unsigned-byte 8) 1)) get-bytes))

(defun get-u8 (bs offs)
  (declare (optimize speed))
  (multiple-value-bind (ci co)
      (index bs offs)
    (let ((chunk (get-chunk bs ci)))
      (when (>= co (length chunk))
	(error 'end-of-file))
      (aref chunk co))))

(defun get-u16 (bs offs)
  (declare (optimize speed))
  (multiple-value-bind (ci co)
      (index bs offs)
    (let* ((chunk1 (get-chunk bs ci))
	   (b0 (aref chunk1 co))
	   (b1 (if (< (+ co 1) (length chunk1))
		   (aref chunk1 (+ co 1))
		   (aref (get-chunk bs (+ ci 1)) 0))))
      (if (binary-buffer-big-endian-p bs)
          (+ (* 256 b0) b1)
	  (+ b0 (* 256 b1))))))

(defun get-u32 (bs offs)
  (declare (optimize speed))
  (multiple-value-bind (ci co)
      (index bs offs)
    (let* ((chunk1 (get-chunk bs ci))
	   (len (length chunk1))
	   (b0 (aref chunk1 co))
	   (chunk2 (if (< (+ co 3) len) chunk1 (get-chunk bs (+ ci 1))))
	   (b1 (if (< (+ co 1) len) (aref chunk1 (+ co 1)) (aref chunk2 (- (+ co 1) len))))
	   (b2 (if (< (+ co 2) len) (aref chunk1 (+ co 2)) (aref chunk2 (- (+ co 2) len))))
	   (b3 (if (< (+ co 3) len) (aref chunk1 (+ co 3)) (aref chunk2 (- (+ co 3) len)))))
      (if (binary-buffer-big-endian-p bs)
          (+ (* 256 256 256 b0) (* 256 256 b1) (* 256 b2) b3)
	  (+ b0 (* 256 b1) (* 256 256 b2) (* 256 256 256 b3))))))

(defun get-s8 (bs offs)
  (let ((v (get-u8 bs offs)))
    (if (< v 128) v (- v 256))))

(defun get-s16 (bs offs)
  (let ((v (get-u16 bs offs)))
    (if (< v 32768) v (- v 65536))))

(defun get-s32 (bs offs)
  (let ((v (get-u32 bs offs)))
    (if (< v 2147483648) v (- v 4294967296))))


(defun get-bytes (bs offset len)
  (let ((result (make-array len :element-type '(unsigned-byte 8))))
    (multiple-value-bind (ci co)
	(index bs offset)
      (do ((chunk-number ci (+ chunk-number 1))
	   (chunk-offset co 0)
	   (i 0))
	  ((>= i len) result)
	(let* ((chunk (get-chunk bs chunk-number))
	       (missing (- len i))
	       (in-chunk (- (length chunk) chunk-offset))
	       (n (min missing in-chunk)))
	  (copy-bytes chunk chunk-offset result i n)
	  (incf i n))))))
