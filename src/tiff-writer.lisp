(in-package :tiff)

(defconstant +BLOCK-SIZE+ 4096)


(defstruct (writer (:constructor %make-writer))
  (stream nil :type stream :read-only t)
  (big-endian-p nil :type boolean :read-only t)
  (buf nil :type (vector (unsigned-byte 8)) :read-only t)
  (buf-start 0 :type (unsigned-byte 32))
  (holes nil :type list))

(defun make-writer (stream &key big-endian)
  (%make-writer
   :stream stream
   :big-endian-p big-endian
   :buf (make-array +BLOCK-SIZE+ :element-type '(unsigned-byte 8) :fill-pointer 0)))

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

(defun write-u8 (writer byte)
  (vector-push-extend byte (writer-buf writer) 1024)
  (flush-when-possible writer))

(defun fill-hole (writer hole-addr value)
  (setf (writer-holes writer) (delete hole-addr (writer-holes writer)))
  (set-u32 writer hole-addr value))

(defstruct address
  (address 0 :type (unsigned-byte 32))
  absolute-refs
  relative-refs)

(defun address-known-p (address)
  (and (null (address-absolute-refs address))
       (null (address-relative-refs address))))

(defun make-forward-ref (current-address)
  (make-address :absolute-refs (list current-address)))

(defun write-tiff (writer tiff)
  "Write TIFF to WRITER."
  (map nil #'(lambda (ifd) (write-ifd w ifd)) (tiff-ifds tiff)))

(defun write-ifd (writer ifd)
  (map nil #'(lambda (e) (write-ifd-entry writer e)) (ifd-entries ifd))
  (map nil #'(lambda (e) (write-ifd-entry-value writer e)) (ifd-entries ifd)))

(defun write-ifd-entry (writer entry)
  (align-position writer 4)
  (write-u16 writer (ifd-entry-tag entry))
  (write-u16 writer (ifd-entry-type entry))
  (write-u32 writer (ifd-entry-count entry))
  (if (ifd-entry-short-value-p entry)
      (write-inline-value writer entry)
      (let ((address (make-forward-ref (writer-current-position writer))))
	(setf (ifd-entry-value-address entry) address)
	(write-hole writer 4))))

(defun write-ifd-entry-value (writer entry)
  (unless (ifd-entry-short-value-p entry)
    (ecase (ifd-entry-type entry)
      (1 ;; byte
       (resolve-address (ifd-entry-value-address entry) (writer-current-position writer))
       (write-bytes writer (ifd-entry-value entry)))
      )))
