(defpackage :report
  (:use :cl :tiff)
  (:import-from :alexandria :when-let :array-index)
  (:export :summary :detail :layout))

(in-package :report)

(declaim (ftype (function (tiff &optional t)) summary))

(defun summary (tiff &optional (stream t))
  "Show metadata summary as in Phocus \"Capture Info\" tab."
  (labels ((label (s) (format stream "~14@A: " s))
	   (tags (s &rest tags)
	     (label s)
	     (princ (apply #'tag-value tiff :default "" tags) stream)
	     (terpri stream))
	   (val (s v)
	     (label s)
	     (princ v stream)
	     (terpri stream)))
    (tags "Device" #x0110 #x0015)
    (tags "Created" #x9003)
    (label "Dimensions")
    (let ((d (tag-value tiff #xC620)))
      (when (and (vectorp d) (= (length d) 2))
	(format stream "~Ax~A" (aref d 0) (aref d 1))))
    (terpri stream)
    (label "Lens")
    (when-let ((lens (tag-value tiff #xA434)))
      (princ lens stream)
      (when-let ((snr (decode-hasselblad-snr (tag-value tiff #xA435 #x0061))))
        (princ ", serial number: " stream)
	(if (consp snr)
	    (format stream "~A (year: ~A, product code: ~A)"
		    (first snr) (second snr) (third snr))
	    (princ snr stream))))
    (terpri stream)
    (val "Converter" "?") ; TODO
    (val "Extension" "?") ; TODO
    (val "HTS" "?") ; TODO
    (tags "ISO" #x8827)
    (label "Shutter")
    (let ((shutter (tag-value tiff #x829A #x9201 #x0063))
	  (mode (tag-value tiff #x004A)))
      (case mode
	((nil 0) (format stream "~A~%" shutter))
	(1 (format stream "~A [E]~%" shutter))
	(t (format stream "~A [~S]~%" shutter mode))))
    (label "Aperture")
    (when-let ((fnum (tag-value tiff #x829D #x9202)))
      (format stream "f/~F" fnum))
    (terpri stream)
    (let ((mode (tag-value tiff #x9207))
	  (compensation (tag-value tiff #x9204)))
      (label "Light Meter")
      (princ (metering-mode-name mode) stream)
      (when (and compensation (not (zerop compensation)))
	(format stream " ~,2@F" compensation))
      (terpri stream))
    (let* ((mode-value (tag-value tiff #x8822))
	   (special (tag-value tiff #x005E))
	   (mode (case mode-value
		   (1 "M")
		   (2 (if (and special (= special 2)) "Full Auto" "P"))
		   (3 "A")
		   (4 "S")
		   (t (format nil "Unknown (~S)" mode-value)))))
      (val "Exposure Mode" mode))
    (val "Focus Mode"
	 (let ((xs (tag-value tiff #x0017)))
	   (if (and (vectorp xs) (= (length xs) 17) (= (aref xs 0) 1)
		    (or (= (aref xs 1) 2) (= (aref xs 1) #x42)))
	       (if (= (aref xs 1) 2) "Manual" "Single")
	       "?"))) ; TODO
    (val "Serial Number" (get-serial-number tiff))
    (val "GPS Coordinate" "?") ; TODO
    ;; Not displayed in Phocus:
    (when-let ((q (get-raw-quality tiff)))
      (label "Quality")
      (format stream "~A bit~%" q))
    (when-let ((wb (decode-white-balance (tag-value tiff #x0005))))
      (val "White Balance" wb))
    (when-let ((m (decode-crop-mode (tag-value tiff #x0059))))
      (val "Crop Mode" m))
    (when-let ((m (decode-release-mode (tag-value tiff #x005B))))
      ;; TODO: counter
      (val "Release Mode" m))))

(defun get-serial-number (tiff)
  "Extract Hasselblad camera serial number."
  (declare (type tiff tiff))
  (or (tag-value tiff #xA431 #xC62F)
      ;; If the regular snr fields are missing or empty, try extracting from ImageUniqueID:
      (let ((s (tag-value tiff #xA420)))
	(if (and (stringp s) (= (length s) 32))
	    (let ((pc1 (code-char (read-hex s 12 14)))
		  (pc2 (code-char (read-hex s 14 16)))
		  (snr (read-hex s 16 24)))
	      (format nil "~C~C~D" pc1 pc2 snr))
	    ""))))

(defun detail (tiff &key (stream t) (filter :known) max-bytes words)
  "Show TIFF IFDs with entries."
  (format stream "Number of IFDs: ~D~%" (length (tiff-ifds tiff)))
  (map nil
       #'(lambda (ifd) (show-ifd ifd :stream stream :filter filter :max-bytes max-bytes :words words))
       (tiff-ifds tiff)))

(defun layout (tiff &optional (stream t))
  "Show TIFF file layout."
  (format stream "File layout:~%")
  (report-regions tiff stream))


(defun show-ifd (ifd &key (stream t) filter max-bytes words)
  (declare (type ifd ifd))
  (format stream "~A (~D entries):~%" (ifd-name ifd) (length (ifd-entries ifd)))
  (loop for entry across (ifd-entries ifd) do
       (let* ((tag (ifd-entry-tag entry))
	      (info (tiff-tag-info tag)))
	 (when (ecase filter
		 (:sensitive (not (sensitive-tag-p tag)))
		 (:diff (not (volatile-tag-p tag)))
		 (:all t)
		 (:known info)
		 (:known-sensitive (and info (not (sensitive-tag-p tag))))
		 (:known-diff (and info (not (volatile-tag-p tag)))))
	   (show-ifd-entry entry info :stream stream :max-bytes max-bytes :words words)))))

(declaim (ftype (function (tiff (or stream boolean))) report-regions))
(defun report-regions (tiff s)
  (let ((last 0))
    (loop for region across (tiff-regions tiff) do
      (let ((start (region-start region))
	    (end (region-end region)))
	(cond ((> start last)
	       (format s "  ~8,'0X - ~8,'0X ~9D  ???~%  " last start (- start last)))
	      ((< start last)
	       (princ "O " s))
	      (t (princ "  " s)))
	(format s "~8,'0X - ~8,'0X ~9D  ~A~%" start end (- end start) (region-description region))
	(setq last end)))))


(declaim (ftype (function ((simple-array (unsigned-byte 8) 1)
			   array-index
			   (member :little :big))
			  (unsigned-byte 16))
		aref-short))

(declaim (ftype (function ((simple-array (unsigned-byte 8) 1)
			   array-index
			   (member :little :big))
			  (unsigned-byte 32))
		aref-long))

(defun aref-short (bs i endianness)
  (declare (optimize speed))
  (let ((b0 (aref bs i))
	(b1 (aref bs (+ i 1))))
    (if (eq endianness :little)
	(+ b0 (* 256 b1))
	(+ (* 256 b0) b1))))

(defun aref-long (bs i endianness)
  (declare (optimize speed))
  (let ((b0 (aref bs i))
	(b1 (aref bs (+ i 1)))
	(b2 (aref bs (+ i 2)))
	(b3 (aref bs (+ i 3))))
    (if (eq endianness :little)
	(+ b0 (* 256 b1) (* 256 256 b2) (* 256 256 256 b3))
	(+ b3 (* 256 b2) (* 256 256 b1) (* 256 256 256 b0)))))

(defun aref-sshort (bs i endianness)
  (let ((v (aref-short bs i endianness)))
    (if (< v 32768) v (- v 65536))))

(defun aref-slong (bs i endianness)
  (let ((v (aref-long bs i endianness)))
    (if (< v 2147483648) v (- v 4294967296))))

(defun show-bytes (bs &optional max-len words (stream t))
  (declare (type (array (unsigned-byte 8) 1) bs))
  (let ((n (if (and max-len (< max-len (length bs)))
	       max-len
	       (length bs)))
	(bytes-per-row 16))
    (declare (type (unsigned-byte 32) n))
    (when (> (length bs) n)
      (princ " (truncated)" stream))
    (do ((i 0 (+ i bytes-per-row)))
	((>= i n))
      (terpri stream)
      (princ "    " stream)
      (do ((k 0 (+ k 1)))
	  ((>= k bytes-per-row))
	(if (< (+ i k) n)
	    (format stream " ~2,'0X" (aref bs (+ i k)))
	    (princ "   " stream)))
      (princ "  " stream)
      (do ((k 0 (+ k 1)))
	  ((or (>= k bytes-per-row) (>= (+ i k) n)))
	(let* ((v (aref bs (+ i k)))
	       (c (if (<= 32 v 126) (code-char v) #\.)))
	  (princ c stream)))
      (when words
	(terpri stream)
	(princ "    " stream)
	(do ((k 0 (+ k 2)))
	    ((or (>= k bytes-per-row)
		 (>= (+ i k 1) n)))
	  (format stream "~6D" (aref-sshort bs (+ i k) words)))
	(terpri stream)
	(princ "    " stream)
	(do ((k 0 (+ k 4)))
	    ((or (>= k bytes-per-row)
		 (>= (+ i k 3) n)))
	  (format stream "~12D" (aref-slong bs (+ i k) words)))
	(terpri stream)
	(princ "    " stream)
	(do ((k 0 (+ k 8)))
	    ((or (>= k bytes-per-row)
		 (>= (+ i k 7) n)))
	  (let* ((n (aref-slong bs (+ i k) words))
		 (d (aref-slong bs (+ i k 4) words))
		 (v (if (zerop d) 0 (/ n d))))
	    (format stream "~24F" v)))))))
	  

(defun show-value (v &optional (stream t))
  (if (and (rationalp v) (not (integerp v)))
      (format stream " ~F" v)
      (format stream " ~S" v)))

(defun show-ifd-entry (e info &key (stream t) max-bytes words)
  (declare (type ifd-entry e)
	   (type list info))
  (let* ((type-name (tiff-type-name (ifd-entry-type e)))
	 (tag-name (if info (car info) "*unknown*"))
	 (values (ifd-entry-values e))
	 (displayed-p nil))
    (format stream "  ~4,'0X ~28A ~9A ~7D "
	    (ifd-entry-tag e) tag-name type-name
	    (ifd-entry-count e))
    (when (cdr info)
      (when-let ((disp (funcall (cadr info) values)))
	(write-char #\space stream)
	(princ disp stream)
	(setq displayed-p t)))
    (unless displayed-p
      (typecase values
	(string (write-char #\space stream) (prin1 values stream))
	((vector (unsigned-byte 8)) (show-bytes values max-bytes words stream))
	(vector (loop for v across values do (show-value v stream)))
	(rational (show-value values stream))
	(t (write-char #\space stream) (prin1 values stream))))
    (terpri stream)))


(defun decode-hasselblad-serial-number (s)
  (if (and (stringp s) (= (length s) 9))
      (flet ((code-to-number (c) (position c "SVHPICTURE")))
	(let ((product-code (subseq s 0 2))
	      (year (+ 2000 (* 10 (code-to-number (char s 2)))
		       (code-to-number (char s 3)))))
	  ;; (unknown (char s 4))
	  ;; (serial (subseq s 5))
	  (format nil "~S (product code: ~A, year: ~A)"
		  s product-code year)))
      nil))

(defun decode-hasselblad-snr (s)
  "If the argument is a string that looks like a Hasselblad serial number
(two-digit product code, two digit year code, snr), return a list
(s year product-code). Otherwise, return the argument."
  (if (and (stringp s) (>= (length s) 8))
      (flet ((code-to-number (c) (position c "SVHPICTURE")))
	(let ((product-code (subseq s 0 2))
	      (yc1 (code-to-number (char s 2)))
	      (yc2 (code-to-number (char s 3))))
	  (if (and yc1 yc2)
	      (list s (+ 2000 (* 10 yc1) yc2) product-code)
	      s)))
      s))


(defun get-raw-quality (raw)
  (declare (type tiff raw))
  (let ((values (tag-value raw #x0013)))
    (if (and values
	     (vectorp values)
	     (= (length values) 2)
	     (or (= (aref values 1) (* 16 512))
		 (= (aref values 1) (* 14 512))
		 (= (aref values 1) (* 12 512))))
	(/ (aref values 1) 512)
	nil)))
