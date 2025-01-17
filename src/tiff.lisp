(defpackage :tiff
  (:use :cl :binary-buffer)
  (:import-from :alexandria :array-index :array-length :if-let :when-let)
  (:export :tiff :tiff-ifds :tiff-regions :read-tiff :tag-value
           :ifd :ifd-name :ifd-address :ifd-entries
           :ifd-entry :ifd-entry-tag :ifd-entry-type :ifd-entry-count :ifd-entry-values
	   :tiff-tag-info :tiff-type-name
	   :metering-mode-name
	   :decode-white-balance
	   :decode-drive-mode
           :decode-crop-mode
           :decode-orientation :portrait-orientation-p
	   :volatile-tag-p :sensitive-tag-p
	   :read-hex
           :region :region-start :region-end :region-description))

(in-package :tiff)

(declaim (inline read-hex))
(defun read-hex (s start end)
  (parse-integer s :start start :end end :radix 16))

(declaim (ftype (function ((unsigned-byte 16)) boolean)
		volatile-tag-p
		sensitive-tag-p))

(defun volatile-tag-p (tag)
  "Does tag typically change between images?"
  (case tag
    ((#x0111 #x0116 0x0117 ; actual image data
      #x0132 #x9003 #x9004 ; date/time
      #xA420 #xC65D ; unique IDs
      #x014A #x8769 #x927C ; IFDs
      ) t)))

(defun sensitive-tag-p (tag)
  "Can the tag contain sensitive or private information?"
  (or (volatile-tag-p tag)
      (case tag
	((#x0061 #xA431 #xA435 #xC62F ; serial numbers
	  #x8298 ; copyright
	  #x02BC ; application notes
	  ) t))))
		 

(declaim (inline make-ifd-entry ifd-entry-tag ifd-entry-type ifd-entry-count
		 ifd-entry-value-address ifd-entry-values
		 make-ifd ifd-name ifd-address ifd-entries))

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

(declaim (inline make-region region-start region-end))
(defstruct region
  (start 0 :type array-index :read-only t)
  (end 0 :type array-index :read-only t)
  (description "" :type string :read-only t))

(declaim (inline make-tiff tiff-ifds tiff-regions))
(defstruct tiff
  ifds
  regions)

(declaim (ftype (function (tiff array-index array-index string)) note-region))
(defun note-region (f start len name)
  (push (make-region :start start :end (+ start len) :description name) (tiff-regions f)))

#+SBCL (declaim (sb-ext:freeze-type ifd ifd-entry region tiff))

(declaim (ftype (function ((unsigned-byte 16)) list) tiff-tag-info))


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
  


(declaim (ftype (function ((unsigned-byte 16)) string) tiff-type-name))
(defun tiff-type-name (type)
  (declare (optimize speed))
  (case type
    (1 "BYTE")
    (2 "ASCII")
    (3 "SHORT")
    (4 "LONG")
    (5 "RATIONAL")
    (6 "SBYTE")
    (7 "UNDEFINED")
    (8 "SSHORT")
    (9 "SLONG")
    (10 "SRATIONAL")
    (11 "FLOAT")
    (12 "DOUBLE")
    (t (format nil "~D" type))))

(defun tiff-type-size (type)
  (case type
    (1 1)
    (2 1)
    (3 2)
    (4 4)
    (5 8)
    (6 1)
    (7 1)
    (8 2)
    (9 4)
    (10 8)
    (11 4)
    (12 8)
    (t (error "unknown type: ~S" type))))

(declaim (inline ifd-value-inline-p))
(defun ifd-value-inline-p (type count)
  (<= (* count (tiff-type-size type)) 4))

(defun apex-value (n d)
  (expt 2 (/ (/ n d) 2)))

(declaim (ftype (function ((unsigned-byte 16)) boolean) apex-tag-p))
(defun apex-tag-p (tag)
  (or (= tag #x9201)
      (= tag #x9202)
      (= tag #x9205)))

(declaim (ftype (function (binary-buffer array-index array-index) list) get-ascii))

(declaim (ftype (function (binary-buffer (unsigned-byte 16) (unsigned-byte 16) (unsigned-byte 32) (unsigned-byte 32)) t) get-values))
(defun get-values (raw tag type count addr)
  (let ((addr (if (ifd-value-inline-p type count) addr (get-u32 raw addr))))
    (declare (optimize speed))
    (case count
      (0 nil)
      (1 (case type
	   ((1 7) (get-u8 raw addr))
	   (6 (get-s8 raw addr))
	   (2 (let ((b (get-u8 raw addr)))
		(if (zerop b)
		    ""
		    (string (code-char b)))))
	   (3 (get-u16 raw addr))
	   (4 (get-u32 raw addr))
	   (5 (let ((n (get-u32 raw addr))
		    (d (get-u32 raw (+ addr 4))))
		(if (apex-tag-p tag)
		    (apex-value n d)
		    (/ n d))))
	   (8 (get-s16 raw addr))
	   (9 (get-s32 raw addr))
	   (10 (let ((n (get-s32 raw addr))
		     (d (get-s32 raw (+ addr 4))))
		 (if (apex-tag-p tag)
		     (apex-value n d)
		     (/ n d))))
	   (11 (get-float raw addr))
	   (12 (get-double raw addr))
	   (t 'unknown-type)))
      (t (case type
	   ((1 7)
	    (get-bytes raw addr count))
	   (2 (let ((strings (get-ascii raw addr count)))
		(if (null (cdr strings))
		    (car strings)
		    (coerce strings 'vector))))
	   (6 (let ((xs (make-array count :element-type '(signed-byte 8))))
		(dotimes (i count xs)
		  (setf (aref xs i) (get-s8 raw (+ addr i))))))
	   (3 (let ((xs (make-array count :element-type '(unsigned-byte 16))))
		(dotimes (i count xs)
		  (setf (aref xs i) (get-u16 raw (+ addr (* i 2)))))))
	   (4 (let ((xs (make-array count :element-type '(unsigned-byte 32))))
		(dotimes (i count xs)
		  (setf (aref xs i) (get-u32 raw (+ addr (* i 4)))))))
	   (5 (let ((xs (make-array count :element-type 'rational)))
		(dotimes (i count xs)
		  (let ((n (get-u32 raw (+ addr (* i 8))))
			(d (get-u32 raw (+ addr (* i 8) 4))))
		    (setf (aref xs i)
			  (if (apex-tag-p tag)
			      (apex-value n d)
			      (/ n d)))))))
	   (10 (let ((xs (make-array count :element-type 'rational)))
		 (dotimes (i count xs)
		   (let ((n (get-s32 raw (+ addr (* i 8))))
			 (d (get-s32 raw (+ addr (* i 8) 4))))
		     (setf (aref xs i)
			   (if (apex-tag-p tag)
			       (apex-value n d)
			       (/ n d)))))))
	   (8 (let ((xs (make-array count :element-type '(signed-byte 16))))
		(dotimes (i count xs)
		  (setf (aref xs i) (get-s16 raw (+ addr (* i 2)))))))
	   (9 (let ((xs (make-array count :element-type '(signed-byte 32))))
		(dotimes (i count xs)
		  (setf (aref xs i) (get-s32 raw (+ addr (* i 4)))))))
	   (11 (let ((xs (make-array count :element-type 'short-float)))
		(dotimes (i count xs)
		  (setf (aref xs i) (get-float raw (+ addr (* i 4)))))))
	   (12 (let ((xs (make-array count :element-type 'double-float)))
		(dotimes (i count xs)
		  (setf (aref xs i) (get-double raw (+ addr (* i 8)))))))
	   (t 'error-unknown-type))))))

(defun metering-mode-name (value)
  (case value
    (0 "Unknown")
    (1 "Average")
    (2 "Centre W")
    (3 "Spot")
    (4 "Multi-spot")
    (5 "Multi-segment")
    (6 "Partial")
    (254 "Centre Spot")	; Hasselblad
    (255 "Other")))

(defun portrait-orientation-p (values)
  "Check if the given orientation code is a portrait orientation."
  (and (numberp values) (> values 4)))

(defun decode-orientation (values)
  (case values
    (1 "Horizontal (normal)")
    (2 "Mirror horizontal")
    (3 "Rotate 180")
    (4 "Mirror vertical")
    (5 "Mirror horizontal and rotate 270 CW")
    (6 "Rotate 90 CW")
    (7 "Mirror horizontal and rotate 90 CW")
    (8 "Rotate 270 CW")))

(defparameter *standard-tags*  
  `((#x00FE "NewSubfileType"
	    ,#'(lambda (values)
		 (case values
		   (0 "Full-resolution image")
		   (1 "Reduced-resolution image"))))
    (#x00FF "SubfileType")
    (#x0100 "ImageWidth")
    (#x0101 "ImageLength")
    (#x0102 "BitsPerSample")
    (#x0103 "Compression"
	    ,#'(lambda (values)
		 (case values
		   (1 "Uncompressed")
		   (2 "CCITT 1D")
		   (3 "T4/Group 3 Fax")
		   (4 "T6/Group 4 Fax")
		   (5 "LZW")
		   (6 "JPEG (old-style)")
		   (7 "JPEG")
		   (8 "Adobe Deflate")
		   (9 "JBIG B&W")
		   (10 "JBIG Color"))))
    (#x0106 "PhotometricInterpretation"
	    ,#'(lambda (values)
		 (case values
		   (0 "WhiteIsZero")
		   (1 "BlackIsZero")
		   (2 "RGB")
		   (3 "RGB Palette")
		   (4 "Transparency Mask")
		   (5 "CMYK")
		   (6 "YCbCr")
		   (8 "CIELab")
		   (9 "ICCLab")
		   (10 "ITULab")
		   (32803 "Color Filter Array")
		   (34892 "Linear Raw"))))
    (#x0107 "Threshholding")
    (#x0108 "CellWidth")
    (#x0109 "CellLength")
    (#x010A "FillOrder")
    (#x010D "DocumentName")
    (#x010E "ImageDescription")
    (#x010F "Make")
    (#x0110 "Model")
    (#x0111 "StripOffsets")
    (#x0112 "Orientation" ,#'decode-orientation)
    (#x0115 "SamplesPerPixel")
    (#x0116 "RowsPerStrip")
    (#x0117 "StripByteCounts")
    (#x0118 "MinSampleValue")
    (#x0119 "MaxSampleValue")
    (#x011A "XResolution")
    (#x011B "YResolution")
    (#x011C "PlanarConfiguration"
	    ,#'(lambda (values)
		 (case values
		   (1 "Chunky")
		   (2 "Planar"))))
    (#x011D "PageName")
    (#x0120 "FreeOffsets")
    (#x0121 "FreeByteCounts")
    (#x0122 "GrayResponseUnit")
    (#x0123 "GrayResponseCurve")
    (#x0128 "ResolutionUnit"
	    ,#'(lambda (values)
		 (case values
		   (2 "inches")
		   (3 "cm"))))
    (#x0129 "PageNumber")
    (#x012D "TransferFunction")
    (#x0131 "Software")
    (#x0132 "DateTime")
    (#x013B "Artist")
    (#x013C "HostComputer")
    (#x013E "WhitePoint")
    (#x013F "PrimaryChromaticities")
    (#x0140 "ColorMap")
    (#x0141 "HalftoneHints")
    (#x0142 "TileWidth")
    (#x0143 "TileLength")
    (#x0144 "TileOffsets")
    (#x0145 "TileByteCounts")
    (#x014A "SubIFD")
    (#x014C "InkSet")
    (#x014D "InkNames")
    (#x014E "NumberOfInks")
    (#x0150 "DotRange")
    (#x0151 "TargetPrinter")
    (#x0152 "ExtraSamples")
    (#x0153 "SampleFormat")
    (#x0154 "SMinSampleValue")
    (#x0155 "SMaxSampleValue")
    (#x0156 "TransferRange")
    (#x0200 "JPEGProc")
    (#x0211 "YCbCrCoefficients")
    (#x0212 "YCbCrSubSampling")
    (#x0213 "YCbCrPositioning")
    (#x0214 "ReferenceBlackWhite")
    (#x02BC "ApplicationNotes")
    (#x8298 "Copyright")
    (#x4746 "Rating")
    (#x4749 "RatingPercent")
    (#x829A "ExposureTime")
    (#x829D "FNumber")
    (#x83BB "IPTC-NAA")
    (#x8568 "AFCP_IPTC")
    (#x8649 "PhotoshopSettings")
    (#x8769 "ExifOffset")
    (#x8822 "ExposureProgram"
	    ,#'(lambda (values)
		 (case values
		   (0 "Not Defined")
		   (1 "Manual")
		   (2 "Program AE")
		   (3 "Aperture-priority AE")
		   (4 "Shutter speed priority AE")
		   (5 "Creative (Slow speed)")
		   (6 "Action (High speed)")
		   (7 "Portrait")
		   (8 "Landscape"))))
    (#x8827 "ISO")
    (#x8833 "ISOSpeed")
    (#x9000 "ExifVersion")
    (#x9003 "DateTimeOriginal")
    (#x9004 "CreateDate")
    (#x9201 "ShutterSpeedValue")
    (#x9202 "ApertureValue")
    (#x9203 "BrightnessValue")
    (#x9204 "ExposureCompensation")
    (#x9205 "MaxApertureValue")
    (#x9206 "SubjectDistance")
    (#x9207 "MeteringMode" ,#'metering-mode-name)
    (#x9209 "Flash"
	    ,#'(lambda (values)
		 (case values
		   (0 "No Flash")
		   (1 "Fired")
		   (5 "Fired, Return not detected")
		   (7 "Fired, Return detected")
		   (8 "On, Did not fire")
		   (9 "On, Fired")
		   (0x0D "On, Return not detected")
		   (0x0F "On, Return detected")
		   (0x10 "Off, Did not fire")
		   ;; TODO
		   )))
    (#x920A "FocalLength")
    (#x927C "MakerNote" ,#'(lambda (vs) (declare (ignore vs)) "(IFD)"))
    (#x9286 "UserComment")
    (#xA001 "Color Space"
	    ,#'(lambda (v)
		 (case v
		   (1 "sRGB")
		   (2 "Adobe RGB")
		   (0xFFFD "Wide Gamut RGB")
		   (0xFFFE "ICC Profile")
		   (0xFFFF "Uncalibrated"))))
    (#xA005 "ExifInteroperabilityOffset")
    (#xA20E "FocalPlaneXResolution")
    (#xA20F "FocalPlaneYResolution")
    (#xA210 "FocalPlaneResolutionUnit"
	    ,#'(lambda (v)
		 (case v
		   (1 "None")
		   (2 "inches")
		   (3 "cm")
		   (4 "mm")
		   (5 "µm"))))
    (#xA405 "Focal Length in 35mm Format")
    (#xA40C "Subject Distance Range")
    (#xA420 "ImageUniqueID"
	    ;; first 16 characters are 12 x '0' followed by the product code ASCII values
	    ;; in hex, e.g. "0000000000004A54" for product code "JT".
	    ;; The seconds half (also 16 characters) are the same as tag C65D.
	    ;; Images exported from Phocus Mobile seem to have the higest bit of count set.
	    ,#'(lambda (s)
		 (if (and (stringp s) (= (length s) 32))
		     (let ((pc1 (code-char (read-hex s 12 14)))
			   (pc2 (code-char (read-hex s 14 16)))
			   (snr (read-hex s 16 24))
			   (count (read-hex s 24 32)))
		       (format nil "~S (serial number: ~C~C~D, count: ~A~A)"
			       s pc1 pc2 snr
			       (logand count #x7fffffff)
			       (if (zerop (logand count #x80000000)) "" ", Phocus Mobile")))
		     nil)))
    (#xA430 "Owner Name")
    (#xA431 "Serial Number")
    (#xA432 "Lens Info")
    (#xA433 "Lens Make")
    (#xA434 "Lens Model")
    (#xA435 "Lens Serial Number")
    (#xB4C3 "HasselbladRawImage")
    (#xC519 "HasselbladXML (PLIST)")
    (#xC51B "HasselbladExif")
    (#xC614 "UniqueCameraModel")
    (#xC61A "BlackLevel")
    (#xC61D "WhiteLevel")
    (#xC61F "DefaultCropOrigin")
    (#xC620 "DefaultCropSize")
    (#xC621 "Color Matrix 1")		; only in .3FR
    (#xC622 "Color Matrix 2")
    (#xC623 "Camera Calibration 1")
    (#xC624 "Camera Calibration 2")
    (#xC627 "Analog Balance")		; only in .fff
    (#xC628 "As Shot Neutral")
    (#xC62A "Baseline Exposure")
    (#xC62B "Baseline Noise")
    (#xC62C "Baseline Sharpness")
    (#xC62F "Camera Serial Number")
    (#xC632 "AntiAliasStrength")
    (#xC65D "RawDataUniqueID")
    (#xC68E "MaskedAreas")))

(defun decode-white-balance (v)
  (case v
    (1 "Auto")
    (2 "Daylight")
    (3 "Tungsten")
    (4 "Fluorescent")
    (5 "Flash")
    (6 "Manual")
    (10 "Cloudy")
    (11 "Shade")))

(defun decode-crop-mode (values &optional portrait-p)
  (if (and (vectorp values) (= (length values) 5))
      (let ((crop (case (aref values 0)
		    (1 "No Crop (645)")
		    (2 "1:1 (6x6)")
		    (3 "7:6 (6x7)")
		    (4 "5:4 (4x5)")
		    (5 "11:8.5 (Letter)")
		    (7 "297:210 (A4)")
		    (8 "3:2 (6x9)")
		    (9 "3:2 Crop (24x36)")
		    (10 "16:9 (Screen)")
		    (11 "2:1 (6x12)")
		    (12 "65:24 (XPan)")
		    (t "unknown")))
	    (w (aref values 3))
	    (h (aref values 4)))
	(format nil "~A: ~Sx~S" crop (if portrait-p h w) (if portrait-p w h)))
      nil))

(defun decode-drive-mode (values)
  (case values
    (0 "Single Shot")
    (1 "Continuous")
    (2 "Self Timer")
    (3 "Interval")
    (4 "Exposure Bracketing")
    (5 "Focus Bracketing")))

(defparameter *hasselblad-makernote-tags*
  `((#x05 "White balance setting" ,#'decode-white-balance)
    (#x13 "Quality"
	  ,#'(lambda (values)
	       (if (and (vectorp values) (= (length values) 2)
			(or (= (aref values 1) (* 16 512))
			    (= (aref values 1) (* 14 512))
			    (= (aref values 1) (* 12 512))))
		   (format nil "~A bit (~S ~S)" (/ (aref values 1) 512)
			   (aref values 0) (aref values 1))
		   nil)))
    (#x15 "Model")
    (#x28 "Phocus Version")
    ;; #x002A only .fff - almost same values as C621 (Color Matrix 1), but with the leading 1 values.
    ;; #x46 related somehow to ISO. On 50C, always == ISO / 100
    ;;      On 100C, 201/200 for ISO 64 and 25079/1000 for ISO 1600
    (#x47 "Focus Point")
    (#x4A "Shutter type"
	  ,#'(lambda (values)
	       (case values
		 (0 "leaf shutter")
		 (1 "electronic shutter"))))
    (#x59 "Crop Mode"			; only in .3FR
	  ,#'decode-crop-mode)
    (#x5B "Release Mode" ,#'decode-drive-mode)
    (#x5C "Release Count")
    ;; 5E
    ;; 5F related to shutter type. On Mode A, 1 is leaf, 0 is electronic.
    ;;    But on FULL AUTO mode, it is always 5E == 2 and 5F == 0, regardless of shutter type
    (#x61 "Lens serial number")
    (#x63 "Exact Exposure time")))

(defparameter *tag-info*
  (let* ((sources (list *standard-tags* *hasselblad-makernote-tags*))
	 (table (make-hash-table :size (apply #'+ (mapcar #'length sources)))))
    (flet ((add (e)
	     (let* ((tag (car e))
		    (info (cdr e))
		    (extant (gethash tag table)))
	       (when extant
		 (error "duplicate tag: #x~4,'0X" tag))
	       (setf (gethash tag table) info))))
      (mapc #'(lambda (tags) (mapc #'add tags)) sources)
      table)))


(defun tiff-tag-info (tag)
  (gethash tag *tag-info*))

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
    (let* ((info (make-tiff))
	   (magic (get-u16 bytes 2))
	   (ifds nil))
      (declare (type list ifds))
      (unless (= magic 42)
	(error "invalid magic number: ~a" magic))
      (note-region info 0 2 "BOM")
      (note-region info 2 2 "Magic")
      (note-region info 4 4 "First IFD address")
      (do ((rest-ifds (list (cons (cons "IFD" 0) (get-u32 bytes 4)))))
	  ((null rest-ifds))
	(destructuring-bind (name . addr) (car rest-ifds)
	  (multiple-value-bind (ifd more-ifds)
	      (parse-ifd bytes info name addr)
	    (push ifd ifds)
	    (setq rest-ifds (append (cdr rest-ifds) more-ifds)))))
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
			      :values (get-values buf tag type count voffs)))))
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
