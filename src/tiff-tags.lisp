(in-package :tiff)


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
	  #x8298  ; copyright
	  #x02BC) ; application notes
	 t))))


(declaim (ftype (function ((signed-byte 33) (signed-byte 33)) real) apex-value))
(defun apex-value (n d)
  "Return the APEX value of a rational (signed or unsigned)."
  (expt 2 (/ (/ n d) 2)))

(declaim (ftype (function ((unsigned-byte 16)) boolean) apex-tag-p))
(defun apex-tag-p (tag)
  (or (= tag #x9201)
      (= tag #x9202)
      (= tag #x9205)))


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
    (#xA001 "ColorSpace"
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
    (#xA405 "FocalLengthIn35mmFormat")
    (#xA40C "SubjectDistanceRange")
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
    (#xA430 "OwnerName")
    (#xA431 "SerialNumber")
    (#xA432 "LensInfo")
    (#xA433 "LensMake")
    (#xA434 "LensModel")
    (#xA435 "LensSerialNumber")
    (#xB4C3 "HasselbladRawImage")
    (#xC519 "HasselbladXML") ; PLIST
    (#xC51B "HasselbladExif")
    (#xC614 "UniqueCameraModel")
    (#xC61A "BlackLevel")
    (#xC61D "WhiteLevel")
    (#xC61F "DefaultCropOrigin")
    (#xC620 "DefaultCropSize")
    (#xC621 "ColorMatrix1")		; only in .3FR
    (#xC622 "ColorMatrix2")
    (#xC623 "CameraCalibration1")
    (#xC624 "CameraCalibration2")
    (#xC627 "AnalogBalance")		; only in .fff
    (#xC628 "AsShotNeutral")
    (#xC62A "BaselineExposure")
    (#xC62B "BaselineNoise")
    (#xC62C "BaselineSharpness")
    (#xC62F "CameraSerialNumber")
    (#xC632 "AntiAliasStrength")
    (#xC65D "RawDataUniqueID")
    (#xC68E "MaskedAreas")))


(defun make-tag-table (sources)
  "Collect lists of tag definitions into hash table with the numeric tag value as key."
  (let ((table (make-hash-table :size (apply #'+ (mapcar #'length sources)))))
    (flet ((add (e)
	     (let* ((tag (car e))
		    (info (cdr e))
		    (extant (gethash tag table)))
	       (when extant
		 (error "duplicate tag: #x~4,'0X" tag))
	       (setf (gethash tag table) info))))
      (mapc #'(lambda (tags) (mapc #'add tags)) sources)
      table)))
