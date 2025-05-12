(defpackage :hasselblad
  (:use :cl)
  (:export :*makernote-tags*
	   :decode-crop-mode :decode-drive-mode
	   :decode-white-balance
	   :decode-serial-number
	   ))

(in-package :hasselblad)

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

(defun hex+binary (values)
  (typecase values
    (string values)
    (integer (format nil "~X (~B)" values values))
    (vector
     (let ((s (make-string-output-stream))
	   (sep ""))
       (loop for v across values do
	    (write-string sep s)
	    (setq sep "  ")
	    (if (integerp v)
		(format s "~X (~B)" v v)
		(prin1 v s)))
       (get-output-stream-string s)))))

(defparameter *makernote-tags*
  `((#x05 "WhiteBalance" ,#'decode-white-balance)
    (#x13 "Quality?" ,#'hex+binary)
    (#x15 "Model")
    ;; #x17 17 bytes camera info?
    ;;   Byte 0: always 1
    ;;   Byte 1: bit #x40: AF, #x80: True Focus
    ;;   Byte 7: focus position? Increases with longer distance
    ;; #x18 17 bytes lens info?
    ;;   Byte 0: always 1
    ;;   Bytes 2 and 3: min. and max. focal length in strange encoding, e.g.:
    ;;     29: 21mm, 41: 30mm, 68: 65mm, 75: 80mm, 89: 120mm
    ;;   Byte 4: 0xF8 for HC 0.8 adapter
    ;;   Byte 5, except high bit: extension in mm. If the high bit is set, the lens changes in Phocus!
    (#x28 "PhocusVersion")
    ;; #x002A only .fff - almost same values as C621 (Color Matrix 1), but with the leading 1 values.
    ;; #x46 related somehow to ISO. On 50C, always == ISO / 100
    ;;      On 100C, 201/200 for ISO 64 and 25079/1000 for ISO 1600
    (#x46 "Gain?")
    (#x47 "FocusPoint")
    (#x4A "ShutterType"
	  ,#'(lambda (values)
	       (case values
		 (0 "leaf shutter")
		 (1 "electronic shutter"))))
    (#x59 "CropMode"			; only in .3FR
	  ,#'decode-crop-mode)
    (#x5B "DriveMode" ,#'decode-drive-mode)
    (#x5C "ReleaseCount") ; within bracket sequence
    ;; 5E
    ;; 5F related to shutter type. On Mode A, 1 is leaf, 0 is electronic.
    ;;    But on FULL AUTO mode, it is always 5E == 2 and 5F == 0, regardless of shutter type
    (#x61 "LensSerialNumber")
    (#x63 "ExactExposureTime")))


(defun decode-serial-number (s)
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
