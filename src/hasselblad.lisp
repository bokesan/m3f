(defpackage :hasselblad
  (:use :cl)
  (:export :*makernote-tags*
	   :decode-crop-mode :decode-drive-mode
	   :decode-white-balance
	   :decode-serial-number
	   ))

(in-package :hasselblad)

(defparameter *focal-length-table*
  #(27 20
    28 20
    29 21
    32 23
    34 24
    35 25
    36 26
    38 27
    39 28
    40 29
    41 30
    42 31
    44 32
    45 33
    47 35
    49 38
    55 45
    68 65
    75 80
    79 90
    89 120
    93 135))

(defun decode-focal-length (v)
  "Returns the focal length, or NIL for unknown codes.
The second value is T if the focal length is exact or NIL if it was approximated."
  (let* ((tbl *focal-length-table*)
	 (n (length tbl)))
    (cond ((< v (aref tbl 0)) (values nil nil))
	  ((= v (aref tbl 0)) (values (aref tbl 1) t))
	  (t
	   (do ((i 2 (+ i 2)))
	       ((>= i n) (values nil nil))
	     ;; > v (aref tbl (- i 2))
	     (when (<= v (aref tbl i))
	       (when (= v (aref tbl i))
		 (return (values (aref tbl (+ i 1)) t)))
	       (let ((f (/ (- v (aref tbl (- i 2))) (- (aref tbl i) (aref tbl (- i 2))))))
		 (return (values (+ (aref tbl (- i 1)) (* f (- (aref tbl (+ i 1)) (aref tbl (- i 1)))))
				 nil)))))))))

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

(defun decode-0017 (v)
  (if (and (vectorp v) (= (length v) 17))
      (format nil "
     ~2,'0x, AF: ~A, ~
TS: 20~2,'0x-~2,'0x-~2,'0x ~2,'0x:~2,'0x:~2,'0x~A, ~
~2,'0x ~2,'0x ~2,'0x ~2,'0x ~2,'0x ~2,'0x, ~
FL: ~A, ~2,'0x ~2,'0x"
	      (aref v 0)
	      (case (aref v 1)
		(2 "Manual")
		(#x42 "AF")
		(#xC2 "True Focus")
		(t (format nil "~2,'0x" (aref v 1))))
	      (aref v 2) (aref v 3) (aref v 4) (logand (aref v 5) #x7F) (aref v 6) (aref v 7)
	      (if (logbitp 7 (aref v 5)) " (HI)" "")
	      (aref v 8) (aref v 9) (aref v 10) (aref v 11) (aref v 12) (aref v 13)
	      (multiple-value-bind (fl exact-p)
		  (decode-focal-length (aref v 14))
		(cond ((not fl) "unknown")
		      (exact-p (format nil "~A mm" fl))
		      (t (format nil "~~~A mm" fl))))
	      (aref v 15) (aref v 16))
      nil))

(defparameter *makernote-tags*
  `((#x05 "WhiteBalance" ,#'decode-white-balance)
    (#x13 "Quality?" ,#'hex+binary)
    (#x15 "Model")
    (#x0017 nil ,#'decode-0017)
    ;; #x17 17 bytes camera info?
    ;;   Byte 0: always 1
    ;;   Byte 1: bit #x40: AF, #x80: True Focus. Bit #x02 always on.
    ;;   Byte 2-4: date in hex, e.g. #x25, #x03, #x07 == 2025-03-07
    ;;   Byte 5: hour. Only lower 7 bits: #x89: 09:00, #x90 == 10:00, #x92: 12:00, #x99 == 19:00
    ;;   Bytes 6-7: timestamp in hex
    ;;   Byte 11 and 15: increase per exposure often by 1 but sometimes more, in lockstep,
    ;;     with byte 15 == Byte 11 + 20 (0x14)
    ;;   Byte 14: focal length in the strange encoding as #x0018, 2 and 3
    ;;      With dumb adapters: 0
    ;;   Bytes 15, 16: with dumb adapter, always 255, 1
    ;;      with native lens, byte 16 is always 0 or 255
    ;; #x18 17 bytes lens info?
    ;;   Byte 0: always 1
    ;;   Bytes 2 and 3: min. and max. focal length in strange encoding.
    ;;   Byte 4: 0xF8 for HC 0.8 adapter
    ;;   Byte 5, except high bit: extension in mm. If the high bit is set, the lens changes in Phocus!
    ;;   Byte 6: decreases with increasing focus distance
    ;;       XCD45P, XCD80: 14 at close focus - 0 for infinity
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
