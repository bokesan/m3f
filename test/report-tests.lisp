(defpackage :report-tests
  (:use :cl :fiveam))

(in-package :report-tests)

(def-suite report-tests)
(in-suite report-tests)

(test summary
      (let ((tiff (tiff:read-tiff "test/resources/raw1.3FR"))
	    (s (make-string-output-stream)))
	(report:summary tiff :stream s)
	(is (string=
	     "        Device: CFV 100C/907X
       Created: 2025:01:16 15:46:52
    Dimensions: 8742x11656, crop mode 65:24 (XPan): 4302x11656
          Lens: XCD 28P, serial number: 8QHI15094 (year: 2024)
     Converter: 
     Extension: 
           HTS: ?
           ISO: 400
       Shutter: 1/160
      Aperture: f/4.0
   Light Meter: Centre W
 Exposure Mode: Aperture
    Focus Mode: Single
 Serial Number: JT63001117
GPS Coordinate: ?
   Orientation: Rotate 90 CW
       Quality: 16 bit
 White Balance: Daylight
    Drive Mode: Single Shot
"
	     (get-output-stream-string s)))))
