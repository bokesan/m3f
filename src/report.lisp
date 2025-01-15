(defpackage :report
  (:use :cl :tiff)
  (:export :summary :detail :layout))

(in-package :report)

(defun summary (tiff &optional (s t))
  "Show metadata summary as in Phocus \"Capture Info\" tab."
  TODO)

(defun detail (tiff &optional (s t))
  "Show TIFF IFDs with entries."
  TODO)

(defun layout (tiff &optional (s t))
  "Show TIFF file layout."
  TODO)
