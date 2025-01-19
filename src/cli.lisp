(defpackage :cli
  (:use :cl)
  (:export :main))

(in-package :cli)

;; Don't add anything before these defparameter forms, because they are referenced
;; by form number in the ASDF file. And don't change their order.
(defparameter *version* "0.1.0")
(defparameter *author* "Christoph Breitkopf <chbreitkopf@gmail.com>")
(defparameter *description* "Extract metadata from Hasselblad raw images.")


(defun top-level/handler (cmd)
  (let ((files (clingon:command-arguments cmd))
	(map-p (clingon:getopt cmd :map))
	(detail (clingon:getopt cmd :detail))
	(unknown (clingon:getopt cmd :unknown))
	(diff (clingon:getopt cmd :no-volatile))
	(privacy (clingon:getopt cmd :privacy))
	(decode-words (clingon:getopt cmd :decode-words))
	(max-bytes (clingon:getopt cmd :max-bytes)))
    (if (null files)
	(clingon:print-usage cmd t)
	(dolist (f files)
	  (when (cdr files)
	    (format t "======== ~A~%" f))
	  (handler-case
	      (let ((tiff (tiff:read-tiff f :max-bytes max-bytes)))
		(cond ((not (or detail unknown map-p))
		       (report:summary tiff :privacy privacy))
		      (t (when (or detail unknown)
			   (report:detail tiff
					  :filter (cond ((and unknown privacy) :sensitive)
							((and unknown diff) :diff)
							(unknown :all)
							(privacy :known-sensitive)
							(diff :known-diff)
							(t :known))
					  :max-bytes max-bytes
					  :words decode-words))
			 (when map-p
			   (report:layout tiff)))))
	    (end-of-file () (format t "error: end of file encountered, malformed tiff?~%")))))))

(defun top-level/options ()
  "Creates and returns the options for the top-level command"
  (list
   (clingon:make-option
    :flag
    :short-name #\D :long-name "detail"
    :key :detail
    :description "show IFD entries instead of summary information")
   (clingon:make-option
    :flag
    :long-name "layout"
    :key :map
    :description "show file layout map")
   (clingon:make-option
    :flag
    :short-name #\U :long-name "unknown"
    :key :unknown
    :description "display unknown tags. Implies --detail")
   (clingon:make-option
    :flag
    :short-name #\V :long-name "no-volatile"
    :key :no-volatile
    :description "omit tags that change with each image, such as timestamps.
The image count for exposure or focus bracket sequences is not omitted.")
   (clingon:make-option
    :flag
    :long-name "privacy"
    :key :privacy
    :description "omit tags containing possibly sensitive information such as serial numbers, timestamps, or location data")
   (clingon:make-option
    :flag
    :long-name "decode-words"
    :key :decode-words
    :description "display short, long, and rational values for bytes and unknown")
   (clingon:make-option
    :integer
    :long-name "max-bytes"
    :initial-value 32
    :key :max-bytes
    :description "truncate byte or undefined fields to this size in detail output")
   (clingon:make-option
    :counter
    :description "show progress messages. Use multiple times (e.g. -vvv) for more output"
    :short-name #\v
    :long-name "verbose"
    :persistent t
    :key :verbose)))

(defun top-level/command ()
  "Creates and returns the top-level command"
  (clingon:make-command
   :name "m3f"
   :description *description*
   :version *version*
   :authors (list *author*)
   :usage "[option...] file..."
   :options (top-level/options)
   :handler #'top-level/handler))

(defun main ()
  (let ((app (top-level/command)))
    (clingon:run app)))
