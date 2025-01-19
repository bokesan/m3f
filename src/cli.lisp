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
	(all (clingon:getopt cmd :all))
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
		(cond ((not (or all privacy diff map-p))
		       (report:summary tiff))
		      (t (when (or all privacy diff)
			   (report:detail tiff
					  :filter (cond ((and all privacy) :sensitive)
							((and all diff) :diff)
							(all :all)
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
    :long-name "map"
    :key :map
    :description "show file layout map")
   (clingon:make-option
    :flag
    :short-name #\A :long-name "all"
    :key :all
    :description "display unknown tags")
   (clingon:make-option
    :flag
    :short-name #\V :long-name "no-volatile"
    :key :no-volatile
    :description "omit tags that change with each image, such as timestamps")
   (clingon:make-option
    :flag
    :short-name #\S :long-name "privacy"
    :key :privacy
    :description "omit tags containing sensitive information such as serial numbers. Implies --no-volatile")
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
    :description "truncate byte or undefined fields to this size in output")
   (clingon:make-option
    :counter
    :description "show more progress messages. Use multiple times (e.g. -vvv) for more output"
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
