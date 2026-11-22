(defpackage :cli
  (:use :cl)
  (:import-from :clingon :option :option-value)
  (:import-from :utils :parse-as-rational)
  (:export :main))

(in-package :cli)

;; Don't add anything before these defparameter forms, because they are referenced
;; by form number in the ASDF file. And don't change their order.
(defparameter *version* "0.2.0")
(defparameter *author* "Christoph Breitkopf <chbreitkopf@gmail.com>")
(defparameter *description* "Extract metadata from Hasselblad raw images.")


;;; Clingon :rational option

(defun parse-rational-or-lose (s)
  (let ((val (parse-as-rational s)))
    (unless val
      (error 'option-derive-error :reason (format nil "Cannot parse ~A as rational" s)))
    val))

(defclass option-rational (option)
  ()
  (:default-initargs :parameter "RATIONAL")
  (:documentation "An option for rational numbers"))

(defmethod clingon:make-option ((kind (eql :rational)) &rest rest)
  (apply #'make-instance 'option-rational rest))

(defmethod clingon:initialize-option ((option option-rational) &key)
  "Initializes the rational option. In case the option was
  first initialized by other means, such as environment variables,
  we make sure that the provided value is a valid rational number."
  (call-next-method)

  ;; Nothing to initialize further
  (unless (option-value option)
    (return-from clingon:initialize-option))

  (let ((value (option-value option)))
    (setf (option-value option)
          (etypecase value
            (rational value)
            (string (parse-rational-or-lose value))))))

(defmethod clingon:derive-option-value ((option option-rational) arg &key)
  (parse-rational-or-lose arg))

;;; ---



(declaim (ftype (function ((or null string)) (or null pathname)) ensure-directory))

(defun ensure-directory (path)
  "If PATH is not NIL, convert it to a directory pathname.
Signal an error if that directory does not exist."
  (if path
      (let ((dir (uiop:ensure-directory-pathname path)))
	(unless (uiop:directory-exists-p dir)
	  (error "output folder not found: ~S" path))
	dir)
      nil))


(defmacro with-options ((cmd options &optional arguments) &body body)
  (let ((binds (mapcar #'(lambda (sym)
			   `(,sym (clingon:getopt ,cmd ,(intern (symbol-name sym) "KEYWORD"))))
		       options)))
    (when arguments
      (push `(,arguments (clingon:command-arguments ,cmd)) binds))
    `(let ,binds ,@body)))


(defun top-level/handler (cmd)
  (with-options (cmd
		 (map-p detail unknown no-volatile privacy decode-words x-mode
		  f-number focal-length lens output-dir force verbose max-bytes)
		 files)
    (if (null files)
	(clingon:print-usage cmd t)
	(let ((config (config:load-config))
	      (lensinfo nil))
	  (when lens
	    (setf lensinfo (gethash lens (lenses:from-config config)))
	    (unless lensinfo
	      (error "lens not found: ~S" lens)))
	  (dolist (f files)
	    (when (and (cdr files) (not (or x-mode f-number lens)))
	      (format t "======== ~A~%" f))
	    (handler-case
		(let ((tiff (tiff:read-tiff f)))
		  (cond (x-mode
			 (when verbose
			   (format t "~A:~%" f))
			 (report:experimental tiff x-mode))
			((or f-number focal-length lens)
			 (tiff:write-metadata f :f-number f-number :lens lensinfo
					      :focal-length focal-length
					      :output-dir output-dir
					      :force force))
			((not (or detail unknown map-p))
			 (report:summary tiff :privacy privacy))
			(t (when (or detail unknown)
			     (report:detail tiff
					    :filter (cond ((and unknown privacy) :sensitive)
							  ((and unknown no-volatile) :diff)
							  (unknown :all)
							  (privacy :known-sensitive)
							  (no-volatile :known-diff)
							  (t :known))
					    :max-bytes max-bytes
					    :words decode-words))
			   (when map-p
			     (report:layout tiff)))))
	      (end-of-file () (format t "error in ~S: end of file encountered, malformed tiff?~%" f))))))))

(defun top-level/options ()
  "Creates and returns the options for the top-level command"
  (list
   (clingon:make-option
    :flag
    :short-name #\v :long-name "verbose"
    :key :verbose
    :description "more verbose output")
   (clingon:make-option
    :rational :long-name "fnumber" :description "set the F-number"
    :key :f-number)
   (clingon:make-option
    :rational :long-name "focallength" :description "set the focal length"
    :key :focal-length)
   (clingon:make-option
    :string :long-name "lens" :description "set the lens"
    :key :lens)
   (clingon:make-option
    :string :short-name #\o :long-name "output-dir" :key :output-dir
    :description "destination folder when writing metadata")
   (clingon:make-option
    :flag :long-name "force" :key :force
    :description "force writing metadata for non-supported cameras or files")
   (clingon:make-option
    :integer
    :short-name #\X
    :key :x-mode
    :description "experimental options")
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
    :description "truncate byte or undefined fields to this size in detail output")))

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
