(defpackage :config
  (:use :cl)
  (:export :read-config-file :load-config))

(in-package :config)


(declaim (inline first-char last-char))

(defun first-char (s)
  "First character of non-empty string."
  (char s 0))

(defun last-char (s)
  "Last character of non-empty string."
  (char s (- (length s) 1)))

(defun read-config-stream (stream table)
  "Read config from STREAM, adding the headers as keys and alists as values to TABLE.
Returns TABLE.
Headers, keys, and values are always strings.

The config format is:

[header]
name1 = value1
name2 = value2
..."
  (declare (type stream stream)
	   (type hash-table table))
  (let ((table (make-hash-table :test #'equalp))
	(whitespace '(#\Space #\Tab))
	(whitespace-and-brackets '(#\Space #\Tab #\[ #\])))
    (labels ((accept (header entries)
	       (declare (type string header)
			(type list entries))
	       (when entries
		 (setf (gethash header table)
		       (append (nreverse entries) (gethash header table nil)))))
	     
	     (parse (header entries)
	       (declare (type string header)
			(type list entries))
	       (let ((line (read-line stream nil)))
		 (if (not line)
		     (accept header entries)
		     (let ((line (string-trim whitespace line)))
		       (cond ((or (zerop (length line)) (char= (first-char line) #\#))
			      (parse header entries))
			     ((char= (first-char line) #\[)
			      (unless (char= (last-char line) #\])
				(error "[header] expected. ~S" line))
			      (accept header entries)
			      (parse (string-trim whitespace-and-brackets line) nil))
			     (t (parse
				 header
				 (cons (let ((sep (position #\= line)))
					 (if (not sep)
					     (cons line "")
					     (cons (string-right-trim whitespace (subseq line 0 sep))
						   (string-left-trim whitespace (subseq line (+ sep 1))))))
				       entries)))))))))
      (parse "" nil)
      table)))


(defun read-config-file (filespec &key (add-to (make-hash-table :test #'equalp)))
  ;; (format *trace-output* "loading configuration from ~S~%" filespec)
  (with-open-file (s filespec)
    (read-config-stream s add-to)))

(defun load-config ()
  (let ((config (make-hash-table :test #'equalp)))
    (loop for file in (mapcar #'uiop:file-exists-p
			      (uiop:xdg-config-pathnames "m3f.conf"))
	  when file
	  do (read-config-file file :add-to config))
    (loop for dir in (mapcar #'uiop:directory-exists-p (uiop:xdg-config-pathnames "m3f"))
	  when dir
	  do (loop for file in (uiop:directory-files dir)
		   when (or (pathname-match-p file "*.conf")
			    (pathname-match-p file "*.txt"))
		   do (read-config-file file :add-to config)))
    config))
