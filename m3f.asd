(defsystem "m3f"
  :version (:read-file-form "src/cli.lisp" :at (2 2))
  :author (:read-file-form "src/cli.lisp" :at (3 2))
  :description (:read-file-form "src/cli.lisp" :at (4 2))
  :license "MIT"
  :depends-on ("alexandria")
  :pathname "src"
  :components ((:file "arrays")
	       (:file "binary" :depends-on ("arrays"))
	       (:file "boxes" :depends-on ("binary"))
	       (:file "tiff" :depends-on ("binary"))
	       (:file "hasselblad")
	       (:file "report" :depends-on ("tiff" "hasselblad"))))

(defsystem "m3f/executable"
  :build-operation program-op
  :build-pathname "m3f"
  :entry-point "cli:main"
  :depends-on ("m3f" "clingon")
  :components ((:file "src/cli")))

(defsystem "m3f/tests"
    :depends-on ("fiveam" "m3f")
    :pathname "test"
    :components ((:file "arrays-tests")
		 (:file "tiff-tests")
		 (:file "report-tests")))
