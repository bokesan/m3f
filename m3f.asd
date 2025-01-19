(defsystem "m3f"
  :version "0.1"
  :depends-on ("alexandria")
  :pathname "src"
  :components ((:file "arrays")
	       (:file "binary" :depends-on ("arrays"))
	       (:file "tiff" :depends-on ("binary"))
	       (:file "hasselblad")
	       (:file "report" :depends-on ("tiff" "hasselblad"))))

(defsystem "m3f/executable"
  :build-operation program-op
  :build-pathname "m3f"
  :entry-point "cli:main"
  :depends-on ("m3f" "clingon")
  :pathname "src"
  :components ((:file "cli")))

(defsystem "m3f/tests"
    :depends-on ("fiveam" "m3f")
    :pathname "test"
    :serial t
    :components ((:file "test-tiff")))
