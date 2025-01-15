(defsystem "m3f"
  :version "0.1"
  :depends-on ("alexandria")
  :pathname "src"
  :components ((:file "binary")
	       (:file "info" :depends-on ("binary"))))

(defsystem "m3f/executable"
  :build-operation program-op
  :build-pathname "lencheck-exe"
  :entry-point "cli:main"
  :depends-on ("m3f" "clingon")
  :pathname "src"
  :components ((:file "cli")))

(defsystem "m3f/tests"
    :depends-on ("fiveam" "m3f")
    :pathname "test"
    :serial t
    :components ((:file "test-tiff")))
