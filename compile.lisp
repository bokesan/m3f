#+CCL (require 'asdf)
(pushnew (uiop/os:getcwd) quicklisp:*local-project-directories* :test #'equalp)
(ql:quickload '("asdf" "alexandria" "clingon"))

#-ECL (asdf:make "m3f/executable")

#+ECL (ql:quickload '("m3f/executable"))
#+ECL (asdf:make-build :m3f/executable :type :program :move-here #P"./"
		       :prologue-code '(progn (setq *load-verbose* nil) (require "asdf"))
		       :epilogue-code '(cli:main))

#+(OR CCL ECL) (quit)
