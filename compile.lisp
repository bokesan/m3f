#+CCL (require 'asdf)
(pushnew (uiop/os:getcwd) quicklisp:*local-project-directories* :test #'equalp)
(ql:quickload '("alexandria" "clingon"))
#-ECL (asdf:make "m3f/executable")
#+ECL (progn (ql:quickload '("m3f" "m3f/executable"))
	     (asdf:make-build :m3f/executable :type :program :move-here #P"./src"
			      :epilogue-code '(cli:main)))
#+(OR CCL ECL) (quit)
