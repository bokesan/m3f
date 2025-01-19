#+CCL (require 'asdf)
#+CCL (push (uiop/os:getcwd) ql:*local-project-directories*)
#+ECL (ql:quickload '("m3f" "m3f/executable"))
#-ECL (asdf:make "m3f/executable")
#+ECL (asdf:make-build :m3f/executable :type :program :move-here #P"./src"
		       :epilogue-code '(cli:main))
#+(OR CCL ECL) (quit)
