LISP = sbcl
LISP_FLAGS = --dynamic-space-size 4096 --non-interactive
# LISP = ccl64
# LISP_FLAGS =

sources := $(wildcard src/*.lisp)
test_sources := $(shell echo test/*.lisp)

.PHONY: test clean

ALL: m3f test

m3f: $(sources) m3f.asd compile.lisp
	$(LISP) $(LISP_FLAGS) --load compile.lisp

test: $(sources) $(test_sources) m3f.asd run-tests.lisp
	$(LISP) $(LISP_FLAGS) --load run-tests.lisp

clean:
	$(RM) m3f
