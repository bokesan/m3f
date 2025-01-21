LISP = sbcl
LISP_LOAD = --load

ifeq ($(LISP),sbcl)
  LISP_FLAGS = --dynamic-space-size 4096 --non-interactive
endif

sources := $(wildcard src/*.lisp)
test_sources := $(shell echo test/*.lisp)

.PHONY: test clean

ALL: m3f test

m3f: $(sources) m3f.asd compile.lisp
	$(LISP) $(LISP_FLAGS) $(LISP_LOAD) compile.lisp

test: $(sources) $(test_sources) m3f.asd run-tests.lisp
	$(LISP) $(LISP_FLAGS) $(LISP_LOAD) run-tests.lisp

clean:
	$(RM) m3f
