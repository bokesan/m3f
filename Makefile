LISP = sbcl
LISP_LOAD = --load

ifeq ($(LISP),sbcl)
  LISP_FLAGS = --dynamic-space-size 4096 --non-interactive
endif

sources := $(wildcard src/*.lisp)
test_sources := $(shell echo test/*.lisp)

.PHONY: unit-tests test clean

ALL: unit-tests m3f test

m3f: $(sources) m3f.asd compile.lisp
	$(LISP) $(LISP_FLAGS) $(LISP_LOAD) compile.lisp

unit-tests:
	$(LISP) $(LISP_FLAGS) $(LISP_LOAD) run-tests.lisp

test: m3f
	test/test-output.sh

clean:
	$(RM) m3f
