sources := $(wildcard src/*.lisp)
test_sources := $(shell echo test/*.lisp)

.PHONY: test clean

ALL: m3f

m3f: $(sources) m3f.asd compile.lisp
	sbcl --dynamic-space-size 2048 --load compile.lisp
	mv ./src/m3f .

m3f-ccl: $(sources) m3f.asd compile.lisp
	ccl64 --load compile.lisp
	mv ./src/m3f m3f-ccl

test: $(sources) $(test_sources) m3f.asd test.lisp
	sbcl --non-interactive --load test.lisp

test-ap: m3f
	./m3f illum test/Z7D_323[23456].NEF

test-shift: m3f
	./m3f illum -H 8 test/Z7D_323[789].NEF test/Z7D_324[01].NEF

test-channels: m3f
	./m3f illum --by-channel test/Z7D_3232.NEF

clean:
	$(RM) m3f m3f-ccl
