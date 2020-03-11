TMP_DOC_DIR:=/tmp/tjr_lru_cache
scratch:=/tmp/l/github/scratch

default: all

all::
	$(MAKE) test

-include Makefile.ocaml

test: FORCE
	dune build test/test_main.exe
#	dune build test/test_performance.exe

run_tests:
	dune exec test/test_main.exe 1 6

run_performance_test:
	$(MAKE) test
	find _build -name "test_main.exe" -exec cp \{\} . \;
	./test_main.exe -count 10000 -cap 100 -evict 10

# for auto-completion of Makefile target
clean::
	rm *.exe
