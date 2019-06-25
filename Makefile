TMP_DOC_DIR:=/tmp/tjr_lru_cache
scratch:=/tmp/l/github/scratch

default: all

-include Makefile.ocaml

test: FORCE
	dune build test/test_main.exe

run_test:
	dune exec test/test_main.exe 1 6

# for auto-completion of Makefile target
clean::

