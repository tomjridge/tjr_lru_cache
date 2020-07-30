default: all

all::
	$(MAKE) test

-include Makefile.ocaml

test_exe lru_test_main.exe &: FORCE
	dune build test/lru_test_main.exe
	cp _build/default/test/lru_test_main.exe ${CURDIR} # assumes default build context

#	dune build test/test_performance.exe

run_tests: lru_test_main.exe
	./lru_test_main.exe 1 6

run_performance_test: lru_test_main.exe
	./lru_test_main.exe -count 10000 -cap 100 -evict 10

update_generated_doc::
	cd src && (ocamldoc_pyexpander im_intf.ml)
	cd src && (ocamldoc_pyexpander mt_intf.ml)
	cd src && (ocamldoc_pyexpander summary.t.ml > summary.ml)


# for auto-completion of Makefile target
clean::
	rm -f *.exe
	rm -f src/GEN.*
