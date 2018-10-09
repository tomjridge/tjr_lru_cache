DUNE:=opam exec dune

build:
	$(DUNE) build @install
#	$(DUNE) build test/test_main.exe

install:
	$(DUNE) install

clean:
	$(DUNE) clean


doc: FORCE
	$(DUNE) build @doc

view_doc:
	google-chrome  _build/default/_doc/_html/index.html


run_tests:
	$(DUNE) exec test/test_main.exe 1 5

# run_more_tests:
# 	$(DUNE) exec test/test_main.exe 1 10



FORCE:
