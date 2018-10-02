build:
	dune build @install
	dune build test/test_main.exe

clean:
	dune clean

run_tests:
	dune exec test/test_main.exe 1 5

# run_more_tests:
# 	dune exec test/test_main.exe 1 10
