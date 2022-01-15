all:
	dune build

install:
	dune build
	mv ./_build/default/src/minilustre.exe ./bin/minilustre

clean:
	rm -rf ./bin/minilustre ./examples/*.o ./examples/*.ll ./examples/*.s ./examples/*.cm* ./examples/*exe ./examples/*native
	dune clean

simple:
	./_build/default/src/minilustre.exe -main n -steps 3 examples/simple.mls
	ocamlfind ocamlopt -linkpkg -package graphics examples/simple.ml -o bin/simple.ml.native
	llc examples/simple.ll
	clang examples/simple.s -o bin/simple.ll.exe
