all:
	dune build

install:
	dune build
	mv ./_build/default/src/minilustre.exe ./bin/minilustre

clean:
	rm -rf ./bin/minilustre
	dune clean

simple:
	./_build/default/src/minilustre.exe -main n -steps 10000000 examples/simple.mls
	ocamlfind ocamlopt -linkpkg -package graphics examples/simple.ml
