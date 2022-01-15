all:
	dune build

install:
	dune build
	mv ./_build/default/src/minilustre.exe ./bin/minilustre

clean:
	rm -rf ./bin/* ./examples/*.o ./examples/*.ll ./examples/*.s ./examples/*.cm* ./examples/*exe
	dune clean

simple:
	./_build/default/src/minilustre.exe -main n -steps 3 examples/simple.mls
	ocamlfind ocamlopt -linkpkg -package graphics examples/simple.ml -o bin/simple.ml.exe
	llc examples/simple.ll
	clang examples/simple.s -o bin/simple.ll.exe

exec:
	@echo "-[ OCaml ]-"
	@echo ""
	@./bin/simple.ml.exe
	@echo ""
	@echo "-[ LLVM ]-"
	@echo ""
	@./bin/simple.ll.exe
