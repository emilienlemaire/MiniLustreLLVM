BUILD  = dune build
CC     = clang
LLC    = llc
CLEAN  = dune clean

OCAMLFIND = ocamlfind
OCAMLC    = ocamlopt
OCAMLOPTS = -linkpkg -package graphics

MLSC  = ./_build/default/src/minilustre.exe
OPTS  = -main n -steps 3

OBJ   = $(wildcard examples/*.o)
LL    = $(wildcard examples/*.ll)
ASM   = $(wildcard examples/*.s)
CMI   = $(wildcard examples/*.cmi)
CMX   = $(wildcard examples/*.cmx)
BIN   = $(wildcard bin/*)

BAZAR = $(OBJ) $(LL) $(ASM) $(CMI) $(CMX) $(EXEC)

all: minilustre_exec

minilustre_exec:
	$(BUILD)

examples/%.ll: minilustre_exec
	$(MLSC) $(OPTS) -ll-only $(@:.ll=.mls)

examples/%.ml: minilustre_exec
	$(MLSC) $(OPTS) -ml-only $(@:.ml=.mls)

examples/%.s: examples/%.ll
	$(LLC) $^

bin/%.ll.exe: examples/%.s
	$(CC) $^ -o $@

bin/%.ml.exe: examples/%.ml
	$(OCAMLFIND) $(OCAMLC) $(OCAMLOPTS) $< -o $@

exec: bin/simple.ml.exe bin/simple.ll.exe
	@echo "\n-------------------"
	@echo "-----[ OCaml ]-----"
	@echo "-------------------\n"
	@./bin/simple.ml.exe
	@echo "\n-------------------"
	@echo "-----[ LLVM  ]-----"
	@echo "-------------------\n"
	@./bin/simple.ll.exe

clean:
	$(CLEAN)
	rm -rf $(BAZAR)

cleanall: clean
	rm -rf $(BIN)
