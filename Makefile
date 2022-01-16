# commands
BUILD      = dune build
CC         = clang
LLC        = llc
CLEAN      = dune clean
RM         = rm -rf
OCAMLFIND  = ocamlfind
OCAMLC     = ocamlopt
OCAMLFLAGS = -linkpkg -package graphics
MLSC       = ./_build/default/src/minilustre.exe
MLSCFLAGS  = -main n -steps 3

# compilers
COMPILE.ocaml = $(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS)
COMPILE.mls   = $(MLSC) $(MLSCFLAGS)

# files
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
	$(COMPILE.mls) -ll-only $(@:.ll=.mls)

examples/%.ml: minilustre_exec
	$(COMPILE.mls) -ml-only $(@:.ml=.mls)

examples/%.s: examples/%.ll
	$(LLC) $^

bin/%.ll.exe: examples/%.s
	$(CC) $^ -o $@

bin/%.ml.exe: examples/%.ml
	$(COMPILE.ocaml) $< -o $@

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
	$(RM) $(BAZAR)

cleanall: clean
	$(RM) $(BIN)
