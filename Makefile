# commands
BUILD      = dune build
CC         = clang
CFLAGS     = -O3
LLC        = llc
LLCFLAGS   = -O3
CLEAN      = dune clean
RM         = rm -rf
OCAMLFIND  = ocamlfind
OCAMLC     = ocamlopt
OCAMLFLAGS = -O3 -linkpkg -package graphics
MLSC       = ./_build/default/src/minilustre.exe
MLSCFLAGS  = -main n -steps 100000000

# compilers
COMPILE.ocaml = $(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS)
COMPILE.mls   = $(MLSC) $(MLSCFLAGS)
COMPILE.ll    = $(LLC) $(LLCFLAGS)
COMPILE.c     = $(CC) $(CFLAGS)

# files
OBJ   = $(wildcard examples/*.o)
LL    = $(wildcard examples/*.ll)
ASM   = $(wildcard examples/*.s)
CMI   = $(wildcard examples/*.cmi)
CMX   = $(wildcard examples/*.cmx)
BIN   = $(wildcard bin)
BAZAR = $(OBJ) $(LL) $(ASM) $(CMI) $(CMX) $(EXEC)
TESTS = $(wildcard examples/*.mls)
PROG  = src/minilustre.ml

.PHONY: all
all: minilustre

.PHONY: minilustre
minilustre:
	$(BUILD) $(PROG:.ml=.exe)

examples/%.ll: minilustre
	$(COMPILE.mls) -ll-only $(@:.ll=.mls)

examples/%.ml: minilustre
	$(COMPILE.mls) -ml-only $(@:.ml=.mls)

examples/%.s: examples/%.ll
	$(COMPILE.ll) $<

bin/%.ll.exe: examples/%.s
	@mkdir -p bin/
	$(COMPILE.c) $< -o $@

bin/%.ml.exe: examples/%.ml
	@mkdir -p bin/
	$(COMPILE.ocaml) $< -o $@

.PHONY: exec
exec: bin/simple.ml.exe bin/simple.ll.exe
	@echo "\n-------------------"
	@echo "-----[ OCaml ]-----"
	@echo "-------------------\n"
	@./bin/simple.ml.exe
	@echo "\n\n-------------------"
	@echo "-----[ LLVM  ]-----"
	@echo "-------------------\n"
	@./bin/simple.ll.exe
	@echo ""

.PHONY: clean
clean:
	$(CLEAN)
	$(RM) $(BAZAR)

.PHONY: cleanall
cleanall: clean
	$(RM) $(BIN)
