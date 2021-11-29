all:
	dune build src/

install:
	dune build src/minilustre.exe
	mv ./_build/default/src/minilustre.exe ./bin/minilustre

clean:
	rm -rf ./bin/minilustre
	dune clean
