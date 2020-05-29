bin:
	dune build ./bin/main.exe
	ln -sf _build/default/bin/main.exe main.exe

js:
	dune build ./js/jsbridge.bc.js

tests:
	dune runtest

clean:
	dune clean


.PHONY: bin js tests clean
