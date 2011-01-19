# This Makefile is used only for testing the installation.

OCAMLC = ocamlfind ocamlc -linkpkg -verbose -package sonet.jsonlib
OCAMLOPT = ocamlfind ocamlopt -linkpkg -verbose -package sonet.jsonlib

all:
	$(OCAMLC) +unix.cma -o test_parser test_parser.ml
	$(OCAMLOPT) +unix.cmxa -o test_parser.opt test_parser.ml
clean:
	-rm -f test_parser{,.opt,.o} *.cm{i,o,x}
