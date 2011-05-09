# This Makefile is used only for testing the installation.

OCAMLC = ocamlfind ocamlc -linkpkg -verbose -package sonet.bencodelib
OCAMLOPT = ocamlfind ocamlopt -linkpkg -verbose -package sonet.bencodelib

.PHONY: all byte native clean

all: byte native

byte:
	$(OCAMLC) -o test_bencode test_bencode.ml

native:
	$(OCAMLOPT) -o test_bencode.opt test_bencode.ml

clean:
	-rm -f test_bencode{,.o,.opt} *.cm{i,o,x}
