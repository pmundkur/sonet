# This Makefile is used only for testing the installation.

OCAMLC = ocamlfind ocamlc -linkpkg -verbose -package sonet.httplib
OCAMLOPT = ocamlfind ocamlopt -linkpkg -verbose -package sonet.httplib

.PHONY: all byte native clean

all: byte native

byte:
	$(OCAMLC) -o test_uri test_uri.ml
	$(OCAMLC) -o test_http test_http.ml
	$(OCAMLC) -o test_http_client test_http_client.ml

native:
	$(OCAMLOPT) -o test_uri.opt test_uri.ml
	$(OCAMLOPT) -o test_http.opt test_http.ml
	$(OCAMLOPT) -o test_http_client.opt test_http_client.ml

clean:
	-rm -f test_uri{,.o,.opt} test_http{,.o,.opt} test_http_client{,.o,.opt} *.cm{i,o,x}
