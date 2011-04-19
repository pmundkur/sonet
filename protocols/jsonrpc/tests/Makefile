# This Makefile is used only for testing the installation.

OCAMLC = ocamlfind ocamlc -linkpkg -verbose -package sonet.jsonlib
OCAMLOPT = ocamlfind ocamlopt -linkpkg -verbose -package sonet.jsonlib

all:
	$(OCAMLC) +unix.cma -o test_parser test_parser.ml
	$(OCAMLOPT) +unix.cmxa -o test_parser.opt test_parser.ml
clean:
	-rm -f test_parser{,.opt,.o} *.cm{i,o,x} ocaml_{y,s}ojson{,.o} {y,s}ojson

# Inspired by: https://github.com/cheecheeo/JSON-shootout

latest.json:
	wget "http://api.leakfeed.com/v1/cables/latest.json"

bench: latest.json
	ocamlfind ocamlopt -o yojson ocaml_yojson.ml -package yojson,unix -linkpkg
	ocamlfind ocamlopt -o sojson ocaml_sojson.ml -package sonet.jsonlib,unix -linkpkg
	./run_bench.sh
