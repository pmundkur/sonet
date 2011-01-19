# This Makefile is used only for testing the installation.

OCAMLC = ocamlfind ocamlc -linkpkg -verbose -package sonet.dbuslib

.PHONY: all byte native clean

all: byte native

byte:
	$(OCAMLC) -o parse_test parse_test.ml
	$(OCAMLC) -o dbus_test dbus_test.ml

native:
	$(OCAMLC) -o parse_test.opt parse_test.ml
	$(OCAMLC) -o dbus_test.opt dbus_test.ml

clean:
	-rm -f parse_test{,.o,.opt} dbus_test{,.o,.opt} *.cm{i,o,x}
