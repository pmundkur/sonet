OCFLAGS = -I ../crypto

all:
	for f in `ls *.mli`; do ocamlc $(OCFLAGS) -c $$f; done
	for f in `ls *.ml`; do ocamlc $(OCFLAGS) -c $$f; done
