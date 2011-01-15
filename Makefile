.PHONY: all clean

all:
	ocamlbuild all.otarget

clean:
	ocamlbuild -clean
