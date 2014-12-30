.PHONY: all clean

all: scan.native

%.native: *.ml
	ocamlbuild -use-ocamlfind $@

clean:
	ocamlbuild -clean
