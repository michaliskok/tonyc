.PHONY: clean depend

TARGET=Main
FLAGS=-cflags -I,+camlp5 -use-menhir -yaccflags --explain
OCAMLBUILD=ocamlbuild

default:
	$(OCAMLBUILD) $(FLAGS) $(TARGET).native

native:
	$(OCAMLBUILD) $(FLAGS) $(TARGET).native

byte:
	$(OCAMLBUILD) $(FLAGS) $(TARGET).byte

all:
	$(OCAMLBUILD) $(FLAGS) $(TARGET).native $(TARGET.byte)

depend:
	@echo "Not needed."

clean:
	$(OCAMLBUILD) -clean
