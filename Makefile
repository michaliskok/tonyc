.PHONY: clean depend

TARGET=tonyc
DRIVER=Main
FLAGS=-cflags -I,+camlp5,-g -use-menhir -yaccflags --explain -no-hygiene
OCAMLBUILD=ocamlbuild

default:
	$(OCAMLBUILD) $(FLAGS) $(DRIVER).native
	rm Main.native
	cp ./_build/Main.native ./$(TARGET)

native: default

byte:
	$(OCAMLBUILD) $(FLAGS) $(DRIVER).byte
	rm Main.byte
	cp ./_build/Main.byte ./$(TARGET)

depend:
	@echo "Not needed."

clean:
	$(OCAMLBUILD) -clean

distclean: clean
	rm ./$(TARGET)
