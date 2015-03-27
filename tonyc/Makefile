lexer : lexer.ml
	ocamlopt -o lexer lexer.ml
lexer.ml : lexer.mll
	ocamllex lexer.mll

.PHONY : clean
clean : 
	rm lexer.cmi lexer.cmx lexer.o
