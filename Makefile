SOURCES = ast.ml parser.mli parser.ml lexer.ml calc.ml

all: calc

clean:
	rm -f calc parser.ml parser.mli lexer.ml *.cmi *.cmx *.o

parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

calc: $(SOURCES)
	ocamlopt -o calc $(SOURCES)