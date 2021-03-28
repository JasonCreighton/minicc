SOURCES = parser.mli parser.ml lexer.ml calc.ml

all: calc

clean:
	rm -f calc parser.ml lexer.ml *.cmi *.cmx *.o

parser.ml: parser.mly
	ocamlyacc parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

calc: $(SOURCES)
	ocamlopt -o calc $(SOURCES)