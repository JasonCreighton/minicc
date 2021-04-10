SOURCES = ast.ml amd64.ml parser.mli parser.ml lexer.ml calc.ml

.PHONY: all test

all: calc

clean:
	rm -f calc parser.ml parser.mli lexer.ml *.cmi *.cmx *.o

parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

calc: $(SOURCES)
	ocamlopt -o calc $(SOURCES)

regression.asm: calc regression.c
	./calc < regression.c > regression.asm	

regression.o: regression.asm
	nasm -felf64 regression.asm

regression: regression.o
	gcc regression.o -o regression

regression_golden: regression.c
	gcc regression.c -o regression_golden

regression_actual_out.txt: regression
	./regression > regression_actual_out.txt

regression_expected_out.txt: regression_golden
	./regression_golden > regression_expected_out.txt

test: regression_expected_out.txt regression_actual_out.txt
	diff -u regression_expected_out.txt regression_actual_out.txt