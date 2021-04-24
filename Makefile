SOURCES = ast.ml amd64.ml parser.mli parser.ml lexer.ml tests.ml main.ml

.PHONY: all test test_ocaml test_regression

all: minicc

clean:
	rm -f minicc parser.ml parser.mli lexer.ml regression.asm regression regression_golden regression_actual_out.txt regression_expected_out.txt *.cmi *.cmx *.o

parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

minicc: $(SOURCES)
	ocamlopt -w +a -o minicc $(SOURCES)

regression.asm: minicc regression.c
	./minicc < regression.c > regression.asm	

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

test_regression: regression_expected_out.txt regression_actual_out.txt
	diff -u regression_expected_out.txt regression_actual_out.txt

test_ocaml: minicc
	./minicc -runtests

test: test_ocaml test_regression