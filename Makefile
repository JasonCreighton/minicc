MAKEFLAGS += --no-builtin-rules
.DELETE_ON_ERROR:

# Kind of hack, we create the "build" directory when the Makefile is parsed
# instead of trying to have it be a target, which is hard for "make" to
# handle nicely for some reason.
_MKDIRS := $(shell mkdir -p build)

SOURCES = ir.ml ast.ml amd64.ml parser.mli parser.ml lexer.ml tests.ml main.ml
BUILD_SOURCES = $(addprefix build/,$(SOURCES))

.PHONY: all debug clean test test_ocaml test_regression

all: test

# ocamlopt seems to need the source files in the same directory as the output
# files, so we copy them in as needed.
build/% : %
	cp $< $@

clean:
	rm -rf ./build

build/parser.ml build/parser.mli build/parser.output: build/parser.mly
	ocamlyacc -v --strict build/parser.mly

build/lexer.ml: build/lexer.mll
	ocamllex build/lexer.mll

build/minicc: $(BUILD_SOURCES)
	ocamlopt -S -o build/minicc -I build $(BUILD_SOURCES)

build/bytecode_minicc: $(BUILD_SOURCES)
	ocamlc -g -o build/bytecode_minicc -I build $(BUILD_SOURCES)

build/regression.asm: build/minicc regression.c
	./build/minicc -i regression.c -o build/regression.asm	

build/regression.o: build/regression.asm
	nasm -felf64 build/regression.asm

build/regression: build/regression.o
	gcc -lm build/regression.o -o build/regression

build/regression_golden: regression.c
	gcc -lm -Wall -fwrapv -fsanitize=undefined regression.c -o build/regression_golden

build/regression_actual_out.txt: build/regression
	./build/regression > ./build/regression_actual_out.txt

build/regression_expected_out.txt: build/regression_golden
	./build/regression_golden > ./build/regression_expected_out.txt

test_regression: build/regression_expected_out.txt build/regression_actual_out.txt
	diff -u build/regression_expected_out.txt build/regression_actual_out.txt

test_ocaml: build/minicc
	./build/minicc -runtests

test: test_ocaml test_regression

debug: build/bytecode_minicc
	rlwrap ocamldebug ./build/bytecode_minicc -i regression.c -o build/regression.asm