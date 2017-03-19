
OUTPUT_NAME = mysplinterpreter

all: main

main: lexer.cmo parser.cmo main.cmo
	ocamlc -o $(OUTPUT_NAME) str.cma lexer.cmo parser.cmo main.cmo

lexer.cmo: lexer.ml parser.ml
	ocamlc -c lexer.ml

parser.cmo: parser.ml
	ocamlc -c parser.ml

main.cmo: main.ml
	ocamlc -w -10 -c main.ml

lexer.ml:
	ocamllex lexer.mll

parser.ml: parser.mli
	ocamlc -c parser.mli

parser.mli:
	ocamlyacc parser.mly

clean:
	rm -f lexer.cmo
	rm -f parser.cmo
	rm -f main.cmo
	rm -f parser.mli
	rm -f parser.ml
	rm -f lexer.ml
	rm -f *.cmi
