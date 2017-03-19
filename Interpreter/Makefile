
OUTPUT_NAME = mysplinterpreter

all: main

main: lexer.cmo parser.cmo main.cmo parseTree.cmo
	ocamlc -o $(OUTPUT_NAME) str.cma lexer.cmo parser.cmo main.cmo parseTree.cmo

lexer.cmo: lexer.ml parser.ml parseTree.cmo
	ocamlc -c lexer.ml

parser.cmo: parser.ml parseTree.cmo
	ocamlc -c parser.ml

main.cmo: main.ml parseTree.cmo
	ocamlc -w -10 -c main.ml

parseTree.cmo: parseTree.ml
		ocamlc -c parseTree.ml

lexer.ml: parseTree.cmo
	ocamllex lexer.mll

parser.ml: parser.mli parseTree.cmo
	ocamlc -c parser.mli

parser.mli: parseTree.cmo
	ocamlyacc parser.mly

clean:
	rm -f lexer.cmo
	rm -f parser.cmo
	rm -f main.cmo
	rm -f parser.mli
	rm -f parser.ml
	rm -f lexer.ml
	rm -f *.cmi
	rm -f parseTree.cmo
