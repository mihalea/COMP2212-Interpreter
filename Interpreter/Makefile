
OUTPUT_NAME = mysplinterpreter

all: main

main: main.cmo parseTree.cmo
	ocamlc -o $(OUTPUT_NAME) str.cma main.cmo parseTree.cmo

main.cmo: main.ml parseTree.cmo
	ocamlc -w -10 -c main.ml

parseTree.cmo: parseTree.ml
		ocamlc -c parseTree.ml

clean:
	rm -f main.cmo
	rm -f parseTree.cmo
	rm -f *.cmi
