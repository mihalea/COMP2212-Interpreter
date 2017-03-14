
OUTPUT_NAME = mysplinterpreter

all: main

main:
	ocamlc -o $(OUTPUT_NAME) str.cma main.ml
clean:
	rm -f main.cmo
	rm -f *.cmi
