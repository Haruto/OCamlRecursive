# OCamlRecursive
 
OCAML=ocamlbuild
OCAMLFLAGS= -use-ocamlfind
LIB := sdlloader
FLAGS := -use-ocamlfind -libs $(LIB)
all:
	${OCAML} ${OCAMLFLAGS} ${FLAGS} OCamlRecursive.native
 
clean::
	rm -f *~ *.native
	rm -r _build	
 
# FIN
