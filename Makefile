# OCamlRecursive
 
OCAML=ocamlbuild
OCAMLFLAGS= -use-ocamlfind
 
all:
	${OCAML} ${OCAMLFLAGS} OCamlRecursive.native
 
clean::
	rm -f *~ *.native
	rm -r _build	
 
# FIN
