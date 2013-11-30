# OCamlRecursive
 
OCAML=ocamlbuild
OCAMLFLAGS= -use-ocamlfind
 
all:
	${OCAML} ${OCAMLFLAGS} OCamlRecursive.native
 
clean::
	rm -f *~ *.native
	rm -f out.bmp
	rm -r _build	
 
# FIN