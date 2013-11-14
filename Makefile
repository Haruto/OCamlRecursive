# Binarisation
 
OCAML=ocamlopt
OCAMLFLAGS= -I +sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
 
binarisation: binarisation.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o binarisation binarisation.ml
 
clean::
	rm -f *~ *.o *.cm? binarisation *.native
	rm -f out.bmp
	rm -r _build	
 
# FIN