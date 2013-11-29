type recherche = BLANC | NOIR

type paragraphe = 
{
  mutable xmin :  int;
  mutable xmax :  int;
  mutable ymin :  int;
  mutable ymax :  int;
}

let list = ref []

(*let print_paragraphes =
  let a = ref !list in 
  while !a <> [] do
    match !a with 
    |e::l -> 
      let xmin = e.xmin and
let xmax =  e.xmax and
let ymin =  e.ymin and
let ymax =  e.ymax in
printf (" %d , %d, %d, %d " xmin xmax ymin ymax )*)
  
let init_para = 
  {xmin = 0; xmax = 0; ymin = 0; ymax = 0}
let set_xmin p x =
  p.xmin <- x

let set_xmax p x =
  p.xmax <- x

let set_ymin p y =
  p.ymin <- y

let set_ymax p y =
  p.ymax <- y


let get_xmax img i (* numero de la ligne*) nb_col =
  let k = ref w in 
  while ((Sdlvideo.get_pixel_color img i !k) = (255,255,255) ) do 
    k := k + 1
  done;
  !k

let get xmin img i = 
  let k = ref 0 in
  while ((Sdlvideo.get_pixel_color img i !k) = (255,255,255)) do 
    k:= k + 1
  done;
  !k

let color_line_red img w h =
  for k = 0 to w do
    Sdlvideo.put_pixel_color img k (h-1) (200,0,0)
  done

let is_line_white img w h_test  = (* retourne true si la ligne est white, false sinon*)
  let i = ref 0 and
      test = ref true and
      nb_pix_black = ref 0 in 
  while (!i < (w) && !test ) do
    if ((Sdlvideo.get_pixel_color img !i h_test) <> (255,255,255)) then 
      begin
	if (!nb_pix_black < 5) then (* Si y'a moins de 5 pixels blancs, 
				       on considere comme une ligne blanche *)
	  nb_pix_black := !nb_pix_black + 1
	else
	  begin
	    test := false;
      end;
    i := !i + 1;
  done;
  !test
 
let get_text_zone img w h =
let u = ref 0 and
let v = ref 0 and 
let recherche = ref BLANC in
for i = 0 to (h-1) do
  if (!recherche = blanc) do 
    let para = init_para in
    if (!recherche = BLANC && not (is_line_white img w i)) then 
      begin
	set_ymin para i;
	u := get_xmin;
	v := get xmax;
	color_line_red img w (i-1);
	recherche := NOIR
      end
    else if (!recherche = NOIR && (is_line_white img w (i))) then
      begin
	set_ymax para i;
	if (!u < get_xmin img i) then
	  u := get_xmin img i;
	if (!v < get_xmax img i w) then
	  v := get_xmax img i w;
	set_xmin para u;
	set_xmax para u;
	list := para::list;
	color_line_red img w i;
	recherche := BLANC
      end
  done 
      
