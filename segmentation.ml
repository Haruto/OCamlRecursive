type recherche = BLANC | NOIR

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
	  test := false
      end;
    i := !i + 1;
  done;
  !test
 
let get_text_zone img w h =
  let recherche = ref BLANC in
  for i = 0 to (h-1) do
    if (!recherche = BLANC && not (is_line_white img w i)) then 
      begin
	color_line_red img w (i-1);
	recherche := NOIR
      end
    else if (!recherche = NOIR && (is_line_white img w (i))) then
      begin
	color_line_red img w i;
	recherche := BLANC
      end
  done 
