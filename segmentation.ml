(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, 
   (Sdlvideo.surface_info img).Sdlvideo.h)

let img2matrix img h w =
  let matrix = Array.make_matrix w h (0) in
  for x = 0 to (w-1) do
    for y = 0 to (h-1) do
      let color = Sdlvideo.get_pixel_color img x y in
      if color = (0,0,0) then
	matrix.(x).(y) <- 0
      else
	matrix.(x).(y) <- 1
    done;
  done;
  matrix

let matrix2img matrix h w =
  let dst = 
  Sdlvideo.create_RGB_surface [ `SWSURFACE ] w h 32 
 (Int32.of_int 255) (Int32.of_int 255) (Int32.of_int 255) (Int32.of_int 0) in
  for x = 0 to (w-1) do
    for y = 0 to (h-1) do
      let c1 = matrix.(x).(y) in
      match c1 with
	0 -> Sdlvideo.put_pixel_color dst x y (0,0,0);
      | 1 -> Sdlvideo.put_pixel_color dst x y (255,255,255);
      | _ -> Sdlvideo.put_pixel_color dst x y (200,0,0);
    done;
  done;
  dst

(* Fonction pour ajouter un element dans une liste pointee*)
let add_list elt l = l := elt::!l;;



let rec print_reflist  li  = match (li) with
    |[]  -> print_string "stop"
    |e::l -> print_int e; print_string ("\n"); print_reflist l;; 




(* On remplie la liste de taille la hauteur de l'image et contenant le *)
(* nombre de pixel noirs par ligne  *) 
let fill_black_lines matr w h =
			   let r = ref 0 in 
			   let lines_with_black = ref  [] in
     for i=h-1 downto 1 do
       for j=1 to w-1 do
         if (matr.(i).(j) = 0) then
            r := !r + 1 ;
       done ;
        add_list !r lines_with_black;
        r := 0
     done;
	!lines_with_black;;


(* On remplie la liste de taille la largeur de l'image et contenant le *)
(* nombre de pixel noirs par colonne  *) 
let fill_black_columns matr w h =
			   let r = ref 0 in
			   let columns_with_black = ref  [] in
     for j=w-1  downto 1 do
       for i=1 to h-1 do
         if ( (matr.(i).(j) = 0)) then
            r := !r + 1;
       done ;
        add_list !r columns_with_black; 
        r := 0
     done;
	!columns_with_black;;






		
		
		
(* Position de la premiere colonne et la derniere colonne qui ont au moins *)
(* un pixel noir pour pouvoir encadrer le texte *)
let first_columns_pos matr w h    = let r = ref 1 in 
			    let columns = fill_black_columns matr w h in 
              while List.nth (columns)  !r < 5 do
              r:= !r + 1;
	      done ; 
              !r;;



let last_columns_pos matr w h =
                           let r = ref (w-2)  in
                           let columns = fill_black_columns matr w h in 
              while List.nth (columns)  !r < 5 do
              r:= !r - 1;
              done ; 
              !r;;





(* Position de la premiere ligne et la derniere ligne qui ont au moins *)
(* un pixel noir pour pouvoir encadrer le texte *)
let first_line_pos matr w h = let r = ref 1 in 
                         let lines = fill_black_lines matr w h in 
              while List.nth (lines)  !r < 3 do
              r:= !r + 1; 
              done ; 
              !r;;



let last_line_pos matr w h =
                        let r = ref (h-2) in 
                        let lines = fill_black_lines matr w h in
              while List.nth (lines)  !r < 3 do
              r:= !r - 1;   
              done ; 
              !r;;



(* On encadre la zone de texte *)
let draw_where_text_is matr w h = let coin =  first_columns_pos matr w h and
                                 cout =  last_columns_pos  matr w h and
                                 liin = first_line_pos matr w h and
                                 liout = last_line_pos matr w h in 
    for i= liin  to liout  do  
     for j= coin-1   to coin-1   do
      matr.(i).(j) <- 0
     done ;
       for j= cout+1   to cout+1  do
	       matr.(i).(j) <- 0
       done
    done ;
   for j = coin to cout do
     for i=liin-1  to liin-1  do
             matr.(i).(j) <- 0
     done ;
     for i=liout+1  to liout+1    do
            matr.(i).(j) <- 0
     done ;
   done ;; 

   
   
   
   
   
   
   
(* On Mesure la taille du rectangle pour estimer la taille des lignes/colonnes *)
   let rectangle_height matr w h = last_line_pos matr w h  - first_line_pos  matr w h ;;

    let  rectangle_width matr w h  = last_columns_pos matr w h  - first_columns_pos matr w h ;;

	

	(* Detection des lignes *)
	
	
(* Le ouexclusif *)
let ouexc (a ,b) = if a then not b else b;;

(* On parcours l'image et dès que  *)
    let fill_blocks_list  matr w h = let blocks_list = ref [] in
                               let r = ref 1 in
			       let fbl = fill_black_lines  matr w h in
       for i = first_line_pos  matr w h to rectangle_height matr w h  do
           if ouexc (List.nth (fbl) i <= 3 ,List.nth (fbl) (i-1) > 3)  then 
        add_list (i,r) blocks_list ; r:= !r + 1 
       done ;
     !blocks_list

(* Fonction qui remplie la ligne x de l'image pour la mettre en evidence *)
    let draw_line  matr w h x = 
      for i =  first_line_pos   matr w h to  last_line_pos matr w h  do
         matr.(i).(x) <- 0
      done;;   

  

  let  it = ref 0;;
  let lines = ref [];;

(* Fonction qui dessine et detecte les lignes *)
(* A chaque fois que l'on change de zone *)
(* ( on passe d'une ligne vide a une non vide) *)
(* on dessine une ligne *)
    let fill_lines  matr w h list = let fbl = fill_black_columns matr w h in 
     if (!it = 0) then
       add_list (first_columns_pos  matr w h) list;
       begin
      for i=  first_columns_pos  matr w h to ((last_columns_pos   matr w h) - 1) do
	if (List.nth  (fbl) i) <=3 && 
        ((List.nth  (fbl) (i-1)) > 3 || 
        (List.nth  (fbl) (i+1)) >3  ) then 
          begin 
              draw_line  matr w h i ; 
              add_list i list;	 	
          end; 
      done;  it:= 0
       end; 
       add_list (last_columns_pos  matr w h) list;;


 
         




let list_blank_columns_line matr w h line  =  let fl = !lines  in 
			   let r = ref 0 in 
			   let lines_with_black = ref  [] in
     for i= (List.nth fl line) to  (List.nth fl (line+1)) do
       for j=1 to w-1 do
         if (matr.(i).(j) = 0) then
            r := !r + 1 ;
       done ;
        add_list !r lines_with_black;
        r := 0
     done;
	!lines_with_black;;























(* Les caracteres*)

 (*   let draw_col  matr w h x lines = let fl = (fill_lines matr w h ) in
      for i = List.nth (fl) x downto (List.nth (fl) (x+1)) do
       matr.(lines).(i) <- 0;
      done;;*)




    let is_col_blank matr w h  x line =  let fl = !lines  in 
					  let entier = ref (( List.nth (fl) x) - 1) in 
					  let inttest = 1 in
					  let bool = ref 0 in  
     while !entier > (List.nth (fl) (x+1)) do
         if (matr.(line).(!entier) = 0 ) then
	   begin
	     if (!bool< inttest) then
	       begin
	       bool:= !bool +1;
               entier := !entier - 1;
	       end
	     else
	     entier := 0;
	   end
	     else
	   entier := !entier - 1; 
	 
      done;

    if (!bool < inttest ) then
      begin
     for i = (List.nth (fl) x) -1 downto (List.nth (fl) (x+1)) +1 do
       matr.(line).(i) <- 2;
     done
      end;;
     




    let draw_col_onlines  matr w h x  = 
      for i = first_line_pos matr w h  to last_line_pos matr w h  do
	(is_col_blank  matr w h  x i)
    done;;
      


    let listcarac = [];;


    let draw_carac matr w h =
      draw_where_text_is matr w h; 
      let fl = fill_lines matr w h lines in
			      for line = 0 to (List.length !lines) -2 do
			      for i = ((first_line_pos matr w h)) to (last_line_pos matr w h) do
			        is_col_blank matr w h line i
			      done;
			      done;;
