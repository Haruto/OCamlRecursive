(* Transformation en niveau gris *)
let img2grey img dst w h =
  let int_grey (r,g,b) = int_of_float 
    (0.299 *. float_of_int r +. 
       0.587 *. float_of_int g +.
       0.114 *. float_of_int b) in
  let triple_grey (r,g,b) = (int_grey (r,g,b), 
			     int_grey (r,g,b), 
			     int_grey (r,g,b)) in
  for y0 = 0 to h-1 do
    for x0 = 0 to w-1 do
      Sdlvideo.put_pixel_color dst x0 y0 
	(triple_grey (Sdlvideo.get_pixel_color img x0 y0))
    done
  done

(* Binarisation de l'image (noir et blanc) *)
let img2bin img dst w h = 
let seuil = ref 0. and
      level (r,g,b) = int_of_float 
    (0.299 *. float_of_int r +. 0.587 *. float_of_int g +. 0.114 *. float_of_int b) in
  for y0 = 0 to h-1 do
    for x0 = 0 to w-1 do
       let average = ref 0. and
	   sqrt_average = ref 0. and
	   variance = ref 0. and
	   sigma = ref 0. in
       for i = -1 to 1 do
	 for j = -1 to 1 do
	   if not ((x0+j) < 0 || (x0+j) > (w-1) || 
		       (y0+i) < 0 || (y0+i) > (h-1)) then
	     begin
	       let current_pix = level (Sdlvideo.get_pixel_color img (x0+j) (y0+i)) in
	       average := !average +. (float_of_int current_pix);
	       sqrt_average := !sqrt_average +. (float (current_pix * current_pix));
	     end	   
	 done
       done;
       average := !average /. 9.;
       sqrt_average := !sqrt_average /. 9.;
       variance := !sqrt_average -. !average *. !average;
       sigma := sqrt !variance; 
       seuil := (!average *. (1. +. 0.1 *. ((!sigma /. 140.) -. 1.)));
	 if (level (Sdlvideo.get_pixel_color img x0 y0) < (int_of_float !seuil)) then
	   Sdlvideo.put_pixel_color dst x0 y0 (0,0,0)
	 else
	   Sdlvideo.put_pixel_color dst x0 y0 (255,255,255)
    done
  done

(* Fonction d'application de matrice de convolution *)     
 let apply_matrix_convolution img dst mtx coeff w h =
   let r (r,g,b) = r and
       g (r,g,b) = g and
       b (r,g,b) = b and 
       newR = ref 0 and
       newG = ref 0 and
       newB = ref 0 in
   for x0 = 1 to (w-1) do
     for y0 = 1 to (h-1) do
       for i = (-1) to 1 do
	 for j = (-1) to 1 do
	   if not ((x0+i) < 0 || (x0+i) > (w-1) || 
		      (y0+j) < 0 || (y0+j) > (h-1)) then
	     begin
	       newR := !newR +
		 (r (Sdlvideo.get_pixel_color img (x0+i) (y0+j)) *
		    mtx.(i+1).(j+1));
	       newG := !newG +
		 (g (Sdlvideo.get_pixel_color img (x0+i) (y0+j)) *
		    mtx.(i+1).(j+1));
	       newB := !newB +
		 (b (Sdlvideo.get_pixel_color img (x0+i) (y0+j)) *
		    mtx.(i+1).(j+1));
	     end
	 done
       done;
       newR := !newR / coeff;
       newG := !newG / coeff;
       newB := !newB / coeff;
       Sdlvideo.put_pixel_color dst x0 y0 (!newR, !newG, !newB);
       newR := 0;
       newG := 0;
       newB := 0;
     done
   done
     
(* Filtre de Gauss *)
 let gauss_filter img dst w h =
   let gauss_matrix = Array.make_matrix 3 3 1 in
   gauss_matrix.(0).(1) <- 2;
   gauss_matrix.(1).(0) <- 2;
   gauss_matrix.(1).(1) <- 4;
   gauss_matrix.(2).(1) <- 2;
   gauss_matrix.(1).(2) <- 2;
   apply_matrix_convolution img dst gauss_matrix 16 w h


(* Binarisation finale *)
 let binarisation img dst w h =
   let s1 = Sdlvideo.create_RGB_surface_format img [] w h and
       s2 = Sdlvideo.create_RGB_surface_format img [] w h and
       display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
   Tools.show img display;
   Tools.wait_key ();
   img2grey img s1 w h;
   Tools.show s1 display;
   Tools.wait_key ();
   gauss_filter s1 s2 w h;
   Tools.show s2 display;
   Tools.wait_key ();
   img2bin s2 dst w h
