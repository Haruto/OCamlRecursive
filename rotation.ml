(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, 
   (Sdlvideo.surface_info img).Sdlvideo.h)

(* absolue d'un float *)
let absf x = 
if ( x > 0.) then x
else x *. (-1.0)


(* convertion float/int rapide *)
let f2i x = int_of_float(x)
let i2f x = float_of_int(x)

(* detection de l'angle par la transformé de hough
 optimisé avec la fonction vote inclue dans la fonction accu*)
let hough_accu img =
 let (w,h) = get_dims img in
 let maxR = f2i(sqrt(i2f((w*w)+(h*h)))+.2.) in
 let d2r x = x *. 3.14 /. 180.0 in
 let m = Array.make_matrix 4100 maxR 0 in
 let maxA = ref 0. in
 let maxV = ref 0 in
 let k = ref 0. in
  for i=0 to h-1 do 
   for j=0 to w-1 do 
    if (Sdlvideo.get_pixel_color img i j = (0,0,0)) then
     begin
     while !k <= 40. do
      let rho = f2i(absf(i2f(j)*. cos(d2r(!k-.20.)) 
              +. i2f(i)*. sin(d2r(!k-.20.)))) in
       m.(f2i(!k*.100.)).(rho) <- m.(f2i(!k*.100.)).(rho) + 1;
      if !maxV < m.(f2i(!k*.100.)).(rho) then
       begin
        maxA := !k -. 20.;
        maxV := m.(f2i(!k*.100.)).(rho);
       end;
      k := !k+.0.01;
     done;
    end;
    k := 0.;
   done;
  done;
!maxA;;


(* zoom avec une rectification de l'algo de rotation cette fonction
 n'est plus utile pour l'instant *)
let zoom dst = 
 let xmax = ref 0 in
 let ymax = ref 0 in
 let (maxx,maxy) = ref false,ref false in
 let (w,h) = get_dims dst in
 for y = 0 to h-1 do
  for x = 0 to w-1 do
   let (r,g,b) = Sdlvideo.get_pixel_color dst x y in
   if (!maxx = false & x > !xmax & (r = 0 or b = 0 or g = 0)) then
    begin
    xmax := x-1;
    maxx := true
    end;
   if (!maxx = true & (r = 255 or b = 255 or g = 255)) then maxx := false;
   if (!maxy = false & x > !ymax & (r = 255 or b = 255 or g = 255)) then
   begin
   ymax := y-1;
   maxy := true
   end;
   if (!maxy = true & (r = 0 or b = 0 or g = 0)) then maxy := false;
  done;
 done;
!xmax,!ymax;;

(* rotation *)
let rotate img alpha =
 let (width,height) = get_dims img in
 let (w,h) = float_of_int(width), float_of_int(height) in 
 let arad = (alpha) *. 3.14 /. 180.0 in
 let cosalpha = cos(arad) in
 let sinalpha = sin(arad) in
 let auxW = ref 0 in
 let auxH = ref 0 in
 if alpha >= 0. then
  begin  
  auxW := int_of_float(ceil((w-.1.)*. cosalpha -. (h-.1.)*.sinalpha)*.1.3);
  auxH := int_of_float(ceil((w-.1.)*. cosalpha +. (h-.1.)*.sinalpha)*.1.3)
  end
 else
  begin
  auxW := int_of_float(ceil((w-.1.)*. cosalpha +. (h-.1.)*.sinalpha)*.1.3);
  auxH := int_of_float(ceil((h-.1.)*. cosalpha -. (w-.1.)*.sinalpha)*.1.3)
  end;
 let (cx,cy) = !auxW/2, !auxH/2 in
 let dstaux = Sdlvideo.create_RGB_surface [ `SWSURFACE ] !auxW !auxH 32 
 (Int32.of_int 255) (Int32.of_int 255) (Int32.of_int 255) (Int32.of_int 0) in
 let initX = ref 0. in
 let initY = ref 0. in  
 for y = 0 to !auxH-1 do
  for x = 0 to !auxW-1 do
   let auxX = float_of_int(x-cx) in
   let auxY = float_of_int(y-cy) in
   if alpha >= 0. then
    begin
     initX := ceil((cosalpha*.auxX)+.(sinalpha*.auxY)+.i2f(cx));
     initY := ceil((cosalpha*.auxY)-.(sinalpha*.auxX)+.i2f(cy));
    end
   else
    begin
     initX := ceil((cosalpha*.auxX)+.(sinalpha*.auxY)+.i2f(cx));
     initY := ceil((cosalpha*.auxY)-.(sinalpha*.auxX)+.i2f(cy));
    end;
   if !initX >= 0. & !initY >= 0. & !initX < w & !initY < h then
    let (r,g,b) = Sdlvideo.get_pixel_color img (f2i(!initX)) (f2i(!initY)) in
    Sdlvideo.put_pixel_color dstaux x y (r,g,b)
   else
    Sdlvideo.put_pixel_color dstaux x y (255,255,255)
  done
 done;
dstaux,!auxW,!auxH;;

(* attendre une touche ... *)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()

(*  show img dst affiche la surface img sur 
 la surface de destination dst (normalement l'écran) *)
let draw img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst
(*
(* main *)
let main () =
  begin
    (* Nous voulons 2 arguments *)
    if Array.length (Sys.argv) < 2 then
      failwith "le nom du fichier est invalide";
    (* Initialisation de SDL *)
    sdl_init ();
    (* Chargement d'une image *)
    let img = Sdlloader.load_image Sys.argv.(1) in
    (* On récupère tous les paramètres *)
    let (w,h) = get_dims img in
    (* On crée la surface d'affichage en doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
      (* on affiche l'image *)
      draw img display;
      (* on attend une touche *)
      wait_key ();
      (* nouvelle surface pour dst *)
      let alpha = ref (hough_accu img) in 
      print_float !alpha;
      let (dst,newW,newH) = rotate img (!alpha) in
      let display2 = Sdlvideo.set_video_mode newW newH [`DOUBLEBUF] in
      draw dst display2;
      (* on attend une touche *)
      wait_key ();
      (* on quitte *)
      Sdlvideo.save_BMP dst ("rotate_" ^ string_of_float(!alpha) ^ "_"
       ^ Sys.argv.(1) ^ ".bmp");
      exit 0
  end
 
let _ = main ()
*)
