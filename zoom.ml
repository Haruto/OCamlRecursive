(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

(*Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, 
   (Sdlvideo.surface_info img).Sdlvideo.h)
  


(* Test du min *)
let minw img =
 let (w,h) = get_dims img in
 let minw = ref w in
 let minh = ref h in
 let maxw = ref 0 in
 let maxh = ref 0 in
 let setminw = ref false in
 let setminh = ref false in
 let setmaxh = ref false in
 let setmaxw = ref false in
 for j=0 to h-1 do
  setminw := false;
  setmaxh := false;
  setmaxw := false;
  setminh := false;
  for i=0 to w-1 do
   let (r,g,b) = Sdlvideo.get_pixel_color img i j in
   match (r,g,b) with
    |(0,0,0) when (!minw > i) & not(!setminw) -> minw := i-1;setminw := true
    |(0,0,0) when (!maxh < j) & not(!setmaxh) -> maxh := j+1;setmaxh := true
    |(0,0,0) when (!maxw < i) & not(!setmaxw) -> maxw := i+1;setmaxw := true
    |(0,0,0) when (!minh > j) & not(!setminh) -> minh := j-1;setminh := true
    |_ ->()
  done;
 done;
 let neww = (!maxw)-(!minw)+1 in
 let newh = (!maxh)-(!minh)+1 in
 let dstout = Sdlvideo.create_RGB_surface [ `SWSURFACE ] neww newh 32
 (Int32.of_int 255) (Int32.of_int 255) (Int32.of_int 255) (Int32.of_int 0) in
 let nw = ref 0 in
 let ny = ref 0 in
 for y = !minh to !maxh do
  nw:= 0;
  for x = !minw to !maxw do
   let (r,g,b) = Sdlvideo.get_pixel_color img x y in
   Sdlvideo.put_pixel_color dstout !nw !ny (r,g,b);
   nw := !nw+1;
  done;
 ny:= !ny+1 
 done;
neww,newh,dstout;;    


(*
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



(* main *)
let main () =
  begin
    (* Nous voulons 2 arguments *)
    if Array.length (Sys.argv) < 1 then
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
      let (nw,ny,dstout) = minw img in
      let display2 = Sdlvideo.set_video_mode nw ny [`DOUBLEBUF] in
      draw dstout display2;
      wait_key();
      exit 0
  end
 
let _ = main ()

  *)
