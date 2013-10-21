(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

 (* main *)
let main () =
  begin
    (* Nous voulons 1 argument *)
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    (* Initialisation de SDL *)
    Tools.sdl_init ();
    (* Chargement d'une image *)
    let img = Sdlloader.load_image Sys.argv.(1) in
    (* On récupère les dimensions *)
    let (w,h) = get_dims img in
    (* On crée la surface d'affichage en doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] and 
	dst = Sdlvideo.create_RGB_surface_format img [] w h in
    (* Application des differentes fonctions de traitement *)
    Binarisation.binarisation img dst w h;
    Segmentation.get_text_zone dst w h;
    (* on affiche l'image *)
    Tools.show dst display;
    (* on attend une touche *)
    Tools.wait_key ();
    (* Enregistrement de l'image en .BMP *)
    Sdlvideo.save_BMP dst "output.bmp";
    (* on quitte *)
    exit 0
  end
 
let _ = main ()
