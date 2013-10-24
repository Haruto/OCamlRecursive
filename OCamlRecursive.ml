(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, 
   (Sdlvideo.surface_info img).Sdlvideo.h)

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
    (* On crée la surface d'arriver pour la binarisation *)
   let bin = Sdlvideo.create_RGB_surface_format img [] w h in
    (* Application des differentes fonctions de traitement *)
    Binarisation.binarisation img bin w h;
    let alpha = Rotation.(hough_accu bin) in
    let (dst1,newW,newH) = Rotation.(rotate bin alpha) in
    let dst_final = Sdlvideo.set_video_mode newW newH [`DOUBLEBUF] in
    Tools.show dst1 dst_final;
    Tools.wait_key ();
    Segmentation.get_text_zone dst1 w h;
    (* on affiche l'image *)
    Tools.show dst1 dst_final;
    Tools.wait_key ();
    (* on quitte *)
    exit 0
  end
 
let _ = main ()
