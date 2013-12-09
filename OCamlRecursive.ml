(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, 
   (Sdlvideo.surface_info img).Sdlvideo.h)

let img_pretreatment img w h = 
  let bin = Sdlvideo.create_RGB_surface_format img [] w h in

  (* Application des differentes fonctions de traitement *)
  print_endline "Binarisation...";
  Binarisation.binarisation img bin w h;
  print_endline "done";

  print_endline "Rotation...";
  let alpha = Rotation.(hough_accu bin) in
  let (dst_rot,newW,newH) = Rotation.(rotate bin alpha) in
  let (nw,ny,final_dst) = Zoom.(minw dst_rot) in
  print_endline "done";

  print_endline "Segmentation and extraction...";
  (*Segmentation.get_text_zone dst1 newW newH;*)
  print_endline "done";

  let final_display = Sdlvideo.set_video_mode nw ny [`DOUBLEBUF] in
  Tools.show final_dst final_display;
  Tools.wait_key ();

  (final_dst, final_display)
    
let xor_test () =
  let xor = new Neuralnetwork.network 2 [|3;1|] in
  xor#learn
    [| 
      [|0.;0.|];
      [|0.;1.|];
      [|1.;0.|];
      [|1.;1.|]
    |]
    [|0.;1.;1.;0.|] 0.8;
     
      xor#layer_update [|0.;0.|];
      Printf.printf "\n XOR 0/0 -> %f\n\n" (((xor#get_layers).(1))#get_output_neuron).(0);

      xor#layer_update [|0.;1.|];
      Printf.printf "\n XOR 0/1 -> %f\n\n" (((xor#get_layers).(1))#get_output_neuron).(0);

      xor#layer_update [|1.;0.|];
      Printf.printf "\n XOR 1/0 -> %f\n\n" (((xor#get_layers).(1))#get_output_neuron).(0);

      xor#layer_update [|1.;1.|];
      Printf.printf "\n XOR 1/1 -> %f\n\n" (((xor#get_layers).(1))#get_output_neuron).(0)
    
(* main *)
let main () =
  begin
    (* Nous voulons 1 argument *)
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    (* Initialisation de SDL *)
    Tools.sdl_init ();
    (* Chargement d'une image *)
    if ((Sys.argv).(1) = "xor") then
      xor_test ()
    else
      let img = Sdlloader.load_image Sys.argv.(1) in
      (* On récupère les dimensions *)
      let (w,h) = get_dims img in
      let (dst,display) = img_pretreatment img w h in
      Sdlvideo.save_BMP dst "output.bmp";
      (* on quitte *)
      exit 0
  end
 
let _ = 
    main ()
