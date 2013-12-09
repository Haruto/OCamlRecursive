(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, 
   (Sdlvideo.surface_info img).Sdlvideo.h)

let window =
 GMain.init ();
 let wnd = GWindow.window
  ~title:"OCR by FTW team"
  ~position:`CENTER
  ~resizable:true
  ~width:1280 ~height:720 () in
 wnd#connect#destroy GMain.quit;
 wnd

let vbox = GPack.vbox ~packing:window#add ()

(* Zone du menu *)
let menubar = GPack.button_box `HORIZONTAL
 ~spacing:5
 ~border_width:5
 ~layout:`START
 ~packing:(vbox#pack ~expand:false) ()

(* Zone de traitement *)
let hbox= 
  GPack.hbox
    ~homogeneous:false
    ~spacing:5
    ~border_width:5
    ~packing:vbox#add ()

(* Insertion de barres de défilement. *)
let scroll = GBin.scrolled_window
  ~height:680
  ~hpolicy:`ALWAYS
  ~vpolicy:`ALWAYS
  ~packing:hbox#add ()

let img = ref "logo"

(* l'image affichée *)
let image = GMisc.image
  ~file: !img
  ~packing:scroll#add_with_viewport ()

(* met à jour l'image affichée *)
let update_img imgl =
  image#set_file imgl;
  img := imgl

(* met à jour l'image depuis le bouton open_button *)
let set_img btn () =
  Gaux.may image#set_file btn#filename;
  match btn#filename with
  | None -> ()
  | Some s -> img := s


(* bouton pour ouvrir une image *)
let open_button =
  let btn = GFile.chooser_button
    ~title:"Open file"
    ~action:`OPEN
    ~packing:(menubar#pack ~expand:false) () in
  let _ = btn#connect#selection_changed ~callback:(set_img btn) in
  btn 

(* Zone de texte avec des barres de défilement. *)
let text =
  let scroll = GBin.scrolled_window
    ~hpolicy:`ALWAYS
    ~vpolicy:`ALWAYS
    ~shadow_type:`ETCHED_IN
    ~packing:hbox#add () in
  let txt = GText.view ~packing:scroll#add () in
  (*GtkSpell.attach
    ~lang:"FR" txt;*)
  txt#misc#modify_font_by_name "Monospace 10";
  txt 

let fonctionbar = GPack.button_box `VERTICAL
 ~spacing:10
 ~border_width:60
 ~layout:`START
 ~packing:(hbox#pack ~expand:false) ()

let about_button =
  let dlg = GWindow.about_dialog
    ~authors:["FTW team"]
    ~version:"2.0 \n of OCamlRecursive"
    ~website:"http://ftwocr.wordpress.com/"
    ~website_label:"OCR by FTW team"
    ~position:`CENTER_ON_PARENT
    ~parent:window
    ~wm_name:"OCamlRecursive"
    ~resizable:false
    ~width:400 ~height:250
    ~destroy_with_parent:true () in
  let btn = GButton.button ~stock:`ABOUT ~packing:menubar#add () in
  GMisc.image ~stock:`ABOUT ~packing:btn#set_image ();
  btn#connect#clicked (fun () -> ignore (dlg#run ()); dlg#misc#hide ());
  btn

let action_button_fonction label = GButton.button ~label ~packing:fonctionbar#add ()

let rotation_button = action_button_fonction "Grey level"
let rotation_button = action_button_fonction "Black and white"
let rotation_button = action_button_fonction "Blur removal"
let rotation_button = action_button_fonction "Rotation"
let rotation_button = action_button_fonction "Segmentation"
let rotation_button = action_button_fonction "Do all process"

let confirm _ = 
  let dlg = GWindow.message_dialog
    ~message:"<b><big>Voulez-vous vraiment quitter ?</big>\n\n\
      Attention :\nSi vou n'avez pas fini le traitement,vous perdrez toutes les modifications que  vous y avez apportées </b>\n"
    ~parent:window
    ~destroy_with_parent:true
    ~use_markup:true
    ~message_type:`QUESTION
    ~position:`CENTER_ON_PARENT
    ~buttons:GWindow.Buttons.yes_no () in
  let res = dlg#run () = `NO in
  dlg#destroy ();
  res



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
    let img = Sdlloader.load_image Sys.argv.(1) in
      (* On récupère les dimensions *)
    let (w,h) = get_dims img in
      let (dst,display) = img_pretreatment img w h in
      Sdlvideo.save_BMP dst "output.bmp";
      (* on quitte *)
      exit 0
  end
 
let _ = 
  if (Array.length (Sys.argv) < 2) then
    begin
      window#event#connect#delete confirm;
      window#show ();
      GMain.main ()
    end
  else    
      match (Sys.argv.(1)) with
  | "-a" -> 
    begin
      let img = Sdlloader.load_image (Sys.argv.(2)) in
      let (w,h) = get_dims img in
      let (dst,display) = img_pretreatment img w h in
      Sdlvideo.save_BMP dst "output.bmp";
    end
  | "-h" -> print_string "USE : ./OCamlRecursive [-a img]"
	
