let img2matrix img w h =
  let matrix = Array.make_matrix w h (0) in
  for x = 0 to (w-1) do
    for y = 0 to (h-1) do
      let color = Sdlvideo.get_pixel_color img x y in
      if color = (0,0,0) then
	matrix.(x).(y) <- 0
      else
	matrix.(x).(y) <- 1
    done
  done
    
let matrix2img matrix w h =
  let dst = 
  Sdlvideo.create_RGB_surface [ `SWSURFACE ] w h 32 
 (Int32.of_int 255) (Int32.of_int 255) (Int32.of_int 255) (Int32.of_int 0) in
  for x = 0 to (w-1) do
    for y = 0 to (h-1) do
      let c1 = matrix.(x).(y) in
      let color = (c1,c1,c1) in
      Sdlvideo.put_pixel_color dst x y color
    done
  done
