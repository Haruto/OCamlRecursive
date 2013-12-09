class neuron nbentries =
object (self)
  val mutable weights:float array = 
    Array.make (nbentries+1) (Random.float 1.)
  val mutable output:float = 0.
  val mutable delta = 0.5

  method sigmoidale x = 1. /. (1. +. (exp (x *. -1.)))

  method get_output = output
  method get_weights pos = weights.(pos)
  method get_delta = delta
    
  method set_weights pos weight = weights.(pos) <- weight
  method set_delta d = delta <- d
  method set_output tab_entries = 
    let aux = ref 0. in
    begin
	for i = 0 to nbentries-1 do
	  if i = nbentries then
	    aux := !aux +. weights.(nbentries)
	  else
	    aux := !aux +. weights.(i) *. tab_entries.(i)
	done;
      output <- self#sigmoidale !aux
    end    
end
  
class layer nbneurons nbentries =
object (self)
  val mutable neuron:neuron array = 
    Array.make nbneurons (new neuron nbentries)
  val mutable output_neuron:float array =
    Array.make nbneurons 0.
    
  method get_neuron = neuron
  method get_output_neuron = output_neuron

  method set_output_tab tab_entries =
    for i = 0 to (nbneurons-1) do
      neuron.(i)#set_output tab_entries;
      output_neuron.(i) <- neuron.(i)#get_output
    done;
end

class network nbentries nbneurons_tab =
object (self)
  val mutable layers =
    let nbinput = ref nbentries in
    Array.map
      ( 
	fun nbneurons ->
	  let lay = new layer nbneurons !nbinput in
	  nbinput := nbneurons;
	  lay
      )
      nbneurons_tab
  val mutable delta = 0.5
  val mutable error = 1.

  method get_layers = layers
  method get_delta = delta
  method get_error = error

  method set_delta d = delta <- d
  method set_error e = error <- e

  method layer_update tab_entries = 
    layers.(0)#set_output_tab tab_entries;
    for i = 1 to Array.length nbneurons_tab -1  do
      layers.(i)#set_output_tab layers.(i-1)#get_output_neuron;
    done

   method learn tab_examples tab_out learn_rate =
    let total_error = ref 1. in
    (*while (!total_error > 0.1) do*)
    for t = 0 to 1000 do
      let nb_examples = (Array.length tab_examples) in
      for i = 0 to nb_examples-1 do
	self#layer_update tab_examples.(i);
	let nb_layers = (Array.length layers) in
	for j = nb_layers-1 downto 0 do
	  let nb_neuron = (Array.length layers.(j)#get_neuron) in
	  for k = 0 to nb_neuron-1 do
	    let output = layers.(j)#get_neuron.(k)#get_output in
	    if j = nb_layers-1 then	      
	      begin
		delta <- output *. (1. -. output) *. (tab_out.(i) -. output);
		layers.(j)#get_neuron.(k)#set_delta delta
	      end
	    else
	      begin
		let nb_neuron_succ = (Array.length layers.(j+1)#get_neuron)-1 in
		let delta_somme = ref 0. in
		for l = 0 to nb_neuron_succ do
		  delta_somme := !delta_somme +. 
		    layers.(j+1)#get_neuron.(l)#get_delta *.
		    (layers.(j+1)#get_neuron.(l)#get_weights k)
		done;
		delta <- output *. ((1. -. output) *. !delta_somme);
		layers.(j)#get_neuron.(k)#set_delta delta
	      end;
	  done;
	  for l = 0 to (Array.length layers.(nb_layers-1)#get_neuron)-1 do
	    (*if j>0 then*)
	      begin
		let diff = tab_out.(i) -. (layers.(nb_layers-1)#get_neuron.(l)#get_output) in
		self#set_error ((error +. diff *. diff ) /. 2.)
	      end;
	  done;
	done;
	let nb_entries = (Array.length tab_examples.(i)) in
	let nb_neuron_prev = ref 0 in
	for j = 0 to nb_layers-1 do
	  let nb_neuron = (Array.length layers.(j)#get_neuron) in	 
	    for k = 0 to nb_neuron-1 do
	      let neuron = layers.(j)#get_neuron.(k) in
	      if (j = 0) then
		begin
		  for l = 0 to nb_entries-1 do
		    neuron#set_weights l 
		      (
			(neuron#get_weights l) +. 
			  learn_rate *. 
			  neuron#get_delta *. 
			  tab_examples.(i).(l)
		      )
		  done;
		  nb_neuron_prev := (Array.length layers.(j)#get_neuron)
 		end
	      else
		for l = 0 to (!nb_neuron_prev -1) do
		  neuron#set_weights l
		    (
		      (neuron#get_weights l) +. 
			learn_rate *. 
			neuron#get_delta *. 
			layers.(j-1)#get_output_neuron.(l)
		    )
		done;
	      nb_neuron_prev := (Array.length layers.(j)#get_neuron);	
	    done;
	done;
	  total_error := !total_error +. self#get_error;
      done;
      total_error := !total_error /. (float_of_int nb_examples);   
      print_endline (string_of_float error);
    done;
end
  
		  
