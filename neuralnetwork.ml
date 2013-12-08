class neuron nbentries =
object (self)
  val mutable weights:float array = 
    Array.make (nbentries-1) (Random.float 0.5)
  val mutable output:float = 0.
  val threshold:float = 0.5
  val mutable delta = 0.

  method sigmoidale x = 1. /. 1. +. (exp (-.x))

  method get_output = output
  method get_weights pos = weights.(pos)
  method get_delta = delta
    
  method set_weights pos weight = weights.(pos) <- weight
  method set_delta d = delta <- d
  method set_output tab_entries = 
      begin
	for i = 0 to nbentries-1 do
	  output <- output +. weights.(i) *. tab_entries.(i)
	done;
	output <- output -. threshold;
	output <- self#sigmoidale output 
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

  method set_neuron pos n = neuron.(pos) <- n

  method set_output_tab tab_entries =
    for i = 0 to (Array.length tab_entries)-1 do
      neuron.(i)#set_output tab_entries;
      output_neuron.(i) <- neuron.(i)#get_output
    done
end

class network nbentries nbneurons_tab tab_input =
object (self)
  val mutable layers =
    let nbinput = ref ((Array.length tab_input) - 1) in
    Array.map
      ( 
	fun nbneurons ->
	  let lay = new layer nbneurons !nbinput in
	  nbinput := nbneurons;
	  lay
      )
      nbneurons_tab
  val threshold = 0.5
  val mutable delta = 0.
  val mutable error = 1.

  method get_layers = layers
  method get_delta = delta
  method get_error = error

  method set_delta d = delta <- d
  method set_error e = error <- e
  method set_layer pos l = layers.(pos) <- l
  method layer_update tab_entries = 
    layers.(0)#set_output_tab tab_entries;
    for i = 1 to (Array.length layers)-1 do
	layers.(i)#set_output_tab layers.(i-1)#get_output_neuron
    done
  method learn tab_examples learn_rate =
    let total_error = ref 1. in
    while (!total_error > 0.01) do
      let nb_examples = (Array.length tab_examples) in
      let nb_layers = (Array.length layers)-1 in
      for i = 0 to nb_examples-1 do
	let (x,c) = tab_examples.(i) in
	self#layer_update x;
	
	for j = nb_layers downto 0 do
	  let nb_neuron = (Array.length layers.(j)#get_neuron) -1 in
	  for k = 0 to nb_neuron do
	    let output = layers.(j)#get_neuron.(k)#get_output in
	    if j = nb_layers then	      
	      begin
		delta <- output *. (1. -. output) *. (c -. output);
		layers.(j)#get_neuron.(k)#set_delta delta
	      end
	    else
	      begin
		let nb_neuron_succ = (Array.length layers.(j+1)#get_neuron)-1 in
		let delta_somme = ref 0. in
		for l = 0 to nb_neuron_succ do
		  delta_somme := !delta_somme +. 
		    layers.(j+1)#get_neuron.(l)#get_delta *.
		    layers.(j+1)#get_neuron.(l)#get_weights k
		done;
		delta <- output *. (1. -. output) *. !delta_somme;
		layers.(j)#get_neuron.(k)#set_delta delta
	      end;
	  done;
	  for l = 0 to nb_examples-1 do
	    if j>0 then
	      begin
		let diff = c -. layers.(nb_layers-1)#get_neuron.(j)#get_output in
		self#set_error (0.5 *. (error +. diff *. diff))
	      end
	  done;
	done;
	let nb_neuron_prev = ref 0 in
	for m = 0 to nb_layers do
	  let nb_neuron = (Array.length layers.(m)#get_neuron) -1 in
	  let nb_entries = (Array.length x)-1 in
	    for j = 0 to nb_neuron do
	      let neuron = layers.(m)#get_neuron.(j) in
	      if (m = 0) then
		begin
		  for k = 0 to nb_entries do
		    neuron#set_weights k 
		      (
			(neuron#get_weights k) +. 
			  learn_rate *. 
			  neuron#get_delta *. 
			  x.(k)
		      )
		  done;
 		end
	      else
		begin
		  for k = 0 to (!nb_neuron_prev -1) do
		    neuron#set_weights k
		      (
			(neuron#get_weights k) +. 
			  learn_rate *. 
			  neuron#get_delta *. 
			  layers.(m-1)#get_output_neuron.(k)
		      )
		  done;
		end;
	      nb_neuron_prev := (Array.length layers.(m)#get_neuron)
	    done
	done;
	  total_error := !total_error +. error;
      done;
      total_error := !total_error /. (float_of_int nb_examples); 
    done;
end
  
		  
