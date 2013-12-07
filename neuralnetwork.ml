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

  method set_neuron_tab pos n = neuron.(pos) <- n

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
  val mutable error = 0.

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
end
  
