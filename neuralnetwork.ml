class neuron nbweights =
object (self)
  val mutable weights:float array = 
    Array.make nbweights (Random.float 2.)
  val mutable output:float = 0.
  val threshold:float = 0.5

  method sigmoidale x = 1. /. 1. +. (exp (-.x))
  method get_output = output
  method set_weights pos weight = weights.(pos) <- weight
  method set_output input = 
    if (nbweights <> (Array.length input)) then
      assert false
    else
      begin
	for i = 0 to nbweights-1 do
	  output <- output +. weights.(i) *. input.(i)
	done;
	output <- output -. threshold;
	output <- self#sigmoidale output 
      end    
end
  
class layer nbneurons nbweights =
object (self)
  val mutable neuron:neuron array = 
    Array.make nbneurons (new neuron nbweights)
  val mutable output_neuron:float array =
    Array.make nbneurons 0.
    
  method get_neuron = neuron
  method get_output_neuron = output_neuron

  method set_neuron_tab pos n = neuron.(pos) <- n

  method set_output_tab input =
    for i = 0 to (Array.length input)-1 do
      neuron.(i)#set_output input;
      output_neuron.(i) <- neuron.(i)#get_output
    done
end
