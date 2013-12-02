class neuron entries nbweights =
object (self)
  val mutable weights:float array = 
    Array.make nbweights (Random.float 2.)
  val mutable output:float = 0.
  val threshold:float = 0.5

  method sigmoidale x = 1. /. 1. +. (exp (-.x))
  method get_output = output
  method set_weights pos nw = weights.(pos) <- nw
  method set_output = 
    let out = ref 0. in
    for i = 0 to nbweights-1 do
      out := !out +. weights.(i) *. entries.(i)
    done;
    output <- self#sigmoidale !out     
end
