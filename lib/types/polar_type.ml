(*
 * The upper boundary of a variable describes what can be passed in
 * as a value, whereas the lower boundary tells what the value
 * satisfies if it is retrieved.
 *)

 (* |n| Requires that t implements at least c (intersection) *)
 type 'a upper_bound = Top | Constrained of 'a
 (* |u| Requires that t implements only c (union) *)
 type 'a lower_bound = Bottom | Constrained of 'a
 
 type 'a t = {
   (* What can be passed in as a value (-) *)
   input_constraints: 'a upper_bound;
   (* What is satisfied if retrieved (+) *)
   output_constraints: 'a lower_bound;
 }
 
 
 let free_type = { input_constraints = Top; output_constraints = Bottom }
 
 (* Appends a constraint to a specific polarity of the provided type *)
 (* (+x, -x) -> +c -> t where -x <: s *)
 let input_constraint
   (c : 'a)
   (x : 'a list t) =
   match x.input_constraints with
   | Top -> { x with input_constraints = Constrained([c]) }
   | Constrained (s) -> { x with input_constraints = Constrained(c :: s) }
 
 let output_constraint
   (c : 'a)
   (x : 'a list t) =
   match x.output_constraints with
   | Bottom -> { x with output_constraints = Constrained([c]) }
   | Constrained (s) -> { x with output_constraints = Constrained(c :: s) }