open OUnit2
open Ice_cream

let test_input_constraint _ =
  let c = Types.Ruby.(Literal TInt) in
  let used_as_integer = Types.free_type |> Types.input_constraint c in
  let expected_constraint : Types.Ruby.upper_bound = Constrained([c]) in
  assert_equal expected_constraint used_as_integer.input_constraints;
  assert_equal Types.Bottom used_as_integer.output_constraints

let test_output_constraint _ =
  let c = Types.Ruby.(Literal TInt) in
  let set_as_integer = Types.free_type |> Types.output_constraint c in
  let expected_constraint : Types.Ruby.lower_bound = Constrained([c]) in
  assert_equal expected_constraint set_as_integer.output_constraints;
  assert_equal Types.Top set_as_integer.input_constraints

(*
  def foo(z)
    x = z
    y = x.incr(1)
    return x
  end
*)

(* let test_subtype _ =
  let open Ruby in
  let z = free_type
  and x = free_type in
  let x' = x
    |> input_constraint (SubType z)
    |> output_constraint (
        Method ("incr", [Constrained([Literal TInt])], Bottom)
      )
  in assert_equal "< .incr : (int -> bottom) >" (solve x) *)

let suite = "Biunification Tests" >::: [
  "input_constraint" >:: test_input_constraint;
  "output_constraint" >:: test_output_constraint;
  (* "subtype" >:: test_subtype; *)
]
