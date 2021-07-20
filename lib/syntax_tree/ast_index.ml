module NodeIntf = struct
  module type S = sig
    type elt
    type child_elt

    val name : elt -> string
    val node_type : elt -> string
    (* val location : elt -> Location.t *)
    val children : elt -> child_elt list option
    val pretty_print : elt -> string
    val attributes : elt -> (string * string) list
  end

  type t =
    | Node : 'a * (module S with type elt = 'a and type child_elt = t) -> t

  module type T = sig
    type node := t
    type t

    val name : t -> string
    val node_type : t -> string
    (* val location : t -> Location.t *)
    val children : t -> node list option
    val pretty_print : t -> string
    val attributes : t -> (string * string) list
  end

  module Make (X : T) : S with type child_elt = t and type elt = X.t =
  struct
    type elt = X.t
    type child_elt = t

    include X
  end

  (* Helper methods *)

  let node_name : t -> string =
    fun (Node (x, (module M))) -> M.name x

  let pretty_print : t -> string =
    fun (Node (x, (module M))) -> M.pretty_print x

  let node_type : t -> string =
    fun (Node (x, (module M))) -> M.node_type x

  let node_children : t -> t list option =
    fun (Node (x, (module M))) -> M.children x
end

module Query = struct
  let lazy_query ~f all =
    List.to_seq all |> Seq.filter f

  let query_all ~f all =
    List.of_seq @@ lazy_query ~f:f all

  let query ~f all =
    let result = lazy_query ~f:f all in
    match result () with
    | Cons (x, _) -> Some x
    | Nil -> None
end

module AssignmentNode = struct
  type u = {
    target : Location.t Ast.id;
    (* location : Location.t; *)
    children : NodeIntf.t list
  }

  module I = NodeIntf.Make(struct
    type t = u
    let name t = let (name, _) = t.target in name
    let node_type _t = "AssignmentNode"
    (* let location t = t.location *)
    let children t = Some (t.children)
    let pretty_print _t = "<AssignmentNode>"
    let attributes _t = []
  end)

  let make target children = NodeIntf.Node ({ target; children }, (module I))
end

module RefNode = struct
  type u = {
    name : string;
    (* location : Location.t; *)
    (* scope : string list; *)
  }

  module I = NodeIntf.Make(struct
    type t = u
    let make name = { name }
    let name t = t.name
    let node_type _ = "RefNode"
    let children _ = None
    let pretty_print _ = "<RefNode>"
    let attributes _ = []
  end)

  let make name = NodeIntf.Node ({ name }, (module I))
end

(* 
  * ValueNode
  * CallNode
  * MethodNode
  * RefNode
  * AssignmentNode
  * ScopingNode
*)

(* (foo[@tailcall]) arg *)

let create _ast = ()

(* let create ast =
  let rec traverse
    (expression : Location.t Ast.expression) =
    let (expr, _location) = expression in
    match expr with
    | ExprAssign (name, expr') ->
      let n = NodeIntf.Node ({
        target = 
      })
    | _ -> raise (Failure "oops") *)

(* module FooNode = NodeIntf.Make(struct
  type t = int
  
  let name t = Stdlib.string_of_int t
  let node_type _t = "FooNode"
  (* let location t = t.location *)
  let children _t = None
  let pretty_print _t = "<FooNode>"
end)

*)

let nodes = [
  AssignmentNode.make ("a", Ast.Nil) [];
  RefNode.make "a";
]

let () = nodes
|> Query.query_all ~f:(fun _node -> true) 
|> List.iter (fun node -> print_endline @@ NodeIntf.pretty_print node) 