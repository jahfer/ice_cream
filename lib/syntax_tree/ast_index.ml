module NodeInterface = struct
  module type S = sig
    type elt
    type child_elt

    val node_type : elt -> string
    val location : elt -> Location.t
    val children : elt -> child_elt list option
    val attributes : elt -> (string * string) list
  end

  type t =
    | Node : 'a * (module S with type elt = 'a and type child_elt = t) -> t

  module type T = sig
    type node := t
    type t

    val node_type : t -> string
    val location : t -> Location.t
    val children : t -> node list option
    val attributes : t -> (string * string) list
  end

  module Make (X : T) : S with type child_elt = t and type elt = X.t =
  struct
    type elt = X.t
    type child_elt = t

    include X
  end

  (* Helper methods *)

  let rec pretty_print : ?indent:int -> t -> string =
    fun ?indent:(i=0) (Node (x, (module M))) ->
      let node_type = M.node_type x in
      let attributes = M.attributes x in
      let attr_string = match attributes with
      | [] -> ""
      | attrs -> let vals = String.concat " "
        (List.map (fun (k,v) -> Printf.sprintf "%s=\"%s\"" k v) attrs) in
        " " ^ vals ^ " " in
      match M.children x with
      | Some (children) ->
        Printf.sprintf "%*s<%s%s>\n%s\n%*s</%s>"
          i ""
          node_type
          attr_string
          (String.concat "\n" (List.map (pretty_print ~indent:(i+1)) children))
          i ""
          node_type
      | None -> Printf.sprintf "%*s<%s%s/>" i "" node_type attr_string

  let node_type : t -> string =
    fun (Node (x, (module M))) -> M.node_type x

  let location : t -> Location.t =
    fun (Node (x, (module M))) -> M.location x

  let attributes : t -> (string * string) list =
    fun (Node (x, (module M))) -> M.attributes x

  let children : t -> t list option =
    fun (Node (x, (module M))) -> M.children x
end

module Query = struct
  let rec lazy_query ~f all =
    List.to_seq all
    |> Seq.flat_map (fun x ->
      match NodeInterface.children x with
      | Some (children) -> Seq.cons x (lazy_query ~f children)
      | None -> Seq.return x
    )
    |> Seq.filter f

  let query_all ~f all =
    List.of_seq @@ lazy_query ~f:f all

  let query ~f all =
    let result = lazy_query ~f:f all in
    match result () with
    | Cons (x, _) -> Some x
    | Nil -> None
end

module ValueNode = struct
  type data = {
    value : Location.t Ast.value;
    location : Location.t;
  }

  module I = NodeInterface.Make(struct
    type t = data
    let node_type _ = "ValueNode"
    let location t = t.location
    let children _ = None
    let attributes t = [
      ("type", Ast.AstPrinter.print_value_type t.value);
      ("value", Ast.AstPrinter.print_value t.value)
    ]
  end)

  let make value location =
    NodeInterface.Node ({ value; location }, (module I))
end

module RefNode = struct
  type data = {
    name : string;
    location : Location.t;
    (* scope : string list; *)
  }

  module I = NodeInterface.Make(struct
    type t = data
    let node_type _ = "RefNode"
    let location t = t.location
    let children _ = None
    let attributes t = [
      ("name", t.name)
    ]
  end)

  let from_data t =
    NodeInterface.Node (t, (module I))

  let make name location =
    from_data { name; location }
end

module CallNode = struct
  type data = {
    receiver : NodeInterface.t option;
    method_name : string;
    args : (NodeInterface.t list * NodeInterface.t option); (* positional args, block *)
    location : Location.t;
  }

  module I = NodeInterface.Make(struct
    type t = data
    let node_type _ = "CallNode"
    let location t = t.location
    let children _ = None
    let attributes t = [
      ("method", t.method_name)
    ]
  end)

  let make receiver method_name args location =
    NodeInterface.Node ({ receiver; method_name; args; location }, (module I))
end

module MethodNode = struct
  type data = {
    name : string;
    args : Location.t Ast.id list;
    location : Location.t;
    children : NodeInterface.t list;
  }

  module I = NodeInterface.Make(struct
    type t = data
    let node_type _ = "MethodNode"
    let location t = t.location
    let children _ = None
    let attributes t = [
      ("name", t.name)
    ]
  end)

  let make name args location children =
    NodeInterface.Node ({ name; args; location; children }, (module I))
end

module ScopingNode = struct
  type data = {
    location : Location.t;
    children : NodeInterface.t list;
  }

  module I = NodeInterface.Make(struct
    type t = data

    let node_type _ = "ScopingNode"

    let location t = t.location

    let children t = Some(t.children)

    let attributes _ = []
  end)

  let from_data t =
    NodeInterface.Node (t, (module I))

  let make location children =
    NodeInterface.Node ({ location; children }, (module I))
end

module AssignmentNode = struct
  type data = {
    target : RefNode.data;
    children : NodeInterface.t list
  }

  module I = NodeInterface.Make(struct
    type t = data

    let node_type _t = "AssignmentNode"

    let location t = t.target.location

    let children t = Some ((RefNode.from_data t.target) :: t.children)

    let attributes _t = []
  end)

  let make target children =
    NodeInterface.Node ({ target; children }, (module I))
end

let create ast =
  let rec traverse
    (acc : NodeInterface.t list)
    (expression : Location.t Ast.expression) =
    let (expr, location) = expression in
    match expr with
    (* AssignmentNode *)
    | ExprAssign (name, expr')
    | ExprIVarAssign (name, expr') ->
      let children = (traverse [] expr') in
      let node = (AssignmentNode.make { name; location } children) in
      let acc' = node :: acc in
      (traverse[@tailcall]) acc' expr'
    | ExprConstAssign ((ExprConst ((name, _t), _nesting), location'), expr') ->
      let children = (traverse [] expr') in
      let node = (AssignmentNode.make { name; location = location' } children) in
      node :: acc
    (* RefNode *)
    | ExprVar (name, _t)
    | ExprIVar (name, _t) ->
      (RefNode.make name location) :: acc
    (* ScopingNode *)
    | ExprModuleBody expr'
    | ExprClassBody expr' ->
      traverse acc expr'
    | ExprBlock (expr1, expr2) -> 
      let children1 = (traverse [] expr1) in
      let children2 = (traverse [] expr2) in
      let children = List.rev_append children1 children2 in
      let node = (ScopingNode.make location children) in
      node :: acc
    (* MethodNode *)
    | ExprFunc (name, args, expr') ->
      let children = (traverse [] expr') in
      let node = MethodNode.make name args location children in
      node :: acc
    (* CallNode *)
    | ExprCall (receiver, method_name, args) ->
      let pos_args, blk = args in
      let pos_args' = List.fold_left traverse [] pos_args in
      let blk' = match blk with
      | Some (b) -> (match traverse [] b with
        | b :: [] -> Some(b)
        | _ -> None)
      | None -> None in
      let node = match traverse [] receiver with
      | x :: [] -> Some(x)
      | _ -> None in
      let node = CallNode.make node method_name (pos_args', blk') location in
      node :: acc
    (* ValueNode *)
    | ExprValue (v) ->
      let node = ValueNode.make v location in
      node :: acc
    | ExprConst _ -> acc (* TODO *)
    | ExprLambda _ -> acc (* TODO *)
    | ExprProc _ -> acc (* TODO *)
    (* Unreachable *)
    | ExprConstAssign _ -> raise (Failure "ConstAssign found without Const subexpr!")
    (* Skip over *)
    | ExprEmptyBlock -> acc
    in List.fold_left traverse [] ast

(* let nodes = [
  AssignmentNode.make ("a", Ast.Nil) [];
  RefNode.make "a";
]

let () = nodes
|> Query.query_all ~f:(fun _node -> true) 
|> List.iter (fun node -> print_endline @@ NodeInterface.pretty_print node)  *)