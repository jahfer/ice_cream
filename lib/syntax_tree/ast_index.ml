module ValueNode = struct
  type data = {
    value : Location.t Ast.value;
    location : Location.t;
  }

  module I = Node.Make(struct
    type t = data
    let node_type _ = "ValueNode"
    let location t = t.location
    let children _ = None
    let parent _ = None
    let attributes t = [
      ("type", Ast.AstPrinter.print_value_type t.value);
      ("value", Ast.AstPrinter.print_value t.value)
    ]
  end)

  let make value location =
    Node.Node ({ value; location }, (module I))
end

module RefNode = struct
  type data = {
    name : string;
    location : Location.t;
    (* scope : string list; *)
  }

  module I = Node.Make(struct
    type t = data
    let node_type _ = "RefNode"
    let location t = t.location
    let children _ = None
    let parent _ = None
    let attributes t = [
      ("name", t.name)
    ]
  end)

  let from_data t =
    Node.Node (t, (module I))

  let make name location =
    from_data { name; location }
end

module CallNode = struct
  type data = {
    receiver : Node.t option;
    method_name : string;
    args : (Node.t list * Node.t option); (* positional args, block *)
    location : Location.t;
  }

  module I = Node.Make(struct
    type t = data

    let positional_args (p, _) = p

    let node_type _ = "CallNode"
    let location t = t.location
    let children t = Some (positional_args t.args)
    let parent _ = None
    let attributes t = [
      ("method", t.method_name);
    ]
  end)

  let make receiver method_name args location =
    Node.Node ({ receiver; method_name; args; location }, (module I))
end

module MethodNode = struct
  type data = {
    name : string;
    args : Location.t Ast.id list;
    location : Location.t;
    children : Node.t list;
  }

  module I = Node.Make(struct
    type t = data
    let node_type _ = "MethodNode"
    let location t = t.location
    let children _ = None
    let parent _ = None
    let attributes t = [
      ("name", t.name)
    ]
  end)

  let make name args location children =
    Node.Node ({ name; args; location; children }, (module I))
end

module ScopingNode = struct
  type data = {
    location : Location.t;
    children : Node.t list;
    scope_type : string;
  }

  module I = Node.Make(struct
    type t = data
    let node_type _ = "ScopingNode"
    let location t = t.location
    let children t = Some(t.children)
    let parent _ = None
    let attributes _ = []
  end)

  let from_data t =
    Node.Node (t, (module I))

  let make location children scope_type =
    Node.Node ({ location; children; scope_type }, (module I))
end

module AssignmentNode = struct
  type data = {
    target : RefNode.data;
    children : Node.t list
  }

  module I = Node.Make(struct
    type t = data
    let node_type _t = "AssignmentNode"
    let location t = t.target.location
    let children t = Some ((RefNode.from_data t.target) :: t.children)
    let parent _ = None
    let attributes _t = []
  end)

  let make target children =
    Node.Node ({ target; children }, (module I))
end

let create ast =
  let rec traverse
    (acc : Node.t list)
    (expression : Location.t Ast.expression) =
    let (expr, location) = expression in
    match expr with
    (* AssignmentNode *)
    | ExprAssign (name, expr')
    | ExprIVarAssign (name, expr') ->
      let children = (traverse [] expr') in
      let node = (AssignmentNode.make { name; location } children) in
      node :: acc
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
      (traverse[@tailcall]) acc expr'
    | ExprBlock (expr1, expr2) -> 
      let children1 = (traverse [] expr1) in
      let children2 = (traverse [] expr2) in
      let children = List.rev_append children1 children2 in
      let node = (ScopingNode.make location children "Block") in
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
    | ExprConst ((name, _t), _nesting) ->
      let node = RefNode.make name location in
      node :: acc
    | ExprLambda _ -> acc (* TODO *)
    | ExprProc _ -> acc (* TODO *)
    (* Unreachable *)
    | ExprConstAssign _ -> raise (Failure "ConstAssign found without Const subexpr!")
    (* Skip over *)
    | ExprEmptyBlock -> acc
    in List.fold_left traverse [] ast