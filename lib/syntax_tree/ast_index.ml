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

    let to_rbs _ = ""
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

    let to_rbs _ = ""
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
    let children t = match t.receiver with
    | Some node -> Some (node :: positional_args t.args)
    | None -> Some (positional_args t.args)
    let parent _ = None
    let attributes t = [
      ("method", t.method_name);
    ]

    let to_rbs _ = ""
  end)

  let make receiver method_name args location =
    Node.Node ({ receiver; method_name; args; location }, (module I))
end

module MethodNode = struct
  type data = {
    name : string;
    args : Location.t Ast.method_args;
    location : Location.t;
    children : Node.t list;
  }

  module I = Node.Make(struct
    type t = data
    let node_type _ = "MethodNode"
    let location t = t.location
    let children t = Some(t.children)
    let parent _ = None
    let attributes t = [
      ("name", t.name)
    ]

    let to_rbs t =
      let print_positional = fun x ->
        String.concat ", " @@
          List.map (fun (s, t) ->
            match t with
            | Ast.Nil -> Printf.sprintf "?Any? %s" s
            | Any -> Printf.sprintf "untyped %s" s
            | _ -> Printf.sprintf "?%s %s" (Ast.AstPrinter.print_value_type t) s) x in

      let print_kwarg = fun x ->
        String.concat ", " @@
          List.map (fun (s, t) ->
            match t with
            | Ast.Nil -> Printf.sprintf "%s?: Any?" s
            | Any -> Printf.sprintf "%s: untyped" s
            | _ -> Printf.sprintf "?%s: %s" s (Ast.AstPrinter.print_value_type t)) x in

      let params = match List.partition_map
        (fun (p, typ) -> match typ with | Ast.Positional -> Left p | Keyword -> Right p)
        t.args with
        | [], [] -> Printf.sprintf ""
        | pos, [] -> (print_positional pos)
        | [], kwarg ->  (print_kwarg kwarg)
        | pos, kwarg -> Printf.sprintf "%s, %s" (print_positional pos) (print_kwarg kwarg) in
        Printf.sprintf "def %s: (%s) -> %s" t.name params "untyped"
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
    let attributes t = [
      ("type", t.scope_type)
    ]

    let to_rbs _ = ""
  end)

  let from_data t =
    Node.Node (t, (module I))

  let make location children scope_type =
    Node.Node ({ location; children; scope_type }, (module I))
end

module AssignmentNode = struct
  type data = {
    target : RefNode.data;
    value : Node.t
  }

  module I = Node.Make(struct
    type t = data
    let node_type _t = "AssignmentNode"
    let location t = t.target.location
    let children t = Some ((RefNode.from_data t.target) :: [t.value])
    let parent _ = None
    let attributes t = [
      ("label", t.target.name)
    ]

    let to_rbs _ = ""
  end)

  let make target value =
    Node.Node ({ target; value }, (module I))
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
      let value = match traverse [] expr' with
      | v :: [] -> v
      (* TODO: Constants don't need a definition *)
      | [] -> raise (Failure (Printf.sprintf "Assignment to `%s` has no value!" name))
      | _ :: _ -> raise (Failure "Assignment has too many nodes for value") in
      let node = (AssignmentNode.make { name; location } value) in
      node :: acc
    | ExprConstAssign ((ExprConst ((name, _), _), location'), expr') ->
      let value = match traverse [] expr' with
      | v :: [] -> v
      (* TODO: Constants don't need a definition *)
      | [] -> ValueNode.make Any  location' 
      | _ :: _ -> raise (Failure "Assignment has too many nodes for value") in
      let node = (AssignmentNode.make { name; location } value) in
      node :: acc
    (* RefNode *)
    | ExprVar (name, _t)
    | ExprIVar (name, _t) ->
      (RefNode.make name location) :: acc
    (* ScopingNode *)
    | ExprModuleBody expr' ->
      let children = traverse [] expr' in
      let node = (ScopingNode.make location children "Module") in
      node :: acc
    | ExprClassBody expr' ->
      let children = traverse [] expr' in
      let node = (ScopingNode.make location children "Class") in
      node :: acc
    | ExprBlock (expr1, expr2) -> 
      let children1 = (traverse [] expr1) in
      let children2 = (traverse [] expr2) in
      let children = List.append children1 children2 in
      List.append children acc
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
    | ExprProc _ -> acc (* TODO *)
    (* Unreachable *)
    | ExprConstAssign _ -> raise (Failure "ConstAssign found without Const subexpr!")
    (* Skip over *)
    | ExprEmptyBlock -> acc
    in List.fold_left traverse [] ast