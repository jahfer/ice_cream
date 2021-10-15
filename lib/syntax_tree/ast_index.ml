module Type = struct
  type Node.nodetype += Value | Ref | Call | Method | Scoping | Assignment
end

module ValueNode = struct
  type data = {
    value : Location.t Ast.value;
    location : Location.t;
  }

  module I = Node.Make(struct
    type t = data
    let node_type _ = Type.Value
    let location t = t.location
    let children _ = None
    let parent _ = None
    let attributes t = let open Node.Attr in [
      ("type", Str_ (Ast.AstPrinter.print_value_type t.value));
      ("value", Str_ (Ast.AstPrinter.print_value t.value));
      ("node_type", Str_ "Value");
    ]

    let to_rbs _ = ""
  end)

  let make value location =
    Node.Node ({ value; location }, (module I))
end

module RefNode = struct
  type reftype = Local | IVar | Const
  
  type data = {
    name : string;
    location : Location.t;
    reftype : reftype;
    super : Node.t option;
    (* scope : string list; *)
  }

  module I = Node.Make(struct
    type t = data
    let node_type _ = Type.Ref
    let location t = t.location
    let children t = match t.super with
    | None -> None
    | Some s -> Some [s]
    let parent _ = None
    let attributes t = let open Node.Attr in [
      ("name", Str_ t.name);
      ("node_type", Str_ "Ref");
    ]

    let to_rbs t = match t.reftype with
    | IVar -> Printf.sprintf "%s: %s" t.name "untyped"
    | _ -> Printf.sprintf "%s: %s" t.name "untyped"
  end)

  let from_data t =
    Node.Node (t, (module I))

  let make ?(super=None) name location reftype =
    from_data { name; location; reftype; super }
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

    let node_type _ = Type.Call
    let location t = t.location
    let children t = match t.receiver with
    | Some node -> Some (node :: (List.rev @@ positional_args t.args))
    | None -> Some (positional_args t.args)
    let parent _ = None
    let attributes t = let open Node.Attr in
    let attrs = [
      ("method", Str_ t.method_name);
      ("node_type", Str_ "Call");
    ] in 
    match t.receiver with
    | Some r -> ("receiver", Node.Node_ r) :: attrs
    | None -> attrs

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

  type Node.Attr.t += private MethodAttribute

  module I = Node.Make(struct
    type t = data
    let node_type _ = Type.Method
    let location t = t.location
    let children t = Some(t.children)
    let parent _ = None
    let attributes t = let open Node.Attr in [
      ("name", Str_ t.name);
      ("node_type", Str_ "Method");
      ("parameter_names", List_ (List.map (fun (name, _default, _argt) -> Str_ name) t.args))
    ]

    let to_rbs t =
      let print_positional = fun x ->
        String.concat ", " @@
          List.map (fun (s, t) ->
            match t with
            | Some Ast.(ExprValue Nil) -> Printf.sprintf "?Any? %s" s
            | Some Ast.(ExprValue Any) -> Printf.sprintf "untyped %s" s
            | Some Ast.ExprValue v -> Printf.sprintf "?%s %s" (Ast.AstPrinter.print_value_type v) s
            | None -> Printf.sprintf "%s" s
            | _ -> Printf.sprintf "?%s: TODO" s) x in

      let print_kwarg = fun x ->
        String.concat ", " @@
          List.map (fun (s, t) ->
            match t with
            | Some Ast.(ExprValue Nil) -> Printf.sprintf "%s?: Any?" s
            | Some Ast.(ExprValue Any) -> Printf.sprintf "%s: untyped" s
            | Some Ast.ExprValue v -> Printf.sprintf "?%s: %s" s (Ast.AstPrinter.print_value_type v)
            | None -> Printf.sprintf "%s" s
            | _ -> Printf.sprintf "?%s: TODO" s) x in

      let params = match List.partition_map
        (fun (name, default, arg_t) ->
          let param = (name, default) in
          match arg_t with | Ast.Positional -> Left param | Keyword -> Right param)
        t.args 
      with
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
  type scope = Module | Class

  type data = {
    location : Location.t;
    children : Node.t list;
    scope : scope;
  }

  let scope_as_str_ = function
  | Module -> "module"
  | Class -> "class"

  module I = Node.Make(struct
    type t = data
    let node_type _ = Type.Scoping
    let location t = t.location
    let children t = Some(t.children)
    let parent _ = None
    let attributes t = let open Node.Attr in [
      ("type", Str_ (scope_as_str_ t.scope));
      ("node_type", Str_ "Scoping");
    ]

    let to_rbs _= ""
  end)

  let from_data t =
    Node.Node (t, (module I))

  let make location children scope =
    Node.Node ({ location; children; scope }, (module I))
end

module AssignmentNode = struct
  type data = {
    target : RefNode.data;
    value : Node.t
  }

  module I = Node.Make(struct
    type t = data
    let node_type _t = Type.Assignment
    let location t = t.target.location
    let children t = Some ((RefNode.from_data t.target) :: [t.value])
    let parent _ = None
    let attributes _t = let open Node.Attr in [
      ("node_type", Str_ "Assignment");
    ]

    (* let to_rbs t = match t.target.reftype with
    | Const -> match Node.node_type t.value with
      | "Scoping" -> Printf.sprintf "%s %s" t.target.name t.attributes
      | _ -> raise (Failure "Unimplemented")
    | _ -> "" *)

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
      | [] -> failwith (Printf.sprintf "Assignment to `%s` has no value!" name)
      | _ :: _ -> failwith "Assignment has too many nodes for value" in
      let reftype = match expr with
      | ExprAssign _ -> RefNode.Local
      | ExprIVarAssign _ -> IVar
      | _ -> failwith "Unreachable" in
      let node = (AssignmentNode.make { name; location; reftype; super = None } value) in
      node :: acc
    | ExprConstAssign ((ExprConst ((name, _), _), location'), super, expr') ->
      let value = match traverse [] expr' with
      | v :: [] -> v
      (* TODO: Constants don't need a definition *)
      | [] -> ValueNode.make Any location' 
      | _ :: _ -> failwith "Assignment has too many nodes for value" in
      let s = match super with
      | Some sup -> Some (List.hd @@ traverse [] sup)
      | _ -> None in
      let node = (AssignmentNode.make { name; location; reftype = Const; super = s } value) in
      node :: acc
    (* RefNode *)
    | ExprVar (name, _t) ->
      (RefNode.make name location Local) :: acc
    | ExprIVar (name, _t) ->
      (RefNode.make name location IVar) :: acc
    (* ScopingNode *)
    | ExprModuleBody expr' ->
      let children = traverse [] expr' in
      let node = (ScopingNode.make location children Module) in
      node :: acc
    | ExprClassBody expr' ->
      let children = traverse [] expr' in
      let node = (ScopingNode.make location children Class) in
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
      let node = RefNode.make name location Const in
      node :: acc
    | ExprProc _ -> acc (* TODO *)
    (* Unreachable *)
    | ExprConstAssign _ -> failwith "ConstAssign found without Const subexpr!"
    (* Skip over *)
    | ExprEmptyBlock -> acc
    in List.fold_left traverse [] ast