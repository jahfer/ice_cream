type 'a value =
  | Hash of ('a value * 'a value) list
  | Bool of bool
  | Float of float
  | Int of int
  | Array of 'a expression list
  | String of string
  | Symbol of string
  | Nil
  | Any

and 'a id = string * 'a value

and 'a nesting = 'a id list

and 'a call_args = 'a expression list * 'a expression option (* positional args, block *)

and 'a expr =
  | ExprCall of 'a expression * string * 'a call_args (* receiver, method, args *)
  | ExprFunc of string * 'a id list * 'a expression (* name, args, body *)
  | ExprLambda of 'a id list * 'a expression (* args, body *)
  | ExprProc of 'a id list * 'a expression (* args, body *)
  | ExprValue of 'a value
  | ExprVar of 'a id
  | ExprConst of 'a id * 'a nesting
  | ExprIVar of 'a id
  | ExprAssign of string * 'a expression
  | ExprIVarAssign of string * 'a expression
  | ExprConstAssign of 'a expression * 'a expression
  | ExprBlock of 'a expression * 'a expression
  | ExprClassBody of 'a expression
  | ExprModuleBody of 'a expression
  | ExprEmptyBlock

and 'a expression = 'a expr * 'a

let rec map_metadata fn expr meta =
  let swap_meta = map_metadata fn in
  let new_expr = match expr with
    | ExprFunc (name, args, (body_expr, body_meta)) ->
      ExprFunc (name, args, swap_meta body_expr body_meta)
    | ExprLambda (args, (body_expr, body_meta))
    | ExprProc (args, (body_expr, body_meta)) ->
      ExprProc (args, swap_meta body_expr body_meta)
    | ExprConst (name, nesting) ->
      ExprConst (name, nesting)
    | ExprAssign (name, (a_expr, a_meta)) ->
      ExprAssign (name, swap_meta a_expr a_meta)
    | ExprIVarAssign (name, (a_expr, a_meta)) ->
      ExprIVarAssign (name, swap_meta a_expr a_meta)
    | ExprConstAssign (cexpr, (a_expr, a_meta)) ->
      ExprConstAssign (cexpr, swap_meta a_expr a_meta) (* todo: swap_meta cexpr? *)
    | ExprIVar name -> ExprIVar name
    | ExprVar name -> ExprVar name
    | ExprValue v -> ExprValue v
    | ExprCall ((expr_a, meta_a), b, (positional_args, block_arg)) ->
      let new_expr = swap_meta expr_a meta_a
      and new_positional_args = List.map (fun (e, m) -> swap_meta e m) positional_args
      and new_block_arg = Option.map (fun (block, block_meta) -> swap_meta block block_meta) block_arg
      in ExprCall (new_expr, b, (new_positional_args, new_block_arg))
    | ExprBlock ((expr_a, meta_a), (expr_b, meta_b)) ->
      let a = swap_meta expr_a meta_a
      and b = swap_meta expr_b meta_b
      in ExprBlock (a, b)
    | ExprEmptyBlock -> ExprEmptyBlock
    | ExprClassBody((expr, meta)) ->
      ExprClassBody (swap_meta expr meta)
    | ExprModuleBody((expr, meta)) ->
      ExprModuleBody (swap_meta expr meta)
  in fn new_expr meta

module AstPrinter = struct
  let rec print_cexpr (expr, _) =
    (* printf "%s\n" Location.print_loc expr_loc; *)
    Printf.sprintf "%s" (print_ast expr)

  and print_ast = function
    | ExprCall (receiver, meth, (positional_args, block)) ->
      Printf.sprintf "(send %s `%s (args %s (blk %s)))"
        (print_cexpr receiver)
        meth
        (print_list print_cexpr positional_args)
        (Option.value ~default:"None" (Option.map print_cexpr block))
    | ExprFunc (name, params, body) ->
      Printf.sprintf "(def `%s %s %s)" name (print_params params) (print_cexpr body)
    | ExprLambda (params, body) ->
      Printf.sprintf "(lambda %s %s)" (print_params params) (print_cexpr body)
    | ExprProc (params, body) ->
      Printf.sprintf "(proc %s %s)" (print_params params) (print_cexpr body)
    | ExprVar ((name, _value))  ->
      Printf.sprintf "(lvar `%s)" name
    | ExprConst ((name, _value), nesting) ->
      Printf.sprintf "(const (nesting [%s]) `%s)" (print_nesting nesting) name
    | ExprIVar ((name, _value)) ->
      Printf.sprintf "(ivar `%s)" name
    | ExprAssign (name, expr) ->
      Printf.sprintf "(lvasgn `%s %s)" name (print_cexpr expr)
    | ExprIVarAssign (name, expr) ->
      Printf.sprintf "(ivasgn %s %s)" name (print_cexpr expr)
    | ExprConstAssign (cexpr, expr) ->
      Printf.sprintf "(casgn %s %s)" (print_cexpr cexpr) (print_cexpr expr)
    | ExprValue (value) ->
      Printf.sprintf "%s" (print_value value)
    | ExprBlock (expr1, expr2) ->
      Printf.sprintf "%s %s" (print_cexpr expr1) (print_cexpr expr2)
    | ExprClassBody(expr) ->
      Printf.sprintf "(class %s)" (print_cexpr expr)
    | ExprModuleBody(expr) ->
      Printf.sprintf "(module %s)" (print_cexpr expr)
    | ExprEmptyBlock ->
      Printf.sprintf "()"

  and print_value = function
    | Hash obj     -> print_hash obj
    | Array l      -> Printf.sprintf "[%s]" (print_list print_cexpr l)
    | String s     -> Printf.sprintf "\"%s\"" s
    | Symbol s     -> Printf.sprintf ":%s" s
    | Int i        -> Printf.sprintf "%d" i
    | Float x      -> Printf.sprintf "%f" x
    | Bool true    -> Printf.sprintf "true"
    | Bool false   -> Printf.sprintf "false"
    | Nil          -> Printf.sprintf "nil"
    | Any          -> Printf.sprintf "?"

  and print_params arr =
    let buf = Buffer.create 256 in
    if List.length(arr) > 0 then (
      Buffer.add_string buf "(params";
      List.iteri (fun _i (id, _value) ->
        Buffer.add_string buf " ";
        Printf.bprintf buf "(param `%s)" id)
        arr;
      Buffer.add_string buf ")"
    ) else Buffer.add_string buf "()";
    Buffer.contents buf

  and print_hash obj =
    let buf = Buffer.create 256 in
    Buffer.add_string buf "{ ";
    List.iteri (fun i (key, value) ->
        if (i <> 0) then Buffer.add_string buf ", ";
        Printf.bprintf buf "%s: %s" (print_value key) (print_value value)) obj;
    Buffer.add_string buf " }";
    Buffer.contents buf

  and print_list: 'a. ('a -> string) -> ('a list) -> string = fun printer lst ->
    let buf = Buffer.create 256 in
    List.iteri (fun i v ->
        if i > 0 then
          Buffer.add_string buf " ";
        Buffer.add_string buf (printer v)) lst;
    Buffer.contents buf

  and print_nesting lst =
    let buf = Buffer.create 256 in
    List.iteri (fun i (name, _value) ->
        if i > 0 then
          Buffer.add_string buf " ";
        Printf.bprintf buf "%s" name) lst;
    Buffer.contents buf
end

(* module Index : sig
  type type_key =
  | KAssign
  | KIVarAssign
  | KConstAssign
  | KFunc

  type const = Root | Const of string
  type scope = const list 

  module Node : sig
    type t = {
      id : int;
      ident : string;
      expr : Location.t expr;
      location : Location.t;
      parent : t option ref;
      scope : scope;
    }
    
    val create : Location.t expression -> scope -> string -> t

    val compare : t -> t -> int

    module Reader : sig
      val read : t -> string
    end
  end

  module NodeSet : Set.S with type elt = Node.t

  type node_list_t = (type_key * NodeSet.t ref) list
  type range = { pos_beg: int; pos_end: int }
  type context = {
    nesting : const list;
    node_list : node_list_t;
  }

  val create : (Location.t expression list) -> context

end = struct

  type const = Root | Const of string
  type scope = const list

  type type_key =
  | KAssign
  | KIVarAssign
  | KConstAssign
  | KFunc

  module Node = struct
    (* 
      TODO: define cross-file "scopes" based on module definitions.
      Browse defined variables by given scope (within file and across)
    *)

    type t = {
      id : int;
      ident : string;
      expr : Location.t expr;
      location : Location.t;
      parent : t option ref;
      scope : scope;
    }

    let create (input : Location.t expression) (scope : scope) (ident : string)= 
      let (expr, location) = input in {
        id = location.id;
        ident;
        expr;
        location;
        parent = ref None;
        scope;
      }

    let compare (a : t) (b : t) = 
      Stdlib.compare a.id b.id

    module Reader = struct
      let scope_as_string (scope) = 
        scope
        |> List.rev_map (fun x -> match x with Root -> "" | Const x -> x)
        |> String.concat "::"

      let read (node : t) = 
        let scope_str = scope_as_string node.scope in
        let title = match node.expr with
        | ExprConstAssign ((ExprConst ((name, _), _), _), _) ->
          let name = String.concat "::" [scope_str; name] in
          Printf.sprintf "\nDefinition of %s" name
        | _ -> raise (Failure "oops") in
        Printf.sprintf "%s\n%s" title (Location.loc_as_string node.location)
    end
  end

  module NodeSet = Set.Make(Node)

  type node_list_t = (type_key * NodeSet.t ref) list
  type range = { pos_beg: int; pos_end: int }
  type context = {
    nesting : scope;
    node_list : node_list_t;
  }

  let node_list_default () : node_list_t = [
    (KAssign, ref NodeSet.empty);
    (KIVarAssign, ref NodeSet.empty);
    (KConstAssign, ref NodeSet.empty);
    (KFunc, ref NodeSet.empty);
  ]

  let context_default () : context = {
    nesting = [Root];
    node_list = node_list_default ();
  }

  let create (ast) = 
    let rec traverse
      (context : context)
      (expression : Location.t expression) =

      let (expr, _loc) = expression in
      match expr with

      | ExprConstAssign ((ExprConst ((name, _t), nesting), _loc), e) -> (
        let original_nesting = context.nesting in
        let ys = List.map (fun (x, _) -> (Const x)) nesting in
        let n = List.append ys context.nesting in

        let list = List.assoc KConstAssign context.node_list in
        list := NodeSet.add (Node.create expression n name) !list;

        let c = { context with nesting = (Const name) :: n } in
        let c' = traverse c e in
        { c' with nesting = original_nesting }
      )

      | ExprBlock (e1, e2) ->
        let c' = traverse context e1 in
        traverse c' e2

      | ExprClassBody e | ExprModuleBody e -> traverse context e

      | ExprAssign (name, e) ->
        let list = List.assoc KAssign context.node_list in
        list := NodeSet.add (Node.create expression context.nesting name) !list;
        traverse context e

      | ExprIVarAssign (name, e) ->
        let list = List.assoc KIVarAssign context.node_list in
        list := NodeSet.add (Node.create expression context.nesting name) !list;
        traverse context e

      | ExprFunc (name, _, e) ->
        let list = List.assoc KFunc context.node_list in
        list := NodeSet.add (Node.create expression context.nesting name) !list;
        traverse context e
        
      (* others *)
      | ExprCall _
      | ExprLambda _
      | ExprProc _
      | ExprValue _
      | ExprVar _
      | ExprConst _
      | ExprIVar _
      | ExprConstAssign _
      | ExprEmptyBlock -> context
      in List.fold_left traverse (context_default ()) ast

    (* 
    - Const assignment
    - Var assignment
    - Instance var assignment
    - Function definition

    - Object interfaces (public methods)

    - Function calls
    - Const reference
     *)
end *)

(* let nodes = [
  (ExprTypeA, [ExprA 0; ExprA 3]);
  (ExprTypeB, [ExprB 1]);
  (ExprTypeC, [ExprC 2; ExprC 4]);
  (ExprTypeD, [ExprD 3])
] *)