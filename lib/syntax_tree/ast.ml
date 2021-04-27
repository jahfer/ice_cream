type value =
  | Hash of (value * value) list
  | Bool of bool
  | Float of float
  | Int of int
  | Array of value list
  | String of string
  | Symbol of string
  | Nil
  | Any

and id = string * value

and nesting = id list

and 'a expr =
  | ExprCall of 'a expression * string * 'a expression list (* receiver, method, args *)
  | ExprFunc of string * id list * 'a expression (* name, args, body *)
  | ExprLambda of id list * 'a expression (* args, body *)
  | ExprValue of value
  | ExprVar of id
  | ExprConst of id * nesting
  | ExprIVar of id
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
    | ExprLambda (args, (body_expr, body_meta)) ->
      ExprLambda (args, swap_meta body_expr body_meta)
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
    | ExprCall ((expr_a, meta_a), b, args) ->
      let new_expr = swap_meta expr_a meta_a
      and new_args = List.map (fun (e, m) -> swap_meta e m) args
      in ExprCall (new_expr, b, new_args)
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
    | ExprCall (receiver, meth, args) ->
      Printf.sprintf "(send %s `%s (args %s))" (print_cexpr receiver) meth (print_cexpr_list args)
    | ExprFunc (name, params, body) ->
      Printf.sprintf "(def `%s %s %s)" name (print_params params) (print_cexpr body)
    | ExprLambda (params, body) ->
      Printf.sprintf "(lambda %s %s)" (print_params params) (print_cexpr body)
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
    | Array l      -> Printf.sprintf "[%s]" (print_value_list l)
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

  and print_value_list lst =
    let buf = Buffer.create 256 in
    List.iteri (fun i v ->
        if i > 0 then
          Buffer.add_string buf " ";
        Buffer.add_string buf (print_value v)) lst;
    Buffer.contents buf

  and print_nesting lst =
    let buf = Buffer.create 256 in
    List.iteri (fun i (name, _value) ->
        if i > 0 then
          Buffer.add_string buf " ";
        Printf.bprintf buf "%s" name) lst;
    Buffer.contents buf

  and print_cexpr_list lst =
    let buf = Buffer.create 256 in
    List.iteri (fun i v ->
        if i > 0 then
          Buffer.add_string buf " ";
        Buffer.add_string buf (print_cexpr v)) lst;
    Buffer.contents buf
end
