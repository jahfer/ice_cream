type arg_type = Positional | Keyword

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
  | Lambda of 'a method_args * 'a expression (* args, body *)

and 'a id = string * 'a value

and 'a nesting = 'a id list

and 'a call_args = 'a expression list * 'a expression option (* positional args, block *)

and 'a method_args = ('a id * arg_type) list

and 'a expr =
  | ExprCall of 'a expression * string * 'a call_args (* receiver, method, args *)
  | ExprFunc of string * 'a method_args * 'a expression (* name, args, body *)
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
  (* TODO: Remove these *)
  | ExprProc of 'a method_args * 'a expression (* args, body *)

and 'a expression = 'a expr * 'a

type 'a decl =
  | DeclClass of string * type_variable list option * 'a type_interface
  | DeclModule of string * type_variable list option
  | DeclMethod of string

and 'a type_interface = 'a declaration list
and type_variable = TypeVar of string
and 'a declaration = 'a decl * 'a

let rec map_metadata fn expr meta =
  let swap_meta = map_metadata fn in
  let new_expr = match expr with
    | ExprFunc (name, args, (body_expr, body_meta)) ->
      ExprFunc (name, args, swap_meta body_expr body_meta)
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
    | Hash obj   -> print_hash obj
    | Array l    -> Printf.sprintf "[%s]" (print_list print_cexpr l)
    | String s   -> Printf.sprintf "%s" s
    | Symbol s   -> Printf.sprintf ":%s" s
    | Int i      -> Printf.sprintf "%d" i
    | Float x    -> Printf.sprintf "%f" x
    | Bool true  -> Printf.sprintf "true"
    | Bool false -> Printf.sprintf "false"
    | Nil        -> Printf.sprintf "nil"
    | Any        -> Printf.sprintf "?"
    | Lambda _   -> Printf.sprintf "-> (...) { ... }"

  and print_value_type = function
  | Hash _     -> "Hash"
  | Array _    -> "Array"
  | String _   -> "String"
  | Symbol _   -> "Symbol"
  | Int _      -> "Integer"
  | Float _    -> "Float"
  | Bool true  -> "bool"
  | Bool false -> "bool"
  | Nil        -> "NilClass"
  | Any        -> "?"
  | Lambda _   -> "Lambda"

  and print_params arr =
    let buf = Buffer.create 256 in
    if List.length(arr) > 0 then (
      Buffer.add_string buf "(params";
      List.iteri (fun _i ((id, _value), _param_type) ->
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
