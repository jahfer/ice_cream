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

let rec replace_metadata fn expr meta =
  let swap_meta = replace_metadata fn in
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
  open Core

  let rec print_cexpr _outc (expr, _) =
    (* printf "%a\n" Location.print_loc expr_loc; *)
    printf "%a" print_ast expr

  and print_ast _outc = function
    | ExprCall (receiver, meth, args) ->
      printf "(send %a `%s (args %a))" print_cexpr receiver meth print_cexpr_list args
    | ExprFunc (name, params, body) ->
      printf "(def `%s %a %a)" name print_params params print_cexpr body
    | ExprLambda (params, body) ->
      printf "(lambda %a %a)" print_params params print_cexpr body
    | ExprVar ((name, _value))  ->
      printf "(lvar `%s)" name
    | ExprConst ((name, _value), nesting) ->
      printf "(const (nesting [%a]) `%s)" print_nesting nesting name
    | ExprIVar ((name, _value)) ->
      printf "(ivar `%s)" name
    | ExprAssign (name, expr) ->
      printf "(lvasgn `%s %a)" name print_cexpr expr
    | ExprIVarAssign (name, expr) ->
      printf "(ivasgn %s %a)" name print_cexpr expr
    | ExprConstAssign (cexpr, expr) ->
      printf "(casgn %a %a)" print_cexpr cexpr print_cexpr expr
    | ExprValue (value) ->
      printf "%a" print_value value
    | ExprBlock (expr1, expr2) ->
      printf "%a %a" print_cexpr expr1 print_cexpr expr2
    | ExprClassBody(expr) ->
      printf "(class %a)" print_cexpr expr
    | ExprModuleBody(expr) ->
      printf "(module %a)" print_cexpr expr
    | ExprEmptyBlock ->
      printf "()"

  and print_value outc = function
    | Hash obj     -> print_hash outc obj
    | Array l      -> printf "[%a]" print_value_list l
    | String s     -> printf "\"%s\"" s
    | Symbol s     -> printf ":%s" s
    | Int i        -> printf "%d" i
    | Float x      -> printf "%f" x
    | Bool true    -> Out_channel.output_string outc "true"
    | Bool false   -> Out_channel.output_string outc "false"
    | Nil          -> Out_channel.output_string outc "nil"
    | Any          -> printf "?"

  and print_params outc arr =
    if List.length(arr) > 0 then begin
      Out_channel.output_string outc "(params";
      List.iteri ~f:(fun _i (id, _value) ->
          Out_channel.output_string outc " ";
          printf "(param `%s)" id) arr;
      Out_channel.output_string outc ")"
    end else printf "()"

  and print_hash outc obj =
    Out_channel.output_string outc "{ ";
    List.iteri ~f:(fun i (key, value) ->
        if (i <> 0) then printf ", ";
        printf "%a: %a" print_value key print_value value) obj;
    Out_channel.output_string outc " }"

  and print_value_list outc lst =
    List.iteri ~f:(fun i v ->
        if i > 0 then
          Out_channel.output_string outc " ";
        print_value outc v) lst

  and print_nesting outc lst =
    List.iteri ~f:(fun i (name, _value) ->
        if i > 0 then
          Out_channel.output_string outc " ";
        printf "%s" name) lst

  and print_cexpr_list outc lst =
    List.iteri ~f:(fun i v ->
        if i > 0 then
          Out_channel.output_string outc " ";
        print_cexpr outc v) lst
end
