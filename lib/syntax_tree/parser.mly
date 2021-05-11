%token NAMESPACE
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token TRUE FALSE NIL
%token LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN LAMBEG
%token COLON COMMA
%token EOS EOF
%token <string> ID FID IVAR
%token <string> CONST
%token EQ DEF END LAMBDA DOT CLASSDEF MODDEF

%{
  open Ast
  open Location

  let loc_annot
    ((start_pos, end_pos) : Lexing.position * Lexing.position)
    (expr : Location.t Ast.expr) =
      (expr, { id = Location.gen_id (); start_pos; end_pos; })
%}

%start <Location.t Ast.expression option> prog

%%

prog:
  | s = top_statement { Some s }
  | EOF               { None   }
  ;

top_statement:
  | s = stmt top_statement_end { s }
  %inline stmt:
  | s = statement { s }
  // this needs to be here so we don't swallow the EOS
  | m = ID args = separated_nonempty_list(COMMA, statement) {
    let sub_expr = ExprValue(Nil) |> loc_annot $sloc in
    ExprCall(sub_expr, m, args)   |> loc_annot $sloc
  }
  ;

statement_end:
  EOS { };

top_statement_end:
  statement_end | EOF { };

statement:
  | ref = ID             {
    ExprVar((ref, Any)) |> loc_annot $sloc
  }
  | ref = iv_identifier          {
    ExprIVar(ref) |> loc_annot $sloc
  }
  | c = const                    {
    let (const, nesting) = c in
    let nesting_t = List.map (fun x -> (x, Any)) nesting in
    ExprConst((const, Any), nesting_t) |> loc_annot $sloc
  }
  | id = ID EQ v = rhs_assign    {
    ExprAssign(id, v) |> loc_annot $sloc
  }
  | id = IVAR EQ v = rhs_assign  {
    ExprIVarAssign(id, v) |> loc_annot $sloc
  }
  | cls = const EQ v = rhs_assign  {
    let (c, nesting) = cls in
    let nesting_t = List.map (fun x -> (x, Any)) nesting in
    let const = ExprConst((c, Any), nesting_t) |> loc_annot $loc(cls) in
    ExprConstAssign(const, v) |> loc_annot ($startpos(cls), $endpos)
  }
  | p = primitive                {
    ExprValue(p) |> loc_annot $sloc
  }
  | c = class_def                { c }
  | m = mod_def                  { m }
  | e = expr                     { e }
  ;

rhs_assign:
  | s = statement { s }
  ;

expr:
  | c = command_call  { c }
  | LAMBDA l = lambda { l }
  | f = func          { f }
  ;

command_call:
  c = command { c } ;

command:
  | m = method_call args = command_args {
    let sub_expr = ExprValue(Nil) |> loc_annot $sloc in
    ExprCall(sub_expr, m, args)   |> loc_annot $sloc
  }
  | m = ID args = command_args {
    let sub_expr = ExprValue(Nil) |> loc_annot $sloc in
    ExprCall(sub_expr, m, args)   |> loc_annot $sloc
  }
  | c1 = ID call_op c2 = method_call {
    let sub_expr = ExprVar((c1, Any)) |> loc_annot $loc(c1) in
    ExprCall(sub_expr, c2, []) |> loc_annot $sloc
  }
  | cst = const call_op c2 = method_call {
    let (c, nesting) = cst in
    let nesting_t = List.map (fun x -> (x, Any)) nesting in
    let const = ExprConst((c, Any), nesting_t) |> loc_annot $loc(cst) in
    ExprCall(const, c2, []) |> loc_annot $sloc
  }
  | c1 = ID call_op c2 = method_call args = command_args {
    let sub_expr = ExprVar((c1, Any))   |> loc_annot $loc(c1) in
    ExprCall(sub_expr, c2, args) |> loc_annot $sloc
  }
  | cst = const call_op c2 = method_call args = command_args {
    let (c, nesting) = cst in
    let nesting_t = List.map (fun x -> (x, Any)) nesting in
    let const = ExprConst((c, Any), nesting_t) |> loc_annot $loc(cst) in
    ExprCall(const, c2, args) |> loc_annot $sloc
  }
  ;

call_op: DOT { } ;

method_call:
  id = FID { id } ;

command_args:
  args = call_args { args } ;

iv_identifier:
  iv = IVAR { iv, Any } ;

func:
  | DEF fn = ID args = fn_args? EOS? END {
    let args = match args with
    | Some(list) -> list
    | None -> []
    in
    let body = ExprValue(Nil) |> loc_annot $sloc in
    ExprFunc(fn, args, body) |> loc_annot $sloc
  }
  // Multi-line function
  | DEF fn = ID args = fn_args? EOS? body = nonempty_list(top_statement) END {
    let args = match args with
    | Some(list) -> list
    | None -> []
    in
    let empty_body = ExprEmptyBlock |> loc_annot $sloc in
    let body_expr = List.fold_left (fun acc expr ->
      ExprBlock(expr, acc) |> loc_annot $sloc
    ) empty_body @@ List.rev body in
    ExprFunc(fn, args, body_expr) |> loc_annot ($symbolstartpos, $endpos(args))
  }
  // Single line function
  | DEF fn = ID args = fn_args? EOS? body = statement END {
    let args = match args with
    | Some(list) -> list
    | None -> []
    in
    let empty_body = ExprEmptyBlock |> loc_annot $sloc in
    let body_expr = ExprBlock(body, empty_body) |> loc_annot $loc(body) in
    ExprFunc(fn, args, body_expr) |> loc_annot $sloc
  }
  ;

class_def:
  | CLASSDEF cls = const EOS END {
    let (c, nesting) = cls in
    let nesting_t = List.map (fun x -> (x, Any)) nesting in
    let const = ExprConst((c, Any), nesting_t) |> loc_annot $loc(cls) in
    let empty_body = ExprEmptyBlock |> loc_annot $sloc in
    let class_body = ExprClassBody(empty_body) |> loc_annot $sloc in
    ExprConstAssign(const, class_body) |> loc_annot ($symbolstartpos, $endpos(cls))
  }
  | CLASSDEF cls = const EOS body = nonempty_list(top_statement) END {
    let (c, nesting) = cls in
    let nesting_t = List.map (fun x -> (x, Any)) nesting in
    let const = ExprConst((c, Any), nesting_t) |> loc_annot $loc(cls) in
    let empty_body = ExprEmptyBlock |> loc_annot $sloc in
    let body_expr = List.fold_left (fun acc expr ->
      ExprBlock(expr, acc) |> loc_annot $sloc
    ) empty_body @@ List.rev body in
    let class_body = ExprClassBody(body_expr) |> loc_annot $loc(body) in
    ExprConstAssign(const, class_body) |> loc_annot ($symbolstartpos, $endpos(cls))
  }

mod_def:
  | MODDEF cls = const EOS END {
    let (c, nesting) = cls in
    let nesting_t = List.map (fun x -> (x, Any)) nesting in
    let const = ExprConst((c, Any), nesting_t) |> loc_annot $loc(cls) in
    let empty_body = ExprEmptyBlock |> loc_annot $sloc in
    let class_body = ExprModuleBody(empty_body) |> loc_annot $sloc in
    ExprConstAssign(const, class_body) |> loc_annot ($symbolstartpos, $endpos(cls))
  }
  | MODDEF cls = const EOS body = nonempty_list(top_statement) END {
    let (c, nesting) = cls in
    let nesting_t = List.map (fun x -> (x, Any)) nesting in
    let const = ExprConst((c, Any), nesting_t) |> loc_annot $loc(cls) in
    let empty_body = ExprEmptyBlock |> loc_annot $sloc in
    let body_expr = List.fold_left (fun acc expr ->
      ExprBlock(expr, acc) |> loc_annot $sloc
    ) empty_body @@ List.rev body in
    let class_body = ExprModuleBody(body_expr) |> loc_annot $loc(body) in
    ExprConstAssign(const, class_body) |> loc_annot ($symbolstartpos, $endpos(cls))
  }

primitive:
  | LBRACE obj = obj_fields RBRACE { Hash obj }
  | LBRACK vl = list_fields RBRACK { Array vl }
  | COLON s = ID                   { Symbol s }
  | s = STRING                     { String s }
  | i = INT                        { Int i }
  | x = FLOAT                      { Float x }
  | TRUE                           { Bool true }
  | FALSE                          { Bool false }
  | NIL                            { Nil }
  ;

lambda:
  | body = lambda_body {
    ExprLambda ([], body)   |> loc_annot $sloc
  }
  | args = fn_args body = lambda_body {
    ExprLambda (args, body) |> loc_annot $sloc
  }
  ;

lambda_body:
  | LAMBEG s = statement statement_end? RBRACE {
    s
  }
  | LAMBEG RBRACE {
    ExprValue(Nil) |> loc_annot $sloc
  }
  ;

fn_args:
  LPAREN p = separated_list(COMMA, ID) RPAREN {
    List.map (fun x -> (x, Any)) p
  } ;

call_args:
  LPAREN p = separated_list(COMMA, statement) RPAREN { p } ;

obj_fields:
  obj = separated_list(COMMA, obj_field)    { obj } ;

obj_field:
  k = primitive COLON v = primitive         { k, v } ;

list_fields:
  vl = separated_list(COMMA, primitive)     { vl } ;

const:
  | root = NAMESPACE? clist = separated_nonempty_list(NAMESPACE, CONST) {
    let clist = match root with
    | Some(_) -> "~ROOT~" :: clist
    | None -> clist 
    in 
    match List.rev clist with
    | [] -> $syntaxerror
    | c :: [] -> (c, [])
    | c :: ns -> let nesting = List.fold_left (fun acc const ->
      (String.concat "::" @@ List.rev (const :: acc)) :: acc
    ) [] (List.rev ns) in (c, nesting)
  }
  ;