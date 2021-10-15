%token NAMESPACE
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token TRUE FALSE NIL
%token LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN LAMBEG
%token COLON COMMA HASHROCKET
%token EOS EOF
%token SELF
%token LESS LSHIFT
%token <string> ID METHOD IVAR ARR_IDX
%token <string> CONST
%token EQ DEF END LAMBDA DOT CLASSDEF MODDEF PIPE
%token DO 

// RBS-specific
%token DECL_ARR

%{
  open Ast
  open Location

  type pos = Lexing.position * Lexing.position

  let loc_annot_expr
    ((start_pos, end_pos) : Lexing.position * Lexing.position)
    (expr : Location.t Ast.expr) =
      (expr, { id = Location.gen_id (); start_pos; end_pos; })

  let loc_annot_decl : type a. pos -> a Ast.Declarations.t -> Ast.Declarations.decl =
  fun (start_pos, end_pos) decl ->
      Ast.Declarations.Decl (decl, { id = Location.gen_id (); start_pos; end_pos; })
%}

%start <Location.t Ast.expression option> ruby
%start <Ast.Declarations.decl option> rbs

%%

// Ruby Parser

ruby:
  | s = top_statement { Some s }
  | EOF               { None   }
  ;

top_statement:
  | s = stmt top_statement_end { s }
  %inline stmt:
  | s = statement { s }
  // this needs to be here so we don't swallow the EOS
  // TODO I don't remember why this exists...
  | m = ID args = separated_nonempty_list(COMMA, statement) {
    let sub_expr = ExprValue(Nil) |> loc_annot_expr $sloc in
    ExprCall(sub_expr, m, (args, None)) |> loc_annot_expr $sloc
  }
  ;

statement_end:
  EOS { };

top_statement_end:
  statement_end | EOF { };

statement:
  | ref = ID             {
    ExprVar((ref, Any)) |> loc_annot_expr $sloc
  }
  | ref = iv_identifier          {
    ExprIVar(ref) |> loc_annot_expr $sloc
  }
  | id = ID EQ v = rhs_assign    {
    ExprAssign(id, v) |> loc_annot_expr $loc(id)
  }
  | id = IVAR EQ v = rhs_assign  {
    ExprIVarAssign(id, v) |> loc_annot_expr $loc(id)
  }
  | cls = const EQ v = rhs_assign  {
    let (_, loc) = cls in
    ExprConstAssign(cls, None, v) |> loc_annot_expr (loc.start_pos, loc.end_pos)
  }
  | p = primitive                {
    ExprValue(p) |> loc_annot_expr $sloc
  }
  | c = class_def                { c }
  | m = mod_def                  { m }
  | e = expr                     { e }
  | c = const                    { c }
  ;

rhs_assign:
  | s = statement { s }
  ;

expr:
  | c = command_call  { c }
  | LAMBDA l = lambda { l }
  | f = func          { f }
  ;

block:
  | DO EOS? body = block_body END {
    ExprProc ([], body) |> loc_annot_expr $sloc
  }
  | DO args = block_args EOS? body = block_body END {
    ExprProc (args, body) |> loc_annot_expr $sloc
  }
  %inline block_body:
  | s = statement statement_end? { s }
  ;

command_call:
  c = command { c } ;

command:
  | m = method_call args = command_args {
    let sub_expr = ExprValue(Nil) |> loc_annot_expr $sloc in
    ExprCall(sub_expr, m, args)   |> loc_annot_expr $sloc
  }
  | m = ID args = command_args {
    let sub_expr = ExprValue(Nil) |> loc_annot_expr $sloc in
    ExprCall(sub_expr, m, args)   |> loc_annot_expr $sloc
  }
  | c1 = ID call_op c2 = method_call {
    let sub_expr = ExprVar((c1, Any)) |> loc_annot_expr $loc(c1) in
    ExprCall(sub_expr, c2, ([], None)) |> loc_annot_expr $sloc
  }
  | cst = const call_op c2 = method_call {
    ExprCall(cst, c2, ([], None)) |> loc_annot_expr $sloc
  }
  | c1 = ID call_op c2 = method_call args = command_args {
    let sub_expr = ExprVar((c1, Any)) |> loc_annot_expr $loc(c1) in
    ExprCall(sub_expr, c2, args) |> loc_annot_expr $sloc
  }
  | c1 = IVAR call_op c2 = method_call args = command_args {
    let sub_expr = ExprVar((c1, Any)) |> loc_annot_expr $loc(c1) in
    ExprCall(sub_expr, c2, args) |> loc_annot_expr $sloc
  }
  | cst = const call_op c2 = method_call args = command_args {
    ExprCall(cst, c2, args) |> loc_annot_expr $sloc
  }
  | c1 = ARR_IDX idx = statement RBRACK a = asgn? {
    let (meth, args) = match a with
    | Some v -> "[]=", (idx :: [v])
    | None -> "[]", [idx] in
    let sub_expr = ExprVar((c1, Any)) |> loc_annot_expr $loc(c1) in
    ExprCall(sub_expr, meth, (args, None)) |> loc_annot_expr $sloc
  }
  %inline asgn:
  | a = preceded(EQ, statement) { a }
  ;

call_op: DOT { } ;

method_call: id = METHOD eq = EQ? {
  match eq with
  | Some _ -> id ^ "="
  | None -> id
};

command_args:
  args = call_args { args } ;

block_args:
  PIPE p = separated_list(COMMA, ID) PIPE { 
    List.map (fun x -> (x, None, Ast.Positional)) p
  }

iv_identifier:
  iv = IVAR { iv, Any } ;

func:
  | DEF fn = method_name args = fn_args? EOS? END {
    let args = match args with
    | Some(a) -> a
    | None -> []
    in
    let body = ExprValue(Nil) |> loc_annot_expr $sloc in
    ExprFunc(fn, args, body) |> loc_annot_expr ($symbolstartpos, $endpos(args))
  }
  // Multi-line function
  | DEF fn = method_name args = fn_args? EOS? body = nonempty_list(top_statement) END {
    let args = match args with
    | Some(a) -> a
    | None -> []
    in
    let empty_body = ExprEmptyBlock |> loc_annot_expr $sloc in
    let body_expr = List.fold_left (fun acc expr ->
      ExprBlock(expr, acc) |> loc_annot_expr $sloc
    ) empty_body @@ List.rev body in
    ExprFunc(fn, args, body_expr) |> loc_annot_expr ($symbolstartpos, $endpos(args))
  }
  // Single line function
  | DEF fn = method_name args = fn_args? EOS? body = statement END {
    let args = match args with
    | Some(a) -> a
    | None -> []
    in
    let empty_body = ExprEmptyBlock |> loc_annot_expr $sloc in
    let body_expr = ExprBlock(body, empty_body) |> loc_annot_expr $loc(body) in
    ExprFunc(fn, args, body_expr) |> loc_annot_expr ($symbolstartpos, $endpos(args))
  }
  %inline method_name:
  | name = ID { name }
  | SELF DOT name = METHOD { "self." ^ name }
  ;

class_def:
  // class << self
  | CLASSDEF LSHIFT SELF EOS END {
    let const = ExprConst(("<<EIGENCLASS>>", Any), []) |> loc_annot_expr $sloc in
    let empty_body = ExprEmptyBlock |> loc_annot_expr $sloc in
    let class_body = ExprClassBody(empty_body) |> loc_annot_expr $sloc in
    ExprConstAssign(const, None, class_body) |> loc_annot_expr $sloc
  }
  | _c = CLASSDEF LSHIFT _s = SELF EOS class_body = body END {
    let const = ExprConst(("<<EIGENCLASS>>", Any), []) |> loc_annot_expr $sloc in
    ExprConstAssign(const, None, class_body) |> loc_annot_expr ($startpos(_c), $endpos(_s))
  }
  // class x < y
  | CLASSDEF cls = const parent = class_inherit? EOS END {
    let empty_body = ExprEmptyBlock |> loc_annot_expr $sloc in
    let class_body = ExprClassBody(empty_body) |> loc_annot_expr $sloc in
    let (_, loc) = cls in
    ExprConstAssign(cls, parent, class_body) |> loc_annot_expr ($symbolstartpos, loc.end_pos)
  }
  | CLASSDEF cls = const parent = class_inherit? EOS class_body = body END {
    let (_, loc) = cls in
    ExprConstAssign(cls, parent, class_body) |> loc_annot_expr ($symbolstartpos, loc.end_pos)
  }
  %inline class_inherit:
  | LESS c = const { c }
  body:
  | body = nonempty_list(top_statement) {
    let empty_body = ExprEmptyBlock |> loc_annot_expr $sloc in
    let body_expr = List.fold_left (fun acc expr ->
      ExprBlock(expr, acc) |> loc_annot_expr $sloc
    ) empty_body @@ List.rev body in
    ExprClassBody(body_expr) |> loc_annot_expr $loc(body)
  }


mod_def:
  | MODDEF cls = const EOS END {
    let empty_body = ExprEmptyBlock |> loc_annot_expr $sloc in
    let class_body = ExprModuleBody(empty_body) |> loc_annot_expr $sloc in
    let (_, loc) = cls in
    ExprConstAssign(cls, None, class_body) |> loc_annot_expr ($symbolstartpos, loc.end_pos)
  }
  | MODDEF cls = const EOS body = nonempty_list(top_statement) END {
    let empty_body = ExprEmptyBlock |> loc_annot_expr $sloc in
    let body_expr = List.fold_left (fun acc expr ->
      ExprBlock(expr, acc) |> loc_annot_expr $sloc
    ) empty_body @@ List.rev body in
    let class_body = ExprModuleBody(body_expr) |> loc_annot_expr $loc(body) in
    let (_, loc) = cls in
    ExprConstAssign(cls, None, class_body) |> loc_annot_expr ($symbolstartpos, loc.end_pos)
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
    let lambda = Lambda (([]), body) in
    ExprValue(lambda) |> loc_annot_expr $sloc
  }
  | args = fn_args body = lambda_body {
    let lambda = Lambda (args, body) in
    ExprValue(lambda) |> loc_annot_expr $sloc
  }
  %inline lambda_body:
  // TODO: Does this need to be a list of statements?
  | LAMBEG s = statement statement_end? RBRACE {
    s
  }
  | LAMBEG RBRACE {
    ExprValue(Nil) |> loc_annot_expr $sloc
  }
  ;

fn_args:
  | params = delimited(LPAREN, separated_list(COMMA, param), RPAREN) {
    params
  }
  %inline param:
  | id = ID {
    (id, None, Positional)
  }
  | id = ID EQ v = default_value { 
    (id, Some v, Positional)
  }
  | id = ID COLON v = default_value? { (id, v, Keyword) }
  %inline default_value:
  | p = primitive { ExprValue p }
  | c = const { let (constant, _loc) = c in constant }
  | iv = iv_identifier { ExprIVar iv }
  | id = ID {
    let caller = ExprValue(Nil) |> loc_annot_expr $sloc in
    ExprCall (caller, id, ([], None))
  }
  ;

call_args:
  // TODO: ambiguous whether block belongs to interior or exterior statement
  | LPAREN p = separated_list(COMMA, statement) RPAREN b = block? { p, b }
  | b = block { [], Some(b) }
  ;

obj_fields:
  obj = separated_list(COMMA, obj_field)   { obj } ;

obj_field:
  | k = primitive COLON v = primitive         { k, v }
  | k = primitive HASHROCKET v = primitive    { k, v }
  | k = ID COLON v = primitive                { (Symbol k), v };

list_fields:
  | vl = separated_list(COMMA, statement)     { vl } ;

const:
  | root = NAMESPACE? clist = separated_nonempty_list(NAMESPACE, CONST) {
    let clist = match root with
    | Some(_) -> "<ROOT>" :: clist
    | None -> clist 
    in 
    match List.rev clist with
    | [] -> $syntaxerror
    | const :: nesting -> 
      let nesting_t = List.map (fun x -> (x, Any)) nesting in
      ExprConst((const, Any), nesting_t) |> loc_annot_expr $sloc
  }
  ;

// RBS Parser

rbs:
  | d = top_decl { Some d }
  | EOF          { None   }
  ;

top_decl:
  | s = decl top_decl_end { s }
  ;

decl_end:
  EOS { };

top_decl_end:
  decl_end | EOF { };

decl:
| CLASSDEF typ = rb_class_type type_vars = module_type_parameters? EOS decls = top_decl* END {
  Declarations.Class((typ, type_vars, decls)) |> loc_annot_decl $sloc
}
| MODDEF typ = rb_class_type type_vars = module_type_parameters? EOS decls = top_decl* END {
  Declarations.Module((typ, type_vars, decls)) |> loc_annot_decl $sloc
}
| DEF id = ID COLON args = delimited(LPAREN, separated_list(COMMA, rb_class_type), RPAREN) DECL_ARR r = ID {
  let open Declarations in
  let typed_args = List.map (fun x -> Type x) args in
  Method(id, typed_args, Type r) |> loc_annot_decl $sloc
}
;

module_type_parameters:
| tlist = delimited(LBRACK, separated_nonempty_list(COMMA, module_type_parameter), RBRACK) {
  tlist
}
%inline module_type_parameter:
| t = type_variable {
  Declarations.Type t
}
%inline type_variable:
| c = rb_class_type { c }
;

rb_class_type:
| c = CONST { c }
| c = ID { c }
