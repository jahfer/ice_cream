open Lexing
open Lexer

type syntax_tree = Location.t Ast.expression list
type declarations = Location.t Ast.declaration list

type t = {
  ast : syntax_tree;
  declarations : declarations;
}

let make () = { ast = []; declarations = []; }

type lex_state = Lexer.lex_ruby_state

let init_state () : lex_ruby_state = {
  pending_termination = false;
  at_eos = false;
  paren_level = 0;
  lambda_stack = [];
  fn_call = false;
}

let state = init_state ()

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Format.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_ruby lexbuf =
  try Parser.ruby (Lexer.read_ruby state) lexbuf with
  | Lexer.SyntaxError msg ->
    Format.fprintf Format.std_formatter "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    let tok = Lexing.lexeme lexbuf in
    Format.fprintf Format.std_formatter "%a: Syntax error ('%s')\n" print_position lexbuf tok;
    None

let parse_rbs lexbuf =
  try Parser.rbs (Lexer.read_rbs state) lexbuf with
  | Lexer.SyntaxError msg ->
    Format.fprintf Format.std_formatter "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    let tok = Lexing.lexeme lexbuf in
    Format.fprintf Format.std_formatter "%a: Syntax error ('%s')\n" print_position lexbuf tok;
      None

let parse_ruby_buffer lexbuf =
  (* iterate through buffer to cumulatively build untyped AST *)
  let rec build_untyped_ast lexbuf acc =
    match parse_ruby lexbuf with
    | Some (expr) ->
      build_untyped_ast lexbuf (expr :: acc)
    | None -> acc in
  build_untyped_ast lexbuf [] 

let parse_rbs_buffer lexbuf =
  (* iterate through buffer to cumulatively build untyped AST *)
  let rec build_untyped_ast lexbuf acc =
    match parse_rbs lexbuf with
    | Some (expr) ->
      build_untyped_ast lexbuf (expr :: acc)
    | None -> acc in
  build_untyped_ast lexbuf [] 
  

let untyped_tree (environment : t) = environment.ast

(* TODO: Parse and import RBS to env *)
let import_rbs (filename : string) (environment : t) : t = 
  Printf.printf "Importing RBS file:  `%s`\n" filename;
  let declarations = Filesystem.lexbuf_of_file filename (fun lexbuf -> 
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    parse_rbs_buffer lexbuf
  ) in { environment with declarations }

let import_rbs_dir (directory : string) (environment : t) : t =
  List.fold_left
    (fun env filename -> import_rbs filename env) environment
    Filesystem.(files_of_dir RBS directory)

let import_rb (filename : string) (environment : t) : t =
  Printf.printf "Importing Ruby file: `%s`\n" filename;
  let ast = Filesystem.lexbuf_of_file filename (fun lexbuf -> 
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    parse_ruby_buffer lexbuf
  ) in { environment with ast }

let import_rb_dir (directory : string) (environment : t) : t =
  List.fold_left
    (fun env filename -> import_rb filename env) environment
    Filesystem.(files_of_dir Ruby directory)
