open Lexing
open Lexer

type syntax_tree = Location.t Ast.expression list

type t = {
  ast : syntax_tree
}

let make () = { ast = [] }

type lex_state = Lexer.lex_state

let init_state () : lex_state = {
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

let parse_with_error lexbuf =
  try Parser.prog (Lexer.read state) lexbuf with
  | Lexer.SyntaxError msg ->
    Format.fprintf Format.std_formatter "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    let tok = Lexing.lexeme lexbuf in
    Format.fprintf Format.std_formatter "%a: Syntax error ('%s')\n" print_position lexbuf tok;
    exit (-1)

let parse lexbuf =
  (* iterate through buffer to cumulatively build untyped AST *)
  let rec build_untyped_ast lexbuf acc =
    match parse_with_error lexbuf with
    | Some (expr) ->
      build_untyped_ast lexbuf (expr :: acc)
    | None -> acc in
  let untyped_ast : 'a Ast.expression list = build_untyped_ast lexbuf [] in
  untyped_ast

let ast_for_filename (_filename : string) (environment : t) = environment.ast

(* TODO: Parse and import RBS to env *)
let import_rbs (filename : string) (environment : t) : t = 
  Printf.printf "Importing RBS file: `%s`\n" filename;
  environment

let import_rbs_dir (directory : string) (environment : t) : t =
  List.fold_left
    (fun env filename -> import_rbs filename env) environment
    (Filesystem.rbs_files_of_dir directory)

let import_rb (filename : string) (_environment : t) : t =
  let ast = Filesystem.lexbuf_of_file filename (fun lexbuf -> 
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    parse lexbuf
  ) in { ast }
