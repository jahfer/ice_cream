open Lexing
open Lexer

type syntax_tree = Location.t Ast.expression list
type declarations = Location.t Ast.declaration list

type t = {
  ast : syntax_tree;
  declarations : declarations;
}

let untyped_tree (environment : t) = environment.ast

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

module Printer = struct
  let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
    ANSITerminal.(sprintf [white] "%s:%d:%d" pos.pos_fname)
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

  let syntax_err lexbuf msg = 
    ANSITerminal.(eprintf [white; on_red; Bold] " %s "  msg);
    ANSITerminal.(eprintf [default] " %s\n"(print_position lexbuf));
    ANSITerminal.(prerr_string [default] (Location.loc_as_string {
      id = 1;
      start_pos = lexbuf.lex_start_p;
      end_pos = lexbuf.lex_curr_p 
    }));
    ANSITerminal.(prerr_string [default] "\n")

  let parser_err lexbuf =
    let tok = Lexing.lexeme lexbuf in
    ANSITerminal.(eprintf [white; on_red; Bold] " Syntax error ('%s') " tok);
    ANSITerminal.(eprintf [default] " %s\n"(print_position lexbuf));
    ANSITerminal.(prerr_string [default] (Location.loc_as_string {
      id = 1;
      start_pos = lexbuf.lex_start_p;
      end_pos = lexbuf.lex_curr_p 
    }));
    ANSITerminal.(prerr_string [default] "\n")
end

let parse_ruby lexbuf =
  try Parser.ruby (Lexer.read_ruby state) lexbuf with
  | Lexer.SyntaxError msg -> Printer.syntax_err lexbuf msg; None
  | Parser.Error -> Printer.parser_err lexbuf; None

let parse_rbs lexbuf =
  try Parser.rbs (Lexer.read_rbs state) lexbuf with
  | Lexer.SyntaxError msg -> Printer.syntax_err lexbuf msg; None
  | Parser.Error -> Printer.parser_err lexbuf; None

let parse_buffer parser lexbuf =
  (* iterate through buffer to cumulatively build untyped AST *)
  let rec build_untyped_ast lexbuf acc =
    match parser lexbuf with
    | Some (expr) ->
      build_untyped_ast lexbuf (expr :: acc)
    | None -> acc in
  build_untyped_ast lexbuf [] 

let import (file : Filesystem.file) (environment : t) : t =
  Printf.printf "Importing file:  `%s`\n" file.name;
  let lexfn = (fun parsefn lexbuf ->
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file.name }; parsefn lexbuf) in
  let ruby_parser = (lexfn (parse_buffer parse_ruby)) in
  let rbs_parser = (lexfn (parse_buffer parse_rbs)) in
  match file.filetype with
  | Ruby -> let ast = Filesystem.lexbuf_of_file file.name ruby_parser in { environment with ast }
  | RBS -> let declarations = Filesystem.lexbuf_of_file file.name rbs_parser in {environment with declarations }
  | Uunsupported -> raise (Failure "Whoops!")

let import_dir ?(filetypes=[Filesystem.Ruby; RBS]) (directory : string) (environment : t)  : t =
  List.fold_left
    (fun env filename -> import filename env) environment
    Filesystem.(files_of_dir filetypes directory)