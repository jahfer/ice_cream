open Lexing
open Lexer
open Effect.Deep

type syntax_tree = Location.t Ast.expression list
type declarations = Ast.Declarations.decl list

type t = {
  ast : syntax_tree;
  declarations : declarations;
}

let get_untyped_tree (environment : t) = environment.ast
let get_declarations (environment : t) = environment.declarations

let make () = { ast = []; declarations = []; }

let init_state () : lex_ruby_state = {
  pending_termination = false;
  at_eos = true;
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
    let open ANSITerminal in
    eprintf [white; on_red; Bold] " %s "  msg;
    eprintf [default] " %s\n"(print_position lexbuf);
    prerr_string [default] (Location.loc_as_string {
      id = 1;
      start_pos = lexbuf.lex_start_p;
      end_pos = lexbuf.lex_curr_p 
    });
    prerr_string [default] "\n"

  let parser_err lexbuf =
    let open ANSITerminal in
    let tok = Lexing.lexeme lexbuf in
    eprintf [white; on_red; Bold] " Syntax error ('%s') " tok;
    eprintf [default] " %s\n"(print_position lexbuf);
    prerr_string [default] (Location.loc_as_string {
      id = 1;
      start_pos = lexbuf.lex_start_p;
      end_pos = lexbuf.lex_curr_p 
    });
    prerr_string [default] "\n"
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

module Witness = struct
  open Filesystem
  type _ witness +=
    | Ast : Location.t Ast.expression list witness
    | Decl : Ast.Declarations.decl list witness
end

let import (file : Filesystem.file) (environment : t) : t =
  Printf.printf "Importing file:  `%s`\n" file.name;
  let open Filesystem in
  match try_with lex_file file {  
    effc = fun (type a) (e : a eff) -> match e with
    | StartLexing l -> Some (fun (k : (a, _) continuation) ->
      l.lex_curr_p <- { l.lex_curr_p with pos_fname = file.name };
      continue k ()
    )
    | LexRuby l -> Some (fun (k : (a, _) continuation) ->
      let parser = parse_buffer parse_ruby in
      continue k (LexResult (Witness.Ast, (parser l)))
    )
    | LexRBS l -> Some (fun (k : (a, _) continuation) ->
      let parser = parse_buffer parse_rbs in
      continue k (LexResult (Witness.Decl, (parser l)))
    )
    | _ -> None
  } with
  | Some LexResult (Witness.Ast, ast) -> { environment with ast }
  | Some LexResult (Witness.Decl, declarations) -> { environment with declarations }
  | None -> environment
  | _ -> failwith "Unreachable!"

let import_dir ?(filetypes=[Filesystem.Ruby; RBS]) (directory : string) (environment : t) : t =
  let open Filesystem in
  let env = ref environment in
  let read_dir dir = 
    try_with (yield_files filetypes) dir {
      effc = fun (type a) (e : a eff) -> match e with
      | YieldFile f -> Some (fun (k : (a, _) continuation) ->
        env := import f !env; continue k ()
      )
      | YieldDir d -> Some (fun (k : (a, _) continuation) ->
        let action = if String.contains d '.' then IgnoreDir else ReadDir in
        continue k action
      )
      | _ -> failwith "Unexpected effect performed"
    } in
  let () = read_dir directory in
  !env