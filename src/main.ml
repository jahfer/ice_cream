open Lexing
open Syntax_tree.Lexer

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
  try Syntax_tree.Parser.prog (Syntax_tree.Lexer.read state) lexbuf with
  | Syntax_tree.Lexer.SyntaxError msg ->
    Format.fprintf Format.std_formatter "%a: %s\n" print_position lexbuf msg;
    None
  | Syntax_tree.Parser.Error ->
    let tok = Lexing.lexeme lexbuf in
    Format.fprintf Format.std_formatter "%a: syntax error ('%s')\n" print_position lexbuf tok;
    exit (-1)

let parse_buf_to_ast lexbuf =
  (* iterate through buffer to cumulatively build untyped AST *)
  let rec build_untyped_ast lexbuf acc =
    match parse_with_error lexbuf with
    | Some (expr) ->
      build_untyped_ast lexbuf (expr :: acc)
    | None -> acc
  in

  let untyped_ast : 'a Syntax_tree.Ast.expression list = build_untyped_ast lexbuf [] in
  untyped_ast
  |> List.rev
  |> List.iter (fun ast -> Printf.printf "%a\n" Syntax_tree.Ast.AstPrinter.print_cexpr ast)

let parse_from_filename filename =
  let inx = Core.In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_buf_to_ast lexbuf;
  Core.In_channel.close inx;
