open Lexing
open Lexer

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

let parse_buf_to_ast lexbuf =
  (* iterate through buffer to cumulatively build untyped AST *)
  let rec build_untyped_ast lexbuf acc =
    match parse_with_error lexbuf with
    | Some (expr) ->
      build_untyped_ast lexbuf (expr :: acc)
    | None -> acc in
  let untyped_ast : 'a Ast.expression list = build_untyped_ast lexbuf [] in
  untyped_ast

let string_from_ast untyped_ast = untyped_ast
|> List.rev
|> List.map (fun ast -> Printf.sprintf "%s\n" (Ast.AstPrinter.print_cexpr ast))
|> String.concat ""

let parse_from_filename filename =
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let ast = parse_buf_to_ast lexbuf in
  let str = string_from_ast ast in
  output_string stdout str;

  print_endline "";

  (* Generate index *)
  let index = Ast_index.create ast in
  index
  |> Query.query_all ~flatten:true ~f:(fun node ->
    (Node.node_type node) = "RefNode"
  )
  |> List.iter (fun node ->
    print_endline @@ "# Original code:";
    print_endline @@ Location.loc_as_string (Node.location node);
    print_endline @@ "# RBS:";
    print_endline @@ Node.to_rbs node;
    print_endline @@ "\n# AST node:";
    print_endline @@ Node.pretty_print node;
    print_endline "\n==========================\n";
  );
  
  (* let scope_as_string (scope) = 
    scope
    |> List.rev_map (fun x -> match x with Ast.Index.Root -> "" | Ast.Index.Const x -> x)
    |> String.concat "::" in

  (* print index data *)
  let index = Ast.Index.create ast in

  print_endline "";

  print_endline "CONST ASSIGNMENTS";
  index.node_list
  |> List.assoc Ast.Index.KConstAssign
  |> (fun x -> x.contents)
  |> Ast.Index.NodeSet.iter (fun node ->
    print_endline @@ Ast.Index.Node.Reader.read node
  );
  print_endline "";

  print_endline "LOCAL VARIABLE ASSIGNMENTS";
  index.node_list
  |> List.assoc Ast.Index.KAssign
  |> (fun x -> x.contents)
  |> Ast.Index.NodeSet.iter (fun node ->
    print_string "\nScope: ";
    print_endline @@ scope_as_string node.scope;
    Location.print_loc node.location
  );
  print_endline "";

  print_endline "INSTANCE VARIABLE ASSIGNMENTS";
  index.node_list
  |> List.assoc Ast.Index.KIVarAssign
  |> (fun x -> x.contents)
  |> Ast.Index.NodeSet.iter (fun node ->
    print_string "\nScope: ";
    print_endline @@ scope_as_string node.scope;
    Location.print_loc node.location
  );
  print_endline "";

  print_endline "FUNCTION DEFINITIONS";
  index.node_list
  |> List.assoc Ast.Index.KFunc
  |> (fun x -> x.contents)
  |> Ast.Index.NodeSet.iter (fun node ->
    print_string "\nScope: ";
    print_endline @@ scope_as_string node.scope;
    Location.print_loc node.location
  ); *)

  close_in inx

let parse_from_string ?filename:(filename : string = "?") str =
  let lexbuf = Lexing.from_string str in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_buf_to_ast lexbuf
