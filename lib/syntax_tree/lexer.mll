{
  open Parser
  open Printf

  exception SyntaxError of string

  type lex_ruby_state = {
    (* newline-agnostic unterminated statement *)
    mutable pending_termination : bool;
    mutable at_eos : bool;
    mutable paren_level : int;
    mutable lambda_stack : int list;
    mutable fn_call : bool;
  }

  let newline_agnostic_tok state =
    state.pending_termination <- true;
    state.at_eos <- false
  and terminating_tok state =
    state.pending_termination <- false;
    state.at_eos <- false
  and ack_tok state =
    state.at_eos <- false

  let print_state state = printf {str|{
  pending_termination: %B,
  at_eos: %B,
  paren_level: %i,
  fn_call: %B
} |str} state.pending_termination state.at_eos state.paren_level state.fn_call;
    printf "\n"
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id =  ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* '?'?
let ivar = ['@'] id
let const = ['A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule read_ruby state = parse
  | white    { read_ruby state lexbuf }
  | newline  {
    Lexing.new_line lexbuf;
    if state.pending_termination || state.at_eos then
      read_ruby state lexbuf
    else begin
      state.at_eos <- true; EOS
    end
  }
  | "def"    { newline_agnostic_tok state; DEF }
  | "do"     { newline_agnostic_tok state; DO }
  | "class"  { ack_tok state; CLASSDEF }
  | "module" { ack_tok state; MODDEF }
  | "->"     {
    ack_tok state;
    state.lambda_stack <- (state.paren_level :: state.lambda_stack);
    LAMBDA
  }
  | '.'      {
    state.fn_call <- true; ack_tok state; DOT
  }
  | '"'      { ack_tok state; read_string (Buffer.create 17) lexbuf }
  | "::"     { ack_tok state; NAMESPACE }
  | "<<"     { ack_tok state; LSHIFT }
  | "=>"     { ack_tok state; HASHROCKET }
  | ':'      { ack_tok state; COLON }
  | ','      { ack_tok state; COMMA }
  | '|'      { ack_tok state; PIPE }
  | '<'      { ack_tok state; LESS }
  | ';'      { ack_tok state; EOS }
  | '='      { ack_tok state; EQ }
  (* | '>'      { ack_tok state; GREATER } *)
  | '{'      { newline_agnostic_tok state;
    match state.lambda_stack with
    | el :: tl when el = state.paren_level ->
      state.lambda_stack <- tl; LAMBEG
    | _ -> LBRACE
  }
  | '['      { newline_agnostic_tok state; LBRACK }
  | '('      { newline_agnostic_tok state; LPAREN }
  | '}'      { terminating_tok state;      RBRACE }
  | ']'      { terminating_tok state;      RBRACK }
  | ')'      { terminating_tok state;      RPAREN }
  | '#'      { comment state lexbuf }
  | int      { ack_tok state; INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { ack_tok state; FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "true"   { ack_tok state; TRUE }
  | "false"  { ack_tok state; FALSE }
  | "nil"    { ack_tok state; NIL }
  | "end"    { terminating_tok state; END }
  | "self"   { ack_tok state; SELF }
  | const    { terminating_tok state; CONST (Lexing.lexeme lexbuf) }
  | ivar     { terminating_tok state; IVAR (Lexing.lexeme lexbuf) }
  | id as i '[' {
    newline_agnostic_tok state;
    ARR_IDX (i)
  }
  | id       {
    terminating_tok state;
    if state.fn_call then begin
      state.fn_call <- false;
      METHOD (Lexing.lexeme lexbuf)
    end else begin
      ID (Lexing.lexeme lexbuf)
    end
  }
  | _        { raise (SyntaxError (sprintf "Unexpected char: '%s'" (Lexing.lexeme lexbuf))) }
  | eof      { EOF }

and comment state = parse
| newline {
  Lexing.new_line lexbuf;
  if state.pending_termination || state.at_eos then
    read_ruby state lexbuf
  else begin
    state.at_eos <- true; EOS
  end
}
| _ { comment state lexbuf }

and read_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\'' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

and read_rbs state = parse
  | white    { read_rbs state lexbuf }
  | newline  {
    Lexing.new_line lexbuf;
    if state.pending_termination || state.at_eos then
      read_rbs state lexbuf
    else begin
      state.at_eos <- true; EOS
    end
  }
  | "def"    { newline_agnostic_tok state; DEF }
  | "class"  { ack_tok state; CLASSDEF }
  | "module" { ack_tok state; MODDEF }
  | "end"    { terminating_tok state; END }
  | "->"     { ack_tok state; DECL_ARR }
  | ':'      { ack_tok state; COLON }
  | '['      { newline_agnostic_tok state; LBRACK }
  | '('      { newline_agnostic_tok state; LPAREN }
  | ']'      { terminating_tok state;      RBRACK }
  | ')'      { terminating_tok state;      RPAREN }
  | '#'      { comment state lexbuf }
  | const    { terminating_tok state; CONST (Lexing.lexeme lexbuf) }
  | id       { terminating_tok state; ID (Lexing.lexeme lexbuf) }
  | _        { raise (SyntaxError (sprintf "Unexpected char: '%s'" (Lexing.lexeme lexbuf))) }
  | eof      { EOF }