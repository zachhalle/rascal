open Lexer
open Lexing
open Printf

let print_pos out lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf out "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let parse_with_error lexbuf =
  try Some (Parser.program Lexer.token lexbuf) with
  | SyntaxError msg -> fprintf stderr "%a: %s\n" print_pos lexbuf msg; None
  | Parser.Error -> fprintf stderr "%a: syntax error\n" print_pos lexbuf; None

let parse_stdin () =
  let lexbuf = Lexing.from_channel stdin in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "(stdin)" };
  let ast = parse_with_error lexbuf in
  ast

let parse_string str =
  let lexbuf = Lexing.from_string str in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "(string)" };
  let ast = parse_with_error lexbuf in
  ast

let parse_file filename =
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let ast = parse_with_error lexbuf in
  close_in inx;
  ast
