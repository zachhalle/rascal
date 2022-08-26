
(* The type of tokens. *)

type token = 
  | TRUE
  | STRING of (string)
  | RPAREN
  | LPAREN
  | LAMBDA
  | INT of (int)
  | IDENTIFIER of (string)
  | FLOAT of (float)
  | FALSE
  | EOF
  | DEFINE

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (stmt list)
