{
open Lexing
open Parser
open Buffer

exception SyntaxError of string
}

let digit = ['0' - '9']
let int   = '-'? digit digit*
let frac  = '.' digit*
let float = digit* frac?

let leading_char  = ['a'-'z' 'A'-'Z' '+' '-' '*' '/' '_' '!' '?']
let trailing_char = ['a'-'z' 'A'-'Z' '0'-'9' '+' '-' '*' '/' '_' '!' '?']
let identifier    = leading_char trailing_char*

let whitespace = [' ' '\t']+
let newline    = '\r' | '\n' | "\r\n"

let lparen = '('
let rparen = ')'

rule token =
  parse 
  | whitespace { token lexbuf }
  | newline    { new_line lexbuf; token lexbuf }
  | int        { INT (int_of_string (lexeme lexbuf)) } 
  | float      { FLOAT (float_of_string (lexeme lexbuf)) }
  | "true"     { TRUE }
  | "false"    { FALSE }
  | "define"   { DEFINE }
  | "lambda"   { LAMBDA }
  | identifier { IDENTIFIER (lexeme lexbuf) }
  | '"'        { read_string (Buffer.create 10) lexbuf }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | _          { raise (SyntaxError ("Unexpected character: " ^ lexeme lexbuf)) }
  | eof        { EOF }

and read_string buffer =
  parse
  | '"'           { STRING (contents buffer) }
  | '\\' '/'      { add_char buffer '/'    ; read_string buffer lexbuf }
  | '\\' '\\'     { add_char buffer '\\'   ; read_string buffer lexbuf }
  | '\\' 'b'      { add_char buffer '\b'   ; read_string buffer lexbuf }
  | '\\' 'f'      { add_char buffer '\012' ; read_string buffer lexbuf }
  | '\\' 'n'      { add_char buffer '\n'   ; read_string buffer lexbuf }
  | '\\' 'r'      { add_char buffer 'r'    ; read_string buffer lexbuf }
  | '\\' 't'      { add_char buffer '\t'   ; read_string buffer lexbuf }
  | [^ '"' '\\']+ { add_string buffer (lexeme lexbuf) ; read_string buffer lexbuf }
  | _             { raise (SyntaxError ("Illegal character in string: " ^ lexeme lexbuf)) }
  | eof           { raise (SyntaxError ("Unexpected end of file while lexing string"))}
