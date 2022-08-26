%{
open Expr      
%}

%token <int> INT
%token <float> FLOAT
%token <string> IDENTIFIER
%token <string> STRING
%token LAMBDA
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token EOF
%start <expr option> program
%%

program:
  | e = expr { Some e }
  | EOF      { None   } ;

expr:
  | i = INT           { Int i } 
  | f = FLOAT         { Float f }
  | x = IDENTIFIER    { Var x }
  | s = STRING        { String s }
  | TRUE              { Bool true }
  | FALSE             { Bool false }
  | LPAREN ;
    es = list(expr) ; 
    RPAREN            { Apply es }

%%
