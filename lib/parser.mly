%{
open Ast      
%}

%token <int> INT
%token <float> FLOAT
%token <string> IDENTIFIER
%token <string> STRING
%token DEFINE
%token LAMBDA
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token QUOTE
%token EOF
%start <stmt list> program
%type <stmt> stmt
%type <stmt list> list(stmt) 
%type <expr> expr
%type <expr list> list(expr)
%%

program:
  | ss = list(stmt); EOF { ss }

stmt:
  | LPAREN ;
    DEFINE ;
    v = IDENTIFIER ;
    e = expr ;
    RPAREN           { Define (v, e) }
  | e = expr         { Expr e }

expr:
  | i = INT           { Int i } 
  | f = FLOAT         { Float f }
  | x = IDENTIFIER    { Var x }
  | s = STRING        { String s }
  | QUOTE ; e = expr  { Quote e }
  | TRUE              { Bool true }
  | FALSE             { Bool false }
  | LPAREN ;
    LAMBDA ;
    v = IDENTIFIER ;
    e = expr ;
    RPAREN            { Lambda (v, e) }
  | LPAREN ;
    es = list(expr) ; 
    RPAREN            { List es }

%%
