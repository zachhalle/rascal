%{
open Ast      
%}

%token <int> INT
%token <float> FLOAT
%token <string> IDENTIFIER
%token <string> STRING
%token LET
%token LET_REC
%token IF
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
    LET ;
    LPAREN ;
    bindings = list(binding)
    RPAREN ;
    e = expr
    RPAREN            { Let (bindings, e) } 
  | LPAREN ;
    LET_REC ;
    LPAREN ;
    LPAREN ;
    v = IDENTIFIER ;
    e1 = expr ;
    RPAREN ;
    RPAREN ;
    e2 = expr ;
    RPAREN            { Let_rec ((v, e1), e2) } 
  | LPAREN ;
    IF ;
    e1 = expr ;
    e2 = expr ;
    e3 = expr ;       
    RPAREN            { If (e1, e2, e3) }
  | LPAREN ;
    LAMBDA ;
    LPAREN ;
    vs = list(IDENTIFIER) ;
    RPAREN ;
    e = expr ;
    RPAREN            { Lambda (vs, e) }
  | LPAREN ;
    es = list(expr) ; 
    RPAREN            { List es }

binding:
  | LPAREN ;
    v = IDENTIFIER
    e = expr ;
    RPAREN          { (v, e) }

%%
