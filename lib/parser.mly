%token <int> INT
%token <float> FLOAT
%token <string> IDENTIFIER
%token <string> STRING
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token EOF
%start <char option> program
%%

program:
  | EOF { None } ;

%%
