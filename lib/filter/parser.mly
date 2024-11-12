%{
open Ast
%}

%token <string> ID
%token DOT
%token EOF
%token LPAREN
%token RPAREN

%left DOT

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | x = ID { Id x } 
  | LPAREN ; e = expr ; RPAREN { e }
  | e1 = expr ; DOT ; e2 = expr { Access (e1, e2) }
