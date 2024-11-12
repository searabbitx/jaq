%{
open Ast
%}

%token <string> ID
%token DOT
%token EOF
%token LPAREN
%token RPAREN
%token SELECT
%token COMMA

%left COMMA
%left DOT

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

select:
  | e = expr { SElement (e, SEmpty) } 
  | e = expr ; COMMA ; s = select { SElement (e, s) } 
  ;

expr:
  | x = ID { Id x } 
  | LPAREN ; e = expr ; RPAREN { e }
  | SELECT ; LPAREN ; s = select ; RPAREN { Select s }
  | e1 = expr ; DOT ; e2 = expr { Access (e1, e2) }
