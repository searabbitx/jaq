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
%token AS

%left AS
%left COMMA
%left DOT

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

select:
  | e = expr { SElement (e, SEmpty) } 
  | e = expr ; AS ; x = ID { SElement (Aliased (e, Id x), SEmpty) }
  | e = expr ; AS ; x = ID ; COMMA ; s = select { SElement (Aliased (e, Id x), s) }
  | e = expr ; COMMA ; s = select { SElement (e, s) } 
  ;

expr:
  | x = ID { Id x } 
  | LPAREN ; e = expr ; RPAREN { e }
  | SELECT ; LPAREN ; s = select ; RPAREN { Select s }
  | e1 = expr ; DOT ; e2 = expr { Access (e1, e2) }
