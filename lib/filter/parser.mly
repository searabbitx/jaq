%{
open Ast
%}

%token <string> ID
%token <string> STRING
%token DOT
%token EOF
%token LPAREN
%token RPAREN
%token SELECT
%token COMMA
%token AS
%token EQ
%token NEQ
%token FILTER

%left EQ
%left NEQ
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

operator:
  | EQ { Eq }
  | NEQ { Neq }
  ;

filter:
  | e1 = expr ; o = operator ; e2 = expr { Filter (e1, o, e2) }
  ;

expr:
  | x = ID { Id x } 
  | s = STRING { String s } 
  | LPAREN ; e = expr ; RPAREN { e }
  | SELECT ; LPAREN ; s = select ; RPAREN { Select s }
  | FILTER ; LPAREN ; f = filter ; RPAREN { f }
  | e1 = expr ; DOT ; e2 = expr { Access (e1, e2) }
