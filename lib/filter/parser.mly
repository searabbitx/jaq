%{
open Ast
%}

%token <string> ID
%token <string> STRING
%token <string> REGEX
%token <int> INDEX
%token <int> INT
%token DOT
%token EOF
%token LPAREN
%token RPAREN
%token SELECT
%token COMMA
%token AS
%token EQ
%token NEQ
%token GT
%token GEQ
%token REGMATCH
%token LT
%token LEQ
%token AND
%token OR
%token FILTER

%left EQ
%left GT
%left LT
%left GEQ
%left LEQ
%left NEQ
%left AND
%left OR
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
  | GT { Gt }
  | LT { Lt }
  | GEQ { Geq }
  | LEQ { Leq }
  | REGMATCH { RegMatch }
  ;

logic_operator:
  | AND { And }
  | OR { Or }
  ;

filter:
  | e1 = expr ; o = operator ; e2 = expr { Op (e1, o, e2) }
  | e1 = filter ; o = logic_operator ; e2 = filter { LogicOp (e1, o, e2) }
  ;

expr:
  | x = ID { Id x } 
  | s = STRING { String s } 
  | r = REGEX { Regex (Re.compile (Re.Posix.re r)) } 
  | i = INT { Int i }
  | i = INDEX { Index i }
  | LPAREN ; e = expr ; RPAREN { e }
  | SELECT ; LPAREN ; s = select ; RPAREN { Select s }
  | FILTER ; LPAREN ; f = filter ; RPAREN { Filter f }
  | e1 = expr ; DOT ; e2 = expr { Access (e1, e2) }
  | e1 = expr ; i = INDEX { Access(e1, Index i) }
