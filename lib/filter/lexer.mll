{
open Parser
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter alphanum*

rule read =
  parse
  | white { read lexbuf }
  | "." { DOT }
  | "," { COMMA }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "select" { SELECT }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
