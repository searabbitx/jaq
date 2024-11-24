{
open Parser

let extract_string s = String.sub s 1 ((String.length s) - 2) ;;
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter alphanum*
let string = ("'" _* "'") | ("\"" _* "\"")

rule read =
  parse
  | white { read lexbuf }
  | "." { DOT }
  | "," { COMMA }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "==" { EQ }
  | "!=" { NEQ }
  | "select" { SELECT }
  | "filter" { FILTER }
  | "as" { AS }
  | id { ID (Lexing.lexeme lexbuf) }
  | string { STRING (lexbuf |> Lexing.lexeme |> extract_string) }
  | eof { EOF }
