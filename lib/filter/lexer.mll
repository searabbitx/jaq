{
open Parser

let extract_string s = String.sub s 1 ((String.length s) - 2) ;;
let extract_function_name s = String.sub s 0 ((String.length s) - 2) ;;
}

let white = [' ' '\t' '\n' '\r']+
let letter = ['a'-'z' 'A'-'Z']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter alphanum*
let string = ("'" _* "'") | ("\"" _* "\"")
let int = ['0'-'9']+
let index = "[" ['0'-'9']+ "]"
let regex = "/" _* "/"
let function_call = id "(" ")"

rule read =
  parse
  | white { read lexbuf }
  | "." { DOT }
  | "," { COMMA }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "==" { EQ }
  | "!=" { NEQ }
  | ">" { GT }
  | "<" { LT }
  | ">=" { GEQ }
  | "<=" { LEQ }
  | "~" { REGMATCH }
  | "&&" { AND }
  | "||" { OR }
  | "select" { SELECT }
  | "filter" { FILTER }
  | "as" { AS }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (lexbuf |> Lexing.lexeme |> int_of_string) }
  | string { STRING (lexbuf |> Lexing.lexeme |> extract_string) }
  | function_call { FUNCTIONCALL (lexbuf |> Lexing.lexeme |> extract_function_name) }
  | regex { REGEX (lexbuf |> Lexing.lexeme |> extract_string) }
  | index { INDEX (lexbuf |> Lexing.lexeme |> extract_string |> int_of_string) }
  | eof { EOF }
