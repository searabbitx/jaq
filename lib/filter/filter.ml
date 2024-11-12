open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let extract_id_assoc id l =
  l |> List.filter (fun x -> fst x = id) |> function
  | x :: _ -> snd x
  | _ -> failwith @@ "Id: " ^ id ^ " not found!"

let rec extract_id id = function
  | `Assoc l -> extract_id_assoc id l
  | `List l -> `List (List.map (extract_id id) l)
  | _ -> failwith @@ "Cannot extract id: " ^ id

let rec exec_ast ast json =
  match ast with
  | Access (e1, e2) -> exec_ast e1 json |> exec_ast e2
  | Id id -> extract_id id json

let exec filter json : Yojson.Safe.t =
  let ast = parse filter in
  exec_ast ast json
