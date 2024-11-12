open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let extract_id_assoc id l =
  l |> List.filter (fun x -> fst x = id) |> function
  | x :: _ -> snd x
  | _ -> failwith @@ "Id: " ^ id ^ " not found!"

let extract_id id = function
  | `Assoc l -> extract_id_assoc id l
  | _ -> failwith @@ "Cannot extract id: " ^ id

let rec exec_ast ast json =
  match ast with
  | Access (Id id, e) -> extract_id id json |> exec_ast e
  | Id id -> extract_id id json
  | _ -> failwith "Not implemented yet!"

let exec filter json : Yojson.Safe.t =
  let ast = parse filter in
  exec_ast ast json
