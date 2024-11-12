open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let list_contains list needle =
  match List.find_opt (fun x -> x = needle) list with
  | Some _ -> true
  | None -> false

let extract_id_assoc id l =
  l |> List.filter (fun x -> fst x = id) |> function
  | x :: _ -> snd x
  | _ -> failwith @@ "Id: " ^ id ^ " not found!"

let rec extract_id id = function
  | `Assoc l -> extract_id_assoc id l
  | `List l -> `List (List.map (extract_id id) l)
  | _ -> failwith @@ "Cannot extract id: " ^ id

let rec select_list_to_id_list = function
  | SElement (Id x, rest) -> x :: select_list_to_id_list rest
  | SEmpty -> []
  | _ -> failwith "Select accepts a list of ids only!"

let rec select s = function
  | `Assoc l ->
      let ids = select_list_to_id_list s in
      `Assoc (List.filter (fun x -> list_contains ids (fst x)) l)
  | `List l -> `List (List.map (select s) l)
  | _ -> failwith "Cannot select from non object"

let rec exec_ast ast json =
  match ast with
  | Access (e1, e2) -> exec_ast e1 json |> exec_ast e2
  | Id id -> extract_id id json
  | Select s -> select s json

let exec filter json : Yojson.Safe.t =
  let ast = parse filter in
  exec_ast ast json
