open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let list_contains list needle =
  match List.find_opt (fun x -> x = needle) list with
  | Some _ -> true
  | None -> false

let concat_json (j1 : Yojson.Safe.t) (j2 : Yojson.Safe.t) =
  match (j1, j2) with
  | `Assoc a1, `Assoc a2 -> `Assoc (a1 @ a2)
  | `List l1, `List l2 -> `List (l1 @ l2)
  | _ -> failwith "Cannot concat assoc with list!"

let rec concat_jsons (js : Yojson.Safe.t list) : Yojson.Safe.t =
  match js with
  | j :: j' :: rest -> concat_json j (concat_jsons (j' :: rest))
  | j :: [] -> j
  | [] -> failwith "Cannot concat empty json list!"

let extract_id_assoc id l =
  l |> List.filter (fun x -> fst x = id) |> function
  | x :: _ -> snd x
  | _ -> failwith @@ "Id: " ^ id ^ " not found!"

let rec extract_id id = function
  | `Assoc l -> extract_id_assoc id l
  | `List l -> `List (List.map (extract_id id) l)
  | _ -> failwith @@ "Cannot extract id: " ^ id

let rec select_list_to_ast_list = function
  | SElement (a, rest) -> a :: select_list_to_ast_list rest
  | SEmpty -> []

(* jesus, refactor this mess! *)
let rec select s = function
  | `Assoc l ->
      select_list_to_ast_list s
      |> List.map (fun ast -> exec_ast_for_select ast (`Assoc l))
      |> concat_jsons
  | `List l -> `List (List.map (select s) l)
  | _ -> failwith "Cannot select from non object"

and exec_ast_for_select ast json =
  match ast with
  | Id id -> `Assoc [ (id, extract_id id json) ]
  | Access (e1, e2) -> exec_ast e1 json |> exec_ast_for_select e2
  | _ -> failwith "Subselects not implemented yet!"

and exec_ast ast json =
  match ast with
  | Access (e1, e2) -> exec_ast e1 json |> exec_ast e2
  | Id id -> extract_id id json
  | Select s -> select s json

let exec filter json : Yojson.Safe.t =
  let ast = parse filter in
  exec_ast ast json
