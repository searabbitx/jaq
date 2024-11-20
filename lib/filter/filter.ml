open Ast
open Jaq

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let extract_id_assoc id l =
  l |> List.filter (fun x -> fst x = id) |> function
  | x :: _ -> snd x
  | _ ->
      let msg = "Id '" ^ id ^ "' not found!" in
      raise (Error.FilterError msg)

let rec extract_id id = function
  | `Assoc l -> extract_id_assoc id l
  | `List l -> `List (List.map (extract_id id) l)
  | _ ->
      let msg = "Cannot extract id: " ^ id in
      raise (Error.FilterError msg)

let rec select s = function
  | `Assoc l ->
      select_list_to_ast_list s
      |> List.map (Fun.flip exec_ast_for_select (`Assoc l))
      |> Util.concat_jsons
  | `List l -> `List (List.map (select s) l)
  | _ -> raise (Error.FilterError "Cannot select from non object")

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

let exec filter json =
  let ast = parse filter in
  exec_ast ast json
