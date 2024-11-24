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

let rec filter_json ast json =
  match ast with
  | Filter (_, _, _) -> json
  | _ -> failwith "Cannot use non-filter ast in filter_json"

and select s = function
  | `Assoc l ->
      select_list_to_ast_list s
      |> List.map (fun x -> exec_ast_for_select x (`Assoc l))
      |> Util.concat_jsons
  | `List l -> `List (List.map (select s) l)
  | _ -> raise (Error.FilterError "Cannot select from non object")

and exec_ast_for_select ?(alias = None) ast json =
  match ast with
  | Id id -> (
      match alias with
      | None -> `Assoc [ (id, extract_id id json) ]
      | Some alias -> `Assoc [ (alias, extract_id id json) ])
  | Access (e1, e2) -> exec_ast e1 json |> exec_ast_for_select ~alias e2
  | Aliased (e1, Id alias) -> exec_ast_for_select ~alias:(Some alias) e1 json
  | _ -> failwith "Subselects not implemented yet!"

and exec_ast ast json =
  match ast with
  | Access (e1, e2) -> exec_ast e1 json |> exec_ast e2
  | Id id -> extract_id id json
  | Select s -> select s json
  | Filter _ -> filter_json ast json
  | Aliased _ -> failwith "Cannot use aliases here"
  | String s -> `String s

let exec filter json =
  let ast = parse filter in
  exec_ast ast json
