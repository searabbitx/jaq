open Ast
open Jaq

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let filter_error msg = raise (Error.FilterError msg)

let extract_id_assoc id l =
  l |> List.filter (fun x -> fst x = id) |> function
  | x :: _ -> snd x
  | _ -> filter_error ("Id '" ^ id ^ "' not found!")

let rec extract_id id = function
  | `Assoc l -> extract_id_assoc id l
  | `List l -> `List (List.map (extract_id id) l)
  | _ -> filter_error ("Cannot extract id: " ^ id)

let extract_index i = function
  | `List l -> (
      match List.nth_opt l i with
      | Some x -> x
      | None -> filter_error ("Index: " ^ string_of_int i ^ " out of bounds"))
  | _ ->
      filter_error
        ("Cannot extract index: " ^ string_of_int i ^ " from non-array")

let function_call f json =
  match (f, json) with
  | "uppercase", `String s -> `String (String.uppercase_ascii s)
  | "uppercase", _ -> filter_error "Uppercase can be called on strings only!"
  | "lowercase", `String s -> `String (String.lowercase_ascii s)
  | "lowercase", _ -> filter_error "Lowercase can be called on strings only!"
  | _, _ -> filter_error ("Function: " ^ f ^ " does not exist")

let rec filter_json ast json =
  match ast with
  | Filter f -> (
      match json with
      | `List l -> filter_json_list ast l
      | _ -> if eval_op f json then json else `Null)
  | _ -> failwith "Cannot use non-filter ast in filter_json"

and filter_json_list ast l =
  `List (List.filter (fun j -> filter_json ast j <> `Null) l)

and eval_op f json =
  match f with
  | LogicOp (le, op, re) -> (
      match op with
      | And -> eval_op le json && eval_op re json
      | Or -> eval_op le json || eval_op re json)
  | Op (le, RegMatch, Regex r) ->
      let left = exec_ast le json in
      eval_regex r left
  | Op (le, op, re) -> (
      let left = exec_ast le json in
      let right = exec_ast re json in
      match op with
      | Eq -> left = right
      | Neq -> left <> right
      | Gt -> left > right
      | Geq -> left >= right
      | Lt -> left < right
      | Leq -> left <= right
      | RegMatch -> failwith "Cannot use '~' here")

and eval_regex r json =
  match json with
  | `String s -> Re.execp r s
  | _ -> filter_error "Cannot match regex against non-string value"

and select s = function
  | `Assoc l ->
      select_list_to_ast_list s
      |> List.map (fun x -> exec_ast_for_select x (`Assoc l))
      |> Util.concat_jsons
  | `List l -> `List (List.map (select s) l)
  | _ -> filter_error "Cannot select from non object"

and exec_ast_for_select ?(alias = None) ast json =
  match ast with
  | Id id -> (
      match alias with
      | None -> `Assoc [ (id, extract_id id json) ]
      | Some alias -> `Assoc [ (alias, extract_id id json) ])
  | Access (Id id, FunctionCall f) -> (
      let json = extract_id id json in
      match alias with
      | None -> `Assoc [ (id, function_call f json) ]
      | Some alias -> `Assoc [ (alias, function_call f json) ])
  | Access (e1, e2) -> exec_ast e1 json |> exec_ast_for_select ~alias e2
  | Aliased (e1, Id alias) -> exec_ast_for_select ~alias:(Some alias) e1 json
  | _ -> failwith "Subselects not implemented yet!"

and exec_ast ast json : Yojson.Safe.t =
  match ast with
  | Access (e1, e2) -> exec_ast e1 json |> exec_ast e2
  | Id id -> extract_id id json
  | Index i -> extract_index i json
  | Select s -> select s json
  | Filter _ -> filter_json ast json
  | String s -> `String s
  | Int i -> `Int i
  | Regex _ -> failwith "Cannot use regex here"
  | Aliased _ -> failwith "Cannot use aliases here"
  | FunctionCall f -> function_call f json

let exec filter json =
  let ast = parse filter in
  exec_ast ast json
