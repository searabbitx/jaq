type op = Eq | Neq

type expr =
  | Id of string
  | String of string
  | Index of int
  | Access of expr * expr
  | Select of select_list
  | Filter of expr * op * expr
  | Aliased of expr * expr

and select_list = SEmpty | SElement of expr * select_list

let rec select_list_to_ast_list = function
  | SElement (a, rest) -> a :: select_list_to_ast_list rest
  | SEmpty -> []
