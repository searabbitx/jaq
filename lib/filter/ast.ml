type expr =
  | Id of string
  | Access of expr * expr
  | Select of select_list
  | Aliased of expr * expr

and select_list = SEmpty | SElement of expr * select_list

let rec select_list_to_ast_list = function
  | SElement (a, rest) -> a :: select_list_to_ast_list rest
  | SEmpty -> []
