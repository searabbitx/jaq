type op = Eq | Neq | Gt | Lt | Geq | Leq
type logic_op = And | Or

type expr =
  | Id of string
  | String of string
  | Int of int
  | Index of int
  | Access of expr * expr
  | Select of select_list
  | Filter of filter_expr
  | Aliased of expr * expr

and filter_expr =
  | Op of expr * op * expr
  | LogicOp of filter_expr * logic_op * filter_expr

and select_list = SEmpty | SElement of expr * select_list

let rec select_list_to_ast_list = function
  | SElement (a, rest) -> a :: select_list_to_ast_list rest
  | SEmpty -> []
