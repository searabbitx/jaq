type expr = Id of string | Access of expr * expr | Select of expr
