type expr = Id of string | Access of expr * expr | Select of select_list
and select_list = SEmpty | SElement of expr * select_list
