let json_string = {|
  {"number" : 42,
   "string" : "yes",
   "list": ["for", "sure", 42]}|}
(* val json_string : string *)

let json = Yojson.Safe.from_string json_string
(* val json : Yojson.Safe.t *)

let json_pp () = Yojson.Safe.pretty_to_string ~std:false json

