module Read = Read

let json json_string = Yojson.Safe.from_string json_string
(* val json : Yojson.Safe.t *)

let json_pp json_string =
  Yojson.Safe.pretty_to_string ~std:false @@ json json_string
