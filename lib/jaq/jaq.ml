module Read = Read
module Util = Util

let parse_json json_string = Yojson.Safe.from_string json_string
(* val json : Yojson.Safe.t *)

let json_pp json = Yojson.Safe.pretty_to_string ~std:false json
