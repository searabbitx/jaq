open Jaq
open Ast
open Error

let function_call f args json =
  match (f, args, json) with
  | "uppercase", FEmpty, `String s -> `String (String.uppercase_ascii s)
  | "uppercase", _, _ -> filter_error "Uppercase can be called on strings only!"
  | "lowercase", FEmpty, `String s -> `String (String.lowercase_ascii s)
  | "lowercase", _, _ -> filter_error "Lowercase can be called on strings only!"
  | "capitalize", FEmpty, `String s -> `String (String.capitalize_ascii s)
  | "capitalize", _, _ ->
      filter_error "Capitalize can be called on strings only!"
  | "replace", FElement (String x, FElement (String y, FEmpty)), `String s ->
      let r = Re.compile (Re.Posix.re x) in
      `String (Re.replace_string ~all:true r ~by:y s)
  | "replace", _, _ -> filter_error "Replace can be called on strings only!"
  | _, _, _ -> filter_error ("Function: " ^ f ^ " does not exist")
