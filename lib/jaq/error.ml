let exit_with_msg m =
  print_endline m;
  exit 1

let error_msg = function
  | Yojson.Json_error msg -> "Error parsing json!\n" ^ msg
  | e -> raise e

let handle_error e = e |> error_msg |> exit_with_msg
