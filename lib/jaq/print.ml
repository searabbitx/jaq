let append_flush = Fun.flip ( ^ ) "\n%!"

type state = InKey | PostKey | InVal | Neutral
type ctx = Start | InArray | InObj

let should_switch_to_neutral c state =
  String.contains ",{}[" c && state = PostKey

let update_ctx_stack stack state c =
  match state with
  | Neutral | PostKey -> (
      match c with
      | '{' -> InObj :: stack
      | '[' -> InArray :: stack
      | '}' | ']' -> List.tl stack
      | _ -> stack)
  | _ -> stack

let rec colorize' ctx_stack state acc raw =
  match raw with
  | [] -> acc
  | '"' :: rest ->
      let next_state, color_tag =
        match (state, List.hd ctx_stack) with
        | Neutral, InArray | PostKey, _ -> (InVal, "@{<green>\"")
        | Neutral, _ -> (InKey, "@{<blue>\"")
        | InKey, _ -> (PostKey, "\"@}")
        | InVal, _ -> (Neutral, "\"@}")
      in
      colorize' ctx_stack next_state (Util.revplode color_tag @ acc) rest
  | c :: rest ->
      let next_state =
        if should_switch_to_neutral c state then Neutral else state
      and next_stack = update_ctx_stack ctx_stack state c in
      colorize' next_stack next_state (c :: acc) rest

let colorize raw =
  let acc = colorize' [ Start ] Neutral [] (Util.explode raw) in
  acc |> List.rev |> List.to_seq |> String.of_seq

let print raw =
  if Unix.isatty Unix.stdout then
    let colorized = raw |> colorize |> append_flush in
    let fmt = Scanf.format_from_string colorized "" in
    Ocolor_format.printf fmt
  else print_endline raw

let rec extract_val = function
  | `String s -> s
  | `Int i -> string_of_int i
  | `Float f -> string_of_float f
  | `Bool b -> string_of_bool b
  | `Null -> "null"
  | `List l -> l |> List.map extract_val |> Util.concat_strings "\n"
  | _ ->
      Error.exit_with_msg
        "You can only extract primitives and lists of primitives"

let extract json = print_endline @@ extract_val json
