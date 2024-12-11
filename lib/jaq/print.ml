let append_flush = Fun.flip ( ^ ) "\n%!"

type state = InKey | PostKey | InVal | Neutral
type ctx = Start | InArray | InObj

let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let revplode s =
  let rec exp i l =
    if i = String.length s then l else exp (i + 1) (s.[i] :: l)
  in
  exp 0 []

let suffix_char s c = s ^ String.make 1 c

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
  | '"' :: rest -> (
      match (state, List.hd ctx_stack) with
      | Neutral, InArray | PostKey, _ ->
          colorize' ctx_stack InVal (revplode "@{<green>\"" @ acc) rest
      | Neutral, _ ->
          colorize' ctx_stack InKey (revplode "@{<blue>\"" @ acc) rest
      | InKey, _ -> colorize' ctx_stack PostKey (revplode "\"@}" @ acc) rest
      | InVal, _ -> colorize' ctx_stack Neutral (revplode "\"@}" @ acc) rest)
  | c :: rest when should_switch_to_neutral c state ->
      colorize' (update_ctx_stack ctx_stack state c) Neutral (c :: acc) rest
  | c :: rest ->
      colorize' (update_ctx_stack ctx_stack state c) state (c :: acc) rest

let colorize raw =
  let acc = colorize' [ Start ] Neutral [] (explode raw) in
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
