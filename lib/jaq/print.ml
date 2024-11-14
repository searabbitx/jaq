let append_flush = Fun.flip ( ^ ) "\n%!"

type state = InKey | PostKey | InVal | Neutral
type ctx = Start | InArray | InObj

let str_head = function "" -> None | s -> Some (String.sub s 0 1)
let str_tail s = String.sub s 1 (String.length s - 1)

let should_switch_to_neutral str state =
  String.contains ",{}[" (String.get str 0) && state = PostKey

let push_ctx_stack stack s =
  match s with
  | "{" -> InObj :: stack
  | "[" -> InArray :: stack
  | "}" | "]" -> List.tl stack
  | _ -> stack

let rec colorize' ctx_stack state acc raw =
  match str_head raw with
  | None -> acc
  | Some "\"" -> (
      match (state, List.hd ctx_stack) with
      | Neutral, InArray | PostKey, _ ->
          colorize' ctx_stack InVal (acc ^ "@{<green>\"") (str_tail raw)
      | Neutral, _ ->
          colorize' ctx_stack InKey (acc ^ "@{<blue>\"") (str_tail raw)
      | InKey, _ -> colorize' ctx_stack PostKey (acc ^ "\"@}") (str_tail raw)
      | InVal, _ -> colorize' ctx_stack Neutral (acc ^ "\"@}") (str_tail raw))
  | Some s when should_switch_to_neutral s state ->
      colorize' (push_ctx_stack ctx_stack s) Neutral (acc ^ s) (str_tail raw)
  | Some s ->
      colorize' (push_ctx_stack ctx_stack s) state (acc ^ s) (str_tail raw)

let colorize = colorize' [ Start ] Neutral ""

let print raw =
  if Unix.isatty Unix.stdout then
    let colorized = raw |> colorize |> append_flush in
    let fmt = Scanf.format_from_string colorized "" in
    Ocolor_format.printf fmt
  else print_endline raw
