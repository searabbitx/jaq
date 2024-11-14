let append_flush = Fun.flip ( ^ ) "\n%!"

type state = InKey | PostKey | InVal | Neutral
type ctx = Start | InArray | InObj

let str_head = function "" -> None | s -> Some (String.sub s 0 1)
let str_tail = function "" -> "" | s -> String.sub s 1 (String.length s - 1)

let should_switch_to_neutral str state =
  String.contains ",{}[" (String.get str 0) && state = PostKey

let update_ctx_stack stack state s =
  match state with
  | Neutral | PostKey -> (
      match s with
      | "{" -> InObj :: stack
      | "[" -> InArray :: stack
      | "}" | "]" -> List.tl stack
      | _ -> stack)
  | _ -> stack

let rec colorize' ctx_stack state acc raw =
  let rest = str_tail raw in
  match str_head raw with
  | None -> acc
  | Some "\"" -> (
      match (state, List.hd ctx_stack) with
      | Neutral, InArray | PostKey, _ ->
          colorize' ctx_stack InVal (acc ^ "@{<green>\"") rest
      | Neutral, _ -> colorize' ctx_stack InKey (acc ^ "@{<blue>\"") rest
      | InKey, _ -> colorize' ctx_stack PostKey (acc ^ "\"@}") rest
      | InVal, _ -> colorize' ctx_stack Neutral (acc ^ "\"@}") rest)
  | Some s when should_switch_to_neutral s state ->
      colorize' (update_ctx_stack ctx_stack state s) Neutral (acc ^ s) rest
  | Some s ->
      colorize' (update_ctx_stack ctx_stack state s) state (acc ^ s) rest

let colorize = colorize' [ Start ] Neutral ""

let print raw =
  if Unix.isatty Unix.stdout then
    let colorized = raw |> colorize |> append_flush in
    let fmt = Scanf.format_from_string colorized "" in
    Ocolor_format.printf fmt
  else print_endline raw
