let append_flush = Fun.flip ( ^ ) "\n%!"

type state = InKey | PostKey | InVal | Neutral

let str_head = function "" -> None | s -> Some (String.sub s 0 1)
let str_tail s = String.sub s 1 (String.length s - 1)

let should_switch_to_neutral str state =
  String.contains ",{}[" (String.get str 0) && state = PostKey

let rec colorize' state acc raw =
  match str_head raw with
  | None -> acc
  | Some "\"" -> (
      match state with
      | Neutral -> colorize' InKey (acc ^ "@{<blue>\"") (str_tail raw)
      | InKey -> colorize' PostKey (acc ^ "\"@}") (str_tail raw)
      | PostKey -> colorize' InVal (acc ^ "@{<green>\"") (str_tail raw)
      | InVal -> colorize' Neutral (acc ^ "\"@}") (str_tail raw))
  | Some s when should_switch_to_neutral s state ->
      colorize' Neutral (acc ^ s) (str_tail raw)
  | Some s -> colorize' state (acc ^ s) (str_tail raw)

let colorize = colorize' Neutral ""

let print raw =
  if Unix.isatty Unix.stdout then
    let colorized = raw |> colorize |> append_flush in
    let fmt = Scanf.format_from_string colorized "" in
    Ocolor_format.printf fmt
  else print_endline raw
