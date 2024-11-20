open Jaq

type args = { filter : string option; file : string option }

let args =
  match Array.length Sys.argv with
  | 1 -> { filter = None; file = None }
  | 2 -> { filter = Some Sys.argv.(1); file = None }
  | 3 -> { filter = Some Sys.argv.(1); file = Some Sys.argv.(2) }
  | _ -> Error.exit_with_msg "Incorrect number of arguments"

let filter f j = match f with None | Some "" -> j | Some f -> Filter.exec f j

let help_msg =
  {|jaq - json processing tool with sane filter language

USAGE: jaq FILTER <INPUT>

if no input file is provided, stdin will be read|}

let help () =
  match Sys.argv.(1) with
  | "-h" | "--help" ->
      print_endline help_msg;
      exit 1
  | (exception Invalid_argument _) | _ -> ()

let () =
  help ();
  try
    Read.read args.file |> parse_json |> filter args.filter |> json_pp
    |> Print.print
  with e -> Error.handle_error e
