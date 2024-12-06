type args = { filter : string option; file : string option }

let args = function
  | [] -> { filter = None; file = None }
  | [ filter ] -> { filter = Some filter; file = None }
  | [ filter; file ] -> { filter = Some filter; file = Some file }
  | _ -> Error.exit_with_msg "Incorrect number of arguments"

let help_msg =
  {|jaq - json processing tool with sane filter language

USAGE: jaq FILTER <INPUT>

if no input file is provided, stdin will be read|}

let print_help () =
  print_endline help_msg;
  exit 0

let specs = [ ('h', "help", Some print_help, None) ]
let apply_optional f s = match f with Some f -> f s | None -> ()

let parse_args () =
  let anons = ref [] in
  Getopt.parse_cmdline specs (apply_optional @@ Getopt.append anons);
  args !anons
