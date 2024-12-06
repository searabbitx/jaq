type args = { filter : string option; file : string option }

let args filter_file anons =
  match filter_file with
  | "" -> (
      match anons with
      | [] -> { filter = None; file = None }
      | [ filter ] -> { filter = Some filter; file = None }
      | [ filter; file ] -> { filter = Some filter; file = Some file }
      | _ -> Error.exit_with_msg "Incorrect number of arguments")
  | ff -> (
      let filter = Read.read (Some ff) in
      match anons with
      | [] -> { filter = Some filter; file = None }
      | [ file ] -> { filter = Some filter; file = Some file }
      | _ -> Error.exit_with_msg "Incorrect number of arguments")

let help_msg =
  {|jaq - json processing tool with sane filter language

USAGE: jaq [options...] filter [input]

ARGS:
  filter             filter string
  input              input file. If none specified, stdin will be read

OPTIONS:
  -h, --help         print this message and exit
  -f, --from-file    read filter from file 

|}

let print_help () =
  print_endline help_msg;
  exit 0

let apply_optional f s = match f with Some f -> f s | None -> ()

let parse_args () =
  let anons = ref [] and filter_file = ref "" in
  let specs =
    [
      ('h', "help", Some print_help, None);
      ( 'f',
        "from-file",
        None,
        Getopt.atmost_once filter_file
          (Getopt.Error "cannot provide more than one filter file") );
    ]
  in
  Getopt.parse_cmdline specs (apply_optional @@ Getopt.append anons);
  args !filter_file !anons
