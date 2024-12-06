type args = { filter : string option; file : string option; extract : bool }

let args extract filter_file anons =
  match filter_file with
  | "" -> (
      match anons with
      | [] -> { filter = None; file = None; extract = extract}
      | [ filter ] -> { filter = Some filter; file = None; extract = extract }
      | [ filter; file ] -> { filter = Some filter; file = Some file; extract = extract }
      | _ -> Error.exit_with_msg "Incorrect number of arguments")
  | ff -> (
      let filter = Read.read (Some ff) in
      match anons with
      | [] -> { filter = Some filter; file = None; extract = extract }
      | [ file ] -> { filter = Some filter; file = Some file; extract = extract }
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
  -e, --extract      extract plain-text value from result json

|}

let print_help () =
  print_endline help_msg;
  exit 0

let apply_optional f s = match f with Some f -> f s | None -> ()

let parse_args () =
  let anons = ref [] 
  and filter_file = ref "" 
  and extract = ref false
  in
  let specs =
    [
      ('h', "help", Some print_help, None);
      ('e', "extract", (Getopt.set extract true), None);
      ( 'f',
        "from-file",
        None,
        Getopt.atmost_once filter_file
          (Getopt.Error "cannot provide more than one filter file") );
    ]
  in
  Getopt.parse_cmdline specs (apply_optional @@ Getopt.append anons);
  args !extract !filter_file !anons
