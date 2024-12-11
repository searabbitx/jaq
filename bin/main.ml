open Jaq

let filter f j = match f with None | Some "" -> j | Some f -> Filter.exec f j

let () =
  let args = Args.parse_args () in
  let print_fun =
    if args.extract then Print.extract else Util.comp Print.print json_pp
  in
  try Read.read args.file |> parse_json |> filter args.filter |> print_fun
  with e -> Error.handle_error e
