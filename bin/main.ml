open Jaq

let filter f j = match f with None | Some "" -> j | Some f -> Filter.exec f j

let () =
  let args = Args.parse_args () in
  let print_fun =
    if args.extract then Print.extract else Util.comp Print.print json_pp
  in
  try
    Read.read args.file
    |> Jaq.Time.time1 "parse_json" parse_json
    |> Jaq.Time.time1 "filter" (filter args.filter)
    |> Jaq.Time.time1 "print_fun" print_fun
  with e -> Error.handle_error e
